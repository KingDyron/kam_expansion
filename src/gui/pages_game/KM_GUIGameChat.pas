unit KM_GUIGameChat;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Math, StrUtils, SysUtils,

  KM_Networking, KM_NetworkTypes,
  KM_InterfaceGame, KM_InterfaceTypes, KM_CommonTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsDragger, KM_ControlsEdit, KM_ControlsMemo, KM_ControlsPopUp,
  KM_Defaults, KM_Points, KM_Console;


type
  TKMGUIGameChat = class
  private
    fUIMode: TUIMode;
    fOnChatMessage: TUnicodeStringEvent;
    procedure Chat_Close(Sender: TObject);
    function DoPost: Boolean;
    function Chat_Post(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
    procedure Chat_Resize(Sender: TObject; X,Y: Integer);
    procedure Chat_MenuClick(Sender: TObject);
    procedure Chat_MenuSelect(aItemTag: TKMNetHandleIndex);
    procedure Chat_MenuShow(Sender: TObject);
    procedure ChatMemo_CopyAllowed_Click(Sender: TObject);
    function GetPanelChatRect: TKMRect;
    function IsKeyEvent_Return_Handled(Sender: TObject; Key: Word): Boolean;

    procedure PostLocalMsg(const aMsg: UnicodeString);
    procedure PostMsg(const aMsg: UnicodeString);
    procedure HandleError(const aMsg: UnicodeString);

    procedure ChatTextChanged(Sender: TObject);
    procedure SetChatHandlers;
    procedure UpdateChatControls;
  protected
    Panel_Chat: TKMPanel; //For multiplayer: Send, reply, text area for typing, etc.
      Dragger_Chat: TKMDragger;
      Image_Chat: TKMImage;
      Memo_ChatText: TKMMemo;
      Button_ChatActionsAllowed: TKMButtonFlat; //Button to allow Memo Copy/Paste or use Up/Down keys for Chat history
      Edit_ChatMsg: TKMEdit;
      Button_ChatRecipient: TKMButtonFlat;
      Image_ChatClose: TKMImage;
      Menu_Chat: TKMPopUpMenu;
  public
    constructor Create(aParent: TKMPanel; aUIMode: TUIMode; aOnChatMessage: TUnicodeStringEvent);

    property PanelChatRect: TKMRect read GetPanelChatRect;

    procedure ChatMessage(const aData: UnicodeString);
    procedure Unfocus;
    procedure Focus;

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_GameApp, KM_RenderUI, KM_ResTexts, KM_GameParams, KM_CommonUtils,
  KM_ResSound, KM_Resource, KM_ResFonts, KM_Sound, KM_NetPlayersList;


{ TKMGUIGameChat }
constructor TKMGUIGameChat.Create(aParent: TKMPanel; aUIMode: TUIMode; aOnChatMessage: TUnicodeStringEvent);
begin
  inherited Create;

  fUIMode := aUIMode;
  fOnChatMessage := aOnChatMessage;

  Panel_Chat := TKMPanel.Create(aParent, TOOLBAR_WIDTH, aParent.Height - MESSAGE_AREA_HEIGHT, 600, MESSAGE_AREA_HEIGHT);
  Panel_Chat.Anchors := [anLeft, anBottom];
  Panel_Chat.Hide;

    Image_Chat := TKMImage.Create(Panel_Chat, 0, 0, 600, 500, 409);
    Image_Chat.Anchors := [anLeft,anTop,anBottom];

    // Allow to resize chat area height
    Dragger_Chat := TKMDragger.Create(Panel_Chat, 45, 36, 600-130, 10);
    Dragger_Chat.Anchors := [anTop];
    Dragger_Chat.SetBounds(0, -MESSAGE_AREA_RESIZE_Y, 0, 0);
    Dragger_Chat.OnMove := Chat_Resize;

    Memo_ChatText := TKMMemo.Create(Panel_Chat,45,50,600-85,101, fntArial, bsGame);
    Memo_ChatText.AnchorsStretch;
    Memo_ChatText.WordWrap := True;
    Memo_ChatText.IndentAfterNL := True; // Don't let players fake system messages
    Memo_ChatText.ScrollDown := True;
    Memo_ChatText.Selectable := False;

    Button_ChatRecipient := TKMButtonFlat.Create(Panel_Chat,45,154,132,20,0);
    Button_ChatRecipient.Font := fntGrey;
    Button_ChatRecipient.CapOffsetY := -11;
    Button_ChatRecipient.OnClick := Chat_MenuShow;
    Button_ChatRecipient.Anchors := [anRight, anBottom];
    Button_ChatRecipient.Visible := fUIMode in [umMP, umSpectate];

    Edit_ChatMsg := TKMEdit.Create(Panel_Chat, 45 + 30*Byte(fUIMode in [umMP, umSpectate]), 154,
                                              410 - 30*Byte(fUIMode in [umMP, umSpectate]), 20, fntArial);
    Edit_ChatMsg.Anchors := [anLeft, anRight, anBottom];
    Edit_ChatMsg.OnChange := ChatTextChanged;
    Edit_ChatMsg.OnKeyDown := Chat_Post;
    Edit_ChatMsg.OnIsKeyEventHandled := IsKeyEvent_Return_Handled;
    Edit_ChatMsg.Text := '';
    Edit_ChatMsg.ShowColors := True;

    Button_ChatActionsAllowed := TKMButtonFlat.Create(Panel_Chat, 45+600-85+3,154-1,24,22,663);
    Button_ChatActionsAllowed.Anchors := [anBottom];
    Button_ChatActionsAllowed.Hint := gResTexts[TX_CHAT_ACTIONS_ALLOWED_HINT];
    Button_ChatActionsAllowed.OnClick := ChatMemo_CopyAllowed_Click;

    Image_ChatClose := TKMImage.Create(Panel_Chat, 600-80, 18, 32, 32, 52);
    Image_ChatClose.Anchors := [anTop, anRight];
    Image_ChatClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_ChatClose.OnClick := Chat_Close;
    Image_ChatClose.HighlightOnMouseOver := True;

    Menu_Chat := TKMPopUpMenu.Create(aParent, 120);
    Menu_Chat.Anchors := [anLeft, anBottom];
    //Menu gets populated right before show
    Menu_Chat.AddItem(NO_TEXT);
    Menu_Chat.OnClick := Chat_MenuClick;
end;


procedure TKMGUIGameChat.Chat_Close(Sender: TObject);
begin
  Hide;
end;


function TKMGUIGameChat.IsKeyEvent_Return_Handled(Sender: TObject; Key: Word): Boolean;
begin
  Result := False;
  case Key of
    //Sending chat during reconnections at best causes messages to be lost and at worst causes
    //crashes due to intermediate connecting states. Therefore we block sending completely.
    VK_RETURN:  Result := ((gNetworking <> nil) and not gNetworking.IsReconnecting)
                           or gGameParams.IsSingleplayerGame;
    VK_UP,
    VK_DOWN:    Result := Button_ChatActionsAllowed.Down;
  end;
end;


procedure TKMGUIGameChat.ChatMemo_CopyAllowed_Click(Sender: TObject);
begin
  Button_ChatActionsAllowed.Down := not Button_ChatActionsAllowed.Down;
  Memo_ChatText.Selectable := Button_ChatActionsAllowed.Down;
end;


procedure TKMGUIGameChat.HandleError(const aMsg: UnicodeString);
begin
  if fUIMode in [umMP, umSpectate] then
    gNetworking.PostLocalMessage(aMsg, csSystem)
  else if Assigned(fOnChatMessage) then
    fOnChatMessage(aMsg);
end;


procedure TKMGUIGameChat.PostLocalMsg(const aMsg: UnicodeString);
begin
  if fUIMode in [umMP, umSpectate] then
    gNetworking.PostLocalMessage(aMsg, csChat)
  else if Assigned(fOnChatMessage) then
    fOnChatMessage(aMsg)
end;


procedure TKMGUIGameChat.PostMsg(const aMsg: UnicodeString);
begin
  if fUIMode in [umMP, umSpectate] then
  begin
    if gGameApp.Chat.Mode = cmWhisper then
      gNetworking.PostChat(aMsg, gGameApp.Chat.Mode, gGameApp.Chat.WhisperRecipient)
    else
      gNetworking.PostChat(aMsg, gGameApp.Chat.Mode);

  end else if Assigned(fOnChatMessage) then
    fOnChatMessage(aMsg);
end;


procedure TKMGUIGameChat.ChatTextChanged(Sender: TObject);
begin
  gGameApp.Chat.Text := Edit_ChatMsg.Text;
end;


function TKMGUIGameChat.DoPost: Boolean;
var
  netI: Integer;
begin
  Result := False;
  if not gGameApp.Chat.IsPostAllowed then
    Exit;

  if not gGameApp.Chat.TryCallConsoleCommand then
  begin
    if gGameApp.Chat.Mode = cmWhisper then
    begin
      netI := gNetworking.NetPlayers.ServerToLocal(gGameApp.Chat.WhisperRecipient);

      // Do not allow to whisper to disconnected player
      if netI = -1 then Exit;
      
      if not gNetworking.NetPlayers[netI].Connected
        or gNetworking.NetPlayers[netI].Dropped then
      begin
        gNetworking.PostLocalMessage(Format(gResTexts[TX_MULTIPLAYER_CHAT_PLAYER_NOT_CONNECTED_ANYMORE],
                                            [gNetworking.NetPlayers[netI].NicknameColored]),
                                     csSystem);
        Chat_MenuSelect(CHAT_MENU_ALL);
      end else
        gGameApp.Chat.Post
    end else
      gGameApp.Chat.Post;
  end;

  Result := True;
  Edit_ChatMsg.Text := '';
  Memo_ChatText.ScrollToBottom;
end;


function TKMGUIGameChat.Chat_Post(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
var
  str: String;
begin
  Result := False;
  if IsKeyEvent_Return_Handled(Self, Key) then
  begin
    case Key of
      VK_RETURN:  Result := DoPost;
      VK_UP:      begin
                    str := gGameApp.Chat.GetNextHistoryMsg;
                    if str <> '' then
                    begin
                      Edit_ChatMsg.Text := str;
                      Result := True;
                    end;
                  end;
      VK_DOWN:    begin
                    str := gGameApp.Chat.GetPrevHistoryMsg;
                    if str <> '' then
                    begin
                      Edit_ChatMsg.Text := str;
                      Result := True;
                    end;
                  end;
    end;
  end;
end;


procedure TKMGUIGameChat.Chat_Resize(Sender: TObject; X,Y: Integer);
var
  H: Integer;
begin
  H := EnsureRange(-Y, 0, MESSAGE_AREA_RESIZE_Y);
  Panel_Chat.Top := Panel_Chat.Parent.Height - (MESSAGE_AREA_HEIGHT + H);
  Panel_Chat.Height := MESSAGE_AREA_HEIGHT + H;
end;


procedure TKMGUIGameChat.Chat_MenuSelect(aItemTag: TKMNetHandleIndex);

  procedure UpdateButtonCaption(aCaption: UnicodeString; aColor: Cardinal = 0);
  const
    MIN_SIZE = 80; //Minimum size for the button
  var
    txtWidth: Word;
  begin
    //Update button width according to selected item
    txtWidth := gRes.Fonts[Button_ChatRecipient.Font].GetTextSize(aCaption).X;
    txtWidth := Max(MIN_SIZE, txtWidth + 10); //Apply minimum size

    if aColor <> 0 then
      aCaption := WrapColor(aCaption, aColor);
    Button_ChatRecipient.Caption := aCaption;
    Button_ChatRecipient.Width := txtWidth;

    Edit_ChatMsg.AbsLeft := Button_ChatRecipient.AbsLeft
                            + Byte(Button_ChatRecipient.IsSetVisible) * (Button_ChatRecipient.Width + 4);
    Edit_ChatMsg.Width := Memo_ChatText.Width
                          - Byte(Button_ChatRecipient.IsSetVisible) * (Button_ChatRecipient.Width + 4);
  end;

var
  netI: Integer;
begin
  case aItemTag of
    CHAT_MENU_ALL:        begin //All
                            gGameApp.Chat.Mode := cmAll;
                            UpdateButtonCaption(gResTexts[TX_CHAT_ALL]);
                            Edit_ChatMsg.DrawOutline := False; //No outline for All
                          end;
    CHAT_MENU_TEAM:       begin //Team
                            gGameApp.Chat.Mode := cmTeam;
                            UpdateButtonCaption(gResTexts[TX_CHAT_TEAM], $FF66FF66);
                            Edit_ChatMsg.DrawOutline := True;
                            Edit_ChatMsg.OutlineColor := $FF66FF66;
                          end;
    CHAT_MENU_SPECTATORS: begin //Spectators
                            gGameApp.Chat.Mode := cmSpectators;
                            UpdateButtonCaption(gResTexts[TX_CHAT_SPECTATORS], $FF66FF66);
                            Edit_ChatMsg.DrawOutline := True;
                            Edit_ChatMsg.OutlineColor := $FF66FF66;
                          end;
    else  begin //Whisper to player
            netI := gNetworking.NetPlayers.ServerToLocal(aItemTag);
            if netI <> -1 then
            begin
              gGameApp.Chat.Mode := cmWhisper;
              Edit_ChatMsg.DrawOutline := True;
              Edit_ChatMsg.OutlineColor := $FF00B9FF;
              with gNetworking.NetPlayers[netI] do
              begin
                gGameApp.Chat.WhisperRecipient := aItemTag;
                UpdateButtonCaption(NicknameU, IfThen(IsColorSet, FlagColorToTextColor(FlagColor), 0));
              end;
            end;
          end;
    end;
end;


procedure TKMGUIGameChat.Chat_MenuClick(Sender: TObject);
begin
  if Menu_Chat.ItemIndex <> -1 then
    Chat_MenuSelect(Menu_Chat.ItemTags[Menu_Chat.ItemIndex]);
end;


function TKMGUIGameChat.GetPanelChatRect: TKMRect;
begin
  Result := Panel_Chat.Rect;
end;


procedure TKMGUIGameChat.Chat_MenuShow(Sender: TObject);
var
  C: TKMControl;
  I: Integer;
  n: TKMNetPlayerInfo;
begin
  Menu_Chat.Clear;

  //Fill lists with options to whom player can whisper
  Menu_Chat.AddItem(gResTexts[TX_CHAT_ALL], CHAT_MENU_ALL);

  //Only show "Team" if the player is on a team
  if gNetworking.MyNetPlayer.Team <> 0 then
    Menu_Chat.AddItem('[$66FF66]' + gResTexts[TX_CHAT_TEAM], CHAT_MENU_TEAM);

  //Only show "Spectator" if the player is a spectator
  if gNetworking.MyNetPlayer.IsSpectator then
    Menu_Chat.AddItem('[$66FF66]' + gResTexts[TX_CHAT_SPECTATORS], CHAT_MENU_SPECTATORS);

  //Fill
  for I := 1 to gNetworking.NetPlayers.Count do
  if I <> gNetworking.MyIndex then //Can't whisper to self
  begin
    n := gNetworking.NetPlayers[I];

    if n.IsHuman and n.Connected and not n.Dropped then
      Menu_Chat.AddItem(n.NicknameColoredU, n.IndexOnServer);
  end;

  C := TKMControl(Sender);
  //Position the menu next to the icon, but do not overlap players name
  Menu_Chat.ShowAt(C.AbsLeft, C.AbsTop - Menu_Chat.Height);
end;


procedure TKMGUIGameChat.ChatMessage(const aData: UnicodeString);
begin
  gGameApp.Chat.AddLine(aData);
end;


procedure TKMGUIGameChat.Unfocus;
begin
  // Update only Edit focus, Memo will lose focus automatically
  Edit_ChatMsg.Focusable := False; // Will update focus automatically
end;


procedure TKMGUIGameChat.Focus;
begin
  // Update focus on chat panel (both Edit and Memo could be focused)
  Edit_ChatMsg.Focusable := True; // Will update focus automatically
end;


procedure TKMGUIGameChat.UpdateChatControls;
begin
  if gGameApp.Chat.Mode = cmWhisper then
    Chat_MenuSelect(gGameApp.Chat.WhisperRecipient)
  else
    Chat_MenuSelect(CHAT_TAG[gGameApp.Chat.Mode]);

  Memo_ChatText.Text := gGameApp.Chat.Messages;
end;


procedure TKMGUIGameChat.SetChatHandlers;
begin
  gGameApp.Chat.OnPost := PostMsg;
  gGameApp.Chat.OnPostLocal := PostLocalMsg;
  gGameApp.Chat.OnError := HandleError;
  gGameApp.Chat.OnChange := UpdateChatControls;

  UpdateChatControls;
  Edit_ChatMsg.Text := gGameApp.Chat.Text; //Update this text only once, when first time show Chat in game
  Memo_ChatText.ScrollToBottom;
end;


procedure TKMGUIGameChat.Show;
begin
  if not Panel_Chat.Visible then
    gSoundPlayer.Play(sfxnMPChatOpen);

  SetChatHandlers;

  Focus;
  Panel_Chat.Show;
end;


procedure TKMGUIGameChat.Hide;
begin
  if Panel_Chat.Visible then
    gSoundPlayer.Play(sfxnMPChatClose);
  Panel_Chat.Hide;
  Menu_Chat.HideMenu;
end;


function TKMGUIGameChat.Visible: Boolean;
begin
  Result := Panel_Chat.Visible;
end;


end.
