unit KM_GUIMenuMultiplayer;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  StrUtils, SysUtils, Math, Classes,
  KromOGLUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsList, KM_ControlsMemo, KM_ControlsScroll,
  KM_Defaults, KM_CommonTypes, KM_Pics,
  KM_InterfaceDefaults, KM_InterfaceTypes, KM_ServerQuery;


type
  TKMMenuMultiplayer = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText; //will be in ancestor class

    fServerSelected: Boolean;
    fSelectedRoomInfo: TKMRoomInfo;
    fSelectedServerInfo: TKMServerInfo;
    fLobbyBusy: Boolean;

    procedure MP_Init;
    procedure MP_SaveSettings;
    procedure MP_UpdateStatus(const aStatus: string; aColor: TColor4; aBusy: Boolean);
    procedure MP_ServersUpdateList(Sender: TObject);
    procedure MP_AnnouncementsUpdated(const S: UnicodeString);
    procedure MP_CreateServerClick(Sender: TObject);
    procedure MP_FindServerClick(Sender: TObject);
    procedure MP_CreateServerCancelClick(Sender: TObject);
    procedure MP_FindServerIPClick(Sender: TObject);
    procedure MP_PasswordClick(Sender: TObject);
    procedure MP_FindServerCancelClick(Sender: TObject);
    procedure MP_ClearServerDetailsPanel;
    procedure MP_ServersRefresh(Sender: TObject);
    procedure MP_ServersSort(aIndex: Integer);
    procedure MP_ServersClick(Sender: TObject);
    procedure MP_ServersDoubleClick(Sender: TObject);
    procedure MP_GetInClick(Sender: TObject);
    function MP_GetInEnabled: Boolean;
    procedure MP_Join(const aServerAddress: string; aPort: Word; aRoom: Integer);
    procedure MP_JoinPassword;
    procedure MP_JoinSuccess;
    procedure MP_JoinFail(const aData: UnicodeString);
    procedure MP_JoinAssignedHost;
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData: UnicodeString);
    procedure BackClick(Sender: TObject);
    function ValidatePlayerName(const aName: UnicodeString): Boolean;
    procedure EscKeyDown(Sender: TObject);
    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure UpdateGameTimeLabel;
    procedure ServerDetailsScrollChangedVisibility(Sender: TObject; aVisible: Boolean);

    procedure StartLobby(aIsHost: Boolean);

    procedure UpdateServerDetailsUI;
  protected
    Panel_MultiPlayer: TKMPanel;
      Panel_MPAnnouncement: TKMPanel;
        Memo_MP_Announcement: TKMMemo;
      ColumnBox_Servers: TKMColumnBox;
      Label_Servers_Status: TKMLabel;
      Button_MP_Back: TKMButton;
      Button_MP_Refresh: TKMButton;
      Button_MP_GetIn: TKMButton;

      Panel_MPPlayerName: TKMPanel;
        Edit_MP_PlayerName: TKMEdit;
        Label_MP_Status: TKMLabel;

      Button_MP_CreateServer: TKMButton;
      Button_MP_FindServer: TKMButton;

      Panel_MPServerDetails: TKMScrollPanel;
        Label_MP_ServerDetails_Header, Label_MP_GameInfo_Header, Label_MP_Map_Header,
        Label_MP_PlayerList_Header, Label_MP_Team_Header,
        Label_MP_Desc, Label_MP_PT_Times, Label_MP_GameTime, Label_MP_MapName: TKMLabel;
        Label_MP_PlayersNames: array[1..MAX_LOBBY_SLOTS] of TKMLabel;
        Label_MP_PlayersTeams: array[1..MAX_LOBBY_SLOTS] of TKMLabel;
        Image_MP_PlayerIcons: array[1..MAX_LOBBY_SLOTS] of TKMImage;
        Image_MP_PlayerSpecIcons: array[1..MAX_LOBBY_SLOTS] of TKMImage;
        Image_MP_PlayerWolIcons: array[1..MAX_LOBBY_SLOTS] of TKMImage; //Win or lose
        Image_MP_Host: TKMImage;

      //PopUps
      Panel_MPCreateServer: TKMPanel;
        Edit_MP_ServerName: TKMEdit;
        Edit_MP_ServerPort: TKMEdit;
        Button_MP_CreateLAN: TKMButton;
        Button_MP_CreateWAN: TKMButton;
        Button_MP_CreateServerCancel: TKMButton;

      Panel_MPFindServer: TKMPanel;
        Button_MP_FindServerIP: TKMButton;
        Button_MP_FindCancel: TKMButton;
        Edit_MP_FindIP: TKMEdit;
        Edit_MP_FindPort: TKMEdit;
        Edit_MP_FindRoom: TKMEdit;

      Panel_MPPassword: TKMPanel;
        Edit_MP_Password: TKMEdit;
        Button_MP_PasswordOk: TKMButton;
        Button_MP_PasswordCancel: TKMButton;
  public
    OnNetworkInit: TEvent;

    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

    procedure Show(const aText: UnicodeString);
    procedure Resize(X, Y: Word);
  end;


implementation
uses
  KM_Main, 
  KM_GameSettings,
  KM_ControlsTypes,
  KM_ServerSettings,
  KM_Networking, KM_NetworkTypes,
  KM_ResTexts, KM_ResLocales, KM_ResFonts, KM_ResSound, KM_ResTypes,
  KM_Sound,
  KM_RenderUI,
  KM_GUIMenuLobby,
  KM_MapTypes,
  KM_CommonUtils, KM_Console;


const
  IMG_COL2 = 8 + 22 + 156 + 35 + 20;
  SERVER_DETAILS_W = 320;
  S_DETAILS_W_INT = SERVER_DETAILS_W - 16;


{ TKMGUIMainMultiplayer }
constructor TKMMenuMultiplayer.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);

  procedure CreateServerPopUp;
  begin
    Panel_MPCreateServer := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPCreateServer.AnchorsCenter;
      TKMBevel.Create(Panel_MPCreateServer, -2000,  -2000, 5000, 5000);
      TKMImage.Create(Panel_MPCreateServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPCreateServer, 20, 10, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_HEADER], fntOutline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 20, 50, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_NAME], fntOutline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 20, 70, 280, 20, fntGrey);
      Edit_MP_ServerName.AllowedChars := acANSI7;
      TKMLabel.Create(Panel_MPCreateServer, 20, 100, 284, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_PORT], fntOutline, taLeft);
      Edit_MP_ServerPort := TKMEdit.Create(Panel_MPCreateServer, 20, 120, 100, 20, fntGrey);
      Edit_MP_ServerPort.AllowedChars := acDigits;
      Button_MP_CreateLAN  := TKMButton.Create(Panel_MPCreateServer, 20, 170, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_LOCAL],  bsMenu);
      Button_MP_CreateWAN  := TKMButton.Create(Panel_MPCreateServer, 20, 210, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_INTERNET],  bsMenu);
      Button_MP_CreateServerCancel := TKMButton.Create(Panel_MPCreateServer, 20, 250, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_CANCEL],  bsMenu);
      Button_MP_CreateLAN.OnClick := MP_HostClick;
      Button_MP_CreateWAN.OnClick := MP_HostClick;
      Button_MP_CreateServerCancel.OnClick := MP_CreateServerCancelClick;
  end;

  procedure FindServerPopUp;
  begin
    Panel_MPFindServer := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPFindServer.AnchorsCenter;
      TKMBevel.Create(Panel_MPFindServer, -2000,  -2000, 5000, 5000);
      TKMImage.Create(Panel_MPFindServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPFindServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPFindServer,  20, 10, 280, 20, gResTexts[TX_MP_MENU_FIND_SERVER_HEADER], fntOutline, taCenter);

      TKMLabel.Create(Panel_MPFindServer, 20, 50, 156, 20, gResTexts[TX_MP_MENU_FIND_SERVER_ADDRESS], fntOutline, taLeft);
      Edit_MP_FindIP := TKMEdit.Create(Panel_MPFindServer, 20, 70, 152, 20, fntGrey);
      Edit_MP_FindIP.AllowedChars := acText; //Server name could be "localhost"
      TKMLabel.Create(Panel_MPFindServer, 172, 50, 60, 20, gResTexts[TX_MP_MENU_FIND_SERVER_PORT], fntOutline, taLeft);
      Edit_MP_FindPort := TKMEdit.Create(Panel_MPFindServer, 172, 70, 60, 20, fntGrey);
      Edit_MP_FindPort.AllowedChars := acDigits;
      TKMLabel.Create(Panel_MPFindServer, 232, 50, 60, 20, gResTexts[TX_MP_MENU_FIND_SERVER_ROOM], fntOutline, taLeft);
      Edit_MP_FindRoom := TKMEdit.Create(Panel_MPFindServer, 232, 70, 60, 20, fntGrey);
      Edit_MP_FindRoom.AllowedChars := acDigits;
      Button_MP_FindServerIP := TKMButton.Create(Panel_MPFindServer, 20, 110, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_FIND], bsMenu);
      Button_MP_FindServerIP.OnClick := MP_FindServerIPClick;
      Button_MP_FindCancel := TKMButton.Create(Panel_MPFindServer, 20, 150, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_FindCancel.OnClick := MP_FindServerCancelClick;
  end;

  procedure PasswordPopUp;
  begin
    Panel_MPPassword := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPPassword.AnchorsCenter;
      TKMBevel.Create(Panel_MPPassword, -2000,  -2000, 5000, 5000);
      TKMImage.Create(Panel_MPPassword, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPPassword,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPPassword,  20, 10, 280, 20, gResTexts[TX_MP_MENU_PASSWORD_HEADER], fntOutline, taCenter);

      TKMLabel.Create(Panel_MPPassword, 20, 50, 156, 20, gResTexts[TX_MP_MENU_PASSWORD], fntOutline, taLeft);
      Edit_MP_Password := TKMEdit.Create(Panel_MPPassword, 20, 70, 152, 20, fntGrey);
      Edit_MP_Password.AllowedChars := acANSI7; //Passwords are basic ANSI so everyone can type them
      Button_MP_PasswordOk := TKMButton.Create(Panel_MPPassword, 20, 110, 280, 30, gResTexts[TX_MP_MENU_SERVER_JOIN], bsMenu);
      Button_MP_PasswordOk.OnClick := MP_PasswordClick;
      Button_MP_PasswordCancel := TKMButton.Create(Panel_MPPassword, 20, 150, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_PasswordCancel.OnClick := MP_PasswordClick;
  end;

var
  I: Integer;
begin
  inherited Create(gpMultiplayer);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := EscKeyDown;
  OnKeyDown := KeyDown;

  Panel_MultiPlayer := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MultiPlayer.AnchorsStretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, gResTexts[TX_MP_MENU_PLAYERNAME], fntOutline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 140, 20, fntGrey);
      Edit_MP_PlayerName.MaxLen := MAX_NICKNAME_LENGTH;
      Edit_MP_PlayerName.AllowedChars := acANSI7;
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, gResTexts[TX_MP_MENU_STATUS], fntOutline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fntGrey, taLeft);
      Label_MP_Status.WordWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, gResTexts[TX_MP_MENU_CREATE_SERVER], bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;


    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, gResTexts[TX_MP_MENU_FIND_SERVER], bsMenu);
    Button_MP_FindServer.OnClick := MP_FindServerClick;

    //Master server announcement
    Memo_MP_Announcement := TKMMemo.Create(Panel_MultiPlayer, 45, 45, 620, 189, fntGrey, bsMenu);
    Memo_MP_Announcement.Anchors := [anLeft, anTop];
    Memo_MP_Announcement.WordWrap := True;
    Memo_MP_Announcement.ItemHeight := 16;

    //List of available servers
    ColumnBox_Servers := TKMColumnBox.Create(Panel_MultiPlayer,45,240,620,465,fntMetal, bsMenu);
    ColumnBox_Servers.Anchors := [anLeft, anTop, anBottom];
    ColumnBox_Servers.Focusable := True;
    ColumnBox_Servers.ShowHintWhenShort := True;
    ColumnBox_Servers.HintBackColor := TKMColor4f.New(87, 72, 37);
    ColumnBox_Servers.SetColumns(fntOutline,
                                 ['','', gResTexts[TX_MP_MENU_SERVERLIST_NAME], gResTexts[TX_MP_MENU_SERVERLIST_STATE],
                                         gResTexts[TX_MP_MENU_SERVERLIST_PLAYERS], gResTexts[TX_MP_MENU_SERVERLIST_PING]],
                                 [0,20,40,350,480,565]);
    ColumnBox_Servers.OnColumnClick := MP_ServersSort;
    ColumnBox_Servers.OnChange := MP_ServersClick;
    ColumnBox_Servers.OnDoubleClick := MP_ServersDoubleClick;
    Label_Servers_Status := TKMLabel.Create(Panel_MultiPlayer, 45+310, 240+230, '', fntGrey, taCenter);
    Label_Servers_Status.Anchors := [anLeft];
    Label_Servers_Status.Hide;

    //Server details area
    //Bevel first, before ScrollPanel
    with TKMBevel.Create(Panel_MultiPlayer, 675, 240, SERVER_DETAILS_W, 465) do
      AnchorsStretch;

    Panel_MPServerDetails := TKMScrollPanel.Create(Panel_MultiPlayer, 675, 240, SERVER_DETAILS_W, 465, [saVertical], bsMenu, ssCommon);
    Panel_MPServerDetails.AnchorsStretch;
    Panel_MPServerDetails.Padding.SetBottom(5); //Small padding at the bottom

    Panel_MPServerDetails.ScrollV.OnChangeVisibility := ServerDetailsScrollChangedVisibility;

      Label_MP_ServerDetails_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 6, S_DETAILS_W_INT, 20, gResTexts[TX_MP_MENU_HEADER_SERVER_DETAILS], fntOutline, taCenter);
      Label_MP_GameInfo_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 30, S_DETAILS_W_INT, 20, gResTexts[TX_MP_MENU_GAME_INFORMATION], fntOutline, taLeft);
      Label_MP_Desc := TKMLabel.Create(Panel_MPServerDetails, 8, 50, S_DETAILS_W_INT, 40, '', fntMetal, taLeft);
      Label_MP_Desc.MaxLines := 2;
      Label_MP_Desc.WordWrap := True;
      Label_MP_PT_Times := TKMLabel.Create(Panel_MPServerDetails, 8, 90, S_DETAILS_W_INT, 20, '', fntMetal, taLeft);
      Label_MP_PT_Times.FontColor := clMPSrvDetailsGameInfoFont;
      Label_MP_GameTime := TKMLabel.Create(Panel_MPServerDetails, 8, 90, S_DETAILS_W_INT, 20, '', fntMetal, taRight);
      Label_MP_GameTime.FontColor := clMPSrvDetailsGameInfoFont;
      Label_MP_Map_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 110, S_DETAILS_W_INT, 20, gResTexts[TX_WORD_MAP] + ':', fntOutline, taLeft);
      Label_MP_MapName := TKMLabel.Create(Panel_MPServerDetails, 8, 130, S_DETAILS_W_INT, 20, '', fntMetal, taLeft);
      Label_MP_PlayerList_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 150, S_DETAILS_W_INT, 20, gResTexts[TX_MP_MENU_PLAYER_LIST], fntOutline, taLeft);

      Label_MP_Team_Header := TKMLabel.Create(Panel_MPServerDetails, 8 + 22 + 176, 150, 150, 20, 'Team', fntOutline, taLeft);
      Label_MP_Team_Header.Hide;

      Image_MP_Host := TKMImage.Create(Panel_MPServerDetails, IMG_COL2, 148, 14, 15, 77, rxGuiMain);
      Image_MP_Host.Visible := False;
      for I := 1 to MAX_LOBBY_SLOTS do
      begin
        Label_MP_PlayersNames[I] := TKMLabel.Create(Panel_MPServerDetails, 8 + 22, 170 + 20*(I-1), 170, 20, '', fntMetal, taLeft);
        Label_MP_PlayersNames[I].Anchors := [anLeft, anTop, anBottom];
        Label_MP_PlayersTeams[I] := TKMLabel.Create(Panel_MPServerDetails, 8 + 22 + 186, 170 + 20*(I-1), 20, 20, '', fntMetal, taLeft);
        Image_MP_PlayerIcons[I] := TKMImage.Create(Panel_MPServerDetails, 8, 170 + 20*(I-1), 16, 11, 0, rxGuiMain);
        Image_MP_PlayerWolIcons[I] := TKMImage.Create(Panel_MPServerDetails, IMG_COL2, 169 + 20*(I-1), 16, 16, 0, rxGuiMain);
        Image_MP_PlayerWolIcons[I].Hide;
        Image_MP_PlayerSpecIcons[I] := TKMImage.Create(Panel_MPServerDetails, 8 + 22 + 180, 171 + 20*(I-1), 16, 11, 0, rxGuiMain);
        Image_MP_PlayerSpecIcons[I].Hide;
      end;

    Button_MP_Back    := TKMButton.Create(Panel_MultiPlayer,  45, 720, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer, 275, 720, 390, 30,gResTexts[TX_MP_MENU_REFRESH_SERVER_LIST], bsMenu);
    Button_MP_GetIn   := TKMButton.Create(Panel_MultiPlayer, 675, 720, 320, 30,gResTexts[TX_MP_MENU_SERVER_JOIN],  bsMenu);
    Button_MP_Back.Anchors    := [anLeft, anBottom];
    Button_MP_Refresh.Anchors := [anLeft, anBottom];
    Button_MP_GetIn.Anchors   := [anLeft, anBottom];
    Button_MP_Back.OnClick    := BackClick;
    Button_MP_Refresh.OnClick := MP_ServersRefresh;
    Button_MP_GetIn.OnClick   := MP_GetInClick;

  CreateServerPopUp;
  FindServerPopUp;
  PasswordPopUp;
end;


procedure TKMMenuMultiplayer.KeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:  if Panel_MPPassword.Visible then
                  MP_PasswordClick(Button_MP_PasswordOk)
                else if Panel_MPFindServer.Visible then
                  MP_FindServerIPClick(Button_MP_FindServerIP);
    // Refresh server list on F5
    VK_F5:      if not Panel_MPPassword.Visible
                  and not Panel_MPCreateServer.Visible
                  and not Panel_MPFindServer.Visible
                  and Button_MP_Refresh.IsClickable then
                  MP_ServersRefresh(Button_MP_Refresh);
  end;
end;


procedure TKMMenuMultiplayer.UpdateServerDetailsUI;
var
  I: Integer;
begin
  if not fServerSelected or (fSelectedRoomInfo.GameInfo.PlayerCount = 0) then Exit;

  //Set all visible
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    Image_MP_PlayerIcons[I].Visible := True;
    Label_MP_PlayersTeams[I].Visible := True;

    Image_MP_PlayerSpecIcons[I].Visible := True;
    Image_MP_PlayerWolIcons[I].Visible := True;
  end;
  Label_MP_Team_Header.Visible := True;
  Image_MP_Host.Visible := True;
end;


procedure TKMMenuMultiplayer.Resize(X, Y: Word);
begin
  UpdateServerDetailsUI;
end;


procedure TKMMenuMultiplayer.MP_Init;
begin
  fServerSelected := False;

  //Refresh the list when they first open the multiplayer page
  MP_ServersRefresh(nil);

  Edit_MP_PlayerName.Text := UnicodeString(gGameSettings.MultiplayerName);

  Edit_MP_ServerName.Text := UnicodeString(gServerSettings.ServerName);
  Edit_MP_ServerPort.Text := gServerSettings.ServerPort;

  Edit_MP_FindIP.Text := gGameSettings.LastIP;
  Edit_MP_FindPort.Text := gGameSettings.LastPort;
  Edit_MP_FindRoom.Text := gGameSettings.LastRoom;

  Button_MP_GetIn.Disable;

  //Fetch the announcements display
  gNetworking.ServerQuery.OnAnnouncements := MP_AnnouncementsUpdated;
  gNetworking.ServerQuery.FetchAnnouncements(gResLocales.UserLocale);
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(gResTexts[TX_MP_MENU_LOADING_ANNOUNCEMENTS]);
end;


procedure TKMMenuMultiplayer.MP_CreateServerCancelClick(Sender: TObject);
begin
  Panel_MPCreateServer.Hide;
end;


procedure TKMMenuMultiplayer.MP_CreateServerClick(Sender: TObject);
begin
  Panel_MPCreateServer.Show;
end;


procedure TKMMenuMultiplayer.MP_FindServerCancelClick(Sender: TObject);
begin
  Panel_MPFindServer.Hide;
end;


procedure TKMMenuMultiplayer.MP_FindServerClick(Sender: TObject);
begin
  Panel_MPFindServer.Show;
end;


procedure TKMMenuMultiplayer.MP_FindServerIPClick(Sender: TObject);
var
  serverPortStr: string;
  serverPort: Word;
begin
  serverPortStr := Trim(Edit_MP_FindPort.Text);
  serverPort    := StrToInt(serverPortStr);
  MP_Join(Edit_MP_FindIP.Text, serverPort, StrToIntDef(Edit_MP_FindRoom.Text, -1));
end;


procedure TKMMenuMultiplayer.MP_PasswordClick(Sender: TObject);
begin
  if Sender = Button_MP_PasswordOk then
  begin
    Panel_MPPassword.Hide;
    gNetworking.SendPassword(AnsiString(Edit_MP_Password.Text));
  end;
  if Sender = Button_MP_PasswordCancel then
  begin
    gNetworking.Disconnect;
    Panel_MPPassword.Hide;
    MP_UpdateStatus(gResTexts[TX_MP_MENU_STATUS_READY], icGreen, False);
  end;
end;


//Save the Player and IP name so it is not lost inbetween activities
procedure TKMMenuMultiplayer.MP_SaveSettings;
begin
  //Player name
  gGameSettings.MultiplayerName := AnsiString(Edit_MP_PlayerName.Text);

  //Create Server popup
  gServerSettings.ServerName := AnsiString(Edit_MP_ServerName.Text);
  gServerSettings.ServerPort := Edit_MP_ServerPort.Text;

  //Join server popup
  gGameSettings.LastPort := Edit_MP_FindPort.Text;
  gGameSettings.LastRoom := Edit_MP_FindRoom.Text;
  gGameSettings.LastIP   := Edit_MP_FindIP.Text;
end;


// Update status line
// When user tries to Join some server disable joining controls for that time
procedure TKMMenuMultiplayer.MP_UpdateStatus(const aStatus: string; aColor: TColor4; aBusy: Boolean);
begin
  fLobbyBusy := aBusy;

  //Toggle server creation
  Button_MP_CreateServer.Enabled := not aBusy;
  Button_MP_CreateLAN.Enabled := not aBusy;
  Button_MP_CreateWAN.Enabled := not aBusy;

  //Toggle server joining
  Button_MP_FindServer.Enabled := not aBusy;
  Button_MP_FindServerIP.Enabled := not aBusy;
  Button_MP_FindCancel.Enabled := not aBusy;
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  Label_MP_Status.Caption := aStatus;
  Label_MP_Status.FontColor := aColor;
end;


procedure TKMMenuMultiplayer.MP_ClearServerDetailsPanel;
var
  I: Integer;
begin
  Label_MP_ServerDetails_Header.Visible := False;
  Label_MP_GameInfo_Header.Visible := False;
  Label_MP_Map_Header.Visible := False;
  Label_MP_PlayerList_Header.Visible := False;
  Label_MP_Team_Header.Visible := False;
  Image_MP_Host.Visible := False;
  Label_MP_Desc.Caption := '';
  Label_MP_PT_Times.Caption := '';
  Label_MP_GameTime.Caption := '';
  Label_MP_MapName.Caption := '';
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    Label_MP_PlayersNames[I].Caption := '';
    Label_MP_PlayersNames[I].Strikethrough := False;
    Label_MP_PlayersTeams[I].Caption := '';
    Label_MP_PlayersTeams[I].Strikethrough := False;
    Label_MP_PlayersTeams[I].Hide;
    Image_MP_PlayerSpecIcons[I].Hide;
    Image_MP_PlayerWolIcons[I].Hide;
    Image_MP_PlayerIcons[I].TexID := 0;
    Image_MP_PlayerIcons[I].Lightness := 0;
  end;
end;


procedure TKMMenuMultiplayer.MP_ServersRefresh(Sender: TObject);
begin
  gNetworking.ServerQuery.OnListUpdated := MP_ServersUpdateList;
  gNetworking.ServerQuery.RefreshList;
  ColumnBox_Servers.Clear;
  MP_ClearServerDetailsPanel;

  //Do not use 'Show' here as it will also make the parent panel visible
  //which could be already hidden if player switched pages
  Label_Servers_Status.Caption := gResTexts[TX_MP_MENU_REFRESHING];
  Label_Servers_Status.Visible := True;
  Button_MP_GetIn.Disable;
end;


//Refresh the display for the list of servers
procedure TKMMenuMultiplayer.MP_ServersUpdateList(Sender: TObject);
const
  GAME_STATE_TEXT_IDS: array [TMPGameState] of Integer = (TX_MP_STATE_NONE, TX_MP_STATE_LOBBY, TX_MP_STATE_LOADING, TX_MP_STATE_GAME);
var
  I, prevTop: Integer;
  displayName: string;
  S: TKMServerInfo;
  R: TKMRoomInfo;
begin
  prevTop := ColumnBox_Servers.TopIndex;
  ColumnBox_Servers.Clear;

  if gNetworking.ServerQuery.Rooms.Count = 0 then
  begin
    //Do not use 'Show' here as it will also make the parent panel visible
    //which could be already hidden if player switched pages
    Label_Servers_Status.Caption := gResTexts[TX_MP_MENU_NO_SERVERS];
    Label_Servers_Status.Visible := True;
  end
  else
  begin
    Label_Servers_Status.Hide;
    for I := 0 to gNetworking.ServerQuery.Rooms.Count - 1 do
    begin
      R := gNetworking.ServerQuery.Rooms[I];

      //Check room game revision
      if (R.GameRevision <> EMPTY_ROOM_DEFAULT_GAME_REVISION)
        and (R.GameRevision <> GAME_REVISION_NUM) then //Room game revision differs from ours, skip it
        Continue;

      S := gNetworking.ServerQuery.Servers[R.ServerIndex];

      //Only show # if Server has more than 1 Room
      displayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));
      ColumnBox_Servers.AddItem(
      MakeListRow(['', '', displayName, gResTexts[GAME_STATE_TEXT_IDS[R.GameInfo.GameState]], IntToStr(R.GameInfo.ConnectedPlayerCount), IntToStr(S.Ping)],
                  [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, GetPingColor(S.Ping)],
                  [MakePic(rxGuiMain, ServerTypePic[S.ServerType]), MakePic(rxGuiMain, IfThen(R.GameInfo.PasswordLocked, 73, 0)), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0)],
                  I));

      //if server was selected, we need to select it again, because TKMColumnListBox was cleared
      if fServerSelected
        and (R.RoomID = fSelectedRoomInfo.RoomID)
        and (S.IP = fSelectedServerInfo.IP)
        and (S.Port = fSelectedServerInfo.Port) then
      begin
        ColumnBox_Servers.ItemIndex := I;
        MP_ServersClick(nil); //Shows info about this selected server
      end;
    end;

    ColumnBox_Servers.TopIndex := prevTop;
    if (ColumnBox_Servers.ItemIndex <> -1)
    and not InRange(ColumnBox_Servers.ItemIndex - ColumnBox_Servers.TopIndex, 0, ColumnBox_Servers.GetVisibleRows - 1) then
    begin
      if ColumnBox_Servers.ItemIndex < ColumnBox_Servers.TopIndex + ColumnBox_Servers.GetVisibleRows - 1 then
        ColumnBox_Servers.TopIndex := ColumnBox_Servers.ItemIndex
      else
      if ColumnBox_Servers.ItemIndex > ColumnBox_Servers.TopIndex + ColumnBox_Servers.GetVisibleRows - 1 then
        ColumnBox_Servers.TopIndex := ColumnBox_Servers.ItemIndex - ColumnBox_Servers.GetVisibleRows + 1;
    end;
  end;
end;


procedure TKMMenuMultiplayer.MP_AnnouncementsUpdated(const S: UnicodeString);
begin
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(S);
end;


//Sort the servers list by said column ID
procedure TKMMenuMultiplayer.MP_ServersSort(aIndex: Integer);
begin
  case ColumnBox_Servers.SortIndex of
    0:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByTypeAsc
        else
          gNetworking.ServerQuery.SortMethod := ssmByTypeDesc;
    1:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByPasswordAsc
        else
          gNetworking.ServerQuery.SortMethod := ssmByPasswordDesc;
    //Sorting by name goes A..Z by default
    2:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByNameAsc
        else
          gNetworking.ServerQuery.SortMethod := ssmByNameDesc;
    //Sorting by state goes Lobby,Loading,Game,None by default
    3:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByStateAsc
        else
          gNetworking.ServerQuery.SortMethod := ssmByStateDesc;
    //Sorting by player count goes 8..0 by default
    4:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByPlayersDesc
        else
          gNetworking.ServerQuery.SortMethod := ssmByPlayersAsc;
    //Sorting by ping goes 0 ... 1000 by default
    5:  if ColumnBox_Servers.SortDirection = sdDown then
          gNetworking.ServerQuery.SortMethod := ssmByPingAsc
        else
          gNetworking.ServerQuery.SortMethod := ssmByPingDesc;
  end;

  //Refresh the display only if there are rooms to be sorted (otherwise it shows "no servers found" immediately)
  if gNetworking.ServerQuery.Rooms.Count > 0 then
    MP_ServersUpdateList(nil);
end;


procedure TKMMenuMultiplayer.MP_ServersClick(Sender: TObject);
var
  sortedNetPlayersIndexes: array [1..MAX_LOBBY_SLOTS] of Integer;

  function GetTeamStr(aTeam: Integer; aIsSpectator: Boolean): String;
  begin
    if aIsSpectator then
      Result := ''
    else if aTeam = 0 then
      Result := '-'
    else
      Result := IntToStr(aTeam);
  end;

  procedure SortPlayersByTeam;
  var
    I, K, T: Integer;
  begin
    // First empty everything
    for I := 1 to MAX_LOBBY_SLOTS do
      sortedNetPlayersIndexes[I] := -1;

    K := 1;
    // Players, sorted by team
    for T := 0 to MAX_TEAMS do
      for I := 1 to fSelectedRoomInfo.GameInfo.PlayerCount do
        if not fSelectedRoomInfo.GameInfo.Players[I].IsSpectator and (fSelectedRoomInfo.GameInfo.Players[I].Team = T) then
        begin
          sortedNetPlayersIndexes[K] := I;
          Inc(K);
        end;

    // Spectators
    for I := 1 to fSelectedRoomInfo.GameInfo.PlayerCount do
      if fSelectedRoomInfo.GameInfo.Players[I].IsSpectator then
      begin
        sortedNetPlayersIndexes[K] := I;
        Inc(K);
      end;
  end;

var
  K, I, ID, localeID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  if (ID = -1) or (ColumnBox_Servers.Rows[ID].Tag = -1) then
  begin
    fServerSelected := False;
    Button_MP_GetIn.Disable;
    MP_ClearServerDetailsPanel;
    Exit;
  end;

  fServerSelected := True;
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  fSelectedRoomInfo := gNetworking.ServerQuery.Rooms[ColumnBox_Servers.Rows[ID].Tag];
  fSelectedServerInfo := gNetworking.ServerQuery.Servers[fSelectedRoomInfo.ServerIndex];

  if fSelectedRoomInfo.GameInfo.PlayerCount = 0 then
  begin
    MP_ClearServerDetailsPanel;
    Exit;
  end;

  Label_MP_ServerDetails_Header.Visible := True;
  Label_MP_GameInfo_Header.Visible := True;
  Label_MP_Map_Header.Visible := True;
  Label_MP_PlayerList_Header.Visible := True;

  Label_MP_Desc.Caption := fSelectedRoomInfo.GameInfo.Description;

  //Game options (Peacetime duration, speed before and after PT)
  Label_MP_PT_Times.Caption := IntToStr(fSelectedRoomInfo.GameInfo.GameOptions.Peacetime) + 'pt' +
                               ' x' + FormatFloat('#.#', fSelectedRoomInfo.GameInfo.GameOptions.SpeedPT) +
                               ' x' + FormatFloat('#.#', fSelectedRoomInfo.GameInfo.GameOptions.SpeedAfterPT);
  Label_MP_GameTime.Caption := fSelectedRoomInfo.GameInfo.GetFormattedTime;
  Label_MP_MapName.Caption := fSelectedRoomInfo.GameInfo.Map;

  if fSelectedRoomInfo.GameInfo.GameOptions.MissionDifficulty <> mdNone then
    Label_MP_MapName.Caption := Format('%s [$%s]( %s )[]', [Label_MP_MapName.Caption,
                                    IntToHex(DIFFICULTY_LEVELS_COLOR[fSelectedRoomInfo.GameInfo.GameOptions.MissionDifficulty] and $00FFFFFF, 6),
                                    gResTexts[DIFFICULTY_LEVELS_TX[fSelectedRoomInfo.GameInfo.GameOptions.MissionDifficulty]]]);

  UpdateGameTimeLabel;

  SortPlayersByTeam;

  for I := 1 to MAX_LOBBY_SLOTS do
    if I <= fSelectedRoomInfo.GameInfo.PlayerCount then
    begin
      K := sortedNetPlayersIndexes[I];
      if K = -1 then raise Exception.Create('Unexpected sorted value'); ;

      case fSelectedRoomInfo.GameInfo.Players[K].WonOrLost of
        wolNone: Image_MP_PlayerWolIcons[I].TexId := 0;
        wolWon:  Image_MP_PlayerWolIcons[I].TexId := 8;  //Red medal
        wolLost: Image_MP_PlayerWolIcons[I].TexId := 87; //Death skull
      end;

      case fSelectedRoomInfo.GameInfo.Players[K].PlayerType of
        nptHuman:     begin
                        Label_MP_PlayersNames[I].Caption := UnicodeString(fSelectedRoomInfo.GameInfo.Players[K].Name);
                        Label_MP_PlayersTeams[I].Caption := GetTeamStr(fSelectedRoomInfo.GameInfo.Players[K].Team, fSelectedRoomInfo.GameInfo.Players[K].IsSpectator);

                        Image_MP_PlayerSpecIcons[I].TexId := IfThen(fSelectedRoomInfo.GameInfo.Players[K].IsSpectator, 86, 0); //spectator eye icon

                        if fSelectedRoomInfo.GameInfo.Players[K].IsHost then
                        begin
                          Image_MP_Host.Top := Label_MP_PlayersNames[1].Top + 20*(I-1) - 1;
                          if fSelectedRoomInfo.GameInfo.Players[K].WonOrLost = wolNone then
                            Image_MP_Host.Left := IMG_COL2
                          else
                            Image_MP_Host.Left := IMG_COL2 + 25; //Move Host star to the right when we have Win/Loss icon
                        end;

                        localeID := gResLocales.IndexByCode(fSelectedRoomInfo.GameInfo.Players[K].LangCode);
                        if localeID <> -1 then
                          Image_MP_PlayerIcons[I].TexID := gResLocales[localeID].FlagSpriteID
                        else
                          Image_MP_PlayerIcons[I].TexID := 0;
                      end;
        nptComputerClassic:
                      begin
                        Label_MP_PlayersNames[I].Caption := gResTexts[TX_AI_PLAYER_CLASSIC_SHORT];
                        Label_MP_PlayersTeams[I].Caption := GetTeamStr(fSelectedRoomInfo.GameInfo.Players[K].Team, fSelectedRoomInfo.GameInfo.Players[K].IsSpectator);
                        Image_MP_PlayerSpecIcons[I].TexId := 0;
                        Image_MP_PlayerIcons[I].TexID := GetAIPlayerIcon(nptComputerClassic);
                      end;
        nptComputerAdvanced:
                      begin
                        Label_MP_PlayersNames[I].Caption := gResTexts[TX_AI_PLAYER_ADVANCED_SHORT];
                        Label_MP_PlayersTeams[I].Caption := GetTeamStr(fSelectedRoomInfo.GameInfo.Players[K].Team, fSelectedRoomInfo.GameInfo.Players[K].IsSpectator);
                        Image_MP_PlayerSpecIcons[I].TexId := 0;
                        Image_MP_PlayerIcons[I].TexID := GetAIPlayerIcon(nptComputerAdvanced);
                      end;
        nptClosed:    begin
                        Label_MP_PlayersNames[I].Caption := gResTexts[TX_LOBBY_SLOT_CLOSED];
                        Label_MP_PlayersTeams[I].Caption := '';
                        Image_MP_PlayerSpecIcons[I].TexId := 0;
                        Image_MP_PlayerIcons[I].TexID := 0;
                      end;
      end;
      Label_MP_PlayersNames[I].FontColor := FlagColorToTextColor(fSelectedRoomInfo.GameInfo.Players[K].Color);
      Label_MP_PlayersNames[I].Strikethrough := not fSelectedRoomInfo.GameInfo.Players[K].Connected;
      Image_MP_PlayerIcons[I].Lightness := IfThen(fSelectedRoomInfo.GameInfo.Players[K].Connected, 0, -0.66);
    end
    else
    begin
      Label_MP_PlayersNames[I].Caption := '';
      Label_MP_PlayersNames[I].Strikethrough := False;
      Label_MP_PlayersTeams[I].Caption := '';
      Label_MP_PlayersTeams[I].Strikethrough := False;
      Image_MP_PlayerSpecIcons[I].TexId := 0;
      Image_MP_PlayerWolIcons[I].TexId := 0;
      Image_MP_PlayerIcons[I].TexId := 0;
      Image_MP_PlayerIcons[I].Lightness := 0;
    end;
    UpdateServerDetailsUI;
end;


procedure TKMMenuMultiplayer.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColumnBox_Servers.ItemIndex <> -1)
  and InRange(ColumnBox_Servers.Rows[ColumnBox_Servers.ItemIndex].Tag, 0, gNetworking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMenuMultiplayer.StartLobby(aIsHost: Boolean);
begin
  gChat.Clear;
  if aIsHost then
    fOnPageChange(gpLobby, 'HOST')
  else
    fOnPageChange(gpLobby, 'JOIN');
end;


procedure TKMMenuMultiplayer.MP_HostClick(Sender: TObject);
var
  serverPortStr: string;
  serverPort: Word;
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;
  serverPortStr := Trim(Edit_MP_ServerPort.Text);
  serverPort    := StrToInt(serverPortStr);

  //Hide the panel so if it fails the error message will be easy to see (e.g. name too long)
  Panel_MPCreateServer.Hide;

  if not ValidatePlayerName(Edit_MP_PlayerName.Text) then
    Exit;

  StartLobby(True);

  gNetworking.OnHostFail := MP_HostFail;
  gNetworking.Host(AnsiString(Edit_MP_ServerName.Text), serverPort,
                   AnsiString(Edit_MP_PlayerName.Text), (Sender = Button_MP_CreateWAN),
                   gServerSettings.ServerUDPAnnounce);
end;


procedure TKMMenuMultiplayer.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


//Make sure that the nickname as a whole is valid (checks that TKMEdit can not always perform)
function TKMMenuMultiplayer.ValidatePlayerName(const aName: UnicodeString): Boolean;

  function IsReserved(const aName: String): Boolean; inline;
  var
    I: Integer;
    Str: String;
  begin
    Result := False;
    Str := Trim(aName);
    for I := Low(LOBBY_PLAYER_NAMES_TEXT_ID_RESERVED) to High(LOBBY_PLAYER_NAMES_TEXT_ID_RESERVED) do
      if Str = gResTexts[LOBBY_PLAYER_NAMES_TEXT_ID_RESERVED[I]] then
        Exit(True);
  end;

var
  I: Integer;
  err: UnicodeString;
begin
  err := '';

  if (aName = '') or (aName <> Trim(aName)) then
    err := gResTexts[TX_GAME_ERROR_BLANK_PLAYERNAME]
  else
  if IsReserved(aName) then
    err := Format(gResTexts[TX_GAME_ERROR_RESERVER_PLAYERNAME], [aName])
  else
  if Length(aName) > MAX_NICKNAME_LENGTH then
    err := Format(gResTexts[TX_GAME_ERROR_LONG_PLAYERNAME], [MAX_NICKNAME_LENGTH])
  else
  if (Pos('|', aName) <> 0) or (Pos('[$', aName) <> 0) or (Pos('[]', aName) <> 0) or (Pos('<$', aName) <> 0) then
    err := gResTexts[TX_GAME_ERROR_ILLEGAL_PLAYERNAME]
  else
  for I := 1 to Length(aName) do
    if not InRange(Ord(aName[I]), 32, 126) then
      err := gResTexts[TX_GAME_ERROR_ILLEGAL_PLAYERNAME];

  Result := (err = '');

  if not Result then
  begin
    MP_UpdateStatus(err, icYellow, False);
    gSoundPlayer.Play(sfxnError);
  end
end;


procedure TKMMenuMultiplayer.UpdateGameTimeLabel;
begin
  Label_MP_GameTime.Width := S_DETAILS_W_INT - 20*Byte(Panel_MPServerDetails.ScrollV.Visible);
end;


procedure TKMMenuMultiplayer.ServerDetailsScrollChangedVisibility(Sender: TObject; aVisible: Boolean);
begin
  //Don't use aVisible, since we call this method directly too
  UpdateGameTimeLabel;
end;


//Join button is enabled if valid server is selected and the lobby is not busy
function TKMMenuMultiplayer.MP_GetInEnabled: Boolean;
var
  ID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  Result := (not fLobbyBusy) and (ID <> -1) and (ColumnBox_Servers.Rows[ID].Tag <> -1);
end;


procedure TKMMenuMultiplayer.MP_Join(const aServerAddress: string; aPort: Word; aRoom: Integer);
begin
  //Save the player and IP name so it is not lost if the connection fails
  MP_SaveSettings;

  if not ValidatePlayerName(Edit_MP_PlayerName.Text) then
    Exit;

  //Disable buttons to prevent multiple clicks while connection process is in progress
  MP_UpdateStatus(gResTexts[TX_MP_MENU_STATUS_CONNECTING], icGreen, True);

  //Send request to join
  gNetworking.OnJoinSucc := MP_JoinSuccess;
  gNetworking.OnJoinFail := MP_JoinFail;
  gNetworking.OnJoinPassword := MP_JoinPassword;
  gNetworking.OnJoinAssignedHost := MP_JoinAssignedHost;
  gNetworking.Join(aServerAddress, aPort, AnsiString(Edit_MP_PlayerName.Text), aRoom); //Init lobby
end;


procedure TKMMenuMultiplayer.MP_JoinPassword;
begin
  Panel_MPFindServer.Hide;
  Edit_MP_Password.Text := '';
  Panel_MPPassword.Show;
end;


// We had recieved permission to join
procedure TKMMenuMultiplayer.MP_JoinSuccess;
begin
  gNetworking.OnJoinSucc := nil;
  gNetworking.OnJoinFail := nil;
  gNetworking.OnJoinAssignedHost := nil;

  StartLobby(False);
end;


procedure TKMMenuMultiplayer.MP_JoinFail(const aData: UnicodeString);
begin
  gNetworking.Disconnect;
  MP_UpdateStatus(Format(gResTexts[TX_GAME_ERROR_CONNECTION_FAILED], [aData]), icYellow, False);
  gSoundPlayer.Play(sfxnError);
end;


procedure TKMMenuMultiplayer.MP_JoinAssignedHost;
begin
  gNetworking.OnJoinSucc := nil;
  gNetworking.OnJoinFail := nil;
  gNetworking.OnJoinAssignedHost := nil;
  gNetworking.OnHostFail := MP_HostFail;

  // We were joining a game and the server assigned hosting rights to us
  StartLobby(True); // Open lobby page in host mode
end;


procedure TKMMenuMultiplayer.EscKeyDown(Sender: TObject);
begin
  if Button_MP_CreateServerCancel.IsClickable then
    MP_CreateServerCancelClick(nil)
  else if Button_MP_FindCancel.IsClickable then
    MP_FindServerCancelClick(nil)
  else if Button_MP_PasswordCancel.IsClickable then
    MP_PasswordClick(Button_MP_PasswordCancel)
  else begin
    BackClick(nil);
  end;
end;


procedure TKMMenuMultiplayer.BackClick(Sender: TObject);
begin
  gNetworking.Disconnect;
  MP_SaveSettings;

  gMain.UnlockMutex; //Leaving MP areas

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuMultiplayer.MP_HostFail(const aData: UnicodeString);
begin
  gNetworking.Disconnect;
  gSoundPlayer.Play(sfxnError);

  fOnPageChange(gpMultiplayer, aData);
end;


procedure TKMMenuMultiplayer.Show(const aText: UnicodeString);
begin
  if Assigned(OnNetworkInit) then
    OnNetworkInit;

  MP_Init;

  if aText = '' then
    //Entering MP anew
    MP_UpdateStatus(gResTexts[TX_MP_MENU_STATUS_READY],icGreen,False)
  else
    //We are in event handler of Lobby.BackClick (show status warning)
    MP_UpdateStatus(aText, icYellow, False);

  UpdateGameTimeLabel;

  Panel_MultiPlayer.Show;
end;


end.
