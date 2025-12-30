unit KM_GUIMapEdPlayerView;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsTrackBar,
   KM_ControlsEdit, KM_ControlsPopUp, KM_ControlsList, KM_ControlsDrop,
   KM_Defaults, KM_ControlsWaresRow, KM_ControlsScroll,
   KM_Points,
   KM_GUIMapEdPlayerCheck;

type
  TKMMapEdPlayerView = class
  private
    procedure Player_ViewClick(Sender: TObject);
  protected
    Panel_PlayerView: TKMPanel;
    Button_Reveal: TKMButtonFlat;
    TrackBar_RevealNewSize: TKMTrackBar;
    CheckBox_RevealAll: TKMCheckBox;
    Button_CenterScreen: TKMButtonFlat;
    Button_PlayerCenterScreen: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
    procedure UpdateState;
    procedure UpdatePlayerColor;
  end;

  TKMMapEdPlayerAdditional = class
  private
    procedure RefreshList;
    procedure Player_WorklessChange(Sender: TObject);
    procedure Player_WaresChange(Sender: TObject; X : Integer);
    procedure Player_ButtonClick(Sender : TOBject);
    procedure Player_PopUpClick(Sender : TOBject);
    procedure Player_MessageChange(Sender : TOBject);
    procedure RefreshMessage;
  protected
    Panel_PlayerAdd: TKMScrollPanel;
    Number_Workless: TKMNumericEdit;

    Button_ShowMessagePopUp : TKMButton;
    Button_CheckPlayerShow : TKMButton;
    Panel_PlayerCheck : TKMMapEdPlayerCheck;

    PopUp_MessageEditor: TKMPopUpPanel;
      ColumnBox_MessageQueue: TKMColumnBox;
      Button_MessageAdd: TKMButton;
      Button_MessageDel: TKMButton;
      Number_Time,
      Number_TextID : TKMNumericEdit;
      Image_MessageKind : TKMButtonFlat;
      DropList_MessageKind : TKMDropList;
      Button_SaveToLibx,
      Button_ClosePopUp : TKMButton;
      Edit_MessageText : TKMEdit;
      Button_AddToLibx: TKMButton;
      Bevel_MessageText : TKMBevel;
      Label_MessageText : TKMLabel;

      CheckBox_NeverHungry,
      CheckBox_DisableUpdateEntities : TKMCheckBox;
      VirtualWares_Row: array of TKMWareOrderRow;

  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_Game, KM_GameTypes,
  KM_ControlsTypes,
  KM_Cursor, KM_RenderUI, KM_ResFonts, KM_Resource, KM_ResTypes,
  KM_InterfaceGame, SysUtils, Math,
  KM_Campaigns, KM_ResLocales;


{ TKMMapEdPlayerAdd }
constructor TKMMapEdPlayerView.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_PlayerView := TKMPanel.Create(aParent, 0, 28 + 30, aParent.Width, 400);
  with TKMLabel.Create(Panel_PlayerView, 0, PAGE_TITLE_Y, Panel_PlayerView.Width, 0, gResTexts[TX_MAPED_FOG], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  Button_Reveal         := TKMButtonFlat.Create(Panel_PlayerView, 9, 30, 33, 33, 394);
  Button_Reveal.Hint    := gResTexts[TX_MAPED_FOG_HINT];
  Button_Reveal.OnClick := Player_ViewClick;
  TrackBar_RevealNewSize  := TKMTrackBar.Create(Panel_PlayerView, 46, 35, Panel_PlayerView.Width - 46, 1, 64);
  TrackBar_RevealNewSize.Anchors := [anLeft, anTop, anRight];
  TrackBar_RevealNewSize.OnChange := Player_ViewClick;
  TrackBar_RevealNewSize.Position := 8;
  CheckBox_RevealAll          := TKMCheckBox.Create(Panel_PlayerView, 9, 75, 140, 20, gResTexts[TX_MAPED_FOG_ALL], fntMetal);
  CheckBox_RevealAll.OnClick  := Player_ViewClick;
  with TKMLabel.Create(Panel_PlayerView, 0, 100, Panel_PlayerView.Width, 0, gResTexts[TX_MAPED_FOG_CENTER], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  Button_CenterScreen         := TKMButtonFlat.Create(Panel_PlayerView, 9, 120, 33, 33, 391);
  Button_CenterScreen.Hint    := gResTexts[TX_MAPED_FOG_CENTER_HINT];
  Button_CenterScreen.OnClick := Player_ViewClick;
  Button_PlayerCenterScreen    := TKMButton.Create(Panel_PlayerView, 49, 120, 80, 33, '[X,Y]', bsGame);
  Button_PlayerCenterScreen.OnClick := Player_ViewClick;
  Button_PlayerCenterScreen.Hint := gResTexts[TX_MAPED_FOG_CENTER_JUMP];
end;


procedure TKMMapEdPlayerView.Player_ViewClick(Sender: TObject);
begin
  //Press the button
  if Sender = Button_Reveal then
  begin
    Button_Reveal.Down := not Button_Reveal.Down;
    Button_CenterScreen.Down := False;
  end;
  if Sender = Button_CenterScreen then
  begin
    Button_CenterScreen.Down := not Button_CenterScreen.Down;
    Button_Reveal.Down := False;
  end;

  if (Sender = nil) and (gCursor.Mode = cmNone) then
  begin
    Button_Reveal.Down := False;
    Button_CenterScreen.Down := False;
  end;

  if Button_Reveal.Down then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_REVEAL;
    gCursor.MapEdSize := TrackBar_RevealNewSize.Position;
  end
  else
  if Button_CenterScreen.Down then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_CENTERSCREEN;
  end
  else
    gCursor.Mode := cmNone;

  if Sender = CheckBox_RevealAll then
    gGame.MapEditor.RevealAll[gMySpectator.HandID] := CheckBox_RevealAll.Checked
  else
    CheckBox_RevealAll.Checked := gGame.MapEditor.RevealAll[gMySpectator.HandID];

  if Sender = Button_PlayerCenterScreen then
    gGame.ActiveInterface.Viewport.Position := KMPointF(gMySpectator.Hand.CenterScreen); //Jump to location

  Button_PlayerCenterScreen.Caption := TypeToString(gMySpectator.Hand.CenterScreen);
end;


procedure TKMMapEdPlayerView.UpdateState;
begin
  Button_CenterScreen.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_CENTERSCREEN);
  Button_Reveal.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_REVEAL);
end;


procedure TKMMapEdPlayerView.Hide;
begin
  Panel_PlayerView.Hide;
end;


procedure TKMMapEdPlayerView.Show;
begin
  Panel_PlayerView.Show;
  Button_PlayerCenterScreen.Caption := TypeToString(gMySpectator.Hand.CenterScreen);
  CheckBox_RevealAll.Checked := gGame.MapEditor.RevealAll[gMySpectator.HandID];
end;


function TKMMapEdPlayerView.Visible: Boolean;
begin
  Result := Panel_PlayerView.Visible;
end;


procedure TKMMapEdPlayerView.UpdatePlayerColor;
begin
  Button_Reveal.FlagColor := gMySpectator.Hand.FlagColor;
end;


{ TKMMapEdPlayerView }
constructor TKMMapEdPlayerAdditional.Create(aParent: TKMPanel);
var kind : TKMMessageKind;
  I : Integer;
begin
  inherited Create;

  Panel_PlayerAdd := TKMScrollPanel.Create(aParent, 0, 33 + 30, aParent.Width, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  //Panel_PlayerAdd.ScrollV.Left := Panel_PlayerAdd.ScrollV.Left;
  Panel_PlayerAdd.AnchorsStretch;

  with TKMLabel.Create(Panel_PlayerAdd, 0, 0, Panel_PlayerAdd.Width - 20, 0, gResTexts[1791], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  with TKMLabel.Create(Panel_PlayerAdd, 9, 25, Panel_PlayerAdd.Width - 20, 0, gResTexts[1842], fntMetal, taLeft) do
    Anchors := [anLeft, anTop, anRight];

  Number_Workless := TKMNumericEdit.Create(Panel_PlayerAdd, 9, 45, 0, high(Word));
  Number_Workless.OnChange := Player_WorklessChange;
  Number_Workless.AutoFocusable := false;
  Button_ShowMessagePopUp := TKMButton.Create(Panel_PlayerAdd, 9, 80, Panel_PlayerAdd.Width - 18 - 25, 25, gResTexts[1843], bsGame);
  Button_ShowMessagePopUp.OnClick := Player_ButtonClick;

  Button_CheckPlayerShow := TKMButton.Create(Panel_PlayerAdd, Panel_PlayerAdd.Width - 18 - 25 - 20, 25, 25, 25, 794, rxGui, bsGame);
  Button_CheckPlayerShow.OnClick := Player_ButtonClick;
  Button_CheckPlayerShow.Hint := gResTexts[2336];

  PopUp_MessageEditor := TKMPopUpPanel.Create(aParent.MasterPanel, 600, 500, gResTexts[1844]);
  //TKMBevel.Create(PopUp_MessageEditor, -2000, -2000, 5000, 5000);
  //TKMImage.Create(PopUp_MessageEditor, 0, 0, PopUp_MessageEditor.Width, PopUp_MessageEditor.Height, 409);

  ColumnBox_MessageQueue := TKMColumnBox.Create(PopUp_MessageEditor, 50, 100, 190, 250, fntGame, bsGame);
  ColumnBox_MessageQueue.SetColumns(fntOutline,
                                    [gResTexts[1845],
                                     gResTexts[1846]], [0, 100], True);
  ColumnBox_MessageQueue.OnChange := Player_PopUpClick;
  ColumnBox_MessageQueue.Focusable := false;

  TKMLabel.Create(PopUp_MessageEditor, 250, 100, 150, 20, gResTexts[1847], fntMetal, taLeft);
  Number_TextID := TKMNumericEdit.Create(PopUp_MessageEditor, 250, 125, -1, high(Word));

  TKMLabel.Create(PopUp_MessageEditor, 250, 150, 150, 20, gResTexts[1848], fntMetal, taLeft);
  Number_Time := TKMNumericEdit.Create(PopUp_MessageEditor, 250, 175, -1, 9999999);
  Number_Time.OnChange := Player_MessageChange;
  Number_Time.AutoFocusable := false;
  Number_TextID.OnChange := Player_MessageChange;
  Number_TextID.AutoFocusable := false;

  Button_MessageAdd := TKMButton.Create(PopUp_MessageEditor, 250, 285, 160, 25, gResTexts[1849], bsGame);
  Button_MessageAdd.OnClick := Player_PopUpClick;
  Button_MessageDel := TKMButton.Create(PopUp_MessageEditor, 250, 315, 160, 25, gResTexts[1850], bsGame);
  Button_MessageDel.OnClick := Player_PopUpClick;

  Edit_MessageText := TKMEdit.Create(PopUp_MessageEditor, 50, ColumnBox_MessageQueue.Bottom + 10, 575 - 25, 25, fntMetal);
  Edit_MessageText.OnChange := Player_MessageChange;
  Edit_MessageText.AllowedChars := acAll;
  Edit_MessageText.SetTextSilently(gResTexts[1851]);

  Button_AddToLibx  := TKMButton.Create(PopUp_MessageEditor, Edit_MessageText.Right, Edit_MessageText.Top, 25, 25, 386, rxGui, bsGame);
  Button_AddToLibx.OnClick := Player_MessageChange;
  Button_AddToLibx.Hint := gResTexts[2340];

  Bevel_MessageText := TKMBevel.Create(PopUp_MessageEditor, 50, Edit_MessageText.Bottom + 5, 575, 180);

  Label_MessageText := TKMLabel.Create(PopUp_MessageEditor, 55, Edit_MessageText.Bottom + 10, 565, 170, '', fntMetal, taLeft);
  Label_MessageText.WordWrap := true;
  Label_MessageText.Caption := Edit_MessageText.Text;

  Button_ClosePopUp := TKMButton.Create(PopUp_MessageEditor, PopUp_MessageEditor.Width div 2 - 75,
                                        PopUp_MessageEditor.Height - 100, 150, 30, gResTexts[172], bsGame);
  Button_ClosePopUp.OnClick := Player_ButtonClick;
  Button_SaveToLibx := TKMButton.Create(PopUp_MessageEditor, PopUp_MessageEditor.Width div 2 - 75,
                                        PopUp_MessageEditor.Height - 70, 150, 30, 'Save To Libx', bsGame);
  Button_SaveToLibx.OnClick := Player_ButtonClick;


  DropList_MessageKind := TKMDropList.Create(PopUp_MessageEditor, 420, 100, 200, 25, fntGame, gResTexts[1852], bsGame);
  for kind := Low(TKMMessageKind) to High(TKMMessageKind) do
    DropList_MessageKind.Add(gResTexts[MSG_KIND_HINT[kind]]);
  DropList_MessageKind.OnChange := Player_MessageChange;

  Image_MessageKind := TKMButtonFlat.Create(PopUp_MessageEditor, 440, 150, 150, 200, 0);
  Image_MessageKind.OnClick := Player_ButtonClick;

  CheckBox_NeverHungry := TKMCheckBox.Create(Panel_PlayerAdd, 9, Button_ShowMessagePopUp.Bottom + 5, Panel_PlayerAdd.Width - 9, 25, gResTexts[1810], fntMetal);
  CheckBox_NeverHungry.OnClick := Player_ButtonClick;

  CheckBox_DisableUpdateEntities := TKMCheckBox.Create(Panel_PlayerAdd, 9, CheckBox_NeverHungry.Bottom, Panel_PlayerAdd.Width - 9, 25, gResTexts[1858], fntMetal);
  CheckBox_DisableUpdateEntities.OnClick := Player_ButtonClick;


  TKMLabel.Create(Panel_PlayerAdd, 9, CheckBox_DisableUpdateEntities.Bottom + 5, Panel_PlayerAdd.Width, 20, gResTexts[1946], fntMetal, taLeft);

  SetLength(VirtualWares_Row, gRes.Wares.VirtualWares.Count);
  for I := 0 to High(VirtualWares_Row) do
  begin
    VirtualWares_Row[I] := TKMWareOrderRow.Create(Panel_PlayerAdd, 9 + (I mod 2) * 100, CheckBox_DisableUpdateEntities.Bottom + 25 + (I div 2) * 25, 95);
    VirtualWares_Row[I].WareRow.TexID := gRes.Wares.VirtualWares[I].GUIIcon;
    //VirtualWares_Row[I].WareRow.Caption := gResTexts[gRes.Wares.VirtualWares[I].TextID];
    VirtualWares_Row[I].WareRow.Hint := gResTexts[gRes.Wares.VirtualWares[I].TextID];
    VirtualWares_Row[I].OrderCntMin := 0;
    VirtualWares_Row[I].OrderCntMax := high(Word);
    VirtualWares_Row[I].WareRow.WareCntAsNumber := true;
    VirtualWares_Row[I].Tag := I;
    VirtualWares_Row[I].OnChange := Player_WaresChange;
    VirtualWares_Row[I].WareRow.TextOffset := 0;
    VirtualWares_Row[I].WareRow.HideHighlight := false;
  end;

  Panel_PlayerCheck := TKMMapEdPlayerCheck.Create(aParent);
end;

procedure TKMMapEdPlayerAdditional.Hide;
begin
  Panel_PlayerAdd.Hide;
end;


procedure TKMMapEdPlayerAdditional.Show;
var I : Integer;
begin
  Number_Workless.Value := gMySpectator.Hand.Workless;
  CheckBox_NeverHungry.Checked := gMySpectator.Hand.NeverHungry;
  CheckBox_DisableUpdateEntities.Checked := not gMySpectator.Hand.UpdateHandEntities;
  for I := 0 to High(VirtualWares_Row) do
  begin
    VirtualWares_Row[I].OrderCount := gMySpectator.Hand.VirtualWare[gRes.Wares.VirtualWares[I].Name];
    VirtualWares_Row[I].WareRow.WareCount := gMySpectator.Hand.VirtualWare[gRes.Wares.VirtualWares[I].Name];

    VirtualWares_Row[I].WareRow.Hint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': ' + IntToStr(VirtualWares_Row[I].OrderCount);
    VirtualWares_Row[I].OrderRemHint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': '  + IntToStr(VirtualWares_Row[I].OrderCount);
    VirtualWares_Row[I].OrderAddHint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': '  + IntToStr(VirtualWares_Row[I].OrderCount);

  end;
    
  Panel_PlayerAdd.Show;
  Button_SaveToLibx.Visible := false;{ gCursor.CampaignData.Path <> '';}
end;


function TKMMapEdPlayerAdditional.Visible: Boolean;
begin
  Result := Panel_PlayerAdd.Visible;
end;

procedure TKMMapEdPlayerAdditional.Player_WorklessChange(Sender: TObject);
begin
  gMySpectator.Hand.Workless := Number_Workless.Value;
end;

procedure TKMMapEdPlayerAdditional.Player_WaresChange(Sender: TObject; X: Integer);
var I : Integer;
begin
  for I := 0 to High(VirtualWares_Row) do
  begin
    if Sender = VirtualWares_Row[I] then
      gMySpectator.Hand.VirtualWare[I] := EnsureRange(X + gMySpectator.Hand.VirtualWare[I], 0, high(word));

    VirtualWares_Row[I].OrderCount := gMySpectator.Hand.VirtualWare[I];
    VirtualWares_Row[I].WareRow.WareCount := VirtualWares_Row[I].OrderCount;

    VirtualWares_Row[I].WareRow.Hint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': '  + IntToStr(VirtualWares_Row[I].OrderCount);
    VirtualWares_Row[I].OrderRemHint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': '  + IntToStr(VirtualWares_Row[I].OrderCount);
    VirtualWares_Row[I].OrderAddHint := gResTexts[gRes.Wares.VirtualWares[I].TextID] + ': '  + IntToStr(VirtualWares_Row[I].OrderCount);

  end;

end;

procedure TKMMapEdPlayerAdditional.Player_ButtonClick(Sender: TObject);
var I : Integer;
  S : TStringList;
begin

  If sender = Button_CheckPlayerShow then
    Panel_PlayerCheck.Show
  else
  if sender = Button_SaveToLibx then
  begin
    if gCursor.CampaignData.Path <> '' then
    begin
      S := TStringList.Create;

      for I := 0 to High(gMySpectator.Hand.ShowMessage) do
        if gMySpectator.Hand.ShowMessage[I].ID = -1 then
        begin
          S.Add(IntToStr(I) + ':' +gMySpectator.Hand.ShowMessage[I].Text);
          gMySpectator.Hand.ShowMessage[I].ID := I;
        end;

      If S.Count > 0 then
        S.SaveToFile(TKMCampaignsCollection.GetFullPath(gCursor.CampaignData.Path,
                                                        gCursor.CampaignData.ShortName,
                                                        gCursor.CampaignData.MissionID,
                                                        '.'+ String(gResLocales.UserLocale) + '.libx'
                                                        )
                                                        );

      S.Free;
      RefreshList;
      RefreshMessage;
    end;

  end else
  if Sender = CheckBox_DisableUpdateEntities then
    gMySpectator.Hand.UpdateHandEntities := not CheckBox_DisableUpdateEntities.Checked
  else
  if sender = CheckBox_NeverHungry then
  begin
    gMySpectator.Hand.NeverHungry := CheckBox_NeverHungry.Checked;
  end else
  if Sender = Button_ShowMessagePopUp then
  begin
    PopUp_MessageEditor.Show;
    RefreshList;
    RefreshMessage;
  end
  else
  if Sender = Button_ClosePopUp then
    PopUp_MessageEditor.Hide;
end;

procedure TKMMapEdPlayerAdditional.RefreshList;
var I, topIndex, Index : Integer;
begin
  Index := ColumnBox_MessageQueue.ItemIndex;
  topIndex := ColumnBox_MessageQueue.TopIndex;

  ColumnBox_MessageQueue.Clear;
  for I := 0 to High(gMySpectator.Hand.ShowMessage) do
    with gMySpectator.Hand.ShowMessage[I] do
      ColumnBox_MessageQueue.AddItem(MakeListRow([IntToStr(ID), IntToStr(Time)]));


  //Try to restore previous selected element
  if Index >= ColumnBox_MessageQueue.RowCount then
    Index := ColumnBox_MessageQueue.RowCount - 1;

  ColumnBox_MessageQueue.ItemIndex := Index;

  ColumnBox_MessageQueue.TopIndex := TopIndex;

  ColumnBox_MessageQueue.JumpToSelected;

end;

procedure TKMMapEdPlayerAdditional.Player_PopUpClick(Sender : TOBject);

var I : Integer;
begin
  I := ColumnBox_MessageQueue.ItemIndex;
  if Sender = ColumnBox_MessageQueue then
    RefreshMessage
  else
  if Sender = Button_MessageAdd then
  begin
    gMySpectator.Hand.AddToMessageQueue;
    RefreshList;
    ColumnBox_MessageQueue.ItemIndex := ColumnBox_MessageQueue.RowCount - 1;
    ColumnBox_MessageQueue.JumpToSelected;
    RefreshMessage;
  end
  else
  if Sender = Button_MessageDel then
    if (I >= 0) and (I <= high(gMySpectator.Hand.ShowMessage)) then
    begin
      gMySpectator.Hand.DeleteFromMessageQueue(I);
      RefreshList;
      RefreshMessage;
    end;
end;

procedure TKMMapEdPlayerAdditional.Player_MessageChange(Sender : TOBject);
var I, newID : Integer;
begin
  I := ColumnBox_MessageQueue.ItemIndex;

  If Sender = Button_AddToLibx then
  begin
    If  Number_TextID.Value <> -1 then
      Exit;

    newID := gGame.TextMission.GetFirstEmpty;
    gGame.TextMission.SetText(gResLocales.UserLocaleIndex, newID, Edit_MessageText.Text);
    Number_TextID.Value := newID;
    Edit_MessageText.Text := '';
  end else
  if ColumnBox_MessageQueue.IsSelected then
  begin
    gMySpectator.Hand.ShowMessage[I].ID := Number_TextID.Value;
    gMySpectator.Hand.ShowMessage[I].Time := Number_Time.Value;
    gMySpectator.Hand.ShowMessage[I].Kind := TKMMessageKind(DropList_MessageKind.ItemIndex);
    gMySpectator.Hand.ShowMessage[I].Text :=  Edit_MessageText.Text;
    if (Sender is TKMNumericEdit) then
    begin
      ColumnBox_MessageQueue.Rows[I].Cells[0].Caption := IntToStr(gMySpectator.Hand.ShowMessage[I].ID);
      ColumnBox_MessageQueue.Rows[I].Cells[1].Caption := IntToStr(gMySpectator.Hand.ShowMessage[I].Time);
    end;

  end;

  Button_AddToLibx.Enabled := (Number_TextID.Value = -1) and (Edit_MessageText.Text <> '');

  RefreshMessage;
  if not (Sender is TKMNumericEdit) then
    RefreshList;

  if (Sender is TKMNumericEdit) then
    TKMNumericEdit(Sender).Focus;

end;

procedure TKMMapEdPlayerAdditional.RefreshMessage;
var I : integer;
begin
  I := ColumnBox_MessageQueue.ItemIndex;
  if not ColumnBox_MessageQueue.IsSelected then
  begin
    Number_TextID.Disable;
    Number_TextID.Value := -1;
    Number_Time.Disable;
    Number_Time.Value := -1;

    Button_MessageDel.Disable;
    DropList_MessageKind.Disable;
    Image_MessageKind.TexID := 0;
    Image_MessageKind.Disable;
    Edit_MessageText.Hide;
    Label_MessageText.Caption := '';
    Label_MessageText.Hide;
    Bevel_MessageText.Hide;
    Edit_MessageText.Hide;
  end else
  begin
    Label_MessageText.Show;
    Bevel_MessageText.Show;
    Edit_MessageText.Show;

    DropList_MessageKind.Enable;
    Number_TextID.Enable;
    Number_TextID.Value := gMySpectator.Hand.ShowMessage[I].ID;
    Number_Time.Enable;
    Number_Time.Value := gMySpectator.Hand.ShowMessage[I].Time;
    Edit_MessageText.SetTextSilently(gMySpectator.Hand.ShowMessage[I].Text);
    Image_MessageKind.Enable;
    Button_MessageDel.Enable;
    DropList_MessageKind.ItemIndex := ord(gMySpectator.Hand.ShowMessage[I].Kind);
    Image_MessageKind.TexID := MSG_ICON[gMySpectator.Hand.ShowMessage[I].Kind];

    if Number_TextID.Value > -1 then
    begin
      Edit_MessageText.Hide;

      Label_MessageText.Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(gMySpectator.Hand.ShowMessage[I].GetText));
    end else
    begin
      Label_MessageText.Caption := gMySpectator.Hand.ShowMessage[I].Text;
    end;

  end;
  
end;

end.
