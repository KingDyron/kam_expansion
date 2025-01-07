unit KM_GUIGameCustomPanel;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_ControlsBase, KM_CommonClasses,
  KM_Pics, KM_ResTypes,
  KM_InterfaceGame, KM_ScriptingTypes;


type
  TKMGUIGameCustomPanel = class
  private
    fOwner : ShortInt;
    procedure RefreshPanel;

    procedure ClosePanel(sender : TObject);
    procedure ShowButtonClicked(sender : TObject);
    procedure ButtonClicked(Sender : TObject);
    procedure ResizePanel(X,Y, Width,Height : Integer);

  protected
    Panel_Custom: TKMPanel;
      Bevel_Back : TKMBevel;
      Image_Close: TKMImage;
    Button_Show : TKMButton;
    Button_Custom: array of TKMButton;
    Image_Custom: array of TKMImage;
    Label_Custom: array of TKMLabel;
    ButtonFlat_Custom: array of TKMButtonFlat;
    Bevel_Custom: array[0..10] of TKMBevel;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_ScriptingEvents,
  KM_GameInputProcess, KM_GameSettings, KM_RenderUI, KM_HandsCollection, KM_Game,
  KM_ResFonts, SysUtils;

{ TKMGUIGameRatios }
constructor TKMGUIGameCustomPanel.Create(aParent: TKMPanel);
var I : Integer;
begin
  inherited Create;

  Panel_Custom := TKMPanel.Create(aParent, 260, 130, 500, 350);
  Bevel_Back := TKMBevel.Create(Panel_Custom, 0, 0, Panel_Custom.Width, Panel_Custom.Height);
  for I := 0 to High(Bevel_Custom) do
  begin
    Bevel_Custom[I] := TKMBevel.Create(Panel_Custom, 0, 0, 0, 0);
    Bevel_Custom[I].Hitable := false;
  end;

  Image_Close := TKMImage.Create(Panel_Custom, Panel_Custom.Width - 10, 0, 32, 32, 52);
  Image_Close.OnClick := ClosePanel;


  Button_Show := TKMButton.Create(aParent, 260, 130, 30, 25, 5, rxGui, bsGame);
  Button_Show.OnClick := ShowButtonClicked;
  Button_Show.Hide;
  Hide;

end;


procedure TKMGUIGameCustomPanel.Show;
begin
  Panel_Custom.Show;
  Button_Show.TexID := 4;
  RefreshPanel;
end;


procedure TKMGUIGameCustomPanel.Hide;
begin
  Panel_Custom.Hide;
  Button_Show.TexID := 5;
end;


function TKMGUIGameCustomPanel.Visible: Boolean;
begin
  Result := Panel_Custom.Visible;
end;

procedure TKMGUIGameCustomPanel.ResizePanel(X,Y, Width,Height : Integer);
begin
  Y := Y + 25;
  Panel_Custom.Left := {260 +} X;
  Panel_Custom.Top := {130 +} Y;
  Panel_Custom.Width := {100 +} Width;
  Panel_Custom.Height := {100 +} Height;

  Image_Close.Left := Panel_Custom.Width - 30;
  Image_Close.Hide;
  Button_Show.Left := {260} + X;
  Button_Show.Top := {130} + Y;
  Bevel_Back.Width := Panel_Custom.Width;
  Bevel_Back.Height := Panel_Custom.Height;
  Bevel_Back.Color.SetColor(0.15, 0.15, 0.15);
  Bevel_Back.BackAlpha := Bevel_Back.DEF_BACK_ALPHA + 0.2
end;
procedure TKMGUIGameCustomPanel.RefreshPanel;
var fLastButtonID,
    fLastLabelID,
    fLastImageID,
    fLastBevelID,
    fLastButtonFlatID : Integer;

  procedure AddButton(aInfo : TKMControlInfo);
  begin
    Inc(fLastButtonID);
    if fLastButtonID >= length(Button_Custom) then
    begin
      SetLength(Button_Custom, length(Button_Custom) + 1);
      Button_Custom[fLastButtonID] := TKMButton.Create(Panel_Custom, aInfo.Left, aInfo.Top + 25, aInfo.Width, aInfo.Height, UnicodeString(aInfo.Caption), bsGame);
      Button_Custom[fLastButtonID].OnClick := ButtonClicked;
      Button_Custom[fLastButtonID].RX := rxGui;
    end;
    Button_Custom[fLastButtonID].Hitable := true;
    with Button_Custom[fLastButtonID] do
    begin
      Left := aInfo.Left;
      Top := aInfo.Top + 25;
      Width := aInfo.Width;
      Height := aInfo.Height;
      Tag := aInfo.ID;
      Tag2 := aInfo.Tag;
      TexId := aInfo.TexID;
      Visible := aInfo.Visible;
      Enabled := aInfo.Enabled;
      Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Caption));
      Hint := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Hint));
    end;

  end;

  procedure AddLabel(aInfo : TKMControlInfo);
  begin
    Inc(fLastLabelID);
    if fLastLabelID >= length(Label_Custom) then
    begin
      SetLength(Label_Custom, length(Label_Custom) + 1);
      Label_Custom[fLastLabelID] := TKMLabel.Create(Panel_Custom, aInfo.Left, aInfo.Top + 25, aInfo.Width, aInfo.Height, UnicodeString(aInfo.Caption), fntMetal, taLeft);
      Label_Custom[fLastLabelID].WordWrap := true;
    end;
    Label_Custom[fLastLabelID].Hitable := false;
    with Label_Custom[fLastLabelID] do
    begin
      Left := aInfo.Left;
      Top := aInfo.Top + 25;
      Width := aInfo.Width;
      Height := aInfo.Height;
      Tag := aInfo.ID;
      Tag2 := aInfo.Tag;
      Visible := aInfo.Visible;
      Enabled := aInfo.Enabled;
      Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Caption));
      Label_Custom[fLastLabelID].WordWrap := true;
    end;
  end;

  procedure AddImage(aInfo : TKMControlInfo);
  begin
    Inc(fLastImageID);
    if fLastImageID >= length(Image_Custom) then
    begin
      SetLength(Image_Custom, length(Image_Custom) + 1);
      Image_Custom[fLastImageID] := TKMImage.Create(Panel_Custom, aInfo.Left, aInfo.Top + 25, aInfo.Width, aInfo.Height, aInfo.TexID);

      Image_Custom[fLastImageID].RX := rxGui;
      //Image_Custom[fLastImageID].AlphaStep := -1;
    end;
    Image_Custom[fLastImageID].FlagColor := gHands[fOwner].FlagColor;
    Image_Custom[fLastImageID].AlphaStep := aInfo.ImageAlphaStep;
    Image_Custom[fLastImageID].Hitable := false;
    with Image_Custom[fLastImageID] do
    begin
      Left := aInfo.Left;
      Top := aInfo.Top + 25;
      Width := aInfo.Width;
      Height := aInfo.Height;
      Tag := aInfo.ID;
      Tag2 := aInfo.Tag;
      TexId := aInfo.TexID;
      Visible := aInfo.Visible;
      Enabled := aInfo.Enabled;
    end;
  end;

  procedure AddBevel(aInfo : TKMControlInfo);
  begin
    Inc(fLastBevelID);
    if fLastBevelID >= length(Bevel_Custom) then
      Exit;

    Bevel_Custom[fLastBevelID].Hitable := false;
    with Bevel_Custom[fLastBevelID] do
    begin
      Left := aInfo.Left;
      Top := aInfo.Top + 25;
      Width := aInfo.Width;
      Height := aInfo.Height;
      Tag := aInfo.ID;
      Tag2 := aInfo.Tag;
      Visible := aInfo.Visible;
    end;
  end;

  procedure AddButtonFlat(aInfo : TKMControlInfo);
  begin
    Inc(fLastButtonFlatID);
    if fLastButtonFlatID >= length(ButtonFlat_Custom) then
    begin
      SetLength(ButtonFlat_Custom, length(ButtonFlat_Custom) + 1);
      ButtonFlat_Custom[fLastButtonFlatID] := TKMButtonFlat.Create(Panel_Custom, aInfo.Left, aInfo.Top + 25, aInfo.Width, aInfo.Height, aInfo.TexID, rxGui);
      ButtonFlat_Custom[fLastButtonFlatID].OnClick := ButtonClicked;
      ButtonFlat_Custom[fLastButtonFlatID].RX := rxGui;
      ButtonFlat_Custom[fLastButtonFlatID].Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Caption));
      ButtonFlat_Custom[fLastButtonFlatID].CapOffsetY := -5;
    end;
    ButtonFlat_Custom[fLastButtonFlatID].Hitable := true;
    with ButtonFlat_Custom[fLastButtonFlatID] do
    begin
      Left := aInfo.Left;
      Top := aInfo.Top + 25;
      Width := aInfo.Width;
      Height := aInfo.Height;
      Tag := aInfo.ID;
      Tag2 := aInfo.Tag;
      TexId := aInfo.TexID;
      Visible := aInfo.Visible;
      Enabled := aInfo.Enabled;
      Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Caption));
      Hint := gGame.TextMission.ParseTextMarkup(UnicodeString(aInfo.Hint));
      CapOffsetY := -5;
    end;

  end;
  procedure HideEveryThing;
  var I : Integer;
  begin
    for I := 0 to High(Button_Custom) do
      Button_Custom[I].Hide;
    for I := 0 to High(Label_Custom) do
      Label_Custom[I].Hide;
    for I := 0 to High(Image_Custom) do
      Image_Custom[I].Hide;
    for I := 0 to High(Bevel_Custom) do
      Bevel_Custom[I].Hide;
    for I := 0 to High(ButtonFlat_Custom) do
      ButtonFlat_Custom[I].Hide;

  end;
var I : Integer;
begin
  fLastButtonID := -1;
  fLastLabelID := -1;
  fLastImageID := -1;
  fLastBevelID := -1;
  fLastButtonFlatID := -1;
  fOwner := gMySpectator.HandID;
  HideEveryThing;
  with gMySpectator.Hand.CustomPanelData do
  begin
    ResizePanel(PanelLeft, PanelTop, PanelSizeX, PanelSizeY);
    for I := 0 to ControlsCount - 1 do
    begin
      case ControlsData[I].CType of
        ctButton : AddButton(ControlsData[I]);
        ctLabel : AddLabel(ControlsData[I]);
        ctImage  : AddImage(ControlsData[I]);
        ctBevel  : AddBevel(ControlsData[I]);
        ctButtonFlat  : AddButtonFlat(ControlsData[I]);
      end;
    end;
  end;

end;


procedure TKMGUIGameCustomPanel.UpdateState;
begin
  Button_Show.Visible := gMySpectator.Hand.CustomPanelData.ControlsCount > 0;
  if not Button_Show.Visible then
    Hide;
  with gMySpectator.Hand.CustomPanelData do
    ResizePanel(PanelLeft, PanelTop, PanelSizeX, PanelSizeY);
  if not Visible then
    Exit;
  RefreshPanel;
  //RatioTabSet(fActiveTab);
end;

procedure TKMGUIGameCustomPanel.ClosePanel(sender: TObject);
begin
  Hide;
end;

procedure TKMGUIGameCustomPanel.ShowButtonClicked(sender : TObject);
begin
  if Button_Show.TexID = 5 then
    Show
  else
    Hide;
end;

procedure TKMGUIGameCustomPanel.ButtonClicked(Sender: TObject);
begin
  if (Sender is TKMButton) or (Sender is TKMButtonFlat) then
    gScriptEvents.ProcCustomPanelButtonClicked(fOwner, TKMControl(Sender).Tag, TKMControl(Sender).Tag2);
end;

end.


