unit KM_GUIMapEdMarkerDefence;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsTrackBar, KM_ControlsSwitch,
   KM_Defaults, KM_Pics, KM_Points, KM_InterfaceGame;


type
  TKMMapEdMarkerDefence = class
  private
    fOwner: TKMHandID;
    fIndex: Integer;
    fOnDone: TNotifyEvent;
    fIsDefend : Boolean;
    procedure Marker_Change(Sender: TObject);
    procedure Marker_UpdateOrder(Sender: TObject; Shift: TShiftState);
    procedure Defend_Change(Sender: TObject);
    procedure Refresh_UnitType(oldGT, newGT : TKMGroupType);
  protected
    Panel_MarkerDefence: TKMPanel;
      Label_MarkerType: TKMLabel;
      Panel_Defence: TKMPanel;
        Image_MarkerPic: TKMImage;
        DropList_DefenceGroup: TKMDropList;
        DropList_DefenceType: TKMDropList;
        DropList_DefenceUnitType: TKMDropList;
        TrackBar_DefenceRad: TKMTrackBar;
        Label_Priority: TKMLabel;
        Button_Priority_Inc, Button_Priority_Dec: TKMButton;
        Button_DefenceCW, Button_DefenceCCW: TKMButton;
        Button_DefenceDelete: TKMButton;
        Button_DefenceClose: TKMButton;
        CheckBox_OrganizePosition : TKMCheckBox;

      Panel_Defend: TKMPanel;
        Image_DefendPic: TKMImage;
        TrackBar_DefendRad: TKMTrackBar;
        Button_DefendDelete: TKMButton;
        Button_DefendClose: TKMButton;

  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    property Index: Integer read fIndex;
    property Owner: TKMHandID read fOwner;

    procedure Show(aPlayer: TKMHandID; aIndex: Integer);
    procedure ShowDefend(aPlayer: TKMHandID; aIndex: Integer);
    procedure Hide;
    function Visible: Boolean;
    property IsDefend : Boolean read  fIsDefend;
  end;


implementation
uses
  KM_Game,
  KM_AIDefensePos,
  KM_HandsCollection, KM_Hand,
  KM_Resource, KM_ResTexts, KM_ResFonts, KM_ResTypes, KM_ResUnits,
  KM_RenderUI,
  KM_AITypes;


{ TKMMapEdMarkerDefence }
constructor TKMMapEdMarkerDefence.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_MarkerDefence := TKMPanel.Create(aParent, TB_PAD, 50, TB_MAP_ED_WIDTH - TB_PAD, 400);

  Label_MarkerType := TKMLabel.Create(Panel_MarkerDefence, 0, 10, Panel_MarkerDefence.Width, 0, '', fntOutline, taCenter);

    Panel_Defence := TKMPanel.Create(Panel_MarkerDefence, 0, 0, Panel_MarkerDefence.Width, Panel_MarkerDefence.Height);

      Image_MarkerPic := TKMImage.Create(Panel_Defence, 0, 10, 32, 32, 338);

      DropList_DefenceGroup := TKMDropList.Create(Panel_Defence, 0, 55, Panel_Defence.Width, 20, fntGame, '', bsGame);
      DropList_DefenceGroup.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_MELEE]);
      DropList_DefenceGroup.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE]);
      DropList_DefenceGroup.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_RANGED]);
      DropList_DefenceGroup.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_MOUNTED]);
      DropList_DefenceGroup.Add(gResTexts[1643]);
      DropList_DefenceGroup.Add(gResTexts[1883]);
      DropList_DefenceGroup.Add(gResTexts[1963]);
      DropList_DefenceGroup.Add(gResTexts[2022]);
      DropList_DefenceGroup.OnChange := Marker_Change;
      DropList_DefenceType := TKMDropList.Create(Panel_Defence, 0, 85, Panel_Defence.Width, 20, fntGame, '', bsGame);
      DropList_DefenceType.Add(gResTexts[TX_MAPED_AI_DEFENCE_DEFENDERS]);
      DropList_DefenceType.Add(gResTexts[TX_MAPED_AI_DEFENCE_ATTACKERS]);
      DropList_DefenceType.Add(gResTexts[2031]);
      DropList_DefenceType.OnChange := Marker_Change;

      DropList_DefenceUnitType := TKMDropList.Create(Panel_Defence, 0, 115, Panel_Defence.Width, 20, fntGame, '', bsGame);
      DropList_DefenceUnitType.OnChange := Marker_Change;

      TrackBar_DefenceRad := TKMTrackBar.Create(Panel_Defence, 0, 145, Panel_Defence.Width, 1, 128);
      TrackBar_DefenceRad.Caption := gResTexts[TX_MAPED_AI_DEFENCE_RADIUS];
      TrackBar_DefenceRad.OnChange := Marker_Change;

      TKMLabel.Create(Panel_Defence, 0, 195, gResTexts[TX_MAPED_AI_DEFENCE_PRIORITY_ORDER], fntMetal, taLeft);
      Label_Priority := TKMLabel.Create(Panel_Defence, 20, 215+2, 20, 0, '', fntGrey, taCenter);
      Button_Priority_Dec := TKMButton.Create(Panel_Defence, 0, 215, 20, 20, '-', bsGame);
      Button_Priority_Inc := TKMButton.Create(Panel_Defence, 40, 215, 20, 20, '+', bsGame);
      Button_Priority_Dec.OnClickShift := Marker_UpdateOrder;
      Button_Priority_Inc.OnClickShift := Marker_UpdateOrder;

      Button_DefenceCCW  := TKMButton.Create(Panel_Defence, 0, 265, 50, 35, 23, rxGui, bsGame);
      Button_DefenceCCW.OnClick := Marker_Change;
      Button_DefenceCW := TKMButton.Create(Panel_Defence, 130, 265, 50, 35, 24, rxGui, bsGame);
      Button_DefenceCW.OnClick := Marker_Change;

      CheckBox_OrganizePosition := TKMCheckBox.Create(Panel_Defence, 0, 310, 200, 25, gResTexts[1939], fntMetal);
      CheckBox_OrganizePosition.OnClick := Marker_Change;
      CheckBox_OrganizePosition.Hint := gResTexts[1940];

      Button_DefenceDelete := TKMButton.Create(Panel_Defence, 0, 340, 25, 25, 340, rxGui, bsGame);
      Button_DefenceDelete.Hint := gResTexts[TX_MAPED_AI_DEFENCE_DELETE_HINT];
      Button_DefenceDelete.OnClick := Marker_Change;

      Button_DefenceClose := TKMButton.Create(Panel_Defence, Panel_Defence.Width-100, 340, 100, 25, gResTexts[TX_MAPED_CLOSE], bsGame);
      Button_DefenceClose.Hint := gResTexts[TX_MAPED_AI_DEFENCE_CLOSE_HINT];
      Button_DefenceClose.OnClick := Marker_Change;


    Panel_Defend := TKMPanel.Create(Panel_MarkerDefence, 0, 0, Panel_MarkerDefence.Width, Panel_MarkerDefence.Height);

      Image_DefendPic := TKMImage.Create(Panel_Defend, 0, 10, 32, 32, 338);

      TrackBar_DefendRad := TKMTrackBar.Create(Panel_Defend, 0, 50, Panel_Defend.Width, 1, 128);
      TrackBar_DefendRad.Caption := gResTexts[TX_MAPED_AI_DEFENCE_RADIUS];
      TrackBar_DefendRad.OnChange := Defend_Change;

      Button_DefendDelete := TKMButton.Create(Panel_Defend, 0, 100, 25, 25, 340, rxGui, bsGame);
      Button_DefendDelete.Hint := gResTexts[TX_MAPED_AI_DEFENCE_DELETE_HINT];
      Button_DefendDelete.OnClick := Defend_Change;

      Button_DefendClose := TKMButton.Create(Panel_Defend, Panel_Defend.Width-100, 100, 100, 25, gResTexts[TX_MAPED_CLOSE], bsGame);
      Button_DefendClose.Hint := gResTexts[TX_MAPED_AI_DEFENCE_CLOSE_HINT];
      Button_DefendClose.OnClick := Defend_Change;

  {Panel_Defend: TKMPanel;
    Image_DefendPic: TKMImage;
    TrackBar_DefendRad: TKMTrackBar;
    Button_DefendDelete: TKMButton;
    Button_DefendClose: TKMButton;}
  Panel_Defend.Hide;
  Panel_Defence.Hide;
end;

procedure TKMMapEdMarkerDefence.Refresh_UnitType(oldGT, newGT : TKMGroupType);
var I : Integer;
    UT : TKMUnitType;
begin
  If oldGT = newGT then
    Exit;
  DropList_DefenceUnitType.Clear;
  DropList_DefenceUnitType.Add(gRes.Units[utNone].GUIName, ord(utNone));
  for I := 0 to High(AI_TROOP_TRAIN_ORDER_NEW[newGT]) do
  begin
    UT := AI_TROOP_TRAIN_ORDER_NEW[newGT, I];
    if UT in [utGolem, utGiant, utFighter] then
      Continue;
    DropList_DefenceUnitType.Add(gRes.Units[UT].GUIName, ord(UT));
  end;
  DropList_DefenceUnitType.ItemIndex := 0;
end;

procedure TKMMapEdMarkerDefence.Marker_Change(Sender: TObject);
var
  DP: TAIDefencePosition;
  oldGT : TKMGroupType;
begin
  DP := gHands[fOwner].AI.General.DefencePositions[fIndex];
  oldGT := DP.GroupType;
  DP.Radius := TrackBar_DefenceRad.Position;
  DP.DefenceType := TKMAIDefencePosType(DropList_DefenceType.ItemIndex);
  DP.GroupType := TKMGroupType(GROUP_TYPE_MIN_OFF + DropList_DefenceGroup.ItemIndex);
  DP.DontRestock := not CheckBox_OrganizePosition.Checked;

  Refresh_UnitType(oldGT, DP.GroupType);

  DP.UnitType := TKMUnitType(DropList_DefenceUnitType.GetSelectedTag);
  if Sender = Button_DefenceCW then
    DP.Position := KMPointDir(DP.Position.Loc, KMNextDirection(DP.Position.Dir));
  if Sender = Button_DefenceCCW then
    DP.Position := KMPointDir(DP.Position.Loc, KMPrevDirection(DP.Position.Dir));

  if Sender = Button_DefenceDelete then
  begin
    gHands[fOwner].AI.General.DefencePositions.Delete(fIndex);
    Hide;
    fOnDone(Self);
  end;

  if Sender = Button_DefenceClose then
  begin
    Hide;
    fOnDone(Self);
  end;
end;

procedure TKMMapEdMarkerDefence.Defend_Change(Sender: TObject);
var
  DD: TAIDefendPosition;
begin
  DD := gHands[fOwner].AI.General.DefendPositions[fIndex];
  DD.Radius := TrackBar_DefendRad.Position;

  if Sender = Button_DefendDelete then
  begin
    gHands[fOwner].AI.General.DefendPositions.Delete(fIndex);
    Hide;
    fOnDone(Self);
  end;

  if Sender = Button_DefendClose then
  begin
    Hide;
    fOnDone(Self);
  end;
end;


procedure TKMMapEdMarkerDefence.Marker_UpdateOrder(Sender: TObject; Shift: TShiftState);
var
  I: Integer;
begin
  if Sender = Button_Priority_Inc then
    for I := 1 to 1 + 9*Byte(ssRight in Shift) do
      if fIndex < gHands[fOwner].AI.General.DefencePositions.Count-1 then
      begin
        gHands[fOwner].AI.General.DefencePositions.MoveUp(fIndex);
        Inc(fIndex);
        Label_Priority.Caption := IntToStr(fIndex + 1);
      end;

  if Sender = Button_Priority_Dec then
    for I := 1 to 1 + 9*Byte(ssRight in Shift) do
      if fIndex > 0 then
      begin
        gHands[fOwner].AI.General.DefencePositions.MoveDown(fIndex);
        Dec(fIndex);
      end;

  Label_Priority.Caption := IntToStr(fIndex + 1);
  Button_Priority_Inc.Enabled := fIndex < gHands[fOwner].AI.General.DefencePositions.Count-1;
  Button_Priority_Dec.Enabled := fIndex > 0;
  gGame.MapEditor.ActiveMarker.Index := fIndex;
end;


procedure TKMMapEdMarkerDefence.Show(aPlayer: TKMHandID; aIndex: Integer);
begin
  fIsDefend := false;
  Panel_MarkerDefence.Show;
  fOwner := aPlayer;
  fIndex := aIndex;
  Label_MarkerType.Caption := gResTexts[TX_MAPED_AI_DEFENCE_POSITION];
  Image_MarkerPic.TexID := 338;
  DropList_DefenceGroup.ItemIndex := Ord(gHands[fOwner].AI.General.DefencePositions[fIndex].GroupType) - GROUP_TYPE_MIN_OFF;
  DropList_DefenceType.ItemIndex := Ord(gHands[fOwner].AI.General.DefencePositions[fIndex].DefenceType);
  TrackBar_DefenceRad.Position := gHands[fOwner].AI.General.DefencePositions[fIndex].Radius;
  CheckBox_OrganizePosition.Checked := not gHands[fOwner].AI.General.DefencePositions[fIndex].DontRestock;
  Refresh_UnitType(gtNone, gHands[fOwner].AI.General.DefencePositions[fIndex].GroupType);
  Marker_UpdateOrder(nil, []);
  DropList_DefenceUnitType.SelectByTag(ord(gHands[fOwner].AI.General.DefencePositions[fIndex].UnitType));

  Panel_Defence.Show;
end;

procedure TKMMapEdMarkerDefence.ShowDefend(aPlayer: TKMHandID; aIndex: Integer);
var
  DD : TAIDefendPosition;
begin
  fIsDefend := true;
  Panel_MarkerDefence.Show;
  fOwner := aPlayer;
  fIndex := aIndex;

  DD := gHands[fOwner].AI.General.DefendPositions[fIndex];

  Label_MarkerType.Caption := gResTexts[2119];
  Image_DefendPic.TexID := 955;

  TrackBar_DefendRad.Position := DD.Radius;


  Panel_Defend.Show;
end;

procedure TKMMapEdMarkerDefence.Hide;
begin
  Panel_MarkerDefence.Hide;
end;


function TKMMapEdMarkerDefence.Visible: Boolean;
begin
  Result := Panel_MarkerDefence.Visible;
end;


end.
