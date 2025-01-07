unit KM_GUIMapEdTownDefence;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_InterfaceDefaults,
   KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsScroll, KM_ControlsSwitch, KM_ControlsTrackBar,
   KM_Defaults, KM_GUIMapEdTownFormationsPopUp;


type
  TKMMapEdTownDefence = class(TKMMapEdSubMenuPage)
  private
    procedure Town_DefenceFormations(Sender: TObject);
    procedure Town_DefenceAddClick(Sender: TObject);
    procedure Town_DefenceRefresh;
    procedure Town_DefenceChange(Sender: TObject);
  protected
    Panel_Defence: TKMScrollPanel;
      Button_DefencePosAdd,
      Button_DefendPosAdd: TKMButtonFlat;
      TrackBar_DPRange: TKMTrackBar;
      CheckBox_Restock: TKMCheckBox;
      CheckBox_AutoDefence: TKMCheckBox;
      CheckBox_DefendAllies: TKMCheckBox;
      TrackBar_AutoAttackRange: TKMTrackBar;
      TrackBar_RecruitCount: TKMTrackBar;
      NumEdit_RecruitDelay: TKMNumericEdit;
      CheckBox_MaxSoldiers: TKMCheckBox;
      NumEdit_MaxSoldiers: TKMNumericEdit;
      Button_EditFormations: TKMButton;
      Button_RemoveDefencePos: TKMButton;

      CheckBox_DefencePositionGiveGroup: TKMCheckBox;
      TrackBar_DefencePositionUType: TKMTrackBar;

      Button_AllUnitsCount,
      Button_AttackingUnitsCount : TKMButtonFlat;
      Button_DefencePosCount : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMButtonFlat;
  public
    FormationsPopUp: TKMMapEdTownFormations;

    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdateHotkeys;
    procedure UpdateState;
    procedure UpdatePlayer(aIndex: TKMHandID);
  end;


implementation
uses
  KM_Game, KM_HandsCollection, KM_Resource, KM_ResTexts, KM_Cursor, KM_RenderUI, KM_ResFonts, KM_InterfaceGame,
  KM_Hand, KM_Utils, KM_MapEdTypes;

const
  DEF_POS_ARMY_THUMB_TX: array[TKMGroupLevel] of Integer = (TX_MAPED_AI_GROUP_LVL_WEAK,
                                                            TX_MAPED_AI_ARMY_TYPE_LEATHER,
                                                            TX_MAPED_AI_ARMY_TYPE_IRON);

{ TKMMapEdTownDefence }
constructor TKMMapEdTownDefence.Create(aParent: TKMPanel);

  function GetDefPosArmyThumbWidth(aFont: TKMFont): Integer;
  var
    GL: TKMGroupLevel;
  begin
    Result := 0;
    for GL := Low(DEF_POS_ARMY_THUMB_TX) to High(DEF_POS_ARMY_THUMB_TX) do
      Result := Max(Result, gRes.Fonts[aFont].GetTextSize(gResTexts[DEF_POS_ARMY_THUMB_TX[GL]]).X);

    Inc(Result, TKMTrackBar.THUMB_WIDTH_ADD);
  end;

const
  DEF_GROUP_LVL: TKMGroupLevel = glLeather; // Leather soldiers by default
var addTop : Integer;
  GT : TKMGroupType;
begin
  inherited Create;

  Panel_Defence := TKMScrollPanel.Create(aParent, 0, 28, aParent.Width, aParent.Height - 28, [saVertical], bsMenu, ssCommon);
  Panel_Defence.Padding.SetBottom(10);
  Panel_Defence.ScrollV_PadTop := 10;
  Panel_Defence.ScrollV_PadBottom := 10;
  Panel_Defence.AnchorsStretch;

  with TKMLabel.Create(Panel_Defence, 0, PAGE_TITLE_Y, Panel_Defence.Width, 0, gResTexts[TX_MAPED_AI_DEFENSE], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  Button_DefencePosAdd := TKMButtonFlat.Create(Panel_Defence, TB_PAD, 30, 33, 33, 338);
  Button_DefencePosAdd.OnClick := Town_DefenceAddClick;

  Button_DefendPosAdd := TKMButtonFlat.Create(Panel_Defence, TB_PAD + 37, 30, 33, 33, 955);
  Button_DefendPosAdd.OnClick := Town_DefenceAddClick;
  Button_DefendPosAdd.Hint := gResTexts[2118];

  TrackBar_DPRange := TKMTrackBar.Create(Panel_Defence, TB_PAD, 65, Panel_Defence.Width - TB_PAD, 1, 128);
  TrackBar_DPRange.Anchors := [anLeft, anTop, anRight];
  TrackBar_DPRange.Caption := gResTexts[1936];
  TrackBar_DPRange.Hint := gResTexts[1936];
  TrackBar_DPRange.OnChange := Town_DefenceChange;
  TrackBar_DPRange.Position := 20;

  addTop := 40;
  with TKMLabel.Create(Panel_Defence, 0, 65 + addTop, TB_MAP_ED_WIDTH, 0, gResTexts[TX_MAPED_AI_DEFENSE_OPTIONS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  addTop := 40;
  CheckBox_AutoDefence := TKMCheckBox.Create(Panel_Defence, TB_PAD, 90 + addTop, Panel_Defence.Width - TB_PAD, 20, gResTexts[TX_MAPED_AI_DEFENSE_AUTO], fntMetal);
  CheckBox_AutoDefence.OnClick := Town_DefenceChange;

  CheckBox_DefendAllies := TKMCheckBox.Create(Panel_Defence, TB_PAD, 110 + addTop, Panel_Defence.Width - TB_PAD, 20, gResTexts[TX_MAPED_AI_DEFEND_ALLIES], fntMetal);
  CheckBox_DefendAllies.OnClick := Town_DefenceChange;

  CheckBox_Restock := TKMCheckBox.Create(Panel_Defence, TB_PAD, 130 + addTop, Panel_Defence.Width - TB_PAD, 20, gResTexts[1939], fntMetal);
  CheckBox_Restock.OnClick := Town_DefenceChange;
  CheckBox_Restock.Hint := gResTexts[1940];

  addTop := 65;

  TrackBar_AutoAttackRange := TKMTrackBar.Create(Panel_Defence, TB_PAD, 136 + addTop, Panel_Defence.Width - TB_PAD, 1, 20);
  TrackBar_AutoAttackRange.Anchors := [anLeft, anTop, anRight];
  TrackBar_AutoAttackRange.Caption := gResTexts[TX_MAPED_AI_AUTO_ATTACK];
  TrackBar_AutoAttackRange.Hint := gResTexts[TX_MAPED_AI_AUTO_ATTACK_HINT];
  TrackBar_AutoAttackRange.OnChange := Town_DefenceChange;

  TrackBar_RecruitCount := TKMTrackBar.Create(Panel_Defence, TB_PAD, 186 + addTop, Panel_Defence.Width - TB_PAD, 1, 20);
  TrackBar_RecruitCount.Anchors := [anLeft, anTop, anRight];
  TrackBar_RecruitCount.Caption := gResTexts[TX_MAPED_AI_RECRUITS];
  TrackBar_RecruitCount.Hint := gResTexts[TX_MAPED_AI_RECRUITS_HINT];
  TrackBar_RecruitCount.OnChange := Town_DefenceChange;

  with TKMLabel.Create(Panel_Defence, TB_PAD, 230 + addTop, Panel_Defence.Width - TB_PAD, 20, gResTexts[TX_MAPED_AI_RECRUIT_DELAY], fntMetal, taLeft) do
    Hint := gResTexts[TX_MAPED_AI_RECRUIT_DELAY_HINT];

  NumEdit_RecruitDelay := TKMNumericEdit.Create(Panel_Defence, TB_PAD, 230 + 20 + addTop, 0, 500);
  NumEdit_RecruitDelay.Hint := gResTexts[TX_MAPED_AI_RECRUIT_DELAY_HINT];
  NumEdit_RecruitDelay.MouseWheelStep := 20;
  NumEdit_RecruitDelay.AutoFocusable := False;
  NumEdit_RecruitDelay.OnChange := Town_DefenceChange;

  CheckBox_MaxSoldiers := TKMCheckBox.Create(Panel_Defence, TB_PAD, 274 + addTop, Panel_Defence.Width - TB_PAD, 20, gResTexts[TX_MAPED_AI_MAX_SOLDIERS], fntMetal);
  CheckBox_MaxSoldiers.OnClick := Town_DefenceChange;

  NumEdit_MaxSoldiers := TKMNumericEdit.Create(Panel_Defence, TB_PAD + 20, 297 + addTop, 0, 500);
  NumEdit_MaxSoldiers.Hint := gResTexts[TX_MAPED_AI_MAX_SOLDIERS_HINT];
  NumEdit_MaxSoldiers.MouseWheelStep := 20;
  NumEdit_MaxSoldiers.AutoFocusable := False;
  NumEdit_MaxSoldiers.OnChange := Town_DefenceChange;

  Button_EditFormations := TKMButton.Create(Panel_Defence, TB_PAD, 327 + addTop, Panel_Defence.Width - TB_PAD, 25, gResTexts[TX_MAPED_AI_FORMATIONS], bsGame);
  Button_EditFormations.Anchors := [anLeft, anTop, anRight];
  Button_EditFormations.OnClick := Town_DefenceFormations;

  Button_RemoveDefencePos := TKMButton.Create(Panel_Defence, TB_PAD, 386 + 50 + addTop, Panel_Defence.Width - TB_PAD, 25, gResTexts[1840], bsGame);
  Button_RemoveDefencePos.Anchors := [anLeft, anTop, anRight];
  Button_RemoveDefencePos.OnClick := Town_DefenceChange;
  Button_RemoveDefencePos.Hide;


  CheckBox_DefencePositionGiveGroup := TKMCheckBox.Create(Panel_Defence, TB_PAD, 365 + addTop , Panel_Defence.Width - TB_PAD, 20, gResTexts[TX_MAPED_AI_ADD_GROUP], fntMetal);
  CheckBox_DefencePositionGiveGroup.OnClick := Town_DefenceChange;
  CheckBox_DefencePositionGiveGroup.Hint := gResTexts[TX_MAPED_AI_ADD_GROUP_HINT];

  TrackBar_DefencePositionUType := TKMTrackBar.Create(Panel_Defence, TB_PAD + 20, 386 + addTop, Panel_Defence.Width - TB_PAD - 20,
                                                      Ord(Low(TKMGroupLevel)), Ord(High(TKMGroupLevel)));
  TrackBar_DefencePositionUType.Caption := gResTexts[TX_MAPED_AI_ARMY_TYPE];
  TrackBar_DefencePositionUType.Anchors := [anLeft, anTop, anRight];
  TrackBar_DefencePositionUType.Hint := gResTexts[TX_MAPED_AI_GROUP_LVL_HINT];
  TrackBar_DefencePositionUType.OnChange := Town_DefenceChange;
  TrackBar_DefencePositionUType.Position := Ord(DEF_GROUP_LVL);
  TrackBar_DefencePositionUType.ThumbText := gResTexts[DEF_POS_ARMY_THUMB_TX[DEF_GROUP_LVL]];
  TrackBar_DefencePositionUType.FixedThumbWidth := True;
  TrackBar_DefencePositionUType.ThumbWidth := GetDefPosArmyThumbWidth(TrackBar_DefencePositionUType.Font);



  Button_AllUnitsCount := TKMButtonFlat.Create(Panel_Defence, TB_PAD + 75, TrackBar_DefencePositionUType.Bottom + 10, 30, 35, 665);
  Button_AllUnitsCount.Hint := gResTexts[2133];
  Button_AttackingUnitsCount := TKMButtonFlat.Create(Panel_Defence, Button_AllUnitsCount.Right + 3, Button_AllUnitsCount.Top, 30, 35, 658);
  Button_AttackingUnitsCount.Hint := gResTexts[2134];
  TKMLabel.Create(Panel_Defence, 0, Button_AllUnitsCount.Bottom + 5, Panel_Defence.Width, 20, gResTexts[2135], fntMetal, taCenter);

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    Button_DefencePosCount[GT] := TKMButtonFlat.Create(Panel_Defence, 30 + 45 * (GROUP_TYPES[GT] mod 4),
                                                    Button_AllUnitsCount.Bottom + 25 + 40 * (GROUP_TYPES[GT] div 4), 30, 35, GROUP_TYPE_GUI_ICON[GT]);
    Button_DefencePosCount[GT].Hint := gResTexts[2136] + gResTexts[GROUP_TYPE_GUI_TEXT[GT]];
  end;


  fSubMenuActionsEvents[0] := Town_DefenceAddClick;
  fSubMenuActionsEvents[1] := Town_DefenceChange;
  fSubMenuActionsEvents[2] := Town_DefenceChange;
  fSubMenuActionsEvents[3] := Town_DefenceChange;
  fSubMenuActionsEvents[4] := Town_DefenceFormations;

  fSubMenuActionsCtrls[0,0] := Button_DefencePosAdd;
  fSubMenuActionsCtrls[1,0] := CheckBox_AutoDefence;
  fSubMenuActionsCtrls[2,0] := CheckBox_DefendAllies;
  fSubMenuActionsCtrls[3,0] := CheckBox_MaxSoldiers;
  fSubMenuActionsCtrls[4,0] := Button_EditFormations;
end;


procedure TKMMapEdTownDefence.Town_DefenceAddClick(Sender: TObject);
begin
  //Press the button
  if (Sender = Button_DefencePosAdd) then
  begin
    Button_DefencePosAdd.Down := not Button_DefencePosAdd.Down;
    Button_DefendPosAdd.Down := false;
  end else
  if Sender = Button_DefendPosAdd then
  begin
    Button_DefendPosAdd.Down := not Button_DefendPosAdd.Down;
    Button_DefencePosAdd.Down := false;
  end;


  if Button_DefencePosAdd.Down then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_DEFENCE;
  end
  else
  if Button_DefendPosAdd.Down then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_DEFEND;
  end
  else
    gCursor.Mode := cmNone;
end;


procedure TKMMapEdTownDefence.Town_DefenceChange(Sender: TObject);
begin
  gMySpectator.Hand.AI.Setup.AutoDefend := CheckBox_AutoDefence.Checked;
  gMySpectator.Hand.AI.Setup.DefendAllies := CheckBox_DefendAllies.Checked;
  gMySpectator.Hand.AI.Setup.AutoAttackRange := TrackBar_AutoAttackRange.Position;
  gMySpectator.Hand.AI.Setup.RecruitCount := TrackBar_RecruitCount.Position;
  gMySpectator.Hand.AI.Setup.RecruitDelay := NumEdit_RecruitDelay.Value * 600;
  gMySpectator.Hand.AI.General.DPDontRestock := not CheckBox_Restock.Checked;
  gCursor.MapEdDefPosGroupLevel := TKMGroupLevel(TrackBar_DefencePositionUType.Position);
  TrackBar_DefencePositionUType.ThumbText := gResTexts[DEF_POS_ARMY_THUMB_TX[gCursor.MapEdDefPosGroupLevel]];

  gCursor.MapEdDefPosSetGroup := CheckBox_DefencePositionGiveGroup.Checked;  //add groups with defence pos

  if not CheckBox_MaxSoldiers.Checked then
    gMySpectator.Hand.AI.Setup.MaxSoldiers := -1
  else
    gMySpectator.Hand.AI.Setup.MaxSoldiers := NumEdit_MaxSoldiers.Value;
  if Sender = Button_RemoveDefencePos then
  begin
     gMySpectator.Hand.AI.General.DefencePositions.Clear;
  end;

  Town_DefenceRefresh;
end;


procedure TKMMapEdTownDefence.Town_DefenceFormations(Sender: TObject);
begin
  FormationsPopUp.Show(gMySpectator.HandID);
end;


procedure TKMMapEdTownDefence.Town_DefenceRefresh;
var
  onlyAdvancedAIHand: Boolean;
begin
  onlyAdvancedAIHand := gGame.MapEditor.OnlyAdvancedAIHand(gMySpectator.HandID);

  CheckBox_AutoDefence.Checked := gMySpectator.Hand.AI.Setup.AutoDefend;
  CheckBox_DefendAllies.Checked := gMySpectator.Hand.AI.Setup.DefendAllies;
  TrackBar_AutoAttackRange.Position := gMySpectator.Hand.AI.Setup.AutoAttackRange;
  TrackBar_AutoAttackRange.Enabled := not onlyAdvancedAIHand;
  TrackBar_RecruitCount.Position := gMySpectator.Hand.AI.Setup.RecruitCount;
  TrackBar_RecruitCount.Enabled := not onlyAdvancedAIHand;
  NumEdit_RecruitDelay.Value := Round(gMySpectator.Hand.AI.Setup.RecruitDelay / 600);
  Button_EditFormations.Enabled := not onlyAdvancedAIHand;

  CheckBox_Restock.Checked := not gMySpectator.Hand.AI.General.DPDontRestock;

  CheckBox_MaxSoldiers.Checked := (gMySpectator.Hand.AI.Setup.MaxSoldiers >= 0);
  NumEdit_MaxSoldiers.Enabled := CheckBox_MaxSoldiers.Checked;
  NumEdit_MaxSoldiers.Value := Max(gMySpectator.Hand.AI.Setup.MaxSoldiers, 0);

  //Update Button_DefencePosAdd after CheckBox_AutoDefence has been set
  Button_DefencePosAdd.Enabled := not CheckBox_AutoDefence.Checked;

  if CheckBox_AutoDefence.Checked then
  begin
    Button_DefencePosAdd.Down := False;
    gCursor.Mode := cmNone;
  end;
  gCursor.MapEdSize := TrackBar_DPRange.Position;
  TrackBar_DefencePositionUType.Enabled := CheckBox_DefencePositionGiveGroup.Checked;
end;


procedure TKMMapEdTownDefence.Hide;
begin
  Panel_Defence.Hide;
end;


procedure TKMMapEdTownDefence.Show;
begin
  Town_DefenceAddClick(nil);
  Town_DefenceRefresh;
  Panel_Defence.Show;
end;


function TKMMapEdTownDefence.Visible: Boolean;
begin
  Result := Panel_Defence.Visible;
end;


procedure TKMMapEdTownDefence.UpdateState;
var GT : TKMGroupType;
begin
  Button_DefencePosAdd.Down := (gCursor.Mode = cmMarkers)
                                and (gCursor.Tag1 = MARKER_DEFENCE)
                                and not CheckBox_AutoDefence.Checked;

  if gMySpectator.Hand.Enabled then
  begin
    Button_AllUnitsCount.Caption := gMySpectator.Hand.AI.General.DefencePositions.GetAlUnitsCount.ToString;
    Button_AttackingUnitsCount.Caption := gMySpectator.Hand.AI.General.DefencePositions.GetAllAttackingUnitsCount.ToString;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      Button_DefencePosCount[GT].Caption := gMySpectator.Hand.AI.General.DefencePositions.GetAttackingPositionsCount(GT).ToString;
  end;
end;


procedure TKMMapEdTownDefence.UpdateHotkeys;
begin
  Button_DefencePosAdd.Hint  := GetHintWHotkey(TX_MAPED_AI_DEFENSE_HINT,             MAPED_SUBMENU_ACTIONS_HOTKEYS[0]);
  CheckBox_AutoDefence.Hint  := GetHintWHotkey(TX_MAPED_AI_DEFENSE_AUTO_HINT,        MAPED_SUBMENU_ACTIONS_HOTKEYS[1]);
  CheckBox_DefendAllies.Hint := GetHintWHotkey(TX_MAPED_AI_DEFEND_ALLIES_HINT,       MAPED_SUBMENU_ACTIONS_HOTKEYS[2]);
  CheckBox_MaxSoldiers.Hint  := GetHintWHotkey(TX_MAPED_AI_MAX_SOLDIERS_ENABLE_HINT, MAPED_SUBMENU_ACTIONS_HOTKEYS[3]);
  Button_EditFormations.Hint := GetHintWHotkey(TX_MAPED_AI_FORMATIONS,               MAPED_SUBMENU_ACTIONS_HOTKEYS[4]);
end;


procedure TKMMapEdTownDefence.UpdatePlayer(aIndex: TKMHandID);
begin
  if Panel_Defence.Visible then
    Show;
end;


end.
