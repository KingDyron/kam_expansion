unit KM_GUIMapEdPlayerGoalPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes,
  KM_CommonTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsSwitch, KM_ControlsDrop,
  KM_Defaults, KM_Pics, KM_AIGoals;

type
  TKMMapEdPlayerGoal = class
  private
    fOwner: TKMHandID;
    fIndex: Integer;

    procedure Goal_Save;
    procedure Goal_Change(Sender: TObject);
    procedure Goal_Close(Sender: TObject);
    procedure Goal_Refresh(const aGoal: TKMGoal);
    function GetVisible: Boolean;
    procedure Goal_NextPrev(Sender : TObject);
    procedure Goal_Duplicate(Sender : TObject);
  protected
    Panel_Goal: TKMPanel;
      Image_GoalFlag: TKMImage;
      Radio_GoalType: TKMRadioGroup;
      Radio_GoalCondition: TKMRadioGroup;
      NumEdit_GoalPlayer: TKMNumericEdit;
      NumEdit_Time: TKMNumericEdit;
      NumEdit_PosX: TKMNumericEdit;
      NumEdit_PosY: TKMNumericEdit;
      NumEdit_Radius: TKMNumericEdit;
      Label_GoalDescription: TKMLabel;
      Button_GoalAdd,
      Button_GoalDuplicate: TKMButton;
      Button_GoalOk: TKMButton;
      Button_GoalCancel: TKMButton;
      DropList_HouseType: TKMDropList;
      CheckBox_ShowMessage: TKMCheckBox;

      Button_GoalPrev, Button_GoalNext: TKMButton;
  public
    fOnDone: TNotifyEvent;
    OnListChange : TEvent;
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure Show(aPlayer: TKMHandID; aIndex: Integer);
  end;


implementation
uses
  KM_HandsCollection, KM_Hand,
  KM_RenderUI,
  KM_ResFonts, KM_ResTexts, KM_ResTypes,
  KM_ResHouses,
  KM_MapUtils, KM_MapTypes;

{ TKMGUIMapEdGoal }
constructor TKMMapEdPlayerGoal.Create(aParent: TKMPanel);
const
  SIZE_X = 700;
  SIZE_Y = 430;

var I : Integer;
begin
  inherited Create;

  Panel_Goal := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Goal.AnchorsCenter;
  Panel_Goal.Hide;

  TKMBevel.Create(Panel_Goal, -2000,  -2000, 5000, 5000);
  with TKMImage.Create(Panel_Goal, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain) do
    ImageStretch;
  TKMBevel.Create(Panel_Goal,   0,  0, SIZE_X, SIZE_Y);
  TKMLabel.Create(Panel_Goal, SIZE_X div 2, 10, gResTexts[TX_MAPED_GOALS_TITLE], fntOutline, taCenter);

  Button_GoalPrev := TKMButton.Create(Panel_Goal, 10, -15, 50, 25, '<<', bsGame);
  Button_GoalPrev.OnClick := Goal_NextPrev;
  Button_GoalNext := TKMButton.Create(Panel_Goal, Panel_Goal.Width - 60, -15, 50, 25, '>>', bsGame);
  Button_GoalNext.OnClick := Goal_NextPrev;

  Image_GoalFlag := TKMImage.Create(Panel_Goal, 10, 10, 0, 0, 30, rxGuiMain);

  TKMLabel.Create(Panel_Goal, 20, 40, 160, 0, gResTexts[TX_MAPED_GOALS_TYPE], fntMetal, taLeft);
  Radio_GoalType := TKMRadioGroup.Create(Panel_Goal, 20, 60, 160, 40, fntMetal);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_NONE], False, False);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_VICTORY]);
  Radio_GoalType.Add(gResTexts[TX_MAPED_GOALS_TYPE_SURVIVE]);
  Radio_GoalType.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 200, 40, 400, 0, gResTexts[TX_MAPED_GOALS_CONDITION], fntMetal, taLeft);
  Radio_GoalCondition := TKMRadioGroup.Create(Panel_Goal, 200, 60, 340, 150, fntMetal);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_NONE], False, False);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TUTORIAL], false, false);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TIME], true, true);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_BUILDS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_TROOPS]);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_UNKNOWN], False, false);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_ASSETS], true);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_SERFS], false, false);
  Radio_GoalCondition.Add(gResTexts[TX_MAPED_GOALS_CONDITION_ECONOMY], false, false);
  Radio_GoalCondition.Add(gResTexts[1885]);
  Radio_GoalCondition.Add(gResTexts[1886]);
  Radio_GoalCondition.Add(gResTexts[1887]);
  Radio_GoalCondition.Add(gResTexts[1888]);
  Radio_GoalCondition.Add(gResTexts[1889]);
  Radio_GoalCondition.Add(gResTexts[1938]);
  Radio_GoalCondition.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 480, 40, gResTexts[TX_MAPED_GOALS_PLAYER], fntMetal, taLeft);
  NumEdit_GoalPlayer := TKMNumericEdit.Create(Panel_Goal, 480, 60, 1, MAX_HANDS);
  NumEdit_GoalPlayer.OnChange := Goal_Change;


  DropList_HouseType := TKMDropList.Create(Panel_Goal, 480, 90, 150, 25, fntMetal, '', bsGame);
  for I := 0 to High(HOUSE_VICTORY_ORDER) do
  begin
    DropList_HouseType.Add(gResTexts[HOUSE_VICTORY_ORDER[I].TextID], I);
  end;

  DropList_HouseType.OnChange := Goal_Change;

  TKMLabel.Create(Panel_Goal, 480, 120, gResTexts[1892], fntMetal, taLeft);
  NumEdit_PosX := TKMNumericEdit.Create(Panel_Goal, 480, 140, 1, MAX_MAP_SIZE - 1);
  NumEdit_PosY := TKMNumericEdit.Create(Panel_Goal, NumEdit_PosX.Right + 5, 140, 1, MAX_MAP_SIZE - 1);

  TKMLabel.Create(Panel_Goal, 480, 165, gResTexts[682], fntMetal, taLeft);
  NumEdit_Radius := TKMNumericEdit.Create(Panel_Goal, 480, 185, 0, 10);

  TKMLabel.Create(Panel_Goal, 480, 210, gResTexts[1848], fntMetal, taLeft);
  NumEdit_Time := TKMNumericEdit.Create(Panel_Goal, 480, 230, 1, high(Integer));
  NumEdit_Time.OnChange := Goal_Change;

  NumEdit_PosX.OnChange := Goal_Change;
  NumEdit_PosY.OnChange := Goal_Change;
  NumEdit_Radius.OnChange := Goal_Change;

  NumEdit_PosX.AutoFocusable := false;
  NumEdit_PosY.AutoFocusable := false;
  NumEdit_Radius.AutoFocusable := false;

  NumEdit_PosX.Hint := gResTexts[1890];
  NumEdit_PosY.Hint := gResTexts[1891];

  CheckBox_ShowMessage := TKMCheckBox.Create(Panel_Goal, 480, 260, 150, 25, gResTexts[1925], fntMetal);
  CheckBox_ShowMessage.Hint := gResTexts[1926];
  CheckBox_ShowMessage.OnClick := Goal_Change;

  Label_GoalDescription := TKMLabel.Create(Panel_Goal, 20, 300, SIZE_X - 40, 20, '', fntMetal, taLeft);
  Label_GoalDescription.WordWrap := True;

  Button_GoalAdd := TKMButton.Create(Panel_Goal, 5, SIZE_Y - 50, 130, 30, gResTexts[2131], bsMenu);
  Button_GoalAdd.OnClick := Goal_Duplicate;
  Button_GoalDuplicate := TKMButton.Create(Panel_Goal, Button_GoalAdd.Right + 5, SIZE_Y - 50, 130, 30, gResTexts[2130], bsMenu);
  Button_GoalDuplicate.OnClick := Goal_Duplicate;

  Button_GoalOk := TKMButton.Create(Panel_Goal, Button_GoalDuplicate.Right + 5, SIZE_Y - 50, 130, 30, gResTexts[TX_MAPED_OK], bsMenu);
  Button_GoalOk.OnClick := Goal_Close;
  Button_GoalCancel := TKMButton.Create(Panel_Goal, Button_GoalOk.Right + 5, SIZE_Y - 50, 130, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
  Button_GoalCancel.OnClick := Goal_Close;
end;


procedure TKMMapEdPlayerGoal.Goal_Change(Sender: TObject);
var
  gc: TKMGoalCondition;
begin
  gc := TKMGoalCondition(Radio_GoalCondition.ItemIndex);
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist
  NumEdit_GoalPlayer.Enabled := not (gc in [gcTime, gcFindPlace, gcRevealPlace, gcSpecified]);

  DropList_HouseType.Enabled := gc = gcBuildingsType;
  NumEdit_Time.Enabled := gc = gcTime;
  NumEdit_PosX.Enabled := gc in [gcFindPlace, gcRevealPlace, gcSpecified];
  NumEdit_PosY.Enabled := gc in [gcFindPlace, gcRevealPlace, gcSpecified];
  NumEdit_Radius.Enabled := gc in [gcFindPlace];
  Radio_GoalType.Enabled := not (gc in [gcFindPlace, gcRevealPlace]);

  if NumEdit_GoalPlayer.Enabled then
    Label_GoalDescription.Caption := GetGoalDescription(gMySpectator.HandID, NumEdit_GoalPlayer.Value - 1,
                                                        TKMGoalType(Radio_GoalType.ItemIndex), gc,
                                                        gMySpectator.Hand.FlagTextColor,
                                                        gHands[NumEdit_GoalPlayer.Value - 1].FlagTextColor,
                                                        icRoyalYellow, icLightOrange, DropList_HouseType.ItemIndex)
  else
    Label_GoalDescription.Caption := '';
end;


function TKMMapEdPlayerGoal.GetVisible: Boolean;
begin
  Result := Panel_Goal.Visible;
end;

procedure TKMMapEdPlayerGoal.Goal_Save;
var
  G: TKMGoal;
begin
  //Copy Goal info from controls to Goals
  FillChar(G, SizeOf(G), #0); //Make sure unused fields like Message are zero, not random data
  G.GoalType := TKMGoalType(Radio_GoalType.ItemIndex);

  G.GoalTime := NumEdit_Time.Value;
  G.BuldingsType := DropList_HouseType.ItemIndex;

  G.GoalCondition := TKMGoalCondition(Radio_GoalCondition.ItemIndex);

  if G.GoalCondition in [gcFindPlace, gcRevealPlace] then
  begin
    G.GoalType := gltVictory;
  end;

  if G.GoalType = gltSurvive then
    G.GoalStatus := gsTrue
  else
    G.GoalStatus := gsFalse;
  G.HandIndex := NumEdit_GoalPlayer.Value - 1;

  if G.GoalCondition = gcTime then
    G.HandIndex := 0;
  G.Position.X := NumEdit_PosX.Value;
  G.Position.Y := NumEdit_PosY.Value;
  G.Radius := NumEdit_Radius.Value;
  G.MessageHasShown := not CheckBox_ShowMessage.Checked;
  gHands[fOwner].AI.Goals[fIndex] := G;
end;

procedure TKMMapEdPlayerGoal.Goal_Close(Sender: TObject);
var
  G: TKMGoal;
begin
  if Sender = Button_GoalOk then
    Goal_Save;

  Panel_Goal.Hide;
  fOnDone(Self);
end;


procedure TKMMapEdPlayerGoal.Goal_Refresh(const aGoal: TKMGoal);
begin

  Button_GoalPrev.Enabled := fIndex > 0;
  Button_GoalNext.Enabled := fIndex < gHands[fOwner].AI.Goals.Count - 1;

  Image_GoalFlag.FlagColor := gHands[fOwner].FlagColor;
  Radio_GoalType.ItemIndex := Byte(aGoal.GoalType);
  Radio_GoalCondition.ItemIndex := Byte(aGoal.GoalCondition);
  NumEdit_GoalPlayer.Value := aGoal.HandIndex + 1;
  DropList_HouseType.ItemIndex := aGoal.BuldingsType;
  NumEdit_Time.Value := aGoal.GoalTime;
  NumEdit_PosX.Value := aGoal.Position.X;
  NumEdit_PosY.Value := aGoal.Position.Y;
  NumEdit_Radius.Value := aGoal.Radius;
  CheckBox_ShowMessage.Checked := not aGoal.MessageHasShown;
  //Certain values disable certain controls
  Goal_Change(nil);
end;

procedure TKMMapEdPlayerGoal.Goal_NextPrev(Sender: TObject);
begin
  if Sender = Button_GoalPrev then
  begin
    Goal_Save;
    Show(fOwner, fIndex - 1);

  end
  else
  if Sender = Button_GoalNext then
  begin
    Goal_Save;
    Show(fOwner, fIndex + 1);
  end;
end;

procedure TKMMapEdPlayerGoal.Goal_Duplicate(Sender: TObject);
var
  G : TKMGoal;
begin
  if Sender = Button_GoalDuplicate then
  begin
    Goal_Save;
    G := gHands[fOwner].AI.Goals[fIndex];
    gHands[fOwner].AI.Goals.AddGoal(G);
    Show(fOwner, gHands[fOwner].AI.Goals.Count - 1);
    if Assigned(OnListChange) then
      OnListChange;
  end else
  begin
    Goal_Save;
    FillChar(G, SizeOf(G), #0);
    G.GoalType := gltVictory;
    G.GoalCondition := gcBuildings;
    G.Disabled := False;
    gHands[fOwner].AI.Goals.AddGoal(G);

    Show(fOwner, gHands[fOwner].AI.Goals.Count - 1);
    if Assigned(OnListChange) then
      OnListChange;
  end;
end;


function TKMMapEdPlayerGoal.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_GoalCancel.IsClickable then
                begin
                  Goal_Close(Button_GoalCancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_GoalOk.IsClickable then
                begin
                  Goal_Close(Button_GoalOk);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdPlayerGoal.Show(aPlayer: TKMHandID; aIndex: Integer);
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Goal_Refresh(gHands[fOwner].AI.Goals[fIndex]);
  Panel_Goal.Show;
end;


end.
