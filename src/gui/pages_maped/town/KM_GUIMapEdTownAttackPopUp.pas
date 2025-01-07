unit KM_GUIMapEdTownAttackPopUp;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsSwitch, KM_ControlsTrackBar,
  KM_Defaults, KM_Pics,
  KM_Points, KM_AIAttacks;

type
  TKMMapEdTownAttack = class
  private
    fOwner: TKMHandID;
    fIndex: Integer;
    procedure Attack_Change(Sender: TObject);
    procedure Attack_Close(Sender: TObject);
    procedure Attack_Refresh(aAttack: TKMAIAttack);
    procedure Attack_Save;
    procedure Attack_Switch(Sender: TObject);
    function GetVisible: Boolean;
  protected
    Panel_Attack: TKMPanel;
      Label_AttackHeader: TKMLabel;
      Button_Next: TKMButton;
      Button_Prev: TKMButton;
      Radio_AttackType: TKMRadioGroup;
      NumEdit_AttackDelay: TKMNumericEdit;
      NumEdit_AttackMen: TKMNumericEdit;
      NumEdit_AttackAmount: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMNumericEdit;
      CheckBox_AttackRandomGroups: TKMCheckBox;
      Radio_AttackTarget: TKMRadioGroup;
      TrackBar_AttackRange: TKMTrackBar;
      NumEdit_AttackLocX: TKMNumericEdit;
      NumEdit_AttackLocY: TKMNumericEdit;
      Button_AttackOk: TKMButton;
      Button_AttackCancel: TKMButton;

      Button_AllUnitsCount,
      Button_AttackingUnitsCount : TKMButtonFlat;
      Button_DefencePosCount : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMButtonFlat;
  public
    fOnDone: TNotifyEvent;
    constructor Create(aParent: TKMPanel);

    property Visible: Boolean read GetVisible;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
    procedure Show(aPlayer: TKMHandID; aIndex: Integer);
  end;


implementation
uses
  KM_HandsCollection, KM_Hand,
  KM_RenderUI,
  KM_ResTexts, KM_ResFonts, KM_ResTypes, KM_AITypes;


const
  GROUP_TEXT: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Integer = (
    TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE,
    TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED,
    1643, 1883, 1963, 2022);


{ TKMMapEdAttack }
constructor TKMMapEdTownAttack.Create(aParent: TKMPanel);
const
  SIZE_X = 750;
  SIZE_Y = 520;
var
  GT: TKMGroupType;
begin
  inherited Create;

  Panel_Attack := TKMPanel.Create(aParent, (aParent.Width - SIZE_X) div 2, (aParent.Height - SIZE_Y) div 2, SIZE_X, SIZE_Y);
  Panel_Attack.AnchorsCenter;
  Panel_Attack.Hide;

    TKMBevel.Create(Panel_Attack, -2000,  -2000, 5000, 5000);
    with TKMImage.Create(Panel_Attack, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain) do ImageStretch;
    TKMBevel.Create(Panel_Attack,   0,  0, SIZE_X, SIZE_Y);
    Label_AttackHeader := TKMLabel.Create(Panel_Attack, SIZE_X div 2, 10, gResTexts[TX_MAPED_AI_ATTACK_INFO], fntOutline, taCenter);

    Button_Prev := TKMButton.Create(Panel_Attack, 20, 10, 60, 20, '<<', bsMenu);
    Button_Prev.OnClick := Attack_Switch;
    Button_Next := TKMButton.Create(Panel_Attack, SIZE_X-20-60, 10, 60, 20, '>>', bsMenu);
    Button_Next.OnClick := Attack_Switch;

    TKMLabel.Create(Panel_Attack, 20, 40, gResTexts[TX_MAPED_AI_ATTACK_COL_TYPE], fntMetal, taLeft);
    Radio_AttackType := TKMRadioGroup.Create(Panel_Attack, 20, 60, 160, 40, fntGrey);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_ONCE]);
    Radio_AttackType.Add(gResTexts[TX_MAPED_AI_ATTACK_TYPE_REP]);
    Radio_AttackType.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 180, 40, 210, 40, gResTexts[TX_MAPED_AI_ATTACK_DELAY], fntMetal, taLeft).WordWrap := True;
    NumEdit_AttackDelay := TKMNumericEdit.Create(Panel_Attack, 180, 80, 0, High(SmallInt));
    NumEdit_AttackDelay.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, SIZE_X-20-320-10, 40, 320, 40, gResTexts[TX_MAPED_AI_ATTACK_SOLDIERS], fntMetal, taLeft).WordWrap := True;
    NumEdit_AttackMen := TKMNumericEdit.Create(Panel_Attack, 400, 80, 0, 1000);
    NumEdit_AttackMen.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 400, 210, gResTexts[TX_MAPED_AI_ATTACK_COUNT], fntMetal, taLeft);
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    begin
      TKMLabel.Create(Panel_Attack, 485, 230 + (Ord(GT) - GROUP_TYPE_MIN_OFF) * 20, 0, 0, gResTexts[GROUP_TEXT[GT]], fntGrey, taLeft);
      NumEdit_AttackAmount[GT] := TKMNumericEdit.Create(Panel_Attack, 400, 230 + (Ord(GT) - GROUP_TYPE_MIN_OFF) * 20, 0, 255);
      NumEdit_AttackAmount[GT].OnChange := Attack_Change;
    end;

    CheckBox_AttackRandomGroups := TKMCheckBox.Create(Panel_Attack, SIZE_X-20-320-10, NumEdit_AttackAmount[GROUP_TYPE_MAX].Bottom + 3, 320, 40, gResTexts[TX_MAPED_AI_ATTACK_TAKE_ANY], fntMetal);
    CheckBox_AttackRandomGroups.Hint := gResTexts[TX_MAPED_AI_ATTACK_TAKE_ANY_HINT];
    CheckBox_AttackRandomGroups.OnClick := Attack_Change;

    //Second row

    with TKMLabel.Create(Panel_Attack, 20, 120, SIZE_X - 40, 80, gResTexts[TX_MAPED_AI_ATTACK_HELP], fntMetal, taLeft) do
    begin
      FontColor := icGoldenYellow;
      WordWrap := True;
    end;

    //Third row

    TKMLabel.Create(Panel_Attack, 20, 210, gResTexts[TX_MAPED_AI_ATTACK_COL_TARGET], fntMetal, taLeft);
    Radio_AttackTarget := TKMRadioGroup.Create(Panel_Attack, 20, 230, 310, 80, fntGrey);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CLOSEST]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE_ARMY]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_HOUSE_START]);
    Radio_AttackTarget.Add(gResTexts[TX_MAPED_AI_TARGET_CUSTOM]);
    Radio_AttackTarget.OnChange := Attack_Change;

    NumEdit_AttackLocX := TKMNumericEdit.Create(Panel_Attack, 40, 310, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocX.OnChange := Attack_Change;
    NumEdit_AttackLocY := TKMNumericEdit.Create(Panel_Attack, 40, 330, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocY.OnChange := Attack_Change;

    //Range is not implemented yet (unused feature in KaM?)
    with TKMLabel.Create(Panel_Attack, 200, 250, 'Range (untested)', fntMetal, taLeft) do Hide;
    TrackBar_AttackRange := TKMTrackBar.Create(Panel_Attack, 200, 310, 100, 0, 255);
    TrackBar_AttackRange.Disable;
    TrackBar_AttackRange.Hide;
    TrackBar_AttackRange.OnChange := Attack_Change;

    Button_AttackOk := TKMButton.Create(Panel_Attack, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_OK], bsMenu);
    Button_AttackOk.OnClick := Attack_Close;
    Button_AttackCancel := TKMButton.Create(Panel_Attack, SIZE_X-20-160, SIZE_Y - 50, 160, 30, gResTexts[TX_MAPED_CANCEL], bsMenu);
    Button_AttackCancel.OnClick := Attack_Close;


    with TKMLabel.Create(Panel_Attack, 10, SIZE_Y - 140, 300, 40, gResTexts[2137], fntMetal, taLeft) do
      WordWrap := true;

    Button_AllUnitsCount := TKMButtonFlat.Create(Panel_Attack, 10, SIZE_Y - 100, 30, 35, 665);
    Button_AllUnitsCount.Hint := gResTexts[2133];
    Button_AttackingUnitsCount := TKMButtonFlat.Create(Panel_Attack, Button_AllUnitsCount.Right + 3, Button_AllUnitsCount.Top, 30, 35, 658);
    Button_AttackingUnitsCount.Hint := gResTexts[2134];
    TKMLabel.Create(Panel_Attack, 10, Button_AllUnitsCount.Bottom + 5, 350, 20, gResTexts[2135], fntMetal, taLeft);

    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    begin
      Button_DefencePosCount[GT] := TKMButtonFlat.Create(Panel_Attack, 10 + 33 * GROUP_TYPES[GT],
                                                      Button_AllUnitsCount.Bottom + 25, 30, 35, GROUP_TYPE_GUI_ICON[GT]);

      Button_DefencePosCount[GT].Hint := gResTexts[2136] + gResTexts[GROUP_TYPE_GUI_TEXT[GT]];
    end;
end;


procedure TKMMapEdTownAttack.Attack_Change(Sender: TObject);
var
  GT: TKMGroupType;
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    NumEdit_AttackAmount[GT].Enabled := not CheckBox_AttackRandomGroups.Checked;

  NumEdit_AttackLocX.Enabled := (TKMAIAttackTarget(Radio_AttackTarget.ItemIndex) = attCustomPosition);
  NumEdit_AttackLocY.Enabled := (TKMAIAttackTarget(Radio_AttackTarget.ItemIndex) = attCustomPosition);
end;


procedure TKMMapEdTownAttack.Attack_Close(Sender: TObject);
begin
  if Sender = Button_AttackOk then
    Attack_Save;

  Panel_Attack.Hide;
  fOnDone(Self);
end;


procedure TKMMapEdTownAttack.Attack_Refresh(aAttack: TKMAIAttack);
var
  GT: TKMGroupType;
begin
  Label_AttackHeader.Caption := gResTexts[TX_MAPED_AI_ATTACK_INFO] + ' (' + IntToStr(fIndex) + ')';

  //Set attack properties to UI
  Radio_AttackType.ItemIndex := Byte(aAttack.AttackType);
  NumEdit_AttackDelay.Value := aAttack.Delay div 10;
  NumEdit_AttackMen.Value := aAttack.TotalMen;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    NumEdit_AttackAmount[GT].Value := aAttack.GroupAmounts[GT];
  CheckBox_AttackRandomGroups.Checked := aAttack.RandomGroups;
  Radio_AttackTarget.ItemIndex := Byte(aAttack.Target);
  TrackBar_AttackRange.Position := aAttack.Range;
  NumEdit_AttackLocX.Value := aAttack.CustomPosition.X;
  NumEdit_AttackLocY.Value := aAttack.CustomPosition.Y;

  //Certain values disable certain controls
  Attack_Change(nil);
end;


procedure TKMMapEdTownAttack.Attack_Save;
var
  AA: TKMAIAttack;
  GT: TKMGroupType;
begin
  //Copy attack info from controls to Attacks
  AA.AttackType := TKMAIAttackType(Radio_AttackType.ItemIndex);
  AA.Delay := NumEdit_AttackDelay.Value * 10;
  AA.TotalMen := NumEdit_AttackMen.Value;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    AA.GroupAmounts[GT] := NumEdit_AttackAmount[GT].Value;
  AA.RandomGroups := CheckBox_AttackRandomGroups.Checked;
  AA.Target := TKMAIAttackTarget(Radio_AttackTarget.ItemIndex);
  AA.Range := TrackBar_AttackRange.Position;
  AA.CustomPosition := KMPoint(NumEdit_AttackLocX.Value, NumEdit_AttackLocY.Value);

  gHands[fOwner].AI.General.Attacks[fIndex] := AA;
end;


//Show previous or next attack
//We save changes before switching
procedure TKMMapEdTownAttack.Attack_Switch(Sender: TObject);
var
  atCount: Integer;
begin
  Attack_Save;

  atCount := gHands[fOwner].AI.General.Attacks.Count;

  if Sender = Button_Prev then
    fIndex := (fIndex + atCount - 1) mod atCount
  else
    fIndex := (fIndex + 1) mod atCount;

  Attack_Refresh(gHands[fOwner].AI.General.Attacks[fIndex]);
end;


function TKMMapEdTownAttack.GetVisible: Boolean;
begin
  Result := Panel_Attack.Visible;
end;


function TKMMapEdTownAttack.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := False;
  case Key of
    VK_ESCAPE:  if Button_AttackCancel.IsClickable then
                begin
                  Attack_Close(Button_AttackCancel);
                  Result := True;
                end;
    VK_RETURN:  if Button_AttackOk.IsClickable then
                begin
                  Attack_Close(Button_AttackOk);
                  Result := True;
                end;
  end;
end;


procedure TKMMapEdTownAttack.Show(aPlayer: TKMHandID; aIndex: Integer);
var GT : TKMGroupType;
begin
  fOwner := aPlayer;
  fIndex := aIndex;

  Attack_Refresh(gHands[fOwner].AI.General.Attacks[fIndex]);
  Panel_Attack.Show;

  if gHands[fOwner].Enabled then
  begin
    Button_AllUnitsCount.Caption := gHands[fOwner].AI.General.DefencePositions.GetAlUnitsCount.ToString;
    Button_AttackingUnitsCount.Caption := gHands[fOwner].AI.General.DefencePositions.GetAllAttackingUnitsCount.ToString;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      Button_DefencePosCount[GT].Caption := gHands[fOwner].AI.General.DefencePositions.GetAttackingPositionsCount(GT).ToString;
  end;
end;


end.
