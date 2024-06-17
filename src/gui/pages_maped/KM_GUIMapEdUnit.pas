unit KM_GUIMapEdUnit;
{$I KaM_Remake.inc}
interface
uses
   Classes, KromUtils, Math, StrUtils, SysUtils,
   Controls,
   KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsEdit, KM_ControlsProgressBar,
   KM_Defaults, KM_Pics, KM_Units, KM_UnitGroup, KM_ControlsSwitch,
   KM_Points, KM_InterfaceGame;

type
  TKMMapEdUnit = class
  private
    fUnit: TKMUnit;
    fGroup: TKMUnitGroup;

    procedure SetUnitsPerRaw(aValue: Integer);
    procedure SetPositionUnitConditions(aValue: Integer);

    procedure Unit_ArmyChange1(Sender: TObject);
    procedure Unit_ArmyChangeShift(Sender: TObject; Shift: TShiftState);
    procedure Unit_ArmyChange2(Sender: TObject; Shift: TShiftState);
    procedure Unit_ArmyClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
    procedure UnitConditionsChange(Sender: TObject; Shift: TShiftState);
    procedure UnitConditionsClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
    procedure UnitFishCntChange(Sender: TObject);
    procedure ColorCodeChange(Sender : TObject);
  protected
    Panel_Unit: TKMPanel;
    Label_UnitName: TKMLabel;
    Label_UnitCondition: TKMLabel;
    Label_UnitDescription: TKMLabel;
    ConditionBar_Unit: TKMPercentBar;
    Button_ConditionInc, Button_ConditionDefault, Button_ConditionDec: TKMButton;
    Image_UnitPic: TKMImage;
    Label_FishCount : TKMLabel;
    Edit_FishCount: TKMNumericEdit;
    CheckBox_Boots: TKMCheckBox;
    Label_FlagColor: TKMLabel;
    Edit_UnitFlagColor: TKMEdit;
    Shape_FlagColor: TKMShape;

    Panel_Army: TKMPanel;
    Button_Army_RotCW, Button_Army_RotCCW: TKMButton;
    Button_Army_ForUp, Button_Army_ForDown: TKMButton;
    ImageStack_Army: TKMImageStack;
    Label_ArmyCount: TKMLabel;
    Button_ArmyDec, Button_ArmyFood, Button_ArmyInc: TKMButton;
    DropBox_ArmyOrder: TKMDropList;
    Edit_ArmyOrderX: TKMNumericEdit;
    Edit_ArmyOrderY: TKMNumericEdit;
    Edit_ArmyOrderDir: TKMNumericEdit;
    CheckBox_NeverHungry: TKMCheckBox;
    CheckBox_InfiniteAmmo: TKMCheckBox;
    CheckBox_BitinAdded: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);

    procedure Show(aUnit: TKMUnit); overload;
    procedure Show(aGroup: TKMUnitGroup); overload;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  KM_ControlsTypes,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_RenderUI,
  KM_Resource, KM_ResFonts, KM_ResTexts, KM_ResUnits, KM_ResTypes,
  KM_Game,
  KM_UtilsExt, KM_Terrain,
  KM_UnitGroupTypes,
  KM_InterfaceTypes,
  KM_MapEdTypes;


{ TKMMapEdUnit }
constructor TKMMapEdUnit.Create(aParent: TKMPanel);
begin
  inherited Create;

  Panel_Unit := TKMPanel.Create(aParent, TB_PAD, 45, TB_MAP_ED_WIDTH - TB_PAD, 400);
  Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,Panel_Unit.Width,0,'',fntOutline,taCenter);
  Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
  Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,0,gResTexts[TX_UNIT_CONDITION],fntGrey,taCenter);

  ConditionBar_Unit       := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
  Button_ConditionDec     := TKMButton.Create(Panel_Unit,65,78,20,20,'-', bsGame);
  Button_ConditionInc     := TKMButton.Create(Panel_Unit,161,78,20,20,'+', bsGame);
  Button_ConditionDefault := TKMButton.Create(Panel_Unit,86,78,74,20,gResTexts[TX_UNIT_CONDITION_DEFAULT], bsGame);

  CheckBox_NeverHungry := TKMCheckBox.Create(Panel_Unit, 65, 103, Panel_Unit.Width - 65, 25, gResTexts[1810], fntMetal);
  CheckBox_NeverHungry.OnClickShift := UnitConditionsChange;
  CheckBox_NeverHungry.Hide;

  CheckBox_Boots := TKMCheckBox.Create(Panel_Unit, 65, 128, Panel_Unit.Width - 65, 25, gResTexts[1819], fntMetal);
  CheckBox_Boots.OnClickShift := UnitConditionsChange;
  CheckBox_Boots.Hide;

  CheckBox_InfiniteAmmo := TKMCheckBox.Create(Panel_Unit, 65, 128, Panel_Unit.Width - 65, 25, gResTexts[1870], fntMetal);
  CheckBox_InfiniteAmmo.OnClickShift := UnitConditionsChange;
  CheckBox_InfiniteAmmo.Hide;
  CheckBox_InfiniteAmmo.Hint := gResTexts[1871];

  CheckBox_BitinAdded := TKMCheckBox.Create(Panel_Unit, 65, 128, Panel_Unit.Width - 65, 25, gResTexts[1923], fntMetal);
  CheckBox_BitinAdded.OnClickShift := UnitConditionsChange;
  CheckBox_BitinAdded.Hide;
  CheckBox_BitinAdded.Hint := gResTexts[1924];

  Button_ConditionDec.OnClickShift := UnitConditionsChange;
  Button_ConditionInc.OnClickShift := UnitConditionsChange;
  Button_ConditionDec.OnClickHold  := UnitConditionsClickHold;
  Button_ConditionInc.OnClickHold  := UnitConditionsClickHold;
  Button_ConditionDefault.OnClickShift  := UnitConditionsChange;

  Label_FlagColor := TKMLabel.Create(Panel_Unit,0,180,Panel_Unit.Width,20,gResTexts[1884],fntMetal,taLeft);//flag color

  Edit_UnitFlagColor := TKMEdit.Create(Panel_Unit, 25, 200, 75, 20, fntMetal);
  Edit_UnitFlagColor.AutoFocusable := False; // No need to make too much attention on that field
  Edit_UnitFlagColor.Anchors := [anLeft, anTop, anRight];
  Edit_UnitFlagColor.AllowedChars := acHex;
  Edit_UnitFlagColor.MaxLen := 6;
  Edit_UnitFlagColor.OnChange := ColorCodeChange;

  TKMBevel.Create(Panel_Unit, 3, 200, 20, 20);
  Shape_FlagColor := TKMShape.Create(Panel_Unit, 4, 201, 18, 18);

  Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,225,Panel_Unit.Width,200,'',fntGrey,taLeft); //Taken from LIB resource
  Label_UnitDescription.WordWrap := True;
  Label_FishCount := TKMLabel.Create(Panel_Unit, 9, 55, 100, 20, gResTexts[1593], fntMetal, taLeft);
  Edit_FishCount := TKMNumericEdit.Create(Panel_Unit, 9, 75, 1, FISH_CNT_MAX);
  Edit_FishCount.AutoFocusable := False;
  Edit_FishCount.OnChange := UnitFishCntChange;
  Edit_FishCount.Value := FISH_CNT_DEFAULT;

  Panel_Army := TKMPanel.Create(Panel_Unit, 0, 230, Panel_Unit.Width, 400);
  Button_Army_RotCCW  := TKMButton.Create(Panel_Army,       0,  0, 56, 40, 23, rxGui, bsGame);
  Button_Army_RotCW   := TKMButton.Create(Panel_Army,     124,  0, 56, 40, 24, rxGui, bsGame);
  Button_Army_ForUp   := TKMButton.Create(Panel_Army,       0, 46, 56, 40, 33, rxGui, bsGame);
  ImageStack_Army     := TKMImageStack.Create(Panel_Army,  62, 46, 56, 40, 43, 50);
  Label_ArmyCount     := TKMLabel.Create(Panel_Army,       62, 60, 56, 20, '-', fntOutline, taCenter);
  Button_Army_ForDown := TKMButton.Create(Panel_Army,     124, 46, 56, 40, 32, rxGui, bsGame);
  Button_Army_RotCW.OnClickShift   := Unit_ArmyChangeShift;
  Button_Army_RotCCW.OnClickShift  := Unit_ArmyChangeShift;
  Button_Army_ForUp.OnClickShift   := Unit_ArmyChangeShift;
  Button_Army_ForDown.OnClickShift := Unit_ArmyChangeShift;

  Button_ArmyDec  := TKMButton.Create(Panel_Army,  0,92,56,40,'-', bsGame);
  Button_ArmyFood := TKMButton.Create(Panel_Army, 62,92,56,40,29, rxGui, bsGame);
  Button_ArmyInc  := TKMButton.Create(Panel_Army,124,92,56,40,'+', bsGame);
  Button_ArmyDec.OnClickShift := Unit_ArmyChange2;
  Button_ArmyFood.OnClick     := Unit_ArmyChange1;
  Button_ArmyInc.OnClickShift := Unit_ArmyChange2;

  Button_Army_ForUp.OnClickHold   := Unit_ArmyClickHold;
  Button_Army_ForDown.OnClickHold := Unit_ArmyClickHold;
  Button_Army_RotCW.OnClickHold   := Unit_ArmyClickHold;
  Button_Army_RotCCW.OnClickHold  := Unit_ArmyClickHold;
  Button_ArmyDec.OnClickHold      := Unit_ArmyClickHold;
  Button_ArmyInc.OnClickHold      := Unit_ArmyClickHold;

  //Group order
  //todo: Orders should be placed with a cursor (but keep numeric input as well?)
  TKMLabel.Create(Panel_Army, 0, 140, Panel_Army.Width, 0, gResTexts[TX_MAPED_GROUP_ORDER], fntOutline, taLeft);
  DropBox_ArmyOrder := TKMDropList.Create(Panel_Army, 0, 160, Panel_Army.Width, 20, fntMetal, '', bsGame);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_NONE]);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_WALK]);
  DropBox_ArmyOrder.Add(gResTexts[TX_MAPED_GROUP_ORDER_ATTACK]);
  DropBox_ArmyOrder.Add(gResTexts[1838]);
  DropBox_ArmyOrder.OnChange := Unit_ArmyChange1;

  TKMLabel.Create(Panel_Army, 0, 185, 'X:', fntGrey, taLeft);
  Edit_ArmyOrderX := TKMNumericEdit.Create(Panel_Army, 20, 185, 1, MAX_MAP_SIZE - 1);
  Edit_ArmyOrderX.AutoFocusable := False;
  Edit_ArmyOrderX.OnChange := Unit_ArmyChange1;
  TKMLabel.Create(Panel_Army, 0, 205, 'Y:', fntGrey, taLeft);
  Edit_ArmyOrderY := TKMNumericEdit.Create(Panel_Army, 20, 205, 1, MAX_MAP_SIZE - 1);
  Edit_ArmyOrderY.AutoFocusable := False;
  Edit_ArmyOrderY.OnChange := Unit_ArmyChange1;
  TKMLabel.Create(Panel_Army, 110, 185, gResTexts[TX_MAPED_GROUP_ORDER_DIRECTION], fntGrey, taLeft);
  Edit_ArmyOrderDir := TKMNumericEdit.Create(Panel_Army, 110, 205, 0, 7);
  Edit_ArmyOrderDir.OnChange := Unit_ArmyChange1;
end;


procedure TKMMapEdUnit.Show(aUnit: TKMUnit);
var C : Cardinal;
begin
  fUnit := aUnit;
  fGroup := nil;

  //Edit_UnitFlagColor.Visible := not (fUnit.UnitType in UNITS_ANIMALS);
  Edit_UnitFlagColor.Show;
  if fUnit.UnitType in UNITS_ANIMALS then
    Edit_UnitFlagColor.Hide;
  Label_FlagColor.Visible := Edit_UnitFlagColor.Visible;
  Shape_FlagColor.Visible := Edit_UnitFlagColor.Visible;

  if Edit_UnitFlagColor.Visible then
  begin
    C := fUnit.FlagColor;
    if C = 0 then
      C := gHands[fUnit.Owner].FlagColor;

    C := C or $FF000000;

    Edit_UnitFlagColor.SetTextSilently( Format('%.6x', [C and $FFFFFF]) );
    Shape_FlagColor.FillColor := C;
  end;

  Button_ConditionDefault.Enable;
  Button_ConditionInc.Enable;
  Button_ConditionDec.Enable;

  //Label_UnitDescription.Show;
  Panel_Unit.Show;
  CheckBox_NeverHungry.Visible := not fUnit.IsAnimal;
  ConditionBar_Unit.Visible := not fUnit.IsAnimal;
  Label_UnitCondition.Visible := not fUnit.IsAnimal;
  Button_ConditionInc.Visible := MAPED_SHOW_CONDITION_UNIT_BTNS and not fUnit.IsAnimal;
  Button_ConditionDec.Visible := MAPED_SHOW_CONDITION_UNIT_BTNS and not fUnit.IsAnimal;
  Button_ConditionDefault.Visible := MAPED_SHOW_CONDITION_UNIT_BTNS and not fUnit.IsAnimal;
  Button_ConditionDefault.Enabled := not fUnit.StartWDefaultCondition;
  Panel_Army.Hide;
  CheckBox_InfiniteAmmo.Hide;
  CheckBox_BitinAdded.Hide;
  CheckBox_Boots.Visible := not fUnit.IsAnimal;
  CheckBox_Boots.Checked := aUnit.BootsAdded;
  CheckBox_NeverHungry.Checked := fUnit.NeverHungry;

  SortVisibleControls(65, 103, 0, 0, [CheckBox_NeverHungry, CheckBox_Boots]);


  Edit_FishCount.Visible := fUnit is TKMUnitFish;
  Label_FishCount.Visible := Edit_FishCount.Visible;
  if Edit_FishCount.Visible then
    Edit_FishCount.Value := TKMUnitFish(fUnit).FishCount;

  if fUnit = nil then Exit;

  Label_UnitName.Caption := gRes.Units[fUnit.UnitType].GUIName;

  if fUnit.IsAnimal then
  begin
    Image_UnitPic.TexID := 0;
    Image_UnitPic.FlagColor := icGray;
    if fUnit is TKMUnitFish then
      Label_UnitDescription.Caption := gResTexts[TX_MAPED_FISH_COUNT]
    else
      Label_UnitDescription.Caption := '';
  end
  else
  begin
    Image_UnitPic.TexID := gRes.Units[fUnit.UnitType].GUIScroll;
    Image_UnitPic.FlagColor := gHands[fUnit.Owner].FlagColor;
    SetPositionUnitConditions(fUnit.Condition);
    Label_UnitDescription.Caption := gRes.Units[fUnit.UnitType].Description;
  end;



end;


procedure TKMMapEdUnit.Show(aGroup: TKMUnitGroup);
var C : Cardinal;
begin
  fUnit := nil;
  fGroup := aGroup;


  Label_UnitDescription.Hide;
  Panel_Unit.Show;
  ConditionBar_Unit.Show;
  Label_UnitCondition.Show;
  Button_ConditionInc.Show;
  Button_ConditionDec.Show;
  Button_ConditionDefault.Show;
  Button_ConditionDefault.Enabled := not fGroup.FlagBearer.StartWDefaultCondition;
  CheckBox_NeverHungry.Checked := fGroup.NeverHungry;
  CheckBox_NeverHungry.Show;
  Edit_FishCount.Hide;
  Label_FishCount.Hide;
  CheckBox_Boots.Hide;
  CheckBox_InfiniteAmmo.Hide;
  Panel_Army.Show;

  if fGroup = nil then Exit;
  CheckBox_BitinAdded.Visible := fGroup.UnitType in WARRIOR_BITIN_EQUIPABLE;

  if fGroup.IsRanged then
  begin
    CheckBox_InfiniteAmmo.Show;
    CheckBox_InfiniteAmmo.Checked := fGroup.FlagBearer.InfinityAmmo;
  end;
  CheckBox_BitinAdded.Checked := fGroup.FlagBearer.BitinAdded;
  SortVisibleControls(65, 103, 0, 0, [CheckBox_NeverHungry, CheckBox_Boots, CheckBox_InfiniteAmmo, CheckBox_BitinAdded]);

  Label_UnitName.Caption := gRes.Units[fGroup.UnitType].GUIName;
  Image_UnitPic.TexID := gRes.Units[fGroup.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := gHands[fGroup.Owner].FlagColor;

  if fGroup.UnitType in SIEGE_MACHINES then
    SetPositionUnitConditions(UNIT_MAX_CONDITION)
  else
    SetPositionUnitConditions(fGroup.Condition);

  //Warrior specific
  Edit_ArmyOrderX.ValueMax := gTerrain.MapX - 1;
  Edit_ArmyOrderY.ValueMax := gTerrain.MapY - 1;

  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);
  DropBox_ArmyOrder.ItemIndex := Byte(fGroup.MapEdOrder.Order);
  Edit_ArmyOrderX.Value := fGroup.MapEdOrder.Pos.Loc.X;
  Edit_ArmyOrderY.Value := fGroup.MapEdOrder.Pos.Loc.Y;
  Edit_ArmyOrderDir.Value := Max(Byte(fGroup.MapEdOrder.Pos.Dir) - 1, 0);

  Edit_UnitFlagColor.Visible := fGroup.UnitType <> utSpy;
  Label_FlagColor.Visible := Edit_UnitFlagColor.Visible;
  Shape_FlagColor.Visible := Edit_UnitFlagColor.Visible;
  if Edit_UnitFlagColor.Visible then
  begin
    C := aGroup.FlagBearer.FlagColor;
    if C = 0 then
      C := gHands[fGroup.Owner].FlagColor;
    C := C or $FF000000;

    Edit_UnitFlagColor.SetTextSilently( Format('%.6x', [C and $FFFFFF]) );
    Shape_FlagColor.FillColor := C;
  end;

  Unit_ArmyChange1(nil);
  UnitConditionsChange(nil, []);
end;


procedure TKMMapEdUnit.UnitConditionsChange(Sender: TObject; Shift: TShiftState);
var
  U: TKMUnit;
  newCondition: Integer;
begin
  if fUnit = nil then
    U := fGroup.FlagBearer
  else
    U := fUnit;
  if Sender = CheckBox_Boots then
  begin
    U.BootsAdded := CheckBox_Boots.Checked;
    Exit;
  end;
  newCondition := U.Condition;
  if Sender = CheckBox_BitinAdded then
  begin
      fGroup.FlagBearer.BitinAdded := CheckBox_BitinAdded.Checked;
  end else
  if Sender = CheckBox_InfiniteAmmo then
  begin
      fGroup.FlagBearer.InfinityAmmo := CheckBox_InfiniteAmmo.Checked;
  end else
  if Sender = CheckBox_NeverHungry then
  begin
    if fUnit <> nil then
      fUnit.NeverHungry := CheckBox_NeverHungry.Checked
    else
      fGroup.NeverHungry := CheckBox_NeverHungry.Checked;
  end else
  if Sender = Button_ConditionDefault then
    U.StartWDefaultCondition := not U.StartWDefaultCondition
  else if Sender = Button_ConditionInc then
  begin
    newCondition := U.Condition + GetMultiplicator(Shift);
    U.StartWDefaultCondition := False;
    Button_ConditionDefault.Enable;
  end else if Sender = Button_ConditionDec then
  begin
    newCondition := U.Condition - GetMultiplicator(Shift);
    U.StartWDefaultCondition := False;
    Button_ConditionDefault.Enable;
  end else if Sender = Button_ArmyFood then
  begin
    if fGroup.Condition = UNIT_MAX_CONDITION then
    begin
      newCondition := TKMUnitGroup.GetDefaultCondition;
      U.StartWDefaultCondition := True;
      Button_ConditionDefault.Disable;
    end else
    begin
      newCondition := UNIT_MAX_CONDITION;
      U.StartWDefaultCondition := False;
      Button_ConditionDefault.Enable;
    end;
  end;

  if fGroup <> nil then
  begin
    fGroup.Condition := newCondition;
    if newCondition = TKMUnitGroup.GetDefaultCondition then
      U.StartWDefaultCondition := True;
  end
  else
  begin
    fUnit.Condition := newCondition;
    if newCondition = TKMUnit.GetDefaultCondition then
      U.StartWDefaultCondition := True;
  end;

  if U.StartWDefaultCondition then
  begin
    if fGroup <> nil then
      fGroup.Condition := TKMUnitGroup.GetDefaultCondition
    else
      fUnit.Condition := TKMUnit.GetDefaultCondition;
    Button_ConditionDefault.Disable;
  end;
  if U.UnitType in SIEGE_MACHINES then
    SetPositionUnitConditions(UNIT_MAX_CONDITION)
  else
    SetPositionUnitConditions(U.Condition);

  if CheckBox_NeverHungry.Visible then
  begin
    Button_ConditionDefault.Enabled := not CheckBox_NeverHungry.Checked;
    Button_ConditionInc.Enabled := not CheckBox_NeverHungry.Checked ;
    Button_ConditionDec.Enabled := not CheckBox_NeverHungry.Checked;
    Button_ArmyFood.Enabled := not CheckBox_NeverHungry.Checked;
  end;
end;


procedure TKMMapEdUnit.UnitConditionsClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
begin
  if (Sender = Button_ConditionDec)
    or (Sender = Button_ConditionInc) then
    UnitConditionsChange(Sender, GetShiftState(aButton));
end;


procedure TKMMapEdUnit.UnitFishCntChange(Sender: TObject);
begin
  Assert(fUnit is TKMUnitFish);

  TKMUnitFish(fUnit).FishCount := Edit_FishCount.Value;
  gGame.MapEditor.Deposits.UpdateAreas([rdFish]);
end;


procedure TKMMapEdUnit.Unit_ArmyChange1(Sender: TObject);
begin
  // Use empty shift state, because value will change too fast otherwise
  Unit_ArmyChangeShift(Sender, []);
end;


procedure TKMMapEdUnit.SetPositionUnitConditions(aValue: Integer);
begin
  ConditionBar_Unit.Caption :=  IntToStr(aValue) + '/' + IntToStr(UNIT_MAX_CONDITION);
    ConditionBar_Unit.Position := aValue / UNIT_MAX_CONDITION;

end;


procedure TKMMapEdUnit.SetUnitsPerRaw(aValue: Integer);
begin
  fGroup.UnitsPerRow := Max(aValue, 1);
end;


procedure TKMMapEdUnit.Unit_ArmyChangeShift(Sender: TObject; Shift: TShiftState);
var
  rotCnt: Integer;
begin
  if Sender = Button_Army_ForUp then
    SetUnitsPerRaw(fGroup.UnitsPerRow - GetMultiplicator(Shift, RMB_ADD_ROWS_CNT));

  if Sender = Button_Army_ForDown then
    SetUnitsPerRaw(fGroup.UnitsPerRow + GetMultiplicator(Shift, RMB_ADD_ROWS_CNT));

  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);

  if (Sender = Button_Army_RotCW) or (Sender = Button_Army_RotCCW) then
  begin
    // 180 degrees by Shift
    if ssShift in Shift then
      rotCnt := 4
    else
    // 90 degrees by RMB
    if IsRMBInShiftState(Shift) then
      rotCnt := 2
    else
      rotCnt := 1;

    rotCnt := rotCnt * (2 * Byte(Sender = Button_Army_RotCW) - 1);

    fGroup.Direction := KMAddDirection(fGroup.Direction, rotCnt);
  end;

  fGroup.ResetAnimStep;

  //Toggle between full and half condition
  if Sender = Button_ArmyFood then
    UnitConditionsChange(Sender, []);

  fGroup.MapEdOrder.Order := TKMGroupInitialOrder(DropBox_ArmyOrder.ItemIndex);
  fGroup.MapEdOrder.Pos.Loc.X := Edit_ArmyOrderX.Value;
  fGroup.MapEdOrder.Pos.Loc.Y := Edit_ArmyOrderY.Value;
  fGroup.MapEdOrder.Pos.Dir := TKMDirection(Edit_ArmyOrderDir.Value + 1);
  if DropBox_ArmyOrder.ItemIndex = 0 then
  begin
    Edit_ArmyOrderX.Disable;
    Edit_ArmyOrderY.Disable;
    Edit_ArmyOrderDir.Disable;
  end
  else
  if DropBox_ArmyOrder.ItemIndex = 2 then
  begin
    Edit_ArmyOrderX.Enable;
    Edit_ArmyOrderY.Enable;
    Edit_ArmyOrderDir.Disable; //Attack position doesn't let you set direction
  end
  else
  begin
    Edit_ArmyOrderX.Enable;
    Edit_ArmyOrderY.Enable;
    Edit_ArmyOrderDir.Enable;
  end;
end;


procedure TKMMapEdUnit.Unit_ArmyClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
begin
  if (Sender = Button_ArmyDec)
    or (Sender = Button_ArmyInc) then
    Unit_ArmyChange2(Sender, GetShiftState(AButton));

  if (Sender = Button_Army_ForUp)
    or (Sender = Button_Army_ForDown) then
    Unit_ArmyChangeShift(Sender, GetShiftState(AButton));

  if (Sender = Button_Army_RotCW)
    or (Sender = Button_Army_RotCCW) then
    Unit_ArmyChange1(Sender);
end;


procedure TKMMapEdUnit.Unit_ArmyChange2(Sender: TObject; Shift: TShiftState);
var
  newCount: Integer;
begin
  if Sender = Button_ArmyDec then
    //Decrease
    newCount := fGroup.MapEdCount - GetMultiplicator(Shift)
  else
    //Increase
    newCount := fGroup.MapEdCount + GetMultiplicator(Shift);

  fGroup.MapEdCount := EnsureRange(newCount, 1, MAPED_GROUP_MAX_CNT); //Limit max members

  if ssCtrl in Shift then
    SetUnitsPerRaw(Ceil(Sqrt(1.5*fGroup.MapEdCount)));

  ImageStack_Army.SetCount(fGroup.MapEdCount, fGroup.UnitsPerRow, fGroup.UnitsPerRow div 2);
  Label_ArmyCount.Caption := IntToStr(fGroup.MapEdCount);
end;


procedure TKMMapEdUnit.Hide;
begin
  Panel_Unit.Hide;
end;


procedure TKMMapEdUnit.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  if (Key = VK_ESCAPE)
    and Visible
    and (gMySpectator.Selected <> nil) then
    begin
      gMySpectator.Selected := nil;
      Hide;
      aHandled := True;
    end;
end;


function TKMMapEdUnit.Visible: Boolean;
begin
  Result := Panel_Unit.Visible;
end;

procedure TKMMapEdUnit.ColorCodeChange(Sender: TObject);
var C : Cardinal;
begin
  if Sender <> Edit_UnitFlagColor then
    Exit;
  Edit_UnitFlagColor.SetTextSilently(UpperCase(Edit_UnitFlagColor.Text));
  if length(Edit_UnitFlagColor.Text) > 0 then
  begin
    C := StrToInt('$' + Edit_UnitFlagColor.Text);
    C := C or $FF000000;

    Shape_FlagColor.FillColor := C;

    if fGroup <> nil then
      fGroup.SetFlagColor(C)
    else
    if fUnit <> nil then
      fUnit.FlagColor := C;
    //Edit_UnitFlagColor.SetTextSilently( Format('%.6x', [C and $FFFFFF]) );
    //Shape_FlagColor.FillColor := C;
  end;
end;

end.
