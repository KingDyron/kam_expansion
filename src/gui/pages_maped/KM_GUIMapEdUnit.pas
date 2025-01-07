unit KM_GUIMapEdUnit;
{$I KaM_Remake.inc}
interface
uses
   Classes, KromUtils, Math, StrUtils, SysUtils,
   Controls,
   KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsEdit, KM_ControlsProgressBar, KM_ControlsList,
   KM_ControlsPopUp, KM_ControlsScroll,
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

    procedure BoatWaresChange(Sender : TObject);
    procedure RefreshBoat;
    procedure UpdateWare;
    procedure SelectBoatWare(aIndex : Integer);

    procedure ShipClick(Sender : TObject);
    procedure RefreshShipList;
    procedure UpdateShipUnit;
    procedure SelectShipUnit(aIndex : Integer);
  protected
    Panel_Unit: TKMScrollPanel;
    Label_UnitName: TKMLabel;
    Label_UnitCondition: TKMLabel;
    Label_UnitDescription: TKMLabel;
    ConditionBar_Unit: TKMPercentBar;
    Button_ConditionInc, Button_ConditionDefault, Button_ConditionDec: TKMButton;
    Image_UnitPic: TKMImage;
    Label_FishCount : TKMLabel;
    Edit_FishCount: TKMNumericEdit;
    CheckBox_Boots: TKMCheckBox;
    CheckBox_Immortal : TKMCheckBox;
    Label_FlagColor: TKMLabel;
    Edit_UnitFlagColor: TKMEdit;
    Bevel_FlagColor : TKMBevel;
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

    Button_Boat : TKMButton;
    PopUp_Boat : TKMPopUpRectPanel;
      ColumnBox_Wares : TKMColumnBox;
      DropList_Ware : TKMDropColumns;
      Edit_Count : TKMNumericEdit;
      Button_Add : TKMButton;
      Button_Remove : TKMButton;

    Button_Ship : TKMButton;

    PopUp_Ship : TKMPopUpRectPanel;
      ColumnBox_Units : TKMColumnBox;
      DropList_UnitType : TKMDropColumns;
      Edit_UnitCount,
      Edit_UnitColumns,
      Edit_UnitBoltCount,
      Edit_UnitCondition : TKMNumericEdit;
      Button_AddUnits : TKMButton;
      Button_RemoveUnits : TKMButton;
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
  KM_Resource, KM_ResFonts, KM_ResTexts, KM_ResUnits, KM_ResTypes, KM_ResWares,
  KM_Game,
  KM_UtilsExt, KM_Terrain,
  KM_UnitGroupTypes, KM_UnitWarrior,
  KM_InterfaceTypes,
  KM_MapEdTypes;


{ TKMMapEdUnit }
constructor TKMMapEdUnit.Create(aParent: TKMPanel);
var I, K : Integer;
  WT : TKMWareType;
  rw : TKMListRow;
  UT : TKMUnitType;
  unitOrder : TKMUnitTypeArray;
  unitAdded : array[TKMunitType] of Boolean;
begin
  inherited Create;

  Panel_Unit := TKMScrollPanel.Create(aParent, TB_PAD, 45, TB_MAP_ED_WIDTH - TB_PAD, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  Panel_Unit.ScrollV.Left := Panel_Unit.ScrollV.Left + 20;
  Panel_Unit.AnchorsStretch;


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

  CheckBox_Immortal := TKMCheckBox.Create(Panel_Unit, 65, 128, Panel_Unit.Width - 65, 25, gResTexts[2105], fntMetal);
  CheckBox_Immortal.OnClickShift := UnitConditionsChange;
  CheckBox_Immortal.Hide;

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

  Label_FlagColor := TKMLabel.Create(Panel_Unit,0,200,Panel_Unit.Width,20,gResTexts[1884],fntMetal,taLeft);//flag color

  Edit_UnitFlagColor := TKMEdit.Create(Panel_Unit, 25, 220, 75, 20, fntMetal);
  Edit_UnitFlagColor.AutoFocusable := False; // No need to make too much attention on that field
  Edit_UnitFlagColor.Anchors := [anLeft, anTop, anRight];
  Edit_UnitFlagColor.AllowedChars := acHex;
  Edit_UnitFlagColor.MaxLen := 6;
  Edit_UnitFlagColor.OnChange := ColorCodeChange;

  Bevel_FlagColor := TKMBevel.Create(Panel_Unit, 3, 220, 20, 20);
  Shape_FlagColor := TKMShape.Create(Panel_Unit, 4, 221, 18, 18);

  Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,255,Panel_Unit.Width,200,'',fntGrey,taLeft); //Taken from LIB resource
  Label_UnitDescription.WordWrap := True;
  Label_FishCount := TKMLabel.Create(Panel_Unit, 9, 55, 100, 20, gResTexts[1593], fntMetal, taLeft);
  Edit_FishCount := TKMNumericEdit.Create(Panel_Unit, 9, 75, 1, FISH_CNT_MAX);
  Edit_FishCount.AutoFocusable := False;
  Edit_FishCount.OnChange := UnitFishCntChange;
  Edit_FishCount.Value := FISH_CNT_DEFAULT;

  Panel_Army := TKMPanel.Create(Panel_Unit, 0, 250, Panel_Unit.Width, 400);
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

  Button_Boat := TKMButton.Create(Panel_Army, 10, Edit_ArmyOrderDir.Bottom + 10, Panel_Army.Width - 20, 25, gResTexts[2076], bsMenu);
  Button_Boat.OnClick := BoatWaresChange;


  PopUp_Boat := TKMPopUpRectPanel.Create(Panel_Army.MasterPanel, 400, 400);

  ColumnBox_Wares := TKMColumnBox.Create(PopUp_Boat, 10, 10, 200, 300, fntGrey, bsMenu);
  ColumnBox_Wares.SetColumns(fntOutline, ['', gResTexts[2074], gResTexts[1040]], [0, 25, 125]);
  ColumnBox_Wares.ShowHintWhenShort := true;
  ColumnBox_Wares.OnClick := BoatWaresChange;
  ColumnBox_Wares.OnChange := BoatWaresChange;


  Button_Add := TKMButton.Create(PopUp_Boat, 10, ColumnBox_Wares.Bottom + 5, 25, 25, '+', bsGame);
  Button_Add.OnClick := BoatWaresChange;
  Button_Remove := TKMButton.Create(PopUp_Boat, 40, ColumnBox_Wares.Bottom + 5, 25, 25, 'X', bsGame);
  Button_Remove.OnClick := BoatWaresChange;


  DropList_Ware := TKMDropColumns.Create(PopUp_Boat, ColumnBox_Wares.Right + 10, 10, 180, 25, fntGrey, gResTexts[2074], bsMenu);
  DropList_Ware.SetColumns(fntOutline, ['', gResTexts[2074]], [0, 30]);
  DropList_Ware.OnChange := BoatWaresChange;
  DropList_Ware.List.ShowHintWhenShort := true;
  DropList_Ware.List.ItemHeight := 25;

  for I := 2 to High(StoreResType) do
  begin
    WT := StoreResType[I];

    if WT = wtNone then
    begin
      rw := MakeListRow(['', '- - - - - - - - -'],//captions
                                    [$FFFFFFFF, $FFFFFFFF],//colors
                                    [MakePic(rxGui, 0), MakePic(rxGui, 0)],
                                    -1);

      for K := 0 to High(rw.Cells) do
        rw.Cells[K].Enabled := false;

      DropList_Ware.Add(rw);
      Continue;
    end;

    DropList_Ware.Add(MakeListRow(['', gRes.Wares[WT].Title],//captions
                                  [$FFFFFFFF, $FFFFFFFF],//colors
                                  [MakePic(rxGui, gRes.Wares[WT].GUIIcon), MakePic(rxGui, 0)], //pics
                                  byte(WT))
                      );
  end;
  TKMLabel.Create(PopUp_Boat, DropList_Ware.Left, 35, 150, 17, gResTexts[1040], fntGrey, taLeft);
  Edit_Count := TKMNumericEdit.Create(PopUp_Boat, DropList_Ware.Left, 53, 1, 50);
  Edit_Count.Width := 75;
  Edit_Count.Value := 50;
  Edit_Count.OnChange := BoatWaresChange;

  Button_Ship := TKMButton.Create(Panel_Army, 10, Edit_ArmyOrderDir.Bottom + 10, Panel_Army.Width - 20, 25, gResTexts[2075], bsMenu);
  Button_Ship.OnClick := ShipClick;


  PopUp_Ship := TKMPopUpRectPanel.Create(Panel_Army.MasterPanel, 400, 400);

  ColumnBox_Units := TKMColumnBox.Create(PopUp_Ship, 10, 10, 200, 300, fntGrey, bsMenu);
  ColumnBox_Units.SetColumns(fntOutline, ['', gResTexts[2077], gResTexts[1040]], [0, 25, 125]);
  ColumnBox_Units.ShowHintWhenShort := true;
  ColumnBox_Units.OnClick := ShipClick;
  ColumnBox_Units.OnChange := ShipClick;
  ColumnBox_Units.ItemHeight := 25;

  Button_AddUnits := TKMButton.Create(PopUp_Ship, 10, ColumnBox_Units.Bottom + 5, 25, 25, '+', bsGame);
  Button_AddUnits.OnClick := ShipClick;
  Button_RemoveUnits := TKMButton.Create(PopUp_Ship, 40, ColumnBox_Units.Bottom + 5, 25, 25, 'X', bsGame);
  Button_RemoveUnits.OnClick := ShipClick;


  DropList_UnitType := TKMDropColumns.Create(PopUp_Ship, ColumnBox_Units.Right + 10, 10, 180, 25, fntGrey, gResTexts[2077], bsMenu);
  DropList_UnitType.SetColumns(fntOutline, ['', gResTexts[2074]], [0, 30]);
  DropList_UnitType.OnChange := ShipClick;
  DropList_UnitType.List.ShowHintWhenShort := true;
  DropList_UnitType.List.ItemHeight := 25;

  unitOrder := SCHOOL_GAME_ORDER + BARRACKS_GAME_ORDER + TH_GAME_ORDER + SIEGE_GAME_ORDER + PALACE_UNITS_ORDER + SHIPYARD_ORDER;

  for I := 0 to High(unitOrder) do
  begin
    UT := unitOrder[I];

    if (gRes.Units[UT].ShipWeight <= 0) or unitAdded[UT] then
      Continue;

    unitAdded[UT] := true;
    DropList_UnitType.Add(MakeListRow(['', gRes.Units[UT].GUIName],//captions
                                  [$FFFFFFFF, $FFFFFFFF],//colors
                                  [MakePic(rxGui, gRes.Units[UT].GUIIcon), MakePic(rxGui, 0)], //pics
                                  byte(UT))
                      );
  end;
  TKMLabel.Create(PopUp_Ship, DropList_Ware.Left, 45, 150, 17, gResTexts[157], fntGrey, taLeft);
  Edit_UnitCondition := TKMNumericEdit.Create(PopUp_Ship, DropList_Ware.Left, 63, 1, 100);
  Edit_UnitCondition.Width := 75;
  Edit_UnitCondition.Value := 1;
  Edit_UnitCondition.OnChange := ShipClick;

  TKMLabel.Create(PopUp_Ship, DropList_Ware.Left, 85, 150, 17, gResTexts[1040], fntGrey, taLeft);
  Edit_UnitCount := TKMNumericEdit.Create(PopUp_Ship, DropList_Ware.Left, 103, 1, 15);
  Edit_UnitCount.Width := 75;
  Edit_UnitCount.Value := 1;
  Edit_UnitCount.OnChange := ShipClick;

  TKMLabel.Create(PopUp_Ship, DropList_Ware.Left, 125, 150, 17, gResTexts[450], fntGrey, taLeft);
  Edit_UnitColumns := TKMNumericEdit.Create(PopUp_Ship, DropList_Ware.Left, 143, 1, 5);
  Edit_UnitColumns.Width := 75;
  Edit_UnitColumns.Value := 1;
  Edit_UnitColumns.OnChange := ShipClick;

  TKMLabel.Create(PopUp_Ship, DropList_Ware.Left, 165, 150, 17, gResTexts[1873], fntGrey, taLeft);
  Edit_UnitBoltCount := TKMNumericEdit.Create(PopUp_Ship, DropList_Ware.Left, 183, 1, 500);
  Edit_UnitBoltCount.Width := 75;
  Edit_UnitBoltCount.Value := 50;
  Edit_UnitBoltCount.OnChange := ShipClick;

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
  Bevel_FlagColor.Visible := Edit_UnitFlagColor.Visible;

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
  CheckBox_Immortal.Hide;
  CheckBox_Boots.Visible := not fUnit.IsAnimal;
  CheckBox_Immortal.Visible := not fUnit.IsAnimal;
  CheckBox_Boots.Checked := aUnit.BootsAdded;
  CheckBox_NeverHungry.Checked := fUnit.NeverHungry;
  CheckBox_Immortal.Checked := fUnit.Immortal;

  SortVisibleControls(65, 103, 0, 0, [CheckBox_NeverHungry, CheckBox_Boots, CheckBox_Immortal]);


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
  CheckBox_Immortal.Show;
  CheckBox_Immortal.Checked := fGroup.FlagBearer.Immortal;
  Panel_Army.Show;

  if fGroup = nil then Exit;
  CheckBox_BitinAdded.Visible := fGroup.UnitType in WARRIOR_BITIN_EQUIPABLE;

  Button_Ship.Visible := fGroup.UnitType = utShip;
  Button_Boat.Visible := fGroup.UnitType = utBoat;

  if fGroup.IsRanged then
  begin
    CheckBox_InfiniteAmmo.Show;
    CheckBox_InfiniteAmmo.Checked := fGroup.FlagBearer.InfinityAmmo;
  end;
  CheckBox_BitinAdded.Checked := fGroup.FlagBearer.BitinAdded;
  SortVisibleControls(65, 103, 0, 0, [CheckBox_NeverHungry, CheckBox_Boots, CheckBox_Immortal, CheckBox_InfiniteAmmo, CheckBox_BitinAdded]);

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
  if Sender = CheckBox_Immortal then
  begin
    U.Immortal := CheckBox_Immortal.Checked;
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

procedure TKMMapEdUnit.RefreshBoat;
var UB : TKMUnitWarriorBoat;
  I, C, oldIndex : Integer;
  wares : TKMWarePlan;
  WT : TKMWareType;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorBoat) then
    Exit;
  UB := TKMUnitWarriorBoat(fGroup.FlagBearer);

  wares := UB.Wares;
  oldIndex := ColumnBox_Wares.ItemIndex;
  ColumnBox_Wares.Clear;
  for I := 0 to wares.Count - 1 do
  begin
    WT := wares[I].W;
    C := wares[I].C;

    ColumnBox_Wares.AddItem(MakeListRow(['', gRes.Wares[WT].Title, IntToStr(C)],//captions
                                    [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF],//colors
                                    [MakePic(rxGui, gRes.Wares[WT].GUIIcon), PIC_CLEAR, PIC_CLEAR],
                                    -1)
                            );
  end;

  if oldIndex > ColumnBox_Wares.RowCount - 1 then
    oldIndex := ColumnBox_Wares.RowCount - 1;

  ColumnBox_Wares.ItemIndex := oldIndex;
end;

procedure TKMMapEdUnit.UpdateWare;
var UB : TKMUnitWarriorBoat;
  id : Integer;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorBoat) then
    Exit;
  UB := TKMUnitWarriorBoat(fGroup.FlagBearer);

  id := ColumnBox_Wares.ItemIndex;

  if DropList_Ware.Item[DropList_Ware.ItemIndex].Tag = -1 then
    UB.Wares[id].W := wtNone
  else
    UB.Wares[id].W := TKMWareType(DropList_Ware.Item[DropList_Ware.ItemIndex].Tag);
  UB.Wares[id].C := Edit_Count.Value;
end;

procedure TKMMapEdUnit.SelectBoatWare(aIndex : Integer);
var UB : TKMUnitWarriorBoat;
  I : Integer;
  ware : TKMWarePlanSingle;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorBoat) then
    Exit;
  UB := TKMUnitWarriorBoat(fGroup.FlagBearer);

  ColumnBox_Wares.ItemIndex := aIndex;
  aIndex := ColumnBox_Wares.ItemIndex;
  if aIndex = -1 then
    Exit;

  ware := UB.Wares[aIndex];

  if ware.W = wtNone then
    DropList_Ware.ItemIndex := -1
  else
    for I := 0 to DropList_Ware.List.RowCount - 1 do
      if DropList_Ware.List.Rows[I].Tag = byte(ware.W) then
      begin
        DropList_Ware.ItemIndex := I;
        Break;
      end;

  Edit_Count.Value := ware.C;
end;

procedure TKMMapEdUnit.BoatWaresChange(Sender: TObject);
var UB : TKMUnitWarriorBoat;
  id : Integer;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorBoat) then
    Exit;
  UB := TKMUnitWarriorBoat(fGroup.FlagBearer);


  if Sender = Button_Boat then
  begin
    RefreshBoat;
    PopUp_Boat.Show;
  end;

  if Sender = Button_Add then
  begin
    UB.Wares.AddWare(wtNone, 1, true);
    RefreshBoat;
    SelectBoatWare(UB.Wares.Count - 1);
  end else
  if Sender = Button_Remove then
  begin
    id := ColumnBox_Wares.ItemIndex;
    if id = -1 then
      Exit;
    UB.Wares.Remove(id);
    RefreshBoat;
    SelectBoatWare(ColumnBox_Wares.ItemIndex);
  end else
  if (Sender = ColumnBox_Wares) then
    SelectBoatWare(ColumnBox_Wares.ItemIndex)
  else
  if (Sender = Edit_Count) or (Sender = DropList_Ware) then
  begin
    UpdateWare;
    RefreshBoat;
  end;

  DropList_Ware.Enabled := ColumnBox_Wares.ItemIndex >= 0;
  Edit_Count.Enabled := ColumnBox_Wares.ItemIndex >= 0;
  Button_Remove.Enabled := ColumnBox_Wares.ItemIndex >= 0;

end;

procedure TKMMapEdUnit.ShipClick(Sender: TObject);
var
  US : TKMUnitWarriorShip;
  id : Integer;
  UT : TKMUnitMainData;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorShip) then
    Exit;
  US := TKMUnitWarriorShip(fGroup.FlagBearer);

  id := ColumnBox_Units.ItemIndex;
  if Sender = Button_Ship then
  begin
    RefreshShipList;
    PopUp_Ship.Show;
  end
  else
  if Sender = Button_AddUnits then
  begin
    US.AddMapEdUnit(utNone, UNIT_MAX_CONDITION div 2, 100, 1, 1);
    RefreshShipList;
    SelectShipUnit(US.MapEdUnitsCount);
  end else
  if Sender = Button_RemoveUnits then
  begin
    US.MapEdUnitsArray.Remove(id);
    RefreshShipList;
  end else
  if (Sender = ColumnBox_Units) then
    SelectShipUnit(ColumnBox_Units.ItemIndex)
  else
  if (Sender = Edit_UnitCount)
  or (Sender = Edit_UnitColumns)
  or (Sender = Edit_UnitCondition)
  or (Sender = Edit_UnitBoltCount)
  or (Sender = DropList_UnitType)
   then
  begin
    UpdateShipUnit;
    RefreshShipList;
  end;
  id := ColumnBox_Units.ItemIndex;
  if id >= 0 then
    UT := US.MapEdUnitsArray[id];

  DropList_UnitType.Enabled := ColumnBox_Units.ItemIndex >= 0;
  Edit_UnitCondition.Enabled := ColumnBox_Units.ItemIndex >= 0;
  Edit_UnitCount.Enabled := ColumnBox_Units.ItemIndex >= 0;
  Edit_UnitColumns.Enabled := (ColumnBox_Units.ItemIndex >= 0) and gRes.Units[UT.UnitType].IsWarrior;
  Edit_UnitBoltCount.Enabled := (ColumnBox_Units.ItemIndex >= 0) and gRes.Units[UT.UnitType].CanOrderAmmo;
  Button_RemoveUnits.Enabled := ColumnBox_Units.ItemIndex >= 0;
  Button_AddUnits.Enabled := US.MapEdUnitsCount < 8;

  if not DropList_UnitType.Enabled then
    DropList_UnitType.ItemIndex := -1;
end;

procedure TKMMapEdUnit.RefreshShipList;
var I : Integer;
  US : TKMUnitWarriorShip;
  oldIndex : Integer;
  UT : TKMUnitMainData;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorShip) then
    Exit;
  US := TKMUnitWarriorShip(fGroup.FlagBearer);

  oldIndex := ColumnBox_Units.ItemIndex;
  ColumnBox_Units.Clear;
  for I := 0 to US.MapEdUnitsCount - 1 do
  begin
    UT := US.MapEdUnits[I];

    ColumnBox_Units.AddItem(MakeListRow(['', gRes.Units[UT.UnitType].GUIName, IntToStr(UT.Count)],//captions
                                    [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF],//colors
                                    [MakePic(rxGui, gRes.Units[UT.UnitType].GUIIcon), PIC_CLEAR, PIC_CLEAR],
                                    -1)
                            );
  end;

  if oldIndex > ColumnBox_Units.RowCount - 1 then
    oldIndex := ColumnBox_Units.RowCount - 1;

  ColumnBox_Units.ItemIndex := oldIndex;
end;

procedure TKMMapEdUnit.UpdateShipUnit;
var
  US : TKMUnitWarriorShip;
  UT : TKMUnitMainData;
  id : Integer;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorShip) then
    Exit;
  US := TKMUnitWarriorShip(fGroup.FlagBearer);

  id := ColumnBox_Units.ItemIndex;
  UT := US.MapEdUnitsArray[id];
  if DropList_UnitType.ItemIndex = -1 then
    UT.UnitType := utNone
  else
    UT.UnitType := TKMUnitType(DropList_UnitType.Item[DropList_UnitType.ItemIndex].Tag);

  UT.Count := Edit_UnitCount.Value;
  UT.Columns := Edit_UnitColumns.Value;
  UT.Condition := Round((Edit_UnitCondition.Value / 100) * UNIT_MAX_CONDITION);
  UT.BoltCount := Edit_UnitBoltCount.Value;

  US.MapEdUnitsArray[id] := UT;
end;

procedure TKMMapEdUnit.SelectShipUnit(aIndex: Integer);
var
  US : TKMUnitWarriorShip;
  UT : TKMUnitMainData;
  I : Integer;
begin
  if not (fGroup.FlagBearer is TKMUnitWarriorShip) then
    Exit;
  US := TKMUnitWarriorShip(fGroup.FlagBearer);

  ColumnBox_Units.ItemIndex := aIndex;
  aIndex := ColumnBox_Units.ItemIndex;
  if aIndex = -1 then
    Exit;

  UT := US.MapEdUnits[aIndex];

  if UT.UnitType = utNone then
    DropList_UnitType.ItemIndex := -1
  else
    for I := 0 to DropList_UnitType.Count - 1 do
      if DropList_UnitType.Item[I].Tag = byte(UT.UnitType) then
      begin
        DropList_UnitType.ItemIndex := I;
        Break;
      end;

  Edit_UnitCount.Value := UT.Count;
  Edit_UnitColumns.Value := UT.Columns;
  Edit_UnitCondition.Value := Round((UT.Condition / UNIT_MAX_CONDITION) * 100);
  Edit_UnitBoltCount.Value := UT.BoltCount;

end;

end.
