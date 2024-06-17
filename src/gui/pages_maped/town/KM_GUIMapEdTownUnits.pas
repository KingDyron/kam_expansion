unit KM_GUIMapEdTownUnits;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_InterfaceDefaults,
   KM_InterfaceGame,
   KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsScroll, KM_ControlsSwitch,
   KM_ControlsTrackBar,
   KM_Defaults, KM_Pics;

type
  TKMMapEdTownUnits = class(TKMMapEdSubMenuPage)
  private
    procedure Town_UnitChange(Sender: TObject);
    procedure Town_NumericChange(Sender: TObject);
    procedure Town_UnitRefresh;
  protected
    Panel_Units: TKMPanel;
      CheckBox_AddBoots: TKMCheckBox;
      ExpandPanels : TKMExpandPanelCollection;
      Button_UnitCancel: TKMButtonFlat;
      BrushSize: TKMTrackBar;
      Button_Citizen: array [0..high(School_Order)] of TKMButtonFlat;
      Button_Warriors: array [0..high(MapEd_Order)] of TKMButtonFlat;
      Button_Animals: array [0..7] of TKMButtonFlat;
      NumEd_WarrCount, NumEd_WarrColumns: TKMNumericEdit;  //number of units in group + number of rows
      NumEd_FishCount: TKMNumericEdit;
      Button_AddUnitsToHouse:TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdatePlayerColor;
    procedure UpdateHotkeys;
    procedure UpdateState;
  end;


implementation
uses
  KM_HandsCollection,
  KM_Cursor, KM_RenderUI,
  KM_Resource, KM_ResUnits, KM_ResFonts, KM_ResTexts, KM_ResTypes,
  KM_Utils,
  KM_MapEdTypes, KM_Game;


{ TKMMapEdTownUnits }
constructor TKMMapEdTownUnits.Create(aParent: TKMPanel);
const
  MAPED_FISH_CNT_DEFAULT = 50;
var
  I, J: Integer;
  lineY: Word;
begin
  inherited Create;

  Panel_Units := TKMPanel.Create(aParent, 9, 28, aParent.Width - 9, 400);
  with TKMLabel.Create(Panel_Units, 0, PAGE_TITLE_Y, Panel_Units.Width, 0, gResTexts[TX_MAPED_UNITS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  lineY := 30;
  Button_AddUnitsToHouse := TKMButton.Create(Panel_Units, 0, lineY, Panel_Units.Width, 30, gResTexts[1828], bsGame);
  Button_AddUnitsToHouse.OnClick := Town_UnitChange;

  Button_UnitCancel := TKMButtonFlat.Create(Panel_Units, 0, lineY + 32, 33, 33, 340);
  Button_UnitCancel.Hint := GetHintWHotkey(TX_MAPED_UNITS_REMOVE_HINT, MAPED_SUBMENU_ACTIONS_HOTKEYS[0]);
  Button_UnitCancel.Tag := UNIT_REMOVE_TAG; //Erase
  Button_UnitCancel.OnClick := Town_UnitChange;

  BrushSize := TKMTrackBar.Create(Panel_Units, 35, lineY + 40, 100, 1, 5);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.OnChange := Town_UnitChange;
  BrushSize.Hint := gResTexts[1839];
  BrushSize.Position := 1;
  BrushSize.Tag := UNIT_REMOVE_TAG;

  Inc(lineY, 5);
  CheckBox_AddBoots := TKMCheckBox.Create(Panel_Units, 9, lineY + 68, Panel_Units.Width, 25, gResTexts[1819], fntMetal);
  CheckBox_AddBoots.OnClick := Town_UnitChange;

  ExpandPanels := TKMExpandPanelCollection.Create(Panel_Units, 0, lineY + 93, Panel_Units.Width);
  J := ExpandPanels.AddPanel(40*3, gResTexts[267]);
  for I := 0 to High(Button_Citizen) do
  begin
    Button_Citizen[I] := TKMButtonFlat.Create(ExpandPanels[J], 9 + (I mod 5)*37,(I div 5)*37,33,33,gRes.Units[School_Order[I]].GUIIcon); //List of tiles 5x5
    Button_Citizen[I].Hint := gRes.Units[School_Order[I]].GUIName;
    if InRange(I, 0, High(fSubMenuActionsCtrls) - 1) then
      Button_Citizen[I].Hint := GetHintWHotkey(Button_Citizen[I].Hint, MAPED_SUBMENU_ACTIONS_HOTKEYS[I+1]);

    Button_Citizen[I].Tag := Byte(School_Order[I]); //Returns unit ID
    Button_Citizen[I].OnClick := Town_UnitChange;
  end;


  J := ExpandPanels.AddPanel(40*6, gResTexts[1227]);

  for I := 0 to High(Button_Warriors) do
  begin
    Button_Warriors[I] := TKMButtonFlat.Create(ExpandPanels[J], 9 + (I mod 5)*37,(I div 5)*37,33,33, gRes.Units[MapEd_Order[I]].GUIIcon, rxGui);
    Button_Warriors[I].Hint := gRes.Units[MapEd_Order[I]].GUIName;
    Button_Warriors[I].Tag := Byte(MapEd_Order[I]); //Returns unit ID
    Button_Warriors[I].OnClick := Town_UnitChange;
  end;
  lineY := Button_Warriors[High(Button_Warriors)].Bottom + 10;


  with TKMLabel.Create(ExpandPanels[J], 9, lineY, Panel_Units.Width, 20, gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER], fntMetal, taLeft) do
  begin
    Anchors := [anLeft, anTop, anRight];
    Hint := gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER_HINT];
  end;

  NumEd_WarrCount := TKMNumericEdit.Create(ExpandPanels[J], 9, lineY + 20, 0, MAPED_GROUP_MAX_CNT);
  NumEd_WarrCount.Anchors := [anLeft, anTop, anRight];
  NumEd_WarrCount.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER_HINT];
  NumEd_WarrCount.AutoFocusable := False;
  NumEd_WarrCount.OnChange := Town_NumericChange;
  NumEd_WarrCount.Value := 1;

  with TKMLabel.Create(ExpandPanels[J], 105, lineY, ExpandPanels[J].Width - 100, 20, gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS], fntMetal, taLeft) do
  begin
    Anchors := [anLeft, anTop, anRight];
    Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  end;

  NumEd_WarrColumns := TKMNumericEdit.Create(ExpandPanels[J], 105, lineY + 20, 1, 25);
  NumEd_WarrColumns.Anchors := [anLeft, anTop, anRight];
  NumEd_WarrColumns.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  NumEd_WarrColumns.AutoFocusable := False;
  NumEd_WarrColumns.OnChange := Town_NumericChange;
  NumEd_WarrColumns.Value := 1;

  J := ExpandPanels.AddPanel(47*2, gResTexts[1342]);

  for I := 0 to High(Button_Animals) do
  begin
    Button_Animals[I] := TKMButtonFlat.Create(ExpandPanels[J], 9 + (I mod 5)*37,(I div 5)*37,33,33, Animal_Icon[I], rxGui);
    Button_Animals[I].Hint := gRes.Units[Animal_Order[I]].GUIName;
    Button_Animals[I].Tag := Byte(Animal_Order[I]); //Returns animal ID
    Button_Animals[I].OnClick := Town_UnitChange;
  end;

  lineY := Button_Animals[High(Button_Animals)].Bottom + 10;

  with TKMLabel.Create(ExpandPanels[J], 9, lineY, Panel_Units.Width - 100, 20, gResTexts[TX_MAPED_FISH_COUNT], fntMetal, taLeft) do
  begin
    Anchors := [anLeft, anTop, anRight];
    Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  end;

  NumEd_FishCount := TKMNumericEdit.Create(ExpandPanels[J], 9, lineY + 20, 1, FISH_CNT_MAX);
  NumEd_FishCount.Anchors := [anLeft, anTop, anRight];
//  NumEd_FishCount.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  NumEd_FishCount.AutoFocusable := False;
  NumEd_FishCount.OnChange := Town_NumericChange;
  NumEd_FishCount.Value := MAPED_FISH_CNT_DEFAULT;

  for I := 0 to High(fSubMenuActionsEvents) do
    fSubMenuActionsEvents[I] := Town_UnitChange;

  fSubMenuActionsCtrls[0,0] := Button_UnitCancel;
  for I := 1 to High(fSubMenuActionsCtrls) do
    fSubMenuActionsCtrls[I, 0] := Button_Citizen[I-1];
end;


procedure TKMMapEdTownUnits.Town_UnitChange(Sender: TObject);
begin
  if sender = Button_AddUnitsToHouse then
  begin
    gGame.MapEditor.AddWorkersToHouses;
    Exit;
  end else
  if Sender = CheckBox_AddBoots then
  begin
    if CheckBox_AddBoots.Checked then
      gCursor.AddMod(medUnitBoots)
    else
      gCursor.RemMod(medUnitBoots);
    Exit;
  end;
  gCursor.Mode := cmUnits;
  gCursor.Tag1 := Byte(TKMButtonFlat(Sender).Tag);

  gCursor.MapEdSize := BrushSize.Position;
  Town_UnitRefresh;
end;


procedure TKMMapEdTownUnits.Town_NumericChange(Sender: TObject);
begin
  //refresh formations
  gCursor.MapEdGroupFormation.NumUnits    := NumEd_WarrCount.Value;
  NumEd_WarrColumns.Enabled := NumEd_WarrCount.Enabled and (NumEd_WarrCount.Value > 0);
  gCursor.MapEdGroupFormation.UnitsPerRow := NumEd_WarrColumns.Value;
  gCursor.MapEdFishCount := NumEd_FishCount.Value;
end;


procedure TKMMapEdTownUnits.Town_UnitRefresh;
var
  I: Integer;
  B: TKMButtonFlat;
  UT: TKMUnitType;
begin

  UT := utNone;
  for I := 0 to High(Button_Citizen) do
  begin
    B := Button_Citizen[I];
    B.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = B.Tag);
    if B.Down then
      UT := TKMUnitType(B.Tag);
  end;
  for I := 0 to High(Button_Warriors) do
  begin
    B := Button_Warriors[I];
    B.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = B.Tag);
    if B.Down then
      UT := TKMUnitType(B.Tag);
  end;
  for I := 0 to High(Button_Animals) do
  begin
    B := Button_Animals[I];
    B.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = B.Tag);
    if B.Down then
      UT := TKMUnitType(B.Tag);
  end;
  Button_UnitCancel.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_UnitCancel.Tag);

  NumEd_WarrCount.Enabled := UT in UNITS_WARRIORS;
  NumEd_WarrColumns.Enabled := NumEd_WarrCount.Enabled and (NumEd_WarrCount.Value > 0);
  NumEd_FishCount.Enabled := UT = utFish;
end;


procedure TKMMapEdTownUnits.Hide;
begin
  Panel_Units.Hide;
end;


procedure TKMMapEdTownUnits.Show;
begin
  CheckBox_AddBoots.Checked := gCursor.MapEd_UnitAddBoots;
  Town_UnitRefresh;
  Town_NumericChange(nil);
  Panel_Units.Show;
end;


function TKMMapEdTownUnits.Visible: Boolean;
begin
  Result := Panel_Units.Visible;
end;


procedure TKMMapEdTownUnits.UpdateState;
begin
  Town_UnitRefresh;
end;


procedure TKMMapEdTownUnits.UpdateHotkeys;
var
  I: Integer;
  hintStr: string;
begin
  for I := 0 to High(Button_Citizen) do
  begin
    hintStr := gRes.Units[School_Order[I]].GUIName;
    if InRange(I, 0, High(fSubMenuActionsCtrls) - 1) then
      Button_Citizen[I].Hint := GetHintWHotkey(hintStr, MAPED_SUBMENU_ACTIONS_HOTKEYS[I+1])
    else
      Button_Citizen[I].Hint := hintStr;
  end;
end;


procedure TKMMapEdTownUnits.UpdatePlayerColor;
var
  I: Integer;
  col: Cardinal;
begin
  col := gMySpectator.Hand.FlagColor;

  for I := Low(Button_Citizen) to High(Button_Citizen) do
    Button_Citizen[I].FlagColor := col;
  for I := Low(Button_Warriors) to High(Button_Warriors) do
    Button_Warriors[I].FlagColor := col;
end;


end.
