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
  TKMMapEdTownUnitsCommon = class(TKMMapEdSubMenuPage)
  private
    procedure Town_UnitChange(Sender: TObject); virtual;
    procedure Town_NumericChange(Sender: TObject); virtual;
    procedure Town_UnitRefresh; virtual;
  protected
    Panel_Units: TKMScrollPanel;
      Button_UnitCancel: TKMButtonFlat;
      BrushSize: TKMTrackBar;

      //Button_Animals: array [0..7] of TKMButtonFlat;
      //NumEd_FishCount: TKMNumericEdit;

    procedure CreateButtons; virtual;
  public
    constructor Create(aParent: TKMPanel); virtual;

    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdatePlayerColor; virtual;
    procedure UpdateHotkeys;virtual;
    procedure UpdateState;virtual;
  end;

  TKMMapEdTownUnits = class(TKMMapEdTownUnitsCommon)
  private
    procedure Town_UnitChange(Sender: TObject); override;
    procedure Town_UnitRefresh; override;
  protected
      CheckBox_AddBoots: TKMCheckBox;
      Button_Citizen: array [0..high(School_Order)] of TKMButtonFlat;
      Button_AddUnitsToHouse:TKMButton;
    procedure CreateButtons; override;
  public

    procedure UpdatePlayerColor; override;
    procedure UpdateHotkeys;override;
    procedure UpdateState;override;
  end;


  TKMMapEdTownWarriors = class(TKMMapEdTownUnitsCommon)
  private
    procedure Town_UnitChange(Sender: TObject); override;
    procedure Town_NumericChange(Sender: TObject); override;
    procedure Town_UnitRefresh; override;
  protected
    Button_Warriors: array  of TKMButtonFlat;
    NumEd_WarrCount, NumEd_WarrColumns: TKMNumericEdit;  //number of units in group + number of rows
    procedure CreateButtons; override;
  public

    procedure UpdatePlayerColor; override;
    procedure UpdateHotkeys;override;
    procedure UpdateState;override;
  end;

implementation
uses
  KM_HandsCollection,
  KM_Cursor, KM_RenderUI,
  KM_Resource, KM_ResUnits, KM_ResFonts, KM_ResTexts, KM_ResTypes,
  KM_Utils,
  KM_MapEdTypes, KM_Game;


{ TKMMapEdTownUnits }
constructor TKMMapEdTownUnitsCommon.Create(aParent: TKMPanel);
const
  MAPED_FISH_CNT_DEFAULT = 50;
var
  lineY: Word;
begin
  inherited Create;

  Panel_Units := TKMScrollPanel.Create(aParent, 9, 28, aParent.Width - 9, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  Panel_Units.ScrollV.Left := Panel_Units.ScrollV.Left + 20;
  Panel_Units.AnchorsStretch;

  lineY := 30;

  Button_UnitCancel := TKMButtonFlat.Create(Panel_Units, 0, lineY, 33, 33, 340);
  Button_UnitCancel.Hint := GetHintWHotkey(TX_MAPED_UNITS_REMOVE_HINT, MAPED_SUBMENU_ACTIONS_HOTKEYS[0]);
  Button_UnitCancel.Tag := UNIT_REMOVE_TAG; //Erase
  Button_UnitCancel.OnClick := Town_UnitChange;

  BrushSize := TKMTrackBar.Create(Panel_Units, 35, lineY + 10, 100, 1, 5);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.OnChange := Town_UnitChange;
  BrushSize.Hint := gResTexts[1839];
  BrushSize.Position := 1;
  BrushSize.Tag := UNIT_REMOVE_TAG;

  CreateButtons;

   {
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
  }
  {for I := 0 to High(fSubMenuActionsEvents) do
    fSubMenuActionsEvents[I] := Town_UnitChange;

  fSubMenuActionsCtrls[0,0] := Button_UnitCancel;
  for I := 1 to High(fSubMenuActionsCtrls) do
    fSubMenuActionsCtrls[I, 0] := Button_Citizen[I-1];}
end;

procedure TKMMapEdTownUnitsCommon.CreateButtons;
begin
end;

procedure TKMMapEdTownUnits.CreateButtons;
var I, lineY : Integer;
begin
  with TKMLabel.Create(Panel_Units, 0, PAGE_TITLE_Y, Panel_Units.Width, 3, gResTexts[TX_MAPED_UNITS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  lineY := Button_UnitCancel.Bottom + 3;
  Button_AddUnitsToHouse := TKMButton.Create(Panel_Units, 0, lineY, Panel_Units.Width, 30, gResTexts[1828], bsGame);
  Button_AddUnitsToHouse.OnClick := Town_UnitChange;

  CheckBox_AddBoots := TKMCheckBox.Create(Panel_Units, 9, lineY + 35, Panel_Units.Width, 25, gResTexts[1819], fntMetal);
  CheckBox_AddBoots.OnClick := Town_UnitChange;

  lineY := CheckBox_AddBoots.Bottom + 10;
  for I := 0 to High(Button_Citizen) do
  begin
    Button_Citizen[I] := TKMButtonFlat.Create(Panel_Units, 9 + (I mod 5)*37, lineY + (I div 5)*37,33,33,gRes.Units[School_Order[I]].GUIIcon); //List of tiles 5x5
    Button_Citizen[I].Hint := gRes.Units[School_Order[I]].GUIName;
    if InRange(I, 0, High(fSubMenuActionsCtrls) - 1) then
      Button_Citizen[I].Hint := GetHintWHotkey(Button_Citizen[I].Hint, MAPED_SUBMENU_ACTIONS_HOTKEYS[I+1]);

    Button_Citizen[I].Tag := Byte(School_Order[I]); //Returns unit ID
    Button_Citizen[I].OnClick := Town_UnitChange;
  end;

  for I := 0 to High(fSubMenuActionsEvents) do
    fSubMenuActionsEvents[I] := Town_UnitChange;

  fSubMenuActionsCtrls[0,0] := Button_UnitCancel;
  for I := 1 to High(fSubMenuActionsCtrls) do
    fSubMenuActionsCtrls[I, 0] := Button_Citizen[I-1];
end;

procedure TKMMapEdTownWarriors.CreateButtons;
var I, J, lineY, fCount : Integer;
  UT : TKMUnitType;
begin
  with TKMLabel.Create(Panel_Units, 0, PAGE_TITLE_Y, Panel_Units.Width, 0, gResTexts[2028], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  lineY := Button_UnitCancel.Bottom + 3;
  fCount := 0;
  J := 0;
  for I := 0 to High(MapEd_Order) do
  begin
    UT := MapEd_Order[I];
    if UT = utNone then
    begin
      lineY := Button_Warriors[fCount - 1].Bottom + 4;
      J := 0;
      Continue;
    end;
    Inc(fCount);
    if fCount > length(Button_Warriors) then
      SetLength(Button_Warriors, fCount + 16);



    Button_Warriors[fCount - 1] := TKMButtonFlat.Create(Panel_Units, 9 + (J mod 5)*37,lineY + (J div 5)*37,33,33, gRes.Units[UT].GUIIcon, rxGui);
    Button_Warriors[fCount - 1].Hint := gRes.Units[UT].GUIName;
    Button_Warriors[fCount - 1].Tag := Byte(UT); //Returns unit ID
    Button_Warriors[fCount - 1].OnClick := Town_UnitChange;
    case gRes.Units[UT].GetTrainingHouse of
      htSchool : Button_Warriors[fCount - 1].BackBevelColor := $2276403E;
      htBarracks : Button_Warriors[fCount - 1].BackBevelColor := $22FF0000;
      htTownhall : Button_Warriors[fCount - 1].BackBevelColor := $2200BDFF;
      htPalace : Button_Warriors[fCount - 1].BackBevelColor := $220000FF;
      htSiegeWorkshop : Button_Warriors[fCount - 1].BackBevelColor := $22FF00C6;
      htShipyard : Button_Warriors[fCount - 1].BackBevelColor := $22FFB400;
      else
        Button_Warriors[fCount - 1].BackBevelColor := $22FFFFFF;
    end;
    Inc(J);
  end;
  Setlength(Button_Warriors, fCount);
  lineY := Button_Warriors[High(Button_Warriors)].Bottom + 10;


  with TKMLabel.Create(Panel_Units, 9, lineY, Panel_Units.Width, 20, gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER], fntMetal, taLeft) do
  begin
    Anchors := [anLeft, anTop, anRight];
    Hint := gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER_HINT];
  end;

  NumEd_WarrCount := TKMNumericEdit.Create(Panel_Units, 9, lineY + 20, 0, MAPED_GROUP_MAX_CNT);
  NumEd_WarrCount.Anchors := [anLeft, anTop, anRight];
  NumEd_WarrCount.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_NUMBER_HINT];
  NumEd_WarrCount.AutoFocusable := False;
  NumEd_WarrCount.OnChange := Town_NumericChange;
  NumEd_WarrCount.Value := 1;

  with TKMLabel.Create(Panel_Units, 105, lineY, Panel_Units.Width - 100, 20, gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS], fntMetal, taLeft) do
  begin
    Anchors := [anLeft, anTop, anRight];
    Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  end;

  NumEd_WarrColumns := TKMNumericEdit.Create(Panel_Units, 105, lineY + 20, 1, 25);
  NumEd_WarrColumns.Anchors := [anLeft, anTop, anRight];
  NumEd_WarrColumns.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  NumEd_WarrColumns.AutoFocusable := False;
  NumEd_WarrColumns.OnChange := Town_NumericChange;
  NumEd_WarrColumns.Value := 1;

  for I := 0 to High(fSubMenuActionsEvents) do
    fSubMenuActionsEvents[I] := Town_UnitChange;

  fSubMenuActionsCtrls[0,0] := Button_UnitCancel;
  for I := 1 to High(fSubMenuActionsCtrls) do
    fSubMenuActionsCtrls[I, 0] := Button_Warriors[I-1];
end;

procedure TKMMapEdTownUnitsCommon.Town_UnitChange(Sender: TObject);
begin
  gCursor.Mode := cmUnits;
  gCursor.Tag1 := Byte(TKMButtonFlat(Sender).Tag);

  gCursor.MapEdSize := BrushSize.Position;
  Town_UnitRefresh;
end;


procedure TKMMapEdTownUnitsCommon.Town_NumericChange(Sender: TObject);
begin
  //refresh formations
  {gCursor.MapEdGroupFormation.NumUnits    := NumEd_WarrCount.Value;
  NumEd_WarrColumns.Enabled := NumEd_WarrCount.Enabled and (NumEd_WarrCount.Value > 0);
  gCursor.MapEdGroupFormation.UnitsPerRow := NumEd_WarrColumns.Value;
  gCursor.MapEdFishCount := NumEd_FishCount.Value;}
end;


procedure TKMMapEdTownUnitsCommon.Town_UnitRefresh;
begin
  Button_UnitCancel.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_UnitCancel.Tag);

  {NumEd_WarrCount.Enabled := UT in UNITS_WARRIORS;
  NumEd_WarrColumns.Enabled := NumEd_WarrCount.Enabled and (NumEd_WarrCount.Value > 0);
  NumEd_FishCount.Enabled := UT = utFish;}


  {CheckBox_AddBoots.Checked := gCursor.MapEd_UnitAddBoots;

  UT := utNone;
  for I := 0 to High(Button_Citizen) do
  begin
    B := Button_Citizen[I];
    B.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = B.Tag);
    if B.Down then
      UT := TKMUnitType(B.Tag);
  end;}
  {for I := 0 to High(Button_Warriors) do
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
  end;}
end;


procedure TKMMapEdTownUnitsCommon.Hide;
begin
  Panel_Units.Hide;
end;


procedure TKMMapEdTownUnitsCommon.Show;
begin
  Town_UnitRefresh;
  Town_NumericChange(nil);
  Panel_Units.Show;
end;


function TKMMapEdTownUnitsCommon.Visible: Boolean;
begin
  Result := Panel_Units.Visible;
end;


procedure TKMMapEdTownUnitsCommon.UpdateState;
begin
  Town_UnitRefresh;
end;


procedure TKMMapEdTownUnitsCommon.UpdateHotkeys;
begin
end;


procedure TKMMapEdTownUnitsCommon.UpdatePlayerColor;
begin

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

  Inherited;
end;

procedure TKMMapEdTownUnits.Town_UnitRefresh;
var I : Integer;
begin
  Inherited;
  for I := 0 to High(Button_Citizen) do
    Button_Citizen[I].Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_Citizen[I].Tag);
end;

procedure TKMMapEdTownUnits.UpdateState;
begin

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
end;

procedure TKMMapEdTownWarriors.Town_NumericChange(Sender: TObject);
begin
  //refresh formations
  gCursor.MapEdGroupFormation.NumUnits    := NumEd_WarrCount.Value;
  NumEd_WarrColumns.Enabled := NumEd_WarrCount.Enabled and (NumEd_WarrCount.Value > 0);
  gCursor.MapEdGroupFormation.UnitsPerRow := NumEd_WarrColumns.Value;
  //gCursor.MapEdFishCount := NumEd_FishCount.Value;
end;

procedure TKMMapEdTownWarriors.Town_UnitChange(Sender: TObject);
begin
  Inherited;
end;

procedure TKMMapEdTownWarriors.Town_UnitRefresh;
var I : Integer;
begin
  Inherited;
  for I := 0 to High(Button_Warriors) do
    Button_Warriors[I].Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_Warriors[I].Tag);
end;

procedure TKMMapEdTownWarriors.UpdateState;
begin

end;


procedure TKMMapEdTownWarriors.UpdatePlayerColor;
var I : Integer;
begin
  for I := 0 to High(Button_Warriors) do
    Button_Warriors[I].FlagColor := gMySpectator.Hand.FlagColor;

end;

procedure TKMMapEdTownWarriors.UpdateHotkeys;
var
  I: Integer;
  hintStr: string;
begin
  for I := 0 to High(Button_Warriors) do
  begin
    hintStr := gRes.Units[MapEd_Order[I]].GUIName;
    if InRange(I, 0, High(fSubMenuActionsCtrls) - 1) then
      Button_Warriors[I].Hint := GetHintWHotkey(hintStr, MAPED_SUBMENU_ACTIONS_HOTKEYS[I+1])
    else
      Button_Warriors[I].Hint := hintStr;
  end;
end;

end.
