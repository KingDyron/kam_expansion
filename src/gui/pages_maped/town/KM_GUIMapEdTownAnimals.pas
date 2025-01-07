unit KM_GUIMapEdTownAnimals;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsTrackBar,
   KM_ControlsEdit, KM_ControlsPopUp, KM_ControlsList, KM_ControlsDrop,
   KM_Defaults, KM_ControlsScroll, KM_InterfaceGame;

type
  TKMMapEdTownAnimals = class
  private
    procedure Player_AnimalClick(Sender: TObject);
    procedure Player_AnimalRefresh;
    procedure Spawner_AnimalTypeChange(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Spawner_AnimalOver(Sender: TObject; Shift: TShiftState);
  protected
    Panel_AnimalSpawner: TKMScrollPanel;
      Button_UnitCancel: TKMButtonFlat;
      BrushSize: TKMTrackBar;
      Button_PlaceAnimals: array [0..high(Animal_Order)] of TKMButtonFlat;
      NumEd_FishCount: TKMNumericEdit;

      Button_PlaceSpawner : TKMButtonFlat;
      TrackBar_MaxCount,
      TrackBar_Radius : TKMTrackBar;
      Edit_SpawnerPace : TKMNumericEdit;
      Button_Animals: array [0..high(Animal_Order)] of TKMButtonFlat;

  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;

implementation
uses
  KM_GameParams,
  KM_MapEdTypes,
  KM_HandsCollection, KM_ResTexts, KM_Game, KM_GameTypes,
  KM_Cursor, KM_RenderUI,
  KM_InterfaceDefaults,
  KM_ResFonts, KM_Resource, KM_ResTypes, KM_ResLocales,
  SysUtils, Math,
  KM_Utils;


constructor TKMMapEdTownAnimals.Create(aParent: TKMPanel);
var I, J, lineY : Integer;
  UT : TKMUnitType;
begin
  Panel_AnimalSpawner := TKMScrollPanel.Create(aParent, 0, 28, aParent.Width, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  Panel_AnimalSpawner.ScrollV.Left := Panel_AnimalSpawner.ScrollV.Left + 20;
  Panel_AnimalSpawner.AnchorsStretch;

  with TKMLabel.Create(Panel_AnimalSpawner, 0, PAGE_TITLE_Y, Panel_AnimalSpawner.Width, 0, gResTexts[1342], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  lineY := 30;

  Button_UnitCancel := TKMButtonFlat.Create(Panel_AnimalSpawner, 9, lineY, 33, 33, 340);
  Button_UnitCancel.Hint := GetHintWHotkey(TX_MAPED_UNITS_REMOVE_HINT, MAPED_SUBMENU_ACTIONS_HOTKEYS[0]);
  Button_UnitCancel.Tag := UNIT_REMOVE_TAG; //Erase
  Button_UnitCancel.OnClick := Player_AnimalClick;

  BrushSize := TKMTrackBar.Create(Panel_AnimalSpawner, 44, lineY + 10, 100, 1, 5);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.OnChange := Player_AnimalClick;
  BrushSize.Hint := gResTexts[1839];
  BrushSize.Position := 1;
  BrushSize.Tag := UNIT_REMOVE_TAG;

  lineY := Button_UnitCancel.Bottom + 5;
  I := 0;
  for J := 0 to high(Animal_Order) do
  begin
    UT := Animal_Order[J];
    Button_PlaceAnimals[J] := TKMButtonFlat.Create(Panel_AnimalSpawner, 9 + (I mod 6) * 33, lineY + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_PlaceAnimals[J].Tag := byte(UT);
    Button_PlaceAnimals[J].OnClick := Player_AnimalClick;
    Button_PlaceAnimals[J].Hint := gRes.Units[UT].GUIName;
    inc(I);
  end;
  lineY := Button_PlaceAnimals[high(Button_PlaceAnimals)].Bottom + 5;

  NumEd_FishCount := TKMNumericEdit.Create(Panel_AnimalSpawner, 9, lineY, 1, FISH_CNT_MAX);
  NumEd_FishCount.Hint := gResTexts[TX_MAPED_UNITS_FORMATION_COLUMNS_HINT];
  NumEd_FishCount.Anchors := [anLeft, anTop, anRight];
  NumEd_FishCount.AutoFocusable := False;
  NumEd_FishCount.OnChange := Player_AnimalClick;
  NumEd_FishCount.Value := 50;


  lineY := NumEd_FishCount.Bottom + 5;


  with TKMLabel.Create(Panel_AnimalSpawner, 0, lineY, Panel_AnimalSpawner.Width, 0, gResTexts[2027], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];


  Button_PlaceSpawner := TKMButtonFlat.Create(Panel_AnimalSpawner, Panel_AnimalSpawner.Width div 2 - 17, lineY + 23, 34, 34, 914, rxGui);
  Button_PlaceSpawner.OnClick := Player_AnimalClick;
  //Button_PlaceSpawner.Hide;

  TrackBar_Radius := TKMTrackBar.Create(Panel_AnimalSpawner, 9, lineY + 60, Panel_AnimalSpawner.Width - 18, 2, 50);
  TrackBar_Radius.Position := 10;
  TrackBar_Radius.Caption := gResTexts[2154];
  TrackBar_Radius.OnChange := Player_AnimalClick;

  TrackBar_MaxCount := TKMTrackBar.Create(Panel_AnimalSpawner, 9, lineY + 100, Panel_AnimalSpawner.Width - 18, 1, 50);
  TrackBar_MaxCount.Position := 10;
  TrackBar_MaxCount.Caption := gResTexts[2155];
  TrackBar_MaxCount.OnChange := Player_AnimalClick;



  with TKMLabel.Create(Panel_AnimalSpawner, 0, lineY + 150, Panel_AnimalSpawner.Width, 0, gResTexts[2156], fntOutline, taCenter) do
    Anchors := [anLeft, anTop];
  Edit_SpawnerPace := TKMNumericEdit.Create(Panel_AnimalSpawner, 9, lineY + 168, 100, 72000);
  Edit_SpawnerPace.Value := 500;
  Edit_SpawnerPace.OnChange := Player_AnimalClick;
  Edit_SpawnerPace.Left := Panel_AnimalSpawner.Width div 2 - Edit_SpawnerPace.Width div 2;


  I := 0;
  for J := 0 to high(Animal_Order) do
  begin
    UT := Animal_Order[J];
    Button_Animals[I] := TKMButtonFlat.Create(Panel_AnimalSpawner, 9 + (I mod 6) * 33, lineY + 200 + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_Animals[I].Tag := byte(UT);
    Button_Animals[I].Hint := gRes.Units[UT].GUIName;
    Button_Animals[I].Tag2 := BUTTON_BLOCK_SPAWNER_TAG_2;
    Button_Animals[I].OnMouseDown := Spawner_AnimalTypeChange;
    Button_Animals[I].OnMouseOver := Spawner_AnimalOver;
    inc(I);
  end;
end;

procedure TKMMapEdTownAnimals.Player_AnimalClick(Sender: TObject);
var I : Integer;
begin
  if (Sender = Button_UnitCancel) or (Sender = BrushSize) then
  begin
    gCursor.Mode := cmUnits;
    gCursor.Tag1 := UNIT_REMOVE_TAG;
    Player_AnimalRefresh;
    Exit;
  end;

  for I := 0 to High(Button_PlaceAnimals) do
    if Sender = Button_PlaceAnimals[I] then
    begin
      gCursor.Mode := cmUnits;
      gCursor.Tag1 := Button_PlaceAnimals[I].Tag;
      Player_AnimalRefresh;
      Exit;
    end;

  if Sender = Button_PlaceSpawner then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_ANIMALS;
    Player_AnimalRefresh;
    Exit;
  end;

  for I := Low(Button_Animals) to High(Button_Animals) do
    if Sender = Button_Animals[I] then
      Button_Animals[I].Down := not Button_Animals[I].Down;

  gCursor.MapEd_Animals := [];

  for I := Low(Button_Animals) to High(Button_Animals) do
    if Button_Animals[I].Down then
    begin
      SetLength(gCursor.MapEd_Animals, length(gCursor.MapEd_Animals) + 1);
      gCursor.MapEd_Animals[high(gCursor.MapEd_Animals)] := TKMUnitType(Button_Animals[I].Tag);
    end;


  gCursor.MapEd_AnimalsCount := TrackBar_MaxCount.Position;
  gCursor.MapEdSize := TrackBar_Radius.Position;

  gCursor.MapEd_AnimalsPace := Edit_SpawnerPace.Value;
  Player_AnimalRefresh;
end;


procedure TKMMapEdTownAnimals.Show;
begin
  Panel_AnimalSpawner.Show;
  Player_AnimalClick(nil);
  gGameParams.VisibleLayers := gGameParams.VisibleLayers + [mlSpawners];
end;

procedure TKMMapEdTownAnimals.Hide;
begin
  Panel_AnimalSpawner.Hide;
  gGameParams.VisibleLayers := gGameParams.VisibleLayers - [mlSpawners];
end;

function TKMMapEdTownAnimals.Visible: Boolean;
begin
  Result := Panel_AnimalSpawner.Visible;
end;

procedure TKMMapEdTownAnimals.Player_AnimalRefresh;
var I : Integer;
begin
  Button_UnitCancel.Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_UnitCancel.Tag);
  NumEd_FishCount.Enabled := (gCursor.Mode = cmUnits) and (TKMUnitType(gCursor.Tag1) = utFish);
  Button_PlaceSpawner.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_ANIMALS);

  for I := 0 to High(Button_PlaceAnimals) do
    Button_PlaceAnimals[I].Down := (gCursor.Mode = cmUnits) and (gCursor.Tag1 = Button_PlaceAnimals[I].Tag);

  gCursor.MapEdFishCount := NumEd_FishCount.Value;
  if gCursor.Mode = cmMarkers then
  begin
    gCursor.MapEd_AnimalsCount := TrackBar_MaxCount.Position;
    gCursor.MapEdSize := TrackBar_Radius.Position;
  end else
  if gCursor.Mode = cmUnits then
    gCursor.MapEdSize := BrushSize.Position;

  gCursor.MapEd_AnimalsPace := Edit_SpawnerPace.Value;

end;


procedure TKMMapEdTownAnimals.Spawner_AnimalTypeChange(Sender: TObject; X: Integer; Y: Integer; Shift: TShiftState);
begin
  Player_AnimalClick(Sender);
end;

procedure TKMMapEdTownAnimals.Spawner_AnimalOver(Sender: TObject; Shift: TShiftState);
var ctrlDown : TKMControl;
  ctrlOver : TKMControl;
begin
  if Sender = nil then
    Exit;

  if not (sender is TKMButtonFlat) then
    Exit;

  if not (TKMButtonFlat(Sender).Tag2 = BUTTON_BLOCK_SPAWNER_TAG_2) then
    Exit;

  ctrlDown := TKMButtonFlat(Sender).MasterPanel.MasterControl.CtrlDown;
  ctrlOver := TKMControl(Sender);
  if (ctrlDown = nil) or not (ctrlDown is TKMButtonFlat) then
    Exit;
  if ctrlDown = ctrlOver then
    Exit;

  if (TKMButtonFlat(ctrlDown).Tag2 = BUTTON_BLOCK_SPAWNER_TAG_2) then
  begin
    if TKMButtonFlat(ctrlDown).Down <> TKMButtonFlat(ctrlOver).Down then
    begin
      TKMButtonFlat(ctrlOver).Down := TKMButtonFlat(ctrlDown).Down;
      Player_AnimalClick(nil);
    end;

  end;


end;

end.
