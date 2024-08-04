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
  protected
    Panel_AnimalSpawner: TKMPanel;
      Button_UnitCancel: TKMButtonFlat;
      BrushSize: TKMTrackBar;
      Button_PlaceAnimals: array [0..high(Animal_Order)] of TKMButtonFlat;
      NumEd_FishCount: TKMNumericEdit;

      Button_PlaceSpawner : TKMButtonFlat;
      TrackBar_MaxCount,
      TrackBar_Radius : TKMTrackBar;
      Edit_SpawnerPace : TKMNumericEdit;
      Button_Animals : array of TKMButtonFlat;

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
var I, lineY : Integer;
  UT : TKMUnitType;
begin
  Panel_AnimalSpawner := TKMPanel.Create(aParent, 0, 28, aParent.Width, aParent.Height);

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
  for UT := ANIMAL_MIN to ANIMAL_MAX do
  begin

    Button_PlaceAnimals[I] := TKMButtonFlat.Create(Panel_AnimalSpawner, 9 + (I mod 6) * 33, lineY + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_PlaceAnimals[I].Tag := byte(UT);
    Button_PlaceAnimals[I].OnClick := Player_AnimalClick;
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
  TrackBar_Radius.Caption := 'Spawner radius';
  TrackBar_Radius.OnChange := Player_AnimalClick;

  TrackBar_MaxCount := TKMTrackBar.Create(Panel_AnimalSpawner, 9, lineY + 100, Panel_AnimalSpawner.Width - 18, 1, 50);
  TrackBar_MaxCount.Position := 10;
  TrackBar_MaxCount.Caption := 'Max animal count';
  TrackBar_MaxCount.OnChange := Player_AnimalClick;



  with TKMLabel.Create(Panel_AnimalSpawner, 0, lineY + 150, Panel_AnimalSpawner.Width, 0, 'Spawner pace', fntOutline, taCenter) do
    Anchors := [anLeft, anTop];
  Edit_SpawnerPace := TKMNumericEdit.Create(Panel_AnimalSpawner, 9, lineY + 165, 100, 72000);
  Edit_SpawnerPace.Value := 500;
  Edit_SpawnerPace.OnChange := Player_AnimalClick;



  I := 0;
  for UT := ANIMAL_MIN to ANIMAL_MAX do
  begin
    SetLength(Button_Animals, I + 1);

    Button_Animals[I] := TKMButtonFlat.Create(Panel_AnimalSpawner, 9 + (I mod 6) * 33, lineY + 200 + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_Animals[I].Tag := byte(UT);
    Button_Animals[I].OnClick := Player_AnimalClick;
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

end.
