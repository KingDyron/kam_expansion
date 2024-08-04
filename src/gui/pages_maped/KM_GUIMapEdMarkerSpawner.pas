unit KM_GUIMapEdMarkerSpawner;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsTrackBar, KM_ControlsEdit,
   KM_Defaults, KM_Pics, KM_MapEdTypes,
   KM_Hand;


type
  TKMMapEdMarkerSpawner = class
  private
    fSpawner : PKMAnimalSpawner;
    fLoading : Boolean;
    fIndex: Integer;
    fType: TKMMapEdMarkerType;
    fOnDone : TNotifyEvent;
    procedure Spawner_Change(Sender: TObject);
  protected
    Panel_MarkerSpawner: TKMPanel;
      Button_SpawnerClose: TKMButton;
      TrackBar_MaxCount,
      TrackBar_Radius : TKMTrackBar;
      Edit_SpawnerPace : TKMNumericEdit;
      Button_Animals : array of TKMButtonFlat;
      RemoveSpawner : TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    property Index: Integer read fIndex;
    property MarkerType: TKMMapEdMarkerType read fType;
    property Spawner : PKMAnimalSpawner read fSpawner;

    procedure Show(aIndex: Integer);
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses
  KM_Game,
  KM_InterfaceGame,
  KM_HandsCollection,
  KM_Resource, KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI,
  KM_CommonClasses;


{ TKMMapEdMarkerSpawner }
constructor TKMMapEdMarkerSpawner.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
var I : Integer;
  UT : TKMUnitType;
begin
  inherited Create;

  Panel_MarkerSpawner := TKMPanel.Create(aParent, TB_PAD, 50, TB_MAP_ED_WIDTH - TB_PAD, 400);
  fOnDone := aOnDone;

  with TKMLabel.Create(Panel_MarkerSpawner, 0, PAGE_TITLE_Y, Panel_MarkerSpawner.Width, 0, gResTexts[2027], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  RemoveSpawner := TKMButtonFlat.Create(Panel_MarkerSpawner, 0, 25, 30, 30, 170, rxGui);
  RemoveSpawner.OnClick := Spawner_Change;

  Button_SpawnerClose := TKMButton.Create(Panel_MarkerSpawner, Panel_MarkerSpawner.Width-100, 25, 100, 25, gResTexts[TX_MAPED_CLOSE], bsGame);
  Button_SpawnerClose.OnClick := Spawner_Change;

  TrackBar_Radius := TKMTrackBar.Create(Panel_MarkerSpawner, 9, 60, Panel_MarkerSpawner.Width - 18, 2, 50);
  TrackBar_Radius.Position := 10;
  TrackBar_Radius.Caption := 'Spawner radius';
  TrackBar_Radius.OnChange := Spawner_Change;

  TrackBar_MaxCount := TKMTrackBar.Create(Panel_MarkerSpawner, 9, 100, Panel_MarkerSpawner.Width - 18, 1, 50);
  TrackBar_MaxCount.Position := 10;
  TrackBar_MaxCount.Caption := 'Max animal count';
  TrackBar_MaxCount.OnChange := Spawner_Change;

  with TKMLabel.Create(Panel_MarkerSpawner, 0, 150, Panel_MarkerSpawner.Width, 0, 'Spawner pace', fntOutline, taCenter) do
    Anchors := [anLeft, anTop];
  Edit_SpawnerPace := TKMNumericEdit.Create(Panel_MarkerSpawner, 9, 165, 100, 72000);
  Edit_SpawnerPace.Value := 500;
  Edit_SpawnerPace.OnChange := Spawner_Change;



  I := 0;
  for UT := ANIMAL_MIN to ANIMAL_MAX do
  begin
    SetLength(Button_Animals, I + 1);

    Button_Animals[I] := TKMButtonFlat.Create(Panel_MarkerSpawner, 9 + (I mod 6) * 33, 200 + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_Animals[I].Tag := byte(UT);
    Button_Animals[I].OnClick := Spawner_Change;
    inc(I);
  end;
  fLoading := false;
end;


procedure TKMMapEdMarkerSpawner.Spawner_Change(Sender: TObject);
var I : Integer;
  UT : TKMUnitType;
begin
  if fLoading then
    Exit;
  if Sender = Button_SpawnerClose then
  begin
    Hide;
    fOnDone(self);
    Exit;
  end else
  if Sender = RemoveSpawner then
  begin
    gHands.PlayerAnimals.RemoveSpawner(fIndex);
    Hide;
    fOnDone(self);
    Exit;
  end;

  for I := 0 to High(Button_Animals) do
    if Sender = Button_Animals[I] then
      Button_Animals[I].Down := not Button_Animals[I].Down;

  fSpawner.Radius := TrackBar_Radius.Position;
  fSpawner.MaxCount := TrackBar_MaxCount.Position;
  fSpawner.Pace := Edit_SpawnerPace.Value;

  fSpawner.ClearAnimalTypes;

  for I := 0 to High(Button_Animals) do
    if Button_Animals[I].Down then
    begin
      UT := TKMUnitType(Button_Animals[I].Tag);
      fSpawner.IncludeAnimal(UT);

    end;

end;


procedure TKMMapEdMarkerSpawner.Show(aIndex: Integer);
var I : Integer;
  UT : TKMUnitType;
begin
  fLoading := true;

  fType := mmtSpawner;
  fIndex := aIndex;

  fSpawner := gHands.PlayerAnimals.Spawners[fIndex];

  TrackBar_Radius.Position := fSpawner.Radius;
  TrackBar_MaxCount.Position := fSpawner.MaxCount;
  Edit_SpawnerPace.Value := fSpawner.Pace;

  Panel_MarkerSpawner.Show;

  for I := Low(Button_Animals) to High(Button_Animals) do
  begin
    UT := TKMUnitType(Button_Animals[I].Tag);

    Button_Animals[I].Down := fSpawner.HasType(UT);
  end;



  fLoading := false;
end;


procedure TKMMapEdMarkerSpawner.Hide;
begin
  Panel_MarkerSpawner.Hide;
end;


function TKMMapEdMarkerSpawner.Visible: Boolean;
begin
  Result := Panel_MarkerSpawner.Visible;
end;


end.
