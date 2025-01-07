unit KM_GUIMapEdMarkerSpawner;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   KM_Controls, KM_ControlsBase, KM_ControlsTrackBar, KM_ControlsEdit,
   KM_Defaults, KM_Pics, KM_MapEdTypes,
   KM_InterfaceGame,
   KM_Hand;


type
  TKMMapEdMarkerSpawner = class
  private
    fSpawner : PKMAnimalSpawner;
    fLoading : Boolean;
    fIndex: Integer;
    fType: TKMMapEdMarkerType;
    fOnDone : TNotifyEvent;
    procedure RefreshButtons;
    procedure Spawner_Change(Sender: TObject);
    procedure Spawner_AnimalTypeChange(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Spawner_AnimalOver(Sender: TObject; Shift: TShiftState);
  protected
    Panel_MarkerSpawner: TKMPanel;
      Button_SpawnerClose: TKMButton;
      TrackBar_MaxCount,
      TrackBar_Radius : TKMTrackBar;
      Edit_SpawnerPace : TKMNumericEdit;
      Button_Animals : array[0..high(Animal_Order)] of TKMButtonFlat;
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
  KM_HandsCollection,
  KM_Resource, KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI,
  KM_CommonClasses;


{ TKMMapEdMarkerSpawner }
constructor TKMMapEdMarkerSpawner.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
var I, J: Integer;
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
  TrackBar_Radius.Caption := gResTexts[2154];
  TrackBar_Radius.OnChange := Spawner_Change;

  TrackBar_MaxCount := TKMTrackBar.Create(Panel_MarkerSpawner, 9, 100, Panel_MarkerSpawner.Width - 18, 1, 50);
  TrackBar_MaxCount.Position := 10;
  TrackBar_MaxCount.Caption := gResTexts[2155];
  TrackBar_MaxCount.OnChange := Spawner_Change;

  with TKMLabel.Create(Panel_MarkerSpawner, 0, 150, Panel_MarkerSpawner.Width, 0, gResTexts[2156], fntOutline, taCenter) do
    Anchors := [anLeft, anTop];
  Edit_SpawnerPace := TKMNumericEdit.Create(Panel_MarkerSpawner, 9, 168, 100, 72000);
  Edit_SpawnerPace.Value := 500;
  Edit_SpawnerPace.OnChange := Spawner_Change;
  Edit_SpawnerPace.Left := Panel_MarkerSpawner.Width div 2 - Edit_SpawnerPace.Width div 2;

  I := 0;
  for J := 0 to high(Animal_Order) do
  begin
    UT := Animal_Order[J];
    Button_Animals[I] := TKMButtonFlat.Create(Panel_MarkerSpawner, 9 + (I mod 6) * 33, 200 + (I div 6) * 33, 30, 30, gRes.Units[UT].GUIIcon, rxGui);
    Button_Animals[I].Tag := byte(UT);
    Button_Animals[I].Tag2 := BUTTON_BLOCK_SPAWNER_TAG_2;
    Button_Animals[I].OnMouseDown := Spawner_AnimalTypeChange;
    Button_Animals[I].OnMouseOver := Spawner_AnimalOver;
    inc(I);
  end;
  fLoading := false;
end;

procedure TKMMapEdMarkerSpawner.RefreshButtons;
var I : Integer;
  UT : TKMUnitType;
begin
  for I := 0 to High(Button_Animals) do
  begin
    UT := TKMUnitType(Button_Animals[I].Tag);
    Button_Animals[I].Down:= fSpawner.HasType(UT);
  end;
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

procedure TKMMapEdMarkerSpawner.Spawner_AnimalTypeChange(Sender: TObject; X: Integer; Y: Integer; Shift: TShiftState);
begin
  Spawner_Change(Sender);
end;

procedure TKMMapEdMarkerSpawner.Spawner_AnimalOver(Sender: TObject; Shift: TShiftState);
var ctrlDown : TKMControl;
  ctrlOver : TKMControl;
  UT1, UT2 : TKMUnitType;
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
    UT1 := TKMUnitType(ctrlDown.Tag);
    UT2 := TKMUnitType(ctrlOver.Tag);
    if fSpawner.HasType(UT1) <> fSpawner.HasType(UT2) then
    begin
      if fSpawner.HasType(UT1) then
        fSpawner.IncludeAnimal(UT2)
      else
        fSpawner.ExcludeAnimal(UT2);

      RefreshButtons;
    end;

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
