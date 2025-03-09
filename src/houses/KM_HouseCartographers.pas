unit KM_HouseCartographers;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_MinimapCartographer,
  KM_ResTypes;

type
  TKMCartographesMode = (cmChartman, cmSpy);
  TKMCartographersPaintLayer = (cplUnderground, cplMiningRadius, cplTowerRange, cplSpawners, cplObjects);

  TKMHouseCartographers = class(TKMHouseWFlagPoint)
  private
    //fMinimap : //not used right now
    fMode : TKMCartographesMode;
    fLayer: array[TKMCartographersPaintLayer] of Boolean;
    fHouseList : TKMHouseArray;
    fUndergoundDeposits : TKMPointTagList;
    fObjectsWithWares : TKMPointTagList;
    fSpawners : TKMWordArray;
    fMinimap : TKMMinimapCartographer;
    function GetLayer(aType : TKMCartographersPaintLayer) : Boolean;
  protected
    procedure SetFlagPoint(aFlagPoint: TKMPoint); override;
    function GetMaxDistanceToPoint: Integer; override;
    procedure Activate(aWasBuilt: Boolean); override;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad; override;
    procedure PostLoadMission; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;

    property Mode : TKMCartographesMode read fMode;
    property Layer[aType : TKMCartographersPaintLayer] : Boolean read GetLayer;
    property Minimap : TKMMinimapCartographer read fMinimap;
    function CanWork : Boolean;
    procedure ToggleLayer(aLayer : TKMCartographersPaintLayer); overload;
    procedure ToggleLayer(aLayer : Byte); overload;
    procedure CollectData(aLoc : TKMPoint);

  end;

implementation
uses
  KM_Game,
  KM_CommonUtils, KromUtils,
  KM_HandsCollection,
  KM_HouseHelpers,
  KM_HouseCollection,
  KM_ResMapElements, KM_Resource,
  KM_RenderAux, KM_RenderDebug, KM_RenderPool,
  KM_Terrain;

constructor TKMHouseCartographers.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var CP : TKMCartographersPaintLayer;
begin
  Inherited;
  fMode := cmChartman;
  for CP := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
    fLayer[CP] := true;
  fLayer[cplObjects] := true;
  If fUndergoundDeposits = nil then
  begin
    fUndergoundDeposits := TKMPointTagList.Create;
    fObjectsWithWares := TKMPointTagList.Create;
    fMinimap := TKMMinimapCartographer.Create(gTerrain.MapX, gTerrain.MapY);
  end;
  SetLength(fHouseList, 0);
end;

constructor TKMHouseCartographers.Load(LoadStream: TKMemoryStream);
var I, C: Integer;
begin
  Inherited;
  If fUndergoundDeposits = nil then
  begin
    fUndergoundDeposits := TKMPointTagList.Create;
    fObjectsWithWares := TKMPointTagList.Create;
    fMinimap := TKMMinimapCartographer.Create(gTerrain.MapX, gTerrain.MapY);
  end;
  fUndergoundDeposits.LoadFromStream(LoadStream);
  fObjectsWithWares.LoadFromStream(LoadStream);
  fMinimap.LoadFromStream(LoadStream);
  LoadStream.Read(fLayer, SizeOf(fLayer));
  LoadStream.Read(fSpawners);
  LoadStream.Read(C);
  SetLength(fHouseList, C);
  for I := 0 to C - 1 do
    LoadStream.Read(fHouseList[I], 4);


end;

destructor TKMHouseCartographers.Destroy;
begin
  fUndergoundDeposits.Free;
  fObjectsWithWares.Free;
  Inherited;
end;

procedure TKMHouseCartographers.Save(SaveStream: TKMemoryStream);
var I, C: Integer;
begin
  Inherited;
  fUndergoundDeposits.SaveToStream(SaveStream);
  fObjectsWithWares.SaveToStream(SaveStream);
  fMinimap.SaveToStream(SaveStream);
  SaveStream.Write(fLayer, SizeOf(fLayer));
  SaveStream.Write(fSpawners);
  C := length(fHouseList);
  SaveStream.Write(C);
  for I := 0 to C - 1 do
    SaveStream.Write(fHouseList[I].UID);
end;

procedure TKMHouseCartographers.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  If fUndergoundDeposits = nil then
  begin
    fUndergoundDeposits := TKMPointTagList.Create;
    fObjectsWithWares := TKMPointTagList.Create;
    fMinimap := TKMMinimapCartographer.Create(gTerrain.MapX, gTerrain.MapY);
  end;
end;

procedure TKMHouseCartographers.SetFlagPoint(aFlagPoint: TKMPoint);
begin
  Inherited;
end;

function TKMHouseCartographers.GetMaxDistanceToPoint: Integer;
begin
  Result := 100;
end;

procedure TKMHouseCartographers.SyncLoad;
var I: Integer;
begin
  Inherited;
  for I := 0 to High(fHouseList) do
    fHouseList[I] := gHands.GetHouseByUID(Integer(fHouseList[I]));
end;

procedure TKMHouseCartographers.PostLoadMission;
begin
  Inherited;
end;

procedure TKMHouseCartographers.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If not IsComplete or not HasWorkerInside then
    Exit;
end;

procedure TKMHouseCartographers.Paint;
  procedure PaintUndergroundDeposits;
  var
    I : Integer;
    C, factor : Cardinal;
  begin

    for I := 0 to fUndergoundDeposits.Count - 1 do
    begin
      case fUndergoundDeposits.Tag[I] of
        1: C := icYellow;
        2: C := icSteelBlue;
        3: C := icLightRed;
        4: C := icBlack;
        else
          C := $FFFFFFFF;
      end;
      factor := fUndergoundDeposits.Tag2[I] * 15 + 40;
      factor := factor shl 24;
      factor := factor or $00FFFFFF;
      C := C and factor;
      gRenderAux.Quad(fUndergoundDeposits[I].X, fUndergoundDeposits[I].Y, C);
    end;
  end;

  procedure PaintObjects;
  var
    I : Integer;
    C : Cardinal;
    factor : Byte;
  begin

    for I := 0 to fObjectsWithWares.Count - 1 do
    begin

      factor := Round(gMapElements[fObjectsWithWares.Tag[I]].ObjectPrice / gRes.MapElements.HighestPrice) * 255;
      C := $AA00FF00 + factor;
      //gRenderAux.Quad(fObjectsWithWares[I].X, fObjectsWithWares[I].Y, C);
      gRenderAux.CircleOnTerrain(fObjectsWithWares[I].X - 0.5, fObjectsWithWares[I].Y - 0.5, 0.75, C and $75FFFFFF, C);
    end;
  end;

  procedure PaintMiningRange(H : TKMHouse);
  begin
    If H.HouseType in [htQuarry, htIronMine, htBitinMine, htGoldMine, htCoalMine,
                        htFishermans, htFarm, htVineyard, htWoodcutters, htPottery] then
      gRenderPool.RenderDebug.PaintMiningRadius(H);
  end;

  procedure PaintTowerRange(H : TKMHouse);
  begin
    If H.HouseType in [htWatchTower, htWallTower] then
      gRenderPool.RenderDebug.RenderTiledArea(TKMHouseTower(H).Entrance, TKMHouseTower(H).RangeMin, TKMHouseTower(H).RangeMax,
                                              GetLength, $40FFFFFF, icWhite);
  end;
var I : Integer;
begin
  Inherited;
  If not IsComplete {or not HasWorkerInside} then
    Exit;

  if gMySpectator.Selected <> self then
    Exit;

  If fMode = cmSpy then
    Exit;
  If fLayer[cplUnderground] then
    PaintUndergroundDeposits;
  If fLayer[cplMiningRadius] then
    for I := 0 to High(fHouseList) do
      If fHouseList[I].IsValid then
        PaintMiningRange(fHouseList[I]);

  If fLayer[cplTowerRange] then
    for I := 0 to High(fHouseList) do
      If fHouseList[I].IsValid then
        PaintTowerRange(fHouseList[I]);

  If fLayer[cplSpawners] then
    for I := 0 to High(fSpawners) do
      gHands.PlayerAnimals.PaintSpawner(fSpawners[I]);

  If fLayer[cplObjects] then
    PaintObjects;

end;

function TKMHouseCartographers.CanWork: Boolean;
begin
  Result := FlagPoint <> PointBelowEntrance;
end;

function TKMHouseCartographers.GetLayer(aType: TKMCartographersPaintLayer): Boolean;
begin
  Result := fLayer[aType];
end;

procedure TKMHouseCartographers.ToggleLayer(aLayer : TKMCartographersPaintLayer);
begin
  fLayer[aLayer] := not fLayer[aLayer];
end;

procedure TKMHouseCartographers.ToggleLayer(aLayer : Byte);
begin
  ToggleLayer(TKMCartographersPaintLayer(aLayer) );
end;

procedure TKMHouseCartographers.CollectData(aLoc: TKMPoint);
  function ContainsSpawner(aID : Integer): Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to High(fSpawners) do
      If fSpawners[I] = aID then
        Exit(true);
  end;
  function ContainsHouse(aID : TKMHouse): Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to High(fHouseList) do
      If fHouseList[I] = aID then
        Exit(true);
  end;
var list : TKMPointTagList;
  I, J : Integer;
  houseList : TKMHouseArray;
begin
  list := TKMPointTagList.Create;
  gTerrain.FindDepositsWithDistance(aLoc, 10, list, 15);
  fUndergoundDeposits.AddListUnique(list);
  list.Clear;
  gTerrain.FindValuableObjects(aLoc, 15, list);
  fObjectsWithWares.AddListUnique(list);
  list.Free;


  for I := 0 to gHands.PlayerAnimals.SpawnersCount - 1 do
    If KMLengthDiag(aLoc, gHands.PlayerAnimals.Spawners[I].Loc) < 20 then
      If not ContainsSpawner(I) then
      begin
        J := length(fSpawners);
        SetLength(fSpawners, J + 1);
        fSpawners[J] := I;
      end;
  If FlagPoint = aLoc then
    FlagPoint := PointBelowEntrance;

  houseList := gHands.GetHousesInRadius(aLoc, Sqr(20));

  for I := 0 to high(houseList) do
    If not ContainsHouse(houseList[I]) then
    begin
      J := length(fHouseList);
      SetLength(fHouseList, J + 1);
      fHouseList[J] := houseList[I];
    end;

  fMinimap.RevealCircle(aLoc, 50);

end;

end.
