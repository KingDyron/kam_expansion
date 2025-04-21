unit KM_HouseCartographers;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_AITypes,
  KM_Houses,
  KM_MinimapCartographer,
  KM_ResTypes;

const STRATEGY_HOUSES = 7;
type
  TKMCartographersMode = (cmChartman, cmSpy);
  TKMCartographersPaintLayer = (cplUnderground, cplMiningRadius, cplTowerRange, cplSpawners, cplObjects);

  TKMSpyPlayerData = record
    ID : Integer;

    Army : Word;
    StrongestUnit : TKMUnitType;
    Groups : TKMGroupTypeValidArray;
    WarHouses : array[0..STRATEGY_HOUSES - 1] of Word;

    Citizens : Word;
    WarriorsKilled : Word;
    Weapons : Integer;
    //AI
    UnlimitedEquip, AutoRepair : Boolean;
    AutoAttackRange : Byte;
    ArmyType : TKMArmyType;
    AttackTarget : array[TKMAIAttackTarget] of Boolean;
  end;
  TKMSpyPlayersData = array of TKMSpyPlayerData;

  TKMHouseCartographers = class(TKMHouseWFlagPoint)
  private
    fMode : TKMCartographersMode;
    //chartman
    fLayer: array[TKMCartographersPaintLayer] of Boolean;
    fHouseList : TKMHouseArray;
    fUndergoundDeposits : TKMPointTagList;
    fObjectsWithWares : TKMPointTagList;
    fSpawners : TKMWordArray;
    fMinimap : TKMMinimapCartographer;
    //spy
    fPlayerToSpy : Integer;
    fPlayers : TKMSpyPlayersData;
    fDoSpying : Boolean;
    procedure SetMode(aMode : TKMCartographersMode);
    function GetLayer(aType : TKMCartographersPaintLayer) : Boolean;
    procedure SetDoSpying(aValue : Boolean);
    procedure SetPlayer(aValue : Integer);
    function NeededWares : TKMWarePlan; overload;
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

    property Mode : TKMCartographersMode read fMode write SetMode;
    property Layer[aType : TKMCartographersPaintLayer] : Boolean read GetLayer;
    property Minimap : TKMMinimapCartographer read fMinimap;
    property DoSpying : Boolean read fDoSpying write SetDoSpying;
    property PlayerToSpy : Integer read fPlayerToSpy write SetPlayer;
    property SpiedPlayers : TKMSpyPlayersData read fPlayers;
    function SpiedPlayer : TKMSpyPlayerData;
    function NeededWares(aMode : TKMCartographersMode) : TKMWarePlan; overload;
    function NeedsWare(aWare : TKMWareType) : Byte;

    function CanWork : Boolean;
    procedure ToggleLayer(aLayer : TKMCartographersPaintLayer); overload;
    procedure ToggleLayer(aLayer : Byte); overload;
    procedure CollectChartmanData(aLoc : TKMPoint);
    procedure CollectSpyData(aPlayer : Integer);
    class function StrategyHouse(aID : Integer) : TKMHouseType;
  end;
implementation
uses
  KM_Game,
  KM_CommonUtils, KromUtils,
  KM_HandsCollection, KM_Hand, KM_Entity,
  KM_HouseHelpers,
  KM_HouseCollection,
  KM_ResMapElements, KM_Resource,
  KM_InterfaceGame,
  KM_RenderAux, KM_RenderDebug, KM_RenderPool,
  KM_Terrain;

constructor TKMHouseCartographers.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var CP : TKMCartographersPaintLayer;
begin
  Inherited;
  fMode := cmChartman;
  fPlayerToSpy := -1;
  fDoSpying := false;

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
  LoadStream.Read(fDoSpying);
  LoadStream.ReadData(fMode);
  LoadStream.Read(C);
  SetLength(fHouseList, C);
  for I := 0 to C - 1 do
    LoadStream.Read(fHouseList[I], 4);

  LoadStream.Read(fPlayerToSpy);
  LoadStream.Read(C);
  SetLength(fPlayers, C);
  for I := 0 to C - 1 do
    LoadStream.Read(fPlayers[I], SizeOf(fPlayers[I]));
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
  SaveStream.Write(fDoSpying);
  SaveStream.WriteData(fMode);
  C := length(fHouseList);
  SaveStream.Write(C);
  for I := 0 to C - 1 do
    SaveStream.Write(fHouseList[I].UID);

  SaveStream.Write(fPlayerToSpy);
  C := length(fPlayers);
  SaveStream.Write(C);
  for I := 0 to C - 1 do
    SaveStream.Write(fPlayers[I], SizeOf(fPlayers[I]));

end;

destructor TKMHouseCartographers.Destroy;
begin
  fUndergoundDeposits.Free;
  fObjectsWithWares.Free;
  Inherited;
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
  If fMode = cmSpy then
    aFlagPoint := PointBelowEntrance;
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
    tile: Word;
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
      case fUndergoundDeposits.Tag[I] of
        1: tile := 750; //gold
        2: tile := 751; //iron
        3: tile := 752; //bitin
        4: tile := 753; //coal
        else
          tile := 0;
      end;
      factor := fUndergoundDeposits.Tag2[I] * 15 + 40;
      factor := factor shl 24;
      factor := factor or $00FFFFFF;
      C := C and factor;
      //gRenderAux.Quad(fUndergoundDeposits[I].X, fUndergoundDeposits[I].Y, C);
      gRenderAux.CircleOnTerrain(fUndergoundDeposits[I].X - 0.5, fUndergoundDeposits[I].Y - 0.5, 0.2, C, 0);
      gRenderPool.RenderTerrain.RenderTile(tile, fUndergoundDeposits[I].X, fUndergoundDeposits[I].Y, 0);
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
      If gTerrain.Land[fObjectsWithWares[I].Y, fObjectsWithWares[I].X].Obj = 255 then
        Continue;
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
  If not IsComplete or (fMode = cmSpy ) then
    Exit;

  if gMySpectator.Selected <> self then
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
  If fMode = cmChartman then
    Result := (FlagPoint <> PointBelowEntrance) and HasWaresIn(NeededWares)
  else
    Result := fDoSpying and HasWaresIn(NeededWares) and (fPlayerToSpy <> -1);
end;

procedure TKMHouseCartographers.SetMode(aMode: TKMCartographersMode);
begin
  fMode := aMode;
  If fMode = cmSpy then
    FlagPoint := PointBelowEntrance;
end;

function TKMHouseCartographers.GetLayer(aType: TKMCartographersPaintLayer): Boolean;
begin
  Result := fLayer[aType];
end;

procedure TKMHouseCartographers.SetDoSpying(aValue : Boolean);
begin
  fDoSpying := aValue;
end;

procedure TKMHouseCartographers.SetPlayer(aValue : Integer);
begin
  fPlayerToSpy := aValue;
end;

function TKMHouseCartographers.NeededWares: TKMWarePlan;
begin
  Result := NeededWares(fMode);
end;
function TKMHouseCartographers.NeededWares(aMode: TKMCartographersMode): TKMWarePlan;
begin
  Result.Clear;
  If aMode = cmChartman then
    Result.AddWare(wtFeathers, 3)
  else
  begin
    Result.AddWare(wtFeathers, 1);
    Result.AddWare(wtWine, 5);
  end;
end;
procedure TKMHouseCartographers.ToggleLayer(aLayer : TKMCartographersPaintLayer);
begin
  fLayer[aLayer] := not fLayer[aLayer];
end;

procedure TKMHouseCartographers.ToggleLayer(aLayer : Byte);
begin
  ToggleLayer(TKMCartographersPaintLayer(aLayer) );
end;

function TKMHouseCartographers.SpiedPlayer: TKMSpyPlayerData;
var I : Integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  Result.ID := -1;
  for I := 0 to High(fPlayers) do
    If fPlayers[I].ID = fPlayerToSpy then
      Exit(fPlayers[I]);
end;

function TKMHouseCartographers.NeedsWare(aWare: TKMWareType): Byte;
begin
  Result := NeededWares.HasWare(aWare);
end;

procedure TKMHouseCartographers.CollectChartmanData(aLoc: TKMPoint);

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

  procedure DeleteHouse(aID : Integer);
  var I : Integer;
  begin
    for I := aID to High(fHouseList) - 1 do
      fHouseList[I] := fHouseList[I + 1];
    SetLength(fHouseList, high(fHouseList));
  end;

var list : TKMPointTagList;
  I, J : Integer;
  houseList : TKMHouseArray;
  rect : TKMRect;
begin
  list := TKMPointTagList.Create;
  rect := KMRectGrow(aLoc, 15);
  gTerrain.FindDepositsWithDistance(aLoc, 15, list, 1);

  //delete old data from this place
  for I := fUndergoundDeposits.Count - 1 downto 0 do
    If KMInRect(fUndergoundDeposits[I], rect) then
      fUndergoundDeposits.Delete(I);
  fUndergoundDeposits.AddListUnique(list);
  list.Clear;

  gTerrain.FindValuableObjects(aLoc, 15, list);
  //delete old data from this place
  for I := fObjectsWithWares.Count - 1 downto 0 do
    If KMInRect(fObjectsWithWares[I], rect) then
      fObjectsWithWares.Delete(I);
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

  for I := high(fHouseList) downto 0 do
    If not fHouseList[I].IsValid(htAny, false, true) then
      DeleteHouse(I);

  for I := 0 to high(houseList) do
    If not ContainsHouse(houseList[I]) then
    begin
      J := length(fHouseList);
      SetLength(fHouseList, J + 1);
      fHouseList[J] := houseList[I];
    end;

  fMinimap.RevealCircle(aLoc, 50);

end;

procedure TKMHouseCartographers.CollectSpyData(aPlayer: Integer);
var hand : TKMHand;
  I, id : Integer;
  AAT : TKMAIAttackTarget;
begin
  If fPlayerToSpy = aPlayer then
  begin
    //fPlayerToSpy := -1;
    fDoSpying := false;
  end;

  hand := gHands[aPlayer];
  id := -1;
  for I := 0 to High(fPlayers) do
    If fPlayers[I].ID = aPlayer then
    begin
      id := I;
      Break;
    end;
  If id = -1 then
  begin
    id := length(fPlayers);
    SetLength(fPlayers, id + 1);
  end;
  fPlayers[id].ID := aPlayer;
  fPlayers[id].Army := hand.Stats.GetArmyCount;
  fPlayers[id].Groups := hand.GetGroupsCount;

  //statistics
  fPlayers[id].Citizens := hand.Stats.GetCitizensCount;
  fPlayers[id].WarriorsKilled := hand.Stats.GetWarriorsKilled;
  fPlayers[id].Weapons := 0;
  for I := low(BarracksResOrder) to High(BarracksResOrder) do
    If BarracksResOrder[I] in WARES_VALID then
      Inc(fPlayers[id].Weapons, hand.Stats.Wares[BarracksResOrder[I]].ActualCnt);

  //AI
  fPlayers[id].UnlimitedEquip := hand.AI.Setup.UnlimitedEquip;
  fPlayers[id].AutoRepair := hand.AI.Setup.IsRepairAlways;
  fPlayers[id].AutoAttackRange := hand.AI.Setup.AutoAttackRange;
  fPlayers[id].ArmyType := hand.AI.Setup.ArmyType;
  fPlayers[id].StrongestUnit := hand.GetBestUnit;

  for AAT := Low(TKMAIAttackTarget) to High(TKMAIAttackTarget) do
    fPlayers[id].AttackTarget[AAT] := false;

  for I := 0 to hand.AI.General.Attacks.Count - 1 do
      fPlayers[id].AttackTarget[hand.AI.General.Attacks[I].Target] := true;

  for I := 0 to High(fPlayers[id].WarHouses) do
    case I of
      0..5: fPlayers[id].WarHouses[I] := hand.Stats.GetHouseQty(StrategyHouse(I));
      6: fPlayers[id].WarHouses[I] := hand.Stats.GetHouseQty([htWall, htWall2, htWall3, htWall4, htWall5]);
    end;

end;

class function TKMHouseCartographers.StrategyHouse(aID: Integer): TKMHouseType;
begin
  case aID of
    0: Result := htBarracks;
    1: Result := htTownhall;
    2: Result := htPalace;
    3: Result := htSiegeWorkshop;
    4: Result := htWallTower;
    5: Result := htWatchTower;
    6: Result := htWall;
    else
      Result := htNone;
  end;
end;

end.
