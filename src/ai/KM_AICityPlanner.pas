unit KM_AICityPlanner;
{$I KaM_Remake.inc}
interface
uses
  KM_TerrainFinder,

  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_ResTypes;


type
  TFindNearest = (fnHouse, fnStone, fnTrees, fnSoil, fnWater, fnCoal, fnIron, fnGold, fnBitin, fnClay);

  // Terrain finder optimized for CityPlanner demands of finding resources and houses
  TKMTerrainFinderCity = class(TKMTerrainFinderCommon)
  protected
    fOwner: TKMHandID;
    function CanWalkHere(const X,Y: Word): Boolean; override;
    function CanUse(const X,Y: Word): Boolean; override;
  public
    FindType: TFindNearest;
    HouseType: TKMHouseType;
    constructor Create(aOwner: TKMHandID);
    procedure OwnerUpdate(aPlayer: TKMHandID);
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
  end;

  TKMCityPlanner = class
  private
    fOwner: TKMHandID;
    fListGold: TKMPointList; // List of possible goldmine locations
    fFinder: TKMTerrainFinderCity;

    function GetSeeds(aHouseType: array of TKMHouseType): TKMPointArray;

    function NextToOre(aHouse: TKMHouseType; aOreType: TKMWareType; out aLoc: TKMPoint; aNearAnyHouse: Boolean = False): Boolean;
    function NextToHouse(aHouse: TKMHouseType; aSeed, aAvoid: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
    function NextToStone(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
    function NextToClay(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
    function NextToWater(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
    function NextToTrees(aHouse: TKMHouseType; aSeed: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
    function NextToGrass(aHouse: TKMHouseType; aSeed: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
    function ModuleFruitTree(out aLoc : TKMPoint) : Boolean;
    function PlaceForWell(out aLoc : TKMPoint) : Boolean;
  public
    constructor Create(aPlayer: TKMHandID);
    destructor Destroy; override;

    procedure AfterMissionInit;

    function FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean; overload;
    procedure FindNearest(const aStart: TKMPointArray; aRadius: Byte; aType: TFindNearest; aPass: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    procedure FindNearest(const aStart: TKMPointArray; aRadius: Byte; aHouse: TKMHouseType; aMaxCount: Word; aLocs: TKMPointTagList); overload;
    function FindPlaceForHouse(aHouse: TKMHouseType; out aLoc: TKMPoint; out aIgnoreRoad : Boolean): Boolean;
    procedure OwnerUpdate(aPlayer: TKMHandID);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


const
  AI_FIELD_HEIGHT = 3;
  AI_FIELD_WIDTH = 4;
  AI_FIELD_MAX_AREA = (AI_FIELD_WIDTH * 2 + 1) * AI_FIELD_HEIGHT;


implementation
uses
  Math,
  KM_HandsCollection, KM_Hand, KM_HandTypes,
  KM_AIFields, KM_AIInfluences,
  KM_Terrain,
  KM_Resource, KM_ResUnits, KM_NavMesh,
  KM_Houses, KM_CommonUtils, KM_CommonTypes;


{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TKMHandID);
begin
  inherited Create;
  fOwner := aPlayer;
  fFinder := TKMTerrainFinderCity.Create(fOwner);

  fListGold := TKMPointList.Create;
end;


destructor TKMCityPlanner.Destroy;
begin
  fListGold.Free;
  fFinder.Free;

  inherited;
end;


function TKMCityPlanner.FindPlaceForHouse(aHouse: TKMHouseType; out aLoc: TKMPoint; out aIgnoreRoad : Boolean): Boolean;
begin
  Result := False;
  aIgnoreRoad := false;
  case aHouse of
    htStore:           Result := NextToHouse(aHouse, [htAny], [], aLoc);
    htArmorSmithy:     Result := NextToHouse(aHouse, [htIronSmithy, htCoalMine, htBarracks], [], aLoc);
    htBakery:          Result := NextToHouse(aHouse, [htMill], [], aLoc);
    htBarracks:        Result := NextToHouse(aHouse, [htAny], [], aLoc);
    htWatchTower:      Result := NextToHouse(aHouse, [htBarracks], [], aLoc);
    htWallTower:       Result := NextToHouse(aHouse, [htBarracks, htStore], [], aLoc);
    htButchers:        Result := NextToHouse(aHouse, [htSwine], [], aLoc);
    htInn:             Result := NextToHouse(aHouse, [htAny], [htInn], aLoc);
    htIronSmithy:      Result := NextToHouse(aHouse, [htIronMine, htCoalMine], [], aLoc);
    htMetallurgists:   Result := NextToHouse(aHouse, [htGoldMine], [], aLoc);

    htHovel,
    htMill:            Result := NextToHouse(aHouse, [htFarm], [], aLoc);
    htStables:         Result := NextToHouse(aHouse, [htFarm], [], aLoc);
    htSwine:           Result := NextToHouse(aHouse, [htFarm], [], aLoc);

    htSawmill:         Result := NextToHouse(aHouse, [htWoodcutters], [], aLoc);
    htSchool:          Result := NextToHouse(aHouse, [htStore, htBarracks], [], aLoc);
    htTannery:         Result := NextToHouse(aHouse, [htSwine], [], aLoc);
    htWeaponSmithy:    Result := NextToHouse(aHouse, [htIronSmithy, htCoalMine, htBarracks], [], aLoc);
    htWeaponWorkshop,
    htArmorWorkshop:   Result := NextToHouse(aHouse, [htSawmill, htBarracks], [], aLoc);
    htTailorsShop:  Result := NextToHouse(aHouse, [htTannery, htBarracks], [], aLoc);

    htCoalMine:      Result := NextToOre(aHouse, wtCoal, aLoc);
    htGoldMine:      Result := NextToOre(aHouse, wtGoldOre, aLoc);
    htIronMine:      Result := NextToOre(aHouse, wtIronOre, aLoc);
    htBitinMine:     Result := NextToOre(aHouse, wtBitinOre, aLoc);
    htPottery:     Result := NextToClay(aHouse, aLoc);

    htQuarry:        Result := NextToStone(aHouse, aLoc);
    htWoodcutters:   Result := NextToTrees(aHouse, [htStore, htWoodcutters, htSawmill], aLoc);
    htFarm:          Result := NextToGrass(aHouse, [htAny], aLoc);
    htVineyard:      Result := NextToGrass(aHouse, [htAny], aLoc);
    htWell:           begin
                        Result := PlaceForWell(aLoc);
                        If not Result then
                          Result := NextToHouse(aHouse, [htAny], [], aLoc)
                          
                      end;
    htFishermans:     Result := NextToWater(aHouse, aLoc);
    htAppleTree:      begin
                        Result := ModuleFruitTree(aLoc);
                        If not Result then
                          Result := NextToHouse(aHouse, [htCottage, htHouse], [], aLoc)
                        else
                          aIgnoreRoad := true;
                      end;
    htStoneWorkshop:  Result := NextToHouse(aHouse, [htWoodCutters, htQuarry], [], aLoc);
    htIronFoundry:  Result := NextToHouse(aHouse, [htIronSmithy, htCoalMine], [], aLoc);
    //htMarketplace:;
    htSiegeWorkshop: Result := NextToHouse(aHouse, [htIronFoundry, htStoneWorkshop], [], aLoc);
    htTownHall: Result := NextToHouse(aHouse, [htAny], [], aLoc);
    //htWatchTower:;
  end;

  //If we failed to find something, try to place the house anywhere (better than ignoring it)
  if not Result and not (aHouse in [htAppleTree, htCoalMine, htGoldMine, htIronMine, htBitinMine, htQuarry, htFarm, htVineyard, htFishermans, htWell, htPottery, htHovel]) then
    Result := NextToHouse(aHouse, [htAny], [], aLoc);
end;


//Receive list of desired house types
//Output list of locations below these houses
function TKMCityPlanner.GetSeeds(aHouseType: array of TKMHouseType): TKMPointArray;
var
  UsedIndex : TIntegerArray;
  function IndexUsed(aID : Integer) : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to High(UsedIndex) do
      If UsedIndex[I] = aID then
        Exit(true);
  end;
var
  I, K, index, J: Integer;
  H: TKMHouseType;
  Count, HQty: Integer;
  House: TKMHouse;
begin
  Count := 0;
  SetLength(Result, Count);

  for I := Low(aHouseType) to High(aHouseType) do
  begin
    H := aHouseType[I];
    HQty := gHands[fOwner].Stats.GetHouseQty(H);

    //always adds all stores
    {for K := 0 to gHands[fOwner].Houses.Stores.Count - 1 do
    begin
      House := gHands[fOwner].Houses.Stores[K];
      if House <> nil then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := House.PointBelowEntrance;
        Inc(Count);
      end;
    end;}

    //htAny picks three random houses for greater variety
    for K := 0 to 1 + Byte(H = htAny) * 2 do
    begin
      index := -1;
      //try to find new index 5 times max
      J := 0;
      while (J < 5) and ((index = -1) or ( not IndexUsed(index))) do
      begin
        index := KaMRandom(HQty, 'TKMCityPlanner.GetSeeds') + 1;
        J := J + 1;
      end;

      //use Index
      setLength(UsedIndex, length(UsedIndex) + 1);
      UsedIndex[high(UsedIndex)] := index;

      House := gHands[fOwner].Houses.FindHouse(H, 0, 0, index);
      if House <> nil then
      begin
        If Result.Contains(House.PointBelowEntrance) then
          Continue;
        SetLength(Result, Count + 1);
        //Position is as good as Entrance for city planning
        Result[Count] := House.PointBelowEntrance;
        Inc(Count);
      end;
    end;
  end;
end;


procedure TKMCityPlanner.AfterMissionInit;
var
  I,K: Integer;
begin
  //Mark all spots where we could possibly place a goldmine
  //some smarter logic can clip left/right edges later on?
  for I := 1 to gTerrain.MapY - 2 do
  for K := 1 to gTerrain.MapX - 2 do
  if gTerrain.TileGoodForGoldmine(K,I) then
    fListGold.Add(KMPoint(K,I));
end;


function TKMCityPlanner.NextToGrass(aHouse: TKMHouseType; aSeed: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
  function CanPlaceHouse(aHouse: TKMHouseType; aX, aY: Word): Boolean;
  var
    I, K: Integer;
    FieldCount: Integer;
  begin
    Result := False;
    if gHands[fOwner].CanAddHousePlanAI(aX, aY, aHouse, True) then
    begin
      FieldCount := 0;
      for I := Min(aY - 2, gTerrain.MapY - 1) to Max(aY + 2 + AI_FIELD_HEIGHT - 1, 1) do
      for K := Max(aX - AI_FIELD_WIDTH, 1) to Min(aX + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddFieldPlan(KMPoint(K,I), ftCorn)
      //Skip fields within actual house areas
      and ((aHouse <> htFarm)     or not InRange(I, aY-2, aY) or not InRange(K, aX-1, aX+2))
      and ((aHouse <> htVineyard) or not InRange(I, aY-1, aY) or not InRange(K, aX-2, aX)) then
      begin
        Inc(FieldCount);
        //Request slightly more than we need to have a good choice
        if FieldCount >= Min(AI_FIELD_MAX_AREA, IfThen(aHouse = htFarm, 16, 10)) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
var
  I, K, J: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  S: TKMPoint;
begin
  Result := False;
  Assert(aHouse in [htFarm, htVineyard]);

  SeedLocs := GetSeeds(aSeed);

  BestBid := MaxSingle;
  for J := Low(SeedLocs) to High(SeedLocs) do
  begin
    S := SeedLocs[J];
    for I := Max(S.Y - 7, 1) to Min(S.Y + 6, gTerrain.MapY - 1) do
    for K := Max(S.X - 7, 1) to Min(S.X + 7, gTerrain.MapX - 1) do
    if CanPlaceHouse(aHouse, K, I) then
    begin
      Bid := KMLength(KMPoint(K,I), S)
             - gAIFields.Influences.Ownership[fOwner, I, K] / 5
             + KaMRandom('TKMCityPlanner.NextToGrass') * 4;
      if Bid < BestBid then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
  end;
end;


function TKMCityPlanner.NextToHouse(aHouse: TKMHouseType; aSeed, aAvoid: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
var
  I: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  Locs: TKMPointTagList;
begin
  Result := False;

  SeedLocs := GetSeeds(aSeed);

  Locs := TKMPointTagList.Create;
  try
    FindNearest(SeedLocs, 32, aHouse, 12, Locs);

    BestBid := MaxSingle;
    for I := 0 to Locs.Count - 1 do
    begin
      Bid := Locs.Tag[I]
             - gAIFields.Influences.Ownership[fOwner,Locs[I].Y,Locs[I].X] / 5;
      if (Bid < BestBid) then
      begin
        aLoc := Locs[I];
        BestBid := Bid;
        Result := True;
      end;
    end;
  finally
    Locs.Free;
  end;
end;


//Called when AI needs to find a good spot for a new Quary
function TKMCityPlanner.NextToStone(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RAD = 8;
var
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPoint;
  Locs: TKMPointTagList;
  SeedLocs: TKMPointArray;
  J, M: Integer;
  tmp: TKMPointDir;
begin
  Result := False;

  SeedLocs := GetSeeds([htAny]);

  Locs := TKMPointTagList.Create;
  try
    //Find all tiles from which stone can be mined, by walking to them
    FindNearest(SeedLocs, 32, fnStone, [tpWalk], 12, Locs);
    if Locs.Count = 0 then Exit;

    //Check few random tiles if we can build Quary nearby
    BestBid := MaxSingle;
    for J := 0 to 2 do
    begin
      M := KaMRandom(Locs.Count, 'TKMCityPlanner.NextToStone');
      StoneLoc := Locs[M];
      for I := Max(StoneLoc.Y - SEARCH_RAD, 1) to Min(StoneLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
      for K := Max(StoneLoc.X - SEARCH_RAD, 1) to Min(StoneLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
      begin
        Bid := Locs.Tag[M]
               - gAIFields.Influences.Ownership[fOwner,I,K] / 10
               + KaMRandom('TKMCityPlanner.NextToStone_2') * 3
               + KMLengthDiag(K, I, StoneLoc); //Distance to stone is important
        if (Bid < BestBid) then
        begin
          aLoc := KMPoint(K,I);
          BestBid := Bid;
          Result := True;
        end;
      end;
    end;
  finally
    Locs.Free;
  end;

  //Make sure stonemason actually can reach some stone (avoid build-destroy loop)
  if Result then
    if not gTerrain.FindStone(aLoc, gRes.Units[utStonemason].MiningRange, KMPOINT_ZERO, True, nil, tmp) then
      Result := False;
end;



//Called when AI needs to find a good spot for a new Quary
function TKMCityPlanner.NextToClay(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RAD = 8;
var
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPoint;
  Locs: TKMPointTagList;
  SeedLocs: TKMPointArray;
  J, M: Integer;
  tmp: TKMPoint;
begin
  Result := False;

  SeedLocs := GetSeeds([htAny]);

  Locs := TKMPointTagList.Create;
  try
    //Find all tiles from which stone can be mined, by walking to them
    FindNearest(SeedLocs, 32, fnClay, [tpWalk], 12, Locs);
    if Locs.Count = 0 then Exit;

    //Check few random tiles if we can build Quary nearby
    BestBid := MaxSingle;
    for J := 0 to 2 do
    begin
      M := KaMRandom(Locs.Count, 'TKMCityPlanner.NextToStone');
      StoneLoc := Locs[M];
      for I := Max(StoneLoc.Y - SEARCH_RAD, 1) to Min(StoneLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
      for K := Max(StoneLoc.X - SEARCH_RAD, 1) to Min(StoneLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
      begin
        Bid := Locs.Tag[M]
               - gAIFields.Influences.Ownership[fOwner,I,K] / 10
               + KaMRandom('TKMCityPlanner.NextToStone_2') * 3
               + KMLengthDiag(K, I, StoneLoc); //Distance to stone is important
        if (Bid < BestBid) then
        begin
          aLoc := KMPoint(K,I);
          BestBid := Bid;
          Result := True;
        end;
      end;
    end;
  finally
    Locs.Free;
  end;

  //Make sure stonemason actually can reach some stone (avoid build-destroy loop)
  if Result then
    if not gTerrain.FindClay(aLoc, KMPOINT_ZERO, True, nil, tmp) then
      Result := False;
end;

//Called when AI needs to find a good spot for a new Quary
function TKMCityPlanner.NextToWater(aHouse: TKMHouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RAD = 8;
var
  I, K: Integer;
  Bid, BestBid: Single;
  StoneLoc: TKMPoint;
  Locs: TKMPointTagList;
  SeedLocs: TKMPointArray;
  J, M: Integer;
  tmp: TKMPoint;
begin
  Result := False;

  SeedLocs := GetSeeds([htAny]);

  Locs := TKMPointTagList.Create;
  try
    //Find all tiles from which stone can be mined, by walking to them
    FindNearest(SeedLocs, 32, fnWater, [tpWalk], 12, Locs);
    if Locs.Count = 0 then Exit;

    //Check few random tiles if we can build fishermans nearby
    BestBid := MaxSingle;
    for J := 0 to 2 do
    begin
      M := KaMRandom(Locs.Count, 'TKMCityPlanner.NextToStone');
      StoneLoc := Locs[M];
      for I := Max(StoneLoc.Y - SEARCH_RAD, 1) to Min(StoneLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
      for K := Max(StoneLoc.X - SEARCH_RAD, 1) to Min(StoneLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
      if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
      begin
        Bid := Locs.Tag[M]
               - gAIFields.Influences.Ownership[fOwner,I,K] / 10
               + KaMRandom('TKMCityPlanner.NextToStone_2') * 3
               + KMLengthDiag(K, I, StoneLoc); //Distance to stone is important
        if (Bid < BestBid) then
        begin
          aLoc := KMPoint(K,I);
          BestBid := Bid;
          Result := True;
        end;
      end;
    end;
  finally
    Locs.Free;
  end;

  //Make sure stonemason actually can reach some stone (avoid build-destroy loop)
  if Result then
    if not gTerrain.FindClay(aLoc, KMPOINT_ZERO, True, nil, tmp) then
      Result := False;
end;

function TKMCityPlanner.ModuleFruitTree(out aLoc: TKMPoint): Boolean;


var I, K, count : integer;
  H : TKMHouse;
  tree, tmp: TKMHouseAppleTree;
  trees : array of TKMHouseAppleTree;//list of not parented trees
  points : TKMPointArray;

  function ContainsTree(aHouse : TKMHouseAppleTree) : Boolean;
  var J : integer;
  begin
    Result := false;
    for J := 0 to High(trees) do
      If trees[J] = aHouse then
        Exit(true);
  end;

  function CheckTree(aTree : TKMHouseAppleTree) : Boolean;
  var J : integer;
    entr : TKMPoint;
    P : TKMPoint;
  begin
    Result := false;
    entr := aTree.Entrance;
    for J := Low(points) to High(points) do
    begin
      P := entr + points[J];
      if gHands[fOwner].CanAddHousePlanAI(P.X, P.Y, htAppleTree, True) then
      begin
        aLoc := points[J] + entr;
        Result := true;
      end;
    end;
  end;

begin
  Result := false;
  count := 0;
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    If not H.IsValid(htAppleTree) then
      Continue;

    If TKMHouseAppleTree(H).ParentTree <> nil then
      tmp := TKMHouseAppleTree(TKMHouseAppleTree(H).ParentTree)
    else
      tmp := TKMHouseAppleTree(H);

    If tmp.ChildCount >= 6 then
      Continue;

    If ContainsTree(tmp) then
      Continue;
    Inc(Count);
    SetLength(trees, count);
    trees[count - 1] := tmp;
  end;
  //offsets of child trees
  points := [KMPoint(-2, 0),
            KMPoint(2, 0),
            KMPoint(0, -2),
            KMPoint(-1, 2)];

  for I := 0 to count - 1 do
  begin
    tree := trees[I];
    //first check if we can build around parent tree
    if CheckTree(tree) then
      Exit(true);
    for K := 0 to tree.ChildCount - 1 do
      if CheckTree(tree.ChildTree(K)) then
        Exit(true);
    //then check if we can build around child trees
  end;

end;

function TKMCityPlanner.PlaceForWell(out aLoc: TKMPoint): Boolean;
const SEARCH_RAD = 8;
var
  I, X, Y: Integer;
  SeedLocs: TKMPointArray;
  P : TKMPoint;
  doDelete : Boolean;
  bid, BestBid : Single;
  Locs : TKMPointTagList;
begin
  Result := False;

  SeedLocs := GetSeeds([htSwine, htBakery, htHovel, htStables, htAppleTree]);
  
  for I := High(SeedLocs) downto 0 do
  begin
    P := SeedLocs[I];
    doDelete := false;
    for X := Max(P.X - SEARCH_RAD, 1) to Min(P.X + SEARCH_RAD, gTerrain.MapX - 1) do
    for Y := Max(P.X - SEARCH_RAD, 1) to Min(P.X + SEARCH_RAD, gTerrain.MapX - 1) do
      If gTerrain.House(X, Y).IsValid(htWell) then 
      begin
        doDelete := true;
        Break;
        Break;
      end;
    If doDelete then
      SeedLocs.Delete(I);
  end;
      
  Locs := TKMPointTagList.Create;
  try
    FindNearest(SeedLocs, 10, htWell, 12, Locs);

    BestBid := MaxSingle;
    for I := 0 to Locs.Count - 1 do
    begin
      Bid := Locs.Tag[I]
             - gAIFields.Influences.Ownership[fOwner,Locs[I].Y,Locs[I].X] / 5;
      if (Bid < BestBid) then
      begin
        aLoc := Locs[I];
        BestBid := Bid;
        Result := True;
      end;
    end;
  finally
    Locs.Free;
  end;
  
end;

function TKMCityPlanner.FindNearest(const aStart: TKMPoint; aRadius: Byte; aType: TFindNearest; out aResultLoc: TKMPoint): Boolean;
begin
  fFinder.FindType := aType;
  fFinder.HouseType := htNone;
  Result := fFinder.FindNearest(aStart, aRadius, [tpWalkRoad, tpMakeRoads], aResultLoc);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPointArray; aRadius: Byte; aType: TFindNearest; aPass: TKMTerrainPassabilitySet; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := aType;
  fFinder.HouseType := htNone;
  fFinder.FindNearest(aStart, aRadius, aPass, aMaxCount, aLocs);
end;


procedure TKMCityPlanner.FindNearest(const aStart: TKMPointArray; aRadius: Byte; aHouse: TKMHouseType; aMaxCount: Word; aLocs: TKMPointTagList);
begin
  fFinder.FindType := fnHouse;
  fFinder.HouseType := aHouse;
  fFinder.FindNearest(aStart, aRadius, [tpWalkRoad, tpMakeRoads], aMaxCount, aLocs);
end;


function TKMCityPlanner.NextToOre(aHouse: TKMHouseType; aOreType: TKMWareType; out aLoc: TKMPoint; aNearAnyHouse: Boolean = False): Boolean;
var
  P: TKMPoint;
  SeedLocs: TKMPointArray;
begin
  Result := False;


  //Look for nearest Ore
  case aOreType of
    wtCoal:    begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(htCoalMine) > 0 then
                      SeedLocs := GetSeeds([htCoalMine])
                    else
                      SeedLocs := GetSeeds([htStore]);
                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre')], 45, fnCoal, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
    wtIronOre: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(htIronMine) > 0 then
                      SeedLocs := GetSeeds([htIronMine, htCoalMine])
                    else
                      SeedLocs := GetSeeds([htCoalMine, htStore]);
                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre_2')], 45, fnIron, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
    wtGoldOre: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(htGoldMine) > 0 then
                      SeedLocs := GetSeeds([htGoldMine, htCoalMine])
                    else
                      SeedLocs := GetSeeds([htCoalMine, htStore]);

                  if Length(SeedLocs) = 0 then Exit;
                  if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre_3')], 45, fnGold, P) then
                    if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                      Exit;
                end;
    wtBitinOre: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(htBitinMine) > 0 then
                      SeedLocs := GetSeeds([htGoldMine, htIronMine, htBitinMine])
                    else
                      SeedLocs := GetSeeds([htCoalMine, htStore]);

                  if Length(SeedLocs) = 0 then Exit;
                    if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre_4')], 45, fnBitin, P) then
                      if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                        Exit;
                end;

    wtTile: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                    if gHands[fOwner].Stats.GetHouseTotal(htPottery) > 0 then
                      SeedLocs := GetSeeds([htPottery])
                    else
                      SeedLocs := GetSeeds([htStore]);

                  if Length(SeedLocs) = 0 then Exit;
                    if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre_5')], 45, fnClay, P) then
                      if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                        Exit;
                end;
    wtFish: begin
                  if aNearAnyHouse then
                    SeedLocs := GetSeeds([htAny])
                  else
                  if gHands[fOwner].Stats.GetHouseTotal(htFishermans) > 0 then
                    SeedLocs := GetSeeds([htFishermans])
                  else
                    SeedLocs := GetSeeds([htStore]);

                  if Length(SeedLocs) = 0 then Exit;
                    if not FindNearest(SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToOre_5')], 45, fnWater, P) then
                      if aNearAnyHouse or not NextToOre(aHouse, aOreType, P, True) then
                        Exit;
                end;
  end;

  //todo: If there's no ore AI should not keep calling this over and over again
  // Maybe AI can cache search results for such non-replenishing resources

  aLoc := P;
  Result := True;
end;


function TKMCityPlanner.NextToTrees(aHouse: TKMHouseType; aSeed: array of TKMHouseType; out aLoc: TKMPoint): Boolean;
const
  SEARCH_RES = 7;
  SEARCH_RAD = 20; //Search for forests within this radius
  SEARCH_DIV = (SEARCH_RAD * 2) div SEARCH_RES + 1;
  HUT_RAD = 6; //Search for the best place for a hut in this radius
var
  I, K: Integer;
  Bid, BestBid: Single;
  SeedLocs: TKMPointArray;
  seedLoc, TreeLoc: TKMPoint;
  Mx, My: SmallInt;
  MyForest: array [0..SEARCH_RES-1, 0..SEARCH_RES-1] of ShortInt;
begin
  Result := False;

  SeedLocs := GetSeeds(aSeed);
  if Length(SeedLocs) = 0 then Exit;

  // Pick one random seed loc from given
  seedLoc := SeedLocs[KaMRandom(Length(SeedLocs), 'TKMCityPlanner.NextToTrees')];

    //todo: Rework through FindNearest to avoid roundabouts
  //Fill in MyForest map
  FillChar(MyForest[0,0], SizeOf(MyForest), #0);
  for I := Max(seedLoc.Y - SEARCH_RAD, 1) to Min(seedLoc.Y + SEARCH_RAD, gTerrain.MapY - 1) do
  for K := Max(seedLoc.X - SEARCH_RAD, 1) to Min(seedLoc.X + SEARCH_RAD, gTerrain.MapX - 1) do
  if gTerrain.ObjectIsChopableTree(K, I) then
  begin
    Mx := (K - seedLoc.X + SEARCH_RAD) div SEARCH_DIV;
    My := (I - seedLoc.Y + SEARCH_RAD) div SEARCH_DIV;

    Inc(MyForest[My, Mx]);
  end;

  //Find cell with most trees
  BestBid := -MaxSingle;
  TreeLoc := seedLoc; //Init incase we cant find a spot at all
  for I := Low(MyForest) to High(MyForest) do
  for K := Low(MyForest[I]) to High(MyForest[I]) do
  begin
    Mx := Round(seedLoc.X - SEARCH_RAD + (K + 0.5) * SEARCH_DIV);
    My := Round(seedLoc.Y - SEARCH_RAD + (I + 0.5) * SEARCH_DIV);
    if InRange(Mx, 1, gTerrain.MapX - 1) and InRange(My, 1, gTerrain.MapY - 1)
    and (gAIFields.Influences.AvoidBuilding[My, Mx] = 0) then
    begin
      Bid := MyForest[I, K] + KaMRandom('TKMCityPlanner.NextToTrees_2') * 2; //Add some noise for varied results
      if Bid > BestBid then
      begin
        TreeLoc := KMPoint(Mx, My);
        BestBid := Bid;
      end;
    end;
  end;

  BestBid := MaxSingle;
  for I := Max(TreeLoc.Y - HUT_RAD, 1) to Min(TreeLoc.Y + HUT_RAD, gTerrain.MapY - 1) do
  for K := Max(TreeLoc.X - HUT_RAD, 1) to Min(TreeLoc.X + HUT_RAD, gTerrain.MapX - 1) do
    if gHands[fOwner].CanAddHousePlanAI(K, I, aHouse, True) then
    begin
      Bid := KMLength(KMPoint(K,I), seedLoc) + KaMRandom('TKMCityPlanner.NextToTrees_3') * 5;
      if (Bid < BestBid) then
      begin
        aLoc := KMPoint(K,I);
        BestBid := Bid;
        Result := True;
      end;
    end;
end;


procedure TKMCityPlanner.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
  fFinder.OwnerUpdate(fOwner);
end;


procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('AICityPlanner');
  SaveStream.Write(fOwner);
  fFinder.Save(SaveStream);
  fListGold.SaveToStream(SaveStream);
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('AICityPlanner');
  LoadStream.Read(fOwner);
  fFinder.Load(LoadStream);
  fListGold.LoadFromStream(LoadStream);
end;


{ TKMTerrainFinderCity }
constructor TKMTerrainFinderCity.Create(aOwner: TKMHandID);
begin
  inherited Create;

  fOwner := aOwner;
end;


procedure TKMTerrainFinderCity.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
end;


function TKMTerrainFinderCity.CanUse(const X, Y: Word): Boolean;
var
  I, K: Integer;
begin
  case FindType of
    fnHouse:  Result := gHands[fOwner].CanAddHousePlanAI(X, Y, HouseType, True);

    fnStone:  Result := (gTerrain.TileIsStone(X, Max(Y-1, 1)) > 1);

    fnCoal:   Result := (gTerrain.TileIsCoal(X, Y) > 1)
                         and gHands[fOwner].CanAddHousePlanAI(X, Y, htCoalMine, False);

    fnClay:   Result := (gTerrain.TileIsClay(X, Y) >= 1);

    fnIron:   begin
                Result := gHands[fOwner].CanAddHousePlanAI(X, Y, htIronMine, False);
                //If we can build a mine here then search for ore
                if Result then
                  for I:=Max(X-4, 1) to Min(X+3, gTerrain.MapX) do
                    for K:=Max(Y-8, 1) to Y do
                      if gTerrain.TileHasIron(I, K) then
                        Exit;
                Result := False; //Didn't find any ore
              end;

    fnGold:   begin
                Result := gHands[fOwner].CanAddHousePlanAI(X, Y, htGoldMine, False);
                //If we can build a mine here then search for ore
                if Result then
                  for I:=Max(X-4, 1) to Min(X+4, gTerrain.MapX) do
                    for K:=Max(Y-8, 1) to Y do
                      if gTerrain.TileHasGold(I, K) then
                        Exit;
                Result := False; //Didn't find any ore
              end;

    fnBitin:   begin
                Result := gHands[fOwner].CanAddHousePlanAI(X, Y, htBitinMine, False);
                //If we can build a mine here then search for ore
                if Result then
                  for I:=Max(X-4, 1) to Min(X+4, gTerrain.MapX) do
                    for K:=Max(Y-8, 1) to Y do
                      if gTerrain.TileHasBitinIron(I, K) then
                        Exit;
                Result := False; //Didn't find any ore
              end;
    fnWater:  begin
                Result := gTerrain.TileHasWater(X, Y);
              end;

    else      Result := False;
  end;
end;


function TKMTerrainFinderCity.CanWalkHere(const X,Y: Word): Boolean;
var
  TerOwner: TKMHandID;
begin
  //Check for specific passabilities
  case FindType of
    fnIron:   Result := (fPassability * gTerrain.Land^[Y,X].Passability <> [])
                        or gTerrain.CanPlaceIronMine(X, Y);

    fnGold:   Result := (fPassability * gTerrain.Land^[Y,X].Passability <> [])
                        or gTerrain.TileGoodForGoldmine(X, Y);

    fnBitin:  Result := (fPassability * gTerrain.Land^[Y,X].Passability <> [])
                        or (gTerrain.TileGoodForGoldmine(X, Y) or gTerrain.CanPlaceIronMine(X, Y));

    fnWater:  Result := (fPassability * gTerrain.Land^[Y,X].Passability <> [])
                        or gTerrain.TileHasWater(X, Y);

    else      Result := (fPassability * gTerrain.Land^[Y,X].Passability <> []);
  end;

  if not Result then Exit;

  //Don't build on allies and/or enemies territory
  TerOwner := gAIFields.Influences.GetBestOwner(X,Y);
  Result := ((TerOwner = fOwner) or (TerOwner = HAND_NONE));
end;


procedure TKMTerrainFinderCity.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TerrainFinderCity');
  SaveStream.Write(fOwner);
end;


procedure TKMTerrainFinderCity.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TerrainFinderCity');
  LoadStream.Read(fOwner);
end;


end.
