unit KM_Terrain;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_CommonUtils, KM_ResTileset,
  KM_TerrainTypes,
  KM_ResHouses, KM_TerrainFinder, KM_ResMapElements,
  KM_CommonTypes,
  KM_ResTypes, KM_ResTilesetTypes,
  KM_Houses, KM_HouseCollection, KM_Units;


type
  // Class to store all terrain data, aswell terrain routines
  TKMTerrain = class
  private
    fLoading : Boolean;
    fTerrainMasks : array of TKMTerrainKind;
    fLand: TKMLand; // Actual Land
    fLandExt: TKMLandExt; // Precalculated Land data to speedup the render, updated on Land update. Does not saved
    fMainLand: PKMLand; // pointer to the actual fLand with all terrain data

    fAnimStep: Cardinal;
    fNightFactor : Cardinal;
    fMapEditor: Boolean; //In MapEd mode some features behave differently
    fMapX: Word; //Terrain width
    fMapY: Word; //Terrain height
    fMapRect: TKMRect; //Terrain rect (1, 1, MapX, MapY)

    fTileset: TKMResTileset;
    fFinder: TKMTerrainFinder;
    fBoundsWC: TKMRect; //WC rebuild bounds used in FlattenTerrain (put outside to fight with recursion SO error in FlattenTerrain EnsureWalkable)

    fTopHill: Integer;
    fOnTopHillChanged: TSingleEvent;
    fHousesPointersTemp,
    fUnitPointersTemp: array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of Pointer;
    fHasDarkPoints : Boolean;
    function TileHasParameter(X, Y: Word; aCheckTileFunc: TBooleanWordFunc; aAllow2CornerTiles: Boolean = False;
                              aStrictCheck: Boolean = False): Boolean;

    function GetMiningRect(aWare: TKMWareType): TKMRect;

    function ChooseCuttingDirection(const aLoc, aTree: TKMPoint; out aCuttingPoint: TKMPointDir): Boolean;
    procedure DoFlattenTerrain(const aLoc: TKMPoint; var aDepth: Byte; aUpdateWalkConnects: Boolean; aIgnoreCanElevate: Boolean; aFactor : Single = 0.5);

    procedure UpdateWalkConnect(const aSet: TKMWalkConnectSet; aRect: TKMRect; aDiagObjectsEffected: Boolean);

    procedure SetField_Init(const aLoc: TKMPoint; aOwner: TKMHandID; aRemoveOverlay: Boolean = True);
    procedure SetField_Complete(const aLoc: TKMPoint; aFieldType: TKMFieldType);

    function TrySetTile(X, Y: Integer; aType, aRot: Integer; aUpdatePassability: Boolean = True): Boolean; overload;
    function TrySetTile(X, Y: Integer; aType, aRot: Integer; out aPassRect: TKMRect;
                        out aDiagonalChanged: Boolean; aUpdatePassability: Boolean = True): Boolean; overload;
    function TrySetTileHeight(X, Y: Integer; aHeight: Byte; aUpdatePassability: Boolean = True): Boolean;
    function TrySetTileObject(X, Y: Integer; aObject: Word; aUpdatePassability: Boolean = True): Boolean; overload;
    function TrySetTileObject(X, Y: Integer; aObject: Word; out aDiagonalChanged: Boolean; aUpdatePassability: Boolean = True): Boolean; overload;

    function HousesNearTile(X,Y: Word): Boolean;

    procedure SetTopHill(aValue: Integer);
    procedure UpdateTopHill; overload;
    procedure UpdateTopHill(X, Y: Integer); overload;

    procedure Init;

    function GetNightFactor : Single;
    procedure SetNightFactor(aValue : Single);
  public
    Land: PKMLand;
    LandExt: PKMLandExt;

    Fences: TKMLandFences;
    Lights,
    FallingTrees: TKMPointTagList;

    constructor Create;
    destructor Destroy; override;

    procedure MakeNewMap(aWidth, aHeight: Integer; aMapEditor: Boolean);
    procedure LoadFromFile(const aFileName: UnicodeString; aMapEditor: Boolean);
    procedure AfterLoadFromFile;
    procedure SaveToFile(const aFile: UnicodeString); overload;
    procedure SaveToFile(const aFile: UnicodeString; const aInsetRect: TKMRect); overload;

    property MainLand: PKMLand read fMainLand; // readonly

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property MapRect: TKMRect read fMapRect;

    procedure SetMainLand;

    procedure SetTileLock(const aLoc: TKMPoint; aTileLock: TKMTileLock);
    procedure UnlockTile(const aLoc: TKMPoint);
    procedure SetRoads(aList: TKMPointTagList; aOwner: TKMHandID; aUpdateWalkConnects: Boolean = True);
    procedure SetRoad(const aLoc: TKMPoint; aOwner: TKMHandID; aRoadType : TKMRoadType);
    procedure SetPalisade(const aLoc: TKMPoint; aOwner: TKMHandID);
    Procedure UpdatePalisadeAround(const aLoc : TKMPoint; aOwner: TKMHandID);

    procedure RemPalisade(const aLoc: TKMPoint);
    procedure SetInitWine(const aLoc: TKMPoint; aOwner: TKMHandID);
    function GetFieldType(const aLoc: TKMPoint): TKMFieldType;
//    procedure SetFieldNoUpdate(const Loc: TKMPoint; aOwner: TKMHandID; aFieldType: TKMFieldType; aStage: Byte = 0);
    procedure SetField(const aLoc: TKMPoint; aOwner: TKMHandID; aFieldType: TKMFieldType; aStage: Integer = 0; aGrainType : TKMGrainType = gftNone;
                       aRandomAge: Boolean = False; aKeepOldObject: Boolean = False; aRemoveOverlay: Boolean = True;
                       aDoUpdate: Boolean = True);
    procedure SetHouse(const aLoc: TKMPoint; aHouseType: TKMHouseType; aHouseStage: TKMHouseStage; aOwner: TKMHandID; const aFlattenTerrain: Boolean = False);overload;
    procedure SetHouse(aHouse : Pointer; aOwner: TKMHandID; aHouseStage : TKMHouseStage; const aFlattenTerrain: Boolean = False); overload;
    procedure SetHouseAreaOwner(const aLoc: TKMPoint; aHouseType: TKMHouseType; aOwner: TKMHandID);

    procedure RemovePlayer(aPlayer: TKMHandID);
    procedure RemRoad(const aLoc: TKMPoint);
    procedure RemField(const aLoc: TKMPoint); overload;
    procedure RemField(const aLoc: TKMPoint; aDoUpdatePass, aDoUpdateWalk, aDoUpdateFences: Boolean); overload;
    procedure RemField(const aLoc: TKMPoint; aDoUpdatePass, aDoUpdateWalk: Boolean; out aUpdatePassRect: TKMRect;
                       out aDiagObjectChanged: Boolean; aDoUpdateFences: Boolean); overload;
    procedure SetDefTile(const aLoc: TKMPoint);
    procedure SetNewDefTile(const aLoc: TKMPoint);
    procedure ClearPlayerLand(aPlayer: TKMHandID);

    procedure RemoveLayers;

    procedure IncDigState(const aLoc: TKMPoint; aRoadType : TKMRoadType = rtNone);
    procedure ResetDigState(const aLoc: TKMPoint);

    function CanPlaceUnit(const aLoc: TKMPoint; aUnitType: TKMUnitType): Boolean;
    function CanPlaceGoldMine(X, Y: Word): Boolean;
    function CanPlaceIronMine(X, Y: Word): Boolean;
    function CanPlaceShipYard(X, Y: Word): Boolean;
    function CanPlaceHouse(aLoc: TKMPoint; aHouseType: TKMHouseType): Boolean;
    function CanPlaceHouseFromScript(aHouseType: TKMHouseType; const aLoc: TKMPoint): Boolean;
    function CanPlaceStructure(const aLoc : TKMPoint; aIndex, aRot : Word) : Boolean;
    procedure PlaceStructure(const aLoc : TKMPoint; aIndex, aRot : Word);
    procedure PlaceStructureTile(const aLoc : TKMPoint; aIndex, aRot, aTileIndex : Word);
    procedure SetStructurePlan(const aLoc: TKMPoint; aIndex, aRot: Word; aBuildType : TKMHouseBuildState);
    procedure PlaceDecoration(const aLoc : TKMPoint; aIndex : Word);
    function CheckHouseBounds(aHouseType: TKMHouseType; const aLoc: TKMPoint; aInsetRect: TKMRect): Boolean;
    function CanAddField(aX, aY: Word; aFieldType: TKMFieldType; aOwner : ShortInt = 0): Boolean;
    function CheckHeightPass(const aLoc: TKMPoint; aPass: TKMHeightPass): Boolean;
    procedure AddHouseRemainder(const aLoc: TKMPoint; aHouseType: TKMHouseType; aBuildState: TKMHouseBuildState; Resources : array of TKMWareType);

    function CanPlaceWall(const aLoc : TKMPoint; aHouseType : TKMHouseType) : Boolean;
    function CanPlaceWell(const aLoc : TKMPoint) : Boolean;

    procedure FindWineFieldLocs(const aLoc: TKMPoint; aRadius: Integer; aCornLocs: TKMPointList);
    function FindWineField(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aProdThatch : Pointer; out aFieldPoint: TKMPointDir): Boolean;
    procedure FindCornFieldLocs(const aLoc: TKMPoint; aRadius: Integer; aCornLocs: TKMPointList);
    function FindCornField(const aLoc: TKMPoint; aRadius:integer; const aAvoidLoc: TKMPoint; aPlantAct: TKMPlantAct; aProdThatch : Pointer;
                           out aPlantActOut: TKMPlantAct; out aFieldPoint: TKMPointDir; aGrainType : TKMGrainFarmSet): Boolean;

    function FindStone(const aLoc: TKMPoint; aRadius: Byte; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                       out aStonePoint: TKMPointDir): Boolean;


    procedure FindStoneLocs(const aLoc: TKMPoint; aRadius: Byte; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                            aStoneLocs: TKMPointList);

    function FindClay(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                       out aOrePoint: TKMPoint): Boolean;

    procedure FindClayPoints(const aLoc: TKMPoint;const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                            aClayLocs: TKMPointListArray);

    function FindCollectors(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                       out aOrePoint: TKMPointDir): TKMWarePlan;

    procedure FindCollectorsPoints(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                            aJewerlyLocs: TKMPointDirCenteredList);
    function DecCollectorsOre(aLoc: TKMPointDir; aCount : Byte) : Boolean;

    function FindHunter(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; out aOrePoint: TKMPointDir) : Boolean;
    procedure FindHunterPoints(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aPoints : TKMWeightedPointList);
    function SetHunterTraps(const aLoc: TKMPoint) : Word;
    function HasMeat(P : TKMPoint) : Boolean;
    function CanSetTrap(P : TKMPoint) : Boolean;

    function FindWareForBoat(const aLoc: TKMPoint; const aRadius : Byte; aFindWares, aFindFish : Boolean): TKMWarePlanSingle;
    function FindVWareForBoat(const aLoc: TKMPoint; const aRadius : Byte): Word;
    function DecWareOnGorund(const aLoc: TKMPoint; aCount : Byte = 1) : Boolean;

    function FindOre(const aLoc: TKMPoint; aWare: TKMWareType; out aOrePoint: TKMPoint; isOnMineShaft : Boolean = false): Boolean;
    procedure FindOrePoints(const aLoc: TKMPoint; aWare: TKMWareType; var aPoints: TKMPointListArray; isOnMineShaft : Boolean = false);
    procedure FindOrePointsByDistance(const aLoc: TKMPoint; aWare: TKMWareType; var aPoints: TKMPointListArray);
    function CanFindTree(const aLoc: TKMPoint; aRadius: Word; aOnlyAgeFull: Boolean = False):Boolean;
    procedure FindTree(const aLoc: TKMPoint; aRadius: Word; const aAvoidLoc: TKMPoint; aPlantAct: TKMPlantAct;
                       aTrees: TKMPointDirCenteredList; aBestToPlant,aSecondBestToPlant: TKMPointCenteredList);
    procedure FindPossibleTreePoints(const aLoc: TKMPoint; aRadius: Word; aTiles: TKMPointList);
    procedure FindFishWaterLocs(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                                aChosenTiles: TKMPointDirList);
    function FindFishWater(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits:
                           Boolean; out aFishPoint: TKMPointDir): Boolean;
    function FindBestClimatType(const aLoc: TKMPoint): TKMTerrainClimat;
    function FindBestTreeClimatType(const aLoc: TKMPoint): TKMTerrainClimat;
    function CanFindFishingWater(const aLoc: TKMPoint; aRadius: Integer): Boolean;

    function CanReachTileInDistance(const aLoc1, aLoc2: TKMPoint; aPassability : TKMTerrainPassability; aMaxDistance : Integer = 10) : Boolean;
    function FogOfWarTileList(const aLoc1: TKMPoint; out aPointList: TKMPointList; aFogOfWar : TBoolean2Array; aMaxDistance: Integer = 10): Boolean;

    function ChooseTreeToPlant(const aLoc: TKMPoint): Integer;
    function ChooseTreeToPlace(const aLoc: TKMPoint; aTreeAge: TKMChopableAge; aAlwaysPlaceTree: Boolean): Integer;

    procedure GetHouseMarks(const aLoc: TKMPoint; aHouseType: TKMHouseType; aList: TKMPointTagList; aIgnoreObjects : Boolean = false);
    procedure GetStructureMarks(const aLoc: TKMPoint; aIndex, aRot: Word; aList: TKMPointTagList);

    function WaterHasFish(const aLoc: TKMPoint): Boolean;
    function CatchFish(aLoc: TKMPointDir; aTestOnly: Boolean = False): Boolean;overload;
    function CatchFish(aLoc: TKMPointDir; aMaxCount : Integer; aTestOnly: Boolean = False): Word; overload;

    procedure SetObject(const aLoc: TKMPoint; aID: Integer);
    function GetObject(const aLoc: TKMPoint) : Word;
    procedure SetOverlay(const aLoc: TKMPoint; aOverlay: TKMTileOverlay; aOverwrite: Boolean);


    function FallTree(const aLoc: TKMPoint): Boolean;
    procedure ChopTree(const aLoc: TKMPoint);
    procedure RemoveObject(const aLoc: TKMPoint);
    procedure RemoveObjectsKilledByRoad(const aLoc: TKMPoint);
    procedure SetWareOnGround(aLoc : TKMPoint; aWare : TKMWareType; aCount : Integer);
    function GetWareOnGround(aLoc : TKMPoint) : TKMWareType;
    function GetWareOnGroundCount(aLoc : TKMPoint) : Byte;

    procedure SowCorn(const aLoc: TKMPoint; aGrainType : TKMGrainFarmSet; aAddManure : Boolean);
    function CutCorn(const aLoc: TKMPoint): Boolean;
    function CutGrapes(const aLoc: TKMPoint): Boolean;

    function DecStoneDeposit(const aLoc: TKMPoint): Boolean;
    function DecOreDeposit(const aLoc: TKMPoint; aWare: TKMWareType): Boolean;

    function GetPassablePointWithinSegment(aOriginPoint, aTargetPoint: TKMPoint; aPass: TKMTerrainPassability; aMaxDistance: Integer = -1): TKMPoint;
    function CheckPassability(X, Y: Integer; aPass: TKMTerrainPassability): Boolean; overload; inline;
    function CheckPassability(const aLoc: TKMPoint; aPass: TKMTerrainPassability): Boolean; overload; inline;
    function HasUnit(const aLoc: TKMPoint): Boolean;
    function HasVertexUnit(const aLoc: TKMPoint): Boolean;
    function GetRoadConnectID(const aLoc: TKMPoint): Byte;
    function GetWalkConnectID(const aLoc: TKMPoint): Byte;
    function GetConnectID(aWalkConnect: TKMWalkConnect; const Loc: TKMPoint): Byte;

    function CheckAnimalIsStuck(const aLoc: TKMPoint; aPass: TKMTerrainPassability; aCheckUnits: Boolean = True): Boolean;
    function GetOutOfTheWay(aUnit: Pointer; const aPusherLoc: TKMPoint; aPass: TKMTerrainPassability; aPusherWasPushed: Boolean = False): TKMPoint;
    function FindSideStepPosition(const aLoc, aLoc2, aLoc3: TKMPoint; aPass: TKMTerrainPassability; out aSidePoint: TKMPoint; aOnlyTakeBest: Boolean): Boolean;
    function RouteCanBeMade(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability): Boolean; overload; inline;
    function RouteCanBeMade(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean; overload;
    function RouteCanBeMadeToVertex(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability): Boolean;
    function GetClosestTile(const aTargetLoc, aOriginLoc: TKMPoint; aPass: TKMTerrainPassability; aAcceptTargetLoc: Boolean): TKMPoint;
    function GetClosestRoad(const aFromLoc: TKMPoint; aWalkConnectIDSet: TKMByteSet; aPass: TKMTerrainPassability = tpWalkRoad): TKMPoint;

    procedure UnitAdd(const aLocTo: TKMPoint; aUnit: Pointer);
    procedure UnitRem(const aLocFrom: TKMPoint);
    procedure UnitWalk(const aLocFrom, aLocTo: TKMPoint; aUnit: Pointer);
    procedure UnitSwap(const aLocFrom, aLocTo: TKMPoint; aUnitFrom: Pointer);
    procedure UnitVertexAdd(const aLocTo: TKMPoint; Usage: TKMVertexUsage); overload;
    procedure UnitVertexAdd(const aLocFrom, aLocTo: TKMPoint); overload;
    procedure UnitVertexRem(const aLocFrom: TKMPoint);
    function VertexUsageCompatible(const aLocFrom, aLocTo: TKMPoint): Boolean;
    function GetVertexUsageType(const aLocFrom, aLocTo: TKMPoint): TKMVertexUsage;

    function CoordsWithinMap(const X, Y: Single; const aInset: Byte = 0): Boolean; inline;
    function PointFInMapCoords(const aPointF: TKMPointF; const aInset: Byte = 0): Boolean; inline;
    function TileInMapCoords(const X, Y: Integer; const aInset: Byte): Boolean; overload; inline;
    function TileInMapCoords(const X, Y: Integer): Boolean; overload; inline;
    function TileInMapCoords(const aCell: TKMPoint; const Inset: Byte = 0): Boolean; overload; inline;
    function TileInMapCoords(const X,Y: Integer; const aInsetRect: TKMRect): Boolean; overload; inline;
    function VerticeInMapCoords(const X, Y: Integer; const aInset: Byte = 0): Boolean; overload; inline;
    function VerticeInMapCoords(const aCell: TKMPoint; const aInset: Byte = 0): Boolean; overload; inline;
    procedure EnsureCoordsWithinMap(var X, Y: Single; const aInset: Byte = 0); inline;
    function EnsureTilesRectWithinMap(const aRectF: TKMRectF; const aInset: Single = 0): TKMRectF; inline;
    function EnsureVerticesRectWithinMap(const aRectF: TKMRectF; const aInset: Single = 0): TKMRectF; inline;
    function EnsureTileInMapCoords(const X, Y: Integer; const aInset: Byte = 0): TKMPoint; overload; inline;
    function EnsureTileInMapCoords(const aLoc: TKMPoint; const aInset: Byte = 0): TKMPoint; overload; inline;

    function TileGoodForIronMine(X, Y: Word): Boolean;
    function TileGoodForGoldmine(X, Y: Word): Boolean;

    function TileGoodForField(X, Y: Word): Boolean;
    function TileGoodForCornField(X, Y: Word): Boolean;
    function TileGoodForWineField(X, Y: Word): Boolean;
    function TileGoodForGrassField(X, Y: Word): Boolean;

    function TileGoodToPlantTree(X, Y: Word): Boolean;
    function TileIsWater(const aLoc: TKMPoint): Boolean; overload; inline;
    function TileIsWater(X, Y: Word): Boolean; overload; inline;
    function TileIsStone(X, Y: Word): Byte; inline;
    function TileIsSnow(X, Y: Word): Boolean; inline;
    function TileIsCoal(X, Y: Word): Byte; inline;
    function TileIsIron(X, Y: Word): Byte; inline;
    function TileIsBitinIron(X, Y: Word): Byte; inline;
    function TileIsClay(X, Y: Word): Byte; inline;
    function TileIsGold(X, Y: Word): Byte; inline;
    function TileIsCornField(const aLoc: TKMPoint): Boolean; overload; inline;
    function TileIsCornField(const X, Y: Word): Boolean; overload; inline;
    function TileIsGrassField(const aLoc : TKMPoint): Boolean; overload; inline;
    function TileIsGrassField(const X, Y: Word): Boolean; overload; inline;
    function TileIsVegeField(const aLoc : TKMPoint): Boolean; overload; inline;
    function TileIsVegeField(const X, Y: Word): Boolean; overload; inline;
    function TileIsMineShaft(aLoc : TKMPoint) : Boolean;
    procedure FindDeposits(const aLoc: TKMPoint; aRadius : Byte; aDeposits : TKMPointTagList; OverlapRadius : Byte = 0);overload;
    procedure FindDeposits(const aLoc: TKMPoint; aRadius : Byte; aDeposits : TKMPointTagList; aIncludeNormalOre : Boolean);overload;
    procedure PalaceExploreDeposits(const aLoc : TKMPoint);

    procedure IncFieldAge(aLoc : TKMPoint);
    procedure SetLand(aTile: Word; const X, Y, aObj: Word);

    function TileIsWineField(const aLoc: TKMPoint): Boolean; overload; inline;
    function TileIsWineField(const X, Y: Word): Boolean; overload; inline;
    function TileIsWalkableRoad(const aLoc: TKMPoint): Boolean;
    function TileIsLocked(const aLoc: TKMPoint): Boolean;
    function TileIsGoodToCutTree(const aLoc: TKMPoint): Boolean;
    function CanCutTreeAtVertex(const aWoodcutterPos, aTreeVertex: TKMPoint): Boolean;
    function TileHasCollectorsGoods(const X, Y : Integer) : Boolean;
    function GetTileCornObject(const aLoc: TKMPoint; aStage : Byte): Word;
    function GetTileWineObject(const aLoc: TKMPoint; aStage : Byte): Word;
    function GetTileCornTile(const aLoc: TKMPoint; aStage : Byte): Word;
    function GetTileWineType(const aLoc: TKMPoint): TKMGrainType;
    function GetTileGrassObject(const aLoc: TKMPoint; aStage : Byte): Word;
    function GetTileGrassTile(const aLoc: TKMPoint; aStage : Byte): Word;

    function TileHasStone(X, Y: Word): Boolean; inline;
    function TileHasCoal(X, Y: Word): Boolean; inline;
    function TileHasIron(X, Y: Word): Boolean; inline;
    function TileHasBitinIron(X, Y: Word): Boolean; inline;
    function TileHasClay(X, Y: Word): Boolean; inline;
    function TileHasGold(X, Y: Word): Boolean; inline;
    function TileHasWall(X, Y: Word): Boolean; inline;
    function TileHasPalisade(X, Y: Word): Boolean; inline;

    function TileHasTerrainKindPart(X, Y: Word; aTerKind: TKMTerrainKind): Boolean; overload;
    function TileHasTerrainKindPart(X, Y: Word; aTerKind: TKMTerrainKind; aDir: TKMDirection): Boolean; overload;
    function TileHasOnlyTerrainKinds(X, Y: Word; const aTerKinds: array of TKMTerrainKind): Boolean; overload;
    function TileHasTerrainKinds(X, Y: Word; const aTerKinds: TKMTerrainKindSet): Boolean;
    function TileHasOnlyTerrainKind(X, Y: Word; const aTerKind: TKMTerrainKind): Boolean;
    function TileHasOnlyTerrainKinds(aLoc : TKMPoint; const aTerKind: TKMTerrainKindSet): Boolean; overload;

    function TileTryGetTerKind(X, Y: Word; var aTerKind: TKMTerrainKind): Boolean;

    function TileIsSand(const aLoc: TKMPoint): Boolean; inline;
    function TileIsSoil(X,Y: Word): Boolean; overload; inline;
    function TileIsSoil(const aLoc: TKMPoint): Boolean; overload; inline;
    function TileIsIce(X, Y: Word): Boolean; inline;
    function TileHasWater(X, Y: Word): Boolean; overload; inline;
    function TileHasWater(P : TKMPoint): Boolean; overload; inline;
    function VerticeIsFactorable(const aLoc: TKMPoint): Boolean;
    function TileIsWalkable(const aLoc: TKMPoint): Boolean; inline;
    function TileIsRoadable(const Loc: TKMPoint): Boolean; inline;

    function TileCornerTerrain(aX, aY: Integer; aCorner: Byte): Word;
    function TileCornersTerrains(aX, aY: Integer): TKMWordArray;
    function TileCornerTerKind(aX, aY: Integer; aCorner: Byte): TKMTerrainKind;
    procedure GetTileCornersTerKinds(aX, aY: Integer; out aCornerTerKinds: TKMTerrainKindCorners);

    procedure GetVerticeTerKinds(const aLoc: TKMPoint; out aVerticeTerKinds: TKMTerrainKindCorners);
    function  TileHasTerKind(aX, aY : Word; aTerKind : TKMTerrainKind; aDefaultTile: Boolean = false; aMinCornersCount : Byte = 1) : Boolean;
    function  TileHasTerKinds(aX, aY : Word; aTerKind : TKMTerrainKindSet; aDefaultTile: Boolean = false; aMinCornersCount : Byte = 1) : Boolean;

    function TileHasRoad(const aLoc: TKMPoint): Boolean; overload; inline;
    function TileHasRoad(X,Y: Integer): Boolean; overload; inline;

    function UnitsHitTest(X, Y: Word): Pointer;
    function UnitsHitTestF(const aLoc: TKMPointF): Pointer;overload;
    procedure UnitsHitAllTestF(const aLoc: TKMPointF; const aRadius : Single; out aUnits : TPointerArray);
    procedure HousesHitAllTestF(const aLoc: TKMPointF; const aRadius : Single; out aHouses : TPointerArray);
    function IsPlayerUnitWithinRad(const aPlayer : Integer; const aLoc: TKMPoint; const aRadius : Byte) : Boolean;
    function UnitsHitTestWithinRad(const aLoc: TKMPoint; aMinRad, aMaxRad: Single; aPlayer: TKMHandID; aAlliance: TKMAllianceType;
                                   aDir: TKMDirection; const aClosest: Boolean; aTestDiagWalkable: Boolean = True): Pointer;
    function FindPlaceForUnit(aLoc : TKMPoint; aPass: TKMTerrainPassability; aMaxDistance : Byte) : TKMPoint;
    function FindWalkableSpot(aLoc : TKMPoint) : TKMPoint;
    function IsTileNearWater(aLoc : TKMPoint) : Boolean;
    function IsTileNearLand(aLoc : TKMPoint) : Boolean;

    function ScriptTrySetTile(X, Y, aType, aRot: Integer): Boolean;
    function ScriptTrySetTileHeight(X, Y, aHeight: Integer): Boolean;
    function ScriptTrySetTileObject(X, Y, aObject: Integer): Boolean;
    function ScriptTrySetTilesArray(var aTiles: array of TKMTerrainTileBrief; aRevertOnFail: Boolean; var aErrors: TKMTerrainTileChangeErrorArray): Boolean;

    function ObjectIsCorn(const aLoc: TKMPoint): Boolean; overload; inline;
    function ObjectIsCorn(X,Y: Word): Boolean; overload; inline;
    function ObjectIsCornOrGrass(const aLoc: TKMPoint): Boolean; overload; inline;
    function ObjectIsCornOrGrass(X,Y: Word): Boolean; overload; inline;
    function ObjectIsVege(X,Y: Word): Boolean; overload; inline;

    function ObjectIsWine(const aLoc: TKMPoint): Boolean; overload; inline;
    function ObjectIsWine(X,Y: Word): Boolean; overload; inline;

    function ObjectIsChopableTree(X,Y: Word): Boolean; overload; inline;
    function ObjectIsChopableTree(const aLoc: TKMPoint; aStage: TKMChopableAge): Boolean; overload; inline;
    function ObjectIsChopableTree(const aLoc: TKMPoint; aStages: TKMChopableAgeSet): Boolean; overload; inline;
    function CanWalkDiagonally(const aFrom: TKMPoint; aX, aY: SmallInt): Boolean;

    function GetFieldStage(const aLoc: TKMPoint): Byte;
    function GetCornStage(const aLoc: TKMPoint): Byte;
    function GetGrassStage(const aLoc: TKMPoint): Byte;
    function GetVegeStage(const aLoc: TKMPoint): Byte;
    function GetWineStage(const aLoc: TKMPoint): Byte;

    property TopHill: Integer read fTopHill;
    property OnTopHillChanged: TSingleEvent read fOnTopHillChanged write fOnTopHillChanged;

    procedure FlattenTerrain(const Loc: TKMPoint; aUpdateWalkConnects: Boolean = True; aIgnoreCanElevate: Boolean = False; aFactor : Single = 0.5); overload;
    procedure FlattenTerrain(LocList: TKMPointList); overload;

    function ConvertCursorToMapCoord(inX, inY:single): Single;
    function FlatToHeight(inX, inY: Single): Single; overload;
    function FlatToHeight(const aPoint: TKMPointF): TKMPointF; overload;
    function RenderFlatToHeight(inX, inY: Single): Single; overload;
    function RenderFlatToHeight(const aPoint: TKMPointF): TKMPointF; overload;
    function HeightAt(inX, inY: Single): Single;
    function RenderHeightAt(inX, inY: Single): Single;

    Procedure AddTerrainMask(aTerKind : TKMTerrainKind);
    Procedure ResetTerrainMask;
    function MaskContains(aTerKind : TKMTerrainKind) : Boolean;
    procedure SelectTile(X, Y : Integer; aSelected : Boolean);
    procedure ReserveForAI(aLoc : TKMPoint);
    function IsReservedForAI(aLoc : TKMPoint) : Boolean;

    function GetRoadType(aLoc : TKMPoint) : TKMRoadType; overload;
    function GetRoadType(X, Y : Integer) : TKMRoadType; overload;
    function GetGrainType(aLoc : TKMPoint) : TKMGrainType;

    function AvoidTile(aX, aY : Integer) : Boolean; overload;
    function AvoidTile(P : TKMPoint) : Boolean; overload;

    procedure UpdateRenderHeight; overload;
    procedure UpdateRenderHeight(const aRect: TKMRect); overload;
    procedure UpdateRenderHeight(X, Y: Integer; aUpdateTopHill: Boolean = True); overload;

    procedure UpdateLighting; overload;
    procedure UpdateLighting(const aRect: TKMRect); overload;
    procedure UpdateLighting(X, Y: Integer); overload;

    procedure UpdatePassability; overload;
    procedure UpdatePassability(const aRect: TKMRect); overload;
    procedure UpdatePassability(const aLoc: TKMPoint); overload;


    procedure UpdateFences(aCheckSurrounding: Boolean = True); overload;
    procedure UpdateFences(const aRect: TKMRect; aCheckSurrounding: Boolean = True); overload;
    procedure UpdateFences(const aLoc: TKMPoint; aCheckSurrounding: Boolean = True); overload;

    procedure UpdateAll; overload;
    procedure UpdateAll(const aRect: TKMRect); overload;

    procedure CallOnMainLand(aProc: TEvent);
    function House(aLoc : TKMPoint) : TKMHouse;overload;
    function House(aX, aY : Integer) : TKMHouse;overload;
    function GetUnit(aLoc : TKMPoint) : TKMUnit;overload;
    function GetUnit(aX, aY : Integer) : TKMUnit;overload;

    procedure UpdateNightAffection;
    procedure UpdateLight(aLoc : TKMPoint; aRadius, aPower : Byte);
    procedure AddLight(aLoc : TKMPoint; aRadius, aPower : Byte);
    procedure RemLight(aLoc : TKMPoint);
    function GetNightAtTile(aX, aY : Integer) : Single;overload;
    function GetNightAtTile(aLoc : TKMPoint) : Single;overload;
    property NightFactor : Single read GetNightFactor write SetNightFactor;
    function LocalNightSpeed : Integer;

    procedure IncAnimStep; //Lite-weight UpdateState for MapEd
    property AnimStep: Cardinal read fAnimStep;
    property HasDarkSpots : Boolean read fHasDarkPoints;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure SaveSeperatedLayer(aID : Byte);//Used for Map Editing
    function  LoadSeperatedLayer(aFileName : String) : Boolean;
    procedure UpdateState;
  end;

var
  //Terrain is a globally accessible resource by so many objects
  //In rare cases local terrain is used (e.g. main menu minimap)
  gTerrain: TKMTerrain;
const HOUSE_BLOCK_RADIUS = 1;

implementation
uses
  KM_Entity,
  KM_Log,
  KM_CommonHelpers,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_TerrainUtils, KM_TerrainWalkConnect,
  KM_Resource{, KM_Units}, KM_DevPerfLog,
  KM_ResSound, KM_Sound, KM_UnitActionStay, KM_UnitActionGoInOut, KM_UnitWarrior, KM_TerrainPainter,{ KM_Houses,}
  KM_ResUnits, KM_ResSprites, KM_ResWares, KM_ResStructures,
  KM_Game, KM_GameParams, KM_GameTypes, KM_GameSettings, KM_Cursor,
  KM_ScriptingEvents, KM_Utils, KM_DevPerfLogTypes,
  KM_Maps,
  KM_CommonExceptions;


{ TKMTerrain }
constructor TKMTerrain.Create;
begin
  inherited;
  fAnimStep := 0;
  //TERRAIN_DARK := MIN_NIGHT_DARKNESS + (Sin(fNightFactor / NIGHT_SPEED) + 1) * 0.6 * (1 - MIN_NIGHT_DARKNESS);
  FallingTrees := TKMPointTagList.Create;
  Lights := TKMPointTagList.Create;
  fTileset := gRes.Tileset; //Local shortcut

  fMainLand := @fLand;
  LandExt := @fLandExt;
  SetMainLand;
end;


destructor TKMTerrain.Destroy;
begin
  FreeAndNil(FallingTrees);
  FreeAndNil(Lights);
  FreeAndNil(fFinder);

  inherited;
end;


procedure TKMTerrain.SetMainLand;
begin
  if Self = nil then Exit;

  Land := @fLand;
end;


//Reset whole map with default values
procedure TKMTerrain.MakeNewMap(aWidth, aHeight: Integer; aMapEditor: Boolean);
var
  I, K: Integer;
begin
  fMapEditor := aMapEditor;
  fMapX := Min(aWidth,  MAX_MAP_SIZE);
  fMapY := Min(aHeight, MAX_MAP_SIZE);
  fMapRect := KMRect(1, 1, fMapX, fMapY);

  for I := 1 to fMapY do
    for K := 1 to fMapX do
    begin
      with Land^[I, K] do
      begin
        //Apply some random tiles for artisticity
        if KaMRandom(5, 'TKMTerrain.MakeNewMap') = 0 then
          BaseLayer.Terrain := RandomTiling[tkGrass, KaMRandom(RandomTiling[tkGrass, 0], 'TKMTerrain.MakeNewMap 2') + 1]
        else
          BaseLayer.Terrain := 0;
        LayersCnt    := 0;
        BaseLayer.SetAllCorners;
        Height       := HEIGHT_DEFAULT + KaMRandom(HEIGHT_RAND_VALUE, 'TKMTerrain.MakeNewMap 3');  //variation in Height
        LandExt[I, K].RenderHeight := GetRenderHeight;
        BaseLayer.Rotation     := KaMRandom(4, 'TKMTerrain.MakeNewMap 4');  //Make it random
        Obj          := OBJ_NONE;             //none
        IsCustom     := False;
        BlendingLvl  := TERRAIN_DEF_BLENDING_LVL;
        //Uncomment to enable random trees, but we don't want that for the map editor by default
        //if KaMRandom(16)=0 then Obj := ChopableTrees[KaMRandom(13)+1,4];
        TileOverlay  := toNone;
        TileOverlay2  := toNone;
        IsHidden     := false;
        TileLock     := tlNone;
        JamMeter     := 0;
        Passability  := []; //Gets recalculated later
        TileOwner    := -1;
        IsUnit       := nil;
        IsHouse      := nil;
        IsVertexUnit := vuNone;
        FieldAge     := 0;
        TreeAge      := 0;
        NightAffection := 1;
      end;
      Fences[I, K].Kind := fncNone;
      Fences[I, K].Side := 0;
    end;
  {
  for I := 1 to fMapY do
  begin

//    if (I mod 4) < 3 then
//      SetRoad(KMPoint( I, 1), 0);
//
//    if ((I + 2) mod 4) < 3 then
//      SetRoad(KMPoint(I, fMapX - 1), 0);

    for K := 1 to fMapX do
    begin
      if ((I mod 2) = 0) and (K <= I) then
        SetRoad(KMPoint(K, I), 0);

      if I = 1 then
        SetRoad(KMPoint(K, I), 0);

      if I = fMapY - 1 then
        SetRoad(KMPoint(K, I), 0);

      if (I = 3) and (((fMapX - 1 - K - 1) mod 4) > 0) then
        SetRoad(KMPoint(K, I), 0);

      ////----------------------------
      if ((K mod 2) = 0) and (K >= I) and (I >= 3) then
        SetRoad(KMPoint(K, I), 0);

//      if K = 1 then
//        SetRoad(KMPoint(K, I), 0);

      if K = fMapY - 1 then
        SetRoad(KMPoint(K, I), 0);

      if (K = 1) and (((fMapY - 1 - I - 1 + 2) mod 4) > 0) then
        SetRoad(KMPoint(K, I), 0);

    end;
  end;           }

  fFinder := TKMTerrainFinder.Create;
  UpdateLighting;
  UpdatePassability;

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWork], MapRect, True);
  Init;
end;


procedure TKMTerrain.LoadFromFile(const aFileName: UnicodeString; aMapEditor: Boolean);
var
  I, J, L: Integer;
  S: TKMemoryStream;
  newX, newY: Integer;
  gameRev: Integer;
  tileBasic: TKMTerrainTileBasic;
begin
  fMapX := 0;
  fMapY := 0;
  if not FileExists(aFileName) then Exit;

  fMapEditor := aMapEditor;

  gLog.AddTime('Loading map file: ' + aFileName);

  fHasDarkPoints := false;

  S := TKMemoryStreamBinary.Create;
  try
    S.LoadFromFile(aFileName);

    LoadMapHeader(S, newX, newY, gameRev);

    fMapX := newX;
    fMapY := newY;

    fMapRect := KMRect(1, 1, fMapX, fMapY);

    for I := 1 to fMapY do
      for J := 1 to fMapX do
      begin
        Land^[I,J].TileLock     := tlNone;
        Land^[I,J].JamMeter     := 0;
        Land^[I,J].Passability  := []; //Gets recalculated later
        Land^[I,J].TileOwner    := HAND_NONE;
        Land^[I,J].IsUnit       := nil;
        Land^[I,J].IsHouse       := nil;
        Land^[I,J].IsVertexUnit := vuNone;
        Land^[I,J].FieldAge     := 0;
        Land^[I,J].TreeAge      := 0;
        Fences[I,J].Kind   := fncNone;
        Fences[I,J].Side   := 0;
        ReadTileFromStream(S, tileBasic, gameRev);
        Land^[I,J].BaseLayer   := tileBasic.BaseLayer;
        Land^[I,J].DefTile := tileBasic.BaseLayer.Terrain;
        Land^[I,J].DefRotation := tileBasic.BaseLayer.Rotation;

        Land^[I,J].SetHeightExact(tileBasic.Height); // Set fHeight directly, without any limitations
        UpdateRenderHeight(J, I);
        Land^[I,J].Obj         := Min(tileBasic.Obj, OBJECTS_CNT);

        Land^[I,J].LayersCnt   := tileBasic.LayersCnt;
        Land^[I,J].IsCustom    := tileBasic.IsCustom;
        Land^[I,J].IsHidden    := tileBasic.IsHidden;
        Land^[I,J].RoadType    := tileBasic.RoadType;
        Land^[I,J].Ware    := tileBasic.Ware;



        Land^[I,J].IsAiReserved := false;
        Land^[I,J].NightAffection := 1;

        if Land^[I,J].IsHidden then
          fHasDarkPoints := true;

        Land^[I,J].BlendingLvl := tileBasic.BlendingLvl;
        Land^[I,J].TileOverlay := tileBasic.TileOverlay;

        if gameRev > 14786 then
          Land^[I,J].TileOverlay2 := tileBasic.TileOverlay2
        else
          Land^[I,J].TileOverlay2 := toNone;

        if Land^[I,J].TileOverlay2 in [toGold, toIron, toBitin] then
          Land^[I,J].Ware.C2 := 5//it has always 5 ores in it
        else
        if Land^[I,J].TileOverlay2 in [toCoal] then
          Land^[I,J].Ware.C2 := 10//coal has always 10 ores in it
        else
          Land^[I,J].Ware.C2 := 0;

        for L := 0 to tileBasic.LayersCnt - 1 do
          Land^[I,J].Layer[L] := tileBasic.Layer[L];

        if ObjectIsChopableTree(KMPoint(J,I), caAgeFull) and (gMapElements[Land^[I,J].Obj].NextTreeAgeObj = 0) then
          Land^[I,J].TreeAge := CORN_AGE_MAX
        else
        if Land^[I,J].Obj = 390 then
          Land^[I,J].TreeAge := 1
        else
        if ObjectIsChopableTree(KMPoint(J,I), caAge1) then
          if gMapElements[Land^[I,J].Obj].PrevTreeAgeObj > 0 then
            Land^[I,J].TreeAge := TREE_AGE_SAPLING + gMapElements[gMapElements[Land^[I,J].Obj].PrevTreeAgeObj].TreeGrowAge
          else
            Land^[I,J].TreeAge := 1 + TREE_AGE_SAPLING;


        {if ObjectIsChopableTree(KMPoint(J,I), caAge1) then Land^[I,J].TreeAge := 1;
        if ObjectIsChopableTree(KMPoint(J,I), caAge2) then Land^[I,J].TreeAge := TREE_AGE_1;
        if ObjectIsChopableTree(KMPoint(J,I), caAge3) then Land^[I,J].TreeAge := TREE_AGE_2;
        if ObjectIsChopableTree(KMPoint(J,I), caAgeFull) then Land^[I,J].TreeAge := TREE_AGE_FULL;}
        //Everything else is default
      end;
  finally
    S.Free;
  end;

  fFinder := TKMTerrainFinder.Create;
  UpdateLighting;
  UpdatePassability;

  //Everything except roads
  UpdateWalkConnect([wcWalk, wcFish, wcWork], MapRect, True);

  Init;
  //SaveSeperatedLayer(0);
  //SaveSeperatedLayer(1);
  //SaveSeperatedLayer(2);
  //SaveSeperatedLayer(3);
  gLog.AddTime('Map file loaded');
end;

procedure TKMTerrain.AfterLoadFromFile;
begin
  fNightFactor := Round((gGame.Weather.Settings.NightTime / 3) * LocalNightSpeed);
  UpdateNightAffection;
end;


procedure TKMTerrain.SaveToFile(const aFile: UnicodeString);
begin
  SaveToFile(aFile, KMRECT_ZERO);
end;

//Save (export) map in KaM .map format with additional tile information on the end?
procedure TKMTerrain.SaveToFile(const aFile: UnicodeString; const aInsetRect: TKMRect);
var
  mapDataSize: Cardinal;
const
  H_RND_HALF = HEIGHT_RAND_VALUE div 2;

  //aDir - direction of enlarge for new generated tile
  procedure SetNewLand(var S: TKMemoryStream; aToX, aToY, aFromX, aFromY: Word;
                       aNewGenTile: Boolean; aDir: TKMDirection = dirNA);
  var
    L, D, adj, hMid: Integer;
    TileBasic: TKMTerrainTileBasic;
    terKind: TKMTerrainKind;
    cornersTerKinds: TKMTerrainKindCorners;
    tileOwner: TKMHandID;
  begin
    tileOwner := HAND_NONE;
    // new appended terrain, generate tile then
    if aNewGenTile then
    begin
      //Check if terrainKind is same for all 4 corners
      if not TileTryGetTerKind(aFromX, aFromY, terKind) then
      begin
        GetTileCornersTerKinds(aFromX, aFromY, cornersTerKinds);

        if aDir = dirNA then // that should never happen usually
          terKind := tkGrass
        else
        begin
          D := Ord(aDir);
          if (D mod 2) = 0 then // corner direction
            terKind := CornersTerKinds[(D div 2) mod 4]
          else
          begin
            adj := Random(2); //Choose randomly between 2 corners terkinds
            terKind  := CornersTerKinds[((D div 2) + adj) mod 4];
          end;
        end;
      end;

      //Apply some random tiles for artisticity
      TileBasic.BaseLayer.Terrain  := gGame.TerrainPainter.PickRandomTile(terKind, True);
      TileBasic.BaseLayer.Rotation := Random(4);
      TileBasic.BaseLayer.SetAllCorners;
      //find height mid point to make random elevation even for close to 0 or 100 height
      hMid := Max(0, fLand[aFromY,aFromX].Height - H_RND_HALF) + H_RND_HALF;
      hMid := Min(100, hMid + H_RND_HALF) - H_RND_HALF;
      TileBasic.Height    := EnsureRange(hMid - H_RND_HALF + Random(HEIGHT_RAND_VALUE), 0, 100);
      TileBasic.Obj       := OBJ_NONE; // No object
      TileBasic.IsCustom  := False;
      TileBasic.BlendingLvl := TERRAIN_DEF_BLENDING_LVL;
      TileBasic.LayersCnt := 0;
      TileBasic.TileOverlay := toNone;
      TileBasic.TileOverlay2 := toNone;
    end
    else
    begin
      // Use fLand, to be sure we save actual Land
      TileBasic.BaseLayer     := fLand[aFromY,aFromX].BaseLayer;
      TileBasic.Height        := fLand[aFromY,aFromX].Height;
      TileBasic.Obj           := fLand[aFromY,aFromX].Obj;
      TileBasic.LayersCnt     := fLand[aFromY,aFromX].LayersCnt;
      TileBasic.IsCustom      := fLand[aFromY,aFromX].IsCustom;
      TileBasic.BlendingLvl   := fLand[aFromY,aFromX].BlendingLvl;
      TileBasic.TileOverlay   := fLand[aFromY,aFromX].TileOverlay;
      TileBasic.TileOverlay2  := fLand[aFromY,aFromX].TileOverlay2;
      TileBasic.IsHidden      := fLand[aFromY,aFromX].IsHidden;
      TileBasic.Ware          := fLand[aFromY,aFromX].Ware;
      for L := 0 to 2 do
        TileBasic.Layer[L] := fLand[aFromY,aFromX].Layer[L];

      tileOwner := fLand[aFromY,aFromX].TileOwner;
    end;
    WriteTileToStream(S, TileBasic, tileOwner, False, mapDataSize);
  end;

  procedure WriteFileHeader(S: TKMemoryStream);
  begin
    S.Write(Integer(0));     //Indicates this map has not standart KaM format, Can use 0, as we can't have maps with 0 width
    S.WriteW(UnicodeString(GAME_REVISION)); //Write KaM Remake revision, in case we will change format in future
    S.Write(mapDataSize);
  end;

var
  S: TKMemoryStream;
  //MapInnerRect: TKMRect;
  NewGenTileI, NewGenTileK, extLeft, extRight, extTop, extBot: Boolean;
  I, K, IFrom, KFrom, D: Integer;
  SizeX, SizeY: Integer;
begin
  Assert(fMapEditor, 'Can save terrain to file only in MapEd');
  ForceDirectories(ExtractFilePath(aFile));

  mapDataSize := 0;
  S := TKMemoryStreamBinary.Create;
  WriteFileHeader(S);
  try
    //Dimensions must be stored as 4 byte integers
    SizeX := fMapX + aInsetRect.Left + aInsetRect.Right;
    SizeY := fMapY + aInsetRect.Top + aInsetRect.Bottom;
    S.Write(SizeX);
    S.Write(SizeY);
    //MapInnerRect := KMRect(1 + EnsureRange(aInsetRect.Left, 0, aInsetRect.Left),
    //                       1 + EnsureRange(aInsetRect.Top, 0, aInsetRect.Top),
    //                       EnsureRange(fMapX + aInsetRect.Left, fMapX + aInsetRect.Left, fMapX + aInsetRect.Left + aInsetRect.Right),
    //                       EnsureRange(fMapY + aInsetRect.Top, fMapY + aInsetRect.Top, fMapY + aInsetRect.Top + aInsetRect.Bottom));


    // Check if we need to generate some of the tiles, if we expand terrain land
    for I := 1 to SizeY do
    begin
      IFrom := EnsureRange(I - aInsetRect.Top, 1, fMapY - 1); //-1 because last row is not part of the map

      // Last col/row is saved into the .map file, but its actually not used!
      // So in case we resize map we do not need to use the exact last row/col, but previous one
      // So we will do that means when aInsetRect.Bottom > 0 or aInsetRect.Right > 0
      // And for simple map save (or when we do not enlarge to the right / bottom)
      // there is no need to generate new tile, just save those 'fake/bot used' tiles
      // Prolly we would need to get rid of that last tiles in the future
      NewGenTileI := (IFrom <> I - aInsetRect.Top)
                      and ((I - aInsetRect.Top <> fMapY) or (aInsetRect.Bottom > 0)); //
      extTop := I <= aInsetRect.Top;
      extBot := I - aInsetRect.Top >= fMapY;
      D :=  Ord(dirN)*Byte(extTop) + Ord(dirS)*Byte(extBot); //Only 1 could happen
      for K := 1 to SizeX do
      begin
        KFrom := EnsureRange(K - aInsetRect.Left, 1, fMapX - 1); //-1 because last col is not part of the map
        NewGenTileK := (KFrom <> K - aInsetRect.Left)
                        and ((K - aInsetRect.Left <> fMapX) or (aInsetRect.Right > 0)); //
        extLeft := K <= aInsetRect.Left;
        extRight := K - aInsetRect.Left >= fMapX;

        if D = 0 then
          D := Ord(dirW)*Byte(extLeft) + Ord(dirE)*Byte(extRight) //Only 1 could happen
        else
        begin
          D := D + (Byte(extBot)*2 - 1)*Byte(extLeft) + (Byte(extTop)*2 - 1)*Byte(extRight); //step left or right
          D := ((D - 1 + 8) mod 8) + 1; //circle around for 0-value as dirNE
        end;

        SetNewLand(S, K, I, KFrom, IFrom, NewGenTileK or NewGenTileI, TKMDirection(D));
      end;
    end;

    //Update header info with MapDataSize
    S.Seek(0, soFromBeginning);
    WriteFileHeader(S);

    S.SaveToFile(aFile);
  finally
    S.Free;
  end;
end;


function TKMTerrain.TrySetTileHeight(X, Y: Integer; aHeight: Byte; aUpdatePassability: Boolean = True): Boolean;

  function UnitWillGetStuck(CheckX, CheckY: Integer): Boolean;
  var
    U: TKMUnit;
  begin
    U := Land^[CheckY, CheckX].IsUnit;
    if (U = nil) or U.IsDeadOrDying
    or (gRes.Units[U.UnitType].DesiredPassability = tpFish) then //Fish don't care about elevation
      Result := False
    else
      Result := not CheckHeightPass(KMPoint(CheckX, CheckY), hpWalking); //All other units/animals need Walkable
  end;

var
  oldHeight: Byte;
  I, K: Integer;
begin
  //To use CheckHeightPass we must apply change then roll it back if it failed
  oldHeight := aHeight;
  //Apply change
  Land^[Y, X].Height := aHeight;
  UpdateRenderHeight(X, Y);

  //Don't check canElevate: If scripter wants to block mines that's his choice

  //Elevation affects all 4 tiles around the vertex
  for I := -1 to 0 do
    for K := -1 to 0 do
      if TileInMapCoords(X+K, Y+I) then
        //Did this change make a unit stuck?
        if UnitWillGetStuck(X+K, Y+I)
        //Did this change elevate a house?
        or (Land^[Y+I, X+K].TileLock = tlHouse) then
        begin
          //Rollback change
          Land^[Y, X].Height := oldHeight;
          UpdateRenderHeight(X, Y);
          Exit(False);
        end;

  //Accept change
  if aUpdatePassability then
  begin
    UpdateLighting(KMRectGrow(KMRect(X, Y, X, Y), 2));
    UpdatePassability(KMRectGrowTopLeft(KMRect(X, Y, X, Y)));
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(X, Y, X, Y)), False);
  end;
  Result := True;
end;


function TKMTerrain.TrySetTile(X, Y: Integer; aType, aRot: Integer; aUpdatePassability: Boolean = True): Boolean;
var
  tempRect: TKMRect;
  tempBool: Boolean;
begin
  Result := TrySetTile(X, Y, aType, aRot, tempRect, tempBool, aUpdatePassability);
end;


function TKMTerrain.TrySetTile(X, Y: Integer; aType, aRot: Integer; out aPassRect: TKMRect;
                               out aDiagonalChanged: Boolean; aUpdatePassability: Boolean = True): Boolean;
  function UnitWillGetStuck: Boolean;
  var
    U: TKMUnit;
  begin
    U := Land^[Y, X].IsUnit;
    if (U = nil) or U.IsDeadOrDying then
      Result := False
    else
      if gRes.Units[U.UnitType].DesiredPassability = tpFish then
        Result := not fTileset[aType].Water //Fish need water
      else
        Result := not fTileset[aType].Walkable; //All other animals need Walkable
  end;
var
  loc: TKMPoint;
  locRect: TKMRect;
  doRemField: Boolean;
begin
  Assert((aType <> -1) or (aRot <> -1), 'Either terrain type or rotation should be set');

  // Do not allow to set some special terrain tiles
  if (aType <> -1) // We could have aType = -1 if only specify rotation
    and not gRes.Tileset.TileIsAllowedToSet(aType) then
    Exit(False);
 
  loc := KMPoint(X, Y);
  locRect := KMRect(loc);
  aPassRect := locRect;
  
  //First see if this change is allowed
  //Will this change make a unit stuck?
  if UnitWillGetStuck
    //Will this change block a construction site?
    or ((Land^[Y, X].TileLock in [tlFenced, tlDigged, tlHouse, tlWall, tlWallFence])
      and (not fTileSet[aType].Roadable or not fTileset[aType].Walkable)) then
    Exit(False);

  aDiagonalChanged := False;

  // Remove field only if we will change tile type
  // and aType is a new one
  doRemField := (aType <> -1)
    and (TileIsCornField(loc) or TileIsWineField(loc));
  if doRemField then
    RemField(loc, False, False, aPassRect, aDiagonalChanged, False);

  //Apply change
  if aType <> -1 then // Do not update terrain, if -1 is passed as an aType parameter
    Land^[Y, X].BaseLayer.Terrain := aType;
  if aRot <> -1 then // Do not update rotation, if -1 is passed as an aRot parameter
    Land^[Y, X].BaseLayer.Rotation := aRot;
 

  if doRemField then
    UpdateFences(loc); // after update Terrain

  if aUpdatePassability then
  begin
    UpdatePassability(aPassRect);
    UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWork], aPassRect, aDiagonalChanged);
  end;

  Result := True;
end;


function TKMTerrain.TrySetTileObject(X, Y: Integer; aObject: Word; aUpdatePassability: Boolean = True): Boolean;
var
  diagonalChanged: Boolean;
begin
  Result := TrySetTileObject(X, Y, aObject, diagonalChanged, aUpdatePassability);
end;


function TKMTerrain.TrySetTileObject(X, Y: Integer; aObject: Word; out aDiagonalChanged: Boolean; aUpdatePassability: Boolean = True): Boolean;
  function HousesNearObject: Boolean;
  var
    I, K: Integer;
  begin
    Result := False;
    //If the object blocks diagonals, houses can't be at -1 either
    for I := -1 * Byte(gMapElements[aObject].DiagonalBlocked) to 0 do
      for K := -1 * Byte(gMapElements[aObject].DiagonalBlocked) to 0 do
      if TileInMapCoords(X+K, Y+I) then
        //Can't put objects near houses or house sites
        if (Land^[Y+I, X+K].TileLock in [tlFenced, tlDigged, tlHouse, tlWall, tlWallFence]) then
          Exit(True);
  end;

  // We do not want falling trees
  function AllowableObject: Boolean;
  begin
    // Hide falling trees
    // Invisible objects like 254 or 255 can be useful to clear specified tile (since delete object = place object 255)
    Result := (gMapElements[aObject].Stump = -1) or (aObject in [OBJ_INVISIBLE, OBJ_NONE]);
  end;
var
  loc: TKMPoint;
  locRect: TKMRect;
begin
  loc := KMPoint(X,Y);
  aDiagonalChanged := False;

  //There's no need to check conditions for 255 (NO OBJECT)
  if (aObject <> OBJ_NONE) then
  begin
    //Will this change make a unit stuck?
    if ((Land^[Y, X].IsUnit <> nil) and gMapElements[aObject].AllBlocked)
      //Is this object part of a wine/corn field?
      or TileIsWineField(loc) or TileIsCornField(loc)
      //Is there a house/site near this object?
      or HousesNearObject
      //Is this object allowed to be placed?
      or not AllowableObject then
      Exit(False);
  end;

  //Did block diagonal property change? (hence xor) UpdateWalkConnect needs to know
  aDiagonalChanged := gMapElements[Land^[Y,X].Obj].DiagonalBlocked xor gMapElements[aObject].DiagonalBlocked;

  Land^[Y, X].Obj := aObject;
  Result := True;
  //Apply change
  //UpdatePassability and UpdateWalkConnect are called in SetField so that we only use it in trees and other objects
  {case aObject of
    88..124,
    126..172: // Trees - 125 is mushroom
              begin
                if ObjectIsChopableTree(loc, caAge1) then Land^[Y,X].TreeAge := 1;
                if ObjectIsChopableTree(loc, caAge2) then Land^[Y,X].TreeAge := TREE_AGE_1;
                if ObjectIsChopableTree(loc, caAge3) then Land^[Y,X].TreeAge := TREE_AGE_2;
                if ObjectIsChopableTree(loc, caAgeFull) then Land^[Y,X].TreeAge := TREE_AGE_FULL;
              end
  end;}

  if ObjectIsChopableTree(loc, caAgeFull) then
    Land^[Y, X].TreeAge := CORN_AGE_MAX
  else
  if aObject = 390 then
    Land^[Y, X].TreeAge := 1
  else
  if ObjectIsChopableTree(loc, caAge1) then
    if gMapElements[aObject].PrevTreeAgeObj > 0 then
      Land^[Y, X].TreeAge := gMapElements[gMapElements[aObject].PrevTreeAgeObj].TreeGrowAge
    else
      Land^[Y, X].TreeAge := 0;

  if aUpdatePassability then
  begin
    locRect := KMRect(loc);
    UpdatePassability(locRect); //When using KMRect map bounds are checked by UpdatePassability
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(locRect), aDiagonalChanged);
  end;
end;


// Try to set an array of Tiles from script. Set terrain, rotation, height and object.
// Update Passability, WalkConnect and Lighting only once at the end.
// This is much faster, then set tile by tile with updates on every change
//
// Returns True if succeeded
// use var for aTiles. aTiles can be huge so we do want to make its local copy. Saves a lot of memory
function TKMTerrain.ScriptTrySetTilesArray(var aTiles: array of TKMTerrainTileBrief; aRevertOnFail: Boolean; var aErrors: TKMTerrainTileChangeErrorArray): Boolean;

  procedure UpdateRect(var aRect: TKMRect; X, Y: Integer);
  begin
    if KMSameRect(aRect, KMRECT_INVALID_TILES) then
      aRect := KMRect(X, Y, X, Y)
    else
      KMRectIncludePoint(aRect, X, Y);
  end;

  procedure UpdateRectWRect(var aRect: TKMRect; aRect2: TKMRect);
  begin
    if KMSameRect(aRect, KMRECT_INVALID_TILES) then
      aRect := aRect2
    else 
      KMRectIncludeRect(aRect, aRect2);
  end;

  procedure SetErrorNSetResult(aType: TKMTileChangeType; var aHasErrorOnTile: Boolean; var aErrorType: TKMTileChangeTypeSet; var aResult: Boolean);
  begin
    Include(aErrorType, aType);
    aHasErrorOnTile := True;
    aResult := False;
  end;

  procedure UpdateHeight(aTileBrief: TKMTerrainTileBrief; var aHeightRect: TKMRect; var aHasErrorOnTile: Boolean; var aErrorTypesOnTile: TKMTileChangeTypeSet);
  begin
    // Update height if needed
    if aTileBrief.UpdateHeight then
    begin
      if InRange(aTileBrief.Height, 0, HEIGHT_MAX) then
      begin
        if TrySetTileHeight(aTileBrief.X, aTileBrief.Y, aTileBrief.Height, False) then
          UpdateRect(aHeightRect, aTileBrief.X, aTileBrief.Y)
        else
          SetErrorNSetResult(tctHeight, aHasErrorOnTile, aErrorTypesOnTile, Result);
      end else
        SetErrorNSetResult(tctHeight, aHasErrorOnTile, aErrorTypesOnTile, Result);
    end;
  end;

var 
  I, J, terr, rot: Integer;
  T: TKMTerrainTileBrief;
  rect, terrRect, heightRect: TKMRect;
  diagonalChangedTotal, diagChanged: Boolean;
  backupLand: array of array of TKMTerrainTile;
  errCnt: Integer;
  hasErrorOnTile: Boolean;
  errorTypesOnTile: TKMTileChangeTypeSet;
begin
  Result := True;
  if Length(aTiles) = 0 then Exit;

  //Initialization
  diagonalChangedTotal := False;
  rect := KMRECT_INVALID_TILES;
  // Use separate HeightRect, because UpdateLight invoked only when Height is changed
  heightRect := KMRECT_INVALID_TILES;
  errCnt := 0;

  // make backup copy of Land only if we may need revert changes
  if aRevertOnFail then
  begin
    SetLength(backupLand, fMapY, fMapX);
    for I := 1 to fMapY do
      for J := 1 to fMapX do
        backupLand[I-1][J-1] := Land^[I, J];
  end;

  for I := 0 to High(aTiles) do
  begin
    T := aTiles[I];

    hasErrorOnTile := False;
    errorTypesOnTile := [];

    if TileInMapCoords(T.X, T.Y) then
    begin
      terr := -1;
      if T.UpdateTerrain then
        terr := T.Terrain;
        
      rot := -1;
      if T.UpdateRotation and InRange(T.Rotation, 0, 3) then
        rot := T.Rotation;

      if T.UpdateTerrain or T.UpdateRotation then
      begin
        if (terr <> -1) or (rot <> -1) then
        begin
          // Update terrain and rotation if needed
          if TrySetTile(T.X, T.Y, terr, rot, terrRect, diagChanged, False) then
          begin
            diagonalChangedTotal := diagonalChangedTotal or diagChanged;
            UpdateRectWRect(rect, terrRect);
          end else begin
            SetErrorNSetResult(tctTerrain, hasErrorOnTile, errorTypesOnTile, Result);
            SetErrorNSetResult(tctRotation, hasErrorOnTile, errorTypesOnTile, Result);
          end;
        end else begin
          SetErrorNSetResult(tctTerrain, hasErrorOnTile, errorTypesOnTile, Result);
          SetErrorNSetResult(tctRotation, hasErrorOnTile, errorTypesOnTile, Result);
        end;
      end;

      // Update height if needed
      UpdateHeight(T, heightRect, hasErrorOnTile, errorTypesOnTile);

      //Update object if needed
      if T.UpdateObject then
      begin
        if TrySetTileObject(T.X, T.Y, T.Obj, diagChanged, False) then
        begin
          UpdateRect(rect, T.X, T.Y);
          diagonalChangedTotal := diagonalChangedTotal or diagChanged;
        end else
          SetErrorNSetResult(tctObject, hasErrorOnTile, errorTypesOnTile, Result);
      end;
    end
    else
    if VerticeInMapCoords(T.X, T.Y) then
      // Update height if needed
      UpdateHeight(T, heightRect, hasErrorOnTile, errorTypesOnTile)
    else
    begin
      hasErrorOnTile := True;
      //When tile is out of map coordinates we treat it as all operations failure
      if T.UpdateTerrain then
        Include(errorTypesOnTile, tctTerrain);
      if T.UpdateHeight then
        Include(errorTypesOnTile, tctHeight);
      if T.UpdateObject then
        Include(errorTypesOnTile, tctObject);
    end;

    // Save error info, if there was some error
    if hasErrorOnTile then
    begin
      if Length(aErrors) = errCnt then
        SetLength(aErrors, errCnt + 16);
      aErrors[errCnt].X := T.X;
      aErrors[errCnt].Y := T.Y;
      aErrors[errCnt].ErrorsIn := errorTypesOnTile;
      Inc(errCnt);
    end;

    if not Result and aRevertOnFail then
      Break;
  end;

  if not Result and aRevertOnFail then
  begin
    //Restore backup Land, when revert needed
    for I := 1 to fMapY do
      for J := 1 to fMapX do
        Land^[I, J] := backupLand[I-1][J-1];
    SetLength(backupLand, 0); // Release dynamic array memory. This array can be huge, so we should clear it as fast as possible
  end
  else
  begin
    // Actualize terrain for map editor (brushes have array which helps them make smooth transitions)
    if (gGameParams.Mode = gmMapEd) then
      for I := 1 to fMapY do
        for J := 1 to fMapX do
          gGame.TerrainPainter.RMG2MapEditor(J,I, Land^[I, J].BaseLayer.Terrain);

    if not KMSameRect(heightRect, KMRECT_INVALID_TILES) then
      gTerrain.UpdateLighting(KMRectGrow(heightRect, 2)); // Update Light only when height was changed

    gTerrain.UpdatePassability(KMRectGrowTopLeft(rect));
    gTerrain.UpdateWalkConnect([wcWalk, wcRoad, wcFish, wcWork], KMRectGrowTopLeft(rect), diagonalChangedTotal);
  end;

  //Cut errors array to actual size
  if Length(aErrors) <> errCnt then
    SetLength(aErrors, errCnt);
end;


// Try to set an tile (Terrain and Rotation) from the script. Failure is an option
function TKMTerrain.ScriptTrySetTile(X, Y, aType, aRot: Integer): Boolean;
begin
  Result := TileInMapCoords(X, Y) and TrySetTile(X, Y, aType, aRot);
end;


// Try to set an tile Height from the script. Failure is an option
function TKMTerrain.ScriptTrySetTileHeight(X, Y, aHeight: Integer): Boolean;
begin
  Result := TileInMapCoords(X, Y) and TrySetTileHeight(X, Y, aHeight);
end;


// Try to set an object from the script. Failure is an option
function TKMTerrain.ScriptTrySetTileObject(X, Y, aObject: Integer): Boolean;
begin
  Result := TileInMapCoords(X, Y) and TrySetTileObject(X, Y, aObject);
end;


// Check if requested tile (X,Y) is within Map boundaries
// X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used
function TKMTerrain.TileInMapCoords(const X,Y: Integer; const aInset: Byte): Boolean;
begin
  // Direct comparison is a bit faster, than using InRange
  Result := (X >= 1 + aInset) and (X <= fMapX - 1 - aInset) and (Y >= 1 + aInset) and (Y <= fMapY - 1 - aInset);
end;


function TKMTerrain.TileInMapCoords(const X,Y: Integer): Boolean;
begin
  // Direct comparison is a bit faster, than using InRange
  Result := (X >= 1) and (X <= fMapX - 1) and (Y >= 1) and (Y <= fMapY - 1);
end;


function TKMTerrain.CoordsWithinMap(const X, Y: Single; const aInset: Byte = 0): Boolean;
begin
  Result :=     (X >= 1 + aInset)
            and (X <= fMapX - 1 - aInset)
            and (Y >= 1 + aInset)
            and (Y <= fMapY - 1 - aInset)
end;


function TKMTerrain.PointFInMapCoords(const aPointF: TKMPointF; const aInset: Byte = 0): Boolean;
begin
  Result := CoordsWithinMap(aPointF.X, aPointF.Y, aInset);
end;


function TKMTerrain.TileInMapCoords(const aCell: TKMPoint; const Inset: Byte = 0): Boolean;
begin
  Result := TileInMapCoords(aCell.X, aCell.Y, Inset);
end;


function TKMTerrain.TileInMapCoords(const X,Y: Integer; const aInsetRect: TKMRect): Boolean;
begin
  Result :=     InRange(X, 1 + aInsetRect.Left, fMapX - 1 + aInsetRect.Right)
            and InRange(Y, 1 + aInsetRect.Top,  fMapY - 1 + aInsetRect.Bottom);
end;


{Check if requested vertice is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TKMTerrain.VerticeInMapCoords(const X,Y: Integer; const aInset: Byte = 0): Boolean;
begin
  Result := InRange(X, 1 + aInset, fMapX - aInset) and InRange(Y, 1 + aInset, fMapY - aInset);
end;


function TKMTerrain.VerticeInMapCoords(const aCell: TKMPoint; const aInset: Byte = 0): Boolean;
begin
  Result := VerticeInMapCoords(aCell.X, aCell.Y, aInset);
end;


{Ensure that requested tile is within Map boundaries}
{X,Y are unsigned int, usually called from loops, hence no TKMPoint can be used}
function TKMTerrain.EnsureTileInMapCoords(const X,Y: Integer; const aInset: Byte = 0): TKMPoint;
begin
  Result.X := EnsureRange(X, 1 + aInset, fMapX - 1 - aInset);
  Result.Y := EnsureRange(Y, 1 + aInset, fMapY - 1 - aInset);
end;


procedure TKMTerrain.EnsureCoordsWithinMap(var X, Y: Single; const aInset: Byte = 0);
begin
  X := EnsureRange(X, 1 + aInset, fMapX - 1 - aInset);
  Y := EnsureRange(Y, 1 + aInset, fMapY - 1 - aInset);
end;


function TKMTerrain.EnsureTilesRectWithinMap(const aRectF: TKMRectF; const aInset: Single = 0): TKMRectF;
begin
  Result.Left   := EnsureRangeF(aRectF.Left,   1 + aInset, fMapX - 1 - aInset);
  Result.Right  := EnsureRangeF(aRectF.Right,  1 + aInset, fMapX - 1 - aInset);
  Result.Top    := EnsureRangeF(aRectF.Top,    1 + aInset, fMapY - 1 - aInset);
  Result.Bottom := EnsureRangeF(aRectF.Bottom, 1 + aInset, fMapY - 1 - aInset);
end;


function TKMTerrain.EnsureVerticesRectWithinMap(const aRectF: TKMRectF; const aInset: Single = 0): TKMRectF;
begin
  Result.Left   := EnsureRangeF(aRectF.Left,   aInset, fMapX - aInset);
  Result.Right  := EnsureRangeF(aRectF.Right,  aInset, fMapX - aInset);
  Result.Top    := EnsureRangeF(aRectF.Top,    aInset, fMapY - aInset);
  Result.Bottom := EnsureRangeF(aRectF.Bottom, aInset, fMapY - aInset);
end;


function TKMTerrain.EnsureTileInMapCoords(const aLoc: TKMPoint; const aInset: Byte = 0): TKMPoint;
begin
  Result := EnsureTileInMapCoords(aLoc.X, aLoc.Y, aInset);
end;


function TKMTerrain.TileGoodForIronMine(X,Y: Word): Boolean;
var
  cornersTKinds: TKMTerrainKindCorners;
begin
  Result :=
    (fTileset[Land^[Y,X].BaseLayer.Terrain].IronMinable)
      and ((Land^[Y,X].BaseLayer.Rotation mod 4 = 0) or (Land^[Y,X].BaseLayer.Terrain > 680)); //only horizontal mountain edges allowed
  if not Result then
  begin
    GetTileCornersTerKinds(X, Y, cornersTKinds);
    Result :=
          (cornersTKinds[0] in [tkIron, tkIronMount])
      and (cornersTKinds[1] in [tkIron, tkIronMount])
      and fTileset[BASE_TERRAIN[cornersTKinds[2]]].Roadable
      and fTileset[BASE_TERRAIN[cornersTKinds[3]]].Roadable;
  end;
end;


function TKMTerrain.CanPlaceIronMine(X,Y: Word): Boolean;
begin
  Result := TileGoodForIronMine(X,Y)
    and ((Land[Y,X].Obj = OBJ_NONE) or (gMapElements[Land^[Y,X].Obj].CanBeRemoved))
    and TileInMapCoords(X,Y, 1)
    and not HousesNearTile(X,Y)
    and (Land^[Y,X].TileLock = tlNone)
    and CheckHeightPass(KMPoint(X,Y), hpBuildingMines);
end;

function TKMTerrain.CanPlaceShipYard(X,Y: Word): Boolean;
  function HasWater(aX, aY : Integer) : Boolean;
  begin
    Result := false;
    if TileInMapCoords(aX, aY) then
      Result := CheckPassability(aX, aY, tpFish);
  end;
begin
  //shipyard needs to be close to water

  Result := HasWater(X - 1, Y + 4)  //under entrance on the right
            or HasWater(X + 2, Y + 4)
            or HasWater(X + 4, Y + 2)//on the right
            or HasWater(X - 3, Y + 2);//on the left from the entrace

  Result := Result
    and ((Land[Y,X].Obj = OBJ_NONE) or (gMapElements[Land^[Y,X].Obj].CanBeRemoved))
    and TileInMapCoords(X,Y, 1)
    and not HousesNearTile(X,Y)
    and (Land^[Y,X].TileLock = tlNone)
    and CheckHeightPass(KMPoint(X,Y), hpBuilding);


end;


function TKMTerrain.TileGoodForGoldMine(X,Y: Word): Boolean;
var
  cornersTKinds: TKMTerrainKindCorners;
begin
  Result :=
    (fTileset[Land^[Y,X].BaseLayer.Terrain].GoldMinable)
      and ((Land^[Y,X].BaseLayer.Rotation mod 4 = 0) or (Land^[Y,X].BaseLayer.Terrain > 680)); //only horizontal mountain edges allowed
  if not Result then
  begin
    GetTileCornersTerKinds(X, Y, cornersTKinds);
    Result :=
          (cornersTKinds[0] in [tkGold, tkGoldMount])
      and (cornersTKinds[1] in [tkGold, tkGoldMount])
      and fTileset[BASE_TERRAIN[cornersTKinds[2]]].Roadable
      and fTileset[BASE_TERRAIN[cornersTKinds[3]]].Roadable;
  end;
end;


function TKMTerrain.TileGoodForField(X,Y: Word): Boolean;
begin
  Result := //TileIsSoil(X,Y)
    {and }not gMapElements[Land^[Y,X].Obj].AllBlocked
    and (Land^[Y,X].TileLock = tlNone)
    and gRes.Tileset[TILE_OVERLAY_IDS[Land^[Y,X].TileOverlay2]].Walkable
    and not (Land^[Y,X].TileOverlay in ROAD_LIKE_OVERLAYS)
    and not (Land^[Y,X].TileOverlay2 in COAL_LIKE_OVERLAYS)
    //and not TileIsWineField(KMPoint(X,Y))
    //and not TileIsCornField(KMPoint(X,Y))
    //and not TileIsGrassField(KMPoint(X,Y))
    and CheckHeightPass(KMPoint(X,Y), hpWalking)
    and not TileHasPalisade(X, Y);
end;

function TKMTerrain.TileGoodForCornField(X, Y: Word): Boolean;
begin
  Result := TileGoodForField(X, Y)
            and not TileIsWineField(KMPoint(X,Y))
            and not TileIsGrassField(KMPoint(X,Y))
            and not TileIsVegeField(KMPoint(X,Y))
            and TileHasTerKinds(X, Y, CORN_TERR_KINDS, false, 3);
end;

function TKMTerrain.TileGoodForWineField(X, Y: Word): Boolean;
begin
  Result := TileGoodForField(X, Y)
            and not TileIsWineField(KMPoint(X,Y))
            and not TileIsCornField(KMPoint(X,Y))
            and not TileIsGrassField(KMPoint(X,Y))
            and not TileIsVegeField(KMPoint(X,Y))
            and TileHasTerKinds(X, Y, WINE_TERR_KINDS, false, 3);

end;

function TKMTerrain.TileGoodForGrassField(X, Y: Word): Boolean;
begin
  Result := TileGoodForField(X, Y)
            and not TileIsWineField(KMPoint(X,Y))
            and not TileIsCornField(KMPoint(X,Y))
            and not TileIsVegeField(KMPoint(X,Y))
            and TileHasTerKinds(X, Y, GRASS_TERR_KINDS, false, 3);

end;


function TKMTerrain.TileGoodToPlantTree(X,Y: Word): Boolean;
  function IsObjectsNearby: Boolean;
  var
    I,K: Integer;
    P: TKMPoint;
  begin
    Result := False;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if ((I<>0) or (K<>0)) and TileInMapCoords(X+I, Y+K) then
        begin
          P := KMPoint(X+I, Y+K);

          //Tiles next to it can't be trees/stumps
          if gMapElements[Land^[P.Y,P.X].Obj].DontPlantNear then
            Result := True;

          //Tiles above or to the left can't be road/field/locked
          if (I <= 0) and (K <= 0) then
            if (Land^[P.Y,P.X].TileLock <> tlNone)
            or (Land^[P.Y,P.X].TileOverlay in ROAD_LIKE_OVERLAYS)
            or not (Land^[Y,X].TileOverlay2 in TILE_OVERLAY_ALLOW_TREES)
            or TileIsCornField(P)
            or TileIsWineField(P) then
              Result := True;

          if Result then Exit;
        end;
  end;

  function HousesNearVertex: Boolean;
  var
    I,K: Integer;
  begin
    Result := False;
    for I := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    for K := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      if TileInMapCoords(X+K, Y+I)
      and (Land^[Y+I,X+K].TileLock in [tlFenced,tlDigged,tlHouse]) then
      begin
        if (I+1 in [0,1]) and (K+1 in [0,1]) then //Only houses above/left of the tile
          Result := True;
      end;
  end;

  // Do not allow to plant tree on vertex with NW-SE only passable tiles around
  // It could trap woodcutter if he came from top-left tile or close some narrow path between areas
  function Is_NW_SE_OnlyVertex: Boolean;
  begin
    Result :=       CheckPassability(X    , Y    , tpWalk)  // O | X   // O - walkable (OK)
            and     CheckPassability(X - 1, Y - 1, tpWalk)  // --T--   // X - not walkable
            and not CheckPassability(X    , Y - 1, tpWalk)  // X | W   // T - Tree to plant
            and not CheckPassability(X - 1, Y    , tpWalk); //         // W - woodcutter
  end;

begin
  //todo: Optimize above functions. Recheck UpdatePass and WC if the check Rects can be made smaller

  Result := {TileIsSoil(X,Y)
    and }not IsObjectsNearby //This function checks surrounding tiles
    and (Land^[Y,X].TileLock = tlNone)
    and (X > 1) and (Y > 1) //Not top/left of map, but bottom/right is ok
    and not (Land^[Y,X].TileOverlay in ROAD_LIKE_OVERLAYS)
    and (Land^[Y,X].TileOverlay2 in TILE_OVERLAY_ALLOW_TREES)
    and not HousesNearVertex
    and not TileHasClay(X, Y)
    and not Is_NW_SE_OnlyVertex
    //Woodcutter will dig out other object in favour of his tree
    and ((Land[Y,X].Obj = OBJ_NONE) or (gMapElements[Land^[Y,X].Obj].CanBeRemoved))
    and CheckHeightPass(KMPoint(X,Y), hpWalking)
    and (FindBestTreeClimatType(KMPoint(X,Y)) <> tcNone); // We could plant some tree type
end;


//Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed
function TKMTerrain.TileIsWater(const aLoc: TKMPoint): Boolean;
begin
  Result := TileIsWater(aLoc.X, aLoc.Y);
end;


function TKMTerrain.TileIsWater(X,Y : Word): Boolean;
begin
  Result := TileHasParameter(X, Y, fTileset.TileIsWater);
end;


//Check if requested tile is sand suitable for crabs
function TKMTerrain.TileIsSand(const aLoc: TKMPoint): Boolean;
begin
  Result := TileHasParameter(aLoc.X, aLoc.Y, fTileset.TileIsSand);
end;


function TKMTerrain.TileIsSnow(X, Y: Word): Boolean;
begin
  Result := TileHasParameter(X, Y, fTileset.TileIsSnow);
end;


//Check if requested tile is Stone and returns Stone deposit
function TKMTerrain.TileIsStone(X,Y: Word): Byte;
begin
  Result := IfThen(Land[Y, X].HasNoLayers, fTileset[Land^[Y, X].BaseLayer.Terrain].Stone, 0);
  //Result := Result + gMapElements[Land[Y,X].Obj].Stone;
end;


function TKMTerrain.TileIsCoal(X,Y: Word): Byte;
var aTO : TKMTileOverlay;
begin
  Result := IfThen(Land[Y, X].HasNoLayers, fTileset[Land^[Y, X].BaseLayer.Terrain].Coal, 0);

  aTO := Land[Y, X].TileOverlay2;
  Result := Result + IfThen(aTO in COAL_LIKE_OVERLAYS, fTileset[TILE_OVERLAY_IDS[aTO]].Coal, 0);
  if Land[Y, X].TileOverlay2 = toCoal then
    Result := Result + Land[Y, X].Ware.C2;

  //Result := Result + gMapElements[Land[Y,X].Obj].Coal;
end;

function TKMTerrain.TileIsClay(X,Y: Word): Byte;
//var aTO : TKMTileOverlay;
begin
  //aTO := Land[Y, X].TileOverlay2;
  //Result := IfThen(aTO in CLAY_LIKE_OVERLAYS, fTileset[TILE_OVERLAY_IDS[aTO]].Clay, 0);
  Result := gMapElements[Land[Y,X].Obj].Clay;
end;


function TKMTerrain.TileIsIron(X,Y: Word): Byte;
begin
  Result := IfThen(Land[Y, X].HasNoLayers, fTileset[Land^[Y, X].BaseLayer.Terrain].Iron, 0);
  if Land[Y, X].TileOverlay2 = toIron then
    Result := Result + Land[Y, X].Ware.C2;
  //Result := Result + gMapElements[Land[Y,X].Obj].Iron;
end;


function TKMTerrain.TileIsBitinIron(X,Y: Word): Byte;
begin
  Result := IfThen(Land[Y, X].HasNoLayers, fTileset[Land^[Y, X].BaseLayer.Terrain].Bitin, 0);
  if Land[Y, X].TileOverlay2 = toBitin then
    Result := Result + Land[Y, X].Ware.C2;
  //Result := Result + gMapElements[Land[Y,X].Obj].Bitin;
end;


function TKMTerrain.TileIsGold(X,Y: Word): Byte;
begin
  Result := IfThen(Land[Y, X].HasNoLayers, fTileset[Land^[Y, X].BaseLayer.Terrain].Gold, 0);
  if Land[Y, X].TileOverlay2 = toGold then
    Result := Result + Land[Y, X].Ware.C2;
  //Result := Result + gMapElements[Land[Y,X].Obj].Gold;
end;


function TKMTerrain.TileHasStone(X, Y: Word): Boolean;
begin
  Result := TileIsStone(X, Y) > 0;
end;


function TKMTerrain.TileHasCoal(X, Y: Word): Boolean;
begin
  Result := TileIsCoal(X, Y) > 0;
end;


function TKMTerrain.TileHasIron(X, Y: Word): Boolean;
begin
  Result := TileIsIron(X, Y) > 0;
end;

function TKMTerrain.TileHasBitinIron(X, Y: Word): Boolean;
begin
  Result := TileIsBitinIron(X, Y) > 0;
end;

function TKMTerrain.TileHasClay(X, Y: Word): Boolean;
begin
  Result := TileIsClay(X, Y) > 0;
end;


function TKMTerrain.TileHasGold(X, Y: Word): Boolean;
begin
  Result := TileIsGold(X, Y) > 0;
end;

function TKMTerrain.TileHasWall(X, Y: Word): Boolean;
begin
  Result := Land^[Y, X].TileLock in [tlWall, tlWallFence, tlWallGate];
end;

function TKMTerrain.TileHasPalisade(X, Y: Word): Boolean;
begin
  Result := InRange(Land^[Y, X].Obj, 256, 263);
  Result := Result and (Land^[Y, X].TileOwner >= 0);
end;

function TKMTerrain.TileHasTerrainKindPart(X, Y: Word; aTerKind: TKMTerrainKind): Boolean;
var
  K: Integer;
  cornersTerKinds: TKMTerrainKindCorners;
begin
  Result := False;
  //GetVerticeTerKinds(KMPoint(X, Y), cornersTerKinds);
  GetTileCornersTerKinds(X, Y, cornersTerKinds);
  for K := 0 to 3 do
    if cornersTerKinds[K] = aTerKind then
      Exit(True);
end;


function TKMTerrain.TileHasTerrainKindPart(X, Y: Word; aTerKind: TKMTerrainKind; aDir: TKMDirection): Boolean;
var
  cornersTKinds: TKMTerrainKindCorners;
begin
  Result := False;
  GetTileCornersTerKinds(X, Y, cornersTKinds);

  case aDir of
    dirNA:  Result := TileHasStone(X, Y);
    dirN:   Result := (cornersTKinds[0] = aTerKind) and (cornersTKinds[1] = aTerKind);
    dirNE:  Result := (cornersTKinds[1] = aTerKind);
    dirE:   Result := (cornersTKinds[1] = aTerKind) and (cornersTKinds[2] = aTerKind);
    dirSE:  Result := (cornersTKinds[2] = aTerKind);
    dirS:   Result := (cornersTKinds[2] = aTerKind) and (cornersTKinds[3] = aTerKind);
    dirSW:  Result := (cornersTKinds[3] = aTerKind);
    dirW:   Result := (cornersTKinds[3] = aTerKind) and (cornersTKinds[0] = aTerKind);
    dirNW:  Result := (cornersTKinds[0] = aTerKind);
  end;
end;


function TerKindArrayContains(aElement: TKMTerrainKind; const aArray: array of TKMTerrainKind): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(aArray) to High(aArray) do
    if aElement = aArray[I] then
      Exit (True);
end;


function TKMTerrain.TileHasOnlyTerrainKinds(X, Y: Word; const aTerKinds: array of TKMTerrainKind): Boolean;
var
  I: Integer;
  cornersTerKinds: TKMTerrainKindCorners;
begin
  Result := True;
  GetTileCornersTerKinds(X, Y, cornersTerKinds);
  for I := 0 to 3 do
    if not TerKindArrayContains(cornersTerKinds[I], aTerKinds) then
      Exit(False);
end;

function TKMTerrain.TileHasTerrainKinds(X: Word; Y: Word; const aTerKinds: TKMTerrainKindSet): Boolean;
var
  I: Integer;
  cornersTerKinds: TKMTerrainKindCorners;
begin
  Result := false;

  GetTileCornersTerKinds(X, Y, cornersTerKinds);
  for I := 0 to 3 do
    if cornersTerKinds[I] in aTerKinds then
      Exit(True);
end;

function TKMTerrain.TileHasOnlyTerrainKind(X, Y: Word; const aTerKind: TKMTerrainKind): Boolean;
var
  I: Integer;
  cornersTerKinds: TKMTerrainKindCorners;
begin
  Result := True;
  GetTileCornersTerKinds(X, Y, cornersTerKinds);
  for I := 0 to 3 do
    if cornersTerKinds[I] <> aTerKind then
      Exit(False);
end;

function TKMTerrain.TileHasOnlyTerrainKinds(aLoc : TKMPoint; const aTerKind: TKMTerrainKindSet): Boolean;
var
  TK : TKMTerrainKind;
begin
  Result := false;

  for TK in aTerKind do
  begin
    Result := Result or TileHasOnlyTerrainKind(aLoc.X, aLoc.Y, TK);
    if Result then
      Exit;
  end;
end;


//Try get the only terkind that this tile represents
function TKMTerrain.TileTryGetTerKind(X, Y: Word; var aTerKind: TKMTerrainKind): Boolean;
var
  I: Integer;
  cornersTerKinds: TKMTerrainKindCorners;
begin
  Result := True;
  GetTileCornersTerKinds(X, Y, cornersTerKinds);
  aTerKind := cornersTerKinds[0];
  for I := 1 to 3 do
    if cornersTerKinds[I] <> aTerKind then
    begin
      aTerKind := tkCustom;
      Exit(False); // Corners has different terKinds, return tkCustom then
    end;
end;


//Check if requested tile is soil suitable for fields and trees
function TKMTerrain.TileIsSoil(X,Y: Word): Boolean;
begin
  Result := TileHasParameter(X, Y, fTileset.TileIsSoil);
end;


function TKMTerrain.TileIsSoil(const aLoc: TKMPoint): Boolean;
begin
  Result := TileIsSoil(aLoc.X, aLoc.Y);
end;


function TKMTerrain.TileIsIce(X, Y: Word): Boolean;
begin
  Result := TileHasParameter(X, Y, fTileset.TileIsIce);
end;


function TKMTerrain.TileHasWater(X, Y: Word): Boolean;
begin
  Result := fTileset[Land^[Y,X].BaseLayer.Terrain].HasWater;
end;

function TKMTerrain.TileHasWater(P : TKMPoint): Boolean;
begin
  Result := TileHasWater(P.X, P.Y);
end;

function TKMTerrain.TileHasParameter(X,Y: Word; aCheckTileFunc: TBooleanWordFunc; aAllow2CornerTiles: Boolean = False;
                                     aStrictCheck: Boolean = False): Boolean;
const
  PROHIBIT_TERKINDS: array[0..1] of TKMTerrainKind = (tkLava, tkAbyss);
  //Strict check (for roadable)
  STRICT_TERKINDS: array[0..4] of TKMTerrainKind = (tkGrassyWater, tkSwamp, tkIce, tkWater, tkFastWater);
var
  I, K, Cnt: Integer;
  corners: TKMTerrainKindCorners;
begin
  Result := False;

  if not TileInMapCoords(X, Y) then Exit;

  if Land^[Y,X].HasNoLayers then
    Result := aCheckTileFunc(Land^[Y, X].BaseLayer.Terrain)
  else
  begin
    Cnt := 0;
    GetTileCornersTerKinds(X, Y, corners);
    for K := 0 to 3 do
    begin
      for I := 0 to High(PROHIBIT_TERKINDS) do
        if corners[K] = PROHIBIT_TERKINDS[I] then
          Exit(False);

      if aStrictCheck then
        for I := 0 to High(STRICT_TERKINDS) do
          if corners[K] = STRICT_TERKINDS[I] then
            Exit(False);

      if aCheckTileFunc(BASE_TERRAIN[corners[K]]) then
        Inc(Cnt);
    end;

    //Consider tile has parameter if it has 3 corners with that parameter or if it has 2 corners and base layer has the parameter
    Result := (Cnt >= 3) or (aAllow2CornerTiles and (Cnt = 2) and aCheckTileFunc(Land^[Y, X].BaseLayer.Terrain));
  end;
end;


//Check if requested tile is generally walkable
function TKMTerrain.TileIsWalkable(const aLoc: TKMPoint): Boolean;
//var
//  L: Integer;
//  Ter: Word;
//  TerInfo: TKMGenTerrainInfo;
begin
  Result := TileHasParameter(aLoc.X, aLoc.Y, fTileset.TileIsWalkable, True);
//  Result := fTileset.TileIsWalkable(Land^[Loc.Y, Loc.X].BaseLayer.Terrain);
//  for L := 0 to Land^[Loc.Y, Loc.X].LayersCnt - 1 do
//  begin
//    if not Result then Exit;
//
//    Ter := Land^[Loc.Y, Loc.X].Layer[L].Terrain;
//    TerInfo := gRes.Sprites.GetGenTerrainInfo(Ter);
//    // Check if this layer walkable
//    // It could be, if its mask does not restrict walkability or its BASE terrain is walkable
//    Result := Result
//                and ((TILE_MASKS_PASS_RESTRICTIONS[TerInfo.Mask.MType,TerInfo.Mask.SubType,0] = 0)
//                  or fTileset.TileIsWalkable(BASE_TERRAIN[TerInfo.TerKind]));
//
//  end;
end;


//Check if requested tile is generally suitable for road building
function TKMTerrain.TileIsRoadable(const Loc: TKMPoint): Boolean;
//var
//  L: Integer;
//  Ter: Word;
//  TerInfo: TKMGenTerrainInfo;
begin
  Result := TileHasParameter(Loc.X, Loc.Y, fTileset.TileIsRoadable, False, True);
//  Result := fTileset.TileIsRoadable(Land^[Loc.Y, Loc.X].BaseLayer.Terrain);
//  for L := 0 to Land^[Loc.Y, Loc.X].LayersCnt - 1 do
//  begin
//    if not Result then Exit;
//
//    Ter := Land^[Loc.Y, Loc.X].Layer[L].Terrain;
//    TerInfo := gRes.Sprites.GetGenTerrainInfo(Ter);
//    // Check if this layer walkable
//    // It could be, if its mask does not restrict walkability or its BASE terrain is walkable
//    Result := Result
//                and ((TILE_MASKS_PASS_RESTRICTIONS[TerInfo.Mask.MType,TerInfo.Mask.SubType,1] = 0)
//                  or fTileset.TileIsRoadable(BASE_TERRAIN[TerInfo.TerKind]));
//
//  end;
end;


//Check if Tile has road overlay
function TKMTerrain.TileHasRoad(const aLoc: TKMPoint): Boolean;
begin
  Result := TileHasRoad(aLoc.X,aLoc.Y);
end;


function TKMTerrain.TileHasRoad(X,Y: Integer): Boolean;
begin
  Result := TileInMapCoords(X, Y) and (Land^[Y, X].TileOverlay = toRoad);
end;


//Check if the tile is a corn field
function TKMTerrain.TileIsCornField(const aLoc: TKMPoint): Boolean;
begin
  if not TileInMapCoords(aLoc.X,aLoc.Y) then Exit(False);

  //Tile can't be used as a field if there is road or any other overlay
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine = 1) and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain].Corn
              and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone);
end;


function TKMTerrain.TileIsCornField(const X, Y: Word): Boolean;
begin
  if not TileInMapCoords(X, Y) then Exit(False);

  //Tile can't be used as a field if there is road or any other overlay
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[Y,X].CornOrWine = 1) and (Land^[Y,X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[Y, X].BaseLayer.Terrain].Corn
              and (Land^[Y,X].TileOverlay = toNone);
end;

function TKMTerrain.TileIsGrassField(const aLoc : TKMPoint): Boolean;
begin
  if not TileInMapCoords(aLoc) then Exit(False);

  //Tile can't be used as a field if there is road or any other overlay
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine = 3) and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain].Grass
              and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone);

end;

function TKMTerrain.TileIsGrassField(const X, Y: Word): Boolean;
begin
  Result := TileIsGrassField(KMPoint(X, Y));
end;

function TKMTerrain.TileIsVegeField(const aLoc : TKMPoint): Boolean;
begin
  //Tile can't be used as a field if there is road or any other overlay
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine = 4) and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain].Vegetables
              and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone);

end;
function TKMTerrain.TileIsVegeField(const X, Y: Word): Boolean;
begin
  Result := TileIsVegeField(KMPoint(X, Y));
end;

function TKMTerrain.TileIsMineShaft(aLoc: TKMPoint): Boolean;
begin
  Result := fTileset[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain].MineShaft;
end;

procedure TKMTerrain.PalaceExploreDeposits(const aLoc: TKMPoint);
var List : TKMPointTagList;
  I : Integer;
begin
  List := TKMPointTagList.Create;
  FindDeposits(aLoc, 25, List, 5);

  for I := 0 to List.Count - 1 do
    if  List.Tag[I] = 4 then
      SetObject(List[I], 537)
    else
      SetObject(List[I], 505 + List.Tag[I]);

  list.Free;
end;

procedure TKMTerrain.FindDeposits(const aLoc: TKMPoint; aRadius : Byte; aDeposits : TKMPointTagList; OverlapRadius : Byte = 0);
var area : TKMByte2Array;
  rec, rec2 : TKMRect;
  I, K, L : Integer;

  tmpPoints : TKMPointArray;
  tmpTags : Integer;

  function HasDeposit(X, Y : Integer; aType : TKMTileOverlay = toNone) : Boolean;
  begin
    if aType = toNone then
      Result := (Land[Y, X].TileOverlay2 in [toGold, toIron, toBitin, toCoal]) and (Land[Y, X].Ware.C2 > 0)
    else
      Result := (Land[Y, X].TileOverlay2 = aType) and (Land[Y, X].Ware.C2 > 0);
  end;

  procedure CheckTile(X, Y : Integer; aDistance : Integer; aType : TKMTileOverlay = toNone);
  begin
    if (area[X, Y] <> 0) or not HasDeposit(X, Y, aType) then
      Exit;
    if aDistance >= 7 then
      Exit;

    //deposits were found
    //if (aType = toNone) or (Land[K, I].TileOverlay2 = aType) then
    begin
      aType := Land[K, I].TileOverlay2;
      area[X, Y] := byte(Land[K, I].TileOverlay2) - byte(toGold) + 1;
      tmpPoints.Add(X, Y);
      if (X - 1 >= 1) then
      begin
        if (Y - 1 >= 1) then            CheckTile(X - 1, Y - 1, aDistance + 1, aType); //left top
                                        CheckTile(X - 1, Y, aDistance + 1, aType);    //left
        if (Y + 1 < fMapY) then         CheckTile(X - 1, Y + 1, aDistance + 1, aType); //left bottom
      end;

      if (Y - 1 >= 1) then
        CheckTile(X, Y - 1, aDistance + 1, aType); // bottom
      if (Y + 1 < fMapY) then
        CheckTile(X, Y + 1, aDistance + 1, aType); // top

      if (X + 1 < fMapX) then
      begin
        if (Y - 1 >= 1) then            CheckTile(X + 1, Y - 1, aDistance + 1, aType); //right top
                                        CheckTile(X + 1, Y, aDistance + 1, aType);    //right
        if (Y + 1 < fMapY) then         CheckTile(X + 1, Y + 1, aDistance + 1, aType); //right bottom
      end;
    end;
  end;

  procedure CheckClosest(aIndex : Integer; aRadius : Byte);
  var J: Integer;
    P : TKMPoint;
  begin

    P := aDeposits[aIndex];
    for J := 0 to aDeposits.Count - 1 do
      if (aDeposits.Tag[J] > 0) and (KMLengthDiag(P, aDeposits[J]) <= aRadius) then
      if (tmpTags = 0) or (aDeposits.Tag[J] = tmpTags) then
      begin
        tmpPoints.Add(aDeposits[J]);
        tmpTags := aDeposits.Tag[J];
        aDeposits.Tag[J] := 0;
      end;
  end;

var
  tmpArr : array of record
    X, Y, Tag : Integer;
  end;
  tmpX, tmpY : Integer;
begin
  SetLength(area, fMapX , fMapY);
  SetLength(tmpArr, 0);
  rec := KMRectGrow(KMRect(aLoc), aRadius);
  rec.FitInMap(fMapX - 1, fMapY - 1);
  for I := rec.Left to rec.Right do
  for K := rec.Top to rec.Bottom do
    if (area[I, K] = 0) and HasDeposit(I, K) then
    begin
      tmpPoints.Clear;
      CheckTile(I, K, 0, toNone);
      if tmpPoints.Count > 0 then
      begin
        //find avarage point
        rec2 := KMRect( fMapX, fMapY, 0, 0);
        for L := 0 to tmpPoints.Count - 1 do
        begin
          rec2.Left := Min(rec2.Left, tmpPoints[L].X);
          rec2.Top := Min(rec2.Top, tmpPoints[L].Y);
          rec2.Right := Max(rec2.Right, tmpPoints[L].X);
          rec2.Bottom := Max(rec2.Bottom, tmpPoints[L].Y);
        end;
        tmpX := rec2.Left + rec2.Width div 2;//tmpX div tmpPoints.Count;
        tmpY := rec2.Top + rec2.Height div 2;//tmpY div tmpPoints.Count;
        aDeposits.Add(KMPoint(tmpX, tmpY), area[I, K]);
      end;
    end;

  //if deposits are close to each other then reduce it's count
  if OverlapRadius > 0 then
  begin
    for I := 0 to aDeposits.Count -1 do
      if aDeposits.Tag[I] > 0 then
      begin
        tmpPoints.Clear;
        tmpTags := aDeposits.Tag[I];
        CheckClosest(I, 8);

        if (tmpPoints.Count > 0) and (tmpTags > 0) then
        begin
          //find avarage point
          rec2 := KMRect( fMapX, fMapY, 0, 0);
          for L := 0 to tmpPoints.Count - 1 do
          begin
            rec2.Left := Min(rec2.Left, tmpPoints[L].X);
            rec2.Top := Min(rec2.Top, tmpPoints[L].Y);
            rec2.Right := Max(rec2.Right, tmpPoints[L].X);
            rec2.Bottom := Max(rec2.Bottom, tmpPoints[L].Y);
          end;
          tmpX := rec2.Left + rec2.Width div 2;//tmpX div tmpPoints.Count;
          tmpY := rec2.Top + rec2.Height div 2;//tmpY div tmpPoints.Count;
          SetLength(tmpArr, length(tmpArr) + 1);
          tmpArr[high(tmpArr)].X := tmpX;
          tmpArr[high(tmpArr)].Y := tmpY;
          tmpArr[high(tmpArr)].Tag := tmpTags;
        end;
      end;
    aDeposits.Clear;
    for I := 0 to High(tmpArr) do
      aDeposits.Add(KMPoint(tmpArr[I].X, tmpArr[I].Y), tmpArr[I].Tag);
  end;
end;

procedure TKMTerrain.FindDeposits(const aLoc: TKMPoint; aRadius : Byte; aDeposits : TKMPointTagList; aIncludeNormalOre : Boolean);
var
  rec : TKMRect;
  I, K : Integer;

  tmpPoints : TKMPointArray;
  tmpTags : Integer;

  function HasDeposit(X, Y : Integer; aType : TKMTileOverlay = toNone) : Boolean;
  begin
    if aType = toNone then
      Result := (Land[Y, X].TileOverlay2 in [toGold, toIron, toBitin, toCoal]) and (Land[Y, X].Ware.C2 > 0)
    else
      Result := (Land[Y, X].TileOverlay2 = aType) and (Land[Y, X].Ware.C2 > 0);
  end;

begin
  rec := KMRectGrow(KMRect(aLoc), aRadius);
  rec.FitInMap(fMapX - 1, fMapY - 1);
  for I := rec.Left to rec.Right do
  for K := rec.Top to rec.Bottom do
    if HasDeposit(I, K) then
      aDeposits.Add(KMPoint(I, K), byte(Land[K, I].TileOverlay2) - byte(toGold) + 1, Land[K, I].Ware.C2);
end;


function TKMTerrain.GetTileCornObject(const aLoc: TKMPoint; aStage: Byte): Word;
begin
  Result := gFieldGrains[Land[aLoc.Y, aLoc.X].GrainType].Stage[aStage].Obj;
  if Result = 0 then
    Result := 255;
end;

function TKMTerrain.GetTileCornTile(const aLoc: TKMPoint; aStage: Byte): Word;
begin
  Result := gFieldGrains[Land[aLoc.Y, aLoc.X].GrainType].Stage[aStage].Terr;
end;
function TKMTerrain.GetTileGrassObject(const aLoc: TKMPoint; aStage: Byte): Word;
begin
  Result := gFieldGrains[Land[aLoc.Y, aLoc.X].GrainType].Stage[aStage].Obj;
  if Result = 0 then
    Result := 255;
end;

function TKMTerrain.GetTileGrassTile(const aLoc: TKMPoint; aStage: Byte): Word;
begin
  Result := gFieldGrains[Land[aLoc.Y, aLoc.X].GrainType].Stage[aStage].Terr;
end;



function TKMTerrain.GetTileWineObject(const aLoc: TKMPoint; aStage: Byte): Word;
begin
  Result := gFieldGrains[Land[aLoc.Y, aLoc.X].GrainType].Stage[aStage].Obj;
end;

function TKMTerrain.GetTileWineType(const aLoc: TKMPoint): TKMGrainType;
begin
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkGrassDirt, tkCoal], not fMapEditor, 2) then
    Result := gftRaspberry
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkMoss, tkGravel], not fMapEditor, 2) then
    Result := gftBlackberry
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkSnowOnGrass, tkSnow], not fMapEditor, 2) then
    Result := gftWildRose
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkGrassSand3, tkCoastSand], not fMapEditor, 2) then
    Result := gftPomegranate
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkSnowOnDirt, tkDirt], not fMapEditor, 2) then
    Result := gftWildStrawberry
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkGrassSand2], not fMapEditor, 2) then
    Result := gftWineWhite
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkPaleGrass], not fMapEditor, 2) then
    Result := gftWineBlack
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkGrassSand1], not fMapEditor, 2) then
    Result := gftWineRed
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkSand], not fMapEditor, 2) then
    Result := gftCactus
  else
  if TileHasTerKinds(aLoc.X, aLoc.Y, [tkDeepSnow], not fMapEditor, 2) then
    Result := gftIceSticks
  else
    Result := gftWinePurple;

end;

//Check if the tile is a wine field
function TKMTerrain.TileIsWineField(const aLoc: TKMPoint): Boolean;
begin
  if not TileInMapCoords(aLoc.X,aLoc.Y) then Exit(False);

  //Tile can't be used as a winefield if there is road or any other overlay
  //It also must have right object on it
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine = 2) and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain].Wine
              and (Land^[aLoc.Y,aLoc.X].TileOverlay = toNone)
              and ObjectIsWine(aLoc);
end;


function TKMTerrain.TileIsWineField(const X, Y: Word): Boolean;
begin
  if not TileInMapCoords(X, Y) then Exit(False);

  //Tile can't be used as a winefield if there is road or any other overlay
  //It also must have right object on it
  if fMapEditor then
    Result := (gGame.MapEditor.LandMapEd^[Y,X].CornOrWine = 2) and (Land^[Y,X].TileOverlay = toNone)
  else
    Result := fTileset[Land^[Y, X].BaseLayer.Terrain].Wine
              and (Land^[Y,X].TileOverlay = toNone)
              and ObjectIsWine(X, Y)
end;


//Check if the tile is a walkable road
function TKMTerrain.TileIsWalkableRoad(const aLoc: TKMPoint): Boolean;
begin
  Result := False;
  if not TileInMapCoords(aLoc.X,aLoc.Y) then
    Exit;
  // Is map editor OK with this?
  Result := (tpWalkRoad in Land^[aLoc.Y,aLoc.X].Passability);
end;   


function TKMTerrain.VerticeIsFactorable(const aLoc: TKMPoint): Boolean;
const
  //Non factorable terkinds
  NON_FACT_TER_KINDS: set of TKMTerrainKind = [tkIron, tkIronMount, tkGold, tkGoldMount, tkLava, tkAbyss, tkCustom];

var
  I: Integer;
  verticeTKinds: TKMTerrainKindCorners;
begin
  if   not TileInMapCoords(aLoc.X,     aLoc.Y)
    or not TileInMapCoords(aLoc.X - 1, aLoc.Y)
    or not TileInMapCoords(aLoc.X,     aLoc.Y - 1)
    or not TileInMapCoords(aLoc.X - 1, aLoc.Y - 1) then Exit(False);

  Result := True;

  GetVerticeTerKinds(aLoc, verticeTKinds);

  for I := 0 to 3 do
    if verticeTKinds[I] in NON_FACT_TER_KINDS then
      Exit(False);
end;


function TKMTerrain.CanCutTreeAtVertex(const aWoodcutterPos, aTreeVertex: TKMPoint): Boolean;

  function TileIsChecked(aLoc: TKMPoint): Boolean;
  begin
    Result := not RouteCanBeMade(aWoodcutterPos, aLoc, tpWalk) // Do not check tiles, which we can't reach
              or TileIsGoodToCutTree(aLoc);
  end;

begin
  Result := RouteCanBeMadeToVertex(aWoodcutterPos, aTreeVertex, tpWalk)
        and TileIsChecked(aTreeVertex)
        and ((aTreeVertex.X = 1) or TileIsChecked(KMPoint(aTreeVertex.X - 1, aTreeVertex.Y))) //if K=1, K-1 will be off map
        and ((aTreeVertex.Y = 1) or TileIsChecked(KMPoint(aTreeVertex.X, aTreeVertex.Y - 1)))
        and ((aTreeVertex.X = 1) or (aTreeVertex.Y = 1) or TileIsChecked(KMPoint(aTreeVertex.X - 1, aTreeVertex.Y - 1)))
end;


function TKMTerrain.TileIsGoodToCutTree(const aLoc: TKMPoint): Boolean;
var
  U: TKMUnit;
begin
  U := Land^[aLoc.Y,aLoc.X].IsUnit;

  Result := (U = nil)
            or U.IsAnimal
            or (U.Action = nil)
            or not U.Action.Locked
            or (U.Action is TKMUnitActionGoInOut);
end;

function TKMTerrain.TileHasCollectorsGoods(const X: Integer; const Y: Integer): Boolean;
begin

  Result := (gMapElements[Land^[Y, X].Obj].Iron > 0)
            or (gMapElements[Land^[Y, X].Obj].Stone > 0)
            or (gMapElements[Land^[Y, X].Obj].Coal > 0)
            or (gMapElements[Land^[Y, X].Obj].Gold > 0)
            or ((Land^[Y, X].Ware.W > 0) and (Land^[Y, X].Ware.C > 0))
            or (length(gMapElements[Land^[Y, X].Obj].VWares) > 0);
end;

function TKMTerrain.TileIsLocked(const aLoc: TKMPoint): Boolean;
var
  U: TKMUnit;
begin
  U := Land^[aLoc.Y,aLoc.X].IsUnit;
  //Action=nil can happen due to calling TileIsLocked during Unit.UpdateState.
  //Checks for Action=nil happen elsewhere, this is not the right place.
  if (U <> nil) and (U.Action = nil) then
    Result := False
  else
    Result := (U <> nil) and (U.Action.Locked);
end;


//Get tile corner terrain id
function TKMTerrain.TileCornerTerrain(aX, aY: Integer; aCorner: Byte): Word;
const
  TOO_BIG_VALUE = 50000;
var
  L: Integer;
begin
  Assert(InRange(aCorner, 0, 3), 'aCorner = ' + IntToStr(aCorner) + ' is not in range [0-3]');
  Result := TOO_BIG_VALUE;
  with gTerrain.Land^[aY,aX] do
  begin
    if BaseLayer.Corner[aCorner] then
      Result := BASE_TERRAIN[gRes.Tileset[BaseLayer.Terrain].TerKinds[(aCorner + 4 - BaseLayer.Rotation) mod 4]]
    else
      for L := 0 to LayersCnt - 1 do
        if Layer[L].Corner[aCorner] then
          Result := BASE_TERRAIN[gRes.Sprites.GetGenTerrainInfo(Layer[L].Terrain).TerKind];
  end;
  Assert(Result <> TOO_BIG_VALUE, Format('[TileCornerTerrain] Can''t determine tile [%d:%d] terrain at Corner [%d]', [aX, aY, aCorner]));
end;


//Get tile corners terrain id
function TKMTerrain.TileCornersTerrains(aX, aY: Integer): TKMWordArray;
var
  K: Integer;
  cornersTKinds: TKMTerrainKindCorners;
begin
  SetLength(Result, 4);
  GetTileCornersTerKinds(aX, aY, cornersTKinds);
  for K := 0 to 3 do
    Result[K] := BASE_TERRAIN[cornersTKinds[K]];
end;


function TKMTerrain.TileCornerTerKind(aX, aY: Integer; aCorner: Byte): TKMTerrainKind;
var
  L: Integer;
begin
  Assert(InRange(aCorner, 0, 3));
  
  Result := tkCustom;
  with gTerrain.Land^[aY,aX] do
  begin
    if BaseLayer.Corner[aCorner] then
      Result := gRes.Tileset[BaseLayer.Terrain].TerKinds[(aCorner + 4 - BaseLayer.Rotation) mod 4]
    else
      for L := 0 to LayersCnt - 1 do
        if Layer[L].Corner[aCorner] then
        begin
          Result := gRes.Sprites.GetGenTerrainInfo(Layer[L].Terrain).TerKind;
          Break;
        end;
  end;
end;


//Get tile corners terrain kinds
procedure TKMTerrain.GetTileCornersTerKinds(aX, aY: Integer; out aCornerTerKinds: TKMTerrainKindCorners);
var
  K: Integer;
begin
  for K := 0 to 3 do
    aCornerTerKinds[K] := TileCornerTerKind(aX, aY, K);
end;


// Get vertice terrain kinds
procedure TKMTerrain.GetVerticeTerKinds(const aLoc: TKMPoint; out aVerticeTerKinds: TKMTerrainKindCorners);
  function GetTerKind(aX, aY, aCorner: Integer): TKMTerrainKind;
  begin
    Result := tkCustom;
    if TileInMapCoords(aX, aY) then
      Result := TileCornerTerKind(aX, aY, aCorner);
  end;
begin
  aVerticeTerKinds[0] := GetTerKind(aLoc.X - 1, aLoc.Y - 1, 2); //  0 | 1
  aVerticeTerKinds[1] := GetTerKind(aLoc.X    , aLoc.Y - 1, 3); //  __|__
  aVerticeTerKinds[2] := GetTerKind(aLoc.X    , aLoc.Y    , 0); //    |
  aVerticeTerKinds[3] := GetTerKind(aLoc.X - 1, aLoc.Y    , 1); //  3 | 2
end;

function TKMTerrain.TileHasTerKind(aX, aY : Word; aTerKind : TKMTerrainKind; aDefaultTile: Boolean = false; aMinCornersCount : Byte = 1) : Boolean;
var aID, I, cnt : Integer;
begin
  if aDefaultTile then
    aID := Land^[aY, aX].DefTile
  else
    aID := Land^[aY, aX].BaseLayer.Terrain;
  cnt := 0;
  Result := false;
  for I := 0 to 3 do
    if gRes.Tileset[aID].TerKinds[I] = aTerKind then
    begin
      Inc(cnt);
      if cnt >= aMinCornersCount then
        Exit(true);
    end;
end;

function TKMTerrain.TileHasTerKinds(aX, aY : Word; aTerKind : TKMTerrainKindSet; aDefaultTile: Boolean = false; aMinCornersCount : Byte = 1) : Boolean;
var aID, I, cnt : Integer;
begin
  if aDefaultTile then
    aID := Land^[aY, aX].DefTile
  else
    aID := Land^[aY, aX].BaseLayer.Terrain;

  Result := false;
  cnt := 0;
  for I := 0 to 3 do
    if gRes.Tileset[aID].TerKinds[I] in aTerKind then
    begin
      Inc(cnt);
      if cnt >= aMinCornersCount then
        Exit(true);
    end;
end;


// Check if there's unit on the tile
// Note that IsUnit refers to where unit started walking to, not the actual unit position
// (which is what we used in unit interaction), so check all 9 tiles to get accurate result
function TKMTerrain.UnitsHitTest(X,Y: Word): Pointer;
var
  I, K: Integer;
  U: TKMUnit;
begin
  Result := nil;
  for I := Max(Y - 1, 1) to Min(Y + 1, fMapY) do
  for K := Max(X - 1, 1) to Min(X + 1, fMapX) do
  begin
    U := Land^[I,K].IsUnit;
    if (U <> nil) and U.HitTest(X,Y) then
      Result := Land^[I,K].IsUnit;
  end;
end;


//Test up to 4x4 related tiles around and pick unit whos no farther than 1 tile
function TKMTerrain.UnitsHitTestF(const aLoc: TKMPointF): Pointer;
var
  I, K: Integer;
  U: TKMUnit;
  T: Single;
begin
  Result := nil;
  for I := Max(Trunc(aLoc.Y) - 1, 1) to Min(Trunc(aLoc.Y) + 2, fMapY) do
    for K := Max(Trunc(aLoc.X) - 1, 1) to Min(Trunc(aLoc.X) + 2, fMapX) do
    begin
      U := Land^[I,K].IsUnit;
      if U <> nil then
      begin
        T := KMLengthSqr(U.PositionF, aLoc);
        if (T <= 1) and ((Result = nil) or (T < KMLengthSqr(TKMUnit(Result).PositionF, aLoc))) then
          Result := U;
      end;
    end;
end;

procedure TKMTerrain.UnitsHitAllTestF(const aLoc: TKMPointF; const aRadius : Single; out aUnits : TPointerArray);
var
  I, K: Integer;
  U: TKMUnit;
  T: Single;
  units : TPointerArray;
begin
  SetLength(units, 0);
  for I := Max(Trunc(aLoc.Y) - 1, 1) to Min(Trunc(aLoc.Y) + 2, fMapY) do
    for K := Max(Trunc(aLoc.X) - 1, 1) to Min(Trunc(aLoc.X) + 2, fMapX) do
    begin
      U := Land^[I,K].IsUnit;
      if (U <> nil) and not U.IsDeadOrDying and not U.IsAnimal then
      begin
        T := KMLength(U.PositionF, aLoc);
        if (T <= aRadius) then
        begin
          SetLength(units, length(units) + 1);
          units[high(units)] := U;
        end;
      end;
    end;
  aUnits := units;
end;

procedure TKMTerrain.HousesHitAllTestF(const aLoc: TKMPointF; const aRadius : Single; out aHouses : TPointerArray);
var
  I, K: Integer;
  H: TKMHouse;
  T: Single;
  houses : TPointerArray;

  function HasHouse(aHouse : Pointer) : Boolean;
  var L : Integer;
  begin
    Result := false;
    for L := 0 to High(houses) do
      if houses[L] = aHouse then
        Exit(true);

  end;

begin
  SetLength(houses, 0);
  for I := Max(Trunc(aLoc.Y) - 1, 1) to Min(Trunc(aLoc.Y) + 2, fMapY) do
    for K := Max(Trunc(aLoc.X) - 1, 1) to Min(Trunc(aLoc.X) + 2, fMapX) do
    begin
      H := Land^[I,K].IsHouse;
      if H <> nil then
      begin
        T := KMLength(KMPointF(K, I), aLoc);
        if (T <= aRadius) and not HasHouse(H) then
        begin
          SetLength(houses, length(houses) + 1);
          houses[high(houses)] := H;
        end;
      end;
    end;
  aHouses := houses;
end;

//Function to use with WatchTowers/Archers/Warriors
{ Should scan withing given radius and return closest unit with given Alliance status
  Should be optimized versus usual UnitsHitTest
  Prefer Warriors over Citizens}
function TKMTerrain.IsPlayerUnitWithinRad(const aPlayer : Integer; const aLoc: TKMPoint; const aRadius: Byte): Boolean;
var
  I, K: Integer;
  U: TKMUnit;
begin
  Result := false;
  for I := Max(aLoc.Y - aRadius, 1) to Min(aLoc.Y + aRadius, fMapY) do
    for K := Max(aLoc.X - aRadius, 1) to Min(aLoc.X + aRadius, fMapX) do
    begin
      U := Land^[I,K].IsUnit;
      if (U <> nil) and (U.Owner = aPlayer) then
        if ( KMLengthSqr(U.Position, aLoc) <= aRadius) then
          Exit(true);
    end;
end;

function TKMTerrain.UnitsHitTestWithinRad(const aLoc: TKMPoint; aMinRad, aMaxRad: Single; aPlayer: TKMHandID; aAlliance: TKMAllianceType;
                                          aDir: TKMDirection; const aClosest: Boolean; aTestDiagWalkable: Boolean = True): Pointer;
type
  TKMUnitArray = array of TKMUnit;
  procedure Append(var aArray: TKMUnitArray; var aCount: Integer; const aUnit: TKMUnit);
  begin
    if aCount >= Length(aArray) then
      SetLength(aArray, aCount + 32);

    aArray[aCount] := aUnit;
    Inc(aCount);
  end;

  function Get90DegreeSectorRect: TKMRect;
  var IntegerRadius: Integer;
  begin
    //Scan one tile further than the maximum radius due to rounding
    IntegerRadius := Round(aMaxRad + 1);  //1.42 gets rounded to 1

    //If direction is east we can skip left half
    if aDir in [dirNE, dirE, dirSE] then Result.Left := aLoc.X+1
                                      else Result.Left := aLoc.X-IntegerRadius;
    //If direction is west we can skip right half
    if aDir in [dirNW, dirW, dirSW] then Result.Right := aLoc.X-1
                                      else Result.Right := aLoc.X+IntegerRadius;
    //If direction is south we can skip top half
    if aDir in [dirSE, dirS, dirSW] then Result.Top := aLoc.Y+1
                                      else Result.Top := aLoc.Y-IntegerRadius;
    //If direction is north we can skip bottom half
    if aDir in [dirNE, dirN, dirNW] then Result.Bottom := aLoc.Y-1
                                      else Result.Bottom := aLoc.Y+IntegerRadius;

    Result := KMClipRect(Result, 1, 1, fMapX, fMapY); //Clip to map bounds
  end;

var
  I,K: Integer; //Counters
  boundsRect: TKMRect;
  dX,dY: Integer;
  requiredMaxRad: Single;
  U: TKMUnit;
  P: TKMPoint;
  wCount, cCount, initialSize: Integer;
  W, C: TKMUnitArray;
begin
  wCount := 0;
  cCount := 0;

  if aClosest then
    initialSize := 1 //We only need to keep 1 result
  else
    initialSize := 32; //Should be enough most times, Append will add more if needed

  SetLength(W, initialSize);
  SetLength(C, initialSize);

  //This function sets LowX, LowY, HighX, HighY based on the direction
  boundsRect := Get90DegreeSectorRect;

  for I := boundsRect.Top to boundsRect.Bottom do
  for K := boundsRect.Left to boundsRect.Right do
  begin
    U := Land^[I,K].IsUnit;
    if U = nil then Continue; //Most tiles are empty, so check it first

    //Check archer sector. If it's not within the 90 degree sector for this direction, then don't use this tile (continue)
    dX := K - aLoc.X;
    dY := I - aLoc.Y;
    case aDir of
      dirN : if not ((Abs(dX) <= -dY) and (dY < 0)) then Continue;
      dirNE: if not ((dX > 0)         and (dY < 0)) then Continue;
      dirE:  if not ((dX > 0) and (Abs(dY) <= dX))  then Continue;
      dirSE: if not ((dX > 0)         and (dY > 0)) then Continue;
      dirS : if not ((Abs(dX) <= dY)  and (dY > 0)) then Continue;
      dirSW: if not ((dX < 0)         and (dY > 0)) then Continue;
      dirW:  if not ((dX < 0) and (Abs(dY) <= -dX)) then Continue;
      dirNW: if not ((dX < 0)         and (dY < 0)) then Continue;
    end;

    //Alliance is the check that will invalidate most candidates, so do it early on
    if U.IsDeadOrDying //U = nil already checked earlier (above sector check)
    or (gHands.CheckAlliance(aPlayer, U.Owner) <> aAlliance) //How do WE feel about enemy, not how they feel about us
    or ((U is TKMUnitWarriorSpikedTrap) and (not TKMUnitWarriorSpikedTrap(U).IsVisible))
    or not U.Visible then //Inside of house
      Continue;

    //Don't check tiles farther than closest Warrior
    if aClosest and (W[0] <> nil)
    and (KMLengthSqr(aLoc, KMPoint(K,I)) >= KMLengthSqr(aLoc, W[0].Position)) then
      Continue; //Since we check left-to-right we can't exit just yet (there are possible better enemies below)

    //In KaM archers can shoot further than sight radius (shoot further into explored areas)
    //so CheckTileRevelation is required, we can't remove it to optimise.
    //But because it will not invalidate many candidates, check it late so other checks can do their work first
    if (gHands[aPlayer].FogOfWar.CheckTileRevelation(K,I) <> 255) then Continue;

    //This unit could be on a different tile next to KMPoint(k,i), so we cannot use that anymore.
    //There was a crash caused by VertexUsageCompatible checking (k,i) instead of U.CurrPosition.
    //In that case aLoc = (37,54) and k,i = (39;52) but U.CurrPosition = (38;53).
    //This shows why you can't use (k,i) in checks because it is distance >2 from aLoc! (in melee fight)
    P := U.Position;

    requiredMaxRad := aMaxRad;
    if U is TKMUnitWarriorSpy then
      if not TKMUnitWarriorSpy(U).IsVisibleForEnemy then
          requiredMaxRad := 1.5;

    if (aMaxRad = 1) and KMStepIsDiag(aLoc, P) then
      requiredMaxRad := 1.42; //Use diagonal radius sqrt(2) instead

    if (not aTestDiagWalkable
        or CanWalkDiagonally(aLoc, P.X, P.Y)
          and ((Abs(aLoc.X - P.X) <> 1)
            or (Abs(aLoc.Y - P.Y) <> 1)
            or VertexUsageCompatible(aLoc, P)
          )
      )
      and InRange(KMLength(KMPointF(aLoc), U.PositionF), aMinRad, requiredMaxRad) //Unit's exact position must be close enough
    then
      if aClosest then
      begin
        if U is TKMUnitWarrior then
          W[0] := U
        else
          C[0] := U;
      end
      else
      begin
        if (U is TKMUnitWarrior)then
          Append(W, wCount, U)
        else
          Append(C, cCount, U);
      end;
  end;

  if aClosest then
  begin
    if W[0] <> nil then
      Result := W[0]
    else
      Result := C[0];
  end
  else
  begin
    if wCount > 0 then
      Result := W[KaMRandom(wCount, 'TKMTerrain.UnitsHitTestWithinRad')]
    else
      if cCount > 0 then
        Result := C[KaMRandom(cCount, 'TKMTerrain.UnitsHitTestWithinRad 2')]
      else
        Result := nil;
  end;
end;

function TKMTerrain.FindPlaceForUnit(aLoc : TKMPoint; aPass: TKMTerrainPassability; aMaxDistance : Byte) : TKMPoint;
var I: Integer;
  P : TKMPoint;
  aList: TKMPointList;
begin
  aList := TKMPointList.Create;

  I := aList.Count;
  aList.Add(aLoc);
  Result := KMPOINT_INVALID_TILE;
  //flood fill to find prefect spot for unit
  while (I < aList.Count) and (Result = KMPOINT_INVALID_TILE) do
  begin
    P := aList[I];

    if KMLengthDiag(aLoc, P) > aMaxDistance then
    begin
      Inc(I);
      Continue;
    end;
    if TileInMapCoords(P, 1) and CheckPassability(P, aPass) and not AvoidTile(P) then
      if (Land^[P.Y, P.X].IsUnit = nil) then
      if gHands.UnitsHitTest(P.X, P.Y) = nil then
        Result := P;

    aList.AddUnique(KMPoint(P.X + 1, P.Y));
    aList.AddUnique(KMPoint(P.X, P.Y + 1));
    aList.AddUnique(KMPoint(P.X - 1, P.Y));
    aList.AddUnique(KMPoint(P.X, P.Y - 1));

    Inc(I);
  end;
  aList.Free;
end;

function TKMTerrain.FindWalkableSpot(aLoc: TKMPoint): TKMPoint;
var I: Integer;
  P : TKMPoint;
  aList: TKMPointList;
begin
  aList := TKMPointList.Create;

  I := aList.Count;
  aList.Add(aLoc);
  Result := KMPOINT_INVALID_TILE;
  //flood fill to find prefect spot for unit
  while (I < aList.Count) and (Result = KMPOINT_INVALID_TILE) do
  begin
    P := aList[I];


    if TileInMapCoords(P, 1) and CheckPassability(P, tpWalk) and not AvoidTile(P) then
      if (Land^[P.Y, P.X].IsUnit = nil) then
        Result := P;

    aList.AddUnique(KMPoint(P.X + 1, P.Y));
    aList.AddUnique(KMPoint(P.X, P.Y + 1));
    aList.AddUnique(KMPoint(P.X - 1, P.Y));
    aList.AddUnique(KMPoint(P.X, P.Y - 1));

    Inc(I);
  end;
  aList.Free;
end;

function TKMTerrain.IsTileNearWater(aLoc: TKMPoint): Boolean;
var I, K : integer;
begin
  Result := false;
  for I := Max(aLoc.X - 1, 0) to Min(aLoc.X + 1, fMapX - 1) do
    for K := Max(aLoc.Y - 1, 0) to Min(aLoc.Y + 1, fMapY - 1) do
      if TileHasWater(I, K) then
        Exit(true);
end;

function TKMTerrain.IsTileNearLand(aLoc: TKMPoint): Boolean;
var I, K : integer;
begin
  Result := false;
  for I := Max(aLoc.X - 1, 0) to Min(aLoc.X + 1, fMapX - 1) do
    for K := Max(aLoc.Y - 1, 0) to Min(aLoc.Y + 1, fMapY - 1) do
      if CheckPassability(I, K, tpWalk) then
        Exit(true);
end;

function TKMTerrain.ObjectIsChopableTree(X,Y: Word): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsChoppableTree(Land^[Y,X].Obj);
end;


function TKMTerrain.ObjectIsChopableTree(const aLoc: TKMPoint; aStage: TKMChopableAge): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsChoppableTree(Land^[aLoc.Y,aLoc.X].Obj, aStage);
end;


function TKMTerrain.ObjectIsChopableTree(const aLoc: TKMPoint; aStages: TKMChopableAgeSet): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsChoppableTree(Land^[aLoc.Y,aLoc.X].Obj, aStages);
end;


function TKMTerrain.ObjectIsWine(const aLoc: TKMPoint): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsWine(Land^[aLoc.Y,aLoc.X].Obj)
end;


function TKMTerrain.ObjectIsWine(X,Y: Word): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsWine(Land^[Y,X].Obj);
end;


function TKMTerrain.ObjectIsCorn(const aLoc: TKMPoint): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsCorn(Land^[aLoc.Y,aLoc.X].Obj)
end;


function TKMTerrain.ObjectIsCorn(X,Y: Word): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsCorn(Land^[Y,X].Obj);
end;

function TKMTerrain.ObjectIsCornOrGrass(const aLoc: TKMPoint): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsCorn(Land^[aLoc.Y,aLoc.X].Obj) or KM_ResMapElements.ObjectIsGrass(Land^[aLoc.Y,aLoc.X].Obj);
end;

function TKMTerrain.ObjectIsCornOrGrass(X,Y: Word): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsCorn(Land^[Y,X].Obj) or KM_ResMapElements.ObjectIsGrass(Land^[Y,X].Obj);
end;

function TKMTerrain.ObjectIsVege(X,Y: Word): Boolean;
begin
  Result := KM_ResMapElements.ObjectIsVege(Land^[Y,X].Obj);
end;

// Check wherever unit can walk from A to B diagonally
// Return True if direction is either walkable or not diagonal
// Maybe this can also be used later for inter-tile passability
function TKMTerrain.CanWalkDiagonally(const aFrom: TKMPoint; aX, aY: SmallInt): Boolean;
begin
  Result := True;

  //Tiles are not diagonal to each other
  if (Abs(aFrom.X - aX) <> 1) or (Abs(aFrom.Y - aY) <> 1) then
    Exit;
                                                               //Relative tiles locations
  if (aFrom.X < aX) and (aFrom.Y < aY) then                                   //   A
    Result := not gMapElements[Land^[aY, aX].Obj].DiagonalBlocked              //     B
  else
  if (aFrom.X < aX) and (aFrom.Y > aY) then                                   //     B
    Result := not gMapElements[Land^[aY+1, aX].Obj].DiagonalBlocked            //   A
  else
  if (aFrom.X > aX) and (aFrom.Y > aY) then                                   //   B
    Result := not gMapElements[Land^[aFrom.Y, aFrom.X].Obj].DiagonalBlocked    //     A
  else
  if (aFrom.X > aX) and (aFrom.Y < aY) then                                   //     A
    Result := not gMapElements[Land^[aFrom.Y+1, aFrom.X].Obj].DiagonalBlocked; //   B
end;


//Place lock on tile, any new TileLock replaces old one, thats okay
procedure TKMTerrain.SetTileLock(const aLoc: TKMPoint; aTileLock: TKMTileLock);
var
  R: TKMRect;
begin
  Assert(aTileLock in [tlDigged, tlRoadWork, tlFieldWork, tlWallFence, tlStructure], 'We expect only these 5 locks, that affect only 1 tile an don''t change neighbours Passability');

  Land^[aLoc.Y, aLoc.X].TileLock := aTileLock;
  R := KMRect(aLoc);

  //Placing a lock on tile blocks tiles CanPlantTree
  UpdatePassability(KMRectGrow(R, 1));

  //Allowed TileLocks affect passability on this single tile
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], R, False);
end;


//Remove lock from tile
procedure TKMTerrain.UnlockTile(const aLoc: TKMPoint);
var
  R: TKMRect;
begin
  Assert(Land^[aLoc.Y, aLoc.X].TileLock in [tlDigged, tlRoadWork, tlFieldWork, tlWallFence, tlStructure], 'We expect only these 5 locks, that affect only 1 tile an don''t change neighbours Passability');

  Land^[aLoc.Y, aLoc.X].TileLock := tlNone;
  R := KMRect(aLoc);

  //Removing a lock from tile unblock BR tiles CanPlantTree
  UpdatePassability(KMRectGrow(R, 1));

  //Allowed TileLocks affect passability on this single tile
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], R, False);
end;


procedure TKMTerrain.SetRoads(aList: TKMPointTagList; aOwner: TKMHandID; aUpdateWalkConnects: Boolean = True);
var
  I: Integer;
  Y2, X2: Integer;
  bounds: TKMRect;
  hasBounds: Boolean;
  P : TKMPoint;
begin
  if aList.Count = 0 then Exit; //Nothing to be done

  for I := 0 to aList.Count - 1 do
  begin
    Y2 := aList[I].Y;
    X2 := aList[I].X;

    Land^[Y2, X2].TileOwner   := aOwner;
    Land^[Y2, X2].TileOverlay := toRoad;
    Land^[Y2, X2].FieldAge    := 0;
    Land^[Y2, X2].RoadType := TKMRoadType(aList.Tag[I]);
    if gMapElements[Land^[Y2, X2].Obj].WineOrCorn then
      RemoveObject(aList[I]);

    if Y2 > 1 then
      if Land^[Y2 - 1, X2].IsHouse <> nil then
      If TKMHouse(Land^[Y2 - 1, X2].IsHouse).PlaceRoad then
      begin
        P := TKMHouse(Land^[Y2 - 1, X2].IsHouse).Entrance;
        if KMPoint(X2, Y2) = KMPointBelow(P) then
          Land^[P.Y,P.X].RoadType := Land^[Y2, X2].RoadType;
      end;

    RemoveObjectsKilledByRoad(aList[I]);
    UpdateFences(aList[I]);
  end;

  hasBounds := aList.GetBounds(bounds);
  Assert(hasBounds);

  //Grow the bounds by extra tile because some passabilities
  //depend on road nearby (e.g. CanPlantTree)
  UpdatePassability(KMRectGrowBottomRight(bounds));

  //Roads don't affect wcWalk or wcFish
  if aUpdateWalkConnects then
    UpdateWalkConnect([wcRoad], bounds, False);
end;


procedure TKMTerrain.RemRoad(const aLoc: TKMPoint);
begin
  Land^[aLoc.Y,aLoc.X].TileOwner := -1;
  Land^[aLoc.Y,aLoc.X].TileOverlay := toNone;
  Land^[aLoc.Y,aLoc.X].FieldAge  := 0;
  UpdateFences(aLoc);
  UpdatePassability(KMRectGrowBottomRight(KMRect(aLoc)));

  //Roads don't affect wcWalk or wcFish
  UpdateWalkConnect([wcRoad], KMRect(aLoc), False);
end;

procedure TKMTerrain.SetDefTile(const aLoc: TKMPoint);
begin

  Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain := Land^[aLoc.Y, aLoc.X].DefTile;
  Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation := Land^[aLoc.Y, aLoc.X].DefRotation;
end;

procedure TKMTerrain.SetNewDefTile(const aLoc: TKMPoint);
begin
  with fTileSet[Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain] do
    if Vegetables or Grass or Corn or Wine then
      Exit;

  Land^[aLoc.Y, aLoc.X].DefTile := Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain;
  Land^[aLoc.Y, aLoc.X].DefRotation := Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation;
end;

procedure TKMTerrain.RemField(const aLoc: TKMPoint; aDoUpdatePass, aDoUpdateWalk, aDoUpdateFences: Boolean);
var
  updatePassRect: TKMRect;
  diagObjectChanged: Boolean;
begin
  RemField(aLoc, aDoUpdatePass, aDoUpdateWalk, updatePassRect, diagObjectChanged, aDoUpdateFences);
end;


procedure TKMTerrain.RemField(const aLoc: TKMPoint; aDoUpdatePass, aDoUpdateWalk: Boolean; out aUpdatePassRect: TKMRect;
  out aDiagObjectChanged: Boolean; aDoUpdateFences: Boolean);
var I : Integer;
  GT : TKMGrainType;
begin
  Land^[aLoc.Y,aLoc.X].TileOwner := -1;
  Land^[aLoc.Y,aLoc.X].TileOverlay := toNone;
  GT := Land^[aLoc.Y,aLoc.X].GrainType;
  Land^[aLoc.Y,aLoc.X].GrainType := gftNone;

  if fMapEditor then
  begin
    gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 0;
    gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWineTerrain := 0;
  end;
  aDiagObjectChanged := false;
  for I := 0 to gFieldGrains[GT].StagesCount - 1 do
    if Land^[aLoc.Y,aLoc.X].Obj = gFieldGrains[GT].Stage[I].Obj then
    begin
      Land^[aLoc.Y,aLoc.X].Obj := OBJ_NONE; //Remove corn/wine
      aDiagObjectChanged := True;
    end;
    
  Land^[aLoc.Y,aLoc.X].FieldAge := 0;

  if aDoUpdateFences then
    UpdateFences(aLoc);
    
  aUpdatePassRect := KMRectGrow(KMRect(aLoc),1);

  if aDoUpdatePass then
    UpdatePassability(aUpdatePassRect);

  if aDoUpdateWalk then
    //Update affected WalkConnect's
    UpdateWalkConnect([wcWalk,wcRoad,wcWork], aUpdatePassRect, aDiagObjectChanged); //Winefields object block diagonals
end;


procedure TKMTerrain.RemField(const aLoc: TKMPoint);
var
  diagObjectChanged: Boolean;
  R: TKMRect;
begin
  Land^[aLoc.Y,aLoc.X].TileOwner := -1;
  Land^[aLoc.Y,aLoc.X].TileOverlay := toNone;


  if fMapEditor then
  begin
    gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 0;
    gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWineTerrain := 0;
  end;

  if ObjectIsCornOrGrass(aLoc) or ObjectIsWine(aLoc) or ObjectIsVege(aLoc.X, aLoc.Y)  then
    Land^[aLoc.Y,aLoc.X].Obj := OBJ_NONE; //Remove corn/wine
  Land^[aLoc.Y,aLoc.X].FieldAge := 0;
  UpdateFences(aLoc);

  R := KMRectGrow(KMRect(aLoc), 1);
  UpdatePassability(R);

  diagObjectChanged := false;
  //Update affected WalkConnect's
  UpdateWalkConnect([wcWalk,wcRoad,wcWork], R, diagObjectChanged); //Winefields object block diagonals
end;


procedure TKMTerrain.RemoveLayers;
var
  I, K: Integer;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      Land^[I, K].LayersCnt := 0;
end;


procedure TKMTerrain.ClearPlayerLand(aPlayer: TKMHandID);
var
  I, K: Integer;
  P: TKMPoint;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      // On the game start TileOwner is not set for roads, be aware of that
      // Its set only in AfterMissionInit procedures
      if (Land^[I, K].TileOwner = aPlayer) then
      begin
        P.X := K;
        P.Y := I;

        if (Land^[I, K].Obj <> OBJ_NONE) then
        begin
          if TileIsCornField(P) and (GetCornStage(P) in [4,5]) then
            SetField(P, Land^[I, K].TileOwner, ftCorn, 3)  // For corn, when delete corn object reduce field stage to 3
          else
          if TileIsGrassField(P) and (GetGrassStage(P) > 0) then
            SetField(P, Land^[I, K].TileOwner, ftGrassLand, 0)  // For corn, when delete corn object reduce field stage to 3
          else
          if TileIsVegeField(P) and (GetVegeStage(P) > 0) then
            SetField(P, Land^[I, K].TileOwner, ftVegeField, 0)  // For corn, when delete corn object reduce field stage to 3
          else
          if TileIsWineField(P) then
            RemField(P)
          else
            SetObject(P, OBJ_NONE);
        end;

        if Land^[I, K].TileOverlay = toRoad then
          RemRoad(P);
        if TileIsCornField(P) or TileIsWineField(P) then
          RemField(P);
      end;

end;


procedure TKMTerrain.RemovePlayer(aPlayer: TKMHandID);
var
  I, K: Word;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
      if Land^[I, K].TileOwner > aPlayer then
        Land[I, K].TileOwner := Pred(Land^[I, K].TileOwner)
      else if Land^[I, K].TileOwner = aPlayer then
        Land^[I, K].TileOwner := -1;
end;


procedure TKMTerrain.SetField_Init(const aLoc: TKMPoint; aOwner: TKMHandID; aRemoveOverlay: Boolean = True);
begin
  Land^[aLoc.Y,aLoc.X].TileOwner   := aOwner;
  if aRemoveOverlay then
    Land^[aLoc.Y,aLoc.X].TileOverlay := toNone;


  Land^[aLoc.Y,aLoc.X].FieldAge    := 0;
end;


procedure TKMTerrain.SetField_Complete(const aLoc: TKMPoint; aFieldType: TKMFieldType);
begin

  UpdateFences(aLoc);
  UpdatePassability(KMRectGrow(KMRect(aLoc), 1));
  //Walk and Road because Grapes are blocking diagonal moves
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(aLoc)), (aFieldType = ftWine)); //Grape object blocks diagonal, others don't
end;


procedure TKMTerrain.SetRoad(const aLoc: TKMPoint; aOwner: TKMHandID; aRoadType : TKMRoadType);
var P : TKMPoint;
begin
  if not TileInMapCoords(aLoc) then
    Exit;
  SetField_Init(aLoc, aOwner);

  Land^[aLoc.Y,aLoc.X].TileOverlay := toRoad;
  Land^[aLoc.Y,aLoc.X].RoadType := aRoadType;

  SetField_Complete(aLoc, ftRoad);

  gScriptEvents.ProcRoadBuilt(aOwner, aLoc.X, aLoc.Y);

  if aLoc.Y > 1 then
    if TKMHouse(Land^[aLoc.Y - 1,aLoc.X].IsHouse).IsValid then
    if TKMHouse(Land^[aLoc.Y - 1,aLoc.X].IsHouse).PlaceRoad then
    begin
      P := TKMHouse(Land^[aLoc.Y - 1,aLoc.X].IsHouse).Entrance;
      if aLoc = KMPointBelow(P) then
        Land^[P.Y,P.X].RoadType := aRoadType;
    end;

end;

procedure TKMTerrain.SetPalisade(const aLoc: TKMPoint; aOwner: TKMHandID);
  function HasWall(A, B : Integer) :Boolean;
  begin
    Result := false;
      If not TileInMapCoords(aLoc.X + A, aLoc.Y + B) then
        Exit;
    Result := TileHasWall(aLoc.X + A, aLoc.Y + B);
  end;
begin
  SetField_Init(aLoc, aOwner);

   if HasWall(0, 1) and HasWall(1, 0) then
    SetObject(aLoc, 263)
   else if HasWall(0, aLoc.Y + 1) and HasWall(-1, 0) then
    SetObject(aLoc, 262)
   else if HasWall(0, -1) and HasWall(1, 0) then
    SetObject(aLoc, 260)
   else if HasWall(0, -1) and HasWall(-1, 0) then
    SetObject(aLoc, 261)
   else

   if HasWall(0, 1) then
    SetObject(aLoc, 256)
   else if HasWall(1, 0) then
    SetObject(aLoc, 257)
   else if HasWall(-1, 0) then
    SetObject(aLoc, 258)
   else if HasWall(0, -1) then
    SetObject(aLoc, 259);


  //gScriptEvents.ProcRoadBuilt(aOwner, aLoc.X, aLoc.Y);
end;

procedure TKMTerrain.RemPalisade(const aLoc: TKMPoint);
begin
  SetObject(aLoc, 255);

  Land^[aLoc.Y,aLoc.X].TileOwner := -1;

end;

procedure TKMTerrain.UpdatePalisadeAround(const aLoc: TKMPoint; aOwner: ShortInt);
var I, K, pX, pY : Integer;
  hadPalisade : array[0..2] of array[0..2] of Boolean;
begin
  for I := -1 to 1 do
    for K := -1 to 1 do
    begin
      hadPalisade[I + 1][K + 1] := false;
      pX := aLoc.X + I;
      pY := aLoc.Y + K;
      if TileInMapCoords(pX, pY) then
        if InRange(Land^[pY, pX].Obj, 256, 263) then
        begin
          hadPalisade[I + 1][K + 1] := true;
          RemPalisade(KMPoint(pX, pY));
        end;

    end;

  for I := -1 to 1 do
    for K := -1 to 1 do
    begin
      pX := aLoc.X + I;
      pY := aLoc.Y + K;
      if hadPalisade[I + 1][K + 1] then
        if CanAddField(pX, pY, ftPalisade) or InRange(Land^[pY, pX].Obj, 256, 263) then
        begin
          SetPalisade(KMPoint(pX, pY), aOwner);
          Land^[pY, pX].TileOwner := aOwner;
        end;

    end;

end;

procedure TKMTerrain.SetInitWine(const aLoc: TKMPoint; aOwner: TKMHandID);
begin
  //Land^[aLoc.Y, aLoc.X].DefTile := Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain;
  //Land^[aLoc.Y, aLoc.X].DefRotation := Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation;
  SetNewDefTile(aLoc);
  SetField_Init(aLoc, aOwner);
  Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain  := 55;
  Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation := 0;

  SetField_Complete(aLoc, ftInitWine);
end;


procedure TKMTerrain.IncDigState(const aLoc: TKMPoint; aRoadType : TKMRoadType = rtNone);
begin
  case aRoadType of
    rtNone, rtStone :
                      case Land^[aLoc.Y,aLoc.X].TileOverlay of
                        toDig1: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig2;
                        toDig2: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig3;
                        toDig3: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig4;
                        else Land^[aLoc.Y,aLoc.X].TileOverlay := toDig1;
                      end;
    rtWooden :
              case Land^[aLoc.Y,aLoc.X].TileOverlay of
                toDig1: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig2;
                toDig2: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig3Wooden;
                toDig3Wooden: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig4Wooden;
                else Land^[aLoc.Y,aLoc.X].TileOverlay := toDig1;
              end;
    rtClay :
              case Land^[aLoc.Y,aLoc.X].TileOverlay of
                toDig1: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig2;
                toDig2: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig3Clay;
                toDig3Clay: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig4Clay;
                else Land^[aLoc.Y,aLoc.X].TileOverlay := toDig1;
              end;
    rtExclusive :
              case Land^[aLoc.Y,aLoc.X].TileOverlay of
                toDig1: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig2;
                toDig2: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig3Exclusive;
                toDig3Exclusive: Land^[aLoc.Y,aLoc.X].TileOverlay := toDig4Exclusive;
                else Land^[aLoc.Y,aLoc.X].TileOverlay := toDig1;
              end;

  end;
end;


procedure TKMTerrain.ResetDigState(const aLoc: TKMPoint);
begin
  Land^[aLoc.Y,aLoc.X].TileOverlay:=toNone;
end;


// Finds a winefield ready to be picked
function TKMTerrain.FindWineField(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aProdThatch : Pointer; out aFieldPoint: TKMPointDir): Boolean;
var
  I: Integer;
  validTiles: TKMPointList;
  nearTiles, farTiles: TKMPointDirList;
  P: TKMPoint;
begin
  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

  nearTiles := TKMPointDirList.Create;
  farTiles := TKMPointDirList.Create;
  for I := 0 to validTiles.Count - 1 do
  begin
    P := validTiles[I];
    if not KMSamePoint(aAvoidLoc,P) then
      if TileIsWineField(P) then
        if Land^[P.Y,P.X].FieldAge >= CORN_AGE_MAX then
          if not TileIsLocked(P) then //Taken by another farmer
            if (aProdThatch = nil) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(P)) then
              if RouteCanBeMade(aLoc, P, tpWalk) then
              begin
                if KMLengthSqr(aLoc, P) <= Sqr(aRadius div 2) then
                  nearTiles.Add(KMPointDir(P, dirNA))
                else
                  farTiles.Add(KMPointDir(P, dirNA));
              end;
  end;

  //Prefer close tiles to reduce inefficiency with shared fields
  Result := nearTiles.GetRandom(aFieldPoint);
  if not Result then
    Result := farTiles.GetRandom(aFieldPoint);

  nearTiles.Free;
  farTiles.Free;
  validTiles.Free;
end;


procedure TKMTerrain.FindCornFieldLocs(const aLoc: TKMPoint; aRadius: Integer; aCornLocs: TKMPointList);
var
  I: Integer;
  P: TKMPoint;
  validTiles: TKMPointList;
begin
  validTiles := TKMPointList.Create;
  try
    fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

    for I := 0 to validTiles.Count - 1 do
    begin
      P := validTiles[I];
      if TileIsCornField(P) and RouteCanBeMade(aLoc, P, tpWalk) then
        aCornLocs.Add(P);
    end;
  finally
    validTiles.Free;
  end;
end;


procedure TKMTerrain.FindWineFieldLocs(const aLoc: TKMPoint; aRadius: Integer; aCornLocs: TKMPointList);
var
  I: Integer;
  P: TKMPoint;
  validTiles: TKMPointList;
begin
  validTiles := TKMPointList.Create;
  try
    fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

    for I := 0 to validTiles.Count - 1 do
    begin
      P := validTiles[I];
      if TileIsWineField(P) and RouteCanBeMade(aLoc, P, tpWalk) then
        aCornLocs.Add(P);
    end;
  finally
    validTiles.Free;
  end;
end;

// Finds a corn field
function TKMTerrain.FindCornField(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aPlantAct: TKMPlantAct; aProdThatch : Pointer;
                                  out aPlantActOut: TKMPlantAct; out aFieldPoint: TKMPointDir; aGrainType : TKMGrainFarmSet): Boolean;
var
  I: Integer;
  validTiles, nearTiles, farTiles: TKMPointList;
  P: TKMPoint;
  stage : Byte;
  hasGrass, hasVege, hasGrain : Boolean;

  procedure CheckGrassField(Loc : TKMPoint);
  begin
    if not TileIsGrassField(Loc) then
      Exit;

    if not KMSamePoint(aAvoidLoc,Loc) then
    begin
        stage := gFieldGrains[Land^[Loc.Y,Loc.X].GrainType].GetStage(Land^[P.Y,P.X].FieldAge);
        if stage = 254 then
          Exit;
        if    ((aPlantAct in [taAny, taPlant])
                and (Land^[Loc.Y,Loc.X].FieldAge = 0)
              )
        or    (hasGrass and (( stage >= CORN_AGE_MAX) or gFieldGrains[GetGrainType(Loc)].Stage[stage].CanBeCut) ) then
          if not TileIsLocked(Loc) then //Taken by another farmer
            if not (TObject(aProdThatch) is TKMHouseProdThatch) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(Loc)) then
            if RouteCanBeMade(aLoc, Loc, tpWalk) then
            begin
              if KMLength(Loc, aLoc) <= 3 then
                nearTiles.Add(P)
              else
                farTiles.Add(P);
            end;
    end;
  end;
  procedure CheckVegeField(Loc : TKMPoint);
  begin
    if not TileIsVegeField(Loc) then
      Exit;

    if not KMSamePoint(aAvoidLoc,Loc) then
    begin
        stage := gFieldGrains[Land^[Loc.Y,Loc.X].GrainType].GetStage(Land^[P.Y,P.X].FieldAge);
;        if stage = 254 then
          Exit;
        if    ((aPlantAct in [taAny, taPlant])
                and (Land^[Loc.Y,Loc.X].FieldAge = 0)
              )
          or  (hasVege and (stage >= CORN_AGE_MAX) ) then
          if not TileIsLocked(Loc) then //Taken by another farmer
            if not (TObject(aProdThatch) is TKMHouseProdThatch) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(Loc)) then
            if RouteCanBeMade(aLoc, Loc, tpWalk) then
            begin
              if KMLength(Loc, aLoc) <= 3 then
                nearTiles.Add(P)
              else
                farTiles.Add(P);
            end;
    end;
  end;

  procedure CheckGrainField(Loc : TKMPoint);
  begin
    if not TileIsCornField(Loc) then
      Exit;
    if not KMSamePoint(aAvoidLoc,Loc) then
      if((aPlantAct in [taAny, taPlant]) and (Land^[Loc.Y,Loc.X].FieldAge = 0))
        or (hasGrain and (Land^[Loc.Y,Loc.X].FieldAge >= CORN_AGE_MAX)) then
        if not TileIsLocked(Loc) then //Taken by another farmer
          if not (TObject(aProdThatch) is TKMHouseProdThatch) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(Loc)) then
          if RouteCanBeMade(aLoc, Loc, tpWalk) then
          begin
            if KMLength(Loc, aLoc) <= 3 then
              nearTiles.Add(P)
            else
              farTiles.Add(P);
          end;
  end;

begin
  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

  hasGrain := (aGrainType[0] <> gftNone) and (aPlantAct in [taCut, taAny]);
  hasGrass := (aGrainType[1] <> gftNone) and (aPlantAct in [taCut, taAny]);
  hasVege := (aGrainType[2] <> gftNone) and (aPlantAct in [taCut, taAny]);

  nearTiles := TKMPointList.Create;
  farTiles := TKMPointList.Create;

  for I := 0 to validTiles.Count - 1 do
  begin
    P := validTiles[i];
    if (aGrainType[0] <> gftNone) then
      CheckGrainField(P);
    if (aGrainType[1] <> gftNone) then
      CheckGrassField(P);
    if (aGrainType[2] <> gftNone) then
      CheckVegeField(P);
  end;
  //Prefer close tiles to reduce inefficiency with shared fields


  if (nearTiles.Count > 0) then
    Result := nearTiles.GetRandom(P)
  else
    Result := farTiles.GetRandom(P);

  //Result := nearTiles.GetClosest(P);

  //Result := nearTiles.GetRandom(P);
  //if not Result then
  //  Result := farTiles.GetClosest(P);

  aFieldPoint := KMPointDir(P, dirNA);
  nearTiles.Free;
  farTiles.Free;
  validTiles.Free;
  if not Result then
    aPlantActOut := taAny
  else
  begin
    P := aFieldPoint.Loc;
    stage := gFieldGrains[GetGrainType(P)].GetStage(Land^[P.Y,P.X].FieldAge);
    if stage = 0 then
      aPlantActOut := taPlant
    else
    if stage >= CORN_AGE_MAX then
      aPlantActOut := taCut
    else
    if gFieldGrains[GetGrainType(P)].Stage[stage].CanBeCut then
      aPlantActOut := taCut;
    {
    if Land^[aFieldPoint.Loc.Y,aFieldPoint.Loc.X].FieldAge = CORN_AGE_MAX then
      aPlantActOut := taCut
    else
    if TileIsGrassField(aFieldPoint.Loc) and (Land^[aFieldPoint.Loc.Y,aFieldPoint.Loc.X].FieldAge = CORN_AGE_3) then
      aPlantActOut := taCut
    else
    if TileIsGrassField(aFieldPoint.Loc) and InRange(Land^[aFieldPoint.Loc.Y,aFieldPoint.Loc.X].FieldAge, 0, CORN_AGE_1 - 1) then
      aPlantActOut := taPlant
    else
      aPlantActOut := taPlant;}
  end;
end;



procedure TKMTerrain.FindStoneLocs(const aLoc: TKMPoint; aRadius: Byte; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;  aProdThatch : Pointer;
                                   aStoneLocs: TKMPointList);
var
  I: Integer;
  validTiles: TKMPointList;
  P: TKMPoint;
begin
  validTiles := TKMPointList.Create;
  try
    fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

    for I := 0 to validTiles.Count - 1 do
    begin
      P := validTiles[I];
      if (P.Y >= 2) //Can't mine stone from top row of the map (don't call TileIsStone with Y=0)
        and not KMSamePoint(aAvoidLoc, P)
        and TileHasStone(P.X, P.Y - 1)
        and (aIgnoreWorkingUnits or not TileIsLocked(P)) //Already taken by another stonemason
        and ( (aProdThatch = nil) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(P)) ) //don't work in here if tile was already taken by someone else in production thatch
        and RouteCanBeMade(aLoc, P, tpWalk) then
        aStoneLocs.Add(P);
    end;
  finally
    validTiles.Free;
  end;
end;



// Find closest harvestable deposit of Stone
// Return walkable tile below Stone deposit
function TKMTerrain.FindStone(const aLoc: TKMPoint; aRadius: Byte; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                              out aStonePoint: TKMPointDir): Boolean;
var
  chosenTiles: TKMPointList;
  P: TKMPoint;
begin
  chosenTiles := TKMPointList.Create;
  try
    FindStoneLocs(aLoc, aRadius, aAvoidLoc, aIgnoreWorkingUnits, aProdThatch, chosenTiles);

    Result := chosenTiles.GetRandom(P);
    aStonePoint := KMPointDir(P, dirN);
  finally
    chosenTiles.Free;
  end;
end;

function TKMTerrain.FindOre(const aLoc: TKMPoint; aWare: TKMWareType; out aOrePoint: TKMPoint; isOnMineShaft : Boolean = false): Boolean;
var
  I: Integer;
  L: TKMPointListArray;
begin
  SetLength(L, ORE_DENSITY_MAX_TYPES);
  //Create separate list for each density, to be able to pick best one
  for I := 0 to Length(L) - 1 do
    L[I] := TKMPointList.Create;
    FindOrePoints(aLoc, aWare, L, isOnMineShaft);

  //Equation elements will be evalueated one by one until True is found
  Result := False;
  for I := ORE_DENSITY_MAX_TYPES - 1 downto 0 do
    if not Result then
      Result := L[I].GetRandom(aOrePoint)
    else
      Break;

  for I := 0 to Length(L) - 1 do
    L[I].Free;
end;

function TKMTerrain.FindClay(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                              out aOrePoint: TKMPoint): Boolean;
var
  L: TKMPointListArray;
  I : Integer;
begin
  SetLength(L, 5);
  for I := 0 to High(L) do
    L[I] := TKMPointList.Create;

  FindClayPoints(aLoc, aAvoidLoc, aIgnoreWorkingUnits, aProdThatch, L);
  Result := false;

  for I := High(L) downto 0 do
    if L[I].Count > 0 then
    begin
      Result := L[I].GetRandom(aOrePoint);
      Break;
    end;
    

  for I := 0 to High(L) do
    L[I].Free;
end;

function TKMTerrain.FindCollectors(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                              out aOrePoint: TKMPointDir): TKMWarePlan;
var
  L: TKMPointDirCenteredList;
  I : Integer;
begin
  L := TKMPointDirCenteredList.Create(aLoc);
  Result.SetCount(WARES_IN_OUT_COUNT, true);
  Result[0].C := 1;
  FindCollectorsPoints(aLoc, aAvoidLoc, aIgnoreWorkingUnits, L);
  if L.Count > 0 then
    Result[0].W := wtAll
  else
    Result[0].W := wtNone;


  If not L.GetWeightedRandom(aOrePoint) then
  begin
    L.Free;
    Exit;
  end;

  if not TileInMapCoords(aOrePoint.DirFaceLoc) then
  begin
    L.Free;
    Exit;
  end;

  I := 255;
  case aOrePoint.Dir of
    dirN : I := Land^[aOrePoint.Y - 1, aOrePoint.X].Obj;
    dirS : I := Land^[aOrePoint.Y + 1, aOrePoint.X].Obj;
    dirE : I := Land^[aOrePoint.Y, aOrePoint.X + 1].Obj;
    dirW : I := Land^[aOrePoint.Y, aOrePoint.X - 1].Obj;
  end;

  if I = 255 then
    Result[0].W := wtNone;

  if Result[0].W <> wtNone then
  begin
    if length(gMapElements[I].VWares) > 0 then
      Result[0].W := wtJewerly
    else
      Result[0].W :=  ObjectGetWare(I);
  end;

  if Result[0].W = wtNone then
  begin
    Result[0].W := GetWareOnGround(aOrePoint.DirFaceLoc);
    Result[0].C := Min(5, GetWareOnGroundCount(aOrePoint.DirFaceLoc));
  end;

  if Result[0].W = wtNone then
    if L.Count > 0 then
      Result[0].W := wtAll;

  L.Free;
end;

function TKMTerrain.GetMiningRect(aWare: TKMWareType): TKMRect;
begin
  case aWare of
    wtGoldOre:  Result := KMRect(7, 11, 6, 2);
    wtIronOre:  Result := KMRect(7, 11, 5, 2);
    wtCoal:     Result := KMRect(4,  5, 5, 2);
    wtBitinOre: Result := KMRect(9, 13, 8, 3);
    wtTile:     Result := KMRect(7,  8, 8, 5);
    wtJewerly:     Result := KMRect(7,  8, 8, 6);
  else
    Result := KMRECT_ZERO;
  end;
end;


procedure TKMTerrain.FindOrePointsByDistance(const aLoc: TKMPoint; aWare: TKMWareType; var aPoints: TKMPointListArray);
var
  I,K: Integer;
  miningRect: TKMRect;
begin
  Assert(Length(aPoints) = 3, 'Wrong length of Points array: ' + IntToStr(Length(aPoints)));

  if not (aWare in [wtIronOre, wtGoldOre, wtCoal, wtBitinOre, wtTile]) then
    raise ELocError.Create('Wrong resource as Ore', aLoc);

  miningRect := GetMiningRect(aWare);

  for I := Max(aLoc.Y - miningRect.Top, 1) to Min(aLoc.Y + miningRect.Bottom, fMapY - 1) do
    for K := Max(aLoc.X - miningRect.Left, 1) to Min(aLoc.X + miningRect.Right, fMapX - 1) do
    begin
      if ((aWare = wtIronOre)   and TileHasIron(K,I))
      or ((aWare = wtGoldOre) and TileHasGold(K,I))
      or ((aWare = wtBitinOre) and TileHasBitinIron(K,I))
      or ((aWare = wtTile) and TileHasClay(K,I))
      or ((aWare = wtCoal)    and TileHasCoal(K,I)) then
      begin
        //Poorest ore gets mined in range - 2
        if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2)
          and InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
            aPoints[0].Add(KMPoint(K, I))
        //Second poorest ore gets mined in range - 1
        else
        if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1)
          and InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
            aPoints[1].Add(KMPoint(K, I))
        else
          //Always mine second richest ore
          aPoints[2].Add(KMPoint(K, I));
      end;
    end;
end;

procedure TKMTerrain.FindClayPoints(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aProdThatch : Pointer;
                            aClayLocs: TKMPointListArray);
var
  I,K, C: Integer;
  miningRect: TKMRect;
begin
  //Assert(Length(aClayLocs) = ORE_DENSITY_MAX_TYPES, 'Wrong length of Points array: ' + IntToStr(Length(aClayLocs)));

  miningRect := GetMiningRect(wtTile);

  //Try to find Clay
  for I := Max(aLoc.Y - miningRect.Top, 1) to Min(aLoc.Y + miningRect.Bottom, fMapY - 1) do
    for K := Max(aLoc.X - miningRect.Left, 1) to Min(aLoc.X + miningRect.Right, fMapX - 1) do
      if not KMSamePoint(aAvoidLoc, KMPoint(I,K))
        and TileHasClay(K, I)
        and (aIgnoreWorkingUnits or not TileIsLocked(KMPoint(K, I)))
        and ( (aProdThatch = nil) or (not TKMHouseProdThatch(aProdThatch).IsPointTaken(KMPoint(K, I))) ) //don't work in here if tile was already taken by someone else in production thatch
        and RouteCanBeMade(aLoc, KMPoint(K, I), tpWalk) then
        begin
          C := TileIsClay(K, I);
          case C of
            0 : ;//do nothing
            1 : if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2)
                        and InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
                          aClayLocs[0].Add(KMPoint(K, I));
            2 : if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1)
                        and InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
                          aClayLocs[1].Add(KMPoint(K, I));
            3: aClayLocs[2].Add(KMPoint(K, I));
            4: aClayLocs[3].Add(KMPoint(K, I));
            else aClayLocs[4].Add(KMPoint(K, I));
          end;
          {
          case Land^[I, K].TileOverlay2 of
            toClay1 :
                      if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2)
                        and InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
                          aClayLocs.Add(KMPoint(K, I));
            toClay2 : if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1)
                        and InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
                          aClayLocs.Add(KMPoint(K, I));


            toClay3,
            toClay4,
            toClay5,
            toInfinityClay : aClayLocs.Add(KMPoint(K, I));
          end;}

        end;

end;

procedure TKMTerrain.FindCollectorsPoints(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                            aJewerlyLocs: TKMPointDirCenteredList);

var
  I,K: Integer;
  miningRect: TKMRect;
  weight : Single;
begin
  miningRect := GetMiningRect(wtJewerly);

  //Try to find Clay
  for I := Max(aLoc.Y - miningRect.Top, 2) to Min(aLoc.Y + miningRect.Bottom, fMapY - 2) do
    for K := Max(aLoc.X - miningRect.Left, 2) to Min(aLoc.X + miningRect.Right, fMapX - 2) do
        if not KMSamePoint(aAvoidLoc, KMPoint(K,I))
          //and TileHasClay(K, I)
          and TileHasCollectorsGoods(K, I)
          and (aIgnoreWorkingUnits or not TileIsLocked(KMPoint(K, I))) then
          begin
            if ObjectIsWare(Land^[I, K].Obj) then
              weight := 100
            else
              weight := gMapElements[Land^[I, K].Obj].ObjectPrice;

            if weight = 0 then
              weight := 10;

            if GetWareOnGround(KMPoint(K, I)) <> wtNone then
              Weight := 250;

            weight := weight / (KMLengthDiag(aLoc, KMPoint(K, I)) * 100);

            If RouteCanBeMade(aLoc, KMPoint(K - 1, I), tpWalk) then //left
              aJewerlyLocs.AddW(KMPointDir(K - 1, I, dirW), weight);

            If RouteCanBeMade(aLoc, KMPoint(K + 1, I), tpWalk) then //right
              aJewerlyLocs.AddW(KMPointDir(K + 1, I, dirE), weight);

            If RouteCanBeMade(aLoc, KMPoint(K, I - 1), tpWalk) then //Top
              aJewerlyLocs.AddW(KMPointDir(K, I - 1, dirS), weight);

            If RouteCanBeMade(aLoc, KMPoint(K, I + 1), tpWalk) then //Bottom
              aJewerlyLocs.AddW(KMPointDir(K, I + 1, dirN), weight);
          end;

end;

function TKMTerrain.DecCollectorsOre(aLoc: TKMPointDir; aCount : Byte): Boolean;
var I : Integer;
  W : TKMWareType;
begin
  Result := false;
  aLoc.Loc := aLoc.DirFaceLoc;

  if Land^[aLoc.Y,aLoc.X].TileOverlay2 = toInfinity then
    Exit(true);
  W := GetWareOnGround(aLoc.Loc);

  if W <> wtNone then
  begin
    Result := DecWareOnGorund(aLoc.Loc, aCount);
    Exit;
  end;
  
  I := Land^[aLoc.Loc.Y, aLoc.Loc.X].Obj;
  if I = 255 then
    Exit(false);
  W := ObjectGetWare(I);

  if W <> wtNone then
  begin
    if ObjectGetWare(I - 1) = W then
      SetObject(aLoc.Loc, I - 1)
    else
      SetObject(aLoc.Loc, OBJ_NONE);
    Result := true;
  end else
  if length(gMapElements[I].VWares) > 0 then
  begin

    if gMapElements[I].PrevTreeAgeObj <> 0 then
      SetObject(aLoc.Loc, gMapElements[I].PrevTreeAgeObj)
    else
      SetObject(aLoc.Loc, OBJ_NONE);

    Result := true;
  end;


end;


function TKMTerrain.FindHunter(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; out aOrePoint: TKMPointDir): Boolean;
var aList : TKMWeightedPointList;
begin
  aList := TKMWeightedPointList.Create;

  FindHunterPoints(aLoc, aAvoidLoc, aIgnoreWorkingUnits,aList);
  Result := aList.GetWeightedRandom(aOrePoint.Loc);
  aOrePoint.Dir := KaMRandomDir('TKMTerrain.FindHunter');
  aList.Free;
end;

procedure TKMTerrain.FindHunterPoints(const aLoc: TKMPoint; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean; aPoints: TKMWeightedPointList);

var
  I,K: Integer;
  miningRect: TKMRect;
  weight : Single;
begin
  miningRect := GetMiningRect(wtJewerly);

  //Try to find Clay
  for I := Max(aLoc.Y - miningRect.Top, 2) to Min(aLoc.Y + miningRect.Bottom, fMapY - 2) do
    for K := Max(aLoc.X - miningRect.Left, 2) to Min(aLoc.X + miningRect.Right, fMapX - 2) do
        if not KMSamePoint(aAvoidLoc, KMPoint(K,I))
          and (CanSetTrap(KMPoint(K, I)) or HasMeat(KMPoint(K, I)))
          and (aIgnoreWorkingUnits or not TileIsLocked(KMPoint(K, I))) then
          begin
            if HasMeat(KMPoint(K, I)) then
              weight := 100
            else
              weight := 1;

            aPoints.Add(KMPoint(K, I), weight);
          end;
end;

function TKMTerrain.CanSetTrap(P : TKMPoint) : Boolean;
var I, K : Integer;
begin
  Result := (CheckPassability(P, tpWolf) or CheckPassability(P, tpPolarBear))
            and (Land^[P.Y, P.X].Obj = OBJ_NONE)
            and ((Land^[P.Y, P.X].Ware.W = 0) or (Land^[P.Y, P.X].Ware.C = 0));
  if not Result then
    Exit;
  // set traps in distance of 1 tile
  for I := -1 to 1 do
    for K := -1 to 1 do
    if not ((I = 0) and (K = 0))  then
      If ArrayContains(Land^[P.Y + I, P.X + K].Obj, [539, 540, 541, 542, 543, 544, 545, 546, 547, 548]) then
        Exit(false);

end;

function TKMTerrain.HasMeat(P : TKMPoint) : Boolean;
begin
  Result := ArrayContains(Land^[P.Y, P.X].Obj, [540, 541, 542, 543, 544, 545, 546, 547, 548])
end;

function TKMTerrain.SetHunterTraps(const aLoc: TKMPoint) : Word;
begin
  Result := OBJ_NONE;
  //collect meat
  if HasMeat(aloc) then
  begin
    Result := Land^[aLoc.Y, aLoc.X].Obj;
    SetObject(aLoc, OBJ_NONE);
    Exit;
  end;
  //set traps
  //if CanSetTrap(aLoc) then
    SetObject(aLoc, 539);
end;

function TKMTerrain.FindWareForBoat(const aLoc: TKMPoint; const aRadius: Byte; aFindWares, aFindFish : Boolean): TKMWarePlanSingle;
var aList : TKMPointArray;

  procedure CheckTile(const aX, aY : Integer);
  begin
    if not TileInMapCoords(aX, aY) then
      Exit;

    if (Land^[aY, aX].Ware.C > 0) and (TKMWareType(Land^[aY, aX].Ware.W) <> wtNone) then
    begin
      aList.Add(aX, aY);
      Exit;
    end;
    if not (KM_ResMapElements.ObjectGetWare(Land^[aY, aX].Obj) in [wtNone, wtTile]) then
    begin
      aList.Add(aX, aY);
      Exit;
    end;
  end;

var P : TKMPoint;
  X, Y : Integer;
  I, C : Integer;
begin
  Result.W := wtNone;
  Result.C := 1;

  if not (aFindWares or aFindFish) then
    Exit;

  if aFindWares then
    if IsTileNearLand(aLoc) then
      for X := aLoc.X - aRadius to aLoc.X + aRadius do
      for Y := aLoc.Y - aRadius to aLoc.Y + aRadius do
        CheckTile(X, Y);

  if length(aList) = 0 then
  begin
    if aFindFish then
    begin
      Result.W := wtFish;
      Result.C := CatchFish(KMPointDir(aLoc, dirN), KamRandom(10, 'TKMTerrain.FindWareForBoat 2') + 1);
    end;
    Exit;
  end;

  P := aList[KaMRandom(length(aList), 'TKMTerrain.FindWareForBoat 1')];


  C := KamRandom(5, 'TKMTerrain.FindWareForBoat 2') + 1;
  Result.C := 0;
  //if anything found, decrease it's deposits
  if (Land^[P.Y, P.X].Ware.C > 0) and (TKMWareType(Land^[P.Y, P.X].Ware.W) <> wtNone) then
  begin
    for I := 1 to C do
      if Land^[P.Y, P.X].Ware.C > 0 then
      begin
        Inc(Result.C);
        Land^[P.Y, P.X].Ware.C := Land^[P.Y, P.X].Ware.C - 1;
      end;

    Result.W := TKMWareType(Land^[P.Y, P.X].Ware.W);
    Exit;
  end;
  if not (ObjectGetWare(Land^[P.Y, P.X].Obj) in [wtNone, wtTile]) then
  begin
    Result.W := ObjectGetWare(Land^[P.Y, P.X].Obj);


    for I := 1 to C do
     if KM_ResMapElements.ObjectGetWare(Land^[P.Y, P.X].Obj) = Result.W then
      begin
        Inc(Result.C);
        if KM_ResMapElements.ObjectGetWare(Land^[P.Y, P.X].Obj - 1) = Result.W then
          SetObject(P, Land^[P.Y, P.X].Obj - 1)
        else
        begin
          SetObject(P,OBJ_NONE);
          Break;
        end;

      end;
  end;

  if Result.W = wtTile then
    Result.C := Result.C div 2;


end;

function TKMTerrain.FindVWareForBoat(const aLoc: TKMPoint; const aRadius: Byte): Word;
var aList : TKMPointArray;

  procedure CheckTile(const aX, aY : Integer);
  begin
    if not TileInMapCoords(aX, aY) then
      Exit;
    if InRange(Land^[aY, aX].Obj, 459, 464) or InRange(Land^[aY, aX].Obj, 477, 491) then
      aList.Add(aX, aY);

  end;

var P : TKMPoint;
  X, Y : Integer;
begin
  Result := OBJ_NONE;

  if IsTileNearLand(aLoc) then
    for X := aLoc.X - aRadius to aLoc.X + aRadius do
    for Y := aLoc.Y - aRadius to aLoc.Y + aRadius do
      CheckTile(X, Y);

  if length(aList) = 0 then
    Exit;

  P := aList[KaMRandom(length(aList), 'TKMTerrain.FindVWareForBoat 1')];
  Result := Land^[P.Y, P.X].Obj;

  if length(gMapElements[Result].VWares) > 0 then
  begin
    if gMapElements[Result].PrevTreeAgeObj <> 0 then
      SetObject(P, gMapElements[Result].PrevTreeAgeObj)
    else
      SetObject(P, OBJ_NONE);
  end;
end;

function TKMTerrain.DecWareOnGorund(const aLoc: TKMPoint; aCount : Byte = 1): Boolean;
begin
  Result := false;
  //if anything found, decrease it's deposits
  if (Land^[aLoc.Y, aLoc.X].Ware.C > 0) and (TKMWareType(Land^[aLoc.Y, aLoc.X].Ware.W) <> wtNone) then
  begin
    if Land^[aLoc.Y, aLoc.X].Ware.C > 0 then
    begin
      Land^[aLoc.Y, aLoc.X].Ware.C := Max(Land^[aLoc.Y, aLoc.X].Ware.C - aCount, 0);
      Result := true;
    end;
  end;

end;


//Given aLoc the function return location of richest ore within predefined bounds
procedure TKMTerrain.FindOrePoints(const aLoc: TKMPoint; aWare: TKMWareType; var aPoints: TKMPointListArray; isOnMineShaft : Boolean = false);
var
  I,K: Integer;
  miningRect: TKMRect;
  //R1,R2,R3,R3_2,R4,R5: Integer; //Ore densities  {OLD}
  OD1, OD2, OD3, OD4, OD5: array of Integer; //Ore densities arrays
begin
  if not (aWare in [wtIronOre, wtGoldOre, wtCoal, wtBitinOre, wtTile]) then
    raise ELocError.Create('Wrong resource as Ore', aLoc);

  Assert(Length(aPoints) = ORE_DENSITY_MAX_TYPES, 'Wrong length of Points array: ' + IntToStr(Length(aPoints)));

  miningRect := GetMiningRect(aWare);



  //Try to find Clay
  if (aWare = wtTile) then
  begin
    for I := Max(aLoc.Y - miningRect.Top, 1) to Min(aLoc.Y + miningRect.Bottom, fMapY - 1) do
      for K := Max(aLoc.X - miningRect.Left, 1) to Min(aLoc.X + miningRect.Right, fMapX - 1) do
        begin
          case Land^[I, K].TileOverlay2 of
            toClay1 :
                      if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2)
                        and InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
                          aPoints[0].Add(KMPoint(K, I));
            toClay2 : if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1)
                        and InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
                          aPoints[1].Add(KMPoint(K, I));
            toClay3 : aPoints[2].Add(KMPoint(K, I));
            toClay4 : aPoints[3].Add(KMPoint(K, I));
            toInfinityClay,
            toClay5 : aPoints[4].Add(KMPoint(K, I));
          end;
        end;
    Exit;
  end;

  //Coal mine from overlay first
  if (aWare = wtCoal) then
  begin
    for I := Max(aLoc.Y - miningRect.Top, 1) to Min(aLoc.Y + miningRect.Bottom, fMapY - 1) do
      for K := Max(aLoc.X - miningRect.Left, 1) to Min(aLoc.X + miningRect.Right, fMapX - 1) do
      begin
        case Land^[I, K].TileOverlay2 of
          toCoal1 : if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2) then
                      if InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
                        aPoints[0].Add(KMPoint(K, I));
          toCoal2 : if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1) then
                      if InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
                        aPoints[1].Add(KMPoint(K, I));
          toCoal3 : aPoints[2].Add(KMPoint(K, I));
          toCoal4 : aPoints[3].Add(KMPoint(K, I));

          toInfinityCoal,
          toCoal5 : aPoints[4].Add(KMPoint(K, I));
          toCoal : aPoints[0].Add(KMPoint(K, I));
        end;
      end;

    //if found something then don't look for normal coal tile
    for I := Low(aPoints) to High(aPoints) do
      if aPoints[I].Count > 0 then
        Exit;
  end;


  case aWare of
    wtGoldOre: begin
                OD1 := [144, 325, 326, 327, 343, 344, 345];
                OD2 := [145];
                OD3 := [146];
                OD4 := [147];
                OD5 := [307];
               end;

    wtIronOre: begin
                OD1 := [148, 334, 335, 336, 325, 326, 327, 328, 329, 330, 337, 338, 339];
                OD2 := [149];
                OD3 := [150, 259];
                OD4 := [151];
                OD5 := [260];
                end;
    wtCoal:    begin
                OD1 := [152];
                OD2 := [153];
                OD3 := [154];
                OD4 := [155];
                OD5 := [263];
               end;
    wtBitinOre:    begin
                OD1 := [609, 613];
                OD2 := [610, 614];
                OD3 := [611, 615];
                OD4 := [612, 616];
                OD5 := [-1];
               end;
    else       begin
                OD1 := [-1];
                OD2 := [-1];
                OD3 := [-1];
                OD4 := [-1];
                OD5 := [-1];
               end;
  end;

  for I := Max(aLoc.Y - miningRect.Top, 1) to Min(aLoc.Y + miningRect.Bottom, fMapY - 1) do
    for K := Max(aLoc.X - miningRect.Left, 1) to Min(aLoc.X + miningRect.Right, fMapX - 1) do
    begin
      if ArrayContains(Land^[I, K].BaseLayer.Terrain, OD1) then
      begin
        //Poorest ore gets mined in range - 2
        if InRange(I - aLoc.Y, - miningRect.Top + 2, miningRect.Bottom - 2) then
          if InRange(K - aLoc.X, - miningRect.Left + 2, miningRect.Right - 2) then
            aPoints[0].Add(KMPoint(K, I));
      end
      else if ArrayContains(Land^[I, K].BaseLayer.Terrain, OD2) then
      begin
        //Second poorest ore gets mined in range - 1
        if InRange(I - aLoc.Y, - miningRect.Top + 1, miningRect.Bottom - 1) then
          if InRange(K - aLoc.X, - miningRect.Left + 1, miningRect.Right - 1) then
            aPoints[1].Add(KMPoint(K, I));
      end
      else if ArrayContains(Land^[I, K].BaseLayer.Terrain, OD3) then
        //Always mine second richest ore
        aPoints[2].Add(KMPoint(K, I))
      else if ArrayContains(Land^[I, K].BaseLayer.Terrain, OD4) then
        // Always mine richest ore
        aPoints[3].Add(KMPoint(K, I))
      else if ArrayContains(Land^[I, K].BaseLayer.Terrain, OD5) then
        // Always mine the most richest ore
        aPoints[4].Add(KMPoint(K, I));

      if isOnMineShaft then
        if (Land^[I, K].Ware.C2 > 0)
        and ( ((aWare = wtGoldOre) and (Land^[I, K].TileOverlay2 = toGold))
              or ((aWare = wtIronOre) and (Land^[I, K].TileOverlay2 = toIron))
              or ((aWare = wtBitinOre) and (Land^[I, K].TileOverlay2 = toBitin))
              )
        then
          aPoints[0].Add(KMPoint(K, I));


    end;

end;


function TKMTerrain.ChooseCuttingDirection(const aLoc, aTree: TKMPoint; out aCuttingPoint: TKMPointDir): Boolean;
var
  I, K, bestSlope, slope: Integer;
begin
  bestSlope := MaxInt;
  Result := False; //It is already tested that we can walk to the tree, but double-check

  for I := -1 to 0 do
    for K := -1 to 0 do
      if RouteCanBeMade(aLoc, KMPoint(aTree.X+K, aTree.Y+I), tpWalk) then
      begin
        slope := Round(HeightAt(aTree.X+K-0.5, aTree.Y+I-0.5) * CELL_HEIGHT_DIV) - Land^[aTree.Y, aTree.X].Height;
        //Cutting trees which are higher than us from the front looks visually poor, (axe hits ground) so avoid it where possible
        if (I = 0) and (slope < 0) then
          slope := slope - HEIGHT_MAX; //Make it worse but not worse than initial BestSlope
        if Abs(slope) < bestSlope then
        begin
          aCuttingPoint := KMPointDir(aTree.X+K, aTree.Y+I, KMGetVertexDir(K, I));
          bestSlope := Abs(slope);
          Result := True;
        end;
      end;
end;


function TKMTerrain.CanFindTree(const aLoc: TKMPoint; aRadius: Word; aOnlyAgeFull: Boolean = False): Boolean;
var
  validTiles: TKMPointList;
  I: Integer;
  T: TKMPoint;
  cuttingPoint: TKMPointDir;
begin
  Result := False;
  //Scan terrain and add all trees/spots into lists
  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);
  for I := 0 to validTiles.Count - 1 do
  begin
     //Store in temp variable for speed
    T := validTiles[I];

    if (KMLengthDiag(aLoc, T) <= aRadius)
      // Only full age
      and ( (aOnlyAgeFull and ObjectIsChopableTree(T, caAgeFull))
      // Any age tree will do
            or (not aOnlyAgeFull and (
              ObjectIsChopableTree(T, caAge1) or ObjectIsChopableTree(T, caAge2) or
              ObjectIsChopableTree(T, caAge3) or ObjectIsChopableTree(T, caAgeFull) )
            )
          )
      and RouteCanBeMadeToVertex(aLoc, T, tpWalk)
      and ChooseCuttingDirection(aLoc, T, cuttingPoint) then
    begin
      Result := True;
      Break;
    end;
  end;
  validTiles.Free;
end;


//Return location of a Tree or a place to plant a tree depending on TreeAct
//taChop - Woodcutter wants to get a Tree because he went from home with an axe
//        (maybe his first target was already chopped down, so he either needs a tree or will go home)
//taPlant - Woodcutter specifically wants to get an empty place to plant a Tree
//taAny - Anything will do since Woodcutter is querying from home
//Result indicates if desired TreeAct place was found successfully
procedure TKMTerrain.FindTree(const aLoc: TKMPoint; aRadius: Word; const aAvoidLoc: TKMPoint; aPlantAct: TKMPlantAct;
                              aTrees: TKMPointDirCenteredList; aBestToPlant, aSecondBestToPlant: TKMPointCenteredList);
var
  validTiles: TKMPointList;
  I: Integer;
  T: TKMPoint;
  cuttingPoint: TKMPointDir;
begin
  //Why do we use 3 lists instead of one like Corn does?
  //Because we should always prefer stumps over empty places
  //even if there's only 1 stump - we choose it

  //Scan terrain and add all trees/spots into lists
  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);
  for I := 0 to validTiles.Count - 1 do
  begin
     //Store in temp variable for speed
    T := validTiles[I];

    if (KMLengthDiag(aLoc, T) <= aRadius)
      and not KMSamePoint(aAvoidLoc, T) then
    begin

      //Grownup tree
      if (aPlantAct in [taCut, taAny])
        and ObjectIsChopableTree(T, caAgeFull)
        //Woodcutter could be standing on any tile surrounding this tree
        and CanCutTreeAtVertex(aLoc, T)
        and ChooseCuttingDirection(aLoc, T, cuttingPoint) then
        aTrees.Add(cuttingPoint); //Tree

      if (aPlantAct in [taPlant, taAny])
        and TileGoodToPlantTree(T.X, T.Y)
        and RouteCanBeMade(aLoc, T, tpWalk)
        and not TileIsLocked(T) then //Taken by another woodcutter
      begin
        if ObjectIsChopableTree(T, caAgeStump) then
          aBestToPlant.Add(T) //Prefer to dig out and plant on stumps to avoid cluttering whole area with em
        else
          aSecondBestToPlant.Add(T); //Empty space and other objects that can be dug out (e.g. mushrooms) if no other options available
      end;
    end;
  end;
  validTiles.Free;
end;


procedure TKMTerrain.FindPossibleTreePoints(const aLoc: TKMPoint; aRadius: Word; aTiles: TKMPointList);
var
  validTiles: TKMPointList;
  I: Integer;
  T: TKMPoint;
  cuttingPoint: TKMPointDir;
begin
  validTiles := TKMPointList.Create;
  try
    //Scan terrain and add all trees/spots into lists
    fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);
    for I := 0 to validTiles.Count - 1 do
    begin
       //Store in temp variable for speed
      T := validTiles[I];

      if (KMLengthDiag(aLoc, T) <= aRadius)
        and RouteCanBeMadeToVertex(aLoc, T, tpWalk)
        and ChooseCuttingDirection(aLoc, T, cuttingPoint)
        and (FindBestClimatType(T) <> tcNone) then // Check if tile is ok to plant a tree there, according to vertex terrainKind
        aTiles.Add(T);
    end;
  finally
    validTiles.Free;
  end;
end;


procedure TKMTerrain.FindFishWaterLocs(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                                       aChosenTiles: TKMPointDirList);
var
  I, J, K: Integer;
  P: TKMPoint;
  validTiles: TKMPointList;
begin
  validTiles := TKMPointList.Create;
  try
    fFinder.GetTilesWithinDistance(aLoc, aRadius, tpWalk, validTiles);

    for I := 0 to validTiles.Count - 1 do
    begin
      P := validTiles[I];
      //Check that this tile is valid
      if (aIgnoreWorkingUnits or not TileIsLocked(P)) //Taken by another fisherman
        and RouteCanBeMade(aLoc, P, tpWalk)
        and not KMSamePoint(aAvoidLoc, P) then
        //Now find a tile around this one that is water
        for J := -1 to 1 do
          for K := -1 to 1 do
            if ((K <> 0) or (J <> 0))
              and TileInMapCoords(P.X+J, P.Y+K)
              and TileIsWater(P.X+J, P.Y+K)
              and WaterHasFish(KMPoint(P.X+J, P.Y+K)) then //Limit to only tiles which are water and have fish
              aChosenTiles.Add(KMPointDir(P, KMGetDirection(J, K)));
    end;
  finally
    validTiles.Free;
  end;
end;


{Find seaside}
{Return walkable tile nearby}
function TKMTerrain.FindFishWater(const aLoc: TKMPoint; aRadius: Integer; const aAvoidLoc: TKMPoint; aIgnoreWorkingUnits: Boolean;
                                  out aFishPoint: TKMPointDir): Boolean;
var
  chosenTiles: TKMPointDirList;
begin
  chosenTiles := TKMPointDirList.Create;
  try
    FindFishWaterLocs(aLoc, aRadius, aAvoidLoc, aIgnoreWorkingUnits, chosenTiles);

    Result := chosenTiles.GetRandom(aFishPoint);
  finally
    chosenTiles.Free;
  end;
end;


function TKMTerrain.CanFindFishingWater(const aLoc: TKMPoint; aRadius: Integer): Boolean;
var
  I, K: Integer;
begin
  Result := False;
  for I := max(aLoc.Y - aRadius, 1) to Min(aLoc.Y + aRadius, fMapY-1) do
    for K := max(aLoc.X - aRadius, 1) to Min(aLoc.X + aRadius, fMapX-1) do
      if (KMLengthDiag(aLoc, KMPoint(K,I)) <= aRadius)
        and TileIsWater(K,I) then
        Exit(True);
end;


function TKMTerrain.FindBestClimatType(const aLoc: TKMPoint): TKMTerrainClimat;
const
  // Dependancy found empirically
  TERKIND_TO_TREE_TYPE: array[TKMTerrainKind] of TKMTerrainClimat = (
    tcNone,             //    tkCustom,
    tcWarm1,            //    tkGrass,
    tcWarm1,            //    tkMoss,
    tcWet1,            //    tkPaleGrass,
    tcDry2,             //    tkCoastSand,
    tcWarm2,            //    tkGrassSand1,
    tcDry1,             //    tkGrassSand2,
    tcDry1,             //    tkGrassSand3,
    tcDry2,             //    tkSand,       //8
    tcWet2,             //    tkGrassDirt,
    tcNeutral,          //    tkDirt,       //10
    tcNone,          //    tkBarrenLand,       //10
    tcNeutral,          //    tkCobbleStone,
    tcWet2,             //    tkGrassyWater,//12
    tcWet1,             //    tkSwamp,      //13
    tcCold2,            //    tkIce,        //14
    tcCold1,            //    tkSnowOnGrass,
    tcCold1,            //    tkSnowOnDirt,
    tcCold2,            //    tkSnow,
    tcCold2,            //    tkDeepSnow,
    tcNone,           //    tkStone,
    tcNone,           //    tkGoldMount,
    tcNone,           //    tkIronMount,  //21
    tcNone,           //    tkAbyss,
    tcNeutral,         //    tkGravel,
    tcNone,           //    tkCoal,
    tcNone,           //    tkGold,
    tcNone,           //    tkIron,
    tcWet2,           //    tkWater,
    tcWet2,           //    tkFastWater,
    tcNone            //    tkLava);
  );
var
  I, K: Integer;
  treeType: TKMTerrainClimat;
  verticeCornerTKinds: TKMTerrainKindCorners;
begin
  // Find tree type to plant by vertice corner terrain kinds
  Result := tcNone;
  if not TileInMapCoords(aLoc) then
    Exit;

  GetVerticeTerKinds(aLoc, verticeCornerTKinds);
  // Compare corner terKinds and find if there are at least 2 of the same tree type
  for I := 0 to 3 do
    for K := I + 1 to 3 do
    begin
      treeType := TERKIND_TO_TREE_TYPE[verticeCornerTKinds[I]];
      if    (treeType <> tcNone)
        and (treeType = TERKIND_TO_TREE_TYPE[verticeCornerTKinds[K]]) then //Pair found - we can choose this tree type
        Exit(treeType);
    end;
end;

function TKMTerrain.FindBestTreeClimatType(const aLoc: TKMPoint): TKMTerrainClimat;
const
  // Dependancy found empirically
  TERKIND_TO_TREE_TYPE: array[TKMTerrainKind] of TKMTerrainClimat = (
    tcNone,             //    tkCustom,
    tcWarm1,            //    tkGrass,
    tcWarm1,            //    tkMoss,
    tcWet1,            //    tkPaleGrass,
    tcDry2,             //    tkCoastSand,
    tcWarm2,            //    tkGrassSand1,
    tcDry1,             //    tkGrassSand2,
    tcDry1,             //    tkGrassSand3,
    tcDry2,             //    tkSand,       //8
    tcWet2,             //    tkGrassDirt,
    tcNeutral,          //    tkDirt,       //10
    tcNone,          //    tkBarrenLand,
    tcNone,          //    tkCobbleStone,
    tcNone,             //    tkGrassyWater,//12
    tcNone,             //    tkSwamp,      //13
    tcCold2,            //    tkIce,        //14
    tcCold1,            //    tkSnowOnGrass,
    tcCold1,            //    tkSnowOnDirt,
    tcCold2,            //    tkSnow,
    tcCold2,            //    tkDeepSnow,
    tcNone,           //    tkStone,
    tcNone,           //    tkGoldMount,
    tcNone,           //    tkIronMount,  //21
    tcNone,           //    tkAbyss,
    tcNeutral,         //    tkGravel,
    tcNone,           //    tkCoal,
    tcNone,           //    tkGold,
    tcNone,           //    tkIron,
    tcNone,           //    tkWater,
    tcNone,           //    tkFastWater,
    tcNone            //    tkLava);
  );
var
  I, K: Integer;
  treeType: TKMTerrainClimat;
  verticeCornerTKinds: TKMTerrainKindCorners;
begin
  // Find tree type to plant by vertice corner terrain kinds
  Result := tcNone;
  GetVerticeTerKinds(aLoc, verticeCornerTKinds);
  // Compare corner terKinds and find if there are at least 2 of the same tree type
  for I := 0 to 3 do
    for K := I + 1 to 3 do
    begin
      treeType := TERKIND_TO_TREE_TYPE[verticeCornerTKinds[I]];
      if    (treeType <> tcNone)
        and (treeType = TERKIND_TO_TREE_TYPE[verticeCornerTKinds[K]]) then //Pair found - we can choose this tree type
        Exit(treeType);
    end;
end;



function TKMTerrain.ChooseTreeToPlant(const aLoc: TKMPoint): Integer;
begin
  //Result := ChooseTreeToPlace(aLoc, caAge1, True); // Default plant age is caAge1
  Result := 390;
end;


function TKMTerrain.ChooseTreeToPlace(const aLoc: TKMPoint; aTreeAge: TKMChopableAge; aAlwaysPlaceTree: Boolean): Integer;
var
  bestTreeType: TKMTerrainClimat;
begin
  //Result := OBJ_NONE;
  //This function randomly chooses a tree object based on the terrain type. Values matched to KaM, using all soil tiles.
  {case Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain of
    0..3,5,6,8,9,11,13,14,18,19,56,57,66..69,72..74,84..86,93..98,180,188: bestTreeType := ttOnGrass;
    26..28,75..80,182,190:                                                 bestTreeType := ttOnYellowGrass;
    16,17,20,21,34..39,47,49,58,64,65,87..89,183,191,220,247:              bestTreeType := ttOnDirt;
    else
      bestTreeType := FindBestTreeType(aLoc);
  end;}
  bestTreeType := FindBestClimatType(aLoc);
  //Result := 88;
  Result := RandomFromArr(gTreeTypeID[bestTreeType]);
  {case bestTreeType of
    ttNone:           if aAlwaysPlaceTree then
                        Result := CHOPABLE_TREES[1 + KaMRandom(Length(CHOPABLE_TREES), 'TKMTerrain.ChooseTreeToPlant 4'), aTreeAge]; //If it isn't one of those soil types then choose a random tree
    ttOnGrass:        Result := CHOPABLE_TREES[1 + KaMRandom(7, 'TKMTerrain.ChooseTreeToPlant'), aTreeAge]; //Grass (oaks, etc.)
    ttOnYellowGrass:  Result := CHOPABLE_TREES[7 + KaMRandom(2, 'TKMTerrain.ChooseTreeToPlant 2'), aTreeAge]; //Yellow dirt
    ttOnDirt:         Result := CHOPABLE_TREES[9 + KaMRandom(5, 'TKMTerrain.ChooseTreeToPlant 3'), aTreeAge]; //Brown dirt (pine trees)
  end;}
end;

function TKMTerrain.CanReachTileInDistance(const aLoc1: TKMPoint; const aLoc2: TKMPoint; aPassability: TKMTerrainPassability; aMaxDistance: Integer = 10): Boolean;
var tmpLocs : array[1..255, 1..255 ] of Byte;

  procedure ResetTMP;
  var I, K : Integer;
  begin
    for I := 1 to 255 do
      for K := 1 to 255 do
        tmpLocs[I, K] := 255;
  end;

  procedure Visit(aX, aY, aDistance: Integer);
  begin
    if not TileInMapCoords(aX, aY) then  Exit;
    if aDistance >= tmpLocs[aY, aX] then Exit;

    if not CheckPassability(aX, aY, aPassability) then Exit;

    if Result then Exit;

    tmpLocs[aY, aX] := aDistance;

    if KMPoint(aX, aY) = aLoc2 then
    begin
      Result := true;
      Exit;
    end;

    If aDistance + 1 <= aMaxDistance then
    begin
      if aX - 1 >= 1 then  Visit(aX - 1, aY, aDistance + 1);
      if aX + 1 <= fMapX then  Visit(aX + 1, aY, aDistance + 1);
      if aY - 1 >= 1 then  Visit(aX, aY - 1, aDistance + 1);
      if aY + 1 <= fMapY then  Visit(aX, aY + 1, aDistance + 1);
    end;



  end;

begin
  Result := false;
  ResetTMP;
  Visit(aLoc1.X, aLoc1.Y, 0);
end;

function TKMTerrain.FogOfWarTileList(const aLoc1: TKMPoint; out aPointList: TKMPointList; aFogOfWar : TBoolean2Array; aMaxDistance: Integer = 10): Boolean;
var tmpLocs : array[1..255, 1..255] of Byte;

  procedure ResetTMP;
  var I, K : Integer;
  begin
    for I := 1 to 255 do
      for K := 1 to 255 do
        tmpLocs[I, K] := 255;
  end;

  procedure Visit(aX, aY, aDistance: Integer);
  begin
    if not TileInMapCoords(aX, aY) then  Exit;

    if aDistance >= tmpLocs[aY, aX] then Exit;

    //if not (CheckPassability(aX, aY, tpWalk) or CheckPassability(aX + 1, aY + 1, tpWalk) or CheckPassability(aX + 1, aY, tpWalk) or CheckPassability(aX, aY + 1, tpWalk)) then Exit;


    if Land^[aY,aX].IsHidden then
        if not (
        (InRange(aX, 1, fMapX - 2 ) and aFogOfWar[aY, aX - 1] and aFogOfWar[aY, aX + 1])
        or (InRange(aY, 1, fMapY - 2 ) and aFogOfWar[aY - 1, aX] and aFogOfWar[aY + 1, aX])
        ) then
          Exit;

    if not KMPointInCircle(KMPoint(aX, aY), aLoc1, aMaxDistance) then
      Exit;

    tmpLocs[aY, aX] := aDistance;
    aPointList.AddUnique(KMPoint(aX, aY));

    if aDistance + 1 <= aMaxDistance + 10 then
    begin
      if aX - 1 >= 1 then  Visit(aX - 1, aY, aDistance + 1);
      if aX + 1 <= fMapX then  Visit(aX + 1, aY, aDistance + 1);
      if aY - 1 >= 1 then  Visit(aX, aY - 1, aDistance + 1);
      if aY + 1 <= fMapY then  Visit(aX, aY + 1, aDistance + 1);
    end;
  end;

begin
  Result := false;
  ResetTMP;
  Visit(aLoc1.X, aLoc1.Y, 0);

  if aPointList.Count > 0 then
    Result := true;

end;

procedure TKMTerrain.GetHouseMarks(const aLoc: TKMPoint; aHouseType: TKMHouseType; aList: TKMPointTagList; aIgnoreObjects : Boolean = false);

  procedure MarkPoint(aPoint: TKMPoint; aID: Integer);
  var
    I: Integer;
  begin
    for I := 0 to aList.Count - 1 do //Skip wires from comparison
      if (aList.Tag[I] <> TC_OUTLINE) and KMSamePoint(aList[I], aPoint) then
        Exit;
    aList.Add(aPoint, aID);
  end;

var
  I,K,S,T: Integer;
  P2: TKMPoint;
  allowBuild: Boolean;
  HA: TKMHouseArea;
  CanDoWall : Boolean;
begin
  Assert(aList.Count = 0);
  HA := gRes.Houses[aHouseType].BuildArea;

  CanDoWall := false;
  if aHouseType in [htWall, htWall3, htWall5] then
    CanDoWall := CanPlaceWall(aLoc, aHouseType)
  else
  if aHouseType = htWell then
    CanDoWall := CanPlaceWell(aLoc)
  else
  if aHouseType = htShipYard then
    CanDoWall := CanPlaceShipYard(aLoc.X, aLoc.Y);

  for I := 1 to 4 do
    for K := 1 to 4 do
      if HA[I,K] <> 0 then
      begin

        if TileInMapCoords(aLoc.X+K-3-gRes.Houses[aHouseType].EntranceOffsetX,aLoc.Y+I-4-gRes.Houses[aHouseType].EntranceOffsetY,1) then
        begin
          //This can't be done earlier since values can be off-map
          P2 := KMPoint(aLoc.X+K-3-gRes.Houses[aHouseType].EntranceOffsetX,aLoc.Y+I-4-gRes.Houses[aHouseType].EntranceOffsetY);


          case aHouseType of
            htIronMine: allowBuild := CanPlaceIronMine(P2.X, P2.Y);
            htGoldMine: allowBuild := CanPlaceGoldMine(P2.X, P2.Y);
            htBitinMine: allowBuild := CanPlaceGoldMine(P2.X, P2.Y) or CanPlaceIronMine(P2.X, P2.Y);
            htAppleTree:  begin
                            {if HA[I,K] = 2 then
                            begin
                              if gMapElements[Land^[P2.Y, P2.X].Obj].IsFruit > 0 then //If there is a fruit tree than we can place it
                                allowBuild := allowBuild and (tpBuildNoObj in Land^[P2.Y,P2.X].Passability)
                              else
                                allowBuild := allowBuild and (tpBuild in Land^[P2.Y,P2.X].Passability);

                            end else}
                            allowBuild := (tpBuildNoObj in Land^[P2.Y,P2.X].Passability);

                            if (HA[I,K] = 2) then
                            begin
                              If (gMapElements[Land^[P2.Y, P2.X].Obj].IsFruit = 0) then
                                allowBuild := allowBuild and (gMapElements[Land^[P2.Y, P2.X].Obj].CanBeRemoved or (Land^[P2.Y, P2.X].Obj = OBJ_NONE));
                            end else
                              allowBuild := allowBuild and (gMapElements[Land^[P2.Y, P2.X].Obj].CanBeRemoved or (Land^[P2.Y, P2.X].Obj = OBJ_NONE));

                            //if I = 4 then
                            //  allowBuild := allowBuild and (not House(P2.X, P2.Y - 2).IsValid(htAppleTree) or TKMHouseAppleTree(House(P2.X, P2.Y - 2)).CanAddChildTree(P2))

                          end;
            else  If aIgnoreObjects then
                    allowBuild := (tpBuildNoObj in Land^[P2.Y,P2.X].Passability)
                  else
                    allowBuild := (tpBuild in Land^[P2.Y,P2.X].Passability);
          end;
          //Check house-specific conditions, e.g. allow shipyards only near water and etc..

          if aHouseType in [htWall, htWall3, htWall5, htWell, htShipYard] then
            allowBuild := allowBuild and CanDoWall;
          //Check surrounding tiles in +/- 1 range for other houses pressence

          if aHouseType <> htAppleTree then
            if allowBuild then
              for S := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
                for T := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
                  if (S <> 0) or (T<>0) then  //This is a surrounding tile, not the actual tile
                    if Land^[P2.Y+T,P2.X+S].TileLock in [tlFenced,tlDigged,tlHouse] then
                    begin
                      MarkPoint(KMPoint(P2.X+S,P2.Y+T), TC_BLOCK);
                      allowBuild := False;
                    end;

          if not (aHouseType in IGNORE_HOUSE_BLOCK) then
            if allowBuild then
              for S := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
                for T := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
                  if (S <> 0) or (T<>0) then  //This is a surrounding tile, not the actual tile
                    if Land^[P2.Y+T,P2.X+S].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence] then
                    begin
                      MarkPoint(KMPoint(P2.X+S,P2.Y+T), TC_BLOCK);
                      allowBuild := False;
                    end;

          if aHouseType = htAppleTree then
            if allowBuild then
              for S := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
              for T := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
                if (S <> 0) or (T<>0) then  //This is a surrounding tile, not the actual tile
                  if (Land^[P2.Y+T,P2.X+S].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence])
                    and House(P2.X+S, P2.Y+T).IsValid(htAppleTree, true)  then
                  begin
                    MarkPoint(KMPoint(P2.X+S,P2.Y+T), TC_BLOCK);
                    allowBuild := False;
                  end;
          //Mark the tile according to previous check results
          if allowBuild then
          begin
            if HA[I,K] = 2 then
              MarkPoint(P2, TC_ENTRANCE)
            else
              MarkPoint(P2, TC_OUTLINE);
          end
          else
          begin
            if HA[I,K] = 2 then
              MarkPoint(P2, TC_BLOCK_ENTRANCE)
            else
              if aHouseType in [htGoldMine, htIronMine, htBitinMine] then
                MarkPoint(P2, TC_BLOCK_MINE)
              else
                MarkPoint(P2, TC_BLOCK);
          end;
        end
        else
          if TileInMapCoords(aLoc.X+K-3-gRes.Houses[aHouseType].EntranceOffsetX,aLoc.Y+I-4-gRes.Houses[aHouseType].EntranceOffsetY, 0) then
            MarkPoint(KMPoint(aLoc.X+K-3-gRes.Houses[aHouseType].EntranceOffsetX,aLoc.Y+I-4-gRes.Houses[aHouseType].EntranceOffsetY), TC_BLOCK);
      end;
end;

procedure TKMTerrain.GetStructureMarks(const aLoc: TKMPoint; aIndex, aRot: Word; aList: TKMPointTagList);
var allowBuild : Boolean;

  procedure MarkPoint(aPoint: TKMPoint; aTag: Integer);
  var
    I: Integer;
  begin
    for I := 0 to aList.Count - 1 do //Skip wires from comparison
      if (aList.Tag[I] <> TC_OUTLINE) and KMSamePoint(aList[I], aPoint) then
        Exit;
    aList.Add(aPoint, aTag);
  end;


  function CheckForHouses(const aPoint : TKMPoint) : Boolean;
  var
    I, K: Integer;
  begin
    Result := true;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if (I <> 0) or (K <> 0) then //This is a surrounding tile, not the actual tile
          if Land^[aPoint.Y+I,aPoint.X+K].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence] then
            Exit(false);

  end;

var I, K : Integer;
  P : TKMPoint;
  canBuildBridge, hasAnyWater : Boolean;
  isBridge : Boolean;
  str : TKMStructureBasic;
begin
  str := gRes.Structures[aIndex].Base[aRot];

  isBridge := gRes.Structures[aIndex].IsBridge;

  //bridge has to have water connection
  canBuildBridge := false;
  hasAnyWater := false;
  if isBridge then
  begin
    hasAnyWater := false;
    for I := 0 to str.Size.X - 1 do
      for K := 0 to str.Size.Y - 1 do
        if str.PointXY[I, K] = 2 then
        begin
          P.X := aLoc.X + I + str.Offset.X;
          P.Y := aLoc.Y + K + str.Offset.Y;
          if not TileInMapCoords(P, 1) then
            Continue;

          canBuildBridge := (TileHasWater(P.X, P.Y + 1) and (str.PointXY[I, K - 1] = 0))
                            or (TileHasWater(P.X, P.Y - 1) and (str.PointXY[I, K + 1] = 0))
                            or (TileHasWater(P.X + 1, P.Y) and (str.PointXY[I - 1, K] = 0))
                            or (TileHasWater(P.X - 1, P.Y) and (str.PointXY[I + 1, K] = 0));

          canBuildBridge := canBuildBridge and not TileHasWater(P);
          canBuildBridge := canBuildBridge and TileIsWalkable(P);
          canBuildBridge := canBuildBridge and (  TileIsWalkable(KMPointBelow(P))
                                                  or TileIsWalkable(KMPointAbove(P))
                                                  or TileIsWalkable(KMPointLeft(P))
                                                  or TileIsWalkable(KMPointRight(P))
                                                );

          canBuildBridge := canBuildBridge
                              and ((Land^[P.Y, P.X].BridgeType = gRes.Structures[aIndex].BridgeType)
                                    or (Land^[P.Y, P.X].BridgeType = 0));
        end else
        begin
          P.X := aLoc.X + I + str.Offset.X;
          P.Y := aLoc.Y + K + str.Offset.Y;
          if not TileInMapCoords(P, 1) then
            Continue;
          hasAnyWater := hasAnyWater or TileHasWater(P);
        end;

  end;

  for I := 0 to str.Size.X - 1 do
    for K := 0 to str.Size.Y - 1 do
    if str.PointXY[I, K] > 0 then
    begin
      P.X := aLoc.X + I + str.Offset.X;
      P.Y := aLoc.Y + K + str.Offset.Y;

      if not TileInMapCoords(P, 1) then
      begin
        if TileInMapCoords(P, 0) then
          MarkPoint(P, TC_BLOCK);

        Continue;
      end;

      allowBuild := CheckForHouses(P);
      allowBuild := allowBuild and (Land^[P.Y, P.X].TileLock <> tlStructure);
      allowBuild := allowBuild and not IsReservedForAI(P);
      if isBridge then
      begin
        allowBuild := allowBuild and canBuildBridge;
        if str.PointXY[I, K] = 1 then
          allowBuild := allowBuild
                        and hasAnyWater
                        and (not TileHasTerrainKinds(P.X, P.Y, NO_BRIDGE_TER_KINDS) or (Land^[P.Y, P.X].BridgeType = gRes.Structures[aIndex].BridgeType));
      end;

      if gRes.Structures[aIndex].IsConstruction then
        allowBuild := allowBuild and ({tpBuild}tpBuildNoObj in Land^[P.Y, P.X].Passability);

      //allowBuild := allowBuild and not (Land^[P.Y, P.X].Obj in [80, 81, 61]);
      if allowBuild then
        begin
          If str.PointXY[I, K] = 2 then
            MarkPoint(P, TC_ENTRANCE)
          else
            MarkPoint(P, TC_OUTLINE);

        end else
        begin
          If str.PointXY[I, K] = 2 then
            MarkPoint(P, TC_BLOCK_ENTRANCE)
          else
            MarkPoint(P, TC_BLOCK);
        end;
    end;
    


  {with gRes.Bridges[aIndex] do
  begin
        allowBuild := allowBuild and not (Land^[P.Y, P.X].Obj in [80, 81, 61]);

        if allowBuild then
        begin
          If Points[aRot, I] = 2 then
            MarkPoint(P, TC_ENTRANCE)
          else
            MarkPoint(P, TC_OUTLINE);

        end else
        begin
          If Points[aRot, I] = 2 then
            MarkPoint(P, TC_BLOCK_ENTRANCE)
          else
            MarkPoint(P, TC_BLOCK);
        end;

      end;

  end;}

end;

function TKMTerrain.WaterHasFish(const aLoc: TKMPoint): Boolean;
begin
  Result := gHands.PlayerAnimals.GetFishInWaterBody(Land^[aLoc.Y,aLoc.X].WalkConnect[wcFish],aLoc,False, FISHERMAN_FISH_MAX_DISTANCE + 5) <> nil;
end;


function TKMTerrain.CatchFish(aLoc: TKMPointDir; aTestOnly: Boolean = False): Boolean;
var
  myFish: TKMUnitFish;
begin
  //Here we are catching fish in the tile 1 in the direction
  aLoc.Loc := KMGetPointInDir(aLoc.Loc, aLoc.Dir);
  myFish := gHands.PlayerAnimals.GetFishInWaterBody(Land^[aLoc.Loc.Y, aLoc.Loc.X].WalkConnect[wcFish],
                                                    aLoc.Loc,
                                                    not aTestOnly,
                                                    IfThen(aTestOnly, FISHERMAN_FISH_MAX_DISTANCE + 10, FISHERMAN_FISH_MAX_DISTANCE));
  Result := (myFish <> nil);
  if not aTestOnly and (myFish <> nil) then
    myFish.ReduceFish; //This will reduce the count or kill it (if they're all gone)
end;

function TKMTerrain.CatchFish(aLoc: TKMPointDir; aMaxCount: Integer; aTestOnly: Boolean = False): Word;
var
  myFish: TKMUnitFish;
begin
  Result := 0;
  //Here we are catching fish in the tile 1 in the direction
  aLoc.Loc := KMGetPointInDir(aLoc.Loc, aLoc.Dir);
  myFish := gHands.PlayerAnimals.GetFishInWaterBody(Land^[aLoc.Loc.Y, aLoc.Loc.X].WalkConnect[wcFish],
                                                    aLoc.Loc,
                                                    not aTestOnly,
                                                    IfThen(aTestOnly, FISHERMAN_FISH_MAX_DISTANCE + 10, FISHERMAN_FISH_MAX_DISTANCE));
  if not aTestOnly and (myFish <> nil) then
    Result := myFish.ReduceFish(aMaxCount); //This will reduce the count or kill it (if they're all gone)
end;


procedure TKMTerrain.SetObject(const aLoc: TKMPoint; aID: Integer);
var
  isObjectSet: Boolean;
  oldObj : Integer;
  H : TKMHouse;
begin
  isObjectSet := False;
  oldObj := Land^[aLoc.Y, aLoc.X].Obj;

  if gMapElements[aID].IsCorn > 0 then
    if TileIsCornField(aLoc) and (GetCornStage(aLoc) <> gMapElements[aID].IsCorn) then
      begin
        SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftCorn, gMapElements[aID].IsCorn, gftWheat, False);
        isObjectSet := True;
      end;

  if gMapElements[aID].IsGrass > 0 then
    if TileIsGrassField(aLoc) and (GetGrassStage(aLoc) <> gMapElements[aID].IsGrass) then
      begin
        SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftGrassland, gMapElements[aID].IsGrass, gftGrass, False);
        isObjectSet := True;
      end;

  if gMapElements[aID].IsVege > 0 then
    if TileIsVegeField(aLoc) and (GetVegeStage(aLoc) <> gMapElements[aID].IsVege) then
      begin
        SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftVegeField, gMapElements[aID].IsGrass, gftGrass, False);
        isObjectSet := True;
      end;

  if gMapElements[aID].IsWine > 0 then
    if TileIsWineField(aLoc) and (GetWineStage(aLoc) <> gMapElements[aID].IsWine) then
      begin
        SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftWine, gMapElements[aID].IsWine, self.GetTileWineType(aLoc), False);
        isObjectSet := True;
      end;


  if gMapElements[aID].IsFruit > 0 then
    if Land^[aLoc.Y, aLoc.X].IsHouse <> nil then
    begin
      H := TKMHouse(Land^[aLoc.Y, aLoc.X].IsHouse);
      if H is TKMHouseAppleTree then
      begin
        TKMHouseAppleTree(H).FruitType := gMapElements[aID].IsFruit - 1;
        TKMHouseAppleTree(H).GrowPhase := gFruitTrees[gMapElements[aID].IsFruit - 1].GetStage(aID);
      end;
    end;

  
  {case aID of
    // Special cases for corn fields 
    58: if TileIsCornField(aLoc) and (GetCornStage(aLoc) <> 4) then
        begin
          SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftCorn, 4, False);
          isObjectSet := True;
        end;
    59: if TileIsCornField(aLoc) and (GetCornStage(aLoc) <> 4) then
        begin
          SetField(aLoc, Land^[aLoc.Y,aLoc.X].TileOwner, ftCorn, 5, False);
          isObjectSet := True;
        end
  end;}

  if not isObjectSet then
  begin
    if (gMapElements[oldObj].LightRadius > 0) and (gMapElements[oldObj].LightPower > 0) then
      RemLight(aLoc);


    if (gMapElements[aID].LightRadius > 0) and (gMapElements[aID].LightPower > 0) then
      AddLight(aLoc, gMapElements[aID].LightRadius, gMapElements[aID].LightPower);

    Land^[aLoc.Y,aLoc.X].Obj := aID;
    if gMapElements[aID].TreeGrowAge > 0 then
      Land^[aLoc.Y,aLoc.X].TreeAge := 1;

    //Add 1 tile on sides because surrounding tiles will be affected (CanPlantTrees)
    UpdatePassability(KMRectGrow(KMRect(aLoc), 1));

    //Tree could have blocked the only diagonal passage
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(aLoc)), True); //Trees block diagonal
  end;
end;

function TKMTerrain.GetObject(const aLoc: TKMPoint): Word;
begin
  Result := Land[aLoc.Y, aLoc.X].Obj;
end;


// Set Tile Overlay
//todo: Do not update walkConnect and passability multiple times here
procedure TKMTerrain.SetOverlay(const aLoc: TKMPoint; aOverlay: TKMTileOverlay; aOverwrite: Boolean);
var
  changed: Boolean;
begin
  if not TileInMapCoords(aLoc.X, aLoc.Y) then Exit;
  if not gGame.TerrainPainter.CanEditTile(aLoc.X, aLoc.Y) then Exit;

  if aOverlay = toRoad then
  begin
    SetRoad(aLoc, HAND_NONE, rtStone);
    Exit;
  end;

  changed := False;

  if aOverlay in TILE_OVERLAY_2 then
  begin
    if not gCursor.MapEdOverrideCustomTiles then
      If Land^[aLoc.Y, aLoc.X].TileOverlay2 <> toNone then
        Exit;


    Land^[aLoc.Y, aLoc.X].TileOverlay2 := aOverlay;
    changed := true;
    Land^[aLoc.Y, aLoc.X].Ware.C2 := 0;
    if aOverlay in [toGold, toIron, toBitin] then
      Land^[aLoc.Y, aLoc.X].Ware.C2 := 5
    else
    if aOverlay in [toCoal] then
      Land^[aLoc.Y, aLoc.X].Ware.C2 := 10;



  end else
  if aOverwrite then
  begin
    if not gCursor.MapEdOverrideCustomTiles then
      If Land^[aLoc.Y, aLoc.X].TileOverlay <> toNone then
        Exit;
      if CanAddField(aLoc.X, aLoc.Y, ftRoad)                       //Can we add road
        or ((Land^[aLoc.Y, aLoc.X].TileOverlay = toRoad)
            and (gHands.HousesHitTest(aLoc.X, aLoc.Y) = nil)) then //or Can we destroy road
      begin
        if Land^[aLoc.Y, aLoc.X].TileOverlay = toRoad then
          RemRoad(aLoc);

        Land^[aLoc.Y, aLoc.X].TileOverlay := aOverlay;

        if fMapEditor then
          gGame.MapEditor.LandMapEd^[aLoc.Y, aLoc.X].CornOrWine := 0;

        UpdateFences(aLoc);

        if (aOverlay in ROAD_LIKE_OVERLAYS) and gMapElements[Land^[aLoc.Y, aLoc.X].Obj].WineOrCorn then
          RemoveObject(aLoc);

        changed := True;
      end;
  end
  else
  begin
    if not gCursor.MapEdOverrideCustomTiles then
      If Land^[aLoc.Y, aLoc.X].TileOverlay <> toNone then
        Exit;
    if CanAddField(aLoc.X, aLoc.Y, ftRoad)
      and not TileIsWineField(KMPoint(aLoc.X, aLoc.Y))
      and not TileIsGrassField(KMPoint(aLoc.X, aLoc.Y))
      and not TileIsVegeField(KMPoint(aLoc.X, aLoc.Y))
      and not TileIsCornField(KMPoint(aLoc.X, aLoc.Y)) then
    begin
        gTerrain.Land^[aLoc.Y, aLoc.X].TileOverlay := aOverlay;



      changed := True;
    end;
  end;

  if aOverlay = toNone then
    if gTerrain.Land^[aLoc.Y, aLoc.X].TileOverlay2 <> toNone then
    begin
      gTerrain.Land^[aLoc.Y, aLoc.X].TileOverlay2 := aOverlay;
      Land^[aLoc.Y, aLoc.X].Ware.C2 := 0;
    end;

  if changed then
  begin
    UpdatePassability(aLoc);
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(aLoc)), False);
  end;
end;


// Remove the tree and place a falling tree instead
function TKMTerrain.FallTree(const aLoc: TKMPoint): Boolean;
var
  aID: Integer;
begin
  Result := False;
  if Land[aLoc.Y, aLoc.X].TileOverlay2 = toInfinity then
    Exit(true);
  aID := Land^[aLoc.Y,aLoc.X].Obj;
  if gMapElements[aID].CuttableTree then
  begin
    if gMapElements[aID].LandStump = 0 then
      Land^[aLoc.Y,aLoc.X].Obj := 255
    else
      Land^[aLoc.Y,aLoc.X].Obj := gMapElements[aID].LandStump;

    if gMapElements[aID].FallTreeAnimObj > 0 then
      FallingTrees.Add(aLoc, gMapElements[aID].FallTreeAnimObj, fAnimStep);

    if gMySpectator.FogOfWar.CheckTileRevelation(aLoc.X, aLoc.Y) >= 255 then
      gSoundPlayer.Play(sfxTreeDown, aLoc, True);

    UpdatePassability(KMRectGrow(KMRect(aLoc), 1));

    Exit(True);

  end;{ else
  begin
    for I := 1 to Length(CHOPABLE_TREES) do
      if CHOPABLE_TREES[I, caAgeFull] = Land^[aLoc.Y,aLoc.X].Obj then
      begin
        Land^[aLoc.Y,aLoc.X].Obj := CHOPABLE_TREES[I, caAgeStump];
        //Remember tick when tree was chopped to calc the anim length
        FallingTrees.Add(aLoc, CHOPABLE_TREES[I, caAgeFall], fAnimStep);
        if gMySpectator.FogOfWar.CheckTileRevelation(aLoc.X, aLoc.Y) >= 255 then
          gSoundPlayer.Play(sfxTreeDown, aLoc, True);

        //Update passability immidiately
        UpdatePassability(KMRectGrow(KMRect(aLoc), 1));
        Exit(True);
      end;
  end;}
    
end;


// Remove the tree and place stump instead
procedure TKMTerrain.ChopTree(const aLoc: TKMPoint);
var
  H: TKMHouse;
  removeStamp: Boolean;
begin
  Land^[aLoc.Y,aLoc.X].TreeAge := 0;
  FallingTrees.Remove(aLoc);

  // Check if that tree was near house entrance (and stamp will block its entrance)
  //  E       entrance
  //   S      stamp
  removeStamp := False;
  H := gHands.HousesHitTest(aLoc.X - 1, aLoc.Y - 1);
  if (H <> nil) 
    and (H.Entrance.X = aLoc.X - 1)
    and (H.Entrance.Y + 1 = aLoc.Y) then
    removeStamp := True;

  if not removeStamp then
  begin
    //  E       entrance
    //  S       stamp
    H := gHands.HousesHitTest(aLoc.X, aLoc.Y - 1);
    if (H <> nil) 
      and (H.Entrance.X = aLoc.X)
      and (H.Entrance.Y + 1 = aLoc.Y) then
      removeStamp := True;
  end;

  if removeStamp then
    Land^[aLoc.Y,aLoc.X].Obj := OBJ_NONE;

  //Update passability after all object manipulations
  UpdatePassability(KMRectGrow(KMRect(aLoc), 1));

  //WalkConnect takes diagonal passability into account
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrowTopLeft(KMRect(aLoc)), True); //Trees block diagonals
end;


procedure TKMTerrain.RemoveObject(const aLoc: TKMPoint);
var
  blockedDiagonal: Boolean;
begin
  if Land^[aLoc.Y,aLoc.X].Obj <> OBJ_NONE then
  begin
    blockedDiagonal := gMapElements[Land^[aLoc.Y,aLoc.X].Obj].DiagonalBlocked;
    Land^[aLoc.Y,aLoc.X].Obj := OBJ_NONE;
    if blockedDiagonal then
      UpdateWalkConnect([wcWalk,wcRoad,wcWork], KMRectGrowTopLeft(KMRect(aLoc)), True);
  end;
end;


procedure TKMTerrain.RemoveObjectsKilledByRoad(const aLoc: TKMPoint);

  procedure RemoveIfWest(Loc: TKMPoint);
  begin
    if gMapElements[Land^[Loc.Y,Loc.X].Obj].KillByRoad = kbrWest then
      RemoveObject(Loc);
  end;

  procedure RemoveIfAlways(Loc: TKMPoint);
  begin
    if gMapElements[Land^[Loc.Y,Loc.X].Obj].KillByRoad = kbrAlways then
      RemoveObject(Loc);
  end;

  procedure KillByRoadCorner(const Loc: TKMPoint);
  begin
    // Check object type first, cos checking roads is more expensive
    if (gMapElements[Land^[Loc.Y,Loc.X].Obj].KillByRoad = kbrNWCorner)
      and (TileHasRoad(Loc.X - 1, Loc.Y)) and (TileHasRoad(Loc.X - 1, Loc.Y - 1))
      and (TileHasRoad(Loc.X, Loc.Y - 1)) and (TileHasRoad(Loc.X, Loc.Y)) then
      RemoveObject(Loc);
  end;
begin
  // Objects killed when surrounded with road on all 4 sides
  // Check for quads this tile affects
  KillByRoadCorner(aLoc);
  KillByRoadCorner(KMPoint(aLoc.X + 1, aLoc.Y));
  KillByRoadCorner(KMPoint(aLoc.X, aLoc.Y + 1));
  KillByRoadCorner(KMPoint(aLoc.X + 1, aLoc.Y + 1));

  // Objects killed by roads on sides only
  // Check 2 tiles this tile affects
  if TileHasRoad(aLoc.X - 1, aLoc.Y) then
    RemoveIfWest(aLoc);
  if TileHasRoad(aLoc.X + 1, aLoc.Y) then
    RemoveIfWest(KMPoint(aLoc.X + 1, aLoc.Y));

  if TileHasRoad(aLoc.X, aLoc.Y) then
    RemoveIfAlways(KMPoint(aLoc.X, aLoc.Y));
end;

procedure TKMTerrain.SetWareOnGround(aLoc : TKMPoint; aWare: TKMWareType; aCount: Integer);
var maxC : Integer;
begin
  if aCount = 0 then
    aWare := wtNone;
  if gGameParams.IsMapEditor then
    if aWare <> wtNone then
    begin
      if not gCursor.MapEdOverrideObjects then
        if (Land^[aLoc.Y, aLoc.X].Ware.W > 0) then
          Exit;

      maxC := length(gRes.Wares[aWare].AtTerrainPic);
      if aCount > maxC then
        aCount := KaMRandom(maxC, 'TKMTerrain.SetWareOnGround') + 1;
    end;

  Land^[aLoc.Y, aLoc.X].Ware.W := byte(aWare);
  Land^[aLoc.Y, aLoc.X].Ware.C := aCount;
  self.UpdatePassability(aLoc);
end;

function TKMTerrain.GetWareOnGround(aLoc: TKMPoint): TKMWareType;
begin
  Result := TKMWareType(Land^[aLoc.Y, aLoc.X].Ware.W);
  if Land^[aLoc.Y, aLoc.X].Ware.C = 0 then
    Result := wtNone;
end;
function TKMTerrain.GetWareOnGroundCount(aLoc: TKMPoint): Byte;
begin
  Result := Land^[aLoc.Y, aLoc.X].Ware.C;
end;


procedure TKMTerrain.SowCorn(const aLoc: TKMPoint; aGrainType : TKMGrainFarmSet;  aAddManure : Boolean);
var GFT2 : TKMGrainType;
begin
  GFT2 := gftNone;
  if TileIsCornField(aLoc) then
  begin
    if aGrainType[0] = gftRandom then
      GFT2 := GRAIN_GUI_ORDER[  KaMRandom(length(GRAIN_GUI_ORDER) - 2, 'SowCorn 1')  ]
    else
      GFT2 := aGrainType[0];
  end else
  if TileIsGrassField(aLoc) then
  begin
    if aGrainType[1] = gftRandom then
      GFT2 := GRASS_GUI_ORDER[  KaMRandom(length(GRASS_GUI_ORDER) - 2, 'SowCorn 1')  ]
    else
      GFT2 := aGrainType[1];
  end else
  if TileIsVegeField(aLoc) then
  begin
    if aGrainType[2] = gftRandom then
      GFT2 := VEGE_GUI_ORDER[  KaMRandom(length(VEGE_GUI_ORDER) - 2, 'SowCorn 1')  ]
    else
      GFT2 := aGrainType[2];
  end;
  if GFT2 = gftNone then //did not found any grain type, something is wrong
    Exit;

  Land^[aLoc.Y,aLoc.X].FieldAge := 1;
  if aAddManure then
    Land^[aLoc.Y,aLoc.X].FieldAge := 10;

  Land^[aLoc.Y,aLoc.X].GrainType := GFT2;
  Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain  := GetTileCornTile(aLoc, 1); //Plant it right away, don't wait for update state
  Land^[aLoc.Y,aLoc.X].Obj := GetTileCornObject(aLoc, 1);
  UpdatePassability(KMRectGrow(KMRect(aLoc), 1));
end;


function TKMTerrain.CutCorn(const aLoc: TKMPoint): Boolean;
var aStage : Byte;
begin
  aStage := 0;
  if TileIsGrassField(aLoc) then
  begin
    aStage := gFieldGrains[GetGrainType(aLoc)].GetStage(Land^[aLoc.Y,aLoc.X].FieldAge);

    Result := (aStage >= CORN_AGE_MAX) or gFieldGrains[GetGrainType(aLoc)].Stage[aStage].CanBeCut;

    if not Result then Exit; //We have no corn here actually, nothing to cut

    if aStage >= CORN_AGE_MAX then
    begin
      aStage := gFieldGrains[GetGrainType(aLoc)].StagesCount - 2;
      Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := GetTileGrassTile(aLoc, aStage);
      aStage := gFieldGrains[GetGrainType(aLoc)].Stage[aStage].NextStage;

      Land^[aLoc.Y,aLoc.X].Obj := GetTileGrassObject(aLoc, aStage);
      Land^[aLoc.Y,aLoc.X].FieldAge := gFieldGrains[GetGrainType(aLoc)].Stage[aStage].Age;
      Result := true;
    end else
    begin
      Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := GetTileGrassTile(aLoc, aStage+1);
      Land^[aLoc.Y,aLoc.X].Obj := GetTileGrassObject(aLoc, aStage+1);
      Inc(Land^[aLoc.Y,aLoc.X].FieldAge);
      Result := false;

    end;
    Exit;
  end;
  if TileIsCornField(aLoc) then
    aStage := GetCornStage(aLoc)
  else
  if TileIsVegeField(aLoc) then
    aStage := GetVegeStage(aLoc);

  {Result := (TileIsCornField(aLoc) or TileIsVegeField(aLoc)) and (aStage = 5); //todo: refactor: use enum instead of magic numbers !

  Result := Result or (TileIsGrassField(aLoc) and (aStage in [4, 5]));}
  Result := (aStage >= CORN_AGE_MAX);
  if not Result then Exit; //We have no corn here actually, nothing to cut
  If Land^[aLoc.Y,aLoc.X].TileOverlay2 = toInfinity then
    Exit;
  Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := GetTileCornTile(aLoc, 6);
  Land^[aLoc.Y,aLoc.X].Obj := GetTileCornObject(aLoc, 6);
  Land^[aLoc.Y,aLoc.X].FieldAge := 0;
end;


function TKMTerrain.CutGrapes(const aLoc: TKMPoint): Boolean;
var aStage : Byte;
begin
  aStage := GetWineStage(aLoc);
  Result := TileIsWineField(aLoc) and gFieldGrains[GetGrainType(aLoc)].Stage[aStage].CanBeCut;
  if not Result then Exit; //We have no wine here actually, nothing to cut

  If Land^[aLoc.Y,aLoc.X].TileOverlay2 = toInfinity then
    Exit;
  Land^[aLoc.Y,aLoc.X].FieldAge := 1;
  Land^[aLoc.Y,aLoc.X].Obj := gFieldGrains[GetGrainType(aLoc)].Stage[0].Obj; //Reset the grapes
end;


//procedure TKMTerrain.SetFieldNoUpdate(const Loc: TKMPoint; aOwner: TKMHandID; aFieldType: TKMFieldType; aStage: Byte = 0);
//begin
//  SetField(Loc, aOwner, aFieldType, aStage, False, False, True, False);
//end;


procedure TKMTerrain.SetField(const aLoc: TKMPoint; aOwner: TKMHandID; aFieldType: TKMFieldType; aStage: Integer = 0; aGrainType : TKMGrainType = gftNone;
                              aRandomAge: Boolean = False; aKeepOldObject: Boolean = False; aRemoveOverlay: Boolean = True;
                              aDoUpdate: Boolean = True);

  procedure SetLand(aFieldAge: Byte; aTerrain: Word; aObj: Integer = -1);
  begin
    Land^[aLoc.Y, aLoc.X].FieldAge := aFieldAge;

    {if aFieldAge = 0 then
      if aTerrain <> 63 then
        case Land[aLoc.Y, aLoc.X].GrainType of
          gftOat,
          gftRice,
          gftWheat : Inc(Land^[aLoc.Y, aLoc.X].FieldAge, CORN_AGE_1 div 2);
        end;}

    if fMapEditor then
    begin
      if aTerrain > 0 then
        gGame.MapEditor.LandMapEd^[aLoc.Y, aLoc.X].CornOrWineTerrain := aTerrain;
    end
    else
    begin
      if aTerrain > 0 then
        Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain := aTerrain;
      Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation := 0;
      Land^[aLoc.Y, aLoc.X].LayersCnt := 0; //Do not show transitions under corn/wine field
    end;

    if aObj <> -1 then
      Land^[aLoc.Y,aLoc.X].Obj := aObj;
  end;

  function GetObj: Integer;
  begin
    Result := -1;
    if aFieldType = ftCorn then
    begin
      if not aKeepOldObject //Keep old object, when loading from script via old SetField command
        and ObjectIsCornOrGrass(aLoc) then
        Result := OBJ_NONE;
    end;
  end;

var
  fieldAge: Byte;
begin
  Assert(aFieldType in [ftCorn, ftWine, ftGrassLand, ftVegeField], 'SetField is allowed to use only for corn or wine.');

  if aFieldType = ftWine then
    aGrainType := GetTileWineType(aLoc);

  if not (aGrainType in GRAIN_VALID) then
    if aFieldType = ftCorn then
      aGrainType :=  gftWheat
    else
    if aFieldType = ftWine then
      aGrainType :=  gftWinePurple
    else
    if aFieldType = ftVegeField then
      aGrainType :=  gftPumpkin
    else
      aGrainType :=  gftNone;

  Land^[aLoc.Y, aLoc.X].GrainType := aGrainType;

  SetField_Init(aLoc, aOwner, aRemoveOverlay);

  if aStage = -1 then
    aStage := KaMRandom(gFieldGrains[aGrainType].StagesCount, 'TKMTerrain.SetField:Random grain Type in map ed');


  aStage := Min(aStage, gFieldGrains[aGrainType].StagesCount - 1);

  if aStage = 0 then //empty field
    fieldAge := 0
  else
  if aStage = gFieldGrains[aGrainType].StagesCount - 1 then //it's already cut
    fieldAge := 0
  else
  if aStage = gFieldGrains[aGrainType].StagesCount - 2 then //one before last stage is full grown grain, ready to cut
    fieldAge := 255
  else
    fieldAge := gFieldGrains[aGrainType].Stage[aStage].Age;

  if (aFieldType = ftCorn) and not TileIsCornField(aloc) then
  begin
    //if aStage = 0 then
    //  Land^[aLoc.Y, aLoc.X].GrainType := gftNone;
    //Land^[aLoc.Y, aLoc.X].DefTile := Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain;
    //Land^[aLoc.Y, aLoc.X].DefRotation := Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation;
    SetNewDefTile(aLoc);
  end;

  if (aFieldType = ftCorn)
    and (aStage >= 0)
    {and (InRange(aStage, 0, CORN_STAGES_COUNT - 1))} then
  begin
    if fMapEditor then
      gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 1;
    SetLand(fieldAge, gFieldGrains[aGrainType].Stage[aStage].Terr, gFieldGrains[aGrainType].Stage[aStage].Obj)

  end;

  if (aFieldType = ftWine)
    {and (InRange(aStage, 0, WINE_STAGES_COUNT - 1)) }then
  begin
    if fMapEditor then
      gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 2;
    fieldAge := gFieldGrains[aGrainType].Stage[aStage].Age;
    if aStage = gFieldGrains[aGrainType].StagesCount - 1 then
      fieldAge := CORN_AGE_MAX;
    SetLand(fieldAge, gFieldGrains[aGrainType].Stage[aStage].Terr, gFieldGrains[aGrainType].Stage[aStage].Obj)
  end;

  if (aFieldType = ftGrassLand) and not TileIsGrassField(aloc) then
  begin
    //Land^[aLoc.Y, aLoc.X].DefTile := Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain;
    //Land^[aLoc.Y, aLoc.X].DefRotation := Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation;
    SetNewDefTile(aLoc);
  end;

  if (aFieldType = ftGrassLand)
    and (aStage >= 0) then
  begin
    if fMapEditor then
      gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 3;
    SetLand(fieldAge, gFieldGrains[aGrainType].Stage[aStage].Terr, gFieldGrains[aGrainType].Stage[aStage].Obj)
  end;


  if (aFieldType = ftVegeField) and not TileIsVegeField(aloc) then
  begin
    //Land^[aLoc.Y, aLoc.X].DefTile := Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain;
    //Land^[aLoc.Y, aLoc.X].DefRotation := Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation;
    SetNewDefTile(aLoc);
  end;

  if (aFieldType = ftVegeField)
    and (aStage >= 0) then
  begin
    if fMapEditor then
      gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWine := 4;
    SetLand(fieldAge, gFieldGrains[aGrainType].Stage[aStage].Terr, gFieldGrains[aGrainType].Stage[aStage].Obj)
  end;

  if aDoUpdate then
    SetField_Complete(aLoc, aFieldType);

  if (aFieldType = ftWine) then
    gScriptEvents.ProcWinefieldBuilt(aOwner, aLoc.X, aLoc.Y)
  else if (aFieldType = ftCorn) then
    gScriptEvents.ProcFieldBuilt(aOwner, aLoc.X, aLoc.Y);
end;


// Extract one unit of stone
function TKMTerrain.DecStoneDeposit(const aLoc: TKMPoint): Boolean;
type
  TKMStoneTransitionType = (sttNone, sttGrass, sttCoastSand, sttDirt, sttSnow, sttSnowOnDirt, sstBarrenLand);
const
  TRANSITIONS_TER_KINDS: array[TKMStoneTransitionType] of TKMTerrainKind =
    (tkGrass, tkGrass, tkCoastSand, tkDirt, tkSnow, tkSnowOnDirt, tkBarrenLand);

  TRAN_TILES: array[TKMStoneTransitionType] of array[0..6] of Word =
              ((  0, 139, 138, 140, 141, 274, 301),
               (  0, 139, 138, 140, 141, 274, 301),
               ( 32, 269, 268, 270, 271, 273, 302),
               ( 35, 278, 277, 279, 280, 282, 303),
               ( 46, 286, 285, 287, 288, 290, 304),
               ( 47, 294, 293, 295, 296, 298, 305),
               ( 698, 742, 741, 743, 744, 745, 746));

  TILE_ID_INDEX:      array[1..14] of Word = (1,1,2,1,3,2,4,1,2,3,4,2,4,4);
  ROT_ID:             array[1..14] of Byte = (0,1,0,2,0,1,3,3,3,1,2,2,1,0);
  TILE_ID_DIAG_INDEX: array[1..15] of Word = (5,5,6,5,5,6,5,5,6,5,5,6,5,5,5);
  ROT_ID_DIAG:        array[1..15] of Byte = (3,0,0,1,3,1,3,2,3,0,0,2,0,0,0);

  NO_REPL = High(Word);
  WATER_DIAG_REPL: array[TKMStoneTransitionType] of Word =
                     (127, 127, 118, 105, NO_REPL, NO_REPL, NO_REPL);

  WATER_DIAG_REPL_ROT: array[TKMStoneTransitionType] of Byte =
                         (1, 1, 1, 3, 100, 100, 100);

  MAX_STEPS = 5; //steps limit

var
  visited: TKMPointArray;
  visitedCnt: Integer;

  procedure InitVisited;
  begin
    SetLength(visited, 8);
    visitedCnt := 0;
  end;

  procedure AddToVisited(X,Y: Word);
  begin
    if Length(visited) = visitedCnt then
      SetLength(visited, visitedCnt + 8);

    visited[visitedCnt] := KMPoint(X,Y);
    Inc(visitedCnt);
  end;

  function GetTile(aTransitionType: TKMStoneTransitionType; aTileIdIndex: Byte): Word;
  begin
    if aTileIdIndex = 0 then
      Result := TKMTerrainPainter.GetRandomTile(TRANSITIONS_TER_KINDS[aTransitionType])
    else
      Result := TRAN_TILES[aTransitionType, aTileIdIndex];

  end;

  function GetStoneTransitionType(X, Y: Word): TKMStoneTransitionType;
  begin
    Result := sttNone;
    case Land^[Y,X].BaseLayer.Terrain of
      0, 138,139,140,141,142,274,301:  Result := sttGrass;
      32,268,269,270,271,272,273,302:  Result := sttCoastSand;
      35,277,278,279,280,281,282,303:  Result := sttDirt;
      46,285,286,287,288,289,290,304:  Result := sttSnow;
      47,293,294,295,296,297,298,305:  Result := sttSnowOnDirt;
      698,742,741,743,744,745,746:  Result := sstBarrenLand;
    end;
  end;

  function UpdateTransition(X,Y: Integer; aStep: Byte): Boolean;

    function GetBits(aX,aY: Integer; aTransition: TKMStoneTransitionType; aDir: TKMDirection = dirNA): Byte;
    var
      dir: TKMDirection;
    begin
      if not TileInMapCoords(aX, aY) then Exit(0);

      dir := dirNA;
      //if tile has no other terrain types, then check if it has stone, via dirNA
      //otherwise check only tile corners, usign direction
      if not TileHasOnlyTerrainKinds(aX, aY, [tkStone, TRANSITIONS_TER_KINDS[aTransition]]) then
        dir := aDir;

      //Check is there anything stone-related (stone tile or corner at least)
      Result := Byte(TileHasTerrainKindPart(aX, aY, tkStone, dir));
    end;

  var
    transition: TKMStoneTransitionType;
    bits, bitsDiag: Byte;
    terRot, repl: Integer;
  begin
    Result := False;
    if not TileInMapCoords(X,Y) //Skip for tiles not in map coords
      or (aStep > MAX_STEPS)  //Limit for steps (no limit for now)
      or TileHasStone(X,Y)      //If tile has stone no need to change it
      or ArrayContains(KMPoint(X,Y), visited, visitedCnt) //If we already changed this tile
      or ((aStep <> 0) and not TileHasTerrainKindPart(X, Y, tkStone)) //If tile has no stone parts (except initial step)
      or not TileHasOnlyTerrainKinds(X, Y, [tkStone, tkGrass, tkCoastSand, tkDirt, tkSnow, tkSnowOnDirt, tkBarrenLand]) then //Do not update transitions with other terrains (mountains f.e.)
      Exit;

    // 1. Get tile transition type (with grass / sand etc)
    transition := GetStoneTransitionType(X,Y);

    // 2. Check what tiles around has stone

    // We 'encode' in bits variable if surrounding tiles are stone tiles
    // Then we found proper tile to replace
    // Starting with dirN and going clockwise
    // f.e. 11 = 1011 = stone is to the left, top and right of the tile
    bits := GetBits(X  , Y-1, transition, dirS)*1 +
            GetBits(X+1, Y  , transition, dirW)*2 +
            GetBits(X  , Y+1, transition, dirN)*4 +
            GetBits(X-1, Y  , transition, dirE)*8;

    // 3. Replace tile with other tile according to the tiles around
    if bits = 0 then
    begin
      // If there are no stone around in the straight directions then check diagonals
      bitsDiag := GetBits(X-1, Y-1, transition, dirSE)*1 +
                  GetBits(X+1, Y-1, transition, dirSW)*2 +
                  GetBits(X+1, Y+1, transition, dirNW)*4 +
                  GetBits(X-1, Y+1, transition, dirNE)*8;

      //This part is not actually used, looks like
      case Land^[Y,X].BaseLayer.Terrain of
        142,
        143:  begin
                // 142 and 143 are stone-water tiles (triangles)
                terRot := (Land[Y,X].BaseLayer.Terrain + Land^[Y,X].BaseLayer.Rotation) mod 4;
                case terRot of
                  0,1:  Exit;
                  2,3:  begin
                          repl := WATER_DIAG_REPL[transition];
                          if repl <> NO_REPL then
                          begin
                            Land^[Y,X].BaseLayer.Terrain := repl;
                            Land^[Y,X].BaseLayer.Rotation := (terRot + WATER_DIAG_REPL_ROT[transition]) mod 4;
                          end
                          else
                          begin
                            Land^[Y,X].BaseLayer.Terrain := 192;
                            Land^[Y,X].BaseLayer.SetCorners([1]);
                            Land^[Y,X].LayersCnt := 1;
                            Land^[Y,X].Layer[0].Terrain := gRes.Sprites.GenTerrainTransitions[TRANSITIONS_TER_KINDS[transition],
                                                                                              mkSoft2, tmt2Diagonal, mstMain];
                            Land^[Y,X].Layer[0].Rotation := terRot;
                            Land^[Y,X].Layer[0].SetCorners([0,2,3]);
                          end;
                        end;
                end;
              end;
        else
        begin
          if bitsDiag = 0 then
          begin
            Land^[Y,X].BaseLayer.Terrain  := TKMTerrainPainter.GetRandomTile(TRANSITIONS_TER_KINDS[transition]);
            Land^[Y,X].BaseLayer.Rotation := KaMRandom(4, 'TKMTerrain.DecStoneDeposit.UpdateTransition'); //Randomise the direction of no-stone terrain tiles
          end else begin
            // 142 and 143 are stone-water tiles (triangles)
            if Land^[Y,X].BaseLayer.Terrain in [142,143] then
              Exit;
            Land^[Y,X].BaseLayer.Terrain := TRAN_TILES[transition, TILE_ID_DIAG_INDEX[bitsDiag]];
            Land^[Y,X].BaseLayer.Rotation := ROT_ID_DIAG[bitsDiag];
          end;
        end;
      end;
    end
    else
    begin
      // 142 and 143 are stone-water tiles (triangles)
      if Land^[Y,X].BaseLayer.Terrain in [142,143] then
        Exit;

      // If tile is surrounded with other stone tiles no need to change it
      if bits <> 15 then
      begin
        Land^[Y,X].BaseLayer.Terrain  := TRAN_TILES[transition, TILE_ID_INDEX[bits]];
        Land^[Y,X].BaseLayer.Rotation := ROT_ID[bits];
      end;
    end;
    UpdatePassability(KMPoint(X,Y));
    AddToVisited(X,Y);
    // 4. Update surrounding tiles
    // Floodfill through around tiles
    UpdateTransition(X,  Y-1, aStep + 1); //  x x x
    UpdateTransition(X+1,Y,   aStep + 1); //  x   x
    UpdateTransition(X,  Y+1, aStep + 1); //  x x x
    UpdateTransition(X-1,Y,   aStep + 1);
    UpdateTransition(X-1,Y-1, aStep + 1);
    UpdateTransition(X+1,Y-1, aStep + 1);
    UpdateTransition(X+1,Y+1, aStep + 1);
    UpdateTransition(X-1,Y+1, aStep + 1);
  end;

var
  transition: TKMStoneTransitionType;
begin
  transition := GetStoneTransitionType(aLoc.X,aLoc.Y + 1); //Check transition type by lower point (Y + 1)

  Result := True;

  if Land^[aLoc.Y,aLoc.X].TileOverlay2 = toInfinity then
    Exit;

  if gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Stone > 0 then
  begin
    if gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Stone = 1 then
    SetObject(aLoc, 255)
    else
    if gMapElements[Land^[aLoc.Y,aLoc.X].Obj - 1].Stone > 0 then
      SetObject(aLoc, Land^[aLoc.Y,aLoc.X].Obj - 1)
    else
    SetObject(aLoc, 255);

    Exit;
  end;

  // Replace with smaller ore deposit tile (there are 2 sets of tiles, we can choose random)
  case Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain of
    132, 137: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 131 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit')*5;
    131, 136: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 130 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 2')*5;
    130, 135: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 129 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 3')*5;
    129, 134: case transition of
                sttNone,
                sttGrass:       Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 128 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 4')*5;
                sttCoastSand:   Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 266 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 5');
                sttDirt:        Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 275 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 6');
                sttSnow:        Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 283 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 7');
                sttSnowOnDirt:  Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 291 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 8');
                sstBarrenLand:  Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 739 + KaMRandom(2, 'TKMTerrain.DecStoneDeposit 9');
              end;
    128, 133,
    266, 267,
    275, 276,
    283, 284,
    291, 292,
    739, 740
    : begin
                Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain  := TRAN_TILES[transition, 0]; //Remove stone tile (so tile will have no stone)
                Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation := KaMRandom(4, 'TKMTerrain.DecStoneDeposit 9');

                InitVisited;
                //Tile type has changed and we need to update these 5 tiles transitions:
                UpdateTransition(aLoc.X,  aLoc.Y, 0);
              end;
  else
    Exit(False);
  end;

  FlattenTerrain(aLoc, True, True); //Ignore canElevate since it can prevent stonehill from being still walkable and cause a crash
end;


// Try to extract one unit of ore
// It may fail cos of two miners mining the same last piece of ore
function TKMTerrain.DecOreDeposit(const aLoc: TKMPoint; aWare: TKMWareType): Boolean;
var isCorner, isOverlayCoal : Boolean;
begin
  if not (aWare in [wtIronOre,wtGoldOre,wtCoal, wtBitinOre, wtTile, wtJewerly]) then
    raise ELocError.Create('Wrong ore decrease', aLoc);

  Result := True;

  isCorner := false;
  isOverlayCoal := false;

  if Land^[aLoc.Y,aLoc.X].TileOverlay2 in [toInfinity .. toInfinityCoal] then
    Exit;

  if aWare = wtJewerly then
  begin
    SetObject(aLoc, 255);
    Exit;
  end;

  if (aWare = wtTile) then
  begin
    if gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Clay > 0 then
    begin
      if gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Clay = 1 then
        SetObject(aLoc, OBJ_NONE)
      else
        SetObject(aLoc, Land^[aLoc.Y,aLoc.X].Obj - 1);

      Result := true;
    end else
    case Land^[aLoc.Y,aLoc.X].TileOverlay2 of
      toClay1 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toNone;
      toClay2 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toClay1;
      toClay3 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toClay2;
      toClay4 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toClay3;
      toClay5 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toClay4;
    end;
    Exit;
  end;
  //check if object has resource
  if ((aWare = wtCoal) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Coal > 0))
    or ((aWare = wtBitinOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Bitin > 0))
    or ((aWare = wtGoldOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Gold > 0))
    or ((aWare = wtIronOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Iron > 0))then
  begin
    if ((aWare = wtCoal) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Coal = 1))
    or ((aWare = wtBitinOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Bitin = 1))
    or ((aWare = wtGoldOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Gold = 1))
    or ((aWare = wtIronOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].Iron = 1)) then
      SetObject(aLoc, OBJ_NONE)
    else
    if ((aWare = wtCoal) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj - 1].Coal >= 1))
    or ((aWare = wtBitinOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj - 1].Bitin >= 1))
    or ((aWare = wtGoldOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj - 1].Gold >= 1))
    or ((aWare = wtIronOre) and (gMapElements[Land^[aLoc.Y,aLoc.X].Obj - 1].Iron >= 1)) then
      SetObject(aLoc, Land^[aLoc.Y,aLoc.X].Obj - 1)
    else
    SetObject(aLoc, OBJ_NONE);

    Result := true;
    Exit;
  end;
  if TileIsMineShaft(aLoc) then
    Exit;

  if ArrayContains(Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain, [325, 326, 327,  343, 344, 345,  334, 335, 336,  328, 329, 330, 337, 338, 339]) then
  begin
    isCorner := true;
    case Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain of


      325: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 322;
      326: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 323;
      327: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 324;

      328..330: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 160 + KaMRandom(4, 'TKMTerrain.DecOreDeposit 2');

      334: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 331;
      335: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 332;
      336: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 333;

      343: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 340;
      344: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 341;
      345: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 342;

      337: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 188;
      338: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 168;
      339: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 184;

    end;

  end else
  begin


    if (aWare = wtCoal) then
    begin
      if Land^[aLoc.Y,aLoc.X].TileOverlay2 in COAL_LIKE_OVERLAYS then
      begin
        isOverlayCoal := true;
        Result := true;
      end;

      case Land^[aLoc.Y,aLoc.X].TileOverlay2 of

        toCoal1 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toNone;
        toCoal2 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toCoal1;
        toCoal3 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toCoal2;
        toCoal4 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toCoal3;
        toCoal5 : Land^[aLoc.Y,aLoc.X].TileOverlay2 := toCoal4;

      end;

    end;
    if not isOverlayCoal then
      case Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain of

        144: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 157 + KaMRandom(3, 'TKMTerrain.DecOreDeposit'); //Gold
        145: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 144;
        146: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 145;
        147: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 146;
        307: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 147;


        148: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 160 + KaMRandom(4, 'TKMTerrain.DecOreDeposit 2'); //Iron
        149: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 148;
        150: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 149;
        259: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 149;
        151: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 150 + KaMRandom(2, 'TKMTerrain.DecOreDeposit 3')*(259 - 150);
        260: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 151;


        152: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 35  + KaMRandom(2, 'TKMTerrain.DecOreDeposit 4'); //Coal
        153: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 152;
        154: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 153;
        155: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 154;
        263: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 155;
        334: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 35;

        609: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 157 + KaMRandom(3, 'TKMTerrain.DecOreDeposit 2'); //Bitin
        610: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 609;
        611: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 610;
        612: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 611;

        613: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 160 + KaMRandom(4, 'TKMTerrain.DecOreDeposit 2'); //Bitin
        614: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 613;
        615: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 614;
        616: Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain := 615;
      else
        Result := False;
      end;
    if not Result and (aWare in [wtGoldOre, wtIronOre, wtBitinOre, wtCoal]) then
      if Land^[aLoc.Y,aLoc.X].Ware.C2 > 0 then
      begin
        Result := true;
        isCorner := true;
        Land^[aLoc.Y,aLoc.X].Ware.C2 := Land^[aLoc.Y,aLoc.X].Ware.C2 - 1;
        if Land^[aLoc.Y,aLoc.X].Ware.C2 = 0 then
          Land^[aLoc.Y,aLoc.X].TileOverlay2 := toNone;
      end;
  end;

  if not isCorner and not isOverlayCoal then
    Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation := KaMRandom(4, 'TKMTerrain.DecOreDeposit 5');

  UpdatePassability(aLoc);
end;


procedure TKMTerrain.UpdatePassability(const aLoc: TKMPoint);

  procedure AddPassability(aPass: TKMTerrainPassability);
  begin
    Land[aLoc.Y,aLoc.X].Passability := Land^[aLoc.Y,aLoc.X].Passability + [aPass];
  end;
  function HasPassability(aPass : TKMTerrainPassability) : Boolean;
  begin
    Result := aPass in Land[aLoc.Y,aLoc.X].Passability;
  end;

var
  I, K: Integer;
  hasHousesNearTile, housesNearVertex, isBuildNoObj: Boolean;
begin
  Assert(TileInMapCoords(aLoc.X, aLoc.Y), 'TKMTerrain.UpdatePassability'); //First of all exclude all tiles outside of actual map

  Land^[aLoc.Y,aLoc.X].Passability := [];

  if TileIsWalkable(aLoc)
    and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
    and CheckHeightPass(aLoc, hpWalking) then
    AddPassability(tpOwn);

  if Land^[aLoc.Y,aLoc.X].TileLock in [tlWall, tlWallFence, tlWallGate] then
    AddPassability(tpWall);


{   if Land^[aLoc.Y,aLoc.X].TileLock = tlWallGate then
      AddPassability(tpWallGate);
}

  //For all passability types other than CanAll, houses and fenced houses are excluded
  if Land^[aLoc.Y,aLoc.X].TileLock in [tlNone, tlFenced, tlFieldWork, tlRoadWork, tlWallEmpty, tlStructure] then
  begin
    if TileIsWalkable(aLoc)
      and gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].Walkable
      and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
      and ((Land^[aLoc.Y,aLoc.X].Ware.C < 5) or (Land^[aLoc.Y,aLoc.X].Ware.W = 0))
      and CheckHeightPass(aLoc, hpWalking) then
      AddPassability(tpWalk);

    if (Land^[aLoc.Y,aLoc.X].TileOverlay = toRoad)
    and (tpWalk in Land^[aLoc.Y,aLoc.X].Passability) then //Not all roads are walkable, they must also have CanWalk passability
      AddPassability(tpWalkRoad);

    //Check for houses around this tile/vertex
    hasHousesNearTile := False;
    for I := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      for K := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
        if TileInMapCoords(aLoc.X+K, aLoc.Y+I)
          and (Land^[aLoc.Y+I,aLoc.X+K].TileLock in [tlFenced,tlDigged,tlHouse]) then
          hasHousesNearTile := True;

    isBuildNoObj := False;
    if TileIsRoadable(aLoc)
      and (tpWalk in Land^[aLoc.Y,aLoc.X].Passability)
      and not TileIsCornField(aLoc) //Can't build houses on fields
      and not TileIsWineField(aLoc)
      and not TileIsGrassField(aLoc)
      and not TileIsVegeField(aLoc)
      and not gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].NotBuildable
      and (Land^[aLoc.Y,aLoc.X].TileLock in [tlNone])
      and TileInMapCoords(aLoc.X, aLoc.Y, 1)
      and CheckHeightPass(aLoc, hpBuilding) then
    begin
      AddPassability(tpBuildNoObj);
      isBuildNoObj := True;
    end;

    if isBuildNoObj and not hasHousesNearTile
      and((Land[aLoc.Y,aLoc.X].Obj = OBJ_NONE) or (gMapElements[Land^[aLoc.Y,aLoc.X].Obj].CanBeRemoved)) then //Only certain objects are excluded
      AddPassability(tpBuild);

    if TileIsRoadable(aLoc)
      and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
      and (Land^[aLoc.Y,aLoc.X].TileLock in [tlNone, tlWallEmpty])
      and (Land^[aLoc.Y,aLoc.X].TileOverlay <> toRoad)
      and CheckHeightPass(aLoc, hpWalking)
      and not (Land^[aLoc.Y,aLoc.X].Obj in [80, 81])
      and not TileHasPalisade(aLoc.X,aLoc.Y) then
      AddPassability(tpMakeRoads);

    if ObjectIsChopableTree(aLoc, [caAge1, caAge2, caAge3, caAgeFull]) then
      AddPassability(tpCutTree);

    if TileIsWater(aLoc)
      and (Land^[aLoc.Y,aLoc.X].TileLock in [tlNone, tlStructure])
      and gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].Walkable
      and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
      and ((Land^[aLoc.Y,aLoc.X].Ware.C < 5) or (Land^[aLoc.Y,aLoc.X].Ware.W = 0))
      and CheckHeightPass(aLoc, hpFish) then
      AddPassability(tpFish);

    if HasPassability(tpWalk)
      and TileHasTerrainKinds(aLoc.X, aLoc.Y, [tkCoastSand, tkSand, tkGrassSand3, tkBarrenLand])
      and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
      //TileLock checked in outer begin/end
      and not (Land^[aLoc.Y,aLoc.X].TileOverlay in ROAD_LIKE_OVERLAYS)
      and not (Land^[aLoc.Y,aLoc.X].TileOverlay2 <> toNone)
      and not TileIsCornField(aLoc)
      and not TileIsWineField(aLoc)
      and not TileIsGrassField(aLoc)
      and not TileIsVegeField(aLoc)
      and not TileHasWater(aLoc)
      and CheckHeightPass(aLoc, hpWalking) then //Can't crab on houses, fields and roads (can walk on fenced house so you can't kill them by placing a house on top of them)
      AddPassability(tpCrab);

    if TileIsSoil(aLoc.X,aLoc.Y)
      and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
      //TileLock checked in outer begin/end
      //Wolf are big enough to run over roads, right?
      and not TileIsCornField(aLoc)
      and not TileIsWineField(aLoc)
      and not TileIsGrassField(aLoc)
      and not TileIsVegeField(aLoc)
      and not (Land^[aLoc.Y,aLoc.X].TileOverlay in ROAD_LIKE_OVERLAYS)
      and gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].Walkable
      and ((Land^[aLoc.Y,aLoc.X].Ware.C < 5) or (Land^[aLoc.Y,aLoc.X].Ware.W = 0))
      and CheckHeightPass(aLoc, hpWalking) then
      AddPassability(tpWolf);

    if HasPassability(tpWalk)
      and not TileIsCornField(aLoc)
      and not TileIsWineField(aLoc)
      and not TileIsGrassField(aLoc)
      and not TileIsVegeField(aLoc)
      and not TileHasWater(aLoc)
      and TileHasTerrainKinds(aLoc.X, aLoc.Y, [tkSnow, tkSnowOnDirt, tkSnowOnGrass, tkDeepSnow])
      and not (Land^[aLoc.Y,aLoc.X].TileOverlay in ROAD_LIKE_OVERLAYS)
      and gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].Walkable then
      AddPassability(tpPolarBear);

    if HasPassability(tpWalk)
      and not TileIsCornField(aLoc)
      and not TileIsWineField(aLoc)
      and not TileIsGrassField(aLoc)
      and not TileIsVegeField(aLoc)
      and not TileHasWater(aLoc)
      and TileHasTerrainKinds(aLoc.X, aLoc.Y, [tkDirt, tkGrassDirt, tkPaleGrass, tkSnowOnDirt, tkGravel, tkCoal])
      and not (Land^[aLoc.Y,aLoc.X].TileOverlay in ROAD_LIKE_OVERLAYS)
      and gRes.Tileset[TILE_OVERLAY_IDS[Land^[aLoc.Y,aLoc.X].TileOverlay2]].Walkable then
      AddPassability(tpFox);
  end;

  if TileIsWalkable(aLoc)
    and not gMapElements[Land^[aLoc.Y,aLoc.X].Obj].AllBlocked
    and CheckHeightPass(aLoc, hpWalking)
    and not (Land^[aLoc.Y,aLoc.X].TileLock in [tlHouse, tlWall]) then
    AddPassability(tpWorker);

  //Check all 4 corners ter kinds around vertice 
  if VerticeIsFactorable(aLoc) then
    AddPassability(tpFactor);

  //Check for houses around this vertice(!)
  //Use only with CanElevate since it's vertice-based!
  housesNearVertex := False;
  for I := -1 to 0 do
    for K := -1 to 0 do
      if TileInMapCoords(aLoc.X+K, aLoc.Y+I) then
        //Can't elevate built houses, can elevate fenced and dug houses though
        if (Land^[aLoc.Y+I,aLoc.X+K].TileLock in [tlHouse]) then
          housesNearVertex := True;

  if VerticeInMapCoords(aLoc.X,aLoc.Y)
  and not housesNearVertex then
    AddPassability(tpElevate);
end;


// Find closest passable point to TargetPoint within line segment OriginPoint <-> TargetPoint
// MaxDistance - maximum distance between found point and origin point. MaxDistance = -1 means there is no distance restriction
function TKMTerrain.GetPassablePointWithinSegment(aOriginPoint, aTargetPoint: TKMPoint; aPass: TKMTerrainPassability;
  aMaxDistance: Integer = -1): TKMPoint;

  function IsDistBetweenPointsAllowed(const aOriginPoint, aTargetPoint: TKMPoint; aMaxDistance: Integer): Boolean; inline;
  begin
    Result := (aMaxDistance = -1) or (KMDistanceSqr(aOriginPoint, aTargetPoint) <= Sqr(aMaxDistance));
  end;

var
  normVector: TKMPoint;
  normDistance: Integer;
begin
  if aMaxDistance = -1 then
    normDistance := Floor(KMLength(aOriginPoint, aTargetPoint))
  else
    normDistance := Min(aMaxDistance, Floor(KMLength(aOriginPoint, aTargetPoint)));

  while (normDistance >= 0)
  and (not IsDistBetweenPointsAllowed(aOriginPoint, aTargetPoint, aMaxDistance)
       or not CheckPassability(aTargetPoint, aPass)) do
  begin
    normVector := KMNormVector(KMPoint(aTargetPoint.X - aOriginPoint.X, aTargetPoint.Y - aOriginPoint.Y), normDistance);
    aTargetPoint := KMPoint(aOriginPoint.X + normVector.X, aOriginPoint.Y + normVector.Y);
    Dec(normDistance);
  end;
  Result := aTargetPoint;
end;


function TKMTerrain.CheckPassability(X, Y: Integer; aPass: TKMTerrainPassability): Boolean;
begin
  Result := TileInMapCoords(X, Y) and (aPass in Land^[Y, X].Passability);
end;


function TKMTerrain.CheckPassability(const aLoc: TKMPoint; aPass: TKMTerrainPassability): Boolean;
begin
  Result := TileInMapCoords(aLoc.X,aLoc.Y) and (aPass in Land^[aLoc.Y,aLoc.X].Passability);
end;


function TKMTerrain.HasUnit(const aLoc: TKMPoint): Boolean;
begin
  Assert(TileInMapCoords(aLoc.X,aLoc.Y));
  Result := Land^[aLoc.Y,aLoc.X].IsUnit <> nil;
end;


function TKMTerrain.HasVertexUnit(const aLoc: TKMPoint): Boolean;
begin
  Assert(TileInMapCoords(aLoc.X,aLoc.Y));
  Result := Land^[aLoc.Y,aLoc.X].IsVertexUnit <> vuNone;
end;


//Check which road connect ID the tile has (to which road network does it belongs to)
function TKMTerrain.GetRoadConnectID(const aLoc: TKMPoint): Byte;
begin
  Result := GetConnectID(wcRoad, aLoc);
end;


//Check which walk connect ID the tile has (to which walk network does it belongs to)
function TKMTerrain.GetWalkConnectID(const aLoc: TKMPoint): Byte;
begin
  Result := GetConnectID(wcWalk, aLoc);
end;


function TKMTerrain.GetConnectID(aWalkConnect: TKMWalkConnect; const Loc: TKMPoint): Byte;
begin
  if TileInMapCoords(Loc.X,Loc.Y) then
    Result := Land^[Loc.Y,Loc.X].WalkConnect[aWalkConnect]
  else
    Result := 0; //No network
end;


function TKMTerrain.CheckAnimalIsStuck(const aLoc: TKMPoint; aPass: TKMTerrainPassability; aCheckUnits: Boolean = True): Boolean;
var
  I,K: integer;
begin
  Result := True; //Assume we are stuck
  for I := -1 to 1 do for K := -1 to 1 do
    if (I <> 0) or (K <> 0) then
      if TileInMapCoords(aLoc.X+K, aLoc.Y+I) then
        if CanWalkDiagonally(aLoc, aLoc.X+K, aLoc.Y+I) then
          if (Land^[aLoc.Y+I,aLoc.X+K].IsUnit = nil) or (not aCheckUnits) then
            if aPass in Land^[aLoc.Y+I,aLoc.X+K].Passability then
              Exit(False); // At least one tile is empty, so unit is not stuck
end;


// Return random tile surrounding Loc with aPass property. PusherLoc is the unit that pushed us
// which is preferable to other units (otherwise we can get two units swapping places forever)
function TKMTerrain.GetOutOfTheWay(aUnit: Pointer; const aPusherLoc: TKMPoint; aPass: TKMTerrainPassability; aPusherWasPushed: Boolean = False): TKMPoint;
var
  U: TKMUnit;
  loc: TKMPoint;

  function GoodForBuilder(X,Y: Word): Boolean;
  var
    distNext: Single;
  begin
    distNext := gHands.DistanceToEnemyTowers(KMPoint(X,Y), U.Owner);
    Result := (distNext > RANGE_WATCHTOWER_MAX)
      or (distNext >= gHands.DistanceToEnemyTowers(loc, U.Owner));
  end;
var
  I, K: Integer;
  tx, ty: Integer;
  isFree, isOffroad, isPushable, exchWithPushedPusher, exchWithPushedPusherChoosen: Boolean;
  newWeight, bestWeight: Single;
  tempUnit: TKMUnit;
begin
  U := TKMUnit(aUnit);
  loc := U.Position;

  Result := loc;
  bestWeight := -1e30;
  exchWithPushedPusherChoosen := False;

  // Check all available walkable positions except self
  for I := -1 to 1 do for K := -1 to 1 do
  if (I <> 0) or (K <> 0) then
    begin
      tx := loc.X + K;
      ty := loc.Y + I;

      if TileInMapCoords(tx, ty)
        and CanWalkDiagonally(loc, tx, ty) //Check for trees that stop us walking on the diagonals!
        and (Land^[ty,tx].TileLock in [tlNone, tlFenced, tlWallFence])
        and (aPass in Land^[ty,tx].Passability)
        and (not (U is TKMUnitWorker) or GoodForBuilder(tx, ty)) then
      begin
        // Try to be pushed to empty tiles
        isFree := Land^[ty, tx].IsUnit = nil;

        // Try to be pushed out to non-road tiles when possible
        isOffroad := False;//not TileHasRoad(tx, ty);

        // Try to be pushed to exchange with pusher or to push other non-locked units
        isPushable := False;
        exchWithPushedPusher := False;
        if Land^[ty, tx].IsUnit <> nil then
        begin
          tempUnit := UnitsHitTest(tx, ty);
          // Always include the pushers loc in the possibilities, otherwise we can get two units swapping places forever
          if (KMPoint(tx, ty) = aPusherLoc) then
          begin
            //Check if we try to exchange with pusher, who was also pushed (that is non-profitable exchange)
            //We want to avoid it
            if aPusherWasPushed then
              exchWithPushedPusher := True //Mark that tile to exchange with pusher
            else
              isPushable := True;
          end
          else
            if ((tempUnit <> nil) and (tempUnit.Action is TKMUnitActionStay)
              and (not TKMUnitActionStay(tempUnit.Action).Locked)) then
              isPushable := True;
        end;
        newWeight := 40*Ord(isFree)
                      + Ord(isOffroad)
                      + Ord(isPushable)
                      - 0.3*Land^[ty,tx].JamMeter
                      + 2*KaMRandom('TKMTerrain.GetOutOfTheWay');

        if newWeight > bestWeight then
        begin
          bestWeight := newWeight;
          Result := KMPoint(tx, ty);
          exchWithPushedPusherChoosen := exchWithPushedPusher;
        end;
      end;
    end;
  //Punish very bad positions, where we decided to exchange with pushed pusher's loc
  //(non-profitable exchange was choosen as the only possibility), so we will mark this pos as very unpleasant
  if exchWithPushedPusherChoosen then
    Land^[loc.Y, loc.X].IncJamMeter(50);
end;


function TKMTerrain.FindSideStepPosition(const aLoc, aLoc2, aLoc3: TKMPoint; aPass: TKMTerrainPassability; out aSidePoint: TKMPoint; aOnlyTakeBest: Boolean): Boolean;
var
  I, K: Integer;
  listAll, listBest: TKMPointList;
begin
  listAll := TKMPointList.Create; //List 1 holds all positions next to both aLoc and aLoc2
  listBest := TKMPointList.Create; // List 2 holds the best positions, ones which are also next to aLoc3 (next position)
  try
    for I := -1 to 1 do
    for K := -1 to 1 do
      if ((I <> 0) or (K <> 0))
      and TileInMapCoords(aLoc.X+K, aLoc.Y+I)
      and not KMSamePoint(KMPoint(aLoc.X+K, aLoc.Y+I), aLoc2)
      and (aPass in Land^[aLoc.Y+I, aLoc.X+K].Passability)
      and CanWalkDiagonally(aLoc, aLoc.X+K, aLoc.Y+I) // Check for trees that stop us walking on the diagonals!
      and (Land^[aLoc.Y+I,aLoc.X+K].TileLock in [tlNone, tlFenced, tlWallFence])
      and (KMLengthDiag(aLoc.X+K, aLoc.Y+I, aLoc2) <= 1) // Right next to aLoc2 (not diagonal)
      and not HasUnit(KMPoint(aLoc.X+K, aLoc.Y+I)) then // Doesn't have a unit
        listAll.Add(KMPoint(aLoc.X+K, aLoc.Y+I));

    // Pick best, if aLoc3 was given
    if not KMSamePoint(aLoc3, KMPOINT_ZERO) then
    for I := 0 to listAll.Count - 1 do
      if KMLengthDiag(listAll[I], aLoc3) < 1.5 then // Next to aLoc3 (diagonal is ok)
        listBest.Add(listAll[I]);

    Result := True;
    if not listBest.GetRandom(aSidePoint) then
      if aOnlyTakeBest or not listAll.GetRandom(aSidePoint) then
        Result := False; // No side step positions available
  finally
    listAll.Free;
    listBest.Free;
  end;
end;


function TKMTerrain.RouteCanBeMade(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability): Boolean;
var
  WC: TKMWalkConnect;
begin
  case aPass of
    tpWalk:      WC := wcWalk;
    tpWalkRoad:  WC := wcRoad;
    tpFish:      WC := wcFish;
    tpWorker:    WC := wcWork;
    else Exit(False);
  end;

  Result :=     CheckPassability(aLocA, aPass)
            and CheckPassability(aLocB, aPass)
            and (Land[aLocA.Y,aLocA.X].WalkConnect[WC] = Land[aLocB.Y,aLocB.X].WalkConnect[WC]);
end;


//Test wherever it is possible to make the route without actually making it to save performance
function TKMTerrain.RouteCanBeMade(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean;
var
  I, K: Integer;
  tstRad1, tstRad2: Boolean;
  distanceSqr: Single;
  WC: TKMWalkConnect;
  x1, x2, y1, y2: Integer;
begin
  // Target could be same point as a source (we dont care)
  // Source point has to be walkable
  Result := CheckPassability(aLocA, aPass);

  if not Result then
    Exit;

  case aPass of
    tpWalk:      WC := wcWalk;
    tpWalkRoad:  WC := wcRoad;
    tpFish:      WC := wcFish;
    tpWorker:    WC := wcWork;
    else Exit;
  end;

  if aDistance = 0 then
    Exit(CheckPassability(aLocB, aPass) and (Land[aLocA.Y,aLocA.X].WalkConnect[WC] = Land[aLocB.Y,aLocB.X].WalkConnect[WC]));

  // Target has to be walkable within Distance
  tstRad1 := False;
  tstRad2 := False;
  distanceSqr := Sqr(aDistance);

  y1 := Max(Round(aLocB.Y - aDistance), 1);
  y2 := Min(Round(aLocB.Y + aDistance), fMapY - 1);
  x1 := Max(Round(aLocB.X - aDistance), 1);
  x2 := Min(Round(aLocB.X + aDistance), fMapX - 1);

  // Walkable way between A and B is proved by FloodFill
  for I := y1 to y2 do
    for K := x1 to x2 do
      if KMLengthSqr(aLocB.X, aLocB.Y, K, I) <= distanceSqr then
      begin
        tstRad1 := tstRad1 or CheckPassability(K, I, aPass);
        tstRad2 := tstRad2 or (Land[aLocA.Y,aLocA.X].WalkConnect[WC] = Land[I,K].WalkConnect[WC]);
      end;

  Result := Result and tstRad1 and tstRad2;
end;


//Check if a route can be made to this vertex, from any direction (used for woodcutter cutting trees)
function TKMTerrain.RouteCanBeMadeToVertex(const aLocA, aLocB: TKMPoint; aPass: TKMTerrainPassability): Boolean;
var
  I, K: Integer;
begin
  Result := False;
  //Check from top-left of vertex to vertex tile itself
  for I := Max(aLocB.Y - 1, 1) to aLocB.Y do
    for K := Max(aLocB.X - 1, 1) to aLocB.X do
      Result := Result or RouteCanBeMade(aLocA, KMPoint(K, I), aPass);
end;


//Returns the closest tile to TargetLoc with aPass and walk connect to OriginLoc
//If no tile found - return Origin location
function TKMTerrain.GetClosestTile(const aTargetLoc, aOriginLoc: TKMPoint; aPass: TKMTerrainPassability; aAcceptTargetLoc: Boolean): TKMPoint;
const
  TEST_DEPTH = 255;
var
  I, walkConnectID: Integer;
  P: TKMPoint;
  T: TKMPoint;
  wcType: TKMWalkConnect;
begin
  case aPass of
    tpWalkRoad: wcType := wcRoad;
    tpFish:     wcType := wcFish;
    else         wcType := wcWalk; //CanWalk is default
  end;

  walkConnectID := Land^[aOriginLoc.Y,aOriginLoc.X].WalkConnect[wcType]; //Store WalkConnect ID of origin

  //If target is accessable then use it
  if aAcceptTargetLoc and CheckPassability(aTargetLoc, aPass) and (walkConnectID = Land^[aTargetLoc.Y,aTargetLoc.X].WalkConnect[wcType]) then
  begin
    Result := aTargetLoc;
    exit;
  end;

  //If target is not accessable then choose a tile near to the target that is accessable
  //As we Cannot reach our destination we are "low priority" so do not choose a tile with another unit on it (don't bump important units)
  for I := 0 to TEST_DEPTH do begin
    P := GetPositionFromIndex(aTargetLoc, I);
    if not TileInMapCoords(P.X,P.Y) then Continue;
    T := KMPoint(P.X,P.Y);
    if CheckPassability(T, aPass)
      and (walkConnectID = Land^[T.Y,T.X].WalkConnect[wcType])
      and (not HasUnit(T) or KMSamePoint(T,aOriginLoc)) //Allow position we are currently on, but not ones with other units
    then
      Exit(T); //Assign if all test are passed
  end;

  Result := aOriginLoc; //If we don't find one, return existing Loc
end;


function TKMTerrain.GetClosestRoad(const aFromLoc: TKMPoint; aWalkConnectIDSet: TKMByteSet; aPass: TKMTerrainPassability = tpWalkRoad): TKMPoint;
const
  DEPTH = 255;
var
  I: Integer;
  P: TKMPoint;
  wcType: TKMWalkConnect;
begin
  Result := KMPOINT_INVALID_TILE;

  case aPass of
    tpWalkRoad: wcType := wcRoad;
    tpFish:     wcType := wcFish;
    else        wcType := wcWalk; //CanWalk is default
  end;

  for I := 0 to DEPTH do
  begin
    P := GetPositionFromIndex(aFromLoc, I);
    if not TileInMapCoords(P.X,P.Y) then Continue;
    if CheckPassability(P, aPass)
      and (Land^[P.Y,P.X].WalkConnect[wcType] in aWalkConnectIDSet)
      and RouteCanBeMade(aFromLoc, P, tpWalk)
    then
    begin
      Result := P; //Assign if all test are passed
      Exit;
    end;
  end;
end;


// Mark tile as occupied
procedure TKMTerrain.UnitAdd(const aLocTo: TKMPoint; aUnit: Pointer);
begin
  if not DO_UNIT_INTERACTION then Exit;

  Assert(Land^[aLocTo.Y,aLocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(aLocTo));
  Land^[aLocTo.Y,aLocTo.X].IsUnit := aUnit
end;


// Mark tile as empty
// We have no way of knowing whether a unit is inside a house, or several units exit a house at once
// when exiting the game and destroying all units this will cause asserts.
procedure TKMTerrain.UnitRem(const aLocFrom: TKMPoint);
begin
  if not DO_UNIT_INTERACTION then Exit;

  Land^[aLocFrom.Y,aLocFrom.X].IsUnit := nil;
end;


// Mark previous tile as empty and next one as occupied
//We need to check both tiles since UnitWalk is called only by WalkTo where both tiles aren't houses
procedure TKMTerrain.UnitWalk(const aLocFrom, aLocTo: TKMPoint; aUnit: Pointer);
var
  U: TKMUnit;
begin
  if not DO_UNIT_INTERACTION then Exit;
  Assert(Land^[aLocFrom.Y, aLocFrom.X].IsUnit = aUnit, 'Trying to remove wrong unit at '+TypeToString(aLocFrom));
  Land^[aLocFrom.Y, aLocFrom.X].IsUnit := nil;
  Assert(Land^[aLocTo.Y, aLocTo.X].IsUnit = nil, 'Tile already occupied at '+TypeToString(aLocTo));
  Land^[aLocTo.Y, aLocTo.X].IsUnit := aUnit;

  U := TKMUnit(aUnit);
  if ((U <> nil) and (U is TKMUnitWarrior)) then
    gScriptEvents.ProcWarriorWalked(U, aLocTo.X, aLocTo.Y);
end;


procedure TKMTerrain.UnitSwap(const aLocFrom,aLocTo: TKMPoint; aUnitFrom: Pointer);
begin
  Assert(Land^[aLocFrom.Y,aLocFrom.X].IsUnit = aUnitFrom, 'Trying to swap wrong unit at '+TypeToString(aLocFrom));
  Land[aLocFrom.Y,aLocFrom.X].IsUnit := Land^[aLocTo.Y,aLocTo.X].IsUnit;
  Land^[aLocTo.Y,aLocTo.X].IsUnit := aUnitFrom;
end;


// Mark vertex as occupied
procedure TKMTerrain.UnitVertexAdd(const aLocTo: TKMPoint; Usage: TKMVertexUsage);
begin
  if not DO_UNIT_INTERACTION then exit;
  Assert(Usage <> vuNone, 'Invalid add vuNone at '+TypeToString(aLocTo));
  Assert((Land[aLocTo.Y,aLocTo.X].IsVertexUnit = vuNone) or (Land^[aLocTo.Y,aLocTo.X].IsVertexUnit = Usage),'Opposite vertex in use at '+TypeToString(aLocTo));

  Land^[aLocTo.Y,aLocTo.X].IsVertexUnit := Usage;
end;


procedure TKMTerrain.UnitVertexAdd(const aLocFrom, aLocTo: TKMPoint);
begin
  Assert(KMStepIsDiag(aLocFrom, aLocTo), 'Add non-diagonal vertex?');
  UnitVertexAdd(KMGetDiagVertex(aLocFrom, aLocTo), GetVertexUsageType(aLocFrom, aLocTo));
end;


// Mark vertex as empty
procedure TKMTerrain.UnitVertexRem(const aLocFrom: TKMPoint);
begin
  if not DO_UNIT_INTERACTION then exit;
  Land^[aLocFrom.Y,aLocFrom.X].IsVertexUnit := vuNone;
end;


//This function tells whether the diagonal is "in use". (a bit like IsUnit) So if there is a unit walking on
//the oppsoite diagonal you cannot use the vertex (same diagonal is allowed for passing and fighting)
//It stops units walking diagonally through each other or walking through a diagonal that has weapons swinging through it
function TKMTerrain.VertexUsageCompatible(const aLocFrom, aLocTo: TKMPoint): Boolean;
var
  vert: TKMPoint;
  vertUsage: TKMVertexUsage;
begin
  Assert(KMStepIsDiag(aLocFrom, aLocTo));
  vert := KMGetDiagVertex(aLocFrom, aLocTo);
  vertUsage := GetVertexUsageType(aLocFrom, aLocTo);
  Result := (Land^[vert.Y, vert.X].IsVertexUnit in [vuNone, vertUsage]);
end;


function TKMTerrain.GetVertexUsageType(const aLocFrom, aLocTo: TKMPoint): TKMVertexUsage;
var
  dx, dy: Integer;
begin
  dx := aLocFrom.X - aLocTo.X;
  dy := aLocFrom.Y - aLocTo.Y;
  Assert((Abs(dx) = 1) and (Abs(dy) = 1));
  if (dx*dy = 1) then Result := vuNWSE
                 else Result := vuNESW;
end;


//todo: Rewrite into controlled recursion to avoid StackOverflows
//@Krom: Stackoverflow usually occurs because keeping mountain walkable with stonemining is
//       sometimes impossible to solve when considering CanElevate (houses near stone).
//       So changing recursion to iteration would just give us an infinite loop in that case :(
//       I've added aIgnoreCanElevate for stonemining only, which means land under houses
//       gets elevated but stops crashes (tested on multiple r5503 crash reports) since
//       now it is possible to keep all tiles walkable by repeatedly flattening.
//       I can't think of a flattening algo that maintains walkability AND CanElevate constraint.
//       It needs major rethinking, rewriting recursion won't solve it.
//Interpolate between 12 vertices surrounding this tile (X and Y, no diagonals)
//Also it is FlattenTerrain duty to preserve walkability if there are units standing
//aIgnoreCanElevate ignores CanElevate constraint which prevents crashes during stonemining (hacky)
procedure TKMTerrain.DoFlattenTerrain(const aLoc: TKMPoint; var aDepth: Byte; aUpdateWalkConnects: Boolean; aIgnoreCanElevate: Boolean; aFactor : Single = 0.5);
const
  // Max depth of recursion for flatten algorythm to use tpElevate as a restriction
  // After depth goes beyond this value we omit tpElevate restriction and allow to change tiles height under any houses
  // Its needed, because there is still a possibility to get into infinite recursion loop EnsureRange -> DoFlattenTerrain -> EnsureRange -> ...
  FLATTEN_RECUR_USE_ELEVATE_MAX_DEPTH = 16;

  // Max depth of recursion
  // Its quite unlikely, but its possible in thory that we will get into infinite recursion even without tpElevate restriction
  // Limit max number of attempts to Flatten terrain to keep only walkable tiles, to avoid StackOverflow
  FLATTEN_RECUR_MAX_DEPTH = 32;

  //If tiles with units standing on them become unwalkable we should try to fix them
  procedure EnsureWalkable(aX,aY: Word; var aDepth: Byte);
  begin
    //We did not recalculated passability yet, hence tile has CanWalk but CheckHeightPass=False already
    if (tpWalk in Land^[aY,aX].Passability)
    //Yield in TestStone is much better if we comment this out, also general result is flatter/"friendlier"
    //and (Land^[aY,aX].IsUnit <> nil)
    and not CheckHeightPass(KMPoint(aX,aY), hpWalking)
    and not fMapEditor //Allow units to become "stuck" in MapEd, as height changing is allowed anywhere
    then
      //This recursive call should be garanteed to exit, as eventually the terrain will be flat enough
      DoFlattenTerrain(KMPoint(aX,aY), aDepth, False, aIgnoreCanElevate or (aDepth > FLATTEN_RECUR_USE_ELEVATE_MAX_DEPTH)); //WalkConnect should be done at the end
  end;

  function CanElevateAt(aX, aY: Word): Boolean;
  begin
    //Passability does not get set for the row below the bottom/right edges
    Result := aIgnoreCanElevate or (tpElevate in Land^[aY, aX].Passability) or (aX = fMapX) or (aY = fMapY);
  end;

var
  vertsFactored: Integer;

  //Note that we need to access vertices, not tiles
  function GetHeight(aX,aY: Word; Neighbour: Boolean): Byte;
  begin
    if VerticeInMapCoords(aX,aY) and (not Neighbour or (tpFactor in Land^[aY,aX].Passability)) then
    begin
      Result := Land^[aY,aX].Height;
      Inc(vertsFactored);
    end
    else
      Result := 0;
  end;

var
  I, K: Word;
  avg: Word;
begin
  Assert(TileInMapCoords(aLoc.X, aLoc.Y), 'Can''t flatten tile outside map coordinates');

  Inc(aDepth); //Increase depth

  // Stop flattening after a certain point.
  // Give up on flatten terrain to keep all around tiles walkable if its impossible (or we failed after number of attempts)
  if aDepth > FLATTEN_RECUR_MAX_DEPTH then
    Exit;

  if aUpdateWalkConnects then
    fBoundsWC := KMRect(aLoc.X, aLoc.Y, aLoc.X, aLoc.Y);

  //Expand fBoundsWC in case we were called by EnsureWalkable, and fBoundsWC won't know about this tile
  if fBoundsWC.Left > aLoc.X - 1 then fBoundsWC.Left := aLoc.X - 1;
  if fBoundsWC.Top > aLoc.Y - 1 then fBoundsWC.Top := aLoc.Y - 1;
  if fBoundsWC.Right < aLoc.X + 1 then fBoundsWC.Right := aLoc.X + 1;
  if fBoundsWC.Bottom < aLoc.Y + 1 then fBoundsWC.Bottom := aLoc.Y + 1;

  vertsFactored := 0; //GetHeight will add to this
  avg :=                                   GetHeight(aLoc.X,aLoc.Y-1,True ) + GetHeight(aLoc.X+1,aLoc.Y-1,True ) +
         GetHeight(aLoc.X-1,aLoc.Y  ,True) + GetHeight(aLoc.X,aLoc.Y  ,False) + GetHeight(aLoc.X+1,aLoc.Y  ,False) + GetHeight(aLoc.X+2,aLoc.Y  ,True) +
         GetHeight(aLoc.X-1,aLoc.Y+1,True) + GetHeight(aLoc.X,aLoc.Y+1,False) + GetHeight(aLoc.X+1,aLoc.Y+1,False) + GetHeight(aLoc.X+2,aLoc.Y+1,True) +
                                           GetHeight(aLoc.X,aLoc.Y+2,True ) + GetHeight(aLoc.X+1,aLoc.Y+2,True );
  Assert(vertsFactored <> 0); //Non-neighbour verts will always be factored
  avg := Round(avg / vertsFactored);

  // X, Y
  if CanElevateAt(aLoc.X  , aLoc.Y) then
  begin
    Land[aLoc.Y, aLoc.X  ].Height := Mix(avg, Land^[aLoc.Y  ,aLoc.X  ].Height, aFactor);
    UpdateRenderHeight(aLoc.X, aLoc.Y);
  end;
  // X + 1, Y
  if CanElevateAt(aLoc.X + 1, aLoc.Y) then
  begin
    Land[aLoc.Y, aLoc.X+1].Height := Mix(avg, Land^[aLoc.Y  ,aLoc.X+1].Height, aFactor);
    UpdateRenderHeight(aLoc.X + 1, aLoc.Y);
  end;
  // X, Y + 1
  if CanElevateAt(aLoc.X, aLoc.Y + 1) then
  begin
    Land[aLoc.Y + 1, aLoc.X].Height := Mix(avg, Land^[aLoc.Y + 1, aLoc.X].Height, aFactor);
    UpdateRenderHeight(aLoc.X, aLoc.Y + 1);
  end;
  // X + 1, Y + 1
  if CanElevateAt(aLoc.X + 1, aLoc.Y + 1) then
  begin
    Land[aLoc.Y + 1 ,aLoc.X + 1].Height := Mix(avg, Land^[aLoc.Y + 1, aLoc.X + 1].Height, aFactor);
    UpdateRenderHeight(aLoc.X + 1, aLoc.Y + 1);
  end;



  //All 9 tiles around and including this one could have become unwalkable and made a unit stuck, so check them all
  for I := Max(aLoc.Y-1, 1) to Min(aLoc.Y+1, fMapY-1) do
    for K := Max(aLoc.X-1, 1) to Min(aLoc.X+1, fMapX-1) do
      EnsureWalkable(K, I, aDepth);

  UpdateLighting(KMRect(aLoc.X-2, aLoc.Y-2, aLoc.X+3, aLoc.Y+3));
  //Changing height will affect the cells around this one
  UpdatePassability(KMRectGrow(KMRect(aLoc), 1));

  if aUpdateWalkConnects then
    UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrow(fBoundsWC, 1), False);
end;


//Flatten terrain loc
procedure TKMTerrain.FlattenTerrain(const Loc: TKMPoint; aUpdateWalkConnects: Boolean = True; aIgnoreCanElevate: Boolean = False; aFactor : Single = 0.5);
var
  depth: Byte;
begin
  depth := 0;
  DoFlattenTerrain(Loc, depth, aUpdateWalkConnects, aIgnoreCanElevate, aFactor);
end;


//Flatten a list of points on mission init
procedure TKMTerrain.FlattenTerrain(LocList: TKMPointList);
var
  I: Integer;
begin
  //Flatten terrain will extend fBoundsWC as necessary, which cannot be predicted due to EnsureWalkable effecting a larger area
  if not LocList.GetBounds(fBoundsWC) then
    Exit;

  for I := 0 to LocList.Count - 1 do
    FlattenTerrain(LocList[I], False); //Rebuild the Walk Connect at the end, rather than every time

  //wcFish not affected by height
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRectGrow(fBoundsWC, 1), False);
end;


procedure TKMTerrain.UpdateRenderHeight;
begin
  UpdateRenderHeight(fMapRect);
end;


procedure TKMTerrain.UpdateRenderHeight(const aRect: TKMRect);
var
  I, K: Integer;
begin
  //Valid vertices are within 1..Map
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX) do
      UpdateRenderHeight(K, I, False);

  UpdateTopHill;
end;


procedure TKMTerrain.UpdateRenderHeight(X, Y: Integer; aUpdateTopHill: Boolean = True);
begin
  LandExt[Y, X].RenderHeight := Land^[Y, X].GetRenderHeight;

  if not aUpdateTopHill then Exit;

  // Update only for top terrain rows
  if Y > Ceil(HEIGHT_MAX / CELL_SIZE_PX) then Exit;

  if fMapEditor then
    UpdateTopHill // Full scan first map rows to get exact TopHill
  else
    UpdateTopHill(X, Y); // Only increase TopHill if needed in the game
end;


procedure TKMTerrain.UpdateLighting;
begin
  UpdateLighting(fMapRect);
end;


//Rebuilds lighting values for given bounds.
//These values are used to draw highlights/shadows on terrain
//Note that input values may be off-map
procedure TKMTerrain.UpdateLighting(const aRect: TKMRect);
var
  I, K: Integer;
begin
  //Valid vertices are within 1..Map
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX) do
      UpdateLighting(K, I);
end;


procedure TKMTerrain.UpdateLighting(X, Y: Integer);

  function ConvertLightToByte(aSLight: Single): Byte;
  begin
    Result := Round((aSLight + 1) * 127.5);
  end;

var
  x0, y2: Integer;
  sLight, sLightWater: Single;
begin
  //Map borders always fade to black
  if (Y = 1) or (Y = fMapY) or (X = 1) or (X = fMapX) then
    Land^[Y,X].Light := 0
  else
  begin
    x0 := Max(X - 1, 1);
    y2 := Min(Y + 1, fMapY);

    sLight := EnsureRange((LandExt^[Y,X].RenderHeight - (LandExt^[y2,X].RenderHeight + LandExt^[Y,x0].RenderHeight)/2)/22, -1, 1); // 1.33*16 ~=22.
    //Use more contrast lighting for Waterbeds
    if fTileset[Land^[Y, X].BaseLayer.Terrain].Water then
    begin
      sLightWater := EnsureRange(sLight*WATER_LIGHT_MULTIPLIER + 0.1, -1, 1);
      Land^[Y,X].Light := ConvertLightToByte(sLightWater);
    end
    else
      Land^[Y,X].Light := ConvertLightToByte(sLight); //  1.33*16 ~=22.
  end;

  LandExt[Y,X].RenderLight := Land^[Y,X].GetRenderLight;
end;


//Rebuilds passability for all map
procedure TKMTerrain.UpdatePassability;
begin
  UpdatePassability(fMapRect);
end;


//Rebuilds passability for given bounds
procedure TKMTerrain.UpdatePassability(const aRect: TKMRect);
var
  I, K: Integer;
begin
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY - 1) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX - 1) do
      UpdatePassability(KMPoint(K, I));
end;


//Rebuilds connected areas using flood fill algorithm
procedure TKMTerrain.UpdateWalkConnect(const aSet: TKMWalkConnectSet; aRect: TKMRect; aDiagObjectsEffected: Boolean);
var
  WC: TKMWalkConnect;
begin
  aRect := KMClipRect(aRect, 1, 1, fMapX - 1, fMapY - 1);

  //Process all items from set
  for WC in aSet do
    TKMTerrainWalkConnect.DoUpdate(aRect, WC, aDiagObjectsEffected);
end;


{Place house plan on terrain and change terrain properties accordingly}
procedure TKMTerrain.SetHouse(const aLoc: TKMPoint; aHouseType: TKMHouseType; aHouseStage: TKMHouseStage; aOwner: TKMHandID; const aFlattenTerrain: Boolean = False);
var
  I, K, X, Y: Word;
  toFlatten: TKMPointList;
  HA: TKMHouseArea;
  objectsEffected: Boolean; //UpdateWalkConnect cares about this for optimisation purposes
begin
  objectsEffected := False;
  if aFlattenTerrain then //We will check aFlattenTerrain only once, otherwise there are compiler warnings
    toFlatten := TKMPointList.Create
  else
    toFlatten := nil;

  if aHouseStage = hsNone then
    SetHouseAreaOwner(aLoc, aHouseType, -1)
  else
    SetHouseAreaOwner(aLoc, aHouseType, aOwner);

  HA := gRes.Houses[aHouseType].BuildArea;

  for I := 1 to 4 do
  for K := 1 to 4 do
    if HA[I,K] <> 0 then
    begin
      X := aLoc.X + K - 3;
      Y := aLoc.Y + I - 4;
      if TileInMapCoords(X,Y) then
      begin
        case aHouseStage of
          hsNone:         Land^[Y,X].TileLock := tlNone;
          hsFence:        If aHouseType in WALL_HOUSES then
                            Land^[Y,X].TileLock := tlWallFence
                          else
                            Land^[Y,X].TileLock := tlFenced; //Initial state, Laborer should assign NoWalk to each tile he digs
          hsBuilt:        begin
                            //Script houses are placed as built, add TileLock for them too
                            If aHouseType in WALL_HOUSES then
                            begin
                              Land^[Y,X].TileLock := tlWall;
                              if  ( HA[I,K] = 3 )  then
                                Land^[Y,X].TileLock := tlWallEmpty;

                              if  not ( HA[I,K] = 3 )  then
                                if (aHouseType in [htWall2, htWall4])then
                                  Land^[Y,X].TileLock := tlWallGate;
                            end
                            else
                              Land^[Y,X].TileLock := tlHouse;

                            if toFlatten <> nil then
                            begin
                              //In map editor don't remove objects (remove on mission load instead)
                              if (Land^[Y,X].Obj <> OBJ_NONE) then
                              begin
                                objectsEffected := objectsEffected or gMapElements[Land^[Y,X].Obj].DiagonalBlocked;
                                Land^[Y,X].Obj := OBJ_NONE;
                              end;
                              //If house was set e.g. in mission file we must flatten the terrain as no one else has
                              toFlatten.Add(KMPoint(X,Y));
                            end;
                          end;
        end;
        UpdateFences(KMPoint(X,Y));
      end;
    end;

  if aHouseType in WALL_HOUSES then
    if aHouseStage = hsNone then
      for I := 1 to 4 do
        for K := 1 to 4 do
          if HA[I,K] <> 0 then
          begin
            X := aLoc.X + K - 3;
            Y := aLoc.Y + I - 4;
            if TileInMapCoords(X,Y) then
                UpdatePalisadeAround(KMPoint(X, Y), aOwner);
          end;

  if toFlatten <> nil then
  begin
    FlattenTerrain(toFlatten);
    toFlatten.Free;
  end;

  //Recalculate Passability for tiles around the house so that they can't be built on too
  UpdatePassability(KMRect(aLoc.X - 2 - HOUSE_BLOCK_RADIUS, aLoc.Y - 3 - HOUSE_BLOCK_RADIUS,
                            aLoc.X + 1 + HOUSE_BLOCK_RADIUS, aLoc.Y + HOUSE_BLOCK_RADIUS));
  UpdateWalkConnect([wcWalk, wcRoad, wcWork], KMRect(aLoc.X - 3, aLoc.Y - 4, aLoc.X + 2, aLoc.Y + 1), objectsEffected);
end;

procedure TKMTerrain.SetHouse(aHouse: Pointer; aOwner: ShortInt; aHouseStage : TKMHouseStage; const aFlattenTerrain: Boolean = False);
var
  I, K, X, Y: Word;
  HA: TKMHouseArea;
begin
  HA := gRes.Houses[TKMHouse(aHouse).HouseType].BuildArea;


  for I := 1 to 4 do
  for K := 1 to 4 do
    if HA[I,K] <> 0 then
    begin
      X := TKMHouse(aHouse).Position.X + K - 3;
      Y := TKMHouse(aHouse).Position.Y + I - 4;
      if TileInMapCoords(X,Y) then
      begin
        if aHouseStage <> hsNone then
          Land^[Y, X].IsHouse := aHouse
        else
          Land^[Y, X].IsHouse := nil;
      end;

      //Add road for scipted houses
      if HA[I,K] = 2 then
        if aHouseStage = hsBuilt then
          if TKMHouse(aHouse).PlaceRoad then
            Land^[Y,X].TileOverlay := toRoad;

    end;
  SetHouse(TKMHouse(aHouse).Position, TKMHouse(aHouse).HouseType, aHouseStage, aOwner, aFlattenTerrain);

  if aHouseStage <> hsNone then
    AddLight(TKMHouse(aHouse).Entrance, 5, 100)
  else
    RemLight(TKMHouse(aHouse).Entrance);

end;

{That is mainly used for minimap now}
procedure TKMTerrain.SetHouseAreaOwner(const aLoc: TKMPoint; aHouseType: TKMHouseType; aOwner: TKMHandID);
var
  I, K: Integer;
  HA: TKMHouseArea;
begin
  HA := gRes.Houses[aHouseType].BuildArea;
  case aHouseType of
    htNone:    Land^[aLoc.Y,aLoc.X].TileOwner := aOwner;
    htAny:     ; //Do nothing
    else        for I := 1 to 4 do
                  for K := 1 to 4 do //If this is a house make change for whole place
                    if HA[I,K] <> 0 then
                      if TileInMapCoords(aLoc.X + K - 3, aLoc.Y + I - 4) then
                        Land^[aLoc.Y + I - 4, aLoc.X + K - 3].TileOwner := aOwner;
  end;
end;


{Check if Unit can be placed here}
//Used by MapEd, so we use AllowedTerrain which lets us place citizens off-road
function TKMTerrain.CanPlaceUnit(const aLoc: TKMPoint; aUnitType: TKMUnitType): Boolean;
begin
  Result := TileInMapCoords(aLoc.X, aLoc.Y)
            and (Land^[aLoc.Y, aLoc.X].IsUnit = nil) //Check for no unit below
            and (gRes.Units[aUnitType].AllowedPassability in Land^[aLoc.Y, aLoc.X].Passability);
end;


function TKMTerrain.HousesNearTile(X,Y: Word): Boolean;
var
  I, K: Integer;
begin
  Result := False;
  for I := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    for K := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      if (Land^[Y + I, X + K].TileLock in [tlFenced, tlDigged, tlHouse]) then
        Result := True;
end;


function TKMTerrain.CanPlaceGoldMine(X,Y: Word): Boolean;
begin
  Result := TileGoodForGoldmine(X,Y)
    and ((Land[Y,X].Obj = OBJ_NONE) or (gMapElements[Land^[Y,X].Obj].CanBeRemoved))
    and not HousesNearTile(X,Y)
    and (Land^[Y,X].TileLock = tlNone)
    and CheckHeightPass(KMPoint(X,Y), hpBuildingMines);
end;


//Check that house can be placed on Terrain
//Other checks are performed on Hands level. Of course Terrain is not aware of that
function TKMTerrain.CanPlaceHouse(aLoc: TKMPoint; aHouseType: TKMHouseType): Boolean;
var
  I,K,X,Y: Integer;
  L, M: Integer;
  HA: TKMHouseArea;
begin
  Result := True;

  if aHouseType in [htWall, htWall3, htWall5] then
    Result := Result and CanPlaceWall(aLoc, aHouseType)
  else
  if aHouseType = htWell then
    Result := Result and CanPlaceWell(aLoc)
  else
  if aHouseType = htShipYard then
    Result := Result and CanPlaceShipYard(aLoc.X, aLoc.Y);

  HA := gRes.Houses[aHouseType].BuildArea;
  aLoc.X := aLoc.X - gRes.Houses[aHouseType].EntranceOffsetX; //update offset
  aLoc.Y := aLoc.Y - gRes.Houses[aHouseType].EntranceOffsetY; //update offset
  for I := 1 to 4 do
  for K := 1 to 4 do
    if Result and (HA[I,K] <> 0) then
    begin
      X := aLoc.X + k - 3;
      Y := aLoc.Y + i - 4;
      //Inset one tile from map edges
      Result := Result and TileInMapCoords(X, Y, 1);

      case aHouseType of
        htIronMine: Result := Result and CanPlaceIronMine(X, Y);
        htGoldMine: Result := Result and CanPlaceGoldMine(X, Y);
        htBitinMine: Result := Result and (CanPlaceIronMine(X, Y) or CanPlaceGoldMine(X, Y));
        htAppleTree:  begin
                        {if HA[I,K] = 2 then
                        begin
                          if gMapElements[Land^[Y, X].Obj].IsFruit > 0 then //If there is a fruit tree than we can place it
                            Result := Result and (tpBuildNoObj in Land^[Y,X].Passability)
                          else
                            Result := Result and (tpBuild in Land^[Y,X].Passability);
                          
                        end else
                          Result := Result and (tpBuild in Land^[Y,X].Passability);}
                        Result := (tpBuildNoObj in Land^[Y,X].Passability);

                        if (HA[I,K] = 2) then
                        begin
                          If (gMapElements[Land^[Y,X].Obj].IsFruit = 0) then
                            Result := Result and (gMapElements[Land^[Y,X].Obj].CanBeRemoved or (Land^[Y,X].Obj = OBJ_NONE));
                        end else
                          Result := Result and (gMapElements[Land^[Y,X].Obj].CanBeRemoved or (Land^[Y,X].Obj = OBJ_NONE));

                       // if I = 4 then
                       //   Result := Result and (not House(X, Y - 2).IsValid(htAppleTree) or TKMHouseAppleTree(House(X, Y - 2)).CanAddChildTree(X, Y))
                      end;
        else         Result := Result and (tpBuild in Land^[Y,X].Passability);
      end;


      if aHouseType <> htAppleTree then
        for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
        for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
        if TileInMapCoords(X+M, Y+L) and (Land^[Y+L, X+M].TileLock in [tlFenced,tlDigged,tlHouse]) then
          Result := False;

      //Check surrounding tiles for another house that overlaps
      if not (aHouseType in IGNORE_HOUSE_BLOCK) then
      for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      if TileInMapCoords(X+M, Y+L) and (Land^[Y+L, X+M].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence]) then
        Result := False;

      if aHouseType = htAppleTree then
      begin
        for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
        for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
        if TileInMapCoords(X+M, Y+L)
        and (Land^[Y+L, X+M].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence])
        and House(X+M, Y+L).IsValid(htAppleTree, true) then
            Result := False;
        //and (Land^[TY+L, TX+M].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence])
        //and (not (( House(TX+M, TY+L) is TKMHouseAppleTree) and (TKMHouseAppleTree(House(TX+M, TY+L)).CanAddChildTree))
      end;


    end;
end;


//Simple check if house could exists on terrain, with only boundaries check
//aInsetRect - insetRect for map rect
function TKMTerrain.CheckHouseBounds(aHouseType: TKMHouseType; const aLoc: TKMPoint; aInsetRect: TKMRect): Boolean;
var
  I, K: Integer;
  HA: TKMHouseArea;
  TX, TY: Integer;
  mapHouseInsetRect: TKMRect;
begin
  Result := True;
  HA := gRes.Houses[aHouseType].BuildArea;

  mapHouseInsetRect := KMRect(aInsetRect.Left + 1, aInsetRect.Top + 1, aInsetRect.Right - 1, aInsetRect.Bottom - 1);

  for I := 1 to 4 do
  for K := 1 to 4 do
  if (HA[I,K] <> 0) then
  begin
    TX := aLoc.X + K - 3;
    TY := aLoc.Y + I - 4;
    Result := Result and TileInMapCoords(TX, TY, mapHouseInsetRect); //Inset one tile from map edges
  end;
end;


//Simple checks when placing houses from the script:
function TKMTerrain.CanPlaceHouseFromScript(aHouseType: TKMHouseType; const aLoc: TKMPoint): Boolean;
var
  I, K, L, M: Integer;
  HA: TKMHouseArea;
  TX, TY: Integer;
begin
  Result := True;
  HA := gRes.Houses[aHouseType].BuildArea;

  for I := 1 to 4 do
  for K := 1 to 4 do
  if (HA[I,K] <> 0) then
  begin
    TX := aLoc.X + K - 3;
    TY := aLoc.Y + I - 4;
    Result := Result and TileInMapCoords(TX, TY, 1); //Inset one tile from map edges
    //We don't use CanBuild since you are allowed to place houses from the script over trees but not over units
    Result := Result and TileIsWalkable(KMPoint(TX, TY)); //Tile must be walkable
    Result := Result and not TileIsCornField(KMPoint(TX, TY));
    Result := Result and not TileIsWineField(KMPoint(TX, TY));
    Result := Result and not TileHasPalisade(TX, TY);

    //Mines must be on a mountain edge
    if aHouseType = htIronMine then
      Result := Result and TileGoodForIronMine(TX,TY);
    if aHouseType = htGoldMine then
      Result := Result and TileGoodForGoldMine(TX,TY);
    if aHouseType = htBitinMine then
      Result := Result and (TileGoodForIronMine(TX,TY) or TileGoodForGoldMine(TX,TY));
    if aHouseType <> htAppleTree then
    for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    if TileInMapCoords(TX+M, TY+L) and (Land^[TY+L, TX+M].TileLock in [tlFenced,tlDigged,tlHouse]) then
      Result := False;

    //Check surrounding tiles for another house that overlaps
    if not (aHouseType in IGNORE_HOUSE_BLOCK) then
    for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
    if TileInMapCoords(TX+M, TY+L) and (Land^[TY+L, TX+M].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence]) then
      Result := False;

    if aHouseType = htAppleTree then
    begin

      Result := (tpBuildNoObj in Land^[TY,TX].Passability);

      if I = 4 then
        Result := Result and (not House(TX, TY - 2).IsValid(htAppleTree) or TKMHouseAppleTree(House(TX, TY - 2)).CanAddChildTree(TX, TY));

      if (HA[I,K] = 2) then
      begin
        If (gMapElements[Land^[TY,TX].Obj].IsFruit = 0) then
          Result := Result and (gMapElements[Land^[TY,TX].Obj].CanBeRemoved or (Land^[TY,TX].Obj = OBJ_NONE));
      end else
        Result := Result and (gMapElements[Land^[TY,TX].Obj].CanBeRemoved or (Land^[TY,TX].Obj = OBJ_NONE));

      for L := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      for M := -HOUSE_BLOCK_RADIUS to HOUSE_BLOCK_RADIUS do
      if TileInMapCoords(TX+M, TY+L)
      and (Land^[TY+L, TX+M].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence])
      and House(TX+M, TY+L).IsValid(htAppleTree, true) then
        Result := False;
    end;

    //Check if there are units below placed BEFORE the house is added
    //Units added AFTER the house will be autoplaced around it
    Result := Result and (Land^[TY, TX].IsUnit = nil);

    if not Result then Exit;
  end;
end;

function TKMTerrain.CanPlaceStructure(const aLoc : TKMPoint; aIndex, aRot : Word) : Boolean;

  function CheckForHouses(const aPoint : TKMPoint) : Boolean;
  var
    I, K: Integer;
  begin
    Result := true;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if (I <> 0) or (K <> 0) then //This is a surrounding tile, not the actual tile
          if Land^[aPoint.Y+I,aPoint.X+K].TileLock in [tlFenced,tlDigged,tlHouse, tlWall, tlWallFence] then
            Exit(false);

  end;

var X, Y : Integer;
  P : TKMPoint;
  canBuildBridge, hasAnyWater, isBridge : Boolean;
  str : TKMStructureBasic;
begin
  Result := true;
  str := gRes.Structures[aIndex].Base[aRot];

  isBridge := gRes.Structures[aIndex].IsBridge;

  //bridge has to have water connection
  canBuildBridge := false;
  hasAnyWater := false;
  if isBridge then
  begin
    hasAnyWater := false;
    for X := 0 to str.Size.X - 1 do
      for Y := 0 to str.Size.Y - 1 do
        if str.PointXY[X, Y] = 2 then
        begin
          P.X := aLoc.X + X + str.Offset.X;
          P.Y := aLoc.Y + Y + str.Offset.Y;

          canBuildBridge := (TileHasWater(P.X, P.Y + 1) and (str.PointXY[X, Y - 1] = 0))
                            or (TileHasWater(P.X, P.Y - 1) and (str.PointXY[X, Y + 1] = 0))
                            or (TileHasWater(P.X + 1, P.Y) and (str.PointXY[X - 1, Y] = 0))
                            or (TileHasWater(P.X - 1, P.Y) and (str.PointXY[X + 1, Y] = 0));

          canBuildBridge := canBuildBridge and not TileHasWater(P);
          canBuildBridge := canBuildBridge and TileIsWalkable(P);
          canBuildBridge := canBuildBridge and (  TileIsWalkable(KMPointBelow(P))
                                                  or TileIsWalkable(KMPointAbove(P))
                                                  or TileIsWalkable(KMPointLeft(P))
                                                  or TileIsWalkable(KMPointRight(P))
                                                );

          canBuildBridge := canBuildBridge
                              and ((Land^[P.Y, P.X].BridgeType = gRes.Structures[aIndex].BridgeType)
                                    or (Land^[P.Y, P.X].BridgeType = 0));
        end else
        begin
          P.X := aLoc.X + X + str.Offset.X;
          P.Y := aLoc.Y + Y + str.Offset.Y;
          hasAnyWater := hasAnyWater or TileHasWater(P);
        end;

  end;


  if isBridge then
    Result := Result and canBuildBridge;
  if Result then
    for X := 0 to str.Size.X - 1 do
      for Y := 0 to str.Size.Y - 1 do
      if str.PointXY[X, Y] > 0 then
      begin
        P.X := aLoc.X + X + str.Offset.X;
        P.Y := aLoc.Y + Y + str.Offset.Y;

        Result := Result and TileInMapCoords(P, 1);

        Result := Result and CheckForHouses(P);
        Result := Result and (Land^[P.Y, P.X].TileLock <> tlStructure);

        if isBridge then
          if str.PointXY[X, Y] = 1 then
            Result := Result
                      and hasAnyWater
                      and (not TileHasTerrainKinds(P.X, P.Y, NO_BRIDGE_TER_KINDS) or (Land^[P.Y, P.X].BridgeType = gRes.Structures[aIndex].BridgeType));

        if gRes.Structures[aIndex].IsConstruction then
            Result := Result and (tpBuildNoObj in Land^[P.Y, P.X].Passability);

        //allowBuild := allowBuild and not (Land^[P.Y, P.X].Obj in [80, 81, 61]);
        if not Result then //no need to check further
          Exit;
      end;
end;

procedure TKMTerrain.PlaceStructureTile(const aLoc: TKMPoint; aIndex: Word; aRot: Word; aTileIndex: Word);
var P : TKMPoint;
begin
  P := KMPointAdd(aLoc, gRes.Structures[aIndex].Base[aRot].Offset);
  P := KMPointAdd(P, gRes.Structures[aIndex].Base[aRot].Order[aTileIndex]);

  with gRes.Structures[aIndex].Basic[aRot] do
    with TileXY[Order[aTileIndex].X, Order[aTileIndex].Y] do
    begin
      ScriptTrySetTile(P.X, P.Y, Tile, Rot);
      SetObject(P, Obj);
    end;
end;

procedure TKMTerrain.PlaceStructure(const aLoc: TKMPoint; aIndex, aRot: Word);
var X, Y : Integer;
  P : TKMPoint;
begin
  SetStructurePlan(aLoc, aIndex, aRot, hbsDone);
  with gRes.Structures[aIndex] do
  begin
    //SetRotation(aRot);
    for X := 0 to Base[aRot].Size.X - 1 do
    for Y := 0 to Base[aRot].Size.Y - 1 do
      if Base[aRot].PointXY[X, Y] > 0 then
      begin
        P.X := aLoc.X + X + Base[aRot].Offset.X;
        P.Y := aLoc.Y + Y + Base[aRot].Offset.Y;
        with Base[aRot].TileXY[X, Y] do
          ScriptTrySetTile(P.X, P.Y, Tile, Rot);
      end;
  end;
end;


procedure TKMTerrain.SetStructurePlan(const aLoc: TKMPoint; aIndex, aRot: Word; aBuildType : TKMHouseBuildState);
var X, Y : Integer;
  P : TKMPoint;
begin
  with gRes.Structures[aIndex] do
    for X := 0 to Base[aRot].Size.X - 1 do
    for Y := 0 to Base[aRot].Size.Y - 1 do
      if Base[aRot].PointXY[X, Y] > 0 then
      begin
        P.X := aLoc.X + X + Base[aRot].Offset.X;
        P.Y := aLoc.Y + Y + Base[aRot].Offset.Y;
        if aBuildType in [hbsNoGlyph, hbsDone, hbsWood] then
        begin
          if Land^[P.Y, P.X].TileLock = tlStructure then
            UnlockTile(P);

          case aBuildType of
            hbsNoGlyph: If not gHands.HasBridgeBuiltAt(P) then Land^[P.Y, P.X].BridgeType := 0;
            hbsWood,
            hbsStone: ;
            hbsDone: Land^[P.Y, P.X].BridgeType := BridgeType;
          end;

        end
        else
         SetTileLock(P, tlStructure);

        UpdateFences(P);
      end;
end;

procedure TKMTerrain.PlaceDecoration(const aLoc: TKMPoint; aIndex: Word);
begin
  case gDecorations[aIndex].DType of
    dtObject : SetObject(aLoc, gDecorations[aIndex].ID);
    dtTile : TrySetTile(aLoc.X, aLoc.Y, gDecorations[aIndex].ID, 0);
    dtTileOverlay : SetOverlay(aLoc, TKMTileOverlay(gDecorations[aIndex].ID), false);
  end;
end;

function TKMTerrain.CanAddField(aX, aY: Word; aFieldType: TKMFieldType; aOwner : ShortInt = 0): Boolean;
  function HasWall : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := Max(aX - 1, 1) to Min(aX + 1, fMapX - 1) do
      Result := Result or (Land^[aY, I].TileLock in [tlWall, tlWallGate]);
    for I := Max(aY - 1, 1) to Min(aY + 1, fMapY - 1) do
      Result := Result or (Land^[I, aX].TileLock in [tlWall, tlWallGate]);
  end;
begin
  //Make sure it is within map, roads can be built on edge

  Result := TileInMapCoords(aX, aY);

  case aFieldType of
    ftRemove : Result := (Land^[aY, aX].TileOwner = aOwner)
                          and not (Land^[aY, aX].TileLock in [tlFieldWork,tlHouse, tlWall, tlWallFence,tlDigged,tlFenced])
                          and (TileIsCornField(KMPoint(aX, aY)) or TileIsWineField(KMPoint(aX, aY)) or TileIsGrassField(KMPoint(aX, aY)) or TileIsVegeField(KMPoint(aX, aY))
                              or TileHasRoad(aX, aY) or TileHasPalisade(aX, aY));
    ftPalisade :  Result := InRange(aX, 1, fMapX - 1)
                            and InRange(aY, 1, fMapY - 1)
                            and not TileHasPalisade(aX,aY)
                            and not TileHasRoad(aX,aY)
                            and not TileIsCornField(KMPoint(aX, aY))
                            and not TileIsWineField(KMPoint(aX, aY))
                            and not TileIsGrassField(KMPoint(aX, aY))
                            and not TileIsVegeField(KMPoint(aX, aY))
                            and not (Land^[aY, aX].TileLock in [tlFieldWork,tlHouse, tlWall, tlWallFence,tlDigged,tlFenced])
                            and (tpWalk in Land^[aY, aX].Passability)
                            and HasWall;

    ftRoad:       Result := Result and (tpMakeRoads in Land^[aY, aX].Passability) or (gGameParams.IsMapEditor and (GetRoadType(aX, aY) <> rtNone) and (GetRoadType(aX, aY) <> gCursor.RoadType) );
    ftGrassLand:  Result := Result and (TileGoodForGrassField(aX, aY) or TileIsGrassField(aX, aY) or TileIsCornField(aX, aY) or TileIsVegeField(aX, aY));
    ftCorn:       Result := Result and (TileGoodForCornField(aX, aY) or TileIsGrassField(aX, aY) or TileIsCornField(aX, aY) or TileIsVegeField(aX, aY));
    ftWine:       Result := Result and TileGoodForWineField(aX, aY);
    ftVegeField:  Result := Result and (TileGoodForGrassField(aX, aY) or TileIsGrassField(aX, aY) or TileIsCornField(aX, aY) or TileIsVegeField(aX, aY));
    else          Result := False;
  end;
end;


function TKMTerrain.CheckHeightPass(const aLoc: TKMPoint; aPass: TKMHeightPass): Boolean;

  function TestHeight(aHeight: Byte): Boolean;
  var
    points: array[1..4] of Byte;
    Y2, X2: Integer;
  begin
    Y2 := Min(aLoc.Y + 1, fMapY);
    X2 := Min(aLoc.X + 1, fMapX);

    //Put points into an array like this so it's easy to understand:
    // 1 2
    // 3 4
    //Local map boundaries test is faster
    points[1] := Land^[aLoc.Y, aLoc.X].Height;
    points[2] := Land^[aLoc.Y, X2].Height;
    points[3] := Land^[Y2,     aLoc.X].Height;
    points[4] := Land^[Y2,     X2].Height;

    {
      KaM method checks the differences between the 4 verticies around the tile.
      There is a special case that means it is more (twice) as tolerant to bottom-left to top right (2-3) and
      bottom-right to top-right (4-2) slopes. This sounds very odd, but if you don't believe me then do the tests yourself. ;)
      The reason for this probably has something to do with the fact that shaddows and stuff flow from
      the bottom-left to the top-right in KaM.
      This formula could be revised later, but for now it matches KaM perfectly.
      The biggest problem with it is backwards sloping tiles which are shown as walkable.
      But it doesn't matter that much because this system is really just a backup (it's more important for
      building than walking) and map creators should block tiles themselves with the special invisible block object.
    }

    //Sides of tile
    Result :=            (Abs(points[1] - points[2]) < aHeight);
    Result := Result and (Abs(points[3] - points[4]) < aHeight);
    Result := Result and (Abs(points[3] - points[1]) < aHeight);
    Result := Result and (Abs(points[4] - points[2]) < aHeight * 2); //Bottom-right to top-right is twice as tolerant

    //Diagonals of tile
    Result := Result and (Abs(points[1] - points[4]) < aHeight);
    Result := Result and (Abs(points[3] - points[2]) < aHeight * 2); //Bottom-left to top-right is twice as tolerant
  end;
begin
  //Three types measured in KaM: >=25 - unwalkable/unroadable; >=25 - iron/gold mines unbuildable;
  //>=18 - other houses unbuildable.
  Result := True;

  if not TileInMapCoords(aLoc.X, aLoc.Y) then exit;

  case aPass of
    hpWalking:        Result := TestHeight(25);
    hpBuilding:       Result := TestHeight(18);
    hpBuildingMines:  Result := TestHeight(25);
    hpFish:           Result := TestHeight(25);
  end;
end;


procedure TKMTerrain.AddHouseRemainder(const aLoc: TKMPoint; aHouseType: TKMHouseType; aBuildState: TKMHouseBuildState; Resources : array of TKMWareType);
var
  I, K: Integer;
  HA: TKMHouseArea;
  W : TKMWareType;
begin
  HA := gRes.Houses[aHouseType].BuildArea;

  if aBuildState in [hbsStone, hbsDone] then //only leave rubble if the construction was well underway (stone and above)
  begin
    //Leave rubble
    for I := 2 to 4 do
      for K := 2 to 4 do
        if (HA[I - 1, K] <> 0) and (HA[I, K - 1] <> 0)
        and (HA[I - 1, K - 1] <> 0) and (HA[I, K] <> 0) then
        begin
          if (length(Resources) > 0) and (KamRandom(101, 'TKMTerrain.AddHouseRemainder1') < 90) then
          begin
            W := Resources[KamRandom(length(Resources), 'TKMTerrain.AddHouseRemainder2')];
            case W of
              wtGold,
              wtGoldOre : SetObject(KMPoint(aLoc.X + K - 3, aLoc.Y + I - 4), 296 + KaMRandom(3, 'TKMTerrain.AddHouseRemainder3'));
              wtIron,
              wtIronOre : SetObject(KMPoint(aLoc.X + K - 3, aLoc.Y + I - 4), 291 + KaMRandom(3, 'TKMTerrain.AddHouseRemainder4'));
              wtStoneBolt,
              wtStone : SetObject(KMPoint(aLoc.X + K - 3, aLoc.Y + I - 4), 286 + KaMRandom(3, 'TKMTerrain.AddHouseRemainder5'));
              wtTrunk,
              wtTimber,
              wtCoal : SetObject(KMPoint(aLoc.X + K - 3, aLoc.Y + I - 4), 301 + KaMRandom(3, 'TKMTerrain.AddHouseRemainder6'));
            end;
          end else
          Land^[aLoc.Y + I - 4, aLoc.X + K - 3].Obj := 68 + KaMRandom(6, 'TKMTerrain.AddHouseRemainder7');
        end;

    //Leave dug terrain
    for I := 1 to 4 do
      for K := 1 to 4 do
        if HA[I, K] <> 0 then
        begin
          Land^[aLoc.Y + I - 4, aLoc.X + K - 3].TileOverlay := toDig3;
          Land^[aLoc.Y + I - 4, aLoc.X + K - 3].TileLock    := tlNone;
        end;

    //if there was clay below it then remove it
    if aHouseType <> htPottery then
      for I := 1 to 4 do
        for K := 1 to 4 do
          if HA[I, K] <> 0 then
            if gMapElements[Land^[aLoc.Y + I - 4, aLoc.X + K - 3].Obj].Clay > 0 then
              Land^[aLoc.Y + I - 4, aLoc.X + K - 3].TileOverlay2 := toNone;

  end else
  begin
      for I := 1 to 4 do
        for K:=1 to 4 do
          if HA[I, K] <> 0 then
            Land^[aLoc.Y + I - 4, aLoc.X + K - 3].TileLock := tlNone;
  end;

  UpdatePassability(KMRect(aLoc.X - 3, aLoc.Y - 4, aLoc.X + 2, aLoc.Y + 1));
  UpdateWalkConnect([wcWalk, wcRoad, wcWork],
                    KMRect(aLoc.X - 3, aLoc.Y - 4, aLoc.X + 2, aLoc.Y + 1),
                    (aBuildState in [hbsStone, hbsDone])); //Rubble objects block diagonals
end;

function  TKMTerrain.CanPlaceWall(const aLoc: TKMPoint; aHouseType: TKMHouseType): Boolean;
Const FIND_GATE_RADIUS = 10;
var BestDIs : Integer;
  tmpLocs : array[1..255, 1..255 ] of Byte;
  minDistance : byte;
  gateFound : Boolean;
  HT : TKMHouseType;
  H : TKMHouse;

  procedure ResetTMP;
  var I, K : Integer;
  begin
    for I := 1 to 255 do
      for K := 1 to 255 do
        tmpLocs[I, K] := 255;
  end;

  procedure Visit(aX, aY, aDistance: Integer);
  begin
    //if not TileInMapCoords(aX, aY) then  Exit;
    if aDistance >= tmpLocs[aY, aX] then Exit;

    if aDistance > minDistance then
      if not CheckPassability(aX, aY, tpWall) then Exit;

    //SetOverlay(KMPoint(aX, aY),  toCoal3, false);

    if gateFound then Exit;

    tmpLocs[aY, aX] := aDistance;

    if Land^[aY, aX].TileLock = tlWallGate  {CheckPassability(aX, aY, tpWallGate)} then
    begin
      H := gHands.HousesHitTest(aX, aY);
      if ((H <> nil) and (H.HouseType = HT)) or (HT = htNone) then
      begin
        gateFound := true;
        Result := true;
        Exit;
      end;
    end;

    If aDistance + 1 <= BestDis then
    begin
      if aX - 1 >= 1 then  Visit(aX - 1, aY, aDistance + 1);
      if aX + 1 <= fMapX then  Visit(aX + 1, aY, aDistance + 1);
      if aY - 1 >= 1 then  Visit(aX, aY - 1, aDistance + 1);
      if aY + 1 <= fMapY then  Visit(aX, aY + 1, aDistance + 1);
    end;



  end;

  function CheckWallNearby(X, Y : Integer) : Boolean;
  begin
    if  not TileInMapCoords(X, Y) then Exit(false);

    Result := Land^[Y, X].TileLock in [tlWall, tlWallFence, tlWallGate]
  end;

var hasWallNearby : Boolean;
begin
  if gGameParams.IsMapEditor then
    Exit(true);

  ResetTMP;
  Result := false;
  gateFound := false;
  hasWallNearby := false;
  if Land^[aLoc.Y, aLoc.X].TileLock in [tlWallGate, tlWallEmpty] then
    Exit;

  case aHouseType of
    htWall3: hasWallNearby := CheckWallNearby(aLoc.X, aLoc.Y - 3) or CheckWallNearby(aLoc.X, aLoc.Y + 1);
    htWall: hasWallNearby := CheckWallNearby(aLoc.X + 2, aLoc.Y) or CheckWallNearby(aLoc.X - 2, aLoc.Y);
    htWall5: hasWallNearby := true;
  end;

  if not hasWallNearby then Exit(false);

  case aHouseType of
    htWall: minDistance := 1;
    htWall3: minDistance := 2;
    htWall5: minDistance := 0;
  end;
  case aHouseType of
    htWall: HT := htWall2;
    htWall3: HT := htWall4;
    else HT := htNone;
  end;

  BestDis := FIND_GATE_RADIUS;

  if gHands.HousesHitTest(aLoc.X, aLoc.Y) = nil then
    Visit(aLoc.X, aLoc.Y, 0);


end;

function TKMTerrain.CanPlaceWell(const aLoc: TKMPoint): Boolean;
begin
  Result := not (TileIsSnow(aLoc.X, aLoc.Y) or TileIsSand(aLoc) or (TileIsCoal(aLoc.X, aLoc.Y) > 0));

end;

procedure TKMTerrain.UpdateFences(aCheckSurrounding: Boolean = True);
begin
  UpdateFences(fMapRect);
end;


procedure TKMTerrain.UpdateFences(const aRect: TKMRect; aCheckSurrounding: Boolean = True);
var
  I, K: Integer;
begin
  for I := Max(aRect.Top, 1) to Min(aRect.Bottom, fMapY - 1) do
    for K := Max(aRect.Left, 1) to Min(aRect.Right, fMapX - 1) do
      UpdateFences(KMPoint(K, I), aCheckSurrounding);
end;


// Make call on DefaultLand, instead of Land, which could be replaced on smth else
procedure TKMTerrain.CallOnMainLand(aProc: TEvent);
var
  tempLand: PKMLand;
begin
  tempLand := Land;
  SetMainLand;
  aProc;
  Land := tempLand;
end;

function TKMTerrain.House(aLoc : TKMPoint) : TKMHouse;
begin
  Result := House(aLoc.X, aLoc.Y);
end;

function TKMTerrain.House(aX, aY : Integer) : TKMHouse;
begin
  if not TileInMapCoords(aX, aY, 1) then
    Exit(nil);

  Result := Land[aY, aX].IsHouse;

end;

function TKMTerrain.GetUnit(aLoc : TKMPoint) : TKMUnit;
begin
  Result := GetUnit(aLoc.X, aLoc.Y);
end;

function TKMTerrain.GetUnit(aX, aY : Integer) : TKMUnit;
begin
  if not TileInMapCoords(aX, aY, 1) then
    Exit(nil);

  Result := Land[aY, aX].IsUnit;

end;

procedure TKMTerrain.UpdateAll;
begin
  UpdateAll(fMapRect);
end;


// Update all map internal data
procedure TKMTerrain.UpdateAll(const aRect: TKMRect);
begin
  UpdatePassability(aRect);
  UpdateFences(aRect);
  UpdateRenderHeight(aRect);
  UpdateLighting(aRect);
  UpdateNightAffection;
end;


{Check 4 surrounding tiles, and if they are different place a fence}
procedure TKMTerrain.UpdateFences(const aLoc: TKMPoint; aCheckSurrounding: Boolean = True);

  function GetFenceType: TKMFenceKind;
  var H : TKMHouse;
  begin
    H := TKMHouse(House(aLoc));

    if not fLoading and (Assigned(H) and (H is TKMHouseAppleTree)) then //fruit tree has it's own fences
      Result := fncAppleTree
    else
    if Land^[aLoc.Y,aLoc.X].TileLock in [tlFenced, tlDigged,tlWallFence, tlStructure] then
      Result := fncHouseFence
    else
    if Land^[aLoc.Y, aLoc.X].BridgeType = 2 then
      Result := fncStoneBridge
    else
    if Land^[aLoc.Y, aLoc.X].BridgeType = 1 then
      Result := fncWoodenBridge
    else
    if TileIsCornField(aLoc) then
      Result := fncCorn
    else
    if TileIsGrassField(aLoc) then
      Result := fncGrassLand
    else
    if TileIsVegeField(aLoc) then
      Result := fncVegeField
    else
    {if TileHasPalisade(aLoc.X, aLoc.Y) then
      Result := fncCorn
    else }
    if TileIsWineField(aLoc) then
      Result := fncWine
    else
      Result := fncNone;
  end;

  function GetFenceEnabled(X, Y: SmallInt): Boolean;
  begin
    Result := True;

    if not TileInMapCoords(X,Y) then exit;

    if (TileIsCornField(aLoc) and TileIsCornField(KMPoint(X,Y))) or //Both are Corn
       (TileIsWineField(aLoc) and TileIsWineField(KMPoint(X,Y))) or //Both are Wine
       (TileIsGrassField(aLoc) and TileIsGrassField(KMPoint(X,Y))) or //Both are Grass
       (TileIsVegeField(aLoc) and TileIsVegeField(KMPoint(X,Y))) or //Both are Grass

       (
        House(aLoc).IsValid(htAppleTree) and House(X, Y).IsValid(htAppleTree)
       ) or //Both are appletree


       ((Land^[aLoc.Y, aLoc.X].BridgeType > 0) and (Land^[Y, X].BridgeType > 0)) or //Both are Wine
       ((Land^[aLoc.Y, aLoc.X].TileLock in [tlFenced, tlDigged, tlWallFence, tlStructure]) and
        (Land^[Y, X].TileLock in [tlFenced, tlDigged, tlWallFence, tlStructure])) then //Both are either house fence
      Result := False;
  end;
begin
  if not TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  Fences[aLoc.Y,aLoc.X].Kind := GetFenceType;

  if Fences[aLoc.Y, aLoc.X].Kind = fncNone then
    Fences[aLoc.Y, aLoc.X].Side := 0
  else
  begin
    Fences[aLoc.Y, aLoc.X].Side := Byte(GetFenceEnabled(aLoc.X,     aLoc.Y - 1))      + //N
                                 Byte(GetFenceEnabled(aLoc.X - 1, aLoc.Y))      * 2 + //E
                                 Byte(GetFenceEnabled(aLoc.X + 1, aLoc.Y))      * 4 + //W
                                 Byte(GetFenceEnabled(aLoc.X,     aLoc.Y + 1))  * 8;  //S
  end;

  if aCheckSurrounding then
  begin
    UpdateFences(KMPoint(aLoc.X - 1, aLoc.Y), False);
    UpdateFences(KMPoint(aLoc.X + 1, aLoc.Y), False);
    UpdateFences(KMPoint(aLoc.X, aLoc.Y - 1), False);
    UpdateFences(KMPoint(aLoc.X, aLoc.Y + 1), False);
  end;
end;


{Cursor position should be converted to tile-coords respecting tile heights}
function TKMTerrain.ConvertCursorToMapCoord(inX,inY: Single): Single;
var
  I, ii: Integer;
  Xc, Yc: Integer;
  tmp, len, lenNegative: Integer;
  Ycoef: array of Single;
begin
  Xc := EnsureRange(Round(inX + 0.5), 1, fMapX - 1); //Cell below cursor without height check
  Yc := EnsureRange(Round(inY + 0.5), 1, fMapY - 1);

  len := 2 * Ceil(HEIGHT_MAX / CELL_HEIGHT_DIV) + 1;
  SetLength(Ycoef, len);

  // We split length 1/3 to negative and 2/3 to positive part
  lenNegative := Ceil(len / 3);

  for I := Low(Ycoef) to High(Ycoef) do //make an array of tile heights above and below cursor (-2..4)
  begin
    ii := I - lenNegative;
    tmp       := EnsureRange(Yc + ii, 1, fMapY);
    Ycoef[I] := (Yc - 1) + ii - (LandExt^[tmp, Xc].RenderHeight * (1 - frac(inX))
                          + LandExt^[tmp, Xc + 1].RenderHeight * frac(inX)) / CELL_HEIGHT_DIV;
  end;

  Result := inY; //Assign something incase following code returns nothing

  for I := Low(Ycoef) to High(Ycoef) - 1 do//check if cursor in a tile and adjust it there
  begin
    ii := I - lenNegative;
    if InRange(inY, Ycoef[I], Ycoef[I + 1]) then
    begin
      Result := Yc + ii - (Ycoef[I + 1] - inY) / (Ycoef[I + 1] - Ycoef[I]);
      Break;
    end;
  end;
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.FlatToHeight(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  tmp1, tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-1);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-1);

  tmp1 := Mix(Land[Yc+1, Min(Xc+2, fMapX)].Height, Land^[Yc+1, Xc+1].Height, Frac(inX));
  tmp2 := Mix(Land[Min(Yc+2, fMapY), Min(Xc+2, fMapX)].Height, Land^[Min(Yc+2, fMapY), Xc+1].Height, Frac(inX));
  Result := inY - Mix(tmp2, tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.RenderFlatToHeight(const aPoint: TKMPointF): TKMPointF;
begin
  Result.X := aPoint.X;
  Result.Y := RenderFlatToHeight(aPoint.X, aPoint.Y);
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.RenderFlatToHeight(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  tmp1, tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-1);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-1);

  tmp1 := Mix(LandExt[Yc+1, Min(Xc+2, fMapX)].RenderHeight, LandExt^[Yc+1, Xc+1].RenderHeight, Frac(inX));
  tmp2 := Mix(LandExt[Min(Yc+2, fMapY), Min(Xc+2, fMapX)].RenderHeight, LandExt^[Min(Yc+2, fMapY), Xc+1].RenderHeight, Frac(inX));
  Result := inY - Mix(tmp2, tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


//Convert point from flat position to height position on terrain
function TKMTerrain.FlatToHeight(const aPoint: TKMPointF): TKMPointF;
begin
  Result.X := aPoint.X;
  Result.Y := FlatToHeight(aPoint.X, aPoint.Y);
end;


//Return height within cell interpolating node heights
//Note that input parameters are 0 based
function TKMTerrain.HeightAt(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  tmp1, tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-1);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-1);

  tmp1 := Mix(Land[Yc+1, Min(Xc+2, fMapX)].Height, Land^[Yc+1, Xc+1].Height, Frac(inX));
  tmp2 := Mix(Land[Min(Yc+2, fMapY), Min(Xc+2, fMapX)].Height, Land^[Min(Yc+2, fMapY), Xc+1].Height, Frac(inX));
  Result := Mix(tmp2, tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


//Return Render height within cell interpolating node heights
//Note that input parameters are 0 based
function TKMTerrain.RenderHeightAt(inX, inY: Single): Single;
var
  Xc, Yc: Integer;
  tmp1, tmp2: single;
begin
  //Valid range of tiles is 0..MapXY-2 because we check height from (Xc+1,Yc+1) to (Xc+2,Yc+2)
  //We cannot ask for height at the bottom row (MapY-1) because that row is not on the visible map,
  //and does not have a vertex below it
  Xc := EnsureRange(Trunc(inX), 0, fMapX-1);
  Yc := EnsureRange(Trunc(inY), 0, fMapY-1);

  tmp1 := Mix(LandExt[Yc+1, Min(Xc+2, fMapX)].RenderHeight, LandExt^[Yc+1, Xc+1].RenderHeight, Frac(inX));
  tmp2 := Mix(LandExt[Min(Yc+2, fMapY), Min(Xc+2, fMapX)].RenderHeight, LandExt^[Min(Yc+2, fMapY), Xc+1].RenderHeight, Frac(inX));
  Result := Mix(tmp2, tmp1, Frac(inY)) / CELL_HEIGHT_DIV;
end;


procedure TKMTerrain.SetTopHill(aValue: Integer);
begin
  if fTopHill = aValue then Exit;

  fTopHill := aValue;
  if Assigned(fOnTopHillChanged) then
    fOnTopHillChanged(fTopHill);
end;


procedure TKMTerrain.UpdateTopHill(X, Y: Integer);
begin
  SetTopHill(Max(fTopHill, LandExt^[Y, X].RenderHeight - (Y-1) * CELL_SIZE_PX));
end;


//Get highest hill on a maps top row to use for viewport top bound
procedure TKMTerrain.UpdateTopHill;
var
  I, K: Integer;
begin
  SetTopHill(0);
  //Check last several strips in case lower has a taller hill
  for I := 1 to Ceil(HEIGHT_MAX / CELL_SIZE_PX) do
    for K := 1 to fMapX  do
      UpdateTopHill(K, I);
end;


procedure TKMTerrain.IncAnimStep;
begin
  Inc(fAnimStep);
end;


procedure TKMTerrain.Save(SaveStream: TKMemoryStream);
var
  I, K, L: Integer;
  isTxtStream: Boolean;
begin
  Assert(not fMapEditor, 'MapEd mode is not intended to be saved into savegame');

  SaveStream.PlaceMarker('Terrain');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fMapRect);
  SaveStream.Write(fAnimStep);
  SaveStream.Write(fNightFactor);
  SaveStream.Write(fHasDarkPoints);
  FallingTrees.SaveToStream(SaveStream);
  Lights.SaveToStream(SaveStream);
  isTxtStream := SaveStream is TKMemoryStreamText;

  if isTxtStream then
    for I := 1 to fMapY do
      for K := 1 to fMapX do
      begin
        SaveStream.PlaceMarker(KMPoint(K,I).ToString);

        with Land^[I,K] do
        begin
          BaseLayer.Save(SaveStream);
          for L := 0 to 2 do
            Layer[L].Save(SaveStream);
          SaveStream.Write(LayersCnt);
          SaveStream.Write(Height);
          SaveStream.Write(Obj);
          SaveStream.Write(IsCustom);
          SaveStream.Write(BlendingLvl);
          SaveStream.Write(TreeAge);
          SaveStream.Write(FieldAge);
          SaveStream.Write(TileLock, SizeOf(TileLock));
          SaveStream.Write(JamMeter);
          SaveStream.Write(TileOverlay, SizeOf(TileOverlay));
          SaveStream.Write(TileOverlay2, SizeOf(TileOverlay2));
          SaveStream.Write(TileOwner, SizeOf(TileOwner));
          SaveStream.Write(TKMUnit(Land^[I,K].IsUnit).UID);
          SaveStream.Write(TKMHouse(Land^[I,K].IsHouse).UID);
          SaveStream.Write(IsVertexUnit, SizeOf(IsVertexUnit));

          SaveStream.Write(Passability, SizeOf(Passability));
          SaveStream.Write(WalkConnect, SizeOf(WalkConnect));
          SaveStream.Write(Light);
          SaveStream.Write(BridgeType);
          SaveStream.Write(TileSelected);
          SaveStream.Write(RoadType, SizeOf(RoadType));
          SaveStream.Write(GrainType, SizeOf(GrainType));
          SaveStream.Write(NightAffection);
          SaveStream.Write(Ware, SizeOf(Ware));
          SaveStream.Write(IsAiReserved);
        end;
      end
  else
  begin
    // Save Unit pointer in temp array
    for I := 1 to fMapY do
      for K := 1 to fMapX do
      begin
        fUnitPointersTemp[I,K] := Land^[I,K].IsUnit;
        fHousesPointersTemp[I,K] := Land^[I,K].IsHouse;
        Land[I,K].IsUnit := Pointer(TKMUnit(Land^[I,K].IsUnit).UID);
        Land[I,K].IsHouse := Pointer(TKMHouse(Land^[I,K].IsHouse).UID);
      end;

    for I := 1 to fMapY do
      SaveStream.Write(Land[I,1], SizeOf(Land^[I,1]) * fMapX);

    // Restore unit pointers
    for I := 1 to fMapY do
      for K := 1 to fMapX do
      begin
        Land^[I,K].IsUnit := fUnitPointersTemp[I,K];
        Land^[I,K].IsHouse := fHousesPointersTemp[I,K];

      end;
  end;
end;

Procedure TKMTerrain.AddTerrainMask(aTerKind : TKMTerrainKind);
begin
  SetLength(fTerrainMasks, length(fTerrainMasks) + 1);
  fTerrainMasks[high(fTerrainMasks)] := aTerKind;
end;

Procedure TKMTerrain.ResetTerrainMask;
begin
  SetLength(fTerrainMasks, 0);
end;

function TKMTerrain.MaskContains(aTerKind : TKMTerrainKind) : Boolean;
var I : Integer;
begin
  Result := false;
  if length(fTerrainMasks) = 0 then
    Exit(true);

  If not (gCursor.MapEdUseTerrainMasks) then
    Exit(true);

  If (aTerKind = tkCustom) then
    Exit(true);

  for I := 0 to High(fTerrainMasks) do
    if fTerrainMasks[I] = aTerKind then
      Exit(true);

end;

procedure TKMTerrain.SelectTile(X: Integer; Y: Integer; aSelected: Boolean);
begin
  Land^[Y, X].TileSelected := aSelected;
end;

procedure TKMTerrain.ReserveForAI(aLoc: TKMPoint);
begin
  Land^[aLoc.Y, aLoc.X].IsAiReserved := true;
end;

function TKMTerrain.IsReservedForAI(aLoc: TKMPoint): Boolean;
begin
  Result := Land^[aLoc.Y, aLoc.X].IsAiReserved;
end;

function TKMTerrain.GetRoadType(aLoc: TKMPoint): TKMRoadType;
begin
  if not TileInMapCoords(aLoc) then
    Exit(rtNone);
  
  Result := Land^[aLoc.Y, aLoc.X].RoadType;
end;

function TKMTerrain.GetRoadType(X, Y : Integer) : TKMRoadType;
begin
  if not TileInMapCoords(X, Y) then
    Exit(rtNone);

  Result := Land^[Y, X].RoadType;
end;

function TKMTerrain.GetGrainType(aLoc: TKMPoint): TKMGrainType;
begin
  if not TileInMapCoords(aLoc) then
    Exit(gftNone);

  Result := Land^[aLoc.Y, aLoc.X].GrainType;
end;

function TKMTerrain.AvoidTile(aX: Integer; aY: Integer): Boolean;
begin
  if not TileInMapCoords(aX, aY) then
    Exit(false);

  Result := Land^[aY, aX].TileLock in [tlStructure];
end;

function TKMTerrain.AvoidTile(P: TKMPoint): Boolean;
begin
  Result := AvoidTile(P.X, P.Y);
end;

procedure TKMTerrain.Load(LoadStream: TKMemoryStream);
var
  I, J: Integer;
begin
  fLoading := true;
  LoadStream.CheckMarker('Terrain');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fMapRect);
  LoadStream.Read(fAnimStep);
  LoadStream.Read(fNightFactor);
  LoadStream.Read(fHasDarkPoints);

  //TERRAIN_DARK := MIN_NIGHT_DARKNESS + (Sin(fNightFactor / NIGHT_SPEED) + 1) * 0.6 * (1 - MIN_NIGHT_DARKNESS);

  FallingTrees.LoadFromStream(LoadStream);
  Lights.LoadFromStream(LoadStream);

  for I := 1 to fMapY do
    LoadStream.Read(Land[I,1], SizeOf(Land^[I,1]) * fMapX);

  for I := 1 to fMapY do
    for J := 1 to fMapX do
    begin
      LandExt[I,J].RenderLight := Land^[I,J].GetRenderLight;
      LandExt[I,J].RenderHeight := Land[I,J].GetRenderHeight;
    end;

  fFinder := TKMTerrainFinder.Create;

  Init;

  gLog.AddTime('Terrain loaded');
  fLoading := false;
end;


procedure TKMTerrain.SyncLoad;
var
  I, K: Integer;
begin
  for I := 1 to fMapY do
    for K := 1 to fMapX do
    begin
      Land[I,K].IsUnit := gHands.GetUnitByUID(Integer(Land^[I,K].IsUnit));
      Land[I,K].IsHouse := gHands.GetHouseByUID(Integer(Land^[I,K].IsHouse));
      if Land[I,K].IsHidden then
        fHasDarkPoints := true;
    end;
  UpdateFences(false);
end;


procedure TKMTerrain.Init;
begin
  UpdateTopHill;
end;


function TKMTerrain.GetFieldStage(const aLoc: TKMPoint): Byte;
begin
  Result := 0;
  if not TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  if TileIsCornField(aLoc) then
    Result := GetCornStage(aLoc)
  else
  if TileIsGrassField(aLoc) then
    Result := GetGrassStage(aLoc)
  else
  if TileIsWineField(aLoc) then
    Result := GetWineStage(aLoc)
  else
  if self.TileIsVegeField(aLoc) then
    Result := self.GetVegeStage(aLoc);
end;


function TKMTerrain.GetCornStage(const aLoc: TKMPoint): Byte;
var
  fieldAge: Byte;
  GT : TKMGrainType;
begin
  Assert(TileIsCornField(aLoc));
  fieldAge := Land^[aLoc.Y,aLoc.X].FieldAge;

  GT := Land^[aLoc.Y,aLoc.X].GrainType;
  Result := gFieldGrains[GT].GetStage(fieldAge);

  if Result = 0 then
  begin
    if (fMapEditor and (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWineTerrain = GetTileCornTile(aLoc, 6)) )
       or (Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain = GetTileCornTile(aLoc, 6)) then
      Result := gFieldGrains[GT].StagesCount - 1
    else
    Result := 0;

  end else
  if Result = 255 then
    Result := gFieldGrains[GT].StagesCount - 2
  else
  if Result = 254 then
    Result := 0;
end;


function TKMTerrain.GetGrassStage(const aLoc: TKMPoint): Byte;
var
  fieldAge: Byte;
  GT : TKMGrainType;
begin
  Assert(TileIsGrassField(aLoc));
  fieldAge := Land^[aLoc.Y,aLoc.X].FieldAge;

  GT := Land^[aLoc.Y,aLoc.X].GrainType;
  Result := gFieldGrains[GT].GetStage(fieldAge);

  if Result = 0 then
  begin
    if (fMapEditor and (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWineTerrain = GetTileGrassTile(aLoc, 6)) )
       or (Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain = GetTileGrassTile(aLoc, 6)) then
      Result := gFieldGrains[GT].StagesCount - 1
    else
    Result := 0;

  end else
  if Result = 255 then
    Result := gFieldGrains[GT].StagesCount - 2
  else
  if Result = 254 then
    Result := 0;
end;

function TKMTerrain.GetVegeStage(const aLoc: TKMPoint): Byte;
var
  fieldAge: Byte;
  GT : TKMGrainType;
begin
  Assert(TileIsVegeField(aLoc));
  fieldAge := Land^[aLoc.Y,aLoc.X].FieldAge;

  GT := Land^[aLoc.Y,aLoc.X].GrainType;
  Result := gFieldGrains[GT].GetStage(fieldAge);

  if Result = 0 then
  begin
    if (fMapEditor and (gGame.MapEditor.LandMapEd^[aLoc.Y,aLoc.X].CornOrWineTerrain = GetTileGrassTile(aLoc, 6)) )
       or (Land^[aLoc.Y,aLoc.X].BaseLayer.Terrain = GetTileGrassTile(aLoc, 6)) then
      Result := gFieldGrains[GT].StagesCount - 1
    else
    Result := 0;

  end else
  if Result = 255 then
    Result := gFieldGrains[GT].StagesCount - 2
  else
  if Result = 254 then
    Result := 0;
end;

function TKMTerrain.GetWineStage(const aLoc: TKMPoint): Byte;
var I : Integer;
  GT : TKMGrainType;
begin
  Result := 0;
  Assert(TileIsWineField(aLoc));
  GT := Land^[aLoc.Y, aLoc.X].GrainType;

  if not (GT in GRAIN_WINE) then
    Exit;
  If Land^[aLoc.Y, aLoc.X].FieldAge >= CORN_AGE_MAX then
    Exit(gFieldGrains[GT].StagesCount - 1);
  for I := 0 to gFieldGrains[GT].StagesCount - 1 do
    if Land^[aLoc.Y, aLoc.X].FieldAge = gFieldGrains[GT].Stage[I].Age then
      Exit(I);
    

  {case Land^[aLoc.Y, aLoc.X].Obj of
    54:   Result := 0;
    55:   Result := 1;
    56:   Result := 2;
    57:   Result := 3;
  end;}
end;


function TKMTerrain.GetFieldType(const aLoc: TKMPoint): TKMFieldType;
begin
  Result := ftNone;
  if not TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  if TileHasRoad(aLoc) then
    Result := ftRoad
  else
  if TileIsCornField(aLoc) then
    Result := ftCorn
  else
  if TileIsGrassField(aLoc) then
    Result := ftGrassLand
  else
  if TileIsVegeField(aLoc) then
    Result := ftVegeField
  else
  if TileIsWineField(aLoc) then
    Result := ftWine
  else
  if TileHasPalisade(aLoc.X, aLoc.Y) then
    Result := ftPalisade;

end;

function TKMTerrain.LoadSeperatedLayer(aFileName : String) : Boolean;
var S : TKMemoryStream;
  fPath, aExt : String;
  X, Y, aID : Integer;
  aW : Word;
  aB : Byte;
begin
  //if not gGameParams.IsMapEditor then Exit;
  aExt := ExtractFileExt(aFileName);
  aID := -1;
  if aExt = '.tiles' then aID := 0 else
  if aExt = '.height' then aID := 1 else
  if aExt = '.objects' then aID := 2 else
  if aExt = '.overlays' then aID := 3 else
  if aExt = '.terrain' then aID := 4;
  if aID = -1 then Exit(false);
  
  Result := true;
  fPath := TKMapsCollection.GetMapPath(Trim(gGameParams.Name)) + 'Layers' + PathDelim + aFileName;
  if not FileExists(fPath) then Exit(false);

  S := TKMemoryStreamBinary.Create;

  S.LoadFromFile(fPath);
  try
    S.Read(X);
    S.Read(Y);
    Assert(X = gGame.MapSize.X, 'Map size X:' + IntToStr(gGame.MapSize.X) +' differs from the seperated layer: ' + IntToStr(X));
    Assert(Y = gGame.MapSize.Y, 'Map size Y:' + IntToStr(gGame.MapSize.Y) +' differs from the seperated layer: ' + IntToStr(Y));

    case aID of
      0 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
          begin
            S.Read(aW);
            gTerrain.Land^[Y, X].BaseLayer.Terrain := Min(aW, TILES_CNT);
            S.Read(aB);
            gTerrain.Land^[Y, X].BaseLayer.Rotation := aB;
          end;

      1 :begin
            for X := 1 to gGame.MapSize.X do
              for Y := 1 to gGame.MapSize.Y do
              begin
                S.Read(aB);
                gTerrain.Land^[Y, X].Height := aB;
                UpdateRenderHeight(X, Y);
              end;
          UpdateLighting(KMRect(1, 1, gGame.MapSize.X,gGame.MapSize.Y));
        end;

      2 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
          begin
            S.Read(aW);
            gTerrain.Land^[Y, X].Obj := Min(aW, OBJECTS_CNT);
          end;

      3 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
          begin
            S.Read(aB);
            gTerrain.Land^[Y, X].TileOverlay := TKMTileOverlay(aB);
          end;

      4 :begin
          for X := 1 to gGame.MapSize.X do
            for Y := 1 to gGame.MapSize.Y do
            begin
              S.Read(aW);
              gTerrain.Land^[Y, X].BaseLayer.Terrain := Min(aW, TILES_CNT);
              S.Read(aB);
              gTerrain.Land^[Y, X].BaseLayer.Rotation := aB;

              S.Read(aB);
              gTerrain.Land^[Y, X].Height := aB;
              UpdateRenderHeight(X, Y);

              S.Read(aW);
              gTerrain.Land^[Y, X].Obj := Min(aW, OBJECTS_CNT);

              S.Read(aB);
              gTerrain.Land^[Y, X].TileOverlay := TKMTileOverlay(aB);
            end;

          UpdateLighting(KMRect(1, 1, gGame.MapSize.X,gGame.MapSize.Y));
        end;
    end;

    UpdatePassability(KMRect(1, 1, gGame.MapSize.X,gGame.MapSize.Y));
    UpdateWalkConnect([wcWalk, wcRoad, wcWork],
                    KMRect(1, 1, gGame.MapSize.X,gGame.MapSize.Y),
                    true); //Rubble objects block diagonals


  finally
    S.Free;
  end;

end;
procedure TKMTerrain.SaveSeperatedLayer(aID : Byte);
var S : TKMemoryStream;
  X, Y : Integer;
  I : Integer;
  Path, aExt : String;
  FileSaved : Boolean;
begin
  //if not gGameParams.IsMapEditor then Exit;

  S := TKMemoryStreamBinary.Create;
  try
    S.Write(gGame.MapSize.X);
    S.Write(gGame.MapSize.Y);
    case aID of
      0 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
          begin
            S.Write(gTerrain.Land^[Y, X].BaseLayer.Terrain);
            S.Write(gTerrain.Land^[Y, X].BaseLayer.Rotation);
          end;

      1 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
            S.Write(gTerrain.Land^[Y, X].Height);

      2 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
            S.Write(gTerrain.Land^[Y, X].Obj);

      3 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
            S.Write(byte(gTerrain.Land^[Y, X].TileOverlay));

      4 :for X := 1 to gGame.MapSize.X do
          for Y := 1 to gGame.MapSize.Y do
          begin
            S.Write(gTerrain.Land^[Y, X].BaseLayer.Terrain);
            S.Write(gTerrain.Land^[Y, X].BaseLayer.Rotation);
            S.Write(gTerrain.Land^[Y, X].Height);
            S.Write(gTerrain.Land^[Y, X].Obj);
            S.Write(byte(gTerrain.Land^[Y, X].TileOverlay));
          end;

    end;
    Path := TKMapsCollection.GetMapPath(gGameParams.Name) + 'Layers';
    if not DirectoryExists(Path) then
      CreateDir(Path);
    case aID of
      0: aExt := '.tiles';
      1:aExt := '.height';
      2:aExt := '.objects';
      3:aExt := '.overlays';
      4:aExt := '.terrain';
    end;
    FileSaved := false;
    I := 0;
    while not FileSaved do
    begin
      if I = 0 then
      begin
        if not FileExists(Path + PathDelim + Trim(gGameParams.Name) + aExt) then
        begin
          FileSaved := true;
          S.SaveToFile(Path + PathDelim + Trim(gGameParams.Name) + aExt);
        end;
      end else
      begin
        if not FileExists(Path + PathDelim + Trim(gGameParams.Name) + ' (' + IntToStr(I) + ')' + aExt) then
        begin
          FileSaved := true;
          S.SaveToFile(Path + PathDelim + Trim(gGameParams.Name) + ' (' + IntToStr(I) + ')' + aExt);
        end;

      end;

      Inc(I);
    end;



  finally
    S.Free;
  end;
    //gGameParams.Name
end;

procedure TKMTerrain.UpdateNightAffection;
var X, Y, I, J : Integer;
  alights : TKMPointTagList;
begin
  if not gGame.Weather.Settings.DynamicLight then
    Exit;
  alights := TKMPointTagList.Create;

  for X := 1 to fMapX - 1 do
    for Y := 1 to fMapY - 1 do
    begin
      with gMapElements[Land^[Y, X].Obj] do
        if (LightRadius > 0) and (LightPower > 0) then
          alights.Add(KMPoint(X, Y), LightRadius, LightPower);

      Land^[Y, X].NightAffection := 1;
    end;

  for J := 0 to gHands.Count - 1 do
    for I := 0 to gHands[J].Houses.Count - 1 do
      if gHands[J].Houses[I].IsValid then
        alights.Add(gHands[J].Houses[I].Entrance, 5, 100);

  for I := 0 to alights.Count - 1 do
    AddLight(alights[I], alights.Tag[I], alights.Tag2[I]);

  alights.Free;
end;

procedure TKMTerrain.UpdateLight(aLoc: TKMPoint; aRadius: Byte; aPower: Byte);
var validTiles : TKMPointList;
  I : Integer;
  night : Single;
  P : TKMPoint;
begin
  if not gGame.Weather.Settings.DynamicLight then
    Exit;
  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, aRadius, tpNone, validTiles);

  for I := 0 to validTiles.Count - 1 do
  begin
    P := validTiles[I];
    night := EnsureRange(KMLengthDiag(aLoc, P) / aRadius, 0, 1);
    night :=  (1 - night) * (aPower / 255);
    night := (1 - night);

    if night < Land[P.Y, P.X].NightAffection then
      Land[P.Y, P.X].NightAffection := night;
  end;
  validTiles.Free;
end;

procedure TKMTerrain.AddLight(aLoc : TKMPoint; aRadius, aPower : Byte);
var  aID : Integer;
begin
  if not gGame.Weather.Settings.DynamicLight then
    Exit;

  aID := Lights.IndexOf(aLoc);

  if aID > 0 then
  begin
    //sameIndex found
    //check if it's different, if so update light
    if (Lights.Tag[aID] <> aRadius) or (Lights.Tag2[aID] <> aPower) then
    begin
      Lights.Tag[aID] := aRadius;
      Lights.Tag2[aID] := aPower;
      UpdateLight(aLoc, aRadius, aPower);
    end;

    Exit;
  end;

  if Lights.Contains(aLoc) then
    Exit;

  Lights.Add(aLoc, aRadius, aPower);
  UpdateLight(aLoc, aRadius, aPower);
end;

procedure TKMTerrain.RemLight(aLoc: TKMPoint);
var I, ID : Integer;
  R : Integer;
  validTiles : TKMPointList;
  P : TKMPoint;
begin
  if not gGame.Weather.Settings.DynamicLight then
    Exit;
  ID := Lights.IndexOf(aLoc);
  if ID = -1 then
    Exit;
  R := Lights.Tag[ID];
  Lights.Delete(ID);

  validTiles := TKMPointList.Create;
  fFinder.GetTilesWithinDistance(aLoc, R, tpNone, validTiles);

  for I := 0 to validTiles.Count - 1 do
  begin
    P := validTiles[I];
    Land[P.Y, P.X].NightAffection := 1;
  end;

  //we must update the closest lights so it won't look sharp
  for I := 0 to Lights.Count - 1 do
    if KMLengthDiag(Lights[I], aLoc) <= R * 2 then
      UpdateLight(Lights[I], Lights.Tag[I], Lights.Tag2[I]);

end;

function TKMTerrain.GetNightFactor: Single;
begin
  Result := fNightFactor / LocalNightSpeed;
end;

procedure TKMTerrain.SetNightFactor(aValue: Single);
begin
  fNightFactor := Abs(Round(aValue * LocalNightSpeed));
end;

function TKMTerrain.GetNightAtTile(aX: Integer; aY: Integer): Single;
begin
  if not TileInMapCoords(aX, aY) then
    Exit(1);

  Result := MIN_NIGHT_DARKNESS + (Sin(GetNightFactor) + 1) * 0.6 * (1 - MIN_NIGHT_DARKNESS);
  Result := Mix(1, Result, 1 - Land[aY, aX].NightAffection);
end;

function TKMTerrain.GetNightAtTile(aLoc: TKMPoint): Single;
begin
  Result := GetNightAtTile(aLoc.X, aLoc.Y);
end;

function TKMTerrain.LocalNightSpeed: Integer;
begin
  //Result := gGame.Weather.Settings.NightSpeed;
  Result := NIGHT_SPEED * (MAX_NIGHT_SPEED - gGame.Weather.Settings.NightSpeed);
end;


procedure TKMTerrain.IncFieldAge(aLoc: TKMPoint);
var aStage : Integer;
  FA : Byte;
  GT : TKMGrainType;
begin
  if Land[aLoc.Y, aLoc.X].TileOverlay2 = toStopGrowing then
    Exit;

  FA := Land[aLoc.Y, aLoc.X].FieldAge;
  GT := Land[aLoc.Y,aLoc.X].GrainType;

  if (FA >= CORN_AGE_MAX) and gGameParams.MBD.IsRealism and not TileIsWineField(aLoc) then
  begin
    Land^[aLoc.Y,aLoc.X].FieldAge := EnsureRange(Land^[aLoc.Y,aLoc.X].FieldAge + 1, 0, CORN_AGE_DEAD);
    if Land^[aLoc.Y,aLoc.X].FieldAge >= CORN_AGE_DEAD then
    begin
      SetLand(gFieldGrains[GT].Dead.Terr, aLoc.X, aLoc.Y,gFieldGrains[GT].Dead.Obj);
      Land[aLoc.Y, aLoc.X].FieldAge := 0;
    end;

    Exit;
  end;


  //grain is already grown up
  if FA = CORN_AGE_MAX then
    Exit;
  //grain is to be sow or is cut
  if (FA = 0) or (GT = gftNone) then
    Exit;
  //current stage
  aStage := gFieldGrains[GT].GetStage(FA);

  if (aStage <> 254) and (aStage <> CORN_AGE_MAX) and TileIsGrassField(aLoc) then //grass has to bu cut two times, so when it's time to cut don't increase field age
    if not gFieldGrains[GT].Stage[aStage].CanBeCut then
      Inc(Land^[aLoc.Y,aLoc.X].FieldAge)
    else
  else
    Inc(Land^[aLoc.Y,aLoc.X].FieldAge);

  //next stage
  aStage := gFieldGrains[GT].GetStage(FA);
  // if aStage = 0 it means that there is no grain stage at current field age
  if (aStage <> 254) and (aStage <> CORN_AGE_MAX) and (aStage > 0) then
  begin
    SetLand(gFieldGrains[GT].Stage[aStage].Terr,//terrain tile
            aLoc.X,aLoc.Y, //location
            //GetTileWineObject(aLoc, aStage)
            gFieldGrains[GT].Stage[aStage].Obj
            );//object

    if TileIsWineField(aLoc) then
    begin
      //last stage
      if aStage = gFieldGrains[GT].StagesCount - 1 then
        Land^[aLoc.Y, aLoc.X].FieldAge := CORN_AGE_MAX;
    end else
    begin
      //last stage is just empty field, previous stage has to be cut
      if aStage + 1 = gFieldGrains[GT].StagesCount - 1 then
        Land^[aLoc.Y, aLoc.X].FieldAge := CORN_AGE_MAX;
    end;

  end;
end;

procedure TKMTerrain.SetLand(aTile: Word; const X: Word; const Y: Word; const aObj: Word);
var FloodfillNeeded: Boolean;
begin
  Land^[Y,X].BaseLayer.Terrain := aTile;
  FloodfillNeeded   := gMapElements[Land^[Y,X].Obj].DiagonalBlocked <> gMapElements[aObj].DiagonalBlocked;
  if aObj = 0 then
    Land^[Y,X].Obj     := 255
  else
    Land^[Y,X].Obj     := aObj;
  if FloodfillNeeded then //When trees are removed by corn growing we need to update floodfill
    UpdateWalkConnect([wcWalk,wcRoad,wcWork], KMRectGrowTopLeft(KMRect(X,Y,X,Y)), True);
end;


//This whole thing is very CPU intesive, updating whole (256*256) tiles map
//Don't use any advanced math here, only simpliest operations - + div *
procedure TKMTerrain.UpdateState;
var
  I, K, A: Integer;
//  J: TKMChopableAge;
  T: Integer;
begin

  if not DYNAMIC_TERRAIN then Exit;


  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psTerrain);
  {$ENDIF}
  try
    inc(fAnimStep);
    if not gGameParams.IsTactic  then
    begin
      //time of day can be set by script, so we can't reset it to NIGHT_SPEED
      if gGame.Weather.Settings.NightSpeed > 0 then
      begin
        inc(fNightFactor);
        //fNightFactor := Sin(fAnimStep);
        if Sin(fNightFactor / LocalNightSpeed) < -0.2 then
          Inc(fNightFactor, 3)
        else
        if Sin(fNightFactor / LocalNightSpeed) < 0.5 then
          Inc(fNightFactor, 10);
      end;
    end else
    begin
      fNightFactor := gGame.Weather.Settings.NightTime * 1000;//NIGHT_SPEED;
    end;



    //Update falling trees animation
    for T := FallingTrees.Count - 1 downto 0 do
    if fAnimStep >= FallingTrees.Tag2[T] + Cardinal(gMapElements[FallingTrees.Tag[T]].Anim.Count - 1) then
      ChopTree(FallingTrees[T]); //Make the tree turn into a stump

    //Process every 200th (TERRAIN_PACE) tile, offset by fAnimStep
    A := fAnimStep mod TERRAIN_PACE;
    while A < (fMapX - 1) * (fMapY - 1) do
    begin
      K := (A mod (fMapX - 1)) + 1;
      I := (A div (fMapX - 1)) + 1;
      //K := (A mod fMapX) + 1;
      //I := (A div fMapX) + 1;

      //Reduce JamMeter over time
      Land^[I,K].IncJamMeter(-3);

      if InRange(Land^[I,K].FieldAge, 1, CORN_AGE_DEAD-1) then
        IncFieldAge(KMPoint(K, I));

      if InRange(Land^[I,K].TreeAge, 1, CORN_AGE_MAX - 1) then
      begin
        Inc(Land^[I,K].TreeAge);
        if (Land^[I,K].Obj = 390) and (Land^[I,K].TreeAge = TREE_AGE_SAPLING) then
        begin
          Land^[I,K].Obj := ChooseTreeToPlace(KMPoint(K,I), caAge1, true);
          UpdatePassability(KMPoint(K, I));
        end else
        if Land^[I,K].TreeAge = gMapElements[Land^[I,K].Obj].TreeGrowAge + TREE_AGE_SAPLING then
        begin
          Land^[I,K].Obj := gMapElements[Land^[I,K].Obj].NextTreeAgeObj;
          if (gMapElements[Land^[I,K].Obj].NextTreeAgeObj = 0) and (gMapElements[Land^[I,K].Obj].CuttableTree) then
            Land^[I,K].TreeAge := CORN_AGE_MAX;
          UpdatePassability(KMPoint(K, I));
          

        end;
      end;

      Inc(A, TERRAIN_PACE);
    end;
  finally
    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psTerrain);
    {$ENDIF}
  end;
end;


end.

