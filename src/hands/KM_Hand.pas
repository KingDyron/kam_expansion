unit KM_Hand;
{$I KaM_Remake.inc}
interface
uses
  Generics.Collections,
  KM_AI,
  KM_Units, KM_UnitsCollection, KM_UnitGroup, KM_UnitWarrior,
  KM_Houses, KM_HouseCollection, KM_HouseInn,
  KM_Structure, KM_StructuresCollection,
  KM_HandLogistics, KM_HandLocks, KM_HandStats, KM_GameTypes,
  KM_FogOfWar, KM_HandConstructions, KM_MessageLog, KM_ResHouses,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_HandEntity, KM_HandTypes, KM_CommonClassesExt,
  KM_ScriptingTypes,
  KM_ResTypes, KM_ResFonts, KM_ResStructures;


type
  TKMChooseLoc = record
    Allowed, Placed: Boolean;
    Resources: array[WARE_MIN..WARE_MAX] of Word;
    Units: array[CITIZEN_MIN..CITIZEN_MAX] of Byte;
  end;

  //Player manages its assets
  TKMHandCommon = class abstract
  private
    fID: TKMHandID; //Index of this hand in gHands
    fUnits: TKMUnitsCollection;
  public
    constructor Create(aHandIndex: TKMHandID);
    destructor Destroy; override;
    property ID: TKMHandID read fID;
    property Units: TKMUnitsCollection read fUnits;

    function AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPoint; aMakeCheckpoint: Boolean = True): TKMUnit;
    function RemUnit(const Position: TKMPoint): Boolean; overload;
    function RemUnit(const Position: TKMPoint; out aUnitType: TKMUnitType): Boolean; overload;
    function UnitsHitTest(const aLoc: TKMPoint; const UT: TKMUnitType = utAny): TKMUnit; overload;
    function UnitsHitTest(X, Y: Integer; const UT: TKMUnitType = utAny): TKMUnit; overload;

    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    procedure UpdateState(aTick: Cardinal); virtual;
    procedure UpdateVisualState;
    procedure Paint(const aRect: TKMRect; aTickLag: Single); virtual;
  end;

  TKMHandMessage = record
    Text : String;
    ID, Time : Integer;
    Kind: TKMMessageKind;
    function GetText : String;
  end;

  TKMHand = class(TKMHandCommon)
  private
    fAI: TKMHandAI;
    fEnabled: Boolean;
    fConstructions: TKMHandConstructions;
    fDeliveries: TKMHandLogistics;
    fFogOfWar: TKMFogOfWar; //Stores FOW info for current player, which includes
    fHouses: TKMHousesCollection;
    fStructures: TKMStructuresCollection;
    fLocks: TKMHandLocks;
    fRoadsList: TKMPointTagList; //Used only once to speedup mission loading, then freed
    fStats: TKMHandStats;
    fUnitGroups: TKMUnitGroups;
    fMessageLog: TKMMessageLog;

    fOwnerNickname: AnsiString; //Multiplayer owner nickname
    fHandType: TKMHandType;
    fCanBeHuman: Boolean;
    fCanBeAITypes: TKMAITypeSet;
    fFlagColor: Cardinal;
    fTeam: Integer;
    fTeamColor: Cardinal;
    fCenterScreen: TKMPoint;
    fChooseLocation: TKMChooseLoc;
    fAlliances: array [0 .. MAX_HANDS - 1] of TKMAllianceType;
    fShareFOW: array [0 .. MAX_HANDS - 1] of Boolean;
    fShareBeacons: array [0 .. MAX_HANDS - 1] of Boolean;

    // Overlays
    fOverlayText: UnicodeString; //Needed for replays. Not saved since it's translated
    fOverlayMarkup: AnsiString;
    fOverlayParams: TKMVarValueList;
    fOverlayTextSettings: TKMOverlayTextSettings;

    //House sketch fields, used to GetNextHouseWSameType
    fHSketch: TKMHouseSketchEdit;
    fFirstHSketch: TKMHouseSketchEdit;
    fFoundHSketch: TKMHouseSketchEdit;

    fOnAllianceChange: TEvent;
    fWorklessCitizens: Word;
    fUpdateHandEntities : Boolean;
    fNeverHungry : Boolean;

    //fBridgesBuilt : TKMStructureBasicArray;
    fCustomPanelData : TKMCustomPanelInfo;

    function IsDisabled: Boolean;
    function GetColorIndex: Byte;

    function  GetAlliances(aIndex: Integer): TKMAllianceType; inline;
    procedure SetAlliances(aIndex: Integer; aValue: TKMAllianceType); inline;
    function  GetShareFOW(aIndex: Integer): Boolean;
    procedure SetShareFOW(aIndex: Integer; aValue: Boolean);
    function  GetShareBeacons(aIndex: Integer): Boolean;
    procedure SetShareBeacons(aIndex: Integer; aValue: Boolean);

    procedure EntityDestroyed(aEntity: TKMHandEntity);
    procedure GroupDied(aGroup: TKMUnitGroup);
    procedure HouseDestroyed(aHouse: TKMHouse; aFrom: TKMHandID);
    procedure UnitDied(aUnit: TKMUnit);

    procedure UnitTrained(aUnit: TKMUnit);
    procedure WarriorWalkedOut(aWarrior: TKMUnitWarrior);
    function LocHasNoAllyPlans(const aLoc: TKMPoint): Boolean;
    function GetGameFlagColor: Cardinal;
    function GetOwnerNicknameU: UnicodeString;
    procedure ChooseFirstStorehouse();

    function GetAI: TKMHandAI;
    procedure SetFlagColor(const Value: Cardinal);

    procedure SetOwnerNickname(const aName: AnsiString);
    function GetDeliveries: TKMHandLogistics;
    procedure SetHandType(const Value: TKMHandType);
    procedure SetEnabled(const Value: Boolean);
    function GetFlagTextColor: Cardinal;
    function GetVWare(aName : String) : Integer;
    procedure SetVWare(aName : String; aValue : Integer);

    function GetVWareByID(aIndex : Integer) : Integer;
    procedure SetVWareByID(aIndex : Integer; aValue : Integer);
    procedure SetNeverHungry(aValue : Boolean);
    procedure SetWorklessCitizens(aValue : Word);
  public

    MessageStack: array of record
      Text : String;
      Loc : TKMPoint;
      Kind : TKMMessageKind;
    end;

    InCinematic: Boolean;
    ShowMessage : array of TKMHandMessage;
    VWaresCount: array of Word;

    //Used for syncing hotkeys in multiplayer saves only. UI keeps local value to avoid GIP delays
    SelectionHotkeys: array[0..DYNAMIC_HOTKEYS_NUM-1] of Integer;

    constructor Create(aHandIndex: TKMHandID; aOnAllianceChange: TEvent);
    destructor Destroy; override;

    property VirtualWare[aName : String] : Integer read GetVWare write SetVWare;default;
    property VirtualWare[aIndex : Integer] : Integer read GetVWareByID write SetVWareByID;default;
    function VirtualWareTake(aName : String; aCount : Integer = 1) : Boolean;overload;
    function VirtualWareTake(aIndex : Integer; aCount : Integer = 1) : Boolean;overload;
    //procedure SetVirtualWare(aID, aCnt);
    procedure SetVirtualWareCnt(aName : String; aCnt : Integer; addTo : Boolean = true);
    property AI: TKMHandAI read GetAI;
    property Constructions: TKMHandConstructions read fConstructions;
    property Deliveries: TKMHandLogistics read GetDeliveries;
    property Houses: TKMHousesCollection read fHouses;
    property Structures: TKMStructuresCollection read fStructures;
    property Locks: TKMHandLocks read fLocks;
    property Stats: TKMHandStats read fStats;
    property FogOfWar: TKMFogOfWar read fFogOfWar;
    property UnitGroups: TKMUnitGroups read fUnitGroups;
    property MessageLog: TKMMessageLog read fMessageLog;
    property UpdateHandEntities : Boolean read fUpdateHandEntities write fUpdateHandEntities;
    property NeverHungry : Boolean read fNeverHungry write SetNeverHungry;

    Procedure DeleteFromMessageQueue(aIndex : Integer);
    Procedure AddToMessageQueue(const aID : Integer = -1; const aTime : Integer = -1; const aKind : TKMMessageKind = mkText; const aText : String = '');

    property Enabled: Boolean read fEnabled write SetEnabled;
    property Disabled: Boolean read IsDisabled;
    procedure SetHandIndex(aNewIndex: TKMHandID);
    property OwnerNickname: AnsiString read fOwnerNickname write SetOwnerNickname; //MP owner nickname (empty in SP)
    property OwnerNicknameU: UnicodeString read GetOwnerNicknameU;
    function CalcOwnerName: UnicodeString; //Universal owner name
    function OwnerName(aNumberedAIs: Boolean = True; aLocalized: Boolean = True): UnicodeString; //Universal owner name
    function GetHandOwnerName(aIsHuman, aIsAdvAI: Boolean; aNumberedAIs: Boolean = True; aLocalized: Boolean = True): UnicodeString;
    function GetOwnerName: UnicodeString;
    function GetOwnerNameColored: AnsiString;
    function GetOwnerNameColoredU: UnicodeString;
    function HasAssets: Boolean;
    property HandType: TKMHandType read fHandType write SetHandType; //Is it Human or AI
    property CanBeHuman: Boolean read fCanBeHuman write fCanBeHuman;
    property CanBeAITypes: TKMAITypeSet read fCanBeAITypes;
    property FlagColor: Cardinal read fFlagColor write SetFlagColor;
    property FlagTextColor: Cardinal read GetFlagTextColor;
    property Team: Integer read fTeam write fTeam;
    property TeamColor: Cardinal read fTeamColor write fTeamColor;
    property GameFlagColor: Cardinal read GetGameFlagColor;
    property FlagColorIndex: Byte read GetColorIndex;
    property Alliances[aIndex: Integer]: TKMAllianceType read GetAlliances write SetAlliances;
    property ShareFOW[aIndex: Integer]: Boolean read GetShareFOW write SetShareFOW;
    property ShareBeacons[aIndex: Integer]: Boolean read GetShareBeacons write SetShareBeacons;
    property CenterScreen: TKMPoint read fCenterScreen write fCenterScreen;
    property ChooseLocation: TKMChooseLoc read fChooseLocation write fChooseLocation;
    Procedure AddJewerly(aObjectType : Word);
    procedure TakeJewerly;
    function CanMakeJewerly : Boolean;
    procedure ProceedStoreBell(aLoc : TKMPoint);

    function GetWorklessCount : Integer;
    function CanTrainSerfs : Integer;
    procedure TakeWorkless;

    property Workless : Word read fWorklessCitizens write SetWorklessCitizens;
    property OverlayText: UnicodeString read fOverlayText write fOverlayText;
    property OverlayTextSettings: TKMOverlayTextSettings read fOverlayTextSettings;
    property OverlayMarkup: AnsiString read fOverlayMarkup write fOverlayMarkup;
    property OverlayParams: TKMVarValueList read fOverlayParams;
    procedure SetOverlayTextWordWrap(aWordWrap: Boolean);
    procedure SetOverlayTextFont(aFont: TKMFont);
    procedure AddAIType(aHandAIType: TKMAIType);

    procedure PostLoadMission;

    function IsAnimal: Boolean;
    function IsHuman: Boolean;
    function IsComputer: Boolean;
    function IsAdvancedAI: Boolean;
    function IsClassicAI: Boolean;
    function IsAlliedWithHuman : Boolean;
    function IsAffectedbyMBD : Boolean;

    function CanBeAI: Boolean;

    procedure AfterMissionInit(aFlattenRoads: Boolean);

    function DuplicateUnitAt(aUnit : TKMUnit; aLoc : TKMPoint; aKillPrev : Boolean = true) : TKMUnit;
    function AddUnit(aUnitData : TKMUnitMainData; const aLoc : TKMPoint): TKMUnit; reintroduce; overload;
    function AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPoint; aAutoPlace: Boolean = True;
                     aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False; aMakeCheckpoint: Boolean = True): TKMUnit; reintroduce; overload;
    function AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPointDir; aAutoPlace: Boolean = True;
                     aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False; aMakeCheckpoint: Boolean = True): TKMUnit; reintroduce; overload;
    function AddUnitGroup(aUnitType: TKMUnitType; const Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aCount: Word;
                          aMakeCheckpoint: Boolean = True): TKMUnitGroup;

    function TrainUnit(aUnitType: TKMUnitType; aInHouse: TKMHouse): TKMUnit;

    function GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer): TKMHouse; overload;
    function GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer;
                                   out aHouseSketch: TKMHouseSketchEdit;
                                   aSketchTypesSet: TKMHouseSketchTypeSet = [hstHouse]): TKMHouse; overload;
    function GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer;
                                   out aHouseSketch: TKMHouseSketchEdit;
                                   aSketchTypesSet: TKMHouseSketchTypeSet;
                                   aVerifySketch: TAnonHouseSketchBoolFn;
                                   aVerifySketchBoolParam: Boolean): TKMHouse; overload;
    function GetNextUnitWSameType(aUnitType: TKMUnitType; aStartFromUID: Integer): TKMUnit;
    function GetNextGroupWSameType(aUnitType: TKMUnitType; aStartFromUID: Integer): TKMUnitGroup;
    function GetNextMarkerDefenceIndex(aIndex : Integer) : Integer;
    function GetNextMarkerFogIndex(aIndex : Integer) : Integer;
    function GetNextMarkerDefendIndex(aIndex : Integer) : Integer;

    function CanAddFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
    function CanAddFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
    function CanRemFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
    function CanAddHousePlan(const aLoc: TKMPoint; aHouseType: TKMHouseType): Boolean;
    function CanAddHousePlanAI(aX, aY: Word; aHouseType: TKMHouseType; aCheckInfluence: Boolean): Boolean;
    function CanAddStructurePlan(const aLoc : TKMPoint; aIndex, aRot : Word) : Boolean;
    function HasVWares(aCost : TKMVWarePlanCommon) : Boolean;
    function CanPlaceDecoration(const aLoc : TKMPoint; aIndex : Word) : Boolean;
    function CanBuildHouse(aHouseType : TKMHouseType) : Boolean;
    procedure AddFirstStorehouse(aEntrance: TKMPoint);
    procedure ResetChooseLocation;
    function NeedToChooseFirstStorehouse: Boolean;
    function NeedToChooseFirstStorehouseInGame: Boolean;
    procedure AddRoadToList(const aLoc: TKMPoint; aRoadType : TKMRoadType);
    procedure AddRoad(const aLoc: TKMPoint; aRoadType : TKMRoadType = rtNone);
    procedure AddPalisade(const aLoc: TKMPoint);
    procedure AddField(const aLoc: TKMPoint; aFieldType: TKMFieldType; aStage: Integer = 0;aGrainType : TKMGrainType = gftNone;
                      aKeepOldObject: Boolean = False; aRemoveOverlay: Boolean = True);
    procedure ToggleFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType; aMakeSound: Boolean; aRoadType : TKMRoadType);
    procedure ToggleFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType; aRoadType : TKMRoadType);
    function AddHouse(aHouseType: TKMHouseType; PosX, PosY: Word; RelativeEntrace: Boolean): TKMHouse;
    procedure AddHousePlan(aHouseType: TKMHouseType; const aLoc: TKMPoint);
    procedure AddStructurePlan(const aLoc : TKMPoint; aIndex, aRot: Word);
    procedure PlaceDecoration(const aLoc : TKMPoint; aIndex: Word);

    function HasHousePlan(const aLoc: TKMPoint): Boolean;
    function AddHouseWIP(aHouseType: TKMHouseType; const aLoc: TKMPoint): TKMHouse;
    function RemGroup(const Position: TKMPoint): Boolean;
    procedure RemHouse(const Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
    procedure RemHousePlan(const Position: TKMPoint);
    procedure RemFieldPlan(const Position: TKMPoint; aMakeSound:Boolean);
    procedure RemFakeFieldPlan(const Position: TKMPoint);
    function FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean = False): TKMHouseInn;
    function FindHouse(aType: TKMHouseType; const aPosition: TKMPoint; Index: Byte = 1): TKMHouse; overload;
    function FindHouse(aType: TKMHouseType; Index: Byte=1): TKMHouse; overload;
    function FindHousesInRadius(const aLoc: TKMPoint; aSqrRadius: Single; aTypes: TKMHouseTypeSet = HOUSES_VALID; aOnlyCompleted: Boolean = True): TKMHouseArray;
    function FindCityCenter: TKMPoint;
    function HitTest(X,Y: Integer): TObject;
    function HousesHitTest(X, Y: Integer): TKMHouse;
    function StructuresHitTest(X, Y: Integer): TKMStructure;
    function GroupsHitTest(X, Y: Integer): TKMUnitGroup;
    function ObjectByUID(aUID: Integer): TObject;
    procedure GetHouseMarks(const aLoc: TKMPoint; aHouseType: TKMHouseType; aList: TKMPointTagList; aIgnoreFOW: Boolean = False; aIgnoreObjects: Boolean = false);
    procedure GetStructureMarks(const aLoc: TKMPoint; aIndex, aRot: Word; aList : TKMPointTagList; aIgnoreFOW: Boolean = false);
    procedure TakeOverHouse(aHouse : TKMHouse);
    procedure TakeOverAppleTree(aHouse : TKMHouse);

    function GetClosestHouse(aLoc : TKMPoint; aHouseTypeSet : TKMHouseTypeSet; aWareSet : TKMWareTypeSet = [wtAll];  aMaxDistance : Single = 999) : TKMHouse;
    function GetClosestStore(aLoc : TKMPoint; aWare: TKMWareType) : TKMHouse;
    function GetClosestBarracks(aLoc : TKMPoint; aWare: TKMWareType) : TKMHouse;


    function GetFieldsCount: Integer;
    procedure GetFieldPlans(aList: TKMPointTagList; const aRect: TKMRect; aIncludeFake: Boolean);
    procedure GetHousePlans(aList: TKMPointDirList; const aRect: TKMRect);
    procedure GetPlansTablets(aList: TKMPointTagList; const aRect: TKMRect);
    function CanDoStatsUpdate(aTick: Cardinal): Boolean;
    function DoCheckGoals: Boolean;
    Procedure ShowMSG(aKind : TKMMessageKind; aMessage: string; aLoc : TKMPoint);

    //custom panel procedures
    function AddControl(aInfo : TKMControlInfo) : Integer;
    procedure ControlChange(aID: Integer; aInfo : TKMControlInfo);
    procedure ControlSetVisibility(aID: Integer; aVisible : Boolean);
    procedure ControlSetCaption(aID: Integer; aCaption : String);
    procedure ControlSetHint(aID: Integer; aHint : String);
    procedure ControlSetTexID(aID: Integer; aTexID : Integer);
    procedure ControlSetRect(aID: Integer; X, Y, Width, Height : Integer);
    procedure ControlSetEnabled(aID: Integer; aEnabled : Boolean);
    procedure ControlSetAlphaStep(aID: Integer; aStep : Single);
    procedure ResizePanel(X, Y, Width, Height : Integer);
    property CustomPanelData: TKMCustomPanelInfo read fCustomPanelData;

    procedure BuildBridge(aLoc : TKMPoint; aIndex, aRot : Word);
    //function HasBridgeBuiltAt(aLoc : TKMPoint) : Boolean;
    //property BridgesBuilt : TKMStructureBasicArray read fBridgesBuilt;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure IncAnimStep;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint(const aRect: TKMRect; aTickLag: Single); override;
    function ObjToString(aSeparator: String = ' '): String;



  end;

  TKMAnimalSpawner = record
    Loc : TKMPoint;
    Radius : Byte;
    AnimalTypes : TKMUnitTypeArray;
    Animals : TKMUnitsArray;
    Pace,
    MaxCount : Integer;
    procedure IncludeAnimal(aType : TKMUnitType);
    procedure ExcludeAnimal(aType : TKMUnitType);
    function HasType(aType : TKMUnitType) : Boolean;
    function GetType : TKMUnitType;
    procedure ClearAnimalTypes;
  end;

  PKMAnimalSpawner = ^TKMAnimalSpawner;

  TKMHandAnimals = class(TKMHandCommon)
  private
    fSpawners : array of TKMAnimalSpawner;
    function GetSpawner(aIndex : Integer) : PKMAnimalSpawner;
    function GetSpawnersCount : Word;
    function GetSpawnerAtLoc(aLoc : TKMPoint) : PKMAnimalSpawner;
  protected
  public
    function GetFishInWaterBody(aWaterID: Byte; aFishermanPos: TKMPoint; FindHighestCount: Boolean = True; aRadius : Integer = 0): TKMUnitFish;

    property SpawnersCount : Word read GetSpawnersCount;
    property Spawners[aIndex : Integer] : PKMAnimalSpawner read GetSpawner;
    procedure AddSpawner(aLoc: TKMPoint; aRadius: Byte; aMaxCount, aSpawnPace : Integer; aAnimals: TKMUnitTypeArray);
    procedure AddAnimalTypeToLastSpawner(aAnimal: TKMUnitType);
    procedure RemoveSpawner(aIndex : integer);
    function IsSpawnerInRadius(aLoc : TKMPoint; aRadius : Single) : Boolean;
    function GetNextMarkerSpawnerIndex(aIndex : Integer) : Integer;
    procedure RemoveFromSpawner(aAnimalID : TKMUnit; aIndex : Integer);


    constructor Create(aHandIndex: TKMHandID);

    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint(const aRect: TKMRect; aTickLag: Single); override;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
  end;

  function GetStatsUpdatePeriod: Integer;


implementation
uses
  Classes, SysUtils, KromUtils, Math, TypInfo,
  KM_Entity,
  KM_Cursor, KM_Game, KM_GameParams, KM_Terrain,
  KM_HandsCollection, KM_Sound, KM_AIFields, KM_MapEdTypes,
  KM_HouseStore, KM_HouseSchool,  KM_HouseBarracks,KM_HouseHelpers,
  KM_Resource, KM_ResSound, KM_ResTexts, KM_ResMapElements, KM_ScriptingEvents, KM_ResUnits,
  KM_RenderPool, KM_RenderAux,
  KM_CommonUtils, KM_GameSettings,
  KM_UnitGroupTypes,
  KM_MapTypes, KM_HouseCottage;

const
  TIME_TO_SET_FIRST_STOREHOUSE = 10*60*2; //We give 2 minutes to set first storehouse, otherwise player will be defeated

function TKMHandMessage.GetText: string;
begin
  if ID > -1 then
    Result := '<$' + IntTOStr(ID) + '>'
  else
    Result := Text;
end;
{ TKMHandCommon }
constructor TKMHandCommon.Create(aHandIndex: TKMHandID);
begin
  inherited Create;

  fID := aHandIndex;
  fUnits := TKMUnitsCollection.Create;
end;


destructor TKMHandCommon.Destroy;
begin
  FreeThenNil(fUnits);

  inherited;
end;

function TKMHandCommon.AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPoint; aMakeCheckpoint: Boolean = True): TKMUnit;
begin
  //Animals are autoplaced by default
  Result := fUnits.AddUnit(fID, aUnitType, KMPointDir(aLoc, dirS), True);


  if gGameParams.IsMapEditor and aMakeCheckpoint then
    gGame.MapEditor.History.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                           [gRes.Units[aUnitType].GUIName, aLoc.ToString]));
end;


procedure TKMHandCommon.Paint(const aRect: TKMRect; aTickLag: Single);
begin
  if mlUnits in gGameParams.VisibleLayers then
    fUnits.Paint(aRect, aTickLag);
end;


function TKMHandCommon.RemUnit(const Position: TKMPoint): Boolean;
var
  UnitType: TKMUnitType;
begin
  Result := RemUnit(Position, UnitType);
end;


function TKMHandCommon.RemUnit(const Position: TKMPoint; out aUnitType: TKMUnitType): Boolean;
var
  U: TKMUnit;
begin
  Assert(gGameParams.IsMapEditor);

  U := fUnits.HitTest(Position.X, Position.Y);

  Result := U <> nil;

  if Result then
  begin
    aUnitType := U.UnitType;
    fUnits.RemoveUnit(U);
  end;
end;


procedure TKMHandCommon.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('HandCommon');
  fUnits.Save(SaveStream);
end;


procedure TKMHandCommon.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('HandCommon');
  fUnits.Load(LoadStream);
end;


procedure TKMHandCommon.SyncLoad;
begin
  fUnits.SyncLoad;
end;


function TKMHandCommon.UnitsHitTest(const aLoc: TKMPoint; const UT: TKMUnitType = utAny): TKMUnit;
begin
  Result := UnitsHitTest(aLoc.X, aLoc.Y, UT);
end;


function TKMHandCommon.UnitsHitTest(X, Y: Integer; const UT: TKMUnitType = utAny): TKMUnit;
begin
  Result := fUnits.HitTest(X, Y, UT);
end;


procedure TKMHandCommon.UpdateState(aTick: Cardinal);
begin
  fUnits.UpdateState(aTick);
end;


procedure TKMHandCommon.UpdateVisualState;
begin
  Assert(gGameParams.IsMapEditor);

  fUnits.UpdateVisualState;
end;


{ TKMHand }
constructor TKMHand.Create(aHandIndex: TKMHandID; aOnAllianceChange: TEvent);
var
  I: Integer;
begin
  inherited Create(aHandIndex);

  Enabled := True;

  fOnAllianceChange := aOnAllianceChange;
  fAI           := TKMHandAI.Create(fID);
  fFogOfWar     := TKMFogOfWar.Create(gTerrain.MapX, gTerrain.MapY, gGameParams.DynamicFOW);
  fLocks        := TKMHandLocks.Create;
  fStats        := TKMHandStats.Create;
  fRoadsList    := TKMPointTagList.Create;
  fHouses       := TKMHousesCollection.Create;
  fStructures   := TKMStructuresCollection.Create;
  fDeliveries   := TKMHandLogistics.Create(fID);
  fConstructions:= TKMHandConstructions.Create;
  fUnitGroups   := TKMUnitGroups.Create;
  fMessageLog   := TKMMessageLog.Create;

  fOwnerNickname := '';
  fHandType     := hndComputer;
  fCanBeHuman   := False;
  fCanBeAITypes  := [];
  for I := 0 to MAX_HANDS - 1 do
  begin
    fShareFOW[I] := True; //Share FOW between allies by default (it only affects allied players)
    fShareBeacons[I] := True; //Share beacons between allies by default (it only affects allied players)
  end;
  for I := 0 to 9 do
    SelectionHotkeys[I] := -1; //Not set

  fAlliances[fID] := atAlly; //Others are set to enemy by default
  fFlagColor := DEFAULT_PLAYERS_COLORS[fID]; //Init with default color, later replaced by Script
  fTeamColor := fFlagColor;
  fTeam := NO_TEAM;

  fOverlayParams := TKMVarValueList.Create;
  fOverlayTextSettings.WordWrap := False;
  fOverlayTextSettings.Font := fntMetal;

  fHSketch := TKMHouseSketchEdit.Create;
  fFirstHSketch := TKMHouseSketchEdit.Create;
  fFoundHSketch := TKMHouseSketchEdit.Create;

  ResetChooseLocation;
  SetLength(VWaresCount, gRes.Wares.VirtualWares.Count);
  fUpdateHandEntities := true;

  with fCustomPanelData do
  begin
    PanelSizeX := 500;
    PanelSizeY := 350;
    ControlsCount := 0;
    SetLength(ControlsData, 0);
  end;
end;


//Destruction order is important as Houses and Units need to access
//Stats/Deliveries and other collection in their Destroy/Abandon/Demolish methods
destructor TKMHand.Destroy;
begin
  FreeAndNil(fHSketch);
  FreeAndNil(fFirstHSketch);
  FreeAndNil(fFoundHSketch);

  FreeAndNil(fOverlayParams);

  //Groups freed before units since we need to release pointers they have to units
  FreeThenNil(fUnitGroups);
  FreeThenNil(fMessageLog);

  //Free units
  inherited;

  FreeThenNil(fRoadsList);
  FreeThenNil(fHouses);
  FreeThenNil(fStructures);

  //Should be freed after Houses and Units, as they write Stats on Destroy
  FreeThenNil(fLocks);
  FreeThenNil(fStats);
  FreeThenNil(fFogOfWar);
  FreeThenNil(fDeliveries);
  FreeThenNil(fConstructions);
  FreeThenNil(fAI);
end;


procedure TKMHand.ResetChooseLocation;
begin
  if Self = nil then Exit;
  
  fChooseLocation.Allowed := False;
  fChooseLocation.Placed := False;
end;


function TKMHand.DuplicateUnitAt(aUnit : TKMUnit; aLoc : TKMPoint; aKillPrev : Boolean = true) : TKMUnit;
var G : TKMUnitGroup;
begin
  if aUnit is TKMUnitWarrior then
  begin
    G := AddUnitGroup(aUnit.UnitType,
                      aLoc,
                      aUnit.Direction,
                      1,
                      1);
    Result := G.Members[0];
    Result.Condition := aUnit.Condition;
    TKMUnitWarrior(Result).BoltCount := TKMUnitWarrior(aUnit).BoltCount;
  end else
  begin
    Result := AddUnit(aUnit.UnitType, aLoc);
    Result.Condition := aUnit.Condition;
  end;


  if aKillPrev then
    aUnit.Kill(HAND_NONE, false, True);
end;

function TKMHand.AddUnit(aUnitData: TKMUnitMainData; const aLoc: TKMPoint): TKMUnit;
var I : integer;
  G : TKMUnitGroup;
begin
  Result := nil;
  if aUnitData.UnitType in UNITS_WARRIORS then
  begin
    with aUnitData do
      G := AddUnitGroup(UnitType, aLoc, dirS, Columns, Count);

    if G = nil then
      Exit;

    for I := 0 to G.Count - 1 do
    begin
      G.Members[I].BoltCount := aUnitData.BoltCount;
      G.Members[I].Condition := aUnitData.Condition;
    end;

    Result := G.Members[0];

  end else
  begin
    Result := AddUnit(aUnitData.UnitType, aLoc);
    if Result <> nil then
      Result.Condition := aUnitData.Condition;
  end;

  Result.Condition := aUnitData.Condition;
end;

function TKMHand.AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPoint; aAutoPlace: Boolean = True;
                         aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False; aMakeCheckpoint: Boolean = True): TKMUnit;
begin
  Result := AddUnit(aUnitType,  KMPointDir(aLoc, dirS), aAutoPlace, aRequiredWalkConnect, aCheat, aMakeCheckpoint);
end;


//Place unit of aUnitType to aLoc via script
//AutoPlace - add unit to nearest available spot if aLoc is already taken (or unwalkable)
function TKMHand.AddUnit(aUnitType: TKMUnitType; const aLoc: TKMPointDir; aAutoPlace: Boolean = True;
                         aRequiredWalkConnect: Byte = 0; aCheat: Boolean = False; aMakeCheckpoint: Boolean = True): TKMUnit;
var
  G: TKMUnitGroup;
begin
  Result := fUnits.AddUnit(fID, aUnitType, aLoc, aAutoPlace, aRequiredWalkConnect);


  //Unit failed to add, that happens
  if Result = nil then Exit;

  if NeverHungry and not gGameParams.IsMapEditor then
    Result.NeverHungry := true;

  if gGameParams.IsMapEditor then
  begin
    if aMakeCheckpoint then
      gGame.MapEditor.History.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                            [gRes.Units[aUnitType].GUIName, aLoc.ToString]));

    Exit;
  end;

  Result.OnUnitDied := UnitDied;
  Result.OnUnitTrained := UnitTrained; //Used for debug Scout placed by a cheat

  if Result is TKMUnitWarrior then
    TKMUnitWarrior(Result).OnWarriorWalkOut := WarriorWalkedOut;

  if Result is TKMUnitWorker then
    fConstructions.AddWorker(TKMUnitWorker(Result));
  if Result is TKMUnitSerf then
    fDeliveries.AddSerf(TKMUnitSerf(Result));

  if not aCheat then
    fStats.UnitCreated(aUnitType, False)
  else
    if Result is TKMUnitWarrior then
    begin
      //When we place a cheat Scout we want to rig it immediately

      //It's simpler to count cheat scouts as initial, rather than trained.
      //Then we don't need to mess with initial recruits.
      fStats.UnitCreated(aUnitType, False);

      G := fUnitGroups.WarriorTrained(TKMUnitWarrior(Result));
      Assert(G <> nil, 'It is certain that equipped warrior creates or finds some group to join to');
      G.OnGroupDied := GroupDied;

      //Scripting doesn't care about scouts added this way.
      //It could cause issues if the scripter assumes the warrior came from the player's barracks.
      //The event is "OnWarriorEquipped" not "OnWarriorCreated".
      //fScriptingESA.ProcWarriorEquipped(Result, G);
    end;
end;


//Start training unit in School/Barracks
//User can cancel the training, so we don't add unit to stats just yet
function TKMHand.TrainUnit(aUnitType: TKMUnitType; aInHouse: TKMHouse): TKMUnit;
begin
  Assert(aInHouse <> nil, 'House to train unit in could not be nil');
  // Add unit and specify that its made inside house (so there is no need to occupy terrain tile)
  Result := fUnits.AddUnit(fID, aUnitType, KMPointDir(aInHouse.Entrance, dirS), False, 0, aInHouse);
  Result.OnUnitDied := UnitDied;
  Result.OnUnitTrained := UnitTrained;


  if Result is TKMUnitWarrior then
    TKMUnitWarrior(Result).OnWarriorWalkOut := WarriorWalkedOut;

  //Do not add unit to statistic just yet, wait till it's training complete
end;


procedure TKMHand.UnitTrained(aUnit: TKMUnit);
begin
  if aUnit.UnitType = utBuilder then
    fConstructions.AddWorker(TKMUnitWorker(aUnit));
  if aUnit.UnitType = utSerf then
    fDeliveries.AddSerf(TKMUnitSerf(aUnit));

  if NeverHungry and not gGameParams.IsMapEditor then
      aUnit.NeverHungry := true;

  //Warriors don't trigger "OnTrained" event, they trigger "WarriorEquipped" in WarriorWalkedOut below
  if not (aUnit is TKMUnitWarrior) then
    gScriptEvents.ProcUnitTrained(aUnit);

  fStats.UnitCreated(aUnit.UnitType, True{, (aUnit.InHouse <> nil) and (aUnit.InHouse.HouseType = htTownHall)});
end;


procedure TKMHand.WarriorWalkedOut(aWarrior: TKMUnitWarrior);
var
  G, G2: TKMUnitGroup;
  H: TKMHouse;
  HWFP: TKMHouseWFlagPoint;
begin
  // Warrior could be killed before he walked out, f.e. by script OnTick ---> Actions.UnitKill
  // Then group will be assigned to invalid warrior and never gets removed from game
  if (aWarrior = nil) or aWarrior.IsDeadOrDying then
    Exit;

  G := fUnitGroups.WarriorTrained(aWarrior);
  if aWarrior.UnitType = utLekter then
    Exit;
  Assert(G <> nil, 'It is certain that equipped warrior creates or finds some group to join to');
  G.OnGroupDied := GroupDied;
  if IsComputer then
  begin
    if AI.Setup.NewAI then
      AI.ArmyManagement.WarriorEquipped(G)
    else
      AI.General.WarriorEquipped(aWarrior);
    G := UnitGroups.GetGroupByMember(aWarrior); //AI might assign warrior to different group
  end
  else
    if G.Count = 1 then
    begin
      //If player is human and this is the first warrior in the group, send it to the rally point
      H := HousesHitTest(aWarrior.Position.X, aWarrior.Position.Y-1);
      if (H is TKMHouseWFlagPoint) then
      begin
        HWFP := TKMHouseWFlagPoint(H);
        HWFP.ValidateFlagPoint; // Validate Flag point first. It will set it to a proper walkable position
        if HWFP.IsFlagPointSet
        and G.CanWalkTo(HWFP.FlagPoint, 0) then
        begin
          G2 := gHands[G.Owner].GroupsHitTest(HWFP.FlagPoint.X, HWFP.FlagPoint.Y);
          if (G2 <> nil) and G.CanLinkTo(G2) then
            G.OrderLinkTo(G2, true)
          else
            G.OrderWalk(HWFP.FlagPoint, True, wtokFlagPoint);
        end;
      end;
    end;
  gScriptEvents.ProcWarriorEquipped(aWarrior, G);
end;


function TKMHand.AddUnitGroup(aUnitType: TKMUnitType; const Position: TKMPoint; aDir: TKMDirection; aUnitPerRow, aCount: Word;
                              aMakeCheckpoint: Boolean = True): TKMUnitGroup;
var
  I: Integer;
begin
  Assert(aDir <> dirNA);
  Result := nil;

  if aUnitType in [CITIZEN_MIN..CITIZEN_MAX] then
    for I := 0 to aCount - 1 do
      AddUnit(aUnitType, Position, True, 0, False, aMakeCheckpoint)
  else
  if aUnitType in [WARRIOR_MIN..WARRIOR_MAX] then
    Result := fUnitGroups.AddGroup(fID, aUnitType, Position.X, Position.Y, aDir, aUnitPerRow, aCount);
  if NeverHungry and not gGameParams.IsMapEditor then
    if Result <> nil then
      Result.NeverHungry := true;
  //Group can be nil if it fails to be placed on terrain (e.g. because of terrain height passability)
  if Result <> nil then
    Result.OnGroupDied := GroupDied;

  if gGameParams.IsMapEditor and aMakeCheckpoint then
    gGame.MapEditor.History.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                           [gRes.Units[aUnitType].GUIName, Position.ToString]));

  //Units will be added to statistic inside the function for some units may not fit on map
end;


//When adding roads from script we want to batch them all into one list to save time
//on WalkConnect and other calculations
procedure TKMHand.AddRoadToList(const aLoc: TKMPoint; aRoadType : TKMRoadType);
begin
  Assert(fRoadsList <> nil);

  //Sometimes maps can have roads placed outside of map bounds - ignore them
  //(on 80x80 map Loc range is 1..79, which is not obvious when placing roads manually in script)
  if gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
    fRoadsList.Add(aLoc, byte(aRoadType));
end;


function TKMHand.IsHuman: Boolean;
begin
  Result := fHandType = hndHuman;
end;


function TKMHand.IsAnimal: Boolean;
begin
  Result := fID = HAND_ANIMAL;
end;


function TKMHand.IsComputer: Boolean;
begin
  Result := fHandType = hndComputer;
end;


function TKMHand.IsAdvancedAI: Boolean;
begin
  Result := IsComputer and fAI.Setup.NewAI;
end;


function TKMHand.IsClassicAI: Boolean;
begin
  Result := IsComputer and not fAI.Setup.NewAI;
end;

function TKMHand.IsAlliedWithHuman: Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to gHands.Count - 1 do
    if gHands[I].Enabled and gHands[I].IsHuman then
      if gHands[I].Alliances[ID] = atAlly then
        Exit(true);
end;

function TKMHand.IsAffectedbyMBD: Boolean;
begin
  if self = nil then
    Exit(false);
  Result := IsHuman or IsAlliedWithHuman;
end;


function TKMHand.CanBeAI: Boolean;
begin
  Result := (fCanBeAITypes <> []) and (fCanBeAITypes <> [aitNone]);
end;


//Lay out all roads at once to save time on Terrain lighting/passability recalculations
procedure TKMHand.AfterMissionInit(aFlattenRoads: Boolean);
var I : Integer;
  H : TKMHouseStore;
begin
  if (Self = nil) or not Enabled then Exit;

  Assert(fRoadsList <> nil);

  gTerrain.SetRoads(fRoadsList, fID, not aFlattenRoads); //If we are flattening roads that will update WalkConnect anyway
  if aFlattenRoads then
    gTerrain.FlattenTerrain(fRoadsList);

  if not gGameParams.IsMapEditor then
    fAI.AfterMissionInit(fCanBeAITypes);

  if (IsComputer and not gGameParams.IsMapEditor) then
    for I := 0 to Stats.GetHouseQty(htStore) do
    begin
      H := TKMHouseStore(FindHouse(htStore, I + 1));
      if (H <> nil) and not H.IsDestroyed then
        H.MaxCount := 3000000;
    end;


end;


function TKMHand.GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer): TKMHouse;
begin
  Result := GetNextHouseWSameType(aHouseType, aStartFromUID, TKMHouseSketchEdit.DummyHouseSketch);
end;


function TKMHand.GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer;
                                       out aHouseSketch: TKMHouseSketchEdit;
                                       aSketchTypesSet: TKMHouseSketchTypeSet = [hstHouse]): TKMHouse;
begin
  Result := GetNextHouseWSameType(aHouseType, aStartFromUID, aHouseSketch, aSketchTypesSet, nil, False);
end;


function TKMHand.GetNextHouseWSameType(aHouseType: TKMHouseType; aStartFromUID: Integer;
                                       out aHouseSketch: TKMHouseSketchEdit;
                                       aSketchTypesSet: TKMHouseSketchTypeSet;
                                       aVerifySketch: TAnonHouseSketchBoolFn;
                                       aVerifySketchBoolParam: Boolean): TKMHouse;

var
  resultSet: Boolean;

  procedure FillHSketchByHouse(out aHouseSketchTmp: TKMHouseSketchEdit; aHouse: TKMHouse);
  begin
    if not aHouse.IsDestroyed // not destroyed
      and (aHouse.HouseType = aHouseType)
      {and (not aConsiderHousePlan or aHouse.IsComplete)} then
    begin
      aHouseSketchTmp.SetHouseUID(aHouse.UID);
      aHouseSketchTmp.SetHouseType(aHouse.HouseType);
      aHouseSketchTmp.SetPosition(aHouse.Position);
    end;
  end;

  procedure FillHSketchByHPlan(out aHouseSketchTmp: TKMHouseSketchEdit; aHousePlan: TKMHousePlan);
  begin
    if not aHousePlan.IsEmpty
      and (aHousePlan.HouseType = aHouseType) then
    begin
      aHouseSketchTmp.SetHouseUID(aHousePlan.UID);
      aHouseSketchTmp.SetHouseType(aHousePlan.HouseType);
      aHouseSketchTmp.SetPosition(aHousePlan.Loc);
    end;
  end;

  function GetNextHSketch(aIndex: Integer; out aHouseSketchTmp: TKMHouseSketchEdit): Boolean;
  var
    sketch2Verify: TKMHouseSketch;
  begin
    aHouseSketchTmp.Clear;

    sketch2Verify := nil;

    if (hstHouse in aSketchTypesSet) and (aIndex < fHouses.Count) then
    begin
      FillHSketchByHouse(aHouseSketchTmp, fHouses[aIndex]);
      sketch2Verify := fHouses[aIndex];
    end else if (hstHousePlan in aSketchTypesSet) then
    begin
      FillHSketchByHPlan(aHouseSketchTmp, fConstructions.HousePlanList.Plans[aIndex - Byte(hstHouse in aSketchTypesSet)*fHouses.Count]);
      sketch2Verify := aHouseSketchTmp;
    end;

    Result := not aHouseSketchTmp.IsEmpty
              and (not Assigned(aVerifySketch) or aVerifySketch(sketch2Verify, aVerifySketchBoolParam));
  end;

  procedure FillResult(aIndex: Integer; aHSketch: TKMHouseSketchEdit);
  begin
    resultSet := True;
    if aHouseSketch <> nil then
      aHSketch.CopyTo(aHouseSketch);
    if aIndex < fHouses.Count then
      Result := fHouses[aIndex];
  end;

var
  found: Boolean; //Flag when we find house sketch (House or HousePlan) with specified Starting UID
  I, firstHSketchI, foundSketchI, cnt: Integer;
begin
  Result := nil;
  aHouseSketch.Clear;

  found := False;
  resultSet := False;

  cnt :=   Byte(hstHouse in aSketchTypesSet)*fHouses.Count
         + Byte(hstHousePlan in aSketchTypesSet)*fConstructions.HousePlanList.Count;

  I := 0;
  firstHSketchI := 0;
  foundSketchI := 0;

  fHSketch.Clear;
  fFirstHSketch.Clear;
  fFoundHSketch.Clear;

  while I < cnt do
  begin
    try
      if not GetNextHSketch(I, fHSketch) then
        Continue;

      //Just find any first house
      if (aStartFromUID = 0) then
      begin
        FillResult(I, fHSketch);
        Break;
      end;

      //Find first house from specified UID
      if (fHSketch.UID = aStartFromUID) then
      begin
        found := True; // Mark that we found our house
        fHSketch.CopyTo(fFoundHSketch);
        foundSketchI := I;
      end
      else if found then
      begin
        FillResult(I, fHSketch); // Save the next house after Found to Result and Break
        Break;
      end else if fFirstHSketch.IsEmpty then
      begin
        fHSketch.CopyTo(fFirstHSketch); // Save 1st house in list in case our house is the last one
        firstHSketchI := I;
      end;
    finally
      Inc(I);
    end;
  end;

  if not resultSet then // Found should be always True here
  begin
    if found then
    begin
      if fFirstHSketch.IsEmpty then
        FillResult(foundSketchI, fFoundHSketch)   //Could happen, when we have only 1 house with that type...
      else
        FillResult(firstHSketchI, fFirstHSketch);
    end else if not fFirstHSketch.IsEmpty then
      FillResult(firstHSketchI, fFirstHSketch);
  end;
end;


function TKMHand.GetNextUnitWSameType(aUnitType: TKMUnitType; aStartFromUID: Integer): TKMUnit;
var
  I: Integer;
  U, firstU, lastU: TKMUnit;
  found: Boolean;
begin
  Result := nil;

  found := False;
  firstU := nil;
  lastU := nil;

  for I := 0 to fUnits.Count - 1 do
  begin
    U := fUnits[I];
    if (U = nil)
    or U.IsDeadOrDying
    or (U.UnitType <> aUnitType)
    or not U.Visible then
      Continue;

    // Just find any first unit
    if (aStartFromUID = 0) then
      Exit(U);

    lastU := U;

    if U.UID = aStartFromUID then
      found := True               // Mark that we found our unit
    else if found then
      Exit(U)                     // Save the next unit after Found to Result and Break
    else
    if firstU = nil then
      firstU := U;                // Save 1st unit in list in case our unit is the last one
  end;

  if found then          // Found should be always True here
  begin
    if firstU = nil then //Could happen, when we have only 1 unit with that type...
      Result := lastU
    else
      Result := firstU;
  end;
end;


function TKMHand.GetNextGroupWSameType(aUnitType: TKMUnitType; aStartFromUID: Integer): TKMUnitGroup;
var
  I: Integer;
  group, firstG, lastG: TKMUnitGroup;
  found: Boolean;
begin
  Result := nil;

  found := False;
  firstG := nil;
  lastG := nil;

  for I := 0 to UnitGroups.Count - 1 do
  begin
    group := UnitGroups[I];

    if (group = nil)
      or group.IsDead //check if group is dead
      or not group.HasUnitType(aUnitType) then // we are interested in groups with the same type only
      Continue;

    //Just find any first house
    if (aStartFromUID = 0) then
    begin
      Result := group;
      Break;
    end;

    lastG := group;

    if group.UID = aStartFromUID then
      found := True               // Mark that we found our group
    else if found then
    begin
      Result := group;            // Save the next group after Found to Result and Break
      Break;
    end else if firstG = nil then
      firstG := group;            // Save 1st group in list in case our group is the last one
  end;
  if (Result = nil) and found then // Found should be always True here
  begin
    if firstG = nil then  //Could happen, when we have only 1 group with that type...
      Result := lastG
    else
      Result := firstG;
  end;
end;

function TKMHand.GetNextMarkerDefenceIndex(aIndex: Integer): Integer;
begin
  Result := aIndex;
  IncLoop(Result, 0, fAi.General.DefencePositions.Count - 1, 1);
end;

function TKMHand.GetNextMarkerDefendIndex(aIndex: Integer): Integer;
begin
  Result := aIndex;
  IncLoop(Result, 0, fAi.General.DefendPositions.Count - 1, 1);
end;

function TKMHandAnimals.GetNextMarkerSpawnerIndex(aIndex: Integer): Integer;
begin
  Result := aIndex;
  IncLoop(Result, 0, high(fSpawners), 1);
end;


function TKMHand.GetNextMarkerFogIndex(aIndex: Integer): Integer;
begin
  Result := aIndex;

  IncLoop(Result, 0, gGame.MapEditor.Revealers[ID].Count - 1, 1);
end;

procedure TKMHand.SetHandIndex(aNewIndex: TKMHandID);
begin
  fID := aNewIndex;
  fUnits.OwnerUpdate(aNewIndex);
  fHouses.OwnerUpdate(aNewIndex);
  fAI.OwnerUpdate(aNewIndex);
end;


procedure TKMHand.SetHandType(const Value: TKMHandType);
begin
  fHandType := Value;
end;


procedure TKMHand.SetOverlayTextFont(aFont: TKMFont);
begin
  fOverlayTextSettings.Font := aFont;
end;


procedure TKMHand.SetOverlayTextWordWrap(aWordWrap: Boolean);
begin
  fOverlayTextSettings.WordWrap := aWordWrap;
end;


procedure TKMHand.SetOwnerNickname(const aName: AnsiString); //MP owner nickname (empty in SP)
begin
  fOwnerNickname := aName;
end;


procedure TKMHand.AddRoad(const aLoc: TKMPoint; aRoadType : TKMRoadType = rtNone);
begin
  gTerrain.SetRoad(aLoc, fID, aRoadType);
end;

procedure TKMHand.AddPalisade(const aLoc: TKMPoint);
begin
  gTerrain.SetPalisade(aLoc, fID);
end;

procedure TKMHand.AddField(const aLoc: TKMPoint; aFieldType: TKMFieldType; aStage: Integer = 0; aGrainType : TKMGrainType = gftNone;
                           aKeepOldObject: Boolean = False; aRemoveOverlay: Boolean = True);
var
  isFieldSet: Boolean;
  obj: Word;
begin
  isFieldSet := False;
  obj := gTerrain.Land^[aLoc.Y,aLoc.X].Obj;
  //If we have corn/wine object on that tile, set appropriate field/wine stage
  if (aFieldType = ftCorn) and not gTerrain.TileIsCornField(aLoc) then
  begin
    if ObjectIsCorn(obj) then
    begin
      gTerrain.SetField(aLoc, fID, aFieldType, gMapElements[obj].IsCorn, aGrainType, True, aKeepOldObject, aRemoveOverlay);
      isFieldSet := True;
    end;
  end else
  if (aFieldType = ftGrassland) and not gTerrain.TileIsGrassField(aLoc) then
  begin
    if ObjectIsGrass(obj) then
    begin
      gTerrain.SetField(aLoc, fID, aFieldType, gMapElements[obj].IsGrass, aGrainType, True, aKeepOldObject, aRemoveOverlay);
      isFieldSet := True;
    end;
  end else
  if (aFieldType = ftVegeField) and not gTerrain.TileIsVegeField(aLoc) then
  begin
    if ObjectIsVege(obj) then
    begin
      gTerrain.SetField(aLoc, fID, aFieldType, gMapElements[obj].IsVege, aGrainType, True, aKeepOldObject, aRemoveOverlay);
      isFieldSet := True;
    end;
  end else
  if (aFieldType = ftWine) and not gTerrain.TileIsWineField(aLoc) then
  begin
    if ObjectIsWine(obj) then
    begin
      gTerrain.SetField(aLoc, fID, aFieldType, gMapElements[obj].IsWine - 1, aGrainType, True, aKeepOldObject, aRemoveOverlay);
      isFieldSet := True;
    end;
  end;

  if not isFieldSet then
    gTerrain.SetField(aLoc, fID, aFieldType, aStage, aGrainType, True, aKeepOldObject, aRemoveOverlay);
end;


function TKMHand.LocHasNoAllyPlans(const aLoc: TKMPoint): Boolean;
var
  I: Integer;
begin
  Result := True;
  //Don't allow placing on allies plans either
  for I := 0 to gHands.Count - 1 do
    if (I <> fID) and (fAlliances[I] = atAlly) then
      Result := Result and (gHands[i].fConstructions.FieldworksList.HasField(aLoc) = ftNone)
                       and not gHands[i].fConstructions.HousePlanList.HasPlan(aLoc)
                       {and not gHands[i].fConstructions.BridgePlanList.HasPlan(aLoc)};
end;


function TKMHand.GetGameFlagColor: Cardinal;
begin
  Result := fFlagColor;
  if gGame = nil then Exit;

  case gGameSettings.PlayersColorMode of
    pcmAllyEnemy: begin
                    if ID = gMySpectator.HandID then
                      Result := gGameSettings.PlayerColorSelf
                    else if (Alliances[gMySpectator.HandID] = atAlly) then
                      Result := gGameSettings.PlayerColorAlly
                    else
                      Result := gGameSettings.PlayerColorEnemy;
                  end;
    pcmTeams:     Result := fTeamColor;
  end;
end;


function TKMHand.GetOwnerNicknameU: UnicodeString;
begin
  Result := UnicodeString(fOwnerNickname);
end;


function TKMHand.IsDisabled: Boolean;
begin
  Result := not Enabled;
end;


//See comment on CanAddFakeFieldPlan
function TKMHand.CanAddFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
begin
  Result := gTerrain.CanAddField(aLoc.X, aLoc.Y, aFieldType, ID)
            and (fConstructions.FieldworksList.HasField(aLoc) = ftNone)
            and not fConstructions.HousePlanList.HasPlan(aLoc)
            //and not fConstructions.BridgePlanList.HasPlan(aLoc)
            and LocHasNoAllyPlans(aLoc)
            and (IsComputer or not gTerrain.IsReservedForAI(aLoc));
end;


//This differs from above only in that it uses HasFakeField instead of HasField.
//We need it because the user expects to be blocked by fake field plans, but the gameplay should not.
//When the result effects the outcome of the game, the above function should be used instead.
function TKMHand.CanAddFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
begin
  Result := gTerrain.CanAddField(aLoc.X, aLoc.Y, aFieldType, ID)
            and (IsComputer or not gTerrain.IsReservedForAI(aLoc))
            and (fConstructions.FieldworksList.HasFakeField(aLoc) = ftNone)
            and not fConstructions.HousePlanList.HasPlan(aLoc)
            //and not fConstructions.BridgePlanList.HasPlan(aLoc)
            and LocHasNoAllyPlans(aLoc);
end;


// Same as for CanAddFakeFieldPlan, but check can we delete plan
function TKMHand.CanRemFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType): Boolean;
begin
  Result := (fConstructions.FieldworksList.HasFakeField(aLoc) = aFieldType)
            and LocHasNoAllyPlans(aLoc);
end;

function TKMHand.CanBuildHouse(aHouseType: TKMHouseType): Boolean;
begin
  if gGameParams.IsMapEditor then
    Exit(true);

  case aHouseType of
    htProductionThatch : Result := Self.Stats.GetHouseTotal(aHouseType) <= 0;
    else
      Result := true;
  end;
end;
function TKMHand.CanAddHousePlan(const aLoc: TKMPoint; aHouseType: TKMHouseType): Boolean;
var
  I, K, J, S, T, Tx, Ty: Integer;
  HA: TKMHouseArea;
begin
  Result := gTerrain.CanPlaceHouse(aLoc, aHouseType);
  Result := Result and CanBuildHouse(aHouseType);
  if not Result then Exit;

  HA := gRes.Houses[aHouseType].BuildArea;
  for I := 1 to 4 do
  for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Tx := aLoc.X + K - 3- gRes.Houses[aHouseType].EntranceOffsetX;
    Ty := aLoc.Y + I - 4 - gRes.Houses[aHouseType].EntranceOffsetY;
    //AI ignores FOW (this function is used from scripting)
    Result := Result and gTerrain.TileInMapCoords(Tx, Ty, 1)
                     and (IsComputer
                      or (NeedToChooseFirstStorehouseInGame and fFogOfWar.CheckTileInitialRevelation(Tx, Ty)) //Use initial revelation for first storehouse
                      or (not NeedToChooseFirstStorehouseInGame and (fFogOfWar.CheckTileRevelation(Tx, Ty) > 0)));
    //This checks below require Tx;Ty to be within the map so exit immediately if they are not
    if not Result then exit;

    if not IsComputer then
      Result := Result and not gTerrain.IsReservedForAI(KMPoint(Tx,Ty));
    if not Result then exit;
    //This tile must not contain fields/houses of allied players or self
    for J := 0 to gHands.Count - 1 do
      if fAlliances[J] = atAlly then
      begin
        Result := Result and (gHands[J].fConstructions.FieldworksList.HasField(KMPoint(Tx,Ty)) = ftNone);
        //Surrounding tiles must not be a house

        if not (aHouseType in WALL_HOUSES) then
        begin
        for S := -1 to 1 do
          for T := -1 to 1 do
          begin
            Result := Result
                      and not gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(Tx+S,Ty+T));
          end;

        end else
          Result := Result
                      and not gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(Tx,Ty));

        
      end;
  end;

end;

function TKMHand.CanAddStructurePlan(const aLoc: TKMPoint; aIndex, aRot: Word): Boolean;
begin
  Result := gTerrain.CanPlaceStructure(aLoc, aIndex, aRot);
end;

function TKMHand.HasVWares(aCost: TKMVWarePlanCommon): Boolean;
var I : Integer;
begin
  Result := true;
  for I := 0 to High(aCost) do
    with aCost[I] do
    if self.VirtualWare[W] < C then
      Exit(false);
end;

function TKMHand.CanPlaceDecoration(const aLoc: TKMPoint; aIndex: Word): Boolean;
begin
  Result := true;

  if not gTerrain.CheckPassability(aLoc, tpWalk) then
    Exit(false);
  case gDecorations[aIndex].DType of
    dtObject : Result := gTerrain.Land[aLoc.Y, aLoc.X].Obj <> gDecorations[aIndex].ID;
    dtTile : Result := gTerrain.Land[aLoc.Y, aLoc.X].BaseLayer.Terrain <> gDecorations[aIndex].ID;
    dtTileOverlay : Result := byte(gTerrain.Land[aLoc.Y, aLoc.X].TileOverlay2) <> gDecorations[aIndex].ID;
  end;
  Result := Result and (not gTerrain.IsReservedForAI(aLoc) or IsComputer);
  if not Result then
    Exit;
  Result := Result and HasVWares(gDecorations[aIndex].Cost);
end;

function TKMHand.CanAddHousePlanAI(aX, aY: Word; aHouseType: TKMHouseType; aCheckInfluence: Boolean): Boolean;
var
  I, K, J, S, T, Tx, Ty: Integer;
  HA: TKMHouseArea;
  enterOff: ShortInt;
  terOwner: TKMHandID;
begin
  Result := False;

  //Check if we can place house on terrain, this also makes sure the house is
  //at least 1 tile away from map border (skip that below)
  if not gTerrain.CanPlaceHouse(KMPoint(aX, aY), aHouseType) then
    Exit;

  //Perform additional cheks for AI
  HA := gRes.Houses[aHouseType].BuildArea;
  enterOff := gRes.Houses[aHouseType].EntranceOffsetX;
  for I := 1 to 4 do
  for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Tx := aX + K - 3 - enterOff;
    Ty := aY + I - 4 - gRes.Houses[aHouseType].EntranceOffsetY;

    //Make sure we don't block existing roads
    if gTerrain.CheckPassability(KMPoint(Tx, Ty), tpWalkRoad) then
      Exit;

    //Check with influence maps
    if aCheckInfluence and AI_GEN_INFLUENCE_MAPS then
    begin
      //Check if tile's blocked
      if (gAIFields.Influences.AvoidBuilding[Ty, Tx] > 0) then
        Exit;

      //Check ownership for entrance (good enough since it does not changes that fast)
      if (HA[I,K] = 2) then
      begin
        terOwner := gAIFields.Influences.GetBestOwner(Tx,Ty);
        if ((terOwner <> fID) and (terOwner <> HAND_NONE)) then
          Exit;
      end;
    end;

    //Avoid placing houses in choke-points _/house\_ by checking upper corners
    if not (aHouseType in [htGoldMine, htIronMine, htBitinMine]) then
      if (gTerrain.Land^[Ty-1, Tx - 1].Passability * [tpMakeRoads, tpWalkRoad] = [])
      or (gTerrain.Land^[Ty-1, Tx + 1].Passability * [tpMakeRoads, tpWalkRoad] = [])
      then
        Exit;

    //Make sure we can add road below house, full width + 1 on each side
    //Terrain already checked we are 1 tile away from map edge
    if (I = 4) and not (aHouseType in [htGoldMine, htIronMine, htBitinMine]) then
      if (gTerrain.Land^[Ty+1, Tx - 1].Passability * [tpMakeRoads, tpWalkRoad] = [])
      or (gTerrain.Land^[Ty+1, Tx    ].Passability * [tpMakeRoads, tpWalkRoad] = [])
      or (gTerrain.Land^[Ty+1, Tx + 1].Passability * [tpMakeRoads, tpWalkRoad] = [])
      then
        Exit;

    //This tile must not contain fields/houseplans of allied players or self
    for J := 0 to gHands.Count - 1 do
      if fAlliances[J] = atAlly then
      begin
        if (gHands[J].fConstructions.FieldworksList.HasField(KMPoint(Tx,Ty)) <> ftNone) then
          Exit;

        //Surrounding tiles must not be a house
        for S := -1 to 1 do
          for T := -1 to 1 do
            if gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(Tx+S,Ty+T)) then
              Exit;
      end;
  end;

  Result := True;
end;


//Due to lag there could be already plans placed by user in previous ticks
//Check if Plan can be placed once again, as we might have conflicting commands caused by lag
//This is called by GIP when a place field command is processed
procedure TKMHand.ToggleFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType; aMakeSound: Boolean; aRoadType : TKMRoadType);
var
  plan: TKMFieldType;
begin
  Assert(aFieldType in [ftRoad, ftCorn, ftGrassLand, ftWine, ftPalisade, ftRemove, ftVegeField], 'Placing wrong FieldType');

  plan := fConstructions.FieldworksList.HasField(aLoc);
  if aFieldType = plan then //Same plan - remove it
    RemFieldPlan(aLoc,aMakeSound)
  else
    if CanAddFieldPlan(aLoc, aFieldType) then
    begin
      if aMakeSound and not gGameParams.IsReplayOrSpectate
        and (ID = gMySpectator.HandID) then
        gSoundPlayer.Play(sfxPlacemarker);
      fConstructions.FieldworksList.AddField(aLoc, aFieldType, aRoadType);
      case aFieldType of
         ftVegeField,
         ftRemove,
         ftPalisade:;
         ftGrassLand:;
         ftRoad: gScriptEvents.ProcPlanRoadPlaced(fID, aLoc.X, aLoc.Y);
         ftCorn: gScriptEvents.ProcPlanFieldPlaced(fID, aLoc.X, aLoc.Y);
         ftWine: gScriptEvents.ProcPlanWinefieldPlaced(fID, aLoc.X, aLoc.Y);
      else
        raise Exception.Create('Unknown aFieldType');
      end;
      case aFieldType of
         ftVegeField: gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftVegetablesField);
         ftRemove: gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftRemove);
         ftPalisade:gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftPalisade);
         ftGrassLand:gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftGrassField);
         ftRoad:case aRoadType of
                  rtNone,
                  rtStone : gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftRoadStone);
                  rtWooden : gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftRoadWooden);
                  rtClay : gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftRoadClay);
                  rtExclusive : gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftRoadExclusive);
                end;
         ftCorn: gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftField);
         ftWine: gScriptEvents.ProcFieldPlanPlaced(fID, aLoc.X, aLoc.Y, lftWineField);
      end;
    end
    else
    begin
      if aMakeSound and not gGameParams.IsReplayOrSpectate
        and (ID = gMySpectator.HandID) then
        gSoundPlayer.Play(sfxCantPlace, 4);
      if plan = ftNone then //If we can't build because there's some other plan, that's ok
      begin
        //Can't build here anymore because something changed between click and command processing, so remove any fake plans
        fConstructions.FieldworksList.RemFakeField(aLoc);
        fConstructions.FieldworksList.RemFakeDeletedField(aLoc);
      end;
    end;
end;


//This procedure does not effect gameplay, it only changes fake field plans to make it look better for the user
//It is called when the user clicks to place a field plan
procedure TKMHand.ToggleFakeFieldPlan(const aLoc: TKMPoint; aFieldType: TKMFieldType; aRoadType : TKMRoadType);
var
  plan: TKMFieldType;
begin
  Assert(aFieldType in [ftRoad, ftCorn, ftWine, ftGrassland, ftVegeField, ftPalisade, ftRemove], 'Placing wrong fake FieldType');

  plan := fConstructions.FieldworksList.HasFakeField(aLoc);
  if aFieldType = plan then //Same plan - remove it
  begin
    fConstructions.FieldworksList.RemFakeField(aLoc); //Remove our fake marker which is shown to the user
    fConstructions.FieldworksList.AddFakeDeletedField(aLoc); //This will hide the real field until it is deleted from game
    if ID = gMySpectator.HandID then gSoundPlayer.Play(sfxClick);
  end
  else
    if CanAddFakeFieldPlan(aLoc, aFieldType) then
    begin
      fConstructions.FieldworksList.AddFakeField(aLoc, aFieldType, aRoadType);
      if ID = gMySpectator.HandID then
        gSoundPlayer.Play(sfxPlacemarker);
    end
    else
      if ID = gMySpectator.HandID then
        gSoundPlayer.Play(sfxCantPlace, 4);
end;


//Used mainly for testing purposes
{procedure TKMHand.AddRoadConnect(LocA,LocB: TKMPoint);
var
  NodeList: TKMPointList;
  RoadExists: Boolean;
  I: Integer;
begin
  NodeList := TKMPointList.Create;
  try
    RoadExists := fTerrain.PathFinding.Route_Make(LocA, LocB, CanMakeRoads, 0, nil, NodeList);
    if RoadExists then
      for I := 1 to NodeList.Count do
        AddField(NodeList.List[i], ftRoad);
  finally
    FreeAndNil(NodeList);
  end;
end;}


procedure TKMHand.AddJewerly(aObjectType: Word);
var K, count : integer;
  WareAdded : Boolean;
  ware : String;
begin
  Assert(length(gMapElements[aObjectType].VWares) > 0);

  WareAdded := false;
  count := 0;
  while not WareAdded do
  begin
    for K := 0 to High(gMapElements[aObjectType].VWares) do
      with gMapElements[aObjectType].VWares[K] do
        if KamRandom(101, 'TKMHand.AddJewerly') < Ch then
        begin
          ware := W;
          count := Cmin + KamRandom(CMax - CMin + 1, 'TKMHand.AddJewerly');
          WareAdded := true;
          Break;
        end;

  end;

  if (KamRandom(101, 'TKMHand.AddJewerly') < gMapElements[aObjectType].VWareChance) or (gMapElements[aObjectType].VWareChance = 0) then
    SetVirtualWareCnt(ware, count);


end;

procedure TKMHand.TakeJewerly;
begin
  SetVirtualWareCnt('vtCopper', -3);
  SetVirtualWareCnt('vtSilver', -2);
  SetVirtualWareCnt('vtAmber', -2);
  SetVirtualWareCnt('vtDiamond', -1);
end;

function TKMHand.CanMakeJewerly: Boolean;
begin
  Result := (VirtualWare['vtCopper'] >= 3) and
            (VirtualWare['vtSilver']  >= 2) and
            (VirtualWare['vtAmber'] >= 2) and
            (VirtualWare['vtDiamond'] >= 1);
end;

procedure TKMHand.ProceedStoreBell(aLoc: TKMPoint);
var I : Integer;
begin
  for I := 0 to fUnits.Count - 1 do
    if fUnits[I] <> nil then
      if not fUnits[I].IsDeadOrDying and fUnits[I].IsIdle then
      if fUnits[I].InHouse = nil then
      
      if fUnits[I].UnitType in UNITS_CITIZEN then
        if KMLength(fUnits[I].Position, aLoc) < 60 then
          fUnits[I].GoToStore(aLoc);
  gSoundPlayer.Play(sfxnStoreBell, aLoc, true, 1);
end;

function TKMHand.AddHouse(aHouseType: TKMHouseType; PosX, PosY: Word; RelativeEntrace: Boolean): TKMHouse;
begin
  Result := fHouses.AddHouse(aHouseType, PosX, PosY, fID, RelativeEntrace);
  Result.OnDestroyed := HouseDestroyed;
end;


//Add plan of a house, house is not created until after worker flattens the terrain
procedure TKMHand.AddHousePlan(aHouseType: TKMHouseType; const aLoc: TKMPoint);
var
  loc: TKMPoint;
begin
  loc.X := aLoc.X - gRes.Houses[aHouseType].EntranceOffsetX;
  loc.Y := aLoc.Y - gRes.Houses[aHouseType].EntranceOffsetY;

  fConstructions.HousePlanList.AddPlan(aHouseType, loc);
  fStats.HousePlanned(aHouseType);
  gScriptEvents.EventHousePlanPlaced(fID, loc.X, loc.Y, aHouseType);

  if (ID = gMySpectator.HandID) and not gGameParams.IsReplayOrSpectate then
    gSoundPlayer.Play(sfxPlacemarker);
end;

procedure TKMHand.AddStructurePlan(const aLoc: TKMPoint; aIndex, aRot: Word);
var
  loc: TKMPoint;
begin
  loc.X := aLoc.X + gRes.Structures[aIndex].Base[aRot].Offset.X;
  loc.Y := aLoc.Y + gRes.Structures[aIndex].Base[aRot].Offset.Y;

  fConstructions.StructureList.AddStructure(fStructures.AddStructure(aLoc, aIndex, aRot, ID));
  if (ID = gMySpectator.HandID) and not gGameParams.IsReplayOrSpectate then
    gSoundPlayer.Play(sfxPlacemarker);
end;

procedure TKMHand.PlaceDecoration(const aLoc: TKMPoint; aIndex: Word);
var I : integer;
begin
  with gDecorations[aIndex] do
    for I := 0 to High(Cost) do
      VirtualWareTake(Cost[I].W, Cost[I].C);
  gTerrain.PlaceDecoration(aLoc, aIndex);
  if (ID = gMySpectator.HandID) and not gGameParams.IsReplayOrSpectate then
    gSoundPlayer.Play(sfxPlacemarker);
end;

function TKMHand.HasHousePlan(const aLoc: TKMPoint): Boolean;
begin
  Result := fConstructions.HousePlanList.HasPlan(aLoc)
end;


function TKMHand.AddHouseWIP(aHouseType: TKMHouseType; const aLoc: TKMPoint): TKMHouse;
begin
  Result := fHouses.AddHouseWIP(aHouseType, aLoc.X, aLoc.Y, fID);
  Result.OnDestroyed := HouseDestroyed;

  fStats.HouseStarted(aHouseType);
end;


//Player wants to remove own house
procedure TKMHand.RemHouse(const Position: TKMPoint; DoSilent: Boolean; IsEditor: Boolean = False);
var
  H: TKMHouse;
begin
  //Sound is handled in Demolish
  H := fHouses.HitTest(Position.X, Position.Y);
  if H = nil then Exit; //Due to network delays the house might have already been destroyed by now

  H.Demolish(fID, IsEditor);
end;


procedure TKMHand.RemHousePlan(const Position: TKMPoint);
var
  hPlan: TKMHousePlan;
begin

  if not fConstructions.HousePlanList.TryGetPlan(Position, hPlan) then //Due to network delays house might not exist now
    Exit;

  fConstructions.HousePlanList.RemPlan(Position);
  fStats.HousePlanRemoved(hPlan.HouseType);
  gScriptEvents.EventHousePlanRemoved(fID, hPlan.Loc.X, hPlan.Loc.Y, hPlan.HouseType);
  if (ID = gMySpectator.HandID) and not gGameParams.IsReplayOrSpectate then
    gSoundPlayer.Play(sfxClick);
end;


//This is called by the GIP when an erase command is processed
procedure TKMHand.RemFieldPlan(const Position: TKMPoint; aMakeSound: Boolean);
var
  fieldType: TKMFieldType;
  roadType : TKMRoadType;
begin
  fieldType := fConstructions.FieldworksList.HasField(Position);
  roadType := fConstructions.FieldworksList.GetRoadType(Position);
  if fieldType = ftNone then Exit; //Can happen due to network delays
  fConstructions.FieldworksList.RemFieldPlan(Position);

  case fieldType of
    ftVegeField,
    ftRemove,
    ftGrassLand,
    ftPalisade:;
    ftRoad: gScriptEvents.ProcPlanRoadRemoved(fID, Position.X, Position.Y);
    ftCorn: gScriptEvents.ProcPlanFieldRemoved(fID, Position.X, Position.Y);
    ftWine: gScriptEvents.ProcPlanWinefieldRemoved(fID, Position.X, Position.Y);
  else
    raise Exception.Create('Unknown fieldType');
  end;
  case fieldType of
     ftVegeField: gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftVegetablesField);
     ftRemove: gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftRemove);
     ftPalisade:gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftPalisade);
     ftGrassLand:gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftGrassField);
     ftRoad:case roadType of
              rtNone:;
              rtStone : gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftRoadStone);
              rtWooden : gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftRoadWooden);
              rtClay : gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftRoadClay);
              rtExclusive : gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftRoadExclusive);
            end;
     ftCorn: gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftField);
     ftWine: gScriptEvents.ProcFieldPlanRemoved(fID, Position.X, Position.Y, lftWineField);
  end;

  if aMakeSound and not gGameParams.IsReplayOrSpectate
  and (ID = gMySpectator.HandID) then
    gSoundPlayer.Play(sfxClick);
end;


function TKMHand.RemGroup(const Position: TKMPoint): Boolean;
var
  group: TKMUnitGroup;
begin
  Assert(gGameParams.IsMapEditor);

  group := fUnitGroups.HitTest(Position.X, Position.Y);
  Result := group <> nil;
  if Result then
    fUnitGroups.RemGroup(group);
end;


//This is called immediately when the user clicks erase on a field plan.
//We know that an erase command is queued and will be processed in some ticks,
//so we AddFakeDeletedField which lets the user think the field was removed,
//while the game does not know the difference.
procedure TKMHand.RemFakeFieldPlan(const Position: TKMPoint);
begin
  fConstructions.FieldworksList.RemFakeField(Position); //Remove our fake marker which is shown to the user
  fConstructions.FieldworksList.AddFakeDeletedField(Position); //This will hide the real field until it is deleted from game
  if ID = gMySpectator.HandID then gSoundPlayer.Play(sfxClick);
end;


function TKMHand.FindHouse(aType: TKMHouseType; const aPosition: TKMPoint; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, aPosition.X, aPosition.Y, Index);
end;


// Very rough but fast way to find approximate city center
function TKMHand.FindCityCenter: TKMPoint;
const
  IMPORTANT_HOUSES: array[0..4] of TKMHouseType = (htStore, htInn, htSchool, htBarracks, htTownhall);
var
  I: Integer;
  H: TKMHouse;
begin
  for I := 0 to High(IMPORTANT_HOUSES) do
  begin
    H := FindHouse(IMPORTANT_HOUSES[I]);
    if (H <> nil) and not H.IsDestroyed then
      Exit(H.Entrance);
  end;

  // Very rough approach. We suggest there will be at least 1 of the important houses 99.99% of times
  // Find any house then
  for I := 0 to fHouses.Count - 1 do
  begin
    H := fHouses[I];
    if (H <> nil) and not H.IsDestroyed then
      Exit(H.Entrance);
  end;
end;


function TKMHand.FindHouse(aType: TKMHouseType; Index: Byte=1): TKMHouse;
begin
  Result := fHouses.FindHouse(aType, 0, 0, Index);
end;


function TKMHand.FindHousesInRadius(const aLoc: TKMPoint; aSqrRadius: Single; aTypes: TKMHouseTypeSet = HOUSES_VALID; aOnlyCompleted: Boolean = True): TKMHouseArray;
begin
  Result := fHouses.FindHousesInRadius(aLoc, aSqrRadius, aTypes, aOnlyCompleted);
end;


function TKMHand.FindInn(Loc: TKMPoint; aUnit: TKMUnit; UnitIsAtHome: Boolean = False): TKMHouseInn;
var
  H: TKMHouseInn;
  I: Integer;
  dist, bestMatch: Single;
begin
  //This function will return the best inn for a unit at Loc, base on distance, food available and space available.
  //Will return nil if no suitable inn is available
  Result := nil;
  I := 1;
  bestMatch := MaxSingle;
  if UnitIsAtHome then Inc(Loc.Y); //From outside the door of the house

  H := TKMHouseInn(FindHouse(htInn));
  repeat
    //First make sure that it is valid
    if (H <> nil) and H.HasFood and H.HasSpace
    and aUnit.CanWalkTo(Loc, H.PointBelowEntrance, tpWalk, 0) then
    begin
      //Take the closest inn out of the ones that are suitable
      dist := KMLengthSqr(H.Position, Loc);
      if dist < bestMatch then
      begin
        Result := H;
        bestMatch := dist;
      end;
    end;

    inc(I);
    H := TKMHouseInn(FindHouse(htInn, I));
  until(H = nil);
end;


//Does the player has any assets (without assets player is harmless)
function TKMHand.HasAssets: Boolean;
begin
  if Self = nil then Exit(False);
  
  Result := (Houses.Count > 0) or (Units.Count > 0) or (GetFieldsCount > 0)
            or NeedToChooseFirstStorehouse; //RMG - added ChooseLocation.Allowed option as a valid player
end;


// usually AIType is added by mission parser
procedure TKMHand.AddAIType(aHandAIType: TKMAIType);
begin
  Include(fCanBeAITypes, aHandAIType);
  AI.CanBeAITypes := fCanBeAITypes;
end;


procedure TKMHand.PostLoadMission;
var
  I: Integer;
begin
  if (Self = nil) or not Enabled then Exit;
  
  for I := 0 to fHouses.Count - 1 do
    fHouses[I].PostLoadMission;
end;


function TKMHand.HitTest(X, Y: Integer): TObject;
var
  H: TKMHouse;
  U: TKMUnit;
  G: TKMUnitGroup;
begin
  //Houses have priority over units, so you can't select an occupant
  //Selection priority is as follows:
  //BuiltHouses > UnitGroups > Units > IncompleteHouses

  H := HousesHitTest(X,Y);
  if (H <> nil) and (H.BuildingState in [hbsStone, hbsDone]) then
    Result := H
  else
  begin
    G := GroupsHitTest(X,Y);
    if (G <> nil) then
      Result := G
    else
    begin
      U := UnitsHitTest(X,Y);
      if (U <> nil) and (not U.IsDeadOrDying) then
        Result := U
      else
        Result := H; //Incomplete house or nil
    end;
  end;
end;


// Some entity was destroyed, but gMySpectator could have pointer to this entity, we have to nil it then
procedure TKMHand.EntityDestroyed(aEntity: TKMHandEntity);
begin
  //gMySpectator is nil during loading, when houses can be destroyed at the start
  if gMySpectator = nil then Exit;

  if gMySpectator.Selected = aEntity then
    gMySpectator.Selected := nil;
  if gMySpectator.LastSelected = aEntity then
    gMySpectator.NilLastSelected;
  if gMySpectator.Highlight = aEntity then
    gMySpectator.Highlight := nil;
  if gMySpectator.HighlightDebug.Entity = aEntity then
    gMySpectator.HighlightDebug.Reset;
  if gMySpectator.HighlightDebug2.Entity = aEntity then
    gMySpectator.HighlightDebug2.Reset;
  if gMySpectator.HighlightDebug3.Entity = aEntity then
    gMySpectator.HighlightDebug3.Reset;
  if gMySpectator.HighlightRoute.Entity = aEntity then
    gMySpectator.HighlightRoute.Reset;
end;


//Which house whas destroyed and by whom
procedure TKMHand.HouseDestroyed(aHouse: TKMHouse; aFrom: TKMHandID);
begin
  //Dispose of delivery tasks performed in DeliverQueue unit
  if aHouse.BuildingState in [hbsWood .. hbsDone] then
  begin
    Deliveries.Queue.RemAllOffers(aHouse);
    Deliveries.Queue.RemDemand(aHouse);
  end;

  //Only Done houses are treated as Self-Destruct, Lost, Destroyed
  if aHouse.BuildingState in [hbsNoGlyph .. hbsStone] then
    fStats.HouseEnded(aHouse.HouseType)
  else
  begin
    //We have to consider destroyed closed house as actually opened, otherwise closed houses stats will be corrupted
    if aHouse.GetWasClosedByHand and not gGameParams.IsMapEditor then
      fStats.HouseClosed(False, aHouse.HouseType);

    //Distribute honors
    if aFrom = fID then
      fStats.HouseSelfDestruct(aHouse.HouseType)
    else
    begin
      Stats.HouseLost(aHouse.HouseType);

      if aFrom <> HAND_NONE then
        gHands[aFrom].Stats.HouseDestroyed(aHouse.HouseType);
    end;
  end;

  //Scripting events happen AFTER updating statistics
  gScriptEvents.ProcHouseDestroyed(aHouse, aFrom);

  EntityDestroyed(aHouse);
end;


function TKMHand.HousesHitTest(X, Y: Integer): TKMHouse;
begin
  Result:= fHouses.HitTest(X, Y);
end;

function TKMHand.StructuresHitTest(X: Integer; Y: Integer): TKMStructure;
begin
  Result:= fStructures.HitTest(X, Y);
end;


function TKMHand.GroupsHitTest(X, Y: Integer): TKMUnitGroup;
begin
  Result:= fUnitGroups.HitTest(X, Y);
end;


function TKMHand.ObjectByUID(aUID: Integer): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Units.Count - 1 do
    if aUID = Units[I].UID then
    begin
      Result := Units[I];
      Exit;
    end;
end;


function TKMHand.GetColorIndex: Byte;
var
  I: Integer;
begin
  Result := 3; //3 = Black which can be the default when a non-palette 32 bit color value is used
  for I := 0 to 255 do
    if gRes.Palettes.DefaultPalette.Color32(I) = fFlagColor then
      Result := I;
end;


function TKMHand.GetDeliveries: TKMHandLogistics;
begin
  if Self = nil then Exit(nil);

  Result := fDeliveries;
end;


function TKMHand.CalcOwnerName: UnicodeString;
var
  numberedAIs: Boolean;
begin
  numberedAIs := not gGameParams.IsSingleplayer;
  //Default names
  if IsHuman then
    Result := gResTexts[TX_PLAYER_YOU]
  else
    if AI.Setup.NewAI then
    begin
      if numberedAIs then
        Result := Format(gResTexts[TX_ADVANCED_AI_PLAYER_SHORT_X], [fID + 1])
      else
        Result := gResTexts[TX_AI_PLAYER_ADVANCED_SHORT];
    end else begin
      if numberedAIs then
        Result := Format(gResTexts[TX_CLASSIC_AI_PLAYER_SHORT_X], [fID + 1])
      else
        Result := gResTexts[TX_AI_PLAYER_CLASSIC_SHORT];
    end;

  //Try to take player name from mission text if we are in SP
  //Do not use names in MP to avoid confusion of AI players with real player nicknames
  if gGameParams.Mode in [gmSingle, gmCampaign, gmMapEd, gmReplaySingle] then
    if gGame.TextMission.HasText(HANDS_NAMES_OFFSET + fID) then
      if IsHuman then
        Result := gResTexts[TX_PLAYER_YOU] + ' (' + gGame.TextMission[HANDS_NAMES_OFFSET + fID] + ')'
      else
        Result := gGame.TextMission[HANDS_NAMES_OFFSET + fID];

  //If this location is controlled by an MP player - show his nik
  if (fOwnerNickname <> '') and IsHuman then
    Result := UnicodeString(fOwnerNickname);
end;


function TKMHand.OwnerName(aNumberedAIs: Boolean = True; aLocalized: Boolean = True): UnicodeString;
begin
  Result := GetHandOwnerName(IsHuman, AI.Setup.NewAI, aNumberedAIs, aLocalized);
end;


function TKMHand.GetHandOwnerName(aIsHuman, aIsAdvAI: Boolean; aNumberedAIs: Boolean = True; aLocalized: Boolean = True): UnicodeString;

  function GetText(aId: Word; aLocalized: Boolean): UnicodeString; inline;
  begin
    if aLocalized then
      Result := gResTexts[aId]
    else
      Result := gResTexts.DefaultTexts[aId];
  end;

begin
  //If this location is controlled by an MP player - show his nik
  if (fOwnerNickname <> '')
    and aIsHuman then //we could ask AI to play on ex human loc, so fOwnerNickname will be still some human name
    Exit(UnicodeString(fOwnerNickname));

  //Try to take player name from mission text if we are in SP
  //Do not use names in MP to avoid confusion of AI players with real player nicknames
  if (gGameParams.Mode in [gmSingle, gmCampaign, gmMapEd, gmReplaySingle])
    and gGame.TextMission.HasText(HANDS_NAMES_OFFSET + fID) then
  begin
    if aIsHuman then
      Result := GetText(TX_PLAYER_YOU, aLocalized) + ' (' + gGame.TextMission[HANDS_NAMES_OFFSET + fID] + ')'
    else
      Result := gGame.TextMission[HANDS_NAMES_OFFSET + fID];

    Exit;
  end;

  //Default names
  if aIsHuman then
    Result := GetText(TX_PLAYER_YOU, aLocalized)
  else
    if aIsAdvAI then
    begin
      if aNumberedAIs then
        Result := Format(GetText(TX_ADVANCED_AI_PLAYER_SHORT_X, aLocalized), [fID + 1])
      else
        Result := GetText(TX_AI_PLAYER_ADVANCED_SHORT, aLocalized);
    end else begin
      if aNumberedAIs then
        Result := Format(GetText(TX_CLASSIC_AI_PLAYER_SHORT_X, aLocalized), [fID + 1])
      else
        Result := GetText(TX_AI_PLAYER_CLASSIC_SHORT, aLocalized);
    end;
end;


function TKMHand.GetOwnerName: UnicodeString;
begin
  Result := OwnerName(not gGameParams.IsSingleplayer);
end;


function TKMHand.GetOwnerNameColored: AnsiString;
begin
  Result := WrapColorA(AnsiString(GetOwnerName), FlagColorToTextColor(FlagColor));
end;


function TKMHand.GetOwnerNameColoredU: UnicodeString;
begin
  Result := WrapColor(GetOwnerName, FlagColorToTextColor(FlagColor));
end;


function TKMHand.GetAI: TKMHandAI;
begin
  if Self = nil then Exit(nil);

  Result := fAI;
end;


function TKMHand.GetAlliances(aIndex: Integer): TKMAllianceType;
begin
  Result := fAlliances[aIndex];
end;


procedure TKMHand.SetAlliances(aIndex: Integer; aValue: TKMAllianceType);
begin
  if Self = nil then Exit;

  fAlliances[aIndex] := aValue;
  gAIFields.Supervisor.UpdateAlliances();

  if Assigned(fOnAllianceChange) then
    fOnAllianceChange;
end;


procedure TKMHand.SetEnabled(const Value: Boolean);
begin
  fEnabled := Value;
end;


procedure TKMHand.SetFlagColor(const Value: Cardinal);
begin
  if Self = nil then Exit;
  
  fFlagColor := Value;
end;


function  TKMHand.GetShareFOW(aIndex: Integer): Boolean;
begin
  Result := fShareFOW[aIndex];
end;


procedure TKMHand.SetShareFOW(aIndex: Integer; aValue: Boolean);
begin
  if Self = nil then Exit;

  fShareFOW[aIndex] := aValue;
end;


function  TKMHand.GetShareBeacons(aIndex: Integer): Boolean;
begin
  Result := fShareBeacons[aIndex];
end;


procedure TKMHand.SetShareBeacons(aIndex: Integer; aValue: Boolean);
begin
  if Self = nil then Exit;

  fShareBeacons[aIndex] := aValue;
end;


{ See if player owns any Fields/Roads/Walls (has any assets on Terrain)
  If Player has none and no Units/Houses we can assume it's empty and does not needs to be saved
  Queried by MapEditor.SaveDAT;
  Might also be used to show Players strength (or builder/warrior balance) in Tavern }
function TKMHand.GetFieldsCount: Integer;
var
  I, K: Integer;
begin
  Result := 0;
    for I := 1 to gTerrain.MapY do
      for K := 1 to gTerrain.MapX do
        if gTerrain.Land^[I,K].TileOwner = fID then
          Inc(Result);
end;


function TKMHand.GetFlagTextColor: Cardinal;
begin
  Result := FlagColorToTextColor(fFlagColor);
end;


procedure TKMHand.GetFieldPlans(aList: TKMPointTagList; const aRect: TKMRect; aIncludeFake: Boolean);
var
  I: TKMHandID;
begin
  //Include self and allies
  for I := 0 to gHands.Count - 1 do
    if gHands[fID].Alliances[I] = atAlly then
      gHands[I].Constructions.FieldworksList.GetFields(aList, aRect, aIncludeFake);
end;


procedure TKMHand.GetHousePlans(aList: TKMPointDirList; const aRect: TKMRect);
var
  I: TKMHandID;
begin
  //Include self and allies
  for I := 0 to gHands.Count - 1 do
    if gHands[fID].Alliances[I] = atAlly then
      gHands[I].Constructions.HousePlanList.GetOutlines(aList, aRect);
end;

procedure TKMHand.GetPlansTablets(aList: TKMPointTagList; const aRect: TKMRect);
var
  I: TKMHandID;
begin
  //Include self and allies
  for I := 0 to gHands.Count - 1 do
    if gHands[fID].Alliances[I] = atAlly then
      gHands[I].Constructions.HousePlanList.GetTablets(aList, aRect);
end;


procedure TKMHand.GetHouseMarks(const aLoc: TKMPoint; aHouseType: TKMHouseType; aList: TKMPointTagList; aIgnoreFOW: Boolean = False; aIgnoreObjects: Boolean = false);
  //Replace existing icon with a Block
  procedure BlockPoint(const aPoint: TKMPoint; aID: Integer);
  var I: Integer;
  begin
    //Remove all existing marks on this tile (entrance can have 2 entries)
    for I := aList.Count - 1 downto 0 do
      if KMSamePoint(aList[I], aPoint) then
        aList.Remove(aPoint);

    aList.Add(aPoint, aID);
  end;

var
  I, K, J, S, T: Integer;
  P2: TKMPoint;
  allowBuild: Boolean;
  HA: TKMHouseArea;
begin
  //Get basic Marks
  gTerrain.GetHouseMarks(aLoc, aHouseType, aList, aIgnoreObjects);

  //Override marks if there are House/FieldPlans (only we know about our plans) and or FogOfWar
  HA := gRes.Houses[aHouseType].BuildArea;

  for I := 1 to 4 do
    for K := 1 to 4 do
      if (HA[I,K] <> 0)
        and gTerrain.TileInMapCoords(aLoc.X + K - 3 - gRes.Houses[aHouseType].EntranceOffsetX, aLoc.Y + I - 4 - gRes.Houses[aHouseType].EntranceOffsetY, 1) then
      begin
        //This can't be done earlier since values can be off-map
        P2 := KMPoint(aLoc.X + K - 3 - gRes.Houses[aHouseType].EntranceOffsetX, aLoc.Y + I - 4 - gRes.Houses[aHouseType].EntranceOffsetY);

        //Forbid planning on unrevealed areas and fieldplans
        allowBuild := aIgnoreFOW
                      or (NeedToChooseFirstStorehouseInGame and fFogOfWar.CheckTileInitialRevelation(P2.X, P2.Y)) //Use initial revelation for first storehouse
                      or (not NeedToChooseFirstStorehouseInGame and (fFogOfWar.CheckTileRevelation(P2.X, P2.Y) > 0));

        allowBuild := allowBuild and (IsComputer or not gTerrain.IsReservedForAI(P2));

        //This tile must not contain fields/houses of allied players or self
        if allowBuild then
          for J := 0 to gHands.Count - 1 do
            if (gHands[fID].Alliances[J] = atAlly)
              and ((gHands[J].fConstructions.FieldworksList.HasField(P2) <> ftNone)
                or gHands[J].fConstructions.HousePlanList.HasPlan(P2)) then
              allowBuild := False;

        //Check surrounding tiles in +/- 1 range for other houses pressence
        if not (aHouseType in WALL_HOUSES) then
          for S := -1 to 1 do
            for T := -1 to 1 do
              if (S <> 0) or (T <> 0) then //This is a surrounding tile, not the actual tile
                for J := 0 to gHands.Count - 1 do
                  if (gHands[fID].Alliances[J] = atAlly)
                    and (gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(P2.X + S, P2.Y + T)) ) then
                  begin
                    BlockPoint(KMPoint(P2.X + S, P2.Y + T), TC_BLOCK); //Block surrounding points
                    allowBuild := False;
                  end;


        //Mark the tile according to previous check results
        if not allowBuild then
          if HA[I,K] = 2 then
            BlockPoint(P2, TC_BLOCK_ENTRANCE)
          else
            if aHouseType in [htGoldMine, htIronMine, htBitinMine] then
              BlockPoint(P2, TC_BLOCK_MINE)
            else
              BlockPoint(P2, TC_BLOCK);
      end;
end;

procedure TKMHand.GetStructureMarks(const aLoc: TKMPoint; aIndex, aRot: Word; aList : TKMPointTagList; aIgnoreFOW: Boolean = false);

var  allowBuild : Boolean;
  procedure BlockPoint(const aPoint : TKMPoint; aTag : Cardinal);
  var I: Integer;
  begin
    //Remove all existing marks on this tile (entrance can have 2 entries)
    for I := aList.Count - 1 downto 0 do
      if KMSamePoint(aList[I], aPoint) then
        aList.Remove(aPoint);

    aList.Add(aPoint, aTag);
  end;

  function CheckForHousePlansAround(const aPoint : TKMPoint) : Boolean;
  var
    I, K, J: Integer;
  begin
    if not allowBuild then Exit(false);

    Result := true;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if (I <> 0) or (K <> 0) then //This is a surrounding tile, not the actual tile
          for J := 0 to gHands.Count - 1 do
            if (gHands[fID].Alliances[J] = atAlly)
              and gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(aPoint.X + I, aPoint.Y + K))  then
              Exit(false);

  end;

  function CheckForPlans(const aPoint : TKMPoint) : Boolean;
  var
    J: Integer;
  begin
    if not allowBuild then Exit(false);
    Result := true;
    for J := 0 to gHands.Count - 1 do
      if (gHands[fID].Alliances[J] = atAlly)
        and (gHands[J].fConstructions.HousePlanList.HasPlan(KMPoint(aPoint.X, aPoint.Y))
            or (gHands[J].fConstructions.FieldworksList.HasField(aPoint) <> ftNone) ) then
        Exit(false);

  end;

begin
  gTerrain.GetStructureMarks(aLoc, aIndex, aRot, aList);
end;

procedure TKMHand.TakeOverHouse(aHouse: TKMHouse);
var aWaresIn, aWaresOut : array[WARE_MIN..WARE_MAX] of Word;
  aType : TKMHouseType;
  aLoc : TKMPoint;
  aDamage, aStyle, aLevel : Word;
  aWariant : Integer;
  WT : TKMWareType;
  H : TKMHouse;
begin
  if aHouse.AppleTree.NotNil then
    if aHouse.AppleTree.ParentTree.NotNil then
    begin
      TakeOverAppleTree(aHouse.AppleTree.ParentTree);
      Exit;
    end else
    begin
      TakeOverAppleTree(aHouse.AppleTree);
      Exit;
    end;
  aLoc := aHouse.Entrance;
  aType := aHouse.HouseType;
  aDamage := aHouse.GetDamage;
  aStyle := aHouse.Style;
  aWariant := aHouse.PicWariant;
  aLevel := aHouse.CurrentLevel;
  for WT := WARE_MIN to WARE_MAX do
  begin
    aWaresIn[WT] := aHouse.CheckWareIn(WT);
    aWaresOut[WT] := aHouse.CheckWareOut(WT);
  end;

  aHouse.Demolish(ID, true);

  H := AddHouse(aType, aLoc.X, aLoc.Y, true);
  H.Style := aStyle;
  H.PicWariant := aWariant;
  H.CurrentLevel := aLevel;
  for WT := WARE_MIN to WARE_MAX do
  begin
    if aWaresIn[WT] > 0 then
      H.WareAddToIn(WT, aWaresIn[WT], true);
    if aWaresOut[WT] > 0 then
      H.WareAddToOut(WT, aWaresOut[WT]);
  end;
  H.AddDamage(aDamage, nil, false);
  H.TakeOver;


end;

procedure TKMHand.TakeOverAppleTree(aHouse: TKMHouse);
var aWaresIn, aWaresOut : array[WARE_MIN..WARE_MAX] of Word;
  appleData : array of record
    Loc : TKMPoint;
    FruitType : Integer;
    GrowPhase : Integer;
    Damage : Word;
  end;
  H, cH : TKMHouseAppleTree;
  WT : TKMWareType;

  procedure AddTree(appleTree : TKMHouseAppleTree);
  var I: Integer;
  begin
    I := length(appleData);
    SetLength(appleData, I + 1);
    with appleData[I] do
    begin
      Loc := appleTree.Entrance;
      FruitType := appleTree.FruitType;
      GrowPhase := appleTree.GrowPhase;
      Damage := appleTree.GetDamage;
    end;
  end;
var I, J : Integer;
begin
  cH := aHouse.AppleTree;
  AddTree(cH);
  for I := 0 to cH.ChildCount - 1 do
    AddTree(cH.ChildTree(I));

  for WT := WARE_MIN to WARE_MAX do
  begin
    aWaresIn[WT] := aHouse.CheckWareIn(WT);
    aWaresOut[WT] := aHouse.CheckWareOut(WT);
  end;

  aHouse.Demolish(ID, true);

  J := 0;
  for I := 0 to High(appleData) do
  begin
    H := AddHouse(htAppleTree, appleData[I].Loc.X, appleData[I].Loc.Y, true).AppleTree;
    if H = nil then
      Continue;
    H.SetDamage(appleData[I].Damage);
    H.FruitType := appleData[I].FruitType;
    H.GrowPhase := appleData[I].GrowPhase;

    if J = 0 then
    begin
      for WT := WARE_MIN to WARE_MAX do
      begin
        if aWaresIn[WT] > 0 then
          H.WareAddToIn(WT, aWaresIn[WT], true);
        if aWaresOut[WT] > 0 then
          H.WareAddToOut(WT, aWaresOut[WT]);
      end;
      H.TakeOver;
    end;
    Inc(J);
  end;


end;

function TKMHand.GetClosestHouse(aLoc : TKMPoint; aHouseTypeSet : TKMHouseTypeSet; aWareSet : TKMWareTypeSet = [wtAll]; aMaxDistance : Single = 999) : TKMHouse;
var I : Integer;
  lastDistance : Single;
  H : TKMHouse;
  HT : TKMHouseType;
  WT : TKMWareType;
  hasWares : Boolean;
begin
  Result := nil;
  lastDistance := 999999;

  for HT in aHouseTypeSet do
  begin
    I := 1;
    repeat
      H := FindHouse(HT, I);

      if (H <> nil)
      and (not H.IsDestroyed)
      and (KMLengthDiag(aLoc, H.Position) < lastDistance)
      and (KMLengthDiag(aLoc, H.Position) <= aMaxDistance) then
        if wtAll in aWareSet then
        begin
          Result := H;
          lastDistance := KMLengthDiag(aLoc, H.Position);
        end else
        begin
          hasWares := true;
          for WT in aWareSet do
            if not (H.CheckWareTotal(WT) > IfThen(WT = wtBoots, H.BootsReserved, 0)) then
            begin
              hasWares := false;
              Break;
            end;
          if hasWares then
          begin
            Result := H;
            lastDistance := KMLengthDiag(aLoc, H.Position);
          end;
          
        end;
      Inc(I);
    until (H = nil);
  end;

end;

function TKMHand.GetClosestStore(aLoc : TKMPoint; aWare: TKMWareType) : TKMHouse;
var I : Integer;
  lastDistance : Single;
  HS : TKMHouseStore;
begin
  Result := nil;
  lastDistance := 999999;

  for I := 0 to Houses.Stores.Count - 1 do
  begin
    HS := TKMHouseStore(Houses.Stores[I]);
    if (HS <> nil)
    and (HS.DeliveryMode = dmDelivery)
    and (not HS.NotAcceptFlag[aWare])
    and (not HS.IsDestroyed)
    and (KMLengthDiag(aLoc, HS.Position) < lastDistance) then
    begin
      Result := HS;
      lastDistance := KMLengthDiag(aLoc, HS.Position);
    end;
  end;

end;

function TKMHand.GetClosestBarracks(aLoc : TKMPoint; aWare: TKMWareType) : TKMHouse;
var I : Integer;
  lastDistance : Single;
  HB : TKMHouseBarracks;
begin
  Result := nil;
  lastDistance := 999999;

  for I := 0 to Houses.Barracks.Count - 1 do
  begin
    HB := TKMHouseBarracks(Houses.Barracks[I]);
    if (HB <> nil)
    and (not HB.IsDestroyed)
    and (HB.DeliveryMode = dmDelivery)
    and (not HB.WareAccepted(aWare))
    and (KMLengthDiag(aLoc, HB.Position) < lastDistance) then
    begin
      Result := HB;
      lastDistance := KMLengthDiag(aLoc, HB.Position);
    end;
  end;

end;

function TKMHand.GetWorklessCount : Integer;
var I: integer;
  H : TKMHouseCottage;
  HS : TKMHouseSchool;
begin

  Result := fWorklessCitizens;
  I := 0;
  H := TKMHouseCottage(FindHouse(htCottage, I + 1));
  while H <> nil do
  begin
    Inc(Result, H.Workless);
    Inc(I);
    H := TKMHouseCottage(FindHouse(htCottage, I + 1));
  end;

  I := 0;
  H := TKMHouseCottage(FindHouse(htHouse, I + 1));
  while H <> nil do
  begin
    Inc(Result, H.Workless);
    Inc(I);
    H := TKMHouseCottage(FindHouse(htHouse, I + 1));
  end;



  I := 0;
  HS := TKMHouseSchool(FindHouse(htSchool, I + 1));
  while HS <> nil do
  begin
    if HS.HideOneGold then
      Dec(Result);

    Inc(I);
    HS := TKMHouseSchool(FindHouse(htSchool, I + 1));
  end;

end;

function TKMHand.CanTrainSerfs: Integer;
begin

  Result := 10 + fStats.GetHouseQty(htCottage) * 10 + fStats.GetHouseQty(htHouse) * 25;

  Result := Result - fStats.GetUnitQty(utSerf) - fStats.GetUnitQty(utBuilder)- fStats.GetUnitTraining(utSerf) - fStats.GetUnitTraining(utBuilder);
end;

procedure TKMHand.TakeWorkless;
var I, J : integer;
  H : TKMHouseCottage;
  houseFound : Boolean;
begin
  H := nil;
  houseFound := false;
  J := 0;
  if Stats.GetHouseQty(htCottage) > 0 then
    repeat
      I := KamRandom(Stats.GetHouseQty(htCottage), 'TKMHand.TakeWorkless');
      H := TKMHouseCottage(FindHouse(htCottage, I + 1));
      if H.Workless > 0 then
        houseFound := true;

      Inc(J);
  until (J = 10) or houseFound;

  J := 0;
  if not houseFound then
    if Stats.GetHouseQty(htHouse) > 0 then
      repeat
        I := KamRandom(Stats.GetHouseQty(htHouse), 'TKMHand.TakeWorkless');
        H := TKMHouseCottage(FindHouse(htHouse, I + 1));

        if H.Workless > 0 then
          houseFound := true;

        Inc(J);
      until (J = 10) or houseFound;

  if houseFound then
  begin
    if H <> nil then
      if not H.IsDestroyed then
        H.TakeWorkless;
  end else
  begin
    if fWorklessCitizens > 0 then
      Dec(fWorklessCitizens);
  end;



end;

procedure TKMHand.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  SaveStream.PlaceMarker('Hand');
  SaveStream.Write(fEnabled);
  SaveStream.Write(InCinematic);
  if not Enabled then Exit;

  inherited;
  fAI.Save(SaveStream);
  fConstructions.Save(SaveStream);
  fDeliveries.Save(SaveStream);
  fFogOfWar.Save(SaveStream);
  fHouses.Save(SaveStream);
  fStructures.Save(SaveStream);
  fLocks.Save(SaveStream);
  fStats.Save(SaveStream);
  fUnitGroups.Save(SaveStream);
  fMessageLog.Save(SaveStream);

  SaveStream.Write(fID);
  SaveStream.WriteA(fOwnerNickname);
  SaveStream.Write(fHandType, SizeOf(fHandType));
  SaveStream.Write(fCanBeHuman, SizeOf(fCanBeHuman));
  SaveStream.Write(fCanBeAITypes, SizeOf(fCanBeAITypes));
  SaveStream.Write(fAlliances, SizeOf(fAlliances));
  SaveStream.Write(fShareFOW, SizeOf(fShareFOW));
  SaveStream.Write(fShareBeacons, SizeOf(fShareBeacons));
  SaveStream.Write(fCenterScreen);
  SaveStream.Write(fFlagColor);
  SaveStream.Write(fTeam);
  SaveStream.Write(SelectionHotkeys, SizeOf(SelectionHotkeys));
  SaveStream.Write(fChooseLocation, SizeOf(TKMChooseLoc));

  newCount := length(ShowMessage);
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(ShowMessage[I].ID);
    SaveStream.Write(ShowMessage[I].Time);
    SaveStream.Write(ShowMessage[I].Kind, SizeOf(ShowMessage[I].Kind));
    SaveStream.WriteANSI(ShowMessage[I].Text);
  end;
  SaveStream.Write(fWorklessCitizens);

  newCount := length(VWaresCount);
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    SaveStream.Write(VWaresCount[I]);

  SaveStream.Write(fUpdateHandEntities);
  SaveStream.Write(fNeverHungry);

  //message stack
  newCount := length(MessageStack);
  SaveStream.Write(newCount);
  SetLength(MessageStack, newCount);
  for I := 0 to newCount - 1 do
  begin
    SaveStream.WriteANSI(MessageStack[I].Text);
    SaveStream.Write(MessageStack[I].Kind, SizeOf(MessageStack[I].Kind));
    SaveStream.Write(MessageStack[I].Loc);
  end;

  // Overlay
  SaveStream.WriteA(fOverlayMarkup);
  SaveStream.Write(fOverlayTextSettings, SizeOf(fOverlayTextSettings));
  fOverlayParams.Save(SaveStream);


  with fCustomPanelData do
  begin

    SaveStream.Write(PanelLeft);
    SaveStream.Write(PanelTop);
    SaveStream.Write(PanelSizeX);
    SaveStream.Write(PanelSizeY);
    SaveStream.Write(ControlsCount);
    for I := 0 to ControlsCount - 1 do
    with ControlsData[I] do
    begin
      SaveStream.Write(ID);
      SaveStream.WriteA(Caption);
      SaveStream.WriteA(Hint);
      SaveStream.Write(Left);
      SaveStream.Write(Top);
      SaveStream.Write(Width);
      SaveStream.Write(Height);
      SaveStream.Write(TexID);
      SaveStream.Write(Tag);
      SaveStream.Write(Enabled);
      SaveStream.Write(Visible);
      SaveStream.Write(CType, SizeOf(CType));
      SaveStream.Write(ImageAlphaStep);
    end;
  end;

  //fBridgesBuilt.SaveToStream(SaveStream);
end;


procedure TKMHand.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
begin
  LoadStream.CheckMarker('Hand');
  LoadStream.Read(fEnabled);
  LoadStream.Read(InCinematic);
  if not Enabled then Exit;

  inherited;
  fAI.Load(LoadStream);
  fConstructions.Load(LoadStream);
  fDeliveries.Load(LoadStream);
  fFogOfWar.Load(LoadStream);
  fHouses.Load(LoadStream);
  fStructures.Load(LoadStream);
  fLocks.Load(LoadStream);
  fStats.Load(LoadStream);
  fUnitGroups.Load(LoadStream);
  fMessageLog.Load(LoadStream);

  LoadStream.Read(fID);
  LoadStream.ReadA(fOwnerNickname);
  LoadStream.Read(fHandType, SizeOf(fHandType));
  LoadStream.Read(fCanBeHuman, SizeOf(fCanBeHuman));
  LoadStream.Read(fCanBeAITypes, SizeOf(fCanBeAITypes));
  LoadStream.Read(fAlliances, SizeOf(fAlliances));
  LoadStream.Read(fShareFOW, SizeOf(fShareFOW));
  LoadStream.Read(fShareBeacons, SizeOf(fShareBeacons));
  LoadStream.Read(fCenterScreen);
  LoadStream.Read(fFlagColor);
  LoadStream.Read(fTeam);
  LoadStream.Read(SelectionHotkeys, SizeOf(SelectionHotkeys));
  LoadStream.Read(fChooseLocation, SizeOf(TKMChooseLoc));

  LoadStream.Read(newCount);
  SetLength(ShowMessage, newCount);
  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(ShowMessage[I].ID);
    LoadStream.Read(ShowMessage[I].Time);
    LoadStream.Read(ShowMessage[I].Kind, SizeOf(ShowMessage[I].Kind));
    LoadStream.ReadANSI(ShowMessage[I].Text);
  end;
  LoadStream.Read(fWorklessCitizens);

  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
    if I < length(VWaresCount) then
      LoadStream.Read(VWaresCount[I]);

  LoadStream.Read(fUpdateHandEntities);
  LoadStream.Read(fNeverHungry);

  //message stack
  LoadStream.Read(newCount);
  SetLength(MessageStack, newCount);
  for I := 0 to newCount - 1 do
  begin
    LoadStream.ReadANSI(MessageStack[I].Text);
    LoadStream.Read(MessageStack[I].Kind, SizeOf(MessageStack[I].Kind));
    LoadStream.Read(MessageStack[I].Loc);
  end;


  // Overlay
  LoadStream.ReadA(fOverlayMarkup);
  LoadStream.Read(fOverlayTextSettings, SizeOf(fOverlayTextSettings));
  fOverlayParams.Clear;
  fOverlayParams.Load(LoadStream);

  with fCustomPanelData do
  begin
    LoadStream.Read(PanelLeft);
    LoadStream.Read(PanelTop);
    LoadStream.Read(PanelSizeX);
    LoadStream.Read(PanelSizeY);
    LoadStream.Read(ControlsCount);
    SetLength(ControlsData, ControlsCount);
    for I := 0 to ControlsCount - 1 do
      with ControlsData[I] do
      begin
        LoadStream.Read(ID);
        LoadStream.ReadA(Caption);
        LoadStream.ReadA(Hint);
        LoadStream.Read(Left);
        LoadStream.Read(Top);
        LoadStream.Read(Width);
        LoadStream.Read(Height);
        LoadStream.Read(TexID);
        LoadStream.Read(Tag);
        LoadStream.Read(Enabled);
        LoadStream.Read(Visible);
        LoadStream.Read(CType, SizeOf(CType));
        LoadStream.Read(ImageAlphaStep);
      end;
  end;

  //fBridgesBuilt.LoadFromStream(LoadStream);

end;


procedure TKMHand.SyncLoad;
var
  I: Integer;
begin
  if not Enabled then Exit;

  inherited;

  //Assign event handler after load
  for I := 0 to fUnits.Count - 1 do
  begin
    fUnits[I].OnUnitDied := UnitDied;
    fUnits[I].OnUnitTrained := UnitTrained;
    if fUnits[I] is TKMUnitWarrior then
      TKMUnitWarrior(fUnits[I]).OnWarriorWalkOut := WarriorWalkedOut;
  end;

  fUnitGroups.SyncLoad;

  //Assign event handler after load
  for I := 0 to fUnitGroups.Count - 1 do
    fUnitGroups[I].OnGroupDied := GroupDied;

  fHouses.SyncLoad;
  //fStructures.SyncLoad;

  //Assign event handler after load
  for I := 0 to fHouses.Count - 1 do
    fHouses[I].OnDestroyed := HouseDestroyed;

  fDeliveries.SyncLoad;
  fConstructions.SyncLoad;
  fAI.SyncLoad;

  if fNeverHungry then
    NeverHungry := true;
end;


procedure TKMHand.IncAnimStep;
begin
  if not Enabled then Exit;

  fHouses.IncAnimStep;
end;


procedure TKMHand.UnitDied(aUnit: TKMUnit);
begin
  Stats.UnitLost(aUnit.UnitType);
  if aUnit.KilledBy <> HAND_NONE then
    gHands[aUnit.KilledBy].Stats.UnitKilled(aUnit.UnitType);

  //Demands: food for soldiers / stone or wood for workers
  Deliveries.Queue.RemDemand(aUnit);

  if aUnit is TKMUnitWarrior then
    AI.General.WarriorDied(TKMUnitWarrior(aUnit));

  //Call script event after updating statistics
  gScriptEvents.ProcUnitDied(aUnit, aUnit.KilledBy);

  EntityDestroyed(aUnit);
end;


procedure TKMHand.GroupDied(aGroup: TKMUnitGroup);
begin
  EntityDestroyed(aGroup);
end;


procedure TKMHand.UpdateState(aTick: Cardinal);
var I, J : Integer;
  WT : TKMWareType;
  H : TKMHouse;
begin
  if not Enabled then Exit;

  //Player has to place first storehouse at some point or will be defeated
	if NeedToChooseFirstStorehouse and (aTick > TIME_TO_SET_FIRST_STOREHOUSE) then
	  AI.Defeat;

  //Update Groups logic before Units
  if fUpdateHandEntities then
    fUnitGroups.UpdateState;

  if fUpdateHandEntities then
    inherited;

  if fUpdateHandEntities then
    fHouses.UpdateState(aTick);

  if (aTick mod (gHands.Count * 5)) = (fID * 5) then
  begin
    for WT := WARE_MIN to WARE_MAX do
      Stats.Wares[WT].ActualCnt := 0;

    for I := 0 to fHouses.Count - 1 do
    begin
      H := fHouses[I];
      if (H = nil) or H.IsDestroyed then
        Continue;

      if H.HouseType = htMarket then
      begin
        for WT in WARES_VALID do
          Inc(Stats.Wares[WT].ActualCnt, H.CheckWareOut(WT));
      end else
      {if H.HouseType = htTownHall then
      begin
        Inc(Stats.Wares[WT].ActualCnt, H.CheckWareIn(wtGold));
      end else}
      if H.HouseType = htStore then
      begin
        for WT in WARES_VALID do
          Inc(Stats.Wares[WT].ActualCnt, H.CheckWareIn(WT));
      end else
      if H.HouseType = htBarracks then
      begin
        for WT in WARES_WARFARE do
          Inc(Stats.Wares[WT].ActualCnt, H.CheckWareIn(WT));
      end else
      for J := 1 to 4 do
      begin
        if H.HouseType = htSmallStore then
          case H.WareInput[J] of
            wtNone : ;
            wtFood : ;
            wtAll : ;
            wtWarfare : ;
            else          Inc(Stats.Wares[H.WareInput[J]].ActualCnt, H.ResIn[J] );
          end;
        
        case H.WareOutput[J] of
          wtNone : ;
          wtFood : ;
          wtAll : ;
          wtWarfare : ;
          else          Inc(Stats.Wares[H.WareOutput[J]].ActualCnt, H.ResOut[J] );
        end;
      end;

    end;

  end;

  //Distribute AI updates among different Ticks to avoid slowdowns
  if (aTick mod gHands.Count) = fID then
  begin
    fConstructions.UpdateState;
    fDeliveries.UpdateState(aTick);
  end;

  if not gGameParams.IsMapEditor then //Do not show messages in map editor
  for I := 0 to High(ShowMessage) do
      if ShowMessage[I].GetText <> '' then
        if ShowMessage[I].Time = aTick then
          ShowMsg(ShowMessage[I].Kind, ShowMessage[I].GetText, KMPOINT_ZERO);


  if gMySpectator.SelectedHandID = fID then
    if aTick mod 10 = 0 then
      fFogOfWar.UpdateState(gGameParams.DynamicFOW); //We might optimize it for AI somehow, to make it work coarse and faster

  //AI update takes care of it's own interleaving, so run it every tick
  if fUpdateHandEntities then
    fAI.UpdateState(aTick, gHands.DoCheckGoals);

  //if (aTick + Byte(fPlayerIndex)) mod 20 = 0 then
    //fArmyEval.UpdateState;

  if CanDoStatsUpdate(aTick) then
    fStats.UpdateState;

  if not gGameParams.IsMapEditor //Do not place first storehouse in map editor etc
    and fChooseLocation.Allowed
    and not fChooseLocation.Placed then
    ChooseFirstStorehouse();

end;


function TKMHand.NeedToChooseFirstStorehouse: Boolean;
begin
  Result := fChooseLocation.Allowed and not fChooseLocation.Placed;
end;


function TKMHand.NeedToChooseFirstStorehouseInGame: Boolean;
begin
  Result := not gGameParams.IsMapEditor and NeedToChooseFirstStorehouse;
end;


procedure TKMHand.ChooseFirstStorehouse();
var
  I: Integer;
  entrance: TKMPoint;
begin
  if IsComputer then
    fChooseLocation.Placed := True
  // Check if storehouse has been placed
  else
  if (Stats.GetHouseTotal(htStore) > 0) then
  begin
    for I := 0 to fConstructions.HousePlanList.Count - 1 do
      with fConstructions.HousePlanList.Plans[I] do
        if (HouseType = htStore) then
        begin
          entrance := KMPointAdd( Loc, KMPoint(gRes.Houses[HouseType].EntranceOffsetX,gRes.Houses[HouseType].EntranceOffsetY) );
          RemHousePlan(entrance);
          if CanAddFieldPlan(KMPoint(entrance.X, entrance.Y+1), ftRoad) then
            AddFirstStorehouse(entrance);
        end;
  end;

  // Preselect storehouse
  if not gGameParams.IsReplayOrSpectate
  and (gMySpectator.HandID = ID)
  and not fChooseLocation.Placed then
  begin
    gCursor.Mode := cmHouses;
    gCursor.Tag1 := Byte(htStore);
  end;
end;


procedure TKMHand.AddFirstStorehouse(aEntrance: TKMPoint);
  // Place road and return True if it is possible
  function AddRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := CanAddFieldPlan(KMPoint(aPoint.X, aPoint.Y), ftRoad);
    if Result then
    begin
      gTerrain.SetRoad(aPoint, fID, rtStone);
      //Terrain under roads is flattened (fields are not)
      gTerrain.FlattenTerrain(aPoint);
      if gMapElements[gTerrain.Land^[aPoint.Y,aPoint.X].Obj].WineOrCorn then
        gTerrain.RemoveObject(aPoint);
    end;
  end;
var
  I: Integer;
  H: TKMHouse;
  WT: TKMWareType;
  UT: TKMUnitType;
begin
  // Add Storehouse
  H := AddHouse(htStore, aEntrance.X, aEntrance.Y, True);
  // Add Wares
  for WT := Low(fChooseLocation.Resources) to High(fChooseLocation.Resources) do
    if H.WareCanAddToIn(WT) OR H.WareCanAddToOut(WT) then
    begin
      Stats.WareInitial(WT, fChooseLocation.Resources[WT]);
      H.WareAddToEitherFromScript(WT, fChooseLocation.Resources[WT]);
    end;
  // Add Roads
  AddRoad( KMPoint(aEntrance.X,  aEntrance.Y+1) );
  AddRoad( KMPoint(aEntrance.X-1,aEntrance.Y+1) );
  AddRoad( KMPoint(aEntrance.X+1,aEntrance.Y+1) );
  // Add Units
  for UT := Low(fChooseLocation.Units) to High(fChooseLocation.Units) do
    for I := 0 to fChooseLocation.Units[UT] - 1 do
      AddUnit(UT, KMPoint(aEntrance.X,aEntrance.Y+1));
  // Finish action
  fChooseLocation.Placed := True;
  gCursor.Mode := cmNone; // Reset cursor
end;


function TKMHand.CanDoStatsUpdate(aTick: Cardinal): Boolean;
begin
  Result := (aTick mod GetStatsUpdatePeriod = 0) or (aTick = 1);
end;


function TKMHand.DoCheckGoals: Boolean;
begin
  Result := not fChooseLocation.Allowed
            or (fChooseLocation.Allowed and fChooseLocation.Placed);
end;

procedure TKMHand.ShowMSG(aKind : TKMMessageKind; aMessage: string; aLoc : TKMPoint);
var J : Integer;
begin
  if ID = gMySpectator.HandID then
    gGame.ShowMessageLocal(aKind,
                            gGame.TextMission.ParseTextMarkup(aMessage),
                          aLoc);

  J := length(MessageStack);
  SetLength(MessageStack, J + 1);
  MessageStack[J].Text := aMessage {gGame.TextMission.ParseTextMarkup(aMessage)};
  MessageStack[J].Loc := aLoc;
  MessageStack[J].Kind := aKind;
end;

procedure TKMHand.Paint(const aRect: TKMRect; aTickLag: Single);
begin
  if not Enabled then Exit;

  inherited;

  if mlUnits in gGameParams.VisibleLayers then
    fUnitGroups.Paint(aRect, aTickLag);

  if mlHouses in gGameParams.VisibleLayers then
  begin
    fHouses.Paint(aRect);
    fStructures.Paint(KMRectGrow(aRect, 1));
  end;

  if not SKIP_RENDER AND OVERLAY_DEFENCES AND not fAI.Setup.NewAI then
    if gMySpectator.SelectedHandID = fID then
      fAI.General.DefencePositions.Paint;

  if not SKIP_RENDER AND OVERLAY_DEFENCES AND not fAI.Setup.NewAI then
    if gMySpectator.SelectedHandID = fID then
    fAI.General.DefendPositions.Paint;

  if not SKIP_RENDER AND fAI.Setup.NewAI then
  begin
    if OVERLAY_AI_BUILD then
    begin
      fAI.CityManagement.Builder.Paint();
      fAI.CityManagement.Builder.Planner.Paint();
    end;
    if OVERLAY_AI_COMBAT then
      fAI.ArmyManagement.Paint();
  end;
end;


function TKMHand.ObjToString(aSeparator: String = ' '): String;
begin
  Result := Format('Enabled = %5s%sID = %d%sAI: [%s]%sOwner = %s%sHandType = %s',
                   [BoolToStr(Enabled, True), aSeparator,
                    fID, aSeparator,
                    AI.ObjToString, aSeparator,
                    OwnerName, aSeparator,
                    GetEnumName(TypeInfo(TKMHandType), Integer(fHandType))]);
end;

procedure TKMHand.DeleteFromMessageQueue(aIndex : Integer);
var I : Integer;
begin
  for I := aIndex to High(ShowMessage) - 1 do
    ShowMessage[I] := ShowMessage[I + 1];

  SetLength(ShowMessage, high(ShowMessage));
end;

procedure TKMHand.AddToMessageQueue(const aID : Integer = -1; const aTime : Integer = -1; const aKind : TKMMessageKind = mkText; const aText : String = '');
var I : Integer;
begin
  I := length(ShowMessage);
  SetLength(ShowMessage, I + 1);
  ShowMessage[I].ID := aID;
  ShowMessage[I].Time := aTime;
  ShowMessage[I].Kind := aKind;
  ShowMessage[I].Text := aText;

end;

function TKMHand.GetVWare(aName : String) : Integer;
var I : Integer;
begin
  Result := 0;
  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    if gRes.Wares.VirtualWares[I].Name = aName then
      Exit(VWaresCount[I]);
end;

procedure TKMHand.SetVirtualWareCnt(aName: string; aCnt: Integer; addTo: Boolean = True);
begin
  if addTo then
    VirtualWare[aName] := EnsureRange(VirtualWare[aName] + aCnt, 0, high(word))
  else
    VirtualWare[aName] := EnsureRange(aCnt, 0, high(word));
end;

procedure TKMHand.SetVWare(aName : String; aValue : Integer);
var I : Integer;
begin
  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    if gRes.Wares.VirtualWares[I].Name = aName then
    begin
      VWaresCount[I] := aValue;
      Exit;
    end;

end;

function TKMHand.GetVWareByID(aIndex : Integer) : Integer;
begin
  Assert(aIndex < length(VWaresCount));
  Result := VWaresCount[aIndex];
end;

procedure TKMHand.SetVWareByID(aIndex : Integer; aValue : Integer);
begin
  Assert(aIndex < length(VWaresCount));
  VWaresCount[aIndex] := aValue;
end;

function TKMHand.VirtualWareTake(aName: string; aCount: Integer = 1): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to High(VWaresCount) do
    if gRes.Wares.VirtualWares[I].Name = aName then
    begin
      if aCount > 0 then
        if VWaresCount[I] >= aCount then
        begin
          Dec(VWaresCount[I], aCount);
          Result := true;
        end;
      if aCount < 0 then
      begin
        //Inc(, -aCount);
        VWaresCount[I] := EnsureRange(VWaresCount[I] - aCount, 0, high(Word));
        Result := true;
      end;
      Break;
    end;
end;

function TKMHand.VirtualWareTake(aIndex : Integer; aCount: Integer = 1): Boolean;
var I : Integer;
begin
  I := aIndex;
  Result := false;
  if aCount > 0 then
    if VWaresCount[I] >= aCount then
    begin
      Dec(VWaresCount[I], aCount);
      Result := true;
    end;
  if aCount < 0 then
  begin
    //Inc(, -aCount);
    VWaresCount[I] := EnsureRange(VWaresCount[I] - aCount, 0, high(Word));
    Result := true;
  end;
end;

procedure TKMHand.SetNeverHungry(aValue: Boolean);
var I : Integer;
begin
  fNeverHungry := aValue;

  if gGameParams.IsMapEditor then
    Exit;

  for I := 0 to fUnits.Count - 1 do
    fUnits[I].NeverHungry := aValue;

  for I := 0 to fUnitGroups.Count - 1 do
    fUnitGroups[I].NeverHungry := aValue;

end;

procedure TKMHand.SetWorklessCitizens(aValue: Word);
begin
  fWorklessCitizens := Min(aValue, high(Word));
end;


function TKMHand.AddControl(aInfo : TKMControlInfo) : Integer;
var I : Integer;
begin
  with fCustomPanelData do
  begin
    I := ControlsCount;
    Assert(I < 100, 'Too many controls. Max 100 controls');

    Inc(ControlsCount);
    SetLength(ControlsData, I + 1);
    ControlsData[I] := aInfo;
    ControlsData[I].ID := I;
    Result := I;
  end;


end;

procedure TKMHand.ControlChange(aID: Integer; aInfo : TKMControlInfo);
begin
  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');

  with fCustomPanelData.ControlsData[aID] do
  begin
    if aInfo.Left > 0 then Left := aInfo.Left;
    if aInfo.Top > 0 then Top := aInfo.Top;
    if aInfo.Width > 0 then Width := aInfo.Width;
    if aInfo.Height > 0 then Height := aInfo.Height;
    if aInfo.TexID > 0 then TexID := aInfo.TexID;
    if aInfo.Tag > 0 then Tag := aInfo.Tag;
    if aInfo.Caption <> '' then Caption := aInfo.Caption;
    if aInfo.Hint <> '' then Caption := aInfo.Caption;
    if aInfo.ImageAlphaStep <> 0 then ImageAlphaStep := aInfo.ImageAlphaStep;

    Visible := aInfo.Visible;
    Enabled := aInfo.Enabled;

  end;

end;

procedure TKMHand.ControlSetVisibility(aID: Integer; aVisible : Boolean);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  with fCustomPanelData.ControlsData[aID] do
    Visible := aVisible;
end;

procedure TKMHand.ControlSetCaption(aID: Integer; aCaption : String);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].Caption := AnsiString(aCaption);
end;

procedure TKMHand.ControlSetHint(aID: Integer; aHint : String);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].Hint := AnsiString(aHint);
end;

procedure TKMHand.ControlSetTexID(aID: Integer; aTexID : Integer);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].TexID := aTexID;
end;

procedure TKMHand.ControlSetRect(aID: Integer; X, Y, Width, Height : Integer);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].Left := X;
  fCustomPanelData.ControlsData[aID].Top := Y;
  fCustomPanelData.ControlsData[aID].Width := Width;
  fCustomPanelData.ControlsData[aID].Height := Height;
end;

procedure TKMHand.ControlSetEnabled(aID: Integer; aEnabled : Boolean);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].Enabled := aEnabled;
end;

procedure TKMHand.ControlSetAlphaStep(aID: Integer; aStep : Single);
begin

  Assert(InRange(aID, 0, fCustomPanelData.ControlsCount - 1), 'wrong custom panel Control ID');
  fCustomPanelData.ControlsData[aID].ImageAlphaStep := aStep;
end;

procedure TKMHand.ResizePanel(X, Y, Width, Height : Integer);
begin
  with fCustomPanelData do
  begin
    PanelLeft := EnsureRange(X, -160, 500);
    PanelTop := EnsureRange(Y, -50, 500);

    PanelSizeX := EnsureRange(Width, 50, 750);
    PanelSizeY := EnsureRange(Height, 50, 750);
  end;
end;

procedure TKMHand.BuildBridge(aLoc: TKMPoint; aIndex: Word; aRot: Word);
begin
  //fBridgesBuilt.Add(KMStructureBasic(aLoc, aIndex, aRot));
  //gTerrain.SetBridgePlan(aLoc, aIndex, aRot, hbsDone);
  //gTerrain.PlaceBridge(aLoc, aIndex, aRot);
end;

{function TKMHand.HasBridgeBuiltAt(aLoc: TKMPoint): Boolean;
var I, J, aRot : Integer;
begin
  Result := false;
  for I := 0 to fBridgesBuilt.Count - 1 do
    with gRes.Structures[fBridgesBuilt[I].Index] do
    begin
      aRot := fBridgesBuilt[I].Rotation;
      for J := 0 to Count - 1 do
      if Points[aRot, J] > 0 then
      begin
        if KMSamePoint(aLoc, KMPointAdd(fBridgesBuilt[I].Loc, KMPoint(J mod Size[aRot].X, J div Size[aRot].X))) then
          Exit(true);
      end;
      
    end;
        
end;}
{TKMAnimalSpawner}
procedure TKMAnimalSpawner.IncludeAnimal(aType: TKMUnitType);
var I, id : Integer;
begin
  Assert(gRes.Units[aType].IsAnimal, 'TKMAnimalSpawner.IncludeAnimal: UnitType is not animal');
  id := -1;

  for I := 0 to High(AnimalTypes) do
    if AnimalTypes[I] = aType then
      id := I;

  if id >= 0 then //don't add the same type again
    Exit;
  SetLength(AnimalTypes, length(AnimalTypes) + 1);

  AnimalTypes[high(AnimalTypes)] := aType;
end;

procedure TKMAnimalSpawner.ExcludeAnimal(aType: TKMUnitType);
var I, id : Integer;
begin
  Assert(gRes.Units[aType].IsAnimal, 'TKMAnimalSpawner.ExcludeAnimal: UnitType is not animal');
  id := -1;
  for I := 0 to High(AnimalTypes) do
    if AnimalTypes[I] = aType then
      id := I;

  if id = -1 then //no type found
    Exit;

  for I := id to High(AnimalTypes) - 1 do
    AnimalTypes[I] := AnimalTypes[I + 1];

  SetLength(AnimalTypes, high(AnimalTypes));
end;

function TKMAnimalSpawner.HasType(aType: TKMUnitType): Boolean;
var I : integer;
begin
  Assert(gRes.Units[aType].IsAnimal, 'TKMAnimalSpawner.HasType: UnitType is not animal');
  Result := false;
  for I := 0 to High(AnimalTypes) do
    if AnimalTypes[I] = aType then
      Exit(true);
end;

procedure TKMAnimalSpawner.ClearAnimalTypes;
begin
  AnimalTypes := [];
end;

function TKMAnimalSpawner.GetType: TKMUnitType;
begin
  if length(AnimalTypes) = 0 then
    Exit(utNone);

  Result := AnimalTypes[KamRandom(length(AnimalTypes), 'TKMAnimalSpawner.GetType')];
end;
{ TKMHandAnimals }

constructor TKMHandAnimals.Create(aHandIndex: ShortInt);
begin
  Inherited;
  SetLength(fSpawners, 0);
end;

function TKMHandAnimals.GetFishInWaterBody(aWaterID: Byte; aFishermanPos: TKMPoint; FindHighestCount: Boolean = True; aRadius : Integer = 0): TKMUnitFish;
var
  I, highestGroupCount: Integer;
  U: TKMUnit;
begin
  Result := nil;
  if aWaterID = 0 then Exit; //Fish should always be in valid water
  highestGroupCount := 0;

  for I := 0 to fUnits.Count - 1 do
  begin
    U := fUnits[I]; //Store locally

    if (U <> nil)
    and (U.UnitType = utFish)
    and not U.IsDeadOrDying //Fish are killed when they are caught or become stuck
    and (gTerrain.Land^[U.Position.Y, U.Position.X].WalkConnect[wcFish] = aWaterID)
    and (TKMUnitFish(U).FishCount > highestGroupCount)
    and ((aRadius = 0) or (KMLengthSqr(aFishermanPos, U.Position) < Sqr(aRadius))) then
    begin
      Result := TKMUnitFish(U);
      //This is for time saving when we don't actually care which group is returned
      if not FindHighestCount then Exit;
      highestGroupCount := Result.FishCount;
    end;
  end;
end;

function TKMHandAnimals.GetSpawnersCount: Word;
begin
  Result := length(fSpawners)
end;


function TKMHandAnimals.GetSpawner(aIndex: Integer): PKMAnimalSpawner;
begin
  Assert(InRange(aIndex, 0, GetSpawnersCount - 1), 'No such spawner ID :' + IntToStr(aIndex));
  Result := @fSpawners[aIndex];
end;

function TKMHandAnimals.GetSpawnerAtLoc(aLoc: TKMPoint): PKMAnimalSpawner;
var I : Integer;
begin
  Result := nil;
  for I := 0 to High(fSpawners) do
    if fSpawners[I].Loc = aLoc then
      Result := GetSpawner(I);

end;

procedure TKMHandAnimals.AddSpawner(aLoc: TKMPoint; aRadius: Byte; aMaxCount, aSpawnPace : Integer; aAnimals: TKMUnitTypeArray);
begin
  if GetSpawnerAtLoc(aLoc) <> nil then //don't add spawner here if there is one alredy
    Exit;

  SetLength(fSpawners, length(fSpawners) + 1);

  with fSpawners[high(fSpawners)] do
  begin
    Loc := aLoc;
    Radius := aRadius;
    AnimalTypes := aAnimals;
    Pace := aSpawnPace;
    MaxCount := aMaxCount;
  end;
end;
procedure TKMHandAnimals.RemoveFromSpawner(aAnimalID: TKMUnit; aIndex: Integer);
var tmp: TKMAnimalSpawner;
begin
  tmp := fSpawners[aIndex];
  tmp.Animals.Remove(aAnimalID);
  fSpawners[aIndex] := tmp;
end;

procedure TKMHandAnimals.AddAnimalTypeToLastSpawner(aAnimal: TKMUnitType);
begin
  Spawners[high(fSpawners)].IncludeAnimal(aAnimal);
end;

procedure TKMHandAnimals.RemoveSpawner(aIndex: Integer);
var I : Integer;
begin
  for I := aIndex to High(fSpawners) - 1 do
    fSpawners[I] := fSpawners[I + 1];

  SetLength(fSpawners, high(fSpawners));
end;

function TKMHandAnimals.IsSpawnerInRadius(aLoc: TKMPoint; aRadius: Single): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to high(fSpawners) do
    if KMLengthDiag(aLoc, fSpawners[I].Loc) <= aRadius + fSpawners[I].Radius then
      Exit(true);
end;

procedure TKMHandAnimals.UpdateState(aTick: Cardinal);

  function GetRandomPos(aLoc : TKMPoint; aRadius : Integer; aPassability : TKMTerrainPassability) : TKMPoint;
  var P : TKMPoint;
    J : Integer;
  begin
    Result := KMPOINT_ZERO;
    J := 0;

    while (Result = KMPOINT_ZERO) and (J < 20) do//check max 5 times
    begin
      P.X := aLoc.X + KM_CommonUtils.KaMRandomI2(aRadius, 'TKMHandAnimals.UpdateState');
      P.Y := aLoc.Y + KM_CommonUtils.KaMRandomI2(aRadius, 'TKMHandAnimals.UpdateState');
      if KMLengthSqr(aLoc, P) <= sqr(aRadius) then
        If gTerrain.CheckPassability(P, aPassability) then
          if gTerrain.GetUnit(P) = nil then
            Result := P;
      Inc(J);
    end;

  end;

  procedure DeleteDeadAnimals(aId : Integer);
  var I : Integer;
    tmp : TKMAnimalSpawner;
  begin
    tmp := fSpawners[aID];
    for I := tmp.Animals.Count - 1 to 0 do
    begin
      If tmp.Animals[I].IsDeadOrDying then
        tmp.Animals.Remove(I);
    end;
    fSpawners[aID] := tmp;
  end;

var I : Integer;
  P : TKMPoint;
  U : TKMUnitAnimal;
  UT : TKMUnitType;
begin
  Inherited;
  //use spawners
  for I := 0 to High(fSpawners) do
    with fSpawners[I] do
        if aTick mod fSpawners[I].Pace = 0 then
        begin
          DeleteDeadAnimals(I);
          if Animals.Count >= MaxCount then
            Continue;
          UT := GetType;
          if UT = utNone then
            Continue;

          P := GetRandomPos(Loc, Radius, gRes.Units[UT].AllowedPassability);
          if P = KMPOINT_ZERO then //no spot detected, skip it
            Continue;
          U := TKMUnitAnimal(AddUnit(UT, P, false));
          if U = nil then
            Continue;
          U.SpawnerID := I;
          Animals.Add(TKMUnit(U));
        end;


end;

procedure TKMHandAnimals.Paint(const aRect: TKMRect; aTickLag: Single);
var I : Integer;
begin
  Inherited;

  if (gGameParams.IsMapEditor and (melSpawners in gGame.MapEditor.VisibleLayers))
      or (gGameParams.IsGame and (mlSpawners in gGameParams.VisibleLayers)) then
    for I := 0 to High(fSpawners) do
      with fSpawners[I] do
        if KMInRect(Loc, KMRectGrow(aRect, Radius div 2)) then
        begin

          gRenderPool.RenderSpriteOnTile(Loc, 914);
          gRenderAux.CircleOnTerrain(Loc.X-0.5, Loc.Y-0.5,
                                      1,
                                      icDeepGreen AND $55FFFFFF,
                                      icDeepGreen);
          gRenderAux.CircleOnTerrain(Loc.X-0.5, Loc.Y-0.5,
                                      Radius,
                                      icGreen AND $10FFFFFF,
                                      icGreen);
        end;



end;

procedure TKMHandAnimals.Save(SaveStream: TKMemoryStream);
var I, J, nC, newCount : Integer;
begin
  Inherited;
  newCount := length(fSpawners);

  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(fSpawners[I].Loc);
    SaveStream.Write(fSpawners[I].Radius);
    SaveStream.Write(fSpawners[I].Pace);
    SaveStream.Write(fSpawners[I].MaxCount);

    nC := length(fSpawners[I].AnimalTypes);
    SaveStream.Write(nC);

    for J := 0 to nc - 1 do
      SaveStream.Write(fSpawners[I].AnimalTypes[J], SizeOf(fSpawners[I].AnimalTypes[J]));

    fSpawners[I].Animals.SaveToStream(SaveStream)

  end;
end;

procedure TKMHandAnimals.Load(LoadStream: TKMemoryStream);
var I, J, nC, newCount : Integer;
begin
  Inherited;
  LoadStream.Read(newCount);
  SetLength(fSpawners, newCount);

  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(fSpawners[I].Loc);
    LoadStream.Read(fSpawners[I].Radius);
    LoadStream.Read(fSpawners[I].Pace);
    LoadStream.Read(fSpawners[I].MaxCount);

    LoadStream.Read(nC);
    Setlength(fSpawners[I].AnimalTypes, nC);
    for J := 0 to nc - 1 do
      LoadStream.Read(fSpawners[I].AnimalTypes[J], SizeOf(fSpawners[I].AnimalTypes[J]));

    fSpawners[I].Animals.LoadFromStream(LoadStream)

  end;
end;

procedure TKMHandAnimals.SyncLoad;
var I : Integer;
begin
  Inherited;

  for I := 0 to high(fSpawners) do
    fSpawners[I].Animals.SyncLoad;

end;
//-----------
function GetStatsUpdatePeriod: Integer;
begin
  Result := 1000;
  case gGameParams.MissionMode of
    mmBuilding:  Result := CHARTS_SAMPLING_FOR_ECONOMY;
    mmFighting:  Result := CHARTS_SAMPLING_FOR_TACTICS;
  end;
end;


end.
