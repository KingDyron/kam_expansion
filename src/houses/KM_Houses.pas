unit KM_Houses;
{$I KaM_Remake.inc}
interface
uses
  KM_ResHouses, KM_ResWares,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_HandEntity,
  KM_GameTypes, KM_ResTypes;

// Houses are ruled by units, hence they don't know about  TKMUnits

// Everything related to houses is here
type
  //* Delivery mode
  TKMDeliveryMode = (dmClosed, dmDelivery, dmTakeOut);

const
  DELIVERY_MODE_SPRITE: array [TKMDeliveryMode] of Word = (38, 37, 664);
  FOOD_TO_ROT = 6000;
type
  TKMHouse = class;
  TKMHouseEvent = procedure(aHouse: TKMHouse) of object;
  TKMHouseTrainedEvent = procedure(aHouse: TKMHouse; aIsTrained : Boolean) of object;
  TKMHouseFromEvent = procedure(aHouse: TKMHouse; aFrom: TKMHandID) of object;
  TKMHouseArray = array of TKMHouse;

  TKMHouseSketch = class;

  TKMHouseSketchType = (hstHousePlan, hstHouse);
  TKMHouseSketchTypeSet = set of TKMHouseSketchType;

  TKMHouseRottenFood = record
    WareType : TKMWareType;
    TimeToRotten : Cardinal;
    Count : Byte;
  end;

  TAnonHouseSketchBoolFn = function(aSketch: TKMHouseSketch; aBoolParam: Boolean): Boolean;
  TKMHouseAction = class
  private
    fHouse: TKMHouse;
    fHouseState: TKMHouseState;
    fSubAction: TKMHouseActionSet;
    procedure SetHouseState(aHouseState: TKMHouseState);
  public
    constructor Create(aHouse: TKMHouse; aHouseState: TKMHouseState);
    procedure SubActionWork(aActionSet: TKMHouseActionType);
    procedure SubActionAdd(aActionSet: TKMHouseActionSet);
    procedure SubActionRem(aActionSet: TKMHouseActionSet);
    property State: TKMHouseState read fHouseState write SetHouseState;
    property SubAction: TKMHouseActionSet read fSubAction;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


  TKMHouseSketch = class(TKMHandEntityPointer<TKMHouse>)
  private

    fType: TKMHouseType; //House type
    fEntrance: TKMPoint;
    fPointBelowEntrance: TKMPoint;
  protected
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
    procedure SetPosition(const aPosition: TKMPoint); virtual;
    procedure SetPointBelowEntrance(aValue : TKMPoint); virtual;
    constructor Create; overload;
    procedure UpdateEntrancePos; virtual;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID); overload;

    property HouseType: TKMHouseType read fType;

    property Position: TKMPoint read fPosition;
    property Entrance: TKMPoint read fEntrance;
    property PointBelowEntrance: TKMPoint read fPointBelowEntrance write SetPointBelowEntrance;

    function ObjToStringShort(const aSeparator: String = '|'): String; override;

    function IsEmpty: Boolean;
    function HasMoreEntrances : Boolean; virtual;
    function GetClosestEntrance(aLoc : TKMPoint) : TKMPointDir; virtual;
    function Entrances : TKMPointDirArray; virtual;
  end;

  // Editable Version of TKMHouseSketch
  // We do not want to allow edit TKMHouse fields, but need to do that for some sketches
  TKMHouseSketchEdit = class(TKMHouseSketch)
  private
    fEditable: Boolean;
  protected
    function GetInstance: TKMHouse; override;
    function GetIsSelectable: Boolean; override;
    function GetPositionForDisplayF: TKMPointF; override;
  public
    constructor Create;

    procedure Clear;
    procedure CopyTo(aHouseSketch: TKMHouseSketchEdit);

    procedure SetHouseUID(aUID: Integer);
    procedure SetHouseType(aHouseType: TKMHouseType);
    procedure SetPosition(const aPosition: TKMPoint); override;

    class var DummyHouseSketch: TKMHouseSketchEdit;
  end;

  TKMHouseBuildCost = record
    Wood,Stone,Tile : Byte;
  end;
  TKMHouse = class(TKMHouseSketch)
  private
    fWariant : Integer;
    fStyle, fSlotID : Byte;
    fBuildSupplyWood: Byte; //How much Wood was delivered to house building site
    fBuildSupplyStone: Byte; //How much Stone was delivered to house building site
    fBuildSupplyTile: Byte; //How much Tiles was delivered to house building site
    //Bilding progress has different supply step than 50(originaly), so it's better to have 2 different variables
    //to declare how much supplies were delivered
    fBuildTileDelivered: Byte; //How much Trunks was delivered to house building site
    fBuildWoodDelivered: Byte; //How much Trunks was delivered to house building site
    fBuildStoneDelivered: Byte; //How much Trunks was delivered to house building site

    fBuildReserve: Word; //Take one build supply resource into reserve and "build from it"
    fBuildingProgress: Word; //That is how many efforts were put into building (Wooding+Stoning)
    fDamage: Word; //Damaged inflicted to house

    fTick: Cardinal;
    fWorker: Pointer; // Worker, who is running this house
    fIsClosedForWorker: Boolean; // house is closed for worker. If worker is already occupied it, then leave house
    fNotAcceptWorkers: TPointerArray;

    fBuildingRepair: Boolean; //If on and the building is damaged then labourers will come and repair it
    fStoreFilled : Boolean;
    fText : String;
    //Switch between delivery modes: delivery on/off/or make an offer from resources available
    fDeliveryMode: TKMDeliveryMode; // REAL delivery mode - using in game interactions and actual deliveries
    fNewDeliveryMode: TKMDeliveryMode; // Fake, NEW delivery mode, used just for UI. After few tick it will be set as REAL, if there will be no other clicks from player
    // Delivery mode set with small delay (couple of ticks), to avoid occasional clicks on delivery mode button
    fUpdateDeliveryModeOnTick: Cardinal; // Tick, on which we have to update real delivery mode with its NEW value

    fWareIn: array[1..WARES_IN_OUT_COUNT] of Byte; // Ware count in input
    fWareBlocked: array[1..WARES_IN_OUT_COUNT] of Word; // Ware count in input
    fWareInput : array[1..WARES_IN_OUT_COUNT] of TKMWareType;
    fWareOutput : array[1..WARES_IN_OUT_COUNT] of TKMWareType;

    // Count of the resources we have ordered for the input (used for ware distribution)
    //
    // We have to keep track of how many deliveries are going on now
    // But when demand is deleted it should be considered as well.
    // It could be deleted or not (if serf is entering demanded house)
    // F.e. when serf is walkin he can't close demand immidiately, he will close demand when he reach the next tile
    // Demand is marked as Deleted and fWareDemandsClosing is increased by 1
    // Then we will need to reduce fWareDeliveryCount and fWareDemandsClosing when demand will notify this house on its close
    // If serf is entering the house then we will not need to reduce fWareDeliveryCount, since ware is already delivered
    fWareDeliveryCount: array[1..WARES_IN_OUT_COUNT] of Word; // = fWareIn + Demands count (including closing demands)
    fWareDemandsClosing: array[1..WARES_IN_OUT_COUNT] of Word; // Number of closing demands at the moment

    fWareOut: array [1..WARES_IN_OUT_COUNT] of Byte; //Resource count in output
    fWareOrder: array [1..WARES_IN_OUT_COUNT] of Word; //If HousePlaceOrders=True then here are production orders
    fWareOutPool: array[0..19] of Byte;
    fFoodToRot : TKMArray<TKMHouseRottenFood>;
    fLastOrderProduced: Byte;
//    fWareOrderDesired: array [1..4] of Single;
    fResetDemands : Boolean;

    fIsOnTerrain: TKMTerrPicType;
    fSnowStep: Single;

    fIsDestroyed: Boolean;
    fIsBeingDemolished: Boolean; //To prevent script calling HouseDestroy on same house within OnHouseDestroyed action.
                                 //Not saved because it is set and used within the same tick only.
    fTimeSinceUnoccupiedReminder: Integer;
    fDisableUnoccupiedMessage: Boolean;
    fResourceDepletedMsgIssued: Boolean;
    fOrderCompletedMsgIssued: Boolean;
    fNeedIssueOrderCompletedMsg: Boolean;
    fPlacedOverRoad: Boolean; //Is house entrance placed over road

    fOnShowGameMessage: TKMGameShowMessageEvent;

    fLevel : record
      CurrentLevel : Byte;
      Progress : Word;
      BuildingProgress : Word;
      IsUpgrading : Boolean;
      UpgradingTime : Cardinal;
    end;

    fForceWorking : Boolean;
    fDontNeedRes : Boolean;
    fIsBurning : Byte;
    fWasTookOver : Boolean;
    fBuildCost : TKMHouseBuildCost;
    procedure CheckOnTerrain;
    function GetWareInArray: TKMByteArray;
    function GetWareOutArray: TKMByteArray;
    function GetWareOutPoolArray: TKMByteArray;

    procedure SetIsClosedForWorker(aIsClosed: Boolean);
    function GetClosedForWorker : Boolean; virtual;
    procedure UpdateDeliveryMode;
    function GetHasWorker: Boolean;
    procedure ShowMsg(aTextID: Integer);
    Function GetWareInput : TKMWareType8;
    Function GetWareOutput : TKMWareType8;
    function GetWorkMultiplier : Single;
    function CheckMaxLevel : Boolean;
    function GetHouseSpec : TKMHouseSpec;
    procedure SetStyle(aValue : Byte);
    function GetProductionCycle(aIndex : Byte) : Word;
    function PaintHouseWork : Boolean; virtual;
    procedure SetWariant(aValue : Integer);
  protected
    fWorkers: TPointerArray;
    fHouseToDeliver : TKMHouse;
    fDeliveryFromHouses: TPointerArray;
    fProductionCycles: array [1..WARES_IN_OUT_COUNT] of Word; //If HousePlaceOrders=True then here are production orders
    fBuildState: TKMHouseBuildState; // = (hbsGlyph, hbsNoGlyph, hbsWood, hbsStone, hbsDone);
    FlagAnimStep: Cardinal; //Used for Flags and Burning animation
    const
      COINS_GET_PACE = 6000;// 10 minutes to get coins
    //WorkAnimStep: Cardinal; //Used for Work and etc.. which is not in sync with Flags
    procedure SetCost;
    procedure Activate(aWasBuilt: Boolean); virtual;
    procedure AfterCreate(aWasBuilt: Boolean); virtual;
    procedure AddDemandsOnActivate(aWasBuilt: Boolean); virtual;
    function GetWareOrder(aId: Byte): Integer; virtual;
    function GetWareIn(aI: Byte): Word; virtual;
    function GetWareOut(aI: Byte): Word; virtual;
    function GetWareDistribution(aID: Byte): Byte;virtual; //Will use GetRatio from mission settings to find distribution amount
    function GetWareInLocked(aI: Byte): Word; virtual;
    procedure SetWareInManageTakeOutDeliveryMode(aWare: TKMWareType; aCntChange: Integer);
    procedure SetWareIn(aI: Byte; aValue: Word); virtual;
    procedure SetWareOut(aI: Byte; aValue: Word); virtual;
    procedure SetBuildingRepair(aValue: Boolean);
    procedure SetWareOrder(aId: Byte; aValue: Integer); virtual;
    procedure SetNewDeliveryMode(aValue: TKMDeliveryMode); virtual;
    procedure CheckTakeOutDeliveryMode; virtual;
    function GetDeliveryModeForCheck(aImmidiate: Boolean): TKMDeliveryMode;
    function ShowUnoccupiedMSG : Boolean; virtual;
    function SlotHasAnyWare(aWares : TKMWareTypeSet) : Boolean;

    procedure SetWareDeliveryCount(aIndex: Integer; aCount: Word);
    function GetWareDeliveryCount(aIndex: Integer): Word;

    procedure SetWareDemandsClosing(aIndex: Integer; aCount: Word);
    function GetWareDemandsClosing(aIndex: Integer): Word;

    property WareDeliveryCnt[aIndex: Integer]: Word read GetWareDeliveryCount write SetWareDeliveryCount;
    property WareDemandsClosing[aIndex: Integer]: Word read GetWareDemandsClosing write SetWareDemandsClosing;

    function GetInstance: TKMHouse; override;
    function GetPositionForDisplayF: TKMPointF; override;
    function GetPositionF: TKMPointF; inline;

    function GetIsSelectable: Boolean; override;
    function GetFlagColor : Cardinal;

    function TryDecWareDelivery(aWare: TKMWareType; aDeleteCanceled: Boolean): Boolean; virtual;

    procedure MakeSound; virtual; //Swine/stables make extra sounds
    property Tick : Cardinal read fTick;
    property SnowStep : Single read fSnowStep write fSnowStep;
    property OnTerrain : TKMTerrPicType read fIsOnTerrain write fIsOnTerrain;
    property ResetDemands : Boolean read fResetDemands write fResetDemands;
    procedure FinishedLevel(aLevel : Byte); virtual;
    procedure SetLevel(aValue : Byte); virtual;
    procedure SetHouseToDeliver(aValue : TKMHouse);


  public
    //fWareIn, fWareBlocked: array[1..WARES_IN_OUT_COUNT] of Byte; // Ware count in input
    CurrentAction: TKMHouseAction; //Current action, withing HouseTask or idle
    WorkAnimStep: Cardinal; //Used for Work and etc.. which is not in sync with Flags
    WorkAnimStepPrev: Cardinal; //Used for interpolated render, not saved
    DoorwayUse: Byte; //number of units using our door way. Used for sliding.
    OnDestroyed: TKMHouseFromEvent;
    TransferWare: array[1..WARES_IN_OUT_COUNT] of Boolean;
    TotalWorkingTime,
    WorkingTime : Integer;
    WareSlotChangedByAI : Boolean;
    BootsReserved : Word;
    LastCellID : Byte;
    Indestructible : Boolean;
    FlagColor : Cardinal;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; virtual;
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    property OnShowGameMessage: TKMGameShowMessageEvent read fOnShowGameMessage write fOnShowGameMessage;

    procedure Remove;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); virtual;
    property BuildingProgress: Word read fBuildingProgress;

    procedure UpdatePosition(const aPos: TKMPoint); virtual;//Used only by map editor
    procedure OwnerUpdate(aOwner: TKMHandID; aMoveToNewOwner: Boolean = False);
    procedure OwnerUpdateByScript(aOwner: TKMHandID);//Use only by scripts

    function GetWoodPic : Integer;
    function GetStonePic : Integer; virtual;
    function GetSnowPic : Integer; virtual;

    function GetClosestCell(const aPos: TKMPoint): TKMPoint;
    function GetDistance(const aPos: TKMPoint): Single;
    function InReach(const aPos: TKMPoint; aDistance: Single): Boolean;
    procedure GetListOfCellsAround(aCells: TKMPointDirList; aPassability: TKMTerrainPassability); virtual;
    procedure GetListOfCellsWithin(aCells: TKMPointList); virtual;
    procedure GetListOfGroundVisibleCells(aCells: TKMPointTagList);
    function GetRandomCellWithin: TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property BuildingRepair: Boolean read fBuildingRepair write SetBuildingRepair;
    property PlacedOverRoad: Boolean read fPlacedOverRoad write fPlacedOverRoad;

    property PositionF: TKMPointF read GetPositionF;
    property PicWariant : Integer read fWariant write SetWariant;
    property DeliveryMode: TKMDeliveryMode read fDeliveryMode;
    property StoreFilled: Boolean read fStoreFilled write fStoreFilled;
    property Text : String read fText write fText;
    property Style : Byte read fStyle Write SetStyle;
    property WareInputSlot : Byte read fSlotID;
    property NewDeliveryMode: TKMDeliveryMode read fNewDeliveryMode write SetNewDeliveryMode;
    procedure SetNextDeliveryMode;
    procedure SetPrevDeliveryMode;
    procedure SetDeliveryModeInstantly(aValue: TKMDeliveryMode);
    function AllowDeliveryModeChange: Boolean;
    procedure IssueResourceDepletedMsg;
    function GetResourceDepletedMessageId: SmallInt;
    function GetStyleGuiIcon : Word;

    property ResourceDepleted: Boolean read fResourceDepletedMsgIssued write fResourceDepletedMsgIssued;
    property OrderCompletedMsgIssued: Boolean read fOrderCompletedMsgIssued;
    property WareInput : TKMWareType8 Read GetWareInput;
    property WareOutput : TKMWareType8 Read GetWareOutput;
    function HasOutput : Boolean;
    property WorkMultiplier : Single read GetWorkMultiplier;
    function AcceptsWares : Boolean;
    function ProducesWares : Boolean;
    function HasSpaceForWareOut(aWare : TKMWareType) : Boolean;
    function HasSpaceForWaresOut(aWares : TKMWareTypeSet; aAnyWare : Boolean) : Boolean;

    procedure CheckWorkersForSlot; virtual;
    procedure MakeWareSlot(aWaresIn, aWaresOut : array of TKMWareType);
    procedure SetWareSlot(aSlotID : Byte; aForceChange : Boolean = false);overload;
    procedure SetWareSlot(aWare : TKMWareType);overload;
    function HasSlotWithWare(aWare : TKMWareType) : Boolean;
    function CanChangeWareInput(IsAI : Boolean = false) : Boolean;
    function GetAcceptWareIn(aWareType : TKMWareType) : Word;
    procedure SetAcceptWareIn(aWareType : TKMWareType; aMax : Word);
    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; virtual;
    function ShouldAbandonDeliveryFrom(aWareType: TKMWareType; aImmidiateCheck: Boolean = False): Boolean; virtual;
    function ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean; virtual;

    property Worker: Pointer read fWorker;
    procedure SetWorker(aWorker: Pointer); //Explicitly use SetWorker, to make it clear its not only pointer assignment
    property HasWorker: Boolean read GetHasWorker; //There's a citizen who runs this house
    //function CanHasWorker: Boolean;//deprecated
    property IsClosedForWorker: Boolean read GetClosedForWorker write SetIsClosedForWorker;
    function GetWasClosedByHand : Boolean;
    property DisableUnoccupiedMessage: Boolean read fDisableUnoccupiedMessage write fDisableUnoccupiedMessage;
    function HasWorkerInside : Boolean;
    function WorkersCount : Word;overload;
    function WorkersCount(aType : TKMUnitType) : byte;overload;
    function CurrentWorkersTypeCount : TKMUnitPlan;
    function MaxWorkers : Byte;overload;
    function MaxWorkers(aType : TKMUnitType) : Byte;overload;
    function CanHasWorker(aType : TKMUnitType) : Boolean; virtual;
    procedure AssignWorker(aWorker: Pointer);
    procedure RemoveWorker(aWorker: Pointer);
    function AcceptsWorker(aWorker: Pointer) : Boolean; virtual;
    procedure DoNotAcceptWorker(aIndex: Integer);
    function GetWorkersIcons : TKMWordArray;
    function GetWorker(aIndex: Integer) : Pointer;


    function GetHealth: Word;
    function GetBuildWoodDelivered: Byte;
    function GetBuildStoneDelivered: Byte;
    function GetBuildTileDelivered: Byte;
    property TileDelivered: Byte read fBuildTileDelivered;
    function GetBuildResourceDelivered: Byte;
    function GetBuildResDeliveredPercent: Single;

    property WareInArray: TKMByteArray read GetWareInArray;
    property WareOutArray: TKMByteArray read GetWareOutArray;
    property WareOutPoolArray: TKMByteArray read GetWareOutPoolArray;

    property BuildingState: TKMHouseBuildState read fBuildState write fBuildState;
    property BuildSupplyWood: Byte read fBuildSupplyWood;
    property BuildSupplyStone: Byte read fBuildSupplyStone;
    property BuildSupplyTile: Byte read fBuildSupplyTile;
    procedure IncBuildingProgress; overload;
    procedure IncBuildingProgress(aStep : Integer); overload;
    procedure IncBuildingUpgradeProgress;


    property IsMaxLevel : Boolean read CheckMaxLevel;
    function MaxHealth: Word;
    procedure AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False; aFromParent : Boolean = false); virtual;
    procedure AddRepair(aAmount: Word = 5);
    procedure UpdateDamage;

    function WoodCost : Byte;
    function StoneCost : Byte;
    function TileCost : Byte;
    function TotalCost : Byte;
    function HealthSupplyStep : Byte;
    procedure AddDemandBuildingMaterials;virtual;
    procedure SetOnFire;
    procedure TakeOver;
    function GetStats(aWares : Boolean) : TKMHouseStats; virtual;
    procedure SetStats(aStats : TKMHouseStats);
    function GetWaresArrayIn : TIntegerArray;
    function GetWaresArrayOut : TIntegerArray;
    function GetWaresArrayTotal : TIntegerArray;
    property HouseToDeliver : TKMHouse read fHouseToDeliver write SetHouseToDeliver;
    procedure AddHouseDeliveryFrom(aHouse : TKMHouse);
    procedure RemHouseDeliveryFrom(aHouse : TKMHouse);

    procedure SetBuildingProgress(aProgress, aWood, aStone, aTile : Word);

    function IsStone: Boolean;
    function IsComplete: Boolean; inline;
    function IsDamaged: Boolean;
    property IsDestroyed: Boolean read fIsDestroyed;
    property GetDamage: Word read fDamage;
    procedure SetDamage(aValue : Word);

    procedure SetState(aState: TKMHouseState; doTowerAnim : Boolean = true);
    function GetState: TKMHouseState;

    procedure HouseDemandWasClosed(aWare: TKMWareType; aDeleteCanceled: Boolean);
    function WareIsInAndOut(aWare: TKMWareType): Boolean;
    function CheckWareIn(aWare: TKMWareType): Word; virtual;
    function CheckWareOut(aWare: TKMWareType): Word; virtual;
    function CheckWareTotal(aWare: TKMWareType): Word; virtual;
    function HasWaresIn(aWares: TKMWarePlan): Boolean; virtual;
    function PickOrder(aUnitType : TKMUnitType): Byte;
    function CheckResToBuild: Boolean;
    function GetMaxInWare: Word;
    function GetMaxOutWare: Word;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); virtual; //override for School and etc..
    procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1); virtual;
    procedure WareAddToEitherFromScript(aWare: TKMWareType; aCount: Integer);
    function WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1) : Boolean; virtual;
    function NeedsWareToBuild(aWare: TKMWareType) : Boolean;
    procedure WareTake(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual;
    procedure WareTakeFromIn(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual;
    procedure WaresTakeFromIn(aWares : TKMWarePlan; aFromScript: Boolean = False); virtual;
    procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual;
    function WareCanAddToIn(aWare: TKMWareType): Boolean; virtual;
    function WareCanAddToOut(aWare: TKMWareType): Boolean;
    function CanHaveWareType(aWare: TKMWareType): Boolean; virtual;
    function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; virtual;
    property WareOrder[aId: Byte]: Integer read GetWareOrder write SetWareOrder;
    property ResIn[aId: Byte]: Word read GetWareIn write SetWareIn;
    property ResOut[aId: Byte]: Word read GetWareOut write SetWareOut;
    property WareInLocked[aId: Byte]: Word read GetWareInLocked;
    procedure ToggleAcceptWaresIn(aWare : TKMWareType; aCount : Integer = 1);
    Procedure ProduceWare(aWare : TKMWareType; aCount : Integer = 1); virtual;
    Procedure ProduceWares(aWares : TKMWarePlan; aTake : Boolean = false); virtual;
    Procedure ProduceWareFromFill(aWare : TKMWareType; var aFillFactor : Single); virtual;
    function GetWareProdCt(aWare : TKMWareType) : Byte;
    procedure SetWareInCnt(aWare : TKMWareType; aValue : Integer);
    function CanMakeUpgrade : Boolean;
    procedure MakeUpgrade;
    property CurrentLevel : Byte Read fLevel.CurrentLevel write SetLevel;
    procedure CancelUpgrading;
    function CanCancelUpgrade : Boolean;
    property IsUpgrading : Boolean read fLevel.IsUpgrading;
    property ForceWorking : Boolean read fForceWorking write fForceWorking;
    property DontNeedRes : Boolean read fDontNeedRes write fDontNeedRes;
    function IsMineShaft : Boolean;
    function CanTaskProduceWare(aWare : TKMWareType) : Boolean;virtual;

    function GetVWareModulo(W : Integer; aWare : TKMWareType) : Byte;
    procedure IncProductionCycle(aIndex : Integer);overload;
    procedure IncProductionCycle(aWare : TKMWareType);overload;
    procedure IncProductionCycle(aWare : TKMWareType; aCount : Integer);overload;
    property ProductionCycle[aIndex : Byte] : Word read GetProductionCycle;
    function GetWareInIndex(aWare : TKMWareType) : Byte;
    function GetWareOutIndex(aWare : TKMWareType) : Byte;
    function IsValid(aHouseType : TKMHouseType = htAny; aDifferent : Boolean = false; aBuilt : Boolean = false) : Boolean; overload;
    function IsValid(aHouseType : TKMHouseTypeSet; aBuiltOnly : Boolean = true) : Boolean; overload;
    function PlaceRoad : Boolean; virtual;
    function CanDeliverToAnyPoint(aWare : TKMWareType) : Boolean; virtual;

    function MiningRange(aUnitType : TKMUnitType) : Integer;overload; virtual;
    function MiningRange(aWare : TKMWareType) : Integer;overload; virtual;
    function MiningRect(aUnitType : TKMUnitType) : TKMRect;overload; virtual;
    function MiningRect(aWare : TKMWareType) : TKMRect;overload; virtual;

    property HSpec : TKMHouseSpec read GetHouseSpec;

    procedure UpdateDemands; virtual;
    procedure PostLoadMission; virtual;
    function ObjToString(const aSeparator: String = '|'): String; override;
    procedure IncSnowStep;virtual;
    procedure IncAnimStep;virtual;
    procedure ProduceFestivalPoints(aType : TKMFestivalPointType; aAmount : Integer);
    procedure ProduceCoins;virtual;
    procedure UpdateState(aTick: Cardinal); virtual;
    procedure Paint; virtual;
  end;


  TKMHouseWFlagPoint = class(TKMHouse)
  private
    fFlagPoint: TKMPoint;
  protected
    procedure SetFlagPoint(aFlagPoint: TKMPoint); virtual;
    function GetMaxDistanceToPoint: Integer; virtual;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    property FlagPoint: TKMPoint read fFlagPoint write SetFlagPoint;
    property MaxDistanceToPoint: Integer read GetMaxDistanceToPoint;
    function IsFlagPointSet: Boolean;
    procedure ValidateFlagPoint;
    function GetValidPoint(aPoint: TKMPoint): TKMPoint;

    function ObjToString(const aSeparator: String = '|'): String; override;
  end;
  TKMHouseQuarry = class(TKMHouseWFlagPoint)
  protected
    function GetMaxDistanceToPoint: Integer; override;
  public
    function MiningRange(aUnitType : TKMUnitType) : Integer;override;
  end;

  TKMHouseTower = class(TKMHouse)
  public
    RangeMax, RangeMin : Single;
    ThrowingCycles : Byte;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function GetRangeMax : Single; virtual;
    procedure Paint; override; //Render debug radius overlay
  end;

  TKMHouseWallTower = class(TKMHouseTower)
  private
    fBoltCount : SmallInt;
    fRecruitChangeSite : Cardinal;
  public
    LeftRecruit, RightRecruit : Boolean;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    procedure  WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); override;
    function CanMakeShot : Boolean;
    procedure RemoveBolt;
    function GetRangeMax : Single; override;
    property Bolts : SmallInt read fBoltCount;
    procedure UpdateState(aTick : Cardinal); override;
    procedure Paint; override; //Render debug radius overlay
  end;

  TKMHouseWell = class(TKMHouse)
    private
      fProgress : Word;
    public
      WorkersInside : Integer;
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure UpdateState(aTick: Cardinal); override;
      procedure DecProgress(aCount : Word);
  end;

  TKMHouseMerchant = class(TKMHouse)
    private
      fProgress : Cardinal;
      fSendToHand: array[0..MAX_HANDS - 1] of Boolean;
      fCurrentHand : ShortInt;
      fStore : TKMHouse;

      function GetSendToHand(aIndex : Integer) : Boolean;
      procedure SetSendToHand(aIndex : Integer; aValue : Boolean);
      procedure FindNextHand;
    public
      function GetStore : Boolean;
      function CanWork : Boolean;
      Procedure ToggleSendToHand(aID : ShortInt);
      procedure SendToAlly(aPlan: TKMWarePlan);

      property SendToHand[aIndex : Integer] : Boolean read GetSendToHand write SetSendToHand;
      property CurrentHand : ShortInt read fCurrentHand;
      property AllyStore : TKMHouse read fStore;

      procedure PostLoadMission; override;
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure SyncLoad; override;
      function ObjToString(const aSeparator: String = '|'): String; override;
      procedure Paint; Override;
      procedure UpdateState(aTcik: Cardinal); override;
  end;

  TKMHouseAppleTree = class(TKMHouse)
    private
      fGrowPhase : Byte;
      fPhase : Word;
      fProgress : Integer;
      fNextFruitTreeID,
      fFruitTreeID : Integer;

      fFillFruits : Single;
      fParentTree : Pointer;
      fChildTrees : TPointerArray;
      fWorkAnim : TKMHouseActionType;
      fStartAnim : Integer;
      fWorkingAtChild : Integer;
      fTmpObject : Word;
      fRechecking: Boolean;
      fLastDamageID : Byte;
      procedure SetProgress(aIgnoreWater : Boolean = false);
      procedure SetGrowPhase(aValue : Byte);
      procedure SetFruitTreeID(aValue : Integer);

      function  GetParentTree: TKMHouse;
      procedure SetParentTree(aHouse : TKMHouse);
      procedure AddChildTree(aTree : Pointer);

      function GetClosedForWorker : Boolean; override;
      function PaintHouseWork : Boolean; override;
      procedure RecheckParenting;
      procedure CheckForParentTree;
      procedure RefreshChilds;
    protected
       procedure Activate(aWasBuilt: Boolean); override;
       function ShowUnoccupiedMSG : Boolean; override;
     public
      procedure AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False; aFromParent : Boolean = false); override;
      procedure UpdatePosition(const aPos: TKMPoint); override;//Used only by map editor
      procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;
      Procedure ProduceWare(aWare : TKMWareType; aCount : Integer = 1); override;
      function PlaceRoad : Boolean; override;
      procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); override; //override for School and etc..
      function WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1) : Boolean; override;

      function CanWork(aCheckChild : Boolean = false; aIncludeWater : Boolean = true) : Integer;
      function IncGrowPhase(aChildID : Integer = 0) : Boolean;
      Function NeedWater(aChildID : Integer = 0): Boolean;
      property GrowPhase : Byte read fGrowPhase write SetGrowPhase;
      property FruitType : Integer read fFruitTreeID write SetFruitTreeID;
      procedure MakeFruits(aChildID : Integer = 0);
      function CanAddChildTree(aLoc : TKMPoint) : Boolean; overload;
      function CanAddChildTree(X, Y : Integer) : Boolean; overload;
      property ParentTree : TKMHouse read GetParentTree write SetParentTree;
      function ChildTree(aIndex : Integer) : TKMHouseAppleTree;
      function ChildCount : Integer;


      procedure AddDemandBuildingMaterials; override;

      function GetFruitType : Byte;
      procedure SetNextFruitType(aStep : Integer);

      procedure SetAnimation(aChildID : Integer = -1);
      procedure GetListOfCellsAround(aCells: TKMPointDirList; aPassability: TKMTerrainPassability); override;
      procedure GetListOfCellsWithin(aCells: TKMPointList); override;
      function CanDeliverToAnyPoint(aWare : TKMWareType) : Boolean; override;

      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure SyncLoad; override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure UpdateState(aTick: Cardinal); override;
      procedure UpdateDemands; override;
      procedure Paint; Override;
      function ObjToString(const aSeparator: String = '|'): String; override;

  end;

  TKMHousePottery = class(TKMHouseWFlagPoint)
    private
      fTiles : array[1..4] of Cardinal;//Time when clay was brought to the pottery
      fStoredClay : Byte;
    protected
      function GetMaxDistanceToPoint: Integer; override;
    public
      const MAX_CLAY_TO_STORE = 30;
      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      function ObjToString(const aSeparator: String = '|'): String; override;
      function MiningRect(aWare : TKMWareType) : TKMRect;override;
      procedure BringTile;
      function TakeTile : Byte;
      function CanTakeTile : Boolean;
      function HasSpaceForNextTile : Boolean;
      function HasAnyTile : Boolean;
      function HasClayStored : Boolean;
      function HasTileOrClay : Boolean;
      function CanUseStoredClay : Boolean;
      function CanStoreClay : Boolean;
      procedure UseStoredClay;
      function FilledClay : Single;
      property StoredClay : Byte read fStoredClay;


      procedure Paint; Override;
  end;

  TKMHouseCollectors = class(TKMHouseWFlagPoint)
  private
    fWaresOut : TKMWarePlan;
    fMode : TKMCollectorsMode;
    fFill : Single;
  protected
    function GetMaxDistanceToPoint: Integer; override;
  public
    procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1); override;
    function CheckWareOut(aWare: TKMWareType): Word; override;
    procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;
    property WaresOut : TKMWarePlan read fWaresOut;
    property Mode : TKMCollectorsMode read fMode;
    procedure SetNextMode;

    procedure FillMeat(aObjType : Word);
    property Fill : Single read fFill;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMHousePalace = class(TKMHouseWFlagPoint)
  private
    const
      NO_TRRAINING_ID = high(byte);
    var
    fTrainingID : Byte;
    fProgress, fPhase, fWait, fOrderCount : Word;

    function HasWares(aIndex : Integer) : Boolean;
    Procedure TakeWares;
    function  GetOrderCount(aIndex : Integer) : Word;
    procedure SetOrderCount(aIndex : Integer; aValue : Word);
    function  GetMaxProgress : Integer;
    function  GetFullProgress : Single;
    function GetPhaseProgress : Single;
    function UnitProgress(aType : TKMUnitType) : Word;
    function GetPhaseCount(aType : TKMUnitType) : Byte;
    function GetCurrentPhase : Byte;
    function GetPhaseColor : Cardinal;
  protected
    function GetMaxDistanceToPoint: Integer; override;
    procedure Activate(aWasBuilt: Boolean); override;
  public
    //VWareIDs :array of array of Integer;
    function IsTraining : Boolean;
    property Orders[aIndex : Integer] : Word read GetOrderCount write SetOrderCount;
    property FullProgress : Single read GetFullProgress;
    property PhaseProgress : Single read GetPhaseProgress;
    property BarColor : Cardinal read GetPhaseColor;
    property UnitTrainPhases[aType : TKMUnitType] : Byte read GetPhaseCount;
    property CurrentPhase : Byte read GetCurrentPhase;
    procedure CancelOrder;
    function GetWarePlan{(aIndex : Integer = -1)} : TKMWarePlan;
    function GetWarePlanOf(aIndex : Integer) : TKMWarePlan;
    //function GetFullCost{(aIndex : Integer = -1)} : TKMWarePlan;
    function CanEquip(aIndex : Integer) : Boolean;
    function TrainedUnitType : TKMUnitType;
    function TrainedUnitID : Byte;
    function TrainingInProgress : Boolean;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    function ObjToString(const aSeparator: String = '|'): String; override;
    procedure Paint; Override;
  end;

  TKMHouseStall = class(TKMHouse)
  private
    fTicker : Cardinal;
    fSpecialPriceTick : Cardinal;
    fWareMultiplier: array[1..WARES_IN_OUT_COUNT] of Single;
    fCostMultiplier: array[0..5] of Single;
    fVWaresCount: array[0..5] of  Byte;
    fVWares: array[0..5] of String;
    procedure SetRandomWares(aUnique : Boolean);
    procedure SetRandomCost(aSpecialPrice : Boolean);
    function GetVWareName(aIndex : Byte) : String;
    function GetVWareCount(aIndex : Byte) : Byte;
  protected
    procedure Activate(aWasBuilt: Boolean); override;
  public
    procedure BuyItem(aIndex : Byte; aCount : Byte);
    procedure BuyCoin(aIndex : Byte; aCount : Byte);
    function RatioFrom(aIndex : Byte) : Word;
    function RatioTo(aIndex : Byte) : Word;
    function WareRatioTo(aIndex : Byte) : Word;
    function CanBuyItem(aIndex : Byte) : Boolean;
    property VWare[aIndex : Byte] : String read GetVWareName;
    property VWareCount[aIndex : Byte] : Byte read GetVWareCount;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;

    procedure Paint; Override;

  end;


  TKMHouseProdThatch = class(TKMHouseWFlagPoint)
    private
      fAnims : array[TKMProdThatchAnimType] of TKMPTAnim;
      fWorkPointsTaken : TKMArray<TKMPoint>;
      fTiles : array[1..15] of Cardinal;
      fStoredClay : Byte;

      fGrainType, fGrassType, fVegeType: TKMGrainType;
      fFillWine,
      fFillHay,
      fFillSeeds,
      fFillCorn,
      fFillVege : Single;
      function HasOrderForWare(aType : TKMWareType) : Boolean;
      function HasOrderForWares(aTypes : TKMWareTypeSet) : Boolean;
      procedure SetNextGrassType(aValue : Integer);
      procedure SetNextVegeType(aValue : Integer);
      function GetGrainTypes : TKMGrainFarmSet;
      function GetCutGrainTypes : TKMGrainFarmSet;
      const MAX_CLAY_TO_STORE = 50;
    protected
      procedure MakeSound; Override; //Swine/stables make extra sounds
      function GetMaxDistanceToPoint: Integer; override;
    public
      function IsPointTaken(P : TKMPoint): Boolean;
      procedure TakePoint(P : TKMPoint);
      procedure RemovePoint(P : TKMPoint);
      procedure ClearPoints;
      //clay picker functions
      procedure BringTile;
      function TakeTile : Byte;
      function CanTakeTile : Boolean;
      function HasSpaceForNextTile : Boolean;
      function HasAnyTile : Boolean;
      function HasClayStored : Boolean;
      function HasTileOrClay : Boolean;
      function CanUseStoredClay : Boolean;
      function CanStoreClay : Boolean;
      procedure UseStoredClay;
      function MiningRange(aUnit : TKMUnitType) : Integer;override;
      function MiningRect(aWare : TKMWareType) : TKMRect;override;

      property GrainType : TKMGrainType read fGrainType;
      property GrassType : TKMGrainType read fGrassType;
      property VegeType : TKMGrainType read fVegeType;
      property GrainTypes : TKMGrainFarmSet read GetGrainTypes;
      property CutGrainTypes : TKMGrainFarmSet read GetCutGrainTypes;
      procedure SetNextGrainType(aValue : Integer; aType : Byte);
      procedure GrainCut(aGrain : TKMGrainType);

      procedure SetGrainTypes(aGrain, aGrass, aVegetables : TKMGrainType); overload;
      procedure SetGrainTypes(aGrain, aGrass, aVegetables : Byte); overload;
      function HasGrain : Boolean;
      function HasGrass : Boolean;
      function HasVege : Boolean;

      function AcceptsWorker(aWorker: Pointer) : Boolean; override;
      procedure CheckWorkersForSlot; override;
      function CanHasWorker(aType : TKMUnitType) : Boolean; override;
      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure ProduceStarts(aWare: TKMWareType);
      procedure UpdateState(aTick: Cardinal); override;
      procedure Paint; Override;
      function ObjToString(const aSeparator: string = '|'): string; override;
  end;

  TKMHouseFarm = class(TKMHouse)
    private
      fGrainType, fGrassType, fVegeType: TKMGrainType;
      fFillHay,
      fFillSeeds,
      fFillCorn,
      fFillVege : Single;
      fMode : TKMWoodcutterMode;
      function GetGrainTypes : TKMGrainFarmSet;
      function GetCutGrainTypes : TKMGrainFarmSet;
      procedure SetNextGrassType(aValue : Integer);
      procedure SetNextVegeType(aValue : Integer);
      procedure SetMode(aValue : TKMWoodcutterMode);
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      property GrainType : TKMGrainType read fGrainType;
      property GrassType : TKMGrainType read fGrassType;
      property VegeType : TKMGrainType read fVegeType;
      property GrainTypes : TKMGrainFarmSet read GetGrainTypes;
      property CutGrainTypes : TKMGrainFarmSet read GetCutGrainTypes;
      procedure SetNextGrainType(aValue : Integer; aType : Byte);
      procedure SetGrainTypes(aGrain, aGrass, aVegetables : TKMGrainType); overload;
      procedure SetGrainTypes(aGrain, aGrass, aVegetables : Byte); overload;

      function GetProgressArray : TSingleArray;
      function GetTexIDsArray : TKMWord2Array;
      procedure GrainCut(aGrain : TKMGrainType);
      function HasGrain : Boolean;
      function HasGrass : Boolean;
      function HasVege : Boolean;
      property Mode: TKMWoodcutterMode read fMode write SetMode;
  end;
  TKMHouseVineYard = class(TKMHouse)
    private
      fFillWine : Single;
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure GrainCut(aGrain : TKMGrainType);
      function CanMakeWine(aNoFieldsToCut : Boolean) : Boolean;
      function GetProgressArray : TSingleArray;
      function WineToProduce : Byte;
      property WineProgress: Single read fFillWine;
      procedure ProduceWine;
  end;

  TKMHouseShipyard = class(TKMHouse)
    private
      fWayOutID : Byte;
      fShipType, fNextShipType : TKMUnitType;
      fShipSketchPosition : TKMPointDir;
      fShipPhase : Byte;
      fShipBuiltOf : TKMWarePlan;
      fDoWork : Boolean;
      fWaresOut : TKMWarePlan;
      function GetPhasesCount : Byte;
      procedure SetNextShipType(aValue : TKMUnitType);overload;
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;


      procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1); override;
      function CheckWareOut(aWare: TKMWareType): Word; override;
      procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
      function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;

      property ShipType : TKMUnitType read fShipType;
      property NextShipType : TKMUnitType read fNextShipType write SetNextShipType;
      property DoWork : Boolean read fDoWork write fDoWork;
      procedure SetNextShipType(aAmount : Integer);overload;
      property WaresOut : TKMWarePlan read fWaresOut;

      procedure IncSketchPhase(aWares : TKMWarePlan);
      procedure StartWorking;
      function CanWork : Boolean;

      function GetWarePlan : TKMWarePlan;


      procedure Paint; Override;
  end;

  TKMHouseWall = class(TKMHouse)
  private
    procedure UpdateWallAround;
  protected
    fTicker : Cardinal;
    procedure AfterCreate(aWasBuilt: Boolean); override;
    procedure Activate(aWasBuilt: Boolean); override;
    procedure FinishedLevel(aLevel : Byte); override;
    procedure UpdateEntrancePos; override;
    procedure UpdatePointBelowEntrance; virtual;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint;override;
  end;

  TKMHouseWallSingle = class(TKMHouseWall)
  private
    fWallStyle : Byte;
  protected
    procedure AfterCreate(aWasBuilt: Boolean); override;
    procedure Activate(aWasBuilt: Boolean); override;
    procedure FinishedLevel(aLevel : Byte); override;
    procedure SetLevel(aValue : Byte); override;
  public
    function GetStonePic : Integer; override;
    function GetSnowPic : Integer; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateConnection;
  end;

  TKMHouseSiegeTower = class (TKMHouseWFlagPoint)
  private
  public
    const
      MAX_UNITS_INSIDE = 10;
    function GetUnitWeight(aUnitType : TKMUnitType) : Byte;
    function GetTotalWeight : Byte;
    function CanEnter(aUnitType : TKMUnitType = utAny) : Boolean;
    procedure Paint;override;
  end;

  TAppleTree = TKMHouseAppleTree;
  TShipyard = TKMHouseShipyard;//just a shurtcut
  TVineyard = TKMHouseVineYard;//just a shurtcut
  TFarm = TKMHouseFarm;//just a shurtcut
  TThatch = TKMHouseProdThatch;//just a shurtcut
  TStall = TKMHouseStall;//just a shurtcut

  function RottenFood(aWare : TKMWareType; aCount : Byte; aTime : Cardinal) : TKMHouseRottenFood;
implementation
uses
  // Do not add KM_Game dependancy! Entities should be isolated as much as possible
  TypInfo, SysUtils, Math, KromUtils,
  KM_CommonHelpers,
  KM_Entity, KM_HandsCollection,
  KM_Game,KM_GameParams, KM_MapTypes,
  KM_Terrain, KM_RenderPool, KM_RenderAux, KM_Sound,
  KM_Hand, KM_HandLogistics, KM_HandTypes,
  KM_Units, KM_UnitWarrior, KM_HouseWoodcutters,
  KM_Resource, KM_ResSound, KM_ResTexts, KM_ResUnits, KM_ResMapElements,
  KM_Log, KM_ScriptingEvents, KM_CommonUtils, KM_MapEdTypes,
  KM_RenderDebug,
  KM_TerrainTypes,
  KM_CommonExceptions, KM_JSONUtils,
  KM_UnitTaskThrowRock,
  KM_ResTileset,
  KM_Achievements,
  KM_Projectiles, KM_CommonGameTypes;

const
  // Delay, in ticks, from user click on DeliveryMode btn, to tick, when mode will be really set.
  // Made to prevent serf's taking/losing deliveries only because player clicks throught modes.
  // No hurry, let's wait a bit for player to be sure, what mode he needs
  UPDATE_DELIVERY_MODE_DELAY = 10;


function RottenFood(aWare : TKMWareType; aCount : Byte; aTime : Cardinal) : TKMHouseRottenFood;
begin
  Result.WareType := aWare;
  Result.TimeToRotten := aTime;
  Result.Count := aCount;
end;
{ TKMHouseSketch }
constructor TKMHouseSketch.Create;
begin
  inherited Create(etHouse, 0, -1); //Just do nothing; (For house loading)
end;


constructor TKMHouseSketch.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID);
begin
  Assert((PosX <> 0) and (PosY <> 0)); // Can create only on map

  inherited Create(etHouse, aUID, aOwner);

  fType     := aHouseType;

  fPosition := KMPoint(PosX, PosY);
  UpdateEntrancePos;
end;


{Return Entrance of the house, which is different than house position sometimes}
procedure TKMHouseSketch.UpdateEntrancePos;
begin
  if IsEmpty then Exit;
  
  fEntrance.X := fPosition.X + gRes.Houses[fType].EntranceOffsetX;
  fEntrance.Y := fPosition.Y + gRes.Houses[fType].EntranceOffsetY;
  Assert((fEntrance.X > 0) and (fEntrance.Y > 0));

  fPointBelowEntrance := KMPointBelow(fEntrance);

  {if not (fType in WALL_HOUSES) then
    Exit;

  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointLeft(fEntrance); 
    
  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointRight(fEntrance);   
    
  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointAbove(fEntrance); 

  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointBelow(fEntrance);}
    
  

end;


procedure TKMHouseSketch.SetPointBelowEntrance(aValue : TKMPoint);
begin
  fPointBelowEntrance := aValue;
end;

procedure TKMHouseSketch.SetPosition(const aPosition: TKMPoint);
begin
  fPosition.X := aPosition.X;
  fPosition.Y := aPosition.Y;


  UpdateEntrancePos;
end;


function TKMHouseSketch.IsEmpty: Boolean;
begin
  Result :=    (UID = -1)
            or (HouseType = htNone)
            or (Position.X = -1)
            or (Position.Y = -1);
end;

function TKMHouseSketch.HasMoreEntrances : Boolean;
begin
  Result := false;
end;

function TKMHouseSketch.GetClosestEntrance(aLoc : TKMPoint) : TKMPointDir;
begin
  Result := KMPointDir(Entrance, dirS);
end;

function TKMHouseSketch.Entrances: TKMPointDirArray;
begin
  Result := [KMPointDir(Entrance, dirS)];
end;

function TKMHouseSketch.ObjToStringShort(const aSeparator: String = '|'): String;
begin
  if Self = nil then Exit('nil');

  Result := inherited ObjToStringShort(aSeparator) +
            Format('%sType = %s%sEntr = %s',
                  [aSeparator,
                   GetEnumName(TypeInfo(TKMHouseType), Integer(fType)), aSeparator,
                   TypeToString(Entrance)]);
end;



{ TKMHouseSketchEdit}
constructor TKMHouseSketchEdit.Create;
begin
  inherited Create(-1, htNone, -1, -1, -1);

  fEditable := True;
end;


procedure TKMHouseSketchEdit.Clear;
begin
  SetUID(-1);
  SetHouseType(htNone);
  SetPosition(KMPoint(0,0));
end;


procedure TKMHouseSketchEdit.CopyTo(aHouseSketch: TKMHouseSketchEdit);
begin
  aHouseSketch.SetUID(UID);
  aHouseSketch.SetHouseType(HouseType);
  aHouseSketch.SetPosition(Position);
end;


procedure TKMHouseSketchEdit.SetHouseUID(aUID: Integer);
begin
  if fEditable then
    SetUID(aUID);
end;


procedure TKMHouseSketchEdit.SetHouseType(aHouseType: TKMHouseType);
begin
  if fEditable then
    fType := aHouseType;
end;


procedure TKMHouseSketchEdit.SetPosition(const aPosition: TKMPoint);
begin
  if not fEditable then Exit;

  inherited;
end;


function TKMHouseSketchEdit.GetInstance: TKMHouse;
begin
  //Not used. Make compiler happy
  raise Exception.Create('Can''t get instance of TKMHouseSketchEdit');
end;


function TKMHouseSketchEdit.GetIsSelectable: Boolean;
begin
  Result := False;
end;


function TKMHouseSketchEdit.GetPositionForDisplayF: TKMPointF;
begin
  Assert(False, 'Should not get positionF of TKMHouseSketchEdit');
  //Not used. Make compiler happy
  Result := Entrance.ToFloat;
end;


{ TKMHouse }
constructor TKMHouse.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
  procedure SetOutputWares(aWares : array of TKMWareType);
  var I : Integer;
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
      if I - 1 < Length(aWares) then
        fWareOutput[I] := aWares[I - 1]
      else
        fWareOutput[I] := wtNone;
  end;
  procedure SetInputWares(aWares : array of TKMWareType);
  var I : Integer;
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
      if I - 1 < Length(aWares) then
        fWareInput[I] := aWares[I - 1]
      else
        fWareInput[I] := wtNone;
  end;

var
  I: Integer;
  //firstHouse : TKMHouse;
begin
  inherited Create(aUID, aHouseType, PosX, PosY, aOwner);
  //SetPosition(KMPoint(PosX - gRes.Houses[aHouseType].EntranceOffsetX, PosY));
  fBuildState := aBuildState;

  fBuildSupplyWood  := 0;
  fBuildSupplyStone := 0;
  fBuildSupplyTile := 0;
  fBuildReserve     := 0;
  fBuildingProgress := 0;
  fDamage           := 0; //Undamaged yet

  SetCost;

  fPlacedOverRoad   := gTerrain.TileHasRoad(Entrance);

  fWorker           := nil;
  SetLength(fWorkers, 0);//no workers at the start
  SetLength(fDeliveryFromHouses, 0);//no workers at the start
  SetLength(fNotAcceptWorkers, 0);//no workers at the start
  //Initially repair is [off]. But for AI it's controlled by a command in DAT script
  fBuildingRepair   := False; //Don't set it yet because we don't always know who are AIs yet (in multiplayer) It is set in first UpdateState
  DoorwayUse        := 0;
  fText           := '';
  fNewDeliveryMode  := dmDelivery;
  fDeliveryMode     := dmDelivery;
  fUpdateDeliveryModeOnTick := 0;

  fWariant := -1;
  if aHouseType = htAppleTree then
    fWariant := -1
  else
  if gRes.Houses[aHouseType].HasStoneWariants > 0 then
    if CheckRandom(100 - 100 div gRes.Houses[aHouseType].HasStoneWariants) then
      fWariant := KaMRandom(gRes.Houses[aHouseType].HasStoneWariants, 'TKMHouse.Create:Random stone pic');

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    fWareIn[I] := 0;
    fWareDeliveryCount[I] := 0;
    fWareDemandsClosing[I] := 0;
    fWareOut[I] := 0;
    fWareOrder[I] := 0;
    fProductionCycles[I] := 0;

    fWareInput[I] := wtNone;
    fWareOutput[I] := wtNone;
    if aHouseType = htStall then
      fWareBlocked[I] := GetMaxInWare
    else
      fWareBlocked[I] := 0;
  end;

  if length(gRes.Houses[fType].WareInputSlots) > 0 then
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
    begin

      fWareInput[I] := gRes.Houses[fType].WareInputSlots[0].WareInput[I];
      fWareOutput[I] := gRes.Houses[fType].WareInputSlots[0].WareOutput[I];
      fWareBlocked[I] := GetMaxInWare;
    end;

  end else
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      fWareInput[I] := gRes.Houses[fType].WareInput[I];
      fWareOutput[I] := gRes.Houses[fType].WareOutput[I];
      TransferWare[I] := false;
    end;

  end;
  If gGame.Params.MPMode = mmBottomless then
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      if fWareInput[I] = wtWater then
        fWareInput[I] := wtNone;
      if fWareOutput[I] = wtWater then
        fWareOutput[I] := wtNone;
    end;

  for I := 0 to high(fWareOutPool) do
    fWareOutPool[I] := 0;
  fStyle := 0;

  fIsDestroyed := False;
//  fPointerCount := 0;
  fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES;

  fResourceDepletedMsgIssued := False;
  fNeedIssueOrderCompletedMsg := False;
  fOrderCompletedMsgIssued := False;

  // By default allow to show all houses to allies for locs, where human could play
  // Do not show AI-only frienly loc houses (they could have thousands of wares)
  AllowAllyToSelect :=  gHands[Owner].IsHuman or gHands[Owner].CanBeHuman;

  if aBuildState = hbsDone then //House was placed on map already Built e.g. in mission maker
  begin
    Activate(False);
    
    fBuildingProgress := gRes.Houses[fType].MaxHealth;
    gTerrain.SetHouse(self, Owner, hsBuilt, false {(gGameParams <> nil) and not gGameParams.IsMapEditor}); //Sets passability and flattens terrain if we're not in the map editor
  end
  else
    gTerrain.SetHouse(self, Owner, hsFence); //Terrain remains neutral yet

  //Built houses accumulate snow slowly, pre-placed houses are already covered
  CheckOnTerrain;
  fSnowStep := Byte(aBuildState = hbsDone);
  WorkingTime := 0;
  TotalWorkingTime := 0;

  {firstHouse := gHands[aOwner].FindHouse(fType, 1);
  if (firstHouse <> nil) and not (firstHouse.IsDestroyed) then
    for I := 1 to WARES_IN_OUT_COUNT do
      fWareBlocked[I] := firstHouse.fWareBlocked[I];}
  LastCellID := 0;
  fWasTookOver := false;
  Indestructible := false;
  FlagColor := 0;
  fFoodToRot.Clear;


  if HouseType = htMetallurgists then
  begin
    WareOrder[1] := 1000;
    WareOrder[2] := 0;
  end;
  AfterCreate(false);
end;



constructor TKMHouse.Load(LoadStream: TKMemoryStream);
var
  I, newCount: Integer;
  hasAct: Boolean;
begin
  inherited;

  LoadStream.CheckMarker('House');
  LoadStream.Read(fType, SizeOf(fType));
  LoadStream.Read(fPosition);
  UpdateEntrancePos;
  LoadStream.Read(fBuildState, SizeOf(fBuildState));
  LoadStream.Read(fBuildSupplyWood);
  LoadStream.Read(fBuildSupplyStone);
  LoadStream.Read(fBuildSupplyTile);

  LoadStream.Read(fBuildTileDelivered);
  LoadStream.Read(fBuildWoodDelivered);
  LoadStream.Read(fBuildStoneDelivered);

  LoadStream.Read(fBuildReserve);
  LoadStream.Read(fBuildingProgress, SizeOf(fBuildingProgress));
  LoadStream.Read(fDamage, SizeOf(fDamage));
  LoadStream.Read(fWorker, 4); //subst on syncload
  LoadStream.Read(fHouseToDeliver, 4); //subst on syncload
  LoadStream.Read(fBuildingRepair);
  LoadStream.Read(Byte(fDeliveryMode));
  LoadStream.Read(Byte(fNewDeliveryMode));
  LoadStream.Read(fUpdateDeliveryModeOnTick);
  LoadStream.Read(fIsClosedForWorker);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareIn[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareDeliveryCount[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareDemandsClosing[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareOut[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareOrder[I], SizeOf(fWareOrder[I]));
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareInput[I], SizeOf(fWareInput[I]));
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareOutput[I], SizeOf(fWareOutput[I]));
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fWareBlocked[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(TransferWare[I]);
  for I:=1 to WARES_IN_OUT_COUNT do LoadStream.Read(fProductionCycles[I]);
//  for I:=1 to 4 do LoadStream.Read(fWareOrderDesired[I], SizeOf(fWareOrderDesired[I]));

  if fType in [htBarracks, htStore] then
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      fWareInput[I] := gRes.Houses[fType].WareInput[I];
      fWareOutput[I] := gRes.Houses[fType].WareOutput[I];
    end;

  if gRes.Houses[fType].IsWorkshop then
    LoadStream.Read(fWareOutPool, SizeOf(fWareOutPool)); //todo: Should be SizeOf()

  LoadStream.Read(fLastOrderProduced);
  LoadStream.Read(FlagAnimStep);
  LoadStream.Read(WorkAnimStep);
  LoadStream.ReadData(fIsOnTerrain);
  LoadStream.Read(fSnowStep);
  LoadStream.Read(fIsDestroyed);
  LoadStream.Read(fTimeSinceUnoccupiedReminder);
  LoadStream.Read(fDisableUnoccupiedMessage);
  LoadStream.Read(fNeedIssueOrderCompletedMsg);
  LoadStream.Read(fOrderCompletedMsgIssued);
  LoadStream.Read(hasAct);
  if hasAct then
  begin
    CurrentAction := TKMHouseAction.Create(nil, hstEmpty); //Create action object
    CurrentAction.Load(LoadStream); //Load actual data into object
  end;
  LoadStream.Read(fResourceDepletedMsgIssued);
  LoadStream.Read(DoorwayUse);
  LoadStream.Read(fPlacedOverRoad);
  LoadStream.Read(fWariant);
  PicWariant := fWariant;
  LoadStream.Read(fStyle);
  LoadStream.Read(fSlotID);
  LoadStream.Read(fIsBurning);
  With fLevel do
  begin
    LoadStream.Read(CurrentLevel);
    LoadStream.Read(Progress);
    LoadStream.Read(BuildingProgress);
    LoadStream.Read(IsUpgrading);
  end;
  LoadStream.ReadANSI(fText);
  LoadStream.Read(fForceWorking);
  LoadStream.Read(fDontNeedRes);
  LoadStream.Read(WorkingTime);
  LoadStream.Read(TotalWorkingTime);
  LoadStream.Read(WareSlotChangedByAI);
  LoadStream.Read(BootsReserved);
  LoadStream.Read(LastCellID);
  LoadStream.Read(fWasTookOver);
  LoadStream.Read(Indestructible);
  LoadStream.Read(FlagColor);

  LoadStream.Read(newCount);
  Setlength(fWorkers, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fWorkers[I], 4);

  LoadStream.Read(newCount);
  Setlength(fDeliveryFromHouses, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fDeliveryFromHouses[I], 4);

  LoadStream.Read(newCount);
  Setlength(fNotAcceptWorkers, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fNotAcceptWorkers[I], 4);

  fFoodToRot.LoadFromStream(LoadStream);
  LoadStream.ReadData(fBuildCost);
end;


procedure TKMHouse.SyncLoad;
var I : Integer;
begin
  fWorker := TKMUnit(gHands.GetUnitByUID(Integer(fWorker)));
  fHouseToDeliver := TKMHouse(gHands.GetHouseByUID(Integer(fHouseToDeliver)));

  for I := 0 to High(fWorkers) do
    fWorkers[I] := TKMUnit(gHands.GetUnitByUID(Integer(fWorkers[I])));

  for I := 0 to High(fDeliveryFromHouses) do
    fDeliveryFromHouses[I] := TKMHouse(gHands.GetHouseByUID(Integer(fDeliveryFromHouses[I])));

  for I := 0 to High(fNotAcceptWorkers) do
    fNotAcceptWorkers[I] := TKMUnit(gHands.GetUnitByUID(Integer(fNotAcceptWorkers[I])));

  CurrentAction.SyncLoad;
end;


destructor TKMHouse.Destroy;
var I : Integer;
begin
  FreeAndNil(CurrentAction);
  gHands.CleanUpUnitPointer(TKMUnit(fWorker));

  for I := 0 to High(fWorkers) do
    gHands.CleanUpUnitPointer(TKMUnit(fWorkers[I]));

  for I := 0 to High(fDeliveryFromHouses) do
    gHands.CleanUpHousePointer(TKMHouse(fDeliveryFromHouses[I]));

  for I := 0 to High(fNotAcceptWorkers) do
    gHands.CleanUpUnitPointer(TKMUnit(fNotAcceptWorkers[I]));

  inherited;
end;


procedure TKMHouse.AddDemandsOnActivate(aWasBuilt: Boolean);
var
  I: Integer;
  W: TKMWareType;
  updated : Boolean;
begin
  updated := false;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    W := fWareInput[I];
    with gHands[Owner].Deliveries.Queue do
    case W of
      wtNone:    ;
      wtWarfare: AddDemand(Self, nil, W, 1, dtAlways, diNorm);
      wtAll:     AddDemand(Self, nil, W, 1, dtAlways, diNorm);
      else        begin
                    If not updated then
                      UpdateDemands;
                    updated := true;
                    //demandsCnt := Min(GetWareDistribution(I), GetMaxInWare - GetAcceptWareIn(W));
                    //AddDemand(Self, nil, W, demandsCnt, dtOnce, diNorm); //Every new house needs 5 resource units
                    //WareDeliveryCnt[I] := WareDeliveryCnt[I] + demandsCnt; //Keep track of how many resources we have on order (for distribution of wares)
                  end;
    end;
  end;
end;


procedure TKMHouse.Activate(aWasBuilt: Boolean);

  function ObjectShouldBeCleared(X,Y: Integer): Boolean;
  begin
    Result := not gTerrain.ObjectIsChopableTree(KMPoint(X,Y), [caAge1,caAge2,caAge3,caAgeFull,caAgeFall])
              and not gTerrain.ObjectIsCorn(X,Y)
              and not gTerrain.ObjectIsWine(X,Y);
  end;

var
  P1, P2: TKMPoint;
  I : Integer;
  sight : Word;
begin
  // Only activated houses count
  gHands[Owner].Locks.HouseCreated(fType);
  gHands[Owner].Stats.HouseCreated(fType, aWasBuilt);
  fLevel.UpgradingTime := gGameParams.Tick + 100;
  sight := gRes.Houses[fType].Sight;

  If (HouseType = htWallTower) and gHands[Owner].BuildDevUnlocked(20) then
    sight := Sight + 5;

  gHands.RevealForTeam(Owner, KMPoint(fPosition.X, fPosition.Y), gRes.Houses[fType].Sight, FOG_OF_WAR_MAX, frtHouse);

  CurrentAction := TKMHouseAction.Create(Self, hstEmpty);
  CurrentAction.SubActionAdd([haFlagpole, haFlag1..haFlag3]);

  UpdateDamage; //House might have been damaged during construction, so show flames when it is built
  AddDemandsOnActivate(aWasBuilt);
  fLevel.CurrentLevel := 0;


  //Fix for diagonal blocking objects near house entrance
  if aWasBuilt then
  begin
    P1 := KMPoint(Entrance.X - 1, Entrance.Y + 1) ; //Point to the left from PointBelowEntrance
    P2 := KMPoint(P1.X + 2, P1.Y);        //Point to the right from PointBelowEntrance




    if not gTerrain.CanWalkDiagonally(Entrance, P1.X, P1.Y)
      and ObjectShouldBeCleared(P1.X + 1, P1.Y) then // Do not clear choppable trees
      gTerrain.RemoveObject(KMPoint(P1.X + 1, P1.Y)); //Clear object at PointBelowEntrance

    if not gTerrain.CanWalkDiagonally(Entrance, P2.X, P2.Y)
      and ObjectShouldBeCleared(P2.X, P2.Y) then
      gTerrain.RemoveObject(P2);
  end;

  if gHands[Owner].IsComputer then
    for I := 1 to WARES_IN_OUT_COUNT do
      TransferWare[I] := true;
  fIsBurning := 0;
end;

procedure TKMHouse.AfterCreate(aWasBuilt: Boolean);
begin

end;


procedure TKMHouse.Remove;
begin
  Assert(gGameParams.IsMapEditor, 'Operation allowed only in the MapEd');

  Demolish(Owner, True);
  gHands[Owner].Houses.DeleteHouseFromList(Self);
end;


//IsSilent parameter is used by Editor and scripts
procedure TKMHouse.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  I: Integer;
  W: TKMWareType;
  WP : array of TKMWareType;
  procedure AddWare(aWare : TKMWareType);
  begin
    if (aWare in [wtGold, wtGoldOre, wtIron, wtIronOre, wtStone, wtStoneBolt, wtCoal, wtTrunk, wtTimber]) then
    begin
      setLength(WP, length(WP) + 1);
      WP[high(WP)] := aWare;
    end;
  end;
begin
  if IsDestroyed or fIsBeingDemolished then Exit;

  fIsBeingDemolished := True; //Make sure script doesn't try to demolish this house again during event
  OnDestroyed(Self, aFrom); //We must do this before setting fIsDestroyed for scripting
  fIsBeingDemolished := False; //No longer required

  //If anyone still has a pointer to the house he should check for IsDestroyed flag
  fIsDestroyed := True;
  IF HouseType = htBarracks then
  fIsDestroyed := True;
  HouseToDeliver.RemHouseDeliveryFrom(self);
  for I := High(fDeliveryFromHouses) downto 0 do
    TKMHouse(fDeliveryFromHouses[I]).HouseToDeliver := nil;

  //Play sound
  if (fBuildState > hbsNoGlyph) and not IsSilent
  and (gMySpectator <> nil) //gMySpectator is nil during loading
  and (gMySpectator.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) >= 255) then
    gSoundPlayer.Play(sfxHouseDestroy, fPosition);

  //NOTE: We don't run Stats.WareConsumed on fBuildSupplyWood/Stone as the
  //delivery task already did that upon delivery (they are irreversibly consumed at that point)
  SetLength(WP, 0);
  for I := 1 to WARES_IN_OUT_COUNT do
  begin

    W := fWareInput[I];
    if W in WARES_VALID then
    begin
      if ResIn[I] > 0 then
        AddWare(W);
      gHands[Owner].Stats.WareConsumed(W, ResIn[I]);
    end;
    W := fWareOutput[I];
    if W in WARES_VALID then
    begin
      if fWareOut[I] > 0 then
        AddWare(W);
      gHands[Owner].Stats.WareConsumed(W, fWareOut[I]);
    end;
  end;

  gTerrain.SetHouse(self, HAND_NONE, hsNone);

  //Leave rubble
  if not IsSilent then
    gTerrain.AddHouseRemainder(fPosition, fType, fBuildState, WP);

  BuildingRepair := False; //Otherwise labourers will take task to repair when the house is destroyed
  if (BuildingState in [hbsNoGlyph, hbsWood]) or IsSilent then
  begin
    if gTerrain.TileHasRoad(Entrance) and not fPlacedOverRoad then
    begin
      gTerrain.RemRoad(Entrance);
      if not IsSilent then
        gTerrain.Land^[Entrance.Y, Entrance.X].TileOverlay := OVERLAY_DIG_3; //Remove road and leave dug earth behind
    end;
  end;
  If aFrom <> -1 then
    If aFrom <> Owner then
    begin
      If HouseType in FEST_BEST_HOUSES then
        gHands[aFrom].AddFestivalPoints(fptBuilding, 20)
      else
      If HouseType in FEST_BETTER_HOUSES then
        gHands[aFrom].AddFestivalPoints(fptBuilding, 10)
      else
      If HouseType in FEST_GOOD_HOUSES then
        gHands[aFrom].AddFestivalPoints(fptBuilding, 6)
      else
        gHands[aFrom].AddFestivalPoints(fptBuilding, 3);
    end;

  FreeAndNil(CurrentAction);

  //Leave disposing of units inside the house to themselves

  //Notify the script that the house is now completely gone
  gScriptEvents.EventHouseAfterDestroyed(HouseType, Owner, Entrance.X, Entrance.Y);
end;


//Used by MapEditor
//Set house to new position
procedure TKMHouse.UpdatePosition(const aPos: TKMPoint);
var
  wasOnSnow : TKMTerrPicType;
  isRallyPointSet: Boolean;
begin
  Assert(gGameParams.IsMapEditor);

  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  gTerrain.SetHouse(self, HAND_NONE, hsNone);

  if gHands[Owner].CanAddHousePlan(aPos, HouseType) then
  begin
    isRallyPointSet := False;
    //Save if flag point was set for previous position
    if (Self is TKMHouseWFlagPoint) then
      isRallyPointSet := TKMHouseWFlagPoint(Self).IsFlagPointSet;

    gTerrain.RemRoad(Entrance);

    SetPosition(KMPoint(aPos.X - gRes.Houses[fType].EntranceOffsetX, aPos.Y - gRes.Houses[fType].EntranceOffsetY));
    gTerrain.SetRoad(Entrance, Owner, rtStone);

    //Update rally/cutting point position for houses with flag point after change fPosition
    if (Self is TKMHouseWFlagPoint) then
    begin
      if not isRallyPointSet then
        TKMHouseWFlagPoint(Self).FlagPoint := PointBelowEntrance
      else
        TKMHouseWFlagPoint(Self).ValidateFlagPoint;
    end;
  end;
  case fBuildState of
    hbsNoGlyph,
    hbsWood: gTerrain.SetHouse(self, Owner, hsFence); // Update terrain tiles for house;
    hbsStone,
    hbsDone: gTerrain.SetHouse(self, Owner, hsBuilt); // Update terrain tiles for house;
  end;



  //Do not remove all snow if house is moved from snow to snow
  wasOnSnow := fIsOnTerrain;
  CheckOnTerrain;
  if not (wasOnSnow <> tptNone) or not (fIsOnTerrain <> tptNone) then
    fSnowStep := 0;
end;


//Check and proceed if we Set or UnSet dmTakeOut delivery mode
procedure TKMHouse.CheckTakeOutDeliveryMode;
var
  I: Integer;
  resCnt: Word;
  W: TKMWareType;
begin
  // House had dmTakeOut delivery mode
  // Remove offers from this house then
  if fDeliveryMode = dmTakeOut then
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      W := fWareInput[I];
      resCnt := ResIn[I] - WareInLocked[I];
      if (W <> wtNone) and (resCnt > 0) then
        gHands[Owner].Deliveries.Queue.RemOffer(Self, W, resCnt);
    end;

  // House will get dmTakeOut delivery mode
  // Add offers to this house then
  if fNewDeliveryMode = dmTakeOut then
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      W := fWareInput[I];
      resCnt := ResIn[I] - WareInLocked[I];

      if not (W in [wtNone, wtAll, wtWarfare]) and (resCnt > 0) then
        gHands[Owner].Deliveries.Queue.AddOffer(Self, W, resCnt);
    end;
  end;
end;


procedure TKMHouse.SetWareDeliveryCount(aIndex: Integer; aCount: Word);
begin
  fWareDeliveryCount[aIndex] := EnsureRange(aCount, 0, High(Word));
end;


function TKMHouse.GetWareDeliveryCount(aIndex: Integer): Word;
begin
  Result := fWareDeliveryCount[aIndex];
end;


procedure TKMHouse.SetWareDemandsClosing(aIndex: Integer; aCount: Word);
begin
  fWareDemandsClosing[aIndex] := EnsureRange(aCount, 0, High(Word));
end;


function TKMHouse.GetWareDemandsClosing(aIndex: Integer): Word;
begin
  Result := fWareDemandsClosing[aIndex];
end;


//Get delivery mode, used for some checks in 'ShouldAbandonDeliveryXX'
//aImmidiate - do we want to have immidiate check (then will get "fake" NewDeliveryMode) or no (real DeliveryMode will be returned)
function TKMHouse.GetDeliveryModeForCheck(aImmidiate: Boolean): TKMDeliveryMode;
begin
  if aImmidiate then
    Result := NewDeliveryMode
  else
    Result := DeliveryMode;
end;

function TKMHouse.ShowUnoccupiedMSG: Boolean;
begin
  Result := true;
end;

function TKMHouse.SlotHasAnyWare(aWares : TKMWareTypeSet) : Boolean;
var WT : TKMWareType;
begin
  Result := false;
  for WT in aWares do
    if GetWareOutIndex(WT) > 0 then
      Exit(true);
end;

procedure TKMHouse.UpdateDeliveryMode;
begin
  if fNewDeliveryMode = fDeliveryMode then
    Exit;

  CheckTakeOutDeliveryMode;

  fUpdateDeliveryModeOnTick := 0;
  fDeliveryMode := fNewDeliveryMode;
  gLog.LogDelivery('DeliveryMode updated to ' + IntToStr(Ord(fDeliveryMode)));
end;


//Set NewDelivery mode. Its going to become a real delivery mode few ticks later
procedure TKMHouse.SetNewDeliveryMode(aValue: TKMDeliveryMode);
begin
  fNewDeliveryMode := aValue;

  fUpdateDeliveryModeOnTick := fTick + UPDATE_DELIVERY_MODE_DELAY;

  gLog.LogDelivery('NewDeliveryMode set to ' + IntToStr(Ord(fNewDeliveryMode)));
end;


procedure TKMHouse.SetNextDeliveryMode;
begin
  SetNewDeliveryMode(TKMDeliveryMode((Ord(fNewDeliveryMode) + 3 - 1) mod 3)); // We use opposite order for legacy support

end;


procedure TKMHouse.SetPrevDeliveryMode;
begin
  SetNewDeliveryMode(TKMDeliveryMode((Ord(fNewDeliveryMode) + 1) mod 3)); // We use opposite order for legacy support
end;


// Set delivery mdoe immidiately
procedure TKMHouse.SetDeliveryModeInstantly(aValue: TKMDeliveryMode);
begin
  fNewDeliveryMode := aValue;
  UpdateDeliveryMode;
end;


function TKMHouse.AllowDeliveryModeChange: Boolean;
begin
  Result := gRes.Houses[fType].AcceptsWares;
end;


procedure TKMHouse.IssueResourceDepletedMsg;
var
  msgID: SmallInt;
begin
  if HouseType = htProductionThatch then
    Exit;

  msgID := GetResourceDepletedMessageId;
  Assert(msgID <> 0, gRes.Houses[HouseType].HouseName + ' resource can''t be depleted');

  if not gGameParams.MBD.IsRealism then
    if msgID <> -1 then
      if HouseType = htFishermans then
      begin
        if not gHands.PlayerAnimals.IsSpawnerInRadius(Entrance, 40) then
          ShowMsg(msgID);
        
      end else
        ShowMsg(msgID);
  ResourceDepleted := True;
end;


function TKMHouse.GetIsSelectable: Boolean;
begin
  Result := not IsDestroyed;
end;


function TKMHouse.GetResourceDepletedMessageId: SmallInt;
begin
  Result := 0;
  case HouseType of
    htProductionThatch: Result := -1;
    htCollectors:     Result := 1950;
    htQuarry:       Result := TX_MSG_STONE_DEPLETED;
    htCoalMine:     Result := TX_MSG_COAL_DEPLETED;
    htPottery:     Result := 1951;
    htBitinMine:     Result := 1949;
    htIronMine:     Result := TX_MSG_IRON_DEPLETED;
    htGoldMine:     Result := TX_MSG_GOLD_DEPLETED;
    htWoodcutters:  if TKMHouseWoodcutters(Self).WoodcutterMode = wmPlant then
                      Result := TX_MSG_WOODCUTTER_PLANT_DEPLETED
                    else
                      Result := TX_MSG_WOODCUTTER_DEPLETED;
    htFishermans:   if not gTerrain.CanFindFishingWater(PointBelowEntrance, gRes.Units[utFisher].MiningRange) then
                      Result := TX_MSG_FISHERMAN_TOO_FAR
                    else
                      Result := TX_MSG_FISHERMAN_CANNOT_CATCH;
  end;
end;

function TKMHouse.GetAcceptWareIn(aWareType : TKMWareType) : Word;
var I : Integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    case fWareInput[I] of
      wtNone: ;
      wtAll:       Result := 0;
      //wtWarfare:   Result := 0;
      else        If fWareInput[I] = aWareType then Result := fWareBlocked[I];
    end;
    {if aWareType = fWareInput[I] then
      Result := fWareBlocked[I];  }
end;

procedure TKMHouse.SetAcceptWareIn(aWareType: TKMWareType; aMax: Word);
var I : Integer;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWareType = fWareInput[I] then
    begin
      fWareBlocked[I] := aMax;
      UpdateDemands;
    end;

end;


//Check if we should abandon delivery to this house
function TKMHouse.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  if IsUpgrading then
    if (aWareType in [wtTimber, wtStone, wtTile]) then
      Exit(false);

  Result := (DeliveryMode <> dmDelivery)
            or ((DeliveryMode = dmDelivery) and (CheckWareIn(aWareType) >= GetMaxInWare - GetAcceptWareIn(aWareType)) ) ;

  if IsComplete then
    if IsUpgrading then
      if not (aWareType in [wtTimber, wtStone, wtTile]) then
        Result := true;
  //Result := Result or (IsUpgrading and (fBuildState = hbsDone));
end;


procedure TKMHouse.ShowMsg(aTextID: Integer);
begin
  if Assigned(fOnShowGameMessage) then
    fOnShowGameMessage(mkHouse, aTextID, Entrance, UID, Owner);
end;

function TKMHouse.GetWareInput: TKMWareType8;
var I : Integer;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
    Result[I] := fWareInput[I];
end;

function TKMHouse.GetWareOutput: TKMWareType8;
var I : Integer;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
    Result[I] := fWareOutput[I];
end;

function TKMHouse.HasOutput: Boolean;
var I : integer;
begin
  Result := false;
  for I := 1 to WARES_IN_OUT_COUNT do
    if not (fWareOutput[I] in [wtNone, wtAll, wtWarfare]) then
      Exit(true);
    
end;

function TKMHouse.GetWorkMultiplier : Single;
begin
  Result := 1;
  if CurrentLevel > 0 then
    Result := HSpec.Levels[CurrentLevel - 1].AnimMultiplier;
end;

function TKMHouse.AcceptsWares: Boolean;
var I : integer;
begin
  Result := false;
  for I := 1 to High(fWareInput) do
      if fWareInput[I] in [WARE_MIN..WARE_MAX, wtFood, wtWarfare, wtValuable] then
        Exit(true);

end;
function TKMHouse.ProducesWares: Boolean;
var I : integer;
begin
  Result := false;
  for I := 1 to High(fWareOutput) do
      if gRes.Wares[fWareOutput[I]].IsValid then
        Exit(true);

end;
function TKMHouse.HasSpaceForWareOut(aWare : TKMWareType) : Boolean;
var I : Integer;
begin
  Result := false;
  I := GetWareOutIndex(aWare);
  if I = 0 then
    Exit;

  Result := fWareOut[I] < GetMaxOutWare;
end;

function TKMHouse.HasSpaceForWaresOut(aWares : TKMWareTypeSet; aAnyWare : Boolean) : Boolean;
var WT : TKMWareType;
begin
  if aAnyWare then
    Result := false
  else
    Result := true;

  for WT in aWares do
  begin

    if aAnyWare then
    begin
      if HasSpaceForWareOut(WT) then
        Exit(true);

    end else
    if not HasSpaceForWareOut(WT) then
      Exit(false);


  end;
end;

function TKMHouse.CanChangeWareInput(IsAI : Boolean = false) : Boolean;
var I : Integer;
begin
  //if fType in [htStore, htTownhall, htInn, htPalace, htSchool, htBarracks, htCottage, htHouse, htMarket] then
  
  Result := true;
  If gGameParams.IsMapEditor then
    Exit(true);

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if not (fWareInput[I] in [wtNone, wtAll, wtFood, wtWarfare, wtValuable]) then
    begin
      if (fWareIn[I] > 0) then
        Exit(false);

      if not (((fWareBlocked[I] = GetMaxInWare) and not IsAI) or IsAI) then
        Exit(false);
    end;
    if not (fWareOutput[I] in [wtNone, wtAll, wtFood, wtWarfare, wtValuable]) then
      if not (fWareOut[I] = 0) then
        Exit(false);


  end;



end;

procedure TKMHouse.CheckWorkersForSlot;
begin
  //do something in child classes
end;

procedure TKMHouse.MakeWareSlot(aWaresIn: array of TKMWareType; aWaresOut: array of TKMWareType);
var I : Integer;
  K : Integer;
begin
  fResetDemands := true;
  UpdateDemands;
  fResetDemands := false;

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    fWareInput[I] := wtNone;
    fWareOutput[I] := wtNone;
    TransferWare[I] := gHands[Owner].IsComputer;
  end;

  K := Low(aWaresIn);
  for I := K to High(aWaresIn) do
    If I - K + 1 <= WARES_IN_OUT_COUNT then
      fWareInput[I - K + 1] := aWaresIn[I];

  K := Low(aWaresOut);
  for I := K to High(aWaresOut) do
    If I - K + 1 <= WARES_IN_OUT_COUNT then
      fWareOutput[I - K + 1] := aWaresOut[I];

  UpdateDemands;
end;


procedure TKMHouse.SetWareSlot(aSlotID : Byte; aForceChange : Boolean = false);
var I, K : integer;
  fOldWares : TKMWarePlan;
begin
  if IsUpgrading then
    Exit;

  if not aForceChange then
    if not CanChangeWareInput then
      Exit;

  if length(gRes.Houses[fType].WareInputSlots) = 0 then
    Exit;
  if aForceChange then
    WareSlotChangedByAI := true;

  //gHands[Owner].Deliveries.Queue.RemDemand(self);
  fResetDemands := true;
  UpdateDemands;
  fResetDemands := false;
  fSlotID := aSlotID;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if fWareInput[I] <> wtNone then
      fOldWares.AddWare(fWareInput[I], fWareIn[I]);
    if fWareOutput[I] <> wtNone then
      fOldWares.AddWare(fWareOutput[I], fWareOut[I]);

    fWareIn[I] := 0;
    fWareOut[I] := 0;
    if gHands[Owner].IsHuman then
      fWareBlocked[I] := GetMaxInWare
    else
      fWareBlocked[I] := 0;

    fWareOrder[I] := 0;
    fWareDeliveryCount[I] := 0;
    fWareDemandsClosing[I] := 0;
    fWareInput[I] := gRes.Houses[fType].WareInputSlots[fSlotID].WareInput[I];
    fWareOutput[I] := gRes.Houses[fType].WareInputSlots[fSlotID].WareOutput[I];
  end;

  for I := 0 to fOldWares.Count - 1 do
    if fOldWares[I].W <> wtNone then
    for K := 1 to WARES_IN_OUT_COUNT do
      if fWareInput[K] = fOldWares[I].W then
      begin
        fWareIn[K] := fWareIn[K] + fOldWares[I].C;
        fWareDeliveryCount[K] := fWareIn[K];
      end
      else
      if fWareOutput[K] = fOldWares[I].W then
        fWareOut[K] := fWareOut[K] + fOldWares[I].C;

  CheckWorkersForSlot;
  UpdateDemands;
end;

procedure TKMHouse.SetWareSlot(aWare : TKMWareType);
var I, J, Index : integer;
begin
  Index := -1;
  for I := 0 to High(gRes.Houses[fType].WareInputSlots) do
    for J := 1 to WARES_IN_OUT_COUNT do
      If gRes.Houses[fType].WareInputSlots[I].WareInput[J] = aWare then
      begin
        Index := I;
        Break;
        Break;
      end;

  if Index <> -1 then
    SetWareSlot(Index);

end;

function TKMHouse.HasSlotWithWare(aWare : TKMWareType) : Boolean;
var I, J : integer;
begin
  Result := false;

  for I := 0 to High(gRes.Houses[fType].WareInputSlots) do
    for J := 1 to WARES_IN_OUT_COUNT do
      If gRes.Houses[fType].WareInputSlots[I].WareInput[J] = aWare then
      begin
        Result := true;
        Break;
        Break;
      end;

end;
//Check if we should abandon delivery from this house
function TKMHouse.ShouldAbandonDeliveryFrom(aWareType: TKMWareType; aImmidiateCheck: Boolean = False): Boolean;
begin
  Result := not WareOutputAvailable(aWareType, 1);
end;


//Check if we should abandon delivery from this house to aToHouse (used in Store only for now)
function TKMHouse.ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean;
begin
  Result := False;
end;


{Returns the closest cell of the house to aPos}
function TKMHouse.GetClosestCell(const aPos: TKMPoint): TKMPoint;
var
  list: TKMPointList;
begin
  Result := KMPOINT_ZERO;
  list := TKMPointList.Create;
  try
    GetListOfCellsWithin(list);
    if not list.GetClosest(aPos, Result) then
      raise Exception.Create('Could not find closest house cell');
  finally
    list.Free;
  end;
end;


// Return distance from aPos to the closest house tile
function TKMHouse.GetDistance(const aPos: TKMPoint): Single;
var
  I: Integer;
  list : TKMPointList;
begin
  Result := MaxSingle;
  list := TKMPointList.Create;
  try
    GetListOfCellsWithin(list);
    for I := 0 to list.Count - 1 do
      Result := Min(Result, KMLength(aPos, list[I]));

  finally
    list.Free;
  end;


  //HA := gRes.Houses[fType].BuildArea;

  {for I := max(loc.Y - 3, 1) to loc.Y do
  for K := max(loc.X - 2, 1) to min(loc.X + 1, gTerrain.MapX) do
  if HA[I - loc.Y + 4, K - loc.X + 3] <> 0 then
    Result := Min(Result, KMLength(aPos, KMPoint(K, I)));}
end;


//Check if house is within reach of given Distance (optimized version for PathFinding)
//Check precise distance when we are close enough
function TKMHouse.InReach(const aPos: TKMPoint; aDistance: Single): Boolean;
begin
  //+6 is the worst case with the barracks, distance from fPosition to top left tile of house could be > 5
  if KMLengthDiag(aPos, fPosition) >= aDistance + 6 then
    Result := False //We are sure they are not close enough to the house
  else
    //We need to perform a precise check
    Result := GetDistance(aPos) <= aDistance;
end;


procedure TKMHouse.GetListOfCellsAround(aCells: TKMPointDirList; aPassability: TKMTerrainPassability);
var
  I, K: Integer;
  loc: TKMPoint;
  HA: TKMHouseAreaNew;

  procedure AddLoc(X,Y: Word; Dir: TKMDirection);
  begin
    //Check that the passabilty is correct, as the house may be placed against blocked terrain
    if (aPassability = tpNone) or gTerrain.CheckPassability(KMPoint(X,Y), aPassability) then
      aCells.Add(KMPointDir(X, Y, Dir));
  end;

begin
  aCells.Clear;
  loc := fPosition;
  HA := gRes.Houses[fType].BuildArea;

  for I := 1 to MAX_HOUSE_SIZE do for K := 1 to MAX_HOUSE_SIZE do
  if HA[I,K] <> 0 then
  begin
    if (I = 1) or (HA[I-1,K] = 0) then
      AddLoc(loc.X + K - 3, loc.Y + I - 4 - 1, dirS); //Above
    if (I = MAX_HOUSE_SIZE) or (HA[I+1,K] = 0) then
      AddLoc(loc.X + K - 3, loc.Y + I - 4 + 1, dirN); //Below
    if (K = MAX_HOUSE_SIZE) or (HA[I,K+1] = 0) then
      AddLoc(loc.X + K - 3 + 1, loc.Y + I - 4, dirW); //FromRight
    if (K = 1) or (HA[I,K-1] = 0) then
      AddLoc(loc.X + K - 3 - 1, loc.Y + I - 4, dirE); //FromLeft
  end;
end;


procedure TKMHouse.GetListOfCellsWithin(aCells: TKMPointList);
var
  I, K: Integer;
  loc: TKMPoint;
  houseArea: TKMHouseAreaNew;
begin
  aCells.Clear;
  loc := fPosition;
  houseArea := gRes.Houses[fType].BuildArea;

  for I := Max(loc.Y - 3, 1) to loc.Y do
    for K := Max(loc.X - 2, 1) to Min(loc.X + 1, gTerrain.MapX) do
      if houseArea[I - loc.Y + 4, K - loc.X + 3] in [1, 2] then
        aCells.Add(KMPoint(K, I));
end;


procedure TKMHouse.GetListOfGroundVisibleCells(aCells: TKMPointTagList);
var
  I, K, ground: Integer;
  loc: TKMPoint;
  groundVisibleArea: TKMHouseAreaNew;
begin
  aCells.Clear;
  loc := fPosition;
  groundVisibleArea := gRes.Houses[fType].GroundVisibleArea;

  for I := Max(loc.Y - 3, 1) to loc.Y do
    for K := Max(loc.X - 2, 1) to Min(loc.X + 1, gTerrain.MapX) do
    begin
      ground := groundVisibleArea[I - loc.Y + 4, K - loc.X + 3];
      if ground <> 0 then
        aCells.Add(KMPoint(K, I), ground);
    end;
end;


function TKMHouse.GetRandomCellWithin: TKMPoint;
var
  cells: TKMPointList;
  success: Boolean;
begin
  cells := TKMPointList.Create;
  GetListOfCellsWithin(cells);
  success := cells.GetRandom(Result);
  Assert(success);
  cells.Free;
end;


function TKMHouse.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X-fPosition.X+3 in [1..MAX_HOUSE_SIZE]) and
            (Y-fPosition.Y+4 in [1..MAX_HOUSE_SIZE]) and
            (gRes.Houses[fType].BuildArea[Y-fPosition.Y+4, X-fPosition.X+3] <> 0) and
            (gRes.Houses[fType].BuildArea[Y-fPosition.Y+4, X-fPosition.X+3] <> 3)
            ;
end;


function TKMHouse.GetHasWorker: Boolean;
begin
  Result := length(fWorkers) > 0;
end;

function TKMHouse.HasWorkerInside : Boolean;
begin
  Result := GetHasWorker and (TKMUnit(fWorkers[0]).InHouse = self);
end;

function TKMHouse.WorkersCount : Word;
begin
  Result := length(fWorkers);
end;

function TKMHouse.WorkersCount(aType : TKMUnitType) : byte;
var I : Integer;
begin
  Result := 0;

  for I := 0 to High(fWorkers) do
    if (fWorkers[I] <> nil)
    and not TKMUnit(fWorkers[I]).IsDeadOrDying
    and (TKMUnit(fWorkers[I]).UnitType = aType) then
      Inc(Result);
end;

function TKMHouse.CurrentWorkersTypeCount: TKMUnitPlan;
  procedure AddUnit(aUnitType : TKMUnitType);
  var I, id : Integer;
  begin
    id := -1;
    for I := 0 to High(Result) do
      if Result[I].UnitType = aUnitType then
      begin
        id := I;
        Break;
      end;

    if id >= 0 then
      Inc(Result[I].Count)
    else
    begin
      id := length(Result);
      SetLength(Result, id + 1);
      Result[id].UnitType := aUnitType;
      Result[id].Count := 1;
    end;

  end;
var I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to high(fWorkers) do
    if (fWorkers[I] <> nil) and not TKMUnit(fWorkers[I]).IsDeadOrDying then
      AddUnit(TKMUnit(fWorkers[I]).UnitType);
end;

function TKMHouse.MaxWorkers : Byte;
begin
  Result := HSpec.MaxWorkersCount;
end;

function TKMHouse.MaxWorkers(aType: TKMUnitType): Byte;
var I : Integer;
begin
  Result := 0;
  for I := 0 to High(HSpec.Workers) do
    if HSpec.Workers[I].UnitType = aType then
      Exit(HSpec.Workers[I].Count);

end;

function TKMHouse.CanHasWorker(aType : TKMUnitType) : Boolean;
  function HasWorkerType(aWType : TKMUnitType) : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to High(fWorkers) do
      if (fWorkers[I] <> nil) and (TKMUnit(fWorkers[I]).UnitType = aWType) then
        Exit(true);
  end;
begin
    Result := (MaxWorkers(aType) > 0)
              and not (WorkersCount >= MaxWorkers)
              and not IsDestroyed
              and IsComplete
              and not IsClosedForWorker
              and (WorkersCount(aType) < MaxWorkers(aType));
end;

procedure TKMHouse.AssignWorker(aWorker: Pointer);
  procedure DeleteIndex(var aArr : TPointerArray; aIndex : Integer);
  var I : integer;
  begin
    for I := aIndex to High(aArr) - 1 do
      aArr[I] := aArr[I + 1];
    SetLength(aArr, high(aArr));
  end;
var J, I, K, L : integer;
  tmp, tmp2 : TPointerArray;
begin
  if Self = nil then Exit;

  //we are adding new worker so don't clean up pointer here, make it in RemoveWorker
  //gHands.CleanUpUnitPointer( TKMUnit(fWorker) );

  if aWorker = nil then
    Exit;
  J := Length(fWorkers);
  SetLength(fWorkers, J + 1);
  fWorkers[J] := TKMUnit(aWorker).GetPointer;

  If length(HSpec.Workers) = 0 then
    Exit;
  tmp2 := fWorkers;
  SetLength(tmp, length(fWorkers));
  L := 0;
  //sort workers
  for I := 0 to high(HSpec.Workers) do
  begin
    if length(tmp2) = 0 then
      Break;

    for K := High(tmp2) downto 0 do
      if TKMUnit(tmp2[K]).UnitType = HSpec.Workers[I].UnitType then
      begin
        tmp[L] := tmp2[K];
        DeleteIndex(tmp2, K);
        Inc(L);
      end;
  end;
  fWorkers := tmp;

  //if aWorker <> nil then
  //  fWorker := TKMUnit(aWorker).GetPointer();
end;

procedure TKMHouse.RemoveWorker(aWorker: Pointer);
var aIndex, I : Integer;
begin
  if self = nil then Exit;

  aIndex := -1;
  for I := 0 to High(fWorkers) do
    if aWorker = fWorkers[I] then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then Exit;
  
  //Assert(aIndex <> -1, 'no worker found with id:' + IntToStr(TKMUnit(aWorker).UID));

  gHands.CleanUpUnitPointer( TKMUnit(fWorkers[aIndex]) );
  for I := aIndex to High(fWorkers) - 1 do
    fWorkers[I] := fWorkers[I + 1];

  SetLength(fWorkers, high(fWorkers));
  //worker should be removed
end;

function TKMHouse.AcceptsWorker(aWorker: Pointer): Boolean;
var I : integer;
begin
  Result := true;
  for I := 0 to High(fNotAcceptWorkers) do
    if fNotAcceptWorkers[I] = aWorker then
      Exit(false);
end;

procedure TKMHouse.DoNotAcceptWorker(aIndex: Integer);
var J : Integer;
begin
  if not InRange(aIndex, 0, high(fWorkers)) then Exit;
  

  for J := 0 to High(fNotAcceptWorkers) do
    if fNotAcceptWorkers[J] = fWorkers[aIndex] then
      Exit;

  J := length(fNotAcceptWorkers);
  SetLength(fNotAcceptWorkers, J + 1);
  fNotAcceptWorkers[J] := fWorkers[aIndex];
end;

function TKMHouse.GetWorkersIcons : TKMWordArray;
var I, J : Integer;
begin
  Result := [];
  for I := 0 to High(fWorkers) do
    if fWorkers[I] <> nil then
      if AcceptsWorker(fWorkers[I]) then
      begin
        J := length(Result);
        SetLength(Result, J + 1);
        Result[J] := gRes.Units[TKMUnit(fWorkers[I]).UnitType].GUIIcon;
      end;
end;

function TKMHouse.GetWorker(aIndex : Integer) : Pointer;
begin
  Assert(aIndex < WorkersCount);
  Result := fWorkers[aIndex];
end;

function TKMHouse.GetHealth:word;
begin
  Result := max(fBuildingProgress - fDamage, 0);
end;


function TKMHouse.GetInstance: TKMHouse;
begin
  Result := Self;
end;


function TKMHouse.GetPositionForDisplayF: TKMPointF;
begin
  Result := Entrance.ToFloat;
end;


function TKMHouse.GetPositionF: TKMPointF;
begin
  Result := Entrance.ToFloat;
end;


function TKMHouse.GetBuildWoodDelivered: Byte;
begin
  case fBuildState of
    hbsStone,
    hbsDone,
    hbsWood: Result := fBuildWoodDelivered;
    else      Result := 0;
  end;
  if IsUpgrading then
    Result := fBuildWoodDelivered;
end;


function TKMHouse.GetBuildStoneDelivered: Byte;
begin

  case fBuildState of
    hbsDone:  Result := StoneCost;
    hbsWood:  Result := fBuildStoneDelivered;
    hbsStone: Result := fBuildStoneDelivered;
    else       Result := 0;
  end;

  if IsUpgrading then
    Result := fBuildStoneDelivered;
end;

function TKMHouse.GetBuildTileDelivered: Byte;
begin

  case fBuildState of
    hbsDone:  Result := TileCost;
    hbsWood:  Result := fBuildTileDelivered;
    hbsStone: Result := fBuildTileDelivered;
    else       Result := 0;
  end;

  if IsUpgrading then
    Result := fBuildTileDelivered;
end;


function TKMHouse.GetBuildResourceDelivered: Byte;
begin
  Result := GetBuildWoodDelivered + GetBuildStoneDelivered + fBuildTileDelivered;
end;


function TKMHouse.GetBuildResDeliveredPercent: Single;
begin
  Result := GetBuildResourceDelivered / (gRes.Houses[fType].TotalBuildSupply);
end;

procedure TKMHouse.IncBuildingProgress(aStep: Integer);
var progress : Single;
begin
  if gGameParams.Mode <> gmMapEd then
    Exit;

  progress := (BuildSupplyWood + BuildSupplyStone + BuildSupplyTile) / HSpec.TotalBuildSupply;
  fBuildingProgress := EnsureRange(fBuildingProgress - aStep, 0, Round(MaxHealth * progress));
end;

// Increase building progress of house. When it reaches some point Stoning replaces Wooding
// and then it's done and house should be finalized
// Keep track on stone/wood reserve here as well
procedure TKMHouse.IncBuildingProgress;
  procedure FinishHouse;
  begin
    fBuildState := hbsDone;
    //reset these values so we can upgrade a house later
    fBuildSupplyWood := 0;
    fBuildSupplyStone := 0;
    fBuildSupplyTile := 0;
    fBuildReserve := 0;

    fBuildTileDelivered := 0;
    fBuildWoodDelivered := 0;
    fBuildStoneDelivered := 0;

    If not (fType in WALL_HOUSES) then
      fPointBelowEntrance := KMPointBelow(fEntrance);
    gHands[Owner].Stats.HouseEnded(fType);
    Activate(True);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      gHands[Owner].Constructions.RepairList.AddHouse(Self);

    gScriptEvents.ProcHouseBuilt(Self); //At the end since it could destroy this house
    gTerrain.SetHouse(self, Owner, hsBuilt);

    IF HouseType in FEST_BEST_HOUSES then
      ProduceFestivalPoints(fptBuilding, 10)
    else
    IF HouseType in FEST_BETTER_HOUSES then
      ProduceFestivalPoints(fptBuilding, 6)
    else
    IF HouseType in FEST_GOOD_HOUSES then
      ProduceFestivalPoints(fptBuilding, 3)
    else
      ProduceFestivalPoints(fptBuilding, 1);
  end;
begin
  if IsComplete then Exit;

  if (fBuildState = hbsWood) and (fBuildReserve <= 0) and (fBuildSupplyWood > 0) then
  begin
    Dec(fBuildSupplyWood);
    Inc(fBuildReserve, HealthSupplyStep + 5);
  end;

  if (fBuildState = hbsStone) and (fBuildReserve <= 0) and ((fBuildSupplyStone > 0) or (fBuildSupplyTile > 0)) then
  begin
    if (fBuildSupplyStone = 0) and (fBuildStoneDelivered = StoneCost) then
    begin

      if TileCost > 0 then
        if fBuildSupplyTile > 0 then
        begin
          Dec(fBuildSupplyTile);
          Inc(fBuildReserve, HealthSupplyStep + 5);
        end;

    end else
    if fBuildSupplyStone > 0 then
    begin
      Dec(fBuildSupplyStone);
      Inc(fBuildReserve, HealthSupplyStep + 5);
    end;
  end;

  Inc(fBuildingProgress, 5); //is how many effort was put into building nevermind applied damage

  //Dec(fBuildReserve, 5); //This is reserve we build from
  fBuildReserve := Max(fBuildReserve - 5, 0);

  if (fBuildState = hbsWood) then
    if (fBuildingProgress >= gRes.Houses[fType].MaxHealth) then
      FinishHouse
    else
    If (fBuildingProgress >= gRes.Houses[fType].MaxWoodHealth) then
      fBuildState := hbsStone;

  if (fBuildState = hbsStone)
    and (fBuildingProgress >= gRes.Houses[fType].MaxHealth) then
      FinishHouse;
end;

function TKMHouse.CheckMaxLevel : Boolean;
begin
  Result := (fLevel.CurrentLevel >= length(gRes.Houses[fType].Levels))
          or ((gHands[Owner].Locks.HouseMaxLvl[fType] <> 0) and (fLevel.CurrentLevel >= gHands[Owner].Locks.HouseMaxLvl[fType] - 1));
end;

function TKMHouse.GetHouseSpec : TKMHouseSpec;
begin
  Result := gRes.Houses[fType];
end;

function TKMHouse.GetWoodPic : Integer;
begin
  Result := gRes.Houses[fType].WoodPic;
end;

procedure TKMHouse.SetWariant(aValue: Integer);
begin
  fWariant := EnsureRange(aValue, -1, gRes.Houses[fType].HasWariants - 1);
end;

function TKMHouse.GetStonePic : Integer;
begin
  Result := gRes.Houses[fType].StonePic;


  if (fStyle > 0)  then
    Result := gRes.Houses[fType].Styles[fStyle - 1].StonePic
  else
  if fWariant <> -1 then
    Result := gRes.Houses[fType].GetStoneWariant(fWariant)
  else
  if fLevel.CurrentLevel > 0 then
    Result := gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].StonePic;

end;

function TKMHouse.GetSnowPic : Integer;
begin
  Result := gRes.Houses[fType].TerrPic[fIsOnTerrain];

  If fIsOnTerrain <> tptNone then
    if (fStyle > 0) and (gRes.Houses[fType].Styles[fStyle - 1].SnowPic[fIsOnTerrain] > 0)  then
      Result := gRes.Houses[fType].Styles[fStyle - 1].SnowPic[fIsOnTerrain]
    else
    if (fLevel.CurrentLevel > 0) and (gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].SnowPic[fIsOnTerrain] > 0) then
      Result := gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].SnowPic[fIsOnTerrain];

  if Result < 1000 then
    Result := -1;
end;


procedure TKMHouse.FinishedLevel(aLevel: Byte);
begin
  //child only
end;

// Increase building progress of house. When it reaches some point Stoning replaces Wooding
// and then it's done and house should be finalized
// Keep track on stone/wood reserve here as well

procedure TKMHouse.IncBuildingUpgradeProgress;
  procedure FinishUpgrade;
  var I : Integer;
    isMax : array[1..WARES_IN_OUT_COUNT] of Boolean;
  begin
    if not fLevel.IsUpgrading then
      Exit;
    //reset these values so we can upgrade a house later
    fBuildSupplyWood := 0;
    fBuildSupplyStone := 0;
    fBuildSupplyTile := 0;
    fBuildReserve := 0;

    fBuildTileDelivered := 0;
    fBuildWoodDelivered := 0;
    fBuildStoneDelivered := 0;
    Inc(fLevel.Progress, HSpec.Levels[CurrentLevel].Progress);

    for I := 1 to WARES_IN_OUT_COUNT do
      isMax[I] := false;

    for I := 1 to WARES_IN_OUT_COUNT do
      If fWareBlocked[I] = 0 then
        isMax[I] := true;

    Inc(fLevel.CurrentLevel);
    fLevel.IsUpgrading := false;
    //Refresh it just in case
    If not (fType in WALL_HOUSES) then
      fPointBelowEntrance := KMPointBelow(fEntrance);

    for I := 1 to WARES_IN_OUT_COUNT do
      if isMax[I] then
        fWareBlocked[I] := 0
      else
        fWareBlocked[I] := GetMaxInWare - fWareIn[I];

    FinishedLevel(fLevel.CurrentLevel);//finish before adding new demands

    fResetDemands := false;
    UpdateDemands;


    if gHands[Owner].IsComputer then
      if CanMakeUpgrade then
        MakeUpgrade;
    //gHands[Owner].Stats.HouseEnded(fType);
    //Activate(True);
    //House was damaged while under construction, so set the repair mode now it is complete
    //if (fDamage > 0) and BuildingRepair then
    //  gHands[Owner].Constructions.RepairList.AddHouse(Self);

    ProduceFestivalPoints(fptBuilding, 3);

    gScriptEvents.ProcHouseUpgraded(Self, fLevel.CurrentLevel); //At the end since it could destroy this house
  end;
begin
  If not fLevel.IsUpgrading then
    Exit;
  if fBuildReserve <= 10 then
  begin
    if fBuildSupplyWood > 0 then
    begin
      Inc(fBuildReserve, gRes.Houses[fType].Levels[fLevel.CurrentLevel].BuildingStep);
      Dec(fBuildSupplyWood)
    end
    else
    if fBuildSupplyStone > 0 then
    begin
      Inc(fBuildReserve, gRes.Houses[fType].Levels[fLevel.CurrentLevel].BuildingStep);
      Dec(fBuildSupplyStone)
    end
    else
    if fBuildSupplyTile > 0 then
    begin
      Inc(fBuildReserve, gRes.Houses[fType].Levels[fLevel.CurrentLevel].BuildingStep);
      Dec(fBuildSupplyTile);
    end;
  end;


  Inc(fBuildingProgress, 5); //is how many effort was put into building nevermind applied damage

  //Dec(fBuildReserve, 5); //This is reserve we build from
  fBuildReserve := Max(fBuildReserve - 5, 0);
  fLevel.BuildingProgress := Max(fLevel.BuildingProgress - 5, 0);

  if (fBuildingProgress >= MaxHealth) then
    FinishUpgrade;
end;


function TKMHouse.MaxHealth: Word;
begin
  if fBuildState = hbsNoGlyph then
    Result := 0
  else
  if fLevel.IsUpgrading then
  begin
    Result := gRes.Houses[fType].MaxHealth + HSpec.Levels[CurrentLevel].Progress;
  end
  else
    Result := gRes.Houses[fType].MaxHealth;

  Result := Result + fLevel.Progress;
end;


procedure TKMHouse.OwnerUpdate(aOwner: TKMHandID; aMoveToNewOwner: Boolean = False);
begin
  if aMoveToNewOwner and (Owner <> aOwner) then
  begin
    Assert(gGameParams.Mode = gmMapEd); // Allow to move existing House directly only in MapEd
    gHands[Owner].Houses.DeleteHouseFromList(Self);
    gHands[aOwner].Houses.AddHouseToList(Self);
  end;
  Owner := aOwner;
end;

procedure TKMHouse.OwnerUpdateByScript(aOwner: TKMHandID);
begin
  gHands[Owner].Houses.DeleteHouseFromList(Self);

  gHands[aOwner].Houses.AddHouseToList(Self);
  Owner := aOwner;
  UpdateDemands;
end;

//Add damage to the house, positive number
procedure TKMHouse.AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False; aFromParent : Boolean = false);
var
  attackerHand: TKMHandID;
begin
  if IsDestroyed then
    Exit;
  if Indestructible then
    Exit;

  //if (fType in WALL_HOUSES) or (fType = htWallTower) then
  //  aAmount := aAmount * 3;
  //(NoGlyph houses MaxHealth = 0, they get destroyed instantly)
  If gHands[Owner].HasPearl(ptRalender) then
    aAmount := Max(aAmount div 2, 1);

  fDamage := Math.min(fDamage + aAmount, MaxHealth);
  if IsComplete then
  begin
    if BuildingRepair then
      gHands[Owner].Constructions.RepairList.AddHouse(Self);
    If not (fType in WALL_HOUSES) then
    fPointBelowEntrance := KMPointBelow(fEntrance);

    //Update fire if the house is complete
    UpdateDamage;
  end;

  if gGameParams.Mode <> gmMapEd then
  begin
    //Let AI and script know when the damage is already applied, so they see actual state
    gHands[Owner].AI.HouseAttackNotification(Self, TKMUnitWarrior(aAttacker));
    if fIsDestroyed then Exit; //Script event might destroy this house

    if aAttacker <> nil then
      attackerHand := TKMUnitWarrior(aAttacker).Owner
    else
      attackerHand := HAND_NONE;

    //Properly release house assets
    //Do not remove house in Editor just yet, mapmaker might increase the hp again
    if (GetHealth = 0) and not aIsEditor then
      Demolish(attackerHand);
  end;
end;

procedure TKMHouse.SetDamage(aValue : Word);
begin
  fDamage := Math.EnsureRange(aValue, 0, MaxHealth);
  if IsComplete then
    UpdateDamage;
end;

//Add repair to the house
procedure TKMHouse.AddRepair(aAmount: Word = 5);
var
  oldDmg: Integer;
begin
  oldDmg := fDamage;
  fDamage := EnsureRange(fDamage - aAmount, 0, High(Word));
  //builders repaired everything
  if fIsBurning > 0 then
    if fDamage = 0 then
      fIsBurning := 0;

  UpdateDamage;

  if gGameParams.Mode <> gmMapEd then
    gScriptEvents.ProcHouseRepaired(Self, oldDmg - fDamage, fDamage);
end;


procedure TKMHouse.SetCost;
begin
  fBuildCost.Wood := gHands[Owner].GetHouseWoodCost(fType);
  fBuildCost.Stone := gHands[Owner].GetHouseStoneCost(fType);
  fBuildCost.Tile := gHands[Owner].GetHouseTileCost(fType);
end;


function TKMHouse.WoodCost : Byte;
begin
  Result := fBuildCost.Wood;
end;

function TKMHouse.StoneCost : Byte;
begin
  Result := fBuildCost.Stone;
end;

function TKMHouse.TileCost : Byte;
begin
  Result := fBuildCost.Tile;
end;


function TKMHouse.TotalCost : Byte;
begin
  Result := WoodCost + StoneCost + TileCost;
end;

function TKMHouse.HealthSupplyStep: Byte;
begin
  Result := HSpec.MaxHealth div TotalCost;
end;

procedure TKMHouse.AddDemandBuildingMaterials;
begin
  gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTimber, WoodCost, dtOnce, diHigh5);
  gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtStone, StoneCost, dtOnce, diHigh5);
  gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTile, TileCost, dtOnce, diHigh5);
end;

procedure TKMHouse.SetBuildingProgress(aProgress: Word; aWood: Word; aStone: Word; aTile: Word);
var I, newAmount, resNeeded, plannedToRemove : Integer;
  progress : Single;
begin
  if gGameParams.Mode = gmMapEd then
  begin
    fBuildSupplyWood := EnsureRange(aWood, 0, HSpec.WoodCost);
    fBuildSupplyStone := EnsureRange(aStone, 0, HSpec.StoneCost);
    fBuildSupplyTile := EnsureRange(aTile, 0, HSpec.TileCost);
    if fBuildSupplyWood < HSpec.WoodCost then //not enough wood, so max progress is BuildSuppyWood / HSpec.TotalBuildSupply;
      progress := BuildSupplyWood / HSpec.TotalBuildSupply
    else
      progress := (BuildSupplyWood + BuildSupplyStone + BuildSupplyTile) / HSpec.TotalBuildSupply;
    fBuildingProgress := EnsureRange(aProgress, 0, Round(progress * HSpec.MaxHealth)) div 5 * 5;
  end else
  begin

    newAmount := EnsureRange(aWood, -BuildSupplyWood, WoodCost - GetBuildWoodDelivered);
    if newAmount > 0 then
    begin
      resNeeded := gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtTimber, newAmount, plannedToRemove);
      Inc(resNeeded, plannedToRemove);
      WareAddToBuild(wtTimber, resNeeded);
    end
    else
    begin
      WareAddToBuild(wtTimber, newAmount);
      gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTimber, -newAmount, dtOnce, diHigh4);
    end;

    newAmount := EnsureRange(aStone, -BuildSupplyStone, StoneCost - GetBuildStoneDelivered);
    if newAmount > 0 then
    begin
      resNeeded := gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtStone, newAmount, plannedToRemove);
      Inc(resNeeded, plannedToRemove);
      WareAddToBuild(wtStone, resNeeded);
    end
    else
    begin
      WareAddToBuild(wtStone, newAmount);
      gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtStone, -newAmount, dtOnce, diHigh4);
    end;

    newAmount := EnsureRange(aTile, -BuildSupplyTile, TileCost - GetBuildTileDelivered);
    if newAmount > 0 then
    begin
      resNeeded := gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtTile, newAmount, plannedToRemove);
      Inc(resNeeded, plannedToRemove);
      WareAddToBuild(wtTile, resNeeded);
    end
    else
    begin
      WareAddToBuild(wtTile, newAmount);
      gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTile, -newAmount, dtOnce, diHigh4);
    end;

    for I := 1 to aProgress div 5 do
    begin
      if IsComplete or not CheckResToBuild then
        Break;
      IncBuildingProgress;
    end;
    

  end;


end;

//Update house damage animation
procedure TKMHouse.UpdateDamage;
var
  dmgLevel: Word;
begin
  if not IsComplete then
    Exit;
  dmgLevel := MaxHealth div 8; //There are 8 fire places for each house, so the increment for each fire level is Max_Health / 8

  gTerrain.AddLight(Entrance, 5 + Round((fDamage / MaxHealth) * 3) , 100 + 5 * Round((fDamage / MaxHealth) * 20) );//update lights, fire gives extra light

  CurrentAction.SubActionRem([haFire1, haFire2, haFire3, haFire4, haFire5, haFire6, haFire7, haFire8]);
  if fDamage > 0 * dmgLevel then CurrentAction.SubActionAdd([haFire1]);
  if fDamage > 1 * dmgLevel then CurrentAction.SubActionAdd([haFire2]);
  if fDamage > 2 * dmgLevel then CurrentAction.SubActionAdd([haFire3]);
  if fDamage > 3 * dmgLevel then CurrentAction.SubActionAdd([haFire4]);
  if fDamage > 4 * dmgLevel then CurrentAction.SubActionAdd([haFire5]);
  if fDamage > 5 * dmgLevel then CurrentAction.SubActionAdd([haFire6]);
  if fDamage > 6 * dmgLevel then CurrentAction.SubActionAdd([haFire7]);
  if fDamage > 7 * dmgLevel then CurrentAction.SubActionAdd([haFire8]);
  //House gets destroyed in UpdateState loop
end;

procedure TKMHouse.SetOnFire;
begin
  fIsBurning := EnsureRange(fIsBurning + 1, 0, 20);
  If (HouseType in WALL_HOUSES) and gHands[Owner].BuildDevUnlocked(21) then
    fIsBurning := 0;

  If (HouseType in WALL_HOUSES) and gHands[Owner].ArmyDevUnlocked(17) then
    fIsBurning := EnsureRange(fIsBurning + 1, 0, 20);
end;

procedure TKMHouse.TakeOver;
begin
  fWasTookOver := true;
end;

function TKMHouse.GetStats(aWares : Boolean): TKMHouseStats;
var I : Integer;
begin
  Result.HouseType := HouseType;
  Result.X := Entrance.X;
  Result.Y := Entrance.Y;
  Result.Owner := Owner;
  Result.FlagX := IfThen(self is TKMHouseWFlagPoint, TKMHouseWFlagPoint(self).FlagPoint.X, 0);
  Result.FlagY := IfThen(self is TKMHouseWFlagPoint, TKMHouseWFlagPoint(self).FlagPoint.Y, 0);
  Result.IsDestroyed := IsDestroyed;
  Result.IsComplete := IsComplete;
  Result.RepairOn := BuildingRepair;
  Result.Damage := fDamage;
  Result.MaxHealth := MaxHealth;
  Result.Level := CurrentLevel;
  Result.WareSlot := WareInputSlot;
  Result.Wares.SetCount(0, true);
  If aWares then
  begin
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      Result.Wares.AddWare(fWareInput[I], fWareIn[I]);
      Result.Wares.AddWare(fWareOutput[I], fWareOut[I]);
    end;
  end;

end;

procedure TKMHouse.SetStats(aStats: TKMHouseStats);
var I : Integer;
begin
  If self is TKMHouseWFlagPoint then
    TKMHouseWFlagPoint(self).FlagPoint := KMPoint(aStats.FlagX, aStats.FlagY);
  BuildingRepair := aStats.RepairOn;
  fDamage := aStats.Damage;
  If aStats.Level <> CurrentLevel then
    SetLevel(aStats.Level);

  SetWareSlot(aStats.WareSlot, true);

  for I := 0 to High(aStats.Wares) do
    self.WareAddToEitherFromScript(aStats.Wares[I].W, aStats.Wares[I].C);

end;

function TKMHouse.GetWaresArrayIn: TIntegerArray;
var I: integer;
begin
  Setlength(Result, WARE_CNT);
  for I := 1 to WARES_IN_OUT_COUNT do
    If fWareInput[I] in WARES_VALID then
    begin
      Result[ WARE_TY_TO_ID[fWareInput[I]] ] := fWareIn[I];
    end;
end;

function TKMHouse.GetWaresArrayOut: TIntegerArray;
var I: integer;
begin
  Setlength(Result, WARE_CNT);
  for I := 1 to WARES_IN_OUT_COUNT do
    If fWareOutput[I] in WARES_VALID then
    begin
      Result[ WARE_TY_TO_ID[fWareOutput[I]] ] := fWareOut[I];
    end;
end;

function TKMHouse.GetWaresArrayTotal: TIntegerArray;
var I: integer;
begin
  Setlength(Result, WARE_CNT);
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    If fWareInput[I] in WARES_VALID then
      Inc(Result[ WARE_TY_TO_ID[fWareInput[I]] ], fWareIn[I]);
    If fWareOutput[I] in WARES_VALID then
      Inc(Result[ WARE_TY_TO_ID[fWareOutput[I]] ], fWareOut[I]);
  end;
end;

procedure TKMHouse.SetBuildingRepair(aValue: Boolean);
begin
  fBuildingRepair := aValue;

  if fBuildingRepair then
  begin
    if IsComplete and IsDamaged and not IsDestroyed then
      gHands[Owner].Constructions.RepairList.AddHouse(Self);
  end
  else
    //Worker checks on house and will cancel the walk if Repair is turned off
    //RepairList removes the house automatically too
end;


procedure TKMHouse.SetIsClosedForWorker(aIsClosed: Boolean);
begin
  if fIsClosedForWorker = aIsClosed then Exit; // Nothing to do. Do not count house closed for worker in stats again and again

  fIsClosedForWorker := aIsClosed;

  if not gGameParams.IsMapEditor then
    gHands[Owner].Stats.HouseClosed(aIsClosed, fType);
  SetLength(fNotAcceptWorkers, 0);//clear it here, cause we are closing whole house, so player might want to get workers inside again.
end;

function TKMHouse.GetClosedForWorker: Boolean;
begin
  if not IsValid then
    Exit(true);

  Result := fIsClosedForWorker or ((fDamage / MaxHealth) > 0.8);
end;

function TKMHouse.GetWasClosedByHand: Boolean;
begin
  if not IsValid then
    Exit(true);
  Result := fIsClosedForWorker;
end;

//deprecated
{function TKMHouse.CanHasWorker: Boolean;
begin
  if Self = nil then Exit(False);
  
  Result := gRes.Houses[fType].CanHasWorker;
end;
}

function TKMHouse.IsStone: Boolean;
begin
  Result := fBuildState in [hbsStone, hbsDone];
end;


{Check if house is completely built, nevermind the damage}
function TKMHouse.IsComplete: Boolean;
begin
  Result := fBuildState = hbsDone;
end;


{Check if house is damaged}
function TKMHouse.IsDamaged: Boolean;
begin
  Result := fDamage <> 0;
end;


procedure TKMHouse.SetState(aState: TKMHouseState; doTowerAnim : Boolean = true);
begin
  CurrentAction.State := aState;
end;


procedure TKMHouse.SetWorker(aWorker: Pointer);
begin
  if Self = nil then Exit;

  gHands.CleanUpUnitPointer( TKMUnit(fWorker) );

  if aWorker <> nil then
    fWorker := TKMUnit(aWorker).GetPointer();
end;


function TKMHouse.GetState: TKMHouseState;
begin
  Result := CurrentAction.State;
end;


function TKMHouse.GetWareInArray: TKMByteArray;
var
  I, iOffset: Integer;
begin
  SetLength(Result, Length(fWareIn));
  iOffset := Low(fWareIn) - Low(Result);
  for I := Low(Result) to High(Result) do
    Result[I] := fWareIn[I + iOffset];
end;


function TKMHouse.GetWareOutArray: TKMByteArray;
var
  I, iOffset: Integer;
begin
  Exit;
  SetLength(Result, Length(fWareOut));
  iOffset := Low(fWareOut) - Low(Result);
  for I := Low(Result) to High(Result) do
    Result[I] := fWareOut[I + iOffset];
end;


function TKMHouse.GetWareOutPoolArray: TKMByteArray;
var
  I: Integer;
begin
  SetLength(Result, Length(fWareOutPool));
  for I := Low(Result) to High(Result) do
    Result[I] := fWareOutPool[I];
end;


// Check if house is placed mostly on snow
procedure TKMHouse.CheckOnTerrain;
var
  I: Integer;
  terrTiles : array[TKMTerrPicType] of Word;
  cells: TKMPointTagList;
  tpt : TKMTerrPicType;
begin
  cells := TKMPointTagList.Create;

  GetListOfGroundVisibleCells(cells);

  for tpt := Low(TKMTerrPicType) to High(TKMTerrPicType) do
    terrTiles[tpt] := 0;

  for I := 0 to cells.Count - 1 do
    Inc(terrTiles[gTerrain.TileHouseTerrain(cells[I].X, cells[I].Y)]);
    {if gTerrain.TileIsSnow(cells[I].X, cells[I].Y) then
      Inc(terrTiles, cells.Tag[I])
    else
      Inc(noTerrTiles, cells.Tag[I]);}
  fIsOnTerrain := tptNone;
  for tpt := tptSnow to High(TKMTerrPicType) do
    If terrTiles[tpt] >  terrTiles[tptNone] then
      fIsOnTerrain := tpt;

  cells.Free;
end;

function TKMHouse.WareIsInAndOut(aWare: TKMWareType): Boolean;
begin
  Result := WareCanAddToOut(aWare) and WareCanAddToIn(aWare);
end;
// How much resources house has in Input
function TKMHouse.CheckWareIn(aWare: TKMWareType): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if (aWare = fWareInput[I]) or (aWare = wtAll) then
      Inc(Result, ResIn[I]);
end;


// How much resources house has in Output
function TKMHouse.CheckWareOut(aWare: TKMWareType): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if (aWare = fWareOutput[I]) or (aWare = wtAll) then
      Inc(Result, fWareOut[I]);
end;

// How much resources house has in Output
function TKMHouse.CheckWareTotal(aWare: TKMWareType): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if (aWare = fWareOutput[I]) or (aWare = wtAll) then
      Inc(Result, fWareOut[I]);
    if (aWare = fWareInput[I]) or (aWare = wtAll) then
      Inc(Result, fWareIn[I]);
  end;
end;

function TKMHouse.HasWaresIn(aWares: TKMWarePlan): Boolean;
var I : integer;
begin
  Result := true;
  for I := 0 to High(aWares) do
    If CheckWareIn(aWares[I].W) < aWares[I].C then
      Exit(false);
end;

procedure TKMHouse.WaresTakeFromIn(aWares: TKMWarePlan; aFromScript: Boolean = False);
var I : integer;
begin
  for I := 0 to High(aWares) do
    WareTakeFromIn(aWares[I].W, aWares[I].C, aFromScript);
end;

// Check amount of placed order for given ID
function TKMHouse.GetWareOrder(aID: Byte): Integer;
begin
  Result := fWareOrder[aID];
end;


//Input value is integer because we might get a -100 order from outside and need to fit it to range
//properly
procedure TKMHouse.SetWareOrder(aID: Byte; aValue: Integer);
//var
//  I: Integer;
//  TotalDesired: Integer;
begin
  fWareOrder[aID] := EnsureRange(aValue, 0, MAX_WARES_ORDER);

  //Calculate desired production ratio (so that we are not affected by fWareOrder which decreases till 0)
//  TotalDesired := fWareOrder[1] + fWareOrder[2] + fWareOrder[3] + fWareOrder[4];
//  for I := 1 to 4 do
//    fWareOrderDesired[I] := fWareOrder[I] / TotalDesired;

  fNeedIssueOrderCompletedMsg := False;
  fOrderCompletedMsgIssued := False;
end;


//Select order we will be making
//Order picking in sequential, so that if orders for 1st = 6 and for 2nd = 2
//then the production will go like so: 12121111
function TKMHouse.PickOrder(aUnitType : TKMUnitType): Byte;
var
  I, K, resI: Integer;
  ware: TKMWareType;
  Warr : TKMWareTypeArray;
  canPickOrder : Boolean;
  NeededWare : array[WARE_MIN..WARE_MAX] of Byte;
begin
  Result := 0;

  if WARFARE_ORDER_SEQUENTIAL then
    for I := 0 to WARES_IN_OUT_COUNT - 1 do
    begin
      resI := ((fLastOrderProduced + I) mod WARES_IN_OUT_COUNT) + 1; //1..4
      ware := fWareOutput[resI];
      if not ((wtAll in gRes.Units[aUnitType].ProducesWares) or (ware in gRes.Units[aUnitType].ProducesWares)) then
        Continue;
      Warr := gRes.Wares[ware].OrderCost;
      canPickOrder := true;

      IF (ware = wtMace) and gHands[Owner].EconomyDevUnlocked(20) then
        SetLength(Warr, high(Warr));
      IF (ware = wtPlateArmor) and gHands[Owner].EconomyDevUnlocked(21) then
        SetLength(Warr, high(Warr));

      for K := 0 to high(Warr) do
        if gRes.Wares[Warr[K]].IsValid then
          NeededWare[Warr[K]] := 0;

      //Count first how much the order needs wares
      for K := 0 to high(Warr) do
        if gRes.Wares[Warr[K]].IsValid then
          Inc(NeededWare[Warr[K]]);


      canPickOrder := canPickOrder and (WareOrder[resI] > 0);
      if fType <> htProductionThatch then
        canPickOrder := canPickOrder and (CheckWareOut(ware) < GetMaxOutWare);

      for K := 0 to high(Warr) do
        if gRes.Wares[Warr[K]].IsValid then
          canPickOrder := canPickOrder and (CheckWareIn(Warr[K]) >= NeededWare[Warr[K]]);


      if canPickOrder then
      begin
        Result := resI;
        fLastOrderProduced := resI;
        Break;
      end;
    end;

  if Result <> 0 then
  begin
    //Dec(fWareOrder[Result]);//decrease only if WorkPlan is valid
    fNeedIssueOrderCompletedMsg := HouseType <> htProductionThatch;
    fOrderCompletedMsgIssued := False;
  end
  else
    //Check all orders are actually finished (input resources might be empty)
    if  (WareOrder[1] = 0) and (WareOrder[2] = 0)
    and (WareOrder[3] = 0) and (WareOrder[4] = 0)
    and (WareOrder[5] = 0) and (WareOrder[6] = 0)
    then
      if fNeedIssueOrderCompletedMsg then
      begin
        fNeedIssueOrderCompletedMsg := False;
        fOrderCompletedMsgIssued := True;
        ShowMsg(TX_MSG_ORDER_COMPLETED);
      end;
end;

//shortcut to ResHouses to get how many resources the house is producing
//probably will be changed so everyone can change it
function TKMHouse.GetWareProdCt(aWare: TKMWareType) : Byte;
begin
  Result := gRes.Wares[aWare].GetProductionCount(fType);
end;

function TKMHouse.GetProductionCycle(aIndex : Byte) : Word;
begin
  Assert(InRange(aIndex, 1, WARES_IN_OUT_COUNT));
  Result := fProductionCycles[aIndex];
end;

function TKMHouse.PaintHouseWork: Boolean;
begin
  Result := true;
end;

function TKMHouse.GetFlagColor : Cardinal;
begin
  if (FlagColor and $00FFFFFF) > 0 then
    Result := FlagColor
  else
    Result := gHands[Owner].GameFlagColor;
end;

function TKMHouse.GetWareInIndex(aWare: TKMWareType): Byte;
var I : integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if fWareInput[I] = aWare then
      Exit(I);
end;

function TKMHouse.GetWareOutIndex(aWare: TKMWareType): Byte;
var I : integer;
begin
  Result := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if fWareOutput[I] = aWare then
      Exit(I);
end;

function TKMHouse.IsValid(aHouseType : TKMHouseType = htAny; aDifferent : Boolean = false; aBuilt : Boolean = false): Boolean;
begin
  if Self = nil then
    Exit(false);
  Result := not self.IsDestroyed;
  if aBuilt then
    Result := Result and IsComplete;
  if aDifferent then
    Result := Result and ((HouseType <> aHouseType) or (aHouseType = htAny))
  else
    Result := Result and ((HouseType = aHouseType) or (aHouseType = htAny));
end;

function TKMHouse.IsValid(aHouseType: TKMHouseTypeSet; aBuiltOnly: Boolean = true): Boolean;
begin
  Result := (self <> nil)
            and not self.IsDestroyed
            and (not aBuiltOnly or IsComplete)
            and ((fType in aHouseType) or (htAny in aHouseType));
end;

function TKMHouse.PlaceRoad: Boolean;
begin
  Result := not (HouseType in NO_ROAD_CONNECTION_HOUSES);
end;

function TKMHouse.CanDeliverToAnyPoint(aWare : TKMWareType) : Boolean;
begin
  Result := HouseType in NO_ROAD_CONNECTION_HOUSES;
end;

function TKMHouse.MiningRange(aUnitType : TKMUnitType) : Integer;
begin
  Result := gRes.Units[aUnitType].MiningRange;
end;

function TKMHouse.MiningRange(aWare : TKMWareType) : Integer;
begin
  Result := 5;
end;

function TKMHouse.MiningRect(aUnitType : TKMUnitType) : TKMRect;
begin
  Result := KMRect(gRes.Units[aUnitType].MiningRange, gRes.Units[aUnitType].MiningRange,
                    gRes.Units[aUnitType].MiningRange, gRes.Units[aUnitType].MiningRange);
end;

function TKMHouse.MiningRect(aWare : TKMWareType) : TKMRect;
begin
  Result := gTerrain.GetMiningRect(aWare);
  If (aWare = wtCoal) and (gHands[Owner].EconomyDevUnlocked(33)) then
  begin
    Inc(Result.Left, 1);
    Inc(Result.Top, 1);
    Inc(Result.Bottom, 1);
    Inc(Result.Right, 1);
  end;
end;


procedure TKMHouse.SetStyle(aValue : Byte);
begin
  fStyle := EnsureRange(aValue, 0, Length(HSpec.Styles));
end;

function TKMHouse.GetStyleGuiIcon : Word;
begin
  IF fStyle = 0 then
    Result := 389//default look
  else
    Result := HSpec.Styles[fStyle - 1].Icon;
end;

procedure TKMHouse.SetLevel(aValue : Byte);
var I : Integer;
begin

  fLevel.CurrentLevel := 0;
  fLevel.Progress := 0;
  fLevel.BuildingProgress := 0;
  fLevel.IsUpgrading := false;
  fBuildingProgress := HSpec.MaxHealth;

  fLevel.CurrentLevel := EnsureRange(aValue, 0, length(HSpec.Levels));

  for I := 1 to EnsureRange(aValue, 0, length(HSpec.Levels)) do
  begin
    Inc(fBuildingProgress, HSpec.Levels[I - 1].Progress);
    Inc(fLevel.Progress, HSpec.Levels[I - 1].Progress);
  end;


end;

procedure TKMHouse.SetHouseToDeliver(aValue: TKMHouse);
var I, K : Integer;
  hasWareConnection : Boolean;
begin
  If (aValue = nil) and (fHouseToDeliver <> nil) then
  begin
    fHouseToDeliver.RemHouseDeliveryFrom(self);
    gHands.CleanUpHousePointer(fHouseToDeliver);
    fHouseToDeliver := nil;
    Exit;
  end;
  If not aValue.IsValid then
    Exit;
  If aValue.Owner <> Owner then
    Exit;
  //to remove delivery, simply set it to self or the same house
  If fHouseToDeliver <> nil then
    If (aValue = self) or (aValue = fHouseToDeliver) then
    begin
      fHouseToDeliver.RemHouseDeliveryFrom(self);
      gHands.CleanUpHousePointer(fHouseToDeliver);
      //fHouseToDeliver := nil;
      Exit;
    end;
  hasWareConnection := false;
  //we must check if we can deliver any ware to another house
  for I := 1 to WARES_IN_OUT_COUNT do //check the output of this house
    for K := 1 to WARES_IN_OUT_COUNT do //check the input of another house
      IF (WareOutput[I] <> wtNone) and (aValue.WareInput[K] <> wtNone) then
        If ValidWareTypePair(WareOutput[I], aValue.WareInput[K]) or ValidWareTypePair(aValue.WareInput[K], WareOutput[I]) then
        begin
          hasWareConnection := true;
          Break;
          Break;
        end;

  If not hasWareConnection then
    Exit;

  If aValue <> fHouseToDeliver then
  begin
    if fHouseToDeliver <> nil then
      fHouseToDeliver.RemHouseDeliveryFrom(self);

    gHands.CleanUpHousePointer(fHouseToDeliver);
    fHouseToDeliver := aValue.GetPointer;
    aValue.AddHouseDeliveryFrom(self);
  end;
end;

procedure TKMHouse.AddHouseDeliveryFrom(aHouse : TKMHouse);
var I : Integer;
begin
  If not aHouse.IsValid then
    Exit;

  I := length(fDeliveryFromHouses);
  SetLength(fDeliveryFromHouses, I + 1);
  fDeliveryFromHouses[I] := aHouse.GetPointer;
end;

procedure TKMHouse.RemHouseDeliveryFrom(aHouse : TKMHouse);
var I, index : integer;
begin
  If self = nil then
    Exit;

  index := -1;
  for I := 0 to High(fDeliveryFromHouses) do
    If fDeliveryFromHouses[I] = aHouse then
    begin
      index := I;
      break;
    end;
  Assert(index <> -1,'TKMHouse.RemHouseDeliveryFrom(aHouse : TKMHouse)');

  gHands.CleanUpHousePointer(aHouse);
  fDeliveryFromHouses[index] := fDeliveryFromHouses[high(fDeliveryFromHouses)];
  SetLength(fDeliveryFromHouses, high(fDeliveryFromHouses));
end;

function TKMHouse.CanMakeUpgrade : Boolean;
begin
  if IsUpgrading or not IsComplete then
   Exit(false);

  Result := CanCancelUpgrade;//can't make upgrade when seconds before we canceled it
  Result := Result and ((gHands[Owner].Locks.HouseMaxLvl[fType] = 0)
              or (CurrentLevel < gHands[Owner].Locks.HouseMaxLvl[fType] - 1))
            and not IsMaxLevel and not IsDamaged;
end;

procedure TKMHouse.MakeUpgrade;

  function TryToTakeWares(aWare : TKMWareType; aMaxCount : Word) : Byte;//return remaining wares
  var C : integer;
  begin
    Result := aMaxCount;
    if Result = 0 then
      Exit;

    C := Min(CheckWareOut(aWare), aMaxCount);//check ware out first

    if C > 0 then
    begin
      WareTakeFromOut(aWare, C, true);
      Result := Result - C;
    end;
    if Result <= 0 then
      Exit;
    C := Min(CheckWareIn(aWare), aMaxCount);//check ware out first
    if C > 0 then
    begin
      WareTakeFromIn(aWare, C, true);
      Result := Result - C;
    end;


  end;

var lvl : TKMHouseLevelRec;
  C : Integer;
begin
  if IsDestroyed then
    Exit;

  if IsDamaged then
    Exit;

  lvl := gRes.Houses[fType].Levels[fLevel.CurrentLevel];

  fBuildCost.Wood := gHands[Owner].GetHouseWoodCost(fType, CurrentLevel);
  fBuildCost.Stone := gHands[Owner].GetHouseStoneCost(fType, CurrentLevel);
  fBuildCost.Tile := gHands[Owner].GetHouseTileCost(fType, CurrentLevel);

  fResetDemands := true;
  UpdateDemands;

  //Inc(fLevel.Progress, lvl.Progress);
  fLevel.BuildingProgress := lvl.Progress;
  fLevel.IsUpgrading := true;
  fLevel.UpgradingTime := gGameParams.Tick + 100;
  //gHands[Owner].Deliveries.Queue.RemDemand(self);

  gHands[Owner].Constructions.HouseUpgradeList.AddHouse(self); //Add the house to JobList, so then all workers can take it
  {if lvl.WoodCost > 0 then
  begin
    //first tak from out wares
    C := Min(CheckWareOut(wtTimber), lvl.WoodCost);

    if C > 0 then
    begin
      WareTakeFromOut(wtTimber, C, true);
      Dec(lvl.WoodCost);
    end;

  end;}

  {If (HouseType in WALL_HOUSES) and (fLevel.CurrentLevel = 0) and gHands[Owner].BuildDevUnlocked(29) then
  begin
    lvl.WoodCost := Max(lvl.WoodCost - 1, 0);
    lvl.StoneCost := Max(lvl.StoneCost - 1, 0);
    lvl.TileCost := Max(lvl.TileCost - 1, 0);
  end;}


  C := TryToTakeWares(wtTimber, fBuildCost.Wood);
  fBuildWoodDelivered := fBuildCost.Wood - C;
  fBuildSupplyWood := fBuildWoodDelivered;
  if C > 0 then
    gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTimber, C, dtOnce, diHigh4);

  C := TryToTakeWares(wtStone, fBuildCost.Stone);
  fBuildStoneDelivered := fBuildCost.Stone - C;
  fBuildSupplyStone := fBuildStoneDelivered;
  if C > 0 then
    gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtStone, C, dtOnce, diHigh4);

  C := TryToTakeWares(wtTile, fBuildCost.Tile);
  fBuildTileDelivered := fBuildCost.Tile - C;
  fBuildSupplyTile := fBuildTileDelivered;
  if C > 0 then
    gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTile, C, dtOnce, diHigh4);

end;
function TKMHouse.CanCancelUpgrade : Boolean;
begin
  Result := fLevel.UpgradingTime <= gGameParams.Tick;
end;

procedure TKMHouse.CancelUpgrading;
var lvl : TKMHouseLevelRec;
begin
  if not CanCancelUpgrade then
    Exit;
  fLevel.UpgradingTime := gGameParams.Tick + 100;
  fLevel.IsUpgrading := false;
  fLevel.BuildingProgress := 0;
  //Dec(fLevel.Progress, lvl.Progress);
  lvl := gRes.Houses[fType].Levels[fLevel.CurrentLevel];
  fBuildingProgress := MaxHealth;
  //gHands[Owner].Constructions.HouseUpgradeList.RemoveHouse(fHouse); //Add the house to JobList, so then all workers could take it
  gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtTimber, lvl.WoodCost - fBuildWoodDelivered);
  gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtStone, lvl.StoneCost - fBuildStoneDelivered);
  gHands[Owner].Deliveries.Queue.TryRemoveDemand(self, wtTile, lvl.TileCost - fBuildTileDelivered);


  fBuildSupplyWood := 0;
  fBuildSupplyStone := 0;
  fBuildSupplyTile := 0;
  fBuildReserve := 0;

  fBuildTileDelivered := 0;
  fBuildWoodDelivered := 0;
  fBuildStoneDelivered := 0;

  fResetDemands := false;
  UpdateDemands;
end;

function TKMHouse.IsMineShaft : Boolean;
begin
  Result := false;

  if not (HouseType in [htGoldMine, htIronMine, htBitinMine]) then
    Exit;
  If gHands[Owner].BuildDevUnlocked(23) then
    Exit;
  Result := gTerrain.TileIsMineShaft(Entrance);
end;

function TKMHouse.CanTaskProduceWare(aWare: TKMWareType): Boolean;
begin
  Result := (CheckWareOut(aWare) < GetMaxOutWare) or ForceWorking;
end;

procedure TKMHouse.ProduceWare(aWare: TKMWareType; aCount: Integer = 1);
begin
  if aCount > 0 then
  begin
    WareAddToOut(aWare, aCount);
    gHands[Owner].Stats.WareProduced(aWare, aCount);

    case aWare of
      wtSawDust,
      wtTile,
      wtIron,
      wtGold,
      wtFlour,
      wtWine,
      wtSausage,
      wtBread : ProduceFestivalPoints(fptEconomy, 1);

      wtStoneBolt,
      wtFish,
      wtPig,
      wtSkin,
      wtLeather : ProduceFestivalPoints(fptEconomy, 2);

      wtAxe, wtLog, wtWheel, wtMace,
      wtLeatherArmor, wtLance, wtBow, wtQuiver, wtBoots,
      wtFeathers, wtPlateArmor,
      wtWoodenShield : ProduceFestivalPoints(fptEconomy, 3);

      wtSword, wtPike, wtCrossbow,
      wtIronArmor, wtSteelE, wtBolt, wtFlail,
      wtIronShield : ProduceFestivalPoints(fptEconomy, 4);

      wtBitinE,
      wtHorse  : ProduceFestivalPoints(fptEconomy, 5);

      wtBitinArmor,
      wtEgg,
      wtBitin : ProduceFestivalPoints(fptEconomy, 6);
      wtJewerly : ProduceFestivalPoints(fptEconomy, 50);
    end;


    gScriptEvents.ProcWareProduced(self, aWare, aCount);

  end else
  if aCount < 0 then
  begin
    aCount := -aCount;
    WareTakeFromIn(aWare, aCount);
    gHands[Owner].Stats.WareConsumed(aWare, aCount);
  end;
end;

procedure TKMHouse.ProduceWares(aWares: TKMWarePlan; aTake: Boolean = False);
var I, C : Integer;
begin
  for I := 0 to High(aWares) do
  begin
    If aTake then
      C := aWares[I].C * -1
    else
      C := aWares[I].C;
    ProduceWare(aWares[I].W, C);
  end;
end;

procedure TKMHouse.ProduceWareFromFill(aWare: TKMWareType; var aFillFactor: Single);
var C : integer;
begin
  C := trunc(aFillFactor);
  if C = 0 then
    Exit;
  aFillFactor := aFillFactor - C;

  ProduceWare(aWare, C);
  IncProductionCycle(aWare, C);
end;

// Check if house has enough resource supply to be built depending on it's state
function TKMHouse.CheckResToBuild: Boolean;
begin
  case fBuildState of
    hbsWood:   Result := (fBuildSupplyWood > 0) or (fBuildReserve > 0);
    hbsStone:  begin
                  Result := ((fBuildSupplyStone > 0) or (fBuildReserve > 0));

                  if (fBuildStoneDelivered = StoneCost) then
                    Result := Result or (TileCost = 0) or (fBuildSupplyTile > 0) or (fBuildReserve > 0);
              end;
    hbsDone:  if IsUpgrading then
                Result := (fBuildWoodDelivered >= WoodCost)
                          and (fBuildStoneDelivered >= StoneCost)
                          and (fBuildTileDelivered >= TileCost){ or (fBuildReserve > 0)}
              else
                Result := False;
  else
    Result := False;
  end;



end;


function TKMHouse.GetMaxInWare: Word;
begin
  //todo: This belongs to gRes.Houses[]
  if IsUpgrading then
    Exit(0);
  Result := gRes.Houses[fType].MaxWareCount;
  if CurrentLevel > 0 then
    if HSpec.Levels[CurrentLevel - 1].MaxInWares > 0 then
      Result := HSpec.Levels[CurrentLevel - 1].MaxInWares;

  If (HouseType = htSmallStore) and gHands[Owner].EconomyDevUnlocked(23) then
    Result := Result + 50 * (CurrentLevel + 1);
end;

function TKMHouse.GetMaxOutWare: Word;
begin
  //todo: This belongs to gRes.Houses[]
  if IsUpgrading then
    Exit(0);
  Result := gRes.Houses[fType].MaxWareOutCount;
  {if CurrentLevel > 0 then
    if HSpec.Levels[CurrentLevel - 1].MaxInWares > 0 then
      Result := HSpec.Levels[CurrentLevel - 1].MaxInWares;}

  If (HouseType = htFarm) and gHands[Owner].BuildDevUnlocked(12) then
    Result := Result + 5;
  If (HouseType = htProductionThatch) and gHands[Owner].BuildDevUnlocked(22) then
    Result := Result + 20;
end;



procedure TKMHouse.HouseDemandWasClosed(aWare: TKMWareType; aDeleteCanceled: Boolean);
begin
  if Self = nil then Exit;

  if TryDecWareDelivery(aWare, aDeleteCanceled) then
    // Update demands, since our DeliveryCount was changed
    // Maybe we need more wares to order
    UpdateDemands;
end;


function TKMHouse.TryDecWareDelivery(aWare: TKMWareType; aDeleteCanceled: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Self = nil then Exit;

  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareInput[I] then
    begin
      // Do not decrease DeliveryCount, if demand delete was cancelled (demand closing was not possible, f.e. when serf enters the house)
      // thus serf brought ware to the house and we should not decrease delivery count in that case here
      // (but it will be decreased anyway in the WareAddToIn for market)
      if not aDeleteCanceled then
        WareDeliveryCnt[I] := Max(WareDeliveryCnt[I] - 1, 0);

      WareDemandsClosing[I] := Max(WareDemandsClosing[I] - 1, 0);
      Exit(True);
    end;
end;


//Maybe it's better to rule out In/Out? No, it is required to separate what can be taken out of the house and what not.
//But.. if we add "Evacuate" button to all house the separation becomes artificial..
procedure TKMHouse.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False);
var
  I, ordersRemoved: Integer;
begin
  Assert(aWare <> wtNone);

  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareInput[I] then
    begin
      //Don't allow the static script to overfill houses
      if aFromStaticScript then
        aCount := EnsureRange(aCount, 0, GetMaxInWare - fWareIn[I]);
      //WareDeliveryCnt stay same, because corresponding demand will be closed
      ResIn[I] := ResIn[I] + aCount;
      if aFromStaticScript then
      begin
        WareDeliveryCnt[I] := WareDeliveryCnt[I] + aCount;
        ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
        WareDeliveryCnt[I] := WareDeliveryCnt[I] - ordersRemoved;
      end;
    end;
  UpdateDemands;
end;


procedure TKMHouse.WareAddToOut(aWare: TKMWareType; const aCount:integer=1);
var
  I, p, count, count2: Integer;
begin
  if aWare = wtNone then
    exit;

  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareOutput[I] then
    begin
      if HSpec.CanOverFill then
        count2 := Min(ResOut[I] + aCount, High(byte)) - ResOut[I]
      else
        count2 := Min(ResOut[I] + aCount, GetMaxOutWare) - ResOut[I];
      if (ResOut[I] >= GetMaxOutWare) and not HSpec.CanOverFill then
        Exit
      else
        ResOut[I] := Min(ResOut[I] + aCount, IfThen(HSpec.CanOverFill, high(byte), GetMaxOutWare));

      if gRes.Houses[fType].IsWorkshop and (aCount > 0) then
      begin
        count := aCount;
        for p := 0 to 19 do
          if fWareOutPool[p] = 0 then
          begin
            fWareOutPool[p] := I;
            Dec(count);
            if count = 0 then
              Break;
          end;
      end;
      gHands[Owner].Deliveries.Queue.AddOffer(Self, aWare, count2);

      //in realism food rottens in some houses
      if gGameParams.MBD.IsRealism then
        if not (HouseType in [htStore, htSmallStore, htInn, htTownhall, htPalace]) then
          if aWare in WARES_HOUSE_FOOD then
            fFoodToRot.Add(RottenFood(aWare, count2, Tick + FOOD_TO_ROT));
    end;
end;


procedure TKMHouse.WareAddToEitherFromScript(aWare: TKMWareType; aCount: Integer);
var
  count: Integer;
  isInOut : Boolean;
begin
  isInOut := WareIsInAndOut(aWare);

  count := 0;
  //Don't allow output to be overfilled from script. This is not checked
  //in WareAddToOut because e.g. stonemason is allowed to overfill it slightly)
  if WareCanAddToOut(aWare) then
    if not (fType in [htStore, htMarket, htBarracks, htInn, htPalace]) then
    begin
      count := Min(aCount, GetMaxOutWare - CheckWareOut(aWare));
      WareAddToOut(aWare, aCount);
      if not isInOut then
        Exit;
    end;

  //No range checking required as WareAddToIn does that
  //If WareCanAddToIn, add it immediately and exit (e.g. store)
  WareAddToIn(aWare, aCount - count, True);
end;

function TKMHouse.NeedsWareToBuild(aWare: TKMWareType): Boolean;
begin
  case aWare of
    wtTimber:   Result := fBuildWoodDelivered < WoodCost;
    wtStone:    Result := fBuildStoneDelivered < StoneCost;
    wtTile:     Result := fBuildTileDelivered < TileCost;
    else
      Result := false;
  end;
end;
// Add resources to building process
function TKMHouse.WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1) : Boolean;
var maxWood, maxStone, maxTile : Byte;
begin
  Result := true;
  maxWood := WoodCost;
  maxStone := StoneCost;
  maxTile := TileCost;

  if IsUpgrading then
  begin
    maxWood := gRes.Houses[fType].Levels[fLevel.CurrentLevel].WoodCost;
    maxStone := gRes.Houses[fType].Levels[fLevel.CurrentLevel].StoneCost;
    maxTile := gRes.Houses[fType].Levels[fLevel.CurrentLevel].TileCost;
  end;


  case aWare of
    wtTimber:  fBuildSupplyWood := EnsureRange(fBuildSupplyWood + aCount, 0, maxWood);
    wtStone:   fBuildSupplyStone := EnsureRange(fBuildSupplyStone + aCount, 0, maxStone);
    wtTile:   fBuildSupplyTile := EnsureRange(fBuildSupplyTile + aCount, 0, maxTile);
  else
    raise ELocError.Create('WIP house is not supposed to recieve ' + gRes.Wares[aWare].Title + ', right?', fPosition);
  end;

  case aWare of
    wtTimber:  fBuildWoodDelivered := EnsureRange(fBuildWoodDelivered + aCount, 0, maxWood);
    wtStone:   fBuildStoneDelivered := EnsureRange(fBuildStoneDelivered + aCount, 0, maxStone);
    wtTile:   fBuildTileDelivered := EnsureRange(fBuildTileDelivered + aCount, 0, maxTile);
    else
      Result := false;
  end;

end;


function TKMHouse.WareCanAddToIn(aWare: TKMWareType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    case fWareInput[I] of
      wtNone: ;
      wtAll:       Result := aWare in WARES_VALID;
      wtWarfare:   Result := aWare in WARES_WARFARE;
      else Result := aWare = fWareInput[I];
    end;
    If Result then
      Exit;
  end;
end;


function TKMHouse.WareCanAddToOut(aWare: TKMWareType): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareOutput[I] then
      Result := True;
end;


function TKMHouse.CanHaveWareType(aWare: TKMWareType): Boolean;
begin
  Result := WareCanAddToIn(aWare) or WareCanAddToOut(aWare);
end;


function TKMHouse.GetWareIn(aI: Byte): Word;
begin
  Result := fWareIn[aI];
end;


function TKMHouse.GetWareOut(aI: Byte): Word;
begin
  Result := fWareOut[aI];
end;


function TKMHouse.GetWareInLocked(aI: Byte): Word;
begin
  Result := 0; //By default we do not lock any In res
end;


procedure TKMHouse.SetWareInManageTakeOutDeliveryMode(aWare: TKMWareType; aCntChange: Integer);
begin
  //In case we brought smth to house with TakeOut delivery mode,
  //then we need to add it to offer
  //Usually it can happens when we changed delivery mode while serf was going inside house
  //and his delivery was not cancelled, but resource was not in the house yet
  //then it was not offered to other houses
  if fDeliveryMode = dmTakeOut then
  begin
    if not (aWare in [wtNone, wtAll, wtWarfare]) and (aCntChange > 0) then
      gHands[Owner].Deliveries.Queue.AddOffer(Self, aWare, aCntChange);
  end;
end;


procedure TKMHouse.SetWareIn(aI: Byte; aValue: Word);
var
  cntChange: Integer;
  W: TKMWareType;
begin
  W := fWareInput[aI];
  cntChange := aValue - fWareIn[aI];

  SetWareInManageTakeOutDeliveryMode(W, cntChange);

  fWareIn[aI] := aValue;

  if not (W in [wtNone, wtAll, wtWarfare]) and (cntChange <> 0) then
    gScriptEvents.ProcHouseWareCountChanged(Self, W, aValue, cntChange);
end;

procedure TKMHouse.SetWareInCnt(aWare : TKMWareType; aValue : Integer);
var I : Integer;
begin
  for I := low(fWareIn) to High(fWareIn) do
    if not (aWare in [wtAll, wtNone, wtFood, wtValuable]) then
      if fWareInput[I] = aWare then
        fWareIn[I] := aValue;

end;

procedure TKMHouse.SetWareOut(aI: Byte; aValue: Word);
var
  cntChange: Integer;
  W: TKMWareType;
begin
  W := fWareOutput[aI];
  cntChange := aValue - fWareOut[aI];

  fWareOut[aI] := aValue;

  if not (W in [wtNone, wtAll, wtWarfare]) and (cntChange <> 0) then
    gScriptEvents.ProcHouseWareCountChanged(Self, W, aValue, cntChange);
end;


function TKMHouse.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareOutput[I] then
      Result := fWareOut[I] >= aCount;

  if not Result and (fNewDeliveryMode = dmTakeOut) then
    for I := 1 to WARES_IN_OUT_COUNT do
      if aWare = fWareInput[I] then
        Result := ResIn[I] - WareInLocked[I] >= aCount;
end;


procedure TKMHouse.WareTake(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  //Range checking is done within WareTakeFromIn and WareTakeFromOut when aFromScript=True
  //Only one will succeed, we don't care which one it is
  WareTakeFromIn(aWare, aCount, aFromScript);
  WareTakeFromOut(aWare, aCount, aFromScript);
end;


// Take resource from Input and order more of that kind if DistributionRatios allow
procedure TKMHouse.WareTakeFromIn(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var
  I, K: Integer;
begin
  Assert(aWare <> wtNone);

  if DontNeedRes and not gGameParams.IsMapEditor then
    Exit;

  for I := 1 to WARES_IN_OUT_COUNT do
  if aWare = fWareInput[I] then
  begin
    if aFromScript then
    begin
      //Script might try to take too many
      aCount := EnsureRange(aCount, 0, ResIn[I]);
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
    end;

    //Keep track of how many are ordered
    WareDeliveryCnt[I] := EnsureRange(WareDeliveryCnt[I] - aCount, 0, High(Word));

    Assert(ResIn[I] >= aCount, 'fResourceIn[i] < 0');
    ResIn[I] := ResIn[I] - aCount;
    //Only request a new resource if it is allowed by the distribution of wares for our parent player
    for K := 1 to aCount do
      if WareDeliveryCnt[I] < GetWareDistribution(I) then
      begin
        gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, aWare, 1, dtOnce, diNorm);
        WareDeliveryCnt[I] := WareDeliveryCnt[I] + 1;
      end;
    Break;
  end;
  UpdateDemands;
end;


procedure TKMHouse.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var
  I, K, index, p, count: integer;
  isWareBoth : Boolean;
begin
  Assert(aWare <> wtNone);
  Assert(not(fType in [htStore,htBarracks]));

  isWareBoth := WareIsInAndOut(aWare) and (fDeliveryMode = dmTakeOut);


  for I := 1 to WARES_IN_OUT_COUNT do
    if (aWare = fWareOutput[I]) then
      if  ( isWareBoth and (fWareOut[I] > 0)) or not isWareBoth then
      begin
        if aFromScript then
        begin
          aCount := Min(aCount, fWareOut[I]);
          if aCount > 0 then
          begin
            gHands[Owner].Stats.WareConsumed(aWare, aCount);
            gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
          end;
        end;

        Assert(aCount <= fWareOut[I]);

        if gRes.Houses[fType].IsWorkshop and (aCount > 0) then
        begin
          count := aCount;
          for p := 0 to 19 do
            if fWareOutPool[p] = I then
              begin
                fWareOutPool[p] := 0;
                Dec(count);
                if count = 0 then
                  Break;
              end;
        end;

        ResOut[I] := ResOut[I] - aCount;
        Exit;
      end;

  // Next part is for take-out mode only
  if fDeliveryMode <> dmTakeOut then Exit;

  // Try to take ware from 'in' queue, if we are in take-out delivery mode
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareInput[I] then
    begin
      //in realism food rottens in some houses
      if gGameParams.MBD.IsRealism then
        if not (HouseType in [htStore, htSmallStore, htInn, htTownhall, htPalace]) then
          if aWare in WARES_HOUSE_FOOD then
          begin
            index := -1;
            for K := 0 to fFoodToRot.Count - 1 do
              if (fFoodToRot[K].WareType = aWare)
              and ((index = -1) or (fFoodToRot[K].TimeToRotten > fFoodToRot[index].TimeToRotten)) then
                index := K;

            if index >= 0 then
              fFoodToRot.Remove(index);
          end;

      if aFromScript then
      begin
        //Script might try to take too many
        aCount := Min(aCount, ResIn[I]);
        if aCount > 0 then
          gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
      end;

      //Keep track of how many are ordered
      WareDeliveryCnt[I] := WareDeliveryCnt[I] - aCount;

      Assert(ResIn[I] >= aCount, 'fResourceIn[i] < 0');
      ResIn[I] := ResIn[I] - aCount;
      //Only request a new resource if it is allowed by the distribution of wares for our parent player
      for K := 1 to aCount do
        if WareDeliveryCnt[I] < Min(GetWareDistribution(I), GetMaxInWare - GetAcceptWareIn(aWare)) then
        begin
          gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, aWare, 1, dtOnce, diNorm);
          WareDeliveryCnt[I] := WareDeliveryCnt[I] + 1;
        end;
      Break;
    end;
  UpdateDemands;
end;


function TKMHouse.GetWareDistribution(aID: Byte): Byte;
begin
  Result := gHands[Owner].Stats.WareDistribution[fWareInput[aID],fType];

  if CurrentLevel > 0 then
    if HSpec.Levels[CurrentLevel - 1].MaxInWares > 0 then
      Result := HSpec.Levels[CurrentLevel - 1].MaxInWares;
  
end;


procedure TKMHouse.MakeSound;
var
  work: TKMHouseActionType;
  step: Byte;
begin
  if SKIP_SOUND then Exit;

  if CurrentAction = nil then exit; //no action means no sound ;)

  if haWork1 in CurrentAction.SubAction then work := haWork1 else
  if haWork2 in CurrentAction.SubAction then work := haWork2 else
  if haWork3 in CurrentAction.SubAction then work := haWork3 else
  if haWork4 in CurrentAction.SubAction then work := haWork4 else
  if haWork5 in CurrentAction.SubAction then work := haWork5 else
    Exit; //No work is going on

  step := gRes.Houses[fType].Anim[work].Count;
  if step = 0 then Exit;

  step := WorkAnimStep mod step;




  //Do not play sounds if house is invisible to gMySpectator
  //This check is slower so we do it after other Exit checks
  if gMySpectator.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then exit;


  //first check if house type has sound from json file
  if gRes.Houses[fType].Sound[work].ID > -1 then
    if step in gRes.Houses[fType].Sound[work].Steps then
    begin
      gSoundPlayer.PlayHouse(gRes.Houses[fType].Sound[work].ID, fPosition, 1);

      Exit;
    end;



  case fType of //Various buildings and HouseActions producing sounds
    //htSchool:        if (work = haWork5)and(step = 28) then gSoundPlayer.Play(sfxSchoolDing, fPosition); //Ding as the clock strikes 12
    htMill:          if (work = haWork2)and(step = 0) then gSoundPlayer.Play(sfxMill, fPosition);
    htCoalMine:      if (work = haWork1)and(step = 5) then gSoundPlayer.Play(sfxcoalDown, fPosition)
                      else if (work = haWork1)and(step = 24) then gSoundPlayer.Play(sfxCoalMineThud, fPosition,True,0.8)
                      else if (work = haWork2)and(step = 7) then gSoundPlayer.Play(sfxmine, fPosition)
                      else if (work = haWork5)and(step = 1) then gSoundPlayer.Play(sfxcoalDown, fPosition);
    htBitinMine,
    htIronMine:      if (work = haWork2)and(step = 7) then gSoundPlayer.Play(sfxmine, fPosition);
    htGoldMine:      if (work = haWork2)and(step = 5) then gSoundPlayer.Play(sfxmine, fPosition);
    htSawmill:       if (work = haWork2)and(step = 1) then gSoundPlayer.Play(sfxsaw, fPosition);
    htVineyard:      if (work = haWork2)and(step in [1,7,13,19]) then gSoundPlayer.Play(sfxwineStep, fPosition)
                      else if (work = haWork5)and(step = 14) then gSoundPlayer.Play(sfxwineDrain, fPosition,True,1.5)
                      else if (work = haWork1)and(step = 10) then gSoundPlayer.Play(sfxwineDrain, fPosition,True,1.5);
    htBakery:        if (work = haWork3)and(step in [6,25]) then gSoundPlayer.Play(sfxBakerSlap, fPosition);
    htStoneWorkshop,
    htQuarry:         if (work = haWork2)and(step in [4,13]) then gSoundPlayer.Play(sfxQuarryClink, fPosition)
                      else if (work = haWork5)and(step in [4,13,22]) then gSoundPlayer.Play(sfxQuarryClink, fPosition);
    htWeaponSmithy:  if (work = haWork1)and(step in [17,22]) then gSoundPlayer.Play(sfxBlacksmithFire, fPosition)
                      else if (work = haWork2)and(step in [10,25]) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition)
                      else if (work = haWork3)and(step in [10,25]) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition)
                      else if (work = haWork4)and(step in [8,22]) then gSoundPlayer.Play(sfxBlacksmithFire, fPosition)
                      else if (work = haWork5)and(step = 12) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition);
    htArmorSmithy:   if (work = haWork2)and(step in [13,28]) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition)
                      else if (work = haWork3)and(step in [13,28]) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition)
                      else if (work = haWork4)and(step in [8,22]) then gSoundPlayer.Play(sfxBlacksmithFire, fPosition)
                      else if (work = haWork5)and(step in [8,22]) then gSoundPlayer.Play(sfxBlacksmithFire, fPosition);

    htIronFoundry: if (work = haWork2)and(step = 6) then gSoundPlayer.Play(sfxmetallurgists, fPosition);

    htMetallurgists: if (work = haWork3)and(step = 6) then gSoundPlayer.Play(sfxmetallurgists, fPosition)
                      else if (work = haWork4)and(step in [16,20]) then gSoundPlayer.Play(sfxwineDrain, fPosition);
    htIronSmithy:    if (work = haWork2)and(step in [1,16]) then gSoundPlayer.Play(sfxmetallurgists, fPosition)
                      else if (work = haWork3)and(step = 1) then gSoundPlayer.Play(sfxmetallurgists, fPosition)
                      else if (work = haWork3)and(step = 13) then gSoundPlayer.Play(sfxwineDrain, fPosition);
    htWeaponWorkshop:if (work = haWork2)and(step in [1,10,19]) then gSoundPlayer.Play(sfxsaw, fPosition)
                      else if (work = haWork3)and(step in [10,21]) then gSoundPlayer.Play(sfxCarpenterHammer, fPosition);

    htSiegeWorkshop:  if (work = haWork2)and(step in [3,13,23]) then gSoundPlayer.Play(sfxsaw, fPosition)
                      else if (work = haWork4)and(step in [3,13,23]) then gSoundPlayer.Play(sfxCarpenterHammer, fPosition)
                      else if (work = haWork5)and(step in [10,20]) then gSoundPlayer.Play(sfxBlacksmithBang, fPosition);

    htArmorWorkshop: if (work = haWork2)and(step in [3,13,23]) then gSoundPlayer.Play(sfxsaw, fPosition)
                      else if (work = haWork3)and(step in [17,28]) then gSoundPlayer.Play(sfxCarpenterHammer, fPosition)
                      else if (work = haWork4)and(step in [10,20]) then gSoundPlayer.Play(sfxCarpenterHammer, fPosition);
    htTannery:       if (work = haWork2)and(step = 5) then gSoundPlayer.Play(sfxLeather, fPosition,True,0.8);
    htButchers:      if (work = haWork2)and(step in [8,16,24]) then gSoundPlayer.Play(sfxButcherCut, fPosition)
                      else if (work = haWork3)and(step in [9,21]) then gSoundPlayer.Play(sfxSausageString, fPosition);
    htSwine:         if ((work = haWork2)and(step in [10,20]))or((work = haWork3)and(step = 1)) then gSoundPlayer.Play(sfxButcherCut, fPosition);
    //htWatchTower:  Sound handled by projectile itself
  end;
end;


procedure TKMHouse.Save(SaveStream: TKMemoryStream);
var
  I, newCount: Integer;
  hasAct: Boolean;
begin
  inherited;

  SaveStream.PlaceMarker('House');
  SaveStream.Write(fType, SizeOf(fType));
  SaveStream.Write(fPosition);
  SaveStream.Write(fBuildState, SizeOf(fBuildState));
  SaveStream.Write(fBuildSupplyWood);
  SaveStream.Write(fBuildSupplyStone);
  SaveStream.Write(fBuildSupplyTile);
  SaveStream.Write(fBuildTileDelivered);
  SaveStream.Write(fBuildWoodDelivered);
  SaveStream.Write(fBuildStoneDelivered);
  SaveStream.Write(fBuildReserve);
  SaveStream.Write(fBuildingProgress, SizeOf(fBuildingProgress));
  SaveStream.Write(fDamage, SizeOf(fDamage));
  SaveStream.Write(TKMUnit(fWorker).UID); // Store UID
  SaveStream.Write(fHouseToDeliver.UID); //subst on syncload
  SaveStream.Write(fBuildingRepair);
  SaveStream.Write(Byte(fDeliveryMode));
  SaveStream.Write(Byte(fNewDeliveryMode));
  SaveStream.Write(fUpdateDeliveryModeOnTick);
  SaveStream.Write(fIsClosedForWorker);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareIn[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareDeliveryCount[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareDemandsClosing[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareOut[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareOrder[I], SizeOf(fWareOrder[I]));
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareInput[I], SizeOf(fWareInput[I]));
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareOutput[I], SizeOf(fWareOutput[I]));
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fWareBlocked[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(TransferWare[I]);
  for I := 1 to WARES_IN_OUT_COUNT do SaveStream.Write(fProductionCycles[I]);
//  for I:=1 to 4 do SaveStream.Write(fWareOrderDesired[I], SizeOf(fWareOrderDesired[I]));

  if gRes.Houses[fType].IsWorkshop then
    SaveStream.Write(fWareOutPool, SizeOf(fWareOutPool)); //todo: Should be SizeOf()

  SaveStream.Write(fLastOrderProduced);
  SaveStream.Write(FlagAnimStep);
  SaveStream.Write(WorkAnimStep);
  SaveStream.WriteData(fIsOnTerrain);
  SaveStream.Write(fSnowStep);
  SaveStream.Write(fIsDestroyed);
  SaveStream.Write(fTimeSinceUnoccupiedReminder);
  SaveStream.Write(fDisableUnoccupiedMessage);
  SaveStream.Write(fNeedIssueOrderCompletedMsg);
  SaveStream.Write(fOrderCompletedMsgIssued);
  hasAct := CurrentAction <> nil;
  SaveStream.Write(hasAct);
  if hasAct then CurrentAction.Save(SaveStream);
  SaveStream.Write(fResourceDepletedMsgIssued);
  SaveStream.Write(DoorwayUse);
  SaveStream.Write(fPlacedOverRoad);
  SaveStream.Write(fWariant);
  SaveStream.Write(fStyle);
  SaveStream.Write(fSlotID);
  SaveStream.Write(fIsBurning);
  With fLevel do
  begin
    SaveStream.Write(CurrentLevel);
    SaveStream.Write(Progress);
    SaveStream.Write(BuildingProgress);
    SaveStream.Write(IsUpgrading);
  end;
  SaveStream.WriteANSI(fText);
  SaveStream.Write(fForceWorking);
  SaveStream.Write(fDontNeedRes);
  SaveStream.Write(WorkingTime);
  SaveStream.Write(TotalWorkingTime);
  SaveStream.Write(WareSlotChangedByAI);
  SaveStream.Write(BootsReserved);
  SaveStream.Write(LastCellID);
  SaveStream.Write(fWasTookOver);
  SaveStream.Write(Indestructible);
  SaveStream.Write(FlagColor);

  newCount := length(fWorkers);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMUnit(fWorkers[I]).UID);

  newCount := length(fDeliveryFromHouses);
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMHouse(fDeliveryFromHouses[I]).UID);

  newCount := length(fNotAcceptWorkers);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMUnit(fNotAcceptWorkers[I]).UID);
  fFoodToRot.SaveToStream(SaveStream);

  SaveStream.WriteData(fBuildCost);
end;


procedure TKMHouse.PostLoadMission;
begin
  //Do nothing, override where needed
end;


procedure TKMHouse.IncSnowStep;
const
  //How much ticks it takes for a house to become completely covered in snow
  SNOW_TIME = 600;
var
  wasOnSnow: TKMTerrPicType;
begin
  if (FlagAnimStep mod 10 = 0) and gGameParams.IsMapEditor then
  begin
    wasOnSnow := fIsOnTerrain;
    CheckOnTerrain;
    if not (wasOnSnow <> tptNone) or not (fIsOnTerrain <> tptNone) then
      fSnowStep := 0;
  end;
  if (fIsOnTerrain <> tptNone) and (fSnowStep < 1) then
    fSnowStep := Min(fSnowStep + (1 + Byte(gGameParams.IsMapEditor) * 10) / SNOW_TIME, 1);
end;

procedure TKMHouse.IncAnimStep;
var
  I, K: Integer;
  HA: TKMHouseAreaNew;
begin
  Inc(FlagAnimStep);
  WorkAnimStepPrev := WorkAnimStep;
  Inc(WorkAnimStep);

  IncSnowStep;
  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if gGameParams.DynamicFOW and (FlagAnimStep mod FOW_PACE = 0) then
  begin
    HA := gRes.Houses[fType].BuildArea;
    //Reveal house from all points it covers
    for I := 1 to MAX_HOUSE_SIZE do
      for K := 1 to MAX_HOUSE_SIZE do
        if HA[I,K] <> 0 then
          gHands.RevealForTeam(Owner, KMPoint(fPosition.X + K - 4, fPosition.Y + I - 4), gRes.Houses[fType].Sight, FOG_OF_WAR_INC, frtHouse);
  end;
end;

procedure TKMHouse.ProduceFestivalPoints(aType : TKMFestivalPointType; aAmount : Integer);
begin
  gHands[Owner].AddFestivalPoints(aType, aAmount);
end;

procedure TKMHouse.ProduceCoins;
var C : Integer;
begin
  case HouseType of
    htCottage, htHouse,
    htInn, htSchool,
    htStore : C := 2;

    htTownhall,
    htBarracks : C := 3;
    htMerchant,
    htCartographers, htProductionThatch,
    htForest, htPasture,
    htPalace, htArena : C := 5;
    htPearl : C := 20;
    else C := 1;
  end;
  gHands[Owner].VirtualWareTake('vtCoin', -C);

end;

function TKMHouse.GetVWareModulo(W: Integer; aWare : TKMWareType): Byte;
begin
  Result := gRes.Wares.VirtualWares[W].GetModulo(HouseType, aWare);

  If gHands[Owner].BuildDevUnlocked(4) and (gRes.Wares.VirtualWares[W].Name = 'vtFurnitures') then
    Result := Max(Result - 1, 0);
end;

procedure TKMHouse.IncProductionCycle(aIndex: Integer);
var W : TKMWareType;
  I, aMod : Integer;
begin
  if aIndex <= 0 then Exit;
  Inc(fProductionCycles[aIndex]);
  W := WareOutput[aIndex];
  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    if HouseType in gRes.Wares.VirtualWares[I].ProduceInHouses then
    begin
      aMod := gRes.Wares.VirtualWares[I].GetModulo(HouseType, W);
      if aMod > 0 then
        if fProductionCycles[aIndex] mod aMod = 0 then
          gHands[Owner].SetVirtualWareCnt(gRes.Wares.VirtualWares[I].Name, gRes.Wares.VirtualWares[I].GetProdCount(HouseType, W));
    end;
end;

procedure TKMHouse.IncProductionCycle(aWare: TKMWareType);
var I, aIndex: Integer;
begin
  aIndex := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareOutput[I] then
      aIndex := I;
  IncProductionCycle(aIndex);
end;

procedure TKMHouse.IncProductionCycle(aWare : TKMWareType; aCount : Integer);
var I, aIndex : Integer;
begin
  if aCount <= 0 then
    Exit;
  aIndex := GetWareOutIndex(aWare);

  for I := 1 to aCount do
    IncProductionCycle(aIndex);
end;
//Request more wares (if distribution of wares has changed)
//todo: Situation: I have timber set to 5 for the weapons workshop, and no timber in my village.
//      I change timber to 0 for the weapons workshop. My woodcutter starts again and 5 timber is still
//      taken to the weapons workshop because the request doesn't get canceled.
//      Maybe it's possible to cancel the current requests if no serf has taken them yet?
procedure TKMHouse.UpdateDemands;
Const MAX_ORDERS = 10;
  function MaxOrders : Word;
  begin
    Result := MAX_ORDERS;
    case HouseType of
      htTownhall : Result := 20;
    end;
  end;
var
  I: Integer;
  demandsRemoved, plannedToRemove, demandsToChange: Integer;
  maxDistribution: Byte;
  resDelivering : Integer;
begin
  If not IsComplete then
    Exit;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if (fWareInput[I] in [wtAll, wtWarfare, wtNone]) then Continue;

    resDelivering := WareDeliveryCnt[I] - WareDemandsClosing[I];

    maxDistribution := Max(0, Min(GetWareDistribution(I), GetMaxInWare - GetAcceptWareIn(fWareInput[I])));
    if fResetDemands then
      maxDistribution := 0;

    //demandsToChange := resDistribution - (WareDeliveryCnt[I] - WareDemandsClosing[I]);

    //demandsToChange := Min( 5 - (demandsToChange - fWareIn[I]), GetMaxInWare -  demandsToChange);

    demandsToChange := Min( MaxOrders - (resDelivering - fWareIn[I]), maxDistribution -  resDelivering);

    //Not enough resources ordered, add new demand
    if demandsToChange > 0 then
    begin
      gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, fWareInput[I], demandsToChange, dtOnce, diNorm);

      WareDeliveryCnt[I] := WareDeliveryCnt[I] + demandsToChange;
    end else
    //Too many resources ordered, attempt to remove demand if nobody has taken it yet
    if demandsToChange < 0 then
    begin
      demandsRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, fWareInput[I], -demandsToChange, plannedToRemove);

      WareDeliveryCnt[I] := WareDeliveryCnt[I] - demandsRemoved; //Only reduce it by the number that were actually removed
      WareDemandsClosing[I] := WareDemandsClosing[I] + plannedToRemove;
    end;
  end;
end;

procedure TKMHouse.ToggleAcceptWaresIn(aWare : TKMWareType; aCount : Integer = 1);
var I : Integer;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
    if (aWare = fWareInput[I]) or (aWare = wtAll) then
      fWareBlocked[I] := EnsureRange(fWareBlocked[I] + aCount, 0, GetMaxInWare);

  UpdateDemands;
end;

function TKMHouse.ObjToString(const aSeparator: String = '|'): String;
var
  I: Integer;
  actStr, resOutPoolStr, workerStr: String;
begin
  if Self = nil then Exit('nil');

  workerStr := 'nil';

  //if fWorker <> nil then
  //  workerStr := TKMUnit(fWorker).ObjToStringShort(' ');
  if length(fWorkers) > 0 then
    if fWorkers[0] <> nil then
      workerStr := TKMUnit(fWorkers[0]).ObjToStringShort(' ');

  actStr := 'nil';
  if CurrentAction <> nil then
    actStr := CurrentAction.ClassName;

  resOutPoolStr := '';
  for I := Low(fWareOutPool) to High(fWareOutPool) do
  begin
    if resOutPoolStr <> '' then
      resOutPoolStr := resOutPoolStr + ',';
    if I = 10 then
      resOutPoolStr := resOutPoolStr + aSeparator;
    resOutPoolStr := resOutPoolStr + IntToStr(fWareOutPool[I]);
  end;


  Result := inherited ObjToString(aSeparator) +
            Format('%sWorker = %s%sAction = %s%sRepair = %s%sIsClosedForWorker = %s%sDeliveryMode = %s%s' +
                   'NewDeliveryMode = %s%sDamage = %d%s' +
                   'BuildState = %s%sBuildDeliveredWood = %d%sBuildDeliveredStone = %d%sBuildDeliveredTile = %d%sBuildSupplyWood = %d%sBuildSupplyStone = %d%sBuildSupplyTile = %d%sBuildReserve = %d%sBuildingProgress = %d%sDoorwayUse = %d%s' +
                   'ResIn = %d,%d,%d,%d%sResDeliveryCnt = %d,%d,%d,%d%sResDemandsClosing = %d,%d,%d,%d%sResOut = %d,%d,%d,%d%s' +
                   'WareOrder = %d,%d,%d,%d%sResOutPool = %s%s'+
                   'CanChangeWareInput =%s%s' +
                   'WareInputSlot =%d%s' +
                   'IsUpgrading =%s%s' +
                   'HousesUpgradingCnt =%d%s' +
                   'ForceWorking = %s%s' +
                   'WorkingTime = %d%s' +
                   'TotalWorkingTime = %d'
                   ,
                   [aSeparator,
                    workerStr, aSeparator,
                    actStr, aSeparator,
                    BoolToStr(fBuildingRepair, True), aSeparator,
                    BoolToStr(fIsClosedForWorker, True), aSeparator,
                    GetEnumName(TypeInfo(TKMDeliveryMode), Integer(fDeliveryMode)), aSeparator,
                    GetEnumName(TypeInfo(TKMDeliveryMode), Integer(fNewDeliveryMode)), aSeparator,
                    fDamage, aSeparator,
                    GetEnumName(TypeInfo(TKMHouseBuildState), Integer(fBuildState)), aSeparator,
                    fBuildWoodDelivered, aSeparator,
                    fBuildStoneDelivered, aSeparator,
                    fBuildTileDelivered, aSeparator,
                    fBuildSupplyWood, aSeparator,
                    fBuildSupplyStone, aSeparator,
                    fBuildSupplyTile, aSeparator,
                    fBuildReserve, aSeparator,
                    fBuildingProgress, aSeparator,
                    DoorwayUse, aSeparator,
                    fWareIn[1], fWareIn[2], fWareIn[3], fWareIn[4], aSeparator,
                    fWareDeliveryCount[1], fWareDeliveryCount[2], fWareDeliveryCount[3], fWareDeliveryCount[4], aSeparator,
                    fWareDemandsClosing[1], fWareDemandsClosing[2], fWareDemandsClosing[3], fWareDemandsClosing[4], aSeparator,
                    fWareOut[1], fWareOut[2], fWareOut[3], fWareOut[4], aSeparator,
                    fWareOrder[1], fWareOrder[2], fWareOrder[3], fWareOrder[4], aSeparator,
                    resOutPoolStr, aSeparator,
                    BoolToStr(CanChangeWareInput, true),aSeparator,
                    fSlotID, aSeparator,
                    BoolToStr(fLevel.IsUpgrading, true), aSeparator,
                    gHands[Owner].Constructions.HouseUpgradeList.Count, aSeparator,
                    BoolToStr(fForceWorking, true), aSeparator,
                    WorkingTime, aSeparator,
                    TotalWorkingTime

                    ]);
end;


procedure TKMHouse.UpdateState(aTick: Cardinal);
const
  HOUSE_PLAN_SIGHT = 2;
var
  I, K: Integer;
  houseUnoccupiedMsgId: Integer;
  HA: TKMHouseAreaNew;
begin
  if not IsComplete then
  begin
    if gGameParams.DynamicFOW and ((aTick + Owner) mod FOW_PACE = 0) then
    begin
      HA := gRes.Houses[fType].BuildArea;
      //Reveal house from all points it covers
      for I := 1 to MAX_HOUSE_SIZE do
        for K := 1 to MAX_HOUSE_SIZE do
          if HA[I,K] <> 0 then
            gHands.RevealForTeam(Owner, KMPoint(fPosition.X + K - 4, fPosition.Y + I - 4), HOUSE_PLAN_SIGHT, FOG_OF_WAR_INC, frtHouse);
    end;
    Exit; //Don't update unbuilt houses
  end;


  fTick := aTick;
  if CurrentAction.State = hstWork then
    Inc(WorkingTime)
  else
    WorkingTime := 0;

  //Update delivery mode, if time has come
  if (fUpdateDeliveryModeOnTick = fTick) then
    UpdateDeliveryMode;

  if fType = htWallTower then
  begin
    if CurrentAction.State = hstIdle then
      if fTick mod 300 = 0 then
        SetState(hstIdle);
  end;

  if fIsBurning > 0 then
    if fTick mod 10 = 0 then
      AddDamage(fIsBurning, nil, false);

  if fTick mod 50 = 0 then
    for I := 1 to WARES_IN_OUT_COUNT do
      if TransferWare[I] then
        if (fWareOutPut[I] <> wtNone) and (fWareOutPut[I] = fWareInPut[I]) then
          if (fWareOut[I] < 5) and (fWareIn[I] > 0) then
          begin
            WareTakeFromIn(fWareOutPut[I]);
            WareAddToOut(fWareOutPut[I]);
          end;

  //Show unoccupied message if needed and house belongs to human player and can have worker at all
  //and is not closed for worker and not a barracks
  if not gGameParams.MBD.IsRealism then
    if not fDisableUnoccupiedMessage and not HasWorker and not fIsClosedForWorker
    and gRes.Houses[fType].CanHasWorker and (fType <> htBarracks) and ShowUnoccupiedMSG then
    begin
      Dec(fTimeSinceUnoccupiedReminder);
      if fTimeSinceUnoccupiedReminder = 0 then
      begin
        houseUnoccupiedMsgId := gRes.Houses[fType].UnoccupiedMsgId;
        if houseUnoccupiedMsgId <> -1 then // HouseNotOccupMsgId should never be -1
          ShowMsg(houseUnoccupiedMsgId)
        else
          gLog.AddTime('Warning: HouseUnoccupiedMsgId for house type ord=' + IntToStr(Ord(fType)) + ' could not be determined.');
        fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES; //Don't show one again until it is time
      end;
    end
    else
      fTimeSinceUnoccupiedReminder := TIME_BETWEEN_MESSAGES;

  if not fIsDestroyed then MakeSound; //Make some sound/noise along the work

  //in realism food rottens in some houses
  if gGameParams.MBD.IsRealism then
    for I := fFoodToRot.Count - 1 downto 0 do
      if Tick >= fFoodToRot[I].TimeToRotten then
      begin
        WareTakeFromOut(fFoodToRot[I].WareType, fFoodToRot[I].Count, true);
        fFoodToRot.Remove(I);
      end;


  IncAnimStep;

  If (fTick + UID) mod COINS_GET_PACE = 0 then
    ProduceCoins;
end;


procedure TKMHouse.Paint;
var
  H: TKMHouseSpec;
  progress, stoneProgress: Single;
  I : Integer;
  H2 : TKMHouse;
begin
  H := gRes.Houses[fType];
  case fBuildState of
    hbsNoGlyph:; //Nothing
    {hbsWood:   begin

                  progress := fBuildingProgress / H.MaxWoodHealth;
                  //if there is no stone required then render it on different progress
                  if (gRes.Houses[fType].StoneCost = 0) and (gRes.Houses[fType].TileCost = 0) then
                  begin
                    If (progress > 0.5) then
                    begin
                      progress := (progress - 0.5) * 2;
                      gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, GetWoodPic, GetStonePic, -1, false, false, 0);
                    end
                    else begin
                      progress := progress * 2;
                      gRenderPool.AddHouse(fType, fPosition, progress, 0, 0, GetWoodPic)
                    end;
                  end
                  else
                    gRenderPool.AddHouse(fType, fPosition, progress, 0, 0, GetWoodPic);

                  gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                end;
    hbsStone:  begin
                  progress := (fBuildingProgress - H.MaxWoodHealth) / H.MaxStoneHealth;
                  gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, GetWoodPic, GetStonePic, -1, false, false, 0);
                  gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                end;}
    hbsWood,
    hbsStone :  begin
                  progress := EnsureRange(fBuildingProgress / H.MaxWoodHealth, 0, 1);//wood progress
                  stoneProgress := EnsureRange((fBuildingProgress - H.MaxWoodHealth) / H.MaxStoneHealth, 0, 1);
                  if (StoneCost = 0) and (TileCost = 0) then
                  begin
                    progress := progress * 2;
                    if progress > 1 then
                    begin
                      stoneProgress := progress - 1;
                      progress := 1;
                    end;

                  end;

                  gRenderPool.AddHouse(fType, fPosition, progress, stoneProgress, 0, GetWoodPic, GetStonePic, -1);
                  gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                end;
    else
                if IsUpgrading then
                begin
                  progress := 1 - EnsureRange(fLevel.BuildingProgress / gRes.Houses[fType].Levels[fLevel.CurrentLevel].Progress, 0, 1);
                  if fLevel.CurrentLevel > 0 then
                  begin
                    gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].StonePic, gRes.Houses[fType].Levels[fLevel.CurrentLevel].StonePic, -1, false, false, 0);
                    gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                  end else
                  begin
                    //gRenderPool.AddHouse(fType, fPosition, 1, 1, progress, GetStonePic, gRes.Houses[fType].Levels[fLevel.CurrentLevel].StonePic, -1, false, false, 0);
                    gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, GetStonePic, gRes.Houses[fType].Levels[fLevel.CurrentLevel].StonePic, -1, false, false, 0);
                    gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                  end;
                
                end else
                begin
                  //Incase we need to render house at desired step in debug mode
                  if HOUSE_BUILDING_STEP = 0 then
                  begin

                    if fIsOnTerrain <> tptNone then
                      gRenderPool.AddHouse(fType, fPosition, 1, 1, fSnowStep, -1, GetStonePic, GetSnowPic, false, false, 0)
                    else
                      gRenderPool.AddHouse(fType, fPosition, 1, 1, 0, -1, GetStonePic, -1, false, false, 0);

                      //AddHouse(H.HouseType, H.Position, 1, 1, 0, aDoImmediateRender, aDoHighlight, aHighlightColor, H.PicWariant);
                    if fStyle > 0 then
                    begin
                      if not HSpec.Styles[fStyle - 1].HideSupplies then
                        gRenderPool.AddHouseSupply(fType, fPosition, fWareIn, fWareOut, fWareOutPool);
                    end else
                      gRenderPool.AddHouseSupply(fType, fPosition, fWareIn, fWareOut, fWareOutPool);

                    if fType = htSmallStore then
                      gRenderPool.AddHouseAnimation(fPosition, gRes.Houses.Silo_Tablets, fSlotID, GetFlagColor);
                      //gRenderPool.AddAnimation(fPosition, gRes.Houses.Silo_Tablets, fSlotID, GetFlagColor, rxHouses);

                    if (CurrentLevel > 0) and (length(HSpec.Levels[CurrentLevel - 1].Anim) > 0) then
                    begin
                      if not HSpec.Levels[CurrentLevel - 1].HideSupplies then
                        gRenderPool.AddHouseSupply(fType, fPosition, fWareIn, fWareOut, fWareOutPool);

                      for I := 0 to High(HSpec.Levels[CurrentLevel - 1].Anim) do
                        gRenderPool.AddHouseAnimation(fPosition, HSpec.Levels[CurrentLevel - 1].Anim[I], WorkAnimStep, GetFlagColor);
                        //gRenderPool.AddAnimation(fPosition, HSpec.Levels[CurrentLevel - 1].Anim[I], WorkAnimStep, GetFlagColor, rxHouses);

                      if not HSpec.Levels[CurrentLevel - 1].ReplaceAnim then
                        if PaintHouseWork then
                          gRenderPool.AddHouseWork(fType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, GetFlagColor);


                    end else
                    if CurrentAction <> nil then
                      if PaintHouseWork then
                      gRenderPool.AddHouseWork(fType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, GetFlagColor);

                    If (gMySpectator.Selected = self) then
                    begin
                      for I := 0 to high(fDeliveryFromHouses) do
                        If fDeliveryFromHouses[I] <> nil then
                        begin
                          H2 := TKMHouse(fDeliveryFromHouses[I]);
                          gRenderAux.LineOnTerrain(KMPointF(Entrance.X - 0.5, Entrance.Y - 0.5),
                                                    KMPointF(H2.Entrance.X - 0.5, H2.Entrance.Y - 0.5),
                                                    $FFFF5500, $00FF);

                        end;

                      If (HouseToDeliver <> nil) then
                        gRenderAux.LineOnTerrain(KMPointF(Entrance.X - 0.5, Entrance.Y),
                                                  KMPointF(HouseToDeliver.Entrance.X - 0.5, HouseToDeliver.Entrance.Y),
                                                  $FF0000FF, $00FF);
                    end;
                  end
                  else
                    gRenderPool.AddHouse(fType, fPosition,
                      Min(HOUSE_BUILDING_STEP * 3, 1),
                      EnsureRange(HOUSE_BUILDING_STEP * 3 - 1, 0, 1),
                      Max(HOUSE_BUILDING_STEP * 3 - 2, 0)
                      ,GetWoodPic, GetStonePic, GetSnowPic
                      );
                end;
  end;

  if SHOW_POINTER_DOTS then
    gRenderAux.UnitPointers(fPosition.X + 0.5, fPosition.Y + 1, PointerCount);

  if fWasTookOver then
    gRenderPool.AddSpriteWH(fEntrance + KMPoint(0, 1), KMPoint(0, 0), 2541, rxHouses, gHands[Owner].FlagColor);//house flag

end;


{ THouseAction }
constructor TKMHouseAction.Create(aHouse: TKMHouse; aHouseState: TKMHouseState);
begin
  inherited Create;
  fHouse := aHouse;
  SetHouseState(aHouseState);
end;


procedure TKMHouseAction.SetHouseState(aHouseState: TKMHouseState);
begin
  fHouseState := aHouseState;
  case fHouseState of
    hstIdle:   begin
                  SubActionRem([haWork1..haSmoke]); //remove all work attributes
                  SubActionRem([haIdle]); //remove all work attributes

                  SubActionAdd([haIdle]);

                end;
    hstWork:   SubActionRem([haIdle]);
    hstEmpty:  begin
                SubActionRem([haIdle,haWork1..haWork5]);
               end;
  end;
end;


procedure TKMHouseAction.SubActionWork(aActionSet: TKMHouseActionType);
begin
  SubActionRem([haWork1..haWork5]); //Remove all work
  fSubAction := fSubAction + [aActionSet];
  fHouse.WorkAnimStep := 0;
  fHouse.WorkAnimStepPrev := 0;

end;


procedure TKMHouseAction.SubActionAdd(aActionSet: TKMHouseActionSet);
begin
  fSubAction := fSubAction + aActionSet;
end;


procedure TKMHouseAction.SubActionRem(aActionSet: TKMHouseActionSet);
begin
  fSubAction := fSubAction - aActionSet;
end;


procedure TKMHouseAction.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fHouse.UID);
  SaveStream.Write(fHouseState, SizeOf(fHouseState));
  SaveStream.Write(fSubAction, SizeOf(fSubAction));
end;


procedure TKMHouseAction.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fHouseState, SizeOf(fHouseState));
  LoadStream.Read(fSubAction, SizeOf(fSubAction));
end;


procedure TKMHouseAction.SyncLoad;
begin
  if Self = nil then Exit;

  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


constructor TKMHouseTower.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  if aHouseType = htWatchTower then
  begin
    RangeMax := RANGE_WATCHTOWER_MAX;
    RangeMin := RANGE_WATCHTOWER_MIN;
    ThrowingCycles := 50;
  end else
  begin
    RangeMax := RANGE_WALLTOWER_MAX;
    RangeMin := RANGE_WATLLTOWER_MIN;
    ThrowingCycles := 10;
  end;
end;

constructor TKMHouseTower.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(RangeMax);
  LoadStream.Read(RangeMin);
  LoadStream.Read(ThrowingCycles);
end;

procedure TKMHouseTower.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(RangeMax);
  SaveStream.Write(RangeMin);
  SaveStream.Write(ThrowingCycles);
end;

function TKMHouseTower.GetRangeMax: Single;
begin
  Result := RangeMax;
end;

procedure TKMHouseTower.Paint;
var
  fillColor, lineColor: Cardinal;
begin

  inherited;
  if fType = htWallTower then Exit;
  
  if SHOW_ATTACK_RADIUS or (mlTowersAttackRadius in gGameParams.VisibleLayers) or (self = gMySpectator.Selected) then
  begin
    fillColor := $40FFFFFF;
    lineColor := icWhite;
    if gMySpectator.Selected = Self then
    begin
      fillColor := icOrange and fillColor;
      lineColor := icCyan;
    end;
    gRenderPool.RenderDebug.RenderTiledArea(Position, RangeMin, RangeMax, GetLength, fillColor, lineColor);
  end;
end;


{ TKMHouseWPoint }
constructor TKMHouseWFlagPoint.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  inherited;

  fFlagPoint := PointBelowEntrance;
end;


constructor TKMHouseWFlagPoint.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseWFlagPoint');
  LoadStream.Read(fFlagPoint);
end;


procedure TKMHouseWFlagPoint.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseWFlagPoint');
  SaveStream.Write(fFlagPoint);
end;


function TKMHouseWFlagPoint.IsFlagPointSet: Boolean;
begin
  Result := not KMSamePoint(fFlagPoint, PointBelowEntrance);
end;


procedure TKMHouseWFlagPoint.SetFlagPoint(aFlagPoint: TKMPoint);
begin
  fFlagPoint := GetValidPoint(aFlagPoint);
end;


procedure TKMHouseWFlagPoint.ValidateFlagPoint;
begin
  //this will automatically update rally point to valid value
  fFlagPoint := GetValidPoint(fFlagPoint);
end;


function TKMHouseWFlagPoint.GetMaxDistanceToPoint: Integer;
begin
  Result := -1; //Unlimited by default
end;


function TKMHouseWFlagPoint.GetValidPoint(aPoint: TKMPoint): TKMPoint;
var
  L, R: Boolean;
  P: TKMPoint;
begin
  P := PointBelowEntrance;
  if not gTerrain.CheckPassability(P, tpWalk) then
  begin
    L := gTerrain.CheckPassability(KMPointLeft(P), tpWalk);
    R := gTerrain.CheckPassability(KMPointRight(P), tpWalk);
    //Choose random between Left and Right
    if L and R then
      P := KMPoint(P.X + 2*KaMRandom(2, 'TKMHouseWFlagPoint.GetValidPoint') - 1, P.Y) // Offset = +1 or -1
    else
    if L then
      P := KMPointLeft(P)
    else
    if R then
      P := KMPointRight(P)
    else
    begin
      Result := KMPOINT_ZERO;
      Exit;
    end;
  end;

  Result := gTerrain.GetPassablePointWithinSegment(P, aPoint, tpWalk, MaxDistanceToPoint);
end;


function TKMHouseWFlagPoint.ObjToString(const aSeparator: String = '|'): String;
begin
  Result := inherited ObjToString(aSeparator) +
            Format('%sFlagPoint = %s', [aSeparator, fFlagPoint.ToString]);
end;

function TKMHouseQuarry.GetMaxDistanceToPoint: Integer;
begin
  Result := gRes.Units[utStonemason].MiningRange;
end;

function TKMHouseQuarry.MiningRange(aUnitType: TKMUnitType): Integer;
begin
  Result := Inherited;
  If IsFlagPointSet then
    Result := Result div 4;
end;



constructor TKMHouseWallTower.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  //Exit;
  LoadStream.CheckMarker('HouseWallTower');
  LoadStream.Read(fBoltCount);
  LoadStream.Read(RightRecruit);
  LoadStream.Read(LeftRecruit);

end;

procedure TKMHouseWallTower.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('HouseWallTower');
  SaveStream.Write(fBoltCount);
  SaveStream.Write(RightRecruit);
  SaveStream.Write(LeftRecruit);
end;

function TKMHouseWallTower.CanMakeShot: Boolean;
begin
  Result := fBoltCount > 0;
end;

procedure TKMHouseWallTower.RemoveBolt;
begin
  fBoltCount := fBoltCount - 1;
  if fBoltCount <= 0 then
    if (fWareIn[1] > 0) or (DontNeedRes) then
    begin
      ProduceWare(fWareInput[1], -1);
      fBoltCount := 100;
    end;
end;

function TKMHouseWallTower.GetRangeMax: Single;
begin
  Result := Inherited;
  If gHands[Owner].ArmyDevUnlocked(34) then
    Result := Result + 2;
end;

procedure TKMHouseWallTower.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False);
begin
  Inherited;
  if gGameParams.IsMapEditor then  Exit;

  if fBoltCount <= 0 then
    if (fWareIn[1] > 0) or (DontNeedRes) then
    begin
      ProduceWare(fWareInput[1], -1);
      fBoltCount := 100;
    end;


end;

procedure TKMHouseWallTower.UpdateState(aTick: Cardinal);
begin
  Inherited;

  if (Self.WorkersCount = 0) or (TKMUnit(fWorkers[0]).InHouse <> self) or (TKMUnit(fWorkers[0]).Task is TKMTaskThrowRock) then
  begin
    RightRecruit := false;
    LeftRecruit := false;
    Exit;
  end;

  //if aTick mod 300 <> 0 then Exit;

  if WorkersCount = 1 then
  begin
    if LeftRecruit or RightRecruit then
      if aTick < fRecruitChangeSite then
        Exit;

    fRecruitChangeSite := aTick + 300;
    //make recruit to look from both windows
    if LeftRecruit then
    begin
      RightRecruit := true;
      LeftRecruit := false;
    end else
    begin
      RightRecruit := false;
      LeftRecruit := true;
    end;
  end else
  if WorkersCount = 2 then
  begin
    RightRecruit := (TKMUnit(fWorkers[0]).InHouse = self) and not (TKMUnit(fWorkers[0]).Task is TKMTaskThrowRock);
    LeftRecruit := (TKMUnit(fWorkers[1]).InHouse = self) and not (TKMUnit(fWorkers[1]).Task is TKMTaskThrowRock);
  end;
end;

procedure TKMHouseWallTower.Paint;
var
  fillColor, lineColor: Cardinal;
begin
  inherited;

  {if RightRecruit then
    gRenderPool.AddAnimation(Position, gRes.Houses.WallTower_RecruitRight, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
  if LeftRecruit then
    gRenderPool.AddAnimation(Position, gRes.Houses.WallTower_RecruitLeft, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
  }
  gRenderPool.AddHouseTowerRecruits(Position, RightRecruit, LeftRecruit, gTerrain.AnimStep, gHands[Owner].GameFlagColor);

  if SHOW_ATTACK_RADIUS or (mlTowersAttackRadius in gGameParams.VisibleLayers) or (self = gMySpectator.Selected) then
  begin
    fillColor := $40FFFFFF;
    lineColor := icWhite;
    if gMySpectator.Selected = Self then
    begin
      fillColor := icOrange and fillColor;
      lineColor := icCyan;
    end;
    if HouseType = htWallTower then
      gRenderPool.RenderDebug.RenderTiledArea(Position, RANGE_WATLLTOWER_MIN, RANGE_WALLTOWER_MAX, GetLength, fillColor, lineColor);
  end;

end;

procedure TKMHouseWell.DecProgress(aCount: Word);
begin
  Inc(fProgress, aCount);
end;

constructor TKMHouseWell.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('HouseTheWell');
  LoadStream.Read(fProgress);
  LoadStream.Read(WorkersInside);

end;

procedure TKMHouseWell.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('HouseTheWell');
  SaveStream.Write(fProgress);
  SaveStream.Write(WorkersInside);
end;

procedure TKMHouseWell.UpdateState(aTick: Cardinal);
  function MaxWater : Byte;
  begin
    Result := 1;
    If gHands[Owner].EconomyDevUnlocked(13) then
      Result := 2;
  end;

begin
  if not IsComplete then  Exit;
  if CheckWareOut(wtWater) >= MaxWater then
    Exit;
  if fProgress > 0 then
    Dec(fProgress);
  if fProgress = 0 then
  begin
    ProduceWare(wtWater, MaxWater);
    Inc(fProductionCycles[1]);
    if fProductionCycles[1] mod 50 = 0  then
    begin
      fProductionCycles[1] := fProductionCycles[1] - 50;
      gHands[Owner].VirtualWareTake('vtPearl', -1);
    end;
    fProgress := 200;
  end;

end;

procedure TKMHouseMerchant.ToggleSendToHand(aID: ShortInt);
begin
  Assert(InRange(aID, 0, MAX_HANDS - 1));

  If (gHands[Owner].Alliances[aID] = atAlly) then
    fSendToHand[aID] := not fSendToHand[aID];

  if not gGameParams.IsMapEditor then
    FindNextHand;
end;

function TKMHouseMerchant.GetSendToHand(aIndex : integer) : Boolean;
begin
  Assert(InRange(aIndex, 0, MAX_HANDS - 1));

  Result := fSendToHand[aIndex];
end;

procedure TKMHouseMerchant.SetSendToHand(aIndex: Integer; aValue: Boolean);
begin
  Assert(InRange(aIndex, 0, gHands.Count - 1));

  if gHands[Owner].Alliances[aIndex] = atAlly then
  begin
    fSendToHand[aIndex] := aValue;
    FindNextHand;
  end
  else
    fSendToHand[aIndex] := false;

end;

procedure TKMHouseMerchant.FindNextHand;
var I, C : Integer;
  hasSelected : Boolean;
begin
  if gGameParams.IsMapEditor then
    Exit;
  C := fCurrentHand + 1;
  //first check if anyone is selected
  hasSelected := false;
  for I := 0 to High(fSendToHand) do
    if fSendToHand[I] then
    begin
      hasSelected := true;
      break;
    end;

  if not hasSelected then
    fCurrentHand := Owner
  else
    for I := 0 to High(fSendToHand) do
    begin
      if C > High(fSendToHand) then
        C := 0;
      if C >= gHands.Count then
        C := 0;
      if (C <> Owner) then
        if gHands[C].Enabled then
          if fSendToHand[C] and (gHands[C].Houses.Stores.Count > 0) then
          begin
            fCurrentHand := C;

            if GetStore then
              Break;
          end;

      Inc(C);
    end;
  if fCurrentHand = Owner then
    fStore := nil;
  
end;

procedure TKMHouseMerchant.SendToAlly(aPlan: TKMWarePlan);
var I : Integer;
begin

  if fStore = nil then Exit;

  for I := Low(aPlan) to High(aPlan) do
    if aPlan[I].C > 0 then
      begin
        fStore.WareAddToIn(aPlan[I].W, aPlan[I].C);
        gHands[fStore.Owner].Stats.WareProduced(aPlan[I].W, aPlan[I].C);

        gScriptEvents.ProcMerchantTrade(self, fStore.Owner, aPlan[I].W, aPlan[I].C);
      end;
  fStore := nil;
  FindNextHand;
end;

function TKMHouseMerchant.GetStore: Boolean;
var I : Integer;
  H : TKMHouse;
  hList : array of TKMHouse;
begin
  fStore := nil;
  if fCurrentHand = Owner then
    Exit(false);

  SetLength(hList, 0);

  for I := 0 to gHands[fCurrentHand].Houses.Stores.Count - 1  do
  begin
    H := gHands[fCurrentHand].Houses.Stores[I];
    if H <> nil then
      if not H.IsDestroyed and H.IsComplete then
      begin
        SetLength(hList, length(hList) + 1);
        hList[high(hList)] := H;
      end;
  end;

  if length(hList) > 0 then
    fStore := hList[KamRandom(length(hList), 'TKMHouseMerchant.GetStore')];

  Result := fStore <> nil;

end;

function TKMHouseMerchant.CanWork : Boolean;
begin

  Result := (CheckWareIn(wtAll) >= 10)
              or ForceWorking
              or ((fWorkers[0] <> nil) and fWorkers[0].ToUnit.BootsAdded);



  Result := Result
             and (fCurrentHand <> Owner)
             and ((fStore <> nil)
                  or ((gHands[fCurrentHand].Stats.GetHouseQty(htShipYard) > 0)
                        and (gHands[Owner].Stats.GetHouseQty(htShipYard) > 0))
                  );
end;

constructor TKMHouseMerchant.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('HouseMerchant');
  LoadStream.Read(fProgress);
  LoadStream.Read(fCurrentHand);
  LoadStream.Read(fStore, 4);
  LoadStream.Read(fSendToHand, SizeOf(fSendToHand));

end;

procedure TKMHouseMerchant.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('HouseMerchant');
  SaveStream.Write(fProgress);
  SaveStream.Write(fCurrentHand);
  SaveStream.Write(fStore.UID);
  SaveStream.Write(fSendToHand, SizeOf(fSendToHand));
end;

procedure TKMHouseMerchant.PostLoadMission;
begin
  fCurrentHand := Owner;
  FindNextHand;
end;

procedure TKMHouseMerchant.SyncLoad;
begin
  Inherited;
  fStore := gHands.GetHouseByUID(Integer(fStore));

end;

procedure TKMHouseMerchant.Paint;
var aCount, I, offX, offY, Row, inRow, C: Integer;
begin
  Inherited;
  aCount := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    Inc(aCount, fWareIn[I]);

  if aCount > 0 then
  begin


    offX := 75;
    offY := -10;
    inRow := 4;
    Row := 0;
    C := 0;
    aCount := Max(aCount div 2, 1);

    for I := Min(aCount - 1, 9)  Downto 0 do
    begin
      Inc(C);
      if C mod inRow = 0 then
      begin
        C := 0;
        Inc(Row);
        case Row of
          1: inRow := 4;
          2: inRow := 4;
          3: inRow := 2;
          4: inRow := 4;
        end;
        Inc(offX, -4);

        case Row of
          1: Inc(offX, -8);
          2: Inc(offX, -4);
          3: Inc(offX, -12);
          4: Inc(offX, 0);
        end;
        Inc(offY, 6);
      end;

      gRenderPool.AddHouseMerchantChests(fPosition, -11 * (C mod inRow) + offX, offY);

    end;
  end;

  if aCount > 10 then
  begin
    offX := 24;
    offY := -12;
    inRow := 4;
    Row := 0;
    C := 0;

    for I := Min(aCount - 10, 9)  Downto 0 do
    begin
      Inc(C);
      if C mod inRow = 0 then
      begin
        C := 0;
        Inc(Row);
        case Row of
          1: inRow := 4;
          2: inRow := 3;
          3: inRow := 5;
          4: inRow := 2;
        end;
        Inc(offX, -4);

        case Row of
          1: Inc(offX, -8);
          2: Inc(offX, 6);
          3: Inc(offX, 6);
          4: Inc(offX, 0);
        end;
        Inc(offY, 6);
      end;
      gRenderPool.AddHouseMerchantChests(fPosition, -11 * (C mod inRow) + offX, offY);
    end;
  end;

  gRenderPool.AddAnimation(fPosition, gRes.Houses.Merchant_Tablets, fSlotID, GetFlagColor, rxHouses);
end;

function TKMHouseMerchant.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited ObjToString(aSeparator) +
            Format('%s fCurrentHand = %d', [aSeparator, fCurrentHand]);
end;

procedure TKMHouseMerchant.UpdateState(aTcik: Cardinal);
begin
  Inherited;
  if CurrentHand <> Owner then
    if not GetStore then
      FindNextHand;
end;

procedure TKMHouseAppleTree.AddDemandBuildingMaterials;
begin
  if ParentTree.IsValid then
  begin
    gHands[ParentTree.Owner].Deliveries.Queue.AddDemand(ParentTree, nil, wtTimber, WoodCost, dtOnce, diHigh5);
    gHands[ParentTree.Owner].Deliveries.Queue.AddDemand(ParentTree, nil, wtStone, StoneCost, dtOnce, diHigh5);
    gHands[ParentTree.Owner].Deliveries.Queue.AddDemand(ParentTree, nil, wtTile, TileCost, dtOnce, diHigh5);
  end else
    Inherited;
end;

procedure TKMHouseAppleTree.AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False; aFromParent : Boolean = false);
var I, C : Integer;
begin
  if aIsEditor or (gGameParams.Tick <= 1) then
  begin
    Inherited;
    Exit;
  end;
  if aFromParent then
    Inherited
  else
  if (fParentTree <> nil) then
    ParentTree.AddDamage(aAmount, aAttacker, aIsEditor, false)
  else
  begin
    C := (aAmount div 4) + 1;
    aAmount := aAmount div C;
    for I := 1 to C do
    begin
      IncLoop(fLastDamageID, 0, ChildCount, 1);
      if fLastDamageID = 0 then
        Inherited
      else
        ChildTree(fLastDamageID - 1).AddDamage(aAmount, aAttacker, aIsEditor, true);
    end;
  end;


end;
procedure TKMHouseAppleTree.UpdatePosition(const aPos: TKMPoint);
var aID, aFruit : Word;
begin
  Inherited;
  aID := gTerrain.Land[aPos.Y, aPos.X].Obj;
  aFruit := gMapElements[aID].IsFruit;
  if aFruit > 0 then
  begin
    FruitType := aFruit - 1;
    GrowPhase := gFruitTrees[aFruit - 1].GetStage(aID);
  end;


  RecheckParenting;
  //CheckForParentTree;
end;

function TKMHouseAppleTree.GetClosedForWorker: Boolean;
begin
  Result := Inherited;
  if ParentTree.IsValid then
    Result := true;
end;

function TKMHouseAppleTree.PaintHouseWork: Boolean;
begin
  Exit(false);
  {
  if fStartAnim > 0 then
    if fTick < fStartAnim + IfThen(fWorkAnim = haWork1, 50, (6 * 20)) then
      Result := false;

  if Result then
  for I := 0 to high(fChildTrees) do
    if ChildTree(I).IsValid then
      If ChildTree(I).fTick < ChildTree(I).fStartAnim + IfThen(ChildTree(I).fWorkAnim = haWork1, 50, (6 * 20)) then
        Result := false;}

end;

procedure TKMHouseAppleTree.CheckForParentTree;
var list : TKMPointDirList;
  I, K : Integer;
  tmp : TKMPointDir;
  H : TKMHouse;
begin
  list := TKMPointDirList.Create;
  GetListOfCellsAround(list, tpNone);

  //sort locations by distance
  for I := 0 to list.Count - 2 do
    for K := I to list.Count - 1 do
    if KMLength(Entrance, list[I].Loc) > KMLength(Entrance, list[K].Loc) then
    begin
      tmp := List[K];
      list[K] := List[I];
      list[I] := tmp;
    end;
  ParentTree := nil;
  for I := 0 to list.Count - 1 do
  begin
    H := gTerrain.House(list[I].Loc);
    if H.IsValid(htAppleTree) and not TAppleTree(H).fRechecking then
    begin
      if TAppleTree(H).ParentTree <> nil then
        ParentTree := TAppleTree(H).ParentTree
      else
        ParentTree := H;
      Break; //no need to look further
    end;
  end;


  list.Free;
end;

procedure TKMHouseAppleTree.RefreshChilds;
var tmp : TPointerArray;
  I : Integer;
begin
  SetLength(tmp, 0);
  for I := 0 to high(fChildTrees) do
    If TKMHouseAppleTree(fChildTrees[I]).IsValid then
    begin
      SetLength(tmp, length(tmp) + 1);
      tmp[high(tmp)] := fChildTrees[I];
    end;
  fChildTrees := tmp;
end;

procedure TKMHouseAppleTree.Demolish(aFrom: ShortInt; IsSilent: Boolean = False);
var I : Integer;
  newParent : TKMHouseAppleTree;
begin
  Inherited;

  If (aFrom = -1) or (aFrom = Owner) then
  begin
    If ParentTree = nil then
    begin
      newParent := nil;
      //Make first valid apple tree a new parent
      for I := 0 to ChildCount - 1 do
        if ChildTree(I).IsValid() then
        begin
          newParent := ChildTree(I);
          Break;
        end;
      If newParent = nil then
        Exit;
      newParent.SetParentTree(nil);
      //now add childs to this parent
      for I := 0 to ChildCount - 1 do
        if ChildTree(I).IsValid and (newParent <> ChildTree(I)) then
          ChildTree(I).SetParentTree(newParent);
    end else
      TKMHouseAppleTree(ParentTree).RefreshChilds;
  end else
  if ParentTree <> nil then
    ParentTree.Demolish(aFrom, IsSilent)
  else
    for I := 0 to ChildCount -1 do
      if ChildTree(I).IsValid() then
        ChildTree(I).Demolish(aFrom, IsSilent);


  //RecheckParenting;
end;

procedure TKMHouseAppleTree.ProduceWare(aWare: TKMWareType; aCount: Integer = 1);
begin
  if ParentTree.IsValid then
    ParentTree.ProduceWare(aWare, aCount)
  else
    Inherited;
end;

function TKMHouseAppleTree.PlaceRoad: Boolean;
begin
  Result := not ParentTree.IsValid;
end;

function TKMHouseAppleTree.WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1): Boolean;
var I : Integer;
  H : TKMHouse;
begin
  Result := false;
  if not IsComplete and NeedsWareToBuild(aWare) then
    Result := Inherited
  else
  begin
    for I := 0 to High(fChildTrees) do
    begin
      H := TKMHouse(fChildTrees[I]);
      if not H.IsComplete then
        if H.NeedsWareToBuild(aWare) then
        begin
          H.WareAddToBuild(aWare, aCount);
          Exit(true);
        end;

    end;
  end;
end;

procedure TKMHouseAppleTree.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False);
var I : Integer;
  H : TKMHouse;
begin
  //wtTimber, wtStone, wtTile is here for child trees
  if aWare in [wtTimber, wtStone, wtTile] then
    for I := 0 to High(fChildTrees) do
    begin
      H := TKMHouse(fChildTrees[I]);
      if not H.IsComplete then
        if H.NeedsWareToBuild(aWare) then
        begin
          H.WareAddToBuild(aWare, aCount);
          Exit;
        end;

    end;
  Inherited;

end;

function TKMHouseAppleTree.CanWork(aCheckChild : Boolean = false; aIncludeWater : Boolean = true): Integer;
var I, J : Integer;
  FW : Boolean;
begin
  Result := -1;
  fWorkingAtChild := -1;
  if IsComplete and (fProgress = 0) then
  begin
    if ParentTree.IsValid then
      J := ParentTree.CheckWareOut(wtApple)
    else
      J := CheckWareOut(wtApple);

    if ParentTree.IsValid then
      FW := ParentTree.ForceWorking
    else
      FW := ForceWorking;

    if (fGrowPhase in [0, 1, 2, 3]) and aIncludeWater then
      Result := 0;
    if Result <> 0 then
      if (fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1) then //tree has fruits, but do not gather them if we are full
        if (FW or (J < GetMaxOutWare)) then
          Result := 0;

  end;

  if Result > -1 then
    Exit;
  fWorkingAtChild := 0;
  if aCheckChild then
    if Result = -1 then //main tree doesn't need to work on. Check for childs
      for I := 0 to High(fChildTrees) do
        if ChildTree(I).IsValid then
          if (ChildTree(I).CanWork(false, aIncludeWater) = 0) then
          begin
            fWorkingAtChild := I + 1;
            Result := I + 1;
            Exit;
          end;
end;

function TKMHouseAppleTree.IncGrowPhase(aChildID : Integer = 0): Boolean;
begin
  Result := false;
  if aChildID > 0 then
  begin
    if ChildTree(aChildID - 1) <> nil then //this house might be destroyed;
      Result := ChildTree(aChildID - 1).IncGrowPhase;
    Exit;
  end;

  if (fProgress > 0) then //main tree is in progress
    Exit;

  if fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1 then //tree has fruits
  begin
    if fPhase > 0 then
    begin
      fGrowPhase := gFruitTrees[fFruitTreeID].MatureTreeStage; //tree can have more fruits
      Dec(fPhase);
    end
    else
    begin
      fPhase := 3 + KamRandom(5, 'TKMHouseAppleTree.SetProgress'); //make it sapling again

      If gHands[Owner].EconomyDevUnlocked(28) then
        fGrowPhase := gFruitTrees[fFruitTreeID].MatureTreeStage
      else
        fGrowPhase := 0;

      if fNextFruitTreeID <> fFruitTreeID then
      begin
        FruitType := fNextFruitTreeID;
        fGrowPhase := 0;
      end;
      gHands[Owner].VirtualWareTake('vtHerbs', -10)

    end;


    Result := true;
  end else
    Inc(fGrowPhase);



  SetProgress;
end;

procedure TKMHouseAppleTree.MakeFruits(aChildID : Integer = 0);
var count : Byte;
  add : Single;
begin
  if aChildID > 0 then
  begin
    if ChildTree(aChildID - 1) <> nil then //this house might be destroyed;
      ChildTree(aChildID - 1).MakeFruits;
    Exit;
  end;
  add := gFruitTrees[fFruitTreeID].Fruits * gFruitTrees[fFruitTreeID].ClimateMulti[gTerrain.FindBestClimatType(Entrance)];


  fFillFruits := fFillFruits + add;
  count := trunc(fFillFruits);
  if count > 0 then
  begin
    ProduceWare(wtApple, count);
    IncProductionCycle(wtApple, count);
  end;
  fFillFruits := fFillFruits - count;
end;


function TKMHouseAppleTree.NeedWater(aChildID : Integer = 0): Boolean;
begin
  If gGame.Params.MPMode = mmBottomless then
    Exit(false);
  if aChildID > 0 then
    if ChildTree(aChildID - 1) <> nil then //this house might be destroyed;
      Exit(ChildTree(aChildID - 1).NeedWater)
    else
      Exit(false);

  Result := fGrowPhase in [0, 1, 2, 3];

end;

procedure TKMHouseAppleTree.SetNextFruitType(aStep: Integer);
begin
  IncLoop(fNextFruitTreeID, 0, high(gFruitTrees), aStep);
end;

function TKMHouseAppleTree.GetFruitType: Byte;
begin
  Result := fNextFruitTreeID;
end;

procedure TKMHouseAppleTree.SetProgress(aIgnoreWater : Boolean = false);
var tc : TKMTerrainClimate;
begin
  if fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1 then //max
    fProgress := 0
  else
  If not NeedWater or aIgnoreWater then
  begin
    if fGrowPhase = 0 then
      if fFruitTreeID <> fNextFruitTreeID then
        FruitType := fNextFruitTreeID;

    fProgress := gFruitTrees[fFruitTreeID].ProgressPerStage + KamRandom(100, 'TKMHouseAppleTree.SetProgress');
    tc := gTerrain.FindBestClimatType(Entrance);
    fProgress := Round(fProgress / gFruitTrees[fFruitTreeID].ClimateMulti[tc]);
  end
  else
    fProgress := 0;
end;

procedure TKMHouseAppleTree.SetFruitTreeID(aValue: Integer);
begin
  if not gGameParams.IsMapEditor then
    gTerrain.RemoveObject(Entrance);
  fFruitTreeID := aValue;
  fNextFruitTreeID := fFruitTreeID;
end;

procedure TKMHouseAppleTree.SetGrowPhase(aValue: Byte);
begin
  fGrowPhase := aValue;
  if aValue > 0 then
    fProgress := gFruitTrees[fFruitTreeID].ProgressPerStage + KamRandom(100, 'TKMHouseAppleTree.SetProgress');
end;

function TKMHouseAppleTree.GetParentTree: TKMHouse;
begin
  Result := TKMHouse(fParentTree);
end;

procedure TKMHouseAppleTree.SetParentTree(aHouse: TKMHouse);
var I : Integer;
begin
  If fParentTree = self then
    fParentTree := nil
  else
    fParentTree := aHouse;

  if not IsValid then
    Exit;
  if fParentTree = nil then
  begin
    gTerrain.SetRoad(Entrance, Owner, rtStone);
    self.IsClosedForWorker := false;
  end;

  if not ParentTree.IsValid then //check if new parent is valid (is not nil)
    Exit;

  gTerrain.RemRoad(Entrance);
  for I := 0 to TKMHouseAppleTree(aHouse).ChildCount - 1 do
    if TKMHouseAppleTree(aHouse).fChildTrees[I] = self then //don't add the same house
      Exit;
  TKMHouseAppleTree(aHouse).AddChildTree(self);//add child
end;

procedure TKMHouseAppleTree.GetListOfCellsAround(aCells: TKMPointDirList; aPassability: TKMTerrainPassability);
  procedure AddLoc(X,Y: Word; Dir: TKMDirection);
  begin
    //Check that the passabilty is correct, as the house may be placed against blocked terrain
    if (aPassability = tpNone) or gTerrain.CheckPassability(KMPoint(X,Y), aPassability) then
      aCells.Add(KMPointDir(X, Y, Dir));
  end;
  procedure GetCellsFromTree(aID : Integer);
  var
    I, K: Integer;
    loc: TKMPoint;
    HA: TKMHouseAreaNew;
    H : TKMHouseAppleTree;
  begin
    If aID = -1 then
      H := self
    else
      H := ChildTree(aID);
    If not H.IsValid then
      Exit;
    loc := H.Position;
    HA := gRes.Houses[H.HouseType].BuildArea;

    for I := 1 to MAX_HOUSE_SIZE do for K := 1 to MAX_HOUSE_SIZE do
    if HA[I,K] <> 0 then
    begin
      if (I = 1) or (HA[I-1,K] = 0) then
        AddLoc(loc.X + K - 3, loc.Y + I - 4 - 1, dirS); //Above
      if (I = MAX_HOUSE_SIZE) or (HA[I+1,K] = 0) then
        AddLoc(loc.X + K - 3, loc.Y + I - 4 + 1, dirN); //Below
      if (K = MAX_HOUSE_SIZE) or (HA[I,K+1] = 0) then
        AddLoc(loc.X + K - 3 + 1, loc.Y + I - 4, dirW); //FromRight
      if (K = 1) or (HA[I,K-1] = 0) then
        AddLoc(loc.X + K - 3 - 1, loc.Y + I - 4, dirE); //FromLeft
    end;
  end;
var I : integer;
begin
  If ParentTree <> nil then
    ParentTree.GetListOfCellsAround(aCells, aPassability)
  else
  begin
    aCells.Clear;
    GetCellsFromTree(-1);//main tree
    for I := 0 to ChildCount - 1 do
      GetCellsFromTree(I);//child trees
  end;

end;

procedure TKMHouseAppleTree.GetListOfCellsWithin(aCells: TKMPointList);
  procedure GetCellsFromTree(aID : Integer);
  var
    I, K: Integer;
    loc: TKMPoint;
    houseArea: TKMHouseAreaNew;
    H : TKMHouseAppleTree;
  begin
    If aID = -1 then
      H := self
    else
      H := ChildTree(aID);
    If not H.IsValid then
      Exit;
    loc := H.Position;
    houseArea := gRes.Houses[H.HouseType].BuildArea;

    for I := Max(loc.Y - 3, 1) to loc.Y do
      for K := Max(loc.X - 2, 1) to Min(loc.X + 1, gTerrain.MapX) do
        if houseArea[I - loc.Y + 4, K - loc.X + 3] in [1, 2] then
          aCells.Add(KMPoint(K, I));
  end;

var I : integer;

begin
  If (self = nil) or IsDestroyed then
    Exit;
  If ParentTree <> nil then
    ParentTree.GetListOfCellsWithin(aCells)
  else
  begin
    aCells.Clear;
    GetCellsFromTree(-1);//main tree
    for I := 0 to ChildCount - 1 do
      GetCellsFromTree(I);//child trees
  end;

end;

function TKMHouseAppleTree.CanDeliverToAnyPoint(aWare : TKMWareType) : Boolean;
begin
  Result := aWare in [wtTimber, wtStone, wtTile, wtAll];
end;

procedure TKMHouseAppleTree.SetAnimation(aChildID : Integer = -1);
begin
  if aChildID > 0 then
  begin
    if ChildTree(aChildID - 1) <> nil then //this house might be destroyed;
      ChildTree(aChildID - 1).SetAnimation;
  end;

  if CanWork = 0 then //first check if main tree can work
  begin
    if NeedWater then
      fWorkAnim := haWork1
    else
      fWorkAnim := haWork2;

    case fWorkAnim of
      haWork1: fStartAnim := fTick;
      haWork2: fStartAnim := fTick;
    end;
    SetProgress(true);
  end;

end;

function TKMHouseAppleTree.ChildTree(aIndex: Integer): TKMHouseAppleTree;
begin
  if InRange(aIndex, 0, high(fChildTrees)) then
    Result := TKMHouseAppleTree(fChildTrees[aIndex])
  else
    Result := nil;
end;


function TKMHouseAppleTree.ChildCount: Integer;
begin
  Result := Length(fChildTrees);
end;

Procedure TKMHouseAppleTree.AddChildTree(aTree: Pointer);
begin
  SetLength(fChildTrees, ChildCount + 1);
  fChildTrees[high(fChildTrees)] := aTree;
end;

function TKMHouseAppleTree.CanAddChildTree(X: Integer; Y: Integer): Boolean;
Const MAX_TREES_COUNT = 30;
begin
  if IsNil(self) then Exit(false);
  if ParentTree = nil then
    Result := (ChildCount < MAX_TREES_COUNT){ and ((X <> Entrance.X) or (Y < Entrance.Y))}
  else
    Result := TKMHouseAppleTree(ParentTree).CanAddChildTree(X, Y);
end;

function TKMHouseAppleTree.CanAddChildTree(aLoc : TKMPoint): Boolean;
begin
  Result := CanAddChildTree(aLoc.X, aLoc.Y)
end;

procedure TKMHouseAppleTree.RecheckParenting;
var I : Integer;
begin
  if ParentTree <> nil then
  begin
    //redirect it to parent
    TAppleTree(ParentTree).RecheckParenting;
  end else
  begin
    //this tree is a parent and is not destroyed
    if IsValid then
    begin

    end else
    //we need to recheck every connected appletree
    //so check first if this one is the parent
    if length(fChildTrees) > 0 then
    begin

      //clear parent in all childs
      for I := 0 to length(fChildTrees) - 1 do
      begin
        ChildTree(I).ParentTree := nil;
        ChildTree(I).fRechecking := true;
      end;

      //parent to new main tree
      for I := 0 to length(fChildTrees) - 1 do
        //if I <> main then
          if ChildTree(I).IsValid then
          begin
            //ChildTree(I).ParentTree := ChildTree(main);
            ChildTree(I).CheckForParentTree;
            ChildTree(I).fRechecking := false;
          end;
          //ChildTree(I).CheckForParentTree;}
    end;
  end;
  
end;

function TKMHouseAppleTree.ShowUnoccupiedMSG: Boolean;
begin
  Result := not IsClosedForWorker;
end;

constructor TKMHouseAppleTree.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
begin
  fStartAnim := 0;
  fLastDamageID := 0;
  inherited;
  CheckForParentTree;
  fTmpObject := gTerrain.Land[Entrance.Y, Entrance.X].Obj;
  fRechecking := false;
end;

procedure TKMHouseAppleTree.Activate(aWasBuilt: Boolean);
var obj : Word;
    I, K : Integer;
    TC : TKMTerrainClimate;
begin
  fProgress := 0;
  fGrowPhase := 0;
  fFillFruits := 0;
  FruitType := 0;

  fPhase := 2 + KamRandom(3, 'TKMHouseAppleTree.SetProgress');
  if fTmpObject <> 0 then
    obj := fTmpObject
  else
    obj := gTerrain.Land[Entrance.Y, Entrance.X].Obj;

  if (obj <> OBJ_NONE) and (gMapElements[obj].IsFruit > 0) then
  begin
    fFruitTreeID := gMapElements[obj].IsFruit - 1;
    fNextFruitTreeID := fFruitTreeID;

    SetGrowPhase(gFruitTrees[fFruitTreeID].GetStage(obj));

    if not gGameParams.IsMapEditor then
      gTerrain.RemoveObject(Entrance);
  end else
  begin
    fFruitTreeID := 0;
    fNextFruitTreeID := fFruitTreeID;
    if aWasBuilt then
    begin
      SetGrowPhase(0);
    end else
    begin
      SetGrowPhase(gFruitTrees[fFruitTreeID].MatureTreeStage);
    end;
  end;
  if not gGameParams.IsMapEditor then
    gTerrain.RemoveObject(Entrance);

  fNextFruitTreeID := fFruitTreeID;

  if gHands[Owner].IsComputer then
    if aWasBuilt then
      if fFruitTreeID = 0 then
      begin
        K := 0;
        for I := 0 to High(gFruitTrees) do
        begin
          TC := gTerrain.FindBestClimatType(Entrance);

          if gFruitTrees[I].ClimateMulti[TC] * gFruitTrees[I].Fruits > gFruitTrees[K].ClimateMulti[TC] * gFruitTrees[I].Fruits then
            K := I;
        end;
        fNextFruitTreeID := K;
      end;


  Inherited;
end;

constructor TKMHouseAppleTree.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
begin
  Inherited;
  LoadStream.CheckMarker('HouseAppleTree');
  LoadStream.Read(fProgress);
  LoadStream.Read(fPhase);
  LoadStream.Read(fGrowPhase);
  LoadStream.Read(fFruitTreeID);
  LoadStream.Read(fNextFruitTreeID);
  LoadStream.Read(fFillFruits);
  LoadStream.Read(fWorkAnim, SizeOf(fWorkAnim));
  LoadStream.Read(fStartAnim);
  LoadStream.Read(fWorkingAtChild);
  LoadStream.Read(fTmpObject);
  LoadStream.Read(fParentTree, 4);

  LoadStream.Read(newCount);
  SetLength(fChildTrees, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fChildTrees[I], 4);



end;

procedure TKMHouseAppleTree.Save(SaveStream: TKMemoryStream);
var newCount, I : Integer;
begin
  Inherited;
  SaveStream.PlaceMarker('HouseAppleTree');
  SaveStream.Write(fProgress);
  SaveStream.Write(fPhase);
  SaveStream.Write(fGrowPhase);
  SaveStream.Write(fFruitTreeID);
  SaveStream.Write(fNextFruitTreeID);
  SaveStream.Write(fFillFruits);
  SaveStream.Write(fWorkAnim, SizeOf(fWorkAnim));
  SaveStream.Write(fStartAnim);
  SaveStream.Write(fWorkingAtChild);
  SaveStream.Write(fTmpObject);
  SaveStream.Write(TKMHouse(fParentTree).UID);

  newCount := ChildCount;
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMHouse(fChildTrees[I]).UID);
end;

procedure TKMHouseAppleTree.SyncLoad;
var I : Integer;
begin
  Inherited;
  for I := 0 to high(fChildTrees) do
    fChildTrees[I] := TKMHouse(gHands.GetHouseByUID(Integer(fChildTrees[I]))  );

  fParentTree := TKMHouse(gHands.GetHouseByUID(Integer(fParentTree)));

end;

procedure TKMHouseAppleTree.UpdateDemands;
begin
  if ParentTree.IsValid then
    Exit;
  Inherited;
end;

procedure TKMHouseAppleTree.UpdateState(aTick: Cardinal);
begin
  Inherited;
  if not IsComplete then
    Exit;

  if fProgress > 0 then
  begin
    Dec(fProgress);
    if fProgress = 0 then
      IncGrowPhase;
    Exit;
  end;

end;

function TKMHouseAppleTree.ObjToString(const aSeparator: String = '|'): String;
begin
  Result := Inherited ObjToString(aSeparator) + Format('%s fProgress: %d%s fGrowPhase: %d%s fPhase: %d%s fWorkingAtChild %d%s CanWork: %d%s HasParent: %s', [aSeparator, fProgress, aSeparator, fGrowPhase, aSeparator, fPhase, aSeparator, fWorkingAtChild, aSeparator, CanWork(true), aSeparator, BoolToStr(ParentTree <> nil, true)])

end;

procedure TKMHouseAppleTree.Paint;
begin
  Inherited;

  if not IsComplete then
    Exit;

  gRenderPool.AddHouseSupply(HouseType, fPosition, [0, CheckWareIn(wtWater), 0, 0], [CheckWareOut(wtApple), 0, 0, 0], []);

  if CurrentAction <> nil then
  begin
    gRenderPool.AddHouseWork(HouseType, fPosition,
                            CurrentAction.SubAction * [haFire1..haFire8],
                            WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);
    If ParentTree = nil then
    begin
      gRenderPool.AddHouseWork(HouseType, fPosition,
                              CurrentAction.SubAction - [haWork1, haWork2],
                              WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);

      gRenderPool.AddSpriteWH(fPosition, KMPOINT_ZERO, 2208, rxHouses, gHands[Owner].GameFlagColor);//log
    end;
  end;

  if fStartAnim > 0 then
    if fTick < fStartAnim + IfThen(fWorkAnim = haWork1, 50, (6 * 20)) then
      gRenderPool.AddHouseWork(HouseType, fPosition,
                              [fWorkAnim],
                              Abs(fTick - fStartAnim), WorkAnimStepPrev, GetFlagColor);
  gRenderPool.RenderMapElement(gFruitTrees[fFruitTreeID].Stage[fGrowPhase], gTerrain.AnimStep, Entrance.X, Entrance.Y, false, false ,false);
end;

constructor TKMHousePottery.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fStoredClay := 0;
end;
constructor TKMHousePottery.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('HousePottery');

  LoadStream.Read(fTiles, SizeOf(fTiles));
  LoadStream.Read(fStoredClay);

end;

procedure TKMHousePottery.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('HousePottery');
  SaveStream.Write(fTiles, SizeOf(fTiles));
  SaveStream.Write(fStoredClay);
end;

function TKMHousePottery.GetMaxDistanceToPoint: Integer;
begin
  If gHands[Owner].BuildDevUnlocked(31) then
    Result := 10
  else
    Result := 0;
end;

function TKMHousePottery.TakeTile : Byte;
var I : Integer;
begin
  Result := 0;

  for I := low(fTiles) to High(fTiles) do
    If (fTiles[I] > 0) and (gGame.Params.Tick > fTiles[I]) then
    begin
      fTiles[I] := 0;
      Exit(I);
    end;
end;

function TKMHousePottery.MiningRect(aWare : TKMWareType) : TKMRect;
begin
  //the OG rect
  //wtTile:     Result := KMRect(9,  7, 10, 9);
  Result := Inherited;
  If IsFlagPointSet then
    Result := KMRect(3, 3, 3, 3);
end;

procedure TKMHousePottery.BringTile;
var I : Integer;
begin
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] = 0 then
    begin
      If gHands[Owner].BuildDevUnlocked(7) then
        fTiles[I] := gGame.Params.Tick + 1400
      else
        fTiles[I] := gGame.Params.Tick + 1800;
      Exit;
    end;
  Inc(fStoredClay);

end;

function TKMHousePottery.CanTakeTile: Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fTiles) to High(fTiles) do
    If (fTiles[I] > 0) and (gGame.Params.Tick > fTiles[I]) then
      Exit(true);

end;

function TKMHousePottery.HasSpaceForNextTile: Boolean;
var I : Integer;
begin
  Result := false;
  //Result := fStoredClay < MAX_CLAY_TO_STORE;
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] = 0 then
      Exit(true);
end;

function TKMHousePottery.HasAnyTile : Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] > 0 then
      Exit(true);
end;

function TKMHousePottery.HasClayStored: Boolean;
begin
  Result := fStoredClay > 0;
end;

function TKMHousePottery.HasTileOrClay: Boolean;
begin
  Result := HasAnyTile or HasClayStored;
end;

function TKMHousePottery.CanUseStoredClay: Boolean;
begin
  Result := (fStoredClay > 0) and HasSpaceForNextTile;
end;

function TKMHousePottery.CanStoreClay: Boolean;
begin
  Result := (fStoredClay < MAX_CLAY_TO_STORE) or HasSpaceForNextTile;
end;

procedure TKMHousePottery.UseStoredClay;
begin
  If fStoredClay > 0 then
  begin
    Dec(fStoredClay);
    BringTile;
  end;
end;

function TKMHousePottery.FilledClay: Single;
begin
  Result := fStoredClay / MAX_CLAY_TO_STORE;
end;

procedure TKMHousePottery.Paint;
var I : Integer;
begin
  Inherited;

  for I := 1 to high(fTiles) do
    if fTiles[I] > 0 then
      gRenderPool.AddHousePotteryTiles(fPosition, I);
end;

function TKMHousePottery.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited+ Format('%sfTiles : %d, %d, %d, %d %s'
                              + 'fStoredClay : %d',
                              [aSeparator,
                              fTiles[1], fTiles[2], fTiles[3], fTiles[4], aSeparator,
                              fStoredClay
                              ]
                              );
end;

function TKMHouseCollectors.GetMaxDistanceToPoint: Integer;
begin
  Result := 35;
end;

procedure TKMHouseCollectors.WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1);
begin
  fWaresOut.AddWareOut(aWare, aCount);
  gHands[Owner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
end;

function TKMHouseCollectors.CheckWareOut(aWare: TKMWareType): Word;
begin
  Result := fWaresOut.HasWare(aWare);
end;

procedure TKMHouseCollectors.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var index : Integer;
begin
  index := fWaresOut.IndexOf(aWare, 1);
  Assert(index >= 0);
  {if index = -1 then
    Exit;}

  if aFromScript then
  begin
    aCount := Min(aCount, fWaresOut[index].C);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;

  Assert(aCount <= fWaresOut[index].C);

  fWaresOut[index].C := fWaresOut[index].C - aCount;
  fWaresOut.DeleteEmpty;
end;

function TKMHouseCollectors.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWaresOut.Count - 1 do
    if (fWaresOut[I].W = aWare) and (fWaresOut[I].C > 0) then
      Result := fWaresOut[I].C >= aCount;

end;
procedure TKMHouseCollectors.FillMeat(aObjType: Word);
var fillSkin : Single;
  UT : TKMUnitType;
begin
  if aObjType = OBJ_NONE then
    Exit;
  case aObjType of
    540: UT := utWolf;//wolf
    541: UT := utDeerMale;//deer male
    542: UT := utDeerFemale;//deer female
    543: UT := utFox;//fox
    544: UT := utBoar;//boar
    545: UT := utBear;//bear
    546: UT := utLandDuck;//duck
    547: UT := utRabbit;//rabbit
    548: UT := utWhiteBear;//PolarBear
    else
      Exit;
  end;
  case aObjType of
    540: Inc(fFill, 0.4);//wolf
    541: Inc(fFill, 1.2);//deer male
    542: Inc(fFill, 1);//deer female
    543: Inc(fFill, 0.30);//fox
    544: Inc(fFill, 1.4);//boar
    545: Inc(fFill, 2);//bear
    546: Inc(fFill, 0.2);//duck
    547: Inc(fFill, 0.2);//rabbit
    548: Inc(fFill, 2);//polar bear
    else
      Exit;
  end;

  fillSkin := fFill * 2;

  If gHands[Owner].EconomyDevUnlocked(5) then
    fFill := fFill * 1.25;

  ProduceWareFromFill(wtPig, fFill);
  if Ut = utLandDuck then
    ProduceWareFromFill(wtFeathers, fillSkin)
  else
    ProduceWareFromFill(wtSkin, fillSkin);
end;

procedure TKMHouseCollectors.SetNextMode;
begin
  if fMode = cmCollector then
    fMode := cmHunter
  else
    fMode := cmCollector;
end;

constructor TKMHouseCollectors.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fWaresOut.Clear;
  fMode := cmCollector;
  fFill := 0;
end;

constructor TKMHouseCollectors.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  fWaresOut.Load(LoadStream);
  LoadStream.Read(fMode, Sizeof(fMode));
  LoadStream.Read(fFill);
end;

procedure TKMHouseCollectors.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  fWaresOut.Save(SaveStream);
  SaveStream.Write(fMode, Sizeof(fMode));
  SaveStream.Write(fFill);
end;

constructor TKMHousePalace.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
begin
  Inherited;

  {SetLength(fProgress, length(PALACE_UNITS_ORDER));
  SetLength(fPhase, length(PALACE_UNITS_ORDER));
  SetLength(fWait, length(PALACE_UNITS_ORDER));
  SetLength(fOrderCount, length(PALACE_UNITS_ORDER));}
  //SetLength(VWareIDs, length(PALACE_UNITS_ORDER));
  fTrainingID := NO_TRRAINING_ID;
end;

function TKMHousePalace.GetMaxDistanceToPoint: Integer;
begin
  Result := 999;
end;

procedure TKMHousePalace.Activate(aWasBuilt : Boolean);
begin
  Inherited;

  //if aWasBuilt then
  //  gTerrain.PalaceExploreDeposits(Entrance);

end;

function TKMHousePalace.UnitProgress(aType: TKMUnitType): Word;
begin
  Result := Max(gRes.Units[aType].PalaceCost.PhaseDuration, 50);
  If gHands[Owner].HasPearl(ptValtaria) then
    Result := Round(Result * 0.9);

  If (aType = utSpy) and gHands[Owner].ArmyDevUnlocked(0) then
    Result := Round(Result * 0.8);

  If gHands[Owner].ArmyDevUnlocked(4) then
    Result := Round(Result * 0.9);
end;

function TKMHousePalace.GetPhaseCount(aType : TKMUnitType) : Byte;
begin
  Result := gRes.Units[aType].PalaceCost.PhaseCount;

  If gHands[Owner].ArmyDevUnlocked(5) then
    Result := Max(Result - 1, 1);
  If gHands[Owner].ArmyDevUnlocked(36) then
    Result := 1;
end;

function TKMHousePalace.GetCurrentPhase : Byte;
begin
  Result := fPhase;
end;

constructor TKMHousePalace.Load(LoadStream: TKMemoryStream);
//var I, newCount : Integer;
//  tmp : Word;
begin
  Inherited;


 { SetLength(fProgress, length(PALACE_UNITS_ORDER));
  SetLength(fPhase, length(PALACE_UNITS_ORDER));
  SetLength(fWait, length(PALACE_UNITS_ORDER));
  SetLength(fOrderCount, length(PALACE_UNITS_ORDER));
  SetLength(VWareIDs, length(PALACE_UNITS_ORDER));}

  LoadStream.CheckMarker('HousePalace');
  LoadStream.Read(fTrainingID);
  LoadStream.Read(fProgress);
  LoadStream.Read(fPhase);
  LoadStream.Read(fWait);
  LoadStream.Read(fOrderCount);
  {LoadStream.Read(newCount);

  for I := 0 to newCount - 1 do
  begin
    if I > high(fProgress) then
    begin
      LoadStream.Read(tmp);
      LoadStream.Read(tmp);
      LoadStream.Read(tmp);
      LoadStream.Read(tmp);
    end else
    begin
      LoadStream.Read(fProgress[I]);
      LoadStream.Read(fPhase[I]);
      LoadStream.Read(fWait[I]);
      LoadStream.Read(fOrderCount[I]);
    end;
  end;}
end;

procedure TKMHousePalace.Save(SaveStream: TKMemoryStream);
//var I, newCount : Integer;
begin
  Inherited;
  SaveStream.PlaceMarker('HousePalace');

  //newCount := length(fProgress);
  SaveStream.Write(fTrainingID);
  SaveStream.Write(fProgress);
  SaveStream.Write(fPhase);
  SaveStream.Write(fWait);
  SaveStream.Write(fOrderCount);
  {SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(fProgress[I]);
    SaveStream.Write(fPhase[I]);
    SaveStream.Write(fWait[I]);
    SaveStream.Write(fOrderCount[I]);
  end;}

end;

function  TKMHousePalace.GetOrderCount(aIndex : Integer) : Word;
begin
  //Assert(aIndex < length(fOrderCount), 'Wrong Index');
  If aIndex = fTrainingID then
    Result := fOrderCount
  else
    Result := 0;
end;

procedure TKMHousePalace.SetOrderCount(aIndex : Integer; aValue : Word);
begin
  //Assert(aIndex < length(fOrderCount), 'Wrong Index');

  {for I := 0 to High(fOrderCount) do
    if  I <> aIndex then
      fOrderCount[I] := 0;

  //if FullProgress[aIndex] > 0 then
  //  Exit;
  fOrderCount[aIndex] := EnsureRange(aValue, 0, high(fOrderCount[aIndex]));}
  If aIndex <> fTrainingID then
    fTrainingID := aIndex;
  fOrderCount := EnsureRange(aValue, 0, high(fOrderCount));
  If fOrderCount = 0 then
    fTrainingID := NO_TRRAINING_ID;
end;

function  TKMHousePalace.GetMaxProgress : Integer;
var UT : TKMUnitType;
begin
  //Assert(aIndex < length(fProgress), 'Wrong Index');
  Result := 0;
  If  fTrainingID = NO_TRRAINING_ID then
    Exit;
  UT := PALACE_UNITS_ORDER[fTrainingID];
  Result := UnitProgress(UT) * Max(GetPhaseCount(UT), 1);

end;

function  TKMHousePalace.GetFullProgress : Single;
var currProg, phaseProg : Integer;
    UT : TKMUnitType;
begin
  //Assert(aIndex < length(fProgress), 'Wrong Index');
  Result := 0;


  if (fProgress = 0) and (fPhase = 0) then
    Exit(0);
  If  fTrainingID = NO_TRRAINING_ID then
    Exit;

  UT := PALACE_UNITS_ORDER[fTrainingID];

  currProg := UnitProgress(UT) * fPhase;
  phaseProg := 0;
  If fWait = 0 then
    phaseProg := UnitProgress(UT) - fProgress;
  currProg := currProg + phaseProg;
  Result := currProg / GetMaxProgress;

end;

function  TKMHousePalace.GetPhaseProgress{(aIndex: Integer)}: Single;
begin
  //Assert(aIndex < length(fProgress), 'Wrong Index');
  Result := 0;
  if ((fProgress = 0) and (fPhase  = 0)) or (fWait > 0) then
    Exit(0);
  If  fTrainingID = NO_TRRAINING_ID then
    Exit;

  Result := 1 - fProgress / UnitProgress(PALACE_UNITS_ORDER[fTrainingID]);
end;

function TKMHousePalace.GetPhaseColor : Cardinal;
begin
  Result := MixColor(icCyan, icGoldenYellow, GetFullProgress);
end;

function TKMHousePalace.IsTraining: Boolean;
begin
  Result := fTrainingID <> NO_TRRAINING_ID;
end;
function TKMHousePalace.TrainingInProgress : Boolean;
begin
  Result := IsTraining and ((fProgress > 0) or (fPhase > 0));
end;

procedure TKMHousePalace.CancelOrder;
begin
  //Assert(aIndex < length(fProgress), 'Wrong Index');

  fProgress := 0;
  fPhase := 0;
  fWait := 0;
  fOrderCount := 0;
  fTrainingID := NO_TRRAINING_ID;
end;

function TKMHousePalace.GetWarePlan: TKMWarePlan;
var I : Integer;
  WP : TKMWarePlan;
begin
  if fTrainingID = NO_TRRAINING_ID then
    Exit;
  //if (fProgress = 0) and (fPhase = 0) then
  //  Exit;

  WP := gRes.Units[PALACE_UNITS_ORDER[fTrainingID]].PalaceCost.Plan;
  Result.SetCount(WP.Count, true);
  for I := Low(wp) to High(wp) do
    if (wp[I].W <> wtNone) then
    begin
      Result[I].W := wp[I].W;
      Result[I].C := wp[I].C;
    end;
end;

function TKMHousePalace.GetWarePlanOf(aIndex : Integer): TKMWarePlan;
var I : Integer;
  WP : TKMWarePlan;
begin
  if aIndex = NO_TRRAINING_ID then
    Exit;
  if fProgress > 0 then
    Exit;

  WP := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.Plan;
  Result.SetCount(WP.Count, true);
  for I := Low(wp) to High(wp) do
    if (wp[I].W <> wtNone) then
    begin
      Result[I].W := wp[I].W;
      Result[I].C := wp[I].C;
    end;
end;

{
function TKMHousePalace.GetFullCost(aIndex : Integer = -1): TKMWarePlan;
var I : Integer;
  WP : TKMWarePlan;
begin
  if aIndex = -1 then
    for I := 0 to High(fProgress) do
      if (fProgress[I] > 0) or (fPhase[I] > 0) or (fOrderCount[I] > 0) then
      begin
        aIndex := I;
        break;
      end;

  wp := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.Plan;

  Result.SetCount(wp.Count, true);
  for I := Low(wp) to High(wp) do
    if (wp[I].W <> wtNone) then
    begin
      Result[I].W := wp[I].W;
      Result[I].C := wp[I].C + 1 * (gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.PhaseCount - 1);
    end;
end;
}
function TKMHousePalace.HasWares(aIndex: Integer): Boolean;
var K : Integer;
    UT : TKMUnitType;
    WP : TKMWarePlan;
begin
  If aIndex = NO_TRRAINING_ID then
    Exit(false);
  UT := PALACE_UNITS_ORDER[aIndex];
  Result := True;
  if GetPhaseCount(UT) = 0 then
    Exit;

  if fPhase = 0 then
    for K := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
      with gRes.Units[UT].PalaceCost.Wares[K] do
        if gHands[Owner].VirtualWare[Index] < C then
          Exit(false);

  WP := GetWarePlan;

  for K := 0 to WP.Count - 1 do
    with WP[K] do
      if (W <> wtNone) and (C > 0) then
        if CheckWareIn(W) < IfThen(fPhase = 0, C, 1) then
          Exit(false);
end;

procedure TKMHousePalace.TakeWares;
var K : Integer;
  UT : TKMUnitType;
  WP : TKMWarePlan;
begin
  UT := PALACE_UNITS_ORDER[fTrainingID];
  if GetPhaseCount(UT) = 0 then
    Exit;

  if fPhase = 0 then
    for K := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
      with gRes.Units[UT].PalaceCost.Wares[K] do
        gHands[Owner].VirtualWareTake(W, C);

  WP := GetWarePlan;
  for K := 0 to 3 do
    with WP[K] do
      if (W <> wtNone) and (C > 0) then
        WareTakeFromIn(W,IfThen(fPhase = 0, C, 1));

end;

function TKMHousePalace.CanEquip(aIndex: Integer): Boolean;
begin
  Result := HasWares(aIndex);
end;
function TKMHousePalace.TrainedUnitType : TKMUnitType;
begin
  if IsTraining then
    Result := PALACE_UNITS_ORDER[fTrainingID]
  else
    Result := utNone;
end;
function TKMHousePalace.TrainedUnitID : Byte;
begin
  Result := fTrainingID
end;

procedure TKMHousePalace.UpdateState(aTick: Cardinal);
var U : TKMUnit;
  doProgress : Boolean;
begin
  Inherited;

  if fWait > 0 then
    Dec(fWait);
  If fWait > 0 then
    Exit;

  if fProgress > 0 then
  begin
    Dec(fProgress);

    if fProgress = 0 then
    begin
      fWait := 100;
      Inc(fPhase);

      if fPhase = GetPhaseCount(TrainedUnitType) then
      begin
        ProduceFestivalPoints(fptWarfare, 10 * fPhase);
        gSoundPlayer.Play(sfxnPalace, fPosition);
        U := gHands[Owner].TrainUnit(TrainedUnitType, Self);
        U.Visible := False; //Make him invisible as he is inside the barracks
        U.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
        //Soldier.OrderLoc := KMPointBelow(Entrance); //Position in front of the barracks facing north
        U.SetActionGoIn(uaWalk, gdGoOutside, Self, true);
        if Assigned(U.OnUnitTrained) then
          U.OnUnitTrained(U);
        If U is TKMUnitWarrior then
          if gHands[Owner].IsComputer then
            if gRes.Units[U.UnitType].CanOrderAmmo then
              TKMUnitWarrior(U).OrderAmmo;

        fPhase := 0;
        If fOrderCount = 0 then
          fTrainingID := NO_TRRAINING_ID;
      end;

    end;

  end else
  begin

    if fPhase > 0 then
    begin
      if HasWares(fTrainingID) then
      begin
        TakeWares;
        fProgress := UnitProgress(PALACE_UNITS_ORDER[fTrainingID])
      end;
    end
    else
    if fOrderCount > 0 then
      if gHands[Owner].GetWorklessCount > 0 then
        if HasWares(fTrainingID) then
        begin
          TakeWares;
          gHands[Owner].TakeWorkless;
          Dec(fOrderCount);
          fProgress := UnitProgress(PALACE_UNITS_ORDER[fTrainingID]);
        end;

  end;
end;


procedure TKMHousePalace.Paint;
  procedure PaintDeposits;
  var list : TKMPointTagList;
    I, tile : Integer;
    C, factor : Cardinal;
  begin
    list := TKMPointTagList.Create;
    gTerrain.FindDeposits(Entrance, 25, list, true);

    for I := 0 to list.Count - 1 do
    begin
      case list.Tag[I] of
        1: C := $FF00d5ff;
        2: C := $FFFF0000;
        3: C := $FF0000FF;
        4: C := $FF000000;
        else
          C := $FFFFFFFF;
      end;
      case list.Tag[I] of
        1: tile := 750; //gold
        2: tile := 751; //iron
        3: tile := 752; //bitin
        4: tile := 753; //coal
        else
          tile := 0;
      end;
      factor := list.Tag2[I] * 15 + 40;
      factor := factor shl 24;
      factor := factor or $00FFFFFF;
      C := C and factor;
      gRenderAux.CircleOnTerrain(list[I].X - 0.5, list[I].Y - 0.5, 0.2, C, 0);//.Quad(list[I].X, list[I].Y, C);
      gRenderPool.RenderTerrain.RenderTile(tile, list[I].X, list[I].Y, 0);
    end;
    list.Free;
  end;
var I : Integer;
begin
  Inherited;

  if fBuildState = hbsDone then
  begin
    for I := 1 to 4 do
        gRenderPool.AddHousePalaceFlags(HouseType, fPosition, I, FlagAnimStep + I * 10, gHands[Owner].FlagColor);
    //if gMySpectator.Selected = self then
    //  PaintDeposits;
  end;

end;

function TKMHousePalace.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited ObjToString(aSeparator) +
            Format('%sPhase[1] : %d%s' +
                    'Progress[1] : %d%s' +
                    'MaxProgress[1] : %d%s' +
                    'fWait[1] : %d',
                  [aSeparator,
                    fPhase, aSeparator,
                    fProgress, aSeparator,
                    GetMaxProgress, aSeparator,
                    fWait]);
end;
constructor TKMHouseStall.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
var I : Integer;
begin
  Inherited;

  for I := Low(fWareMultiplier) to High(fWareMultiplier) do
    fWareMultiplier[I] := 1;
end;

constructor TKMHouseStall.Load(LoadStream: TKMemoryStream);
var I : Integer;
begin
  inherited;
  LoadStream.Read(fTicker);
  LoadStream.Read(fSpecialPriceTick);

  for I := Low(fWareMultiplier) to High(fWareMultiplier) do
    LoadStream.Read(fWareMultiplier[I]);

  for I := Low(fCostMultiplier) to High(fCostMultiplier) do
    LoadStream.Read(fCostMultiplier[I]);

  for I := Low(fVWaresCount) to High(fVWaresCount) do
    LoadStream.Read(fVWaresCount[I]);

  for I := Low(fVWares) to High(fVWares) do
    LoadStream.ReadANSI(fVWares[I]);
end;

procedure TKMHouseStall.Save(SaveStream: TKMemoryStream);
var I : Integer;
begin
  inherited;
  SaveStream.Write(fTicker);
  SaveStream.Write(fSpecialPriceTick);

  for I := Low(fWareMultiplier) to High(fWareMultiplier) do
    SaveStream.Write(fWareMultiplier[I]);
  for I := Low(fCostMultiplier) to High(fCostMultiplier) do
    SaveStream.Write(fCostMultiplier[I]);
  for I := Low(fVWaresCount) to High(fVWaresCount) do
    SaveStream.Write(fVWaresCount[I]);
  for I := Low(fVWares) to High(fVWares) do
    SaveStream.WriteANSI(fVWares[I]);
end;

procedure TKMHouseStall.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  //SetRandomWares(true);
end;

procedure TKMHouseStall.SetRandomCost(aSpecialPrice : Boolean);
var I : integer;
begin
  if aSpecialPrice then
  begin
    fSpecialPriceTick := gGameParams.Tick + 1200;
    for I := 0 to High(fCostMultiplier) do
    begin
      case KaMRandom(100, 'TKMHouseStall.SetRandomCost:Type') of
        0..9 : fCostMultiplier[I] := 0.1 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        10..49 : fCostMultiplier[I] := 0.4 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        50..84 : fCostMultiplier[I] := 0.3 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        85..99 : fCostMultiplier[I] := 0.2 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
      end;
      If gHands[Owner].HasPearl(ptArium) then
        fCostMultiplier[I] := fCostMultiplier[I] * 0.8
    end;

    for I := Low(fWareMultiplier) to High(fWareMultiplier) do
      fWareMultiplier[I] := 1 + KaMRandom('');

    If gHands[Owner].HasPearl(ptArium) then
      for I := Low(fWareMultiplier) to High(fWareMultiplier) do
        fWareMultiplier[I] := fWareMultiplier[I] * 1.2

  end else
  begin
    if gGameParams.Tick < fSpecialPriceTick then
      Exit;

    for I := 0 to High(fCostMultiplier) do
    begin
      case KaMRandom(100, 'TKMHouseStall.SetRandomCost:Type') of
        0..9 : fCostMultiplier[I] := 0.1 + KaMRandom('TKMHouseStall.SetRandomCost:Count');
        10..49 : fCostMultiplier[I] := 0.9 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        50..84 : fCostMultiplier[I] := 0.5 + KaMRandom('TKMHouseStall.SetRandomCost:Count');
        85..99 : fCostMultiplier[I] := 0.75 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 3;
      end;
      If gHands[Owner].HasPearl(ptArium) then
        fCostMultiplier[I] := fCostMultiplier[I] * 0.8
    end;
    for I := Low(fWareMultiplier) to High(fWareMultiplier) do
      fWareMultiplier[I] := 0.75 + KaMRandom('') / 2;
    If gHands[Owner].HasPearl(ptArium) then
      for I := Low(fWareMultiplier) to High(fWareMultiplier) do
        fWareMultiplier[I] := fWareMultiplier[I] * 1.2
  end;
end;

procedure TKMHouseStall.SetRandomWares(aUnique: Boolean);

  function HasWare(aWare : String) : Boolean;
  var I : integer;
  begin
    Result := false;
    for I := 0 to High(fVWares) do
      If fVWares[I] = aWare then
        Exit(true);
  end;
var I, K : Integer;
  wareName : String;
  stallsQty : Word;
begin
  for I := 0 to High(fVWares) do
    fVWares[I] := '';

  I := 0;
  stallsQty := gHands[Owner].Stats.GetHouseQty(htStall) div 2 + 1;
  If gHands[Owner].HasPearl(ptArium) then
    stallsQty := 1;
  while I < length(fVWares) do
  begin
    K := KamRandom(gRes.Wares.VirtualWares.Count, 'TKMHouseStall.SetRandomWares:VWareID');
    wareName := gRes.Wares.VirtualWares.Ware[K].Name;
    if aUnique then
      while HasWare(wareName) or (gRes.Wares.VirtualWares.Ware[K].CoinPrice = 0) do
      begin
        K := KamRandom(gRes.Wares.VirtualWares.Count, 'TKMHouseStall.SetRandomWares:VWareID2');
        wareName := gRes.Wares.VirtualWares.Ware[K].Name;
      end;

    fVWares[I] := wareName;
    fVWaresCount[I] := KamRandom(Max((gRes.Wares.VirtualWares.Ware[K].MaxCountInStall + 1) div stallsQty, 2), 'TKMHouseStall.SetRandomWares:Count');
    //fCostMultiplier[I] := KaMRandom('') * 2;
    Inc(I);
  end;
  SetRandomCost(false);
end;
procedure TKMHouseStall.UpdateState(aTick: Cardinal);
begin
  Inherited;
  if not HasWorkerInside then
    Exit;
  if gGameParams.Tick = fSpecialPriceTick then
    SetRandomCost(false);

  Inc(fTicker);
  if fTicker mod 3000 = 0 then
  begin
    SetRandomWares(true)
  end
  else
  if fTicker mod 600 = 0 then
    SetRandomCost(false);

  if fTicker mod 14000 = 0 then
    SetRandomCost(true);

end;

function TKMHouseStall.WareRatioTo(aIndex: Byte): Word;
var costFrom, costTo : Single;
begin
  Assert(InRange(aIndex, 1, WARES_IN_OUT_COUNT));

  costFrom := 1;
  costTo := gRes.Wares[fWareinput[aIndex]].CoinPrice * fWareMultiplier[aIndex];
  if (CostTo = 0) or (costFrom = 0) then
    Exit(0);

  If gHands[Owner].EconomyDevUnlocked(15) then
    costFrom := 0.8;

  Result := Round(costTo / Min(costTo, costFrom));
end;

function TKMHouseStall.RatioFrom(aIndex : Byte) : Word;
var costFrom, costTo : Single;
begin
  Assert(InRange(aIndex, 0, high(fVWares)) );

  costFrom := 1;
  costTo := gRes.Wares.VirtualWares.WareS[fVWares[aIndex]].CoinPrice * Max(fCostMultiplier[aIndex], 1);
  if (CostTo = 0) or (costFrom = 0) then
    Exit(0);
  Result := Round(costTo / Min(costTo, costFrom));
end;

function TKMHouseStall.RatioTo(aIndex : Byte) : Word;
var costFrom, costTo : Single;
begin
  Assert(InRange(aIndex, 0, high(fVWares)));

  costFrom := 1;
  costTo := gRes.Wares.VirtualWares.WareS[fVWares[aIndex]].CoinPrice * fCostMultiplier[aIndex];
  if (CostTo = 0) or (costFrom = 0) then
    Exit(0);
  Result := Round(costFrom / Min(costTo, costFrom));
end;

function TKMHouseStall.CanBuyItem(aIndex: Byte): Boolean;
begin
  Result := false;
  if not HasWorkerInside then
    Exit(false);
  Assert(InRange(aIndex, 0, high(fVWares)));

  if fVWaresCount[aIndex] <= 0 then
    Exit;

  if gHands[Owner].VirtualWare['vtCoin'] < RatioFrom(aIndex) then
    Exit;

  Result := true;
end;

procedure TKMHouseStall.BuyCoin(aIndex: Byte; aCount : Byte);
var W : TKMWareType;
  I : Integer;
begin
  if not HasWorkerInside then
    Exit;
  Assert(InRange(aIndex, 1, WARES_IN_OUT_COUNT));

  W := WareInput[aIndex];
  for I := 1 to aCount do
  begin
    if ResIn[aIndex] <= 0 then Exit;

    WareTakeFromIn(W, 1, true);

    gHands[Owner].SetVirtualWareCnt('vtCoin', WareRatioTo(aIndex));
  end;

end;

procedure TKMHouseStall.BuyItem(aIndex: Byte; aCount : Byte);
var I : integer;
begin
  if not HasWorkerInside then
    Exit;
  Assert(InRange(aIndex, 0, high(fVWares)));

  for I := 1 to aCount do
  begin
    if not CanBuyItem(aIndex) then
      Exit;

    Dec(fVWaresCount[aIndex], Min(RatioTo(aIndex), fVWaresCount[aIndex]));

    gHands[Owner].SetVirtualWareCnt(fVWares[aIndex], RatioTo(aIndex));
    gHands[Owner].SetVirtualWareCnt('vtCoin', -RatioFrom(aIndex));
  end;
end;

function TKMHouseStall.GetVWareName(aIndex: Byte): string;
begin
  Assert(InRange(aIndex, 0, high(fVWares)));
  Result := fVWares[aIndex];
end;

function TKMHouseStall.GetVWareCount(aIndex: Byte): Byte;
begin
  Assert(InRange(aIndex, 0, high(fVWares)));
  Result := fVWaresCount[aIndex];
end;

procedure TKMHouseStall.Paint;
var I : integer;
begin
  Inherited;
  if not IsComplete then
    Exit;
  for I := 0 to High(fVWares) do
    if (fVWares[I] <> '') and (fVWaresCount[I] > 0) then
      gRenderPool.AddSpriteWH(Position, KMPoint(57 - 6 * I,15 + 1 * I),
                              gRes.Wares.VirtualWares.WareS[fVWares[I]].SpriteInStall,
                              rxHouses,
                              gHands[Owner].FlagColor);
  If gGameParams.Tick < fSpecialPriceTick then
    gRenderPool.AddSpriteWH(Position, KMPoint(0, 0), 2508, rxHouses, gHands[Owner].FlagColor);
end;

constructor TKMHouseProdThatch.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
var PT : TKMProdThatchAnimType;
begin
  Inherited;
  for PT := Low(TKMProdThatchAnimType) to High(TKMProdThatchAnimType) do
  begin
    fAnims[PT].StartTick := high(Integer);
    fAnims[PT].EndTick := high(Integer);
  end;

  fWorkPointsTaken.Clear;
  fGrainType := GRAIN_GUI_ORDER[0];
  fGrassType := GRASS_GUI_ORDER[0];
  fVegeType := VEGE_GUI_ORDER[0];
  fFillSeeds := 0;
  fFillCorn := 0;
  fFillHay := 0;
  fFillWine := 0;
  fFillVege := 0;
  fStoredClay := 0;
end;

constructor TKMHouseProdThatch.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  fWorkPointsTaken.Clear;
  LoadStream.Read(fAnims, SizeOf(fAnims));
  LoadStream.Read(fTiles, SizeOf(fTiles));
  fWorkPointsTaken.LoadFromStream(LoadStream);
  LoadStream.Read(fGrainType, SizeOf(fGrainType));
  LoadStream.Read(fGrassType, SizeOf(fGrassType));
  LoadStream.Read(fVegeType, SizeOf(fVegeType));
  LoadStream.Read(fFillHay);
  LoadStream.Read(fFillWine);
  LoadStream.Read(fFillSeeds);
  LoadStream.Read(fFillCorn);
  LoadStream.Read(fFillVege);
  LoadStream.Read(fStoredClay);
end;

procedure TKMHouseProdThatch.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fAnims, SizeOf(fAnims));
  SaveStream.Write(fTiles, SizeOf(fTiles));
  fWorkPointsTaken.SaveToStream(SaveStream);
  SaveStream.Write(fGrainType, SizeOf(fGrainType));
  SaveStream.Write(fGrassType, SizeOf(fGrassType));
  SaveStream.Write(fVegeType, SizeOf(fVegeType));
  SaveStream.Write(fFillHay);
  SaveStream.Write(fFillWine);
  SaveStream.Write(fFillSeeds);
  SaveStream.Write(fFillCorn);
  SaveStream.Write(fFillVege);
  SaveStream.Write(fStoredClay);
end;

function TKMHouseProdThatch.GetMaxDistanceToPoint: Integer;
begin
  If gHands[Owner].BuildDevUnlocked(31) then
    Result := 10
  else
    Result := 0;
end;


function TKMHouseProdThatch.IsPointTaken(P: TKMPoint): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWorkPointsTaken.Count - 1 do
    if fWorkPointsTaken[I] = P then
      Exit(true);
end;

procedure TKMHouseProdThatch.TakePoint(P: TKMPoint);
begin
  fWorkPointsTaken.Add(P);
end;

procedure TKMHouseProdThatch.RemovePoint(P: TKMPoint);
var I : Integer;
begin
  for I := fWorkPointsTaken.Count - 1 downto 0 do
    if fWorkPointsTaken[I] = P then
      fWorkPointsTaken.Remove(I);
end;

procedure TKMHouseProdThatch.ClearPoints;
begin
  fWorkPointsTaken.Clear;
end;

function TKMHouseProdThatch.TakeTile: Byte;
var I : Integer;
begin
  Result := 0;

  for I := low(fTiles) to High(fTiles) do
    If (fTiles[I] > 0) and (gGame.Params.Tick > fTiles[I]) then
    begin
      fTiles[I] := 0;
      Exit(I);
    end;
end;

procedure TKMHouseProdThatch.BringTile;
var I : Integer;
begin
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] = 0 then
    begin
        If gHands[Owner].BuildDevUnlocked(7) then
          fTiles[I] := gGame.Params.Tick + 1400
        else
          fTiles[I] := gGame.Params.Tick + 1800;
      Exit;
    end;
  Inc(fStoredClay);

end;

function TKMHouseProdThatch.CanTakeTile: Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fTiles) to High(fTiles) do
    If (fTiles[I] > 0) and (gGame.Params.Tick > fTiles[I]) then
      Exit(true);

end;

function TKMHouseProdThatch.HasSpaceForNextTile: Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] = 0 then
      Exit(true);
end;

function TKMHouseProdThatch.HasAnyTile : Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fTiles) to High(fTiles) do
    If fTiles[I] > 0 then
      Exit(true);
end;

function TKMHouseProdThatch.HasClayStored: Boolean;
begin
  Result := fStoredClay > 0;
end;

function TKMHouseProdThatch.HasTileOrClay: Boolean;
begin
  Result := HasAnyTile or HasClayStored;
end;

function TKMHouseProdThatch.CanUseStoredClay: Boolean;
begin
  Result := (fStoredClay > 0) and HasSpaceForNextTile;
end;

function TKMHouseProdThatch.CanStoreClay: Boolean;
begin
  Result := (fStoredClay < MAX_CLAY_TO_STORE) or HasSpaceForNextTile;
end;

procedure TKMHouseProdThatch.UseStoredClay;
begin
  If fStoredClay > 0 then
  begin
    Dec(fStoredClay);
    BringTile;
  end;
end;

function TKMHouseProdThatch.MiningRange(aUnit: TKMUnitType): Integer;
begin
  Result := Inherited;
  If IsFlagPointSet then
    Result := Result div Round(sqrt(Result));

end;
function TKMHouseProdThatch.MiningRect(aWare : TKMWareType) : TKMRect;
begin
  //the OG rect
  {case aWare of
    wtGoldOre:  Result := KMRect(7, 11, 6, 2);
    wtIronOre:  Result := KMRect(7, 11, 5, 2);
    wtCoal:     Result := KMRect(4,  5, 5, 2);
    wtBitinOre: Result := KMRect(9, 13, 8, 3);
    wtTile:     Result := KMRect(9,  7, 10, 9);
    wtJewerly:     Result := KMRect(7,  8, 8, 6);
  else}

  Result := Inherited;
  If IsFlagPointSet then
  begin
    Result.Left :=  Result.Left div Round(sqrt(Result.Left));
    Result.Top :=  Result.Top div Round(sqrt(Result.Top));
    Result.Right := Result.Right div  Round(sqrt(Result.Right));
    Result.Bottom := Result.Bottom div Round(sqrt(Result.Bottom));
  end;
end;


procedure TKMHouseProdThatch.ProduceStarts(aWare: TKMWareType);
  procedure StartAnim(aType : TKMProdThatchAnimType);
  begin
    fAnims[aType].StartTick := gGameParams.Tick;
    fAnims[aType].EndTick := fAnims[aType].StartTick
                              + ((800 div gRes.Houses.ProdThatch_Anims[aType].Count) * gRes.Houses.ProdThatch_Anims[aType].Count);
  end;
var PT : TKMProdThatchAnimType;
begin
  case aWare of
    wtSeed,
    wtCorn: PT := ptaCorn;

    wtWine: PT := ptaWine;
    wtBread: PT := ptaSmokeBakery;

    wtLog,
    wtWheel,
    wtTimber,
    wtSawDust: PT := ptaSawDust;

    wtStone,
    wtStoneBolt: PT := ptaStoneDust;

    wtGold: PT := ptaSmokeGold;
    wtIron,
    wtBitin,
    wtSteelE,
    wtBitinE: PT := ptaSmokeIron;
    else
    begin
      Exit;
    end;
  end;
  StartAnim(PT);
  if PT in [{ptaSmokeIron, ptaSmokeGold, }ptaSmokeBakery] then
    StartAnim(ptaWindows);

end;

procedure TKMHouseProdThatch.MakeSound;
begin
  if SKIP_SOUND then Exit;

  if fAnims[ptaSawDust].StartTick <> high(Integer) then
    if gGameParams.Tick mod 20 = 0 then
      gSoundPlayer.Play(sfxsaw, fPosition);

  if fAnims[ptaSmokeGold].StartTick <> high(Integer) then
    if gGameParams.Tick mod 25 = 0 then
      gSoundPlayer.Play(sfxmetallurgists, fPosition);

  if fAnims[ptaSmokeIron].StartTick <> high(Integer) then
    if gGameParams.Tick mod 15 = 0 then
      gSoundPlayer.Play(sfxwineDrain, fPosition);

  if fAnims[ptaSmokeBakery].StartTick <> high(Integer) then
    if gGameParams.Tick mod 20 = 0 then
      gSoundPlayer.Play(sfxBakerSlap, fPosition);

  if fAnims[ptaStoneDust].StartTick <> high(Integer) then
    if gGameParams.Tick mod 8 = 0 then
      gSoundPlayer.Play(sfxQuarryClink, fPosition);
end;

procedure TKMHouseProdThatch.SetNextGrainType(aValue : Integer; aType : Byte);
var aIndex, I : Integer;
begin
  if aType = 1 then
  begin
    SetNextGrassType(aValue);
    Exit;
  end;
  if aType = 2 then
  begin
    SetNextVegeType(aValue);
    Exit;
  end;


  aIndex := -1;
  for I := 0 to High(GRAIN_GUI_ORDER) do
    if GRAIN_GUI_ORDER[I] = fGrainType then
    begin
      aIndex := I;
      Break;
    end;

  if aIndex = -1 then
  begin
    fGrainType := GRAIN_GUI_ORDER[0];
    Exit;
  end;

  IncLoop(aIndex, low(GRAIN_GUI_ORDER), high(GRAIN_GUI_ORDER), aValue);
  fGrainType := GRAIN_GUI_ORDER[aIndex];

end;

procedure TKMHouseProdThatch.SetGrainTypes(aGrain: TKMGrainType; aGrass: TKMGrainType; aVegetables: TKMGrainType);
begin
  fGrainType := aGrain;
  fGrassType := aGrass;
  fVegeType := aVegetables;
end;

procedure TKMHouseProdThatch.SetGrainTypes(aGrain: Byte; aGrass: Byte; aVegetables: Byte);
begin
  SetGrainTypes(TKMGrainType(aGrain), TKMGrainType(aGrass), TKMGrainType(aVegetables))
end;

function TKMHouseProdThatch.HasOrderForWare(aType: TKMWareType): Boolean;
var aID : Integer;
begin
  Result := false;
  aID := GetWareOutIndex(aType);
  if aID <= 0 then
    Exit;
  Result := WareOrder[aID] > 0;
end;

function TKMHouseProdThatch.HasOrderForWares(aTypes: TKMWareTypeSet): Boolean;
var WT : TKMWareType;
begin
  Result := false;
  for WT in aTypes do
    if HasOrderForWare(WT) then
      Exit(true);
end;

function TKMHouseProdThatch.GetGrainTypes: TKMGrainFarmSet;
begin
  Result := GRAIN_FARM_SET_NONE;

  if HasOrderForWares([wtCorn, wtSeed]) then
    Result[0] := fGrainType;

  if HasOrderForWares([wtHay]) then
    Result[1] := fGrassType;

  if HasOrderForWares([wtVegetables]) then
    Result[2] := fVegeType;
end;

procedure TKMHouseProdThatch.SetNextGrassType(aValue: Integer);
var aIndex, I : Integer;
begin
  aIndex := -1;
  for I := 0 to High(GRASS_GUI_ORDER) do
    if GRASS_GUI_ORDER[I] = fGrassType then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
  begin
    fGrassType := GRASS_GUI_ORDER[0];
    Exit;
  end;
  IncLoop(aIndex, low(GRASS_GUI_ORDER), high(GRASS_GUI_ORDER), aValue);
  fGrassType := GRASS_GUI_ORDER[aIndex];
end;

procedure TKMHouseProdThatch.SetNextVegeType(aValue: Integer);
var aIndex, I : Integer;
begin
  aIndex := -1;
  for I := 0 to High(VEGE_GUI_ORDER) do
    if VEGE_GUI_ORDER[I] = fVegeType then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
  begin
    fVegeType := VEGE_GUI_ORDER[0];
    Exit;
  end;
  IncLoop(aIndex, low(VEGE_GUI_ORDER), high(VEGE_GUI_ORDER), aValue);
  fVegeType := VEGE_GUI_ORDER[aIndex];
end;

procedure TKMHouseProdThatch.GrainCut(aGrain: TKMGrainType);
begin

  if HasOrderForWare(wtCorn) then
  begin
    fFillCorn := fFillCorn + gFieldGrains[aGrain].Straw;
    ProduceWareFromFill(wtCorn, fFillCorn);
  end;
  if HasOrderForWare(wtSeed) then
  begin
    fFillSeeds := fFillSeeds + gFieldGrains[aGrain].Seeds;
    ProduceWareFromFill(wtSeed, fFillSeeds);
  end;
  if HasOrderForWare(wtWine) then
  begin
    fFillWine := fFillWine + gFieldGrains[aGrain].Wine;
    ProduceWareFromFill(wtWine, fFillWine);
  end;
  if HasOrderForWare(wtHay) then
  begin
    fFillHay := fFillHay + gFieldGrains[aGrain].Hay;
    ProduceWareFromFill(wtHay, fFillHay);
  end;
  if HasOrderForWare(wtVegetables) then
  begin
    fFillVege := fFillVege + gFieldGrains[aGrain].Vege;
    ProduceWareFromFill(wtVegetables, fFillVege);
  end;

end;

function TKMHouseProdThatch.HasGrain : Boolean;
begin
  Result := (fGrainType <> gftNone)
            and HasOrderForWares([wtCorn, wtSeed])
            and (HasSpaceForWaresOut([wtCorn, wtSeed], false)or ForceWorking);
end;

function TKMHouseProdThatch.HasGrass : Boolean;
begin
  Result := (fGrassType <> gftNone)
            and HasOrderForWares([wtHay])
            and (HasSpaceForWaresOut([wtHay], false)or ForceWorking);
end;

function TKMHouseProdThatch.HasVege : Boolean;
begin
  Result := (fVegeType <> gftNone)
            and HasOrderForWares([wtVegetables])
            and (HasSpaceForWaresOut([wtVegetables], false)or ForceWorking);
end;


function TKMHouseProdThatch.GetCutGrainTypes: TKMGrainFarmSet;
begin
  Result[0] := gftNone;
  Result[1] := gftNone;
  Result[2] := gftNone;
  if HasGrain then
    Result[0] := fGrainType;
  if HasGrass then
    Result[1] := fGrassType;
  if HasVege then
    Result[2] := fVegeType;
end;

procedure TKMHouseProdThatch.CheckWorkersForSlot;
var I : Integer;
  UT : TKMUnitType;
begin
  for I := High(fWorkers) downto 0 do
  begin
    UT := TKMUnit(fWorkers[I]).UnitType;
    if not SlotHasAnyWare(gRes.Units[UT].ProducesWares) then
      RemoveWorker(fWorkers[I]);
  end;
end;

function TKMHouseProdThatch.AcceptsWorker(aWorker: Pointer): Boolean;
var UT : TKMUnitType;
begin
  Result := inherited;

  UT := TKMUnit(aWorker).UnitType;
  Result := Result and SlotHasAnyWare(gRes.Units[UT].ProducesWares);
end;

function TKMHouseProdThatch.CanHasWorker(aType: TKMUnitType): Boolean;
begin
  Result := Inherited;
  Result := Result and SlotHasAnyWare(gRes.Units[aType].ProducesWares);
end;

procedure TKMHouseProdThatch.UpdateState(aTick: Cardinal);
var PT : TKMProdThatchAnimType;
begin
  Inherited;

  if not IsComplete then
    Exit;

  for PT := Low(TKMProdThatchAnimType) to High(TKMProdThatchAnimType) do
    if gGameParams.Tick >= fAnims[PT].EndTick then
    begin
      fAnims[PT].StartTick := high(Integer);
      fAnims[PT].EndTick := high(Integer);
    end;

end;

procedure TKMHouseProdThatch.Paint;
var PT : TKMProdThatchAnimType;
begin
  Inherited;

  if not IsComplete then
    Exit;

  for PT := Low(TKMProdThatchAnimType) to High(TKMProdThatchAnimType) do
    if gGameParams.Tick >= fAnims[PT].StartTick then
      gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_Anims[PT], gGameParams.Tick - fAnims[PT].StartTick, gHands[Owner].FlagColor, rxHouses);

end;

function TKMHouseProdThatch.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited+ Format('%sfTiles : %d, %d, %d, %d %s'
                              + 'fStoredClay : %d',
                              [aSeparator,
                              fTiles[1], fTiles[2], fTiles[3], fTiles[4], aSeparator,
                              fStoredClay
                              ]
                              );
end;

function TKMHouseFarm.GetGrainTypes: TKMGrainFarmSet;
begin
  Result[0] := fGrainType;
  Result[1] := fGrassType;
  Result[2] := fVegeType;
end;

function TKMHouseFarm.GetCutGrainTypes: TKMGrainFarmSet;
begin
  Result := GRAIN_FARM_SET_NONE;
  if HasGrain then
    Result[0] := fGrainType;
  if HasGrass then
    Result[1] := fGrassType;
  if HasVege then
    Result[2] := fVegeType;
end;

constructor TKMHouseFarm.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fGrainType, SizeOf(fGrainType));
  LoadStream.Read(fGrassType, SizeOf(fGrassType));
  LoadStream.Read(fVegeType, SizeOf(fVegeType));
  LoadStream.Read(fFillHay);
  LoadStream.Read(fFillSeeds);
  LoadStream.Read(fFillCorn);
  LoadStream.Read(fFillVege);
end;

procedure TKMHouseFarm.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fGrainType, SizeOf(fGrainType));
  SaveStream.Write(fGrassType, SizeOf(fGrassType));
  SaveStream.Write(fVegeType, SizeOf(fVegeType));
  SaveStream.Write(fFillHay);
  SaveStream.Write(fFillSeeds);
  SaveStream.Write(fFillCorn);
  SaveStream.Write(fFillVege);
end;

procedure TKMHouseFarm.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  fGrainType := gftWheat;
  fGrassType := gftGrass;
  fVegeType := gftPumpkin;
  fFillHay := 0;
  fFillSeeds := 0;
  fFillCorn := 0;
  fFillVege := 0;
  fMode := wmChopAndPlant;
end;

procedure TKMHouseFarm.SetNextGrainType(aValue : Integer; aType : Byte);
var aIndex, I : Integer;
begin
  if aType = 1 then
  begin
    SetNextGrassType(aValue);
    Exit;
  end;
  if aType = 2 then
  begin
    SetNextVegeType(aValue);
    Exit;
  end;


  aIndex := -1;
  for I := 0 to High(GRAIN_GUI_ORDER) do
    if GRAIN_GUI_ORDER[I] = fGrainType then
    begin
      aIndex := I;
      Break;
    end;

  if aIndex = -1 then
  begin
    fGrainType := GRAIN_GUI_ORDER[0];
    Exit;
  end;

  IncLoop(aIndex, low(GRAIN_GUI_ORDER), high(GRAIN_GUI_ORDER), aValue);
  fGrainType := GRAIN_GUI_ORDER[aIndex];
end;

procedure TKMHouseFarm.SetNextGrassType(aValue: Integer);
var aIndex, I : Integer;
begin
  aIndex := -1;
  for I := 0 to High(GRASS_GUI_ORDER) do
    if GRASS_GUI_ORDER[I] = fGrassType then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
  begin
    fGrassType := GRASS_GUI_ORDER[0];
    Exit;
  end;
  IncLoop(aIndex, low(GRASS_GUI_ORDER), high(GRASS_GUI_ORDER), aValue);
  fGrassType := GRASS_GUI_ORDER[aIndex];
end;

procedure TKMHouseFarm.SetGrainTypes(aGrain: TKMGrainType; aGrass: TKMGrainType; aVegetables: TKMGrainType);
begin
  fGrainType := aGrain;
  fGrassType := aGrass;
  fVegeType := aVegetables;
end;

procedure TKMHouseFarm.SetGrainTypes(aGrain: Byte; aGrass: Byte; aVegetables: Byte);
begin
  SetGrainTypes(TKMGrainType(aGrain), TKMGrainType(aGrass), TKMGrainType(aVegetables))
end;


procedure TKMHouseFarm.SetNextVegeType(aValue: Integer);
var aIndex, I : Integer;
begin
  aIndex := -1;
  for I := 0 to High(VEGE_GUI_ORDER) do
    if VEGE_GUI_ORDER[I] = fVegeType then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
  begin
    fVegeType := GRASS_GUI_ORDER[0];
    Exit;
  end;
  IncLoop(aIndex, low(VEGE_GUI_ORDER), high(VEGE_GUI_ORDER), aValue);
  fVegeType := VEGE_GUI_ORDER[aIndex];
end;

procedure TKMHouseFarm.SetMode(aValue : TKMWoodcutterMode);
begin
  fMode := aValue;
end;

function TKMHouseFarm.HasGrain : Boolean;
begin
  Result := fGrainType <> gftNone;
  Result := Result and self.HasSpaceForWaresOut([wtCorn, wtSeed], false);
  Result := Result or ForceWorking;
end;

function TKMHouseFarm.HasGrass : Boolean;
begin
  Result := fGrassType <> gftNone;
  Result := Result and HasSpaceForWareOut(wtHay);
  Result := Result or ForceWorking;
end;

function TKMHouseFarm.HasVege : Boolean;
begin
  Result := fVegeType <> gftNone;
  Result := Result and HasSpaceForWareOut(wtVegetables);
  Result := Result or ForceWorking;
end;

procedure TKMHouseFarm.GrainCut(aGrain: TKMGrainType);
//var count : Byte;
begin
  fFillCorn := fFillCorn + gFieldGrains[aGrain].Straw;
  fFillSeeds := fFillSeeds + gFieldGrains[aGrain].Seeds;
  fFillHay := fFillHay + gFieldGrains[aGrain].Hay;
  fFillVege := fFillVege + gFieldGrains[aGrain].Vege;

  ProduceWareFromFill(wtCorn, fFillCorn);
  ProduceWareFromFill(wtSeed, fFillSeeds);
  ProduceWareFromFill(wtHay, fFillHay);
  ProduceWareFromFill(wtVegetables, fFillVege);
  {count := trunc(fFillCorn);
  if count > 0 then ProduceWare(wtCorn, count);
  IncProductionCycle(wtCorn, Max(count, 0));
  fFillCorn := fFillCorn - count;

  count := trunc(fFillSeeds);
  if count > 0 then ProduceWare(wtSeed, count);
  IncProductionCycle(wtSeed, Max(count, 0));
  fFillSeeds := fFillSeeds - count;

  count := trunc(fFillHay);
  if count > 0 then ProduceWare(wtHay, count);
  IncProductionCycle(wtHay, Max(count, 0));
  fFillHay := fFillHay - count;

  count := trunc(fFillVege);
  if count > 0 then ProduceWare(wtVegetables, count);
  IncProductionCycle(wtVegetables, Max(count, 0));
  fFillVege := fFillVege - count;}
end;

function TKMHouseFarm.GetProgressArray: TSingleArray;
var I, J : Integer;
begin
  J := 0;
  SetLength(Result, Ceil(fFillCorn));
  for I := 0 to Ceil(fFillCorn) - 1 do
    Result[I + J] := Min(fFillCorn - I, 1);


  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillSeeds));
  for I := 0 to Ceil(fFillSeeds) - 1 do
    Result[J + I] := Min(fFillSeeds - I, 1);

  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillHay));
  for I := 0 to Ceil(fFillHay) - 1 do
    Result[J + I] := Min(fFillHay - I, 1);

  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillVege));
  for I := 0 to Ceil(fFillVege) - 1 do
    Result[J + I] := Min(fFillVege - I, 1);
end;

function TKMHouseFarm.GetTexIDsArray: TKMWord2Array;
var I, J : Integer;
begin
  J := 0;
  SetLength(Result, Ceil(fFillCorn));
  for I := 0 to Ceil(fFillCorn) - 1 do
  begin
    SetLength(Result[I + J], 1);
    Result[I + J, 0] := gRes.Wares[wtCorn].GUIIcon;
  end;


  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillSeeds));
  for I := 0 to Ceil(fFillSeeds) - 1 do
  begin
    SetLength(Result[I + J], 1);
    Result[I + J, 0] := gRes.Wares[wtSeed].GUIIcon;
  end;

  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillHay));
  for I := 0 to Ceil(fFillHay) - 1 do
  begin
    SetLength(Result[I + J], 1);
    Result[I + J, 0] := gRes.Wares[wtHay].GUIIcon;
  end;

  J := length(Result);
  SetLength(Result, length(Result) + Ceil(fFillVege));
  for I := 0 to Ceil(fFillVege) - 1 do
  begin
    SetLength(Result[I + J], 1);
    Result[I + J, 0] := gRes.Wares[wtVegetables].GUIIcon;
  end;
end;


procedure TKMHouseVineYard.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  fFillWine := 0;
end;

constructor TKMHouseVineYard.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fFillWine);
end;

procedure TKMHouseVineYard.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fFillWine);
end;

procedure TKMHouseVineYard.GrainCut(aGrain : TKMGrainType);
begin
  fFillWine := fFillWine + gFieldGrains[aGrain].Wine;
end;

procedure TKMHouseVineYard.ProduceWine;
var wineCount : Byte;
begin
  wineCount := EnsureRange(trunc(fFillWine), 0, 5);
  fFillWine := fFillWine - wineCount;

  if wineCount > 0 then
    ProduceWare(wtWine, wineCount);
  IncProductionCycle(wtWine, Max(wineCount, 0));
end;

function TKMHouseVineYard.CanMakeWine(aNoFieldsToCut: Boolean): Boolean;
begin
  if aNoFieldsToCut then
    Result := fFillWine > 1
  else
    Result := fFillWine >= 5;
  
end;
function TKMHouseVineYard.WineToProduce: Byte;
begin
  Result := EnsureRange(Trunc(fFillWine), 0, 5);
end;

function TKMHouseVineYard.GetProgressArray: TSingleArray;
var I : Integer;
begin
  SetLength(Result, Ceil(fFillWine));
  for I := 0 to Ceil(fFillWine) - 1 do
    Result[I] := Min(fFillWine - I, 1);
end;

Constructor TKMHouseShipyard.Load(LoadStream: TKMemoryStream);
begin
  Inherited;

  LoadStream.Read(fShipType, SizeOf(fShipType));
  LoadStream.Read(fShipSketchPosition);
  LoadStream.Read(fShipPhase);
  LoadStream.Read(fDoWork);
  LoadStream.Read(fNextShipType, SizeOf(fNextShipType));
  LoadStream.Read(fWayOutID);
  fWaresOut.Load(LoadStream);
  fShipBuiltOf.Load(LoadStream);
end;

function TKMHouseShipyard.GetPhasesCount : Byte;
begin
  Result := 0;
  case fShipType of
    utBoat: Result := gRes.Units.FishermansShipSketch[fShipSketchPosition.Dir].Count;
    utBattleShip: Result := gRes.Units.BattleShipSketch[fShipSketchPosition.Dir].Count;
    utShip : Result := gRes.Units.ShipSketch[fShipSketchPosition.Dir].Count;
  end;
end;

procedure TKMHouseShipyard.SetNextShipType(aValue: TKMUnitType);
begin
  if gHands[Owner].Locks.UnitUnlocked(aValue, htShipYard) then
    fNextShipType := aValue;

  if (fShipPhase = 0) and (GetState <> hstWork) then
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;
end;

procedure TKMHouseShipyard.Activate(aWasBuilt: Boolean);
  function CheckWater(aX, aY : Integer) : Boolean;
  begin
    Result := false;
    if gTerrain.TileInMapCoords(aX, aY, 1) then
      Result := gTerrain.CheckPassability(aX, aY, tpFish);
  end;
var I : integer;

begin
  Inherited;
  fShipType := SHIPYARD_ORDER[0];
  for I := 0 to High(SHIPYARD_ORDER) do
    if gHands[Owner].Locks.UnitUnlocked(SHIPYARD_ORDER[I], HouseType) then
    begin
      fShipType := SHIPYARD_ORDER[I];
      Break;
    end;


  fNextShipType := fShipType;
  fShipPhase := 0;
  fShipSketchPosition.Dir := dirN;

  if CheckWater(Entrance.X - 1, Entrance.Y + 4) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X - 1, Entrance.Y + 4);
    fShipSketchPosition.Dir := dirS;
    fWayOutID := 1
  end
  else
  if CheckWater(Entrance.X + 2, Entrance.Y + 4) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X + 2, Entrance.Y + 4);
    fShipSketchPosition.Dir := dirS;
    fWayOutID := 2
  end
  else
  if CheckWater(Entrance.X + 4, Entrance.Y + 2) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X + 4, Entrance.Y + 2);
    fShipSketchPosition.Dir := dirE;
    fWayOutID := 3
  end
  else
  if CheckWater(Entrance.X - 3, Entrance.Y + 2) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X - 3, Entrance.Y + 2);
    fShipSketchPosition.Dir := dirW;
    fWayOutID := 4
  end else
    fWayOutID := 0;

  Assert(fWayOutID <> 0, 'TKMHouseShipyard did not found water')
end;

procedure TKMHouseShipyard.Save(SaveStream: TKMemoryStream);
begin
  Inherited;

  SaveStream.Write(fShipType, SizeOf(fShipType));
  SaveStream.Write(fShipSketchPosition);
  SaveStream.Write(fShipPhase);
  SaveStream.Write(fDoWork);
  SaveStream.Write(fNextShipType, SizeOf(fNextShipType));
  SaveStream.Write(fWayOutID);
  fWaresOut.Save(SaveStream);
  fShipBuiltOf.Save(SaveStream);
end;

procedure TKMHouseShipyard.IncSketchPhase(aWares : TKMWarePlan);
  procedure FinishShip;
    var U : TKMUnitWarrior;
  begin

    //gTerrain.UnlockTile(fShipSketchPosition.Loc);
    U := gHands[Owner].AddUnitGroup(fShipType, fShipSketchPosition.Loc, fShipSketchPosition.Dir, 1, 1).FlagBearer;


    if gHands[Owner].IsComputer then
    begin
      U.BoltCount := 100;
      U.InfinityAmmo := true;
      if fShipType = utBattleShip then
        WareTakeFromIn(wtBolt, CheckWareIn(wtBolt), true)
      else
      if fShipType = utBoat then
        WareTakeFromIn(wtAxe, CheckWareIn(wtAxe), true);
    end;

    if fShipBuiltOf.HasWare(wtBitinE) > 0 then
    begin
      U.Defence := U.Defence + (fShipBuiltOf.HasWare(wtBitinE) div 2);
      U.ProjectilesDefence := U.ProjectilesDefence + (fShipBuiltOf.HasWare(wtBitinE) / 2)
    end;
    if fShipBuiltOf.HasWare(wtSteelE) > 5 then
      U.SetSpeed(fShipBuiltOf.HasWare(wtSteelE) - 5, true);

    if fShipBuiltOf.HasWare(wtSteelE) > 0 then
      U.HitPointsMax := U.HitPointsMax + (fShipBuiltOf.HasWare(wtSteelE) div 2);

    U.HitPointsChangeFromScript(U.HitPointsMax);

    //fShipType := utNone;
    fShipPhase := 0;
    //fShipSketchPosition.Loc := KMPOINT_INVALID_TILE;
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;
    fShipBuiltOf.Clear;
    gScriptEvents.ProcWarriorEquipped(U, U.Group);
  end;
var count : Integer;
begin
  Inc(fShipPhase);
  count := GetPhasesCount;

  if aWares.HasWare(wtBitinE) > 0 then
    fShipBuiltOf.AddWare(wtBitinE);
  if aWares.HasWare(wtSteelE) > 0 then
    fShipBuiltOf.AddWare(wtSteelE);

  if ((fShipType = utBoat) and (fShipPhase > 2))
    or ((fShipType = utShip) and (fShipPhase > 4))
    or ((fShipType = utBattleShip) and (fShipPhase > 5))
   then
  if aWares.HasWare(wtLeather) > 0 then
    fShipBuiltOf.AddWare(wtLeather);



  if fShipPhase >= count then
  begin
    FinishShip;
  end else
  if fShipPhase = 1 then
  begin
    //fShipSketchPosition.Dir := KaMRandomDir('TKMHouseShipyard.Activate');
    //fShipSketchPosition.Loc := gTerrain.FindPlaceForUnit(Position, tpFish, 5);
    //gTerrain.SetTileLock(fShipSketchPosition.Loc, tlRoadWork);
  end;


end;

procedure TKMHouseShipyard.StartWorking;
begin
    //gTerrain.SetTileLock(fShipSketchPosition.Loc, tlRoadWork);
end;

procedure TKMHouseShipyard.SetNextShipType(aAmount: Integer);
var I, Index: Integer;
  procedure SelectNext;
  begin
    IncLoop(Index, low(SHIPYARD_ORDER), high(SHIPYARD_ORDER), aAmount);
    fNextShipType := SHIPYARD_ORDER[Index];
  end;

begin
  Index := -1;
  for I := 0 to High(SHIPYARD_ORDER) do
    If SHIPYARD_ORDER[I] = fNextShipType then
    begin
      Index := I;
      Break;
    end;
  if Index = -1 then
    Exit;
  SelectNext;
  while not gHands[Owner].Locks.UnitUnlocked(fNextShipType, HouseType) do
    SelectNext;

  if (fShipPhase = 0) and (GetState <> hstWork) then
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;


end;

function TKMHouseShipyard.CanWork: Boolean;
begin
  Result := gTerrain.CheckPassability(fShipSketchPosition.Loc, tpFish) or (fShipPhase > 0);
  Result := Result and (gTerrain.GetUnit(fShipSketchPosition.Loc) = nil);
  Result := Result and (fDoWork or (fShipPhase > 0));
end;

function TKMHouseShipyard.GetWarePlan: TKMWarePlan;
begin
  Result.Reset;
  case fShipType of
    utBoat :  begin
                Result.AddWare(wtLog);
                Result.AddWare(wtSkin);

                if CheckWareIn(wtSteelE) > 0 then
                  Result.AddWare(wtSteelE);
              end;
    utShip :  begin
                Result.AddWare(wtLog);
                Result.AddWare(wtSkin);

                if CheckWareIn(wtSteelE) > 0 then
                  Result.AddWare(wtSteelE);
                if CheckWareIn(wtBitinE) > 0 then
                  Result.AddWare(wtBitinE);
              end;
    utBattleShip :  begin
                      Result.AddWare(wtLog);
                      Result.AddWare(wtSkin);

                      if CheckWareIn(wtSteelE) > 0 then
                        Result.AddWare(wtSteelE);
                      if CheckWareIn(wtBitinE) > 0 then
                        Result.AddWare(wtBitinE);


                    end;
  end;

end;

procedure TKMHouseShipyard.WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1);
begin
  fWaresOut.AddWare(aWare, aCount);
  gHands[Owner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
end;

function TKMHouseShipyard.CheckWareOut(aWare: TKMWareType): Word;
begin
  Result := fWaresOut.HasWare(aWare);
end;

procedure TKMHouseShipyard.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var index : Integer;
begin
  index := fWaresOut.IndexOf(aWare, 1);
  Assert(index >= 0);
  {if index = -1 then
    Exit;}

  if aFromScript then
  begin
    aCount := Min(aCount, fWaresOut[index].C);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;

  Assert(aCount <= fWaresOut[index].C);

  fWaresOut[index].C := fWaresOut[index].C - aCount;

end;

function TKMHouseShipyard.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWaresOut.Count - 1 do
    if (fWaresOut[I].W = aWare) and (fWaresOut[I].C > 0) then
      Result := fWaresOut[I].C >= aCount;

end;

procedure TKMHouseShipyard.Paint;
  function SketchPointF : TKMPointF;
  begin
    Result.X := fShipSketchPosition.Loc.X + 0.5;
    Result.Y := fShipSketchPosition.Loc.Y + 0.5;
  end;
begin
  if not IsComplete then
    Inherited
  else
  begin

    //Incase we need to render house at desired step in debug mode
    if HOUSE_BUILDING_STEP = 0 then
    begin
      //Shipyard is Painted as seperated parts
      {if fIsOnSnow then
        gRenderPool.AddHouse(fType, fPosition, 1, 1, fSnowStep, -1, GetStonePic, GetSnowPic, false, false, 0)
      else
        gRenderPool.AddHouse(fType, fPosition, 1, 1, 0, -1, GetStonePic, -1, false, false, 0);}

      If fIsOnTerrain <> tptNone then
      begin
        If fSnowStep = 1 then
          gRenderPool.AddSpriteGSnow(fPosition, KMPOINT_ZERO, GetSnowPic + 1, 1, rxHouses, gHands[Owner].FlagColor)
        else
        begin
          gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2538, rxHouses, gHands[Owner].FlagColor);
          gRenderPool.AddSpriteGSnow(fPosition, KMPOINT_ZERO, GetSnowPic + 1, fSnowStep, rxHouses, gHands[Owner].FlagColor);
        end;
      end else
      gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2538, rxHouses, gHands[Owner].FlagColor);//house

      gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2534, rxHouses, gHands[Owner].FlagColor);//yard
      case fWayOutID of
        1: begin
            gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
            gRenderPool.AddSpriteG(fPosition, KMPoint(-102, 18), 2540, rxHouses, gHands[Owner].FlagColor);//bottom Right
           end;
        2: begin
            gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            gRenderPool.AddSpriteG(fPosition, KMPoint(0, 18), 2540, rxHouses, gHands[Owner].FlagColor);//bottom Right
           end;
        3:  begin
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2536, rxHouses, gHands[Owner].FlagColor);//right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            end;
        4:  begin
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2537, rxHouses, gHands[Owner].FlagColor);//left
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            end;
      end;


      if CurrentAction <> nil then
        if PaintHouseWork then
        gRenderPool.AddHouseWork(fType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, GetFlagColor);

    end
    else
      gRenderPool.AddHouse(fType, fPosition,
        Min(HOUSE_BUILDING_STEP * 3, 1),
        EnsureRange(HOUSE_BUILDING_STEP * 3 - 1, 0, 1),
        Max(HOUSE_BUILDING_STEP * 3 - 2, 0)
        ,GetWoodPic, GetStonePic, GetSnowPic
        );
  end;

  if fShipPhase > 0 then
  begin
    case fShipType of
      utBoat : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.FishermansShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
      utBattleShip : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.BattleShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
      utShip : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.ShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
    end;

  end;
  
end;

procedure TKMHouseWall.UpdateWallAround;
var cells : TKMPointDirList;
  I : Integer;
  H : TKMHouse;
begin
  If {aWasBuilt} true then
  begin
    cells := TKMPointDirList.Create;
    GetListOfCellsAround(cells, tpNone);

    for I := 0 to cells.Count - 1 do
    begin
      H := gTerrain.House(cells[I].Loc);
      If H.IsValid(htWall5) then
        TKMHouseWallSingle(H).UpdateConnection;
    end;

    cells.Free;
  end;
end;

constructor TKMHouseWall.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fTicker);
end;

procedure TKMHouseWall.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fTicker);
end;

procedure TKMHouseWall.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
begin
  Inherited;
  UpdateWallAround;
end;

procedure TKMHouseWall.AfterCreate(aWasBuilt: Boolean);
begin
  UpdateWallAround;
end;

procedure TKMHouseWall.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  If aWasBuilt then
    UpdateWallAround;
end;

procedure TKMHouseWall.FinishedLevel(aLevel : Byte);
begin
  UpdateWallAround;
  gHands[Owner].Deliveries.Queue.RemDemand(self);
end;

procedure TKMHouseWall.UpdateEntrancePos;
begin
  Inherited;
  UpdatePointBelowEntrance;
end;

procedure TKMHouseWall.UpdatePointBelowEntrance;
var aCells : TKMPointDirList;
begin
  aCells := TKMPointDirList.Create;

  GetListOfCellsAround(aCells, tpWalk);
  IF aCells.Count > 0 then
    fPointBelowEntrance := aCells[0].Loc
  else
    fPointBelowEntrance := KMPointBelow(Entrance);
  aCells.Free;
end;

procedure TKMHouseWall.UpdateState(aTick: Cardinal);


  function AttackDelay: Byte;
  begin
    Case HouseType of
      htWall : Result := 35;
      htWall2 : Result := 25;
      htWall3 : Result := 35;
      htWall4 : Result := 25;
      htWall5 : Result := 20;
      else
        Result := 10;
    End;
  end;

  procedure AttackTheEnemy;
  var U : TKMUnit;
    from : TKMPoint;
  begin
    from := GetRandomCellWithin;

    U := gTerrain.UnitsHitTestWithinRad(from, 2, 8, Owner, atEnemy, dirNA, not RANDOM_TARGETS);
    //no unit found so no need to check every archer
    If (U = nil) or (U.IsDeadOrDying) then
      Exit;
    Inc(fTicker, KaMRandom(3, ''));
    gProjectiles.AimTarget(from.ToFloat, U.PositionF, 0.2, ptWallBolt, nil, 15, 0);//(from.ToFloat, U, ptWallBolt, nil, 0, 15.99);
  end;

begin
  Inherited;
  If not IsComplete then
  begin
    If aTick mod 50 = 0 then
      UpdatePointBelowEntrance;
  end else
  begin
    Inc(fTicker);
    If aTick mod 600 = 0 then
      UpdatePointBelowEntrance;

    If CurrentLevel = 2 then
      if fTicker mod AttackDelay = 0 then
        AttackTheEnemy;
  end;
end;

procedure TKMHouseWall.Paint;
begin
  Inherited;
end;

function TKMHouseWallSingle.GetStonePic : Integer;
begin
  If (Style = 0) and (fWallStyle > 0) then
    Result := gRes.Houses[fType].Styles[fWallStyle - 1].StonePic
  else
    Result := Inherited;
end;

function TKMHouseWallSingle.GetSnowPic : Integer;
begin
  If (Style = 0) and (fWallStyle > 0) then
    Result := gRes.Houses[fType].Styles[fWallStyle - 1].SnowPic[OnTerrain]
  else
  Result := Inherited;

end;

procedure TKMHouseWallSingle.AfterCreate(aWasBuilt: Boolean);
begin
  fWallStyle := 0;
  UpdateConnection;
  Inherited;
end;

procedure TKMHouseWallSingle.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  If aWasBuilt then
    UpdateConnection;
end;

procedure TKMHouseWallSingle.FinishedLevel(aLevel : Byte);
begin
  UpdateConnection;
  Inherited;
end;

procedure TKMHouseWallSingle.SetLevel(aValue : Byte);
begin
  Inherited;
  UpdateConnection;
end;


procedure TKMHouseWallSingle.UpdateConnection;
var connID : Byte;
  pX, pY : Integer;
  newStyle : Byte;
begin
  pX := Entrance.X;
  pY := Entrance.Y;
  connID := 0;
  connID := connID + byte(gTerrain.House(pX, pY - 1).IsValid(WALL_HOUSES)) shl 0;
  connID := connID + byte(gTerrain.House(pX + 1, pY).IsValid(WALL_HOUSES)) shl 1;
  connID := connID + byte(gTerrain.House(pX, pY + 1).IsValid(WALL_HOUSES)) shl 2;
  connID := connID + byte(gTerrain.House(pX - 1, pY).IsValid(WALL_HOUSES)) shl 3;

  case connID of
    5: newStyle := 1;
    10: newStyle := 2;
    else newStyle := 0;
  end;
  If (newStyle > 0) and (CurrentLevel = 0) then
    newStyle := newStyle + 2;
  If (newStyle > 0) and (CurrentLevel = 2) then
    newStyle := newStyle + 4;
  fWallStyle := newStyle;
end;

constructor TKMHouseWallSingle.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fWallStyle);
end;

procedure TKMHouseWallSingle.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fWallStyle);
end;

function TKMHouseSiegeTower.GetUnitWeight(aUnitType: TKMUnitType): Byte;
begin
  case aUnitType of
    //ships cannot be asigned
    utGolem,
    utShip, utBoat,
    utBattleShip : Result := 255;

    utAny,
    utArcher,
    utBowman,
    utCrossbowman,
    utRogue,
    utSkirmisher: Result := 1;

    utBallista:  Result := 6;
    utCatapult:  Result := 8;
    utMobileTower:  Result := 8;
    else raise Exception.Create('Unknown shooter');
  end;
end;

function TKMHouseSiegeTower.GetTotalWeight: Byte;
var I : Integer;
begin
  Result := 0;
  for I := 0 to high(fWorkers) do
    inc(Result, GetUnitWeight(TKMUnit(fWorkers[I]).UnitType));
end;

function TKMHouseSiegeTower.CanEnter(aUnitType : TKMUnitType = utAny): Boolean;
begin
  Result := not IsClosedForWorker;
  Result := Result and (GetTotalWeight + GetUnitWeight(aUnitType) <= MAX_UNITS_INSIDE);
end;

procedure TKMHouseSiegeTower.Paint;
  function GetMinRadius : Single;
  var I : Integer;
  begin
    Result := 20;
    for I := 0 to WorkersCount - 1 do
      Result := Min(fWorkers[I].ToWarrior.RangeMin, Result);
  end;
  function GetMaxRadius : Single;
  var I : Integer;
  begin
    Result := 0;
    for I := 0 to WorkersCount - 1 do
      Result := Max(fWorkers[I].ToWarrior.RangeMax, Result);
  end;

begin
  inherited;
  if not (gMySpectator.Selected = Self) then
    Exit;

  if WorkersCount = 0 then
    Exit;

  gRenderPool.RenderDebug.RenderTiledArea(Entrance{ + KMPoint(0, -4)}, GetMinRadius, GetMaxRadius, GetLength, $20FFFFFF, $A0FFFFFF);
end;


initialization
begin
  TKMHouseSketchEdit.DummyHouseSketch := TKMHouseSketchEdit.Create;
  TKMHouseSketchEdit.DummyHouseSketch.fEditable := False;
end;

finalization
begin
  FreeAndNil(TKMHouseSketchEdit.DummyHouseSketch);
end;


end.

