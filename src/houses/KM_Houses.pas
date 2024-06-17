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
type
  TKMHouse = class;
  TKMHouseEvent = procedure(aHouse: TKMHouse) of object;
  TKMHouseFromEvent = procedure(aHouse: TKMHouse; aFrom: TKMHandID) of object;
  TKMHouseArray = array of TKMHouse;

  TKMHouseSketch = class;

  TKMHouseSketchType = (hstHousePlan, hstHouse);
  TKMHouseSketchTypeSet = set of TKMHouseSketchType;

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
    procedure UpdateEntrancePos;
  protected
    fPosition: TKMPoint; //House position on map, kinda virtual thing cos it doesn't match with entrance
    procedure SetPosition(const aPosition: TKMPoint); virtual;
    constructor Create; overload;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID); overload;

    property HouseType: TKMHouseType read fType;

    property Position: TKMPoint read fPosition;
    property Entrance: TKMPoint read fEntrance;
    property PointBelowEntrance: TKMPoint read fPointBelowEntrance;

    function ObjToStringShort(const aSeparator: String = '|'): String; override;

    function IsEmpty: Boolean;
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
    fLastOrderProduced: Byte;
//    fWareOrderDesired: array [1..4] of Single;
    fResetDemands : Boolean;
    fIsOnSnow: Boolean;
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

    procedure CheckOnSnow;
    function GetWareInArray: TKMByteArray;
    function GetWareOutArray: TKMByteArray;
    function GetWareOutPoolArray: TKMByteArray;

    function GetWareDistribution(aID: Byte): Byte; //Will use GetRatio from mission settings to find distribution amount
    procedure SetIsClosedForWorker(aIsClosed: Boolean);
    procedure UpdateDeliveryMode;
    function GetHasWorker: Boolean;
    procedure ShowMsg(aTextID: Integer);
    Function GetWareInput : TKMWareType8;
    Function GetWareOutput : TKMWareType8;
    function GetWorkMultiplier : Single;
    function CheckMaxLevel : Boolean;
    function GetHouseSpec : TKMHouseSpec;
    procedure SetStyle(aValue : Byte);
    procedure SetLevel(aValue : Byte);
    function GetProductionCycle(aIndex : Byte) : Word;
  protected
    fWorkers: TPointerArray;
    fProductionCycles: array [1..WARES_IN_OUT_COUNT] of Word; //If HousePlaceOrders=True then here are production orders
    fBuildState: TKMHouseBuildState; // = (hbsGlyph, hbsNoGlyph, hbsWood, hbsStone, hbsDone);
    FlagAnimStep: Cardinal; //Used for Flags and Burning animation
    //WorkAnimStep: Cardinal; //Used for Work and etc.. which is not in sync with Flags
    procedure Activate(aWasBuilt: Boolean); virtual;
    procedure AddDemandsOnActivate(aWasBuilt: Boolean); virtual;
    function GetWareOrder(aId: Byte): Integer; virtual;
    function GetWareIn(aI: Byte): Word; virtual;
    function GetWareOut(aI: Byte): Word; virtual;
    function GetWareInLocked(aI: Byte): Word; virtual;
    procedure SetWareInManageTakeOutDeliveryMode(aWare: TKMWareType; aCntChange: Integer);
    procedure SetWareIn(aI: Byte; aValue: Word); virtual;
    procedure SetWareOut(aI: Byte; aValue: Word); virtual;
    procedure SetBuildingRepair(aValue: Boolean);
    procedure SetWareOrder(aId: Byte; aValue: Integer); virtual;
    procedure SetNewDeliveryMode(aValue: TKMDeliveryMode); virtual;
    procedure CheckTakeOutDeliveryMode; virtual;
    function GetDeliveryModeForCheck(aImmidiate: Boolean): TKMDeliveryMode;

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

    function TryDecWareDelivery(aWare: TKMWareType; aDeleteCanceled: Boolean): Boolean; virtual;

    procedure MakeSound; virtual; //Swine/stables make extra sounds
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
    function GetStonePic : Integer;
    function GetSnowPic : Integer;

    function GetClosestCell(const aPos: TKMPoint): TKMPoint;
    function GetDistance(const aPos: TKMPoint): Single;
    function InReach(const aPos: TKMPoint; aDistance: Single): Boolean;
    procedure GetListOfCellsAround(aCells: TKMPointDirList; aPassability: TKMTerrainPassability);
    procedure GetListOfCellsWithin(aCells: TKMPointList);
    procedure GetListOfGroundVisibleCells(aCells: TKMPointTagList);
    function GetRandomCellWithin: TKMPoint;
    function HitTest(X, Y: Integer): Boolean;
    property BuildingRepair: Boolean read fBuildingRepair write SetBuildingRepair;
    property PlacedOverRoad: Boolean read fPlacedOverRoad write fPlacedOverRoad;

    property PositionF: TKMPointF read GetPositionF;
    property PicWariant : Integer read fWariant;
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


    procedure SetWareSlot(aSlotID : Byte; aForceChange : Boolean = false);overload;
    procedure SetWareSlot(aWare : TKMWareType);overload;
    function HasSlotWithWare(aWare : TKMWareType) : Boolean;
    function CanChangeWareInput(IsAI : Boolean = false) : Boolean;
    function GetAcceptWareIn(aWareType : TKMWareType) : Word;
    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; virtual;
    function ShouldAbandonDeliveryFrom(aWareType: TKMWareType; aImmidiateCheck: Boolean = False): Boolean; virtual;
    function ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean; virtual;

    property Worker: Pointer read fWorker;
    procedure SetWorker(aWorker: Pointer); //Explicitly use SetWorker, to make it clear its not only pointer assignment
    property HasWorker: Boolean read GetHasWorker; //There's a citizen who runs this house
    //function CanHasWorker: Boolean;//deprecated
    property IsClosedForWorker: Boolean read fIsClosedForWorker write SetIsClosedForWorker;
    property DisableUnoccupiedMessage: Boolean read fDisableUnoccupiedMessage write fDisableUnoccupiedMessage;
    function HasWorkerInside : Boolean;
    function WorkersCount : Word;overload;
    function WorkersCount(aType : TKMUnitType) : byte;overload;
    function MaxWorkers : Byte;overload;
    function MaxWorkers(aType : TKMUnitType) : Byte;overload;
    function CanHasWorker(aType : TKMUnitType) : Boolean;
    procedure AssignWorker(aWorker: Pointer);
    procedure RemoveWorker(aWorker: Pointer);
    function AcceptsWorker(aWorker: Pointer) : Boolean;
    procedure DoNotAcceptWorker(aIndex: Integer);
    function GetWorkersIcons : TKMWordArray;

    function GetHealth: Word;
    function GetBuildWoodDelivered: Byte;
    function GetBuildStoneDelivered: Byte;
    property TileDelivered: Byte read fBuildTileDelivered;
    function GetBuildResourceDelivered: Byte;
    function GetBuildResDeliveredPercent: Single;

    property WareInArray: TKMByteArray read GetWareInArray;
    property WareOutArray: TKMByteArray read GetWareOutArray;
    property WareOutPoolArray: TKMByteArray read GetWareOutPoolArray;

    property BuildingState: TKMHouseBuildState read fBuildState write fBuildState;
    property BuildSupplyWood: Byte read fBuildSupplyWood;
    property BuildSupplyStone: Byte read fBuildSupplyStone;
    procedure IncBuildingProgress;
    procedure IncBuildingUpgradeProgress;
    property IsMaxLevel : Boolean read CheckMaxLevel;
    function MaxHealth: Word;
    procedure AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False);
    procedure AddRepair(aAmount: Word = 5);
    procedure UpdateDamage;

    function IsStone: Boolean;
    function IsComplete: Boolean; inline;
    function IsDamaged: Boolean;
    property IsDestroyed: Boolean read fIsDestroyed;
    property GetDamage: Word read fDamage;

    procedure SetState(aState: TKMHouseState; doTowerAnim : Boolean = true);
    function GetState: TKMHouseState;

    procedure HouseDemandWasClosed(aWare: TKMWareType; aDeleteCanceled: Boolean);
    function WareIsInAndOut(aWare: TKMWareType): Boolean;
    function CheckWareIn(aWare: TKMWareType): Word; virtual;
    function CheckWareOut(aWare: TKMWareType): Word; virtual;
    function CheckWareTotal(aWare: TKMWareType): Word; virtual;
    function PickOrder(aUnitType : TKMUnitType): Byte;
    function CheckResToBuild: Boolean;
    function GetMaxInWare: Word;
    function GetMaxOutWare: Word;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); virtual; //override for School and etc..
    procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1);
    procedure WareAddToEitherFromScript(aWare: TKMWareType; aCount: Integer);
    function WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1) : Boolean;
    procedure WareTake(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual;
    procedure WareTakeFromIn(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); virtual;
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
    Procedure ProduceWare(aWare : TKMWareType; aCount : Integer = 1);
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

    procedure IncProductionCycle(aWare : TKMWareType);
    property ProductionCycle[aIndex : Byte] : Word read GetProductionCycle;
    function GetWareInIndex(aWare : TKMWareType) : Byte;
    function GetWareOutIndex(aWare : TKMWareType) : Byte;


    property HSpec : TKMHouseSpec read GetHouseSpec;

    procedure UpdateDemands; virtual;
    procedure PostLoadMission; virtual;
    function ObjToString(const aSeparator: String = '|'): String; override;
    procedure IncAnimStep;
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


  TKMHouseTower = class(TKMHouse)
  public
    RangeMax, RangeMin : Single;
    ThrowingCycles : Byte;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
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
      fFruitTreeID : ShortInt;
      fFillFruits : Single;
      procedure SetProgress;
      procedure SetGrowPhase(aValue : Byte);
      procedure SetFruitTreeID(aValue : ShortInt);
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      procedure UpdatePosition(const aPos: TKMPoint); override;//Used only by map editor


      function CanWork : Boolean;
      function IncGrowPhase : Boolean;
      Function NeedWater: Boolean;
      property GrowPhase : Byte read fGrowPhase write SetGrowPhase;
      property FruitType : ShortInt read fFruitTreeID write SetFruitTreeID;
      procedure MakeFruits;

      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure UpdateState(aTick: Cardinal); override;
      procedure Paint; Override;
      function ObjToString(const aSeparator: String = '|'): String; override;

  end;

  TKMHousePottery = class(TKMHouse)
    private
      fLastTile : byte;
      fTiles : array[1..4] of Byte;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      function ObjToString(const aSeparator: String = '|'): String; override;
      function TakeTile : Byte;
      procedure Paint; Override;
  end;

  TKMHouseCollectors = class(TKMHouseWFlagPoint)
  protected
    function GetMaxDistanceToPoint: Integer; override;
  end;

  TKMHousePalace = class(TKMHouseWFlagPoint)
  private
    fTrainingID : Byte;
    fProgress, fPhase, fWait, fOrderCount : array of Word;

    function HasWares(aIndex : Integer) : Boolean;
    Procedure TakeWares(aIndex : Integer);
    function  GetOrderCount(aIndex : Integer) : Word;
    procedure SetOrderCount(aIndex : Integer; aValue : Word);
    function  GetMaxProgress(aIndex : Integer) : Integer;
    function  GetFullProgress(aIndex : Integer) : Single;
    function GetPhaseProgress(aIndex : Integer) : Single;
    function UnitProgress(aType : TKMUnitType) : Word;
    function GetPhaseCount(aIndex : Integer) : Byte;
    function GetCurrentPhase(aIndex : Integer) : Byte;
    function GetPhaseColor(aIndex : Integer) : Cardinal;
    procedure SetUnitVWareArray;
  protected
    function GetMaxDistanceToPoint: Integer; override;
  public
    VWareIDs :array of array of Integer;
    function IsTraining : Boolean;
    property Orders[aIndex : Integer] : Word read GetOrderCount write SetOrderCount;
    property FullProgress[aIndex : Integer] : Single read GetFullProgress;
    property PhaseProgress[aIndex : Integer] : Single read GetPhaseProgress;
    property BarColor[aIndex : Integer] : Cardinal read GetPhaseColor;
    property UnitTrainPhases[aIndex : Integer] : Byte read GetPhaseCount;
    property CurrentPhase[aIndex : Integer] : Byte read GetCurrentPhase;
    procedure CancelOrder(aIndex : Integer);
    function GetWarePlan(aIndex : Integer = -1) : TKMWarePlan;
    function GetFullCost(aIndex : Integer = -1) : TKMWarePlan;
    function CanEquip(aIndex : Integer) : Boolean;

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

  TKMHouseProdThatch = class(TKMHouse)
    private
      fAnimStartTickDust,
      fAnimStartTickSmoke1,
      fAnimStartTickSmoke23,
      fAnimEndTickDust,
      fAnimEndTickSmoke1,
      fAnimEndTickSmoke23 : Cardinal;
      fWorkPointsTaken : TKMArray<TKMPoint>;
      fLastTile : Byte;
      fTiles : array[1..10] of Byte;

      fGrainType, fGrassType, fVegeType: TKMGrainType;
      fFillWine,
      fFillHay,
      fFillSeeds,
      fFillCorn,
      fFillVege : Single;
      procedure SetNextGrassType(aValue : Integer);
      procedure SetNextVegeType(aValue : Integer);
      function GetGrainTypes : TKMGrainTypeSet;
    protected
      procedure MakeSound; Override; //Swine/stables make extra sounds
    public
      function IsPointTaken(P : TKMPoint): Boolean;
      procedure TakePoint(P : TKMPoint);
      procedure RemovePoint(P : TKMPoint);
      procedure ClearPoints;
      function TakeTile : Byte;

      property GrainType : TKMGrainType read fGrainType;
      property GrassType : TKMGrainType read fGrassType;
      property VegeType : TKMGrainType read fVegeType;
      property GrainTypes : TKMGrainTypeSet read GetGrainTypes;
      procedure SetNextGrainType(aValue : Integer; aType : Byte);
      procedure GrainCut(aGrain : TKMGrainType);

      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure ProduceStarts(aWare: TKMWareType);
      procedure UpdateState(aTick: Cardinal); override;
      procedure Paint; Override;
  end;

  TKMHouseFarm = class(TKMHouse)
    private
      fGrainType, fGrassType, fVegeType: TKMGrainType;
      fFillHay,
      fFillSeeds,
      fFillCorn,
      fFillVege : Single;
      function GetGrainTypes : TKMGrainTypeSet;
      procedure SetNextGrassType(aValue : Integer);
      procedure SetNextVegeType(aValue : Integer);
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      property GrainType : TKMGrainType read fGrainType;
      property GrassType : TKMGrainType read fGrassType;
      property VegeType : TKMGrainType read fVegeType;
      property GrainTypes : TKMGrainTypeSet read GetGrainTypes;
      procedure SetNextGrainType(aValue : Integer; aType : Byte);
      function GetProgressArray : TSingleArray;
      function GetTexIDsArray : TKMWord2Array;
      procedure GrainCut(aGrain : TKMGrainType);
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
      procedure ProduceWine;
  end;

  TKMHouseShipyard = class(TKMHouse)
    private
      fShipType : TKMUnitType;
      fShipSketchPosition : TKMPointDir;
      fShipPhase : Byte;
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      property ShipType : TKMUnitType read fShipType write fShipType;

      procedure IncSketchPhase;
      function CanWork : Boolean;


      procedure Paint; Override;
  end;

implementation
uses
  // Do not add KM_Game dependancy! Entities should be isolated as much as possible
  TypInfo, SysUtils, Math, KromUtils,
  KM_Entity, KM_HandsCollection,
  KM_GameParams, KM_Terrain, KM_RenderPool, KM_RenderAux, KM_Sound,
  KM_Hand, KM_HandLogistics, KM_HandTypes,
  KM_Units, KM_UnitWarrior, KM_HouseWoodcutters,
  KM_Resource, KM_ResSound, KM_ResTexts, KM_ResUnits, KM_ResMapElements,
  KM_Log, KM_ScriptingEvents, KM_CommonUtils, KM_MapEdTypes,
  KM_RenderDebug,
  KM_TerrainTypes,
  KM_CommonExceptions, KM_JSONUtils,
  KM_UnitTaskThrowRock,
  KM_ResTileset;

const
  // Delay, in ticks, from user click on DeliveryMode btn, to tick, when mode will be really set.
  // Made to prevent serf's taking/losing deliveries only because player clicks throught modes.
  // No hurry, let's wait a bit for player to be sure, what mode he needs
  UPDATE_DELIVERY_MODE_DELAY = 10;


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
  fPosition := KMPoint (PosX, PosY);
  UpdateEntrancePos;
end;


{Return Entrance of the house, which is different than house position sometimes}
procedure TKMHouseSketch.UpdateEntrancePos;
begin
  if IsEmpty then Exit;
  
  fEntrance.X := fPosition.X + gRes.Houses[fType].EntranceOffsetX;
  fEntrance.Y := fPosition.Y;
  Assert((fEntrance.X > 0) and (fEntrance.Y > 0));

  fPointBelowEntrance := KMPointBelow(fEntrance);

  if not (fType in WALL_HOUSES) then
    Exit;

  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointLeft(fEntrance); 
    
  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointRight(fEntrance);   
    
  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointAbove(fEntrance); 

  if not gTerrain.CheckPassability(fPointBelowEntrance, tpWalk) then
      fPointBelowEntrance := KMPointBelow(fEntrance);
    
  

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
var
  I: Integer;
  //firstHouse : TKMHouse;
begin
  inherited Create(aUID, aHouseType, PosX, PosY, aOwner);

  fBuildState := aBuildState;

  fBuildSupplyWood  := 0;
  fBuildSupplyStone := 0;
  fBuildSupplyTile := 0;
  fBuildReserve     := 0;
  fBuildingProgress := 0;
  fDamage           := 0; //Undamaged yet

  fPlacedOverRoad   := gTerrain.TileHasRoad(Entrance);

  fWorker           := nil;
  SetLength(fWorkers, 0);//no workers at the start
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
  CheckOnSnow;
  fSnowStep := Byte(aBuildState = hbsDone);
  WorkingTime := 0;
  TotalWorkingTime := 0;

  {firstHouse := gHands[aOwner].FindHouse(fType, 1);
  if (firstHouse <> nil) and not (firstHouse.IsDestroyed) then
    for I := 1 to WARES_IN_OUT_COUNT do
      fWareBlocked[I] := firstHouse.fWareBlocked[I];}
  LastCellID := 0;
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
  LoadStream.Read(fIsOnSnow);
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
  LoadStream.Read(fStyle);
  LoadStream.Read(fSlotID);
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

  LoadStream.Read(newCount);
  Setlength(fWorkers, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fWorkers[I], 4);

  LoadStream.Read(newCount);
  Setlength(fNotAcceptWorkers, newCount);
  for I := 0 to newCount - 1 do
    LoadStream.Read(fNotAcceptWorkers[I], 4);
end;


procedure TKMHouse.SyncLoad;
var I : Integer;
begin
  fWorker := TKMUnit(gHands.GetUnitByUID(Integer(fWorker)));

  for I := 0 to High(fWorkers) do
    fWorkers[I] := TKMUnit(gHands.GetUnitByUID(Integer(fWorkers[I])));

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

  inherited;
end;


procedure TKMHouse.AddDemandsOnActivate(aWasBuilt: Boolean);
var
  I: Integer;
  W: TKMWareType;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    W := fWareInput[I];
    with gHands[Owner].Deliveries.Queue do
    case W of
      wtNone:    ;
      wtWarfare: AddDemand(Self, nil, W, 1, dtAlways, diNorm);
      wtAll:     AddDemand(Self, nil, W, 1, dtAlways, diNorm);
      else        begin
                    UpdateDemands;
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
begin
  // Only activated houses count
  gHands[Owner].Locks.HouseCreated(fType);
  gHands[Owner].Stats.HouseCreated(fType, aWasBuilt);
  fLevel.UpgradingTime := gGameParams.Tick + 100;

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
        gTerrain.Land^[Entrance.Y, Entrance.X].TileOverlay := toDig3; //Remove road and leave dug earth behind
    end;
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
  wasOnSnow, isRallyPointSet: Boolean;
begin
  Assert(gGameParams.IsMapEditor);

  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  gTerrain.SetHouse(self, HAND_NONE, hsNone);

  if gMySpectator.Hand.CanAddHousePlan(aPos, HouseType) then
  begin
    isRallyPointSet := False;
    //Save if flag point was set for previous position
    if (Self is TKMHouseWFlagPoint) then
      isRallyPointSet := TKMHouseWFlagPoint(Self).IsFlagPointSet;

    gTerrain.RemRoad(Entrance);

    SetPosition(KMPoint(aPos.X - gRes.Houses[fType].EntranceOffsetX, aPos.Y));

    //Update rally/cutting point position for houses with flag point after change fPosition
    if (Self is TKMHouseWFlagPoint) then
    begin
      if not isRallyPointSet then
        TKMHouseWFlagPoint(Self).FlagPoint := PointBelowEntrance
      else
        TKMHouseWFlagPoint(Self).ValidateFlagPoint;
    end;
  end;

  gTerrain.SetHouse(self, Owner, hsBuilt); // Update terrain tiles for house

  //Do not remove all snow if house is moved from snow to snow
  wasOnSnow := fIsOnSnow;
  CheckOnSnow;
  if not wasOnSnow or not fIsOnSnow then
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

  msgID := GetResourceDepletedMessageId;
  Assert(msgID <> 0, gRes.Houses[HouseType].HouseName + ' resource can''t be depleted');

  if msgID <> -1 then
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
    if aWareType = fWareInput[I] then
      Result := fWareBlocked[I];
end;

//Check if we should abandon delivery to this house
function TKMHouse.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  if IsUpgrading then
    if (aWareType in [wtTimber, wtStone, wtTile]) then
      Exit(false);

  Result := (DeliveryMode <> dmDelivery) or ((DeliveryMode = dmDelivery) and (CheckWareIn(aWareType) >= GetMaxInWare - GetAcceptWareIn(aWareType)) ) ;

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
      if gRes.Wares[fWareInput[I]].IsValid then
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
var I : Integer;
  WT : TKMWareType;
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
    if not (fWareInput[I] in [wtNone, wtAll, wtFood, wtWarfare]) then
    begin
      if (fWareIn[I] > 0) then
        Exit(false);

      if not (((fWareBlocked[I] = GetMaxInWare) and not IsAI) or IsAI) then
        Exit(false);
    end;
    if not (fWareOutput[I] in [wtNone, wtAll, wtFood, wtWarfare]) then
      if not (fWareOut[I] = 0) then
        Exit(false);


  end;



end;

procedure TKMHouse.SetWareSlot(aSlotID : Byte; aForceChange : Boolean = false);
var I : integer;
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

  fSlotID := aSlotID;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    //Reset everything to make sure
    fWareIn[I] := 0;
    fWareOut[I] := 0;
    if gHands[Owner].IsHuman then
      fWareBlocked[I] := GetMaxInWare
    else
      fWareBlocked[I] := 0;

    fWareOrder[I] := 0;
  end;

  fResetDemands := true;
  UpdateDemands;
  fResetDemands := false;

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    fWareInput[I] := gRes.Houses[fType].WareInputSlots[fSlotID].WareInput[I];
    fWareOutput[I] := gRes.Houses[fType].WareInputSlots[fSlotID].WareOutput[I];
    TransferWare[I] := gHands[Owner].IsComputer;
  end;

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
  I, K: Integer;
  loc: TKMPoint;
  HA: TKMHouseArea;
begin
  Result := MaxSingle;
  loc := fPosition;
  HA := gRes.Houses[fType].BuildArea;

  for I := max(loc.Y - 3, 1) to loc.Y do
  for K := max(loc.X - 2, 1) to min(loc.X + 1, gTerrain.MapX) do
  if HA[I - loc.Y + 4, K - loc.X + 3] <> 0 then
    Result := Min(Result, KMLength(aPos, KMPoint(K, I)));
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
  HA: TKMHouseArea;

  procedure AddLoc(X,Y: Word; Dir: TKMDirection);
  begin
    //Check that the passabilty is correct, as the house may be placed against blocked terrain
    if gTerrain.CheckPassability(KMPoint(X,Y), aPassability) then
      aCells.Add(KMPointDir(X, Y, Dir));
  end;

begin
  aCells.Clear;
  loc := fPosition;
  HA := gRes.Houses[fType].BuildArea;

  for I := 1 to 4 do for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    if (I = 1) or (HA[I-1,K] = 0) then
      AddLoc(loc.X + K - 3, loc.Y + I - 4 - 1, dirS); //Above
    if (I = 4) or (HA[I+1,K] = 0) then
      AddLoc(loc.X + K - 3, loc.Y + I - 4 + 1, dirN); //Below
    if (K = 4) or (HA[I,K+1] = 0) then
      AddLoc(loc.X + K - 3 + 1, loc.Y + I - 4, dirW); //FromRight
    if (K = 1) or (HA[I,K-1] = 0) then
      AddLoc(loc.X + K - 3 - 1, loc.Y + I - 4, dirE); //FromLeft
  end;
end;


procedure TKMHouse.GetListOfCellsWithin(aCells: TKMPointList);
var
  I, K: Integer;
  loc: TKMPoint;
  houseArea: TKMHouseArea;
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
  groundVisibleArea: TKMHouseArea;
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
  Result := (X-fPosition.X+3 in [1..4]) and
            (Y-fPosition.Y+4 in [1..4]) and
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
      Exit(HSpec.Workers[I].MaxCount);

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
var J : integer;
begin
  if Self = nil then Exit;

  //we are adding new worker so don't clean up pointer here, make it in RemoveWorker
  //gHands.CleanUpUnitPointer( TKMUnit(fWorker) );

  if aWorker = nil then
    Exit;
  J := Length(fWorkers);
  SetLength(fWorkers, J + 1);
  fWorkers[J] := TKMUnit(aWorker).GetPointer;

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
    hbsDone:  Result := gRes.Houses[fType].StoneCost;
    hbsWood:  Result := fBuildStoneDelivered;
    hbsStone: Result := fBuildStoneDelivered;
    else       Result := 0;
  end;

  if IsUpgrading then
    Result := fBuildStoneDelivered;
end;


function TKMHouse.GetBuildResourceDelivered: Byte;
begin
  Result := GetBuildWoodDelivered + GetBuildStoneDelivered + fBuildTileDelivered;
end;


function TKMHouse.GetBuildResDeliveredPercent: Single;
begin
  Result := GetBuildResourceDelivered / (gRes.Houses[fType].TotalBuildSupply);
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

    fPointBelowEntrance := KMPointBelow(fEntrance);
    gHands[Owner].Stats.HouseEnded(fType);
    Activate(True);
    //House was damaged while under construction, so set the repair mode now it is complete
    if (fDamage > 0) and BuildingRepair then
      gHands[Owner].Constructions.RepairList.AddHouse(Self);

    gScriptEvents.ProcHouseBuilt(Self); //At the end since it could destroy this house
  end;
begin
  if IsComplete then Exit;

  if (fBuildState = hbsWood) and (fBuildReserve <= 0) then
  begin
    Dec(fBuildSupplyWood);
    Inc(fBuildReserve, gRes.Houses[fType].HealthSupplyStep);
  end;

  if (fBuildState = hbsStone) and (fBuildReserve <= 0) then
  begin
    if (fBuildSupplyStone = 0) and (fBuildStoneDelivered = gRes.Houses[fType].StoneCost) then
    begin

      if gRes.Houses[fType].TileCost > 0 then
      begin
        Dec(fBuildSupplyTile);
        Inc(fBuildReserve, gRes.Houses[fType].HealthSupplyStep + 5);
      end;

    end else
    begin
      Dec(fBuildSupplyStone);
      Inc(fBuildReserve, gRes.Houses[fType].HealthSupplyStep + 5);
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
  Result := gRes.Houses[fType].SnowPic;


  if (fStyle > 0) and (gRes.Houses[fType].Styles[fStyle - 1].SnowPic > 0)  then
    Result := gRes.Houses[fType].Styles[fStyle - 1].SnowPic
  else
  if (fLevel.CurrentLevel > 0) and (gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].SnowPic > 0) then
    Result := gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].SnowPic;

  if Result < 1000 then
    Result := -1;
end;


// Increase building progress of house. When it reaches some point Stoning replaces Wooding
// and then it's done and house should be finalized
// Keep track on stone/wood reserve here as well
procedure TKMHouse.IncBuildingUpgradeProgress;
  procedure FinishUpgrade;
  var I : Integer;
    isMax : array[1..WARES_IN_OUT_COUNT] of Boolean;
  begin
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
    fPointBelowEntrance := KMPointBelow(fEntrance);

    for I := 1 to WARES_IN_OUT_COUNT do
      if isMax[I] then
        fWareBlocked[I] := 0
      else
        fWareBlocked[I] := GetMaxInWare - fWareIn[I];

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

    //gScriptEvents.ProcHouseBuilt(Self); //At the end since it could destroy this house
  end;
begin
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
procedure TKMHouse.AddDamage(aAmount: Word; aAttacker: TObject; aIsEditor: Boolean = False);
var
  attackerHand: TKMHandID;
begin
  if IsDestroyed then
    Exit;

  //if (fType in WALL_HOUSES) or (fType = htWallTower) then
  //  aAmount := aAmount * 3;
  //(NoGlyph houses MaxHealth = 0, they get destroyed instantly)
  fDamage := Math.min(fDamage + aAmount, MaxHealth);
  if IsComplete then
  begin
    if BuildingRepair then
      gHands[Owner].Constructions.RepairList.AddHouse(Self);

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


//Add repair to the house
procedure TKMHouse.AddRepair(aAmount: Word = 5);
var
  oldDmg: Integer;
begin
  oldDmg := fDamage;
  fDamage := EnsureRange(fDamage - aAmount, 0, High(Word));
  UpdateDamage;

  if gGameParams.Mode <> gmMapEd then
    gScriptEvents.ProcHouseRepaired(Self, oldDmg - fDamage, fDamage);
end;


//Update house damage animation
procedure TKMHouse.UpdateDamage;
var
  dmgLevel: Word;
begin
  dmgLevel := MaxHealth div 8; //There are 8 fire places for each house, so the increment for each fire level is Max_Health / 8
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
procedure TKMHouse.CheckOnSnow;
var
  I: Integer;
  snowTiles, noSnowTiles: Integer;
  cells: TKMPointTagList;
begin
  cells := TKMPointTagList.Create;

  GetListOfGroundVisibleCells(cells);

  snowTiles := 0;
  noSnowTiles := 0;
  for I := 0 to cells.Count - 1 do
    if gTerrain.TileIsSnow(cells[I].X, cells[I].Y) then
      Inc(snowTiles, cells.Tag[I])
    else
      Inc(noSnowTiles, cells.Tag[I]);

  fIsOnSnow := snowTiles > noSnowTiles;

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
    fNeedIssueOrderCompletedMsg := True;
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
procedure TKMHouse.SetStyle(aValue : Byte);
begin
  fStyle := EnsureRange(aValue, 0, Length(HSpec.Styles));
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
      Dec(Result, C);
    end;
    if Result <= 0 then
      Exit;
    C := Min(CheckWareIn(aWare), aMaxCount);//check ware out first
    if C > 0 then
    begin
      WareTakeFromIn(aWare, C, true);
      Dec(Result, C);
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


  C := TryToTakeWares(wtTimber, lvl.WoodCost);
  fBuildWoodDelivered := lvl.WoodCost - C;
  fBuildSupplyWood := fBuildWoodDelivered;
  if C > 0 then
    gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtTimber, C, dtOnce, diHigh4);

  C := TryToTakeWares(wtStone, lvl.StoneCost);
  fBuildStoneDelivered := lvl.StoneCost - C;
  fBuildSupplyStone := fBuildStoneDelivered;
  if C > 0 then
    gHands[Owner].Deliveries.Queue.AddDemand(self, nil, wtStone, C, dtOnce, diHigh4);

  C := TryToTakeWares(wtTile, lvl.TileCost);
  fBuildTileDelivered := lvl.TileCost - C;
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

procedure TKMHouse.ProduceWare(aWare: TKMWareType; aCount: Integer = 1);
begin
  if aCount > 0 then
  begin
    WareAddToOut(aWare, aCount);
    gHands[Owner].Stats.WareProduced(aWare, aCount);
    gScriptEvents.ProcWareProduced(self, aWare, aCount);
  end else
  if aCount < 0 then
  begin
    aCount := -aCount;
    WareTakeFromIn(aWare, aCount);
    gHands[Owner].Stats.WareConsumed(aWare, aCount);
  end;

end;

// Check if house has enough resource supply to be built depending on it's state
function TKMHouse.CheckResToBuild: Boolean;
begin
  case fBuildState of
    hbsWood:   Result := (fBuildSupplyWood > 0) or (fBuildReserve > 0);
    hbsStone:  begin
                  Result := ((fBuildSupplyStone > 0) or (fBuildReserve > 0));

                  if (fBuildStoneDelivered = gRes.Houses[fType].StoneCost) then
                    Result := Result or (gRes.Houses[fType].TileCost = 0) or (fBuildSupplyTile > 0) or (fBuildReserve > 0);
              end;
    hbsDone:  if IsUpgrading then
                Result := (fBuildSupplyWood > 0) or (fBuildSupplyStone > 0) or (fBuildSupplyTile > 0) or (fBuildReserve > 0)
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
    if not (fType in [htStore, htTownhall, htMarket, htBarracks, htInn, htPalace]) then
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


// Add resources to building process
function TKMHouse.WareAddToBuild(aWare: TKMWareType; aCount: Integer = 1) : Boolean;
var maxWood, maxStone, maxTile : Byte;
begin
  Result := true;
  maxWood := gRes.Houses[fType].WoodCost;
  maxStone := gRes.Houses[fType].StoneCost;
  maxTile := gRes.Houses[fType].TileCost;

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
    if aWare = fWareInput[I] then
      Result := True;
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
    if not (aWare in [wtAll, wtNone, wtFood]) then
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

  if DontNeedRes then
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
  I, K, p, count: integer;
  isWareBoth : Boolean;
begin
  Assert(aWare <> wtNone);
  Assert(not(fType in [htStore,htBarracks,htTownHall]));

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
  SaveStream.Write(fIsOnSnow);
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

  newCount := length(fWorkers);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMUnit(fWorkers[I]).UID);

  newCount := length(fNotAcceptWorkers);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
    SaveStream.Write(TKMUnit(fNotAcceptWorkers[I]).UID);
end;


procedure TKMHouse.PostLoadMission;
begin
  //Do nothing, override where needed
end;


procedure TKMHouse.IncAnimStep;
const
  //How much ticks it takes for a house to become completely covered in snow
  SNOW_TIME = 300;
var
  I, K: Integer;
  wasOnSnow: Boolean;
  HA: TKMHouseArea;
begin
  Inc(FlagAnimStep);
  WorkAnimStepPrev := WorkAnimStep;
  Inc(WorkAnimStep);

  if (FlagAnimStep mod 10 = 0) and gGameParams.IsMapEditor then
  begin
    wasOnSnow := fIsOnSnow;
    CheckOnSnow;
    if not wasOnSnow or not fIsOnSnow then
      fSnowStep := 0;
  end;

  if fIsOnSnow and (fSnowStep < 1) then
    fSnowStep := Min(fSnowStep + (1 + Byte(gGameParams.IsMapEditor) * 10) / SNOW_TIME, 1);

  //FlagAnimStep is a sort of counter to reveal terrain once a sec
  if gGameParams.DynamicFOW and (FlagAnimStep mod FOW_PACE = 0) then
  begin
    HA := gRes.Houses[fType].BuildArea;
    //Reveal house from all points it covers
    for I := 1 to 4 do
      for K := 1 to 4 do
        if HA[I,K] <> 0 then
          gHands.RevealForTeam(Owner, KMPoint(fPosition.X + K - 4, fPosition.Y + I - 4), gRes.Houses[fType].Sight, FOG_OF_WAR_INC, frtHouse);
  end;
end;

procedure TKMHouse.IncProductionCycle(aWare: TKMWareType);
var I, aIndex, aMod: Integer;
begin
  aIndex := 0;
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = fWareOutput[I] then
      aIndex := I;

  if aIndex = 0 then Exit;
  Inc(fProductionCycles[aIndex]);

  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    if HouseType in gRes.Wares.VirtualWares[I].ProduceInHouses then
    begin
      aMod := gRes.Wares.VirtualWares[I].GetModulo(HouseType, aWare);
      if aMod > 0 then
        if fProductionCycles[aIndex] mod aMod = 0 then
          gHands[Owner].SetVirtualWareCnt(gRes.Wares.VirtualWares[I].Name, gRes.Wares.VirtualWares[I].GetProdCount(HouseType, aWare));
    end;
end;

//Request more wares (if distribution of wares has changed)
//todo: Situation: I have timber set to 5 for the weapons workshop, and no timber in my village.
//      I change timber to 0 for the weapons workshop. My woodcutter starts again and 5 timber is still
//      taken to the weapons workshop because the request doesn't get canceled.
//      Maybe it's possible to cancel the current requests if no serf has taken them yet?
procedure TKMHouse.UpdateDemands;
Const MAX_ORDERS = 10;
var
  I: Integer;
  demandsRemoved, plannedToRemove, demandsToChange: Integer;
  maxDistribution: Byte;
  resDelivering : Integer;
begin
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if (fType = htTownHall) or (fWareInput[I] in [wtAll, wtWarfare, wtNone]) then Continue;

    resDelivering := WareDeliveryCnt[I] - WareDemandsClosing[I];

    maxDistribution := Min(GetWareDistribution(I), GetMaxInWare - GetAcceptWareIn(fWareInput[I]));
    if fResetDemands then
      maxDistribution := 0;

    //demandsToChange := resDistribution - (WareDeliveryCnt[I] - WareDemandsClosing[I]);

    //demandsToChange := Min( 5 - (demandsToChange - fWareIn[I]), GetMaxInWare -  demandsToChange);

    demandsToChange := Min( MAX_ORDERS - (resDelivering - fWareIn[I]), maxDistribution -  resDelivering);

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
  HA: TKMHouseArea;
begin
  if not IsComplete then
  begin
    if gGameParams.DynamicFOW and ((aTick + Owner) mod FOW_PACE = 0) then
    begin
      HA := gRes.Houses[fType].BuildArea;
      //Reveal house from all points it covers
      for I := 1 to 4 do
        for K := 1 to 4 do
          if HA[I,K] <> 0 then
            gHands.RevealForTeam(Owner, KMPoint(fPosition.X + K - 4, fPosition.Y + I - 4), HOUSE_PLAN_SIGHT, FOG_OF_WAR_INC, frtHouse);
    end;
    if HouseType in WALL_HOUSES then
      UpdateEntrancePos;
    Exit; //Don't update unbuilt houses
  end;


  fTick := aTick;

  if CurrentAction.State = hstWork then
    Inc(WorkingTime)
  else
    WorkingTime := 0;

  if HouseType in WALL_HOUSES then
    if IsUpgrading then
      UpdateEntrancePos;

  //Update delivery mode, if time has come
  if (fUpdateDeliveryModeOnTick = fTick) then
    UpdateDeliveryMode;

  if fType = htWallTower then
  begin
    if CurrentAction.State = hstIdle then
      if fTick mod 300 = 0 then
        SetState(hstIdle);
  end;


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
  if not fDisableUnoccupiedMessage and not HasWorker and not fIsClosedForWorker
  and gRes.Houses[fType].CanHasWorker and (fType <> htBarracks) then
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

  IncAnimStep;
end;


procedure TKMHouse.Paint;
var
  H: TKMHouseSpec;
  progress: Single;
  I : Integer;
begin
  H := gRes.Houses[fType];
  case fBuildState of
    hbsNoGlyph:; //Nothing
    hbsWood:   begin

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
                end;
    else
                if IsUpgrading then
                begin
                  progress := (1 - fLevel.BuildingProgress / gRes.Houses[fType].Levels[fLevel.CurrentLevel].Progress);
                  if fLevel.CurrentLevel > 0 then
                  begin
                    gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, gRes.Houses[fType].Levels[fLevel.CurrentLevel - 1].StonePic, gRes.Houses[fType].Levels[fLevel.CurrentLevel].StonePic, -1, false, false, 0);
                    gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                  end else
                  begin
                    gRenderPool.AddHouse(fType, fPosition, 1, progress, 0, GetStonePic, gRes.Houses[fType].Levels[fLevel.CurrentLevel].StonePic, -1, false, false, 0);
                    gRenderPool.AddHouseBuildSupply(fType, fPosition, fBuildSupplyWood, fBuildSupplyStone, fBuildSupplyTile);
                  end;
                
                end else
                begin
                  //Incase we need to render house at desired step in debug mode
                  if HOUSE_BUILDING_STEP = 0 then
                  begin
                    if fIsOnSnow then
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
                      gRenderPool.AddAnimation(fPosition, gRes.Houses.Silo_Tablets, fSlotID, gHands[Owner].GameFlagColor, rxHouses);

                    if (CurrentLevel > 0) and (length(HSpec.Levels[CurrentLevel - 1].Anim) > 0) then
                    begin
                      if not HSpec.Levels[CurrentLevel - 1].HideSupplies then
                        gRenderPool.AddHouseSupply(fType, fPosition, fWareIn, fWareOut, fWareOutPool);

                      for I := 0 to High(HSpec.Levels[CurrentLevel - 1].Anim) do
                        gRenderPool.AddAnimation(fPosition, HSpec.Levels[CurrentLevel - 1].Anim[I], WorkAnimStep, gHands[Owner].GameFlagColor, rxHouses);

                      if not HSpec.Levels[CurrentLevel - 1].ReplaceAnim then
                        gRenderPool.AddHouseWork(fType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);


                    end else
                    if CurrentAction <> nil then
                      gRenderPool.AddHouseWork(fType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);
                    
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

procedure TKMHouseTower.Paint;
var
  fillColor, lineColor: Cardinal;
begin

  inherited;
  if fType = htWallTower then Exit;
  
  if SHOW_ATTACK_RADIUS or (mlTowersAttackRadius in gGameParams.VisibleLayers) then
  begin
    fillColor := $40FFFFFF;
    lineColor := icWhite;
    if gMySpectator.Selected = Self then
    begin
      fillColor := icRed and fillColor;
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

  if RightRecruit then
    gRenderPool.AddAnimation(Position, gRes.Houses.WallTower_RecruitRight, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
  if LeftRecruit then
    gRenderPool.AddAnimation(Position, gRes.Houses.WallTower_RecruitLeft, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);

  if SHOW_ATTACK_RADIUS or (mlTowersAttackRadius in gGameParams.VisibleLayers) then
  begin
    fillColor := $40FFFFFF;
    lineColor := icWhite;
    if gMySpectator.Selected = Self then
    begin
      fillColor := icRed and fillColor;
      lineColor := icCyan;
    end;
    if HouseType = htWallTower then
      gRenderPool.RenderDebug.RenderTiledArea(Position, RANGE_WATLLTOWER_MIN, RANGE_WALLTOWER_MAX, GetLength, fillColor, lineColor);
  end;

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
begin
  if not IsComplete then  Exit;
  if CheckWareOut(wtWater) > 0 then
    Exit;

  Inc(fProgress);
  if fProgress = 180 then
  begin
    ProduceWare(wtWater);
    Inc(fProductionCycles[1]);
    if fProductionCycles[1] mod 100 = 0  then
    begin
      fProductionCycles[1] := fProductionCycles[1] - 100;
      gHands[Owner].VirtualWareTake('vtPearl', -1);
    end;

    fProgress := 0;
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
             and (fCurrentHand <> Owner)
             and (fStore <> nil);
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

  gRenderPool.AddAnimation(fPosition, gRes.Houses.Merchant_Tablets, fSlotID, gHands[Owner].GameFlagColor, rxHouses);
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
end;

function TKMHouseAppleTree.CanWork: Boolean;
begin
  Result := fGrowPhase in [0, 1, 2, 3]; //0..3: water the tree, 7: gather apples

  Result := Result or (fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1);
end;

function TKMHouseAppleTree.IncGrowPhase: Boolean;
begin
  Result := false;
  if fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1 then
  begin
    if fPhase > 0 then
    begin
      fGrowPhase := gFruitTrees[fFruitTreeID].MatureTreeStage;
      Dec(fPhase);
    end
    else
    begin
      fPhase := 3;
      fGrowPhase := 0;
    end;


    Result := true;
  end else
    Inc(fGrowPhase);

  //fWariant := fGrowPhase;

  SetProgress;
end;

procedure TKMHouseAppleTree.MakeFruits;
var count : Byte;
begin
  fFillFruits := fFillFruits + gFruitTrees[fFruitTreeID].Fruits;
  count := trunc(fFillFruits);
  if count > 0 then
  begin
    ProduceWare(wtApple, count);
    Inc(fProductionCycles[1], count);
  end;
  fFillFruits := fFillFruits - count;
end;


function TKMHouseAppleTree.NeedWater: Boolean;
begin
  Result := fGrowPhase in [0, 1, 2, 3];
end;


procedure TKMHouseAppleTree.SetProgress;
begin
  if fGrowPhase = gFruitTrees[fFruitTreeID].StagesCount - 1 then
    fProgress := 0
  else
    fProgress := gFruitTrees[fFruitTreeID].ProgressPerStage;

  {
    case fGrowPhase of
      0..6 : fProgress := 400 + KamRandom(400, 'AppleTreeNextStage');
      7: fProgress := 0; //set delay before animation
    end;
  }
end;

procedure TKMHouseAppleTree.SetFruitTreeID(aValue: ShortInt);
begin
  if not gGameParams.IsMapEditor then
    gTerrain.RemoveObject(Entrance);
  fFruitTreeID := aValue;
end;

procedure TKMHouseAppleTree.SetGrowPhase(aValue: Byte);
begin
  fGrowPhase := aValue;
  fProgress := gFruitTrees[fFruitTreeID].ProgressPerStage;

end;

constructor TKMHouseAppleTree.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
begin
  inherited;
end;

procedure TKMHouseAppleTree.Activate(aWasBuilt: Boolean);
var obj : Word;
begin
  Inherited;
  fProgress := 0;
  fGrowPhase := 0;
  fWariant := 0;
  fFillFruits := 0;
  fFruitTreeID := 0;

  {if not aWasBuilt then
  begin
    fPhase := 3 + KamRandom(4, 'TKMHouseAppleTree.Activate:Randomize cut apple tree');
    //fGrowPhase := 4 + KamRandom(4, 'Randomize Apple trees');
    SetGrowPhase(gFruitTrees[fFruitTreeID].MatureTreeStage);
  end;}
  obj := gTerrain.Land[Entrance.Y, Entrance.X].Obj;
  if (obj <> OBJ_NONE) and (gMapElements[obj].IsFruit > 0) then
  begin
    fFruitTreeID := gMapElements[obj].IsFruit - 1;

    SetGrowPhase(gFruitTrees[fFruitTreeID].GetStage(obj));

    if not gGameParams.IsMapEditor then
      gTerrain.RemoveObject(Entrance);
  end else
  begin
    fFruitTreeID := 0;
    if aWasBuilt then
      SetGrowPhase(0)
    else
      SetGrowPhase(gFruitTrees[fFruitTreeID].MatureTreeStage);
  end;
  if not gGameParams.IsMapEditor then
    gTerrain.RemoveObject(Entrance);

  fFillFruits := 0;
end;

constructor TKMHouseAppleTree.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('HouseAppleTree');
  LoadStream.Read(fProgress);
  LoadStream.Read(fPhase);
  LoadStream.Read(fGrowPhase);
  LoadStream.Read(fFruitTreeID);

end;

procedure TKMHouseAppleTree.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('HouseAppleTree');
  SaveStream.Write(fProgress);
  SaveStream.Write(fPhase);
  SaveStream.Write(fGrowPhase);
  SaveStream.Write(fFruitTreeID);
end;

procedure TKMHouseAppleTree.UpdateState(aTick: Cardinal);
begin
  Inherited;
  if not IsComplete then
    Exit;
  //Don't fo anything for these phases. The worker has animation here.
  if CanWork{fGrowPhase in [0,1,2,3, 7]} then
    Exit;

  if fProgress > 0 then
  begin
    Dec(fProgress);
    Exit;
  end;

  if fProgress = 0 then
    IncGrowPhase;
end;

function TKMHouseAppleTree.ObjToString(const aSeparator: String = '|'): String;
begin
  Result := Inherited ObjToString(aSeparator) + Format('%s fProgress: %d%s fGrowPhase: %d%s CanWork: %s', [aSeparator, fProgress, aSeparator, fGrowPhase, aSeparator, BoolToStr(CanWork, true)])

end;

procedure TKMHouseAppleTree.Paint;
begin
  Inherited;

  if not IsComplete then
    Exit;
  gRenderPool.RenderMapElement(gFruitTrees[fFruitTreeID].Stage[fGrowPhase], gTerrain.AnimStep, Entrance.X, Entrance.Y, false, false ,true);

  gRenderPool.AddHouseSupply(HouseType, fPosition, [0, CheckWareIn(wtWater), 0, 0], [], []);
  if CurrentAction <> nil then
    gRenderPool.AddHouseWork(HouseType, fPosition,
                            CurrentAction.SubAction * [haWork1, haWork2, haWork3, haWork4, haWork5],
                            WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);
  //first house
  //second tree
  //and the rest



end;
constructor TKMHousePottery.Load(LoadStream: TKMemoryStream);
var I : Integer;
begin
  Inherited;
  LoadStream.CheckMarker('HousePottery');

  for I := 1 to high(fTiles) do
    LoadStream.Read(fTiles[I]);

  LoadStream.Read(fLastTile);
end;

procedure TKMHousePottery.Save(SaveStream: TKMemoryStream);
var I : Integer;
begin
  Inherited;
  SaveStream.PlaceMarker('HousePottery');
  for I := 1 to high(fTiles) do
    SaveStream.Write(fTiles[I]);

  SaveStream.Write(fLastTile);
end;

function TKMHousePottery.TakeTile : Byte;
var J : Integer;
begin
  Result := 0;
  J := (fLastTile) mod 4 + 1 ;

  fLastTile := J;

  Inc(fTiles[J]);

  if fTiles[J] = 2 then
  begin
    Result := J;
    fTiles[J] := 0;
  end;
end;

procedure TKMHousePottery.Paint;
var I : Integer;
begin
  Inherited;

  for I := 1 to high(fTiles) do
    if fTiles[I] = 1 then
      gRenderPool.AddHousePotteryTiles(fPosition, I);
end;

function TKMHousePottery.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited ObjToString(aSeparator) +
            Format('%sLastTile = %d', [aSeparator, fLastTile]);
end;

function TKMHouseCollectors.GetMaxDistanceToPoint: Integer;
begin
  Result := 20;
end;

constructor TKMHousePalace.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
begin
  Inherited;

  SetLength(fProgress, length(PALACE_UNITS_ORDER));
  SetLength(fPhase, length(PALACE_UNITS_ORDER));
  SetLength(fWait, length(PALACE_UNITS_ORDER));
  SetLength(fOrderCount, length(PALACE_UNITS_ORDER));
  SetLength(VWareIDs, length(PALACE_UNITS_ORDER));
  SetUnitVWareArray;
  fTrainingID := high(Byte);
end;

procedure TKMHOusePalace.SetUnitVWareArray;
var I, K : Integer;
  tmp : Integer;
  UT : TKMUnitType;
begin

  for I := 0 to High(VWareIDs) do
  begin
    SetLength(VWareIDs[I], 0);

    UT := PALACE_UNITS_ORDER[I];
    for K := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
      with gRes.Units[UT].PalaceCost do
      begin
        tmp := gRes.Wares.VirtualWares.WareS[Wares[K].W].Index;
        Wares[K].Index := tmp;
        if ArrayContains(tmp, VWareIDs[I]) then
          Continue;
        SetLength(VWareIDs[I], length(VWareIDs[I]) + 1);
        VWareIDs[I][high(VWareIDs[I])] := tmp;
      end;

  end;
    
end;

function TKMHousePalace.GetMaxDistanceToPoint: Integer;
begin
  Result := 999;
end;

function TKMHousePalace.UnitProgress(aType: TKMUnitType): Word;
begin
  Result := Max(gRes.Units[aType].PalaceCost.PhaseDuration, 50);
end;

function TKMHousePalace.GetPhaseCount(aIndex : Integer) : Byte;
begin
  Result := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.PhaseCount;
end;

function TKMHousePalace.GetCurrentPhase(aIndex : Integer) : Byte;
begin
  Result := fPhase[aIndex];
end;

constructor TKMHousePalace.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
  tmp : Word;
begin
  Inherited;


  SetLength(fProgress, length(PALACE_UNITS_ORDER));
  SetLength(fPhase, length(PALACE_UNITS_ORDER));
  SetLength(fWait, length(PALACE_UNITS_ORDER));
  SetLength(fOrderCount, length(PALACE_UNITS_ORDER));
  SetLength(VWareIDs, length(PALACE_UNITS_ORDER));

  LoadStream.CheckMarker('HousePalace');
  LoadStream.Read(fTrainingID);
  LoadStream.Read(newCount);

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
  end;
  SetUnitVWareArray;
end;

procedure TKMHousePalace.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  Inherited;
  SaveStream.PlaceMarker('HousePalace');

  newCount := length(fProgress);
  SaveStream.Write(fTrainingID);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(fProgress[I]);
    SaveStream.Write(fPhase[I]);
    SaveStream.Write(fWait[I]);
    SaveStream.Write(fOrderCount[I]);
  end;

end;

function  TKMHousePalace.GetOrderCount(aIndex : Integer) : Word;
begin
  Assert(aIndex < length(fOrderCount), 'Wrong Index');
  Result := fOrderCount[aIndex];
end;

procedure TKMHousePalace.SetOrderCount(aIndex : Integer; aValue : Word);
var I : Integer;
begin
  Assert(aIndex < length(fOrderCount), 'Wrong Index');

  for I := 0 to High(fOrderCount) do
    if  I <> aIndex then
      fOrderCount[I] := 0;

  //if FullProgress[aIndex] > 0 then
  //  Exit;
  fOrderCount[aIndex] := EnsureRange(aValue, 0, high(fOrderCount[aIndex]));

end;

function  TKMHousePalace.GetMaxProgress(aIndex : Integer) : Integer;
begin
  Assert(aIndex < length(fProgress), 'Wrong Index');

  Result := UnitProgress(PALACE_UNITS_ORDER[aIndex]) * Max(gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.PhaseCount, 1);

end;

function  TKMHousePalace.GetFullProgress(aIndex : Integer) : Single;
var currProg : Integer;
begin
  Assert(aIndex < length(fProgress), 'Wrong Index');


  if (fProgress[aIndex] = 0) and (fPhase[aIndex] = 0) then
    Exit(0);

  currProg := UnitProgress(PALACE_UNITS_ORDER[aIndex]) * fPhase[aIndex];
  currProg := currProg + (UnitProgress(PALACE_UNITS_ORDER[aIndex]) - fProgress[aIndex]);
  Result := currProg / GetMaxProgress(aIndex);

end;

function  TKMHousePalace.GetPhaseProgress(aIndex: Integer): Single;
begin
  Assert(aIndex < length(fProgress), 'Wrong Index');

  if (fProgress[aIndex] = 0) and (fPhase[aIndex] = 0) then
    Exit(0);

  with gRes.Units[PALACE_UNITS_ORDER[aIndex]] do
  begin
    Result := (PalaceCost.PhaseDuration - fProgress[aIndex])/ PalaceCost.PhaseDuration
  end;
end;

function TKMHousePalace.GetPhaseColor(aIndex : Integer) : Cardinal;
begin
  Result := MixColor(icCyan, icGoldenYellow, GetFullProgress(aIndex));
end;

function TKMHousePalace.IsTraining: Boolean;
begin
  Result := fTrainingID <> high(byte);
end;

procedure TKMHousePalace.CancelOrder(aIndex : Integer);
var I : Integer;
  WP : TKMWarePlan;
begin
  Assert(aIndex < length(fProgress), 'Wrong Index');
  WP := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.Plan;

  fProgress[aIndex] := 0;
  fPhase[aIndex] := 0;
  fWait[aIndex] := 0;
  fOrderCount[aIndex] := 0;
  for I := Low(WP) to High(WP) do
    if (WP[I].W = wtJewerly) and (WP[I].C > 0) then
    begin
      WareAddToIn(wtJewerly);
      Break;
    end;
  fTrainingID := high(byte);
end;

function TKMHousePalace.GetWarePlan(aIndex : Integer = -1): TKMWarePlan;
var I : Integer;
begin
  if aIndex = -1 then
    for I := 0 to High(fProgress) do
      if (fProgress[I] > 0) or (fPhase[I] > 0) or (fOrderCount[I] > 0) then
      begin
        aIndex := I;
        break;
      end;
  if aIndex = -1 then
    Exit;
  if fProgress[aIndex] > 0 then
    Exit;

  Result := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.Plan;

  if fPhase[aIndex] > 0 then
    for I := Low(Result) to High(Result) do
      if (Result[I].W <> wtNone) then
      begin
        if Result[I].W = wtJewerly then
        begin
          Result[I].W := wtNone;
          Result[I].C := 1;
        end;
      end;
end;

function TKMHousePalace.GetFullCost(aIndex : Integer = -1): TKMWarePlan;
var I : Integer;
begin
  if aIndex = -1 then
    for I := 0 to High(fProgress) do
      if (fProgress[I] > 0) or (fPhase[I] > 0) or (fOrderCount[I] > 0) then
      begin
        aIndex := I;
        break;
      end;

  Result := gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.Plan;

  for I := Low(Result) to High(Result) do
    if (Result[I].W <> wtNone) then
    begin
      if Result[I].W <> wtJewerly then
        Result[I].C := Result[I].C + 1 * (gRes.Units[PALACE_UNITS_ORDER[aIndex]].PalaceCost.PhaseCount - 1);

    end;
end;

function TKMHousePalace.HasWares(aIndex: Integer): Boolean;
var K : Integer;
    UT : TKMUnitType;
    WP : TKMWarePlan;
begin
  UT := PALACE_UNITS_ORDER[aIndex];
  Result := True;
  if GetPhaseCount(aIndex) = 0 then
    Exit;

  if fPhase[aIndex] = 0 then
    for K := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
      with gRes.Units[UT].PalaceCost.Wares[K] do
        if gHands[Owner].VirtualWare[Index] < C then
          Exit(false);

  WP := GetWarePlan(aIndex);

  for K := 0 to 3 do
    with WP[K] do
      if (W <> wtNone) and (C > 0) then
        if CheckWareIn(W) < IfThen(fPhase[aIndex] = 0, C, 1) then
          Exit(false);
end;

procedure TKMHousePalace.TakeWares(aIndex : Integer);
var K : Integer;
  UT : TKMUnitType;
  WP : TKMWarePlan;
begin
  UT := PALACE_UNITS_ORDER[aIndex];
  if GetPhaseCount(aIndex) = 0 then
    Exit;

  if fPhase[aIndex] = 0 then
    for K := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
      with gRes.Units[UT].PalaceCost.Wares[K] do
        gHands[Owner].VirtualWareTake(W, C);

  WP := GetWarePlan(aIndex);
  for K := 0 to 3 do
    with WP[K] do
      if (W <> wtNone) and (C > 0) then
        WareTakeFromIn(W,IfThen(fPhase[aIndex] = 0, C, 1));

end;

function TKMHousePalace.CanEquip(aIndex: Integer): Boolean;
begin
  Result := HasWares(aIndex);
end;

procedure TKMHousePalace.UpdateState(aTick: Cardinal);
var Soldier : TKMUnitWarrior;
  I, L : Integer;
begin
  Inherited;

  for I := 0 to High(fProgress) do
  if (fTrainingID = 255) or (I = fTrainingID) then     
  begin

    if fWait[I] > 0 then
    begin
      Dec(fWait[I]);
      Continue;
    end;
    if fProgress[I] > 0 then
    begin
      Dec(fProgress[I]);

      if fProgress[I] = 0 then
      begin
        fWait[I] := 100;
        Inc(fPhase[I]);

        if fPhase[I] = GetPhaseCount(I) then
        begin
          fTrainingID := high(byte);
          gSoundPlayer.Play(sfxnPalace, fPosition);
          soldier := TKMUnitWarrior(gHands[Owner].TrainUnit(PALACE_UNITS_ORDER[I], Self));
          soldier.Visible := False; //Make him invisible as he is inside the barracks
          soldier.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
          //Soldier.OrderLoc := KMPointBelow(Entrance); //Position in front of the barracks facing north
          soldier.SetActionGoIn(uaWalk, gdGoOutside, Self);
          if Assigned(soldier.OnUnitTrained) then
            soldier.OnUnitTrained(soldier);
          if gHands[Owner].IsComputer then
            if gRes.Units[soldier.UnitType].CanOrderAmmo then
              soldier.OrderAmmo;
          fPhase[I] := 0;
        end;

      end;

    end;


    if fProgress[I] = 0 then
    begin
      if fPhase[I] > 0 then
      begin
        if HasWares(I) then
        begin
          TakeWares(I);
          fProgress[I] := UnitProgress(PALACE_UNITS_ORDER[I])
        end;
      end
      else
      if fOrderCount[I] > 0 then
        if gHands[Owner].GetWorklessCount > 0 then
          if HasWares(I) then
          begin
            fTrainingID := I;
            TakeWares(I);
            gHands[Owner].TakeWorkless;
            Dec(fOrderCount[I]);
            fProgress[I] := UnitProgress(PALACE_UNITS_ORDER[I]);
          end;

    end;

  end;


end;

procedure TKMHousePalace.Paint;
var I : Integer;
begin
  Inherited;

  if fBuildState = hbsDone then
    for I := 1 to 4 do
        gRenderPool.AddHousePalaceFlags(HouseType, fPosition, I, FlagAnimStep + I * 10, gHands[Owner].FlagColor);

end;

function TKMHousePalace.ObjToString(const aSeparator: string = '|'): string;
begin
  Result := inherited ObjToString(aSeparator) +
            Format('%sPhase[1] : %d%s' +
                    'Progress[1] : %d%s' +
                    'MaxProgress[1] : %d%s' +
                    'fWait[1] : %d',
                  [aSeparator,
                    fPhase[0], aSeparator,
                    fProgress[0], aSeparator,
                    GetMaxProgress(0), aSeparator,
                    fWait[0]]);
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
      case KaMRandom(100, 'TKMHouseStall.SetRandomCost:Type') of
        0..9 : fCostMultiplier[I] := 0.1 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        10..49 : fCostMultiplier[I] := 0.4 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        50..84 : fCostMultiplier[I] := 0.3 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        85..99 : fCostMultiplier[I] := 0.2 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
      end;

    for I := Low(fWareMultiplier) to High(fWareMultiplier) do
      fWareMultiplier[I] := 1 + KaMRandom('');
  end else
  begin
    if gGameParams.Tick < fSpecialPriceTick then
      Exit;

    for I := 0 to High(fCostMultiplier) do
      case KaMRandom(100, 'TKMHouseStall.SetRandomCost:Type') of
        0..9 : fCostMultiplier[I] := 0.1 + KaMRandom('TKMHouseStall.SetRandomCost:Count');
        10..49 : fCostMultiplier[I] := 0.9 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 2;
        50..84 : fCostMultiplier[I] := 0.5 + KaMRandom('TKMHouseStall.SetRandomCost:Count');
        85..99 : fCostMultiplier[I] := 0.75 + KaMRandom('TKMHouseStall.SetRandomCost:Count') / 3;
      end;
    for I := Low(fWareMultiplier) to High(fWareMultiplier) do
      fWareMultiplier[I] := 0.75 + KaMRandom('') / 2;
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
      gRenderPool.AddSprite(Position, KMPoint(57 - 6 * I, 17 + 1 * I), gRes.Wares.VirtualWares.WareS[fVWares[I]].SpriteInStall, rxHouses, gHands[Owner].FlagColor);
  If gGameParams.Tick < fSpecialPriceTick then
    gRenderPool.AddSprite(Position, KMPoint(0, 0), 2508, rxHouses, gHands[Owner].FlagColor);
end;

constructor TKMHouseProdThatch.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: ShortInt; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fAnimStartTickSmoke1 := high(Integer);
  fAnimStartTickSmoke23 := high(Integer);
  fAnimStartTickDust := high(Integer);
  fAnimEndTickSmoke1 := high(Integer);
  fAnimEndTickSmoke23 := high(Integer);
  fAnimEndTickDust := high(Integer);
  fWorkPointsTaken.Clear;
  fGrainType := GRAIN_GUI_ORDER[0];
  fGrassType := GRASS_GUI_ORDER[0];
  fVegeType := VEGE_GUI_ORDER[0];
  fFillSeeds := 0;
  fFillCorn := 0;
  fFillHay := 0;
  fFillWine := 0;
  fFillVege := 0;
end;

constructor TKMHouseProdThatch.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  fWorkPointsTaken.Clear;
  LoadStream.Read(fAnimStartTickSmoke1);
  LoadStream.Read(fAnimStartTickSmoke23);
  LoadStream.Read(fAnimStartTickDust);
  LoadStream.Read(fAnimEndTickDust);
  LoadStream.Read(fAnimEndTickSmoke1);
  LoadStream.Read(fAnimEndTickSmoke23);
  LoadStream.Read(fLastTile);
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
end;

procedure TKMHouseProdThatch.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fAnimStartTickSmoke1);
  SaveStream.Write(fAnimStartTickSmoke23);
  SaveStream.Write(fAnimStartTickDust);
  SaveStream.Write(fAnimEndTickDust);
  SaveStream.Write(fAnimEndTickSmoke1);
  SaveStream.Write(fAnimEndTickSmoke23);
  SaveStream.Write(fLastTile);
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
var J : Byte;
begin
  J := (fLastTile mod high(fTiles)) + 1;
  fLastTile := J;
  Inc(fTiles[J]);
  if fTiles[J] = 2 then
  begin
    Result := J;
    fTiles[J] := 0;
  end;
end;


procedure TKMHouseProdThatch.ProduceStarts(aWare: TKMWareType);
begin
  case aWare of
    wtNone : ;
    wtBread: If fAnimStartTickSmoke1 = high(Integer) then fAnimStartTickSmoke1 := gGameParams.Tick;

    wtLog,
    wtWheel,
    wtTimber,
    wtSawDust : If fAnimStartTickDust = high(Integer) then fAnimStartTickDust := gGameParams.Tick;

    wtGold,
    wtIron,
    wtBitin,
    wtSteelE,
    wtBitinE: If fAnimStartTickSmoke23 = high(Integer) then fAnimStartTickSmoke23 := gGameParams.Tick;
  end;

  case aWare of
    wtNone : ;
    wtBread: fAnimEndTickSmoke1 := gGameParams.Tick + 800;

    wtLog,
    wtWheel,
    wtTimber,
    wtSawDust :  fAnimEndTickDust := gGameParams.Tick + 800;

    wtGold,
    wtIron,
    wtBitin,
    wtSteelE,
    wtBitinE: fAnimEndTickSmoke23 := gGameParams.Tick + 800;
  end;
end;

procedure TKMHouseProdThatch.MakeSound;
begin
  if SKIP_SOUND then Exit;

  if fAnimEndTickDust <> high(Integer) then
    if gGameParams.Tick mod 20 = 0 then
      gSoundPlayer.Play(sfxsaw, fPosition);

  if fAnimEndTickSmoke23 <> high(Integer) then
  begin
    if gGameParams.Tick mod 25 = 0 then
      gSoundPlayer.Play(sfxwineDrain, fPosition)
    else
    if gGameParams.Tick mod 15 = 0 then
      gSoundPlayer.Play(sfxmetallurgists, fPosition);
  end;

  if fAnimEndTickSmoke1 <> high(Integer) then
    if gGameParams.Tick mod 20 = 0 then
      gSoundPlayer.Play(sfxBakerSlap, fPosition);
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

function TKMHouseProdThatch.GetGrainTypes: TKMGrainTypeSet;
var I: integer;
begin
  Result := [];
  I := GetWareOutIndex(wtCorn);
  if I = 0 then
    Exit;

  if (I > 0) and (fWareOrder[I] > 0) then
    Result := Result + [fGrainType];

  I := GetWareOutIndex(wtSeed);
  if (I > 0) and (fWareOrder[I] > 0) then
    Result := Result + [fGrainType];

  I := GetWareOutIndex(wtHay);
  if (I > 0) and (fWareOrder[I] > 0) then
    Result := Result + [fGrassType];

  I := GetWareOutIndex(wtVegetables);
  if (I > 0) and (fWareOrder[I] > 0) then
    Result := Result + [fVegeType];
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
var count : Byte;
begin
  if WareOrder[GetWareOutIndex(wtCorn)] > 0 then
    fFillCorn := fFillCorn + gFieldGrains[aGrain].Straw;
  if WareOrder[GetWareOutIndex(wtSeed)] > 0 then
    fFillSeeds := fFillSeeds + gFieldGrains[aGrain].Seeds;
  if WareOrder[GetWareOutIndex(wtWine)] > 0 then
    fFillWine := fFillWine + gFieldGrains[aGrain].Wine;
  if WareOrder[GetWareOutIndex(wtHay)] > 0 then
    fFillHay := fFillHay + gFieldGrains[aGrain].Hay;
  if WareOrder[GetWareOutIndex(wtVegetables)] > 0 then
    fFillVege := fFillVege + gFieldGrains[aGrain].Vege;

  if GetWareOutIndex(wtCorn) > 0 then
    if WareOrder[GetWareOutIndex(wtCorn)] > 0 then
    begin
      count := trunc(fFillCorn);
      if count > 0 then ProduceWare(wtCorn, count);
      fFillCorn := fFillCorn - count;
    end;

  if GetWareOutIndex(wtSeed) > 0 then
    if WareOrder[GetWareOutIndex(wtSeed)] > 0 then
    begin

      count := trunc(fFillSeeds);
      if count > 0 then ProduceWare(wtSeed, count);
      fFillSeeds := fFillSeeds - count;
    end;

  if GetWareOutIndex(wtWine) > 0 then
    if WareOrder[GetWareOutIndex(wtWine)] > 0 then
    begin
      count := trunc(fFillWine);
      if count > 0 then ProduceWare(wtWine, count);
      fFillWine := fFillWine - count;
    end;

  if GetWareOutIndex(wtHay) > 0 then
    if WareOrder[GetWareOutIndex(wtHay)] > 0 then
    begin
      count := trunc(fFillHay);
      if count > 0 then ProduceWare(wtHay, count);
      fFillHay := fFillHay - count;
    end;

  if GetWareOutIndex(wtVegetables) > 0 then
    if WareOrder[GetWareOutIndex(wtVegetables)] > 0 then
    begin
      count := trunc(fFillVege);
      if count > 0 then ProduceWare(wtVegetables, count);
      fFillVege := fFillVege - count;
    end;

end;

procedure TKMHouseProdThatch.UpdateState(aTick: Cardinal);
begin
  Inherited;

  if not IsComplete then
    Exit;
    //try to stop animation
  if gGameParams.Tick >= fAnimEndTickDust then
    if (gGameParams.Tick - fAnimStartTickDust) mod gRes.Houses.ProdThatch_SawDust.Count = 0 then
    begin
      fAnimStartTickDust := high(Integer);
      fAnimEndTickDust := high(Integer);
    end;

  if gGameParams.Tick >= fAnimEndTickSmoke23 then
  begin
    fAnimStartTickSmoke23 := high(Integer);
    fAnimEndTickSmoke23 := high(Integer);
  end;

  if gGameParams.Tick >= fAnimEndTickSmoke1 then
  begin
    fAnimStartTickSmoke1 := high(Integer);
      fAnimEndTickSmoke1 := high(Integer);
  end;

end;

procedure TKMHouseProdThatch.Paint;
begin
  Inherited;

  if not IsComplete then
    Exit;

  if gGameParams.Tick >= fAnimStartTickDust then
    gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_SawDust, gGameParams.Tick - fAnimStartTickDust, gHands[Owner].FlagColor, rxHouses);

  if gGameParams.Tick >= fAnimStartTickSmoke23 then
  begin
    gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_Smoke1, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
    gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_Smoke2, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
    gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_Light, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
  end;
  if gGameParams.Tick >= fAnimStartTickSmoke1 then
    gRenderPool.AddAnimation(Position, gRes.Houses.ProdThatch_Smoke3, gTerrain.AnimStep, gHands[Owner].FlagColor, rxHouses);
end;

function TKMHouseFarm.GetGrainTypes: TKMGrainTypeSet;
begin
  Result := [fGrainType, fGrassType, fVegeType];
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


procedure TKMHouseFarm.GrainCut(aGrain: TKMGrainType);
var count : Byte;
begin
  fFillCorn := fFillCorn + gFieldGrains[aGrain].Straw;
  fFillSeeds := fFillSeeds + gFieldGrains[aGrain].Seeds;
  fFillHay := fFillHay + gFieldGrains[aGrain].Hay;
  fFillVege := fFillVege + gFieldGrains[aGrain].Vege;

  count := trunc(fFillCorn);
  if count > 0 then ProduceWare(wtCorn, count);
  fFillCorn := fFillCorn - count;

  count := trunc(fFillSeeds);
  if count > 0 then ProduceWare(wtSeed, count);
  fFillSeeds := fFillSeeds - count;

  count := trunc(fFillHay);
  if count > 0 then ProduceWare(wtHay, count);
  fFillHay := fFillHay - count;

  count := trunc(fFillVege);
  if count > 0 then ProduceWare(wtVegetables, count);
  fFillVege := fFillVege - count;
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
  wineCount := trunc(fFillWine);
  fFillWine := fFillWine - wineCount;

  if wineCount > 0 then
    ProduceWare(wtWine, wineCount);
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
  Result := Trunc(fFillWine);
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
end;

procedure TKMHouseShipyard.Activate(aWasBuilt: Boolean);
begin
  Inherited;
  fShipType := SHIPYARD_ORDER[0];
  fShipPhase := 0;
  fShipSketchPosition.Dir := KaMRandomDir('TKMHouseShipyard.Activate');
end;

procedure TKMHouseShipyard.Save(SaveStream: TKMemoryStream);
begin
  Inherited;

  SaveStream.Write(fShipType, SizeOf(fShipType));
  SaveStream.Write(fShipSketchPosition);
  SaveStream.Write(fShipPhase);
end;

procedure TKMHouseShipyard.IncSketchPhase;
  procedure FinishShip;
  begin
    gTerrain.UnlockTile(fShipSketchPosition.Loc);
    gHands[Owner].AddUnitGroup(fShipType, fShipSketchPosition.Loc, fShipSketchPosition.Dir, 1, 1);
    //fShipType := utNone;
    fShipPhase := 0;
    fShipSketchPosition.Loc := KMPOINT_INVALID_TILE;
  end;
var count : Integer;
begin
  Inc(fShipPhase);
  count := 0;
    case fShipType of
      utShip : count := gRes.Units.ShipSketch[fShipSketchPosition.Dir].Count;
    end;

  if fShipPhase >= count then
  begin
    FinishShip;
  end else
  if fShipPhase = 1 then
  begin
    fShipSketchPosition.Dir := KaMRandomDir('TKMHouseShipyard.Activate');
    fShipSketchPosition.Loc := gTerrain.FindPlaceForUnit(Position, tpFish, 5);
    gTerrain.SetTileLock(fShipSketchPosition.Loc, tlRoadWork);
  end;


end;

function TKMHouseShipyard.CanWork: Boolean;
begin
  Result := true;
end;

procedure TKMHouseShipyard.Paint;
  function SketchPointF : TKMPointF;
  begin
    Result.X := fShipSketchPosition.Loc.X + 0.5;
    Result.Y := fShipSketchPosition.Loc.Y + 0.5;
  end;
begin
  Inherited;

  if fShipPhase > 0 then
  begin
    case fShipType of
      utShip : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.ShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
    end;

  end;
  
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

