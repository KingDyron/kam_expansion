unit KM_Units;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, KromUtils, Types,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points, KM_CommonUtils, KM_UnitVisual,
  KM_ResHouses, KM_Houses, KM_HouseSchool, KM_HouseBarracks, KM_HouseInn, KM_ResUnits,
  KM_HouseCartographers,
  KM_Structure,
  KM_HandEntity,
  KM_ResTypes;

//Memo on directives:
//Dynamic - declared and used (overriden) occasionally
//Virtual - declared and used (overriden) always
//Abstract - declared but must be overriden in child classes

const
  MAX_WH_WARES = 5;
type
  TKMUnit = class;
  TKMUnitWorker = class;
  TKMUnitEvent = procedure(aUnit: TKMUnit) of object;

  TKMActionResult = (arActContinues, arActDone, arActAborted, arActCanNotStart);

  TKMUnitEffectType = (uetNone, uetHealing, uetSpeedUp, uetAttack, uetDefence);
  TKMUnitEffect = record
    Duration : Word;
    EffectType : TKMUnitEffectType;
  end;

  TKMUnitArray = array of TKMUnit;
  TKMDirRotate = (drNone, drLeft, drRight);
  TKMUnitAction = class abstract
  protected
    fType: TKMUnitActionType;
    fUnit: TKMUnit;
  public
    Locked: Boolean; //Means that unit can't take part in interaction, must stay on its tile
    StepDone: Boolean; //True when single action element is done (unit walked to new tile, single attack loop done)
    constructor Create(aUnit: TKMUnit; aActionType: TKMUnitActionType; aLocked: Boolean);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;

    function CanBeInterrupted(aForced: Boolean = True): Boolean; virtual;
    function ActName: TKMUnitActionName; virtual; abstract;
    property ActionType: TKMUnitActionType read fType;
    function GetExplanation: UnicodeString; virtual; abstract;
    function HasNoAnim : Boolean; virtual;
    function Execute: TKMActionResult; virtual; abstract;
    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure Paint; virtual;

    function ObjToStringShort(const aSeparator: String = ' '): String; virtual;
    function ObjToString(const aSeparator: String = ' '): String; virtual;
  end;

  TKMTaskResult = (trTaskContinues, trTaskDone); //There's no difference between Done and Aborted



  TKMUnitTask = class abstract
  private
    function GetPhase: Byte;
  protected
    fType: TKMUnitTaskType;
    fUnit: TKMUnit; //Unit who's performing the Task
    fPhase: Byte;
    fPhase2: Byte;
    fLastActionResult: TKMActionResult;
    procedure InitDefaultAction; virtual;
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); virtual;
    procedure SyncLoad; virtual;
    destructor Destroy; override;

    property Phase: Byte read GetPhase write fPhase;
    property TaskType: TKMUnitTaskType read fType;
    function WalkShouldAbandon: Boolean; virtual;

    function CouldBeCancelled: Boolean; virtual;
    function CanRestartAction(aLastActionResult: TKMActionResult): Boolean; virtual;

    function ObjToString(const aSeparator: String = ', '): String; virtual;

    function Execute: TKMTaskResult; virtual; abstract;
    procedure Save(SaveStream: TKMemoryStream); virtual;
    procedure UpdateState; virtual;

    procedure Paint; virtual;
  end;


  TKMUnit = class abstract(TKMHandEntityPointer<TKMUnit>)
  protected //Accessible for child classes
    fType: TKMUnitType;
    fTask: TKMUnitTask;
    fAction: TKMUnitAction;
    fThought: TKMUnitThought;

    fHitPoints: Byte;
    fHitPointCounter: Cardinal; //Counter for hit point restoration, separate cos it resets on first hit
    fCondition: Integer; //Unit condition, when it reaches zero unit should die (rarely can be negative due to WalkExchange)
    fStartWDefaultCondition: Boolean; //For MapEditor only. Shows if this unit conditions will be set to default at the start of the game
    fTicker: Cardinal; //ticks of life for the unit (allows to spread updates)
    fHome: TKMHouse;
    fPositionF: TKMPointF;
    fVisible: Boolean;
    fIsDead: Boolean;
    fKillASAP: Boolean;
    fDismissASAP: Boolean;
    fKillASAPShowAnimation: Boolean;
    fInHouse: TKMHouse; //House we are currently in
    fPositionRound: TKMPoint; //Where we are now
    fPositionPrev: TKMPoint; //Where we were
    fPositionNext: TKMPoint; //Where we will be. Next tile in route or same tile if stay on place
    fDirection: TKMDirection; //Direction
    fKilledBy: TKMHandID; //Who killed us?
    fLastTimeTrySetActionWalk: Cardinal; //LastTime we tried to set action walk
    //No saved fields, used only in players UI
    fDismissInProgress: Boolean; //Mark unit as waiting for Dismiss GIC cmd, to show proper UI

    fAttack, fAttackHorse, fDefence, fSight : SmallInt;
    fProjectileDefence : Single;
    fHitPointsMax : Byte;
    fSpeed : Single;
    fStepsPerTile, fStepsPerTileDiag, fStepsPerTileStorm, fStepsPerTileStormDiag : SmallInt;
    fConditionPace : Cardinal;
    fVisual: TKMUnitVisual;
    fNeverHungry : Boolean;

    //pecialEffect : array of Reco
    function GetDesiredPassability: TKMTerrainPassability; virtual;
    function GetHitPointsMax: Byte; virtual;
    procedure SetDirection(aValue: TKMDirection);
    procedure SetAction(aAction: TKMUnitAction; aStep: Integer = 0);
    procedure SetPositionNext(const aLoc: TKMPoint);
    procedure SetPositionRound(const aLoc: TKMPoint);
    procedure SetPositionRoundByPosF;
    procedure SetCondition(aValue: Integer);
    function CanAccessHome: Boolean;
    procedure SetHome(aHome: TKMHouse);

    procedure SetThought(aThought: TKMUnitThought);
    procedure SetInHouse(aInHouse: TKMHouse);
    procedure UpdateThoughts;
    function UpdateVisibility: Boolean;
    procedure UpdateHitPoints(UseEffect : Boolean = true); virtual;
    procedure DoKill(aShowAnimation: Boolean);
    procedure DoDismiss;virtual;
    procedure SetDefence(aValue : SmallInt);
    function GetDefence : SmallInt;virtual;
    function GetSight : SmallInt;virtual;
    procedure SetAttack(aValue : SmallInt);
    function GetAttack : SmallInt; virtual;
    procedure UpdateLastTimeTrySetActionWalk;
    function GetUnitSpec : TKMUnitSpec;
    function IsSelected : Boolean; virtual;
  private
    function GetTask: TKMUnitTask;
    function GetInHouse: TKMHouse;
  protected
    fSpecialEffect : TKMUnitEffect;
    function GetInstance: TKMUnit; override;
    function GetPositionForDisplayF: TKMPointF; override;
    procedure SetPositionF(const aPositionF: TKMPointF);
    function GetIsSelectable: Boolean; override;
    procedure PaintUnit(aTickLag: Single); virtual;
  public
    FlagColor : Cardinal;
    AnimStep: Integer;
    IsExchanging, InstantKill, IsHero: Boolean; //Current walk is an exchange, used for sliding
    OnUnitDied: TKMUnitEvent;
    OnUnitTrained: TKMUnitEvent;

    HitPointsInvulnerable: Boolean;
    Dismissable: Boolean; //Is it allowed to dismiss this unit ?
    BootsAdded : Boolean;
    InShip : Pointer;
    BlockWalking,
    Immortal : Boolean;
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    destructor Destroy; override;

    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; virtual;
    procedure Save(SaveStream: TKMemoryStream); override;

    //Creates TTaskDie which then will Close the unit from further access
    procedure Kill(aFrom: TKMHandID; aShowAnimation, aForceDelay: Boolean); virtual;

    //Creates TTaskDismiss
    procedure Dismiss; virtual;
    procedure DismissCancel; virtual;
    //Could be used only in UI
    procedure DismissStarted;
    property DismissInProgress: Boolean read fDismissInProgress write fDismissInProgress;
    procedure UpdateSpeed;

    procedure CloseUnit(aRemoveTileUsage: Boolean = True); virtual;
    property NeverHungry : Boolean read fNeverHungry write fNeverHungry;
    property ConditionPace : Cardinal read fConditionPace write fConditionPace;
    property PositionF: TKMPointF read fPositionF write SetPositionF;
    property Position: TKMPoint read fPositionRound;
    property PositionPrev: TKMPoint read fPositionPrev;
    property PositionNext: TKMPoint read fPositionNext write SetPositionNext;
    procedure SetUnitPosition(const aPos: TKMPoint);
    function SetUnitPositionFromShip(const aPos: TKMPoint): Boolean;

    property Direction: TKMDirection read fDirection write SetDirection;
    property CurrentHitPoints: Byte read fHitPoints;

    function HitTest(X,Y: Integer; const UT: TKMUnitType = utAny): Boolean;

    procedure SetActionAbandonWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionGoIn(aAction: TKMUnitActionType; aGoDir: TKMGoInDirection; aHouse: TKMHouse; aIsTrained : Boolean = false); virtual;
    procedure SetActionStay(aTimeToStay: Integer; aAction: TKMUnitActionType; aStayStill: Boolean = True; aStillFrame: Byte = 0; aStep: Integer = 0);
    procedure SetActionStorm(aRow: Integer);
    procedure SetActionSteer;
    procedure SetActionLockedStay(aTimeToStay: Integer; aAction: TKMUnitActionType; aStayStill: Boolean=True; aStillFrame: Byte = 0; aStep: Integer = 0);

    procedure SetActionWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType; aDistance: Single; aTargetUnit: TKMUnit;
                            aTargetHouse: TKMHouse; aAvoidLockedByMovementCost: Boolean = True);
    function TrySetActionWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType; aDistance: Single; aTargetUnit: TKMUnit;
                              aTargetHouse: TKMHouse; aAvoidLockedByMovementCost: Boolean = True;
                              aSilent: Boolean = True): Boolean;
    procedure SetActionWalkToHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionWalkFromHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionWalkToUnit(aUnit: TKMUnit; aDistance:single; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionWalkFromUnit(aUnit: TKMUnit; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionWalkToSpot(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk; aDistance: Single = 0; aAnimStep: Integer = 0);
    procedure SetActionWalkFromSpot(const aLoc: TKMPoint; aDistanceMin, aDistanceMaX: Single; aActionType: TKMUnitActionType = uaWalk);
    procedure SetActionWalkToRoad(aActionType: TKMUnitActionType = uaWalk; aDistance: Single = 0;
                                         aTargetPassability: TKMTerrainPassability = tpWalkRoad; aTargetWalkConnectSet: TKMByteSet = []);
    procedure SetActionWalkPushed(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk);

    procedure Feed(Amount: Single);
    function IsHungry: Boolean;
    procedure AbandonWalk;
    property  DesiredPassability: TKMTerrainPassability read GetDesiredPassability;
    property KilledBy: TKMHandID read fKilledBy;

    property  Home: TKMHouse read fHome write SetHome;
    property  Action: TKMUnitAction read fAction;
    property  Task: TKMUnitTask read GetTask;
    property  UnitType: TKMUnitType read fType;
    function  GetActionText: UnicodeString;
    property  Condition: Integer read fCondition write SetCondition;
    property  StartWDefaultCondition: Boolean read fStartWDefaultCondition write fStartWDefaultCondition;
    property  Visual: TKMUnitVisual read fVisual;
    property Ticker : Cardinal read fTicker;



    function GiveBoots(aFromScript : Boolean = false) : Boolean;
    procedure OwnerUpdate(aOwner: TKMHandID; aMoveToNewOwner: Boolean = False);
    procedure DoHitFrom(aAttacker : TKMUnit);
    procedure HitPointsChangeFromScript(aAmount: Integer);
    procedure HitPointsDecrease(aAmount: Byte; aAttacker: TKMUnit); overload;
    procedure HitPointsDecrease(aAttack, aDamage : Integer; aAttacker : TKMUnit;aInstantKill : Boolean = false); overload;
    property  HitPointsMax: Byte read GetHitPointsMax write fHitPointsMax;
    procedure SetHitTime; virtual;
    property  Attack : SmallInt read GetAttack write SetAttack;
    property  AttackHorse : SmallInt read  fAttackHorse write fAttackHorse;
    property  Defence : SmallInt read  GetDefence write SetDefence;
    property  ProjectilesDefence : Single read  fProjectileDefence write fProjectileDefence;
    property  Speed : Single read fSpeed;
    property  Sight : SmallInt read GetSight write fSight;
    function GetProjectileDefence(isBolt : Boolean) : Single; virtual;
    procedure SetSpeed(aValue : SmallInt; addTo : Boolean = false);
    procedure AssignToShip(aShip : Pointer); virtual;
    procedure UnloadFromShip(aShip : Pointer); virtual;
    function IsAssigningToShip : Boolean;
    function IsUnloadingFromShip : Boolean;
    procedure GoToStore(aLocTo : TKMPoint);
    procedure GoToPearl(aLocTo : TKMPoint);
    procedure GoMakePearlRally(aPearl : TKMHouse);
    procedure SetEffect(aType : TKMUnitEffectType; aDuration : Word);

    procedure CancelTask(aFreeTaskObject: Boolean = True);
    property  Visible: Boolean read fVisible write fVisible;
    property  InHouse: TKMHouse read GetInHouse write SetInHouse;
    property  IsDead: Boolean read fIsDead;
    function  IsDeadOrDying: Boolean;
    function  IsDismissing: Boolean;
    function  IsDismissAvailable: Boolean;
    function  IsDismissCancelAvailable: Boolean;

    function IsAnimal: Boolean; virtual;

    property  Thought: TKMUnitThought read fThought write SetThought;
    function  GetMovementVector: TKMPointF;
    function  IsIdle: Boolean;
    procedure TrainInHouse(aSchool: TKMHouseSchool);

    function CanStepTo(X,Y: Integer; aPass: TKMTerrainPassability): Boolean; virtual;
    function CanWalkTo(const aTo: TKMPoint; aDistance: Single): Boolean; overload;
    function CanWalkTo(const aTo: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean; overload;
    function CanWalkTo(const aFrom, aTo: TKMPoint; aDistance: Single): Boolean; overload;
    function CanWalkTo(const aFrom, aTo: TKMPoint; aPass: TKMTerrainPassability): Boolean; overload;
    function CanWalkTo(const aFrom, aTo: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean; overload;
    function CanWalkTo(const aFrom: TKMPoint; aHouse: TKMHouse; aPass: TKMTerrainPassability; aDistance: Single): Boolean; overload;
    function CanWalkDiagonally(const aFrom, aTo: TKMPoint): Boolean;
    procedure VertexRem(const aLoc: TKMPoint);
    function  VertexUsageCompatible(const aFrom, aTo: TKMPoint): Boolean;
    procedure VertexAdd(const aFrom, aTo: TKMPoint);
    procedure Walk(const aFrom, aTo: TKMPoint);
    function GetActivityText: UnicodeString; virtual;
    function GetSlide(aCheck: TKMCheckAxis): Single;
    function GetSlides: TKMPointF;
    function PathfindingShouldAvoid: Boolean; virtual;

    procedure Show;
    procedure Hide;
    function ConvertSpeed(aSpeed : Integer; aIsDiag: Boolean) : Single;
    function GetEffectiveSpeed(aMovementType: TKMUnitMoveType): Single;
    function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single; virtual;
    function GetEffectiveStormSpeed(aIsDiag: Boolean): Single;

    function ObjToString(const aSeparator: String = '|'): String; override;
    function ObjToStringShort(const aSeparator: String = '|'): String; override;

    property Spec : TKMUnitSpec read GetUnitSpec;

    function UpdateState: Boolean; virtual;
    function UpdateTask : Boolean; virtual;
    procedure UpdateVisualState;
    procedure Paint(aTickLag: Single); virtual;

    class function GetDefaultCondition: Integer;


    ///scripting
    function GetStats : TKMUnitStats; virtual;
    procedure SetStats( aStats : TKMUnitStats); virtual;


  end;

  //Common class for all civil units
  TKMCivilUnit = class abstract(TKMUnit)
  public
    fIdleTimer : Cardinal;
    function GoEat(aInn: TKMHouseInn; aUnitAlreadyInsideInn: Boolean = False): Boolean;
    function GoGetBoots: Boolean;
    function CheckCondition: Boolean;
    procedure KillInHouse;

    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  //This is common class for all units, who can be an owner for house (recruits and all citizen workers)
  TKMSettledUnit = class abstract(TKMCivilUnit)
  private
    procedure CleanHousePointer(aFreeAndNilTask: Boolean = False);
  protected
    procedure TaskGetWork; virtual; abstract;
    function FindHome: Boolean;
    procedure ProceedHouseClosedForWorker;
    procedure PaintUnit(aTickLag: Single); override;
  public
    function UpdateState: Boolean; override;
  end;

  //This is a common class for all units, who can work in house
  TKMUnitCitizen = class(TKMSettledUnit)
  protected
    procedure TaskGetWater;
    procedure TaskGetWork; override;
  public
    function CanWorkAt(aLoc: TKMPoint; aGatheringScript: TKMGatheringScript): Boolean;
    function GetActivityText: UnicodeString; override;
  end;


  TKMUnitRecruit = class(TKMSettledUnit)
  private
    procedure TaskSendWares;
    procedure TaskCartographer;
  protected
    procedure TaskGetWork; override;
  end;

  TKMUnitWoodcutter = class(TKMUnitCitizen)
  private
    procedure TaskCutForest;
  protected
    procedure TaskGetWork; override;
  end;

  //Serf - transports all wares between houses
  TKMUnitSerf = class(TKMCivilUnit)
  private
    fCarry: TKMWareType;
    function GetCarry: TKMWareType;
  protected
    procedure PaintUnit(aTickLag: Single); override;
    function GetDesiredPassability: TKMTerrainPassability; override;
  public
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    procedure Deliver(aFrom: TKMHouse; toHouse: TKMHouse; aWare: TKMWareType; aID: integer); overload;
    procedure Deliver(aFrom: TKMHouse; toUnit: TKMUnit; aWare: TKMWareType; aID: integer); overload;
    procedure Deliver(aFrom: TKMHouse; aTo: TKMPoint; aWare: TKMWareType; aID: integer); overload;
    procedure Deliver(aFrom: TKMHouse; aToStruc: TKMStructure; aWare: TKMWareType; aID: integer); overload;
    function TryDeliverFrom(aFrom: TKMHouse): Boolean;
    procedure DelegateDelivery(aToSerf: TKMUnit);

    property Carry: TKMWareType read GetCarry;
    procedure CarryGive(aWare: TKMWareType);
    procedure CarryTake;
    function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single; override;

    function ObjToString(const aSeparator: String = '|'): String; override;

    function UpdateState: Boolean; override;
  end;

  TKMUnitMountedSerf = class(TKMUnitSerf)
    protected
      procedure PaintUnit(aTickLag: Single); override;
      function GetDesiredPassability: TKMTerrainPassability; override;
    public
      function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single; override;
  end;
  //Worker class - builds everything in game
  TKMUnitWorker = class(TKMCivilUnit)
  protected
    procedure PaintUnit(aTickLag: Single); override;
  public
    procedure BuildStructure(aStructure : TKMStructure; aIndex: Integer);
    procedure BuildPearl(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildHouseUpgrade(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildHouse(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildHouseRepair(aHouse: TKMHouse; aIndex: Integer);
    procedure BuildField(aField: TKMFieldType; aLoc: TKMPoint; aIndex: Integer; aRoadType : TKMRoadType);
    procedure BuildHouseArea(aHouseType: TKMHouseType; aLoc: TKMPoint; aIndex: Integer);
    function PickRandomSpot(aList: TKMPointDirList; out Loc: TKMPointDir): Boolean;
    function PickNextSpot(aList: TKMPointDirList; out Loc: TKMPointDir; var aLastCellID : Byte): Boolean;

    function OnlyBuildsHouse : Boolean;
    function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single; override;

    function UpdateState: Boolean; override;
  end;

  TKMUnitFeederGroup = record
    Group : Pointer;
    FoodRequested : Word;
  end;

  TKMUnitFeeder = class(TKMCivilUnit)
  private
    fGroupsToFeed : array of TKMUnitFeederGroup;
    fFoodCount : Word;
    procedure CheckGroups;
    function HasGroup(aGroup : Pointer) : Boolean;
    function DeleteGroup(aGroup : Pointer) : Boolean;

    function FindBestStore : TKMHouse;
    procedure TryToFeedGroups;
  protected
    procedure PaintUnit(aTickLag: Single); override;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; Override;

    function NeededFood : Word;
    function TaskStarted : Boolean;
    function FirstGroup : Pointer;

    procedure CollectFood(aCount : Word);
    procedure FeedGroup(aGroup : Pointer);

    procedure AddGroupToFeed(aGroup: Pointer);
    procedure CloseUnit(aRemoveTileUsage: Boolean = True); override;
    function UpdateState: Boolean; override;
  end;


  // Animals
  TKMUnitAnimal = class(TKMUnit)
  private
    fSpawnerID : Integer;
    //walking boundaries
    fStartLoc : TKMPoint;
    fRadius : Word;
    function CheckForTrap : Boolean;
  protected
    function GetIsSelectable: Boolean; override;
    function GetPaintActionType(aAct: TKMUnitActionType): TKMUnitActionType; virtual;
  protected
    procedure PaintUnit(aTickLag: Single); override;
  public
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID); overload;

    function IsAnimal: Boolean; override;

    property SpawnerID : Integer read fSpawnerID write fSpawnerID;
    function CanStepTo(X,Y: Integer; aPass: TKMTerrainPassability): Boolean; override;

    constructor Load(LoadStream: TKMemoryStream); override;
    function UpdateState: Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


  TKMUnitFish = class(TKMUnitAnimal)
  private
    fFishCount: Word; //1-255
  protected
    function GetPaintActionType(aAct: TKMUnitActionType): TKMUnitActionType; override;
    //procedure PaintUnit(aTickLag: Single); override;
  public
    constructor Create(aID: Cardinal; const aLoc: TKMPointDir; aOwner: TKMHandID); overload;
    constructor Load(LoadStream: TKMemoryStream); override;

    property FishCount: Word read fFishCount write fFishCount;
    function ReduceFish(aCount : Integer = 1) : Word;

    procedure Save(SaveStream: TKMemoryStream); override;

    class function GetFishActionType(aFishCount: Integer): TKMUnitActionType;
  end;


  TKMUnitsArray = record
    private
      fCount : Integer;
      fList : TPointerArray;
      function GetItem(aIndex : Integer) : TKMUnit;
    public
      function Add(aItem : TKMUnit) : Integer;
      function Remove(aIndex : Integer) : Boolean; Overload;
      function Remove(aPointer : TKMUnit) : Boolean; Overload;
      procedure Clear;

      property Units[aIndex : Integer] : TKMUnit read GetItem; default;
      property Count : Integer read fCount;
      function Contains(aUnit : TKMUnit) : Boolean;

      procedure SaveToStream(aSaveStream : TKMemoryStream);
      procedure LoadFromStream(aLoadStream : TKMemoryStream);
      procedure SyncLoad;
  end;

const
  TRY_SET_ACTION_WALK_FREQ = 40; //in ticks. How often do we allow to try set action walk


implementation
uses
  TypInfo,
  KM_Entity,
  KM_Game, KM_GameParams, KM_GameApp,
  KM_RenderPool, KM_RenderAux, KM_ResTexts,
  KM_HandsCollection, KM_UnitWarrior, KM_Resource,
  KM_Hand, KM_MapEdTypes, KM_HousePearl, KM_HouseForest,
  KM_CommonHelpers,
  KM_UnitActionAbandonWalk,
  KM_UnitActionFight,
  KM_UnitActionGoInOut,
  KM_UnitActionStay,
  KM_UnitActionSteer,
  KM_UnitActionStormAttack,
  KM_UnitActionWalkTo,

  KM_UnitTaskAttackHouse,
  KM_UnitTaskBuild,
  KM_UnitTaskDelivery,
  KM_UnitTaskDie,
  KM_UnitTaskGoEat,
  KM_UnitTaskGoHome,
  KM_UnitTaskGoForBoots,
  KM_UnitTaskDismiss,
  KM_UnitTaskGoOutShowHungry,
  KM_UnitTaskMining,
  KM_UnitTaskSelfTrain,
  KM_UnitTaskThrowRock,
  KM_UnitTaskCollectWares,
  KM_UnitTaskGoToWell,
  KM_UnitTaskMerchant,
  KM_UnitTaskCartographer,
  KM_UnitTaskWoodCutter,
  KM_UnitTaskFeeder,

  KM_UnitGroup,
  KM_HouseStore,

  KM_SpecialAnim,
  KM_GameTypes,
  KM_HandTypes,
  KM_CommonExceptions,
  KM_Terrain, KM_TerrainTypes,
  KM_Particles,
  KM_Sound, KM_ResSound;


//Pixel positions (waypoints) for sliding around other units. Uses a lookup to save on-the-fly calculations.
//Follows a sort of a bell curve (normal distribution) shape for realistic acceleration/deceleration.
//I tweaked it by hand to look similar to KaM.
//1st row for straight, 2nd for diagonal sliding
const
  SLIDE_LOOKUP: array[1..2, 0..Round(CELL_SIZE_PX * 1.42)] of Byte = ( //1.42 instead of 1.41 because we want to round up just in case (it was causing a crash because Round(40*sqrt(2)) = 57 but Round(40*1.41) = 56)
    (0,0,0,0,0,0,1,1,2,2,3,3,4,5,6,7,7,8,8,9,9,9,9,8,8,7,7,6,5,4,3,3,2,2,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,6,6,7,7,7,7,6,6,6,5,5,5,4,4,4,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0,0,0,0));


{ TKMCivilUnit }
function TKMCivilUnit.CheckCondition: Boolean;
var
  H: TKMHouseInn;
begin
  Result := False;
  if IsHungry then
  begin
    H := gHands[Owner].FindInn(fPositionRound, Self, InHouse <> nil);
    Result := GoEat(H);
  end;
end;


procedure TKMCivilUnit.KillInHouse;
begin
  Assert(fInHouse <> nil, 'Unit could be silently killed only inside some House');
  //Dispose of current action/task BEFORE we close the unit (action might need to check fPosition if recruit was about to walk out to eat)
  //Normally this isn't required because TTaskDie takes care of it all, but recruits in barracks don't use TaskDie.
  SetAction(nil);
  FreeAndNil(fTask);

  CloseUnit(False); //Don't remove tile usage, we are inside the barracks
end;


function TKMCivilUnit.GoEat(aInn: TKMHouseInn; aUnitAlreadyInsideInn: Boolean = False): Boolean;
begin
  Result := False;
  if InShip <> nil then
    Exit;

  if aInn <> nil then
  begin
    FreeAndNil(fTask);
    fTask := TKMTaskGoEat.Create(aInn, Self);
    if aUnitAlreadyInsideInn then
      fTask.Phase := 3; //We are inside Inn already
    Result := True;
  end;
end;

function TKMCivilUnit.GoGetBoots: Boolean;
var aHouse: TKMHouse;
begin
  Result := False;
  if BootsAdded then Exit;
  if not (fType in UNITS_WITH_BOOTS) then Exit;

  aHouse := gHands[Owner].GetClosestHouse(self.Position, [htStore, htTailorsShop], [wtBoots]);
  if aHouse = nil then
    Exit;

  fTask := TKMTaskGoGetBoots.Create(Self, aHouse);
  Result := True;
end;

procedure TKMCivilUnit.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fIdleTimer);
end;

constructor TKMCivilUnit.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fIdleTimer);
end;
{ TKMSettledUnit }
//Find home for settled unit
function TKMSettledUnit.FindHome: Boolean;
var
  H: TKMHouse;
begin
  Result := False;
  H := gHands[Owner].Houses.FindEmptyHouse(self, fPositionRound);
  if H <> nil then
  begin
    fHome  := H.GetPointer;
    if not (fHome.HouseType in [htBarracks{, htWallTower}]) then // Become house worker except Barracks
      fHome.AssignWorker(self);
      //fHome.SetWorker(Self) //Self Pointer is managed via House
    {else
      if fHome.HouseType = htWallTower then
      begin
        if TKMHouseWallTower(fHome).Recruit1 = 0 then
          TKMHouseWallTower(fHome).Recruit1 := self.UID
        else
        if TKMHouseWallTower(fHome).Recruit2 = 0 then
        begin
          TKMHouseWallTower(fHome).Recruit2 := self.UID;
          fHome.AssignWorker(self)
          //fHome.SetWorker(Self) //Self Pointer is managed via House
        end;
        //fHome.SetState(hstIdle);
      end;}



    Result := True;
  end;
end;


procedure TKMSettledUnit.CleanHousePointer(aFreeAndNilTask: Boolean = False);
begin
  if aFreeAndNilTask then
    FreeAndNil(fTask);

  //fHome.SetWorker(nil);
  fHome.RemoveWorker(self);
  gHands.CleanUpHousePointer(fHome);
end;


procedure TKMSettledUnit.PaintUnit(aTickLag: Single);
var
  ID: Integer;
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  xPaintPos, yPaintPos: Single;
begin

  V := fVisual.GetLerp(aTickLag);
  act := V.Action;

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  if fType = utClayPicker then
    if (fHome <> nil) and (fHome.HouseType = htCollectors) then
      if (fAction.fType in [uaWork, uaWork1])
      and (fTask is TKMTaskMining)
      and (TKMTaskMining(fTask).WorkPlan.GatheringScript = gsCollector) then
        case Direction of
          dirN : yPaintPos := yPaintPos - 25/CELL_SIZE_PX;
          dirS: yPaintPos := yPaintPos + 15/CELL_SIZE_PX;
          dirE: xPaintPos := xPaintPos + 20/CELL_SIZE_PX;
          dirW: xPaintPos :=xPaintPos - 20/CELL_SIZE_PX;
        end;




  ID := UID * Byte(not (act in [uaDie, uaEat]));

  case fAction.fType of
    uaStay:
      begin
        gRenderPool.AddUnit(fType, ID, uaStay, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);
      end;
    uaWalk:
      begin

        gRenderPool.AddUnit(fType, ID, uaWalk, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);
        if gRes.Units[fType].SupportsAction(uaWalkArm) then
          gRenderPool.AddUnit(fType, ID, uaWalkArm, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), False);
      end;
    uaWork..uaEat:
        gRenderPool.AddUnit(fType, ID, act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);
    uaWalkArm .. uaWalkBooty2:
      begin
        gRenderPool.AddUnit(fType, ID, uaWalk, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);
        gRenderPool.AddUnit(fType, ID, act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), False);
      end;
  end;

  if fThought <> thNone then
    gRenderPool.AddUnitThought(fType, act, Direction, V.AnimStep, fThought, xPaintPos, yPaintPos);
end;



// Manage house is closed for worker process
procedure TKMSettledUnit.ProceedHouseClosedForWorker;
var
  wGoingInsideHouse: Boolean;
  wThrowingRock: Boolean;
  wGoingForEating: Boolean;
  wWentOutShowHungry: Boolean;
  wWantToGoOutShowHungry: Boolean;
  wWalkingOutside: Boolean;
  wWorkingOutsideHouse: Boolean;
  wHasNoTask: Boolean;
  wIsInsideHouse: Boolean;
begin
  if (fHome <> nil)
    and not fHome.IsDestroyed
    and (fHome.IsClosedForWorker or not fHome.AcceptsWorker(self) or ((fHome.HouseType = htBarracks) and (TKMHouseBarracks(fHome).NotAcceptRecruitFlag)))
    and not (fTask is TKMTaskDie)
    and not (fTask is TKMTaskGoToWell)
    and not (fTask is TKMTaskDismiss) then
    begin
      if ((fTask is TKMTaskMerchant) and not TKMTaskMerchant(fTask).CouldBeCancelled)
      or ((fTask is TKMTaskGoToWell) and not TKMTaskGoToWell(fTask).CouldBeCancelled) then
        Exit;
      wGoingInsideHouse := (fAction is TKMUnitActionGoInOut) and ((TKMUnitActionGoInOut(fAction)).Direction = gdGoInside);
      // let recruits finish throwing animation
      wThrowingRock := (fTask is TKMTaskThrowRock) and (fHome.GetState in [hstWork]);
      // do not cancel eating task
      wGoingForEating := (fTask is TKMTaskGoEat);
      // Assume worker is inside the house if not Visible.
      wIsInsideHouse := not Visible;
      // cancel GoOutShowHungry task if we outside of the house
      wWentOutShowHungry := (fTask is TKMTaskGoOutShowHungry) and not wIsInsideHouse;
      // cancel GoOutShowHungry task and go out of the house, if we inside of it
      wWantToGoOutShowHungry := (fTask is TKMTaskGoOutShowHungry) and wIsInsideHouse;
      // We are on the way to somewhere. AbandonWalk 'n cancel task.
      wWalkingOutside := (fAction is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(fAction).DoingExchange;
      // Working outside
      wWorkingOutsideHouse := (fTask is TKMTaskMining) and not wIsInsideHouse;
      // Somehow no task
      wHasNoTask := (fTask = nil);
      if (not wThrowingRock) then       // Let recruit finish rock throwing
        if (wGoingForEating) then
        begin
          CleanHousePointer;            // Clean house pointer, do not cancel eating task
        end else begin
          if (wWalkingOutside) then begin
            AbandonWalk;                // Stop walking
            CleanHousePointer(True);    // Clean house pointer and free task
          end else
          if not wGoingInsideHouse and  // Let worker get into the house
            ((wWentOutShowHungry or wWorkingOutsideHouse) // When already outside the house
            // Not sure we need this
            or (wHasNoTask and not wIsInsideHouse)) then  // Or has no task outside the house.
          begin
            CleanHousePointer(True);    // Clean house pointer and free task
          end else
          if (wIsInsideHouse or wWantToGoOutShowHungry)
            and not (fHome.HouseType = htBarracks) then // Recruits should not go out of Barracks
          begin
            SetActionGoIn(uaWalk, gdGoOutside, fHome); //Walk outside the house
            // If working inside - first we need to set house state to Idle, then to Empty
            fHome.SetState(hstIdle);
            fHome.SetState(hstEmpty);
            CleanHousePointer(True)     // Clean house pointer and free task
          end;
        end;
    end;
end;


function TKMSettledUnit.UpdateState: Boolean;
var
  oldThought: TKMUnitThought;
begin
  Result := True;
  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at start of TKMSettledUnit.UpdateState', fPositionRound);

  //Reset unit activity if home was destroyed, except when unit is dying or eating (finish eating/dying first)
  if (fHome <> nil)
    and fHome.IsDestroyed
    and not(fTask is TKMTaskDie)
    and not(fTask is TKMTaskGoEat) then
  begin
    if (fAction is TKMUnitActionWalkTo)
      and not TKMUnitActionWalkTo(Action).DoingExchange then
      AbandonWalk;
    FreeAndNil(fTask);
    gHands.CleanUpHousePointer(fHome);
  end;

  ProceedHouseClosedForWorker;

  if inherited UpdateState then Exit;
  if IsDead then Exit; //Caused by SelfTrain.Abandoned

  oldThought := fThought;
  Thought := thNone;

  if IsHungry
    and not CheckCondition
    and (fHome <> nil)
    and not fVisible then
  begin
    if fTask <> nil then
      FreeAndNil(fTask);
    fTask := TKMTaskGoOutShowHungry.Create(Self);
  end;

  if fTask = nil then //If Unit still got nothing to do, nevermind hunger
    if fHome = nil then
      if FindHome then
      begin
        fTask := TKMTaskGoHome.Create(Self); //Home found - go there
        fIdleTimer := 0;
      end
      else
      begin
        if IsHungry then
          Thought := oldThought
        else
          Thought := thQuest; //Always show quest when idle, unlike serfs who randomly show it

        Inc(fIdleTimer);
        if fIdleTimer > 10 then
          GoGetBoots;
        SetActionStay(20, uaWalk) //There's no home, but there will hopefully be one soon so don't sleep too long
      end
    else
    if fVisible then //Unit is not at home, but it has one
    begin
      fIdleTimer := 0;
      if CanAccessHome then
        fTask := TKMTaskGoHome.Create(Self)
      else
        SetActionStay(60, uaWalk) //Home can't be reached
    end else
    begin
      fIdleTimer := 0;
      if not gRes.Houses[fHome.HouseType].IsWorkshop
      or (fHome.CheckWareOut(wtAll) < MAX_WARES_OUT_WORKSHOP) then //Do not do anything if we have too many ready resources
      begin
        TaskGetWork; //Unit is at home, so go get a job
      end;
      //maybe his home needs water
      if fTask = nil then
        If self is TKMUnitCitizen then
         TKMUnitCitizen(self).TaskGetWater;


      if fTask = nil then //We didn't find any job to do - rest at home
        SetActionStay(Max(gRes.Houses[fHome.HouseType].WorkerRest,1)*10, uaWalk); //By default it's 0, don't scan that often
    end;

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at end of TKMSettledUnit.UpdateState', fPositionRound);

  Result := False;
end;


{ TKMUnitCitizen }
function TKMUnitCitizen.CanWorkAt(aLoc: TKMPoint; aGatheringScript: TKMGatheringScript): Boolean;
var
  I: Integer;
begin
  case aGatheringScript of
    gsWoodCutterPlant: begin
                          //Woodcutters should not plant trees on our own or our ally's house plans
                          //(it's very annoying if they do)
                          Result := True;
                          for I := 0 to gHands.Count - 1 do
                            if gHands[Owner].Alliances[I] = atAlly then
                              Result := Result and not gHands[I].Constructions.HousePlanList.HasPlan(aLoc);
                        end;
    else Result := True;
  end;
end;


function TKMUnitCitizen.GetActivityText: UnicodeString;
begin
  Result := Inherited GetActivityText; //Default
  if fTask is TKMTaskMining then
    Result := TKMTaskMining(fTask).GetActivityText;
end;


procedure TKMUnitCitizen.TaskGetWater;
var well : TKMHouse;
begin
  fTask := nil;
  if not Home.WareCanAddToIn(wtWater) then //this home needs no water
    Exit;

  if Home.CheckWareIn(wtWater) >= gHands[Owner].Stats.WareDistribution[wtWater,Home.HouseType] then //we have enough water
    Exit;

  if not KMSamePoint(fPositionRound, fHome.Entrance) then
    raise ELocError.Create('Working from wrong spot', fPositionRound);

  well := gHands.GetClosestHouse(fHome.Entrance, [htWell], [], IfThen(gHands[Owner].IsComputer, 12, 6));

  //no well found
  if well = nil then
    Exit;

  if well.IsValid then
    fTask := TKMTaskGoToWell.Create(self, well);
end;


procedure TKMUnitCitizen.TaskGetWork;
var
  res: Integer;
  tm: TKMTaskMining;
begin
  fTask := nil;

  if not KMSamePoint(fPositionRound, fHome.Entrance) then
    raise ELocError.Create('Mining from wrong spot', fPositionRound);

  res := 1;
  // Check if House has production orders
  // Ask the house what order we should make
  if gRes.Houses[fHome.HouseType].DoesOrders then
  begin
    res := fHome.PickOrder(UnitType);
    if res = 0 then Exit;
  end;

  // Don't bother creating a task if there's no room for resulting ware
  // Saves us time on Fishers/Stonecutters/Woodcutters when they calculate routes to nearby deposits
  // Other houses where workers walk out can choose between cut/plant
  if (fHome.HouseType in [htFishermans, htQuarry, htVineyard{, htPottery}]) then
  begin
    //If (fHome.CheckWareOut(fHome.WareOutput[res]) >= fHome.GetMaxOutWare) and not fHome.ForceWorking then
    If not fHome.CanTaskProduceWare(fHome.WareOutput[res]) then
      Exit;
  end;

  fTask := TKMTaskMining.Create(Self, fHome.WareOutput[res]);
  tm := TKMTaskMining(fTask);

  if tm.WorkPlan.ResourceDepleted then
  begin
    if not fHome.ResourceDepleted then
      fHome.IssueResourceDepletedMsg;
  end
  else
    //Reset resource depleted state for home, if we find any resource
    //(it could appear again, f.e. by script)
    fHome.ResourceDepleted := False;

  // Verify the task can be done
  if not ( //todo: Invert negation here
  tm.WorkPlan.IsIssued
  and tm.WorkPlan.CanWork(Self)
  )
   then
    // If task can't be done - discard it
    FreeAndNil(fTask);

end;


{ TKMUnitRecruit }
procedure TKMUnitRecruit.TaskSendWares;
var H : TKMHouse;
    HM : TKMHouseMerchant;
    HS : TKMHouseShipYard;
    W : TKMWarePlan;
    I : Integer;
begin
  if fTask <> nil then
    Exit;
  HM := TKMHouseMerchant(Home);
  if not HM.CanWork then
    Exit;

  H := HM.AllyStore;

  if not gTerrain.RouteCanBeMade(HM.PointBelowEntrance, H.PointBelowEntrance, tpWalk) then //can't walk to current store
    H := nil;

  HS := nil;
  if H = nil then
    if gHands[Owner].Stats.GetHouseQty(htShipYard) > 0 then
    begin
      HS := TKMHouseShipYard(gHands[Owner].GetClosestHouse(Home.Entrance, [htShipYard]));
      if not gTerrain.RouteCanBeMade(HM.PointBelowEntrance, HS.PointBelowEntrance, tpWalk) then //can't walk to this shipyard
        HS := nil;
    end;

  if (H = nil) and (HS = nil) then
    Exit;

  W.Reset;
  for I := 1 to WARES_IN_OUT_COUNT do
    W.AddWare(HM.WareInput[I], HM.CheckWareIn(HM.WareInput[I]));

  fTask := TKMTaskMerchant.Create(self, H, HS, HM.CurrentHand, W);
end;

procedure TKMUnitRecruit.TaskCartographer;
var H : TKMHouseCartographers;
begin
  if not (InHouse = Home) then
    Exit;
  fTask := nil;
  H := TKMHouseCartographers(Home);
  If not H.IsValid then
    Exit;
  If H.Mode = cmChartman then
  begin
    if not gTerrain.RouteCanBeMade(H.PointBelowEntrance, H.FlagPoint, tpWalk) then //can't reach the flag point
      Exit;

    If H.CanWork then
      fTask := TKMTaskCartographer.Create(self, H.FlagPoint, H.Mode);
  end else
  If H.Mode = cmSpy then
  begin
    If H.CanWork then
      fTask := TKMTaskCartographer.Create(self, H.PlayerToSpy, H.Mode);
  end;
end;

procedure TKMUnitRecruit.TaskGetWork;
var
  enemy: TKMUnit;
begin
  if not (InHouse = Home) then
    Exit;
  fTask := nil;
  //because he is also in the merchant house, he has another task
  if (fHome is TKMHouseMerchant) then
    TaskSendWares
  else
  if fHome.HouseType = htCartographers then
    TaskCartographer;

  if not (fHome is TKMHouseTower)  then
    Exit;

  if fHome.HouseType = htWatchTower then
    if (fHome.CheckWareIn(fHome.WareInput[1]) <= 0) then
      Exit;

  if fHome.HouseType = htWallTower then
    If not TKMHouseWallTower(fHome).CanMakeShot then
      Exit;

  enemy := gTerrain.UnitsHitTestWithinRad(fPositionRound,
                                          TKMHouseTower(fHome).RangeMin,
                                          TKMHouseTower(fHome).GetRangeMax,
                                          Owner, atEnemy, dirNA, not RANDOM_TARGETS, False);

  // Note: In actual game there might be two Towers nearby,
  // both throwing a stone into the same enemy. We should not
  // negate that fact, thats real-life situation.

  if enemy <> nil then
    fTask := TKMTaskThrowRock.Create(Self, enemy);
end;

procedure TKMUnitWoodcutter.TaskCutForest;
var TF : TKMHouseForest;
  I : Integer;
begin
  If Home = nil then
    Exit;
  If Home.HouseType <> htForest then
    Exit;
  TF := TKMHouseForest(Home);
  If TF.CheckWareOut(wtTrunk) >= TF.GetMaxOutWare then
    Exit;
  I := TF.HasTreeToCut;
  If I = -1 then
    Exit;
  fTask := TKMTaskForestCutting.Create(self, Home, I);
end;

procedure TKMUnitWoodcutter.TaskGetWork;
begin
  If Home.HouseType = htForest then
    TaskCutForest
  else
    Inherited;
end;


{ TKMSerf }
constructor TKMUnitSerf.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
begin
  inherited;
  fCarry := wtNone;
  fIdleTimer := 0;
end;


procedure TKMUnitSerf.Deliver(aFrom, toHouse: TKMHouse; aWare: TKMWareType; aID: integer);
begin
  fThought := thNone; //Clear ? thought
  fTask := TKMTaskDeliver.Create(Self, aFrom, toHouse, aWare, aID);
end;


procedure TKMUnitSerf.Deliver(aFrom: TKMHouse; toUnit: TKMUnit; aWare: TKMWareType; aID: integer);
begin
  fThought := thNone; //Clear ? thought
  fTask := TKMTaskDeliver.Create(Self, aFrom, toUnit, aWare, aID);
end;

procedure TKMUnitSerf.Deliver(aFrom: TKMHouse; aTo: TKMPoint; aWare: TKMWareType; aID: Integer);
begin
  fThought := thNone; //Clear ? thought
  fTask := TKMTaskDeliver.Create(Self, aFrom, aTo, aWare, aID);
end;

procedure TKMUnitSerf.Deliver(aFrom: TKMHouse; aToStruc: TKMStructure; aWare: TKMWareType; aID: integer);
begin
  fThought := thNone; //Clear ? thought
  fTask := TKMTaskDeliver.Create(Self, aFrom, aToStruc, aWare, aID);
end;

function TKMUnitSerf.GetCarry: TKMWareType;
begin
  if Self = nil then Exit(wtNone);

  Result := fCarry;
end;


function TKMUnitSerf.TryDeliverFrom(aFrom: TKMHouse): Boolean;
var
  T: TKMUnitTask;
begin
  Result := False;

  //Save current task
  T := fTask;

  //Try to get a new one
  if gHands[Owner].Deliveries.Queue.AskForDelivery(Self, aFrom) then
  begin
    Result := True;
    FreeAndNil(T); // Destroy old task, we created a new one
  end;

  // If we got ourselves a new task then skip to resource-taking part, as we are already in this house
  if Result and (aFrom <> nil) then
    fTask.Phase := 2; // Skip  of the new task
end;


//Delegate delivery to other serf
procedure TKMUnitSerf.DelegateDelivery(aToSerf: TKMUnit);
begin
  Assert(Task is TKMTaskDeliver, 'UnitTask is not TKMTaskDeliver');

  TKMTaskDeliver(Task).DelegateToOtherSerf(aToSerf); //Update task to be used with new serf
  aToSerf.fTask := Task; //Set delivery task to new serf

  //AbandonWalk if needed and cleanup task pointer, but do not free task object, as it was delegated to other serf already
  CancelTask(False);
end;


constructor TKMUnitSerf.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.Read(fCarry, SizeOf(fCarry));
end;


function TKMUnitSerf.GetDesiredPassability: TKMTerrainPassability;
begin
  Result := Inherited;

  IF gHands[Owner].EconomyDevUnlocked(26) then
    Result := tpWalk;
end;

procedure TKMUnitSerf.PaintUnit(aTickLag: Single);
var
  ID: Integer;
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  xPaintPos, yPaintPos: Single;
begin

  V := fVisual.GetLerp(aTickLag);
  act := V.Action;

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  ID := UID * Byte(not (act in [uaDie, uaEat]));

  gRenderPool.AddUnit(UnitType, ID, act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if fTask is TKMTaskDie then Exit; //Do not show unnecessary arms

  if Carry <> wtNone then
    gRenderPool.AddUnitCarry(Carry, ID, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor))
  else
    gRenderPool.AddUnit(UnitType, ID, uaWalkArm, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), False);

  if fThought <> thNone then
    gRenderPool.AddUnitThought(fType, act, V.Dir, V.AnimStep, fThought, xPaintPos, yPaintPos);
end;


procedure TKMUnitSerf.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fCarry, SizeOf(fCarry));
end;


function TKMUnitSerf.ObjToString(const aSeparator: String = '|'): String;
begin
  if Self = nil then Exit('nil');

  Result := inherited ObjToString(aSeparator)
          + Format('%sCarry = %s', [aSeparator, GetEnumName(TypeInfo(TKMWareType), Integer(fCarry))]);
end;


function TKMUnitSerf.UpdateState: Boolean;
var
  oldThought: TKMUnitThought;
  wasIdle: Boolean;
begin
  Result := True; //Required for override compatibility
  wasIdle := IsIdle;
  if fAction = nil then raise ELocError.Create(gRes.Units[UnitType].GUIName+' has no action at start of TKMUnitSerf.UpdateState',fPositionRound);
  if inherited UpdateState then
    Exit;

  oldThought := fThought;
  fThought := thNone;

  CheckCondition;

  //Only show quest thought if we have been idle since the last update (not HadTask)
  //and not thinking anything else (e.g. death)
  if fTask = nil then
  begin
    Inc(fIdleTimer);
    if wasIdle and not IsHungry{(oldThought = thNone)} and (KaMRandom(2, 'TKMUnitSerf.UpdateState') = 0) then
      fThought := thQuest
    else
    if oldThought <> thQuest then
      fThought := oldThought;

    if fIdleTimer > 10 then
      GoGetBoots;

    SetActionStay(60,uaWalk); //Stay idle
  end else
    fIdleTimer := 0;

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName+' has no action at end of TKMUnitSerf.UpdateState', fPositionRound);
end;


procedure TKMUnitSerf.CarryGive(aWare: TKMWareType);
begin
  Assert(fCarry = wtNone, 'Giving Serf another Carry');
  fCarry := aWare;
  //SetSpeed(28 - SERF_WARE_SPEED_DECREASE[aWare]);
end;


procedure TKMUnitSerf.CarryTake;
begin
  Assert(Carry <> wtNone, 'Taking wrong ware from Serf');
  fCarry := wtNone;
  //SetSpeed(28);
end;

function TKMUnitSerf.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
var fCarrySpeed : Integer;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtWalkDiag)
  else
    Result := GetEffectiveSpeed(umtWalk);

  fCarrySpeed := SERF_WARE_SPEED_DECREASE[Carry];

  case gTerrain.GetRoadType(fPositionRound) of
    rtWooden: If gHands[Owner].BuildDevUnlocked(6) then fCarrySpeed := -1;
    rtStone : fCarrySpeed := fCarrySpeed - 2;
    rtClay: fCarrySpeed := fCarrySpeed - 5;
    rtExclusive: fCarrySpeed := -5;
  end;
  Result := Result - ConvertSpeed(fCarrySpeed, aIsDiag);
end;

function TKMUnitMountedSerf.GetDesiredPassability: TKMTerrainPassability;
begin
  Result := Inherited;
  //Result := tpWalk;
end;

function TKMUnitMountedSerf.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
var fCarrySpeed : Integer;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtWalkDiag)
  else
    Result := GetEffectiveSpeed(umtWalk);
  fCarrySpeed := 0;
  case gTerrain.GetRoadType(fPositionRound) of
    rtWooden: If gHands[Owner].BuildDevUnlocked(6) then fCarrySpeed := -1;
    rtStone : fCarrySpeed := fCarrySpeed - 2;
    rtClay: fCarrySpeed := fCarrySpeed - 5;
    rtExclusive: fCarrySpeed := -5;
  end;
  Result := Result - ConvertSpeed(fCarrySpeed, aIsDiag);
end;

procedure TKMUnitMountedSerf.PaintUnit(aTickLag: Single);
var
  ID: Integer;
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  xPaintPos, yPaintPos: Single;
begin

  V := fVisual.GetLerp(aTickLag);
  act := V.Action;

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  ID := UID * Byte(not (act in [uaDie, uaEat]));
  If Carry <> wtNone then
    act := uaWork;

  gRenderPool.AddUnit(UnitType, ID, act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if fThought <> thNone then
    gRenderPool.AddUnitThought(fType, act, V.Dir, V.AnimStep, fThought, xPaintPos, yPaintPos);
end;


{ TKMWorker }
function TKMUnitWorker.OnlyBuildsHouse: Boolean;
begin
  Result := UnitType = utHouseBuilder;
end;

function TKMUnitWorker.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
begin
  Result := Inherited;

  If gHands[Owner].BuildDevUnlocked(14) then
    Result := Result - ConvertSpeed(-2, aIsDiag);
end;

procedure TKMUnitWorker.BuildHouse(aHouse: TKMHouse; aIndex: Integer);
begin
  fTask := TKMTaskBuildHouse.Create(Self, aHouse, aIndex);
end;

procedure TKMUnitWorker.BuildHouseUpgrade(aHouse: TKMHouse; aIndex: Integer);
begin
  fTask := TKMTaskBuildHouseUpgrade.Create(Self, aHouse, aIndex);
end;

procedure TKMUnitWorker.BuildPearl(aHouse: TKMHouse; aIndex: Integer);
begin
  fTask := TKMTaskBuildPearl.Create(Self, aHouse, aIndex);
end;

procedure TKMUnitWorker.BuildStructure(aStructure : TKMStructure; aIndex: Integer);
begin
  fTask := TKMTaskBuildStructure.Create(Self, aStructure, aIndex);
end;


procedure TKMUnitWorker.BuildField(aField: TKMFieldType; aLoc: TKMPoint; aIndex: Integer; aRoadType : TKMRoadType);
begin
  case aField of
    ftRemove:   fTask := TKMTaskBuildRemove.Create(Self, aLoc, aIndex);
    ftPalisade: fTask := TKMTaskBuildPalisade.Create(Self, aLoc, aIndex);
    ftRoad: fTask := TKMTaskBuildRoad.Create(Self, aLoc, aIndex, aRoadType);
    ftGrassLand: fTask := TKMTaskBuildGrassLand.Create(Self, aLoc, aIndex);
    ftVegeField: fTask := TKMTaskBuildVegeField.Create(Self, aLoc, aIndex);
    ftCorn: fTask := TKMTaskBuildField.Create(Self, aLoc, aIndex);
    ftWine: fTask := TKMTaskBuildWine.Create(Self, aLoc, aIndex);
  else
    raise Exception.Create('Unexpected TKMFieldType');
  end;
end;


procedure TKMUnitWorker.BuildHouseArea(aHouseType: TKMHouseType; aLoc: TKMPoint; aIndex: Integer);
begin
  fTask := TKMTaskBuildHouseArea.Create(Self, aHouseType, aLoc, aIndex);
end;


procedure TKMUnitWorker.BuildHouseRepair(aHouse: TKMHouse; aIndex: Integer);
begin
  fTask := TKMTaskBuildHouseRepair.Create(Self, aHouse, aIndex);
end;


//Given a list check the locations in it and pick those that can be walked to
//excluding current location. We assume that caller already made the list
//with only walkable valid tiles
function TKMUnitWorker.PickRandomSpot(aList: TKMPointDirList; out Loc: TKMPointDir): Boolean;
var
  I, myCount: Integer;
  spots: array of Word;
begin
  SetLength(spots, aList.Count);

  //Scan the list and pick suitable locations
  myCount := 0;
  for I := 0 to aList.Count - 1 do
  if {not KMSamePoint(aList[I].Loc, Position)
  and }CanWalkTo(aList[I].Loc, 0) then
  begin
    spots[myCount] := I;
    Inc(myCount);
  end;

  Result := (myCount > 0);
  if Result then
    Loc := aList[spots[KaMRandom(myCount, 'TKMUnitWorker.PickRandomSpot')]];
end;

function TKMUnitWorker.PickNextSpot(aList: TKMPointDirList; out Loc: TKMPointDir; var aLastCellID : Byte): Boolean;
  function TileHasWorkingWorker(aLoc : TKMPoint) : Boolean;
  var W : TKMUnitWorker;
  begin
    if (gTerrain.GetUnit(aLoc) = nil) or not (gTerrain.GetUnit(aLoc) is TKMUnitWorker) then //unit must be worker
      Exit(false);
    W := TKMUnitWorker(gTerrain.GetUnit(aLoc));
    Result := not W.IsDeadOrDying; //ignore dead workers
  end;
var
  I, J, myCount, aSpot: Integer;
  spots: array of Word;
begin
  SetLength(spots, aList.Count);

  //Scan the list and pick suitable locations
  myCount := 0;
  for I := 0 to aList.Count - 1 do
  if CanWalkTo(aList[I].Loc, 0)
  and not gTerrain.AvoidTile(aList[I].Loc)
  //and not TileHasWorkingWorker(aList[I].Loc)
  then
  begin
    spots[myCount] := I;
    Inc(myCount);
  end;
  if aList.Count = 0 then
    Exit(false);
  //Scan the list and pick suitable locations
  I := 0;
  if myCount = 0 then
    Exit(false);
  aSpot := aLastCellID mod {aList.Count}myCount;
  repeat
    //aLastCellID := EnsureRange(aLastCellID + 1, 0, 255);
    IncLoop(aLastCellID, 0, 255);
    J := spots[aSpot];
    if not TileHasWorkingWorker(aList[J].Loc) then
      Break
    else
      aSpot := aLastCellID mod {aList.Count}myCount;

    Inc(I);
  until I = 10;

  Result := (myCount > 0);
  if Result then
    Loc := aList[spots[aSpot]];
end;

procedure TKMUnitWorker.PaintUnit(aTickLag: Single);
var
  ID: Integer;
  V: TKMUnitVisualState;
  xPaintPos, yPaintPos: Single;
begin
  V := fVisual.GetLerp(aTickLag);

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  ID := UID * Byte(not (V.Action in [uaDie, uaEat]));

  gRenderPool.AddUnit(UnitType, ID, V.Action, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if fThought <> thNone then
    gRenderPool.AddUnitThought(fType, V.Action, V.Dir, V.AnimStep, fThought, xPaintPos, yPaintPos);
end;


function TKMUnitWorker.UpdateState: Boolean;
begin
  Result := True; //Required for override compatibility
  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at start of TKMUnitWorker.UpdateState', fPositionRound);

  if inherited UpdateState then Exit;

  CheckCondition;

  if fTask = nil then
  begin
    Inc(fIdleTimer);
    if fIdleTimer > 10 then
      GoGetBoots;
  end else
    fIdleTimer := 0;

  if (fThought = thBuild) and (fTask = nil) then
    Thought := thNone; //Remove build thought if we are no longer doing anything

  if (fTask = nil) and (fAction = nil) then SetActionStay(20, uaWalk);

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at end of TKMUnitWorker.UpdateState', fPositionRound);
end;




{ TKMUnitFeeder }

procedure TKMUnitFeeder.CheckGroups;

  procedure Delete(aIndex : integer);
  begin
    gHands.CleanUpGroupPointer(TKMUnitGroup(fGroupsToFeed[aIndex].Group) );
    fGroupsToFeed[aIndex] := fGroupsToFeed[high(fGroupsToFeed)];
    SetLength(fGroupsToFeed, high(fGroupsToFeed));
  end;

var i : integer;
begin
  for I := High(fGroupsToFeed) downto 0 do
    If TKMUnitGroup(fGroupsToFeed[I].Group).IsDead then
      Delete(I);
end;

function TKMUnitFeeder.HasGroup(aGroup: Pointer): Boolean;
var I : Integer;
begin
  Result := false;
  for I := High(fGroupsToFeed) downto 0 do
    If fGroupsToFeed[I].Group = aGroup then
      Exit(true);
end;

function TKMUnitFeeder.DeleteGroup(aGroup: Pointer): Boolean;
  procedure Delete(aIndex : integer);
  begin
    gHands.CleanUpGroupPointer(TKMUnitGroup(fGroupsToFeed[aIndex].Group) );
    fGroupsToFeed[aIndex] := fGroupsToFeed[high(fGroupsToFeed)];
    SetLength(fGroupsToFeed, high(fGroupsToFeed));
  end;
var I : Integer;
begin
  Result := false;
  for I := High(fGroupsToFeed) downto 0 do
    If fGroupsToFeed[I].Group = aGroup then
      Delete(I);
end;

procedure TKMUnitFeeder.AddGroupToFeed(aGroup: Pointer);
var I : Integer;
begin
  I := length(fGroupsToFeed);
  SetLength(fGroupsToFeed, I + 1);
  fGroupsToFeed[I].Group := TKMUnitGroup(aGroup).GetPointer;
  fGroupsToFeed[I].FoodRequested := TKMUnitGroup(aGroup).Count;
end;

function TKMUnitFeeder.FirstGroup: Pointer;
begin
  //delete died groups
  CheckGroups;

  If length(fGroupsToFeed) > 0 then
    Result := fGroupsToFeed[0].Group
  else
    Result := nil;
end;

procedure TKMUnitFeeder.CollectFood(aCount: Word);
begin
  Inc(fFoodCount, aCount);
end;

procedure TKMUnitFeeder.FeedGroup(aGroup: Pointer);
var G : TKMUnitGroup;
  W : TKMUnitWarrior;
  I : Integer;
begin
  G := aGroup;
  If HasGroup(aGroup) then
    DeleteGroup(aGroup);

  If G.IsDead then
    Exit;

  for I := 0 to G.Count - 1 do
  begin
    W := G.Members[I];
    If not W.IsDeadOrDying then
    begin
      If fFoodCount > 0 then
        dec(fFoodCount);
      W.Feed(UNIT_MAX_CONDITION);
    end;
  end;
  G.FoodRequested := false;

end;

function TKMUnitFeeder.NeededFood: Word;
var I : integer;
begin
  Result := 0;
  for I := 0 to High(fGroupsToFeed) do
    Inc(Result, fGroupsToFeed[I].FoodRequested);
  Result := EnsureRange(Result - fFoodCount, 0, high(Word));
end;

function TKMUnitFeeder.TaskStarted: Boolean;
begin
  Result := IsHungry or ((fTask is TKMTaskFeeder) and (TKMTaskFeeder(fTask).Phase > 2));
end;

function TKMUnitFeeder.FindBestStore: TKMHouse;
var I, lastBest, needsFood : Integer;
  arr : TKMArray<TKMHouse>;
  bid : array of Integer;
  H : TKMHouseStore;
begin
  Result := nil;
  arr.Clear;
  for I := 0 to gHands[Owner].Houses.Stores.Count - 1 do
  begin
    H := TKMHouseStore(gHands[Owner].Houses.Stores[I]);
    //get valid stores
    If H.IsValid and (H.FoodCount > 0) and CanWalkTo(H.PointBelowEntrance, 1.42) then
      arr.Add(TKMHouse(H));
  end;

  If arr.Count = 0 then
    Exit;
  SetLength(bid, arr.Count);

  needsFood := NeededFood;

  for I := 0 to arr.Count - 1 do
  begin
    bid[I] := 100000;
    H := TKMHouseStore(arr[I]);

    Inc(  bid[I], Round(KMLengthDiag(Position, H.Entrance))   );
    If H.FoodCount > needsFood then
      Dec(bid[I], H.FoodCount);
  end;

  lastBest := high(Integer);

  for I := 0 to arr.Count - 1 do
    If bid[I] < lastBest then
    begin
      Result := arr[I];
      lastBest := bid[I];
    end;
end;

procedure TKMUnitFeeder.TryToFeedGroups;
var H : TKMHouse;
begin
  If length(fGroupsToFeed) = 0 then
    Exit;

  CancelTask;
  H := FindBestStore;
  If H = nil then
    Exit;
  fTask := TKMTaskFeeder.Create(self, H);
end;

procedure TKMUnitFeeder.CloseUnit(aRemoveTileUsage: Boolean = True);
var I : integer;
begin
  for I := 0 to High(fGroupsToFeed) do
  begin
    TKMUnitGroup(fGroupsToFeed[I].Group).FoodRequested := false;
    TKMUnitGroup(fGroupsToFeed[I].Group).OrderFood(true);
    gHands.CleanUpGroupPointer(TKMUnitGroup(fGroupsToFeed[I].Group) );
  end;
  Inherited;
end;

procedure TKMUnitFeeder.PaintUnit(aTickLag: Single);
var
  ID: Integer;
  V: TKMUnitVisualState;
  xPaintPos, yPaintPos: Single;
begin
  V := fVisual.GetLerp(aTickLag);

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  ID := UID * Byte(not (V.Action in [uaDie, uaEat]));

  gRenderPool.AddUnit(UnitType, ID, V.Action, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if fThought <> thNone then
    gRenderPool.AddUnitThought(fType, V.Action, V.Dir, V.AnimStep, fThought, xPaintPos, yPaintPos);
end;

constructor TKMUnitFeeder.Load(LoadStream: TKMemoryStream);
var I, J : integer;
begin
  Inherited;
  LoadStream.Read(J);
  SetLength(fGroupsToFeed, J);
  for I := 0 to J - 1 do
  begin
    LoadStream.Read(fGroupsToFeed[I].FoodRequested);
    LoadStream.Read(fGroupsToFeed[I].Group, 4);
  end;
  LoadStream.Read(fFoodCount);

end;

procedure TKMUnitFeeder.Save(SaveStream: TKMemoryStream);
var I, J : integer;
begin
  Inherited;
  J := length(fGroupsToFeed);
  SaveStream.Write(J);
  for I := 0 to J - 1 do
  begin
    SaveStream.Write(fGroupsToFeed[I].FoodRequested);
    SaveStream.Write(TKMUnitGroup(fGroupsToFeed[I].Group).UID);
  end;
  SaveStream.Write(fFoodCount);
end;

procedure TKMUnitFeeder.SyncLoad;
var I: integer;
begin
  Inherited;
  for I := 0 to High(fGroupsToFeed) do
    fGroupsToFeed[I].Group := gHands.GetGroupByUID(Integer(fGroupsToFeed[I].Group) );

end;

function TKMUnitFeeder.UpdateState: Boolean;
begin
  Result := True; //Required for override compatibility
  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at start of TKMUnitWorker.UpdateState', fPositionRound);

  if inherited UpdateState then Exit;

  CheckCondition;
  Inc(fTicker);
  If fTask = nil then
      TryToFeedGroups;

  if fTask = nil then
  begin
    Inc(fIdleTimer);
    if fIdleTimer > 10 then
      GoGetBoots;
  end else
    fIdleTimer := 0;

  if (fTask = nil) then
    Thought := thNone; //Remove build thought if we are no longer doing anything

  if (fTask = nil) and (fAction = nil) then SetActionStay(20, uaWalk);

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at end of TKMUnitWorker.UpdateState', fPositionRound);
end;





{ TKMUnitAnimal }
constructor TKMUnitAnimal.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID);
begin
  inherited Create(aID, aUnitType, aLoc, aOwner, nil);
  fSpawnerID := -1;
  fStartLoc := aLoc.Loc;
  fRadius := 20
end;


function TKMUnitAnimal.IsAnimal: Boolean;
begin
  Result := True;
end;

function TKMUnitAnimal.CanStepTo(X: Integer; Y: Integer; aPass: TKMTerrainPassability): Boolean;
var spawner : PKMAnimalSpawner;
begin
  Result := Inherited;
  if fSpawnerID <> -1 then
  begin
    spawner := gHands.PlayerAnimals.Spawners[fSpawnerID];
    Result := Result and (KMLengthSqr(spawner.Loc.X, spawner.Loc.Y, X, Y) <= sqr(spawner.Radius) );
  end else
    Result := Result and (KMLengthSqr(fStartLoc.X, fStartLoc.Y, X, Y) <= sqr(fRadius) );
end;

function TKMUnitAnimal.CheckForTrap : Boolean;
var P : TKMPoint;
begin
  Result := false;
  P := PositionNext;
  if gTerrain.GetObject(P) = 539 then
  begin
    Result := true;
    case UnitType of
      utWolf: gTerrain.SetObject(P, 540);
      utDeerMale: gTerrain.SetObject(P, 541);
      utDeerFemale: gTerrain.SetObject(P, 542);
      utFox: gTerrain.SetObject(P, 543);
      utBoar: gTerrain.SetObject(P, 544);
      utBear: gTerrain.SetObject(P, 545);
      utLandDuck: gTerrain.SetObject(P, 546);
      utRabbit: gTerrain.SetObject(P, 547);
      utWhiteBear: gTerrain.SetObject(P, 548);
      else
        Result := false;
    end;

  end;

end;

function TKMUnitAnimal.GetIsSelectable: Boolean;
begin
  Result := False;
end;

constructor TKMUnitAnimal.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fSpawnerID);
  LoadStream.Read(fStartLoc);
  LoadStream.Read(fRadius);
end;

procedure TKMUnitAnimal.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fSpawnerID);
  SaveStream.Write(fStartLoc);
  SaveStream.Write(fRadius);
end;

function TKMUnitAnimal.UpdateState: Boolean;
var makeKill : Boolean;
begin
  Result := True; //Required for override compatibility

  SetPositionRoundByPosF;

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at start of TKMUnitAnimal.UpdateState', fPositionRound);

  if fKillASAP then
  begin
    if fSpawnerID <> -1 then
      gHands.PlayerAnimals.RemoveFromSpawner(self, fSpawnerID);
      //gHands.PlayerAnimals.Spawners[fSpawnerID].Animals.Remove(self);
    DoKill(fKillASAPShowAnimation);
    fKillASAP := False;
    Exit;
  end;
  Inc(fTicker);
  makeKill := false;
  if fTicker mod 10 = 0 then
    If CheckForTrap then
    begin
      if fSpawnerID <> -1 then
        gHands.PlayerAnimals.RemoveFromSpawner(self, fSpawnerID);
        //gHands.PlayerAnimals.Spawners[fSpawnerID].Animals.Remove(self);

      Kill(HAND_NONE, false, False); //Animal is stuck so it dies
    end;

  case fAction.Execute of
    arActContinues: Exit;
    arActAborted :  begin
                      makeKill := true;
                      FreeAndNil(fAction);
                    end
  else
    FreeAndNil(fAction);
  end;
  SetPositionRoundByPosF;

  Assert((fTask = nil) or (fTask is TKMTaskDie));
  if fTask is TKMTaskDie then
    case fTask.Execute of
      trTaskContinues:  Exit;
      trTaskDone:       raise Exception.Create('Unexpected fUnitTask.Execute value = trTaskDone'); //TTaskDie never returns trTaskDone yet
    end;
  //First make sure the animal isn't stuck (check passibility of our position)
  if makeKill
  or (not gTerrain.CheckPassability(fPositionRound, DesiredPassability))
  or gTerrain.CheckAnimalIsStuck(fPositionRound, DesiredPassability) then
  begin
    if fSpawnerID <> -1 then
      gHands.PlayerAnimals.Spawners[fSpawnerID].Animals.Remove(self);

    Kill(HAND_NONE, false, False); //Animal is stuck so it dies
    Exit;
  end;

  SetActionSteer;

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at end of TKMUnitAnimal.UpdateState', fPositionRound);
end;


function TKMUnitAnimal.GetPaintActionType(aAct: TKMUnitActionType): TKMUnitActionType;
begin
  Result := aAct;
end;


//For fish the action is the number of fish in the group
procedure TKMUnitAnimal.PaintUnit(aTickLag: Single);
var
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  xPaintPos, yPaintPos: Single;
begin
  V := fVisual.GetLerp(aTickLag);

  act := GetPaintActionType(V.Action);

  xPaintPos := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  yPaintPos := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  //Make fish/watersnakes more visible in the MapEd
  if (gGameParams.Mode = gmMapEd) and (fType in [utFish, utWatersnake, utSeastar]) then
    gRenderAux.Circle(fPositionF.X - 0.5,
                      gTerrain.FlatToHeight(fPositionF.X - 0.5, fPositionF.Y - 0.5),
                      0.5, $30FF8000, $60FF8000);

  //Animals share the same WalkTo logic as other units and they exchange places if necessary
  //Animals can be picked only in MapEd
  gRenderPool.AddUnit(fType, UID * Byte(gGameParams.IsMapEditor), act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, $FFFFFFFF, True);
end;


{ TKMUnitFish }
constructor TKMUnitFish.Create(aID: Cardinal; const aLoc: TKMPointDir; aOwner: TKMHandID);
begin
  inherited Create(aID, utFish, aLoc, aOwner);

  // Always start with 10 fish in the group
  fFishCount := FISH_CNT_DEFAULT;
end;


constructor TKMUnitFish.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('UnitFish');
  LoadStream.Read(fFishCount);
end;


class function TKMUnitFish.GetFishActionType(aFishCount: Integer): TKMUnitActionType;
begin
  Result := uaUnknown;

  if aFishCount > 8 then
    Result := uaWork1
  else
    case aFishCount of
      1,2: Result := uaWalk;
      3,4: Result := uaWork;
      5,6: Result := uaSpec;
      7,8: Result := uaDie;
    end;
end;


function TKMUnitFish.GetPaintActionType(aAct: TKMUnitActionType): TKMUnitActionType;
begin
  Result := GetFishActionType(fFishCount);
end;


function TKMUnitFish.ReduceFish(aCount : Integer = 1) : Word;
begin
  Result := Min(aCount, fFishCount);

  if fFishCount > aCount then
    Dec(fFishCount, aCount)
  else
    Kill(HAND_NONE, True, False);
end;


procedure TKMUnitFish.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('UnitFish');
  SaveStream.Write(fFishCount);
end;


{ TKMUnit }
constructor TKMUnit.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
begin
  inherited Create(etUnit, aID, aOwner);
  fTicker       := aID; //Units update states will be spread more evenly that way
  fIsDead       := False;
  fKillASAP     := False;
  fThought      := thNone;
  fHome         := nil;
  fInHouse      := nil;
  fPositionF    := KMPointF(aLoc.Loc);
  fPositionRound := aLoc.Loc;
  fPositionPrev := aLoc.Loc; //Init values
  fPositionNext := aLoc.Loc; //Init values
  fType         := aUnitType;
  fDirection    := aLoc.Dir;
  fVisible      := True;
  IsExchanging  := False;
  AnimStep      := UNIT_STILL_FRAMES[fDirection]; //Use still frame at begining, so units don't all change frame on first tick
  Dismissable   := True;
  fLastTimeTrySetActionWalk := 0;
  fKilledBy     := HAND_NONE;
  InstantKill := false;
  IsHero := false;

  fSpecialEffect.Duration := 0;

  //Now units condition pace can be changed by scripts
  if aUnitType in SIEGE_MACHINES then
    fConditionPace := 20
  else
    fConditionPace := 10;

  if gGameParams.MBD.IsEasy then
    fConditionPace := Round(fConditionPace * 1.5)
  else
  if gGameParams.MBD.IsHardOrRealism then
    fConditionPace := Round(fConditionPace * 0.8);

  If aUnitType in UNITS_HUMAN then
    if gHands[Owner].HasPearl(ptValtaria) then
      fConditionPace := Round(fConditionPace * 1.3);

  //Units start with a random amount of condition ranging from 0.5 to 0.7 (KaM uses 0.6 for all units)
  //By adding the random amount they won't all go eat at the same time and cause crowding, blockages, food shortages and other problems.

  if fType in SIEGE_MACHINES then
    fCondition := UNIT_MAX_CONDITION
  else
  if (gGameParams <> nil) and not gGameParams.IsMapEditor then
    fCondition    := Round(UNIT_MAX_CONDITION * (UNIT_CONDITION_BASE + KaMRandomS2(UNIT_CONDITION_RANDOM, 'TKMUnit.Create')))
  else begin
    fCondition    := GetDefaultCondition;
    fStartWDefaultCondition := True;
  end;

  fHitPointCounter := 1;
  HitPointsInvulnerable := False;

  fAttack     := gRes.Units[fType].Attack;
  fAttackHorse:= gRes.Units[fType].AttackHorse;
  fDefence    := gRes.Units[fType].Defence;
  fSpeed      := gRes.Units[fType].Speed;
  fSight      := gRes.Units[fType].Sight;
  fHitPointsMax := gRes.Units[fType].HitPoints;
  fHitPoints      := HitPointsMax;
  fProjectileDefence := gRes.Units[fType].ProjectileDefence;
  UpdateSpeed;

  //Must be locked for this initial pause so animals don't get pushed
  if IsAnimal then
    SetActionLockedStay(10, uaWalk)
  else
    SetActionStay(10, uaWalk);

  // Use SetInHouse for a safe unit pointers operation
  SetInHouse(aInHouse);
  // Do not add units which are trained inside house
  if fInHouse = nil then
    gTerrain.UnitAdd(PositionNext, Self);

  // Create UnitVisual after InHouse is set
  fVisual := TKMUnitVisual.Create(Self);

  //The area around the unit should be visible at the start of the mission
  if InRange(Owner, 0, MAX_HANDS - 1) then //Not animals
    gHands.RevealForTeam(Owner, fPositionRound, fSight, FOG_OF_WAR_MAX, frtUnit);

  if fType in UNITS_SHIPS then
  begin
    Condition := UNIT_MAX_CONDITION;
    NeverHungry := true;
  end;
  Immortal := false;


  AllowAllyToSelect := true {gHands[Owner].IsHuman or gHands[Owner].CanBeHuman};

end;


destructor TKMUnit.Destroy;
begin
  if not IsDead then gTerrain.UnitRem(PositionNext); //Happens only when removing player from map on GameStart (network)

  fHome.RemoveWorker(self);
  //fHome.SetWorker(nil);
  FreeAndNil(fAction);
  FreeAndNil(fTask);
  FreeAndNil(fVisual);
  SetInHouse(nil); //Free pointer
  inherited;
end;


constructor TKMUnit.Load(LoadStream: TKMemoryStream);
var
  hasTask, hasAct: Boolean;
  taskName: TKMUnitTaskType;
  actName: TKMUnitActionName;
begin
  inherited;

  LoadStream.CheckMarker('Unit');
  LoadStream.Read(fType, SizeOf(fType));
  LoadStream.Read(hasTask);
  if hasTask then
  begin
    LoadStream.Read(taskName, SizeOf(taskName));
    case taskName of
      uttUnknown:         raise Exception.Create('TaskName can''t be handled');
      uttSelfTrain:       fTask := TKMTaskSelfTrain.Load(LoadStream);
      uttDeliver:         fTask := TKMTaskDeliver.Load(LoadStream);
      uttBuildRoad:       fTask := TKMTaskBuildRoad.Load(LoadStream);
      uttBuildPalisade:   fTask := TKMTaskBuildPalisade.Load(LoadStream);
      uttBuildRemove:     fTask := TKMTaskBuildRemove.Load(LoadStream);
      uttBuildWine:       fTask := TKMTaskBuildWine.Load(LoadStream);
      uttBuildField:      fTask := TKMTaskBuildField.Load(LoadStream);
      uttBuildHouseArea:  fTask := TKMTaskBuildHouseArea.Load(LoadStream);
      uttBuildHouse:      fTask := TKMTaskBuildHouse.Load(LoadStream);
      uttBuildHouseUpgrade:fTask := TKMTaskBuildHouseUpgrade.Load(LoadStream);
      uttBuildHouseRepair:fTask := TKMTaskBuildHouseRepair.Load(LoadStream);
      uttGoHome:          fTask := TKMTaskGoHome.Load(LoadStream);
      uttDismiss:         fTask := TKMTaskDismiss.Load(LoadStream);
      uttAttackHouse:     fTask := TKMTaskAttackHouse.Load(LoadStream);
      uttThrowRock:       fTask := TKMTaskThrowRock.Load(LoadStream);
      uttGoEat:           fTask := TKMTaskGoEat.Load(LoadStream);
      uttMining:          fTask := TKMTaskMining.Load(LoadStream);
      uttDie:             fTask := TKMTaskDie.Load(LoadStream);
      uttGoOutShowHungry: fTask := TKMTaskGoOutShowHungry.Load(LoadStream);
      uttGoGetBoots:      fTask := TKMTaskGoGetBoots.Load(LoadStream);
      uttBuildStructure:  fTask := TKMTaskBuildStructure.Load(LoadStream);
      uttGoToShip:        fTask := TKMTaskAssignToShip.Load(LoadStream);
      uttBuildGrassLand:  fTask := TKMTaskBuildGrassLand.Load(LoadStream);
      uttUnloadFromShip:  fTask := TKMTaskUnloadFromShip.Load(LoadStream);
      uttGoToStore:       fTask := TKMTaskGoToStore.Load(LoadStream);
      uttGoToLoc:         fTask := TKMTaskGoToLoc.Load(LoadStream);
      uttCollectWares:    fTask := TKMTaskCollectWares.Load(LoadStream);
      uttUnloadWares:     fTask := TKMTaskUnloadWares.Load(LoadStream);
      uttGoToWell:        fTask := TKMTaskGoToWell.Load(LoadStream);
      uttTakeOverHouse:   fTask := TKMTaskTakeOverHouse.Load(LoadStream);
      uttMerchant:        fTask := TKMTaskMerchant.Load(LoadStream);
      uttCartographer:    fTask := TKMTaskCartographer.Load(LoadStream);
      uttBuildPearl:      fTask := TKMTaskBuildPearl.Load(LoadStream);
      uttPearlRally:      fTask := TKMTaskPearlRally.Load(LoadStream);
      uttGoToPearl:       fTask := TKMTaskGoToPearl.Load(LoadStream);
      uttForestCutter:    fTask := TKMTaskForestCutting.Load(LoadStream);
      uttFeedGroup:       fTask := TKMTaskFeeder.Load(LoadStream);
      uttShootAtSpot:     fTask := TKMTaskShootAtSpot.Load(LoadStream);
      uttShootFromSiege:     fTask := TKMTaskShootFromSiegeTower.Load(LoadStream);
      uttEnterSiegeTower:     fTask := TKMTaskEnterSiegeTower.Load(LoadStream);
    else
      raise Exception.Create('TaskName can''t be handled');
    end;
  end
  else
    fTask := nil;

  LoadStream.Read(hasAct);
  if hasAct then
  begin
    LoadStream.Read(actName, SizeOf(actName));
    case actName of
      uanStay:        fAction := TKMUnitActionStay.Load(LoadStream);
      uanWalkTo:      fAction := TKMUnitActionWalkTo.Load(LoadStream);
      uanAbandonWalk: fAction := TKMUnitActionAbandonWalk.Load(LoadStream);
      uanGoInOut:     fAction := TKMUnitActionGoInOut.Load(LoadStream);
      uanFight:       fAction := TKMUnitActionFight.Load(LoadStream);
      uanStormAttack: fAction := TKMUnitActionStormAttack.Load(LoadStream);
      uanSteer:       fAction := TKMUnitActionSteer.Load(LoadStream);
    else
      raise Exception.Create('ActName can''t be handled');
    end;
  end
  else
    fAction := nil;

  LoadStream.Read(fThought, SizeOf(fThought));
  LoadStream.Read(fCondition);
  LoadStream.Read(fStartWDefaultCondition);
  LoadStream.Read(fTicker);
  LoadStream.Read(fHitPoints);
  LoadStream.Read(fHitPointCounter);
  LoadStream.Read(HitPointsInvulnerable);
  LoadStream.Read(fInHouse, 4);
  LoadStream.Read(fHome, 4); //Substitute it with reference on SyncLoad
  LoadStream.Read(InShip, 4); //Substitute it with reference on SyncLoad
  LoadStream.Read(fPositionF);
  LoadStream.Read(fVisible);
  LoadStream.Read(fIsDead);
  LoadStream.Read(fKillASAP);
  LoadStream.Read(fKillASAPShowAnimation);
  LoadStream.Read(IsExchanging);
  LoadStream.Read(AnimStep);
  LoadStream.Read(fDirection);
  LoadStream.Read(fPositionRound);
  LoadStream.Read(fPositionPrev);
  LoadStream.Read(fPositionNext);
  LoadStream.Read(fDismissASAP);
  LoadStream.Read(Dismissable);
  LoadStream.Read(fKilledBy, SizeOf(fKilledBy));
  LoadStream.Read(fLastTimeTrySetActionWalk);

  LoadStream.Read(fAttack);
  LoadStream.Read(fAttackHorse);
  LoadStream.Read(fDefence);
  LoadStream.Read(fSight);
  LoadStream.Read(fHitPointsMax);
  LoadStream.Read(fSpeed);
  LoadStream.Read(fProjectileDefence);

  UpdateSpeed;
  LoadStream.Read(InstantKill);
  LoadStream.Read(IsHero);
  LoadStream.Read(fConditionPace);
  LoadStream.Read(BootsAdded);
  LoadStream.Read(fNeverHungry);
  LoadStream.Read(FlagColor);
  LoadStream.Read(BlockWalking);
  LoadStream.Read(Immortal);
  LoadStream.ReadData(fSpecialEffect);
end;


procedure TKMUnit.SyncLoad;
begin
  if fTask <> nil then
    fTask.SyncLoad;

  if fAction <> nil then
    fAction.SyncLoad;

  fHome := gHands.GetHouseByUID(Integer(fHome));
  fInHouse := gHands.GetHouseByUID(Integer(fInHouse));
  InShip := Pointer(gHands.GetUnitByUID(Integer(InShip)));

  //Create last so it can initialise with loaded (and sync-loaded) values
  fVisual := TKMUnitVisual.Create(Self);
end;

procedure TKMUnit.UpdateSpeed;
var
  A : Single;
begin
    A := fSpeed;

    fStepsPerTile          := Round(1    / A);
    fStepsPerTileDiag      := Round(1.41 / A);
    fStepsPerTileStorm     := Round(1    / (A * 1.5));
    fStepsPerTileStormDiag := Round(1.41 / (A * 1.5));

end;

function TKMUnit.ConvertSpeed(aSpeed : Integer; aIsDiag: Boolean) : Single;
begin
  if aSpeed = 0 then
    Result := 0
  else
    Result := 1 / ((1 + Byte(aIsDiag) * 0.41) / (aSpeed/240));
end;


function TKMUnit.GetEffectiveSpeed(aMovementType: TKMUnitMoveType): Single;
begin
  case aMovementType of
    umtWalk:      Result := 1 / fStepsPerTile;
    umtWalkDiag:  Result := 1 / fStepsPerTileDiag;
    umtStorm:     Result := 1 / fStepsPerTileStorm;
    umtStormDiag: Result := 1 / fStepsPerTileStormDiag;
  else
    raise Exception.Create('Unexpected type');
  end;
end;


function TKMUnit.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
var addSpeed : Integer;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtWalkDiag)
  else
    Result := GetEffectiveSpeed(umtWalk);
  //Exit;
  addSpeed := 0;
  case gTerrain.GetRoadType(fPositionRound) of
    rtWooden: If gHands[Owner].BuildDevUnlocked(6) then addSpeed := -1;
    rtStone : addSpeed := -2;
    rtClay: addSpeed := -4;
    rtExclusive: addSpeed := -10;
  end;
  Inc(addSpeed, -6 * byte(fSpecialEffect.EffectType = uetSpeedUp));
  Result := Result - ConvertSpeed(addSpeed, aIsDiag);

  //It needs 10 seconds to "hide the bow". Slow down the speed so it make Archer less OP
  {if fType = utArcher then
    if gGameParams.Tick < (TKMUnitWarrior(self).LastShootTime + 25) then
      Result := Result / 4
    else
    if gGameParams.Tick < (TKMUnitWarrior(self).LastShootTime + 50) then
      Result := Result / 3
    else
    if gGameParams.Tick < (TKMUnitWarrior(self).LastShootTime + 75) then
      Result := Result / 2.2
    else
    if gGameParams.Tick < (TKMUnitWarrior(self).LastShootTime + 100) then
      Result := Result / 1.5;}
end;


function TKMUnit.GetEffectiveStormSpeed(aIsDiag: Boolean): Single;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtStormDiag)
  else
    Result := GetEffectiveSpeed(umtStorm);
end;

procedure TKMUnit.TrainInHouse(aSchool: TKMHouseSchool);
begin
  fTask := TKMTaskSelfTrain.Create(Self, aSchool);
end;


// Erase everything related to unit status to exclude it from being accessed by anything but the old pointers
procedure TKMUnit.CloseUnit(aRemoveTileUsage: Boolean = True);
begin
  fHome.RemoveWorker(self);
  //fHome.SetWorker(nil);
  if fHome <> nil then
  begin
    //fHome.SetState(hstIdle);
    //fHome.SetState(hstEmpty);
    if fHome is TKMHouseProdThatch then
      TKMHouseProdThatch(fHome).ClearPoints;
  end;

  if InShip <> nil then
  begin
    TKMUnitWarriorShip(InShip).RemoveUnit(self, true);
  end;

  gHands.CleanUpHousePointer(fHome);

  if aRemoveTileUsage
    and (gTerrain.Land^[PositionNext.Y, PositionNext.X].IsUnit = Self) then //remove lock only if it was made by this unit
    gTerrain.UnitRem(fPositionNext); //Must happen before we nil NextPosition

  fIsDead       := True;
  fThought      := thNone;
  fPositionF    := KMPOINTF_ZERO;
  fPositionRound := KMPOINT_ZERO;
  fPositionPrev := fPositionRound;
  fPositionNext := fPositionRound;
  Owner        := HAND_NONE;
  //Do not reset the unit type when they die as we still need to know during Load
  //fUnitType     := utNone;
  fDirection    := dirNA;
  fVisible      := False;
  fCondition    := 0;
  AnimStep      := 0;
  FreeAndNil(fAction);
  FreeAndNil(fTask);

  Assert(gMySpectator.Selected <> Self,
    'Removed units should be flushed from UI earlier in TaskDie or never appear there when training cancelled or alike');
end;


procedure TKMUnit.DismissStarted;
begin
  fDismissInProgress := True;
end;


procedure TKMUnit.Dismiss;
begin
  if not Dismissable or IsDeadOrDying then
    Exit;

  fDismissInProgress := False;

  if (fAction is TKMUnitActionWalkTo) and TKMUnitActionWalkTo(fAction).DoingExchange then
  begin
    fDismissASAP := True; //Unit will be dismissed ASAP, when unit is ready for it
    Exit;
  end;

  DoDismiss;
end;


procedure TKMUnit.DismissCancel;
begin
  fDismissInProgress := False; //remove fDismissInProgress mark, which is used only in UI

  if not IsDismissing then Exit;

  fThought := thNone; //Reset thought
  fDismissASAP := False;

  if (fAction is TKMUnitActionWalkTo)
    and not TKMUnitActionWalkTo(fAction).DoingExchange then
  begin
    AbandonWalk;
  end else
  if fAction.CanBeInterrupted then
  begin
    SetActionLockedStay(0, uaWalk);
    if fTask = nil then
      SetActionStay(5, uaWalk);
  end;

  if fTask <> nil then
    FreeAndNil(fTask);
end;


procedure TKMUnit.DoDismiss;

  procedure TryCreateDismissTask;
  begin
    FreeAndNil(fTask);
    fTask := TKMTaskDismiss.Create(Self); //Will create empty locked stay action
    if TKMTaskDismiss(fTask).ShouldBeCancelled then
      FreeAndNil(fTask);
  end;

begin
  //We can update existing Walk action with minimum changes
  if (fAction is TKMUnitActionWalkTo)
    and not TKMUnitActionWalkTo(fAction).DoingExchange then
  begin
    AbandonWalk;
    TryCreateDismissTask;
  end else
  if fAction.CanBeInterrupted then
  begin
    SetActionLockedStay(0, uaWalk);
    TryCreateDismissTask;
    if fTask = nil then
      SetActionStay(5, uaWalk);
  end else
    fDismissASAP := True; // Delay Dismiss for 1 more tick, until action interrupt could be possible
end;


{Call this procedure to properly kill a unit. ForceDelay means we always use KillASAP}
//killing a unit is done in 3 steps
// Kill - release all unit-specific tasks
// TTaskDie - perform dying animation
// CloseUnit - erase all unit data and hide it from further access
procedure TKMUnit.Kill(aFrom: TKMHandID; aShowAnimation, aForceDelay: Boolean);
begin
  //Don't kill unit if it's already dying
  if IsDeadOrDying then
    Exit;

  // Don't allow to kill invulnerable units (by any means)
  if HitPointsInvulnerable then
    Exit;

  //From this moment onwards the unit is guaranteed to die (no way to avoid it even with KillASAP), so
  //signal to our owner that we have died (doesn't have to be assigned since f.e. animals don't use it)
  //This must be called before actually killing the unit because gScriptEvets needs to access it
  //and script is not allowed to touch dead/dying/KillASAP units

  // Remember who killed us.
  // If worker is killed we want to tell the script, that house area, which he was digging was destroyed by same source (enemy or hunger)
  // Should be saved in game save as well, since could be used later during our death (f.e. on next tick if fKillASAP)
  fKilledBy := aFrom;

  //after killing someone we get some by-products
  if aFrom >= 0 then
  with gHands[aFrom] do
  begin
    VirtualWareTake('vtLeatherSheet', -1);
    VirtualWareTake('vtCoin', -1);

    If UnitType in SPECIAL_UNITS then
      AddFestivalPoints(fptWarfare, 15)
    else
    If UnitType in SIEGE_MACHINES then
      AddFestivalPoints(fptWarfare, 9)
    else
    If UnitType in WARRIORS_IRON then
      AddFestivalPoints(fptWarfare, 3)
    else
    If UnitType in WARRIORS_LEATHER then
      AddFestivalPoints(fptWarfare, 2)
    else AddFestivalPoints(fptWarfare, 1);
  end;

  if Assigned(OnUnitDied) then
    OnUnitDied(Self);

  // Wait till units exchange (1 tick) and then do the killing
  if aForceDelay
    or ((fAction is TKMUnitActionWalkTo) and TKMUnitActionWalkTo(fAction).DoingExchange) then
  begin
    fKillASAP := True; //Unit will be killed ASAP, when unit is ready for it
    fKillASAPShowAnimation := aShowAnimation;
    Exit;
  end;
  
  //if we were delivering food to warrior then reset RequestFood
  if fTask is TKMTaskDeliver then
    if TKMTaskDeliver(fTask).DeliverKind = dkToUnit then
      if (TKMTaskDeliver(fTask).ToUnit is TKMUnitWarrior) then    
      begin

        if TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).RequestedFood then
        begin
          TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).RequestedFood := false;
          //TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).OrderFood;
        end;

        if TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).RequestedAmmo then
        begin
          TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).RequestedAmmo := false;
          //TKMUnitWarrior(TKMTaskDeliver(fTask).ToUnit).OrderAmmo;
        end;

      end;
                    
  // If we didn't exit above, we are safe to do the kill now (no delay from KillASAP required)
  DoKill(aShowAnimation);
end;


procedure TKMUnit.DoKill(aShowAnimation: Boolean);
begin
  fThought := thNone; //Reset thought
  SetAction(nil); //Dispose of current action (TTaskDie will set it to LockedStay)
  FreeAndNil(fTask); //Should be overriden to dispose of Task-specific items
  if aShowAnimation then
  begin
    //gSpecAnim.AddUnitDeath(Spec.UnitAnim[uaDie, Direction], PositionF + KMPointF(0.5, 0.5), PositionF + KMPointF(0.5, 0.5));
    {aShowAnimation := false;
    if gMySpectator.FogOfWar.CheckTileRevelation(Position.X, Position.Y) >= 255 then
    begin
      if self is TKMUnitWarrior then
        gSoundPlayer.PlayWarrior(UnitType, spDeath, PositionF)
      else
        gSoundPlayer.PlayCitizen(UnitType, spDeath, PositionF);
    end;}
  end;
  fTask := TKMTaskDie.Create(Self, aShowAnimation);
end;


//procedure TKMUnit.SetOwner(aOwner: TKMHandID);
//begin
//  Owner := aOwner;
//end;


procedure TKMUnit.OwnerUpdate(aOwner: TKMHandID; aMoveToNewOwner: Boolean = False);
begin
  if aMoveToNewOwner and (Owner <> aOwner) then
  begin
    Assert(gGameParams.Mode = gmMapEd); // Allow to move existing Unit directly only in MapEd
    gHands[Owner].Units.DeleteUnitFromList(Self);
    gHands[aOwner].Units.AddUnitToList(Self);
  end;
  Owner := aOwner;
end;


procedure TKMUnit.SetUnitPosition(const aPos: TKMPoint);
var
  newPos: Boolean;
begin
  //This is only used by the map editor, set all positions to aPos
  Assert(gGameParams.IsMapEditor);

  if not gTerrain.CanPlaceUnit(aPos, UnitType) then Exit;

  newPos := fPositionRound <> aPos;

  gTerrain.UnitRem(fPositionRound);
  fPositionRound := aPos;
  fPositionNext := aPos;
  fPositionPrev := aPos;
  fPositionF := KMPointF(aPos);
  gTerrain.UnitAdd(fPositionRound, Self);

  if newPos then
    gGame.MapEditor.History.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_MOVE_SMTH],
                                                           [gRes.Units[UnitType].GUIName, aPos.ToString]));
end;

function TKMUnit.SetUnitPositionFromShip(const aPos: TKMPoint) : Boolean;
begin
  Result := false;
  if not gTerrain.CanPlaceUnit(aPos, UnitType) then Exit;
  if Visible then
    gTerrain.UnitRem(fPositionRound);
  fPositionRound := aPos;
  fPositionNext := aPos;
  fPositionPrev := aPos;

  fPositionF := KMPointF(fPositionRound);
  //gTerrain.UnitAdd(aPos, Self);
  Result := true;

end;

function TKMUnit.CanAccessHome: Boolean;
begin
  Result := (fHome = nil) or CanWalkTo(fHome.PointBelowEntrance, tpWalk, 0);
end;


function TKMUnit.GetActionText: UnicodeString;
begin
  Result := fAction.GetExplanation;
end;


procedure TKMUnit.HitPointsDecrease(aAmount: Byte; aAttacker: TKMUnit);
begin
  If aAmount = 0 then
    Exit;
  //Assert(aAmount > 0, '0 damage should be handled outside so not to reset HPCounter');

  If IsDeadOrDying then
    Exit;
  if Immortal then
    Exit;
  //When we are first hit reset the counter
  if fHitPoints = HitPointsMax then
    fHitPointCounter := 1;

  fHitPoints := Max(fHitPoints - aAmount, 0);

  gHands[Owner].AI.UnitHPDecreaseNotification(Self, aAttacker);

  //Make sure to kill only once
  if (fHitPoints = 0) and not IsDeadOrDying then
    if aAttacker <> nil then
      Kill(aAttacker.Owner, True, False)
    else
      Kill(HAND_NONE, True, False)
end;

procedure TKMUnit.HitPointsDecrease(aAttack, aDamage : Integer; aAttacker : TKMUnit; aInstantKill : Boolean = false);
var att : Integer;
begin
  if Immortal then
    Exit;
  if aInstantKill then
    HitPointsDecrease(fHitPointsMax, aAttacker)
  else
  begin
    att := aAttack div Math.Max(Defence, 1);

    If (att >= KaMRandom(101, 'TKMUnit.HitPointsDecrease')) then
      HitPointsDecrease(aDamage, aAttacker);

  end;



end;

procedure TKMUnit.HitPointsChangeFromScript(aAmount: Integer);
begin
  fHitPoints := EnsureRange(fHitPoints + aAmount, 0, GetHitPointsMax);
  if (fHitPoints = 0)
  and (not IsDeadOrDying) then
    Kill(HAND_NONE, True, False);
end;

procedure TKMUnit.DoHitFrom(aAttacker: TKMUnit);

procedure MakeSound(IsHit: Boolean);
var
  //Battlecry is the most noticable random sound, we would like to repeat it exactly the same in each replay (?)
  makeBattleCry: Boolean;
begin
  //Randomly make a battle cry. KaMRandom must always happen regardless of tile revelation
  makeBattleCry := KaMRandom(20, 'TKMUnitActionFight.MakeSound') = 0;

  //Do not play sounds if unit is invisible to gMySpectator
  //We should not use KaMRandom below this line because sound playback depends on FOW and is individual for each player
  if gMySpectator.FogOfWar.CheckTileRevelation(aAttacker.Position.X, aAttacker.Position.Y) < 255 then Exit;

  if makeBattleCry then
    gSoundPlayer.PlayWarrior(aAttacker.UnitType, spBattleCry, aAttacker.PositionF);

  case aAttacker.UnitType of
    utGolem,
    utBallista,
    utCatapult,
    utCrossbowman: gSoundPlayer.Play(sfxCrossbowDraw, aAttacker.PositionF); // Aiming
    utArcher,
    utBowman:     gSoundPlayer.Play(sfxBowDraw,      aAttacker.PositionF); // Aiming
    utRogue:  gSoundPlayer.Play(sfxSlingerShoot, aAttacker.PositionF);
    else           begin
                     if IsHit then
                       gSoundPlayer.Play(MeleeSoundsHit[Random(Length(MeleeSoundsHit))], aAttacker.PositionF)
                     else
                       gSoundPlayer.Play(MeleeSoundsMiss[Random(Length(MeleeSoundsMiss))], aAttacker.PositionF);
                   end;
  end;
end;
var
  isHit: Boolean;
  damage: Word;
begin
  if aAttacker = nil then
    Exit;
  if aAttacker.IsDeadOrDying then
    Exit;
  if aAttacker.Owner = self.Owner then
    Exit;

  //gScriptEvents.ProcUnitHit(self, aAttacker);
  self.SetHitTime;//set hittime no matter if it does damage or not. He is in fight.

  if self is TKMUnitWarriorSpy then
    TKMUnitWarriorSpy(self).SetAttackedTime;//Spy can only attack house

  if aAttacker.InstantKill then
  begin
    isHit := true;
    self.HitPointsDecrease(self.HitPointsMax, aAttacker);
  end else
  if (aAttacker.Attack > 300) or (aAttacker.UnitType = utTrainedWolf) then
  begin
    isHit := true;
    self.HitPointsDecrease(TKMUnitWarrior(aAttacker).DamageUnits, aAttacker);
  end else
  begin
    //Base damage is the unit attack strength + AttackHorse if the enemy is mounted
    damage := aAttacker.Attack;
    if (self.UnitType in [low(UNIT_TO_GROUP_TYPE) .. high(UNIT_TO_GROUP_TYPE)]) and (UNIT_TO_GROUP_TYPE[self.UnitType] = gtMounted) then
      damage := damage + aAttacker.AttackHorse;
    if self.UnitType <> utShieldBearer then
      damage := damage * (GetDirModifier(aAttacker.Direction, self.Direction) + 1); // Direction modifier
    //Defence modifier
    //If (UNIT_TO_GROUP_TYPE[aAttacker.UnitType] <> gtWreckers) and (aUnit.UnitType <> utPikeMachine) then
    If not TKMUnitWarrior(aAttacker).CanIgnoreDefence(self) then
      damage := damage div Math.max(self.Defence, 1); //Not needed, but animals have 0 defence

    isHit := (self.Defence <= 0) or (damage >= KaMRandom(101, 'TKMUnitActionFight.ExecuteProcessMelee')); //Damage is a % chance to hit
    if isHit then
      if TKMUnitWarrior(aAttacker).DamageUnits > 0 then
        self.HitPointsDecrease(TKMUnitWarrior(aAttacker).DamageUnits, aAttacker);
  end;
  MakeSound(isHit); //Different sounds for hit and for miss

end;


function TKMUnit.GetHitPointsMax: Byte;
begin
  //fHitPointsMax := gRes.Units[fType].HitPoints;
  Result := fHitPointsMax;
end;

procedure TKMUnit.SetHitTime;
begin
  If fSpecialEffect.EffectType <> uetHealing then
    fHitPointCounter := 1;
end;

function TKMUnit.GetInHouse: TKMHouse;
begin
  if Self = nil then Exit(nil);
  
  Result := fInHouse;
end;


function TKMUnit.GetInstance: TKMUnit;
begin
  Result := Self;
end;


procedure TKMUnit.CancelTask(aFreeTaskObject: Boolean = True);
begin
  if (fTask <> nil)
    and (fAction is TKMUnitActionWalkTo)
    and not TKMUnitActionWalkTo(Action).DoingExchange then
    AbandonWalk;
  if aFreeTaskObject then
    FreeAndNil(fTask)
  else
    fTask := nil;
end;

function TKMUnit.GetProjectileDefence(isBolt: Boolean): Single;
begin
  if isBolt and not (fType in [utPaladin, utRam]) then
    Result := Defence + Self.ProjectilesDefence / 4
  else
    Result := Defence + Self.ProjectilesDefence;

  If (UnitType = utSwordFighter) and gHands[Owner].ArmyDevUnlocked(8) then
    Result := Result + 1;

  If (UnitType in [utRam, utWoodenWall]) and gHands[Owner].ArmyDevUnlocked(29) then
    Result := Result + 3;
end;

procedure TKMUnit.SetSpeed(aValue: SmallInt; addTo: Boolean = false);
var A : Single;
begin
  if addTo then
  begin
    A := fSpeed * 240;

    fSpeed := (A + aValue) / 240;
    UpdateSpeed;

  end else
  begin
    fSpeed := aValue / 240;
    UpdateSpeed;
  end;
end;

function TKMUnit.GetStats : TKMUnitStats;
begin
  Result.GroupID := -1;
  Result.UnitType := UnitType;
  Result.X := Position.X;
  Result.Y := Position.Y;
  Result.Owner := Owner;
  Result.Attack := fAttack;
  Result.Defence := fDefence;
  Result.AttackHorse := fAttackHorse;
  Result.HP := fHitPoints;
  Result.MaxHP := fHitPointsMax;
  Result.Speed := Round(fSpeed * 240);
  Result.Sight := fSight;
  Result.Condition := Condition;
  Result.DamageHouse := 0;
  Result.DamageUnits := 0;
  Result.Ammo := 0;
end;

procedure TKMUnit.SetStats(aStats: TKMUnitStats);
begin
  If aStats.Attack > 0 then
    fAttack := aStats.Attack;
  If aStats.Defence > 0 then
    fDefence := aStats.Defence;
  If aStats.AttackHorse > 0 then
    fAttackHorse := aStats.AttackHorse;
  If aStats.HP > 0 then
    fHitPoints := aStats.HP;
  If aStats.MaxHP > 0 then
    fHitPointsMax := aStats.MaxHP;
  If aStats.Speed > 0 then
    SetSpeed(aStats.Speed);
  If aStats.Sight > 0 then
    fSight := aStats.Sight;
  If aStats.Condition > 0 then
    Condition := aStats.Condition;
end;

procedure TKMUnit.AssignToShip(aShip : Pointer);
begin
  CancelTask;
  if fTask = nil then
    fTask := TKMTaskAssignToShip.Create(self, TKMUnitWarriorShip(aShip));
end;

function TKMUnit.IsAssigningToShip: Boolean;
begin
  Result := fTask is TKMTaskAssignToShip;
end;
procedure TKMUnit.UnloadFromShip(aShip : Pointer);
begin
  CancelTask;
  if fTask = nil then
    fTask := TKMTaskUnloadFromShip.Create(self, TKMUnitWarriorShip(aShip));
end;

function TKMUnit.IsUnloadingFromShip : Boolean;
begin
  Result := fTask is TKMTaskUnloadFromShip;
end;

procedure TKMUnit.GoToStore(aLocTo: TKMPoint);
begin
  CancelTask;
  if fTask = nil then
    fTask := TKMTaskGoToStore.Create(self, aLocTo);
end;
procedure TKMUnit.GoToPearl(aLocTo: TKMPoint);
begin
  CancelTask;
  if fTask = nil then
    fTask := TKMTaskGoToPearl.Create(self, aLocTo);
end;

procedure TKMUnit.GoMakePearlRally(aPearl : TKMHouse);
begin
  CancelTask;
  if fTask = nil then
    fTask := TKMTaskPearlRally.Create(self, TKMHousePearl(aPearl));
end;

procedure TKMUnit.SetEffect(aType: TKMUnitEffectType; aDuration: Word);
begin
  fSpecialEffect.EffectType := aType;
  fSpecialEffect.Duration := (aDuration div 10) * 10;
end;

procedure TKMUnit.SetThought(aThought: TKMUnitThought);
begin
  // Only allow to set Dismiss thought if unit is deathly hungry
  // other thoughs are not allowed to set
  if (fThought <> thDeath) or (aThought = thDismiss) then
  begin
    fThought := aThought;

    // Update Thoughts if we reset he thought (f.e. on task destruction)
    if fThought = thNone then
      UpdateThoughts;
  end;
end;


procedure TKMUnit.SetInHouse(aInHouse: TKMHouse);
begin
  gHands.CleanUpHousePointer(fInHouse);
  if aInHouse <> nil then
    fInHouse := aInHouse.GetPointer;
end;


function TKMUnit.HitTest(X,Y: Integer; const UT: TKMUnitType = utAny): Boolean;
begin
  Result := (X = fPositionRound.X) and //Comparing X,Y to CurrentPosition separately, cos they can be negative numbers
            (Y = fPositionRound.Y) and
            ((fType = UT) or (UT = utAny));
end;


//As long as we only ever set PrevPos to NextPos and do so everytime before NextPos changes,
//there can be no problems (as were occurring in GetSlide)
//This procedure ensures that these values always get updated correctly so we don't get a problem
//where GetLength(PrevPosition,NextPosition) > sqrt(2)
procedure TKMUnit.SetPositionNext(const aLoc: TKMPoint);
begin
  fPositionPrev := PositionNext;
  fPositionNext := aLoc;
end;


procedure TKMUnit.SetPositionF(const aPositionF: TKMPointF);
begin
  fPositionF := aPositionF;
end;


function TKMUnit.GetPositionForDisplayF: TKMPointF;
begin
  Result := fPositionF;
end;


procedure TKMUnit.SetPositionRound(const aLoc: TKMPoint);
begin
  if {not gGameApp.DynamicFOWEnabled
  and }(Owner <> HAND_ANIMAL)
  and (fPositionRound <> aLoc) then  //Update FOW only for new loc
  begin
    gHands.RevealForTeam(Owner, aLoc, fSight, FOG_OF_WAR_MAX, frtUnit);
    //show boots dust only at new pos
    if BootsAdded then
      gSpecAnim.Add(gRes.Units.BootsAnim, PositionF, 1, rxUnits, true, -0.6);
  end;



  fPositionRound := aLoc;
end;


procedure TKMUnit.SetPositionRoundByPosF;
begin
  // Choose between prev and next position
  // Do not do simple Round of fPositionF, since it could be rounded to a wrong tile, not prevPos and not nextPos
  if KMLengthSqr(fPositionPrev, fPositionF) < KMLengthSqr(fPositionNext, fPositionF) then
    SetPositionRound(fPositionPrev)
  else
    SetPositionRound(fPositionNext);
end;


//Only ClearUnit can set fDirection to NA, no other circumstances it is allowed
procedure TKMUnit.SetDirection(aValue: TKMDirection);
begin
  Assert(aValue <> dirNA);
  fDirection := aValue;

  //if fCurrentAction is TUnitActionStay then
  //  AnimStep := UnitStillFrames[fDirection];
end;

function TKMUnit.GetUnitSpec: TKMUnitSpec;
begin
  Result := gRes.Units[UnitType];
end;

function TKMUnit.IsSelected : Boolean;
begin
  Result := gMySpectator.Selected = self;
end;
//Assign the following Action to unit and set AnimStep
procedure TKMUnit.SetAction(aAction: TKMUnitAction; aStep: Integer = 0);
begin
  AnimStep := aStep;
  if aAction = nil then
  begin
    FreeAndNil(fAction);
    Exit;
  end;
  if not gRes.Units[fType].SupportsAction(aAction.ActionType) then
  begin
    FreeAndNil(aAction);
    raise Exception.Create('Unit ' + gRes.Units[UnitType].GUIName + ' was asked to do unsupported action');
  end;
  if fAction <> aAction then
  begin
    fAction.Free;
    fAction := aAction;
  end;
end;


procedure TKMUnit.SetActionGoIn(aAction: TKMUnitActionType; aGoDir: TKMGoInDirection; aHouse: TKMHouse; aIsTrained : Boolean = false);
begin
  SetAction(TKMUnitActionGoInOut.Create(Self, aAction, aGoDir, aHouse, aIsTrained), AnimStep);

end;


procedure TKMUnit.SetActionStay(aTimeToStay: Integer; aAction: TKMUnitActionType; aStayStill: Boolean = True; aStillFrame: Byte = 0; aStep: Integer = 0);
begin
  //When standing still in walk, use default frame
  if (aAction = uaWalk) and aStayStill then
  begin
    aStillFrame := UNIT_STILL_FRAMES[Direction];
    aStep := UNIT_STILL_FRAMES[Direction];
  end;
  SetAction(TKMUnitActionStay.Create(Self, aTimeToStay, aAction, aStayStill, aStillFrame, False), aStep);
end;


procedure TKMUnit.SetActionStorm(aRow: Integer);
begin
  SetAction(TKMUnitActionStormAttack.Create(Self, uaWalk, aRow), 0); //Action is uaWalk for that is the inital one
end;


procedure TKMUnit.SetActionSteer;
begin
  SetAction(TKMUnitActionSteer.Create(Self, uaWalk, True), 0);
end;


//Same as above but we will ignore get-out-of-the-way (push) requests from interaction system
procedure TKMUnit.SetActionLockedStay(aTimeToStay: Integer; aAction: TKMUnitActionType; aStayStill: Boolean = True;
                                      aStillFrame: Byte = 0; aStep: Integer = 0);
begin
  //When standing still in walk, use default frame
  if (aAction = uaWalk) and aStayStill then
  begin
    aStillFrame := UNIT_STILL_FRAMES[Direction];
    aStep := UNIT_STILL_FRAMES[Direction];
  end;
  SetAction(TKMUnitActionStay.Create(Self, aTimeToStay, aAction, aStayStill, aStillFrame, True), aStep);
end;


procedure TKMUnit.UpdateLastTimeTrySetActionWalk;
begin
  fLastTimeTrySetActionWalk := fTicker + KaMRandom(10, 'TKMUnit.TrySetActionWalk'); //Add random component to try set acion for group with a small delay
end;


//Try to set action walk for unit
//Used for warriors when they try to update route when attacking house and their destination loc is locked, so they try to avoid it
function TKMUnit.TrySetActionWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType; aDistance: Single;
                                   aTargetUnit: TKMUnit; aTargetHouse: TKMHouse;
                                   aAvoidLockedByMovementCost: Boolean = True;
                                   aSilent: Boolean = True): Boolean; //Silent by default, as we consider fail could happen
var
  newAction: TKMUnitActionWalkTo;
begin
  Result := False;
  //Don't do tries too often
  if fTicker <= fLastTimeTrySetActionWalk + TRY_SET_ACTION_WALK_FREQ then
    Exit;

  UpdateLastTimeTrySetActionWalk;

  IF BlockWalking then
    Exit(false);

  newAction := TKMUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, False, aTargetUnit, aTargetHouse,
                                          tpNone, [], True, aAvoidLockedByMovementCost, aSilent);

  //Update action only if route was built, otherwise just keep using previous action
  if newAction.RouteBuilt then
  begin
    SetAction(newAction);
    Exit(True);
  end
  else
  begin
    newAction.Free;
    Exit(False);
  end;
end;


//WalkTo action with exact options (retranslated from WalkTo if Obstcale met)
procedure TKMUnit.SetActionWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType; aDistance: Single;
                                aTargetUnit: TKMUnit; aTargetHouse: TKMHouse; aAvoidLockedByMovementCost: Boolean = True);
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('');
  if BlockWalking then
    Exit;

  SetAction(TKMUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, False, aTargetUnit, aTargetHouse,
                                       tpNone, [], True, aAvoidLockedByMovementCost));
  UpdateLastTimeTrySetActionWalk;
end;


//Approach house
procedure TKMUnit.SetActionWalkToHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('');
  if BlockWalking then
    Exit;


  SetAction(TKMUnitActionWalkTo.Create( Self,               //Who's walking
                                      //Target position is the closest cell to our current position (only used for estimating in path finding)
                                      aHouse.GetClosestCell(Self.Position),
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      False,              //If we were pushed
                                      nil,                //Unit
                                      aHouse              //House
                                      ));
end;


procedure TKMUnit.SetActionWalkFromHouse(aHouse: TKMHouse; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
var locB : TKMPoint;
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('');
  if BlockWalking then
    Exit;
  gTerrain.GetClosestWalkable(Position, aHouse.Entrance, aDistance, aDistance + 2, locB);

  If locB = KMPOINT_ZERO then
    SetActionStay(20, aActionType)
  else
    SetAction(TKMUnitActionWalkTo.Create(Self, locB, aActionType, 1, False, nil, nil), 0);
end;

//Approach unit
procedure TKMUnit.SetActionWalkToUnit(aUnit: TKMUnit; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
   raise Exception.Create('');
  Assert(aDistance >= 1, 'Should not walk to units place');
  if BlockWalking then
    Exit;

  SetAction(TKMUnitActionWalkTo.Create( Self,               //Who's walking
                                      aUnit.fPositionRound,//Target position
                                      aActionType,        //
                                      aDistance,          //Proximity
                                      False,              //If we were pushed
                                      aUnit,              //Unit
                                      nil                 //House
                                      ));
end;


procedure TKMUnit.SetCondition(aValue: Integer);
begin
  if NeverHungry or Immortal then
    Exit;

  fCondition := EnsureRange(aValue, 0, UNIT_MAX_CONDITION);
end;


procedure TKMUnit.SetActionWalkFromUnit(aUnit: TKMUnit; aDistance: Single; aActionType: TKMUnitActionType = uaWalk);
var locB : TKMPoint;
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('');
  if BlockWalking then
    Exit;
  If gTerrain.GetClosestWalkable(Position, aUnit.Position, aDistance, aDistance + 2, locB) then
    SetAction(TKMUnitActionWalkTo.Create(Self, locB, aActionType, 1, False, nil, nil), 0)
  else
    SetActionStay(20, aActionType);
end;


//Walk to spot or its neighbourhood
procedure TKMUnit.SetActionWalkToSpot(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk; aDistance: Single = 0; aAnimStep: Integer = 0);
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('Interrupting unabandonable Walk action');
  if BlockWalking then
    Exit;
  SetAction(TKMUnitActionWalkTo.Create(Self, aLocB, aActionType, aDistance, False, nil, nil), aAnimStep);
end;

procedure TKMUnit.SetActionWalkFromSpot(const aLoc: TKMPoint; aDistanceMin, aDistanceMaX: Single; aActionType: TKMUnitActionType = uaWalk);
var locB : TKMPoint;
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('');
  if BlockWalking then
    Exit;
  If gTerrain.GetClosestWalkable(Position, aLoc, aDistanceMin, aDistanceMaX, locB) then
    SetAction(TKMUnitActionWalkTo.Create(Self, locB, aActionType, 2, False, nil, nil), 0)
  else
    SetActionStay(20, aActionType);
end;

procedure TKMUnit.SetActionWalkToRoad(aActionType: TKMUnitActionType = uaWalk; aDistance: Single = 0;
                                             aTargetPassability: TKMTerrainPassability = tpWalkRoad; aTargetWalkConnectSet: TKMByteSet = []);
begin
  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise Exception.Create('Interrupting unabandonable Walk action');
  if BlockWalking then
    Exit;
  SetAction(TKMUnitActionWalkTo.Create(Self, KMPOINT_INVALID_TILE, aActionType, aDistance, False, nil, nil,
                                       aTargetPassability, aTargetWalkConnectSet));
end;


//We were pushed (walk to spot with wider Passability)
procedure TKMUnit.SetActionWalkPushed(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk);
begin
  //1. Only idle units can be pushed, for they are low priority to busy units
  //2. If unit can't get away it will re-push itself once again
  Assert(((Action is TKMUnitActionStay) and (not Action.Locked)) or
         ((Action is TKMUnitActionWalkTo) and TKMUnitActionWalkTo(Action).CanAbandonExternal));
  if BlockWalking then
    Exit;

  SetAction(TKMUnitActionWalkTo.Create(Self, aLocB, aActionType, 0, True, nil, nil));
  //Once pushed, unit will try to walk away, if he bumps into more units he will
  //
end;


procedure TKMUnit.SetActionAbandonWalk(const aLocB: TKMPoint; aActionType: TKMUnitActionType = uaWalk);
var
  tempVertexOccupied: TKMPoint;
begin
  if Action is TKMUnitActionWalkTo then
  begin
    tempVertexOccupied := TKMUnitActionWalkTo(Action).fVertexOccupied;
    TKMUnitActionWalkTo(Action).fVertexOccupied := KMPOINT_ZERO; //So it doesn't try to DecVertex on destroy (now it's AbandonWalk's responsibility)
  end
  else
    tempVertexOccupied := KMPOINT_ZERO;
  if BlockWalking then
    Exit;

  SetAction(TKMUnitActionAbandonWalk.Create(Self, aLocB, tempVertexOccupied, aActionType), AnimStep); //Use the current animation step, to ensure smooth transition
end;


procedure TKMUnit.AbandonWalk;
begin
  if Action is TKMUnitActionWalkTo then
    SetActionAbandonWalk(PositionNext, uaWalk)
  else
    SetActionLockedStay(0, uaWalk); //Error
end;


//Used by barracks when creating a recruit inside
procedure TKMUnit.SetHome(aHome: TKMHouse);
begin
  gHands.CleanUpHousePointer(fHome);
  fHome := aHome.GetPointer;
end;


//Specific unit desired passability may depend on several factors
function TKMUnit.GetDesiredPassability: TKMTerrainPassability;
begin
  Result := gRes.Units[fType].DesiredPassability;

  //Delivery to unit
  if (fType = utSerf) then
    If (fTask is TKMTaskDeliver) then
    begin
      If (TKMTaskDeliver(fTask).DeliverKind in [dkToUnit, dkToWall]){ or (TKMUnitSerf(self).Carry = wtNone) }then
        Result := tpWalk;
    end;

  //Preparing house area
  if (fType in [utBuilder, utHouseBuilder]) and (fTask is TKMTaskBuildHouseArea)
  and TKMTaskBuildHouseArea(fTask).Digging
  then
    Result := tpWorker; //Special mode that allows us to walk on building sites

  //Miners at work need to go off roads
  if (fType in [utWoodcutter, utFarmer, utFisher, utStonemason])
  and (fTask is TKMTaskMining)
  then
    Result := tpWalk;
end;


procedure TKMUnit.Feed(Amount: Single);
begin
  fCondition := Math.min(fCondition + Round(Amount), UNIT_MAX_CONDITION);
end;


function TKMUnit.IsHungry: Boolean;
begin
  Result := fCondition < UNIT_MIN_CONDITION;
end;


function TKMUnit.IsAnimal: Boolean;
begin
  Result := False;
end;


//It's better not to start doing anything with dying units
function TKMUnit.IsDeadOrDying: Boolean;
begin
  Result := (self = nil) or fIsDead or fKillASAP or (fTask is TKMTaskDie);
end;


function TKMUnit.IsDismissing: Boolean;
begin
  Result := fDismissASAP or (fTask is TKMTaskDismiss) or (fTask is TKMTaskDismissWarrior);
end;


function TKMUnit.IsDismissAvailable: Boolean;
begin
  Result := (fTask = nil) or fTask.CouldBeCancelled;
end;


function TKMUnit.IsDismissCancelAvailable: Boolean;
begin
  Result := fDismissASAP
            or ((fTask is TKMTaskDismiss)
              and TKMTaskDismiss(fTask).CouldBeCancelled)
            or ((fTask is TKMTaskDismissWarrior)
              and TKMTaskDismissWarrior(fTask).CouldBeCancelled);
end;


function TKMUnit.CanWalkDiagonally(const aFrom, aTo: TKMPoint): Boolean;
begin
  Result := gTerrain.CanWalkDiagonally(aFrom, aTo.X, aTo.Y);
end;


function TKMUnit.CanWalkTo(const aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := gTerrain.RouteCanBeMade(Position, aTo, DesiredPassability, aDistance);
end;


function TKMUnit.CanWalkTo(const aTo: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean;
begin
  Result := gTerrain.RouteCanBeMade(Position, aTo, aPass, aDistance);
end;


function TKMUnit.CanWalkTo(const aFrom, aTo: TKMPoint; aDistance: Single): Boolean;
begin
  Result := gTerrain.RouteCanBeMade(aFrom, aTo, DesiredPassability, aDistance);
end;


function TKMUnit.CanWalkTo(const aFrom, aTo: TKMPoint; aPass: TKMTerrainPassability): Boolean;
begin
  Result := gTerrain.RouteCanBeMade(aFrom, aTo, aPass);
end;


function TKMUnit.CanWalkTo(const aFrom, aTo: TKMPoint; aPass: TKMTerrainPassability; aDistance: Single): Boolean;
begin
  Result := gTerrain.RouteCanBeMade(aFrom, aTo, aPass, aDistance);
end;


// Check if a route can be made to any tile around this house
function TKMUnit.CanWalkTo(const aFrom: TKMPoint; aHouse: TKMHouse; aPass: TKMTerrainPassability; aDistance: Single): Boolean;
var
  I: Integer;
  cells: TKMPointList;
begin
  Result := False;
  cells := TKMPointList.Create;
  try
    aHouse.GetListOfCellsWithin(cells);
    for I := 0 to cells.Count - 1 do
    if gTerrain.RouteCanBeMade(aFrom, cells[I], aPass, aDistance) then
      Exit(True);
  finally
    cells.Free;
  end;
end;


function TKMUnit.CanStepTo(X,Y: Integer; aPass: TKMTerrainPassability): Boolean;
begin
  Result := gTerrain.TileInMapCoords(X,Y)
        and (gTerrain.Land^[Y,X].IsUnit = nil)
        and (gTerrain.CheckPassability(KMPoint(X,Y), aPass))
        and not (gTerrain.AvoidTile(X, Y))
        and (not KMStepIsDiag(Position, KMPoint(X,Y)) //Only check vertex usage if the step is diagonal
             or (not gTerrain.HasVertexUnit(KMGetDiagVertex(Position, KMPoint(X,Y)))))
        and (gTerrain.CanWalkDiagonally(Position, X, Y));
end;


procedure TKMUnit.UpdateThoughts;
begin
  if (fThought in [thNone, thQuest]) and IsHungry then
    fThought := thEat;

  if (fCondition <= UNIT_MIN_CONDITION div 3) then
    fThought := thDeath;

  if (fThought in [thDeath, thEat]) and (fCondition > UNIT_MIN_CONDITION) then
    fThought := thNone;

  // Show dismiss thought even for almost dead units
  if (fTask is TKMTaskDismiss) then
    fThought := thDismiss;

  if (fTask is TKMTaskDie) then //Clear thought if we are in the process of dying
    fThought := thNone;
end;


//Return True if the unit has to be killed due to lack of space
function TKMUnit.UpdateVisibility: Boolean;
var
  newCurrPosition: TKMPoint;
  placedOnOccupiedTile: Boolean;
  wasTrained : Boolean;
begin
  Result := False;
  if fInHouse = nil then Exit; //There's nothing to update, we are always visible

  if fInHouse.IsDestroyed then //Someone has destroyed the house we were in
  begin
    fVisible := True;
    wasTrained := (Action is TKMUnitActionGoInOut) and TKMUnitActionGoInOut(Action).IsTrained;
    // If we are walking into/out of the house then don't set our position, ActionGoInOut will sort it out
    if not (Action is TKMUnitActionGoInOut)
    or not TKMUnitActionGoInOut(Action).IsStarted then
    begin
      // Position in a spiral nearest to entrance of house, updating IsUnit.
      if not gHands.FindPlaceForUnit(fInHouse.Entrance.X, fInHouse.Entrance.Y, Self, newCurrPosition, gTerrain.GetWalkConnectID(fInHouse.Entrance)) then
      begin
        // There is no space for this unit so it must be destroyed
        //todo: re-route to KillUnit and let it sort out that unit is invisible and cant be placed
        fKilledBy := HAND_NONE;
        if (Owner <> HAND_NONE)
        and not IsDeadOrDying
        and Assigned(OnUnitDied) then
          OnUnitDied(Self);

        //These must be freed before running CloseUnit because task destructors sometimes need access to unit properties
        SetAction(nil);
        FreeAndNil(fTask);
        CloseUnit(False); //Close the unit without removing tile usage (because this unit was in a house it has none)
        Exit(True);
      end;
      SetPositionRound(newCurrPosition); //will update FOW

      // Unit was occupying tile (he was walking inside house when house was destroyed)
      placedOnOccupiedTile := gTerrain.Land^[fPositionRound.Y, fPositionRound.X].IsUnit = Self;
      //Make sure these are reset properly
      Assert(not gTerrain.HasUnit(fPositionRound) or placedOnOccupiedTile);
      IsExchanging := False;
      fPositionF := KMPointF(fPositionRound);
      fPositionPrev := fPositionRound;
      fPositionNext := fPositionRound;

      // Do not add unit to terrain, if he is already occupying house entrance tile
      if not placedOnOccupiedTile then
        gTerrain.UnitAdd(fPositionRound, Self); //Unit was not occupying tile while inside the house, hence just add do not remove

      //OnWarriorWalkOut usually happens in TUnitActionGoInOut, otherwise the warrior doesn't get assigned a group
      //Do this after setting terrain usage since OnWarriorWalkOut calls script events
      if (Self is TKMUnitWarrior) then
      begin
        TKMUnitWarrior(Self).WalkedOut(nil, wasTrained);
      end;

      if Action is TKMUnitActionGoInOut then
        SetActionLockedStay(0, uaWalk); //Abandon the walk out in this case

      if (Task is TKMTaskGoEat) and (TKMTaskGoEat(Task).Eating) then
      begin
        FreeAndNil(fTask); //Stop the eating animation and makes the unit appear
        SetActionStay(0, uaWalk); //Free the current action and give the unit a temporary one
      end;
      //If we were idle abandon our action so we look for a new house immediately (rather than after 20 seconds for the fisherman)
      //Reset task even if tile is locked, otherwise unit will wait till the end of his old ActionStayLocked...
      if (Task = nil) and (Action is TKMUnitActionStay) then
        SetActionStay(0, uaWalk); //Free the current action and give the unit a temporary one
    end;
    SetInHouse(nil); //Can't be in a destroyed house
  end;
end;


procedure TKMUnit.UpdateVisualState;
begin
  // Action could be nil just before death of the unit
  if (fAction <> nil) then
    fVisual.UpdateState;
end;


procedure TKMUnit.VertexAdd(const aFrom, aTo: TKMPoint);
begin
  gTerrain.UnitVertexAdd(aFrom, aTo);
end;


procedure TKMUnit.VertexRem(const aLoc: TKMPoint);
begin
  gTerrain.UnitVertexRem(aLoc); //Unoccupy vertex
end;


function TKMUnit.VertexUsageCompatible(const aFrom, aTo: TKMPoint): Boolean;
begin
  Result := gTerrain.VertexUsageCompatible(aFrom, aTo);
end;


procedure TKMUnit.Walk(const aFrom, aTo: TKMPoint);
begin
  gTerrain.UnitWalk(aFrom, aTo, Self)
end;


function TKMUnit.GetActivityText: UnicodeString;
const
  TASK_TEXT: array[TKMUnitTaskType] of Integer = (
      -1,-1,                   //uttUnknown, uttSelfTrain
      TX_UNIT_TASK_DELVERING,  //uttDeliver
      TX_UNIT_TASK_ROAD,       //uttBuildRoad
      TX_UNIT_TASK_WINEFIELD,  //uttBuildWine
      TX_UNIT_TASK_FIELD,      //uttBuildField
      TX_UNIT_TASK_HOUSE_SITE, //uttBuildHouseArea
      TX_UNIT_TASK_HOUSE,      //uttBuildHouse
      TX_UNIT_TASK_REPAIRING,  //uttBuildHouseRepair
      TX_UNIT_TASK_HOME,       //uttGoHome
      TX_UNIT_TASK_DISMISS,    //uttDismiss
      TX_UNIT_TASK_INN,        //uttGoEat
      -1,                      //uttMining (overridden by Citizen)
      -1,                      //uttDie (never visible)
      TX_UNIT_TASK_INN,        //uttGoOutShowHungry
      -1,                      //uttAttackHouse (overridden by Warrior)
      -1,                      //uttThrowRock (never visible)
      TX_UNIT_TASK_ROAD,        //uttBuildPalisade
      TX_UNIT_TASK_ROAD,        //uttBuildRemove
      TX_UNIT_TASK_HOUSE,        //uttBuildRemove
      -1,                      //uttBuildRemove
      -1,                      //uttBuildRemove
      -1,                      //uttGoToShip
      TX_UNIT_TASK_ROAD,        //uttBuildGrassland
      -1,                      //uttUnloadFromShip
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1,
      -1
    );
begin
  if (fTask <> nil) and (TASK_TEXT[fTask.TaskType] <> -1) then
    Result := gResTexts[TASK_TEXT[fTask.TaskType]]
  else
    Result := gResTexts[TX_UNIT_TASK_IDLE];
end;


procedure TKMUnit.UpdateHitPoints(UseEffect : Boolean = true);
var restorePace : Byte;
begin
  if UnitType in UNITS_SHIPS then   //do not increase health if it's ship
    Exit;
  restorePace := HITPOINT_RESTORE_PACE;
  If gHands[Owner].ArmyDevUnlocked(10) then
    restorePace := restorePace - 20;
  //Use fHitPointCounter as a counter to restore hit points every X ticks (Humbelum says even when in fights)
  if restorePace = 0 then Exit; //0 pace means don't restore

  if fHitPointCounter > 200 then//wait minimum 20 seconds
    if (fHitPointCounter mod restorePace = 0) and (fHitPoints < HitPointsMax) then
      Inc(fHitPoints);

  Inc(fHitPointCounter, 1); //Increasing each tick by 1 would require 13,6 years to overflow Cardinal
end;

function TKMUnit.GetDefence : SmallInt;
begin
  Result := fDefence;

  inc(Result, byte(fSpecialEffect.EffectType = uetDefence) * 3);
  if fCondition < 150 then
    Result := Result - 3
  else
  if fCondition < 300 then
    Result := Result - 2
  else
  if fCondition < 600 then
    Result := Result - 1;

  if gHands[Owner].IsAffectedbyMBD then
  begin
    if gGameParams.MBD.IsEasy then
      Result := Round(Result * 1.5)
    else
    if gGameParams.MBD.IsHardOrRealism then
      Result := Round(Result * 0.8);
  end;

  Result := Max(Result, 0);
end;

function TKMUnit.GetSight : SmallInt;
begin
  Result := fSight;

  If gHands[Owner].ArmyDevUnlocked(23) then
    Inc(Result, 4);
end;


Procedure TKMUnit.SetDefence(aValue : SmallInt);
begin
  fDefence := Max(aValue, 0);
end;

function TKMUnit.GetAttack : SmallInt;
begin
  Result := fAttack;
  inc(Result, byte(fSpecialEffect.EffectType = uetAttack) * 50);

  if fCondition < 150 then
    Result := Round(Result * 0.25)
  else
  if fCondition < 300 then
    Result := Round(Result * 0.5)
  else
  if fCondition < 600 then
    Result := Round(Result * 0.75);

  if gHands[Owner].IsAffectedbyMBD then
  begin
    if gGameParams.MBD.IsEasy then
      Result := Round(Result * 1.5)
    else
    if gGameParams.MBD.IsHardOrRealism then
      Result := Round(Result * 0.8);
  end;

  Result := Max(Result, 1);
end;

Procedure TKMUnit.SetAttack(aValue : SmallInt);
begin
  fAttack := Max(aValue, 1);
end;

function TKMUnit.GetSlide(aCheck: TKMCheckAxis): Single;
var
  dY, dX, pixelPos, lookupDiagonal: ShortInt;
begin
  Result := 0;

  // When going into a house, units "slide" towards the door when it is not on center
  if Action is TKMUnitActionGoInOut then
    Result := Result + TKMUnitActionGoInOut(Action).GetDoorwaySlide(aCheck);

  if not IsExchanging or not (Action.ActName in [uanWalkTo, uanGoInOut]) then Exit;

  // Uses Y because a walk in the Y means a slide in the X
  dX := Sign(PositionNext.X - fPositionF.X);
  dY := Sign(PositionNext.Y - fPositionF.Y);

  if (aCheck = axX) and (dY = 0) then Exit; // Unit is not shifted
  if (aCheck = axY) and (dX = 0) then Exit;

  lookupDiagonal := Abs(dX) + Abs(dY); // Which gives us swith: 1-straight, 2-diagonal.

  case aCheck of
    axX:  begin
            pixelPos := Round(Abs(fPositionF.Y - PositionPrev.Y) * CELL_SIZE_PX * Sqrt(lookupDiagonal)); //Diagonal movement *sqrt(2)
            Result := Result + (dY * SLIDE_LOOKUP[lookupDiagonal, pixelPos]) / CELL_SIZE_PX;
          end;
    axY:  begin
            pixelPos := Round(Abs(fPositionF.X - PositionPrev.X) * CELL_SIZE_PX * Sqrt(lookupDiagonal)); //Diagonal movement *sqrt(2)
            Result := Result - (dX * SLIDE_LOOKUP[lookupDiagonal, pixelPos]) / CELL_SIZE_PX;
          end;
  end;
end;


function TKMUnit.GetSlides: TKMPointF;
var
  dY, dX, pixelPos, lookupDiagonal: ShortInt;
  tmp: Single;
begin
  // When going into a house, units "slide" towards the door when it is not on center
  if Action is TKMUnitActionGoInOut then
    Result := TKMUnitActionGoInOut(Action).GetDoorwaySlides
  else
    Result := KMPointF(0, 0);

  if not IsExchanging or not (Action.ActName in [uanWalkTo, uanGoInOut]) then Exit;

  // Uses Y because a walk in the Y means a slide in the X
  dX := Sign(PositionNext.X - fPositionF.X);
  dY := Sign(PositionNext.Y - fPositionF.Y);

  if (dX = 0) and (dY = 0) then Exit; // Unit is not shifted

  lookupDiagonal := Abs(dX) + Abs(dY); // Which gives us swith: 1-straight, 2-diagonal.

  tmp := CELL_SIZE_PX * Sqrt(lookupDiagonal); // Diagonal movement *sqrt(2)

  if dY <> 0 then
  begin
    pixelPos := Round(Abs(fPositionF.Y - PositionPrev.Y) * tmp);
    Result.X := Result.X + (dY * SLIDE_LOOKUP[lookupDiagonal, pixelPos]) / CELL_SIZE_PX;
  end;

  if dX <> 0 then
  begin
    pixelPos := Round(Abs(fPositionF.X - PositionPrev.X) * tmp);
    Result.Y := Result.Y - (dX * SLIDE_LOOKUP[lookupDiagonal, pixelPos]) / CELL_SIZE_PX;
  end;
end;


Function TKMUnit.GiveBoots(aFromScript : Boolean = false) : Boolean;
begin
  Result := false;
  //if not aFromScript then
  //  if not (fType in UNITS_WITH_BOOTS) then Exit;
  if BootsAdded then Exit;

  BootsAdded := true;
  if self is TKMUnitWarrior then
    SetSpeed(4, true)
  else
    SetSpeed(8, true);
  Result := true;
end;

function TKMUnit.GetTask: TKMUnitTask;
begin
  if Self = nil then Exit(nil);

  Result := fTask;
end;


function TKMUnit.PathfindingShouldAvoid: Boolean;
begin
  Result := not (fAction is TKMUnitActionWalkTo); //If we're walking, pathfinding should not route around us
end;


function TKMUnit.GetMovementVector: TKMPointF;
var
  movementSpeed: Single;
begin
  if (Action is TKMUnitActionWalkTo) and TKMUnitActionWalkTo(Action).DoesWalking then
    movementSpeed := GetEffectiveWalkSpeed(DIAG_DIRECTION[fDirection]){gRes.Units[fType].GetEffectiveWalkSpeed(DIAG_DIRECTION[fDirection])}
  else
  if (Action is TKMUnitActionStormAttack) then
    movementSpeed := TKMUnitActionStormAttack(Action).GetSpeed
  else
    movementSpeed := 0;

  Result.X := KMGetVertex(fDirection).X * movementSpeed;
  Result.Y := KMGetVertex(fDirection).Y * movementSpeed;
end;


//procedure TKMUnit.SetPosition(const aValue: TKMPoint);
//begin
//  raise Exception.Create('Set position is not allowed outside of TKMUnit');
//end;


function TKMUnit.IsIdle: Boolean;
begin
  Result := (fTask = nil) and ((fAction is TKMUnitActionStay) and not TKMUnitActionStay(fAction).Locked);
  Result := Result and (InShip = nil);
end;


function TKMUnit.GetIsSelectable: Boolean;
begin
  Result := not IsDeadOrDying;
end;


function TKMUnit.ObjToStringShort(const aSeparator: String = '|'): String;
var
  actStr, taskStr: String;
begin
  if Self = nil then Exit('nil');

  actStr := 'nil';
  taskStr := 'nil';
  if fAction <> nil then
    actStr := fAction.ObjToStringShort;
  if fTask <> nil then
    taskStr := fTask.ObjToString;

  Result := inherited ObjToStringShort(aSeparator) +
            Format('%sType = %s%sAction = %s%sTask = [%s]%sIsDead = %s',
                   [aSeparator,
                    GetEnumName(TypeInfo(TKMUnitType), Integer(fType)), aSeparator,
                    actStr, aSeparator,
                    taskStr, aSeparator,
                    BoolToStr(fIsDead, True)]);
end;


function TKMUnit.ObjToString(const aSeparator: String = '|'): String;
var
  homeStr, inHouseStr, actStr: String;
begin
  if Self = nil then Exit('nil');

  homeStr := 'nil';
  inHouseStr := 'nil';

  if fHome <> nil then
    homeStr := Format('[UID = %d, Type = %s]', [fHome.UID, GetEnumName(TypeInfo(TKMHouseType), Integer(fHome.HouseType))]);

  if fInHouse <> nil then
    inHouseStr := Format('[UID = %d, Type = %s]', [fInHouse.UID, GetEnumName(TypeInfo(TKMHouseType), Integer(fInHouse.HouseType))]);

  if fAction <> nil then
    actStr := fAction.ObjToString(aSeparator + '  ');

  Result := inherited ObjToString(aSeparator) +
            Format('%sDir = %s%sPrevPosition = %s%sNextPosition = %s%s' +
                   'Thought = %s%sHitPoints = %d%sHitPointCounter = %d%sCondition = %d%s' +
                   'Home = %s%sInHouse = %s%sVisible = %s%sAnimStep = %d%s' +
                   'BootsAdded = %s%s' +
                   'Speed = %d%s',
                   [aSeparator,
                    GetEnumName(TypeInfo(TKMDirection), Integer(fDirection)), aSeparator,
                    TypeToString(fPositionPrev), aSeparator,
                    TypeToString(fPositionNext), aSeparator,
                    GetEnumName(TypeInfo(TKMUnitThought), Integer(fThought)), aSeparator,
                    fHitPoints, aSeparator,
                    fHitPointCounter, aSeparator,
                    fCondition, aSeparator,
                    homeStr, aSeparator,
                    inHouseStr, aSeparator,
                    BoolToStr(fVisible, True), aSeparator,
                    AnimStep, aSeparator,
                    BoolToStr(BlockWalking, True), aSeparator,
                    Round(fSpeed * 240), aSeparator]);
end;


procedure TKMUnit.Save(SaveStream: TKMemoryStream);
var
  hasTask, hasAct: Boolean;
  actName: TKMUnitActionName;
begin
  inherited;

  SaveStream.PlaceMarker('Unit');
  SaveStream.Write(fType, SizeOf(fType));

  hasTask := fTask <> nil; //Thats our switch to know if unit should write down his task.
  SaveStream.Write(hasTask);
  if hasTask then
  begin
    //We save TaskName to know which Task class to load
    SaveStream.Write(fTask.TaskType, SizeOf(fTask.TaskType));
    fTask.Save(SaveStream);
  end;

  hasAct := fAction <> nil;
  SaveStream.Write(hasAct);
  if hasAct then
  begin
    actName := fAction.ActName; //Can not pass function result to Write
    //We save ActName to know which Task class to load
    SaveStream.Write(actName, SizeOf(actName));
    fAction.Save(SaveStream);
  end;

  SaveStream.Write(fThought, SizeOf(fThought));
  SaveStream.Write(fCondition);
  SaveStream.Write(fStartWDefaultCondition);
  SaveStream.Write(fTicker);
  SaveStream.Write(fHitPoints);
  SaveStream.Write(fHitPointCounter);
  SaveStream.Write(HitPointsInvulnerable);

  SaveStream.Write(fInHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fHome.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(TKMunit(InShip).UID);

  SaveStream.Write(fPositionF);
  SaveStream.Write(fVisible);
  SaveStream.Write(fIsDead);
  SaveStream.Write(fKillASAP);
  SaveStream.Write(fKillASAPShowAnimation);
  SaveStream.Write(IsExchanging);

  SaveStream.Write(AnimStep);
  SaveStream.Write(fDirection);
  SaveStream.Write(fPositionRound);
  SaveStream.Write(fPositionPrev);
  SaveStream.Write(fPositionNext);
  SaveStream.Write(fDismissASAP);
  SaveStream.Write(Dismissable);
  SaveStream.Write(fKilledBy, SizeOf(fKilledBy));
  SaveStream.Write(fLastTimeTrySetActionWalk);

  SaveStream.Write(fAttack);
  SaveStream.Write(fAttackHorse);
  SaveStream.Write(fDefence);
  SaveStream.Write(fSight);
  SaveStream.Write(fHitPointsMax);
  SaveStream.Write(fSpeed);
  SaveStream.Write(fProjectileDefence);

  SaveStream.Write(InstantKill);
  SaveStream.Write(IsHero);
  SaveStream.Write(fConditionPace);
  SaveStream.Write(BootsAdded);
  SaveStream.Write(fNeverHungry);
  SaveStream.Write(FlagColor);
  SaveStream.Write(BlockWalking);
  SaveStream.Write(Immortal);
  SaveStream.WriteData(fSpecialEffect);
end;


procedure TKMUnit.Show;
begin
  Visible := true;
  gTerrain.UnitAdd(fPositionRound, self);
end;

procedure TKMUnit.Hide;
begin
  Visible := false;
  gTerrain.UnitRem(Position);
end;


function TKMUnit.UpdateTask: Boolean;
var
  actResult: TKMActionResult;
begin
  Result := true;
  actResult := fAction.Execute;

  case actResult of
    arActContinues:     begin
                          SetPositionRoundByPosF; //will update FOW
                          Exit;
                        end;
    arActAborted:       begin
                          FreeAndNil(fAction);
                          FreeAndNil(fTask);
                        end;
    arActCanNotStart:   begin
                          FreeAndNil(fAction);
                          if (fTask <> nil) and not fTask.CanRestartAction(arActCanNotStart) then
                            FreeAndNil(fTask);
                        end;
    arActDone:          FreeAndNil(fAction);
  end;

  SetPositionRoundByPosF; //will update FOW

  if fTask <> nil then
  begin
    fTask.fLastActionResult := actResult;

    case fTask.Execute of
      trTaskContinues:  Exit;
      trTaskDone:       FreeAndNil(fTask);
    end;

  end;
  Result := false;
end;

{Here are common Unit.UpdateState routines}
function TKMUnit.UpdateState: Boolean;
begin
  //There are layers of unit activity (bottom to top):
  // - Action (Atom creating layer (walk 1frame, etc..))
  // - Task (Action creating layer)
  // - specific UpdateState (Task creating layer)
  Result := True;

  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action at start of TKMUnit.UpdateState', fPositionRound);

  //We only need to update fog of war regularly if we're using dynamic fog of war, otherwise only update it when the unit moves

  if gMySpectator.SelectedHandID = Owner then
    if gGameParams.DynamicFOW and (fTicker mod FOW_PACE = 0) then
      gHands.RevealForTeam(Owner, fPositionRound, fSight, FOG_OF_WAR_MAX, frtUnit);

  if not ((fAction is TKMUnitActionWalkTo) and TKMUnitActionWalkTo(fAction).DoingExchange) then
  begin
    //UpdateState can happen right after unit gets killed (Exchange still in progress)
    if fKillASAP then
    begin
      DoKill(fKillASAPShowAnimation);
      fKillASAP := False;
      Assert(IsDeadOrDying, 'Unit should be dead or dying'); //Just in case KillUnit failed
    end;

    //UpdateState can happen right after unit gets dismissed (Exchange still in progress)
    if fDismissASAP then
    begin
      fDismissASAP := False; //Could be set back to True in DoDismiss
      DoDismiss;
    end;
  end;

  //Reset unit dismiss, when target school is destroyed
  if not fDismissASAP and (fTask is TKMTaskDismiss)
    and ((TKMTaskDismiss(fTask).School = nil)
      or TKMTaskDismiss(fTask).School.IsDestroyed) then
    DoDismiss;

  Inc(fTicker);

  //Update hunger
  if not (fNeverHungry or Immortal or gHands[Owner].NeverHungry) then
    If (UnitType in SIEGE_MACHINES) and gHands[Owner].ArmyDevUnlocked(30) then
    begin
      //do nothing
    end else
    if (fTicker mod Max(fConditionPace, 1) = 0) then
    begin
      If (fCondition > 0)
      and not ((fTask is TKMTaskGoEat) and TKMTaskGoEat(fTask).Eating) then
        Dec(fCondition);

    end;

  //kill by palisade
  if (fTicker mod 10 = 0) then
  begin
    If (fSpecialEffect.Duration > 0) then
    begin
      Dec(fSpecialEffect.Duration, 10);
      If fSpecialEffect.Duration = 0 then
        fSpecialEffect.EffectType := uetNone;
    end;

    if gTerrain.TileHasPalisade(fPositionRound.X, fPositionRound.Y) then
      if gHands.CheckAlliance(Owner, gTerrain.Land^[fPositionRound.Y, fPositionRound.X].TileOwner) = atEnemy then
      begin
        If gHands[Owner].ArmyDevUnlocked(15) then
          HitPointsDecrease(60, 2, nil)
        else
          HitPointsDecrease(45, 1, nil);
      end;
    //check if unit is on locked tile;

    if gTerrain.AvoidTile(Position) and IsIdle then
    begin
      CancelTask;
      fTask := TKMTaskGoToLoc.Create(self, gTerrain.FindPlaceForUnit(Position, gRes.Units[fType].AllowedPassability, 7));
    end;

  end;

  //Unit killing could be postponed by few ticks, hence fCondition could be <0
  if fCondition <= 0 then
    Kill(HAND_NONE, True, False);



  UpdateThoughts;
  UpdateHitPoints;
  if UpdateVisibility then Exit; //incase units home was destroyed. Returns True if the unit was killed due to lack of space

  //Shortcut to kill unit in place if it's on an unwalkable tile. We use fNextPosition rather than fCurrPosition
  //because once we have taken a step from a tile we no longer care about it. (fNextPosition matches up with IsUnit in terrain)
  if fAction is TKMUnitActionWalkTo then
    if DesiredPassability = tpWalkRoad then
    begin
      if not gTerrain.CheckPassability(fPositionNext, tpWalk) then
        {$IFNDEF RUNNER}
        Self.Kill(HAND_NONE, False, True);
        //Grayter 18.01.2018
        //Despite checking passability of current tile, some units can walk on
        //unwalkable tile especially when there are many soldiers on the map (> 4000)
        //I don't know why it happens really, so we decided to kill this unit instead
        //of rising error. This problem does not occur in 99.9% gameplays and is fired
        //randomly so it is practically impossible to debug.
        {$ELSE}
        raise ELocError.Create(Format('%s on unwalkable tile at %s pass CanWalk', [gRes.Units[UnitType].GUIName, fPositionNext.ToString]), fPositionNext);
        {$ENDIF}
    end else
    if not gTerrain.CheckPassability(fPositionNext, DesiredPassability) then
      {$IFNDEF RUNNER}
      Self.Kill(HAND_NONE, False, True);
      //Explanation above
      {$ELSE}
      raise ELocError.Create(Format('%s on unwalkable tile at %s pass: ''%s''', [gRes.Units[UnitType].GUIName, fPositionNext.ToString, PASSABILITY_GUI_TEXT[DesiredPassability]]), fPositionNext);
      {$ENDIF}

  //
  //Performing Tasks and Actions now
  //------------------------------------------------------------------------------------------------
  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName + ' has no action in TKMUnit.UpdateState', fPositionRound);

  SetPositionRoundByPosF; //will update FOW

  //If we get to this point then it means that common part is done and now
  //we can perform unit-specific activities (ask for job, etc..)
  if fTask <> nil then
    fTask.UpdateState;
  if UpdateTask then
    Exit;

  Result := False;
end;


procedure TKMUnit.PaintUnit(aTickLag: Single);
begin
  //only for subclasses
end;

procedure TKMUnit.Paint(aTickLag: Single);
var
  V: TKMUnitVisualState;
  paintPos : TKMPointF;
begin
  //Here should be catched any cases where unit has no current action - this is a flaw in TTasks somewhere
  //Unit always meant to have some Action performed.
  //However, do not assert it here because then the player cannot close the message (paint happens repeatedly)
  //We check it at the start and end of UpdateState, that is the only place.

  if fAction <> nil then
    fAction.Paint;

  if fTask <> nil then
    fTask.Paint;

  if SHOW_POINTER_DOTS then
    gRenderAux.UnitPointers(fPositionF.X + 0.5 + GetSlide(axX), fPositionF.Y + 1 + GetSlide(axY), PointerCount);

  if SHOW_TILE_UNIT then
    gRenderAux.CircleOnTerrain(fPositionF.X - 0.5 + GetSlide(axX), fPositionF.Y - 0.5 + GetSlide(axY), 0.35, GetRandomColorWSeed(UID));

  if IsHero then
    gRenderAux.CircleOnTerrain(fPositionF.X - 0.5 + GetSlide(axX), fPositionF.Y - 0.5 + GetSlide(axY), 0.5, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor) and $55FFFFFF);
  if Visible and (fAction <> nil) then
    PaintUnit(aTickLag);

  if IsSelected then
  begin

    V := fVisual.GetLerp(aTickLag);

    paintPos.X := V.PositionF.X + UNIT_OFF_X + V.SlideX;
    paintPos.Y := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;
    paintPos :=  gTerrain.RenderFlatToHeight(paintPos);

    gRenderPool.AddSprite(paintPos + KMPointF(0.9, -0.5 + sin(gGameApp.GlobalTickCount) * 0.1),
                954, rxGui, FlagColor, true, false, 0, true);
  end;

end;


class function TKMUnit.GetDefaultCondition: Integer;
begin
  Result := Round(UNIT_MAX_CONDITION * UNIT_CONDITION_BASE);
end;


{ TUnitTask }
constructor TKMUnitTask.Create(aUnit: TKMUnit);
begin
  inherited Create;

  fType := uttUnknown;
  fLastActionResult := arActDone;
  Assert(aUnit <> nil);
  fUnit := aUnit.GetPointer;
  fPhase  := 0;
  fPhase2 := 0;

  InitDefaultAction;
end;


constructor TKMUnitTask.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;

  LoadStream.CheckMarker('UnitTask');
  LoadStream.Read(fType, SizeOf(fType));
  LoadStream.Read(fUnit, 4);//Substitute it with reference on SyncLoad
  LoadStream.Read(fPhase);
  LoadStream.Read(fPhase2);
  LoadStream.Read(fLastActionResult, SizeOf(fLastActionResult));
end;


procedure TKMUnitTask.SyncLoad;
begin
  fUnit := gHands.GetUnitByUID(Integer(fUnit));
end;


destructor TKMUnitTask.Destroy;
begin
  fUnit.Thought := thNone; //Stop any thoughts
  gHands.CleanUpUnitPointer(fUnit);
  fPhase        := High(Byte) - 1; //-1 so that if it is increased on the next run it won't overrun before exiting
  fPhase2       := High(Byte) - 1;
  inherited;
end;


function TKMUnitTask.GetPhase: Byte;
begin
  if Self = nil then Exit(0);

  Result := fPhase;
end;


procedure TKMUnitTask.InitDefaultAction;
begin
  fUnit.SetActionLockedStay(0, uaWalk);
end;


function TKMUnitTask.WalkShouldAbandon: Boolean;
begin
  Result := False; //Only used in some child classes
end;


function TKMUnitTask.CanRestartAction(aLastActionResult: TKMActionResult): Boolean;
begin
  Result := False; // Can't restart by default
end;


function TKMUnitTask.CouldBeCancelled: Boolean;
begin
  Result := False; //Only used in some child classes
end;


function TKMUnitTask.ObjToString(const aSeparator: String = ', '): String;
begin
  if Self = nil then Exit('nil');

  Result := Format('Type %s%sPhase = %d%sPhase2 = %d',
                   [GetEnumName(TypeInfo(TKMUnitTaskType), Integer(fType)), aSeparator,
                    fPhase, aSeparator,
                    fPhase2]);
end;


procedure TKMUnitTask.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('UnitTask');
  SaveStream.Write(fType, SizeOf(fType)); //Save task type before anything else for it will be used on loading to create specific task type
  SaveStream.Write(fUnit.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fPhase);
  SaveStream.Write(fPhase2);
  SaveStream.Write(fLastActionResult, SizeOf(fLastActionResult));
end;

procedure TKMUnitTask.UpdateState;
begin
  //later
end;


procedure TKMUnitTask.Paint;
begin

end;


{ TUnitAction }
constructor TKMUnitAction.Create(aUnit: TKMUnit; aActionType: TKMUnitActionType; aLocked: Boolean);
begin
  inherited Create;

  //Unit who will be performing the action
  //Does not require pointer tracking because action should always be destroyed before the unit that owns it
  fUnit       := aUnit;
  fType       := aActionType;
  Locked      := aLocked;
  StepDone    := False;
end;


constructor TKMUnitAction.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;

  LoadStream.CheckMarker('UnitAction');
  LoadStream.Read(fType, SizeOf(fType));
  LoadStream.Read(fUnit, 4);
  LoadStream.Read(Locked);
  LoadStream.Read(StepDone);
end;


procedure TKMUnitAction.SyncLoad;
begin
  fUnit := gHands.GetUnitByUID(Integer(fUnit));
end;

function TKMUnitAction.HasNoAnim: Boolean;
begin
  Result := false;
end;


procedure TKMUnitAction.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('UnitAction');
  SaveStream.Write(fType, SizeOf(fType));
  SaveStream.Write(fUnit.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(Locked);
  SaveStream.Write(StepDone);
end;


procedure TKMUnitAction.Paint;
begin
  //Used for debug, paint action properties here
end;


function TKMUnitAction.CanBeInterrupted(aForced: Boolean = True): Boolean;
begin
  Result := True;
end;


function TKMUnitAction.ObjToStringShort(const aSeparator: String = ' '): String;
begin
  Result := ClassName;
end;


function TKMUnitAction.ObjToString(const aSeparator: String = ' '): String;
begin
  Result := ObjToStringShort(aSeparator);
end;

procedure TKMUnitsArray.Clear;
begin
  fCount := 0;
  SetLength(fList, 0);
end;

function TKMUnitsArray.Add(aItem : TKMUnit) : Integer;
begin
  SetLength(fList, fCount + 1);
  fList[fCount] := aItem;
  Result := fCount;
  Inc(fCount);
end;


function TKMUnitsArray.Remove(aIndex: Integer) : Boolean;
var I : Integer;
begin
  if fCount = 0 then
    Exit(false);

  for I := aIndex to fCount - 2 do
    fList[I] := fList[I + 1];

  SetLength(fList, fCount - 1);
  Dec(fCount);

  Result := true;

end;

function TKMUnitsArray.Remove(aPointer: TKMUnit): Boolean;
var I : Integer;
begin
  Result := false;
  if fCount = 0 then
    Exit;

  for I := fCount - 1 downto 0 do
    if TKMUnit(fList[I]).UID = aPointer.UID then
      Result := Remove(I);
end;

function TKMUnitsArray.GetItem(aIndex : Integer) : TKMUnit;
begin
  Result := nil;
  if not InRange(aIndex, 0, fCount - 1) then
    Exit;
  Result := TKMUnit(fList[aIndex]);
end;

function TKMUnitsArray.Contains(aUnit: TKMUnit): Boolean;
var I : Integer;
begin
  Result := false;
  If aUnit = nil then
    Exit;
  for I := 0 to fCount - 1 do
    if TKMUnit(fList[I]) = aUnit then
      Exit(true);

end;

procedure TKMUnitsArray.SaveToStream(aSaveStream : TKMemoryStream);
var I : Integer;
begin
  aSaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    aSaveStream.Write(TKMUnit(fList[I]).UID);
end;

procedure TKMUnitsArray.LoadFromStream(aLoadStream : TKMemoryStream);
var I : Integer;
begin
  aLoadStream.Read(fCount);
  SetLength(fList, fCount);
  for I := 0 to fCount - 1 do
    aLoadStream.Read(fList[I], 4);
end;
procedure TKMUnitsArray.SyncLoad;
var I : Integer;
begin
  for I := 0 to fCount - 1 do
    fList[I] := TKMUnit(gHands.GetUnitByUID(Integer(fList[I])));
end;

{
  TKMUnitsArray = record
    private
      fCount : Integer;
      fList : array of TKMUnit;
      function GetItem(aIndex : Integer) : TKMUnit;
    public
      function Add(aItem : TKMUnit) : Integer;
      function Remove(aIndex : Integer) : Boolean; Overload;

      property Units[aIndex : Integer] : TKMUnit read GetItem; default;
      property Count : Integer read fCount;

      procedure SaveToStream(aSaveStream : TKMemoryStream);
      procedure LoadFromStream(aLoadStream : TKMemoryStream);
      procedure SyncLoad;
  end;}

end.





