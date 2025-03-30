unit KM_UnitWarrior;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KromUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_ResTypes,
  KM_Houses, KM_Terrain, KM_Units, KM_CommonGameTypes;


type
  TKMUnitWarrior = class;
  TKMUnitWarriorShip = class;
  TKMWarriorEvent = procedure(aWarrior: TKMUnitWarrior) of object;
  TKMWarriorHouseTrainedEvent = procedure(aWarrior: TKMUnitWarrior; aHouse: TKMHouse; aIsTrained : Boolean) of object;
  TKMWarrior2Event = procedure(aWarrior: TKMUnitWarrior; aUnit: TKMUnit) of object;

  // What player has ordered us to do
  TKMWarriorOrder = (
    woNone, //No orders
    woWalk, //Walk somewhere
    woWalkOut, //Walk out of Barracks
    woAttackUnit, //Attack someone
    woAttackHouse, //Attack house
    woStorm, //Do Storm attack
    woStay,
    woShipBoatUnload,
    woBoatCollectWares,
    woAssignToShip,
    woTakeOverHouse
  );

  TKMUnitWarrior = class(TKMUnit)
  private
    fGroup: Pointer; // Warrior's group (pointer will be converted to TKMUnitGroup in TKMHandsCollection.GetGroupByMember
    fNextOrder: TKMWarriorOrder; //New order we should perform as soon as we can change tasks
    fNextOrderForced: Boolean; //Next order considered not forced if it comes as "repeated order" after Split/Link orders/Die member/Link after training
    fOrder: TKMWarriorOrder; //Order we are performing
    fOrderLoc: TKMPoint; //Dir is the direction to face after order
    fOrderTargetShip: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use GetOrderTarget instead.
    fOrderTargetUnit: TKMUnit; //Unit we are ordered to attack. This property should never be accessed, use GetOrderTarget instead.
    fAttackingUnit: TKMUnit; //Unit we are attacking or following to attack (f.e. group offenders). This property should never be accessed, use GetAttackingUnit instead.
    fOrderTargetHouse: TKMHouse; //House we are ordered to attack. This property should never be accessed, use GetOrderHouseTarget instead.
    fUseExactTarget: Boolean; //Do we try to reach exact position or is it e.g. unwalkable
    fLastShootTime: Cardinal; //Used to prevent archer rate of fire exploit

    fInfinityAmmo: Boolean;
    fRequestedFood: Boolean;
    fRequestedAmmo: Boolean;
    fStormDelay: Word;
    fBoltCount : Word;
    fBitinAdded : Boolean;
    fRamTicker : Cardinal;
    fDamageUnits : Word;
    fDamageHouse : Word;
    fRageTime : Word;

    procedure ClearOrderTarget(aClearAttackingUnit: Boolean = True);
    procedure ClearAttackingUnit;
    procedure SetOrderTarget(aUnit: TKMUnit);
    procedure DoSetAttackingUnit(aUnit: TKMUnit);
    function GetAttackingUnit: TKMUnit;
    function GetOrderTargetUnit: TKMUnit;
    function GetOrderHouseTarget: TKMHouse;
    procedure SetOrderHouseTarget(aHouse: TKMHouse);
    function GetOrderShipTarget: TKMUnit;
    procedure SetOrderShipTarget(aShip: TKMUnit);
    procedure UpdateOrderTargets;

    procedure TakeNextOrder; virtual;
    function CanInterruptAction(aForced: Boolean = True): Boolean;

    function GetFiringDelay: Byte;
    function GetAimingDelay: Byte;
    function GetRangeMin: Single;
    function GetRangeMax: Single;
    function GetProjectileType: TKMProjectileType;
    function GetAimSoundDelay: Byte;

    function GetDamageUnit : Word;virtual;
    function GetDamageHouse : Word;virtual;

    const RAGE_TIME_DELAY = 1200;
  protected
    function GetAllowAllyToSelect: Boolean; override;
    procedure SetAllowAllyToSelect(aAllow: Boolean); override;
    function GetDefence : SmallInt;override;
    function GetAttack : SmallInt; override;
    procedure UpdateHitPoints(UseEffect : Boolean = true); override;
    procedure PaintUnit(aTickLag: Single); override;
    procedure DoDismiss;override;
    function IsSelected : Boolean; override;
  public
    OnWarriorDismissed: TKMWarriorEvent; //Separate event from OnUnitDied to report to Group
    OnWarriorDied: TKMWarriorEvent; //Separate event from OnUnitDied to report to Group
    OnPickedFight: TKMWarrior2Event;
    OnWarriorWalkOut: TKMWarriorHouseTrainedEvent;
    // Todo: do we actually need it? Should not Group.OrderLoc.Dir be used for the same purpose ?
    FaceDir: TKMDirection; //Direction we should face after walking. Only check for enemies in this direction.
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure CloseUnit(aRemoveTileUsage: Boolean = True); override;
    destructor Destroy; override;
    procedure KillInHouse;

    function GetPointer: TKMUnitWarrior; reintroduce;

    property Group: pointer read fGroup; // Property for GetGroupByMember function
    // This procedure should not be called by anyone except UnitGroups class(it is out of property)
    procedure SetGroup(aGroup: Pointer); //Explicitly use SetWorker, to make it clear its not only pointer assignment

    function GetWarriorActivityText(aIsAttackingUnit: Boolean): UnicodeString;
    procedure Kill(aFrom: TKMHandID; aShowAnimation, aForceDelay: Boolean); override;
    procedure Dismiss; override;
    procedure DismissCancel; override;

    //Commands from TKMUnitGroup
    procedure ReloadAmmo(aWare : TKMWareType; doMax : Boolean = false);  Virtual;
    procedure OrderAmmo(aForceOrder : Boolean = false); Virtual;
    procedure OrderFood(aForceOrder : Boolean = false);
    function CanOrderAmmoFromCart : Boolean;

    procedure OrderNone;
    procedure OrderStorm(aDelay: Word; aForced: Boolean = True);
    procedure OrderWalk(const aLoc: TKMPoint; aUseExactTarget: Boolean = True; aForced: Boolean = True);
    procedure OrderStay(aForced: Boolean = True);
    procedure OrderAttackHouse(aTargetHouse: TKMHouse; aForced: Boolean = True);virtual;
    procedure OrderFight(aTargetUnit: TKMUnit);
    procedure AssignToShip(aShip : Pointer); override;
    procedure UnloadFromShip(aShip : Pointer); override;

    //Ranged units properties
    property AimingDelay: Byte read GetAimingDelay;
    property FiringDelay: Byte read GetFiringDelay;
    property RangeMin: Single read GetRangeMin;
    property RangeMax: Single read GetRangeMax;
    property ProjectileType: TKMProjectileType read GetProjectileType;
    property AimSoundDelay: Byte read GetAimSoundDelay;
    property BoltCount: Word read fBoltCount write fBoltCount;
    property DamageUnits: Word read GetDamageUnit write fDamageUnits;
    property DamageHouse: Word read GetDamageHouse write fDamageHouse;
    function TakeBolt : Boolean;
    property BitinAdded: Boolean read fBitinAdded write fBitinAdded;
    procedure SetActionFight(aAction: TKMUnitActionType; aOpponent: TKMUnit);

    function GetFightMinRange: Single;
    function GetFightMaxRange(aTileBased: Boolean = False): Single;
    function WithinFightRange(const aValue: TKMPoint): Boolean;
    function OrderDone: Boolean;
    property RequestedFood: Boolean read fRequestedFood write fRequestedFood; //Cleared by Serf delivering food
    property RequestedAmmo: Boolean read fRequestedAmmo write fRequestedAmmo; //Cleared by Serf delivering stone
    property InfinityAmmo: Boolean read fInfinityAmmo write fInfinityAmmo; //Cleared by Serf delivering stone
    property LastShootTime: Cardinal read fLastShootTime;
    function IsRanged: Boolean;
    function CanShootAndFight: Boolean;
    function InFight(aCountCitizens: Boolean = False): Boolean;
    function InFightAgaist(var aUnit: TKMUnit; aCountCitizens: Boolean = False): Boolean;
    function InAGroup: Boolean;
    function NeedsToReload(aFightAnimLength: Byte): Boolean;
    procedure SetLastShootTime;
    function FindLinkUnit(const aLoc: TKMPoint): TKMUnitWarrior;
    function CheckForEnemy: Boolean;
    procedure FightEnemy(aEnemy: TKMUnit);
    function FindEnemy(const aMinRange, aMaxRange : Single): TKMUnit;
    function PathfindingShouldAvoid: Boolean; override;
    function CanOrderAmmo : Boolean; Virtual;
    procedure AddBitin(aCount : Integer = 1);
    function IsAttackingUnit(aUnit: TKMUnit): Boolean;
    function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single; override;
    property NextOrder : TKMWarriorOrder  read fNextOrder;
    property CurrentOrder : TKMWarriorOrder  read fOrder;

    function CanJoinToGroup(aGroup : Pointer) : Boolean;
    procedure SetRageTime(aTime : Word);

    procedure SetAttackingUnit(aUnit: TKMUnit);

    procedure WalkedOut(aHouse : TKMHouse; aIsTrained : Boolean);

    procedure SetActionGoIn(aAction: TKMUnitActionType; aGoDir: TKMGoInDirection; aHouse: TKMHouse; aisTrained : Boolean = false); override;

    function ObjToStringShort(const aSeparator: String = '|'): String; override;
    function ObjToString(const aSeparator: String = '|'): String; override;

    procedure Save(SaveStream: TKMemoryStream); override;
    function UpdateState: Boolean; override;
  end;

  TKMUnitWarriorSpy = class(TKMUnitWarrior)
  private
    fAttackedPlayerTime : Cardinal;
    fFlagColor : Cardinal;
  protected
    procedure PaintUnit(aTickLag: Single); override;
  public
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;

    Procedure SetAttackedTime;
    function IsVisibleForEnemy : Boolean;
    function UpdateState: Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMUnitWarriorPaladin = class(TKMUnitWarrior)
  private
    function GetDamageUnit : Word; override;
    function GetDamageHouse : Word;override;
  protected
    procedure PaintUnit(aTickLag: Single); override;
    function GetDefence : SmallInt;override;
    function GetAttack : SmallInt; override;
  public
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    function UpdateState : Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMUnitWarriorAmmoCart = class(TKMUnitWarrior)
  private
    fAmmo : array[TKMUnitAmmoType] of Word;
    fAmmoRequested : array[TKMUnitAmmoType] of Boolean;
    function GetAmmoMinCount(aType : TKMUnitAmmoType) : Word;
    function GetAmmoMaxCount(aType : TKMUnitAmmoType) : Word;
    function GetAmmo(aType : TKMUnitAmmoType): Word;
    procedure SetAmmo(aType : TKMUnitAmmoType; aValue : Word);
  public

    procedure ReloadAmmo(aWare : TKMWareType; doMax : Boolean = false);  override;
    procedure OrderAmmo(aForceOrder : Boolean = false); override;
    procedure OrderAmmoType(aType : TKMUnitAmmoType);
    function CanOrderAmmo : Boolean; override;
    function CanOrderAmmoType(aType : TKMUnitAmmoType) : Boolean;
    property Ammo[aType : TKMUnitAmmoType] : Word read GetAmmo write SetAmmo;
    function TakeAmmo(aType : TKMUnitAmmoType; aCount: Word): Word;

    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;

    procedure Save(SaveStream: TKMemoryStream); override;
    function ObjToString(const aSeparator: String = '|'): String; override;
  end;

  TKMUnitWarriorSpikedTrap = class(TKMUnitWarrior)
    private
      fAnimKill,
      fStayTime : Word;
      procedure CheckForEnemy;
      procedure HitEnemies;
      function HitDelay : Byte;
    protected
      procedure PaintUnit(aTickLag: Single); override;
    public
      constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);

      constructor Load(LoadStream: TKMemoryStream); override;

      procedure Save(SaveStream: TKMemoryStream); override;
      function IsVisible : Boolean;

      function UpdateState : Boolean; override;
  end;

  TKMUnitWarriorMedic = class(TKMUnitWarrior)
    private
      fIsHealing : Boolean;
      fHealingTick : Cardinal;
    protected
      procedure PaintUnit(aTickLag: Single); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;

      procedure Save(SaveStream: TKMemoryStream); override;
      property IsHealing : Boolean read fIsHealing;
      function UpdateState : Boolean; override;
  end;

  TKMUnitWarriorShipCommon = class (TKMUnitWarrior)
  public
    procedure UpdateHitPoints(UseEffect : Boolean = true); override;
  end;

  TKMUnitWarriorShip = class(TKMUnitWarriorShipCommon)
  private
    fUnitsInside : TKMUnitsArray;
    fUnitsFromMap : TKMArray<TKMUnitMainData>;

    fUnloading : Boolean;
    fUnloadingPoint : TKMPoint;
    fLastUnloadTick,
    fLastAsignementTick : Cardinal;
    fStoredUnits : Single;
    function GetMapEdUnit(aIndex : Integer) : TKMUnitMainData;
    procedure SetMapEdUnit(aIndex : Integer; aValue : TKMUnitMainData);
    function GetMapEdUnitCount : Byte;
  public
    function CanAssignWarrior(aWarrior : Pointer) : Boolean;
    function WarriorMustWait : Boolean;
    function WarriorMustWaitToUnload : Boolean;
    function UnloadUnit(aUnit : TKMUnit; aLocTo : TKMPoint; aForceByKill : Boolean = false) : Boolean;
    procedure RemoveUnit(aUnit : TKMUnit;  aForceByKill : Boolean = false);
    function IsCloseToWater : Boolean;

    procedure AssignWarrior(aWarrior : Pointer);
    function GetClosestSpot : TKMPoint;

    procedure OrderAmmo(aForceOrder : Boolean = false); override;
    procedure DoUnloadUnits;
    function GetUnloadingPoint : TKMPoint;

    property MapEdUnitsCount : Byte read GetMapEdUnitCount;
    property MapEdUnits[aIndex : Integer] : TKMUnitMainData read GetMapEdUnit write SetMapEdUnit;
    property MapEdUnitsArray : TKMArray<TKMUnitMainData> read fUnitsFromMap;
    procedure AddMapEdUnit(aUnitType : TKMUnitType; aCondition, aBoltCount, aMembersCount, aColumnsCount  : Integer);

    function GetAllUnitsInside : TKMUnitPlan;

    procedure CloseUnit(aRemoveTileUsage: Boolean = True); override;
    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);

    constructor Load(LoadStream: TKMemoryStream); override;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function UpdateState : Boolean; override;
  end;

  TKMUnitWarriorBShip = class(TKMUnitWarriorShipCommon)
  public
    procedure OrderAmmo(aForceOrder : Boolean = false); override;
  end;

  TKMUnitWarriorBoat = class(TKMUnitWarriorShipCommon)
  private
    fWares : TKMWarePlan;
    fIdleTimer : Byte;
    fCollectWares, fCollectFish : Boolean;
    function HasAnyWares : Boolean;
    procedure StartCollectingWares;
    procedure SetCollectingWares(aValue : Boolean);
    procedure SetCollectingFish(aValue : Boolean);
    function GetCanCollectWares : Boolean;
  protected
  public
    function TotalWaresCount : Word;
    procedure AddWare(aWare : TKMWareType; aCount : Integer);overload;
    function AddWare(aWare : TKMWarePlanSingle) : Boolean;overload;
    procedure AddVWare(aObject : Word);
    property Wares : TKMWarePlan read fWares;

    procedure UnloadWares;
    procedure UnloadWare(aShipyard : TKMHouse);
    property CanCollectWares : Boolean read GetCanCollectWares write SetCollectingWares;
    property CanCollectFish : Boolean read fCollectFish write SetCollectingFish;
    procedure OrderAmmo(aForceOrder : Boolean = false); override;

    constructor Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function UpdateState : Boolean; override;
  end;

  TKMUnitWarriorPyro = class(TKMUnitWarrior)

  end;

  TKMUnitWarriorLekter = class(TKMUnitWarrior)
  public
    procedure OrderAttackHouse(aTargetHouse: TKMHouse; aForced: Boolean = True);override;
  end;


implementation
uses
  TypInfo, Generics.Collections,
  KM_Entity,
  KM_ResTexts, KM_HandsCollection, KM_RenderPool, KM_UnitTaskAttackHouse,
  KM_AIDefensePos, KM_UnitTaskDie, KM_AIFields,
  KM_Hand, KM_HandLogistics, KM_HandTypes, KM_HandEntity,
  KM_UnitActionFight, KM_UnitActionGoInOut, KM_UnitActionWalkTo, KM_UnitActionStay,
  KM_UnitActionStormAttack, KM_Resource, KM_ResUnits, KM_UnitGroup,
  KM_UnitTaskCollectWares, KM_UnitTaskDismiss,
  KM_Game,
  KM_GameParams, KM_CommonUtils, KM_RenderDebug, KM_UnitVisual,
  KM_CommonExceptions, KM_CommonHelpers,
  KM_UnitGroupTypes,
  KM_ScriptingEvents;


{ TKMUnitWarrior }
constructor TKMUnitWarrior.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: TKMHandID; aInHouse: TKMHouse);
begin
  inherited;

  fGroup             := nil;
  fOrderTargetUnit   := nil;
  fOrderTargetHouse  := nil;
  fOrderTargetShip   := nil;
  fRequestedFood     := False;
  fRequestedAmmo     := False;
  fInfinityAmmo      := False;
  fNextOrder         := woNone;
  fOrder             := woNone;
  fOrderLoc          := aLoc.Loc;
  fBoltCount         := 0;
  fBitinAdded        := false;
  fLastShootTime     := 0;
  fDamageUnits := gRes.Units[aUnitType].UnitDamage;
  fDamageHouse := gRes.Units[aUnitType].HouseDamage;

  if gGame.Resource.IsTSK or gGame.Resource.IsTPR then
    fInfinityAmmo := true;
  fRageTime := 0;
end;



constructor TKMUnitWarrior.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.Read(fGroup, 4); //subst on syncload
  LoadStream.Read(fNextOrder, SizeOf(fNextOrder));
  LoadStream.Read(fNextOrderForced);
  LoadStream.Read(fOrder, SizeOf(fOrder));
  LoadStream.Read(fOrderLoc);
  LoadStream.Read(fOrderTargetHouse, 4); //subst on syncload
  LoadStream.Read(fOrderTargetUnit, 4); //subst on syncload
  LoadStream.Read(fAttackingUnit, 4); //subst on syncload
  LoadStream.Read(fOrderTargetShip, 4); //subst on syncload
  LoadStream.Read(fRequestedFood);
  LoadStream.Read(fStormDelay);
  LoadStream.Read(fUseExactTarget);
  LoadStream.Read(FaceDir);
  LoadStream.Read(fLastShootTime);
  LoadStream.Read(fBoltCount);
  LoadStream.Read(fRequestedAmmo);
  LoadStream.Read(fInfinityAmmo);
  LoadStream.Read(fBitinAdded);
  LoadStream.Read(fDamageUnits);
  LoadStream.Read(fDamageHouse);
  LoadStream.Read(fRamTicker);

end;


procedure TKMUnitWarrior.SyncLoad;
begin
  inherited;

  fGroup := TKMUnitGroup(gHands.GetGroupByUID(Integer(fGroup)));
  fOrderTargetUnit := TKMUnitWarrior(gHands.GetUnitByUID(Integer(fOrderTargetUnit)));
  fAttackingUnit := TKMUnitWarrior(gHands.GetUnitByUID(Integer(fAttackingUnit)));
  fOrderTargetShip := TKMUnitWarrior(gHands.GetUnitByUID(Integer(fOrderTargetShip)));
  fOrderTargetHouse := gHands.GetHouseByUID(Integer(fOrderTargetHouse));

  if Action is TKMUnitActionGoInOut then
    TKMUnitActionGoInOut(Action).OnWalkedOut := WalkedOut;
end;


procedure TKMUnitWarrior.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.Write( TKMUnitGroup(fGroup).UID); //Store ID
  SaveStream.Write(fNextOrder, SizeOf(fNextOrder));
  SaveStream.Write(fNextOrderForced);
  SaveStream.Write(fOrder, SizeOf(fOrder));
  SaveStream.Write(fOrderLoc);
  SaveStream.Write(fOrderTargetHouse.UID); //Store ID
  SaveStream.Write(fOrderTargetUnit.UID); //Store ID
  SaveStream.Write(fAttackingUnit.UID); //Store ID
  SaveStream.Write(fOrderTargetShip.UID); //Store ID
  SaveStream.Write(fRequestedFood);
  SaveStream.Write(fStormDelay);
  SaveStream.Write(fUseExactTarget);
  SaveStream.Write(FaceDir);
  SaveStream.Write(fLastShootTime);
  SaveStream.Write(fBoltCount);
  SaveStream.Write(fRequestedAmmo);
  SaveStream.Write(fInfinityAmmo);
  SaveStream.Write(fBitinAdded);
  SaveStream.Write(fDamageUnits);
  SaveStream.Write(fDamageHouse);
  SaveStream.Write(fRamTicker);
end;


procedure TKMUnitWarrior.CloseUnit;
begin
  //This ensures that pointer usage tracking is reset
  ClearOrderTarget;

  fNextOrder := woNone;

  inherited;
end;


destructor TKMUnitWarrior.Destroy;
begin
  //This ensures that pointer usage tracking is reset
  ClearOrderTarget;
  gHands.CleanUpGroupPointer( TKMUnitGroup(fGroup) );

  inherited;
end;

procedure TKMUnitWarrior.KillInHouse;
begin
  Assert(fInHouse <> nil, 'Unit could be silently killed only inside some House');
  //Dispose of current action/task BEFORE we close the unit (action might need to check fPosition if recruit was about to walk out to eat)
  //Normally this isn't required because TTaskDie takes care of it all, but recruits in barracks don't use TaskDie.
  SetAction(nil);
  FreeAndNil(fTask);


  CloseUnit(False); //Don't remove tile usage, we are inside the barracks
  //Report to Group that we have died
  if Assigned(OnWarriorDismissed) then
    OnWarriorDismissed(Self);
end;


function TKMUnitWarrior.GetPointer: TKMUnitWarrior;
begin
  Result := TKMUnitWarrior(inherited GetPointer);
end;


procedure TKMUnitWarrior.SetGroup(aGroup: Pointer);
begin
  gHands.CleanUpGroupPointer( TKMUnitGroup(fGroup) );
  fGroup := TKMUnitGroup(aGroup).GetPointer();
end;


procedure TKMUnitWarrior.DoDismiss;

  procedure TryCreateDismissTask;
  begin
    FreeAndNil(fTask);
    fTask := TKMTaskDismissWarrior.Create(Self); //Will create empty locked stay action
    if TKMTaskDismissWarrior(fTask).ShouldBeCancelled then
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

procedure TKMUnitWarrior.Dismiss;
begin
  //raise Exception.Create('Warrior unit can not be dismissed');
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


procedure TKMUnitWarrior.DismissCancel;
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


procedure TKMUnitWarrior.Kill(aFrom: TKMHandID; aShowAnimation, aForceDelay: Boolean);
var
  alreadyDeadOrDying: Boolean;
  I : Integer;
begin
  alreadyDeadOrDying := IsDeadOrDying; //Inherited will kill the unit

  //after killing someone we get some by-products
  If not IsDead then
    if aFrom >= 0 then
    with gHands[aFrom] do
    begin
      if UnitType in WARRIORS_IRON then
      begin
        VirtualWareTake('vtIronFerrule', -2);
        VirtualWareTake('vtNeedle', -1);
      end else
      if UnitType in SIEGE_MACHINES then
      begin
        VirtualWareTake('vtIronFerrule', -4);
        VirtualWareTake('vtWoodenPlate', -10);
      end else
      if UnitType in UNITS_SHIPS then
      begin
        VirtualWareTake('vtIronFerrule', -2);
        VirtualWareTake('vtWoodenPlate', -10);
        VirtualWareTake('vtNeedle', -5);
        VirtualWareTake('vtLeatherSheet', -10);
        VirtualWareTake('vtPearl', -1);
      end else
      if UnitType in SPECIAL_UNITS then
      begin
        for I := 0 to high(gRes.Units[UnitType].PalaceCost.Wares) do
          VirtualWareTake(gRes.Units[UnitType].PalaceCost.Wares[I].W, gRes.Units[UnitType].PalaceCost.Wares[I].C div 5);

      end else
      begin
        VirtualWareTake('vtNeedle', -1);
        VirtualWareTake('vtGemStone', -1);
        VirtualWareTake('vtCoin', -1);
        VirtualWareTake('vtLeatherSheet', -1);
        VirtualWareTake('vtWoodenPlate', -2);
      end;
    end;

  inherited;

  //After inherited so script events can still check which group the warrior is from
  if not alreadyDeadOrDying then
  begin
    ClearOrderTarget; //This ensures that pointer usage tracking is reset

    //Report to Group that we have died
    if Assigned(OnWarriorDied) then
      OnWarriorDied(Self);
  end;
end;


//Order some food for troops
procedure TKMUnitWarrior.OrderFood(aForceOrder : Boolean = false);
begin
  if not fRequestedFood or aForceOrder then
  begin
    if UnitType = utTrainedWolf then
      gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtSausage, 1, dtOnce, diHigh2)
    else
      gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtFood, 1, dtOnce, diHigh2);

    fRequestedFood := True;
  end;
end;

procedure TKMUnitWarrior.AddBitin(aCount: Integer = 1);
begin
  if fBitinAdded then Exit;
  if aCount = 0 then Exit;

  fBitinAdded := true;

  if UnitType in SIEGE_MACHINES then
  begin
    case aCount of
      1 : begin
            HitPointsMax := HitPointsMax + 1;
            HitPointsChangeFromScript(2);
          end;
      2 : begin
            Defence := Defence + 1;
            HitPointsMax := HitPointsMax + 1;
            HitPointsChangeFromScript(2);
          end;
      3 : begin
            Defence := Defence + 1;
            HitPointsMax := HitPointsMax + 2;
            HitPointsChangeFromScript(3);
            if UnitType = utBallista then
            begin
              InstantKill := true;
              SetSpeed(4, true);
              Defence := Defence + 2;
              HitPointsMax := HitPointsMax + 2;
            end;
          end;
      4 : begin

            SetSpeed(4, true);
            Defence := Defence + 2;
            HitPointsMax := HitPointsMax + 2;
            HitPointsChangeFromScript(3);

            if UnitType = utBallista then
            begin
              InstantKill := true;
              SetSpeed(4, true);
              Defence := Defence + 2;
              HitPointsMax := HitPointsMax + 2;
            end;
          end;
    end;
  end else
  begin
    Attack := Attack + IfThen(IsRanged, 15, 5);

    if AttackHorse > 0 then
      AttackHorse := AttackHorse + 5;

    Defence := Defence + 4;
    HitPointsMax := HitPointsMax + 2;
    HitPointsChangeFromScript(2);
  end;


end;

function TKMUnitWarrior.CanOrderAmmo : Boolean;
begin
  if fRequestedAmmo then
    Exit(false);
  if not gRes.Units[UnitType].CanOrderAmmo then
    Exit(false);

  case fType of
    utGolem,
    utArcher,
    utCrossbowman,
    utBowman,
    utRogue:  Result := fBoltCount <= 30;
    utCatapult: Result := fBoltCount <= 5;
    utBallista:  Result := fBoltCount <= 10;
    else Result := fBoltCount <= 10;
  end;
end;

function TKMUnitWarrior.CanOrderAmmoFromCart : Boolean;
var ammoCart : TKMUnitWarriorAmmoCart;
begin
  ammoCart := nil;
  if UnitType in [utCatapult, utBallista] then
    if fGroup <> nil then
      ammoCart :=  TKMUnitGroup(fGroup).GetUnitAmmoCart(gRes.Units[UnitType].AmmoType);

  Result := ammoCart <> nil;

end;

//Order some food for troops
procedure TKMUnitWarrior.OrderAmmo(aForceOrder : Boolean = false);
var ammoCart : TKMUnitWarriorAmmoCart;
  count : Word;
begin
  //Assert(gRes.Units[UnitType].CanOrderAmmo, 'Warrior cant order ammo');

  if not CanOrderAmmo then
    Exit;
  ammoCart := nil;
  if UnitType in [utCatapult, utBallista] then
    if fGroup <> nil then
      ammoCart :=  TKMUnitGroup(fGroup).GetUnitAmmoCart(gRes.Units[UnitType].AmmoType);

  if ammoCart <> nil then
  begin
    count := 0;
    case UnitType of
      utGolem,
      utArcher,
      utCrossbowMan,
      utBowMan : count := ammoCart.TakeAmmo(gRes.Units[UnitType].AmmoType, 30);

      utRogue : begin
                  count := ammoCart.TakeAmmo(gRes.Units[UnitType].AmmoType, 1);
                  count := count * 10;
                end;
      utBallista,
      utCatapult : count := ammoCart.TakeAmmo(gRes.Units[UnitType].AmmoType, 5);
    end;
    Inc(fBoltCount, count);
  end else
  if not fRequestedAmmo then
  begin
    case gRes.Units[UnitType].AmmoType of
      uatNone: ;
      uatArrow: gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtQuiver, 1, dtOnce, diHigh2);
      uatRogueStone: gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtStoneBolt, 1, dtOnce, diHigh2);
      uatStoneBolt: gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtStoneBolt, 2, dtOnce, diHigh2);
      uatBolt: gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtBolt, 2, dtOnce, diHigh2);
    end;

    fRequestedAmmo := True;
  end;
end;

procedure TKMUnitWarrior.ReloadAmmo(aWare : TKMWareType; doMax : Boolean = false);
begin
  Assert(gRes.Units[UnitType].CanOrderAmmo, 'Non ranged warrior cant reload Ammo');
  if UnitType = utBoat then
    Inc(fBoltCount, 100)
  else                  
  if UnitType = utBattleShip then
    Inc(fBoltCount, 500)
  else
  case gRes.Units[UnitType].AmmoType of
    uatNone: ;
    uatArrow: Inc(fBoltCount, 90 + KamRandom(20, 'TKMUnitWarrior.ReloadAmmo1'));
    uatRogueStone: Inc(fBoltCount, 90 + KamRandom(20, 'RTKMUnitWarrior.ReloadAmmo2'));
    uatStoneBolt: If doMax then Inc(fBoltCount, 20) else Inc(fBoltCount, 10);
    uatBolt: If doMax then Inc(fBoltCount, 30) else  Inc(fBoltCount, 15);
  end;

  fRequestedAmmo := false;
end;

procedure TKMUnitWarrior.OrderNone;
begin
  ClearOrderTarget;

  fNextOrder := woNone;
  fUseExactTarget := False;
end;


procedure TKMUnitWarrior.OrderStorm(aDelay: Word; aForced: Boolean = True);
begin
  //Can't order another storm attack until the current one stops
  if Action is TKMUnitActionStormAttack then Exit;

  ClearOrderTarget;

  if not gRes.Units[fType].CanStorm then
    Exit;

  fNextOrder := woStorm;
  fNextOrderForced := aForced;
  fStormDelay := aDelay;
end;


procedure TKMUnitWarrior.ClearOrderTarget(aClearAttackingUnit: Boolean = True);
begin
  //Set fOrderTargets to nil, removing pointer if it's still valid
  gHands.CleanUpUnitPointer(fOrderTargetUnit);
  gHands.CleanUpHousePointer(fOrderTargetHouse);
  gHands.CleanUpUnitPointer(fOrderTargetShip);
  if aClearAttackingUnit then
    ClearAttackingUnit;
end;


procedure TKMUnitWarrior.ClearAttackingUnit;
begin
  gHands.CleanUpUnitPointer(fAttackingUnit);
end;


procedure TKMUnitWarrior.SetOrderTarget(aUnit: TKMUnit);
begin
  //Remove previous value
  ClearOrderTarget(False); // Don't clear attacking unit
  if aUnit <> nil then
    fOrderTargetUnit := aUnit.GetPointer; //Else it will be nil from ClearOrderTarget
end;


procedure TKMUnitWarrior.DoSetAttackingUnit(aUnit: TKMUnit);
begin
  //Remove previous value
  ClearAttackingUnit;
  if aUnit <> nil then
    fAttackingUnit := aUnit.GetPointer; //Else it will be nil from ClearAttackingUnit
end;


function TKMUnitWarrior.GetAttackingUnit: TKMUnit;
begin
  //If the target unit has died then return nil
  //Don't clear fAttackingUnit here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fAttackingUnit <> nil) and fAttackingUnit.IsDead then
    Result := nil
  else
    Result := fAttackingUnit;
end;


function TKMUnitWarrior.GetOrderTargetUnit: TKMUnit;
begin
  //If the target unit has died then return nil
  //Don't clear fOrderTargetUnit here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetUnit <> nil) and fOrderTargetUnit.IsDead then
    Result := nil
  else
    Result := fOrderTargetUnit;
end;


procedure TKMUnitWarrior.SetOrderHouseTarget(aHouse: TKMHouse);
begin
  //Remove previous value
  ClearOrderTarget;
  if aHouse <> nil then
    fOrderTargetHouse := aHouse.GetPointer; //Else it will be nil from ClearOrderTarget
end;


function TKMUnitWarrior.GetOrderHouseTarget:TKMHouse;
begin
  //If the target house has been destroyed then return nil
  //Don't clear fOrderTargetHouse here, since we could get called from UI
  //depending on player actions (getters should be side effect free)
  if (fOrderTargetHouse <> nil) and (fOrderTargetHouse.IsDestroyed) then
    Result := nil
  else
    Result := fOrderTargetHouse;
end;

function TKMUnitWarrior.GetOrderShipTarget: TKMUnit;
begin
  if (fOrderTargetShip <> nil) and fOrderTargetShip.IsDeadOrDying then
    Result := nil
  else
    Result := fOrderTargetShip;
end;

procedure TKMUnitWarrior.SetOrderShipTarget(aShip: TKMUnit);
begin
  //Remove previous value
  ClearOrderTarget;
  if (aShip <> nil) then
    fOrderTargetShip := aShip.GetPointer;
end;


//Clear target unit/house if they are dead/destroyed
procedure TKMUnitWarrior.UpdateOrderTargets;
begin
  if (fOrderTargetUnit <> nil) and fOrderTargetUnit.IsDead then
    gHands.CleanUpUnitPointer(fOrderTargetUnit);

  if (fOrderTargetHouse <> nil) and fOrderTargetHouse.IsDestroyed then
    gHands.CleanUpHousePointer(fOrderTargetHouse);

  if (fOrderTargetShip <> nil) and fOrderTargetShip.IsDeadOrDying then
    gHands.CleanUpUnitPointer(fOrderTargetShip);
end;


//At which range we can fight
function TKMUnitWarrior.GetFightMaxRange(aTileBased: Boolean = False): Single;
begin
  Result := RangeMax;
  if (RangeMax = 0) or (gRes.Units[fType].FightType = ftMelee) then
  begin
    //During storm attack we look for enemies 1.42 tiles away so we engage enemies easier and don't accidentially walk past them diagonally
    if aTileBased and not (Action is TKMUnitActionStormAttack) then
      Result := 1 //Enemy must maximum be 1 tile away
    else
      Result := 1.42; //slightly bigger than sqrt(2) for diagonal fights
  end;

end;


//At which range we can fight
function TKMUnitWarrior.GetFightMinRange: Single;
begin
  Result := RangeMin;
  if (RangeMax = 0) or (gRes.Units[fType].FightType = ftMelee) then
    Result := 0.5;
end;


function TKMUnitWarrior.WithinFightRange(const aValue: TKMPoint): Boolean;
begin
  if CanShootAndFight then
    Result := InRange(KMLength(PositionNext, aValue), 0.5, 1.42) or InRange(KMLength(PositionNext, aValue), 5, GetFightMaxRange)
  else
    Result := InRange(KMLength(PositionNext, aValue), GetFightMinRange, GetFightMaxRange);
end;


//Is unit a part of the group
//(units are independent when leaving barracks, till they find a group to link to)
function TKMUnitWarrior.InAGroup: Boolean;
begin
  //Event is assigned when unit is added to a group, so we can use it as a marker
  Result := Assigned(OnWarriorDied);
end;


//Used to prevent rate of fire exploit
function TKMUnitWarrior.NeedsToReload(aFightAnimLength: Byte): Boolean;
begin
  Result := (fLastShootTime <> 0) and ((gGameParams.Tick - fLastShootTime) < aFightAnimLength);
end;


//Used to prevent rate of fire exploit
procedure TKMUnitWarrior.SetLastShootTime;
begin
  fLastShootTime := gGameParams.Tick;
end;


//We are actively fighting with an enemy
function TKMUnitWarrior.InFight(aCountCitizens: Boolean = False): Boolean;
begin
  Result := (Action is TKMUnitActionFight)
            and (aCountCitizens or (TKMUnitActionFight(Action).GetOpponent is TKMUnitWarrior))
            and not TKMUnitActionFight(Action).GetOpponent.IsDeadOrDying;
end;


function TKMUnitWarrior.InFightAgaist(var aUnit: TKMUnit; aCountCitizens: Boolean = False): Boolean;
begin
  Result := InFight(aCountCitizens);
  aUnit := nil;
  if Result then
    aUnit := TKMUnitActionFight(Action).GetOpponent;
end;


// Return True if we are attacking or following specified unit
function TKMUnitWarrior.IsAttackingUnit(aUnit: TKMUnit): Boolean;
begin
  if (Self = nil) or (aUnit = nil) then Exit(False);
  
  Result := ((fOrder = woAttackUnit) and (aUnit = GetOrderTargetUnit)) or (aUnit = GetAttackingUnit);
end;


procedure TKMUnitWarrior.SetAttackingUnit(aUnit: TKMUnit);
begin
  DoSetAttackingUnit(aUnit);
end;


function TKMUnitWarrior.IsRanged: Boolean;
begin
  Result := gRes.Units[fType].FightType = ftRanged;
end;

function TKMUnitWarrior.CanShootAndFight: Boolean;
begin
  Result := gRes.Units[fType].FightType = ftBoth;
end;

function TKMUnitWarrior.FindLinkUnit(const aLoc: TKMPoint): TKMUnitWarrior;
var
  I: Integer;
  foundUnits: TList<TKMUnit>;
  U: TKMUnit;
  best, L: Single;
begin
  Result := nil;
  best := MaxSingle;

  foundUnits := TList<TKMUnit>.Create;
  gHands[Owner].Units.GetUnitsInRect(KMRect(aLoc.X - LINK_RADIUS,
                                            aLoc.Y - LINK_RADIUS,
                                            aLoc.X + LINK_RADIUS,
                                            aLoc.Y + LINK_RADIUS),
                                     foundUnits);

  for I := 0 to foundUnits.Count - 1 do
  begin
    U := foundUnits[I];
    if (U is TKMUnitWarrior)
    and (U <> Self)
    and (UNIT_TO_GROUP_TYPE[U.UnitType] = UNIT_TO_GROUP_TYPE[fType]) // They must be the same group type
    and TKMUnitWarrior(U).InAGroup then // Check if warrior belongs to some Group
    begin
      L := KMLength(aLoc, U.Position);
      if (L < best) then
      begin
        best := L;
        Result := TKMUnitWarrior(U);
      end;
    end;
  end;

  foundUnits.Free;
end;


//Only the group knows the difference between Walking and Attacking unit, so we need aIsAttackingUnit parameter
function TKMUnitWarrior.GetWarriorActivityText(aIsAttackingUnit: Boolean): UnicodeString;
begin
  //We can't rely on fOrder because it does not get reset, so look at actions/tasks
  if fAction is TKMUnitActionFight then
    if IsRanged then
      Result := gResTexts[TX_UNIT_TASK_FIRING]
    else
      Result := gResTexts[TX_UNIT_TASK_FIGHTING]
  else
  if fAction is TKMUnitActionStormAttack then
    Result := gResTexts[TX_UNIT_TASK_STORM_ATTACK]
  else
  if fTask is TKMTaskAttackHouse then
    Result := gResTexts[TX_UNIT_TASK_ATTACKING_HOUSE]
  else
  if fAction is TKMUnitActionGoInOut then
    Result := gResTexts[TX_UNIT_TASK_MOVING]
  else
  if fAction is TKMUnitActionWalkTo then
    if aIsAttackingUnit then
      Result := gResTexts[TX_UNIT_TASK_ATTACKING]
    else
      Result := gResTexts[TX_UNIT_TASK_MOVING]
  else
    Result := gResTexts[TX_UNIT_TASK_IDLE];
end;


procedure TKMUnitWarrior.SetActionGoIn(aAction: TKMUnitActionType; aGoDir: TKMGoInDirection; aHouse: TKMHouse; aisTrained : Boolean = false);
begin
  //Assert(aGoDir = gdGoOutside, 'Walking inside is not implemented yet');
  {Assert((aHouse.HouseType = htBarracks)
          or (aHouse.HouseType = htTownHall)
          or (aHouse.HouseType = htSiegeWorkshop)
          or (aHouse.HouseType = htPalace), 'Only Barracks and TownHall so far');}
  inherited;

  TKMUnitActionGoInOut(Action).OnWalkedOut := WalkedOut;
end;


procedure TKMUnitWarrior.OrderWalk(const aLoc: TKMPoint; aUseExactTarget: Boolean = True; aForced: Boolean = True);
begin
  ClearOrderTarget;

  fNextOrder := woWalk;
  fNextOrderForced := aForced;
  fOrderLoc := aLoc;
  fUseExactTarget := aUseExactTarget;
end;

procedure TKMUnitWarrior.OrderStay(aForced: Boolean = True);
begin
  ClearOrderTarget;

  fNextOrder := woWalk;
  fNextOrderForced := aForced;
  fOrderLoc := Position;
  fUseExactTarget := false;
end;


function TKMUnitWarrior.OrderDone: Boolean;
begin
  Result := False;
  if fNextOrder <> woNone then Exit; //We haven't had time to take the order yet, so return False

  //Did we performed the Order?
  case fOrder of
    woNone:         Result := True;
    woWalk:         begin
                      if not fUseExactTarget or KMSamePoint(Position, fOrderLoc) then
                        Result := True
                      else
                      begin
                        Result := False;
                        {//Maybe unit from different group took our place
                        U := fTerrain.UnitsHitTest(fOrderLoc.Loc.X, fOrderLoc.Loc.Y);
                        if U <> nil then}
                      end;
                    end;
    woWalkOut:      Result := IsIdle;
    woAttackUnit:   Result := (GetOrderTargetUnit = nil);
    woAttackHouse:  Result := (GetOrderHouseTarget = nil);
    woStorm:        Result := IsIdle;
    woStay:         Result := IsIdle;
    woAssignToShip: Result := IsIdle;
    woShipBoatUnload: Result := IsIdle;
    woBoatCollectWares: Result := IsIdle;
    woTakeOverHouse: Result := IsIdle;
  end;
end;


//All units are assigned TTaskAttackHouse which does everything for us (move to position, hit house, abandon, etc.) }
procedure TKMUnitWarrior.OrderAttackHouse(aTargetHouse: TKMHouse; aForced: Boolean = True);
begin
  fNextOrder := woAttackHouse;
  fNextOrderForced := aForced;
  SetOrderHouseTarget(aTargetHouse);
end;


procedure TKMUnitWarrior.OrderFight(aTargetUnit: TKMUnit);
begin
  fNextOrder := woAttackUnit;
  SetOrderTarget(aTargetUnit);
end;

procedure TKMUnitWarrior.AssignToShip(aShip: Pointer);
begin
  fNextOrder := woAssignToShip;
  SetOrderShipTarget(TKMUnit(aShip));
end;

procedure TKMUnitWarrior.UnloadFromShip(aShip: Pointer);
begin
  fNextOrder := woShipBoatUnload;
  SetOrderShipTarget(TKMUnit(aShip));
end;

function TKMUnitWarrior.PathfindingShouldAvoid: Boolean;
begin
  Result := Inherited PathfindingShouldAvoid;
  Result := Result and (fNextOrder = woNone); //If we have been given an order we're about to move somewhere 
end;

function TKMUnitWarrior.TakeBolt: Boolean;
begin
  Result := gRes.Units[UnitType].CanOrderAmmo;
  if not Result then
    Exit;

  Result := (fBoltCount > 0) or InfinityAmmo;

  if not InfinityAmmo and (fBoltCount > 0) then
  begin
    dec(fBoltCount);
    if UnitType <> utBattleShip then
      if (gHands[Owner].IsComputer) or CanOrderAmmoFromCart then
        if BoltCount < 5 then
          OrderAmmo;
  end;
end;

function TKMUnitWarrior.CheckForEnemy: Boolean;
var
  newEnemy: TKMUnit;
begin
  Result := False; //Didn't find anyone to fight

  //Ranged units should not check for enemy while walking or when facing the wrong way

  if IsRanged and ((not IsIdle) or ((FaceDir <> Direction) and (FaceDir <> dirNA))) then Exit;


  //this warrior can shoot from far and attack nearby
  if CanShootAndFight then
  begin
    //first check for nearby enemies to fight with hands
    newEnemy := FindEnemy(0.5, 1.42);
    if newEnemy <> nil then
    begin
      OnPickedFight(Self, newEnemy);
      //If the target is close enough attack it now, otherwise OnPickedFight will handle it through Group.OffendersList
      //Remember that AI's AutoAttackRange feature means a melee warrior can pick a fight with someone out of range

      if WithinFightRange(newEnemy.Position) then
        FightEnemy(newEnemy);

      Result := True; //Found someone
    end else
    begin //find enemy to shoot at

      if ((not IsIdle) or ((FaceDir <> Direction) and (FaceDir <> dirNA))) then Exit;
      newEnemy := FindEnemy(5, GetFightMaxRange(true));
      if newEnemy <> nil then
      begin
        OnPickedFight(Self, newEnemy);
        //If the target is close enough attack it now, otherwise OnPickedFight will handle it through Group.OffendersList
        //Remember that AI's AutoAttackRange feature means a melee warrior can pick a fight with someone out of range

        if WithinFightRange(newEnemy.Position) then
          FightEnemy(newEnemy);

        Result := True; //Found someone
      end;
    end;

    Exit;
  end;

  
  newEnemy := FindEnemy(GetFightMinRange, GetFightMaxRange(true));
  if newEnemy <> nil then
  begin
    OnPickedFight(Self, newEnemy);
    //If the target is close enough attack it now, otherwise OnPickedFight will handle it through Group.OffendersList
    //Remember that AI's AutoAttackRange feature means a melee warrior can pick a fight with someone out of range

    if WithinFightRange(newEnemy.Position) then
      FightEnemy(newEnemy);

    Result := True; //Found someone
  end;
end;


function TKMUnitWarrior.FindEnemy(const aMinRange, aMaxRange : Single): TKMUnit;
var
  testDir: TKMDirection;
  range: Single;
begin
  Result := nil; //No one to fight
  if not CanInterruptAction then exit;

  if IsRanged then
  begin
    //We are busy with an action (e.g. in a fight)
    if (Action <> nil) and Action.Locked then Exit;

    //We are shooting at house
    if (fTask <> nil) and (fTask is TKMTaskAttackHouse) then Exit;

    //catapult is not for attacking units so don't order automatically to attack it, but only for humans
    if gHands[Owner].IsHuman then
      if (fType = utCatapult) and (TKMUnitGroup(fGroup).Order <> goAttackUnit) then Exit;
    
    //Archers should only look for opponents when they are idle or when they are finishing another fight (function is called by TUnitActionFight)
    if (Action is TKMUnitActionWalkTo)
    and ((GetOrderTargetUnit = nil) or GetOrderTargetUnit.IsDeadOrDying or not WithinFightRange(GetOrderTargetUnit.Position))
    then
      Exit;
  end else
  if CanShootAndFight and (aMinRange >= 3) then //if warrior can shoot and fight then don't check thos if minrange is too small, min 3 tiles
  begin
    //We are busy with an action (e.g. in a fight)
    if (Action <> nil) and Action.Locked then Exit;

    //We are shooting at house
    if (fTask <> nil) and (fTask is TKMTaskAttackHouse) then Exit;

    //Archers should only look for opponents when they are idle or when they are finishing another fight (function is called by TUnitActionFight)
    if (Action is TKMUnitActionWalkTo)
    and ((GetOrderTargetUnit = nil) or GetOrderTargetUnit.IsDeadOrDying or not WithinFightRange(GetOrderTargetUnit.Position))
    then
      Exit;
  end;

  if IsRanged or (CanShootAndFight and (aMinRange >= 3))  then
    testDir := Direction //Use direction for ranged attacks, if it was not already specified
  else
    testDir := dirNA;


  range := aMaxRange;
  //AI has an "auto attack range" for melee like in TSK/TPR so you can't sneak past them (when idle)
  if not IsRanged and IsIdle and gHands[Owner].IsComputer then
    range := Max(range, gHands[Owner].AI.Setup.AutoAttackRange);

  if UnitType = utBattleShip then
    testDir := DIR_TO_NEXT2[Direction];
  //This function should not be run too often, as it will take some time to execute (e.g. with lots of warriors in the range area to check)
  Result := gTerrain.UnitsHitTestWithinRad(Position, aMinRange, range, Owner, atEnemy, testDir, not RANDOM_TARGETS);
  if Result = nil then
    if UnitType = utBattleShip then
    begin
      testDir := DIR_TO_PREV2[Direction];
      Result := gTerrain.UnitsHitTestWithinRad(Position, aMinRange, range, Owner, atEnemy, testDir, not RANDOM_TARGETS);
    end;
  //if fType in [utSpy, utAmmoCart, utRam] then
  if not gRes.Units[fType].CanAttackUnits then   
    Result := nil;

  //Only stop attacking a house if it's a warrior
  if (fTask <> nil) and (fTask is TKMTaskAttackHouse) and (Action is TKMUnitActionStay) and not (Result is TKMUnitWarrior) then
    Result := nil;
end;


procedure TKMUnitWarrior.SetActionFight(aAction: TKMUnitActionType; aOpponent: TKMUnit);
var
  cycle, step: Byte;
begin
  //Archers should start in the reloading if they shot recently phase to avoid rate of fire exploit
  step := 0; //Default
  cycle := Max(gRes.Units[UnitType].UnitAnim[aAction, Direction].Count, 1);
  if (TKMUnitWarrior(Self).IsRanged) and TKMUnitWarrior(Self).NeedsToReload(cycle) then
    //Skip the unit's animation forward to 1 step AFTER firing
    step := (FiringDelay + (gGameParams.Tick - TKMUnitWarrior(Self).LastShootTime)) mod cycle;

  if CanShootAndFight and (KMLengthDiag(Position, aOpponent.Position) > 4) and NeedsToReload(cycle) then
    step := (FiringDelay + (gGameParams.Tick - TKMUnitWarrior(Self).LastShootTime)) mod cycle;

  
  if (IsRanged and (fBoltCount <= 0)) then
    step := 0;



  if (Action is TKMUnitActionWalkTo) and not TKMUnitActionWalkTo(Action).CanAbandonExternal then
    raise ELocError.Create('Unit fight overrides walk', fPositionRound);

  SetAction(TKMUnitActionFight.Create(Self, aAction, aOpponent), step);
end;


procedure TKMUnitWarrior.FightEnemy(aEnemy: TKMUnit);
begin
  Assert(aEnemy <> nil, 'Fight no one?');

  //Free the task or set it up to be resumed afterwards
  if Task <> nil then
  begin
    if (Task is TKMTaskAttackHouse) and not (aEnemy is TKMUnitWarrior) then
      TKMTaskAttackHouse(Task).Phase := 0 //Reset task so it will resume after the fight
    else
      FreeAndNil(fTask); //e.g. TaskAttackHouse
  end;

  DoSetAttackingUnit(aEnemy);

  SetActionFight(uaWork, aEnemy);
  if aEnemy is TKMUnitWarrior then
    TKMUnitWarrior(aEnemy).CheckForEnemy; //Let opponent know he is attacked ASAP

end;


{ See if we can abandon other actions in favor of more important things }
function TKMUnitWarrior.CanInterruptAction(aForced: Boolean = True): Boolean;
begin
  if (Action is TKMUnitActionStay)
    and (Task is TKMTaskAttackHouse) then
    Result := True //We can abandon attack house if the action is stay
  else
    Result := Action.CanBeInterrupted(aForced);
end;


function TKMUnitWarrior.GetFiringDelay: Byte;
const
  SLINGSHOT_FIRING_DELAY = 15; //on which frame slinger fires his rock
  FIRING_DELAY = 0; //on which frame archer fires his arrow/bolt
  CATAPULT_FIRING_DELAY = 18;
  BALLISTA_FIRING_DELAY = 8;
begin
  Result := 0;
  if IsRanged then
    case UnitType of
      utBattleShip : Result := 8;
      utGolem,
      utArcher,
      utBowman,
      utCrossbowman: Result := FIRING_DELAY;
      utRogue:  Result := SLINGSHOT_FIRING_DELAY;
      utBallista:  Result := BALLISTA_FIRING_DELAY;
      utCatapult:  Result := CATAPULT_FIRING_DELAY;
      else raise Exception.Create('Unknown shooter');
    end;
end;


function TKMUnitWarrior.GetAimingDelay: Byte;
const
  BOWMEN_AIMING_DELAY_MIN      = 6; //minimum time for bowmen to aim
  BOWMEN_AIMING_DELAY_ADD      = 6; //random component
  SLINGSHOT_AIMING_DELAY_MIN   = 0; //minimum time for slingshot to aim
  SLINGSHOT_AIMING_DELAY_ADD   = 4; //random component
  CROSSBOWMEN_AIMING_DELAY_MIN = 8; //minimum time for crossbowmen to aim
  CROSSBOWMEN_AIMING_DELAY_ADD = 8; //random component

  CATAPULT_AIMING_DELAY_MIN = 16; //minimum time for crossbowmen to aim
  CATAPULT_AIMING_DELAY_ADD = 16; //random component
  BALLISTA_AIMING_DELAY_MIN = 16; //minimum time for balista to aim
begin
  Result := 0;
  if IsRanged then
    case UnitType of
      utBattleShip : Result := 16;
      utGolem,
      utArcher,
      utBowman:     Result := BOWMEN_AIMING_DELAY_MIN + KaMRandom(BOWMEN_AIMING_DELAY_ADD, 'TKMUnitWarrior.GetAimingDelay');

      utCrossbowman: Result := CROSSBOWMEN_AIMING_DELAY_MIN + KaMRandom(CROSSBOWMEN_AIMING_DELAY_ADD, 'TKMUnitWarrior.GetAimingDelay 2');
      utRogue:  Result := SLINGSHOT_AIMING_DELAY_MIN + KaMRandom(SLINGSHOT_AIMING_DELAY_ADD, 'TKMUnitWarrior.GetAimingDelay 3');
      utBallista:  Result := BALLISTA_AIMING_DELAY_MIN + KaMRandom(BALLISTA_AIMING_DELAY_MIN, 'TKMUnitWarrior.GetAimingDelay 4');
      utCatapult:  Result := CATAPULT_AIMING_DELAY_MIN + KaMRandom(CATAPULT_AIMING_DELAY_ADD, 'TKMUnitWarrior.GetAimingDelay 5');
      else raise Exception.Create('Unknown shooter');
    end;
end;

procedure TKMUnitWarrior.UpdateHitPoints(UseEffect : Boolean = true);
var medicsCount : Byte;
begin
  if UnitType in UNITS_SHIPS then   //do not increase health if it's ship
    Exit;
  //Use fHitPointCounter as a counter to restore hit points every X ticks (Humbelum says even when in fights)
  if HITPOINT_RESTORE_PACE = 0 then Exit; //0 pace means don't restore
  medicsCount := 0;
  if Group <> nil then
  begin
    medicsCount := TKMUnitGroup(Group).UnitTypeCount(utMedic);
    medicsCount := Min(medicsCount, (TKMUnitGroup(Group).Count - medicsCount) div 2);
  end;
  if medicsCount > 0 then
  begin
    if (fTicker mod (HITPOINT_RESTORE_PACE div medicsCount) = 0)
      and (fHitPoints < HitPointsMax) then
      Inc(fHitPoints);

  end else
  if (fHitPointCounter > 200) then
    if (fHitPointCounter mod (  HITPOINT_RESTORE_PACE) = 0)
      and (fHitPoints < HitPointsMax) then
      Inc(fHitPoints);

  Inc(fHitPointCounter, 1); //Increasing each tick by 1 would require 13,6 years to overflow Cardinal
  If UseEffect and (fSpecialEffect.EffectType = uetHealing) then
    UpdateHitPoints(false);
end;


function TKMUnitWarrior.GetDefence : SmallInt;
begin
  Result := Inherited;
  Result := Result + 2 * Result * byte(fRageTime > RAGE_TIME_DELAY);

  if not (UnitType in SPECIAL_UNITS) then
    if UNIT_TO_GROUP_TYPE[UnitType] = gtMelee then
      if fGroup <> nil then
        if TKMUnitGroup(fGroup).HasUnitType(utPaladin) then //add one defense
          Result := Result + 1;
  If gHands[Owner].HasPearl(ptRalender) then
    Result := Result + 1;
end;

function TKMUnitWarrior.GetAttack : SmallInt;
begin
  Result := Inherited;
  Result := Result + 2 * Result * byte(fRageTime > RAGE_TIME_DELAY);

  if not (UnitType in SPECIAL_UNITS) then
    if UNIT_TO_GROUP_TYPE[UnitType] = gtAntiHorse then
      if fGroup <> nil then
        if TKMUnitGroup(fGroup).HasUnitType(utPikeMachine) then  //add attack
          Result := Result + 15;

  If gHands[Owner].HasPearl(ptAgros) then
    Result := Result + 20;

end;

function TKMUnitWarrior.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
begin
  Result := inherited;

  if not (UnitType in SPECIAL_UNITS) then
    if UNIT_TO_GROUP_TYPE[UnitType] = gtMounted then
      if fGroup <> nil then
        if TKMUnitGroup(fGroup).HasUnitType(utTrainedWolf) then
          Result := Result + 1/((1 + byte(aIsDiag) * 0.41) / (3/240)); //add 3 to speed if there is wolf in the group
end;

function TKMUnitWarrior.CanJoinToGroup(aGroup: Pointer): Boolean;
var G : TKMUnitGroup;
begin
  if not (TObject(aGroup) is TKMUnitGroup) then
    Exit(false);

  G := TKMUnitGroup(aGroup);

  Result := (G.GetMembersGroupType = gtAny) or (TKMUnitGroup(self.Group).GetMembersGroupType in [G.GetMembersGroupType, gtAny]);

end;

procedure TKMUnitWarrior.SetRageTime(aTime : Word);
begin
  fRageTime := RAGE_TIME_DELAY + aTime;
end;

function TKMUnitWarrior.GetAimSoundDelay: Byte;
const
  SLINGSHOT_AIMING_SOUND_DELAY = 2;
begin
  Result := 0;
  if UnitType = utRogue then
    Result := SLINGSHOT_AIMING_SOUND_DELAY;
end;

function TKMUnitWarrior.GetDamageUnit: Word;
begin
  Result := fDamageUnits;
  Result := Result + 2 * Result * byte(fRageTime > RAGE_TIME_DELAY);
  if gHands[Owner].IsAffectedbyMBD then
  begin
    if gGameParams.MBD.IsEasy then
      Result := Round(Result * 1.5)
    else    
    if gGameParams.MBD.IsHardOrRealism then
      Result := Max(Round(Result * 0.8), 1);
  end;

  if fGroup <> nil then
    if not (UnitType in SPECIAL_UNITS) then
      if UNIT_TO_GROUP_TYPE[UnitType] = gtWreckers then
        if TKMUnitGroup(fGroup).HasUnitType(utPyro) then //
          Result := Result + 1;
end;

function TKMUnitWarrior.GetDamageHouse: Word;
begin
  Result := fDamageHouse;
  Result := Result + 2 * Result * byte(fRageTime > RAGE_TIME_DELAY);
  if gHands[Owner].IsAffectedbyMBD then
  begin
    if gGameParams.MBD.IsEasy then
      Result := Round(Result * 1.5)
    else    
    if gGameParams.MBD.IsHardOrRealism then
      Result := Max(Round(Result * 0.8), 1);
  end;

  if fGroup <> nil then
    if not (UnitType in SPECIAL_UNITS) then
      if UNIT_TO_GROUP_TYPE[UnitType] = gtWreckers then
        if TKMUnitGroup(fGroup).HasUnitType(utPyro) then //
          Result := Result + 2;
end;

function TKMUnitWarrior.IsSelected: Boolean;
begin
  Result := (gMySpectator.Selected = fGroup) and (self = TKMUnitGroup(Group).SelectedUnit);
end;


function TKMUnitWarrior.GetAllowAllyToSelect: Boolean;
begin
  // Warriors considered to be allowed to select by allies at the same time as his group is
  Result := (fGroup <> nil) and TKMUnitGroup(fGroup).AllowAllyToSelect;
end;


procedure TKMUnitWarrior.SetAllowAllyToSelect(aAllow: Boolean);
begin
  if fGroup = nil then Exit;
  // Warriors considered to be allowed to select by allies at the same time as his group is
  TKMUnitGroup(fGroup).AllowAllyToSelect := aAllow;
end;


function TKMUnitWarrior.GetRangeMin: Single;
begin
  Result := gRes.Units[fType].MinRange;

  if fGroup <> nil then
    if not (UnitType in SPECIAL_UNITS) then
      if UNIT_TO_GROUP_TYPE[UnitType] = gtRanged then
        if TKMUnitGroup(fGroup).HasUnitType(utArcher) then //
          Result := 2;
end;


function TKMUnitWarrior.GetRangeMax: Single;
begin
  Result := gRes.Units[fType].MaxRange;
  IF fBitinAdded then
    Result := Result + IfThen(UnitType in [utCatapult], 2, 1);

  If gGameParams.MissionBuiltInDifficulty.IsRealism then
    If gTerrain.IsNight then
      Result := Result - 2;

  if fGroup <> nil then
    if not (UnitType in SPECIAL_UNITS) then
      if UNIT_TO_GROUP_TYPE[UnitType] = gtRanged then
        if TKMUnitGroup(fGroup).HasUnitType(utArcher) then //add bonus
          Result := Result + 2;

  If gHands[Owner].HasPearl(ptAgros) then
    Result := Result + 2;
end;


function TKMUnitWarrior.GetProjectileType: TKMProjectileType;
begin
  Assert(IsRanged or CanShootAndFight, 'Can''t get projectile type for not ranged warriors');
  case UnitType of
    utGolem,
    utArcher,
    utBowman:     Result := ptArrow;
    utCrossbowman: Result := ptBolt;
    utBattleShip,
    utBallista : Result := ptBallistaBolt;
    utCatapult : Result := ptCatapultRock;
    utRogue:  Result := ptSlingRock;
    else raise Exception.Create('Unknown shooter');
  end;
end;


//Override current action if there's an Order in queue paying attention
//to unit WalkTo current position (let the unit arrive on next tile first!)
//As well let the unit finish it's curent Attack action before taking a new order
//This should make units response a bit delayed.
procedure TKMUnitWarrior.TakeNextOrder;
var
  loc: TKMPoint;
begin
  //Make sure attack orders are still valid
  if ((fNextOrder = woAttackUnit) and (GetOrderTargetUnit = nil))
    or ((fNextOrder = woAttackHouse) and (GetOrderHouseTarget = nil))
    or ( (UnitType <> utBoat) and (fNextOrder in [woAssignToShip, woShipBoatUnload]) and (GetOrderShipTarget = nil)) then
    fNextOrder := woNone;

  if (UnitType = utBoat) and (fNextOrder = woShipBoatUnload) and (GetOrderHouseTarget = nil) then
    fNextOrder := woNone;

  if (fNextOrder = woAttackHouse) and not gRes.Units[fType].CanAttackHouses then
    fNextOrder := woNone;
  if (fNextOrder = woAttackUnit) and not gRes.Units[fType].CanAttackUnits then
    fNextOrder := woNone;

  case fNextOrder of
    woNone: ;
    woWalk:         begin
                      //We can update existing Walk action with minimum changes
                      if (Action is TKMUnitActionWalkTo)
                        and not TKMUnitActionWalkTo(Action).DoingExchange then
                      begin
                        FreeAndNil(fTask); //e.g. TaskAttackHouse

                        loc := gTerrain.GetClosestTile(fOrderLoc, Position, GetDesiredPassability, fUseExactTarget);

                        TKMUnitActionWalkTo(Action).ChangeWalkTo(loc, 0);
                        fNextOrder := woNone;
                        fOrder := woWalk;
                      end
                      else
                      //Other actions are harder to interrupt
                      if CanInterruptAction(fNextOrderForced) then
                      begin
                        FreeAndNil(fTask);

                        loc := gTerrain.GetClosestTile(fOrderLoc, Position, GetDesiredPassability, fUseExactTarget);

                        // No need to walk if we reached destination already
                        if loc <> fPositionRound then
                          SetActionWalkToSpot(loc, uaWalk);

                        fNextOrder := woNone;
                        fOrder := woWalk;
                      end;
                    end;
    woWalkOut:      ;
    woAttackUnit:   begin
                      if CanInterruptAction(fNextOrderForced) then
                        if not ( IsRanged and (fBoltCount <= 0)) then
                        begin
                          FreeAndNil(fTask); //e.g. TaskAttackHouse
                          fNextOrder := woNone;
                          fOrder := woAttackUnit;
                          fOrderLoc := GetOrderTargetUnit.Position;
                          FightEnemy(GetOrderTargetUnit);
                        end;
                    end;
    woAttackHouse:  begin
                      //No need to update order  if we are going to attack same house
                      if (fOrder <> woAttackHouse)
                          or (Task = nil)
                          or not (Task is TKMTaskAttackHouse)
                          or (TKMTaskAttackHouse(Task).House <> GetOrderHouseTarget) then
                      begin
                        //Abandon walk so we can take attack house
                        if (Action is TKMUnitActionWalkTo)
                          and not TKMUnitActionWalkTo(Action).DoingExchange
                          and not TKMUnitActionWalkTo(Action).WasPushed then
                          AbandonWalk;

                        //Take attack house order
                        if CanInterruptAction(fNextOrderForced) then
                        if not (IsRanged and (fBoltCount <= 0)) then
                        begin
                          FreeAndNil(fTask); //e.g. TaskAttackHouse
                          fTask := TKMTaskAttackHouse.Create(Self, GetOrderHouseTarget);
                          fOrder := woAttackHouse;
                          fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                          fNextOrder := woNone;
                        end;
                      end;
                    end;
    woStorm:        begin
                      //Abandon walk so we can take attack house or storm attack order
                      if (Action is TKMUnitActionWalkTo)
                        and not TKMUnitActionWalkTo(Action).DoingExchange then
                        AbandonWalk;

                      //Storm
                      if CanInterruptAction(fNextOrderForced) then
                      begin
                        FreeAndNil(fTask); //e.g. TaskAttackHouse
                        SetActionStorm(fStormDelay);
                        fNextOrder := woNone;
                        fOrder := woStorm;
                      end;
                    end;
    woAssignToShip:  begin
                      //No need to update order  if we are going to attack same house
                      if (fOrder <> woAssignToShip)
                          or (Task = nil)
                          or not (Task is TKMTaskAssignToShip)
                          or (TKMTaskAssignToShip(Task).Ship <> GetOrderShipTarget) then
                      begin
                        //Abandon walk so we can take attack house
                        if (Action is TKMUnitActionWalkTo)
                          and not TKMUnitActionWalkTo(Action).DoingExchange
                          and not TKMUnitActionWalkTo(Action).WasPushed then
                          AbandonWalk;

                        //Take attack house order
                        if CanInterruptAction(fNextOrderForced) then
                        begin
                          FreeAndNil(fTask); //e.g. TaskAttackHouse
                          fTask := TKMTaskAssignToShip.Create(Self, TKMUnitWarriorShip(GetOrderShipTarget));
                          fOrder := woAssignToShip;
                          fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                          fNextOrder := woNone;
                        end;
                      end;
                    end;
    woShipBoatUnload: if UnitType = utBoat then
                      begin
                        //No need to update order  if we are going to attack same house
                        if (fOrder <> woShipBoatUnload)
                            or (Task = nil)
                            or not (Task is TKMTaskUnloadWares)
                            or (TKMTaskUnloadWares(Task).ShipYard <> GetOrderHouseTarget) then
                        begin
                          //Abandon walk so we can take attack house
                          if (Action is TKMUnitActionWalkTo)
                            and not TKMUnitActionWalkTo(Action).DoingExchange
                            and not TKMUnitActionWalkTo(Action).WasPushed then
                            AbandonWalk;

                          //Take attack house order
                          if CanInterruptAction(fNextOrderForced) then
                          begin
                            FreeAndNil(fTask); //e.g. TaskAttackHouse
                            fTask := TKMTaskUnloadWares.Create(Self, TKMHouseShipYard(GetOrderHouseTarget) );
                            fOrder := woShipBoatUnload;
                            fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                            fNextOrder := woNone;
                          end;
                        end;

                      end else
                      begin
                      //No need to update order  if we are going to attack same house
                        if (fOrder <> woShipBoatUnload)
                            or (Task = nil)
                            or not (Task is TKMTaskUnloadFromShip)
                            or (TKMTaskUnloadFromShip(Task).Ship <> GetOrderShipTarget) then
                        begin
                          //Abandon walk so we can take attack house
                          if (Action is TKMUnitActionWalkTo)
                            and not TKMUnitActionWalkTo(Action).DoingExchange
                            and not TKMUnitActionWalkTo(Action).WasPushed then
                            AbandonWalk;

                          //Take attack house order
                          if CanInterruptAction(fNextOrderForced) then
                          if not (IsRanged and (fBoltCount <= 0)) then
                          begin
                            FreeAndNil(fTask); //e.g. TaskAttackHouse
                            fTask := TKMTaskUnloadFromShip.Create(Self, TKMUnitWarriorShip(GetOrderShipTarget));
                            fOrder := woShipBoatUnload;
                            fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                            fNextOrder := woNone;
                          end;
                        end;
                      end;
    woBoatCollectWares:   begin
                            //No need to update order  if we are going to attack same house
                            if (fOrder <> woBoatCollectWares)
                                or (Task = nil)
                                or not (Task is TKMTaskCollectWares) then
                            begin
                              //Abandon walk so we can take attack house
                              if (Action is TKMUnitActionWalkTo)
                                and not TKMUnitActionWalkTo(Action).DoingExchange
                                and not TKMUnitActionWalkTo(Action).WasPushed then
                                AbandonWalk;

                              //Take attack house order
                              if CanInterruptAction(fNextOrderForced) then
                              begin
                                FreeAndNil(fTask); //e.g. TaskAttackHouse
                                fTask := TKMTaskCollectWares.Create(Self);
                                fOrder := woBoatCollectWares;
                                fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                                fNextOrder := woNone;
                              end;
                            end;
                          end;
    woTakeOverHouse:  begin
                        //No need to update order  if we are going to take over same house
                        if (fOrder <> woTakeOverHouse)
                            or (Task = nil)
                            or not (Task is TKMTaskTakeOverHouse)
                            or (TKMTaskTakeOverHouse(Task).House <> GetOrderHouseTarget) then
                        begin
                          //Abandon walk so we can take over house
                          if (Action is TKMUnitActionWalkTo)
                            and not TKMUnitActionWalkTo(Action).DoingExchange
                            and not TKMUnitActionWalkTo(Action).WasPushed then
                            AbandonWalk;

                          //Take order
                          if CanInterruptAction(fNextOrderForced) then
                          begin
                            FreeAndNil(fTask); //e.g. TaskAttackHouse
                            fTask := TKMTaskTakeOverHouse.Create(Self, GetOrderHouseTarget);
                            fOrder := woTakeOverHouse;
                            fOrderLoc := Position; //Once the house is destroyed we will position where we are standing
                            fNextOrder := woNone;
                          end;
                        end;
                      end;
  end;
end;


//Warrior has walked out of the Barracks
procedure TKMUnitWarrior.WalkedOut(aHouse : TKMHouse; aIsTrained : Boolean);
begin
  //Report for duty (Groups will link us or create a new group)
  if Assigned(OnWarriorWalkOut) then
    OnWarriorWalkOut(Self, aHouse, aIsTrained);

  If gHands[Owner].HasPearl(ptAgros) then
    SetSpeed(4, true);

  If gHands[Owner].HasPearl(ptRalender) then
    fHitPointsMax := fHitPointsMax + 2;
end;


{procedure TKMUnitWarrior.ChaseTargetUnit;
begin
  if InFight or (GetOrderTarget = nil) or not CanInterruptAction then Exit;

  //--We don't take advantage of ChangeWalkTo yet for the sake of simplicity?
  if IsRanged then
  begin
    //Check target in range, and if not - chase it / back up from it
    if (KMLength(GetPosition, GetOrderTarget.GetPosition) > GetFightMaxRange) then
    begin
      //Too far away
      if (Action is TUnitActionWalkTo)
      and not TUnitActionWalkTo(Action).DoingExchange then
        TUnitActionWalkTo(Action).ChangeWalkTo(GetOrderTarget, GetFightMaxRange)
      else
      if CanInterruptAction then
        SetActionWalkToUnit(GetOrderTarget, GetFightMaxRange, uaWalk);
    end
    else
    if (KMLength(GetPosition, GetOrderTarget.GetPosition) < GetFightMinRange) then
    begin
      //todo: Archer is too close, back up
    end
    else
      //WithinRange
      FightEnemy(GetOrderTarget);
  end
  else
  //IsMelee
  begin
    if (Action is TUnitActionWalkTo)
    and not TUnitActionWalkTo(Action).DoingExchange then
      TUnitActionWalkTo(Action).ChangeWalkTo(GetOrderTarget, 1)
    else
    if CanInterruptAction then
      SetActionWalkToUnit(GetOrderTarget, 1, uaWalk);
  end;

  fOrder := woAttackUnit;
  fOrderLoc := GetOrderTarget.GetPosition;
end;}


function TKMUnitWarrior.ObjToStringShort(const aSeparator: String = '|'): String;
begin
  if Self = nil then Exit('nil');

  Result := inherited ObjToStringShort(aSeparator) +
            Format('%sWOrder = %s%sNextOrder = %s%sNextOrderForced = %s%sOrderLoc = %s%s' +
                   'HasOTargetU = [%s]%sHasOTargetH = [%s]%s'+
                   'RequestedFood = %s%s'+
                   'RequestedAmmo = %s%s'
                   ,
                   [aSeparator,
                    GetEnumName(TypeInfo(TKMWarriorOrder), Integer(fOrder)), aSeparator,
                    GetEnumName(TypeInfo(TKMWarriorOrder), Integer(fNextOrder)), aSeparator,
                    BoolToStr(fNextOrderForced, True), aSeparator,
                    TypeToString(fOrderLoc), aSeparator,
                    BoolToStr(fOrderTargetUnit <> nil, True), aSeparator,
                    BoolToStr(fOrderTargetHouse <> nil, True),aSeparator,
                    BoolToStr(fRequestedFood, true), aSeparator,
                    BoolToStr(fRequestedAmmo, true), aSeparator

                    ]);
end;


function TKMUnitWarrior.ObjToString(const aSeparator: String = '|'): String;
var
  unitStr, attackUStr, houseStr, groupStr: String;
begin
  if Self = nil then Exit('nil');

  groupStr := 'nil';
  unitStr := 'nil';
  houseStr := 'nil';
  attackUStr := 'nil';

  if fGroup <> nil then
    groupStr := TKMUnitGroup(fGroup).ObjToString('|  ');

  if fOrderTargetUnit <> nil then
    unitStr := fOrderTargetUnit.ObjToStringShort('; ');

  if fAttackingUnit <> nil then
    attackUStr := fAttackingUnit.ObjToStringShort('; ');

  if fOrderTargetHouse <> nil then
    houseStr := fOrderTargetHouse.ObjToStringShort('; ');

  Result := inherited ObjToString(aSeparator) +
            Format(
                   'RequestedFood = %s%s'+
                   'RequestedAmmo = %s%s'+
                  'FaceDir = %s%sOrderTargetU = [%s]%sAttackingU = [%s]%sOrderTargetH = [%s]%sGroup = %s%s'
                  ,
                   [BoolToStr(fRequestedFood, true), aSeparator,
                    BoolToStr(fRequestedAmmo, true), aSeparator,
                    GetEnumName(TypeInfo(TKMDirection), Integer(FaceDir)), aSeparator,
                    unitStr, aSeparator,
                    attackUStr, aSeparator,
                    houseStr, aSeparator,
                    groupStr, aSeparator
                    ]);
end;


function TKMUnitWarrior.UpdateState: Boolean;
var U : TKMUnit;
begin
  if fAction = nil then
    raise ELocError.Create(gRes.Units[UnitType].GUIName+' has no action at start of TKMUnitWarrior.UpdateState', fPositionRound);

  if IsDeadOrDying then
  begin
    Result := True; //Required for override compatibility
    inherited UpdateState;
    Exit;
  end;
  If fRageTime > 0 then
    Dec(fRageTime);
  UpdateOrderTargets;

  if fBitinAdded then
    Thought := thArmor;

  if fCondition < UNIT_MIN_CONDITION then
    fThought := thEat; //thDeath checked in parent UpdateState

  if not fNeverHungry then
    if fConditionPace >= 10 then
      if (fTicker mod fConditionPace = 0) then
      begin
        if not (fType in SIEGE_MACHINES) then
          case fOrder of
            woWalk: If KaMRandom(100, 'TKMUnitWarrior.UpdateState:Condition1') <= 50 then dec(fCondition);
            woAttackUnit,
            woAttackHouse: If KaMRandom(100, 'TKMUnitWarrior.UpdateState:Condition2') <= 80 then dec(fCondition);
            woStorm: dec(fCondition);
          end;
      end;

  //Part 1 - Take orders into execution if there are any
  //Part 2 - UpdateState
  //Part 3 -
  if (fType = utRam) and ((Action is TKMUnitActionWalkTo) or (Task is TKMTaskAttackHouse)) then
  begin
    if Position = PositionNext then
    begin
      Inc(fRamTicker);
      if fRamTicker >= 25 then
      begin
        fRamTicker := 0;
        case self.Direction of
          dirN: U := gHands.UnitsHitTest(Position.X, Position.Y - 1);
          dirNE: U := gHands.UnitsHitTest(Position.X + 1, Position.Y - 1);
          dirE: U := gHands.UnitsHitTest(Position.X + 1, Position.Y);
          dirSE: U := gHands.UnitsHitTest(Position.X + 1, Position.Y + 1);
          dirS: U := gHands.UnitsHitTest(Position.X, Position.Y + 1);
          dirSW: U := gHands.UnitsHitTest(Position.X - 1, Position.Y + 1);
          dirW: U := gHands.UnitsHitTest(Position.X - 1, Position.Y);
          dirNW: U := gHands.UnitsHitTest(Position.X - 1, Position.Y - 1);
          else
            U := nil;
        end;

        if (U <> nil) and (gHands[Owner].Alliances[U.Owner] <> atAlly) then
        begin
          U.SetHitTime;
          U.HitPointsDecrease(4, self);
          gScriptEvents.ProcUnitHit(U, self);
        end;
      end;
    end else
    fRamTicker := 0;
  end;

  if fNextOrder <> woNone then
    TakeNextOrder;

    if (fTicker mod 6 = 0) and not InFight then
      CheckForEnemy; //Split into separate procedure so it can be called from other places

  Result := True; //Required for override compatibility
  if inherited UpdateState then Exit;

  //Make sure we didn't get an action above
  if Action <> nil then
    Exit;

  SetActionStay(50, uaWalk);
end;


procedure TKMUnitWarrior.PaintUnit(aTickLag: Single);
var
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  unitPos: TKMPointF;
  fillColor, lineColor: Cardinal;
begin
  {if not (fType in [utSpy, utSpikedTrap, utMedic]) then
  begin}

  V := fVisual.GetLerp(aTickLag);

  act := V.Action;
  unitPos.X := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  unitPos.Y := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;


  if fRageTime > RAGE_TIME_DELAY then
  begin

    gRenderPool.AddAnimation(unitPos + KMPointF(0.3, -2.3), gRes.Units.RageAnim, fTicker,
                            IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), rxUnits,
                            false, false, 0, true);
  end;

  gRenderPool.AddUnit(fType, UID, act, V.Dir, V.AnimStep, V.AnimFraction, unitPos.X, unitPos.Y, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if (fThought <> thNone) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, fThought, unitPos.X, unitPos.Y)
  else
  if (gGameParams.IsMapEditor and BitinAdded) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, thArmor, unitPos.X, unitPos.Y);
    


  if SHOW_ATTACK_RADIUS or (mlUnitsAttackRadius in gGameParams.VisibleLayers) then
    if IsRanged then
    begin
      fillColor := $40FFFFFF;
      lineCOlor := icWhite;
      if (gMySpectator.Selected = Self)
        or ((gMySpectator.Selected is TKMUnitGroup)
          and (TKMUnitGroup(gMySpectator.Selected).FlagBearer = Self)) then
      begin
        fillColor := icRed and fillColor;
        lineColor := icCyan;
      end;

      gRenderPool.RenderDebug.RenderTiledArea(Position, GetFightMinRange, GetFightMaxRange, GetLength, fillColor, lineColor);
    end;
end;

constructor TKMUnitWarriorSpy.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
begin
  Inherited;

  fFlagColor := gHands[Owner].GameFlagColor;
end;

constructor TKMUnitWarriorSpy.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('WarriorSpy');
  LoadStream.Read(fFlagColor);
  LoadStream.Read(fAttackedPlayerTime);
end;

procedure TKMUnitWarriorSpy.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.PlaceMarker('WarriorSpy');
  SaveStream.Write(fFlagColor);
  SaveStream.Write(fAttackedPlayerTime);
end;

procedure TKMUnitWarriorSpy.SetAttackedTime;
begin
  fAttackedPlayerTime := gGameParams.Tick + 10 * 600; //Spy is now visible for every enemy dor 4 min
end;

function TKMUnitWarriorSpy.IsVisibleForEnemy: Boolean;
begin
  Result := fAttackedPlayerTime > gGameParams.Tick;
end;

function TKMUnitWarriorSpy.UpdateState: Boolean;
var I : Integer;
begin
  Result := Inherited;

  if not fVisible then exit;
  if fAction = nil then exit;

  if gGameParams.IsMapEditor then
  begin
    fFlagColor := gHands[Owner].GameFlagColor;
    Exit;
  end;

  if (fTicker mod 20 = 0) then
  begin
    //fFlagColor := gHands[Owner].GameFlagColor;
    if IsVisibleForEnemy then
      Exit;
    I := gAIFields.Influences.GetBestOwner(PositionNext.X, PositionNext.Y);
    if I = HAND_NONE then
      Exit;
    fFlagColor := gHands[I].GameFlagColor;
  end;

end;


procedure TKMUnitWarriorSpy.PaintUnit(aTickLag: Single);
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

  gRenderPool.AddUnit(fType, ID, act, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, fFlagColor, True);

  if fTask is TKMTaskDie then Exit; //Do not show unnecessary arms

  gRenderPool.AddUnit(UnitType, ID, uaWalkArm, V.Dir, V.AnimStep, V.AnimFraction, xPaintPos, yPaintPos, fFlagColor, False);

  if fThought in [thDeath,thEat] then
    gRenderPool.AddUnitThought(fType, act, V.Dir, fThought, xPaintPos, yPaintPos)
  else
  if gMySpectator.HandID = Owner then
  //if fFlagColor = gHands[Owner].GameFlagColor then
    gRenderPool.AddUnitThought(fType, act, V.Dir, thSpy, xPaintPos, yPaintPos);
end;

constructor TKMUnitWarriorPaladin.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
begin
  Inherited;
  //fRageDuration := 0;
  //fToRageTime := 0;
end;

function TKMUnitWarriorPaladin.GetDamageUnit: Word;
begin
  Result := Inherited;
  //Result := Result + 2 * Result * byte(fRageDuration > 0);
end;
function TKMUnitWarriorPaladin.GetDamageHouse: Word;
begin
  Result := Inherited;
  //Result := Result +  2 * Result * byte(fRageDuration > 0);
end;


function TKMUnitWarriorPaladin.GetDefence: SmallInt;
begin
  Result := Inherited;
  //Result := Result + 2 * Result * byte(fRageDuration > 0);
end;
function TKMUnitWarriorPaladin.GetAttack: SmallInt;
begin
  Result := Inherited;
  //Result := Result +  2 * Result * byte(fRageDuration > 0);
end;

function TKMUnitWarriorPaladin.UpdateState: Boolean;
begin
  Result := Inherited;

  If (fRageTime = 0) and InFight then
    SetRageTime(150);//15 secs of special damage and defense

  {if fRageDuration > 0 then
  begin
    Dec(fRageDuration);

    if fRageDuration = 0 then
      fToRageTime := 1200;
  end else
  begin
    if fToRageTime > 0 then
      Dec(fToRageTime);
    if (fToRageTime = 0) and InFight then
      fRageDuration := 150;//15 secs of special damage and defense

  end;}

end;

constructor TKMUnitWarriorPaladin.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  //LoadStream.Read(fRageDuration);
  //LoadStream.Read(fToRageTime);
end;

procedure TKMUnitWarriorPaladin.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  //SaveStream.Write(fRageDuration);
  //SaveStream.Write(fToRageTime);
end;

procedure TKMUnitWarriorPaladin.PaintUnit(aTickLag: Single);
{var
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  unitPos: TKMPointF;
  fillColor, lineColor: Cardinal; }
begin
  Inherited;
  {if not (fType in [utSpy, utSpikedTrap, utMedic]) then
  begin}

  {V := fVisual.GetLerp(aTickLag);

  act := V.Action;
  unitPos.X := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  unitPos.Y := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;
  }
  {if fRageDuration > 0 then
  begin

    gRenderPool.AddAnimation(unitPos + KMPointF(0.3, -2.3), gRes.Units.RageAnim, fTicker,
                            IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), rxUnits,
                            false, false, 0, true);
  end;}
  {gRenderPool.AddUnit(fType, UID, act, V.Dir, V.AnimStep, V.AnimFraction, unitPos.X, unitPos.Y, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if (fThought <> thNone) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, fThought, unitPos.X, unitPos.Y)
  else
  if (gGameParams.IsMapEditor and BitinAdded) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, thArmor, unitPos.X, unitPos.Y);



  if SHOW_ATTACK_RADIUS or (mlUnitsAttackRadius in gGameParams.VisibleLayers) then
    if IsRanged then
    begin
      fillColor := $40FFFFFF;
      lineCOlor := icWhite;
      if (gMySpectator.Selected = Self)
        or ((gMySpectator.Selected is TKMUnitGroup)
          and (TKMUnitGroup(gMySpectator.Selected).FlagBearer = Self)) then
      begin
        fillColor := icRed and fillColor;
        lineColor := icCyan;
      end;

      gRenderPool.RenderDebug.RenderTiledArea(Position, GetFightMinRange, GetFightMaxRange, GetLength, fillColor, lineColor);
    end;}
end;

constructor TKMUnitWarriorAmmoCart.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
var AT : TKMUnitAmmoType;
begin
  Inherited;
  for AT := Low(fAmmo) to High(fAmmo) do
    fAmmo[AT] := 0;
end;

constructor TKMUnitWarriorAmmoCart.Load(LoadStream: TKMemoryStream);
var AT : TKMUnitAmmoType;
begin
  inherited;
  for AT := Low(fAmmo) to High(fAmmo) do
  begin
    LoadStream.Read(fAmmo[AT]);
    LoadStream.Read(fAmmoRequested[AT]);
  end;

end;

procedure TKMUnitWarriorAmmoCart.Save(SaveStream: TKMemoryStream);
var AT : TKMUnitAmmoType;
begin
  Inherited;
  for AT := Low(fAmmo) to High(fAmmo) do
  begin
    SaveStream.Write(fAmmo[AT]);
    SaveStream.Write(fAmmoRequested[AT]);
  end;
end;

function TKMUnitWarriorAmmoCart.GetAmmoMinCount(aType: TKMUnitAmmoType) : Word;
begin
  case aType of
      uatArrow: Result := 100;
      uatRogueStone: Result := 100;
      uatStoneBolt: Result := 30;
      uatBolt: Result := 45;
    else
      Result := 999;
  end;
end;

function TKMUnitWarriorAmmoCart.GetAmmoMaxCount(aType: TKMUnitAmmoType) : Word;
begin
  case aType of
      uatArrow: Result := 1000;
      uatRogueStone: Result := 1000;
      uatStoneBolt: Result := 200;
      uatBolt: Result := 300;
    else
      Result := 0;
  end;
end;

function TKMUnitWarriorAmmoCart.TakeAmmo(aType : TKMUnitAmmoType; aCount: Word): Word;
begin

  if aCount > fAmmo[aType] then
    Result := fAmmo[aType]
  else
    Result := aCount;

  fAmmo[aType] := Max(fAmmo[aType] - Result, 0);

end;

function TKMUnitWarriorAmmoCart.GetAmmo(aType : TKMUnitAmmoType): Word;
begin
  Result := fAmmo[aType];
end;

procedure TKMUnitWarriorAmmoCart.SetAmmo(aType : TKMUnitAmmoType; aValue : Word);
begin
  fAmmo[aType] := aValue;
end;

procedure TKMUnitWarriorAmmoCart.ReloadAmmo(aWare : TKMWareType; doMax : Boolean = false);
begin
  Assert(aWare in [wtNone, wtBolt, wtQuiver, wtStoneBolt]);
  if aWare = wtNone then
  begin
    fAmmo[uatStoneBolt] := GetAmmoMaxCount(uatStoneBolt);
    fAmmo[uatBolt] := GetAmmoMaxCount(uatBolt);
  end else
  case aWare of
    wtQuiver :  begin
                  Inc(fAmmo[uatArrow], IfThen(doMax, GetAmmoMaxCount(uatArrow), 180 + KamRandom(40, 'TKMUnitWarriorAmmoCart.ReloadAmmo1') ));
                  if fAmmo[uatArrow] > GetAmmoMinCount(uatArrow) then
                    fAmmoRequested[uatArrow] := false;
                end;
    wtStoneBolt : begin
                    if fAmmoRequested[uatRogueStone] then
                    begin
                      Inc(fAmmo[uatRogueStone], IfThen(doMax, GetAmmoMaxCount(uatRogueStone), 180 + KamRandom(40, 'TKMUnitWarriorAmmoCart.ReloadAmmo2') ));
                      if fAmmo[uatRogueStone] > GetAmmoMinCount(uatRogueStone) then
                        fAmmoRequested[uatRogueStone] := false;

                    end else
                    begin
                      Inc(fAmmo[uatStoneBolt], IfThen(doMax, GetAmmoMaxCount(uatStoneBolt), 20));
                      if fAmmo[uatStoneBolt] > GetAmmoMinCount(uatStoneBolt) then
                        fAmmoRequested[uatStoneBolt] := false;
                    end;
                  end;
    wtBolt :    begin
                  Inc(fAmmo[uatBolt], IfThen(doMax, GetAmmoMaxCount(uatBolt), 30));
                  if fAmmo[uatBolt] > GetAmmoMinCount(uatBolt) then
                    fAmmoRequested[uatBolt] := false;
                end;
  end;
end;

procedure TKMUnitWarriorAmmoCart.OrderAmmo(aForceOrder : Boolean = false);
var AT : TKMUnitAmmoType;
  I : Integer;
begin
  for AT := Low(fAmmo) to High(fAmmo) do
    if AT in AMMOCART_AMMO then
      if (fAmmo[AT] <= GetAmmoMinCount(AT)) and not fAmmoRequested[AT] then
        case AT of
          uatArrow: begin
                      I := (GetAmmoMaxCount(AT) - fAmmo[AT]) div 100;
                      gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtQuiver, I, dtOnce, diHigh2);
                      fAmmoRequested[AT] := true;
                    end;
          uatRogueStone: begin
                          I := (GetAmmoMaxCount(AT) - fAmmo[AT]) div 10;
                          gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtStoneBolt, I, dtOnce, diHigh2);
                          fAmmoRequested[AT] := true;
                        end;
          uatStoneBolt: begin
                          I := (GetAmmoMaxCount(AT) - fAmmo[AT]) div 10;
                          gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtStoneBolt, I, dtOnce, diHigh2);
                          fAmmoRequested[AT] := true;
                        end;
          uatBolt:  begin
                      I := (GetAmmoMaxCount(AT) - fAmmo[AT]) div 15;
                      gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtBolt, I, dtOnce, diHigh2);
                      fAmmoRequested[AT] := true;
                    end;
        end;

end;

procedure TKMUnitWarriorAmmoCart.OrderAmmoType(aType : TKMUnitAmmoType);
var I : Integer;
begin
  if not CanOrderAmmoType(aType) then
    Exit;

  case aType of
    uatArrow: begin
                I := (GetAmmoMaxCount(aType) - fAmmo[aType]) div 100;
                gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtQuiver, I, dtOnce, diHigh2);
                fAmmoRequested[aType] := true;
              end;
    uatStoneBolt: begin
                    I := (GetAmmoMaxCount(aType) - fAmmo[aType]) div 10;
                    gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtStoneBolt, I, dtOnce, diHigh2);
                    fAmmoRequested[aType] := true;
                  end;
    uatBolt:  begin
                I := (GetAmmoMaxCount(aType) - fAmmo[aType]) div 15;
                gHands[Owner].Deliveries.Queue.AddDemand(nil, Self, wtBolt, I, dtOnce, diHigh2);
                fAmmoRequested[aType] := true;
              end;
  end;
end;

function TKMUnitWarriorAmmoCart.CanOrderAmmo : Boolean;
var AT : TKMUnitAmmoType;
begin
  Result := true;
  for AT := Low(fAmmo) to High(fAmmo) do
    if (fAmmo[AT] > GetAmmoMinCount(AT)) or fAmmoRequested[AT] then
      Exit(false);
end;

function TKMUnitWarriorAmmoCart.CanOrderAmmoType(aType: TKMUnitAmmoType): Boolean;
begin
  Result := (fAmmo[aType] <= GetAmmoMinCount(aType)) and not fAmmoRequested[aType];
end;

function TKMUnitWarriorAmmoCart.ObjToString(const aSeparator: string = '|'): string;
begin
  result := Inherited + Format( 'fAmmo[0] %d%s'+
                                'fAmmo[1] %d%s'+
                                'fAmmo[1] %d',
                                [fAmmo[uatArrow], aSeparator,
                                fAmmo[uatStoneBolt], aSeparator,
                                fAmmo[uatBolt]]
                              );
end;

procedure TKMUnitWarriorShipCommon.UpdateHitPoints(UseEffect : Boolean = true);
var H : TKMHouse;
begin
  if HITPOINT_RESTORE_PACE = 0 then Exit; //0 pace means don't restore

  if (fHitPointCounter mod (  HITPOINT_RESTORE_PACE) = 0)
  and (fHitPoints < HitPointsMax) then
  begin
    if not gTerrain.IsTileNearLand(Position) then
      Exit;

    H := gHands[Owner].GetClosestHouse(Position, [htShipYard], [wtLog], 8);
    if H.IsValid then
    begin
      H.WareTakeFromIn(wtLog, 1, true);
      Inc(fHitPoints);
    end;
  end;

  Inc(fHitPointCounter); //Increasing each tick by 1 would require 13,6 years to overflow Cardinal
end;


constructor TKMUnitWarriorShip.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
begin
  Inherited;
  Visible := true;

  fUnitsInside.Clear;
end;

constructor TKMUnitWarriorShip.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fUnloading);
  LoadStream.Read(fUnloadingPoint);
  LoadStream.Read(fLastUnloadTick);
  LoadStream.Read(fLastAsignementTick);
  LoadStream.Read(fStoredUnits);
  fUnitsInside.Clear;
  fUnitsInside.LoadFromStream(LoadStream);
  fUnitsFromMap.Clear;
  fUnitsFromMap.LoadFromStream(LoadStream);
end;

procedure TKMUnitWarriorShip.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fUnloading);
  SaveStream.Write(fUnloadingPoint);
  SaveStream.Write(fLastUnloadTick);
  SaveStream.Write(fLastAsignementTick);
  SaveStream.Write(fStoredUnits);
  fUnitsInside.SaveToStream(SaveStream);
  fUnitsFromMap.SaveToStream(SaveStream);
end;

procedure TKMUnitWarriorShip.SyncLoad;
begin
  Inherited;
  fUnitsInside.SyncLoad;
end;

function TKMUnitWarriorShip.GetClosestSpot: TKMPoint;
begin
  Result := gTerrain.FindWalkableSpot(Position);
end;

function TKMUnitWarriorShip.GetUnloadingPoint: TKMPoint;
begin
  Result := gTerrain.FindPlaceForUnit(self.Position, tpWalk, 5);
end;

function TKMUnitWarriorShip.IsCloseToWater: Boolean;
begin
  Result := gTerrain.IsTileNearLand(Position);
end;

procedure TKMUnitWarriorShip.OrderAmmo;
begin
  Inherited;
  //DoUnloadUnits;
end;

procedure TKMUnitWarriorShip.CloseUnit(aRemoveTileUsage: Boolean = True);
var I : Integer;
begin
  for I := 0 to fUnitsInside.Count - 1 do
  begin
    fUnitsInside[I].Kill(HAND_NONE, false, false);
    fUnitsInside[I].InShip := nil;
  end;
  fUnitsInside.Clear;
  Inherited;
end;
function TKMUnitWarriorShip.WarriorMustWait: Boolean;
begin
  Result := gGameParams.Tick <= fLastAsignementTick + 10;
end;

function TKMUnitWarriorShip.WarriorMustWaitToUnload: Boolean;
begin
  Result := gGameParams.Tick <= fLastUnloadTick + 10;
end;

function TKMUnitWarriorShip.CanAssignWarrior(aWarrior : Pointer) : Boolean;
//var I : Integer;
begin
  //Result := fUnitsInside.Count <= 10;
  if gRes.Units[TKMUnit(aWarrior).UnitType].ShipWeight < 0 then
    Exit(false);

  Result := fStoredUnits + gRes.Units[TKMUnit(aWarrior).UnitType].ShipWeight < SHIP_MAX_CAPACITY;
  {for I := 0 to fUnitsInside.Count - 1 do
    If fUnitsInside[I] = aWarrior then
      Exit(false);}
  Result := Result and gTerrain.IsTileNearLand(self.Position);

end;

procedure TKMUnitWarriorShip.AssignWarrior(aWarrior : Pointer);
//var U : TKMUnitMainData;
begin
  fLastAsignementTick := gGameParams.Tick;
  {U.aType := TKMUnit(aWarrior).UnitType;
  U.Condition := TKMUnit(aWarrior).Condition;

  if TKMUnit(aWarrior) is TKMUnitWarrior then
    U.IsWarrior := true
  else
    U.IsWarrior := false;

  If U.IsWarrior then
    U.BoltCount := TKMUnitWarrior(aWarrior).BoltCount;}


  fUnitsInside.Add(TKMUnit(aWarrior));
  TKMUnit(aWarrior).Hide;
  TKMUnit(aWarrior).InShip := self;
  fStoredUnits := fStoredUnits + gRes.Units[TKMUnit(aWarrior).UnitType].ShipWeight;
    //TKMUnit(aWarrior).Kill(HAND_NONE, false, true);
  //gTerrain.UnitRem(TKMUnit(aWarrior).Position);
end;

procedure TKMUnitWarriorShip.RemoveUnit(aUnit: TKMUnit; aForceByKill : Boolean = false);
var I, aIndex : Integer;
begin
  aIndex := -1;
  for I := 0 to fUnitsInside.Count do
    if aUnit = fUnitsInside[I] then
    begin
      aIndex := I;
      Break;
    end;
  Assert(aIndex <> -1, 'TKMUnitWarriorShip.RemoveUnit');

  fUnitsInside.Remove(aIndex);
  if not aForceByKill then
    fLastUnloadTick := gGameParams.Tick;

  fStoredUnits := fStoredUnits - gRes.Units[aUnit.UnitType].ShipWeight;
  TKMUnit(aUnit).InShip := nil;
end;

function TKMUnitWarriorShip.UnloadUnit(aUnit: TKMUnit; aLocTo : TKMPoint; aForceByKill : Boolean = false): Boolean;
begin
  Result := false;
  if IsNil(aUnit) then
    Exit;
  if WarriorMustWaitToUnload and not aForceByKill then
    Exit;
  if (aLocTo = KMPOINT_INVALID_TILE) and not aForceByKill then
    Exit;
  If not TKMUnit(aUnit).SetUnitPositionFromShip(aLocTo) then
    Exit;
  Result := true;
  RemoveUnit(aUnit);

end;

procedure TKMUnitWarriorShip.DoUnloadUnits;
var I : Integer;
//  U : TKMUnit;
begin
  {If TKMUnitGroup(fGroup).Count = 1 then
    Exit;}

  {with TKMUnitGroup(fGroup) do
    for I := 0 to Count - 1 do
    if (Members[I] <> nil) and not Members[I].IsDeadOrDying  then
      if Members[I] <> self then
      begin
        P := gTerrain.FindPlaceForUnit(Position, Members[I]);
        if P <> KMPOINT_INVALID_TILE then
        begin
          Members[I].SetUnitPositionFromShip(P);
          Members[I].OrderNone;
        end;

      end;}
  fUnloading := true;
  If fUnitsInside.Count = 0 then
    Exit;

  for I := 0 to fUnitsInside.Count - 1 do
    if not fUnitsInside[I].IsUnloadingFromShip then
      fUnitsInside[I].UnloadFromShip(self);



  {fUnloading := true;
  fUnloadingPoint := gTerrain.FindPlaceForUnit(self.Position, tpWalk, 5);

  if fUnloadingPoint = KMPOINT_INVALID_TILE then
    Exit;
  Exit;}
  {for I := 0 to fUnitsInside.Count - 1 do
    if gRes.Units[fUnitsInside[I].aType].IsValid then
    begin
      {fUnitsInside[I].SetUnitPositionFromShip(P);

      fUnitsInside[I].Visible := true;
      fUnitsInside[I].InShip := nil;}
      //gHands[Owner].DuplicateUnitAt(fUnitsInside[I], P, true);
      {gHands[Owner].AddUnit(fUnitsInside[I], fUnloadingPoint);
    end;
  fUnitsInside.Clear;}
end;

function TKMUnitWarriorShip.GetMapEdUnit(aIndex: Integer): TKMUnitMainData;
begin
  Assert(InRange(aIndex, 0, fUnitsFromMap.Count - 1), 'TKMUnitWarriorShip.GetMapEdUnit wrong ID: ' + IntToStr(aIndex));
  Result := fUnitsFromMap[aIndex];
end;

procedure TKMUnitWarriorShip.SetMapEdUnit(aIndex: Integer; aValue: TKMUnitMainData);
begin
  Assert(InRange(aIndex, 0, fUnitsFromMap.Count - 1), 'TKMUnitWarriorShip.SetMapEdUnit wrong ID: ' + IntToStr(aIndex));
  fUnitsFromMap[aIndex] := aValue;
end;

function TKMUnitWarriorShip.GetMapEdUnitCount: Byte;
begin
  Result := fUnitsFromMap.Count;
end;

procedure TKMUnitWarriorShip.AddMapEdUnit(aUnitType: TKMUnitType; aCondition: Integer; aBoltCount: Integer;
                                          aMembersCount: Integer; aColumnsCount: Integer);
var UD : TKMUnitMainData;
begin
  UD.UnitType := aUnitType;
  UD.Condition := aCondition;
  UD.Count := aMembersCount;
  UD.Columns := aColumnsCount;
  UD.BoltCount := aBoltCount;

  fUnitsFromMap.Add(UD);
end;

function TKMUnitWarriorShip.GetAllUnitsInside: TKMUnitPlan;
  function ContainsUnit(aUnit : TKMUnitType; aArray : TKMUnitPlan) : Integer;
  var I : Integer;
  begin
    Result := -1;
    for I := 0 to High(aArray) do
      if aArray[I].UnitType = aUnit then
        Exit(I);
  end;
var I, J, id : Integer;

begin
  SetLength(Result, 0);
  for I := 0 to fUnitsFromMap.Count - 1 do
  begin
    id := ContainsUnit(fUnitsFromMap[I].UnitType, Result);

    if id = -1 then
    begin
      J := length(Result);
      SetLength(Result, J + 1);
      Result[J].UnitType := fUnitsFromMap[I].UnitType;
      Result[J].Count := fUnitsFromMap[I].Count;
    end else
      Inc(Result[id].Count, fUnitsFromMap[I].Count);
  end;

  for I := 0 to fUnitsInside.Count - 1 do
  begin
    id := ContainsUnit(fUnitsInside[I].UnitType, Result);

    if id = -1 then
    begin
      J := length(Result);
      SetLength(Result, J + 1);
      Result[J].UnitType := fUnitsInside[I].UnitType;
      Result[J].Count := 1;
    end else
      Inc(Result[id].Count, 1);
  end;

end;


function TKMUnitWarriorShip.UpdateState: Boolean;
var P : TKMpoint;
  UT : TKMUnitMainData;
begin
  Result := Inherited;
  if gGameParams.Tick mod 25 <> 0 then
    Exit;

  If fUnitsFromMap.Count = 0 then
    Exit;
  //only for AI
  if gHands[Owner].IsComputer then
  begin
    if IsIdle then
      DoUnloadUnits;
  end;


  if not fUnloading then
    Exit;

  if not gTerrain.IsTileNearLand(Position) then
  begin
    fUnloading := false;
    Exit;
  end;
  P := gTerrain.FindPlaceForUnit(Position, tpWalk, 5);

  if P = KMPOINT_INVALID_TILE then //no free tile found, stop unloading
  begin
    fUnloading := false;
    Exit;
  end;
  UT := fUnitsFromMap[0];
  UT.Count := UT.Count - 1;
  gHands[Owner].AddUnit(fUnitsFromMap[0], P);
  Dec(fStoredUnits, gRes.Units[fUnitsFromMap[0].UnitType].ShipWeight);
  fStoredUnits := Max(fStoredUnits, 0);
  fUnitsFromMap[0] := UT;

  if (fUnitsFromMap[0].Count = 0) or gRes.Units[fUnitsFromMap[0].UnitType].IsWarrior then
    fUnitsFromMap.Remove(0);


  {if fUnitsInside[0] <> nil then
  begin
    fStoredUnits := fStoredUnits - gRes.Units[TKMUnit(fUnitsInside[0]).UnitType].ShipWeight;
    TKMUnit(fUnitsInside[0]).SetUnitPositionFromShip(gTerrain.FindPlaceForUnit(fUnloadingPoint, tpWalk, 5));
    //TKMUnit(fUnitsInside[0]).Show;
    TKMUnit(fUnitsInside[0]).InShip := nil;
    //gHands[Owner].DuplicateUnitAt(fUnitsInside[0], fUnloadingPoint, true);
  end;
  fUnitsInside.Remove(0);

  if fUnitsInside.Count = 0 then
  begin
    fUnloading := false;
    fStoredUnits := 0;
  end;}

end;

constructor TKMUnitWarriorSpikedTrap.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
begin
  Inherited;
  fStayTime := 0;
end;

constructor TKMUnitWarriorSpikedTrap.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fStayTime);
  LoadStream.Read(fAnimKill);
end;

function TKMUnitWarriorSpikedTrap.HitDelay: Byte;
begin
  Result := 15;
end;

procedure TKMUnitWarriorSpikedTrap.HitEnemies;
  function TryKillUnit(aUnit : TKMUnit) : Boolean;
  begin
    Result := false;
    if aUnit = nil then
      Exit;
    if aUnit.IsDeadOrDying then
      Exit;

    if not aUnit.IsAnimal then
      if (aUnit.Owner = self.Owner) or (gHands[Owner].Alliances[aUnit.Owner] = atAlly) then
        Exit;
    aUnit.Kill(Owner, true, false);
    Result := true;
  end;
var
  U : TKMUnit;
  I, K : Integer;
begin
  for I := -1 to 1 do
    for K := -1 to 1 do
    if not ((I = 0) and (K = 0) )then
      if gTerrain.TileInMapCoords(Position.X + I, Position.Y + K) then
      begin
        U := gTerrain.Land[Position.Y + K, Position.X + I].IsUnit;
        TryKillUnit(TKMUnit(U));
      end;


end;

procedure TKMUnitWarriorSpikedTrap.CheckForEnemy;
  function TryKillUnit(aUnit : TKMUnit) : Boolean;
  begin
    Result := false;
    if aUnit = nil then
      Exit;
    if aUnit.IsDeadOrDying then
      Exit;
    if not aUnit.IsAnimal then
      if (aUnit.Owner = self.Owner) or (gHands[Owner].Alliances[aUnit.Owner] = atAlly) then
        Exit;
    Result := true;
  end;
var
  U : TKMUnit;
  DidHit : Boolean;
  I, K : Integer;
begin
  fAnimKill := 0;
  if (fStayTime / 250 * 9) <> 9 then
    Exit;
  DidHit := false;
  //if byte(Direction) mod 2 = 0 then
  for I := -1 to 1 do
    for K := -1 to 1 do
    if not ((I = 0) and (K = 0) )then
      if gTerrain.TileInMapCoords(Position.X + I, Position.Y + K) then
      begin
        U := gTerrain.Land[Position.Y + K, Position.X + I].IsUnit;
        DidHit := DidHit or  TryKillUnit(TKMUnit(U));
        if DidHit then
          Break;
      end;

  if didHit then
    fAnimKill := 1;
end;

procedure TKMUnitWarriorSpikedTrap.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fStayTime);
  SaveStream.Write(fAnimKill);
end;

function TKMUnitWarriorSpikedTrap.UpdateState: Boolean;
var I : Integer;
begin
  Result := Inherited;

  if IsIdle then
  begin
    fStayTime := EnsureRange(fStayTime + 1, 0, 250);
    for I := 1 to TKMUnitGroup(Group).UnitTypeCount(utMedic) do
      fStayTime := EnsureRange(fStayTime + 1, 0, 250);
  end
  else
    fStayTime := 0;

  if fAnimKill = 0 then
  begin
    if fTicker mod 10 = 0 then
      CheckForEnemy
  end else
  begin
    Inc(fAnimKill);
    if fAnimKill + 9 = 13 + HitDelay then
    begin
      fAnimKill := 0;
      fStayTime := 0;
      HitEnemies;
    end;
  end;
end;

function TKMUnitWarriorSpikedTrap.IsVisible: Boolean;
begin
  Result := fStayTime <> 250;
end;

procedure TKMUnitWarriorSpikedTrap.PaintUnit(aTickLag: Single);
Const OPEN_FRAME = 9;

var
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  unitPos: TKMPointF;
  fillColor, lineColor, animStep: Cardinal;
  AlphaStep : Single;
  dir : TKMDirection;
begin
  V := fVisual.GetLerp(aTickLag);

  act := V.Action;
  unitPos.X := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  unitPos.Y := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  AlphaStep := fStayTime / 250;

  animStep := ifThen(fStayTime > 0, Min(Round(AlphaStep * OPEN_FRAME), OPEN_FRAME), V.AnimStep);

  if fAnimKill > HitDelay then
    animStep := OPEN_FRAME + fAnimKill - HitDelay;

  dir := V.Dir;
  AlphaStep := AlphaStep * IfThen(gMySpectator.HandID = Owner, 0.4, 0.8) - 1;

  if fStayTime > 0 then
    if Byte(Direction) mod 2 = 0 then
      dir := DirNE
    else
      dir := dirN;
  if fStayTime > 0 then
    act := uaWork;

  gRenderPool.AddUnit(fType, UID, act, dir, animStep, V.AnimFraction, unitPos.X, unitPos.Y, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True, false, false, 0, AlphaStep);

  if (fThought <> thNone) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, fThought, unitPos.X, unitPos.Y)
  else
  if (gGameParams.IsMapEditor and BitinAdded) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, thArmor, unitPos.X, unitPos.Y);



  if SHOW_ATTACK_RADIUS or (mlUnitsAttackRadius in gGameParams.VisibleLayers) then
    if IsRanged then
    begin
      fillColor := $40FFFFFF;
      lineCOlor := icWhite;
      if (gMySpectator.Selected = Self)
        or ((gMySpectator.Selected is TKMUnitGroup)
          and (TKMUnitGroup(gMySpectator.Selected).FlagBearer = Self)) then
      begin
        fillColor := icRed and fillColor;
        lineColor := icCyan;
      end;

      gRenderPool.RenderDebug.RenderTiledArea(Position, GetFightMinRange, GetFightMaxRange, GetLength, fillColor, lineColor);
    end;
end;

constructor TKMUnitWarriorMedic.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHealingTick);
  LoadStream.Read(fIsHealing);
end;

procedure TKMUnitWarriorMedic.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fHealingTick);
  SaveStream.Write(fIsHealing);
end;

function TKMUnitWarriorMedic.UpdateState: Boolean;
var tmp : Boolean;
  //I : Integer;

begin
  Result := inherited;

  tmp := fIsHealing;

  if Group <> nil then
  begin
    fIsHealing := TKMUnitGroup(Group).HasDamagedUnits(false);
    If TKMUnitGroup(Group).InFight(false) then
      BlockWalking := true
    else
      BlockWalking := false;

  end
  else
    fIsHealing := false;
  if fIsHealing <> tmp then
  begin
    if fIsHealing then
    begin
      fHealingTick := fTicker;
      OrderStay(true);
      BlockWalking := true;
    end
    else
    begin
      fHealingTick := 0;
      OrderStay(true);
      BlockWalking := false;
    end;
  end;
  if fIsHealing then
  begin
    if PositionPrev = self.PositionNext then
      if (Action <> nil) and Action.CanBeInterrupted(true) then
        SetActionLockedStay(64, uaSpec, false)
    else
    else
      OrderStay(true);
  end;
end;

procedure TKMUnitWarriorMedic.PaintUnit(aTickLag: Single);
var
  V: TKMUnitVisualState;
  act: TKMUnitActionType;
  unitPos: TKMPointF;
  animStep: Cardinal;
  dir : TKMDirection;
begin

  V := fVisual.GetLerp(aTickLag);

  if (fHealingTick > 0) then
  begin
    act := uaSpec;
    animStep := fTicker - fHealingTick;
  end
  else
  begin
    act := V.Action;
    animStep := V.AnimStep;
  end;

  unitPos.X := V.PositionF.X + UNIT_OFF_X + V.SlideX;
  unitPos.Y := V.PositionF.Y + UNIT_OFF_Y + V.SlideY;

  dir := V.Dir;
  gRenderPool.AddUnit(fType, UID, act, dir, animStep, V.AnimFraction, unitPos.X, unitPos.Y, IfThen(FlagColor <> 0, FlagColor, gHands[Owner].GameFlagColor), True);

  if (fThought <> thNone) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, fThought, unitPos.X, unitPos.Y)
  else
  if (gGameParams.IsMapEditor and BitinAdded) then
    gRenderPool.AddUnitThought(fType, act, V.Dir, thArmor, unitPos.X, unitPos.Y);

end;


procedure TKMUnitWarriorBShip.OrderAmmo(aForceOrder: Boolean = False);
var HS : TKMHouse;//shipyard
begin

  if fBoltCount >= 500 then
    Exit;

  if not gTerrain.IsTileNearLand(Position) then
    Exit;
  HS := gHands[Owner].GetClosestHouse(Position, [htShipYard], [wtBolt], 8);

  if not HS.IsValid then
    Exit;
  if not HS.HasWorkerInside then
    Exit;


  HS.WareTakeFromIn(wtBolt, 1, true);
  Inc(fBoltCount, 50)
end;

constructor TKMUnitWarriorBoat.Create(aID: Cardinal; aUnitType: TKMUnitType; const aLoc: TKMPointDir; aOwner: ShortInt; aInHouse: TKMHouse);
begin
  Inherited;

end;

constructor TKMUnitWarriorBoat.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  fWares.Load(LoadStream);
  LoadStream.Read(fIdleTimer);
end;

procedure TKMUnitWarriorBoat.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  fWares.Save(SaveStream);
  SaveStream.Write(fIdleTimer);
end;

procedure TKMUnitWarriorBoat.AddWare(aWare: TKMWareType; aCount: Integer);
begin
  if not (aWare in WARES_VALID) then
    Exit;
  if aCount = 0 then
    Exit;

  fWares.AddWare(aWare, aCount);
end;

function TKMUnitWarriorBoat.AddWare(aWare: TKMWarePlanSingle) : Boolean;
var I : Integer;
begin
  Result := true;
  if (aWare.C = 0) or (aWare.W = wtNone) then
    Exit(false);

  AddWare(aWare.W, aWare.C);

  if aWare.W <> wtFish then
    for I := 1 to aWare.C do
      TakeBolt;
  if TotalWaresCount > 20 then
    SetCollectingWares(false);
end;

procedure TKMUnitWarriorBoat.AddVWare(aObject: Word);
begin
  if aObject = 255 then
    Exit;
  TakeBolt;
  gHands[Owner].AddJewerly(aObject);
end;

function TKMUnitWarriorBoat.HasAnyWares: Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWares.Count - 1 do
    Result := Result or ((fWares[I].W <> wtNone) and (fWares[I].C > 0));
end;

function TKMUnitWarriorBoat.TotalWaresCount: Word;
var I : Integer;
begin
  Result := 0;
  for I := 0 to High(fWares) do
    if (fWares[I].W <> wtNone) then
      Inc(Result, fWares[I].C);
end;

procedure TKMUnitWarriorBoat.StartCollectingWares;
begin
  if fTask is TKMTaskCollectWares then
    Exit;
  if fCollectWares or fCollectFish and not (fTask is TKMTaskCollectWares) then
  begin
    if TotalWaresCount < 20 then
      if not (fTask is TKMTaskCollectWares) then
        fNextOrder := woBoatCollectWares;
      {
      begin
        CancelTask;
        fTask := TKMTaskCollectWares.Create(self);
      end;}
  end;
end;

procedure TKMUnitWarriorBoat.SetCollectingWares(aValue: Boolean);
begin
  if TotalWaresCount >= 20 then
  begin
    fCollectWares := false;
    fCollectFish := false;
    Exit;
  end;

  fCollectWares := aValue;

  StartCollectingWares;
end;

procedure TKMUnitWarriorBoat.SetCollectingFish(aValue: Boolean);
begin
  if TotalWaresCount >= 20 then
  begin
    fCollectWares := false;
    fCollectFish := false;
    Exit;
  end;

  fCollectFish := aValue;
  StartCollectingWares;
end;

function TKMUnitWarriorBoat.GetCanCollectWares: Boolean;
begin
  Result := fCollectWares and (fBoltCount > 0);
end;

procedure TKMUnitWarriorBoat.UnloadWares;
var H : TKMHouse;
begin
  if HasAnyWares then
    if gTerrain.IsTileNearLand(Position) then
    begin
      H := gHands.GetClosestHouse(Position, [htShipYard], [wtAll], 7);
      if H.IsValid(htShipYard, false, true) then
      begin
        fNextOrder := woShipBoatUnload;
        SetOrderHouseTarget(H);

        Exit;
      end;
    end;
end;

procedure TKMUnitWarriorBoat.UnloadWare(aShipyard: TKMHouse);
var I : Integer;
begin
  if (aShipyard = nil) or (aShipyard.IsDestroyed) then
    Exit;
  if not HasAnyWares then
    Exit;

  for I := 0 to fWares.Count - 1 do
    if (fWares[I].C > 0) and (fWares[I].W in WARES_VALID) then
    begin
      aShipyard.WareAddToOut(fWares[I].W, Min(fWares[I].C, 3));
      fWares[I].C := Max(fWares[I].C - 3, 0);
      if fWares[I].C = 0 then
        fWares[I].W := wtNone;
      Exit;
    end;

end;


procedure TKMUnitWarriorBoat.OrderAmmo(aForceOrder: Boolean = False);
var HS : TKMHouse;//shipyard
begin

  if fBoltCount >= 100 then
    Exit;
  if not gTerrain.IsTileNearLand(Position) then
    Exit;

  HS := gHands[Owner].GetClosestHouse(Position, [htShipYard], [wtAxe], 8);

  if not HS.IsValid then
    Exit;
  if not HS.HasWorkerInside then
    Exit;


  HS.WareTakeFromIn(wtAxe, 1, true);
  Inc(fBoltCount, 20)
end;

function TKMUnitWarriorBoat.UpdateState: Boolean;
begin
  Result := Inherited;
  if IsIdle then
  begin
    IncLoop(fIdleTimer, 1, 200);
    //fIdleTimer := Min(fIdleTimer + 1, high(byte));

    if fIdleTimer mod 50 = 0 then
      StartCollectingWares;


  end
  else
    fIdleTimer := 0;
end;

procedure TKMUnitWarriorLekter.OrderAttackHouse(aTargetHouse: TKMHouse; aForced: Boolean = True);
begin
  fNextOrder := woTakeOverHouse;
  fNextOrderForced := aForced;
  SetOrderHouseTarget(aTargetHouse);
end;

end.
