unit KM_ResUnits;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  JsonDataObjects, KM_JsonHelpers,
  KM_ResTypes;


type
  // Used to separate close-combat units from archers (they use different fighting logic)
  TKMFightType = (ftMelee, ftRanged, ftBoth);

  TKMUnitMoveType = (umtWalk, umtWalkDiag, umtStorm, umtStormDiag);
  TKMUnitDat = packed record
    HitPoints, Attack, AttackHorse, x4, Defence, Speed, x7, Sight: SmallInt;
    x9, x10: ShortInt;
    CanWalkOut, x11: SmallInt;
  end;
  TKMUnitSpriteSingle = packed record
      Dir: array [dirN..dirNW] of TKMAnimLoop;
      procedure Clear;
  end;
  //this is old TKMUnitSprite;
  TKMUnitSprite = packed record
    Act: array [TKMUnitActionType] of TKMUnitSpriteSingle;
  end;

  TKMUnitSpriteNew = array [TKMUnitActionType, dirN..dirNW] of TKMAnimation;//TKMAnimation can have more steps

  TKMUnitSpecInfo = record
    StepsPerTile: Byte;
    StepsPerTileDiag: Byte;
    StepsPerTileStorm: Byte;
    StepsPerTileStormDiag: Byte;
  end;

  TKMUnitShipSketch = array[dirN..dirNW] of TKMAnimLoop;

  TKMUnitSprite2 = array [1..18] of SmallInt; //Sound indices vs sprite ID

  TKMUnitSpec = class
  private
    fUnitType: TKMUnitType;
    fUnitDat: TKMUnitDat;
    fUnitSpecInfo: TKMUnitSpecInfo;
    fUnitSprite: TKMUnitSprite;
    fUnitSprite2: TKMUnitSprite2;
    fUnitSpriteNew : TKMUnitSpriteNew;

    fHouses : TKMHouseTypeArray;
    fTrainingHouses : TKMHouseTypeSet;
    function GetAllowedPassability: TKMTerrainPassability;
    function GetDescription: UnicodeString;
    function GetDesiredPassability: TKMTerrainPassability;
    function GetFightType: TKMFightType;
    function GetGUIIcon: Word;
    function GetGUIScroll: Word;
    function GetMinimapColor: Cardinal;
    function GetMiningRange: Byte;
    function GetSpeed: Single;
    function GetUnitAnim(aAction: TKMUnitActionType; aDir: TKMDirection): TKMAnimation;
    function GetUnitTextID: Integer;
    function GetUnitName: UnicodeString;
    function GetWorkerHouses : TKMHouseTypeArray;
    function GetUnitPower : Word;
    Function GetCanOrderAmmo : Boolean;
    Function GetAmmoType : TKMUnitAmmoType;
    function GetMinFightRange : Single;
    function GetMaxFightRange : Single;
    function GetShipWeight : Single;

  public
    ProjectileDefence : Single;
    HouseDamage, UnitDamage : Word;
    BarracksCost : array of record
      W : TKMWareType;
      C : Integer;
    end;
    PalaceCost : TKMVWarePlan;
    TownhallCost : Byte;
    SiegePhasesCount : Byte;
    SiegeCost : TKMWarePlan;
    AmmoType : TKMUnitAmmoType;
    CanOrderAmmo,
    CanStorm,
    CanAttackUnits,
    CanAttackHouses : Boolean;
    FightType : TKMFightType;
    MinRange, MaxRange : Single;
    DescriptionID : Word;
    StrikeSteps : array of Byte;
    ProducesWares : TKMWareTypeSet;
    ShipWeight : Single;
    SchoolTime : Byte;
    constructor Create(aType: TKMUnitType);
    function IsValid: Boolean;
    function IsAnimal: Boolean;
    function IsCitizen: Boolean;
    function IsWarrior: Boolean;
    function IsWarriorEquipable: Boolean;
    function GetHouseDamage: Integer;
    function GetUnitDamage: Integer;
    function GetTrainingHouse : TKMHouseType;
    function GetDefenceVsProjectiles(aIsBolt: Boolean): Single;

    procedure LoadFromStream(Stream: TMemoryStream);
    //Derived from KaM
    property HitPoints: SmallInt read fUnitDat.HitPoints;
    property Attack: SmallInt read fUnitDat.Attack;
    property AttackHorse: SmallInt read fUnitDat.AttackHorse;
    property Defence: SmallInt read fUnitDat.Defence;
    property Description: UnicodeString read GetDescription;
    property Sight: SmallInt read fUnitDat.Sight;
    property AbsSpeed: SmallInt read fUnitDat.Speed;
    property TrainingHouses : TKMHouseTypeSet read fTrainingHouses;
    function Damage : Word;
    //Additional properties added by Remake
    property AllowedPassability: TKMTerrainPassability read GetAllowedPassability;
    property DesiredPassability: TKMTerrainPassability read GetDesiredPassability;
    property GUIIcon: Word read GetGUIIcon;
    property GUIScroll: Word read GetGUIScroll;
    property MinimapColor: Cardinal read GetMinimapColor;
    property MiningRange: Byte read GetMiningRange;
    property Speed: Single read GetSpeed;
    function GetEffectiveSpeed(aMovementType: TKMUnitMoveType): Single;
    function GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
    function GetEffectiveStormSpeed(aIsDiag: Boolean): Single;
    function SupportsAction(aAct: TKMUnitActionType): Boolean;
    property UnitAnim[aAction: TKMUnitActionType; aDir: TKMDirection]: TKMAnimation read GetUnitAnim;
    property GUIName: UnicodeString read GetUnitName;
    property GUITextID: Integer read GetUnitTextID;

    property WorkerOfHouses : TKMHouseTypeArray read GetWorkerHouses;

    class function IsMelee(aUnitType: TKMUnitType): Boolean;
    class function IsMounted(aUnitType: TKMUnitType): Boolean;
    class function IsAntihorse(aUnitType: TKMUnitType): Boolean;
    class function IsRanged(aUnitType: TKMUnitType): Boolean;
    procedure LoadFromJson(aUnit : TKMJson);
  end;


  TKMResUnits = class
  private
    fCRC: Cardinal;
    fItems: array [TKMUnitType] of TKMUnitSpec;
    fSerfCarry: array [WARE_MIN..wtFish, dirN..dirNW] of TKMAnimLoop;
    fSerfCarryNew: array [wtBitin..WARE_MAX, dirN..dirNW] of TKMAnimLoop;

    function LoadUnitsDat(const aPath: UnicodeString): Cardinal;
    function GetItem(aType: TKMUnitType): TKMUnitSpec; inline;
    function GetSerfCarry(aType: TKMWareType; aDir: TKMDirection): TKMAnimLoop;
    procedure CalculateTroopTrainOrder;
  public
    BootsAnim : TKMAnimLoop;
    FishermansShipSketch,
    BattleShipSketch,
    ShipSketch : TKMUnitShipSketch;
    RageAnim,
    Explosion : TKMAnimation;
    constructor Create;
    destructor Destroy; override;


    property Items[aType: TKMUnitType]: TKMUnitSpec read GetItem; default;
    property SerfCarry[aType: TKMWareType; aDir: TKMDirection]: TKMAnimLoop read GetSerfCarry;
    property CRC: Cardinal read fCRC; //Return hash of all values

    procedure ExportCSV(const aPath: UnicodeString);
    procedure SetUnitHousesList;
    procedure SaveCustomData(aSaveStream: TKMemoryStream);
    procedure LoadCustomData(aLoadStream: TKMemoryStream);
    function LoadFromJson(aPath : String) : Cardinal;
    Procedure ReloadJSONData(UpdateCRC: Boolean);
  end;

const
  //This is a map of the valid values for !SET_UNIT,
  //TSK did not had place for new warriors that were inserted in the middle(!)
  UNIT_OLD_ID_TO_TYPE: array[0..46] of TKMUnitType = (
    utSerf,utWoodcutter,utMiner,utAnimalBreeder,utFarmer,
    utCarpenter,utBaker,utButcher,utFisher,utBuilder,
    utStonemason,utSmith,utMetallurgist,utRecruit, //Units
    utMilitia,utAxeFighter,utSwordFighter,utBowman,utCrossbowman,
    utLanceCarrier,utPikeman,utScout,utKnight,utBarbarian, //Troops
    utWolf,utFish,utWatersnake,utSeastar,utCrab,
    utWaterflower,utWaterleaf,utDuck, utRam, utGolem, utGiant, utOperator, utClayPicker,
    utDeerMale, utDeerFemale, utFox, utBoar, utBear, utLandDuck, utRabbit,
    utWhiteBear, utSandSnake, utSpider); //Animals

  //and the corresponing unit that will be created (matches KaM behavior)
  UNIT_TYPE_TO_OLD_ID: array[TKMUnitType] of integer = (
    -1, -1, //utNone, utAny
    0,1,2,3,4,5,6,7,8,9,10,11,12,13, //Citizens
    35, 36,
    14,15,16,17,18,19,20,21,22,23, //Warriors
    -1,-1,-1,-1, -1,-1, -1, -1, -1, -1, -1, - 1, -1, -1, -1, -1, //TPR warriors (can't be placed with SET_UNIT)
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    24,25,26,27,28,29,30,31, 37, 38, 39, 40, 41, 42, 43,
    44, 45, 46); //Animals

  //This is a map of the valid values for !SET_GROUP, and the corresponing unit that will be created (matches KaM behavior)
  UNIT_ID_TO_TYPE: array[0..75] of TKMUnitType = (
    utSerf,utWoodcutter,utMiner,utAnimalBreeder,utFarmer,
    utCarpenter,utBaker,utButcher,utFisher,utBuilder,
    utStonemason,utSmith,utMetallurgist,utRecruit, //Units
    utMilitia,utAxeFighter,utSwordFighter,utBowman,utCrossbowman,
    utLanceCarrier,utPikeman,utScout,utKnight,utBarbarian, //TSK Troops
    utRebel,utRogue,utWarrior,utVagabond,
    utCatapult,utBallista, //Placeholder for Seige weapons
    utWolf, utFish, utWatersnake, utSeastar, utCrab,
    utWaterflower, utWaterleaf, utDuck, utRam, utGolem,
    utGiant,utPaladin, utArcher, utSpy, utTrainedWolf, utAmmoCart,
    utPikeMachine, utShip,
    utClubMan, utMaceFighter, utFlailFighter,
    utOperator, utShieldBearer, utFighter,
    utSpikedTrap, utWoodenWall, utClayPicker,
    utTorchMan, utMedic, utBattleShip, utBoat,
    utPyro, utLekter, utDeerMale, utDeerFemale, utFox, utBoar, utBear,
    utLandDuck, utRabbit,utWhiteBear, utSandSnake, utSpider,
    utNone, utNone, utNone
    );

  UNIT_TYPE_TO_ID: array[TKMUnitType] of ShortInt = (
    -1, -1, //utNone, utAny
    0,1,2,3,4,5,6,7,8,9,10,11,12,13, //Citizens
    51,56,
    14,15,16,17,18,19,20,21,22,23, //Warriors
    24,25,26,27, 28,29,//TPR warriors
    38, 39, 40, 41, 42, 43, 44, 45, 46, 47,//My Warriors
    48, 49, 50, 52, 53, 54, 55, 57, 58, 59, 60,
    61, 62,//pyro, lekter
    30,31,32,33,34,35,36,37, //Animals
    63, 64, 65, 66, 67, 68, 69, 70, 71, 72);//deerMale ..


  //Number means ResourceType as it is stored in Barracks, hence it's not rtSomething
  TROOP_COST: array [utMilitia..utKnight, 1..4] of TKMWareType = (
    (wtAxe,          wtNone,        wtNone,  wtNone ), //Militia
    (wtWoodenShield,       wtLeatherArmor,       wtAxe,   wtNone ), //Axefighter
    (wtIronShield,  wtIronArmor,  wtSword, wtNone ), //Swordfighter
    (wtLeatherArmor,        wtBow,         wtNone,  wtNone ), //Bowman
    (wtIronArmor,   wtCrossbow,     wtNone,  wtNone ), //Crossbowman
    (wtLeatherArmor,        wtLance,        wtNone,  wtNone ), //Lance Carrier
    (wtIronArmor,   wtPike,   wtNone,  wtNone ), //Pikeman
    (wtWoodenShield,       wtLeatherArmor,       wtAxe,   wtHorse), //Scout
    (wtIronShield,  wtIronArmor,  wtSword, wtHorse)  //Knight
  );


  //The frame shown when a unit is standing still in uaWalk. Same for all units!
  UNIT_STILL_FRAMES: array [TKMDirection] of Byte = (0,3,2,2,1,6,7,6,6);
  SERF_WARE_SPEED_DECREASE: array[TKMWareType] of Byte =
  ( 0,
    2, 5, 2, 3, 3,
    3, 5, 4, 3, 1,
    1, 3, 1, 1, 6,
    1, 2, 3, 2, 3,
    2, 2, 2, 3, 2,
    3, 15, 3, 6, 1,
    3, 3, 3, 3, 3,
    2, 1, 1, 2, 3,
    3, 3, 3, 1, 1,
    1, 2, 3, 1, 2,
    3, 1,
    0,0,0
  );
  UNITS_WITH_BOOTS : set of TKMUnitType = [utSerf, utWoodcutter, utStonemason, utFisher, utBuilder, utFarmer];
  var
  //TownHall default units troops cost (number of gold chests needed)
  //Could be modified by script functions
  SHIPYARD_ORDER,
  SIEGE_GAME_ORDER,
  SCHOOL_GAME_ORDER,
  BARRACKS_GAME_ORDER,
  TH_GAME_ORDER,
  PALACE_UNITS_ORDER : TKMUnitTypeArray;

  AI_TROOP_TRAIN_ORDER_NEW : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMUnitTypeArray;

implementation
uses
  KromUtils, KM_ResTexts, KM_CommonUtils, KM_CommonClassesExt, KM_Resource,
  KM_JSONUtils, KM_CommonShellUtils,
  Math, Windows;

const
  STORM_SPEEDUP = 1.5;


{ TKMUnitsDatClass }
constructor TKMUnitSpec.Create(aType: TKMUnitType);
var I : Integer;
begin
  inherited Create;
  fUnitType := aType;


  PalaceCost.Plan.SetCount(4);
  SiegeCost.SetCount(4);
  AmmoType := GetAmmoType;
  CanOrderAmmo := GetCanOrderAmmo;
  CanAttackUnits := aType in UNITS_WARRIORS;
  CanAttackHouses := aType in UNITS_WARRIORS;
  FightType := GetFightType;
  HouseDamage := self.GetHouseDamage;
  UnitDamage := self.GetUnitDamage;

  MinRange := GetMinFightRange;
  MaxRange := GetMaxFightRange;
  ShipWeight := GetShipWeight;
  SchoolTime := 30;
  SetLength(BarracksCost, 0);
  if fUnitType in [utMilitia..utKnight] then
    for I := 1 to 4 do
      if TROOP_COST[fUnitType, I] <> wtNone then
      begin
        SetLength(BarracksCost, length(BarracksCost) + 1);
        BarracksCost[high(BarracksCost)].W := TROOP_COST[fUnitType, I];
        BarracksCost[high(BarracksCost)].C := 1;
      end;


  if fUnitType in [utAxeFighter, utSwordFighter, utScout, utKnight] then
    ProjectileDefence := 1
  else
  if fUnitType = utPaladin then
    ProjectileDefence := 3
  else
  if fUnitType in [utRam, utAmmoCart] then
    ProjectileDefence := 6;
  SetLength(StrikeSteps, 1);
  StrikeSteps[0] := 5;

  ProducesWares := [wtAll];
end;


function TKMUnitSpec.IsValid: boolean;
begin
  Result := not (fUnitType in [utNone, utAny]);
end;


function TKMUnitSpec.IsAnimal: boolean;
begin
  Result := fUnitType in [ANIMAL_MIN..ANIMAL_MAX];
end;


function TKMUnitSpec.IsCitizen: boolean;
begin
  Result := fUnitType in [CITIZEN_MIN..CITIZEN_MAX];
end;


function TKMUnitSpec.IsWarrior: boolean;
begin
  Result := fUnitType in [WARRIOR_MIN..WARRIOR_MAX];
end;


function TKMUnitSpec.IsWarriorEquipable: boolean;
begin
  Result := fUnitType in [WARRIOR_EQUIPABLE_BARRACKS_MIN..WARRIOR_EQUIPABLE_BARRACKS_MAX];
end;


function TKMUnitSpec.GetDefenceVsProjectiles(aIsBolt: Boolean): Single;
begin
  Result := Defence;
  //Shielded units get a small bonus
  if fUnitType in [utPaladin, utRam] then
    Result := Result + 3
  else
  if fUnitType in [utAxeFighter, utSwordFighter, utScout, utKnight] then
  begin
    if aIsBolt then
      Result := Result + 0.25
    else
      Result := Result + 1;
  end;
end;

function TKMUnitSpec.GetCanOrderAmmo: Boolean;
begin
  Result := fUnitType in [utBowMan, utCrossbowMan, utBallista, utCatapult, utRogue,
                          utAmmoCart, utArcher, utShip, utBattleShip, utBoat];
end;

function TKMUnitSpec.GetAmmoType: TKMUnitAmmoType;
begin
  case fUnitType of
    utBattleShip : Result := uatBolt;
    utArcher,
    utCrossbowMan,
    utBowMan : Result := uatArrow;
    utBallista : Result := uatBolt;
    utGolem,
    utCatapult: Result := uatStoneBolt;
    utRogue :  Result := uatRogueStone;
    utBoat :  Result := uatAxe;
    else
      Result := uatNone;
  end;
end;

function TKMUnitSpec.GetMinFightRange : Single;
const
  RANGE_ARBALETMAN_MIN  = 4; //KaM: We will shoot a unit standing 4 tiles away, but not one standing 3 tiles away
  RANGE_BOWMAN_MIN      = 4;
  RANGE_SLINGSHOT_MIN   = 4;
begin
  Result := 0;
  case fUnitType of
    utGolem,
    utCatapult:     Result := Trunc(RANGE_WATCHTOWER_MAX - 7);
    utBattleShip:     Result := 2;
    utBallista:     Result := 4;
    utArcher:     Result := 2;
    utBowman:     Result := RANGE_BOWMAN_MIN;
    utCrossbowman: Result := RANGE_ARBALETMAN_MIN;
    utRogue:  Result := RANGE_SLINGSHOT_MIN;
  end;
end;

function TKMUnitSpec.GetMaxFightRange : Single;

const
  RANGE_ARBALETMAN_MAX  = 10.99; //KaM: Unit standing 10 tiles from us will be shot, 11 tiles not
  RANGE_BOWMAN_MAX      = 9.99;
  RANGE_SLINGSHOT_MAX   = 8.99;
  RANGE_BALLISTA_MAX   = RANGE_WATCHTOWER_MAX - 2;
  RANGE_CATAPULT_MAX   = RANGE_WATCHTOWER_MAX - 1;
begin
  Result := 0;

  case fUnitType of
    utArcher:     Result := 7.99;
    utBowman:     Result := RANGE_BOWMAN_MAX;
    utBallista:   Result := RANGE_BALLISTA_MAX;
    utBattleShip: Result := RANGE_ARBALETMAN_MAX;
    utGolem,
    utCatapult:   Result := RANGE_CATAPULT_MAX;
    utCrossbowman: Result := RANGE_ARBALETMAN_MAX;
    utRogue:  Result := RANGE_SLINGSHOT_MAX;
  end;
end;

function TKMUnitSpec.GetShipWeight: Single;
begin
  case fUnitType of
    utNone, utAny : Result := -1;
    utBallista: Result := 6;
    utCatapult: Result := 8;
    utAmmoCart,
    utRam: Result := 12;
    utBoat,
    utBattleShip,
    utShip,
    utGolem: Result := -1;
    utGiant: Result := -1;
    utPikeMachine: Result := 6;
    utPaladin: Result := 3;

    utVagabond,
    utKnight,
    utScout : Result := 3;//horse

    utWolf,
    utSerf..utRecruit: Result := 1;

    else Result := 2;//2 for nonhorse units
  end;
end;

procedure TKMUnitSpec.LoadFromStream(Stream: TMemoryStream);
var act : TKMUnitActionType;
begin
  if fUnitType in UNITS_NEW then Exit;

  Stream.Read(fUnitDat, SizeOf(TKMUnitDat));
  for act := uaWalk to uaUnknown do
    if act <> uaStay then
      Stream.Read(fUnitSprite.Act[act], SizeOf(fUnitSprite.Act[act]));

  //Stream.Read(fUnitSprite, SizeOf(TKMUnitSpriteOld));
  Stream.Read(fUnitSprite2, SizeOf(TKMUnitSprite2));
end;


function TKMUnitSpec.SupportsAction(aAct: TKMUnitActionType): Boolean;
const
  UNIT_SUPPOSTED_ACTIONS: array [TKMUnitType] of TKMUnitActionTypeSet = (
    [], [], //None, Any
    [uaWalk, uaDie, uaEat, uaWalkArm], //Serf
    [uaWalk, uaWork, uaDie, uaWork1, uaEat..uaWalkTool2],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaWork, uaDie..uaWalkBooty2],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaWork, uaDie, uaWork1..uaWalkBooty],
    [uaWalk, uaWork, uaDie, uaEat, uaWork1, uaWork2],
    [uaWalk, uaWork, uaDie, uaWork1, uaEat..uaWalkBooty],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaDie, uaEat],
    [uaWalk, uaSpec, uaDie, uaEat], //Recruit
    [uaWalk, uaSpec, uaDie, uaEat], //operator
    [uaWalk, uaWork, uaDie, uaEat, uaSpec, uaWork1, uaStay], //ClayPicker
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Militia
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Axeman
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Swordsman
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Bowman
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Crossbowman
    [uaWalk, uaWork, uaDie, uaEat],
    [uaWalk, uaWork, uaDie, uaEat],
    [uaWalk, uaWork, uaDie, uaEat],
    [uaWalk, uaWork, uaDie, uaEat], //Cavalry
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Barbarian
    [uaWalk, uaWork, uaDie, uaEat], //Rebel
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Slingshot
    [uaWalk, uaWork, uaSpec, uaDie, uaEat], //Warrior
    [uaWalk, uaWork, uaDie, uaEat],
    [uaWalk, uaWork, uaSpec, uaDie], //catapult
    [uaWalk, uaWork, uaSpec, uaDie], //balista
    [uaWalk, uaWork, uaSpec, uaDie], //Ram
    [uaWalk, uaWork, uaSpec, uaDie], //utGolem
    [uaWalk, uaWork, uaSpec, uaDie], //utGiant
    [uaWalk, uaWork, uaDie, uaEat],//paladin
    [uaWalk, uaWork, uaDie, uaEat, uaWork1],//archer
    [uaWalk, uaWork, uaSpec, uaDie, uaEat, uaWalkArm],//Spy
    [uaWalk, uaWork, uaSpec, uaDie, uaEat],//TrainedWolf
    [uaWalk, uaWork, uaDie, uaEat],//AmmoCart
    [uaWalk, uaWork, uaDie, uaEat],//PikeMachine
    [uaWalk, uaWork, uaDie, uaEat],//Ship

    [uaWalk, uaWork, uaDie, uaEat],//ClubMan
    [uaWalk, uaWork, uaDie, uaEat],//MaceFighter
    [uaWalk, uaWork, uaDie, uaEat],//FlailFighter

    [uaWalk, uaWork, uaDie, uaEat],//utShieldBearer
    [uaWalk, uaWork, uaDie, uaEat],//utFighter
    [uaWalk, uaWork, uaDie, uaEat],//utSpikedTrap
    [uaWalk, uaWork, uaDie, uaEat],//utWoodenWall
    [uaWalk, uaWork, uaDie, uaEat, uaStay],//utTorchMan
    [uaWalk, uaSpec, uaDie, uaEat],//utSHIP
    [uaWalk, uaWork, uaDie, uaEat],//utBattleShip
    [uaWalk, uaWork, uaDie, uaEat],//utBoat
    [uaWalk, uaWork, uaDie, uaEat, uaStay],//utPyro
    [uaWalk, uaWork, uaDie, uaEat, uaStay],//utLekter

    [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], //Animals
    [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk], [uaWalk],//deermale
    [uaWalk], [uaWalk]
  );
begin
  Result := aAct in UNIT_SUPPOSTED_ACTIONS[fUnitType];
end;


function TKMUnitSpec.GetAllowedPassability: TKMTerrainPassability;
const
  // Defines which animal prefers which terrain
  ANIMAL_TERRAIN: array[ANIMAL_MIN .. ANIMAL_MAX] of TKMTerrainPassability = (
    tpWolf, tpFish, tpFish, tpFish, tpCrab, tpFish, tpFish, tpFish,
    tpWolf, tpWolf, tpFox, tpFox, tpFox, tpWolf, tpWolf,//utdeer..utbear
    tpPolarBear, tpCrab, tpCrab);
begin
  case fUnitType of
    ANIMAL_MIN..ANIMAL_MAX:  Result := ANIMAL_TERRAIN[fUnitType]; //Animals
  else
    if fUnitType in [utShip, utBoat, utBattleShip] then
      Result := tpFish
    else
      Result := tpWalk; // Worker, Warriors
  end;
end;


// Where unit would like to be
function TKMUnitSpec.GetDesiredPassability: TKMTerrainPassability;
begin
  if fUnitType in [CITIZEN_MIN..CITIZEN_MAX] - [utBuilder] then
    Result := tpWalkRoad //Citizens except Worker
  else
    Result := GetAllowedPassability; //Workers, warriors, animals
end;


function TKMUnitSpec.GetFightType: TKMFightType;
const
  WARRIOR_FIGHT_TYPE: array[WARRIOR_MIN..WARRIOR_MAX] of TKMFightType = (
    ftMelee,ftMelee,ftMelee, //Militia, AxeFighter, Swordsman
    ftRanged,ftRanged,        //Bowman, Arbaletman
    ftMelee,ftMelee,          //Pikeman, Hallebardman,
    ftMelee,ftMelee,          //HorseScout, Cavalry,
    ftMelee,                   //Barbarian
    ftMelee,                   //Peasant
    ftRanged,                  //utSlingshot
    ftMelee,                   //utMetalBarbarian
    ftMelee,                    //utHorseman
    ftRanged,ftRanged,ftMelee,  //utCatapult, utBallista, utRam
    ftBoth, ftMelee, //golems
    ftMelee, ftRanged, //paladin, archer
    ftMelee, ftMelee, ftMelee, ftMelee, //spy, wolf, ammo cart
    ftMelee, // ship
    ftMelee, ftMelee, ftMelee,
    ftMelee, ftMelee, ftMelee,//utShieldBearer, utFighter, utSpikedTrap,
    ftMelee,//utWoodenWall
    ftMelee,//utWoodenWall
    ftMelee,
    ftRanged,
    ftMelee,
    ftMelee,
    ftMelee
  );
begin
  If fUnitType in UNITS_WARRIORS then
    Result := WARRIOR_FIGHT_TYPE[fUnitType]
  else
    Result := ftMelee;
end;


function TKMUnitSpec.GetGUIIcon: Word;
begin
  case fUnitType of
    utNone, utAny:  Result := 0;
    utClayPicker: Result := 882;
    utBarbarian:    Result := 70;
    utCatapult:    Result := 684;
    utBallista:    Result := 683;
    utRam:    Result := 734;
    utGolem:    Result := 734;
    utGiant:    Result := 734;
    utPaladin:    Result := 738;
    utArcher:    Result := 751;
    utSpy:    Result := 791;
    utTrainedWolf:    Result := 789;
    utAmmoCart:    Result := 790;
    utPikeMachine:    Result := 793;

    utBoat: Result := 917;
    utBattleShip: Result := 916;
    utShip: Result := 848;
    utClubMan: Result := 849;
    utMaceFighter: Result := 850;
    utFlailFighter: Result := 851;
    utShieldBearer: Result := 852;
    utFighter: Result := 853;
    utSpikedTrap: Result := 854;
    utWoodenWall: Result := 855;
    utOperator: Result := 857;
    utTorchman: Result := 859;
    utMedic: Result := 861;
    utPyro: Result := 946;
    utLekter: Result := 950;

    utWolf : Result := 71;
    utFish : Result := 72;
    utWatersnake : Result := 73;
    utSeastar : Result := 74;
    utCrab : Result := 75;
    utWaterflower : Result := 76;
    utWaterleaf : Result := 77;
    utDuck : Result := 78;
    utDeerMale : Result := 959;
    utDeerFemale : Result := 960;
    utFox : Result := 961;
    utBoar : Result := 962;
    utBear : Result := 963;
    utLandDuck : Result := 964;
    utRabbit : Result := 965;
    utWhiteBear : Result := 969;
    utSandSnake : Result := 970;
    utSpider : Result := 971;
  else
    if IsCitizen then
      Result := 141 + UNIT_TYPE_TO_ID[fUnitType]
    else if IsWarriorEquipable then
      Result := 47 + UNIT_TYPE_TO_ID[fUnitType]
    else if IsWarrior then
      Result := 55 + UNIT_TYPE_TO_ID[fUnitType]
    else
      Result := 0;
  end;
end;


function TKMUnitSpec.GetGUIScroll: Word;
begin

  Result := 0;
  if IsValid then
  begin
    case fUnitType of
      utSerf..utRecruit,
      utMilitia..utBallista: Result := 521 + UNIT_TYPE_TO_ID[fUnitType];
      utRam: Result := 736;
      utGolem: Result := 521;
      utGiant: Result := 521;
      utPaladin: Result := 737;
      utArcher: Result := 750;
      utSpy: Result := 781;
      utTrainedWolf: Result := 783;
      utAmmoCart: Result := 784;
      utPikeMachine: Result := 792;
      utBoat: Result := 948;
      utBattleShip: Result := 949;
      utShip: Result := 840;
      utClubMan: Result := 841;
      utMaceFighter: Result := 842;
      utFlailFighter: Result := 843;
      utShieldBearer: Result := 844;
      utFighter: Result := 845;
      utSpikedTrap: Result := 846;
      utWoodenWall: Result := 847;
      utOperator: Result := 856;
      utTorchman: Result := 858;
      utClayPicker: Result := 881;
      utMedic: Result := 860;
      utPyro: Result := 947;
      utLekter: Result := 951;
    end;
  end;
end;


//Units are rendered on minimap with their team color
//Animals don't have team and thus are rendered in their own prefered clors
function TKMUnitSpec.GetMinimapColor: Cardinal;
const
  MM_COLOR: array[TKMUnitType] of Cardinal = (
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0, 0,
    $B0B0B0,$B08000,$B08000,$80B0B0,$00B0B0,$B080B0,$00B000,$80B0B0,
    $80B0B0 ,$80B0B0,$80B0B0,$80B0B0,$80B0B0, $80B0B0, $80B0B0, $80B0B0, $80B0B0, $80B0B0,
    $80B0B0, $80B0B0 ,$80B0B0,$80B0B0, $80B0B0, $80B0B0 ,$80B0B0,$80B0B0,$80B0B0,$80B0B0,
    $80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,$80B0B0,
    $80B0B0,$80B0B0,$80B0B0); //Exact colors can be tweaked
begin
  Result := MM_COLOR[fUnitType] or $FF000000;
end;


//Unit mining ranges. (measured from KaM)
function TKMUnitSpec.GetMiningRange: Byte;
begin
  case fUnitType of
    utWoodcutter:  Result := 10;
    utFarmer:      Result := 10;
    utStonemason:  Result := 16;
    utFisher:      Result := 14;
    utBoat:        Result := 5;
  else
    raise Exception.Create(GUIName + ' has no mining range');
  end;
end;


function TKMUnitSpec.GetSpeed: single;
begin
  Result := fUnitDat.Speed / 240;
end;


function TKMUnitSpec.GetEffectiveSpeed(aMovementType: TKMUnitMoveType): Single;
begin
  case aMovementType of
    umtWalk:      Result := 1 / fUnitSpecInfo.StepsPerTile;
    umtWalkDiag:  Result := 1 / fUnitSpecInfo.StepsPerTileDiag;
    umtStorm:     Result := 1 / fUnitSpecInfo.StepsPerTileStorm;
    umtStormDiag: Result := 1 / fUnitSpecInfo.StepsPerTileStormDiag;
  else
    raise Exception.Create('Unexpected type');
  end;
end;


function TKMUnitSpec.GetEffectiveWalkSpeed(aIsDiag: Boolean): Single;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtWalkDiag)
  else
    Result := GetEffectiveSpeed(umtWalk);
end;


function TKMUnitSpec.GetEffectiveStormSpeed(aIsDiag: Boolean): Single;
begin
  if aIsDiag then
    Result := GetEffectiveSpeed(umtStormDiag)
  else
    Result := GetEffectiveSpeed(umtStorm);
end;


function TKMUnitSpec.GetUnitAnim(aAction: TKMUnitActionType; aDir: TKMDirection): TKMAnimation;
begin
  Assert(aDir <> dirNA);
  Assert(aAction in [Low(TKMUnitActionType)..High(TKMUnitActionType)]);

  Result := fUnitSpriteNew[aAction, aDir];
end;


function TKMUnitSpec.GetUnitTextID: Integer;
begin
  if not IsValid then Exit(-1);

  case fUnitType of
    utWolf:              Result := TX_UNITS_WOLF;
    utFish:              Result := TX_UNITS_FISH;
    utWatersnake:        Result := TX_UNITS_WATERSNAKE;
    utSeastar:           Result := TX_UNITS_SEASTAR;
    utCrab:              Result := TX_UNITS_CRAB;
    utWaterflower:       Result := TX_UNITS_WATERFLOWER;
    utWaterleaf:         Result := TX_UNITS_WATERLEAF;
    utDuck:              Result := TX_UNITS_DUCK;
    utRam:               Result := 1674;
    utGolem:             Result := 1672;
    utGiant:             Result := 1673;
    utPaladin:           Result := 1680;
    utArcher:            Result := 1722;
    utSpy:               Result := 1820;
    utAmmoCart:          Result := 1872;
    utPikeMachine:       Result := 1918;
    utTrainedWolf:       Result := 1935;
    utClubMan:           Result := 1964;
    utMaceFighter:       Result := 1965;
    utFlailFighter:      Result := 1966;
    utOperator:          Result := 1967;
    utShip:              Result := 1968;
    utShieldBearer:      Result := 1969;
    utFighter:           Result := 1970;
    utSpikedTrap:        Result := 1971;
    utWoodenWall:        Result := 1972;
    utClayPicker:        Result := 1976;
    utTorchman:          Result := 1996;
    utMedic:             Result := 1997;
    utBattleShip:        Result := 2023;
    utBoat:              Result := 2024;
    utPyro:              Result := 2102;
    utLekter:            Result := 2103;
    utDeerMale:          Result := 2144;
    utDeerFemale:        Result := 2145;
    utFox:               Result := 2146;
    utBoar:              Result := 2147;
    utBear:              Result := 2148;
    utLandDuck:          Result := 2149;
    utRabbit:            Result := 2150;
    utWhiteBear:         Result := 2151;
    utSandSnake:         Result := 2152;
    utSpider:            Result := 2153;
  else
    Result := TX_UNITS_NAMES__29 + UNIT_TYPE_TO_ID[fUnitType];
  end;
end;


function TKMUnitSpec.GetUnitName: UnicodeString;
begin
  case fUnitType of
    utAny:             Result := gResTexts[TX_UNITS_ALL];
    utNone:            Result := 'N/A';
  else
    Result := gResTexts[GetUnitTextID];
  end;
end;

function TKMUnitSpec.GetHouseDamage: Integer;
begin

  case fUnitType of
    utRam : Result := 250;
    utCatapult : Result := 50;
    utBallista : Result := 8;
    utWarrior : Result := 4;
    utPaladin : Result := 3;
    else
      if IsRanged(fUnitType) then
        Result := 1
      else
        Result := 2;

  end;
end;

function TKMUnitSpec.GetUnitDamage: Integer;
begin
  case fUnitType of
    utRam : Result := 0;
    utCatapult : Result := 2;
    utBallista : Result := 5;
    utBarbarian,
    utWarrior : Result := 1;
    utPikeMachine : Result := 2;
    utPaladin : Result := 3;
    else
        Result := 1;
  end;
end;

function TKMUnitSpec.GetTrainingHouse: TKMHouseType;
begin
  Result := htNone;

  if htSchool in fTrainingHouses then
    Result := htSchool
  else
  if htShipYard in fTrainingHouses then
    Result := htShipYard
  else
  if htPalace in fTrainingHouses then
    Result := htPalace
  else
  if htSiegeWorkshop in fTrainingHouses then
    Result := htSiegeWorkshop
  else
  if htTownHall in fTrainingHouses then
    Result := htTownHall
  else
  if htBarracks in fTrainingHouses then
    Result := htBarracks;

end;


function TKMUnitSpec.GetWorkerHouses: TKMHouseTypeArray;
begin
  Result := fHouses;
end;

function TKMUnitSpec.GetUnitPower: Word;
begin
  Result := Round(HitPoints + Attack + AttackHorse
                  + Defence + Speed + ProjectileDefence + HouseDamage + UnitDamage);
end;

function TKMUnitSpec.GetDescription: UnicodeString;
begin
  if DescriptionID <> 0 then
    Result := gResTexts[DescriptionID]
  else
  if IsValid and not IsAnimal then
  begin
    if fUnitType = utSpy then
      Result := gResTexts[1821]
    else
    if fUnitType >= utMilitia then
      Result := gResTexts[1770 + ord(fUnitType) - 18]
    else
      Result := gResTexts[TX_UNITS_DESCRIPTIONS__13 + UNIT_TYPE_TO_ID[fUnitType]];

  end else
    Result := 'N/A';
end;

function TKMUnitSpec.Damage : Word;
begin
  case fUnitType of
    utGolem: Result := 2;
    utGiant: Result := 4;
    utPaladin: Result := 4;
    else Result := 1;
  end;
end;

class function TKMUnitSpec.IsMelee(aUnitType: TKMUnitType): Boolean;
begin
  Result := aUnitType in [utMilitia, utAxeFighter, utSwordFighter, utBarbarian, utWarrior];
end;


class function TKMUnitSpec.IsMounted(aUnitType: TKMUnitType): Boolean;
begin
  Result := aUnitType in [utScout, utKnight, utVagabond];
end;


class function TKMUnitSpec.IsAntihorse(aUnitType: TKMUnitType): Boolean;
begin
  Result := aUnitType in [utLanceCarrier, utPikeman, utRebel];
end;


class function TKMUnitSpec.IsRanged(aUnitType: TKMUnitType): Boolean;
begin
  Result := aUnitType in [utBowman, utCrossbowman, utRogue, utCatapult, utBallista, utArcher, utGolem];
end;

procedure TKMUnitSpec.LoadFromJson(aUnit: TKMJson);
var I, K, J : Integer;
  nArr, nArr2: TJsonArray;
  nAnim : TKMJson;
  arr : array of Integer;
  aAct, UAT : TKMUnitActionType;
  aDir : TKMDirection;
  WT : TKMWareType;
begin
  if aUnit.Contains('HitPoints') then fUnitDat.HitPoints := aUnit.I['HitPoints'];
  if aUnit.Contains('Attack') then fUnitDat.Attack := aUnit.I['Attack'];
  if aUnit.Contains('AttackHorse') then fUnitDat.AttackHorse := aUnit.I['AttackHorse'];
  if aUnit.Contains('Defence') then fUnitDat.Defence := aUnit.I['Defence'];
  if aUnit.Contains('Speed') then fUnitDat.Speed := aUnit.I['Speed'];
  if aUnit.Contains('Sight') then fUnitDat.Sight := aUnit.I['Sight'];
  if aUnit.Contains('HouseDamage') then HouseDamage := aUnit.I['HouseDamage'];
  if aUnit.Contains('UnitDamage') then UnitDamage := aUnit.I['UnitDamage'];
  if aUnit.Contains('ProjectileDefence') then ProjectileDefence := aUnit.D['ProjectileDefence'];

  if aUnit.Contains('CanOrderAmmo') then CanOrderAmmo := aUnit.B['CanOrderAmmo'];
  if aUnit.Contains('CanStorm') then CanStorm := aUnit.B['CanStorm'];
  if aUnit.Contains('CanAttackUnits') then CanAttackUnits := aUnit.B['CanAttackUnits'];
  if aUnit.Contains('CanAttackHouses') then CanAttackHouses := aUnit.B['CanAttackHouses'];

  if aUnit.Contains('AmmoType') then
    if not TKMEnumUtils.TryGetAs<TKMUnitAmmoType>(aUnit.S['AmmoType'],  AmmoType) then
      raise Exception.Create('Wrong TKMUnitAmmoType: ' + aUnit.S['AmmoType']);


  if aUnit.Contains('MinRange') then MinRange := aUnit.D['MinRange'];
  if aUnit.Contains('MaxRange') then MaxRange := aUnit.D['MaxRange'];
  if aUnit.Contains('DescriptionID') then DescriptionID := aUnit.I['DescriptionID'];
  if aUnit.Contains('SchoolTime') then SchoolTime := aUnit.I['SchoolTime'];

  if aUnit.Contains('FightType') then
    if not TKMEnumUtils.TryGetAs<TKMFightType>(aUnit.S['FightType'],  FightType) then
      raise Exception.Create('Wrong TKMFightType: ' + aUnit.S['FightType']);

  {fUnitSpecInfo.StepsPerTile          := Round(1    / fUnitDat.Speed);
  fUnitSpecInfo.StepsPerTileDiag      := Round(1.41 / fUnitDat.Speed);
  fUnitSpecInfo.StepsPerTileStorm     := Round(1    / (fUnitDat.Speed * STORM_SPEEDUP));
  fUnitSpecInfo.StepsPerTileStormDiag := Round(1.41 / (fUnitDat.Speed * STORM_SPEEDUP));}

  nArr := aUnit.A['StrikeSteps'];

  JSONArrToValidSet(aUnit.A['ProducesWares'], ProducesWares);

  if nArr.Count > 0 then
  begin
    SetLength(StrikeSteps, nArr.Count);

    for I := 0 to nArr.Count - 1 do
      StrikeSteps[I] := nArr.I[I];
  end;

  nArr := aUnit.A['Animations'];
  for I := 0 to nArr.Count - 1 do
  begin
    nAnim := nArr.O[I];
    aAct := uaUnknown;

    for UAT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
      if UNIT_ACT_STR[UAT] = nAnim.S['ActionType'] then
      begin
        aAct := UAT;
        Break;
      end;

    if aAct = uaUnknown then
      raise Exception.Create('Wrong UnitActionType : ' + nAnim.S['ActionType']);

    if not TKMEnumUtils.TryGetAs<TKMDirection>(nAnim.S['Dir'],  aDir) then
      raise Exception.Create('Wrong DirectionType : ' + nAnim.S['Dir']);
    nAnim.GetAnim(fUnitSpriteNew[aAct,aDir]);
    //JSONToAnim(nAnim,fUnitSpriteNew[aAct,aDir]);
  end;

  nArr := aUnit.A['FullUnitAnimations'];
  for J := 0 to nArr.Count - 1 do
  begin
    aAct := uaUnknown;
    nAnim := nArr.O[J];
    for UAT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
      if UNIT_ACT_STR[UAT] = nAnim.S['ActionType'] then
      begin
        aAct := UAT;
        Break;
      end;

    if aAct = uaUnknown then
      raise Exception.Create('Wrong UnitActionType : ' + nAnim.S['ActionType']);
   I := nAnim.I['StartStep'];
   K := nAnim.I['StepsCount'];

   for aDir := DirN to DirNW do
   begin
    fUnitSpriteNew[aAct, aDir].Create(nAnim.I['X'], nAnim.I['Y'], I + (K * (byte(aDir) - 1)), K, nAnim.I['Offset'], nAnim.B['Backward'] );
    if nAnim.Contains('Extend') then
      fUnitSpriteNew[aAct, aDir].Extend(nAnim.I['Extend'])

   end;

  end;

  If aUnit.Contains('PalaceCost') then
  begin
    PalaceCost.PhaseDuration := aUnit.O['PalaceCost'].I['PhaseDuration'];
    PalaceCost.PhaseCount := aUnit.O['PalaceCost'].I['PhaseCount'];
    nArr := aUnit.O['PalaceCost'].A['VirtualWares'];
    if nArr.Count > 0 then
      SetLength(PalaceCost.Wares, 0);

    for I := 0 to nArr.Count - 1 do
    begin
      if nArr[I].I['Count'] = 0 then
        Continue;

      SetLength(PalaceCost.Wares, length(PalaceCost.Wares) + 1);

      PalaceCost.Wares[high(PalaceCost.Wares)].W := nArr[I].S['VirtualWareName'];
      PalaceCost.Wares[high(PalaceCost.Wares)].C := nArr[I].I['Count'];
    end;

    nArr := aUnit.O['PalaceCost'].A['Wares'];

    for I := 0 to nArr.Count - 1 do
      if I <= 3 then
      begin
        if not TKMEnumUtils.TryGetAs<TKMWareType>(nArr.O[I].S['WareType'],  PalaceCost.Plan[I].W) then
          raise Exception.Create('Wrong TKMWareTypeType : ' + nArr.O[I].S['WareType']);

        PalaceCost.Plan[I].C := nArr.O[I].I['Count'];
      end
      else
        break;
  end;

  nArr := aUnit.A['BarracksCost'];
  if nArr.Count > 0 then
    SetLength(BarracksCost, 0);

  for I := 0 to nArr.COunt - 1  do
    if not TKMEnumUtils.TryGetAs<TKMWareType>(nArr[I].S['WareType'],  WT) then
      raise Exception.Create('Wrong WareType')
    else
    begin
      if WT in [wtNone, wtWarfare, wtAll, wtFood] then
        Continue;

      SetLength(BarracksCost, length(BarracksCost) + 1);
      BarracksCost[high(BarracksCost)].W := WT;
      BarracksCost[high(BarracksCost)].C := nArr[I].I['Count'];
    end;

  SiegePhasesCount := EnsureRange(aUnit.I['SiegeWorkshopPhasesCount'], 0, 255);

  nArr := aUnit.A['SiegeWorkshopCost'];
  if nArr.Count > 0 then
    for I := 0 to 3 do
      SiegeCost[I].W := wtNone;

  for I := 0 to nArr.Count - 1 do
  begin
    if I > 3 then
      Break;

    if not TKMEnumUtils.TryGetAs<TKMWareType>(nArr[I].S['WareType'],  SiegeCost[I].W) then
      raise Exception.Create('Wrong WareType');

    SiegeCost[I].C := nArr[I].I['Count'];
  end;



  if aUnit.Contains('TownhallCost') then
    TownhallCost := aUnit.I['TownhallCost'];
  

end;

{ TKMUnitsDatCollection }
constructor TKMResUnits.Create;

  procedure AddAnimation(aUnit: TKMUnitType; aAnim: TKMUnitActionType; aDir : TKMDirection; aMoveX, aMoveY: Integer; const aSteps: array of SmallInt);  overload;
  var
    I: Integer;
  begin
    with fItems[aUnit].fUnitSprite.Act[aAnim].Dir[aDir] do
    begin
      MoveX := aMoveX;
      MoveY := aMoveY;
      Count := length(aSteps);
      for I := 1 to Count do
        Step[I] := aSteps[I-1] - 1;
    end;
  end;

  procedure AddAnimation(aUnit: TKMUnitType; aAnim: TKMUnitActionType; aDir : TKMDirection; aMoveX, aMoveY: Integer; const aStart, aCount: SmallInt; aOffset : Byte = 0; BackWard : Boolean = false); overload;
  begin
    with fItems[aUnit].fUnitSprite.Act[aAnim].Dir[aDir] do
      Create(aMoveX, aMoveY, aStart, aCount, aOffset, BackWard)
  end;
const
  DEF_SCOUT_SIGHT = 9;
  DEF_HORSEMAN_ATTACK = 40;
  DEF_PEASANT_ATTACK_HORSE = 60;
  DEF_PIKEMAN_ATTACK_HORSE = 55;
  DEF_MOUNTED_SPEED = 39;
  // Offsets for flags rendering in pixels
  FlagXOffset: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX, TKMDirection] of shortint = (
    ( 0, 10, -1,  2,  1, -6,-10,  4, 13),  // gtMelee
    ( 0,  6,  5,  7, -3,-10, -4, 10,  9),  // gtAntiHorse
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gtRanged
    ( 0,  6,  2,  3, -5,-10, -8,  5,  6),  //gtMounted
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gtmachines
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gtMachinesMelee
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gtWreckers
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6)  // gtShips
  );

  FlagYOffset: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX, TKMDirection] of shortint = (
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtMelee
    ( 0, 23, 25, 25, 21, 20, 19, 20, 22),  // gtAntiHorse
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtRanged
    ( 0,  4, 16, 16,  4,  5,  2,  3,  4),  //gtMounted
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtmachines
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtMachinesMelee
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtWreckers
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27)  // gtShips
    );
var
  UT: TKMUnitType;
  act : TKMUnitActionType;
  dir : TKMDirection;
begin
  inherited;

  for UT := utNone to UNIT_MAX do
      fItems[UT] := TKMUnitSpec.Create(UT);

  fCRC := LoadUnitsDat(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'unit.dat');

  for UT := utNone to utRecruit do
    fItems[UT].fUnitDat.Sight := 4;

  for UT := utMilitia to UNIT_MAX do
    fItems[UT].fUnitDat.Sight := 6;

  // Overwrite units stats only if they are set for default values from original game
  // We don't want to update them, in case player manually edited unit.dat file
  fItems[utScout].fUnitDat.Sight := 9;
  fItems[utVagabond].fUnitDat.Sight := 7;

  fItems[utVagabond].fUnitDat.Attack := 35;

  fItems[utRebel].fUnitDat.AttackHorse := 50;
  fItems[utLanceCarrier].fUnitDat.AttackHorse := 65;
  fItems[utPikeman].fUnitDat.AttackHorse := 90;

  fItems[utScout].fUnitDat.Speed := 40;
  fItems[utKnight].fUnitDat.Speed := 36;
  fItems[utVagabond].fUnitDat.Speed := 44;

  fItems[utWarrior].fUnitDat.Defence := fItems[utWarrior].fUnitDat.Defence + 1;

  fItems[utRam].fUnitType := utRam;
  fItems[utRam].fUnitDat := fItems[utCatapult].fUnitDat;

  fItems[utRam].fUnitDat.Attack := 3;
  fItems[utRam].fUnitDat.AttackHorse := 0;
  fItems[utRam].fUnitDat.HitPoints := 10;
  fItems[utRam].fUnitDat.Defence := 10;
  fItems[utRam].fUnitDat.Sight := 5;


  fItems[utRam].fUnitSprite := fItems[utCatapult].fUnitSprite;
  fItems[utRam].fUnitSprite2 := fItems[utCatapult].fUnitSprite2;
  fItems[utRam].fUnitDat.Speed := 12;

  fItems[utBallista].fUnitDat.HitPoints := 4;
  fItems[utBallista].fUnitDat.Attack := 250;
  fItems[utBallista].fUnitDat.AttackHorse := 0;

  fItems[utGolem].fUnitDat := fItems[utBarbarian].fUnitDat;
  fItems[utGolem].fUnitSprite := fItems[utBarbarian].fUnitSprite;
  fItems[utGolem].fUnitSprite2 := fItems[utBarbarian].fUnitSprite2;
  fItems[utGolem].fUnitDat.Speed := 24;

  fItems[utGiant].fUnitDat := fItems[utBarbarian].fUnitDat;
  fItems[utGiant].fUnitSprite := fItems[utBarbarian].fUnitSprite;
  fItems[utGiant].fUnitSprite2 := fItems[utBarbarian].fUnitSprite2;
  fItems[utGiant].fUnitDat.Speed := 20;

  fItems[utGolem].fUnitDat.HitPoints := 4;
  fItems[utGolem].fUnitDat.Attack := 199;
  fItems[utGolem].fUnitDat.Defence := 3;
  fItems[utGolem].fUnitDat.Sight := 8;

  fItems[utGiant].fUnitDat.HitPoints := 7;
  fItems[utGiant].fUnitDat.Defence := 5;
  fItems[utGiant].fUnitDat.Attack := 250;
  fItems[utGiant].fUnitDat.Sight := 15;

  //fItems[utSerf].fUnitDat.Speed := 24;


  //fItems[utRam].fUnitSpecInfo.StepsPerTile          := Round(1    / fItems[utRam].Speed);

  fItems[utPaladin].fUnitDat := fItems[utSwordFighter].fUnitDat;
  fItems[utPaladin].fUnitSprite := fItems[utSwordFighter].fUnitSprite;
  fItems[utPaladin].fUnitSprite2 := fItems[utSwordFighter].fUnitSprite2;
  fItems[utPaladin].fUnitDat.HitPoints := 7;
  fItems[utPaladin].fUnitDat.Defence := 8;
  fItems[utPaladin].fUnitDat.Attack := 115;
  fItems[utPaladin].fUnitDat.AttackHorse := 20;
  fItems[utPaladin].fUnitDat.Sight := 5;
  fItems[utPaladin].fUnitDat.Speed := 20;
  //archer test speed

  fItems[utArcher].fUnitDat := fItems[utBowMan].fUnitDat;
  fItems[utArcher].fUnitSprite := fItems[utBowMan].fUnitSprite;
  fItems[utArcher].fUnitSprite2 := fItems[utBowMan].fUnitSprite2;
  fItems[utArcher].fUnitDat.HitPoints := 2;
  fItems[utArcher].fUnitDat.Speed := 40;
  fItems[utArcher].fUnitDat.Sight := 9;

  fItems[utSpy].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utSpy].fUnitSprite := fItems[utSerf].fUnitSprite;
  fItems[utSpy].fUnitSprite2 := fItems[utSerf].fUnitSprite2;
  fItems[utSpy].fUnitSprite.Act[uaWork] := fItems[utSerf].fUnitSprite.Act[uaWalk];
  fItems[utSpy].fUnitDat.HitPoints := 1;
  fItems[utSpy].fUnitDat.Speed := 20;
  fItems[utSpy].fUnitDat.Sight := 7;

  fItems[utTrainedWolf].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utTrainedWolf].fUnitSprite := fItems[utWolf].fUnitSprite;
  fItems[utTrainedWolf].fUnitSprite2 := fItems[utWolf].fUnitSprite2;
  fItems[utTrainedWolf].fUnitDat.HitPoints := 2;
  fItems[utTrainedWolf].fUnitDat.Defence := 1;
  fItems[utTrainedWolf].fUnitDat.Attack := 55;
  fItems[utTrainedWolf].fUnitDat.Speed := 40;
  fItems[utTrainedWolf].fUnitDat.Sight := 6;
  fItems[utTrainedWolf].fUnitSprite.Act[uaWork] := fItems[utTrainedWolf].fUnitSprite.Act[uaWalk];

  fItems[utAmmoCart].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utAmmoCart].fUnitSprite := fItems[utCatapult].fUnitSprite;
  fItems[utAmmoCart].fUnitDat.HitPoints := 10;
  fItems[utAmmoCart].fUnitDat.Defence := 3;
  fItems[utAmmoCart].fUnitDat.Attack := 55;
  fItems[utAmmoCart].fUnitDat.Speed := 12;
  fItems[utAmmoCart].fUnitDat.Sight := 4;

  fItems[utPikeMachine].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utPikeMachine].fUnitSprite := fItems[utCatapult].fUnitSprite;
  fItems[utPikeMachine].fUnitDat.HitPoints := 20;
  fItems[utPikeMachine].fUnitDat.Defence := 3;
  fItems[utPikeMachine].fUnitDat.Attack := 55;
  fItems[utPikeMachine].fUnitDat.AttackHorse := 100;
  fItems[utPikeMachine].fUnitDat.Speed := 12;
  fItems[utPikeMachine].fUnitDat.Sight := 4;

  fItems[utShip].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utShip].fUnitSprite := fItems[utCatapult].fUnitSprite;
  for dir := DirN to dirNW do
    fItems[utShip].fUnitSprite.Act[uaWalkArm].Dir[dir].Count := 0;
  //fItems[utShip].fUnitSprite.Act[uaWalkArm].Clear;
  fItems[utShip].fUnitDat.HitPoints := 4;
  fItems[utShip].fUnitDat.Defence := 3;
  fItems[utShip].fUnitDat.Attack := 0;
  fItems[utShip].fUnitDat.AttackHorse := 0;
  fItems[utShip].fUnitDat.Speed := 24;
  fItems[utShip].fUnitDat.Sight := 7;

  fItems[utBattleShip].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utBattleShip].fUnitSprite := fItems[utCatapult].fUnitSprite;
  for dir := DirN to dirNW do
    fItems[utBattleShip].fUnitSprite.Act[uaWalkArm].Dir[dir].Count := 0;

  fItems[utBattleShip].fUnitDat.HitPoints := 6;
  fItems[utBattleShip].fUnitDat.Defence := 5;
  fItems[utBattleShip].fUnitDat.Attack := 500;
  fItems[utBattleShip].fUnitDat.AttackHorse := 0;
  fItems[utBattleShip].HouseDamage := 25;
  fItems[utBattleShip].fUnitDat.Speed := 20;
  fItems[utBattleShip].fUnitDat.Sight := 7;


  fItems[utBoat].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utBoat].fUnitSprite := fItems[utCatapult].fUnitSprite;
  for dir := DirN to dirNW do
    fItems[utBoat].fUnitSprite.Act[uaWalkArm].Dir[dir].Count := 0;
  fItems[utBoat].fUnitDat.HitPoints := 5;
  fItems[utBoat].fUnitDat.Defence := 1;
  fItems[utBoat].fUnitDat.Attack := 0;
  fItems[utBoat].fUnitDat.AttackHorse := 0;
  fItems[utBoat].fUnitDat.Speed := 29;
  fItems[utBoat].fUnitDat.Sight := 7;


  fItems[utOperator].fUnitDat := fItems[utRecruit].fUnitDat;
  fItems[utOperator].fUnitSprite := fItems[utRecruit].fUnitSprite;

  fItems[utClayPicker].fUnitDat := fItems[utStoneMason].fUnitDat;
  fItems[utClayPicker].fUnitSprite := fItems[utStoneMason].fUnitSprite;

  fItems[utClubMan].fUnitDat := fItems[utMilitia].fUnitDat;
  fItems[utClubMan].fUnitSprite := fItems[utMilitia].fUnitSprite;

  fItems[utMaceFighter].fUnitDat := fItems[utAxeFighter].fUnitDat;
  fItems[utMaceFighter].fUnitSprite := fItems[utAxeFighter].fUnitSprite;

  fItems[utFlailFighter].fUnitDat := fItems[utSwordFighter].fUnitDat;
  fItems[utFlailFighter].fUnitSprite := fItems[utSwordFighter].fUnitSprite;

  fItems[utShieldBearer].fUnitDat := fItems[utSwordFighter].fUnitDat;
  fItems[utShieldBearer].fUnitSprite := fItems[utSwordFighter].fUnitSprite;

  fItems[utFighter].fUnitDat := fItems[utSwordFighter].fUnitDat;
  fItems[utFighter].fUnitSprite := fItems[utSwordFighter].fUnitSprite;

  fItems[utSpikedTrap].fUnitDat := fItems[utBallista].fUnitDat;
  fItems[utSpikedTrap].fUnitSprite := fItems[utBallista].fUnitSprite;

  fItems[utWoodenWall].fUnitDat := fItems[utCatapult].fUnitDat;
  fItems[utWoodenWall].fUnitSprite := fItems[utCatapult].fUnitSprite;

  fItems[utTorchMan].fUnitDat := fItems[utRecruit].fUnitDat;
  fItems[utTorchMan].fUnitSprite := fItems[utMilitia].fUnitSprite;

  fItems[utPyro].fUnitDat := fItems[utRecruit].fUnitDat;
  fItems[utPyro].fUnitSprite := fItems[utMilitia].fUnitSprite;

  fItems[utMedic].fUnitDat := fItems[utSwordFighter].fUnitDat;
  fItems[utMedic].fUnitSprite := fItems[utSwordFighter].fUnitSprite;

  fItems[utCatapult].fUnitDat.HitPoints := 4;
  fItems[utBallista].fUnitDat.HitPoints := 4;
  fItems[utBallista].fUnitDat.Attack := 500;


  fItems[utStoneMason].fUnitSprite.Act[uaWork1].Dir[dirN] :=  fItems[utBuilder].fUnitSprite.Act[uaWork1].Dir[dirN];

  AddAnimation(utStoneMason, uaWork1, dirN, 0, 0, [9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516]);

  AddAnimation(utRam, uaWalk, dirN, 0, 0, 9516, 9, 0, true);
  AddAnimation(utRam, uaWalk, dirNE, 0, 0, 9525, 10);
  AddAnimation(utRam, uaWalk, dirE, 0, 0, 9535, 10);
  AddAnimation(utRam, uaWalk, dirSE, 0, 0, 9545, 10, 0, true);
  AddAnimation(utRam, uaWalk, dirS, 0, 0, 9555, 10);
  AddAnimation(utRam, uaWalk, dirSW, 0, 0, 9565, 10, 0, true);
  AddAnimation(utRam, uaWalk, dirW, 0, 0, 9575, 10, 0, true);
  AddAnimation(utRam, uaWalk, dirNW, 0, 0, 9585, 10);

  AddAnimation(utRam, uaWork, dirN, 0, 0, [9610, 9611, 9612, 9613, 9614, 9615, 9610, 9610, 9610, 9610, 9610, 9610]);
  AddAnimation(utRam, uaWork, dirNE, 0, 0, [9616, 9617, 9618, 9619, 9620, 9621, 9622, 9621, 9616, 9616, 9616, 9616, 9616, 9616]);
  AddAnimation(utRam, uaWork, dirE, 0, 0, [9623, 9624, 9625, 9626, 9627, 9628, 9629, 9628, 9623, 9623, 9623, 9623, 9623, 9623]);
  AddAnimation(utRam, uaWork, dirSE, 0, 0, [9630, 9631, 9632, 9633, 9634, 9635, 9636, 9635, 9630, 9630, 9630, 9630, 9630, 9630]);
  AddAnimation(utRam, uaWork, dirS, 0, 0, [9637, 9638, 9639, 9640, 9641, 9642, 9641,  9637, 9637, 9637, 9637, 9637, 9637]);
  AddAnimation(utRam, uaWork, dirSW, 0, 0, [9643, 9644, 9645, 9646, 9647, 9648, 9649, 9647, 9643, 9643, 9643, 9643, 9643, 9643]);
  AddAnimation(utRam, uaWork, dirW, 0, 0, [9650, 9651, 9652, 9653, 9654, 9655, 9656, 9655, 9650, 9650, 9650, 9650, 9650, 9650]);
  AddAnimation(utRam, uaWork, dirNW, 0, 0, [9657, 9658, 9659, 9660, 9661, 9662, 9663, 9661, 9657, 9657, 9657, 9657, 9657, 9657]);

  AddAnimation(utGolem, uaWalk, dirN, 0, 0, 9721, 8);
  AddAnimation(utGolem, uaWalk, dirNE, 0, 0, 9713, 8);
  AddAnimation(utGolem, uaWalk, dirE, 0, 0, 9705, 8);
  AddAnimation(utGolem, uaWalk, dirSE, 0, 0, 9697, 8);
  AddAnimation(utGolem, uaWalk, dirS, 0, 0, 9689, 8);
  AddAnimation(utGolem, uaWalk, dirSW, 0, 0, 9681, 8);
  AddAnimation(utGolem, uaWalk, dirW, 0, 0, 9673, 8);
  AddAnimation(utGolem, uaWalk, dirNW, 0, 0, 9665, 8);

  AddAnimation(utGolem, uaWork, dirN, 0, 0, 9793, 7);
  AddAnimation(utGolem, uaWork, dirNE, 0, 0, 9783, 10);
  AddAnimation(utGolem, uaWork, dirE, 0, 0, 9773, 10);
  AddAnimation(utGolem, uaWork, dirSE, 0, 0, 9763, 9);
  AddAnimation(utGolem, uaWork, dirS, 0, 0, 9756, 7);
  AddAnimation(utGolem, uaWork, dirSW, 0, 0, 9749, 7);
  AddAnimation(utGolem, uaWork, dirW, 0, 0, 9739, 10);
  AddAnimation(utGolem, uaWork, dirNW, 0, 0, 9729, 10);

  AddAnimation(utGiant, uaWalk, dirN, 0, 0, 9800, 8);
  AddAnimation(utGiant, uaWalk, dirNE, 0, 0, 9808, 8);
  AddAnimation(utGiant, uaWalk, dirE, 0, 0, 9816, 8);
  AddAnimation(utGiant, uaWalk, dirSE, 0, 0, 9824, 8);
  AddAnimation(utGiant, uaWalk, dirS, 0, 0, 9832, 8);
  AddAnimation(utGiant, uaWalk, dirSW, 0, 0, 9840, 8);
  AddAnimation(utGiant, uaWalk, dirW, 0, 0, 9848, 8);
  AddAnimation(utGiant, uaWalk, dirNW, 0, 0, 9856, 8);

  AddAnimation(utGiant, uaWork, dirN, 0, 0, 9864, 7);
  AddAnimation(utGiant, uaWork, dirNE, 0, 0, 9771, 10);
  AddAnimation(utGiant, uaWork, dirE, 0, 0, 9781, 9);
  AddAnimation(utGiant, uaWork, dirSE, 0, 0, 9790, 10);
  AddAnimation(utGiant, uaWork, dirS, 0, 0, 9900, 7);
  AddAnimation(utGiant, uaWork, dirSW, 0, 0, 9907, 7);
  AddAnimation(utGiant, uaWork, dirW, 0, 0, 9914, 10);
  AddAnimation(utGiant, uaWork, dirNW, 0, 0, 9924, 10);

  AddAnimation(utPaladin, uaWalk, dirN, 0, 0, 10096, 15);
  AddAnimation(utPaladin, uaWalk, dirNE, 0, 0, 10080, 15);
  AddAnimation(utPaladin, uaWalk, dirE, 0, 0, 10063, 15);
  AddAnimation(utPaladin, uaWalk, dirSE, 0, 0, 10176, 15);
  AddAnimation(utPaladin, uaWalk, dirS, 0, 0, 10160, 15);
  AddAnimation(utPaladin, uaWalk, dirSW, 0, 0, 10144, 15);
  AddAnimation(utPaladin, uaWalk, dirW, 0, 0, 10128, 15);
  AddAnimation(utPaladin, uaWalk, dirNW, 0, 0, 10112, 15);

  AddAnimation(utPaladin, uaWork, dirN, 0, 0, 9951, 14);
  AddAnimation(utPaladin, uaWork, dirNE, 0, 0, 9934, 15);
  AddAnimation(utPaladin, uaWork, dirE, 0, 0, 10046, 15);
  AddAnimation(utPaladin, uaWork, dirSE, 0, 0, 10030, 15);
  AddAnimation(utPaladin, uaWork, dirS, 0, 0, 10014, 15);
  AddAnimation(utPaladin, uaWork, dirSW, 0, 0, 9998, 15);
  AddAnimation(utPaladin, uaWork, dirW, 0, 0, 9982, 15);
  AddAnimation(utPaladin, uaWork, dirNW, 0, 0, 9966, 15);

  AddAnimation(utArcher, uaWalk, dirN, 0, 0, 10215, 6);
  AddAnimation(utArcher, uaWalk, dirNE, 0, 0, 10209, 6);
  AddAnimation(utArcher, uaWalk, dirE, 0, 0, 10203, 6);
  AddAnimation(utArcher, uaWalk, dirSE, 0, 0, 10197, 6);
  AddAnimation(utArcher, uaWalk, dirS, 0, 0, 10191, 6);
  AddAnimation(utArcher, uaWalk, dirSW, 0, 0, 10233, 6);
  AddAnimation(utArcher, uaWalk, dirW, 0, 0, 10227, 6);
  AddAnimation(utArcher, uaWalk, dirNW, 0, 0, 10221, 6);

  AddAnimation(utArcher, uaWork, dirN, 0, 0, 10239, 15);
  AddAnimation(utArcher, uaWork, dirNE, 0, 0, 10254, 16);
  AddAnimation(utArcher, uaWork, dirE, 0, 0, 10270, 16);
  AddAnimation(utArcher, uaWork, dirSE, 0, 0, 10286, 16);
  AddAnimation(utArcher, uaWork, dirS, 0, 0, 10302, 16);
  AddAnimation(utArcher, uaWork, dirSW, 0, 0, 10318, 16);
  AddAnimation(utArcher, uaWork, dirW, 0, 0, 10334, 16);
  AddAnimation(utArcher, uaWork, dirNW, 0, 0, 10350, 15);

  AddAnimation(utAmmoCart, uaWalk, dirE, 0, 0, 10564, 15);
  AddAnimation(utAmmoCart, uaWalk, dirNE, 0, 0, 10579, 15);
  AddAnimation(utAmmoCart, uaWalk, dirN, 0, 0, 10594, 15);
  AddAnimation(utAmmoCart, uaWalk, dirNW, 0, 0, 10609, 15);
  AddAnimation(utAmmoCart, uaWalk, dirW, 0, 0, 10624, 15);
  AddAnimation(utAmmoCart, uaWalk, dirSW, 0, 0, 10639, 15);
  AddAnimation(utAmmoCart, uaWalk, dirS, 0, 0, 10654, 15);
  AddAnimation(utAmmoCart, uaWalk, dirSE, 0, 0, 10669, 15);

  AddAnimation(utTrainedWolf, uaWalk, dirN, 0, 0, [10685,10686,10687,10688,10689,10690, 10691,10748]);
  AddAnimation(utTrainedWolf, uaWalk, dirNE, 0, 0, 10691, 8);
  AddAnimation(utTrainedWolf, uaWalk, dirNW, 0, 0, 10699, 8, 3);
  AddAnimation(utTrainedWolf, uaWalk, dirE, 0, 0, 10707, 8);
  AddAnimation(utTrainedWolf, uaWalk, dirSE, 0, 0, 10715, 8);
  AddAnimation(utTrainedWolf, uaWalk, dirW, 0, 0, 10723, 8);
  AddAnimation(utTrainedWolf, uaWalk, dirSW, 0, 0, 10731, 8, 7);
  AddAnimation(utTrainedWolf, uaWalk, dirS, 0, 0, 10739, 8);

  AddAnimation(utTrainedWolf, uaWork, dirN, 0, 0, [10749,10750,10751,10752,10753,10754, 10755,10812]);
  AddAnimation(utTrainedWolf, uaWork, dirNE, 0, 0, 10755, 8);
  AddAnimation(utTrainedWolf, uaWork, dirNW, 0, 0, 10763, 8, 3);
  AddAnimation(utTrainedWolf, uaWork, dirE, 0, 0, 10771, 8);
  AddAnimation(utTrainedWolf, uaWork, dirSE, 0, 0, 10779, 8);
  AddAnimation(utTrainedWolf, uaWork, dirW, 0, 0, 10787, 8);
  AddAnimation(utTrainedWolf, uaWork, dirSW, 0, 0, 10795, 8, 7);
  AddAnimation(utTrainedWolf, uaWork, dirS, 0, 0, 10803, 8);

  fItems[utTrainedWolf].fUnitSprite.Act[uaSpec] := fItems[utTrainedWolf].fUnitSprite.Act[uaWalk];



  BootsAnim.Create(10, 0, 9596, 6);

  //we can override it in JSON file
  fItems[utTrainedWolf].TownhallCost := 4;
  fItems[utRebel].TownhallCost := 2;
  fItems[utRogue].TownhallCost := 3;
  fItems[utVagabond].TownhallCost := 5;
  fItems[utBarbarian].TownhallCost := 8;
  fItems[utWarrior].TownhallCost := 10;

  fItems[utRam].CanAttackUnits := false;
  fItems[utAmmoCart].CanAttackUnits := false;
  fItems[utAmmoCart].CanAttackHouses := false;
  fItems[utSpy].CanAttackUnits := false;
  fItems[utWoodenWall].CanAttackUnits := false;

  fItems[utMilitia].CanStorm := true;
  fItems[utAxeFighter].CanStorm := true;
  fItems[utSwordFighter].CanStorm := true;
  fItems[utBarbarian].CanStorm := true;
  fItems[utWarrior].CanStorm := false;
  fItems[utTrainedWolf].CanStorm := true;

  //fItems[utVagabond].CanAttackHouses := aType in UNITS_WARRIORS;

  for UT := WARRIOR_MIN to WARRIOR_MAX do
    if UT <> utSpy then
      for dir := dirN to dirNW do
      begin
        if UNIT_TO_GROUP_TYPE[UT] = gtAny then
        begin
          fItems[UT].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveY := -70 + FlagYOffset[gtMelee, dir];
          fItems[UT].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveX := 0 + FlagXOffset[gtMelee, dir];
        end else
        begin
          fItems[UT].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveY := -70 + FlagYOffset[UNIT_TO_GROUP_TYPE[UT], dir];
          fItems[UT].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveX := 0 + FlagXOffset[UNIT_TO_GROUP_TYPE[UT], dir];
        end;
      end;

  //for dir := dirN to dirNW do
  //  Inc(fItems[utBallista].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveX, 10);

  //for dir := dirN to dirNW do
  //  Inc(fItems[utFighter].fUnitSprite.Act[uaWalkArm].Dir[dir].MoveY, 22);
  ShipSketch[dirN].Create(0, 0, 12844, 13);
  ShipSketch[dirNE].Create(0, 0, 12857, 14);
  ShipSketch[dirE].Create(0, 0, 12871, 14);
  ShipSketch[dirSE].Create(0, 0, 12885, 14);
  ShipSketch[dirS].Create(0, 0, 12899, 14);
  ShipSketch[dirSW].Create(0, 0, 12913, 14);
  ShipSketch[dirW].Create(0, 0, 12927, 14);
  ShipSketch[dirNW].Create(0, 0, 12941, 14);

  for dir := dirN to dirNW do
  begin
    FishermansShipSketch[dir].Create(0, 0, 14666 + (byte(dir) - 1) * 6, 6);
    BattleShipSketch[dir].Create(0, 0, 14714 + (byte(dir) - 1) * 16, 16);
  end;

  TH_GAME_ORDER := [utRebel, utRogue, utVagabond, utBarbarian, utWarrior];
  BARRACKS_GAME_ORDER := [utMilitia, utAxefighter, utSwordfighter, utBowMan, utCrossbowMan, utLanceCarrier, utPikeman, utScout, utKnight];
  PALACE_UNITS_ORDER := [utPaladin, utArcher, utSpy, utTrainedWolf, utAmmoCart];
  SCHOOL_GAME_ORDER := [utSerf, utBuilder, utStonemason, utWoodcutter, utCarpenter,
                        utFisher, utFarmer, utBaker, utAnimalBreeder, utButcher,
                        utMiner, utMetallurgist, utSmith, utRecruit];
  SIEGE_GAME_ORDER := [utCatapult, utBallista, utRam];
  SHIPYARD_ORDER := [utBoat, utShip, utBattleShip];


  Explosion.Create(10, 0, [13354, 13355, 13356, 13357, 13358]);
  Explosion.Extend(0);
  RageAnim.Create(10, 0, [14843, 14844, 14845, 14844]);

  //convert AnimLoop to Animation
  for UT := UNIT_MIN to UNIT_MAX do
    for act := low(TKMUnitActionType) to High(TKMUnitActionType) do
      for dir := DirN to DirNW do
      begin
        fItems[UT].fUnitSpriteNew[act, dir].Create(fItems[UT].fUnitSprite.Act[act].Dir[dir]);
      end;
  fCRC := fCRC xor LoadFromJson(gRes.JsonData[dtUnits]);

  for UT := UNIT_MIN to UNIT_MAX do
  begin
    fItems[UT].fUnitSpecInfo.StepsPerTile          := Round(1    / fItems[UT].Speed);
    fItems[UT].fUnitSpecInfo.StepsPerTileDiag      := Round(1.41 / fItems[UT].Speed);
    fItems[UT].fUnitSpecInfo.StepsPerTileStorm     := Round(1    / (fItems[UT].Speed * STORM_SPEEDUP));
    fItems[UT].fUnitSpecInfo.StepsPerTileStormDiag := Round(1.41 / (fItems[UT].Speed * STORM_SPEEDUP));
  end;
  CalculateTroopTrainOrder;
end;


destructor TKMResUnits.Destroy;
var
  U: TKMUnitType;
begin
  for U := Low(TKMUnitType) to High(TKMUnitType) do
    //if U <> utRam then
    FreeAndNil(fItems[U]);

  inherited;
end;


procedure TKMResUnits.SaveCustomData(aSaveStream: TKMemoryStream);
begin
  aSaveStream.PlaceMarker('UnitsCustomData');
end;


procedure TKMResUnits.LoadCustomData(aLoadStream: TKMemoryStream);
begin
  aLoadStream.CheckMarker('UnitsCustomData');
end;

procedure TKMResUnits.SetUnitHousesList;
var J : Integer;
  H : TKMHouseType;
  U : TKMUnitType;
begin
  for U := CITIZEN_MIN to CITIZEN_MAX do
    if not (U in [utSerf, utBuilder]) then
      for H := htArmorSmithy to high(TKMHouseType) do
        if gRes.Houses[H].CanHasWorker(U) then
        begin
          J := length(fItems[U].fHouses);
          SetLength(fItems[U].fHouses, J + 1);
          fItems[U].fHouses[J] := H;
        end;

  //fHouses
end;

procedure TKMResUnits.ExportCSV(const aPath: UnicodeString);
var
  ft: textfile;
  ii: TKMUnitType;
  I : Integer;
begin
  If IsFileInUse(aPath) then
    Exit;
  AssignFile(ft,aPath);
  rewrite(ft);

  writeln(ft,'Name;ID;HitPoints;Attack;AttackHorse;Defence;Speed;Sight;');
  for I := 0 to high(UNIT_ID_TO_TYPE) do
  begin
    ii := UNIT_ID_TO_TYPE[I];
    if not fItems[ii].IsValid then
      Continue;
    write(ft,Items[ii].GUIName+';');
    write(ft,IntToStr(I)+ ';');
    write(ft,inttostr(Items[ii].HitPoints)+';');
    write(ft,inttostr(Items[ii].Attack)+';');
    write(ft,inttostr(Items[ii].AttackHorse)+';');
    write(ft,inttostr(Items[ii].Defence)+';');
    write(ft,floattostr(Items[ii].Speed)+';');
    write(ft,inttostr(Items[ii].Sight)+';');
    writeln(ft);

  end;

  {for ii := Low(TKMUnitType) to High(TKMUnitType) do
  if Items[ii].IsValid then
  begin
    write(ft,Items[ii].GUIName+';');
    write(ft,inttostr(Items[ii].HitPoints)+';');
    write(ft,inttostr(Items[ii].Attack)+';');
    write(ft,inttostr(Items[ii].AttackHorse)+';');
    write(ft,inttostr(Items[ii].Defence)+';');
    write(ft,floattostr(Items[ii].Speed)+';');
    write(ft,inttostr(Items[ii].Sight)+';');
    writeln(ft);
  end;}
  closefile(ft);

  {AssignFile(ft,ExeDir+'Units.txt'); rewrite(ft);
  for ii:=Low(TKMUnitType) to High(TKMUnitType) do
  if UnitsDat[ii].IsValid then
  begin
    writeln(ft);
    writeln(ft);
    writeln(ft,'NewUnit'+inttostr(ii));
    for kk:=1 to 14 do
    for hh:=1 to 8 do
    //  if UnitSprite[ii].Act[kk].Dir[hh].Step[1]>0 then
        begin
          write(ft,inttostr(kk)+'.'+inttostr(hh)+#9);
          for jj:=1 to 30 do
          if UnitSprite[ii].Act[kk].Dir[hh].Step[jj]>0 then //write(ft,'#');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Step[jj])+'. ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Count)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveX)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveY)+' ');
          writeln(ft);
        end;
  end;
  closefile(ft);}
end;


function TKMResUnits.GetSerfCarry(aType: TKMWareType; aDir: TKMDirection): TKMAnimLoop;
begin
  Assert(aType in WARES_VALID);

  if aType >= wtBitin then
   Result := fSerfCarryNew[aType, aDir]
  else
   Result := fSerfCarry[aType, aDir];


end;


function TKMResUnits.GetItem(aType: TKMUnitType): TKMUnitSpec;
begin
  Result := fItems[aType];
end;


function TKMResUnits.LoadUnitsDat(const aPath: UnicodeString): Cardinal;
const
  UNIT_DAT_COUNT = 41;
  MISSING_CARRY_ANIMATIONS : array[wtBitin .. WARE_MAX] of TKMWareType =
  (
  wtIron, wtIronOre, wtIronOre, wtStone, wtTrunk,//wtBitin .. wtLog
  wtIron, wtIron, wtTimber, wtBow, wtBow, wtFish, //wtSteelE .. wtWater
  wtStone, wtCoal, wtWine, wtWine, wtStone, wtLeather, //wtTile..wtSawDust
  wtCorn, wtLance, wtPike, wtCoal, wtLeatherArmor,//wtHay..wtPlateArmor
  wtIron, wtIronOre
  );

var
  S: TKMemoryStream;
  I: Integer;
  WT : TKMWareType;
  DIR : TKMDirection;
begin
  Assert(FileExists(aPath), 'unit.dat not found at: ' + aPath);

  S := TKMemoryStreamBinary.Create;
  try
    S.LoadFromFile(aPath);

    S.Read(fSerfCarry, SizeOf(fSerfCarry){28*8*70});

    for I := 0 to UNIT_DAT_COUNT - 1 do
    if not (UNIT_ID_TO_TYPE[I] in UNITS_NEW + [utNone])  then
      fItems[UNIT_ID_TO_TYPE[I]].LoadFromStream(S)
    else //Skip
    if not (UNIT_ID_TO_TYPE[I] in UNITS_NEW) then
      S.Seek(SizeOf(TKMUnitDat) + SizeOf(TKMUnitSprite) + SizeOf(TKMUnitSprite2), soFromCurrent);
    Result := Adler32CRC(S);
    for WT := wtBitin to WARE_MAX do
      for DIR := dirN to dirNW do
        fSerfCarryNew[WT, DIR] := fSerfCarry[MISSING_CARRY_ANIMATIONS[WT], DIR];
        {if WT in SERF_CARRY_DO_WARES then
        begin

          for I := 0 to 7 do
            fSerfCarryNew[WT, DIR].Step[I + 1] := SERF_CARRY_ADD[WT, Dir, I] - 1;

        end;}

      fSerfCarryNew[wtVegetables, dirN].Create(0, 0, 9464, 8);
      fSerfCarryNew[wtVegetables, dirNE].Create(0, 0, 9480, 8);
      fSerfCarryNew[wtVegetables, dirE].Create(0, 0, 9488, 8);
      fSerfCarryNew[wtEgg, dirSE].Create(0, 0, 9464, 8);
      fSerfCarryNew[wtEgg, dirS].Create(0, 0, 9464, 8);
      fSerfCarryNew[wtEgg, dirSW].Create(0, 0, 9464, 8);
      fSerfCarryNew[wtVegetables, dirW].Create(0, 0, 9496, 8);
      fSerfCarryNew[wtVegetables, dirNW].Create(0, 0, 9472, 8);

      fSerfCarryNew[wtBitin, dirN].Create(0, 0, 9400, 8);
      fSerfCarryNew[wtBitin, dirNE].Create(0, 0, 9408, 8);
      fSerfCarryNew[wtBitin, dirE].Create(0, 0, 9424, 8);
      fSerfCarryNew[wtBitin, dirSE].Create(0, 0, 9440, 8);
      fSerfCarryNew[wtBitin, dirS].Create(0, 0, 9448, 8);
      fSerfCarryNew[wtBitin, dirSW].Create(0, 0, 9456, 8);
      fSerfCarryNew[wtBitin, dirW].Create(0, 0, 9432, 8);
      fSerfCarryNew[wtBitin, dirNW].Create(0, 0, 9416, 8);

      fSerfCarryNew[wtBitinOre, dirN].Create(0, 0, 10429, 8);
      fSerfCarryNew[wtBitinOre, dirNE].Create(0, 0, 10445, 8);
      fSerfCarryNew[wtBitinOre, dirE].Create(0, 0, 10453, 8);
      fSerfCarryNew[wtBitinOre, dirSE].Create(0, 0, 10469, 8);
      fSerfCarryNew[wtBitinOre, dirS].Create(0, 0, 10477, 8);
      fSerfCarryNew[wtBitinOre, dirSW].Create(0, 0, 10485, 8);
      fSerfCarryNew[wtBitinOre, dirW].Create(0, 0, 10461, 8);
      fSerfCarryNew[wtBitinOre, dirNW].Create(0, 0, 10437, 8);

      fSerfCarryNew[wtLog, dirN].Create(0, 0, [10493, 10494, 10495, 10496, 10497, 10498, 10555, 10556]);
      fSerfCarryNew[wtLog, dirNE].Create(0, 0, 10499, 8);
      fSerfCarryNew[wtLog, dirE].Create(0, 0, 10515, 8);
      fSerfCarryNew[wtLog, dirSE].Create(0, 0, 10539, 8);
      fSerfCarryNew[wtLog, dirS].Create(0, 0, 10547, 8);
      fSerfCarryNew[wtLog, dirSW].Create(0, 0, 10531, 8);
      fSerfCarryNew[wtLog, dirW].Create(0, 0, 10523, 8);
      fSerfCarryNew[wtLog, dirNW].Create(0, 0, 10507, 8);

      fSerfCarryNew[wtApple, dirN].Create(0, 0, 10365, 8);
      fSerfCarryNew[wtApple, dirNE].Create(0, 0, 10389, 8);
      fSerfCarryNew[wtApple, dirE].Create(0, 0, 10397, 8);
      fSerfCarryNew[wtApple, dirSE].Create(0, 0, 10405, 8);
      fSerfCarryNew[wtApple, dirS].Create(0, 0, 10413, 8);
      fSerfCarryNew[wtApple, dirSW].Create(0, 0, 10421, 8);
      fSerfCarryNew[wtApple, dirW].Create(0, 0, 10373, 8);
      fSerfCarryNew[wtApple, dirNW].Create(0, 0, 10381, 8);

  finally
    S.Free;
  end;

end;

function TKMResUnits.LoadFromJson(aPath: string) : Cardinal;

  function UnitTypeArray(arr : TJsonArray; out aUnits : TKMUnitTypeArray) : Boolean;
  var I : Integer;
    tmp : TKMUnitTypeArray;
    UT : TKMUnitType;
  begin
    for I := 0 to arr.Count - 1 do
    begin
      if not TKMEnumUtils.TryGetAs<TKMUnitType>(arr[I],  UT) then
        raise Exception.Create('Wrong UnitType: ' + arr[I]);
      if UT in [utNone, utAny] then
        Continue;
      SetLength(tmp, length(tmp) + 1);
      tmp[high(tmp)] := UT;
    end;
    aUnits := tmp;
    Result := Length(tmp) > 0;
  end;

var I : Integer;
  nArr: TJsonArray;
  nRoot : TKMJson;
  UT : TKMUnitType;
  //tmp : TKMUnitTypeArray;
begin
  if not FileExists(aPath) then
    Exit(0);

  nRoot := TJsonObject.ParseFromFile(aPath) as TJsonObject;
  //nRoot := gRes.JsonData['units'];
  Result := nRoot.CRC;

  nArr := nRoot.A['Units'];

  for I := 0 to nArr.Count - 1 do
  begin
    if not TKMEnumUtils.TryGetAs<TKMUnitType>(nArr.O[I].S['UnitType'],  UT) then
      raise Exception.Create('Wrong UnitType');

    fItems[UT].LoadFromJson(nArr.O[I]);
  end;
  //townhall
  nRoot.GetArray('Townhall Order', TH_GAME_ORDER);
  nRoot.GetArray('Barracks Order', BARRACKS_GAME_ORDER);
  nRoot.GetArray('School Order', SCHOOL_GAME_ORDER);
  nRoot.GetArray('SiegeWorkshop Order', SIEGE_GAME_ORDER);
  nRoot.GetArray('Palace Order', PALACE_UNITS_ORDER);
  nRoot.GetArray('Shipyard Order', SHIPYARD_ORDER);
  {
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(TH_GAME_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      TH_GAME_ORDER[I] := tmp[I];
  end;
  //barracks
  nArr := nRoot.A['Barracks Order'];
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(BARRACKS_GAME_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      BARRACKS_GAME_ORDER[I] := tmp[I];
  end;
  //school
  nArr := nRoot.A['School Order'];
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(SCHOOL_GAME_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      SCHOOL_GAME_ORDER[I] := tmp[I];
  end;
  //siege
  nArr := nRoot.A['SiegeWorkshop Order'];
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(SIEGE_GAME_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      SIEGE_GAME_ORDER[I] := tmp[I];
  end;
  //Palace
  nArr := nRoot.A['Palace Order'];
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(PALACE_UNITS_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      PALACE_UNITS_ORDER[I] := tmp[I];
  end;

  //shipsyard
  nArr := nRoot.A['Shipyard Order'];
  if UnitTypeArray(nArr, tmp) then
  begin
    SetLength(SHIPYARD_ORDER, length(tmp));
    for I := 0 to High(tmp) do
      SHIPYARD_ORDER[I] := tmp[I];
  end;}
end;

Procedure TKMResUnits.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin
  oldCRC := fCRC;
  fCRC := LoadUnitsDat(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'unit.dat');
  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtUnits]);
  if not UpdateCRC then
    fCRC := oldCRC;
end;

procedure TKMResUnits.CalculateTroopTrainOrder;
  function UnitArrContains(UT : TKMUnitType; arr : array of TKMUnitType) : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := Low(arr) to High(arr) do
      if arr[I] = UT then
        Exit(true);

  end;

  procedure SwapUnits(var A, B : TKMUnitType);
  var tmp : TKMUnitType;
  begin
    tmp := A;
    A := B;
    B := tmp;
  end;

var GT : TKMGroupType;
  I, K : Integer;
  UT : TKMUnitType;
begin
  for UT in UNITS_HUMAN do
  begin
    with fItems[UT] do
    begin
      fTrainingHouses := [];
      if UnitArrContains(UT, SCHOOL_GAME_ORDER) then
        fTrainingHouses := fTrainingHouses + [htSchool];

      if UnitArrContains(UT, BARRACKS_GAME_ORDER) then
        fTrainingHouses := fTrainingHouses + [htBarracks];

      if UnitArrContains(UT, TH_GAME_ORDER) then
        fTrainingHouses := fTrainingHouses + [htTownhall];

      if UnitArrContains(UT, SIEGE_GAME_ORDER) then
        fTrainingHouses := fTrainingHouses + [htSiegeWorkshop];

      if UnitArrContains(UT, PALACE_UNITS_ORDER) then
        fTrainingHouses := fTrainingHouses + [htPalace];
      if UnitArrContains(UT, SHIPYARD_ORDER) then
        fTrainingHouses := fTrainingHouses + [htShipYard];
    end;

    if UT in UNITS_WARRIORS then
    begin
      GT := UNIT_TO_GROUP_TYPE[UT];
      if GT in [gtNone] then Continue;

      if GT = gtAny then
        Continue;
      {begin
        for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
        begin
          I := length(AI_TROOP_TRAIN_ORDER_NEW[GT]);
          SetLength(AI_TROOP_TRAIN_ORDER_NEW[GT], I + 1);
          AI_TROOP_TRAIN_ORDER_NEW[GT][I] := UT;
        end;
      end else}
      begin
        I := length(AI_TROOP_TRAIN_ORDER_NEW[GT]);
        SetLength(AI_TROOP_TRAIN_ORDER_NEW[GT], I + 1);
        AI_TROOP_TRAIN_ORDER_NEW[GT][I] := UT;
      end;
    end;

  end;


  //sort units in training order by its powers
  for GT := Low(AI_TROOP_TRAIN_ORDER_NEW) to High(AI_TROOP_TRAIN_ORDER_NEW) do
  begin
    for I := 0 to High(AI_TROOP_TRAIN_ORDER_NEW[GT]) do
      for K := I to High(AI_TROOP_TRAIN_ORDER_NEW[GT]) do
        if fItems[AI_TROOP_TRAIN_ORDER_NEW[GT][I]].GetUnitPower < fItems[AI_TROOP_TRAIN_ORDER_NEW[GT][K]].GetUnitPower then
          SwapUnits(AI_TROOP_TRAIN_ORDER_NEW[GT][I], AI_TROOP_TRAIN_ORDER_NEW[GT][K]);
  end;

  for UT in UNITS_HUMAN do
    ;

end;

procedure TKMUnitSpriteSingle.Clear;
var d : TKMDirection;
begin
  for d := dirN to dirNE do
    Dir[d].Clear;

end;

end.
