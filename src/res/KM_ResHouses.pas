unit KM_ResHouses;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults,KM_Points, VCL.Dialogs,
  KM_ResTypes;

const
  HOUSE_MIN = htArmorSmithy;
  HOUSE_MAX = high(TKMHouseType);
  HOUSES_VALID = [HOUSE_MIN..HOUSE_MAX];

  HOUSES_CNT = Integer(HOUSE_MAX) - Integer(HOUSE_MIN) + 1;
  WALL_UNLOCK_HOUSE = htBarracks;
  NO_HOUSE_IMAGE = 5;
type
  THouseAnim = array [TKMHouseActionType] of TKMAnimLoop;

  TKMHouseLevelRec = record
  private
  public
    StonePic : Word;
    SnowPic : Word;
    Progress : Word;
    StoneCost : Byte;
    TileCost : Byte;
    WoodCost : Byte;
    ReplaceAnim : Boolean;
    Anim : array of TKMAnimLoop;
    MaxInWares : Word;
    AnimMultiplier : Single;
    HideSupplies: Boolean;
    function BuildingStep : Word;
  end;

  //House fields as they are in a DAT file
  TKMHouseDat = packed record
    StonePic, WoodPic, WoodPal, StonePal: SmallInt;
    SupplyIn: THouseSupply;
    SupplyOut: THouseSupply;
    Anim: THouseAnim;
    WoodPicSteps, StonePicSteps: Word;
    a1: SmallInt;
    EntranceOffsetX, EntranceOffsetY: ShortInt;
    EntranceOffsetXpx, EntranceOffsetYpx: ShortInt; //When entering house units go for the door, which is offset by these values
    BuildArea: array [1..10,1..10] of ShortInt;
    WoodCost,StoneCost: Byte;
    BuildSupply : THouseBuildSupplyOld;
    a5,SizeArea: SmallInt;
    SizeX,SizeY,sx2,sy2: ShortInt;
    WorkerWork,WorkerRest: SmallInt;
    WareInput, WareOutput: array [1..4] of ShortInt; //KaM_Remake will use its own tables for this matter
    ResProductionX: ShortInt;
    MaxHealth,Sight: SmallInt;
    WorkerType: ShortInt;
    Foot1: array [1..12] of ShortInt; //Sound indices
    Foot2: array [1..12] of SmallInt; //vs sprite ID
  end;

  // This class wraps KaM House info
  // it hides unused fields and adds new ones
  TKMHouseSpec = class
  private
    fHouseType: TKMHouseType; //Our class
    fDescriptionID : Word;
    fNameTextID: Integer;
    fHouseDat: TKMHouseDat;
    fTileCost: Byte;

    fWariants : TKMWordArray;
    fWareInput, fWareOutput: TKMWareType8; //KaM_Remake will use its own tables for this matter
    fSupplyIn: THouseSupply8;
    fSupplyOut: THouseSupply8;
    fBuildSupply : THouseBuildSupply;
    fGatheringScript : TKMGatheringScript;
    fMaxInWares : Word;
    fMaxOutWares : Word;
    fUnlockAfter : TKMHouseTypeSet;
    fCanForceWork : Boolean;
    function GetArea: TKMHouseArea;
    function GetDoesOrders: Boolean;
    function GetGUIIcon: Word;
    function GetHouseName: UnicodeString;
    function GetWareInput: TKMWareType8;
    function GetWareOutput: TKMWareType8;
    function GetWorkerType: TKMUnitType;
    function GetGUIWorkerType: TKMUnitType;
    function GetReleasedBy: TKMHouseTypeSet;
    function GetTabletIcon: Word;
    function GetSnowPic: SmallInt;
    function GetUnoccupiedMsgId: SmallInt;
    function GetGroundVisibleArea: TKMHouseArea;
    function GetBuildSupply : THouseBuildSupply;
    function GetMaxWareCount : Word;
    function GetMaxWareOutCount : Word;
    function GetHouseDescription : UnicodeString;
    procedure SaveToNewStream(aSaveStream : TKMemoryStream);
    procedure LoadFromNewStream(aLoadStream : TKMemoryStream);
    procedure AddWorker(aType : TKMUnitType; aCount : Byte);
  public
    Sound : array[haWork1..haWork5] of record
      ID : Integer;
      Steps : TByteSet;
    end;

    WorkAnim : array of record
      Action : TKMHouseActionType;
      Cycles : Single;
    end;

    WareInputSlots : array of record
      WareInput : TKMWareType8;
      WareOutput : TKMWareType8;
      Icon : Word;
    end;

    Styles : array of record
      StonePic : Word;
      SnowPic : Word;
      Icon : Word;
      HideSupplies: Boolean;
    end;

    Levels : array of TKMHouseLevelRec;
    Workers: array of record
      UnitType : TKMUnitType;
      MaxCount : Byte;
    end;

    MaxWorkersCount: Byte;
    //WorkerTypes: TKMUnitTypeSet;
    //UniqueWorkers : Boolean;
    CanOverFill: Boolean;


    constructor Create(aHouseType: TKMHouseType);
    procedure LoadFromStream(Stream: TMemoryStream);

    // Property accessors:
    // Derived from KaM

    property StonePic: Smallint read fHouseDat.StonePic;
    property WoodPic: Smallint read fHouseDat.WoodPic;
    property WoodPal: Smallint read fHouseDat.WoodPal;
    property StonePal: Smallint read fHouseDat.StonePal;
    property SupplyIn: THouseSupply8 read fSupplyIn;
    property SupplyOut: THouseSupply8 read fSupplyOut;
    property Anim: THouseAnim read fHouseDat.Anim;
    property WoodPicSteps: Word read fHouseDat.WoodPicSteps;
    property StonePicSteps: Word read fHouseDat.StonePicSteps;
    property EntranceOffsetX: ShortInt read fHouseDat.EntranceOffsetX;
    property EntranceOffsetY: ShortInt read fHouseDat.EntranceOffsetY;
    property EntranceOffsetXpx: ShortInt read fHouseDat.EntranceOffsetXpx;
    property EntranceOffsetYpx: ShortInt read fHouseDat.EntranceOffsetYpx;
    property WoodCost: Byte read fHouseDat.WoodCost;
    property StoneCost: Byte read fHouseDat.StoneCost;
    property BuildSupply: THouseBuildSupply read GetBuildSupply;{fHouseDat.BuildSupply}
    property WorkerRest: Smallint read fHouseDat.WorkerRest;
    property ResProductionX: ShortInt read fHouseDat.ResProductionX;
    property Sight: Smallint read fHouseDat.Sight;
    property WorkerType: TKMUnitType read GetWorkerType;
    property GUIWorkerType: TKMUnitType read GetGUIWorkerType;
    function CanHasWorker: Boolean;overload;
    function CanHasWorker(aUnitType : TKMUnitType): Boolean;overload;
    function HasStoneWariants : Byte;
    function GetStoneWariant(aID : Integer) : Integer;
    // Additional properties added by Remake
    property BuildArea: TKMHouseArea read GetArea;
    property GroundVisibleArea: TKMHouseArea read GetGroundVisibleArea;
    property DoesOrders: Boolean read GetDoesOrders;
    property GUIIcon: Word read GetGUIIcon;
    property HouseDescription: UnicodeString read GetHouseDescription;
    property HouseName: UnicodeString read GetHouseName;
    property HouseNameTextID: Integer read fNameTextID;
    property ReleasedBy: TKMHouseTypeSet read GetReleasedBy;
    property WareInput: TKMWareType8 read GetWareInput;
    property WareOutput: TKMWareType8 read GetWareOutput;
    property TabletIcon: Word read GetTabletIcon;
    property UnoccupiedMsgId: SmallInt read GetUnoccupiedMsgId;
    property SnowPic: SmallInt read GetSnowPic;
    property HasWariants: Byte read HasStoneWariants;
    property GatheringScript : TKMGatheringScript read fGatheringScript;
    property MaxWareCount : Word read GetMaxWareCount;
    property MaxWareOutCount : Word read GetMaxWareOutCount;
    property CanForceWork : Boolean read fCanForceWork;
    // Functions
    function AcceptsWares: Boolean;

    function MaxHealth: Word;
    function MaxWoodHealth: Word;
    function MaxStoneHealth: Word;
    function TotalBuildSupply: Word;
    function HealthSupplyStep: Word;
    function TileCost : Word;
    function ProducesWares: Boolean;
    function IsWorkshop: Boolean;
    function FlagPointTexId: Word;
    procedure Outline(aList: TKMPointList);
    function GetDoorwayOffset(aCheck: TKMCheckAxis): Single;
    function GetWareProdCt(aWare : TKMWareType) : byte;
    function GetRandomStonePic : Word;
  end;


  TKMResHouses = class
  private
    fCRC: Cardinal;
    fItems: array [HOUSE_MIN..HOUSE_MAX] of TKMHouseSpec;
    //Swine&Horses, 5 beasts in each house, 3 ages for each beast
    fBeastAnim: array [1..2,1..5,1..3] of TKMAnimLoop;
    fMarketBeastAnim: array [1..3] of TKMAnimLoop;
    fHovelBeastAnim: array [1..3] of TKMAnimLoop;
    function LoadHouseDat(const aPath: string): Cardinal;//deprecated
    procedure CreateHouses;
    function GetHouse(aType: TKMHouseType): TKMHouseSpec; inline;
    function GetBeastAnim(aType: TKMHouseType; aBeast, aAge:integer): TKMAnimLoop;
  public
    Palace_Flags : array[1..4] of TKMAnimLoop;
    Shool_Clock,
    WallTower_RecruitLeft,
    WallTower_RecruitRight,
    Merchant_Tablets,
    Silo_Tablets: TKMAnimLoop;

    ProdThatch_Anims : array[TKMProdThatchAnimType] of TKMAnimation;
    SimilarTypes : TKMHouseTypeArray2;

    constructor Create;
    destructor Destroy; override;

    function IsValid(aType: TKMHouseType): Boolean;

    property Houses[aType: TKMHouseType]: TKMHouseSpec read GetHouse; default;
    property BeastAnim[aType: TKMHouseType; aBeast, aAge: Integer]: TKMAnimLoop read GetBeastAnim;
    property CRC: Cardinal read fCRC; //Return hash of all values

    procedure SaveToNewStream;
    procedure LoadFromNewStream;

    procedure ExportCSV(const aPath: string);

    function LoadFromJSON(aFileName : String) : Cardinal;
    procedure ReloadJSONData(UpdateCRC : Boolean);
  end;


const
  //Sprites in the marketplace
  MARKET_WARES_OFF_X = -93;
  MARKET_WARES_OFF_Y = -88;
  MARKET_WARES_TEX_CNT = 323;
  MARKET_WARES_TEX_START = 1724; //ID of where market ware sprites start. Allows us to relocate them easily.
  MARKET_WARES: array[TKMWareType] of record
                                         TexStart: Integer; //Tex ID for first sprite
                                         Count: Integer; //Total sprites for this resource
                                       end
  = (
      (TexStart: 0; Count: 0;), //rtNone

      (TexStart: MARKET_WARES_TEX_START+237; Count: 20;), //rtTrunk
      (TexStart: MARKET_WARES_TEX_START+47;  Count: 36;), //rtStone
      (TexStart: MARKET_WARES_TEX_START+94;  Count: 19;), //rtWood
      (TexStart: MARKET_WARES_TEX_START+113; Count: 11;), //rtIronOre
      (TexStart: MARKET_WARES_TEX_START+135; Count: 12;), //rtGoldOre
      (TexStart: MARKET_WARES_TEX_START+207; Count: 11;), //rtCoal
      (TexStart: MARKET_WARES_TEX_START+130; Count: 5;),  //rtSteel
      (TexStart: MARKET_WARES_TEX_START+147; Count: 9;),  //rtGold
      (TexStart: MARKET_WARES_TEX_START+1;   Count: 23;), //rtWine
      (TexStart: MARKET_WARES_TEX_START+24;  Count: 23;), //rtCorn
      (TexStart: MARKET_WARES_TEX_START+218; Count: 12;), //rtBread
      (TexStart: MARKET_WARES_TEX_START+186; Count: 12;), //rtFlour
      (TexStart: MARKET_WARES_TEX_START+156; Count: 9;),  //rtLeather
      (TexStart: MARKET_WARES_TEX_START+283; Count: 16;), //rtSausages
      (TexStart: MARKET_WARES_TEX_START+299; Count: 6;),  //rtPig
      (TexStart: MARKET_WARES_TEX_START+230; Count: 7;),  //rtSkin
      (TexStart: MARKET_WARES_TEX_START+85;  Count: 9;),  //rtShield
      (TexStart: MARKET_WARES_TEX_START+127; Count: 3;),  //rtMetalShield
      (TexStart: MARKET_WARES_TEX_START+165; Count: 6;),  //rtArmor
      (TexStart: MARKET_WARES_TEX_START+124; Count: 3;),  //rtMetalArmor
      (TexStart: MARKET_WARES_TEX_START+201; Count: 6;),  //rtAxe
      (TexStart: MARKET_WARES_TEX_START+183; Count: 3;),  //rtSword
      (TexStart: MARKET_WARES_TEX_START+171; Count: 6;),  //rtPike
      (TexStart: MARKET_WARES_TEX_START+198; Count: 3;),  //rtHallebard
      (TexStart: MARKET_WARES_TEX_START+177; Count: 6;),  //rtBow
      (TexStart: MARKET_WARES_TEX_START+83;  Count: 2;),  //rtArbalet
      (TexStart: 0;                      Count: 2;),  //rtHorse (defined in fMarketBeastAnim)
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtFish
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtEgg
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitin
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //rtBitinOre
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //water
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //Tile
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //Seed
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //SawDust
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //SawDust
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //jewerly
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBoots
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtBitinArmor
      (TexStart: MARKET_WARES_TEX_START+305; Count: 19;), //wtEgg

      (TexStart: 0; Count: 0;), //rtAll
      (TexStart: 0; Count: 0;), //rtWarfare
      (TexStart: 0; Count: 0;)  //rtFood
    );

  //These tables are used to convert between KaM script IDs and Remake enums
  HOUSE_DAT_COUNT = byte(high(TKMHouseType));
  //KaM scripts and HouseDat address houses in this order
  HOUSE_ID_TO_TYPE: array [0 .. HOUSE_DAT_COUNT - 1] of TKMHouseType = (
    htSawmill, htIronSmithy, htWeaponSmithy, htCoalMine, htIronMine,
    htGoldMine, htFishermans, htBakery, htFarm, htWoodcutters,
    htArmorSmithy, htStore, htStables, htSchool, htQuarry,
    htMetallurgists, htSwine, htWatchTower, htTownHall, htWeaponWorkshop,
    htArmorWorkshop, htBarracks, htMill, htSiegeWorkshop, htButchers,
    htTannery, htNone, htInn, htVineyard, htMarket,

    htWall, htWall2, htWall3, htWall4, htWall5,
    htHovel, htSign, htBitinMine, htWallTower, htWell,
    htStoneWorkshop, htIronFoundry, htMerchant, htPottery, htWoodBurner,
    htAppleTree, htSmallStore, htCollectors, htTailorsShop, htCottage,
    htHouse, htPalace, htStall, htProductionThatch, htShipyard);

  //TKMHouseType corresponds to this index in KaM scripts and libs
  //KaM scripts are 0 based, so we must use HouseTypeToIndex[H]-1 in script usage. Other cases are 1 based.
  HOUSE_TYPE_TO_ID: array [TKMHouseType] of Byte = (0, 0,
    11, 21, 8,  22, 25,
    4,  9,  7,  6,  28,
    5,  2,  30, 16, 23,
    15, 1,  14, 24, 13,
    12, 17, 26, 19, 18,
    3,  20, 29, 10,
    //added by me
    31, 32, 33, 34, 35,
    36, 37, 38, 39, 40,
    41, 42, 43, 44, 45,
    46, 47, 48, 49, 50,
    51, 52, 53, 54, 55
    );

  WALL_HOUSES : TKMHouseTypeSet = [ htWall, htWall2, htWall3, htWall4, htWall5];
  IGNORE_HOUSE_BLOCK : TKMHouseTypeSet = [htWall..htWall5, htAppleTree];

Var
  HOUSE_GUI_TAB_ORDER : array of record
    TextID, GuiIcon : Word;
    H : TKMHouseTypeArray2;
  end;
  HOUSE_VICTORY_ORDER : array of record
    TextID, GuiIcon : Word;
    H : TKMHouseTypeArray;
  end;

implementation
uses
  TypInfo, KromUtils,
  JsonDataObjects, KM_Resource,
  KM_Outline, KM_PolySimplify, KM_ResTexts, KM_ResWares, KM_ResUnits, KM_CommonClassesExt,
  KM_JSONUtils, KM_JsonHelpers,
  KM_CommonUtils;

CONST
  JSON_PATH = 'data' + PathDelim + 'defines' + PathDelim;

type
  //Additional house info appended to classic format
  THouseInfo = record
    PlanYX: TKMHouseArea;
    NeedsPlayerOrder: Boolean; //Does house output needs to be ordered by Player or it's producing by itself
    BuildIcon: Word;  //Icon in GUI
    TabletSpriteId: Word; //House area WIP tablet
    Input: TKMWareType4;
    Output: TKMWareType4;
    UnlockedByHouse: TKMHouseType; //Which house type allows to build this house type
    SnowSpriteId: SmallInt;
    GroundArea: TKMHouseArea; //Ground that is visible on house tiles (and a bit near). With weight of 'how much' is ground visible
  end;

const
  // Remake stores additional house properties here. This looks like House.Dat, but hardcoded.
  HOUSE_DAT_X: array [HOUSE_MIN..HOUSE_MAX] of THouseInfo = (
    ( //Armor smithy
    PlanYX:           ((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        311;
    TabletSpriteId:   261;
    Input:            (wtCoal,       wtIron,      wtNone,       wtNone);
    Output:           (wtIronArmor, wtIronShield,wtNone,       wtNone);
    UnlockedByHouse:  htIronSmithy;
    SnowSpriteId:     2074;
    GroundArea:       ((0,0,0,0), (1,2,2,1), (3,1,0,2), (3,1,2,3));
    ),
    ( //Armor workshop
    PlanYX:           ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        321;
    TabletSpriteId:   271;
    Input:            (wtLeather,    wtTimber,       wtNone,       wtNone);
    Output:           (wtWoodenShield,     wtLeatherArmor,      wtQuiver,       wtSawDust);
    UnlockedByHouse:  htTannery;
    SnowSpriteId:     2067;
    GroundArea:       ((0,0,0,0), (0,3,1,0), (0,0,0,1), (0,1,2,3));
    ),
    ( //Bakery
    PlanYX:           ((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        308;
    TabletSpriteId:   258;
    Input:            (wtFlour,      wtNone,       wtWater,       wtNone);
    Output:           (wtBread,      wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htMill;
    SnowSpriteId:     2054;
    GroundArea:       ((0,0,0,0), (0,3,1,2), (0,1,0,1), (0,3,2,1));
    ),
    ( //Barracks
    PlanYX:           ((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        322;
    TabletSpriteId:   272;
    Input:            (wtWarfare,    wtNone,       wtNone,       wtNone);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2075;
    GroundArea:       ((2,0,0,1), (0,0,0,0), (1,0,0,1), (4,1,2,2));
    ),
    ( //Butchers
    PlanYX:           ((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        325;
    TabletSpriteId:   275;
    Input:            (wtPig,        wtNone,       wtNone,       wtNone);
    Output:           (wtSausage,   wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSwine;
    SnowSpriteId:     2066;
    GroundArea:       ((0,0,0,0), (0,2,2,3), (0,0,0,1), (0,2,2,1));
    ),
    ( //Coal mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        304;
    TabletSpriteId:   254;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtCoal,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2070;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (2,1,1,0), (3,1,3,0));
    ),
    ( //Farm
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        309;
    TabletSpriteId:   259;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtCorn,       wtSeed,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2055;
    GroundArea:       ((0,0,0,0), (3,2,2,3), (2,0,0,2), (3,1,3,3));
    ),
    ( //Fisher hut
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        307;
    TabletSpriteId:   257;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtFish,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2053;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,0,1), (0,1,1,3));
    ),
    ( //Gold mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        306;
    TabletSpriteId:   256;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtGoldOre,    wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2073;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,2,1,0));
    ),
    ( //Inn
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        328;
    TabletSpriteId:   278;
    Input:            (wtBread,      wtSausage,    wtWine,       wtFish);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htStore;
    SnowSpriteId:     2063;
    GroundArea:       ((0,0,0,0), (1,0,0,2), (2,0,0,2), (4,1,2,3));
    ),
    ( //Iron mine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        305;
    TabletSpriteId:   255;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtIronOre,    wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2052;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,3,1,3));
    ),
    ( //Iron smithy
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        302;
    TabletSpriteId:   252;
    Input:            (wtIronOre,    wtCoal,       wtBitinOre,       wtNone);
    Output:           (wtIron,      wtBitin,       wtNone,       wtNone);
    UnlockedByHouse:  htIronMine;
    SnowSpriteId:     2051;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (3,0,0,2), (3,2,1,3));
    ),
    ( //Marketplace
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        327;
    TabletSpriteId:   277;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2072;
    GroundArea:       ((0,0,0,0), (2,2,1,1), (0,0,0,0), (2,2,2,1));
    ),
    ( //Metallurgist
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: true;
    BuildIcon:        316;
    TabletSpriteId:   266;
    Input:            (wtGoldOre,    wtCoal,       wtNone,       wtNone);
    Output:           (wtGold,       wtJewerly,       wtNone,       wtNone);
    UnlockedByHouse:  htGoldMine;
    SnowSpriteId:     2068;
    GroundArea:       ((0,0,0,0), (3,0,2,0), (3,1,2,0), (3,1,3,0));
    ),
    ( //Mill
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        323;
    TabletSpriteId:   273;
    Input:            (wtSeed,       wtNone,       wtNone,       wtNone);
    Output:           (wtFlour,      wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htFarm;
    SnowSpriteId:     2062;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,1,3), (0,3,1,4));
    ),
    ( //Quarry
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        315;
    TabletSpriteId:   265;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtStone,      wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSchool;
    SnowSpriteId:     2058;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,0,1), (0,3,1,3));
    ),
    ( //Sawmill
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        301;
    TabletSpriteId:   251;
    Input:            (wtTrunk,      wtNone,       wtNone,       wtNone);
    Output:           (wtTimber,       wtSawDust,       wtNone,       wtNone);
    UnlockedByHouse:  htWoodcutters;
    SnowSpriteId:     2050;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (3,1,0,2), (4,1,1,3));
    ),
    ( //School
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        314;
    TabletSpriteId:   264;
    Input:            (wtGold,       wtBoots,       wtNone,       wtNone);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htStore;
    SnowSpriteId:     2059;
    GroundArea:       ((0,0,0,0), (1,0,1,0), (1,0,0,0), (3,1,3,0));
    ),
    ( //Siege workshop
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1));
    NeedsPlayerOrder: false;
    BuildIcon:        324;
    TabletSpriteId:   274;
    Input:            (wtLog,       wtSteelE,      wtWheel,       wtBitinE);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htIronFoundry;
    SnowSpriteId:     2078;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,1,0,2), (0,1,2,3));
    ),
    ( //Stables
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        313;
    TabletSpriteId:   263;
    Input:            (wtCorn,       wtWater,       wtNone,       wtNone);
    Output:           (wtHorse,      wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htFarm;
    SnowSpriteId:     2071;
    GroundArea:       ((0,0,0,0), (2,1,0,2), (1,0,0,1), (3,2,1,4));
    ),
    ( //Store
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0));
    NeedsPlayerOrder: False;
    BuildIcon:        312;
    TabletSpriteId:   262;
    Input:            (wtAll,        wtNone,       wtNone,       wtNone);
    Output:           (wtAll,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htNone;
    SnowSpriteId:     2056;
    GroundArea:       ((0,0,0,0), (3,0,2,0), (2,0,2,0), (3,1,3,0));
    ),
    ( //Swine
    PlanYX:           ((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        317;
    TabletSpriteId:   267;
    Input:            (wtCorn,       wtWater,       wtNone,       wtNone);
    Output:           (wtPig,        wtSkin,       wtNone,       wtNone);
    UnlockedByHouse:  htFarm;
    SnowSpriteId:     2064;
    GroundArea:       ((0,0,0,0), (2,2,0,2), (2,1,0,2), (4,4,3,1));
    ),
    ( //Tannery
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: False;
    BuildIcon:        326;
    TabletSpriteId:   276;
    Input:            (wtSkin,       wtNone,       wtNone,       wtNone);
    Output:           (wtLeather,    wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSwine;
    SnowSpriteId:     2076;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,2), (0,3,1,3));
    ),
    ( //Town hall
    PlanYX:           ((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: False;
    BuildIcon:        319;
    TabletSpriteId:   269;
    Input:            (wtGold,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htMetallurgists;
    SnowSpriteId:     2077;
    GroundArea:       ((0,0,0,0), (1,0,0,2), (2,0,0,2), (3,1,1,2));
    ),
    ( //Watch tower
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        318;
    TabletSpriteId:   268;
    Input:            (wtStoneBolt,      wtNone,       wtNone,       wtNone);
    Output:           (wtNone,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htBarracks;
    SnowSpriteId:     2060;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,1,2,0), (0,3,1,0));
    ),
    ( //Weapon smithy
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        303;
    TabletSpriteId:   253;
    Input:            (wtCoal,       wtIron,      wtNone,       wtNone);
    Output:           (wtSword,      wtPike,  wtCrossbow,    wtNone);
    UnlockedByHouse:  htIronSmithy;
    SnowSpriteId:     2069;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (3,0,0,3), (4,1,2,3));
    ),
    ( //Weapon workshop
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1));
    NeedsPlayerOrder: True;
    BuildIcon:        320;
    TabletSpriteId:   270;
    Input:            (wtTimber,       wtNone,       wtNone,       wtNone);
    Output:           (wtAxe,        wtLance,       wtBow,        wtSawDust);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2061;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (3,0,0,3), (3,1,2,3));
    ),
    ( //Wineyard
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2));
    NeedsPlayerOrder: False;
    BuildIcon:        329;
    TabletSpriteId:   279;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtWine,       wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2065;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,0,0), (0,2,3,1));
    ),
    ( //Woodcutter
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: False;
    BuildIcon:        310;
    TabletSpriteId:   260;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtTrunk,      wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSchool;
    SnowSpriteId:     2057;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (3,0,1,0), (4,2,1,0));
    //SPicWariants:     [2168];
    ),
    //Added by me \///////////////////////////////////////////////////////
    ( //htWall
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1));
    NeedsPlayerOrder: false;
    BuildIcon:        338;
    TabletSpriteId:   268;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  WALL_UNLOCK_HOUSE;
    SnowSpriteId:     2480;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,2,3,2));
    ),
    ( //htWall2
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (1,3,3,2));
    NeedsPlayerOrder: false;
    BuildIcon:        347;
    TabletSpriteId:   268;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  WALL_UNLOCK_HOUSE;
    SnowSpriteId:     2481;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (3,1,1,3));
    ),
    ( //htWall3
    PlanYX:           ((0,0,0,0), (0,0,1,0), (0,0,1,0), (0,0,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        333;
    TabletSpriteId:   268;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  WALL_UNLOCK_HOUSE;
    SnowSpriteId:     2482;
    GroundArea:       ((0,0,0,0), (0,0,2,0), (0,0,2,0), (0,0,2,0));
    ),
    ( //htWall4
    PlanYX:           ((0,0,1,0), (0,0,3,0), (0,0,3,0), (0,0,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        334;
    TabletSpriteId:   268;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  WALL_UNLOCK_HOUSE;
    SnowSpriteId:     2483;
    GroundArea:       ((0,0,3,0), (0,0,1,0), (0,0,1,0), (0,0,3,0));
    ),
    ( //htWall5
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0, 0,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        332;
    TabletSpriteId:   268;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  WALL_UNLOCK_HOUSE;
    SnowSpriteId:     2484;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,3,0));
    ),
    ( //htHovel
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,1), (0, 1,1,2));
    NeedsPlayerOrder: false;
    BuildIcon:        680;
    TabletSpriteId:   679;
    Input:            (wtSeed,       wtWater,       wtNone,       wtNone);
    Output:           (wtFeathers,    wtSausage,       wtNone,       wtNone);
    UnlockedByHouse:  htFarm;
    SnowSpriteId:     2466;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,1,2,3), (0,1,2,3));
    ),
    ( //htSign
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0, 0,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        685;
    TabletSpriteId:   679;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htNone;
    SnowSpriteId:     2608;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,3,0));
    ),
    ( //htBitinMine
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0, 1,1,2));
    NeedsPlayerOrder: false;
    BuildIcon:        686;
    TabletSpriteId:   687;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtBitinOre,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2465;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,3));
    ),
    ( //htWallTower
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,0,0), (0, 1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        689;
    TabletSpriteId:   690;
    Input:            (wtQuiver,       wtNone,       wtNone,       wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2121;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,3,0,0), (0,4,2,0));
    ),
    ( //htWell
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0, 0,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        703;
    TabletSpriteId:   704;
    Input:            (wtNone,       wtNone,       wtNone,       wtNone);
    Output:           (wtWater,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     5;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,3,0));
    ),
    ( //htStoneWorkshop
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,1), (0, 1,1,2));
    NeedsPlayerOrder: true;
    BuildIcon:        691;
    TabletSpriteId:   692;
    Input:            (wtStone,       wtTrunk,       wtNone,       wtNone);
    Output:           (wtStoneBolt,        wtLog,       wtWheel,       wtSawDust);
    UnlockedByHouse:  htQuarry;
    SnowSpriteId:     2472;
    GroundArea:       ((0,0,0,0), (1,1,3,0), (1,1,3,1), (0,3,3,2));
    ),
    ( //htIronFoundry
    PlanYX:           ((0,0,0,0), (0,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: true;
    BuildIcon:        693;
    TabletSpriteId:   694;
    Input:            (wtTrunk,       wtCoal, wtIron,       wtBitin);
    Output:           (wtSteelE,        wtBitinE,       wtBolt,       wtNone);
    UnlockedByHouse:  htIronSmithy;
    SnowSpriteId:     2467;
    GroundArea:       ((0,0,0,0), (0,1,1,0), (2,2,2,0), (3,3,3,0));
    ),
    ( //htMerchant
    PlanYX:           ((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: false;
    BuildIcon:        719;
    TabletSpriteId:   720;
    Input:            (wtTrunk,          wtStone,       wtTimber,     wtTile);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htTownhall;
    SnowSpriteId:     2468;
    GroundArea:       ((0,3,3,3), (0,3,3,3), (0,2,2,2), (0,1,1,1));
    ),
    ( //htPottery
    PlanYX:           ((0,0,0,0), (0,0,1,1), (0,1,1,1), (0,1,2,1));
    NeedsPlayerOrder: false;
    BuildIcon:        708;
    TabletSpriteId:   715;
    Input:            (wtNone,        wtNone,       wtNone,     wtNone);
    Output:           (wtTile,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htSawmill;
    SnowSpriteId:     2464;
    GroundArea:       ((0,0,0,0), (0,0,1,1), (0,2,2,2), (0,3,3,1));
    ),
    ( //htWoodBurner
    PlanYX:           ((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        711;
    TabletSpriteId:   713;
    Input:            (wtTrunk,        wtCorn,       wtSawDust,     wtCoal);
    Output:           (wtCoal,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htFarm;
    SnowSpriteId:     2185;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (1,2,1,0), (1,3,1,0));
    ),
    ( //htAppleTree
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        712;
    TabletSpriteId:   714;
    Input:            (wtWater,        wtNone,       wtNone,     wtNone);
    Output:           (wtApple,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htWell;
    SnowSpriteId:     5;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,2,0), (0,2,2,0));
    ),
    ( //htSmallStore
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        753;
    TabletSpriteId:   752;
    Input:            (wtStone,          wtNone,       wtNone,     wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htNone;
    SnowSpriteId:     2469;
    GroundArea:       ((0,0,0,0), (0,0,0,0), (0,2,2,0), (0,2,2,0));
    ),
    ( //htCollectors
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        731;
    TabletSpriteId:   730;
    Input:            (wtSausage,        wtNone,       wtNone,     wtNone);
    Output:           (wtCoal,        wtStone,       wtIronOre,       wtGoldOre);
    UnlockedByHouse:  htSawMill;
    SnowSpriteId:     2473;
    GroundArea:       ((0,0,0,0), (2,2,2,2), (2,4,4,3), (1,1,3,1));
    ),
    ( //htTailorsShop
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        732;
    TabletSpriteId:   729;
    Input:            (wtLeather,        wtNone,       wtNone,     wtNone);
    Output:           (wtBoots,        wtLeatherArmor,       wtNone,       wtNone);
    UnlockedByHouse:  htTannery;
    SnowSpriteId:     2474;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htCottage
    PlanYX:           ((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        741;
    TabletSpriteId:   739;
    Input:            (wtApple,        wtNone,       wtNone,     wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htStore;
    SnowSpriteId:     2462;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htHouse
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        742;
    TabletSpriteId:   740;
    Input:            (wtApple,        wtNone,       wtNone,     wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2461;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htPalace
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        743;
    TabletSpriteId:   744;
    Input:            (wtBitinE,        wtGold,       wtLeatherArmor,     wtHorse);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2463;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htStall
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        867;
    TabletSpriteId:   868;
    Input:            (wtApple,        wtGold,       wtBread,     wtSausage);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2609;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htProductionThatch
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: true;
    BuildIcon:        869;
    TabletSpriteId:   870;
    Input:            (wtTrunk,        wtGold,       wtBread,     wtSausage);
    Output:           (wtTimber,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     2610;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    ),
    ( //htShipyard
    PlanYX:           ((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,1,2,0));
    NeedsPlayerOrder: false;
    BuildIcon:        872;
    TabletSpriteId:   873;
    Input:            (wtNone,        wtNone,       wtNone,     wtNone);
    Output:           (wtNone,        wtNone,       wtNone,       wtNone);
    UnlockedByHouse:  htPottery;
    SnowSpriteId:     -1;
    GroundArea:       ((2,2,2,2), (2,2,2,2), (2,2,2,2), (2,2,2,2));
    )
    );


  //For some reason in KaM the piles of building supply are not aligned, each one has a different offset.
  //These values were taking from the barracks offsets and are for use with new houses.
  BUILD_SUPPLY_OFFSETS: THouseBuildSupply = ( ( //wood
                                                (MoveX:  0; MoveY: 0), (MoveX: -7; MoveY: 0), (MoveX:-26; MoveY: 0),  //Wood 1-3
                                                (MoveX:-26; MoveY: 0), (MoveX:-26; MoveY:-1), (MoveX:-26; MoveY:-4),   //Wood 4-6
                                                (MoveX:-26; MoveY: 0), (MoveX:-26; MoveY:-1), (MoveX:-26; MoveY:-4)     //Wood 7-9
                                               ),
                                              (
                                                (MoveX:  0; MoveY: 0), (MoveX:  0; MoveY: 0), (MoveX: -7; MoveY: 0),  //Stone 1-3
                                                (MoveX: -7; MoveY:-4), (MoveX:-16; MoveY:-4), (MoveX:-16; MoveY:-4),   //Stone 4-6
                                                (MoveX: -7; MoveY:-4), (MoveX:-16; MoveY:-4), (MoveX:-16; MoveY:-4)   //Stone 7-9
                                              ),
                                              (
                                                (MoveX:  0; MoveY: 0), (MoveX:  0; MoveY: 0), (MoveX: 0; MoveY: 0),  //Tile 1-3
                                                (MoveX: 0; MoveY:0), (MoveX:0; MoveY:0), (MoveX:0; MoveY:0),  //Tile 4-6
                                                (MoveX: 0; MoveY:0), (MoveX:0; MoveY:0), (MoveX:0; MoveY:0)   //Tile 7-9
                                              )
                                               );//Stone 4-6


  //'This house is unoccupied' msg index
  HOUSE_TYPE_2_UNOCCUPIED_MSG_INDEX: array[TKMHouseType] of SmallInt = (
    -1, -1,     //utNone, utAny
    0,1,2,
    -1,         //htBarracks
    3,4,5,6,7,
    -1,         //htInn
    8,9,
    -1,         //htMarketplace
    10,11,12,13,
    -1,         //htSchool
    14,15,
    -1,         //htStore
    16,17,
    -1,         //htTownHall
    18,19,20,21,22,
    //added by me
    -1, -1, -1, -1, -1,//wall
    1684,//htHovel
    -1, 1686,
    1690, -1, 1688, 1687,
    1691, 1683, 1689, 1685, -1, 1692,
    1693,
    -1, -1, -1,//ht cottage, htHouse
    -1, -1, -1
    );

{ TKMHouseDatClass }
function TKMHouseLevelRec.BuildingStep: Word;
begin
  Result := Round(Progress / (WoodCost + StoneCost + TileCost));
end;

constructor TKMHouseSpec.Create(aHouseType: TKMHouseType);
begin
  inherited Create;
  fHouseType := aHouseType;
  fNameTextID := TX_HOUSES_NAMES__29 + HOUSE_TYPE_TO_ID[fHouseType] - 1; //May be overridden for new houses
  SetLength(Workers, 0)
end;


function TKMHouseSpec.AcceptsWares: boolean;
var I : Integer;
begin
  Result := false;
  for I := 1 to 4 do
    if WareInput[I] <> wtNone then //Check if house has any inputs
      Result := true;

  Result := Result or (fHouseType in [htMarket, htStore, htBarracks]); //Marketplace also accepts wares
end;


function TKMHouseSpec.GetArea: TKMHouseArea;
var I, K : Integer;
begin

  for I := 1 to 4 do
    for K := 1 to 4 do
      Result[I, K] := fHouseDat.BuildArea[I, K];


end;

function TKMHouseSpec.HasStoneWariants : Byte;
begin
  Result := Length(fWariants);
end;

function TKMHouseSpec.GetStoneWariant(aID : Integer) : Integer;
begin
  Result := fWariants[aID];
end;

function TKMHouseSpec.GetDoesOrders: Boolean;
begin
  Result := HOUSE_DAT_X[fHouseType].NeedsPlayerOrder;
end;


function TKMHouseSpec.GetGroundVisibleArea: TKMHouseArea;
begin
  Result := HOUSE_DAT_X[fHouseType].GroundArea;
end;

function TKMHouseSpec.GetGUIIcon: Word;
begin
  Result := HOUSE_DAT_X[fHouseType].BuildIcon;
end;


function TKMHouseSpec.GetHouseName: UnicodeString;
begin
  Result := gResTexts[fNameTextID];

end;


//MaxHealth is always a cost of construction * 50
function TKMHouseSpec.MaxHealth: Word;
begin
  //Result := (fHouseDat.WoodCost + fHouseDat.StoneCost) * 50;
  Result := fHouseDat.MaxHealth;
end;

function TKMHouseSpec.MaxWoodHealth: Word;
begin
  Result := HealthSupplyStep * fHouseDat.WoodCost;
end;

function TKMHouseSpec.MaxStoneHealth: Word;
begin
  Result := fHouseDat.MaxHealth - MaxWoodHealth;
end;

function TKMHouseSpec.TotalBuildSupply : Word;
begin
  Result := fHouseDat.WoodCost + fHouseDat.StoneCost + fTileCost;
end;

function TKMHouseSpec.HealthSupplyStep : Word;
begin
  Result := fHouseDat.MaxHealth div (TotalBuildSupply);
end;

function TKMHouseSpec.TileCost: Word;
begin
  Result := fTileCost;
end;
//Write houses outline into given list
procedure TKMHouseSpec.Outline(aList: TKMPointList);
var
  I, K: Integer;
  tmp: TKMByte2Array;
  outlines: TKMShapesArray;
begin
  aList.Clear;
  SetLength(tmp, 6, 6);

  for I := 0 to 3 do
  for K := 0 to 3 do
    tmp[I+1,K+1] := byte(BuildArea[I+1,K+1] > 0);
    //tmp[I+1,K+1] := Byte(HOUSE_DAT_X[fHouseType].PlanYX[I+1,K+1] > 0);

  GenerateOutline(tmp, 2, outlines);
  //Assert(outlines.Count = 1, 'Houses are expected to have single outline');
  for K := 0 to outlines.Count - 1 do
  begin
    aList.Add(KMPoint(-1, -1));
    for I := 0 to outlines.Shape[K].Count - 1 do
      aList.Add(KMPoint(outlines.Shape[K].Nodes[I].X, outlines.Shape[K].Nodes[I].Y));
  end;
end;


function TKMHouseSpec.GetWorkerType: TKMUnitType;
begin
  //fHouseDat.OwnerType is read from DAT file and is ShortInt, it can be out of range (i.e. -1)
  if InRange(fHouseDat.WorkerType, Low(UNIT_ID_TO_TYPE), High(UNIT_ID_TO_TYPE)) then
    Result := UNIT_ID_TO_TYPE[fHouseDat.WorkerType]
  else
    Result := utNone;
end;

procedure TKMHouseSpec.AddWorker(aType : TKMUnitType; aCount : Byte);
begin
  SetLength(Workers, length(Workers) + 1);

  Workers[high(Workers)].UnitType := aType;
  Workers[high(Workers)].MaxCount := aCount;
end;

function TKMHouseSpec.GetGUIWorkerType: TKMUnitType;
var I : Integer;
  C : Byte;
begin
  C := 0;
  Result := utNone;
  {for UT in WorkerTypes do
  begin
    Result := UT;
    Inc(C);
    if C > 1 then
      Exit(utAny);
  end;}

  for I := 0 to High(Workers) do
  begin
    Result := Workers[I].UnitType;
    inc(C, Workers[I].MaxCount);
    if C > 1 then
      Exit(utAny);
  end;

end;

function TKMHouseSpec.IsWorkshop: Boolean;
begin
  Result := fHouseType in [htWeaponSmithy, htArmorSmithy, htWeaponWorkshop, htArmorWorkshop];
end;


function TKMHouseSpec.FlagPointTexId: Word;
begin
  case fHouseType of
    htBarracks:     Result := 249;
    htTownHall:     Result := 249;
    htWoodcutters:  Result := 660;
    htSiegeWorkshop:  Result := 727;
    htCollectors:  Result := 728;
    htPalace:  Result := 745;
  else
    Result := 0; // Has no such thing
  end;
end;

function TKMHouseSpec.GetMaxWareCount : Word;
begin
  Result := fMaxInWares;
end;

function TKMHouseSpec.GetMaxWareOutCount : Word;
begin
  Result := fMaxOutWares;
end;


function TKMHouseSpec.GetHouseDescription : UnicodeString;
begin

  if fDescriptionID < 1000 then
    Result := ''
  else
    Result := gResTexts[fDescriptionID];

end;

// Returns True if this house could have a worker (or occupant)
function TKMHouseSpec.CanHasWorker: Boolean;
var I : Integer;
begin
  //Result := (MaxWorkersCount > 0) and (WorkerTypes <> []);
  Result := false;
  for I := 0 to High(Workers) do
    if not (Workers[I].UnitType in [utAny, utNone, utSerf, utBuilder]) then
      if Workers[I].MaxCount > 0 then
      Exit(true);
  //Result := (WorkerType <> utNone);
end;

function TKMHouseSpec.CanHasWorker(aUnitType: TKMUnitType): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to High(Workers) do
    if (Workers[I].UnitType = aUnitType) then
      if Workers[I].MaxCount > 0 then
      Exit(true);
end;

function TKMHouseSpec.ProducesWares: Boolean;
begin
  Result := not (WareOutput[1] in [wtNone, wtAll, wtWarfare]); //Exclude aggregate types
end;

function TKMHouseSpec.GetReleasedBy: TKMHouseTypeSet;
begin
  Result := fUnlockAfter;
end;


function TKMHouseSpec.GetWareInput: TKMWareType8;
begin
  //Result := HOUSE_DAT_X[fHouseType].Input;
  Result := fWareInput;

end;


function TKMHouseSpec.GetWareOutput: TKMWareType8;
begin
  //Result := HOUSE_DAT_X[fHouseType].Output;
  Result := fWareOutput;

end;


function TKMHouseSpec.GetSnowPic: SmallInt;
begin
  Result := HOUSE_DAT_X[fHouseType].SnowSpriteId;
end;


function TKMHouseSpec.GetTabletIcon: Word;
begin
  Result := HOUSE_DAT_X[fHouseType].TabletSpriteId;
end;

function TKMHouseSpec.GetBuildSupply: THouseBuildSupply;
begin
  Result := fBuildSupply;
end;

function TKMHouseSpec.GetUnoccupiedMsgId: SmallInt;
var
  houseUnnocupiedMsgIndex: SmallInt;
begin
  Result := -1;
  houseUnnocupiedMsgIndex := HOUSE_TYPE_2_UNOCCUPIED_MSG_INDEX[fHouseType];
  if houseUnnocupiedMsgIndex > 1000 then
    Result := houseUnnocupiedMsgIndex
  else
  if houseUnnocupiedMsgIndex <> -1 then
    Result := TX_MSG_HOUSE_UNOCCUPIED__22 + houseUnnocupiedMsgIndex;
end;

function TKMHouseSpec.GetWareProdCt(aWare: TKMWareType): Byte;
begin
  Result := gRes.Wares[aWare].GetProductionCount(fHouseType);
end;

function TKMHouseSpec.GetRandomStonePic : Word;
begin
  if length(fWariants) > 0 then
  begin
    if Random > (1 / (length(fWariants) + 1)) then
      Result := fWariants[Random(Length(fWariants))]
    else
      Result := StonePic;

  end else
    Result := StonePic;
end;


procedure TKMHouseSpec.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fHouseDat, SizeOf(TKMHouseDat));
end;


// Get doorway offset in tile fraction
function TKMHouseSpec.GetDoorwayOffset(aCheck: TKMCheckAxis): Single;
begin
  if aCheck = axX then
    Result := EntranceOffsetXpx - CELL_SIZE_PX div 2
  else
    Result := EntranceOffsetYpx;

  Result := Result / CELL_SIZE_PX;
end;

procedure TKMHouseSpec.SaveToNewStream(aSaveStream : TKMemoryStream);
var I, K : Integer;
  HA : TKMHouseActionType;
  newCount, nC2 : Integer;
begin
  aSaveStream.PlaceMarker('HouseSpec');
  aSaveStream.Write(fHouseDat, SizeOf(fHouseDat));
  for HA := haWork1 to haWork5 do
  begin
    aSaveStream.Write(Sound[HA].ID);
    aSaveStream.Write(Sound[HA].Steps, SizeOf(Sound[HA].Steps));
  end;

  newCount := length(WorkAnim);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    aSaveStream.Write(WorkAnim[I].Action, SizeOf(WorkAnim[I].Action));
    aSaveStream.Write(WorkAnim[I].Cycles);
  end;

  newCount := length(WareInputSlots);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1do
    aSaveStream.Write(WareInputSlots[I], SizeOf(WareInputSlots[I]));


  newCount := length(Styles);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(Styles[I], SizeOf(Styles[I]));

  newCount := length(Levels);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    aSaveStream.Write(Levels[I].StonePic);
    aSaveStream.Write(Levels[I].SnowPic);
    aSaveStream.Write(Levels[I].Progress);
    aSaveStream.Write(Levels[I].StoneCost);
    aSaveStream.Write(Levels[I].TileCost);
    aSaveStream.Write(Levels[I].WoodCost);
    aSaveStream.Write(Levels[I].MaxInWares);
    aSaveStream.Write(Levels[I].AnimMultiplier);
    aSaveStream.Write(Levels[I].HideSupplies);

    nC2 := length(Levels[I].Anim);
    aSaveStream.Write(nC2);
    for K := 0 to nC2 - 1 do
      aSaveStream.Write(Levels[I].Anim[K], SizeOf(Levels[I].Anim[K]));

      
  end;

  aSaveStream.Write(fHouseType, SizeOf(fHouseType));
  aSaveStream.Write(fDescriptionID);
  aSaveStream.Write(fNameTextID);
  aSaveStream.Write(fTileCost);
  aSaveStream.Write(fDescriptionID);
  aSaveStream.Write(fMaxInWares);
  aSaveStream.Write(fWareInput, SizeOf(fWareInput));
  aSaveStream.Write(fWareOutput, SizeOf(fWareOutput));
  aSaveStream.Write(fBuildSupply, SizeOf(fBuildSupply));
  aSaveStream.Write(fGatheringScript, SizeOf(fGatheringScript));
  aSaveStream.Write(fUnlockAfter, SizeOf(fUnlockAfter));
  aSaveStream.Write(fCanForceWork);



  {newCount := length(fWariants);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(fWariants[I]);}

  SaveArrToStream(aSaveStream, fWariants);

end;

procedure TKMHouseSpec.LoadFromNewStream(aLoadStream : TKMemoryStream);
var I, K : Integer;
  HA : TKMHouseActionType;
  newCount, nC2 : Integer;
begin
  aLoadStream.CheckMarker('HouseSpec');
  aLoadStream.Read(fHouseDat, SizeOf(fHouseDat));
  for HA := haWork1 to haWork5 do
  begin
    aLoadStream.Read(Sound[HA].ID);
    aLoadStream.Read(Sound[HA].Steps, SizeOf(Sound[HA].Steps));
  end;

  aLoadStream.Read(newCount);
  SetLength(WorkAnim, newCount);
  for I := 0 to newCount - 1 do
  begin
    aLoadStream.Read(WorkAnim[I].Action, SizeOf(WorkAnim[I].Action));
    aLoadStream.Read(WorkAnim[I].Cycles);
  end;

  aLoadStream.Read(newCount);
  SetLength(WareInputSlots, newCount);
  for I := 0 to newCount - 1do
    aLoadStream.Read(WareInputSlots[I], SizeOf(WareInputSlots[I]));


  aLoadStream.Read(newCount);
  SetLength(Styles, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(Styles[I], SizeOf(Styles[I]));

  aLoadStream.Read(newCount);
  SetLength(Levels, newCount);
  for I := 0 to newCount - 1 do
  begin
    aLoadStream.Read(Levels[I].StonePic);
    aLoadStream.Read(Levels[I].SnowPic);
    aLoadStream.Read(Levels[I].Progress);
    aLoadStream.Read(Levels[I].StoneCost);
    aLoadStream.Read(Levels[I].TileCost);
    aLoadStream.Read(Levels[I].WoodCost);
    aLoadStream.Read(Levels[I].MaxInWares);
    aLoadStream.Read(Levels[I].AnimMultiplier);
    aLoadStream.Read(Levels[I].HideSupplies);

    aLoadStream.Read(nC2);
    SetLength(Levels[I].Anim, nC2);
    for K := 0 to nC2 - 1 do
      aLoadStream.Read(Levels[I].Anim[K], SizeOf(Levels[I].Anim[K]));
  end;

  aLoadStream.Read(fHouseType, SizeOf(fHouseType));
  aLoadStream.Read(fDescriptionID);
  aLoadStream.Read(fNameTextID);
  aLoadStream.Read(fTileCost);
  aLoadStream.Read(fDescriptionID);
  aLoadStream.Read(fMaxInWares);
  aLoadStream.Read(fWareInput, SizeOf(fWareInput));
  aLoadStream.Read(fWareOutput, SizeOf(fWareOutput));
  aLoadStream.Read(fBuildSupply, SizeOf(fBuildSupply));
  aLoadStream.Read(fGatheringScript, SizeOf(fGatheringScript));
  aLoadStream.Read(fUnlockAfter, SizeOf(fUnlockAfter));
  aLoadStream.Read(fCanForceWork);

  LoadArrFromStream(aLoadStream, fWariants);

  {aLoadStream.Read(newCount);
  SetLength(fWariants, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(fWariants[I]);}

end;
{ TKMResHouses }
procedure TKMResHouses.CreateHouses;

  procedure AddAnimation(aHouse: TKMHouseType; aAnim: TKMHouseActionType; aMoveX, aMoveY: Integer; const aSteps: array of SmallInt);
  var
    I: Integer;
  begin
    with fItems[aHouse].fHouseDat.Anim[aAnim] do
    begin
      MoveX := aMoveX;
      MoveY := aMoveY;
      Count := length(aSteps);
      for I := 1 to Count do
        Step[I] := aSteps[I-1];
    end;
  end;

  procedure AddMarketBeastAnim(aBeast: Integer; const aStep: array of SmallInt);
  var
    I: Integer;
  begin
    // Beast anims are 0 indexed
    for I := 1 to 30 do
      fMarketBeastAnim[aBeast].Step[I] := aStep[I - 1] + MARKET_WARES_TEX_START - 1;
  end;

  procedure SetNoImageSupplies(aType : TKMHouseType);
  var I, K : Integer;
  begin
    //fItems[H].fHouseDat.SupplyOut[1,1]
    with fItems[aType].fHouseDat do
      for I := 1 to 4 do
        for K := 1 to 5 do
        begin
         SupplyOut[I,K] := NO_HOUSE_IMAGE;
         SupplyIn[I,K] := NO_HOUSE_IMAGE;
        end;


  end;

  procedure SetClearAnimation(aType : TKMHouseType; aAnim: TKMHouseActionType; aCount : Word);
  var I : Integer;
  begin
    with fItems[aType].fHouseDat.Anim[aAnim] do
    begin
      MoveX := 0;
      MoveY := 0;
      Count := aCount;
      for I := 1 to aCount do
        Step[I] := NO_HOUSE_IMAGE;
    end;

  end;

  procedure ResetSound(aHouse : TKMHouseType);
  var HA : TKMHouseActionType;

  begin
    with  fItems[aHouse] do
      for HA := haWork1 to haWork5 do
        Sound[HA].ID := -1;


  end;

  procedure SetBuildSupplies(aHouse : TKMHouseType);
  var I : Integer;
  begin
    with fItems[aHouse] do
      for I := 1 to 6 do
      begin
        fBuildSupply[1, 6].MoveX := fHouseDat.BuildSupply[1, I].MoveX;
        fBuildSupply[1, 6].MoveY := fHouseDat.BuildSupply[1, I].MoveY;

        fBuildSupply[2, 6].MoveX := fHouseDat.BuildSupply[2, I].MoveX;
        fBuildSupply[2, 6].MoveY := fHouseDat.BuildSupply[2, I].MoveY;
      end;

  end;
var
  H: TKMHouseType;
  I, K: Integer;
var PT : TKMProdThatchAnimType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
  begin
    fItems[H] := TKMHouseSpec.Create(H);

    ResetSound(H);
  end;

  fCRC := LoadHouseDat(ExeDir+'data' + PathDelim + 'defines' + PathDelim + 'houses.dat');
  //LoadFromNewStream;

  for H := HOUSE_MIN to HOUSE_MAX do
  begin
    fItems[H].MaxWorkersCount := 1;
    if not (fItems[H].GetWorkerType in [utNone, utSerf, utBuilder]) then
      fItems[H].AddWorker(fItems[H].GetWorkerType, 1);

      //fItems[H].WorkerTypes := [fItems[H].GetWorkerType];
    fItems[H].fMaxOutWares := MAX_WARES_IN_HOUSE;
    fItems[H].CanOverfill := false;
    for I := 1 to WARES_IN_OUT_COUNT do
      for K := 1 to 5 do
      begin
        fItems[H].fSupplyIn[I, K] := -1;
        fItems[H].fSupplyOut[I, K] := -1;
      end;
  end;


  fItems[htFishermans].fHouseDat.ResProductionX := 1; //we cathc 1 fish per time
  fItems[htFishermans].fHouseDat.WorkerRest := 5; //Set fisher's rest similar to what other houses have

  fItems[htTannery].fHouseDat.Anim[haFlag3].Count := 0; //fix for tannery 2 flags at one place. Flag3 is unnecessary

  fItems[htMarket].fHouseType := htMarket;
  fItems[htMarket].fHouseDat.WorkerType := -1; //No unit works here (yet anyway)
  fItems[htMarket].fHouseDat.StonePic := 150;
  fItems[htMarket].fHouseDat.WoodPic := 151;
  fItems[htMarket].fHouseDat.WoodPal := 152;
  fItems[htMarket].fHouseDat.StonePal := 153;
  fItems[htMarket].fHouseDat.SupplyIn[1,1] := 154;
  fItems[htMarket].fHouseDat.SupplyIn[1,2] := 155;
  fItems[htMarket].fHouseDat.SupplyIn[1,3] := 156;
  fItems[htMarket].fHouseDat.SupplyIn[1,4] := 157;
  fItems[htMarket].fHouseDat.SupplyIn[1,5] := 158;
  fItems[htMarket].fHouseDat.WoodPicSteps := 23;
  fItems[htMarket].fHouseDat.StonePicSteps := 140;
  fItems[htMarket].fHouseDat.EntranceOffsetX := 1;
  fItems[htMarket].fHouseDat.EntranceOffsetXpx := 4; //Enterance is slightly to the left
  fItems[htMarket].fHouseDat.EntranceOffsetYpx := 10;
  fItems[htMarket].fHouseDat.WoodCost := 5;
  fItems[htMarket].fHouseDat.StoneCost := 6;

  for I := 1 to 6 do begin
    fItems[htMarket].fBuildSupply[1,I].MoveX := -55+ BUILD_SUPPLY_OFFSETS[1,I].MoveX;
    fItems[htMarket].fBuildSupply[1,I].MoveY := 15 + BUILD_SUPPLY_OFFSETS[1,I].MoveY;
    fItems[htMarket].fBuildSupply[2,I].MoveX := 28 + BUILD_SUPPLY_OFFSETS[2,I].MoveX;
    fItems[htMarket].fBuildSupply[2,I].MoveY := 20 + BUILD_SUPPLY_OFFSETS[2,I].MoveY;
  end;

  fItems[htMarket].fHouseDat.Sight := 10;
  fItems[htMarket].fHouseDat.SizeArea := 11;
  fItems[htMarket].fHouseDat.SizeX := 4;
  fItems[htMarket].fHouseDat.SizeY := 3;
  fItems[htMarket].fHouseDat.MaxHealth := 550;


  AddAnimation(htMarket, haFlag1, -80, -33, [1165,1166,1167,1163,1164]);


  AddAnimation(htMarket, haFlag2, -73, -7, [1163,1164,1165,1166,1167]);
  AddAnimation(htMarket, haFlag3, 73, -80, [1161,1162,1158,1159,1160]);
  AddAnimation(htMarket, haFire1, 18, -83, [1623,1624,1625,1620,1621,1622]);
  AddAnimation(htMarket, haFire2, 78, -67, [1637,1632,1633,1634,1635,1636]);
  AddAnimation(htMarket, haFire3, -30, -103, [1620,1621,1622,1623,1624,1625]);
  AddAnimation(htMarket, haFire4, -3, -54, [1617,1618,1619,1614,1615,1616]);
  AddAnimation(htMarket, haFire5, -12, -38, [1632,1633,1634,1635,1636,1637]);
  AddAnimation(htMarket, haFire6, 39, -47, [1629,1630,1631,1626,1627,1628]);
  AddAnimation(htMarket, haFire7, 25, 13, [1635,1636,1637,1632,1633,1634]);
  AddAnimation(htMarket, haFire8, -82, -40, [1621,1622,1623,1624,1625,1620]);
  //Now add horse animations for the market
  AddMarketBeastAnim(1,[278, 277, 276, 275, 274, 273, 272, 271, 271, 271, 271, 271, 272,
                        273, 274, 275, 276, 277, 277, 278, 279, 280, 281, 282, 282, 282,
                        282, 281, 280, 279]);
  fMarketBeastAnim[1].Count := 30;
  fMarketBeastAnim[1].MoveX := MARKET_WARES_OFF_X;
  fMarketBeastAnim[1].MoveY := MARKET_WARES_OFF_Y;
  AddMarketBeastAnim(2,[266, 265, 264, 263, 262, 261, 260, 259, 258, 257, 258, 259, 260,
                        261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 270, 270, 270,
                        270, 269, 268, 267]);
  fMarketBeastAnim[2].Count := 30;
  fMarketBeastAnim[2].MoveX := MARKET_WARES_OFF_X;
  fMarketBeastAnim[2].MoveY := MARKET_WARES_OFF_Y;




  //set smaller sight
  for H := HOUSE_MIN to HOUSE_MAX do
  begin
    fItems[H].fHouseDat.Sight := 6;
    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      if I <= 4 then
      begin
        for K := 1 to 5 do
        begin
          if fItems[H].fHouseDat.SupplyIn[I, K] > 0 then
            fItems[H].fSupplyIn[I, K] := fItems[H].fHouseDat.SupplyIn[I, K];
          if fItems[H].fHouseDat.SupplyIn[I, K] > 0 then
            fItems[H].fSupplyOut[I, K] := fItems[H].fHouseDat.SupplyOut[I, K];
        end;
      end;

      if I > high(HOUSE_DAT_X[H].Input) then
      begin
        fItems[H].fWareInput[I] := wtNone;
        fItems[H].fWareOutput[I] := wtNone;
      end else
      begin
        fItems[H].fWareInput[I] := HOUSE_DAT_X[H].Input[I];
        fItems[H].fWareOutput[I] := HOUSE_DAT_X[H].Output[I];
      end;
    end;
  end;

  //set Default thing, so it don't need to do it in many places
  for H := htWall to HOUSE_MAX do
  begin
    fItems[H].fHouseType := H;
    SetNoImageSupplies(H);
    fItems[H].fHouseDat.WorkerRest := 5;
  end;

  //////////////////////////////////////////////////////////////////////////
  H := htSiegeWorkshop;
  fItems[H].fHouseDat.WorkerRest := 10;

  fItems[H].fHouseDat.SupplyIn[3,1] := 2147;
  fItems[H].fHouseDat.SupplyIn[3,2] := 2148;
  fItems[H].fHouseDat.SupplyIn[3,3] := 2149;
  fItems[H].fHouseDat.SupplyIn[3,4] := 2150;
  fItems[H].fHouseDat.SupplyIn[3,5] := 2151;

  //////////////////////////////////////////////////////////////////////////
  H := htStoneWorkshop;
  SetClearAnimation(H, haWork2, 20);
  SetClearAnimation(H, haIdle, 10);

  //////////////////////////////////////////////////////////////////////////
  H := htIronFoundry;
  SetClearAnimation(H, haWork2, 20);
  SetClearAnimation(H, haIdle, 10);
  //////////////////////////////////////////////////////////////////////////
  H := htMerchant;
  With fItems[H].fHouseDat do
  begin
    WorkerRest := 5;
    Anim[haFlag1] := fItems[htInn].fHouseDat.Anim[haFlag3];
    Anim[haFlag2] := fItems[htQuarry].fHouseDat.Anim[haFlag1];
    //Anim[haFlag3] := Anim[haFlag1];

    Anim[haFlag1].MoveX := 99;
    Anim[haFlag1].MoveY := -148;

    Anim[haFlag2].MoveX := 87;
    Anim[haFlag2].MoveY := -57;

    //Anim[haFlag3].MoveX := -25;
    //Anim[haFlag3].MoveY := -19;
    //Anim[haIdle] := fItems[htVineyard].fHouseDat.Anim[haIdle];
  end;

  //////////////////////////////////////////////////////////////////////////
  H := htPottery;
  With fItems[H].fHouseDat do
  begin
    Anim[haFlag1] := fItems[htSchool].fHouseDat.Anim[haFlag1];
    Anim[haFlag1].MoveX := 98;
    Anim[haFlag1].MoveY := -63;

  end;
  SetClearAnimation(H, haWork2, 20);
  SetClearAnimation(H, haIdle, 10);

  //////////////////////////////////////////////////////////////////////////
  H := htWoodBurner;
  With fItems[H].fHouseDat do
  begin
    Anim[haFlag1] := fItems[htWeaponWorkshop].fHouseDat.Anim[haFlag1];
    Anim[haFlag1].MoveX := 66;
    Anim[haFlag1].MoveY := -24;
  end;
  SetClearAnimation(H, haWork1, 10);
  SetClearAnimation(H, haWork2, 20);
  SetClearAnimation(H, haWork5, 10);


  //////////////////////////////////////////////////////////////////////////
  H := htAppleTree;
  fItems[H].fHouseType := H;
  SetNoImageSupplies(H);
  With fItems[H].fHouseDat do
  begin
    Anim[haFlag1] := fItems[htStore].fHouseDat.Anim[haFlag1];
    Anim[haFlag1].MoveX := -34;
    Anim[haFlag1].MoveY := 9;

  end;

  fItems[htWatchTower].fHouseDat.Sight := 8;

  fItems[htIronSmithy].fHouseDat.SupplyIn[3,1] := 2152;
  fItems[htIronSmithy].fHouseDat.SupplyIn[3,2] := 2153;
  fItems[htIronSmithy].fHouseDat.SupplyIn[3,3] := 2154;
  fItems[htIronSmithy].fHouseDat.SupplyIn[3,4] := 2155;
  fItems[htIronSmithy].fHouseDat.SupplyIn[3,5] := 2156;

  fItems[htIronSmithy].fHouseDat.SupplyOut[2,1] := 2157;
  fItems[htIronSmithy].fHouseDat.SupplyOut[2,2] := 2158;
  fItems[htIronSmithy].fHouseDat.SupplyOut[2,3] := 2159;
  fItems[htIronSmithy].fHouseDat.SupplyOut[2,4] := 2160;
  fItems[htIronSmithy].fHouseDat.SupplyOut[2,5] := 2161;

  for H := HOUSE_MIN to HOUSE_MAX do
  begin
    //default unlocking house
    fItems[H].fUnlockAfter := [HOUSE_DAT_X[H].UnlockedByHouse];
    //default MaxInWares count
    case H of
      htStore,
      htBarracks,
      htMarket: fItems[H].fMaxInWares := High(Word);

      htWell: fItems[H].fMaxInWares := 1;
      htWallTower: fItems[H].fMaxInWares := 1;
      htSmallStore: fItems[H].fMaxInWares := 100;
      htMerchant: fItems[H].fMaxInWares := 10;

      htTownHall: fItems[H].fMaxInWares := 120;

      htCottage: fItems[H].fMaxInWares := 10;

      htHouse: fItems[H].fMaxInWares := 25;

      htInn: fItems[H].fMaxInWares := 5;
      htPalace: fItems[H].fMaxInWares := 20;

      else fItems[H].fMaxInWares := MAX_WARES_IN_HOUSE;

    end;
    //tile cost for each house. Hardcoded cause it may not be in json file
    with fItems[H] do
      case H of
        htArmorSmithy: fTileCost := 4;
        htBakery: fTileCost := 4;
        htBarracks: fTileCost := 4;
        htButchers: fTileCost := 3;
        htFishermans: fTileCost := 1;
        htGoldMine: fTileCost := 2;
        htInn: fTileCost := 6;
        htIronMine: fTileCost := 2;
        htIronSmithy: fTileCost := 4;
        htMarket: fTileCost := 6;
        htMetallurgists: fTileCost := 3;
        htMill: fTileCost := 2;
        htSchool: fTileCost := 5;
        htStables: fTileCost := 6;
        htStore: fTileCost := 6;
        htSwine: fTileCost := 2;
        htTownHall: fTileCost := 6;
        htWatchTower: fTileCost := 2;
        htWeaponSmithy: fTileCost := 4;
        htVineyard: fTileCost := 2;

      end;
    with fItems[H].fHouseDat do
      for I := 1 to 4 do
        for K := 1 to 4 do
          BuildArea[I, K] := HOUSE_DAT_X[H].PlanYX[I, K];

  end;


 //set some default values if none of them were asigned
  for H := HOUSE_MIN to HOUSE_MAX do
    if fItems[H].fHouseDat.MaxHealth = 0 then
      fItems[H].fHouseDat.MaxHealth := 250;

  fHovelBeastAnim[1].Create(-20, -5, [2093, 2094, 2095, 2096, 2097, 2098, 2098, 2098, 2098, 2098, 2097, 2093, 2094, 2095, 2096, 2093, 2094, 2095, 2096, 2093, 2094, 2095, 2096]);

  fHovelBeastAnim[2].Create(0, 3, [2372, 2373, 2374, 2375, 2376, 2377, 2377, 2377, 2377, 2377, 2376, 2372, 2373, 2374, 2375, 2372, 2373, 2374, 2375, 2372, 2373, 2374, 2375]);

  fHovelBeastAnim[3] := fHovelBeastAnim[1];
  fHovelBeastAnim[3].MoveX := 40;
  fHovelBeastAnim[3].MoveY := 0;

  Palace_Flags[1].Create(-78, -22, [1165,1166,1167,1163,1164]);
  Palace_Flags[2].Create(-78, 30, [1165,1166,1167,1163,1164]);
  Palace_Flags[3].Create(28, 30, [1165,1166,1167,1163,1164]);
  Palace_Flags[4].Create(70, 30, [1165,1166,1167,1163,1164]);

  Silo_Tablets.Create(56, -11, 2445, 10);
  Merchant_Tablets.Create(0, 0, [2446, 2453, 2459, 2458, 2449, 2450, 2457, 2456, 2455, 2454]);


  ProdThatch_Anims[ptaSawDust].Create(0, 0, [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 2519, 2520, 2521, 2522, 2523, 2524, 2521, 2522, 2523, 2524, 2521, 2522, 2523, 2524, 2525]);
  ProdThatch_Anims[ptaSmokeBakery].Create(-81, 38, [888, 889, 890, 891]);
  ProdThatch_Anims[ptaSmokeGold].Create(-57, -30, [888, 889, 890, 891], 2);
  ProdThatch_Anims[ptaSmokeIron].Create(17, -14, [888, 889, 890, 891], 4);
  ProdThatch_Anims[ptaWindows].Create(0, 0, [2526, 2527, 2527, 2528, 2528, 2529, 2529, 2686, 2686, 2686, 2686, 2686, 2686, 2686, 2686, 2686, 2529, 2529, 2528, 2528, 2527, 2527, 2526, 2526, 2526, 2526, 2526, 2526]);
  ProdThatch_Anims[ptaCorn].Create(0, 0, [5, 5, 5, 5, 5, 5, 5, 5, 2687, 2688, 2689, 2690, 2691, 2689, 2690, 2691, 2689, 2690, 2691, 2689, 2690, 2691, 2689, 2690, 2691, 2689, 2690, 2691, 2692]);
  ProdThatch_Anims[ptaWine].Create(0, 0, [5, 5, 5, 5, 5, 5, 5, 5, 2702, 2703, 2704, 2705, 2706, 2704, 2705, 2706, 2704, 2705, 2706, 2704, 2705, 2706, 2704, 2705, 2706, 2707, 2708]);
  ProdThatch_Anims[ptaStoneDust].Create(0, 0, [5, 5, 5, 5, 5, 5, 5, 5, 2693, 2694, 2695, 2696, 2697, 2698, 2696, 2697, 2698, 2696, 2697, 2698, 2696, 2697, 2698, 2696, 2697, 2698, 2696, 2697, 2698, 2699, 2700, 2701]);

  for PT := Low(TKMProdThatchAnimType) to High(TKMProdThatchAnimType) do
    for I := 0 to ProdThatch_Anims[PT].Count - 1 do
      ProdThatch_Anims[PT].Step[I] := ProdThatch_Anims[PT].Step[I] + 1;
      


  WallTower_RecruitLeft.Create(0, 0, [2122,2122,2122,2122,2122,2122,2122,2123,2124,2125,2126,2126,2126,2126,2126,2126,2126,2125,2124,2123,2122]);
  WallTower_RecruitRight.Create(0, 0, [2127,2127,2127,2127,2127,2127,2127,2128,2129,2130,2131,2131,2131,2131,2131,2131,2131,2130,2129,2128,2127,2127,2127,2127,2127]);


  fItems[htSchool].Anim[haIdle].Create([]);
  Shool_Clock.Create(-17, 5, [813, 814, 815, 816, 817, 861, 862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878]);
  //SaveToNewStream;


end;

constructor TKMResHouses.Create;
begin
  inherited;
  CreateHouses;

  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtOldHouses]);
  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtNewHouses]);
end;
Procedure TKMResHouses.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin

  oldCRC := fCRC;

  //fCRC := LoadHouseDat(ExeDir+'data' + PathDelim + 'defines' + PathDelim + 'houses.dat');
  CreateHouses;
  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtOldHouses]);
  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtNewHouses]);
  if not UpdateCRC then
    fCRC := oldCRC;
end;


destructor TKMResHouses.Destroy;
var
  H: TKMHouseType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    FreeAndNil(fItems[H]);

  inherited;
end;


function TKMResHouses.GetHouse(aType: TKMHouseType): TKMHouseSpec;
begin
  Result := fItems[aType];
end;


function TKMResHouses.IsValid(aType: TKMHouseType): Boolean;
begin
  Result := aType in HOUSES_VALID;
end;


function TKMResHouses.GetBeastAnim(aType: TKMHouseType; aBeast, aAge: Integer): TKMAnimLoop;
begin
  Assert(aType in [htSwine, htStables, htMarket,htHovel]);
  Assert(InRange(aBeast, 1, 5), 'Beast ID: ' + IntToStr(aBeast));
  Assert(InRange(aAge, 1, 3),  'Beast Age: ' + IntToStr(aAge));
  case aType of
    htSwine:    Result := fBeastAnim[1, aBeast, aAge];
    htStables:  Result := fBeastAnim[2, aBeast, aAge];
    htHovel:    begin
                  Result := fHovelBeastAnim[aBeast];
                  Result.Step := fHovelBeastAnim[aAge].Step;
                end;
    htMarket:   Result := fMarketBeastAnim[aBeast];
  end;
end;


//Return CRC of loaded file
//CRC should be calculated right away, cos file may be swapped after loading
function TKMResHouses.LoadHouseDat(const aPath: string): Cardinal;
var
  S: TKMemoryStream;
  i: Integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStreamBinary.Create;
  try
    S.LoadFromFile(aPath);

    S.Read(fBeastAnim, SizeOf(fBeastAnim)); //Swine&Horses animations

    //Read the records one by one because we need to reorder them and skip one in the middle
    for i:=0 to 28 do //KaM has only 28 houses
    if HOUSE_ID_TO_TYPE[i] <> htNone then
      fItems[HOUSE_ID_TO_TYPE[i]].LoadFromStream(S)
    else
      S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


function TKMResHouses.LoadFromJSON(aFileName : String) : Cardinal;

  procedure AddAnimation(aHouse: TKMHouseType; aAnim: TKMHouseActionType; aMoveX, aMoveY: Integer; const aSteps: array of SmallInt);
  var
    I: Integer;
  begin
    with fItems[aHouse].fHouseDat.Anim[aAnim] do
    begin
      MoveX := aMoveX;
      MoveY := aMoveY;
      Count := length(aSteps);
      for I := 1 to Count do
        Step[I] := aSteps[I-1];
    end;
  end;

  Procedure SetValue(var A : Integer; B : Integer; aCondition : Boolean = true); Overload;
  begin
    if aCondition then
      A := B;
  end;
  Procedure SetValue(var A : Word; B : Word; aCondition : Boolean = true);  Overload;
  begin
    if aCondition then
      A := B;
  end;
  Procedure SetValue(var A : Boolean; B : Boolean; aCondition : Boolean = true); Overload;
  begin
    if aCondition then
      A := B;
  end;
  Procedure SetValue(var A : Byte; B : Byte; aCondition : Boolean = true); Overload;  begin if aCondition then  A := B; end;
  Procedure SetValue(var A : SmallInt; B : SmallInt; aCondition : Boolean = true); Overload;  begin if aCondition then  A := B; end;
  Procedure SetValue(var A : ShortInt; B : ShortInt; aCondition : Boolean = true); Overload;  begin if aCondition then  A := B; end;

  function ConvertToHouseArray2(aHouses : TKMHouseTypeArray; aHouseType : TKMHouseType = htNone) : TKMHouseTypeArray2;
  var I, J, K : Integer;
  begin
    J := 0;
    K := 0;
    for I := 0 to High(aHouses) do
    begin
      if (I = 0) or (aHouses[I] = aHouseType) then
      begin
        SetLength(Result, K + 1);
        J := 0;
        Inc(K);
      end;

      if (aHouses[I] = aHouseType) then
        Continue;

      SetLength(Result[K - 1], J + 1);

      Result[K - 1, J] := aHouses[I];
      Inc(J);
    end;


  end;

var
  I, K, J, L : Integer;
  jsonPath: string;
  nHouse, nAnim, nAnim2, nRoot: TKMJson;
  nHouses, nAR, nAR2: TJsonArray;
  H, HT : TKMHouseType;
  W : TKMWareType;
  HA : TKMHouseActionType;
  AnimSteps : array of SmallInt;
  GS : TKMGatheringScript;
  C : Cardinal;
  UT : TKMUnitType;

begin

  //jsonPath := ExeDir + //JSON_PATH + aFileName + '.json';
  jsonPath := aFileName;
  if not FileExists(jsonPath) then
    Exit;

  nRoot := TJsonObject.ParseFromFile(jsonPath) as TKMJson;

  Result := Result xor nRoot.CRC;
  try

    nAR := nRoot.A['HousesOrder'];

    if nAR.Count > 0 then
    begin
      //Inc(fCRC);
      SetLength(HOUSE_GUI_TAB_ORDER, nAR.Count);
      for I := 0 to nAR.Count - 1 do
      begin
        SetLength(HOUSE_GUI_TAB_ORDER[I].H, 1);
        HOUSE_GUI_TAB_ORDER[I].TextID := nAR[I].I['TitleID'];
        HOUSE_GUI_TAB_ORDER[I].GuiIcon := nAR[I].I['GuiIcon'];

        nAR.O[I].GetArray('Houses', HOUSE_GUI_TAB_ORDER[I].H[0]);
        HOUSE_GUI_TAB_ORDER[I].H := ConvertToHouseArray2(HOUSE_GUI_TAB_ORDER[I].H[0]);
      end;
    end;

    nAR := nRoot.A['HousesVictoryOrder'];

    if nAR.Count > 0 then
    begin
      //Inc(fCRC);
      SetLength(HOUSE_VICTORY_ORDER, nAR.Count);
      for I := 0 to nAR.Count - 1 do
      begin
        SetLength(HOUSE_VICTORY_ORDER[I].H, 1);
        HOUSE_VICTORY_ORDER[I].TextID := nAR[I].I['TitleID'];
        HOUSE_VICTORY_ORDER[I].GuiIcon := nAR[I].I['GuiIcon'];

        nAR.O[I].GetArray('Houses', HOUSE_VICTORY_ORDER[I].H);
      end;
    end;
    nHouses := nRoot.A['Houses'];
    //Assert(nHouses.Count <> 0, 'No houses data is there');

    for I := 0 to nHouses.Count - 1 do
    begin
      //Inc(fCRC);
      nHouse := nHouses.O[I];
      if nHouse.Count <= 1 then
        Continue;

      if not TKMEnumUtils.TryGetAs<TKMHouseType>(nHouse.S['Name'],  H) then
        raise Exception.Create('Error loading ' + jsonPath + ': wrong HouseType name: ' + nHouse.S['Name']);

      with fItems[H] do
      begin
        SetValue(fNameTextID, nHouse.I['TextID'], nHouse.Contains('TextID'));
        SetValue(fDescriptionID, nHouse.I['DescriptionID'], nHouse.Contains('DescriptionID'));
        SetValue(fTileCost, nHouse.I['TileCost'], nHouse.Contains('TileCost'));
        //fNameTextID := IfThen(nHouse.Contains('TextID'), nHouse.I['TextID'], fNameTextID);
        //fDescriptionID := nHouse.I['DescriptionID'];
        //fTileCost := IfThen(nHouse.Contains('TileCost'), nHouse.I['TileCost'], fTileCost);
        with fHouseDat do
        begin
          //only replace when the values are not 0

          SetValue(MaxHealth, nHouse.I['MaxHealth'], nHouse.Contains('MaxHealth'));
          SetValue(WoodCost, nHouse.I['WoodCost'], nHouse.Contains('WoodCost'));
          SetValue(StoneCost, nHouse.I['StoneCost'], nHouse.Contains('StoneCost'));
          SetValue(WoodPic, nHouse.I['WoodPic'], nHouse.Contains('WoodPic'));
          SetValue(StonePic, nHouse.I['StonePic'], nHouse.Contains('StonePic'));
          SetValue(EntranceOffsetX, nHouse.I['EntranceOffsetX'], nHouse.Contains('EntranceOffsetX'));
          SetValue(EntranceOffsetY, nHouse.I['EntranceOffsetY'], nHouse.Contains('EntranceOffsetY'));
          SetValue(EntranceOffsetXpx, nHouse.I['EntranceOffsetXpx'], nHouse.Contains('EntranceOffsetXpx'));
          SetValue(EntranceOffsetYpx, nHouse.I['EntranceOffsetYpx'], nHouse.Contains('EntranceOffsetYpx'));
          SetValue(WorkerType, nHouse.I['WorkerType'], nHouse.Contains('WorkerType'));
          SetValue(WorkerRest, nHouse.I['WorkerRest'], nHouse.Contains('WorkerRest'));
          SetValue(Sight, nHouse.I['Sight'], nHouse.Contains('Sight'));
          SetValue(fMaxInWares, nHouse.I['MaxInWares'], nHouse.Contains('MaxInWares'));
          SetValue(fMaxOutWares, nHouse.I['MaxOutWares'], nHouse.Contains('MaxOutWares'));
          {MaxHealth := IfThen(nHouse.Contains('MaxHealth'), nHouse.I['MaxHealth'], MaxHealth);
          WoodCost := IfThen(nHouse.Contains('WoodCost'), nHouse.I['WoodCost'], WoodCost);
          StoneCost := IfThen(nHouse.Contains('StoneCost'), nHouse.I['StoneCost'], StoneCost);
          WoodPic := IfThen(nHouse.Contains('WoodPic'), nHouse.I['WoodPic'], WoodPic);
          StonePic := IfThen(nHouse.Contains('StonePic'), nHouse.I['StonePic'], StonePic);
          EntranceOffsetX := IfThen(nHouse.Contains('EntranceOffsetX'), nHouse.I['EntranceOffsetX'], EntranceOffsetX);
          EntranceOffsetXpx := IfThen(nHouse.Contains('EntranceOffsetXpx'), nHouse.I['EntranceOffsetXpx'], EntranceOffsetXpx);
          EntranceOffsetYpx := IfThen(nHouse.Contains('EntranceOffsetYpx'), nHouse.I['EntranceOffsetYpx'], EntranceOffsetYpx);
          WorkerType := IfThen(nHouse.Contains('WorkerType'), nHouse.I['WorkerType'], WorkerType);
          WorkerRest := IfThen(nHouse.Contains('WorkerRest'), nHouse.I['WorkerRest'], WorkerRest);
          Sight := IfThen(nHouse.Contains('Sight'), nHouse.I['Sight'], Sight);
          fMaxInWares := IfThen(nHouse.Contains('MaxInWares'), nHouse.I['MaxInWares'], fMaxInWares);}

          SetValue(fCanForceWork, nHouse.B['CanForceWork'], nHouse.Contains('CanForceWork'));

          //JSONArrToValidSet(nHouse.A['WorkerTypes'], WorkerTypes);

          nAR := nHouse.A['WorkerTypes'];

          if nAr.Count > 0 then
          begin
            SetLength(Workers, 0);
            MaxWorkersCount := 0;
          end;

          for K := 0 to nAr.Count - 1 do
          begin
            nAnim := nAr.O[K];
            if nAnim.I['MaxCount'] > 0 then
            begin
              if not TKMEnumUtils.TryGetAs<TKMUnitType>(nAnim.S['UnitType'],  UT) then
                raise Exception.Create('Error loading' + aFileName + ': wrong UnitType name: ' + nAnim.S['UnitType']);

              AddWorker(UT, nAnim.I['MaxCount']);
              Inc(MaxWorkersCount, nAnim.I['MaxCount']);
            end;
          end;


          SetValue(MaxWorkersCount, nHouse.I['MaxWorkersCount'], nHouse.Contains('MaxWorkersCount'));
          SetValue(CanOverfill, nHouse.B['CanOverfill'], nHouse.Contains('CanOverfill'));

          if nHouse.Contains('GatheringScript') then
              if not TKMEnumUtils.TryGetAs<TKMGatheringScript>(nHouse.S['GatheringScript'],  fGatheringScript) then
                raise Exception.Create('Error loading' + aFileName + ': wrong GatheringScript name: ' + nHouse.S['GatheringScript']);


          nAR := nHouse.A['WareInput'];
          if nAR.Count > 0 then
            for K := 1 to WARES_IN_OUT_COUNT do
            begin
              if (K > nAR.Count) then
                fWareInput[K] := wtNone
              else
              if TKMEnumUtils.TryGetAs<TKMWareType>(nAR.S[K - 1],  W) then
                fWareInput[K] := W
              else
                raise Exception.Create('Error loading ' + jsonPath + ': wrong WareType name: ' + nAR.S[K - 1]);
            end;

          nAR := nHouse.A['UnlockAfter'];
          if nAR.Count > 0 then
          begin
            fUnlockAfter := [];
            for K := 0 to nAR.Count - 1 do
            begin
              if TKMEnumUtils.TryGetAs<TKMHouseType>(nAR[K],  HT) then
                fUnlockAfter := fUnlockAfter + [HT]
              else
                raise Exception.Create('Error loading' + aFileName + ': wrong TKMHouseType name: ' + nAR[K]);
            end;
          end;

          nAR := nHouse.A['WareOutput'];
          if nAR.Count > 0 then
            for K := 1 to WARES_IN_OUT_COUNT do
            begin
              if (K > nAR.Count) then
                fWareOutput[K] := wtNone
              else
              if TKMEnumUtils.TryGetAs<TKMWareType>(nAR.S[K - 1],  W) then
                fWareOutput[K] := W
              else
                raise Exception.Create('Error loading ' + jsonPath + ': wrong WareType name: ' + nAR.S[K - 1]);
            end;



          for J := 1 to WARES_IN_OUT_COUNT do
          begin
            nAR := nHouse.A['SupplyIn' + IntToStr(J)];
            for K := 1 to Min(nAR.Count, WARES_IN_OUT_COUNT) do
              SetValue(fSupplyIn[J, K], nAR[K - 1]);


            nAR := nHouse.A['SupplyOut' + IntToStr(J)];
            for K := 1 to Min(nAR.Count, WARES_IN_OUT_COUNT) do
              SetValue(fSupplyOut[J, K], nAR[K - 1]);


          end;

          for HA := haWork1 to haFire8 do
          begin

            if not nHouse.Contains(HOUSE_ACTION_STR[HA]) then
              Continue;

            nAnim := nHouse.O[HOUSE_ACTION_STR[HA]];
            TKMJson(nAnim).GetAnim(fItems[H].fHouseDat.Anim[HA]);
            //JSONToAnim(nAnim, fItems[H].fHouseDat.Anim[HA]);
          end;

          for HA := haWork1 to haWork5 do
          begin

            if not nHouse.Contains('MakeSound_' + HOUSE_ACTION_STR[HA]) then
              Continue;

            nAnim := nHouse.O['MakeSound_' + HOUSE_ACTION_STR[HA]];

            Sound[HA].ID := nAnim.I['SoundID'];
            nAR := nAnim.A['Steps'];

            for K := 0 to nAR.Count - 1 do
              Sound[HA].Steps :=  Sound[HA].Steps + [byte(nAR[K])];

          end;

          if nHouse.Contains('WorkAnim') then
          begin
            nAR := nHouse.A['WorkAnim'];
            SetLength(WorkAnim, 0);
            for K := 0 to nAR.Count - 1 do
            begin
              nAnim := nAR.O[K];

              if TKMEnumUtils.TryGetAs<TKMHouseActionType>(nAnim.S['WorkType'],  HA) and (HA in [haWork1..haWork5]) then
              begin
                SetLength(WorkAnim, length(WorkAnim) + 1);
                WorkAnim[high(WorkAnim)].Action := HA;
                WorkAnim[high(WorkAnim)].Cycles := Max(nAnim.I['Cycles'], 1);
              end else
              raise Exception.Create('Error loading ' + jsonPath + ': wrong ActionType name: ' + nAnim.S['WorkType']);

            end;

          end;

          if nHouse.Contains('WareSlots') then
          begin
            nAR := nHouse.A['WareSlots'];
            SetLength(WareInputSlots, 0);
            for K := 0 to nAR.Count - 1 do
            begin
              nAnim := nAR.O[K];
              SetLength(WareInputSlots, length(WareInputSlots) + 1);
              with WareInputSlots[high(WareInputSlots)] do
              begin
                Icon := nAnim.I['GuiIcon'];

                nAR2 := nAnim.A['WareInput'];

                for L := low(WareInput) to high(WareInput) do
                  WareInput[L] := wtNone;


                if nAR2.Count > 0 then
                  for L := 1 to nAR2.Count do
                    if TKMEnumUtils.TryGetAs<TKMWareType>(nAR2.S[L - 1],  W) then
                    begin
                      WareInput[L] := W;
                    end else
                    raise Exception.Create('Error loading ' + jsonPath + ': wrong WareType name: ' + nAR2.S[L-1] );

                nAR2 := nAnim.A['WareOutput'];

                for L := low(WareOutput) to high(WareOutput) do
                  WareOutput[L] := wtNone;

                if nAR2.Count > 0 then
                  for L := 1 to nAR2.Count do
                    if TKMEnumUtils.TryGetAs<TKMWareType>(nAR2.S[L - 1],  W) then
                    begin
                      WareOutput[L] := W;
                    end else
                    raise Exception.Create('Error loading ' + jsonPath + ': wrong WareType name: ' + nAR2.S[L-1] );

              end;


            end;

          end;

          if nHouse.Contains('Styles') then
          begin
            nAR := nHouse.A['Styles'];
            SetLength(Styles, 0);
            for K := 0 to nAR.Count - 1 do
            begin
              nAnim := nAR.O[K];
              SetLength(Styles, length(Styles) + 1);

              with Styles[high(Styles)] do
              begin
                Icon := nAnim.I['GuiIcon'];
                StonePic := nAnim.I['StonePic'];
                SnowPic := nAnim.I['SnowPic'];
                HideSupplies := nAnim.B['HideSupplies'];
              end;
            end;
          end;
          if nHouse.Contains('Levels') then
          begin
            nAR := nHouse.A['Levels'];
            SetLength(Levels, 0);
            for K := 0 to nAR.Count - 1 do
            begin
              nAnim := nAR.O[K];
              SetLength(Levels, length(Levels) + 1);

              with Levels[high(Levels)] do
              begin
                StonePic := nAnim.I['StonePic'];
                SnowPic := nAnim.I['SnowPic'];
                WoodCost := nAnim.I['WoodCost'];
                StoneCost := nAnim.I['StoneCost'];
                TileCost := nAnim.I['TileCost'];
                MaxInWares := nAnim.I['MaxInWares'];
                HideSupplies := nAnim.B['HideSupplies'];

                ChangeIfDifferent(AnimMultiplier,nAnim.D['WorkCyclesMultiplier'], 0);

                Progress := IfThen( nAnim.Contains('AddHealth'), nAnim.I['AddHealth'], 250);
                ReplaceAnim := nAnim.Contains('ReplaceAnim') and nAnim.B['ReplaceAnim'];
              end;

              if nAnim.Contains('Anim') then
              begin
                nAR2 := nAnim.A['Anim'];

                for J := 0 to nAR2.Count - 1 do
                begin
                  nAnim2 := nAR2.O[J];
                  if nAnim2.A['Steps'].Count = 0 then
                    Continue;
                  SetLength(Levels[high(Levels)].Anim, length(Levels[high(Levels)].Anim) + 1);


                  with Levels[high(Levels)].Anim[high(Levels[high(Levels)].Anim)] do
                  begin
                    Count := nAnim2.A['Steps'].Count;
                    MoveX := nAnim2.I['X'];
                    MoveY := nAnim2.I['Y'];

                    for L := 1 to Count do
                      Step[L] := nAnim2.A['Steps'].I[L - 1];
                  end
                  //  .Create();
                end;


              end;


            end;
          end;




          if nHouse.Contains('WoodPileOffset') then
          begin
            nAnim := nHouse.O['WoodPileOffset'];

            for K := 1 to 9 do
            begin
              fBuildSupply[1, K].MoveX := nAnim.I['X'] + BUILD_SUPPLY_OFFSETS[1, K].MoveX;
              fBuildSupply[1, K].MoveY := nAnim.I['Y'] + BUILD_SUPPLY_OFFSETS[1, K].MoveY;
            end;
              
          end;
          if nHouse.Contains('StonePileOffset') then
          begin
            nAnim := nHouse.O['StonePileOffset'];

            for K := 1 to 9 do
            begin
              fBuildSupply[2, K].MoveX := nAnim.I['X'] + BUILD_SUPPLY_OFFSETS[2, K].MoveX;
              fBuildSupply[2, K].MoveY := nAnim.I['Y'] + BUILD_SUPPLY_OFFSETS[2, K].MoveY;
            end;

          end;
          if nHouse.Contains('TilePileOffset') then
          begin
            nAnim := nHouse.O['TilePileOffset'];

            for K := 1 to 9 do
            begin
              fBuildSupply[3, K].MoveX := nAnim.I['X'] + BUILD_SUPPLY_OFFSETS[3, K].MoveX;
              fBuildSupply[3, K].MoveY := nAnim.I['Y'] + BUILD_SUPPLY_OFFSETS[3, K].MoveY;
            end;

          end;

          If nHouse.Contains('Build_Area') then
          begin
            nAnim :=  nHouse.O['Build_Area'];

            for J := 1 to 4 do
              if nAnim.A['Y' + IntToStr(J)].Count > 0 then
              begin
                nAR := nAnim.A['Y' + IntToStr(J)];


                for K := 1 to 4 do
                  BuildArea[J, K] := nAR[K-1];
                  
              end;
          end;

          JSONArrToValidArr(nHouse.A['Wariants'], fWariants);


        end;


      end;

    end;

  finally
    nRoot.Free;
  end;

end;

procedure TKMResHouses.SaveToNewStream;
var I, J, K, L: Integer;
  H : TKMHouseType;
  S : TKMemoryStream;
begin
  S := TKMemoryStreamBinary.Create;
  S.PlaceMarker('ResHouses');

  S.Write(Palace_Flags, SizeOf(Palace_Flags));
  S.Write(Merchant_Tablets, SizeOf(Merchant_Tablets));
  S.Write(Silo_Tablets, SizeOf(Silo_Tablets));

  S.Write(fMarketBeastAnim, SizeOf(fMarketBeastAnim));
  S.Write(fBeastAnim, SizeOf(fBeastAnim));
  S.Write(fHovelBeastAnim, SizeOf(fHovelBeastAnim));

  J := length(HOUSE_GUI_TAB_ORDER);
  S.Write(J);
  for I := 0 to J - 1 do
  begin
    S.Write(HOUSE_GUI_TAB_ORDER[I].TextID);

    L := Length(HOUSE_GUI_TAB_ORDER[I].H);
    S.Write(L);
    for K := 0 to L - 1 do
      S.Write(HOUSE_GUI_TAB_ORDER[I].H[K], SizeOf(HOUSE_GUI_TAB_ORDER[I].H[K]));
      
  end;
  H := HOUSE_MAX;
  S.Write(H, SizeOf(H));
  for H := HOUSE_MIN to HOUSE_MAX do
    fItems[H].SaveToNewStream(S);
  S.SaveToFile(ExeDir+ 'data' + PathDelim + 'defines' + PathDelim + 'NewHouses.dat');
  S.Free;
end;

procedure TKMResHouses.LoadFromNewStream;
var I, J, K, L : Integer;
  H, H2 : TKMHouseType;
  S : TKMemoryStream;
begin
  if not FileExists(ExeDir+ 'data' + PathDelim + 'defines' + PathDelim + 'NewHouses.dat') then
    Exception.Create('Error loading ResHouses : NewHouses.dat not found');

  S := TKMemoryStreamBinary.Create;
  S.LoadFromFile(ExeDir+ 'data' + PathDelim + 'defines' + PathDelim + 'NewHouses.dat');

  S.CheckMarker('ResHouses');
  S.Read(Palace_Flags, SizeOf(Palace_Flags));
  S.Read(Merchant_Tablets, SizeOf(Merchant_Tablets));
  S.Read(Silo_Tablets, SizeOf(Silo_Tablets));

  S.Read(fMarketBeastAnim, SizeOf(fMarketBeastAnim));
  S.Read(fBeastAnim, SizeOf(fBeastAnim));
  S.Read(fHovelBeastAnim, SizeOf(fHovelBeastAnim));

  S.Read(J);
  SetLength(HOUSE_GUI_TAB_ORDER, J);

  for I := 0 to J - 1 do
  begin
    S.Read(HOUSE_GUI_TAB_ORDER[I].TextID);

    S.Read(L);
    SetLength(HOUSE_GUI_TAB_ORDER[I].H, L);
    for K := 0 to L - 1 do
      S.Read(HOUSE_GUI_TAB_ORDER[I].H[K], SizeOf(HOUSE_GUI_TAB_ORDER[I].H[K]));

  end;
  S.Read(H2, sizeOf(H2));
  for H := HOUSE_MIN to H2 do
    fItems[H].LoadFromNewStream(S);

  fCRC := Adler32CRC(S);
  S.Free;

end;

procedure TKMResHouses.ExportCSV(const aPath: string);
var
  HT: TKMHouseType;
  S: string;
  SL: TStringList;
  {K, }L: Integer;
  {anim: TKMHouseActionType;}

  procedure AddField(const aField: string); overload;
  begin S := S + aField + ';'; end;
  procedure AddField(aField: Integer); overload;
  begin S := S + IntToStr(aField) + ';'; end;

begin
  if IsFileInUse(aPath) then
    Exit;
  SL := TStringList.Create;

  S := 'House Name;HouseID;WoodCost;StoneCost;ResProductionX;WorkerRest;EntranceOffsetX';
  SL.Append(S);

  for L := 0 to high(HOUSE_ID_TO_TYPE) do
  begin
    HT := HOUSE_ID_TO_TYPE[L];
    if not (HT in [HOUSE_MIN..HOUSE_MAX]) then
      Continue;
    S := '';
    AddField(fItems[HT].HouseName);
    AddField(L);
    AddField(fItems[HT].WoodCost);
    AddField(fItems[HT].StoneCost);
    AddField(fItems[HT].ResProductionX);
    AddField(fItems[HT].WorkerRest);
    AddField(fItems[HT].EntranceOffsetX);
    SL.Append(S);
    {
    for I := 1 to 4 do
    begin
      S := '';
      for K := 1 to 4 do
        AddField(fItems[HT].BuildArea[I, K]);
      SL.Append(S);
    end;

    S := 'Animation;Count;OffsetX;OffsetY';
    SL.Append(S);
    for anim := Low(TKMHouseActionType) to High(TKMHouseActionType) do
    begin
      if fItems[HT].fHouseDat.Anim[anim].Count > 0 then
      begin
        S := '';
        AddField(GetEnumName(TypeInfo(TKMHouseActionType), Integer(anim)));
        AddField(fItems[HT].fHouseDat.Anim[anim].Count);
        AddField(fItems[HT].fHouseDat.Anim[anim].MoveX);
        AddField(fItems[HT].fHouseDat.Anim[anim].MoveY);
        SL.Append(S);
      end;
    end;}

    //S := '';
    //SL.Append(S);
  end;

  ForceDirectories(ExtractFilePath(aPath));

  SL.SaveToFile(aPath);
  SL.Free;
end;


end.
