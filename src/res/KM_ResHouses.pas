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
  THouseAnim = array [TKMHouseActionType] of TKMAnimation;

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
    Anim : array of TKMAnimation;
    MaxInWares : Word;
    AnimMultiplier : Single;
    HideSupplies: Boolean;
    function BuildingStep : Word;
  end;

  //House fields as they are in a DAT file
  TKMHouseDat = packed record
    StonePic, WoodPic, SnowPic: Word;
    Anim: THouseAnim;
    EntranceOffsetX, EntranceOffsetY: ShortInt;
    EntranceOffsetXpx, EntranceOffsetYpx: ShortInt; //When entering house units go for the door, which is offset by these values
    WoodCost,StoneCost: Byte;
    WorkerWork,WorkerRest: SmallInt;
    MaxHealth,Sight: SmallInt;
    NeedsPlayerOrder : Boolean;
    BuildIcon : Word;
    TabletID : Word;
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
    fBuildArea, fGroundArea : TKMHouseAreaNew;
    fUnoccupiedMsgId : SmallInt;
    function GetGroundVisibleArea: TKMHouseAreaNew;
    function GetHouseName: UnicodeString;
    function GetWareInput: TKMWareType8;
    function GetWareOutput: TKMWareType8;
    function GetWorkerType: TKMUnitType;
    function GetGUIWorkerType: TKMUnitType;
    function GetReleasedBy: TKMHouseTypeSet;
    function GetBuildSupply : THouseBuildSupply;
    function GetMaxWareCount : Word;
    function GetMaxWareOutCount : Word;
    function GetHouseDescription : UnicodeString;
    procedure AddWorker(aType : TKMUnitType; aCount : Byte);
  public
    Sound : array[haWork1..haWork5] of TKMHouseSound;

    WorkAnim : array of TKMHouseWorkAnim;

    WareInputSlots : array of TKMHouseWareSlot;

    Styles : array of record
      StonePic : Word;
      SnowPic : Word;
      Icon : Word;
      HideSupplies: Boolean;
    end;

    Levels : array of TKMHouseLevelRec;
    Workers: TKMUnitPlan;

    MaxWorkersCount: Byte;
    //WorkerTypes: TKMUnitTypeSet;
    //UniqueWorkers : Boolean;
    CanOverFill: Boolean;


    constructor Create(aHouseType: TKMHouseType);
    procedure LoadFromStream(Stream: TMemoryStream);

    // Property accessors:
    // Derived from KaM

    property StonePic: Word read fHouseDat.StonePic;
    property WoodPic: Word read fHouseDat.WoodPic;
    property SupplyIn: THouseSupply8 read fSupplyIn;
    property SupplyOut: THouseSupply8 read fSupplyOut;
    property Anim: THouseAnim read fHouseDat.Anim;
    property EntranceOffsetX: ShortInt read fHouseDat.EntranceOffsetX;
    property EntranceOffsetY: ShortInt read fHouseDat.EntranceOffsetY;
    property EntranceOffsetXpx: ShortInt read fHouseDat.EntranceOffsetXpx;
    property EntranceOffsetYpx: ShortInt read fHouseDat.EntranceOffsetYpx;
    property WoodCost: Byte read fHouseDat.WoodCost;
    property StoneCost: Byte read fHouseDat.StoneCost;
    property BuildSupply: THouseBuildSupply read GetBuildSupply;
    property WorkerRest: Smallint read fHouseDat.WorkerRest;
    property Sight: Smallint read fHouseDat.Sight;
    property WorkerType: TKMUnitType read GetWorkerType;
    property GUIWorkerType: TKMUnitType read GetGUIWorkerType;
    function CanHasWorker: Boolean;overload;
    function CanHasWorker(aUnitType : TKMUnitType): Boolean;overload;
    function HasStoneWariants : Byte;
    function GetStoneWariant(aID : Integer) : Integer;
    // Additional properties added by Remake
    property BuildArea: TKMHouseAreaNew read fBuildArea;
    property GroundVisibleArea: TKMHouseAreaNew read GetGroundVisibleArea;
    property DoesOrders: Boolean read fHouseDat.NeedsPlayerOrder;
    property GUIIcon: Word read fHouseDat.BuildIcon;
    property HouseDescription: UnicodeString read GetHouseDescription;
    property HouseName: UnicodeString read GetHouseName;
    property HouseNameTextID: Integer read fNameTextID;
    property ReleasedBy: TKMHouseTypeSet read GetReleasedBy;
    property WareInput: TKMWareType8 read GetWareInput;
    property WareOutput: TKMWareType8 read GetWareOutput;
    property TabletIcon: Word read fHouseDat.TabletID;
    property UnoccupiedMsgId: SmallInt read fUnoccupiedMsgId;
    property SnowPic: Word read fHouseDat.SnowPic;
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


    procedure DebugChangeAnimation(HA : TKMHouseActionType; aAnim : TKMAnimation);
    procedure DebugChangePileOffset(aID : Integer; aX, aY : Integer);
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
    function GetHouse(aType: TKMHouseType): TKMHouseSpec; inline;
    function GetBeastAnim(aType: TKMHouseType; aBeast, aAge:integer): TKMAnimLoop;
  public
    Palace_Flags : array[1..4] of TKMAnimation;
    School_Clock,
    WallTower_RecruitLeft,
    WallTower_RecruitRight,
    Merchant_Tablets,
    Silo_Tablets: TKMAnimation;

    ProdThatch_Anims : array[TKMProdThatchAnimType] of TKMAnimation;

    constructor Create;
    destructor Destroy; override;

    function IsValid(aType: TKMHouseType): Boolean;

    property Houses[aType: TKMHouseType]: TKMHouseSpec read GetHouse; default;
    property BeastAnim[aType: TKMHouseType; aBeast, aAge: Integer]: TKMAnimLoop read GetBeastAnim;
    property CRC: Cardinal read fCRC; //Return hash of all values

    procedure ExportCSV(const aPath: string);

    function LoadFromJSON(aFileName : String) : Cardinal;
    procedure ReloadJSONData(UpdateCRC : Boolean);

    procedure SaveToJson(const aPath: String);

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
    htHouse, htPalace, htStall, htProductionThatch, htShipyard,
    htCartographers);

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
    51, 52, 53, 54, 55,
    56
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


function TKMHouseSpec.GetGroundVisibleArea: TKMHouseAreaNew;
begin
  Result := fGroundArea;
end;

function TKMHouseSpec.HasStoneWariants : Byte;
begin
  Result := Length(fWariants);
end;

function TKMHouseSpec.GetStoneWariant(aID : Integer) : Integer;
begin
  Result := fWariants[aID];
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
  SetLength(tmp, MAX_HOUSE_SIZE + 1, MAX_HOUSE_SIZE + 1);

  for I := 0 to MAX_HOUSE_SIZE - 1 do
  for K := 0 to MAX_HOUSE_SIZE - 1 do
    tmp[I+1,K+1] := byte(BuildArea[I+1,K+1] > 0);

  GenerateOutline(tmp, 2, outlines);
  for K := 0 to outlines.Count - 1 do
  begin
    aList.Add(KMPoint(-1, -1));
    for I := 0 to outlines.Shape[K].Count - 1 do
      aList.Add(KMPoint(outlines.Shape[K].Nodes[I].X, outlines.Shape[K].Nodes[I].Y));
  end;
end;


function TKMHouseSpec.GetWorkerType: TKMUnitType;
begin
  If length(Workers) = 0 then
    Result := utNone
  else
  If length(Workers) > 1 then
    Result := utAny
  else
    Result := Workers[0].UnitType;
end;

procedure TKMHouseSpec.AddWorker(aType : TKMUnitType; aCount : Byte);
begin
  SetLength(Workers, length(Workers) + 1);

  Workers[high(Workers)].UnitType := aType;
  Workers[high(Workers)].Count := aCount;
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
    inc(C, Workers[I].Count);
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
    htCartographers:  Result := 999;
  else
    Result := 990; // Default Flag
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
      if Workers[I].Count > 0 then
      Exit(true);
  //Result := (WorkerType <> utNone);
end;

function TKMHouseSpec.CanHasWorker(aUnitType: TKMUnitType): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to High(Workers) do
    if (Workers[I].UnitType = aUnitType) then
      if Workers[I].Count > 0 then
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
  Result := fWareInput;
end;


function TKMHouseSpec.GetWareOutput: TKMWareType8;
begin
  Result := fWareOutput;
end;

function TKMHouseSpec.GetBuildSupply: THouseBuildSupply;
begin
  Result := fBuildSupply;
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

procedure TKMHouseSpec.DebugChangeAnimation(HA: TKMHouseActionType; aAnim: TKMAnimation);
begin
  fHouseDat.Anim[HA] := aAnim;
end;

procedure TKMHouseSpec.DebugChangePileOffset(aID: Integer; aX: Integer; aY: Integer);
begin
  fBuildSupply[aID].MoveX := aX;
  fBuildSupply[aID].MoveY := aY;
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

constructor TKMResHouses.Create;
var H : TKMHouseType;
  procedure ResetData(aHouse : TKMHouseType);
  var HA : TKMHouseActionType;
    I, K : Integer;

  begin
    with  fItems[aHouse] do
    begin
      for HA := haWork1 to haWork5 do
        Sound[HA].ID := -1;
      for I := Low(fSupplyIn) to High(fSupplyIn) do
        for K := Low(fSupplyIn[I]) to High(fSupplyIn[I]) do
        begin
          fSupplyIn[I, K] := 5;
          fSupplyOut[I, K] := 5;
        end;
    end;

  end;

begin
  inherited;
  for H := HOUSE_MIN to HOUSE_MAX do
  begin
    fItems[H] := TKMHouseSpec.Create(H);

    ResetData(H);
  end;

  fCRC := fCRC xor LoadFromJSON(gRes.JsonData[dtNewHouses]);
end;
Procedure TKMResHouses.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin

  oldCRC := fCRC;

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
      //fItems[HOUSE_ID_TO_TYPE[i]].LoadFromStream(S)
      S.Seek(SizeOf(TKMHouseDat), soFromCurrent)
    else
      S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;


function TKMResHouses.LoadFromJSON(aFileName : String) : Cardinal;

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
  S : String;

  PTA : TKMProdThatchAnimType;
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
          SetValue(WorkerRest, nHouse.I['WorkerRest'], nHouse.Contains('WorkerRest'));
          SetValue(Sight, nHouse.I['Sight'], nHouse.Contains('Sight'));
          SetValue(fMaxInWares, nHouse.I['MaxInWares'], nHouse.Contains('MaxInWares'));
          SetValue(fMaxOutWares, nHouse.I['MaxOutWares'], nHouse.Contains('MaxOutWares'));
          SetValue(SnowPic, nHouse.I['SnowPic'], nHouse.Contains('SnowPic'));
          SetValue(NeedsPlayerOrder, nHouse.B['NeedsPlayerOrder'], nHouse.Contains('NeedsPlayerOrder'));
          SetValue(BuildIcon, nHouse.I['BuildIcon'], nHouse.Contains('BuildIcon'));
          SetValue(TabletID, nHouse.I['TabletID'], nHouse.Contains('TabletID'));
        end;

        SetValue(fCanForceWork, nHouse.B['CanForceWork'], nHouse.Contains('CanForceWork'));
        SetValue(fUnoccupiedMsgID, nHouse.I['UnoccupiedMsgId'], nHouse.Contains('UnoccupiedMsgId'));

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

                nAnim2.GetAnim(Levels[high(Levels)].Anim[high(Levels[high(Levels)].Anim)]);
              end;


            end;


          end;
        end;




        if nHouse.Contains('WoodPileOffset') then
        begin
          nAnim := nHouse.O['WoodPileOffset'];
          fBuildSupply[1].MoveX := nAnim.I['X'];
          fBuildSupply[1].MoveY := nAnim.I['Y'];
        end;
        if nHouse.Contains('StonePileOffset') then
        begin
          nAnim := nHouse.O['StonePileOffset'];
          fBuildSupply[2].MoveX := nAnim.I['X'];
          fBuildSupply[2].MoveY := nAnim.I['Y'];
        end;
        if nHouse.Contains('TilePileOffset') then
        begin
          nAnim := nHouse.O['TilePileOffset'];
          fBuildSupply[3].MoveX := nAnim.I['X'];
          fBuildSupply[3].MoveY := nAnim.I['Y'];
        end;

        nHouse.GetHouseArea('Build_Area', fBuildArea);
        nHouse.GetHouseArea('Ground_Area', fGroundArea);
        JSONArrToValidArr(nHouse.A['Wariants'], fWariants);
      end;
    end;
    /////////////////////////////////////////////////Beast Anim
    nHouses := nRoot.A['BeastAnim'];
    for I := 1 to 2 do
      for K := 1 to 5 do
        for J := 1 to 3 do
          nHouses.A[I - 1].A[K-1].O[J-1].GetAnim(fBeastAnim[I, K, J]);

    nHouses := nRoot.A['MarketBeastAnim'];
    for I := 1 to Min(nHouses.Count, 3) do
      nHouses.O[I - 1].GetAnim(fMarketBeastAnim[I]);

    nHouses := nRoot.A['HovelBeastAnim'];
    for I := 1 to Min(nHouses.Count, 3) do
      nHouses.O[I - 1].GetAnim(fHovelBeastAnim[I]);
    ///////////////////////////////////////////////////Additional animations
    nHouses := nRoot.A['Palace_Flags'];
    for I := 1 to Min(nHouses.Count, High(Palace_Flags)) do
      nHouses.O[I - 1].GetAnim(Palace_Flags[I]);

    nRoot.GetAnim('Silo_Tablets', Silo_Tablets);
    nRoot.GetAnim('Merchant_Tablets', Merchant_Tablets);

    nHouse := nRoot.O['ProductionThatch_Anims'];
    for PTA := Low(TKMProdThatchAnimType) to High(TKMProdThatchAnimType) do
    begin
      If TKMEnumUtils.GetName<TKMProdThatchAnimType>(PTA, S) then
        nHouse.GetAnim(S, ProdThatch_Anims[PTA]);
    end;

    nRoot.GetAnim('WallTower_RecruitLeft', WallTower_RecruitLeft);
    nRoot.GetAnim('WallTower_RecruitRight', WallTower_RecruitRight);
    nRoot.GetAnim('School_Clock', School_Clock);
  finally
    nRoot.Free;
  end;

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

  S := 'House Name;HouseID;WoodCost;StoneCost;ResProductionX;WorkerRest;EntranceOffsetX;EntranceOffsetY';
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
    AddField(fItems[HT].WorkerRest);
    AddField(fItems[HT].EntranceOffsetX);
    AddField(fItems[HT].EntranceOffsetY);
    SL.Append(S);
  end;

  ForceDirectories(ExtractFilePath(aPath));

  SL.SaveToFile(aPath);
  SL.Free;

  //SaveToJson(aPath);
end;

procedure TKMResHouses.SaveToJson(const aPath: string);
var
  root : TKMJsonSaver;
  I, K, J, L : Integer;
  HT, HT2 : TKMHouseType;
  HA : TKMHouseActionType;
  S : String;
  B : Byte;
begin
  root := TKMJsonSaver.Create;

  try
    root.BeginFile;
      root.WriteArray('HousesOrder', true);
        for I := 0 to High(HOUSE_GUI_TAB_ORDER) do
        begin
          root.WriteObject('', I = 0);
            root.Write('TitleID', HOUSE_GUI_TAB_ORDER[I].TextID, true);
            root.Write('GuiIcon', HOUSE_GUI_TAB_ORDER[I].GuiIcon);
            root.Write('Houses', HOUSE_GUI_TAB_ORDER[I].H);
          root.EndObject;
        end;

      root.EndArray;
      root.WriteArray('HousesVictoryOrder');
        for I := 0 to High(HOUSE_VICTORY_ORDER) do
        begin
          root.WriteObject('', I = 0);
            root.Write('TitleID', HOUSE_VICTORY_ORDER[I].TextID, true);
            root.Write('GuiIcon', HOUSE_VICTORY_ORDER[I].GuiIcon);
            root.Write('Houses', HOUSE_VICTORY_ORDER[I].H);
          root.EndObject;
        end;

      root.EndArray;

      root.WriteArray('Houses');
      for I := 0 to high(HOUSE_ID_TO_TYPE) do
      begin
        HT := HOUSE_ID_TO_TYPE[I];
        If not (HT in HOUSES_VALID) then
          Continue;
        root.WriteEmptyObject(I = 0);

        with fItems[HT] do
        begin
          //base data
          root.Write('Name', HT, true);
          root.Write('GameID', HOUSE_TYPE_TO_ID[HT] - 1);
          root.Write('TextID', fNameTextID);
          root.Write('DescriptionID', fDescriptionID);

          with fHouseDat do
          begin
            root.Write('MaxHealth', MaxHealth);
            root.Write('WoodPic', WoodPic);
            root.Write('StonePic', StonePic);
            root.Write('SnowPic', SnowPic);
            root.Write('WoodCost', WoodCost);
            root.Write('StoneCost', StoneCost);
            root.Write('TileCost', fTileCost);
            root.Write('EntranceOffsetX', EntranceOffsetX);
            root.Write('EntranceOffsetY', EntranceOffsetY);
            root.Write('EntranceOffsetXpx', EntranceOffsetXpx);
            root.Write('EntranceOffsetYpx', EntranceOffsetYpx);
            root.Write('WorkerRest', WorkerRest);
            root.Write('Sight', Sight);
            root.Write('NeedsPlayerOrder', NeedsPlayerOrder);
            root.Write('BuildIcon', BuildIcon);
            root.Write('TabletID', TabletID);
          end;
          root.Write('MaxInWares', fMaxInWares);
          root.Write('MaxOutWares', fMaxOutWares);
          root.Write('CanForceWork', fCanForceWork);
          root.Write('UnoccupiedMsgId', fUnoccupiedMsgId);
          //workers
          If length(Workers) > 0 then
          begin
            root.WriteArray('WorkerTypes');
              for K := 0 to High(Workers) do
              begin
                root.WriteLineObject('', K = 0);
                  root.Write('UnitType', Workers[K].UnitType, true);
                  root.Write('MaxCount', Workers[K].Count);
                root.EndLineObject;
              end;
            root.EndArray;
          end;
          root.Write('MaxWorkersCount', MaxWorkersCount);
          root.Write('CanOverfill', CanOverfill);

          If not TKMEnumUtils.GetName<TKMGatheringScript>(fGatheringScript, S) then
            raise Exception.Create('Wrong TKMGatheringScript, TKMResHouses');
          root.Write('GatheringScript', S);
          //house wares types
          root.Write('WareInput', fWareInput);
          root.Write('WareOutput', fWareOutput);
          //house release after
          root.WriteLineArray('UnlockAfter');
            L := 0;
            for HT2 in fUnlockAfter do
            begin
              If not TKMEnumUtils.GetName<TKMHouseType>(HT2, S) then
                raise Exception.Create('Wrong TKMHouseType, TKMResHouses');
              root.AddToArray(S, L = 0);
              Inc(L);
            end;
          root.EndLineArray;
          //wares supplies textures
          for J := 1 to WARES_IN_OUT_COUNT do
          begin
            If fSupplyIn[J, 1] < 10 then
              Continue;
            root.WriteLineArray('SupplyIn' + IntToStr(J));
            for K := 1 to 5 do
              root.AddToArray(fSupplyIn[J, K], K = 1);
            root.EndLineArray;
          end;

          for J := 1 to WARES_IN_OUT_COUNT do
          begin
            If fSupplyOut[J, 1] < 10 then
              Continue;

            root.WriteLineArray('SupplyOut' + IntToStr(J));
            for K := 1 to 5 do
              root.AddToArray(fSupplyOut[J, K], K = 1);
            root.EndLineArray;
          end;
          //house animations
          for HA := haWork1 to haFire8 do
          begin
            If fHouseDat.Anim[HA].Count = 0 then
              Continue;
            root.Write(HOUSE_ACTION_STR[HA], fHouseDat.Anim[HA]);
          end;
          //work animation sounds
          for HA := haWork1 to haWork5 do
          begin
            L := 0;
            If Sound[HA].Steps = [] then
              Continue;
            root.WriteLineObject('MakeSound_' + HOUSE_ACTION_STR[HA]);
              root.Write('SoundID', Sound[HA].ID, true);
              root.WriteLineArray('Steps');
                for B in Sound[HA].Steps do
                begin
                  root.AddToArray(B, L = 0);
                  Inc(L);
                end;
              root.EndLineArray;
            root.EndLineObject;
            Inc(L);
          end;
          //Work animation cycles
          If length(WorkAnim) > 0 then
          begin
            root.WriteArray('WorkAnim');
              for K := 0 to High(WorkAnim) do
              begin
                HA := WorkAnim[K].Action;
                root.WriteLineObject('', K = 0);
                  If not TKMEnumUtils.GetName<TKMHouseActionType>(HA, S) then
                    raise Exception.Create('Wrong TKMHouseActionType, TKMResHouses');
                  root.Write('WorkType', S, true);
                  root.Write('Cycles', WorkAnim[K].Cycles);
                root.EndLineObject;
              end;
            root.EndArray;
          end;
          //wares Slots
          If length(WareInputSlots) > 0 then
          begin
            root.WriteArray('WareSlots');
              for K := 0 to High(WareInputSlots) do
              begin
                root.WriteLineObject('', K = 0);
                  root.Write('GuiIcon', WareInputSlots[K].Icon, true);
                  root.Write('WareInput', WareInputSlots[K].WareInput);
                  root.Write('WareOutput', WareInputSlots[K].WareOutput);
                root.EndLineObject;
              end;
            root.EndArray;
          end;
          //house styles
          If length(Styles) > 0 then
          begin
            root.WriteArray('Styles');
              for K := 0 to High(Styles) do
              begin
                root.WriteLineObject('', K = 0);
                  root.Write('GuiIcon', Styles[K].Icon, true);
                  root.Write('StonePic', Styles[K].StonePic);
                  root.Write('SnowPic', Styles[K].SnowPic);
                  root.Write('HideSupplies', Styles[K].HideSupplies);
                root.EndLineObject;
              end;
            root.EndArray;
          end;
          //house levels
          If length(Levels) > 0 then
          begin
            root.WriteArray('Levels');
              for K := 0 to High(Levels) do
              begin
                root.WriteObject('', K = 0);
                  root.Write('StonePic', Levels[K].StonePic, true);
                  root.Write('SnowPic', Levels[K].SnowPic);
                  root.Write('WoodCost', Levels[K].WoodCost);
                  root.Write('StoneCost', Levels[K].StoneCost);
                  root.Write('TileCost', Levels[K].TileCost);
                  root.Write('MaxInWares', Levels[K].MaxInWares);
                  root.Write('WorkCyclesMultiplier', Levels[K].AnimMultiplier);
                  root.Write('ReplaceAnim', Levels[K].ReplaceAnim);
                  root.Write('HideSupplies', Levels[K].HideSupplies);
                  If length(Levels[K].Anim) > 0 then
                  begin
                    root.WriteArray('Anim', Levels[K].HideSupplies);
                    for J := 0 to High(Levels[K].Anim) do
                    begin
                      root.Write('', Levels[K].Anim[J], J = 0);
                    end;
                    root.EndArray;
                  end;
                root.EndObject;
              end;
            root.EndArray;
          end;
          //house build supply
          root.WriteLineObject('WoodPileOffset');
            root.Write('X', fBuildSupply[1].MoveX, true);
            root.Write('Y', fBuildSupply[1].MoveY);
          root.EndLineObject;
          root.WriteLineObject('StonePileOffset');
            root.Write('X', fBuildSupply[2].MoveX, true);
            root.Write('Y', fBuildSupply[2].MoveY);
          root.EndLineObject;
          root.WriteLineObject('TilePileOffset');
            root.Write('X', fBuildSupply[3].MoveX, true);
            root.Write('Y', fBuildSupply[3].MoveY);
          root.EndLineObject;
          //root.Write('Build_Area', BuildArea);
          root.Write('Build_Area', fBuildArea);
          root.Write('Ground_Area', fGroundArea);
          root.Write('Wariants', fWariants);
        end;
        root.EndObject;
      end;

      root.EndArray;
      //beast anim
      root.WriteArray('BeastAnim');
        for I := low(fBeastAnim) to High(fBeastAnim) do
        begin
          root.WriteArray('', I = low(fBeastAnim));
          for J := low(fBeastAnim[I]) to High(fBeastAnim[I]) do
          begin
            root.WriteArray('', J = low(fBeastAnim[I]));
            for K := low(fBeastAnim[I, J]) to High(fBeastAnim[I, J]) do
              root.Write('', fBeastAnim[I, J, K], K = low(fBeastAnim[I, J]));

            root.EndArray;
          end;
          root.EndArray;
        end;

      root.EndArray;

      //market beast anim
      root.WriteArray('MarketBeastAnim');
        for I := low(fMarketBeastAnim) to High(fMarketBeastAnim) do
          root.Write('', fMarketBeastAnim[I], I = low(fMarketBeastAnim));
      root.EndArray;
      //Hovel beast anim
      root.WriteArray('HovelBeastAnim');
        for I := low(fHovelBeastAnim) to High(fHovelBeastAnim) do
          root.Write('', fHovelBeastAnim[I], I = low(fHovelBeastAnim));
      root.EndArray;

      root.WriteArray('Palace_Flags');
        for I := low(Palace_Flags) to High(Palace_Flags) do
          root.Write('', Palace_Flags[I], I = low(Palace_Flags));
      root.EndArray;
      root.Write('Silo_Tablets', Silo_Tablets);
      root.Write('Merchant_Tablets', Merchant_Tablets);

      root.WriteObject('ProductionThatch_Anims');
      for I := 0 to Byte(High(TKMProdThatchAnimType)) do
      begin
        If not TKMEnumUtils.GetName<TKMProdThatchAnimType>(TKMProdThatchAnimType(I), S) then
          raise Exception.Create('Wrong TKMProdThatchAnimType, TKMResHouses');

        root.Write(S, ProdThatch_Anims[TKMProdThatchAnimType(I)], I = 0);
      end;
      root.EndObject;

      root.Write('WallTower_RecruitLeft', WallTower_RecruitLeft);
      root.Write('WallTower_RecruitRight', WallTower_RecruitRight);
      root.Write('School_Clock', School_Clock);

    root.EndFile;

    root.SaveToFile(ChangeFileExt(aPath, '.json'));
  finally
    root.Free;
  end;
end;


end.
