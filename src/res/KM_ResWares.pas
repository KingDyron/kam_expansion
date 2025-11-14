unit KM_ResWares;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_CommonClasses,
  KM_ResTypes,
  JsonDataObjects,
  KM_CommonTypes;

type
  // Ware type specification
  TKMVirtualWare = record
  private
    procedure AddProdWare(aType : TKMWareType; aMin, aMax, aChance : Byte; aReplaceWare : Boolean; aCycles : Word);
  public
    Index : Word;
    Name : String;
    TextID : Word;
    GUIIcon : Word;
    ProduceAfterWare: array of record
      WT : TKMWareType;
      Chance : Byte;
      MinCount : Byte;
      MaxCount : Byte;
      ReplaceWare : Boolean;
      Cycles : Word;
    end;
    MinProd : Byte;
    MaxProd : Byte;
    Cycles : Word;
    ProduceInHouses : TKMHouseTypeSet;
    ShowInHouses : TKMHouseTypeSet;
    CoinPrice : Single;
    MaxCountInStall: Byte;
    SpriteInStall: Word;
    Function GetProdCount(aHouse : TKMHouseType; aWareType : TKMWareType) : Byte;
    Function GetModulo(aHouse : TKMHouseType; aWareType : TKMWareType) : Byte;
    Function ReplacesWare(aHouse : TKMHouseType; aWareType : TKMWareType; aCount : Byte) : Boolean;
    function Title : String;
  end;

  TKMVirtualWareList = record
    private
      fCount : Integer;
      fList : array of TKMVirtualWare;
      function GetItem(aIndex : Integer) : TKMVirtualWare; overload;
      function GetItem(aName : String) : TKMVirtualWare; overload;
    public
      PALACE_WARES : array of Integer;
      procedure Add(aName : String; aTextID: Word; aIconID : Word;aHouses : TKMHouseTypeSet = []);
      property Ware[aIndex : Integer] : TKMVirtualWare read GetItem;default;
      property WareS[aName : String] : TKMVirtualWare read GetItem;
      property Count : Integer read fCount;
  end;


  TKMWareSpec = class
  private
    fType: TKMWareType;
    fMarketPrice: Single;
    fMarketPriceMultiplier: Single;
    fGuiIcon : Word;
    fProduction : record
      MinCount, MaxCount : Byte;
      OrderCost : TKMWareTypeArray;
      Houses: array of record
        House : TKMHouseType;
        MinCount, MaxCount : Byte;
      end;
    end;
    function GetGUIIcon: Word;
    function GetTextID: Integer;
    function GetTitle: UnicodeString;
    function GetGUIColor: Cardinal;
    function GetMarketPrice: Single;
    function GetWareOrderCost : TKMWareTypeArray;
    procedure SetMarketPriceMultiplier(aValue: Single);
  public
    CoinPrice : Word;
    AtTerrainPic : TKMWordArray;
    constructor Create(aType: TKMWareType);
    function IsValid: Boolean;
    property GUIColor: Cardinal read GetGUIColor;
    property GUIIcon: Word read fGuiIcon;
    property MarketPriceMultiplier: Single read fMarketPriceMultiplier write SetMarketPriceMultiplier;
    property MarketPrice: Single read GetMarketPrice;
    property Title: UnicodeString read GetTitle;
    property TextID: Integer read GetTextID;
    property OrderCost: TKMWareTypeArray read fProduction.OrderCost;

    function MaxProduction(aHouse : TKMHouseType) : Word;
    function MinProduction(aHouse : TKMHouseType) : Word;
    function GetProductionCount(aHouse : TKMHouseType) : Word;
    function GetTerrainPic(aCount : Byte) : Integer;
  end;

  TKMResWares = class
  private
    fCRC : Cardinal;
    fList: array [TKMWareType] of TKMWareSpec;
    fVirtualWares: TKMVirtualWareList;
    procedure CalculateCostsTable;
    function GetWare(aIndex: TKMWareType): TKMWareSpec;
    procedure LoadWareFromJson(aJSONFile : TJsonObject);
    procedure LoadVirtualWaresFromJson(aJSONFile : TJsonObject);
  public

    WareDistribution : TKMWareDistributionType;

    constructor Create;
    destructor Destroy; override;
    property Wares[aIndex: TKMWareType]: TKMWareSpec read GetWare; default;
    property VirtualWares : TKMVirtualWareList read fVirtualWares;
    procedure ExportCostsTable(const aFilename: string);
    function RatioFrom(aWareFrom, aWareTo : TkMWareType) : Word;
    function RatioTo(aWareFrom, aWareTo : TkMWareType) : Word;
    procedure ResetToDefaults;

    property CRC: Cardinal read fCRC; //Return hash of all values

    procedure SaveCustomData(aSaveStream: TKMemoryStream);
    procedure LoadCustomData(aLoadStream: TKMemoryStream);

    function WaresArrToIconArr(aWares : TKMWareTypeArray) : TIntegerArray;
    procedure ExportCSV(const aPath : String);
    function LoadWaresFromJson(aPath : String) : Cardinal;
    function LoadWareDistribution(aPath : String) : Cardinal;
    Procedure ReloadJSONData(UpdateCRC: Boolean);
  end;

  function ValidWareTypePair(oWT, dWT: TKMWareType): Boolean;

const
  MARKET_TRADEOFF_FACTOR = 2.2; //X resources buys 1 resource of equal value

  WARE_TY_TO_ID: array [TKMWareType] of byte = (0, //rtNone
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27,
    28, 29, 30, 31, 32, 33, 34,
    35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46,
    47, 48, 49, 50, 51,

    0, 0, 0, 0 //rtAll, rtWarfare, rtFood
    );
  RES_COUNT = 52;
  WARE_ID_TO_TYPE: array [0..RES_COUNT-1] of TKMWareType = (
    wtTrunk, wtStone, wtTimber, wtIronOre, wtGoldOre,//4
    wtCoal, wtIron, wtGold, wtWine, wtCorn,//9
    wtBread, wtFlour, wtLeather, wtSausage, wtPig,//14
    wtSkin, wtWoodenShield, wtIronShield, wtLeatherArmor, wtIronArmor,//19
    wtAxe, wtSword, wtLance, wtPike, wtBow,//24
    wtCrossbow, wtHorse, wtFish, wtBitin, wtVegetables,//29
    wtBitinOre,    wtStoneBolt,     wtLog,  wtSteelE,       wtBitinE,//34
    wtWheel,    wtBolt,          wtQuiver, wtWater, wtTile,//39
    wtSeed, wtSawDust, wtApple, wtJewerly, wtBoots, wtHay,
    wtMace, wtFlail, wtFeathers, wtPlateArmor, wtBitinArmor,
    wtEgg
    );

  ORE_DENSITY_MAX_TYPES = 5; // How many different ore densities there are

   WARES_PROD_COUNT: array [TKMWareType] of byte = (
    0,//wtNone
    1, 3, 2, 1, 1, 1, 1, 2,//wtTrunk..wtGold
    1, 1, 2, 1, 2, 2, 1, 1,//wtWine..wtSkin
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, //wtWoodenShield .. wtArbalet
    1, 1, 1, 1, 1, 2, 1, //wtHorse..wtLog
    2, 1, 2, 2, 2, //wtSteelE..wtQuiver
    1, 2, 1, 2, 3, 1,//wtWater
    2, 1, 1, 1, 1, 1,//wtBoots, wtHay..wtPlateArmor
    1, 1,//wtBitinArmor, wtEgg
    0, 0, 0, 0//wtFood,wtAny, wtWarfare, wtValuable

  );
  //How many of resource gets produced per minute on AVERAGE
  //Measured on a test map RES_COUNT / TIME in minutes
  PRODUCTION_RATE: array [WARE_MIN..WARE_MAX] of Single = (
    88/120, 414/120, 390/120, 160/120,  160/120,
    155/120, 218/120, 330/120, 78/120, 292/120,
    336/120, 162/120, 324/120, 600/120, 300/120,
    84/180,  180/120, 155/120, 180/120, 155/120,
    200/120, 195/120, 200/120, 195/120, 200/120,
    195/120, 69/120,  122/120, 199/120, 250/120,

    65/120,  168/120,  84/120,  166/120,  83/120,
    168/120,  167/120,  160/120,  345/120,  102/120,
    192/120, 219/120, 216/120,     1/400,     55/120,
    195/120, 195/120, 195/120, 225/120, 195/120,
    195/120, 22/120
    );

implementation
uses
  Math, KM_ResTexts, KM_Resource, KM_Defaults,
  KM_CommonClassesExt, KM_CommonUtils, KM_JSONUtils,
  KM_Points,
  KM_JsonHelpers;

procedure TKMVirtualWare.AddProdWare(aType : TKMWareType; aMin, aMax, aChance : Byte; aReplaceWare : Boolean; aCycles : Word);
begin
  SetLength(ProduceAfterWare, length(ProduceAfterWare) + 1);
  with ProduceAfterWare[high(ProduceAfterWare)] do
  begin
      WT := aType;
      MinCount := aMin;
      MaxCount := aMax;
      Chance := aChance;
      ReplaceWare := aReplaceWare;
      Cycles := aCycles;
  end;
end;

function TKMVirtualWare.GetProdCount(aHouse : TKMHouseType; aWareType : TKMWareType) : Byte;
var I : Integer;
begin
  Result := 0;

  if aHouse <> htAny then
    if not (aHouse in self.ProduceInHouses) then
      Exit;

  if (length(ProduceAfterWare) = 0) or (aWareType = wtAll) then
  begin
    //Result := MinProd + KamRandom(MaxProd - MinProd + 1, '');
    Exit(MinProd);
  end else
  for I := 0 to High(ProduceAfterWare) do
    if (ProduceAfterWare[I].WT = wtAll) or (ProduceAfterWare[I].WT = aWareType) then
      with ProduceAfterWare[I] do
        Exit(MinCount);
        //if (Chance = 0) or (KamRandom(100, '') <= Chance - 1) then
          //Exit(MinCount + KamRandom(MaxCount - MinCount + 1, ''));
end;

function TKMVirtualWare.GetModulo(aHouse : TKMHouseType; aWareType : TKMWareType) : Byte;
var I : Integer;
begin
  Result := 0;

  if aHouse <> htAny then
    if not (aHouse in self.ProduceInHouses) then
      Exit;

  if (length(ProduceAfterWare) = 0) or (aWareType = wtAll) then
  begin
    Result := Cycles;
  end else
  for I := 0 to High(ProduceAfterWare) do
    if (ProduceAfterWare[I].WT = wtAll) or (ProduceAfterWare[I].WT = aWareType) then
      Exit(ProduceAfterWare[I].Cycles)
      {
        if (Chance = 0) or (KamRandom(100, '') <= Chance - 1) then
          Exit(MinCount + KamRandom(MaxCount - MinCount + 1, ''));}
end;
Function TKMVirtualWare.ReplacesWare(aHouse : TKMHouseType; aWareType : TKMWareType; aCount : Byte) : Boolean;
var I : Integer;
begin
  Result := false;

  if aHouse <> htAny then
    if not (aHouse in self.ProduceInHouses) then
      Exit;

  if (length(ProduceAfterWare) = 0) or (aWareType = wtAll) then
  begin
    Result := false;
  end else
  for I := 0 to High(ProduceAfterWare) do
    if (ProduceAfterWare[I].WT = wtAll) or (ProduceAfterWare[I].WT = aWareType) then
      with ProduceAfterWare[I] do
        if aCount > 0 then
          Exit(ReplaceWare);


end;

function TKMVirtualWare.Title: string;
begin
  Result := gResTexts[Self.TextID];
end;

procedure TKMVirtualWareList.Add( aName : String; aTextID: Word; aIconID: Word; aHouses: TKMHouseTypeSet = []);
begin

  Inc(fCount);
  SetLength(fList, fCount);
  with fList[fCount - 1] do
  begin
    Name := aName;
    TextID := aTextID;
    GUIIcon := aIconID;
    ProduceInHouses := aHouses;
    Index := fCount - 1;
  end;

end;

function  TKMVirtualWareList.GetItem(aIndex: Integer): TKMVirtualWare;
begin
  Assert((aIndex >= 0) or (aIndex <=high(fList)));

  Result := fList[aIndex];
end;

function  TKMVirtualWareList.GetItem(aName: String): TKMVirtualWare;
var I : Integer;
begin
  Result.Name := 'NotFound:' + aName;
  Result.Index := high(word);
  for I := 0 to High(fList) do
    if fList[I].Name = aName then
      Exit(fList[I]);
end;

constructor TKMWareSpec.Create(aType: TKMWareType);
begin
  inherited Create;

  fMarketPriceMultiplier := 1;

  fType := aType;

  case fType of
    wtApple : CoinPrice := 4;
    wtGold : CoinPrice := 10;
    wtEgg : CoinPrice := 30;
    wtJewerly : CoinPrice := 500;
    else CoinPrice := 1;
  end;


  fProduction.MinCount := WARES_PROD_COUNT[aType];
  fProduction.MaxCount := WARES_PROD_COUNT[aType];
  fGuiIcon := GetGUIIcon;
  fProduction.OrderCost := GetWareOrderCost;
end;


function TKMWareSpec.GetGUIColor: Cardinal;
const
  //Resources colors for Results charts
  //Made by naospor from kamclub.ru
  WARE_COLOR: array [TKMWareType] of Cardinal = (
    $004080,
    $004080, $BFBFBF, $0080BF, $BF4040, $00FFFF,
    $606060, $BF0000, $00BFFF, $000080, $80FFFF,
    $80BFFF, $FFFFFF, $4040BF, $0000FF, $0040BF,
    $008080, $00BF00, $00FF7F, $FFBF00, $BF0080,
    $FF0040, $00FF40, $FFFF40, $FF0080, $FFFF80,
    $101080, $0080FF, $FFBF00, $FFBF00, $FFBF00,
    $004080, $101080, $0080FF, $FFBF00, $FFBF00,
    $101080, $0080FF, $FFBF00, $FFBF00, $FFBF00,//wtTIle
    $FFBF00, $FFBF00, $00BFFF, $00BFFF, $00BFFF,
    $FFBF00, $FFBF00, $00BFFF, $00BFFF, $00BFFF,
    $00BFFF, $FFBF00,
    $004080,$004080,$004080,$004080 //wtAll, wtFood, wtWarfare, wtValuable
    );
begin
  Result := WARE_COLOR[fType];

end;

function TKMWareSpec.MaxProduction(aHouse : TKMHouseType) : Word;
var I : Integer;
begin
  Result := fProduction.MaxCount;

  for I := 0 to High(fProduction.Houses) do
    if fProduction.Houses[I].House = aHouse then
      Result := fProduction.Houses[I].MaxCount;

end;

function TKMWareSpec.MinProduction(aHouse : TKMHouseType) : Word;
var I : Integer;
begin
  Result := fProduction.MinCount;

  for I := 0 to High(fProduction.Houses) do
    if fProduction.Houses[I].House = aHouse then
      Result := fProduction.Houses[I].MinCount;

end;

function TKMWareSpec.GetProductionCount(aHouse : TKMHouseType) : Word;
var aMin : Integer;
begin
  aMin := MinProduction(aHouse);
  Result := aMin;
  //Result := aMin + KaMRandom(aMax - aMin + 1, 'TKMWareSpec.GetProductionCount');
end;

function TKMWareSpec.GetTerrainPic(aCount: Byte): Integer;
begin
  Result := 0;

  if Length(AtTerrainPic) = 0 then
    Exit;
  Result := AtTerrainPic[Min(aCount - 1, high(AtTerrainPic))];
end;

function TKMWareSpec.GetMarketPrice: Single;
begin
  Result := fMarketPrice * fMarketPriceMultiplier;
end;

//That function is instead of Constant array because wares that are produced by orders are in not linear order
function TKMWareSpec.GetWareOrderCost: TKMWareTypeArray;
begin
  Result := [wtNone, wtNone, wtNone];
  case fType of

    //Warfare
    wtWoodenShield: Result := [wtTimber];
    wtIronShield: Result := [wtCoal, wtIron];
    wtLeatherArmor: Result := [wtLeather];
    wtIronArmor: Result := [wtCoal, wtIron];
    wtAxe: Result := [wtTimber, wtTimber];
    wtSword: Result := [wtCoal, wtIron];
    wtLance: Result := [wtTimber, wtTimber];
    wtPike: Result := [wtCoal, wtIron];
    wtBow: Result := [wtTimber, wtTimber];
    wtCrossbow: Result := [wtCoal, wtIron];
    wtQuiver: Result := [wtLeather, wtTimber];

    //Machines
    wtStoneBolt: Result := [wtStone];
    wtLog: Result := [wtTrunk];

    wtSteelE: Result := [wtCoal, wtIron];
    wtBitinE: Result := [wtCoal, wtBitin];
    wtBolt: Result := [wtTrunk, wtCoal, wtIron];


    wtWheel: Result := [wtTrunk];
    //others
    wtSawDust: Result := [wtTrunk];
  end;
end;

procedure TKMWareSpec.SetMarketPriceMultiplier(aValue: Single);
begin
  fMarketPriceMultiplier := EnsureRange(aValue, 0.01, 100);
end;


function TKMWareSpec.GetGUIIcon: Word;
begin
  case fType of
    WARE_MIN..wtFish: Result := 351 + WARE_TY_TO_ID[fType];
    wtAll:             Result := 657;
    wtWarfare:         Result := 658;
    wtFood:            Result := 659;
    wtValuable:        Result := 1111;
    wtBitin:           Result := 682;
    wtBitinOre:        Result := 688;
    wtVegetables:      Result := 681;

    wtStoneBolt:        Result := 695;
    wtBolt:             Result := 696;
    wtWheel:            Result := 697;
    wtLog:              Result := 698;
    wtQuiver:           Result := 699;
    wtSteelE:           Result := 700;
    wtBitinE:           Result := 701;
    wtWater:            Result := 702;
    wtTile:            Result := 706;
    wtSeed:            Result := 707;
    wtSawDust:         Result := 710;
    wtApple:           Result := 716;
    wtJewerly:           Result := 725;
    wtBoots:           Result := 726;
    wtHay:           Result := 837;

    wtMace            : Result := 862;
    wtFlail           : Result := 863;
    wtFeathers        : Result := 864;
    wtPlateArmor      : Result := 865;
    wtBitinArmor      : Result := 866;
    wtEgg             : Result := 936;
  else
    Result := 0; // "Question mark"
  end;
end;


function TKMWareSpec.GetTextID: Integer;
begin
  case fType of
    WARE_MIN..wtFish: Result := TX_RESOURCES_NAMES__27 + WARE_TY_TO_ID[fType];
    wtAll:             Result := TX_RESOURCES_ALL;
    wtWarfare:         Result := TX_RESOURCES_WARFARE;
    wtFood:            Result := TX_RESOURCES_FOOD;
    wtValuable:        Result := 1661;
    wtBitinOre: Result := 1607;
    wtBitin: Result :=1606;
    wtVegetables: Result := 1608;
    wtStoneBolt: Result := 1609;
    wtLog: Result := 1612;
    wtSteelE: Result := 1614;
    wtBitinE: Result := 1615;
    wtWheel: Result := 1611;
    wtBolt: Result := 1610;
    wtQuiver: Result := 1613;
    wtWater: Result := 1616;
    wtTile: Result := 1630;
    wtSeed: Result := 1631;
    wtSawDust: Result := 1641;
    wtApple: Result := 1642;
    wtJewerly: Result := 1669;
    wtBoots: Result := 1670;
    wtHay: Result := 1973;

    wtMace: Result := 1977;
    wtFlail: Result := 1978;
    wtFeathers: Result := 1979;
    wtPlateArmor: Result := 1980;
    wtBitinArmor: Result := 1981;
    wtEgg: Result := 2101;
  else
    Result := -1;
  end;

end;


function TKMWareSpec.GetTitle: UnicodeString;
begin
  if GetTextID <> -1 then
    Result := gResTexts[GetTextID]
  else
    Result := 'N/A';

end;


function TKMWareSpec.IsValid: Boolean;
begin
  Result := fType in WARES_VALID;
end;


{ TKMResWares }
constructor TKMResWares.Create;
var
  I: TKMWareType;
begin
  inherited;

  for I := Low(TKMWareType) to High(TKMWareType) do
    fList[I] := TKMWareSpec.Create(I);

  // Calcuate the trade costs for marketplace once
  CalculateCostsTable;
  fCRC := LoadWaresFromJson(gRes.JsonData[dtWares]);
  fCRC := fCRC xor LoadWareDistribution(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'WareDistribution.json');



end;


destructor TKMResWares.Destroy;
var
  I: TKMWareType;
begin
  for I := Low(TKMWareType) to High(TKMWareType) do
    fList[I].Free;

  inherited;
end;


function TKMResWares.GetWare(aIndex: TKMWareType): TKMWareSpec;
begin
  Result := fList[aIndex];
end;

function TKMResWares.RatioFrom(aWareFrom, aWareTo : TkMWareType) : Word;
var
  costFrom, costTo: Single;
begin
  if (aWareFrom <> wtNone) and (aWareTo <> wtNone) then
  begin
    //When trading target ware is priced higher
    costFrom := gRes.Wares[aWareFrom].MarketPrice;
    costTo := gRes.Wares[aWareTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Min(Round(costTo / Min(costFrom, costTo)), High(Word));
  end else
    Result := 1;
end;

function TKMResWares.RatioTo(aWareFrom, aWareTo : TkMWareType) : Word;
var
  costFrom, costTo: Single;
begin
  if (aWareFrom <> wtNone) and (aWareTo <> wtNone) then
  begin
    //When trading target ware is priced higher
    costFrom := gRes.Wares[aWareFrom].MarketPrice;
    costTo := gRes.Wares[aWareTo].MarketPrice * MARKET_TRADEOFF_FACTOR;
    Result := Min(Round(costFrom / Min(costFrom, costTo)), High(Word));
  end else
    Result := 1;
end;


procedure TKMResWares.ResetToDefaults;
var
  I: TKMWareType;
begin
  for I := Low(TKMWareType) to High(TKMWareType) do
    fList[I].fMarketPriceMultiplier := 1;
end;


procedure TKMResWares.SaveCustomData(aSaveStream: TKMemoryStream);
var
  I: TKMWareType;
begin
  aSaveStream.PlaceMarker('WaresCustomData');
  for I := Low(TKMWareType) to High(TKMWareType) do
    aSaveStream.Write(fList[I].fMarketPriceMultiplier);
end;


procedure TKMResWares.LoadCustomData(aLoadStream: TKMemoryStream);
var
  I: TKMWareType;
begin
  aLoadStream.CheckMarker('WaresCustomData');
  for I := Low(TKMWareType) to High(TKMWareType) do
    aLoadStream.Read(fList[I].fMarketPriceMultiplier);
end;

function TKMResWares.WaresArrToIconArr(aWares: TKMWareTypeArray): TIntegerArray;
var I : Integer;
begin
  SetLength(Result, Length(aWares));
  for I := 0 to High(aWares) do
    Result[I] := fList[aWares[I]].fGuiIcon;
end;

// Export costs table for analysis in human-friendly form
procedure TKMResWares.ExportCostsTable(const aFilename: string);
var
  SL: TStringList;
  I: TKMWareType;
begin
  SL := TStringList.Create;
  try
    for I := WARE_MIN to WARE_MAX do
      SL.Add(fList[I].GetTitle + #9 + #9 + FloatToStr(fList[I].fMarketPrice));

    SL.SaveToFile(aFilename);
  finally
    SL.Free;
  end;
end;


procedure TKMResWares.CalculateCostsTable;
const
  NON_RENEW = 1.25; //Non-renewable resources are more valuable than renewable ones
  TREE_ADDN = 0.15; //Trees require a large area (e.g. compared to corn)
  WINE_ADDN = 0.1; //Wine takes extra wood to build
  ORE_ADDN = 0.2; //You can only build a few iron/gold mines on most maps (compared to coal)
begin
  //Take advantage of the fact that we have both classes in same unit
  //and assign to private field directly
  Wares[wtTrunk      ].fMarketPrice := (1/PRODUCTION_RATE[wtTrunk]) + TREE_ADDN;
  Wares[wtStone      ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtStone]);
  Wares[wtTimber       ].fMarketPrice := (1/PRODUCTION_RATE[wtTimber]) + (1/2)*Wares[wtTrunk].MarketPrice;
  Wares[wtIronOre    ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtIronOre]) + ORE_ADDN;
  Wares[wtGoldOre    ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtGoldOre]) + ORE_ADDN;
  Wares[wtCoal       ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtCoal]);
  Wares[wtIron      ].fMarketPrice := (1/PRODUCTION_RATE[wtIron]) + Wares[wtIronOre].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtGold       ].fMarketPrice := (1/PRODUCTION_RATE[wtGold]) + (1/2)*(Wares[wtGoldOre].MarketPrice + Wares[wtCoal].MarketPrice);
  Wares[wtWine       ].fMarketPrice := (1/PRODUCTION_RATE[wtWine]) + WINE_ADDN;
  Wares[wtCorn       ].fMarketPrice := (1/PRODUCTION_RATE[wtCorn]) / 2;
  Wares[wtFlour      ].fMarketPrice := (1/PRODUCTION_RATE[wtFlour]) + Wares[wtCorn].MarketPrice;
  Wares[wtWater       ].fMarketPrice := (1/PRODUCTION_RATE[wtWater]);
  Wares[wtPig        ].fMarketPrice := (1/PRODUCTION_RATE[wtPig]) + (4*Wares[wtCorn].MarketPrice + 4*Wares[wtWater].MarketPrice) / 4; //1/2 because two products are made simultaneously
  Wares[wtSkin       ].fMarketPrice := (1/PRODUCTION_RATE[wtSkin]) + (4*Wares[wtCorn].MarketPrice + 4*Wares[wtWater].MarketPrice) / 4; //1/2 because two products are made simultaneously
  Wares[wtLeather    ].fMarketPrice := (1/PRODUCTION_RATE[wtLeather]) + (1/2)*Wares[wtSkin].MarketPrice;
  Wares[wtSausage   ].fMarketPrice := (1/PRODUCTION_RATE[wtSausage]) + Wares[wtPig].MarketPrice / 3;
  Wares[wtWoodenShield     ].fMarketPrice := (1/PRODUCTION_RATE[wtWoodenShield]) + Wares[wtTimber].MarketPrice;
  Wares[wtIronShield].fMarketPrice := (1/PRODUCTION_RATE[wtIronShield]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtLeatherArmor      ].fMarketPrice := (1/PRODUCTION_RATE[wtLeatherArmor]) + Wares[wtLeather].MarketPrice;
  Wares[wtIronArmor ].fMarketPrice := (1/PRODUCTION_RATE[wtIronArmor]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtAxe        ].fMarketPrice := (1/PRODUCTION_RATE[wtAxe]) + 2*Wares[wtTimber].MarketPrice;
  Wares[wtSword      ].fMarketPrice := (1/PRODUCTION_RATE[wtSword]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtLance       ].fMarketPrice := (1/PRODUCTION_RATE[wtLance]) + 2*Wares[wtTimber].MarketPrice;
  Wares[wtPike  ].fMarketPrice := (1/PRODUCTION_RATE[wtPike]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtBow        ].fMarketPrice := (1/PRODUCTION_RATE[wtBow]) + 2*Wares[wtTimber].MarketPrice;
  Wares[wtCrossbow    ].fMarketPrice := (1/PRODUCTION_RATE[wtCrossbow]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtHorse      ].fMarketPrice := (1/PRODUCTION_RATE[wtHorse]) + 4*Wares[wtCorn].MarketPrice + 4*Wares[wtWater].MarketPrice;
  Wares[wtFish       ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtFish]);


  Wares[wtVegetables       ].fMarketPrice := (1/PRODUCTION_RATE[wtVegetables]) + Wares[wtSeed].MarketPrice / 2;
  Wares[wtBitinOre      ].fMarketPrice := NON_RENEW*(1/PRODUCTION_RATE[wtBitinOre]) * 2;
  Wares[wtBitin      ].fMarketPrice := (1/PRODUCTION_RATE[wtBitin] + Wares[wtCoal].MarketPrice + Wares[wtBitinOre].MarketPrice);

  Wares[wtStoneBolt       ].fMarketPrice := (1/PRODUCTION_RATE[wtStoneBolt])+ Wares[wtStone].MarketPrice / 2;
  Wares[wtLog       ].fMarketPrice := (1/PRODUCTION_RATE[wtLog])+ Wares[wtTrunk].MarketPrice;
  Wares[wtSteelE       ].fMarketPrice := (1/PRODUCTION_RATE[wtSteelE])+Wares[wtIron].MarketPrice/2 + Wares[wtCoal].MarketPrice/2;
  Wares[wtBitinE       ].fMarketPrice := (1/PRODUCTION_RATE[wtBitinE])+Wares[wtBitin].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtWheel       ].fMarketPrice := (1/PRODUCTION_RATE[wtWheel])+Wares[wtTrunk].MarketPrice/2;
  Wares[wtBolt       ].fMarketPrice :=  (1/PRODUCTION_RATE[wtBolt]) + (Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice + Wares[wtTrunk].MarketPrice) / 2;
  Wares[wtQuiver       ].fMarketPrice := (1/PRODUCTION_RATE[wtQuiver]) + (Wares[wtLeather].MarketPrice + Wares[wtFeathers].MarketPrice) / 2;

  Wares[wtTile       ].fMarketPrice := (1/PRODUCTION_RATE[wtTile]);
  Wares[wtSeed       ].fMarketPrice := (1/PRODUCTION_RATE[wtSeed]) / 2;
  Wares[wtSawDust       ].fMarketPrice := (1/PRODUCTION_RATE[wtSawDust]) + (Wares[wtTrunk].MarketPrice / 2);
  Wares[wtApple       ].fMarketPrice := (1/PRODUCTION_RATE[wtApple]) + Wares[wtWater].MarketPrice;
  Wares[wtJewerly       ].fMarketPrice := 300;
  Wares[wtBoots       ].fMarketPrice := (1/PRODUCTION_RATE[wtBoots]) + Wares[wtLeather].fMarketPrice;
  Wares[wtHay       ].fMarketPrice := (1/PRODUCTION_RATE[wtHay]);

  Wares[wtMace       ].fMarketPrice := (1/PRODUCTION_RATE[wtMace]) + Wares[wtTimber].MarketPrice * 3;
  Wares[wtFlail       ].fMarketPrice := (1/PRODUCTION_RATE[wtFlail]) + Wares[wtIron].MarketPrice + Wares[wtCoal].MarketPrice;
  Wares[wtFeathers       ].fMarketPrice := (1/PRODUCTION_RATE[wtFeathers]) + (Wares[wtSeed].MarketPrice + Wares[wtWater].MarketPrice) * 3;
  Wares[wtPlateArmor       ].fMarketPrice := (1/PRODUCTION_RATE[wtPlateArmor]) + Wares[wtTimber].MarketPrice * 2;
  Wares[wtBitinArmor       ].fMarketPrice := (1/PRODUCTION_RATE[wtBitinArmor]) + Wares[wtCoal].MarketPrice * 2 + Wares[wtBitinE].MarketPrice ;

  Wares[wtBread      ].fMarketPrice := (1/PRODUCTION_RATE[wtBread]) + Wares[wtFlour].MarketPrice / 2 + Wares[wtVegetables].MarketPrice / 2 + Wares[wtWater].MarketPrice / 2;
  Wares[wtEgg].fMarketPrice := Wares[wtJewerly].fMarketPrice / 7;
end;

function TKMResWares.LoadWareDistribution(aPath : String) : Cardinal;
var
  I, J, K : Integer;
  jsonPath: string;
  nWare, nRoot, nHouse: TJsonObject;
  nWares, nArr: TJsonArray;
  H : TKMHouseType;
  W : TKMWareType;
begin
  jsonPath := aPath;
  if not FileExists(jsonPath) then
    Exit(0);

  nRoot := TJsonObject.ParseFromFile(jsonPath) as TJsonObject;

  SetLength(WareDistribution, 0);
  try
    nWares := nRoot.A['Wares'];
    Assert(nWares.Count <> 0, 'No Wares data found');
    Result := GetJSONCRC(nRoot);
    for I := 0 to nWares.Count - 1 do
    begin
      nWare := nWares.O[I];

      if not TKMEnumUtils.TryGetAs<TKMWareType>(nWare.S['Ware'],  W) then
        raise Exception.Create('Error loading ' + jsonPath + ': wrong WareType name: ' + nWare.S['Ware']);

      nArr := nWare.A['Houses'];
      if nArr.Count = 0 then
        Continue;

      J := Length(WareDistribution);
      SetLength(WareDistribution, J + 1);

      WareDistribution[J].WareType := W;


      SetLength(WareDistribution[J].Houses, nArr.Count);

      for K := 0 to nArr.Count - 1 do
      begin
        nHouse := nArr.O[K];

        if not TKMEnumUtils.TryGetAs<TKMHouseType>(nHouse.S['HouseName'],  H) then
          raise Exception.Create('Error loading ' + jsonPath + ': wrong HouseType name: ' + nHouse.S['HouseName']);

        WareDistribution[J].Houses[K].House := H;
        if nHouse.Contains('Qty') then
          WareDistribution[J].Houses[K].Qty := EnsureRange(nHouse.I['Qty'], 0, 5)
        else
          WareDistribution[J].Houses[K].Qty := 5;

      end;


    end;

  finally

  end;





end;

function TKMResWares.LoadWaresFromJson(aPath: string) : Cardinal;
var
  I : Integer;
  nRoot: TJsonObject;
  nWares: TJsonArray;
begin
  if not FileExists(aPath) then
    Exit(0);

  nRoot := TJsonObject.ParseFromFile(aPath) as TJsonObject;
  Result := GetJSONCRC(nRoot);

  try
    if nRoot.Contains('VirtualWares') then
      LoadVirtualWaresFromJson(nRoot);

    nWares := nRoot.A['Wares DATA'];

    if nWares.Count > 0 then
      for I := 0 to nWares.Count - 1 do
        LoadWareFromJson(nWares.O[I]);

  finally
    nRoot.Free;
  end;
end;

procedure TKMResWares.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin
  oldCRC := fCRC;
  fCRC := LoadWaresFromJson(gRes.JsonData[dtWares]);
  fCRC := fCRC xor LoadWareDistribution(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'WareDistribution.json');
  if not UpdateCRC then
    fCRC := oldCRC;
end;

procedure TKMResWares.LoadWareFromJson(aJSONFile : TJsonObject);
var I : Integer;
  nArr: TJsonArray;
  WT, WT2 : TKMWareType;
begin

  if not TKMEnumUtils.TryGetAs<TKMWareType>(aJSONFile.S['Name'],  WT) then
    raise Exception.Create('Wrong WareType');

  if aJSONFile.Contains('MarketPrice') then
    fList[WT].fMarketPrice := aJSONFile.I['MarketPrice'];
  if aJSONFile.Contains('MarketPriceMultiplier') then
    fList[WT].fMarketPriceMultiplier := aJSONFile.I['MarketPriceMultiplier'];

  if aJSONFile.Contains('GuiIcon') then
    fList[WT].fGuiIcon := aJSONFile.I['GuiIcon'];

  with fList[WT].fProduction do
  begin

    if aJSONFile.Contains('MinCount') then MinCount := aJSONFile.I['MinCount'];
    if aJSONFile.Contains('MaxCount') then MaxCount := aJSONFile.I['MaxCount'];

    if aJSONFile.Contains('InHouses') then
    begin
      nArr := aJSONFile.A['InHouses'];

      for I := 0 to nArr.Count - 1 do
      begin
        SetLength(Houses, length(Houses) + 1);

        if not TKMEnumUtils.TryGetAs<TKMHouseType>(nArr[I].S['HouseType'],  Houses[high(Houses)].House) then
          raise Exception.Create('Wrong HouseType');

        Houses[high(Houses)].MinCount := nArr[I].I['MinCount'];
        Houses[high(Houses)].MaxCount := nArr[I].I['MaxCount'];
      end;
    end;

    nArr := aJsONFile.A['OrderCost'];
    if nArr.Count > 0 then
      SetLength(OrderCost, 0);

    for I := 0 to nArr.Count - 1 do
    begin
      if not TKMEnumUtils.TryGetAs<TKMWareType>(nArr[I],  WT2) then
        raise Exception.Create('Wrong WareType');

      SetLength(OrderCost, length(OrderCost) + 1);
      OrderCost[high(OrderCost)] := WT2;
    end;
    JSONArrToValidArr(aJSONFile.A['TerrainPics'], fList[WT].AtTerrainPic);

    //OrderCost : array of TKMWareType;
  end;

end;

procedure TKMResWares.LoadVirtualWaresFromJson(aJSONFile : TJsonObject);
var I, K, L : Integer;
  nVWare : TJsonObject;
  nArr1, nArr2: TJsonArray;
  setHouses : TKMHouseTypeSet;
  HT: TKMHouseType;
  WT : TKMWareType;
begin

  nArr1 := aJSONFile.A['VirtualWares'];

  if nArr1.Count = 0 then
    Exit;
  SetLength(fVirtualWares.fList, 0);
  fVirtualWares.fCount := 0;
  for I := 0 to nArr1.Count - 1 do
  begin
    nVWare := nArr1.O[I];
    nArr2 := nVWare.A['ProduceInHouses'];

    setHouses := [];
    for K := 0 to nArr2.Count - 1 do
    begin
      if not TKMEnumUtils.TryGetAs<TKMHouseType>(nArr2.S[K],  HT) then
        raise Exception.Create('Wrong HouseType');

      if HT <> htNone then
        setHouses := setHouses + [HT];

    end;

    fVirtualWares.Add(nVWare.S['Name'],nVWare.I['TextID'], nVWare.I['IconID'], setHouses);
    L := high(fVirtualWares.fList);

    fVirtualWares.fList[L].MinProd := nVWare.I['ProductionCountMin'];
    fVirtualWares.fList[L].MaxProd := nVWare.I['ProductionCountMax'];
    fVirtualWares.fList[L].CoinPrice := nVWare.D['CoinPrice'];
    fVirtualWares.fList[L].MaxCountInStall := nVWare.I['MaxCountInStall'];
    fVirtualWares.fList[L].Cycles := nVWare.I['Cycles'];
    fVirtualWares.fList[L].SpriteInStall := nVWare.I['SpriteInStall'];

    nArr2 := nVWare.A['ShowInHouses'];
    setHouses := [];
    for K := 0 to nArr2.Count - 1 do
    begin
      if not TKMEnumUtils.TryGetAs<TKMHouseType>(nArr2.S[K],  HT) then
        raise Exception.Create('Wrong HouseType');

      if HT <> htNone then
        if HT = htPalace then
        begin
          SetLength(fVirtualWares.PALACE_WARES, length(fVirtualWares.PALACE_WARES) + 1);
          fVirtualWares.PALACE_WARES[high(fVirtualWares.PALACE_WARES)] := L;
        end else
          setHouses := setHouses + [HT];

    end;
    fVirtualWares.fList[L].ShowInHouses := setHouses;


    nArr2 := nVWare.A['ProduceAfterWares'];
    for K := 0 to nArr2.Count - 1 do
    begin
      if not TKMEnumUtils.TryGetAs<TKMWareType>(nArr2.O[K].S['Ware'],  WT) then
        raise Exception.Create('Wrong WareType: ' + nArr2.O[K].S['Ware']);

      if WT in [wtNone] then
        Continue;

      fVirtualWares.fList[L].AddProdWare(WT, nArr2.O[K].I['Min'], nArr2.O[K].I['Max'],
                                          nArr2.O[K].I['Chance'], nArr2.O[K].B['ReplaceWare'],
                                          nArr2.O[K].I['Cycles']);

    end;

  end;

  nArr1 := aJSONFile.A['VirtualWaresPalaceOrder'];
  if nArr1.Count > 0 then
    Setlength(fVirtualWares.PALACE_WARES, 0);

  for I := 0 to nArr1.Count - 1 do
  begin
    K := fVirtualWares.WareS[nArr1.S[I]].Index;
    if K = high(word) then
      raise Exception.Create(fVirtualWares.WareS[nArr1.S[I]].Name)
    else
    begin
      Setlength(fVirtualWares.PALACE_WARES, length(fVirtualWares.PALACE_WARES) + 1);
      fVirtualWares.PALACE_WARES[high(fVirtualWares.PALACE_WARES)] := K;
    end;

  end;
      

end;

procedure TKMResWares.ExportCSV(const aPath : String);
var
  WT: TKMWareType;
  S: string;
  SL: TStringList;
  I, K: Integer;
var
  root : TKMJsonSaver;

  procedure AddField(const aField: string); overload;
  begin S := S + aField + ';'; end;
  procedure AddField(aField: Integer); overload;
  begin S := S + IntToStr(aField) + ';'; end;
begin
  if IsFileInUse(aPath) then
    Exit;

  SL := TStringList.Create;

  S := 'Ware Name; WareID;';
  SL.Append(S);

  for I := 0 to high(WARE_ID_TO_TYPE) do
  begin
    WT := WARE_ID_TO_TYPE[I];
    S := '';
    AddField(fList[WT].Title);
    AddField(I);
    SL.Append(S);
  end;

  SL.Append('');
  S := 'Virtual Ware Name;WareID;WareTitle';
  SL.Append(S);
  for I := 0 to VirtualWares.Count - 1 do
  begin
    S := '';
    AddField(VirtualWares[I].Name);
    AddField(I);
    AddField(gResTexts[VirtualWares[I].TextID]);
    SL.Append(S);
  end;

  ForceDirectories(ExtractFilePath(aPath));

  try
    SL.SaveToFile(aPath);
  finally
    SL.Free;
  end;
  Exit;
  root := TKMJsonSaver.Create;

  try
    root.BeginFile;
      root.WriteArray('Wares DATA', true);
        for I := 0 to high(WARE_ID_TO_TYPE) do
        begin
          WT := WARE_ID_TO_TYPE[I];
          If not TKMEnumUtils.GetName<TKMWareType>(WT, S) then
            raise Exception.Create('Error 1');

          root.WriteEmptyObject(I = 0);
            //base
            root.Write('Name', S, true);
            root.Write('GuiIcon', fList[WT].GUIIcon);
            ///production data
            root.Write('MinCount', fList[WT].fProduction.MinCount);
            root.Write('MaxCount', fList[WT].fProduction.MaxCount);
            If length(fList[WT].fProduction.Houses) > 0 then
            begin
              root.WriteArray('InHouses');
                for K := 0 to high(fList[WT].fProduction.Houses) do
                begin
                  root.WriteEmptyObject(K = 0);
                    root.Write('MinCount', fList[WT].fProduction.Houses[K].MinCount, true);
                    root.Write('MaxCount', fList[WT].fProduction.Houses[K].MaxCount);

                    If not TKMEnumUtils.GetName<TKMHouseType>(fList[WT].fProduction.Houses[K].House, S) then
                      raise Exception.Create('Error 2');

                    root.Write('HouseType', S);
                  root.EndObject;
                end;
              root.EndArray;
            end;
            //order cost
            If length(fList[WT].OrderCost) > 0 then
            begin
              root.WriteLineArray('OrderCost');
              for K := 0 to High(fList[WT].OrderCost) do
              begin
                If not TKMEnumUtils.GetName<TKMWareType>(fList[WT].OrderCost[K], S) then
                  raise Exception.Create('Error 3');
                root.AddToArray(S, K = 0);
              end;
              root.EndLineArray;
            end;
            //additional
            root.Write('CoinPrice', fList[WT].CoinPrice);
            root.Write('TerrainPics', fList[WT].AtTerrainPic);
          root.EndObject;
        end;
      root.EndArray;
    root.EndFile;
    root.SaveToFile(ChangeFileExt(aPath, '.json'));
  finally
    root.Free;
  end;


end;

function ValidWareTypePair(oWT, dWT: TKMWareType): Boolean;
begin
  Result := (dWT = oWT)
            or (dWT = wtAll)
            or ((dWT = wtWarfare) and (oWT in WARES_WARFARE))
            or ((dWT = wtFood) and (oWT in WARES_FOOD))
            or ((dWT = wtValuable) and (oWT in WARES_VALUABLE));
end;
end.
