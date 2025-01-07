{
Artificial intelligence
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_CityPredictor;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes, Math,
  KromUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes,
  KM_AISetup, KM_ResHouses, KM_ResWares, KM_HandStats, KM_AIParameters,
  KM_ResTypes;

type
  TWareBalance = record
    Production, ActualConsumption, FinalConsumption, Exhaustion, Fraction: Single;
  end;
  // Information about city (it can be replaced by player's stats but it doesnt allow prediction with different values)
  TCityStats = record
    CitizensCnt, WarriorsCnt, HousesCnt: Integer;
    Citizens: array[CITIZEN_MIN..CITIZEN_MAX] of Integer; // Stats sometimes throw negative number in case of Recruit so keep integer
    Warriors: array[WARRIOR_MIN..WARRIOR_MAX] of Integer;
    Houses: array[HOUSE_MIN..HOUSE_MAX] of Integer;
  end;
  THouseBuildHistory = record
    Count: Word;
    Quantity: TKMWordArray;
    Tick: TKMCardinalArray;
  end;
  TWareBalanceArray = array[WARE_MIN..WARE_MAX] of TWareBalance;
  //TWarfareDemands = array[WARFARE_MIN..WARFARE_MAX] of Single;
  TRequiredHousesArray = array[HOUSE_MIN..HOUSE_MAX] of Integer;

  // City predictor (calculation of required houses based on prediction of resource flow)
  TKMCityPredictor = class
  private
    fOwner: TKMHandID;
    fCityStats: TCityStats;
    fCityUnderConstruction, fCityCompleted: Boolean;
    fWorkerCount: Word;
    fMaxGoldMineCnt, fDecCoalMineCnt, fIronMineCnt, fFieldCnt, fBuildCnt: Integer;
    fMaxIronWeapProd, fMaxWoodWeapProd, fMaxSoldiersInMin, fPeaceFactor, fUpdatedPeaceFactor: Single;
    fWareBalance: TWareBalanceArray;
    fFarmBuildHistory: THouseBuildHistory; // Farms are another exception in production (production is delayed and depends on position of each farm)
    fSetup: TKMHandAISetup;

    procedure UpdateWareProduction(aWT: TKMWareType);
    procedure UpdateWareConsumption(aWT: TKMWareType; aInitialization: Boolean = False);
    procedure UpdateFoodConsumption(aInitialization: Boolean = False);
    procedure UpdateGoldConsumption(aInitialization: Boolean = False);
    procedure UpdateBuildMaterialConsumption(aInitialization: Boolean = False);
    procedure UpdateWareDerivation(aWT: TKMWareType; aInitialization: Boolean = False);
    procedure UpdateWareBalance(aInitialization: Boolean = False);

    procedure UpdateBasicHouses(aTick: Cardinal; aInitialization: Boolean = False);
    procedure UpdateFinalProduction(aIncPeaceFactor: Single = 0; aIncrementMines: Boolean = False);
    procedure UpdateCityStats();

    procedure FilterRequiredHouses(aTick: Cardinal);
  public
    RequiredHouses: TRequiredHousesArray;

    constructor Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property UpdatedPeaceFactor: Single read fUpdatedPeaceFactor;
    property CityStats: TCityStats read fCityStats;
    property CityCompleted: Boolean read fCityCompleted;
    property WareBalance: TWareBalanceArray read fWareBalance;
    property WorkerCount: Word read fWorkerCount;
    property MaxGoldMineCnt: Integer read fMaxGoldMineCnt write fMaxGoldMineCnt;
    property IronMineCnt: Integer read fIronMineCnt write fIronMineCnt;
    property FieldCnt: Integer read fFieldCnt write fFieldCnt;
    property BuildCnt: Integer read fBuildCnt write fBuildCnt;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure OwnerUpdate(aPlayer: TKMHandID);
    procedure MarkExhaustedMine(aMineType: TKMHouseType);

  end;

const

  {
  TKMWareType:
  wtNone,
  wtTrunk,   wtStone,   wtWood,        wtIronOre,   wtGoldOre,
  wtCoal,    wtSteel,   wtGold,        wtWine,      wtCorn,
  wtBread,   wtFlour,   wtLeather,     wtSausages,  wtPig,
  wtSkin,    wtShield,  wtMetalShield, wtArmor,     wtMetalArmor,
  wtAxe,     wtSword,   wtPike,        wtHallebard, wtBow,
  wtArbalet, wtHorse,   wtFish,
  wtAll,     wtWarfare, wtFood
  }
  // Array of wares which are produced by specific houses
  PRODUCTION_WARE2HOUSE: array[WARE_MIN..WARE_MAX] of TKMHouseType = (
    htWoodcutters,    htQuarry,         htSawmill,        htIronMine,      htGoldMine,
    htCoalMine,       htIronSmithy,    htMetallurgists,  htVineyard,      htFarm,
    htBakery,         htMill,          htTannery,        htButchers,      htSwine,
    htSwine,          htArmorWorkshop, htArmorSmithy,    htArmorWorkshop, htArmorSmithy,
    htWeaponWorkshop, htWeaponSmithy,  htWeaponWorkshop, htWeaponSmithy,  htWeaponWorkshop,
    htWeaponSmithy,   htStables,       htFishermans,     htHovel, htIronSmithy, htBitinMine,
    htNone,           htNone,          htNone,           htNone,  htNone,       htNone,
    htNone,           htNone,          htPottery,        htFarm, htSawMill, htAppleTree,
    htCollectors,     htNone,          htNone,     htNone,          htNone,     htNone,          htNone,
    htArmorSmithy,    htHovel
  );
  // Array of wares which are consumed in specific amount to achieve specific production (because of exceptions in htWeaponWorkshop / htSwine / htStables etc.)
  CONSUMPTION_RATIO: array[wtTrunk..wtSkin] of array[0..3] of Single = (
    // 1 trunk = 2 Wood                2 wood = 1 axe/bow/spear        1 GoldOre = 2 Gold (+ 1 Coal)
    (0.5, 1, 1, 1),   (1, 1, 1, 1),   (2, 1, 1, 1),   (1, 1, 1, 1),   (0.5, 1, 1, 1),
    // 1 coal = 2 gold (+ 1 gold ore)                                    BEAST_COST = 4 (per a pig / horse)
    (0.5, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (1, 4, 4, 1),
    //               1 flour = 2 bread                                 1 pig = 3 sausages
    (1, 1, 1, 1),   (0.5, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (0.333, 1, 1, 1),
    // 1 skin = 2 leather
    (0.5, 1, 1, 1)
  );
  CONSUMPTION_ORDER: array[0..27] of TKMWareType = ( // Basicaly TKMWareType but sorted by order: resource -> product
    wtStone,   wtTrunk,    wtTimber,
    wtCorn,    wtFlour,    wtBread,     wtWine,        wtFish,
    wtPig,     wtSausage, wtSkin,      wtLeather,     wtHorse,
    wtGoldOre, wtIronOre,  wtCoal,      wtGold,        wtIron,
    wtAxe,     wtBow,      wtLance,      wtLeatherArmor,       wtWoodenShield,
    wtSword,   wtCrossbow,  wtPike, wtIronShield, wtIronArmor
  );
  CO_WEAPONS_MIN = 18;
  CO_WEAPONS_MAX = 27;
    CO_IRON_WEAPONS_MIN = 23;
    CO_IRON_WEAPONS_MAX = 27;
    CO_WOOD_WEAPONS_MIN = 22;
    CO_WOOD_WEAPONS_MAX = 18;
  CO_WARE_MIN = 0;
  CO_WARE_MAX = 17;

implementation
uses
  KM_HandsCollection, KM_Hand, KM_Game, KM_GameParams, KM_CityPlanner;


{ TKMCityPredictor }
constructor TKMCityPredictor.Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);
begin
  inherited Create;
  fOwner := aPlayer;
  fSetup := aSetup;
  fWorkerCount := 0;
  fCityUnderConstruction := False;
  fCityCompleted := False;

  fMaxGoldMineCnt := 0;
  fDecCoalMineCnt := 0;
  fIronMineCnt := 0;
  fFieldCnt := 0;
  fBuildCnt := 0;

  fMaxIronWeapProd := 0;
  fMaxWoodWeapProd := 0;
  fMaxSoldiersInMin := 0;
  fPeaceFactor := 0;
  fUpdatedPeaceFactor := 0;

  with fFarmBuildHistory do
  begin
    Count := 1;
    SetLength(Quantity,1);
    SetLength(Tick,1);
    Quantity[0] := 0;
    Tick[0] := 10 * 60 * 10; // Init delay 10 min
  end;
end;

destructor TKMCityPredictor.Destroy();
begin
  inherited;
end;


procedure TKMCityPredictor.Save(SaveStream: TKMemoryStream);
var
  WT: TKMWareType;
begin
  SaveStream.PlaceMarker('CityPredictor');
  SaveStream.Write(fOwner);
  SaveStream.Write(fWorkerCount);
  SaveStream.Write(fCityUnderConstruction);
  SaveStream.Write(fCityCompleted);


  SaveStream.Write(fMaxGoldMineCnt);
  SaveStream.Write(fDecCoalMineCnt);
  SaveStream.Write(fIronMineCnt);
  SaveStream.Write(fFieldCnt);
  SaveStream.Write(fBuildCnt);

  SaveStream.Write(fMaxIronWeapProd);
  SaveStream.Write(fMaxWoodWeapProd);
  SaveStream.Write(fMaxSoldiersInMin);
  SaveStream.Write(fPeaceFactor);
  SaveStream.Write(fUpdatedPeaceFactor);

  SaveStream.Write(fFarmBuildHistory.Count);
  if (fFarmBuildHistory.Count > 0) then
  begin
    SaveStream.Write(fFarmBuildHistory.Quantity[0], SizeOf(fFarmBuildHistory.Quantity[0]) * fFarmBuildHistory.Count);
    SaveStream.Write(fFarmBuildHistory.Tick[0], SizeOf(fFarmBuildHistory.Tick[0]) * fFarmBuildHistory.Count);
  end;

  // Requred houses should be saved because of public variable
  SaveStream.Write(RequiredHouses, SizeOf(TRequiredHousesArray));
  // fWareBalance must be saved because of FinalConsumption, Trading and building algorithm
  for WT := WARE_MIN to WARE_MAX do
    SaveStream.Write(fWareBalance[WT], SizeOf(TWareBalance));
  // Stats are updated each cycle and doesn't have to be saved
  //fCityStats: TCityStats;
end;


procedure TKMCityPredictor.Load(LoadStream: TKMemoryStream);
var
  WT: TKMWareType;
begin
  LoadStream.CheckMarker('CityPredictor');
  LoadStream.Read(fOwner);
  LoadStream.Read(fWorkerCount);
  LoadStream.Read(fCityUnderConstruction);
  LoadStream.Read(fCityCompleted);

  LoadStream.Read(fMaxGoldMineCnt);
  LoadStream.Read(fDecCoalMineCnt);
  LoadStream.Read(fIronMineCnt);
  LoadStream.Read(fFieldCnt);
  LoadStream.Read(fBuildCnt);

  LoadStream.Read(fMaxIronWeapProd);
  LoadStream.Read(fMaxWoodWeapProd);
  LoadStream.Read(fMaxSoldiersInMin);
  LoadStream.Read(fPeaceFactor);
  LoadStream.Read(fUpdatedPeaceFactor);

  LoadStream.Read(fFarmBuildHistory.Count);
  if (fFarmBuildHistory.Count > 0) then
  begin
    SetLength(fFarmBuildHistory.Quantity, fFarmBuildHistory.Count);
    SetLength(fFarmBuildHistory.Tick, fFarmBuildHistory.Count);
    LoadStream.Read(fFarmBuildHistory.Quantity[0], SizeOf(fFarmBuildHistory.Quantity[0]) * fFarmBuildHistory.Count);
    LoadStream.Read(fFarmBuildHistory.Tick[0], SizeOf(fFarmBuildHistory.Tick[0]) * fFarmBuildHistory.Count);
  end;

  // Requred houses should be saved because of public variable
  LoadStream.Read(RequiredHouses, SizeOf(TRequiredHousesArray));
  // fWareBalance must be saved because of FinalConsumption
  for WT := WARE_MIN to WARE_MAX do
    LoadStream.Read(fWareBalance[WT], SizeOf(TWareBalance));
  // Stats are updated each cycle and doesn't have to be saved
  //fCityStats: TCityStats;
end;


procedure TKMCityPredictor.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
end;


procedure TKMCityPredictor.MarkExhaustedMine(aMineType: TKMHouseType);
begin
  case aMineType of
    htIronMine: Dec(fIronMineCnt);
    htGoldMine: Dec(fMaxGoldMineCnt);
    htCoalMine: Inc(fDecCoalMineCnt);
  end;
  UpdateFinalProduction();
end;


// Update ware production
procedure TKMCityPredictor.UpdateWareProduction(aWT: TKMWareType);
begin
  fWareBalance[aWT].Production := fCityStats.Houses[ PRODUCTION_WARE2HOUSE[aWT] ] * PRODUCTION_RATE[aWT];
end;


// Update ware consumption
procedure TKMCityPredictor.UpdateWareConsumption(aWT: TKMWareType; aInitialization: Boolean = False);
//var
  //I: Integer;
begin
  fWareBalance[aWT].ActualConsumption := 0;
  if aInitialization then
    fWareBalance[aWT].FinalConsumption := 0;
end;


// Update food consumption
procedure TKMCityPredictor.UpdateFoodConsumption(aInitialization: Boolean = False);
const
  CITIZEN_FOOD_COEF = 0.05; // On average citizen needs to eat each 40min but takes 2 food to full status = 1/20 = 0.05
  SOLDIER_FOOD_COEF = 0.025; // On average soldier needs to eat each 40min and takes 1 food to full status = 1/40 = 0.025
var
  Consumption: Single;
begin
  // Get consumption of city + army
  Consumption := (fCityStats.CitizensCnt * CITIZEN_FOOD_COEF) + (fCityStats.WarriorsCnt * SOLDIER_FOOD_COEF);
  // Calculate consumption of leather armor / minute and pigs which are produced with this cycle
  // 2x armor = 2x leather = 1x skin = 1x pig = 3x sausages ... sausages = 3 / 2 * armor = 1.5 * armor
  fWareBalance[wtSausage].ActualConsumption := Min(Consumption, fWareBalance[wtLeatherArmor].FinalConsumption * 1.5);
  // Split rest of consumtion into other houses
  Consumption := Max(0, Consumption - fWareBalance[wtSausage].ActualConsumption);
  fWareBalance[wtBread].ActualConsumption := Consumption * 0.7;
  fWareBalance[wtWine].ActualConsumption := Consumption * 0.3;
  fWareBalance[wtFish].ActualConsumption := 0;
  // Expected food consumption of the final city size (it helps with build order to secure food and weapons production ASAP)
  if aInitialization then
  begin
    fWareBalance[wtSausage].FinalConsumption := fWareBalance[wtSausage].ActualConsumption;
    fWareBalance[wtBread].FinalConsumption := fWareBalance[wtBread].ActualConsumption;
    fWareBalance[wtWine].FinalConsumption := fWareBalance[wtWine].ActualConsumption;
    fWareBalance[wtFish].FinalConsumption := fWareBalance[wtFish].ActualConsumption;
  end;
end;


// Update gold consumption
procedure TKMCityPredictor.UpdateGoldConsumption(aInitialization: Boolean = False);
const
  GOLD_NEED_PER_A_SCHOOL = 3.5; // Amount of gold which requires school (in 1 minute)
begin
  fWareBalance[wtGold].ActualConsumption := Min(fMaxSoldiersInMin, (fCityStats.Houses[htSchool] + RequiredHouses[htSchool]) * GOLD_NEED_PER_A_SCHOOL);
end;


// Update build material consumption
procedure TKMCityPredictor.UpdateBuildMaterialConsumption(aInitialization: Boolean = False);
begin
  // Worker count is decreased after peace time -> compute with maximal count
  fWareBalance[wtStone].ActualConsumption := Min(fCityStats.Citizens[utBuilder] + Round(AI_Par[PREDICTOR_WareNeedPerAWorker_StoneOffset]), fWorkerCount) * AI_Par[PREDICTOR_WareNeedPerAWorker_Stone];
  fWareBalance[wtStone].FinalConsumption := fWareBalance[wtStone].ActualConsumption;
  // Raw wood expectations
  UpdateWareConsumption(wtTimber, aInitialization);
  fWareBalance[wtTimber].ActualConsumption := Max(fWareBalance[wtTimber].ActualConsumption, fCityStats.Citizens[utBuilder] * AI_Par[PREDICTOR_WareNeedPerAWorker_Wood]);
  fWareBalance[wtTimber].FinalConsumption := Max(fWareBalance[wtTimber].FinalConsumption, fWorkerCount * AI_Par[PREDICTOR_WareNeedPerAWorker_Wood]);
end;


// Update ware derivation - 2 views:
// 1. Exhaustion = estimation of time when will be ware depleted (determine which house should be built at first)
// 2. Fraction = fraction of required and available houses
procedure TKMCityPredictor.UpdateWareDerivation(aWT: TKMWareType; aInitialization: Boolean = False);
  function GetDependentWareBalance(): Single;
  begin
    Result := gHands[fOwner].Stats.GetWareBalance(aWT);
    case aWT of
      wtTrunk:   Result := Result + gHands[fOwner].Stats.GetWareBalance(wtTimber) / 2;
      wtIronOre: Result := Result + gHands[fOwner].Stats.GetWareBalance(wtIron);
      //wtGoldOre: Result := Result + gHands[fOwner].Stats.GetWareBalance(wtGold) / 2;
      //wtCorn:    Result := Result + gHands[fOwner].Stats.GetWareBalance(wtGold) / 2;
      wtFlour:   Result := Result + gHands[fOwner].Stats.GetWareBalance(wtBread) / 2;
      wtPig:     Result := Result + gHands[fOwner].Stats.GetWareBalance(wtSausage) / 3;
      wtSkin:    Result := Result + gHands[fOwner].Stats.GetWareBalance(wtLeather) / 2;
      wtBitinOre:    Result := Result + gHands[fOwner].Stats.GetWareBalance(wtGoldOre) / 2;
    end;
  end;
var
  houseReqCnt: Integer;
  HT: TKMHouseType;
begin
  if aWT in [wtBitinOre, wtBitin, wtVegetables] then
    Exit;

  HT := PRODUCTION_WARE2HOUSE[aWT];
  with fWareBalance[aWT] do
  begin
    // Calculate when will be ware depleted
    Exhaustion := 999;
    if (ActualConsumption - Production > 0) then
      Exhaustion := Min( Exhaustion, GetDependentWareBalance() / (ActualConsumption - Production) );
    houseReqCnt := Ceil(( Max(ActualConsumption, FinalConsumption) - Production) / Max(0.0001, PRODUCTION_RATE[aWT]*1.0));
    if (houseReqCnt > 0) AND (Exhaustion >= 950) AND (fCityStats.Houses[ PRODUCTION_WARE2HOUSE[aWT] ] = 0) then
      houseReqCnt := 1
    else
      houseReqCnt := Byte(Exhaustion < 950) * houseReqCnt;
    Fraction := houseReqCnt / Max(1.0,((fCityStats.Houses[HT] + houseReqCnt)*1.0));
  end;
  RequiredHouses[HT] := houseReqCnt;
end;


// Update ware balance
procedure TKMCityPredictor.UpdateWareBalance(aInitialization: Boolean = False);
var
  I: Integer;
begin
  // Update weapons
  for I := CO_WEAPONS_MAX downto CO_WEAPONS_MIN do
  begin
    UpdateWareProduction(CONSUMPTION_ORDER[I]);
    // Final and actual consumptions for weapons are constant
    fWareBalance[ CONSUMPTION_ORDER[I] ].ActualConsumption :=  fWareBalance[ CONSUMPTION_ORDER[I] ].FinalConsumption;
    UpdateWareDerivation(CONSUMPTION_ORDER[I],aInitialization);
  end;

  // Update "Normal" ware flow
  UpdateFoodConsumption(aInitialization);
  UpdateGoldConsumption(aInitialization);
  UpdateBuildMaterialConsumption(aInitialization);
  for I := CO_WARE_MAX downto CO_WARE_MIN do
  begin
    UpdateWareProduction(CONSUMPTION_ORDER[I]);
    // Exeptions
    case CONSUMPTION_ORDER[I] of
      // Food was updated at once
      wtWine, wtBread, wtSausage, wtFish: begin end;
      // Update Gold
      wtGold: begin end;
      // Update materials
      wtStone: begin end;
      wtTimber: begin end;
      // Other cases
      else
        UpdateWareConsumption(CONSUMPTION_ORDER[I], aInitialization);
    end;
    UpdateWareDerivation(CONSUMPTION_ORDER[I],aInitialization);
  end;
end;


// Basic house requirements
procedure TKMCityPredictor.UpdateBasicHouses(aTick: Cardinal; aInitialization: Boolean = False);
const
  INN_TIME_LIMIT = 60 * 10 * 14; // ~ 14 minutes from start
  SCHOOL_PRODUCTION = 3; // Amount of gold which requires school (in 1 minute) - in ideal case it requires only 3.5 in real there is not gold so it must be lower
  FIRST_MARKETPLACE = 10 * 60 * 60;
  SECOND_MARKETPLACE = 10 * 60 * 100;
  BARRACKS_PEACE_DELAY = 30; // Build barracks since 30 min
  BARRACKS_BEFORE_PEACE_END = 20; // Allow to build barracks before peace time end
begin
  // 1 Storehouse
  RequiredHouses[htStore] := 1 - fCityStats.Houses[htStore];
  // 1 Barracks (build only when we have weapons and (from X tick or Y ticks before peace end -> avoid to build barracks in 1 minute when is still peace and we have predefined weapons in storehouse))
  RequiredHouses[htBarracks] := Byte(aInitialization OR ((gHands[fOwner].Stats.GetWareBalance(wtWarfare) > 0) AND ((aTick > BARRACKS_PEACE_DELAY * 600) OR (aTick > (gGame.Options.Peacetime - BARRACKS_BEFORE_PEACE_END) * 600)))) - fCityStats.Houses[htBarracks];
  // Schools (at least 1 + WarriorsPerMinute criterium)
  with gHands[fOwner].AI.CityManagement do
    RequiredHouses[htSchool] := Max(
                                  0,
                                  Max(
                                    1 + Byte( (RequiredUnitsCnt > Round(AI_Par[PREDICTOR_SecondSchool_MinRequiredUnits])) AND not Builder.GoldShortage AND not Builder.StoneShortage AND not Builder.TrunkShortage AND not Builder.WoodShortage),
                                    Byte(  (fCityStats.Houses[htBarracks] > 0) OR aInitialization ) * (Ceil(fMaxSoldiersInMin / SCHOOL_PRODUCTION))
                                  ) - fCityStats.Houses[htSchool]
                                );
  // Inn (at least 1 after INN_TIME_LIMIT + CitizensCnt criterium)
  RequiredHouses[htInn] := Max(
                             0,
                             Ceil(  Byte( (aTick > INN_TIME_LIMIT) OR aInitialization ) * fCityStats.CitizensCnt / 80  ) - fCityStats.Houses[htInn]
                           );
  // Marketplace - 1. after FIRST_MARKETPLACE; 2. after SECOND_MARKETPLACE
  RequiredHouses[htMarket] := Byte( aInitialization OR (aTick > FIRST_MARKETPLACE) ) + Byte( aInitialization OR (aTick > SECOND_MARKETPLACE) ) - fCityStats.Houses[htMarket];
end;


// Get players stats and store them into local variable (to be able to edit them later)
procedure TKMCityPredictor.UpdateCityStats();
var
  constructedHouses: Integer;
  UT: TKMUnitType;
  HT: TKMHouseType;
begin
  with fCityStats do
  begin
    CitizensCnt := 0;
    for UT := Low(Citizens) to High(Citizens) do
    begin
      Citizens[UT] := gHands[fOwner].Stats.GetUnitQty(UT);
      Inc(CitizensCnt, Citizens[UT]);
    end;
    Dec(CitizensCnt, Citizens[utRecruit]); // Count recruits as soldiers
    WarriorsCnt := Citizens[utRecruit];
    for UT := Low(Warriors) to High(Warriors) do
    begin
      Warriors[UT] := gHands[fOwner].Stats.GetUnitQty(UT);
      Inc(WarriorsCnt, Warriors[UT]);
    end;
  end;

  constructedHouses := 0;
  fCityStats.HousesCnt := 0;
  with gHands[fOwner].AI.CityManagement.Builder do
  begin
    for HT := Low(fCityStats.Houses) to High(fCityStats.Houses) do
    begin
      //Houses[HT] := gHands[fOwner].Stats.GetHouseTotal(HT); // Does not consider planned houses
      // Consider only placed, constructed or planned houses (not destroyed houses because plans will remain in CityPlanner)
      fCityStats.Houses[HT] := Planner.PlannedHouses[HT].Completed + Planner.PlannedHouses[HT].UnderConstruction + Planner.PlannedHouses[HT].Planned;
      Inc(fCityStats.HousesCnt, fCityStats.Houses[HT]);
      Inc(constructedHouses, Planner.PlannedHouses[HT].UnderConstruction + Planner.PlannedHouses[HT].Planned);
    end;
    fCityUnderConstruction := (constructedHouses > 0) AND ((FreeWorkerCnt < 10) OR (constructedHouses > 3));
    fCityCompleted := (constructedHouses = 0) AND (FreeWorkerCnt = fCityStats.Citizens[utBuilder]);
  end;
end;


procedure TKMCityPredictor.UpdateFinalProduction(aIncPeaceFactor: Single = 0; aIncrementMines: Boolean = False);
const
  IRON_WARFARE: set of TKMWareType = [wtIronShield, wtIronArmor, wtSword, wtPike, wtCrossbow];
  WOOD_WARFARE: set of TKMWareType = [wtAxe, wtLance, wtBow];
  INV_AFTER_PEACE_SCALING = 1 / (30*10*60); // Peace factor will be completely removed after {30} mins since end of peace
  MIN_IRON_PRODUCTION = 2;
  MAX_IRON_PRODUCTION = 6;
  INV_REQUIRED_TILES_PER_IRON = 1/250;
  MIN_WOOD_PRODUCTION = 2;
  MAX_WOOD_PRODUCTION = 6;
  INV_REQUIRED_TILES_PER_WOOD = 1/450.0;
  TILE_RESERVE = 1000;
  IRON_RESERVE_COEF = 300;
var
  maxIronWeapProd, maxWoodWeapProd, freePlace: Single;
  WT: TKMWareType;
begin
  // Update peace factor
  fUpdatedPeaceFactor := Min(1,fUpdatedPeaceFactor + aIncPeaceFactor);
  // Consider available space around loc
  // Iron weapons - use only fixed peace factor because mines will run out anyway
  freePlace :=
    Max(
      0,
      (Min(fBuildCnt,2000) - TILE_RESERVE) * INV_REQUIRED_TILES_PER_IRON
    );
  maxIronWeapProd :=
    Min(
      Round(freePlace) + MIN_IRON_PRODUCTION,
      Min(
        MAX_IRON_PRODUCTION,
        fIronMineCnt + (gHands[fOwner].Stats.GetWareBalance(wtIronOre) + gHands[fOwner].Stats.GetWareBalance(wtIron)) / IRON_RESERVE_COEF
      )
    );
  // Wooden weapons
  freePlace :=
    Max(
      0,
      (Min(fFieldCnt,3000) - TILE_RESERVE) * INV_REQUIRED_TILES_PER_WOOD
    );
  maxWoodWeapProd :=
    Min(
      MAX_WOOD_PRODUCTION,
      Max( MIN_WOOD_PRODUCTION, Round(freePlace * fUpdatedPeaceFactor) )
    );
  // Consider Iron production
  maxWoodWeapProd :=
    Max(
      MIN_WOOD_PRODUCTION - Byte(maxIronWeapProd > 0),
      Round(maxWoodWeapProd - maxIronWeapProd * (1.0 - fUpdatedPeaceFactor) * 0.5)
    );

  // Iron weapons
  maxIronWeapProd := maxIronWeapProd * PRODUCTION_RATE[wtIronOre] * 0.5; // Division into half because of iron weapon and armor
  for WT in IRON_WARFARE do
    fWareBalance[WT].FinalConsumption := maxIronWeapProd;

  // Wooden weapons
  maxWoodWeapProd := maxWoodWeapProd * PRODUCTION_RATE[wtAxe]; // Production of weapons / armors is ~identical
  for WT in WOOD_WARFARE do
    fWareBalance[WT].FinalConsumption := maxWoodWeapProd;
  // Exceptions
  fWareBalance[wtLeatherArmor].FinalConsumption := maxWoodWeapProd;
  fWareBalance[wtWoodenShield].FinalConsumption := maxWoodWeapProd / 5; // This only affect wood requirements, shields will be ordered by count of axes

  // Soldiers / min (only expected not final value)
  fMaxSoldiersInMin := maxWoodWeapProd + maxIronWeapProd;
  // Maybe there is no need to keep variable fMaxSoldiersInMin but I am afraid what scripters may do with fSetup
  if (fSetup.NewAI AND fSetup.UnlimitedEquip) then
  begin
    fSetup.EquipRateIron := Round(600 / Max(0.01, maxIronWeapProd));
    fSetup.EquipRateLeather := Round(600 / Max(0.01, maxWoodWeapProd));
  end;

  // Predict final city stats (by potential size of city)
  fCityStats.CitizensCnt := Round(  Max( 0, Min(fBuildCnt,4000)-1500 )*0.052+70  ); // Min cnt of citizens is 70 and max 200
  fCityStats.WarriorsCnt := Round(  Max( 0, Min(fBuildCnt,4000)-1500 )*0.042+50  ); // Min cnt of soldiers is 50 and max 150
  UpdateWareBalance(True);

  // Decide count of workers + build nodes
  freePlace := Max(  0, Min( 2000, Min(fFieldCnt,fBuildCnt) - 1000 )  ); // FreePlace in <0,2000>
  fWorkerCount := Round( Min( 50 - 35 * fUpdatedPeaceFactor * Byte(not gGame.IsPeaceTime), // Decrease count of required workers after peace
                              15 + freePlace*AI_Par[PREDICTOR_WorkerCountCoef] + fPeaceFactor*8 )
                       );
  // Try to build mines even when perf. optimalization prohibits it (once in ~8 min)
  if aIncrementMines then
  begin
    //Inc(fMaxGoldMineCnt);
    fDecCoalMineCnt := Max(0,fDecCoalMineCnt - 1);
  end;
end;


// City initialization, estimation of maximal possible production and restriction by peace time and loc properties
procedure TKMCityPredictor.AfterMissionInit;
const
  SCALE_MIN_PEACE_TIME = 50;
  SCALE_MAX_PEACE_TIME = 90;
  SCALE_PEACE_FACTOR = 1.0 / ((SCALE_MAX_PEACE_TIME - SCALE_MIN_PEACE_TIME)*1.0);
begin
  // PeaceFactor: 0 = peace <= SCALE_MIN_PEACE_TIME; 1 = peace >= SCALE_MAX_PEACE_TIME
  //fPeaceFactor := Max(0,
  //                    (Min(SCALE_MAX_PEACE_TIME, gGame.Options.Peacetime) - SCALE_MIN_PEACE_TIME)
  //                   ) * SCALE_PEACE_FACTOR;
  fPeaceFactor := 0; // According to simulations the peace factor only harms

  UpdateFinalProduction();
end;


procedure TKMCityPredictor.FilterRequiredHouses(aTick: Cardinal);

  function UpdateFarmHistory(): Boolean;
  const
    CORN_DELAY = 10 * 60 * 6; // Delay 6 minutes or use array ProductionLag from KM_ResWares
  var
    I, K, cnt: Integer;
  begin
    with fFarmBuildHistory do
    begin
      // Remove old history
      if (Count > 1) then // Keep at least 1 element (the latest)
      begin
        I := 0;
        while (I < Count) do // Find the actual tick
        begin
          if (Tick[I] > aTick) then
            break;
          I := I + 1;
        end;
        if (I > 1) then // Keep the latest older element
        begin
          cnt := 0;
          for K := I - 1 to Count - 1 do // Remove old ticks
          begin
            Quantity[cnt] := Quantity[K];
            Tick[cnt] := Tick[K];
            cnt := cnt + 1;
          end;
          Count := cnt;
        end;
      end;
      cnt := gHands[fOwner].Stats.GetHouseQty(htFarm);
      if (Quantity[Count-1] <> cnt) then
      begin
        if (Length(Quantity) <= Count) then
        begin
          SetLength(Quantity, Length(Quantity) + 5);
          SetLength(Tick, Length(Tick) + 5);
        end;
        Quantity[Count] := cnt;
        Tick[Count] := aTick + CORN_DELAY;
        Count := Count + 1;
      end;
    end;
    Result := + fWareBalance[wtFlour].Production
              + fWareBalance[wtPig].Production * 4
              + fWareBalance[wtHorse].Production * 4
              >=
              + fFarmBuildHistory.Quantity[0] * PRODUCTION_RATE[wtCorn]
              + gHands[fOwner].Stats.GetWareBalance(wtCorn) * 0.25;
  end;
  {
  procedure CheckPeaceFactor();
  const
    IGNORE_HOUSES: set of TKMHouseType = [htCoalMine, htGoldMine, htIronMine, htQuary, htVineyard];
  var
    Cnt: Integer;
    HT: TKMHouseType;
  begin
    if (fUpdatedPeaceFactor < 1) then
    begin
      Cnt := 0;
      for HT := Low(RequiredHouses) to High(RequiredHouses) do
        if not (HT in IGNORE_HOUSES) AND gHands[fOwner].Locks.HouseCanBuild(HT) then
          Inc(Cnt, Max(0,RequiredHouses[HT]));
      if (Cnt >= 0) then
        UpdateFinalProduction(0.1);
    end;
  end;
  //}

const
  WEAP_WORKSHOP_DELAY = 35 * 60 * 10;
  WINEYARD_DELAY = 50 * 60 * 10;
var
  stats: TKMHandStats;
  planner: TKMCityPlanner;
begin
  planner := gHands[fOwner].AI.CityManagement.Builder.Planner;
  stats := gHands[fOwner].Stats;

  // Dont build anything if there is not completed school
  if (fCityStats.Houses[htSchool] = 0)
    OR ( (fCityStats.Houses[htSchool] = 1)
         AND (not (planner.PlannedHouses[htSchool].Plans[0].Placed)
              OR not ( (planner.PlannedHouses[htSchool].Plans[0].House <> nil)
                       AND planner.PlannedHouses[htSchool].Plans[0].House.IsComplete
                     )
             )
       ) then
  begin
    FillChar(RequiredHouses, SizeOf(RequiredHouses), #0);
    // Allow to reserve quarries
    UpdateWareProduction(wtStone);
    fWareBalance[wtStone].ActualConsumption := Min(fCityStats.Citizens[utBuilder]+8, fWorkerCount) * AI_Par[PREDICTOR_WareNeedPerAWorker_Stone];
    fWareBalance[wtStone].FinalConsumption := Max(fCityStats.Citizens[utBuilder], fWorkerCount) * AI_Par[PREDICTOR_WareNeedPerAWorker_Stone];
    UpdateWareDerivation(wtStone);
    RequiredHouses[htSchool] := Max(0, 1 - planner.PlannedHouses[htSchool].Count);
    Exit;
  end;

  // Check requirements
  if not fCityUnderConstruction then
    UpdateFinalProduction(0.1);
    //CheckPeaceFactor();

  // Remove unused houses (iron production)
  if (RequiredHouses[htIronSmithy] < 0) AND (stats.GetWareBalance(wtIronOre) < 20) then // Dont destroy iron production when there is still iron ore
    planner.RemoveHouseType(htIronSmithy);
  if (RequiredHouses[htArmorSmithy] < 0) AND (RequiredHouses[htIronSmithy] = 0) then // And dont destroy following production if you dont want to destroy IronSmithy
    planner.RemoveHouseType(htArmorSmithy);
  if (RequiredHouses[htWeaponSmithy] < 0) AND (RequiredHouses[htIronSmithy] = 0) then
    planner.RemoveHouseType(htWeaponSmithy);


  // Change house requirements due to nonlinear delay, toons of exceptions and unlock order
  // Dont build wineyard too early
  if (fUpdatedPeaceFactor < 1) then
    RequiredHouses[htVineyard] := 0;
  // Consideration of corn delay - only remove all required houses, builder will find the right one if they are not removed
  if UpdateFarmHistory() AND not gHands[fOwner].Locks.HouseBlocked[htFarm] then
  begin
    RequiredHouses[htMill] := Byte(fCityStats.Houses[htMill] = 0); // Allow 1 mill from the start
    RequiredHouses[htSwine] := 0;
    RequiredHouses[htStables] := 0;
  end;
  // Houses in dependence on corn delay
  RequiredHouses[htBakery] := Min(RequiredHouses[htBakery], stats.GetHouseQty(htMill) - fCityStats.Houses[htBakery]);
  RequiredHouses[htButchers] := Min(RequiredHouses[htButchers], Ceil(stats.GetHouseQty(htSwine)/3 - fCityStats.Houses[htButchers]));
  RequiredHouses[htTannery] := Min(RequiredHouses[htTannery], Ceil(stats.GetHouseQty(htSwine)/2 - fCityStats.Houses[htTannery]));
  RequiredHouses[htArmorWorkshop] := Min(RequiredHouses[htArmorWorkshop], stats.GetHouseTotal(htTannery)*2 - fCityStats.Houses[htArmorWorkshop]);
  // Consideration of wood production
  RequiredHouses[htWeaponWorkshop] := RequiredHouses[htWeaponWorkshop] * Byte( (RequiredHouses[htTannery] > 0) OR (WEAP_WORKSHOP_DELAY < aTick) OR (aTick > (gGame.Options.Peacetime-20) * 10 * 60) );

  // Coal mines are used by top priority houses (Metallurgists) and low priority houses (smithy)
  // To get reasonable production there should use something like following logic, good luck with understanding ;)
  {
  RequiredHouses[htCoalMine] := Min( RequiredHouses[htCoalMine],
                                     Max( Planner.PlannedHouses[htMetallurgists].Count, // Gold production requirements
                                          RequiredHouses[htGoldMine]) // Build coal mine in parallel to gold mine
                                     + Stats.GetHouseTotal(htIronSmithy) // Iron production requirements
                                     + Stats.GetHouseTotal(htArmorSmithy)
                                     + Stats.GetHouseTotal(htWeaponSmithy)
                                     - Planner.PlannedHouses[htCoalMine].Count
                                   ); //}
  RequiredHouses[htCoalMine] := Max( planner.PlannedHouses[htMetallurgists].Count,RequiredHouses[htGoldMine]) // Gold production requirements
                                - planner.PlannedHouses[htCoalMine].Count // Current production
                                  // Build coal mine in parallel to gold mine
                                + stats.GetHouseTotal(htIronSmithy) // Iron production requirements
                                + stats.GetHouseTotal(htArmorSmithy)
                                + stats.GetHouseTotal(htWeaponSmithy)
                                + 1; // +1 works better for some reason

  // Consider exhausted city (save procesor time)
  RequiredHouses[htCoalMine] := RequiredHouses[htCoalMine] - fDecCoalMineCnt;
  RequiredHouses[htGoldMine] := Min(RequiredHouses[htGoldMine], fMaxGoldMineCnt);

  // Logical house requirements (delay takes too long so it is not used)
  {
  RequiredHouses[htSwine] := RequiredHouses[htSwine] * Byte(Stats.GetWareBalance(wtCorn) > 0);
  RequiredHouses[htButchers] := RequiredHouses[htButchers] * Byte(Stats.GetWareBalance(wtPig) > 0);
  RequiredHouses[htTannery] := RequiredHouses[htTannery] * Byte(Stats.GetWareBalance(wtLeather) > 0);
  RequiredHouses[htArmorWorkshop] := RequiredHouses[htArmorWorkshop] * Byte(Stats.GetWareBalance(wtSkin) > 0);
  RequiredHouses[htMill] := RequiredHouses[htMill] * Byte(Stats.GetWareBalance(wtFlour) > 0);
  RequiredHouses[htBakery] := RequiredHouses[htBakery] * Byte(Stats.GetWareBalance(wtCorn) > 0);
  //}
  // Iron production (it will give time to build more mines)
  {
  RequiredHouses[htIronSmithy] := RequiredHouses[htIronSmithy] * Byte(Stats.GetWareBalance(wtIronOre) > 0);
  RequiredHouses[htWeaponSmithy] := RequiredHouses[htWeaponSmithy] * Byte(Stats.GetWareBalance(wtSteel) > 0);
  RequiredHouses[htArmorSmithy] := RequiredHouses[htArmorSmithy] * Byte(Stats.GetWareBalance(wtSteel) > 0);
  //}
end;


procedure TKMCityPredictor.UpdateState(aTick: Cardinal);
const
  UPDATE_PRODUCTION = MAX_HANDS * 60 * 5;
begin
  // Update final production (based on size of city, mines etc.)
  if (aTick mod UPDATE_PRODUCTION = fOwner) then
    UpdateFinalProduction(0, True);
  // Clear required houses
  FillChar(RequiredHouses, SizeOf(RequiredHouses), #0);
  // Update city stats
  UpdateCityStats();
  // Update prediction and other houses (production of ware)
  UpdateWareBalance();
  // Update required basic houses (main buildings)
  UpdateBasicHouses(aTick, False);

  // Filter required houses
  FilterRequiredHouses(aTick);
end;


procedure TKMCityPredictor.LogStatus(var aBalanceText: UnicodeString);
const
  COLOR_WHITE = '[$FFFFFF]';
  COLOR_RED = '[$0000FF]';
  COLOR_YELLOW = '[$00FFFF]';
  COLOR_GREEN = '[$00FF00]';
  WARE_TO_STRING: array[WARE_MIN..WARE_MAX] of UnicodeString = (
    'Trunk'#9#9,   'Stone'#9#9,   'Wood'#9#9,        'Iron ore'#9,    'Gold ore'#9,
    'Coal'#9#9,    'Steel'#9#9,   'Gold'#9#9,        'Wine'#9#9,      'Corn'#9#9,
    'Bread'#9#9,   'Flour'#9#9,   'Leather'#9,       'Sausages'#9,    'Pig'#9#9,
    'Skin'#9#9,    'Shield'#9#9,  'MetalShield'#9#9, 'Armor'#9#9,     'MetalArmor'#9#9,
    'Axe'#9#9,     'Sword'#9#9,   'Pike'#9#9,        'Hallebard'#9#9, 'Bow'#9#9,
    'Arbalet'#9#9, 'Horse'#9#9,   'Fish'#9#9,        'Egg'#9#9,       'Bitin Steel'#9#9,
    'Bitin Ore'#9#9,'Stone bolt'#9#9,'Log'#9#9 ,'Steel Element'#9#9,'Bitin Element'#9#9,
    'Wheel'#9#9,   'Ballista Bolt'#9#9,'Quiver'#9#9,  'Water'#9#9,  'Tile'#9#9 ,
    'Seed'#9#9,    'Saw Dust'#9#9, 'Apple'#9#9 , 'JEwerly'#9#9, 'JEwerly'#9#9,
    'Jewerly'#9#9, 'Seed'#9#9,    'Saw Dust'#9#9, 'Apple'#9#9 , 'JEwerly'#9#9,
    'Jewerly'#9#9, 'Eggs'#9#9
  );

  procedure AddWare(aWT: TKMWareType; const aSpecificText: String);
  var
    houseCntColor, productionColor, actualConsumptionColor, finalConsumptionColor, fractionColor, exhaustionColor: UnicodeString;
    cnt: Integer;
  begin
    cnt := RequiredHouses[ PRODUCTION_WARE2HOUSE[aWT] ];
    houseCntColor := COLOR_WHITE;
    if (cnt > 0) then
      houseCntColor := COLOR_RED;
    with fWareBalance[aWT] do
    begin
      productionColor := COLOR_YELLOW;
      actualConsumptionColor := COLOR_YELLOW;
      finalConsumptionColor := COLOR_YELLOW;
      fractionColor := COLOR_YELLOW;
      exhaustionColor := COLOR_RED;

      if (Production > 0) then         productionColor := COLOR_GREEN;
      if (ActualConsumption > 0) then  actualConsumptionColor := COLOR_RED;
      if (FinalConsumption > 0) then   finalConsumptionColor := COLOR_RED;
      if (Fraction <= 0.1) then        fractionColor := COLOR_GREEN
      else                             fractionColor := COLOR_RED;
      if (Exhaustion > 10) then        exhaustionColor := COLOR_GREEN
      else if (Exhaustion > 1) then    exhaustionColor := COLOR_YELLOW;

      aBalanceText := Format('%s'#9 +
                             '%s%dx %s' +
                             '%s'#9 +
                             '%s%5.2f%s'#9#9#9 +
                             '%s%5.2f%s'#9#9#9#9 +
                             '%s%5.2f%s'#9#9#9 +
                             '%s%5.2f%s'#9#9 +
                             '%s%5.2f%s|',
                       [                        aBalanceText,
                        houseCntColor,          cnt,               COLOR_WHITE,
                                                aSpecificText,
                        productionColor,        Production,        COLOR_WHITE,
                        actualConsumptionColor, ActualConsumption, COLOR_WHITE,
                        finalConsumptionColor,  FinalConsumption,  COLOR_WHITE,
                        fractionColor,          Fraction,          COLOR_WHITE,
                        exhaustionColor,        Exhaustion,        COLOR_WHITE
                       ]
                      );
    end;
  end;
var
  K: Integer;
begin
  aBalanceText := Format('%sWare balance|Required houses'#9'Production'#9#9'Actual consumption'#9#9'Final consumption'#9#9'Fraction'#9'Exhaustion|',[aBalanceText]);
  for K := CO_WARE_MIN to CO_WARE_MAX do
    AddWare(CONSUMPTION_ORDER[K], WARE_TO_STRING[ CONSUMPTION_ORDER[K] ]);
  AddWare(wtLeatherArmor, 'Armor'#9#9);
  AddWare(wtAxe, 'Weapon'#9#9);
  AddWare(wtIronArmor, 'Iron Armor'#9);
  AddWare(wtSword, 'Iron Weapon'#9);
  aBalanceText := Format('%sPeace factor = %1.2f (update = %1.2f); Max workers: %d; Max gold = %d; Dec coal = %d; Iron = %d; Field coef = %d; Build coef = %d; |', [aBalanceText, fPeaceFactor, fUpdatedPeaceFactor, fWorkerCount, fMaxGoldMineCnt, fDecCoalMineCnt, fIronMineCnt, fFieldCnt, fBuildCnt]);
end;


end.
