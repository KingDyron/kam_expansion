unit KM_AIMayorBalance;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults,
  KM_ResTypes;

type
  //MayorBalance is a cluster of functions that choose optimal houses to build

  // Calculate various demands and save intermediate numbers in case we need them
  // in determing what exactly to build to satisfy demand the best
  // Production is how much of this resource gets made each minute
  //  - we evaluate each links theoretical production of end resource (f.e. 1 Corn = 3/5 Sausages)
  //  - in chain production the speed is limited by slowest link
  //todo: - resource in reserve adds to each production rate a fraction
  // Consumption is how much gets consumed
  // Balance = Production - Consumption;

  TKMCoreBalance = record
    StoreBalance, CottageBalance, SchoolBalance, InnBalance, BarracksBalance, TowerBalance, MarketBalance,
    TownhallBalance, PalaceBalance, PearlBalance: Single;
    Balance: Single; //Resulting balance
  end;
  TKMMaterialsBalance = record
    WoodcutTheory, SawmillTheory: Single;
    WoodcutReserve, SawmillReserve: Single;
    StoneReserve: Single;
    StoneProduction, WoodProduction: Single;
    StoneBalance, WoodBalance: Single;

    TilesReserve,
    TilesProduction,
    TileBalance: Single;

    FruitsBalance : Single;
    //WaterBalance : Single;

    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceGold = record
    CoalTheory, GoldOreTheory, GoldTheory: Single;
    Production: Single; //How much do we produce
    Consumption: Single; //How much is used
    Reserve: Single; //
    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceFood = record
    Bread: record
      FarmTheory, MillTheory, BakeryTheory: Single;
    end;
    Sausages: record
      FarmTheory, SwineTheory, ButchersTheory: Single;
    end;
    BreadProduction, SausagesProduction, WineProduction, FishProduction: Single;

    Production: Single; //How much food do we produce
    Consumption: Single; //How much food do we use
    Reserve: Single;
    Balance: Single; //Resulting balance
  end;
  TKMWareBalanceWarfare = record
    SteelWeapon: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
      DoBuild : Boolean;
    end;

    SteelArmor: record
      CoalTheory, IronTheory, SteelTheory, SmithyTheory: Single;
      DoBuild : Boolean;
    end;

    WoodWeapon: record
      TrunkTheory, WoodTheory, WorkshopTheory: Single;
      DoBuild : Boolean;
    end;

    LeatherArmor: record
      TrunkTheory, WoodTheory, ArmoryTheory: Single; //Shields
      FarmTheory, SwineTheory, TanneryTheory, TailorsTheory: Single; //Armor
      HovelTheory : Single;//Quivers
      DoBuild : Boolean;
    end;
    Horse: record
      FarmTheory, StablesTheory: Single;
      DoBuild : Boolean;
    end;
    SiegeMachines: record
      CoalTheory, SteelTheory, FoundryTheory, StoneWorkshopTheory, SiegeTheory: Single;
      Balance : Single;
    end;

    //Ratios at which warfare should be produced in workshops/smithies
    OrderRatio: array [WARE_MIN..WARE_MAX] of Single;

    Warfare: array [WARE_MIN..WARE_MAX] of record
      Production, Demand, Balance: Single;
    end;


    Balance: Single; //Resulting balance
  end;

  TWarfareDemands = array [WARE_MIN..WARE_MAX] of Single;

type
  TKMayorBalance = class
  private
    const
      FARM_THEORY_FACTOR = 2.2;
    var
    fOwner: TKMHandID;

    fAdvice: array of TKMHouseType;

    //The following are recalculated before each use, so they don't need saving
    fCore: TKMCoreBalance;
    fMaterials: TKMMaterialsBalance;
    fGold: TKMWareBalanceGold;
    fFood: TKMWareBalanceFood;
    fWarfare: TKMWareBalanceWarfare;

    //For debugging (doesn't need saving)
    fCoreText: UnicodeString;
    fMaterialsText: UnicodeString;
    fGoldText: UnicodeString;
    fFoodText: UnicodeString;
    fWarfareText: UnicodeString;
    fAdviceText: UnicodeString;

    function WeaponUsed(aWare: TKMWareType): Boolean;
    function AdviceContains(aHouse: TKMHouseType): Boolean;

    procedure AppendCore;
    procedure AppendMaterials;
    procedure AppendGold;
    procedure AppendFood;
    procedure AppendWeaponry;

    procedure Append(aHouse: TKMHouseType; aCount: Byte = 1; aFromHouses : TKMHouseTypeSet = []);
    function HouseCount(aHouse: TKMHouseType): Integer; overload;
    function HouseCount(aHouse: TKMHouseTypeArray): Integer;overload;
    function HouseUnlocked(aHouse: TKMHouseType): Boolean;overload;
    function HouseUnlocked(aHouses: TKMHouseTypeArray): Boolean;overload;
    function AllHouseUnlocked(aHouses: TKMHouseTypeArray): Boolean;
    function AIFeatures : Boolean;

    procedure DistributeCorn;
    procedure DistributeCoal;
    procedure DistributeSteel;
    procedure DistributeWood;

    procedure UpdateBalanceGold;
    procedure UpdateBalanceCore;
    procedure UpdateBalanceMaterials;
    procedure UpdateBalanceFood;
    procedure UpdateBalanceLeather;
    procedure UpdateBalanceIron;
    procedure UpdateBalanceWarfare;
  public
    GoldNeed: Single; //How much gold the town needs per minute (may change over time)
    StoneNeed: Single; //How much building materials do we need for city development
    WoodNeed: Single; //How much building materials do we need for city development
    TileNeed: Single;
    constructor Create(aPlayer: TKMHandID);

    procedure OwnerUpdate(aPlayer: TKMHandID);
    procedure Refresh;
    function Peek: TKMHouseType;
    procedure Take;
    procedure Reject;
    procedure SetArmyDemand(aNeeds: TWarfareDemands);
    function BalanceText: UnicodeString;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  SysUtils, StrUtils, Math, KromUtils,
  KM_GameParams,
  KM_Hand, KM_HandsCollection, KM_Resource, KM_ResWares;


{ TKMayorBalance }
constructor TKMayorBalance.Create(aPlayer: TKMHandID);
begin
  inherited Create;
  fOwner := aPlayer;

  GoldNeed := 1;
  StoneNeed := 3.5;
  WoodNeed := 3.5;
  TileNeed := 0.8;
end;


//How many houses of certain type we have (assume all wip houses will be finished)
function TKMayorBalance.HouseCount(aHouse: TKMHouseType): Integer;
begin
  Result := gHands[fOwner].Stats.GetHouseTotal(aHouse);
end;

//How many houses of certain type we have (assume all wip houses will be finished)
function TKMayorBalance.HouseCount(aHouse: TKMHouseTypeArray): Integer;
var HT : TKMHouseType;
begin
  Result := 0;
  for HT in aHouse do
    Inc(Result, HouseCount(HT));
end;

function TKMayorBalance.HouseUnlocked(aHouse: TKMHouseType): Boolean;
begin
  Result := not gHands[fOwner].Locks.HouseBlocked[aHouse];
end;

function TKMayorBalance.HouseUnlocked(aHouses: TKMHouseTypeArray): Boolean;
var HT : TKMHouseType;
begin
  Result := true;
  for HT in aHouses do
    Result := Result or HouseUnlocked(HT);
end;

function TKMayorBalance.AllHouseUnlocked(aHouses: TKMHouseTypeArray): Boolean;
var HT : TKMHouseType;
begin
  Result := true;
  for HT in aHouses do
    Result := Result and HouseUnlocked(HT);
end;

function TKMayorBalance.AIFeatures: Boolean;
begin
  Result := gHands[fOwner].AI.Setup.AIFeatures;
end;


function TKMayorBalance.WeaponUsed(aWare: TKMWareType): Boolean;
begin
  case gHands[fOwner].AI.Setup.ArmyType of
    atLeather:         Result := aWare in [wtWoodenShield, wtPlateArmor, wtLeatherArmor, wtAxe, wtLance, wtBow, wtMace, wtHorse];
    atIron:            Result := aWare in [wtIronShield, wtIronArmor, wtSword, wtPike, wtCrossbow, wtFlail, wtHorse];
    atIronAndLeather:  Result := True;
    atIronThenLeather: Result := True;
    else               Result := False;
  end;
end;


function TKMayorBalance.AdviceContains(aHouse: TKMHouseType): Boolean;
var I: Integer;
begin
  Result := False;
  for I := 0 to Length(fAdvice) - 1 do
    if fAdvice[I] = aHouse then
    begin
      Result := True;
      Exit;
    end;
end;


procedure TKMayorBalance.Append(aHouse: TKMHouseType; aCount: Byte = 1; aFromHouses : TKMHouseTypeSet = []);
var
  I: Integer;
  HT : TKMHouseType;
begin
  //ArmorWorkshop is needed to produce Shields before Tannery is made
  //Handle in generic way since in custom missions it might apply to other houses
  if aHouse in [htAny, htNone] then Exit;
  
  if not gHands[fOwner].Locks.HouseCanBuild(aHouse)
  and not gHands[fOwner].Locks.HouseBlocked[aHouse]
  and (gRes.Houses[aHouse].ReleasedBy <> []) then //Storehouse might be blocked
    for HT in gRes.Houses[aHouse].ReleasedBy do
      If (gHands[fOwner].Stats.GetHouseTotal(HT) = 0) then
        If not (HT in aFromHouses) then
          Append(HT, 1, aFromHouses + [HT]);

  //If the same house is asked for independently then don't add it again, since f.e. gold and iron
  //might both ask for a coal mine, when only 1 extra is needed.
  for I := 0 to Length(fAdvice) - 1 do
    if fAdvice[I] = aHouse then
    begin
      Dec(aCount);
      if aCount = 0 then Exit;
    end;

  SetLength(fAdvice, Length(fAdvice) + aCount);
  for I := Length(fAdvice) - aCount to Length(fAdvice) - 1 do
    fAdvice[I] := aHouse;
end;


//These houses are core for town development
procedure TKMayorBalance.AppendCore;
begin
  with fCore do
  case PickMin([0, StoreBalance, CottageBalance, SchoolBalance, InnBalance, BarracksBalance, TowerBalance, MarketBalance, TownhallBalance, PalaceBalance, PearlBalance]) of
    0: ;
    1: Append(htStore);
    2:  if gHands[fOwner].Locks.HouseCanBuild(htHouse) then
          Append(htHouse)
        else
          Append(htCottage);
    3: Append(htSchool);
    4: Append(htInn);
    5: Append(htBarracks);
    6: begin Append(htWatchTower); Append(htWallTower); end;
    7: Append(htMarket);
    8: Append(htTownhall);
    9: Append(htPalace);
    10: Append(htpearl);
  end;
end;


procedure TKMayorBalance.AppendMaterials;
var
  List: array [0..5] of Single;
begin
  List[0] := 0;
  List[1] := fMaterials.StoneBalance;
  List[2] := fMaterials.WoodcutTheory - WoodNeed;
  List[3] := fMaterials.SawmillTheory - WoodNeed;
  List[4] := fMaterials.TileBalance - TileNeed;
  List[5] := fMaterials.FruitsBalance;
  //List[6] := fMaterials.WaterBalance;

  repeat
    //Do not build extra houses if we are low on building materials
    if (gHands[fOwner].Stats.GetHouseQty(htQuarry) = 0)
    and (gHands[fOwner].Stats.GetWareBalance(wtStone)
        - gHands[fOwner].Constructions.FieldworksList.FieldCount(ftRoad) < 40)
    and (AdviceContains(htQuarry) or (gHands[fOwner].Stats.GetHouseWip(htQuarry) > 1)) then
      Break;

    case PickMin(List{[0, List[0], List[1], List[2], List[3]]}) of
      0:  Break;
      1:  begin
            Append(htQuarry);
            List[1] := List[1] + PRODUCTION_RATE[wtStone];
          end;
      2:  begin
            Append(htWoodcutters);
            List[2] := List[2] + PRODUCTION_RATE[wtTrunk] * 2; //Each trunk brings 2 wood
          end;
      3:  begin
            Append(htSawmill);
            List[3] := List[3] + PRODUCTION_RATE[wtTimber];
          end;
      4:  begin
            Append(htPottery);
            List[4] := List[4] + PRODUCTION_RATE[wtTile];
          end;
      5:  begin
            Append(htAppleTree);
            List[5] := List[5] + PRODUCTION_RATE[wtApple];
          end;
      {6:  begin
            Append(htWell);
            List[6] := List[6] + PRODUCTION_RATE[wtWater];
          end;}
    end;

{  if (fPlayers[fOwner].Stats.GetHouseQty(htQuary) = 0)
  and (fPlayers[fOwner].Stats.GetWareBalance(wtStone) < 40) then
  while not (Result in [htNone, htSchool, htQuary]) do
  begin
    Take;
    Result := Peek;
  end;}
  
  until False;
end;


//Increase Gold production
procedure TKMayorBalance.AppendGold;
begin
  //If all 3 shares 0 we whould pick in that order Gold > Coal > Metallurgists
  with fGold do
  if Balance < 0 then
  case PickMin([GoldOreTheory, CoalTheory, GoldTheory]) of
    0:  Append(htGoldMine);
    1:  Append(htCoalMine, 2);
    2:  Append(htMetallurgists);
  end;
end;


procedure TKMayorBalance.AppendFood;
var SkipSausage: Boolean;
begin
  //When making iron only don't make sausages, they're inefficient if you don't use skins
  SkipSausage := gHands[fOwner].AI.Setup.ArmyType = atIron;
  
  //Pick smallest production and increase it
  //If all 3 shares 0 we whould pick Sausages first to ensure Leather supply
  with fFood do
  if Balance < 0 then
  case PickMin([SausagesProduction + 10000*Byte(SkipSausage), BreadProduction, WineProduction, FishProduction]) of
    0:  with Sausages do
        case PickMin([FarmTheory, SwineTheory, ButchersTheory]) of
          0:  Append(htFarm, 2);
          1:  Append(htSwine);
          2:  Append(htButchers);
        end;
    1:  with Bread do
        case PickMin([FarmTheory, MillTheory, BakeryTheory]) of
          0:  Append(htFarm, 2);
          1:  Append(htMill);
          2:  Append(htBakery);
        end;
    2:  Append(htVineyard, 2);
    3:  Append(htFishermans, 1);
  end;
end;


procedure TKMayorBalance.AppendWeaponry;
const
  MAX_WEAPON_TYPES = Byte(WARFARE_MAX) - Byte(WARFARE_MIN) + 1;
var
  I, TmpWare: TKMWareType;
  Tmp: Single;
  WeaponsCount, J, K: Integer;
  Weapons: array of TKMWareType;
  WeaponSatisfactions: array of Single;
begin
  //List all the required weapons
  J := 0;
  K := 0;
  for I in WARES_WARFARE do
    inc(K);
  SetLength(Weapons, K);
  SetLength(WeaponSatisfactions, K);
  for I in WARES_WARFARE do
    if WeaponUsed(I) and (fWarfare.Warfare[I].Demand > 0)
    and (fWarfare.Warfare[I].Production < fWarfare.Warfare[I].Demand) then
    begin
      Weapons[J] := I;
      //Calculate weapon production satisfaction rate (0..1)
      WeaponSatisfactions[J] := fWarfare.Warfare[I].Production / Max(0.01,fWarfare.Warfare[I].Demand);
      Inc(J);
    end;
  WeaponsCount := J;

  //Sort (since there's not many items bubble sort is okay for now)
  for J := WeaponsCount-1 downto 0 do
    for K := WeaponsCount-1 downto 0 do
      if WeaponSatisfactions[J] > WeaponSatisfactions[K] then
      begin
        Tmp := WeaponSatisfactions[J];
        WeaponSatisfactions[J] := WeaponSatisfactions[K];
        WeaponSatisfactions[K] := Tmp;

        TmpWare := Weapons[J];
        Weapons[J] := Weapons[K];
        Weapons[K] := TmpWare;
      end;

  for J := 0 to WeaponsCount-1 do
  case Weapons[J] of
    wtIronShield,
    wtIronArmor:  with fWarfare.SteelArmor do
                    If DoBuild then
                      case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
                        0: Append(htCoalMine);
                        1: begin Append(htIronMine); Append(htBitinMine); end;
                        2: Append(htIronSmithy);
                        3: Append(htArmorSmithy);
                      end;
    wtFlail,
    wtSword,
    wtPike,
    wtCrossbow:     with fWarfare.SteelWeapon do
                      If DoBuild then
                        case PickMin([CoalTheory, IronTheory, SteelTheory, SmithyTheory]) of
                          0: Append(htCoalMine);
                          1: begin Append(htIronMine); Append(htBitinMine); end;
                          2: Append(htIronSmithy);
                          3: Append(htWeaponSmithy);
                        end;
    wtWoodenShield: with fWarfare.LeatherArmor do
                      If DoBuild then
                        case PickMin([TrunkTheory, WoodTheory, ArmoryTheory*fWarfare.OrderRatio[wtWoodenShield]]) of
                          0: Append(htWoodcutters);
                          1: Append(htSawmill);
                          2: Append(htArmorWorkshop);
                        end;
    wtQuiver,
    wtLeatherArmor: with fWarfare.LeatherArmor do
                      If DoBuild then
                        case PickMin([FarmTheory, SwineTheory, TanneryTheory, TailorsTheory, HovelTheory]) of
                          0: Append(htFarm);
                          1: Append(htSwine);
                          2: Append(htTannery);
                          3: Append(htTailorsShop);
                          4: Append(htHovel);
                        end;
    wtMace,
    wtAxe,
    wtLance,
    wtBow:         with fWarfare.WoodWeapon do
                    If DoBuild then
                      case PickMin([TrunkTheory, WoodTheory, WorkshopTheory]) of
                        0: Append(htWoodcutters);
                        1: Append(htSawmill);
                        2: Append(htWeaponWorkshop);
                      end;
    wtHorse:       with fWarfare.Horse do
                    If DoBuild then
                      case PickMin([FarmTheory, StablesTheory]) of
                        0: Append(htFarm, 2);
                        1: Append(htStables);
                      end;
  end;

  with fWarfare.SiegeMachines do
    If Balance < 0 then
    case PickMin([CoalTheory, SteelTheory, FoundryTheory, StoneWorkshopTheory, SiegeTheory]) of
      0: Append(htCoalMine, 2);
      1: Append(htIronSmithy);
      2: Append(htIronFoundry);
      3: Append(htStoneWorkshop);
      4: Append(htSiegeWorkshop);
    end;

end;


procedure TKMayorBalance.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
end;


procedure TKMayorBalance.DistributeCoal;
var
  CoalProductionRate, CoalConsumptionRate, CoalReserve: Single;
  GoldPerMin, SteelPerMin, WeaponsPerMin, ArmorPerMin, SiegePerMin: Single;
  ExtraCoal, DeficitCoal: Single;
begin
  with fWarfare do
  begin
    //Theoretical gold production
    GoldPerMin := Min(HouseCount(htGoldMine) * PRODUCTION_RATE[wtGoldOre] * 2,
                      HouseCount(htMetallurgists) * PRODUCTION_RATE[wtGold]);

    //Theoretical steel production
    SteelPerMin := Min(HouseCount(htIronMine) * PRODUCTION_RATE[wtIronOre],
                       HouseCount(htIronSmithy) * PRODUCTION_RATE[wtIron],
                       HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE] * 2);

    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(htWeaponSmithy) * PRODUCTION_RATE[wtSword],
                         Warfare[wtSword].Demand
                         + Warfare[wtPike].Demand
                         + Warfare[wtCrossbow].Demand
                         + Warfare[wtFlail].Demand);

    //Theoretical armor production
    ArmorPerMin := Min(HouseCount(htArmorSmithy) * PRODUCTION_RATE[wtIronArmor],
                       Warfare[wtIronShield].Demand + Warfare[wtIronArmor].Demand);

    SiegePerMin := Min(HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE],
                      Warfare[wtSteelE].Demand / 3);

    //Current coal consumption
    CoalConsumptionRate := GoldPerMin/2 + SteelPerMin + WeaponsPerMin + ArmorPerMin + SiegePerMin;
  end;
  CoalReserve := gHands[fOwner].Stats.GetWareBalance(wtCoal) / Max(1,CoalConsumptionRate);
  CoalProductionRate := HouseCount(htCoalMine) * PRODUCTION_RATE[wtCoal] + Max(CoalReserve - 30, 0);

  if CoalProductionRate >= CoalConsumptionRate then
  begin
    //Let every industry think the extra belongs to it
    ExtraCoal := CoalProductionRate - CoalConsumptionRate;
    fGold.CoalTheory := GoldPerMin + ExtraCoal * 2;
    fWarfare.SteelWeapon.CoalTheory := WeaponsPerMin + ExtraCoal / 2; //Takes 2 coal to make each weapon
    fWarfare.SteelArmor.CoalTheory := ArmorPerMin + ExtraCoal / 2; //Takes 2 coal to make each armor
    fWarfare.SiegeMachines.CoalTheory := SiegePerMin + ExtraCoal; //Takes 1 coal to make 2 SteelE
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    DeficitCoal := CoalConsumptionRate - CoalProductionRate;
    fGold.CoalTheory := Max(0, GoldPerMin - DeficitCoal * 2); //Each coal makes 2 gold
    fWarfare.SteelWeapon.CoalTheory := Max(0, WeaponsPerMin - DeficitCoal);
    fWarfare.SteelArmor.CoalTheory := Max(0, ArmorPerMin - DeficitCoal);
    fWarfare.SiegeMachines.CoalTheory := Max(0, SiegePerMin - DeficitCoal);
  end;
end;


procedure TKMayorBalance.DistributeSteel;
var
  WeaponsPerMin, ArmorPerMin, SiegePerMin: Single;

  SteelPerMin, SteelConsumptionRate: Single;
  ExtraSteel, DeficitSteel: Single;

  IronPerMin, IronProduction, IronConsumption, IronReserve: Single;
  ExtraIron, RateIron: Single;
begin
  SteelPerMin := HouseCount(htIronSmithy) * PRODUCTION_RATE[wtIron];
  IronPerMin := HouseCount(htIronMine) * PRODUCTION_RATE[wtIronOre];

  with fWarfare do
  begin
    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(htWeaponSmithy) * PRODUCTION_RATE[wtSword],
                      Warfare[wtSword].Demand
                      + Warfare[wtPike].Demand
                      + Warfare[wtCrossbow].Demand
                      + Warfare[wtFlail].Demand);

    //Theoretical armor production
    ArmorPerMin := Min(HouseCount(htArmorSmithy) * PRODUCTION_RATE[wtIronArmor],
                      Warfare[wtIronArmor].Demand
                      + Warfare[wtIronShield].Demand);
    SiegePerMin := Min(HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE],
                      Warfare[wtSteelE].Demand);
  end;

  //Current steel consumption
  SteelConsumptionRate := WeaponsPerMin + ArmorPerMin + SiegePerMin;

  if SteelPerMin >= SteelConsumptionRate then
  begin
    //Let every industry think the extra belongs to it
    ExtraSteel := SteelPerMin - SteelConsumptionRate;
    fWarfare.SteelWeapon.SteelTheory := WeaponsPerMin + ExtraSteel;
    fWarfare.SteelArmor.SteelTheory := ArmorPerMin + ExtraSteel;
    fWarfare.SiegeMachines.SteelTheory := Max(0, SiegePerMin + ExtraSteel);
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    DeficitSteel := SteelConsumptionRate - SteelPerMin;
    fWarfare.SteelWeapon.SteelTheory := Max(0, WeaponsPerMin - DeficitSteel);
    fWarfare.SteelArmor.SteelTheory := Max(0, ArmorPerMin - DeficitSteel);
    fWarfare.SiegeMachines.SteelTheory := Max(0, SiegePerMin - DeficitSteel);
  end;

  IronConsumption := SteelConsumptionRate; //Any not being used for steel is excess
  IronReserve := gHands[fOwner].Stats.GetWareBalance(wtIronOre) / Max(1,IronConsumption);
  IronProduction := IronPerMin + Max(IronReserve - 30, 0);

  if IronProduction >= IronConsumption then
  begin
    //Let every industry think the extra belongs to it
    ExtraIron := IronProduction - IronConsumption;
    fWarfare.SteelWeapon.IronTheory := WeaponsPerMin + ExtraIron;
    fWarfare.SteelArmor.IronTheory := ArmorPerMin + ExtraIron;
  end
  else
  begin
    //Share proportionaly
    RateIron := IronProduction / Max(1,IronConsumption);
    fWarfare.SteelWeapon.IronTheory := RateIron * IronPerMin;
    fWarfare.SteelArmor.IronTheory := RateIron * IronPerMin;
  end;
end;


procedure TKMayorBalance.DistributeWood;
const
  WEAP_COST = 2;
  WT = 2; //Wood from Trunk ratio
var
  TrunkPerMin, WoodPerMin: Single;
  WeaponsPerMin, ShieldsPerMin: Single;
  WoodProduction, WoodConsumption, WoodReserve: Single;
  TrunkProduction, TrunkConsumption, TrunkReserve: Single;
  ExtraWood, DeficitWood: Single;
  ExtraTrunk, DeficitTrunk: Single;
begin
  //Theoretical limit on Wood production
  TrunkPerMin := HouseCount(htWoodcutters) * PRODUCTION_RATE[wtTrunk];
  WoodPerMin := HouseCount(htSawmill) * PRODUCTION_RATE[wtTimber];

  with fWarfare do
  begin
    //Theoretical weapon production
    WeaponsPerMin := Min(HouseCount(htWeaponWorkshop) * PRODUCTION_RATE[wtAxe],
                      Warfare[wtAxe].Demand
                      + Warfare[wtLance].Demand
                      + Warfare[wtBow].Demand);

    //Min from available production (shields are only part of workshops orders) and demand
    ShieldsPerMin := Min(HouseCount(htArmorWorkshop) * OrderRatio[wtWoodenShield] * PRODUCTION_RATE[wtWoodenShield],
                         Warfare[wtWoodenShield].Demand);

    //Current wood consumption
    WoodConsumption := WoodNeed + ShieldsPerMin + WeaponsPerMin * WEAP_COST;
    WoodReserve := gHands[fOwner].Stats.GetWareBalance(wtTimber) / Max(1,WoodConsumption);
    WoodProduction := WoodPerMin + Max(WoodReserve - 30, 0);

    //Wood shares
    if WoodProduction >= WoodConsumption then
    begin
      //Let every industry think the extra belongs to it
      ExtraWood := WoodProduction - WoodConsumption;
      WoodWeapon.WoodTheory := WeaponsPerMin + ExtraWood / Max(1,WEAP_COST);
      LeatherArmor.WoodTheory := ShieldsPerMin + ExtraWood;
    end
    else
    begin
      //Sharing proportionaly doesn't work since closer houses get more.
      //Let every industry think the deficit belongs to it
      if (WoodConsumption > 0) and (WoodProduction > WoodNeed) then
      begin
        DeficitWood := WoodConsumption - WoodProduction;
        WoodWeapon.WoodTheory := Max(0, WeaponsPerMin - DeficitWood / Max(1,WEAP_COST));
        LeatherArmor.WoodTheory := Max(0, ShieldsPerMin - DeficitWood);
      end
      else
      begin
        WoodWeapon.WoodTheory := 0;
        LeatherArmor.WoodTheory := 0;
      end;
    end;

    TrunkConsumption := WoodPerMin / Max(1,WT);
    TrunkReserve := gHands[fOwner].Stats.GetWareBalance(wtTrunk) / Max(1,TrunkConsumption);
    TrunkProduction := TrunkPerMin + Max(TrunkReserve - 30, 0);

    //Trunk shares
    if TrunkProduction >= TrunkConsumption then
    begin
      //Let every industry think the extra belongs to it
      ExtraTrunk := TrunkProduction - TrunkConsumption;
      WoodWeapon.TrunkTheory := WeaponsPerMin + ExtraTrunk * WT / Max(1,WEAP_COST);
      LeatherArmor.TrunkTheory := ShieldsPerMin + ExtraTrunk * WT;
    end
    else
    begin
      //Sharing proportionaly doesn't work since closer houses get more.
      //Let every industry think the deficit belongs to it
      if TrunkConsumption <> 0 then
      begin
        DeficitTrunk := TrunkConsumption - TrunkProduction;
        WoodWeapon.TrunkTheory := Max(0, WeaponsPerMin - DeficitTrunk * WT / Max(1,WEAP_COST));
        LeatherArmor.TrunkTheory := Max(0, ShieldsPerMin - DeficitTrunk * WT);
      end
      else
      begin
        WoodWeapon.TrunkTheory := 0;
        LeatherArmor.TrunkTheory := 0;
      end;
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceGold;
var GoldOreReserve: Single;
begin
  with fGold do
  begin
    //How much Gold do we need
    Consumption := GoldNeed + Byte(HouseCount(htBarracks) > 0) * gHands[fOwner].AI.Setup.WarriorsPerMinute
                   + Byte(HouseCount(htTownhall) > 0) * gHands[fOwner].AI.Setup.WarriorsPerMinute;

    GoldOreReserve := gHands[fOwner].Stats.GetWareBalance(wtGoldOre) / Max(1,(2 * Consumption));

    //How much gold in theory we could get
    //CoalTheory - coal calculated separately
    GoldOreTheory := HouseCount(htGoldMine) * PRODUCTION_RATE[wtGoldOre] * 2; //*2 since every Ore becomes 2 Gold
    GoldOreTheory := GoldOreTheory + Max(0, GoldOreReserve - 40);
    GoldTheory := HouseCount(htMetallurgists) * PRODUCTION_RATE[wtGold];

    //Actual production is minimum of the above
    Production := Min(CoalTheory, GoldOreTheory, GoldTheory);

    //How much reserve do we have
    Reserve := gHands[fOwner].Stats.GetWareBalance(wtGold) / Max(1,Consumption);

    Balance := Production - Consumption + Max(Reserve - 40, 0);

    fGoldText := Format('%.2f Gold (%.2f - %.2f + %.2f)', [Balance, Production, Consumption, Reserve]);
  end;
end;


procedure TKMayorBalance.UpdateBalanceLeather;
begin
  with fWarfare do
  begin
    with WoodWeapon do
    begin
      //Trunk
      //Wood
      //All 3 produced at the same speed
      WorkshopTheory := HouseCount(htWeaponWorkshop) * PRODUCTION_RATE[wtLance];
       DoBuild := AllHouseUnlocked([htBarracks, htWeaponWorkshop]);

      fWarfare.Warfare[wtAxe].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * OrderRatio[wtAxe];
      fWarfare.Warfare[wtLance].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * OrderRatio[wtLance];
      fWarfare.Warfare[wtBow].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * OrderRatio[wtBow];
      fWarfare.Warfare[wtMace].Production := Min(TrunkTheory, WoodTheory, WorkshopTheory) * OrderRatio[wtMace];
    end;

    with LeatherArmor do
    begin
      //Trunk
      //Wood
      //FarmTheory calculated above
      SwineTheory := HouseCount(htSwine) * PRODUCTION_RATE[wtSkin] * 2;
      TanneryTheory := HouseCount(htTannery) * PRODUCTION_RATE[wtLeather];

      ArmoryTheory := (HouseCount(htArmorWorkshop) * 2) * PRODUCTION_RATE[wtWoodenShield];
      HovelTheory  := HouseCount(htHovel) * PRODUCTION_RATE[wtFeathers]; //feathers needed for quivers
      TailorsTheory := HouseCount(htTailorsShop) * PRODUCTION_RATE[wtLeatherArmor] / 2;

      DoBuild := HouseUnlocked(htBarracks) and HouseUnlocked([htArmorWorkshop, htTailorsShop]);

      Warfare[wtWoodenShield].Production := Min(TrunkTheory, WoodTheory, ArmoryTheory * OrderRatio[wtWoodenShield]);
      Warfare[wtLeatherArmor].Production := Min(Min(FarmTheory, SwineTheory), Min(TanneryTheory, TailorsTheory * OrderRatio[wtLeatherArmor]));
      Warfare[wtQuiver].Production := Min(Min(FarmTheory, SwineTheory), Min(TanneryTheory, TailorsTheory * OrderRatio[wtQuiver]));
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceIron;
begin
  with fWarfare do
  begin
    //Weapon
    //Calculate how much Weapon each link could possibly produce
    with SteelWeapon do
    begin
      //Coal/steel/iron calculated above
      SmithyTheory := HouseCount(htWeaponSmithy) * PRODUCTION_RATE[wtPike];
      DoBuild := HouseUnlocked(htBarracks) and HouseUnlocked([htArmorSmithy, htWeaponSmithy]);

      //All 3 weapons are the same for now
      Warfare[wtSword].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtSword];
      Warfare[wtPike].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtPike];
      Warfare[wtCrossbow].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtCrossbow];
      Warfare[wtFlail].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtFlail];
    end;

    //Armor
    //Calculate how many Armor each link could possibly produce
    with SteelArmor do
    begin
      //Coal/steel/iron calculated above
      SmithyTheory := HouseCount(htArmorSmithy) * PRODUCTION_RATE[wtIronArmor];
      DoBuild := HouseUnlocked(htBarracks) and HouseUnlocked([htArmorSmithy, htWeaponSmithy]);

      Warfare[wtIronShield].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtIronShield];
      Warfare[wtIronArmor].Production := Min(Min(CoalTheory, IronTheory), Min(SteelTheory, SmithyTheory)) * OrderRatio[wtIronArmor];
    end;
  end;
end;


procedure TKMayorBalance.UpdateBalanceWarfare;
var
  I: TKMWareType;
  S: UnicodeString;
  SiegeNeeded : Single;
begin
  UpdateBalanceLeather;
  UpdateBalanceIron;

  with fWarfare do
  begin
    //Horse.FarmTheory calculated above
    Horse.StablesTheory := HouseCount(htStables) * PRODUCTION_RATE[wtHorse];

    Horse.DoBuild := AllHouseUnlocked([htStables, htBarracks])
                     and (AllHouseUnlocked([htArmorWorkshop, htTailorsShop, htWeaponWorkshop])
                          or AllHouseUnlocked([htArmorSmithy, htWeaponSmithy]) );

    Warfare[wtHorse].Production := Min(Horse.FarmTheory, Horse.StablesTheory);
    //stone workshop creates 2 different wares for machines
    SiegeNeeded := (gHands[fOwner].AI.Mayor.ArmyDemand[gtMachines] + gHands[fOwner].AI.Mayor.ArmyDemand[gtMachinesMelee]);
    SiegeMachines.SteelTheory := SiegeMachines.SteelTheory - (HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE]);
    SiegeMachines.CoalTheory := SiegeMachines.CoalTheory - (HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE]);
    SiegeMachines.StoneWorkshopTheory := HouseCount(htStoneWorkshop) * PRODUCTION_RATE[wtLog] - SiegeNeeded - HouseCount(htPearl);


    SiegeMachines.FoundryTheory := HouseCount(htIronFoundry) * PRODUCTION_RATE[wtSteelE] - SiegeNeeded - HouseCount(htPearl);

    SiegeMachines.SiegeTheory := HouseCount(htSiegeWorkshop) - SiegeNeeded ;
    //for every 20 machines we need 1 siege workshop
    SiegeMachines.Balance := Min([SiegeMachines.CoalTheory, SiegeMachines.SteelTheory, SiegeMachines.SiegeTheory, SiegeMachines.FoundryTheory, SiegeMachines.StoneWorkshopTheory]);

    for I := WARFARE_MIN to WARFARE_MAX do
      Warfare[I].Balance := Warfare[I].Production - Warfare[I].Demand;

    //Set Weaponry balance to the most required warfare kind
    Balance := MaxSingle;
    for I := WARFARE_MIN to WARFARE_MAX do
    if WeaponUsed(I) and (Warfare[I].Balance < Balance) then
      Balance := Warfare[I].Balance;

    S := Format('%.2f Weaponry: |', [Balance]);
    with fWarfare.LeatherArmor do
    begin
      S := S + Format('WoodShields: T%.1f : W%.1f : W%.1f|', [TrunkTheory, WoodTheory, ArmoryTheory*OrderRatio[wtWoodenShield]]);
      S := S + Format('LeatherArm: F%.1f : S%.1f : T%.1f : W%.1f|', [FarmTheory, SwineTheory, TanneryTheory, TailorsTheory*OrderRatio[wtLeatherArmor]]);
    end;

    with fWarfare.WoodWeapon do
      S := S + Format('WoodWeap: T%.1f W%.1f W%.1f|', [TrunkTheory, WoodTheory, WorkshopTheory]);

    with fWarfare.SteelArmor do
      S := S + Format('SteelArmor: C%.1f : I%.1f : S%.1f : S%.1f|', [CoalTheory, IronTheory, SteelTheory, SmithyTheory]);

    with fWarfare.SteelWeapon do
      S := S + Format('SteelWeapon: C%.1f : I%.1f : S%.1f : S%.1f|', [CoalTheory, IronTheory, SteelTheory, SmithyTheory]);

    for I := WARFARE_MIN to WARFARE_MAX do
    if WeaponUsed(I) then
      S := S + Format('%s: %.2f - %.2f|', [gRes.Wares[I].Title, Warfare[I].Production, Warfare[I].Demand]);

    fWarfareText := S;
  end;
end;


procedure TKMayorBalance.UpdateBalanceCore;
var
  P: TKMHand;
begin
  P := gHands[fOwner];

  with fCore do
  begin
    //Balance = Available - Required + Reserve
    StoreBalance    := HouseCount(htStore)       - 1 - (HouseCount(htAny) * 0.01 - 1);
    //Build 2 schools if we need to equip a lot of warriors per minute
    SchoolBalance   := HouseCount(htSchool)      - 1 - Byte((gHands[fOwner].Stats.GetHouseQty(htBarracks) > 0) and (gHands[fOwner].AI.Setup.WarriorsPerMinute > 2));

    InnBalance      := HouseCount(htInn) - (P.Stats.GetCitizensCount / 140) + (byte(gGameParams.Tick < 15 * 60 * 10) * 100);  //build inn after 15 minutes

    BarracksBalance := HouseCount(htBarracks)    - Byte(P.Stats.GetWarfareProduced > 0);
    TowerBalance    := HouseCount(htWallTower) + HouseCount(htWatchTower)  - 1 * gHands[fOwner].Stats.GetHouseQty(htBarracks);

    //If there are more schools then we need more cottage
    //must be at least one cottage
    CottageBalance  := (HouseCount(htCottage) + HouseCount(htHouse) * 2) - (HouseCount(htSchool)) - 1;

    If not AIFeatures then
      MarketBalance := 999999
    else
      MarketBalance  := HouseCount(htMarket) - HouseCount(htAny) / 85;

    TownhallBalance := HouseCount(htTownhall) - Min(1, HouseCount(htMetallurgists) / 2);
    //needs atleast 85 buildings to build a palace
    PalaceBalance := HouseCount(htPalace) * 10000 - HouseCount(htAny) / 85 + 1;
    //needs atleast 100 houses to build pearl
    If not AIFeatures then
      PearlBalance := 999999
    else
      PearlBalance := HouseCount(htPearl) * 10000 - HouseCount(htAny) / 100 + 1;

    Balance := Min([StoreBalance, SchoolBalance, InnBalance, BarracksBalance, TowerBalance, CottageBalance, MarketBalance, TownhallBalance, PalaceBalance, PearlBalance]);
    fCoreText := Format
      ('%.2f Core: (Store %.2f, Cottage/House %.2f, School %.2f, Inn %.2f, Barracks %.2f, Tower %.2f)',
      [Balance, StoreBalance, CottageBalance, SchoolBalance, InnBalance, BarracksBalance, TowerBalance]);
  end;
end;


procedure TKMayorBalance.UpdateBalanceMaterials;
begin
  with fMaterials do
  begin
    //In some maps there is no stone so quarry is blocked
    if gHands[fOwner].Locks.HouseBlocked[htQuarry] then
      StoneProduction := 99999
    else
    begin
      StoneReserve := gHands[fOwner].Stats.GetWareBalance(wtStone) / Max(0.001,StoneNeed);
      StoneProduction := HouseCount(htQuarry) * PRODUCTION_RATE[wtStone] + Max(StoneReserve - 30, 0);
    end;

    //In some maps there is no clay so pottery is blocked
    if gHands[fOwner].Locks.HouseBlocked[htQuarry] then
      TilesProduction := 99999
    else
    begin
      TilesReserve := gHands[fOwner].Stats.GetWareBalance(wtTile) / Max(0.001,TileNeed);
      TilesProduction := HouseCount(htPottery) * PRODUCTION_RATE[wtTile] + Max(TilesReserve - 30, 0);
    end;
    WoodcutReserve := gHands[fOwner].Stats.GetWareBalance(wtTrunk) * 2 / Max(0.001,WoodNeed);
    WoodcutTheory := HouseCount(htWoodcutters) * PRODUCTION_RATE[wtTrunk] * 2 + Max(WoodcutReserve - 30, 0);

    SawmillReserve := gHands[fOwner].Stats.GetWareBalance(wtTimber) / Max(0.001,WoodNeed);
    SawmillTheory := HouseCount(htSawmill) * PRODUCTION_RATE[wtTimber] + Max(SawmillReserve - 30, 0);
    WoodProduction := Min(WoodcutTheory, SawmillTheory);



    StoneBalance    := StoneProduction - StoneNeed;
    WoodBalance     := WoodProduction - WoodNeed;
    TileBalance     := TilesProduction - TileNeed;

    //production rate for apples were tested with 10 fruit trees at once
    FruitsBalance := HouseCount(htAppleTree) * PRODUCTION_RATE[wtApple]//actual count
                      + gHands[fOwner].Stats.Wares[wtApple].ActualCnt//actual fruits count
                      - 2//reserve
                      - HouseCount(htCottage) * 3 - HouseCount(htHouse) * 6;//required
    {
    WaterBalance := HouseCount(htWell)//actual count
                      + gHands[fOwner].Stats.Wares[wtWater].ActualCnt//reserve
                      - HouseCount(htAppleTree) * 0.25 - HouseCount([htSwine, htBakery, htStables, htHovel]);//required
    }
    Balance := Min([StoneBalance, WoodBalance, TileBalance, FruitsBalance{, WaterBalance}]);
    fMaterialsText := Format('%.2f Materials: (Stone %.1f-%.1f, Wood (%.1f:%.1f)-%.1f), Tiles (%.1f-%.1f), Fruits : %.1f',
                             [Balance, StoneProduction, StoneNeed, WoodcutTheory, SawmillTheory, WoodNeed, TilesProduction, TileNeed, FruitsBalance{, WaterBalance}]);
  end;
end;


procedure TKMayorBalance.DistributeCorn;
const
  BEAST_COST = 4;
var
  CornProduction, CornConsumption, CornReserve: Single;
  FlourPerMin, PigPerMin, HorsePerMin: Single;
  CornExtra, CornDeficit: Single;
begin

  //With stable production rate we can assume consumption rate that would be required
  FlourPerMin := HouseCount(htMill) * PRODUCTION_RATE[wtFlour];
  PigPerMin   := HouseCount(htSwine) * PRODUCTION_RATE[wtPig] * BEAST_COST;
  HorsePerMin := HouseCount(htStables) * PRODUCTION_RATE[wtHorse] * BEAST_COST;
  CornConsumption := FlourPerMin + PigPerMin + HorsePerMin;
  CornReserve := gHands[fOwner].Stats.Wares[wtCorn].ActualCnt / Max(CornConsumption,0.001);
  CornProduction := HouseCount(htFarm) * PRODUCTION_RATE[wtCorn] * FARM_THEORY_FACTOR + CornReserve;

  if CornProduction >= CornConsumption then
  begin
    //Let every industry think the extra belongs to it
    CornExtra := CornProduction - CornConsumption;
    fFood.Bread.FarmTheory := (FlourPerMin + CornExtra) * 2;
    fFood.Sausages.FarmTheory := (PigPerMin + CornExtra) / BEAST_COST * 3;
    fWarfare.LeatherArmor.FarmTheory := (PigPerMin + CornExtra) / BEAST_COST * 2;
    fWarfare.Horse.FarmTheory := (HorsePerMin + CornExtra) / BEAST_COST;
  end
  else
  begin
    //Sharing proportionaly doesn't work since closer houses get more.
    //Let every industry think the deficit belongs to it
    CornDeficit := CornConsumption - CornProduction;
    fFood.Bread.FarmTheory := Max(0, (FlourPerMin - CornDeficit) * 2);
    fFood.Sausages.FarmTheory := Max(0, (PigPerMin - CornDeficit) / BEAST_COST * 3);
    fWarfare.LeatherArmor.FarmTheory := Max(0, (PigPerMin - CornDeficit) / BEAST_COST * 2);
    fWarfare.Horse.FarmTheory := Max(0, (HorsePerMin - CornDeficit) / BEAST_COST);
  end;
end;


procedure TKMayorBalance.UpdateBalanceFood;
var
  P: TKMHand;
  UT: TKMUnitType;
begin
  P := gHands[fOwner];

  with fFood do
  begin
    //Bread
    //Calculate how much bread each link could possibly produce
    //Bread.FarmTheory calculated above
    Bread.MillTheory := HouseCount(htMill) * PRODUCTION_RATE[wtFlour] * 2;
    Bread.BakeryTheory := HouseCount(htBakery) * PRODUCTION_RATE[wtBread];
    BreadProduction := Min([Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory]);

    //Sausages
    //Calculate how many sausages each link could possibly produce
    //Sausages.FarmTheory calculated above
    Sausages.SwineTheory := HouseCount(htSwine) * PRODUCTION_RATE[wtPig] * 3;
    Sausages.ButchersTheory := HouseCount(htButchers) * PRODUCTION_RATE[wtSausage];
    SausagesProduction := Min([Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory]);

    //Wine, Fish
    WineProduction := HouseCount(htVineyard) * PRODUCTION_RATE[wtWine] * 2;
    FishProduction := HouseCount(htFishermans) * PRODUCTION_RATE[wtFish] * 10;
    //actual count - needed
    //Count in "food units per minute"
    Production := BreadProduction * BREAD_RESTORE +
                  SausagesProduction * SAUSAGE_RESTORE +
                  WineProduction *  WINE_RESTORE +
                  FishProduction * FISH_RESTORE;

    Consumption := 0;

    for UT := CITIZEN_MIN to CITIZEN_MAX do
       Consumption := Consumption + P.Stats.GetUnitQty(UT) / 60; //On average unit needs to eat each 40min

    Consumption := Consumption * 2; //Otherwise not enough food is made for citizens

    //Warriors eat on average half as much as citizens
    for UT := WARRIOR_MIN to WARRIOR_MAX do
       Consumption := Consumption + P.Stats.GetUnitQty(UT) / 2 / 60; //On average unit needs to eat each 40min

    Reserve := gHands[fOwner].Stats.GetWareBalance(wtBread) * BREAD_RESTORE +
               gHands[fOwner].Stats.GetWareBalance(wtSausage) * SAUSAGE_RESTORE +
               gHands[fOwner].Stats.GetWareBalance(wtWine) * WINE_RESTORE +
               gHands[fOwner].Stats.GetWareBalance(wtFish) * FISH_RESTORE;
    Reserve := Reserve / Max(1,Consumption);

    Balance := Production - Consumption + Max(Reserve - 30, 0);
    fFoodText := Format('%.2f Food: %.2f - %.2f + %.2f|', [Balance, Production, Consumption, Reserve])
               + Format('       Bread: min(F%.2f, M%.2f, B%.2f)|', [Bread.FarmTheory, Bread.MillTheory, Bread.BakeryTheory])
               + Format('    Sausages: min(F%.2f, S%.2f, B%.2f)|', [Sausages.FarmTheory, Sausages.SwineTheory, Sausages.ButchersTheory])
               + Format('        Wine: W%.2f)|', [WineProduction])
               + Format('        Fish: F%.2f)|', [FishProduction])
               + Format('  Food value: %.2f + %.2f + %.2f + %.2f|', [BreadProduction * BREAD_RESTORE, SausagesProduction * SAUSAGE_RESTORE, WineProduction * WINE_RESTORE, FishProduction * FISH_RESTORE]);
  end;
end;


//Tell Mayor what proportions of army is needed
procedure TKMayorBalance.SetArmyDemand(aNeeds: TWarfareDemands);
var
  WT: TKMWareType;
begin
  //Convert army request into how many weapons are needed
  with fWarfare do
  begin
    for WT := low(aNeeds) to high(aNeeds) do
      Warfare[WT].Demand := aNeeds[WT];

    //Calculate ratios at which warfare should be produced in workshops
    OrderRatio[wtWoodenShield] := aNeeds[wtWoodenShield] / Max(0.001,(aNeeds[wtWoodenShield] + aNeeds[wtPlateArmor]));
    OrderRatio[wtLeatherArmor]  := aNeeds[wtLeatherArmor]  / Max(0.001,(aNeeds[wtLeatherArmor] + aNeeds[wtQuiver]));
    OrderRatio[wtAxe]  := aNeeds[wtAxe]  / Max(0.001,(aNeeds[wtAxe] + aNeeds[wtLance] + aNeeds[wtBow] + aNeeds[wtMace]));
    OrderRatio[wtLance] := aNeeds[wtLance] / Max(0.001,(aNeeds[wtAxe] + aNeeds[wtLance] + aNeeds[wtBow] + aNeeds[wtMace]));
    OrderRatio[wtBow]  := aNeeds[wtBow]  / Max(0.001,(aNeeds[wtAxe] + aNeeds[wtLance] + aNeeds[wtBow] + aNeeds[wtMace]));
    OrderRatio[wtMace]  := aNeeds[wtMace]  / Max(0.001,(aNeeds[wtAxe] + aNeeds[wtLance] + aNeeds[wtBow] + aNeeds[wtMace]));

    OrderRatio[wtIronShield] := aNeeds[wtIronShield] / Max(0.001,(aNeeds[wtIronShield] + aNeeds[wtIronArmor]));
    OrderRatio[wtIronArmor]  := aNeeds[wtIronArmor]  / Max(0.001,(aNeeds[wtIronShield] + aNeeds[wtIronArmor]));
    OrderRatio[wtSword]     := aNeeds[wtSword]     / Max(0.001,(aNeeds[wtSword] + aNeeds[wtPike] + aNeeds[wtCrossbow] + aNeeds[wtFlail]));
    OrderRatio[wtPike] := aNeeds[wtPike] / Max(0.001,(aNeeds[wtSword] + aNeeds[wtPike] + aNeeds[wtCrossbow] + aNeeds[wtFlail]));
    OrderRatio[wtCrossbow]   := aNeeds[wtCrossbow]   / Max(0.001,(aNeeds[wtSword] + aNeeds[wtPike] + aNeeds[wtCrossbow] + aNeeds[wtFlail]));
    OrderRatio[wtFlail]   := aNeeds[wtFlail]   / Max(0.001,(aNeeds[wtSword] + aNeeds[wtPike] + aNeeds[wtCrossbow] + aNeeds[wtFlail]));
  end;
end;


function TKMayorBalance.BalanceText: UnicodeString;
begin
  Result := fCoreText + '|' +
            fMaterialsText + '|' +
            fGoldText + '|' +
            fFoodText + '|' +
            fWarfareText + '|' +
            fAdviceText;
end;


procedure TKMayorBalance.Refresh;
var
  I: Integer;
begin
  SetLength(fAdvice, 0);
  try
    //Refresh balance of each industry
    //Try to express needs in terms of Balance = Production - Demand
    UpdateBalanceCore;
    AppendCore;

    //Don't build anything if we don't have a working School
    if gHands[fOwner].Stats.GetHouseQty(htSchool) = 0 then Exit;

    UpdateBalanceMaterials;
    AppendMaterials;

    //Don't build anything if we are short on materials
    if (fMaterials.StoneProduction < 0.5)
    or (fMaterials.WoodProduction < 0.5) then Exit;

    DistributeCorn;
    DistributeCoal;
    DistributeSteel;
    DistributeWood;

    UpdateBalanceGold;
    AppendGold;

    UpdateBalanceFood;
    AppendFood;

    UpdateBalanceWarfare;
    AppendWeaponry;

   // if (gHands[fOwner].Stats.GetHouseQty(htAny) div 10) > gHands[fOwner].Stats.GetHouseQty(htStore) then
   //   Append(htStore);

  finally
    fAdviceText := 'Advice: ';
    for I := 0 to High(fAdvice) do
      fAdviceText := fAdviceText + gRes.Houses[fAdvice[I]].HouseName + IfThen(I < High(fAdvice), ', ', '.');
  end;
end;


//Look at next item in advice queue
function TKMayorBalance.Peek: TKMHouseType;
begin
  //Take element from fAdvice queue
  if Length(fAdvice) > 0 then
    Result := fAdvice[0]
  else
    Result := htNone;
end;


//Reject the item because it could not be built
procedure TKMayorBalance.Reject;
begin
  Take;

  //Some logic that would not build anything and waste resources on that
  //if we dont have e.g. gold supply
end;


//Advised item was taken and most certainly will be finished
procedure TKMayorBalance.Take;
begin
  if Length(fAdvice) > 1 then
    Move(fAdvice[1], fAdvice[0], Length(fAdvice) - 1);

  //Trim last element
  SetLength(fAdvice, Length(fAdvice) - 1);
end;


procedure TKMayorBalance.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('AIMayorBalance');
  SaveStream.Write(fOwner);

  SaveStream.Write(GoldNeed);
  SaveStream.Write(StoneNeed);
  SaveStream.Write(WoodNeed);

  //Save because there are demands for weaponry (set by General)
  SaveStream.Write(fWarfare.Warfare, SizeOf(fWarfare.Warfare));
  SaveStream.Write(fWarfare.OrderRatio, SizeOf(fWarfare.OrderRatio));

  //These are not saved because they are recalculated before each use
  {LoadStream.Read(fDemandCore, SizeOf(fDemandCore));
  LoadStream.Read(fDemandMaterials, SizeOf(fDemandMaterials));
  LoadStream.Read(fDemandGold, SizeOf(fDemandGold));
  LoadStream.Read(fDemandFood, SizeOf(fDemandFood));
  LoadStream.Read(fDemandWeaponry, SizeOf(fDemandWeaponry));}
end;


procedure TKMayorBalance.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('AIMayorBalance');
  LoadStream.Read(fOwner);

  LoadStream.Read(GoldNeed);
  LoadStream.Read(StoneNeed);
  LoadStream.Read(WoodNeed);

  //Load demands for weaponry set by General
  LoadStream.Read(fWarfare.Warfare, SizeOf(fWarfare.Warfare));
  LoadStream.Read(fWarfare.OrderRatio, SizeOf(fWarfare.OrderRatio));

  //These are not saved because they are recalculated before each use
  {LoadStream.Read(fDemandCore, SizeOf(fDemandCore));
  LoadStream.Read(fDemandMaterials, SizeOf(fDemandMaterials));
  LoadStream.Read(fDemandGold, SizeOf(fDemandGold));
  LoadStream.Read(fDemandFood, SizeOf(fDemandFood));
  LoadStream.Read(fDemandWeaponry, SizeOf(fDemandWeaponry));}
end;


end.
