unit KM_AIMayor;
{$I KaM_Remake.inc}
interface
uses
  KM_AIMayorBalance, KM_AICityPlanner, KM_AISetup, KM_AIRecordBuilding,
  KM_PathfindingRoad,
  KM_ResHouses, KM_HouseCollection,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_NavMeshDefences,
  KM_ResTypes;


type
  // Mayor is the one who manages the town
  TKMayor = class
  private
    fOwner: TKMHandID;
    fSetup: TKMHandAISetup;
    fBalance: TKMayorBalance;
    fCityPlanner: TKMCityPlanner;
    fRecorder : TKMAIRecorder;
    fPathFindingRoad: TKMPathFindingRoad;
    fPathFindingRoadShortcuts: TKMPathFindingRoadShortcuts;

    fRoadBelowStore: Boolean;
    fDefenceTowersPlanned: Boolean;
    fDefenceTowers: TKMPointTagList;

    WarfareRatios: TWarfareDemands;
    fArmyDemand : TAIArmyDemandF;
    fPearlChecked : Boolean;
    procedure SetArmyDemand(aDemand : TAIArmyDemand);

    function GetMaxPlans: Byte;
    procedure CheckAutoRepair;
    procedure CheckUnitCount;
    procedure CheckWareFlow;

    procedure CheckHouseCount;
    function TryConnectToRoad(const aLoc: TKMPoint): Boolean;
    function TryBuildHouse(aHouse: TKMHouseType): Boolean;
    procedure CheckHousePlans;
    procedure CheckRoadsCount;
    procedure CheckExhaustedMines;
    procedure PlanDefenceTowers;
    procedure TryBuildDefenceTower;


    procedure CheckWeaponOrderCount;
    procedure CheckArmyDemand;
    procedure CheckMarketTrades;
    procedure CheckSilos;
    procedure CheckMerchants;
    procedure CheckPearl;
    function NeedsWare(aWare : TKMWareType) : Boolean;
    function WareToMaxCount(aWare : TKMWareType) : Integer;
    function WareFromMinCount(aWare : TKMWareType) : Integer;
    function HasHouses(aHouses : TKMHouseTypeSet) : Boolean;
    function HasUnits(aUnits : TKMUnitTypeSet) : Boolean;
    function HouseConnectedToStore(aHouse : Pointer) : boolean;
  public
    constructor Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    property CityPlanner: TKMCityPlanner read fCityPlanner;
    property Recorder : TKMAIRecorder read fRecorder;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TKMHandID);
    function BalanceText: UnicodeString;
    property ArmyDemand : TAIArmyDemandF read fArmyDemand;

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  Classes, Math, Generics.Collections,
  KM_Game,
  KM_Hand, KM_HandsCollection, KM_HandTypes,
  KM_AIFields, KM_Terrain,
  KM_Houses, KM_HouseSchool, KM_HouseStore, KM_HouseBarracks, KM_HouseMarket, KM_HouseQueue, KM_HouseTownHall,
  KM_HousePearl,
  KM_Units, KM_UnitsCollection, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitTaskDelivery,
  KM_Resource, KM_ResUnits, KM_AITypes,
  KM_CommonUtils, KM_DevPerfLog, KM_DevPerfLogTypes;


const // Sample list made by AntonP
  WarriorHouses: array [0..49] of TKMHouseType = (
  htSchool, htInn, htQuarry, htQuarry, htQuarry,
  htWoodcutters, htWoodcutters, htWoodcutters, htWoodcutters, htWoodcutters,
  htSawmill, htSawmill, htWoodcutters, htGoldMine, htCoalMine,
  htGoldMine, htCoalMine, htMetallurgists, htCoalMine, htCoalMine,
  htIronMine, htIronMine, htCoalMine, htIronMine, htWeaponSmithy,
  htWeaponSmithy, htVineyard, htVineyard, htVineyard, htStore,
  htBarracks, htFarm, htFarm, htFarm, htMill,
  htMill, htBakery, htBakery, htWell, htWell, htHovel, htHovel, htSchool, htIronSmithy,
  htIronSmithy, htFarm, htSwine, htWell, htWeaponSmithy, htArmorSmithy
  );

  WARE_MARKET_IMPORTANCE: array[0..WARE_CNT - 2] of TKMWareType = (
    wtGold,
    wtStone,
    wtTimber, wtTile,
    wtApple, wtBread, wtWine,
    wtSausage,
    wtCoal,
    wtQuiver,
    wtStoneBolt,
    wtBoots,
    wtTrunk,    wtIronOre,       wtGoldOre,
    wtIron,     wtCorn,
    wtSeed,       wtVegetables,     wtSawDust,
    wtLog,        wtSteelE,
    wtWheel,    wtBolt,     wtWater,
    wtFlour,    wtLeather,       wtPig,
    wtSkin,
    wtFeathers,
    wtWoodenShield,  wtIronShield, wtLeatherArmor, wtIronArmor,
    wtAxe,      wtSword,         wtLance,      wtPike,         wtBow,
    wtCrossbow,
    wtMace, wtFlail, wtPlateArmor,
    wtHorse,
    wtBitinE,
    wtBitinOre,
    wtBitin,
    wtFish,
    wtHay,
    wtBitinArmor,
    wtEgg
  );

  //Vital (Store, School, Inn)
  //Mining_Core (Quary x3, Woodcutters x3, Sawmill)
  //Mining_Gold (CoalMine x2, GoldMine, Metallurgists)
  //Food_Basic (Farm, Mill, Bakery, Wineyard)

  //Food

  //Warfare_Leather (Woodcutters x2, Sawmill, Swine x4, Tannery x2, Armor x2, Weapon x3)
  //Warfare_Iron (Coal x4, Iron x2, IronSmithy x2, Armor, Weapon x2)

  //Hiring_Army (Barracks)
  //Hiring_Army2 (School, CoalMine x2, GoldMine)

  WOOD_BLOCK_RAD = 5.8;


{ TKMayor }
constructor TKMayor.Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fBalance := TKMayorBalance.Create(fOwner);
  fCityPlanner := TKMCityPlanner.Create(fOwner);
  fPathFindingRoad := TKMPathFindingRoad.Create(fOwner);
  fPathFindingRoadShortcuts := TKMPathFindingRoadShortcuts.Create(fOwner);
  fDefenceTowers := TKMPointTagList.Create;
  fRecorder := TKMAIRecorder.Create(fOwner);
end;


destructor TKMayor.Destroy;
begin
  fBalance.Free;
  fCityPlanner.Free;
  fPathFindingRoad.Free;
  fPathFindingRoadShortcuts.Free;
  fDefenceTowers.Free;
  fRecorder.Free;
  inherited;
end;


procedure TKMayor.AfterMissionInit;
begin
  fCityPlanner.AfterMissionInit;
  CheckArmyDemand;
  CheckAutoRepair;
  fBalance.StoneNeed := GetMaxPlans * 2.5;
  if gHands[fOwner].IsComputer then
    fRecorder.AfterMissionStart;
end;


// Check existing unit count vs house count and train missing citizens
procedure TKMayor.CheckUnitCount;
var
  P: TKMHand;
  tmpUnitCnt,
  UnitReq: array [CITIZEN_MIN..CITIZEN_MAX] of Integer;

  // Check that AI has enough Gold to train serfs/workers
  function HasEnoughGoldForAux: Boolean;
  begin
    //Producing gold or (Gold > 10)
    Result := (P.Stats.Wares[wtGold].ActualCnt > 10);
  end;

  function TryToTrain(aSchool: TKMHouseSchool; aUnitType: TKMUnitType; aRequiredCount: Integer): Boolean;
  begin
    // We summ up requirements for e.g. Recruits required at Towers and Barracks
    if P.Stats.GetUnitQty(aUnitType) < (aRequiredCount + UnitReq[aUnitType]) then
    begin
      Dec(UnitReq[aUnitType]); //So other schools don't order same unit
      aSchool.AddUnitToQueue(aUnitType, 1);
      Result := True;
    end
    else
      Result := False;
  end;

  function RecruitsNeeded: Integer;
  var AxesLeft: Integer;
    I : Integer;
    H : TKMHouseBarracks;
  begin
    Result := 0;
    if P.Stats.GetHouseQty(htBarracks) = 0 then
      Result := 0
    else
      if gGame.IsPeaceTime then
      begin
        //Keep enough recruits to equip using all weapons once PT ends
        //Iron soldiers
        Result := Min(P.Stats.GetWareBalance(wtIronArmor),
                      P.Stats.GetWareBalance(wtCrossbow) + P.Stats.GetWareBalance(wtPike)
                      + Min(P.Stats.GetWareBalance(wtSword), P.Stats.GetWareBalance(wtIronShield)));
        //Leather soldiers we can make
        Inc(Result, Min(P.Stats.GetWareBalance(wtLeatherArmor),
                        P.Stats.GetWareBalance(wtBow) + P.Stats.GetWareBalance(wtLance)
                        + Min(P.Stats.GetWareBalance(wtAxe), P.Stats.GetWareBalance(wtWoodenShield))));
        //Militia with leftover axes
        AxesLeft := P.Stats.GetWareBalance(wtAxe) - Min(P.Stats.GetWareBalance(wtLeatherArmor), P.Stats.GetWareBalance(wtWoodenShield));
        if AxesLeft > 0 then
          Inc(Result, AxesLeft);
      end
      else
      begin
        Result := 0;
        for I := 0 to P.Houses.Barracks.Count - 1 do
        begin
          H := TKMHouseBarracks(P.Houses.Barracks[I]);
          if (H <> nil) and not H.IsDestroyed then
            Inc(Result, fSetup.RecruitCount - H.RecruitsCount);
        end;

        //Result := fSetup.RecruitCount * P.Stats.GetHouseQty(htBarracks);
        Inc(Result, P.Stats.GetHouseQty(htWallTower) * 2);
        Inc(Result, P.Stats.GetHouseQty(htMerchant));
        Inc(Result, P.Stats.GetHouseQty(htStall));
      end;
  end;

var
  I,K, J, L: Integer;
  UT: TKMUnitType;
  Schools: array of TKMHouseSchool;
  HS: TKMHouseSchool;
  serfCount: Integer;
  house : TKMHouse;
  unitPlan : TKMUnitPlan;
begin

  //todo: When training new units make sure we have enough gold left to train
  //stonemason-woodcutter-carpenter-2miners-metallurgist. In other words -
  //dont waste gold if it's not producing yet

  P := gHands[fOwner];

  //Citizens
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  FillChar(UnitReq, SizeOf(UnitReq), #0); //Clear up
  for I := 0 to P.Houses.Count - 1 do
  begin
    house := P.Houses[I];
    if house.IsValid(htAny, false, true) then
    if not house.IsClosedForWorker then
    if not (house.HouseType in [htBarracks, htStore, htInn, htMarket]) then
    if gRes.Houses[house.HouseType].CanHasWorker then
    begin
      FillChar(tmpUnitCnt, SizeOf(tmpUnitCnt), #0); //Clear up

      unitPlan := house.CurrentWorkersTypeCount;
      for K := 0 to high(unitPlan) do //set how many workers house already has
        tmpUnitCnt[unitPlan[K].UnitType] := unitPlan[K].Count;
      L := 0;
      J := house.MaxWorkers - house.WorkersCount; //remember how many free space house has
      while (J > 0) and (L < 5) do
      begin
        for K := 0 to high(house.HSpec.Workers) do
          if J > 0 then
          begin
            UT := house.HSpec.Workers[K].UnitType;
            if house.CanHasWorker(UT) then
              if tmpUnitCnt[UT] < house.HSpec.Workers[K].Count then
              begin
                Inc(UnitReq[UT], 1);
                Inc(tmpUnitCnt[UT]);
                Dec(J);
              end;
          end;
        Inc(L);
      end;
    end;

  end;

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, P.Stats.GetHouseQty(htSchool));
  K := 1;
  HS := TKMHouseSchool(P.FindHouse(htSchool, K));
  while HS <> nil do
  begin
    Schools[K-1] := HS;
    for I := 0 to HS.QueueLength - 1 do //Decrease requirement for each unit in training
      if HS.Queue[I] <> utNone then
        Dec(UnitReq[HS.Queue[I]]); //Can be negative and compensated by e.g. ReqRecruits
    Inc(K);
    HS := TKMHouseSchool(P.FindHouse(htSchool, K));
  end;

  //Order the training. Keep up to 2 units in the queue so the school doesn't have to wait
  for K := 0 to High(Schools) do
  begin
    HS := Schools[K];
    if (HS <> nil) and not HS.IsDestroyed and (HS.QueueCount < 2) then
    begin
      //Order citizen training
      for UT := Low(UnitReq) to High(UnitReq) do
        if (UnitReq[UT] > 0) //Skip units that houses dont need (Serfs/Workers)
        {and (UnitReq[UT] > P.Stats.GetUnitQty(UT)) }then
        begin
          Dec(UnitReq[UT]); //So other schools don't order same unit
          HS.AddUnitToQueue(UT, 1);
          Break; //Don't need more UnitTypes yet
        end;

      // If we are here then a citizen to train wasn't found, so try other unit types (citizens get top priority)
      // Serf factor is like this: Serfs = (10/FACTOR)*Total_Building_Count) (from: http://atfreeforum.com/knights/viewtopic.php?t=465)

      // While still haven't found a match...
      while (HS.QueueCount < 2) do
      begin
        // If we are low on Gold don't hire more ppl (next school will fail this too, so we can exit)
        if not HasEnoughGoldForAux then Exit;

        serfCount := Round(fSetup.SerfsPerHouse * (P.Stats.GetHouseQty(htAny) + P.Stats.GetUnitQty(utBuilder)/2));

        if not TryToTrain(HS, utSerf, serfCount) then
          if not TryToTrain(HS, utBuilder, fSetup.WorkerCount) then
            if not gGame.CheckTime(fSetup.RecruitDelay) then //Recruits can only be trained after this time
              Break
            else
              if not TryToTrain(HS, utRecruit, RecruitsNeeded) then
                Break; //There's no unit demand at all
      end;
    end;
  end;
end;


procedure TKMayor.CheckArmyDemand;
var demands : TAIArmyDemand;
begin
  gHands[fOwner].AI.General.DefencePositions.GetArmyDemand(demands);
  SetArmyDemand(demands);
end;


//Check that we have weapons ordered for production
procedure TKMayor.CheckWeaponOrderCount;
const
  //Order weapons in portions to avoid overproduction of one ware over another
  //(e.g. Shields in armory until Leather is available)
  PORTIONS = 8;
var
  I,K: Integer;
  H: TKMHouse;
  WareOrder: Integer;

  function WareInBarracks(aWare :TKMWareType) : Integer;
  begin
    Result := gHands[fOwner].Stats.Wares[aWare].ActualCnt;
    {Count := gHands[fOwner].Houses.Barracks.Count;

    for I := 0 to Count - 1 do
    begin
      H := TKMHouseBarracks(gHands[fOwner].Houses.Barracks[I]);
      if (H <> nil) and not H.IsDestroyed then
        Inc(Result, H.CheckWareIn(aWare));
    end;}

  end;

  function CheckOtherWares(aWares : array of TKMwareType) : Boolean;
  var I : Integer;
  begin
    Result := true;
    for I := Low(aWares) to High(aWares) do
      Result := Result and (WareInBarracks(aWares[I]) >= 100 * Round(WarfareRatios[aWares[I]]));
  end;

  function MachinesUnlocked : Boolean;
  var I : Integer;
  begin
    Result := false;

    for I := Low(SIEGE_GAME_ORDER) to High(SIEGE_GAME_ORDER) do
      if gHands[fOwner].Locks.GetUnitBlocked(SIEGE_GAME_ORDER[I], htSiegeWorkshop) = ulUnlocked then
        Exit(true);

  end;

  function UnitUnlocked(aType : TKMUnitType; aHouse : TKMHouseType = htAny) : Boolean;
  begin
    Result :=  gHands[fOwner].Locks.GetUnitBlocked(aType, aHouse) = ulUnlocked;
  end;

  function DoProduceWare(aWare : TKMWareType) : Boolean;
  begin
    if aWare in [wtAll, wtNone, wtWarfare, wtFood] then
      Exit(false);
    if not (aWare in WARES_WARFARE) then
      Exit(false);
    Result := true;
    {case aWare of
      wtWoodenShield,
      wtLeatherArmor: Result := (WareInBarracks(aWare) < 100) or CheckOtherWares([wtWoodenShield, wtLeatherArmor]);

      wtIronArmor,
      wtIronShield: Result := (WareInBarracks(aWare) < 100) or CheckOtherWares([wtIronArmor, wtIronShield]);

      wtAxe,
      wtLance,
      wtBow: Result := (WareInBarracks(aWare) < 100) or CheckOtherWares([wtAxe, wtLance, wtBow]);

      wtPike,
      wtSword,
      wtCrossbow: Result := (WareInBarracks(aWare) < 100) or CheckOtherWares([wtPike, wtSword, wtCrossbow]);
    end;}

  end;

begin
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    WareOrder := 0;
    if H.HouseType <> htProductionThatch then
      for K := 1 to WARES_IN_OUT_COUNT do
        WareOrder := WareOrder + H.WareOrder[K];

    if not H.IsDestroyed and ((WareOrder = 0) or (H.HouseType in [htStoneWorkshop, htProductionThatch, htTailorsShop, htIronFoundry])) then
    case H.HouseType of
      htProductionThatch: for K := 1 to WARES_IN_OUT_COUNT do
                          begin
                            {if H.WareOrder[K] = MAX_WARES_ORDER then
                              Continue;}

                            H.WareOrder[K] := 0;
                            case H.WareOutPut[K] of
                              wtHay,
                              wtCorn :If (WarfareRatios[wtLeatherArmor] > 0)
                                      or (WarfareRatios[wtHorse] > 0)
                                      or (WarfareRatios[wtQuiver] > 0)
                                      or HasHouses([htSwine, htStables]) then
                                        H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;


                              wtSeed :If (WarfareRatios[wtQuiver] > 0)
                                      or HasHouses([htMill, htHovel]) then
                                        H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;

                              wtVegetables :If HasHouses([htInn]) then
                                              H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;
                              wtWine :If HasHouses([htInn]) then
                                              H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;
                              wtStoneBolt :If NeedsWare(wtStoneBolt) and (HasHouses([htWatchTower]) or HasUnits([utRogue])) then
                                        H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;

                              wtTile,
                              wtStone,
                              wtIron,
                              wtGold,
                              wtBitin,
                              wtBitinE,
                              wtSteelE,
                              wtBread,
                              wtFlour,
                              wtSawDust,
                              wtLog,
                              wtWheel,
                              wtTimber :If NeedsWare(H.WareOutPut[K]) then
                                        H.WareOrder[K] := MAX_WARES_ORDER
                                      else
                                        H.WareOrder[K] := 0;

                            end;
                          end;

      htTailorsShop:If TKMHouseQueue(H).QueueIsEmpty then
                      for K := 1 to WARES_IN_OUT_COUNT do
                          case H.WareOutput[K] of
                            //no need for AI to make boots
                            wtBoots : if gHands[fOwner].Stats.Wares[wtBoots].ActualCnt < 10 then //we have made so many boots that everyone has, no need to make more   n
                                        if gHands[fOwner].Stats.Wares[wtFeathers].ActualCnt > 3 then
                                      TKMHouseQueue(H).AddWareToQueue(wtBoots, 1, 1);
                            wtQuiver :  if gHands[fOwner].Stats.Wares[wtQuiver].ActualCnt < 25 * gHands[fOwner].Stats.GetHouseQty(htStore) then
                                        if gHands[fOwner].Stats.Wares[wtFeathers].ActualCnt > 1 then
                                          TKMHouseQueue(H).AddWareToQueue(wtQuiver, 1, 1);
                            wtLeatherArmor : If WarfareRatios[wtLeatherArmor] > 0 then
                                              if (gHands[fOwner].Stats.Wares[wtLeatherArmor].ActualCnt < 100) or (gHands[fOwner].Stats.Wares[wtLeatherArmor].ActualCnt > 200) then
                                                TKMHouseQueue(H).AddWareToQueue(wtLeatherArmor, 3, 1);
                          end;
      htIronFoundry:
                      for K := 1 to WARES_IN_OUT_COUNT do
                        If H.WareOrder[K] = 0 then
                          case H.WareOutput[K] of
                            wtSteelE : if  NeedsWare(wtSteelE) then H.WareOrder[K] := 8;
                            wtBitinE : If H.CheckWareIn(wtBitin) > 0 then H.WareOrder[K] := 3;
                            wtBolt : if gHands[fOwner].Stats.GetUnitQty(utBallista) > 0 then
                                        H.WareOrder[K] := 2;//produce bolt only if they are unlocked
                          end;

      htStoneWorkshop:   for K := 1 to WARES_IN_OUT_COUNT do
                          If H.WareOrder[K] = 0 then
                            case H.WareOutput[K] of
                              wtStoneBolt : If (H.CheckWareIn(wtStone) > 0)
                                            and ((gHands[fOwner].Stats.GetHouseQty(htWatchTower) > 0)
                                                  or UnitUnlocked(utCatapult, htSiegeWorkshop)
                                                  or((UnitUnlocked(utRogue, htTownHall) and (fArmyDemand[gtRanged] > 0))))
                                            then H.WareOrder[K] := 6;

                              wtLog : If MachinesUnlocked and NeedsWare(wtLog) then H.WareOrder[K] := 6;
                              wtWheel : If MachinesUnlocked and NeedsWare(wtWheel) then H.WareOrder[K] := 3;

                              wtSawDust : If (gHands[fOwner].Stats.GetHouseQty(htWoodburner) > 0) and (H.CheckWareIn(wtTrunk) > 0) then H.WareOrder[K] := 1;
                            end;
      htArmorWorkshop:   for K := 1 to WARES_IN_OUT_COUNT do
                          case H.WareOutput[K] of
                            wtWoodenShield : H.WareOrder[K] := Round(WarfareRatios[H.WareOutput[K]] * PORTIONS);
                            wtPlateArmor : H.WareOrder[K] := Round(WarfareRatios[wtPlateArmor] * PORTIONS);
                          end;
      htArmorSmithy:   for K := 1 to WARES_IN_OUT_COUNT do
                          case H.WareOutput[K] of
                            wtBitinArmor : If H.CheckWareIn(wtBitinE) > 0 then H.WareOrder[K] := 1;
                            else
                              if DoProduceWare(H.WareOutput[K]) then
                                H.WareOrder[K] := Round(WarfareRatios[H.WareOutput[K]] * PORTIONS);
                          end;

      else
      begin
        for K := 1 to WARES_IN_OUT_COUNT do
          if DoProduceWare(H.WareOutput[K]) then
            H.WareOrder[K] := Round(WarfareRatios[H.WareOutput[K]] * PORTIONS);
      end;
    end;
  end;
end;

function TKMayor.HasHouses(aHouses : TKMHouseTypeSet) : Boolean;
var HT : TKMHouseType;
begin
  Result := false;
  for HT in aHouses do
    if gHands[fOwner].Stats.GetHouseQty(HT) > 0 then
      Exit(true);
end;

function TKMayor.HasUnits(aUnits: TKMUnitTypeSet): Boolean;
var UT : TKMUnitType;
begin
  Result := false;
  for UT in aUnits do
    if gHands[fOwner].Stats.GetUnitQty(UT) > 0 then
      Exit(true);
end;

function TKMayor.HouseConnectedToStore(aHouse: Pointer): Boolean;
var H, HS : TKMHouse;
  I : Integer;
begin
  Result := false;
  H := TKMHouse(aHouse);

  for I := 0 to gHands[fOwner].Houses.Stores.Count - 1 do
  begin
    HS := gHands[fOwner].Houses.Stores[I];
    if not HS.IsValid(htStore, false, true) then
      Continue;

    if gTerrain.RouteCanBeMade(H.PointBelowEntrance, HS.PointBelowEntrance, tpWalkRoad) then
      Exit(true);
  end;
    

end;

function TKMayor.GetMaxPlans: Byte;
begin
  Result := Ceil(fSetup.WorkerCount / 4);
end;


//We want to connect to nearest road piece (not necessarily built yet)
function TKMayor.TryConnectToRoad(const aLoc: TKMPoint): Boolean;
const
  MAX_DISTANCE = 150;
var
  I: Integer;
  P: TKMHand;
  H: TKMHouse;
  LocTo: TKMPoint;
  RoadConnectID: Byte;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  Result := False;
  P := gHands[fOwner];

  //Find nearest wip or ready house
  //H := P.Houses.FindHouse(htAny, aLoc.X, aLoc.Y, 1, False);
  H := P.Houses.FindHouseToBuildRoad(HOUSES_VALID, aLoc.X, aLoc.Y, 1, False);
  if H = nil then Exit; //We are screwed, no houses left
  LocTo := H.PointBelowEntrance;

  //Find nearest complete house to get the road connect ID
  //H := P.Houses.FindHouse(htAny, aLoc.X, aLoc.Y, 1, True);
  H := P.Houses.FindHouseToBuildRoad(HOUSES_VALID, aLoc.X, aLoc.Y, 1, True);
  if H = nil then Exit; //We are screwed, no houses left
  RoadConnectID := gTerrain.GetRoadConnectID(H.PointBelowEntrance);

  NodeList := TKMPointList.Create;
  try
    RoadExists := fPathFindingRoad.Route_ReturnToWalkable(aLoc, LocTo, RoadConnectID, NodeList);

    if not RoadExists OR (NodeList.Count > MAX_DISTANCE) then
      Exit;

    for I := 0 to NodeList.Count - 1 do
      //We must check if we can add the plan ontop of plans placed earlier in this turn
      if P.CanAddFieldPlan(NodeList[I], ftRoad) then
         P.Constructions.FieldworksList.AddField(NodeList[I], ftRoad, rtStone);
    Result := True;
  finally
    NodeList.Free;
  end;
end;

procedure TKMayor.PlanDefenceTowers;
const
  DISTANCE_BETWEEN_TOWERS = 7;
var
  P: TKMHand;
  PL1, PL2: TKMHandID;
  pom: boolean;
  //Outline1, Outline2: TKMWeightSegments;
  I, K, DefCount: Integer;
  // DefCount: Integer;
  Point1, Point2: TKMPoint;
  //Loc: TKMPoint;
  Ratio: Single;
  //SegLength: Single;
  DefLines: TKMDefenceLines;
begin
  if fDefenceTowersPlanned then
    Exit;
  fDefenceTowersPlanned := True;
  P := gHands[fOwner];
  if not P.Locks.HouseCanBuild(htWatchTower) then
    Exit;
  pom := not gAIFields.NavMesh.Defences.FindDefenceLines(fOwner, DefLines) OR (DefLines.Count < 1);
  if pom then
    Exit;

  for I := 0 to DefLines.Count - 1 do
    with DefLines.Lines[I] do
    begin
      Point1 := gAIFields.NavMesh.Nodes[DefLines.Lines[I].Nodes[0]];
      Point2 := gAIFields.NavMesh.Nodes[DefLines.Lines[I].Nodes[1]];
      PL1 := gAIFields.Influences.GetBestAllianceOwner(fOwner, Point1, atAlly);
      PL2 := gAIFields.Influences.GetBestAllianceOwner(fOwner, Point2, atAlly);
      if (PL1 <> fOwner) AND (PL2 <> fOwner) AND (PL1 <> HAND_NONE) AND (PL2 <> HAND_NONE) then
        Continue;
      DefCount := Ceil( KMLength(Point1, Point2) / DISTANCE_BETWEEN_TOWERS );
      for K := 0 to DefCount - 1 do
      begin
        Ratio := (K + 1) / (DefCount + 1);
        fDefenceTowers.Add( KMPointRound(KMLerp(Point1, Point2, Ratio)), gAIFields.Influences.GetBestAllianceOwnership(fOwner, Polygon, atEnemy));
      end;
    end;

  //Get defence Outline with weights representing how important each segment is
  //gAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);

  //Make list of defence positions
  //for I := 0 to High(Outline2) do
  //begin
  //  //Longer segments will get several towers
  //  SegLength := KMLength(Outline2[I].A, Outline2[I].B);
  //  DefCount := Max(Trunc(SegLength / DISTANCE_BETWEEN_TOWERS), 1);
  //  for K := 0 to DefCount - 1 do
  //  begin
  //    Ratio := (K + 1) / (DefCount + 1);
  //    Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
  //    fDefenceTowers.Add(Loc, Trunc(1000*Outline2[I].Weight));
  //  end;
  //end;
  fDefenceTowers.SortByTag;
  fDefenceTowers.Inverse; //So highest weight is first
end;

procedure TKMayor.TryBuildDefenceTower;
const
  SEARCH_RAD = 6;
  MAX_ROAD_DISTANCE = 50;
var
  P: TKMHand;
  IY, IX: Integer;
  Loc: TKMPoint;
  DistSqr, BestDistSqr: Integer;
  BestLoc: TKMPoint;

  NodeList: TKMPointList;
  H: TKMHouse;
  LocTo: TKMPoint;
  RoadConnectID: Byte;
  RoadExists: Boolean;
begin
  P := gHands[fOwner];
  //Take the first tower from the list
  Loc := fDefenceTowers[0];
  fDefenceTowers.Delete(0);
  //Look for a place for the tower
  BestDistSqr := High(BestDistSqr);
  BestLoc := KMPOINT_ZERO;
  for IY := Max(1, Loc.Y-SEARCH_RAD) to Min(gTerrain.MapY, Loc.Y+SEARCH_RAD) do
    for IX := Max(1, Loc.X-SEARCH_RAD) to Min(gTerrain.MapX, Loc.X+SEARCH_RAD) do
    begin
      DistSqr := KMLengthSqr(Loc, KMPoint(IX, IY));
      if (DistSqr < BestDistSqr) and P.CanAddHousePlanAI(IX, IY, htWatchTower, False) then
      begin
        BestLoc := KMPoint(IX, IY);
        BestDistSqr := DistSqr;
      end;
    end;
  if (BestLoc.X > 0) then
  begin
    //See if the road required is too long (tower might be across unwalkable terrain)
    H := P.Houses.FindHouse(htAny, BestLoc.X, BestLoc.Y, 1, False);
    if H = nil then Exit; //We are screwed, no houses left
    LocTo := H.PointBelowEntrance;

    //Find nearest complete house to get the road connect ID
    H := P.Houses.FindHouse(htAny, BestLoc.X, BestLoc.Y, 1, True);
    if H = nil then Exit; //We are screwed, no houses left
    RoadConnectID := gTerrain.GetRoadConnectID(H.PointBelowEntrance);

    NodeList := TKMPointList.Create;
    RoadExists := fPathFindingRoad.Route_ReturnToWalkable(BestLoc, LocTo, RoadConnectID, NodeList);
    //If length of road is short enough, build the tower
    if RoadExists and (NodeList.Count <= MAX_ROAD_DISTANCE) then
    begin
      if P.Locks.HouseCanBuild(htWallTower) and CheckRandom(30) then
        gHands[fOwner].AddHousePlan(htWallTower, BestLoc)
      else
        gHands[fOwner].AddHousePlan(htWatchTower, BestLoc);

      TryConnectToRoad(KMPointBelow(BestLoc));
    end;
    NodeList.Free;
  end;
end;
//Try to place a building plan for requested house
//Report back if failed to do so (that will allow requester to choose different action)
function TKMayor.TryBuildHouse(aHouse: TKMHouseType): Boolean;
var
  I, K: Integer;
  Loc: TKMPoint;
  P: TKMHand;
  NodeTagList: TKMPointTagList;
  Weight: Cardinal;
  ignoreRoad : Boolean;
begin
  Result := False;
  P := gHands[fOwner];
  //Skip disabled houses
  if not P.Locks.HouseCanBuild(aHouse) then Exit;

  //Number of simultaneous WIP houses is limited
  if (P.Stats.GetHouseWip(htAny) > GetMaxPlans) then Exit;

  //Maybe we get more lucky next tick
  //todo: That only works if FindPlaceForHouse is quick, right now it takes ~11ms for iron/gold/coal mines (to decide that they can't be placed).
  //      If there's no place for the house we try again and again and again every update, so it's very inefficient
  //      I think the best solution would be to make FindPlaceForHouse only take a long time if we succeed in finding a place for the house, if we
  //      fail it should be quick. Doing a flood fill with radius=40 should really be avoided anyway, 11ms is a long time for placing 1 house.
  //      We could also make it not try to place houses again each update if it failed the first time, if we can't make FindPlaceForHouse quick when it fails.
  if not fCityPlanner.FindPlaceForHouse(aHouse, Loc, ignoreRoad) then Exit;

  //Place house before road, so that road is made around it
  P.AddHousePlan(aHouse, Loc);

  // Script could delete house plan we placed, so check if we actually added it
  if not P.HasHousePlan(Loc) then
    Exit(False);

  //Try to connect newly planned house to road network
  //if it is not possible - scrap the plan
  if not ignoreRoad and not TryConnectToRoad(KMPointBelow(Loc)) then
  begin
    P.RemHousePlan(Loc);
    Exit;
  end;
  
  //I tried to use this when the bug occured but it didn't always work because AI places multiple house/field plans at once (if P.CanAddFieldPlan(KMPointBelow(Loc), ftRoad) then)
  //Fixes Classical AI bug related to houses never being finished/connected to road network
  If not ignoreRoad then
  begin
   P.Constructions.FieldworksList.RemFieldPlan(KMPointBelow(Loc)); //Make sure our entrance to the house has no plan (vine/corn) in front of it
   P.Constructions.FieldworksList.AddField(KMPointBelow(Loc), ftRoad, rtStone); //Place a road below house entrance to make sure it is connected to our city!
  end;
  //Build fields for Farm
  if aHouse = htFarm then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y - 2, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ftCorn) then
        begin
          //Base weight is distance from door (weight X higher so nice rectangle is formed)
          Weight := Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y);
          //Prefer fields below the farm
          if (I < Loc.Y + 2) then
            Inc(Weight, 100);
          //Avoid building on row with roads (so we can expand from this house)
          if I = Loc.Y + 1 then
            Inc(Weight, 1000);
          //avoid building on clay deposits
          Inc(Weight, gTerrain.TileIsClay(Loc.X, Loc.Y) * 200);

          NodeTagList.Add(KMPoint(K, I), Weight);
        end;

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 16) - 1 do
        P.Constructions.FieldworksList.AddField(NodeTagList[I], ftCorn, rtNone);
    finally
      NodeTagList.Free;
    end;
  end;

  //Build fields for Wineyard
  if aHouse = htVineyard then
  begin
    NodeTagList := TKMPointTagList.Create;
    try
      for I := Min(Loc.Y - 2, gTerrain.MapY - 1) to Min(Loc.Y + 2 + AI_FIELD_HEIGHT - 1, gTerrain.MapY - 1) do
      for K := Max(Loc.X - AI_FIELD_WIDTH, 1) to Min(Loc.X + AI_FIELD_WIDTH, gTerrain.MapX - 1) do
        if P.CanAddFieldPlan(KMPoint(K,I), ftWine) then
        begin
          //Base weight is distance from door (weight X higher so nice rectangle is formed)
          Weight := Abs(K - Loc.X)*3 + Abs(I - 2 - Loc.Y);
          //Prefer fields below the farm
          if (I < Loc.Y + 2) then
            Inc(Weight, 100);
          //Avoid building on row with roads (so we can expand from this house)
          if I = Loc.Y + 1 then
            Inc(Weight, 1000);
          NodeTagList.Add(KMPoint(K, I), Weight);
        end;

      NodeTagList.SortByTag;
      for I := 0 to Min(NodeTagList.Count, 10) - 1 do
        P.Constructions.FieldworksList.AddField(NodeTagList[I], ftWine, rtNone);
    finally
      NodeTagList.Free;
    end;
  end;

  //Block any buildings nearby
  if aHouse = htWoodcutters then
    gAIFields.Influences.AddAvoidBuilding(Loc.X-1, Loc.Y, WOOD_BLOCK_RAD); //X-1 because entrance is on right

  //Build more roads around 2nd Store
  if aHouse = htStore then
    for I := Max(Loc.Y - 3, 1) to Min(Loc.Y + 2, gTerrain.MapY - 1) do
    for K := Max(Loc.X - 2, 1) to Min(Loc.X + 2, gTerrain.MapY - 1) do
    if P.CanAddFieldPlan(KMPoint(K, I), ftRoad) then
      P.Constructions.FieldworksList.AddField(KMPoint(K, I), ftRoad, rtStone);

  Result := True;
end;
//todo: Check if planned houses are being connected with roads
//(worker could die while digging a road piece or elevation changed to impassable)
procedure TKMayor.CheckHousePlans;
begin
  //
end;


//Manage ware distribution
procedure TKMayor.CheckWareFlow;
var
  I: Integer;
  S: TKMHouseStore;
  B : TKMHouseBarracks;
  Houses: TKMHousesCollection;

  function BlockWare(aWare : TKMWareType; aHouses : TKMHouseTypeSet; Min1, Min2 : Integer) : Boolean;
  begin
    if S.WareAIBlockAouto[aWare] then
      Exit(false);
    if HasHouses(aHouses) then
      S.NotAcceptFlag[aWare] := S.CheckWareIn(aWare) >= Min1
    else
      S.NotAcceptFlag[aWare] := S.CheckWareIn(aWare) >= Min2;
    Result := S.NotAcceptFlag[aWare];
  end;


begin
  Houses := gHands[fOwner].Houses;

  //Iterate through all Stores and block certain wares to reduce serf usage
  for I := 0 to Houses.Count - 1 do
    If Houses[I].IsComplete
    and not Houses[I].IsDestroyed then
    case Houses[I].HouseType of

      htTownhall: if Houses[I].GetAcceptWareIn(wtGold) < 120 then
                    Houses[I].SetAcceptWareIn(wtGold, IfThen(gHands[fOwner].Stats.Wares[wtGold].ActualCnt < 15, 120, 120 - 15) )
                  else
                    Houses[I].SetAcceptWareIn(wtGold, IfThen(gHands[fOwner].Stats.Wares[wtGold].ActualCnt < 30, 120, 120 - 15) );

      htFarm : Houses[I].ForceWorking := true;
      htBarracks: begin
                    B := TKMHouseBarracks(Houses[I]);

                    B.NotAcceptFlag[wtQuiver] := B.CheckWareIn(wtQuiver) > 10;
                    B.NotAcceptFlag[wtBoots] := B.CheckWareIn(wtBoots) > 10;
                    B.NotAcceptFlag[wtBitinArmor] := B.CheckWareIn(wtBitinArmor) > 10;
                  end;
      htStore : begin
                  S := TKMHouseStore(Houses[I]);
                  //if S.ChangedByAIBuildScript then
                  //  Continue;

                  BlockWare(wtWater, [], 0, 0);
                  BlockWare(wtBitin, [htIronFoundry], 0, 100);
                  BlockWare(wtBitinOre, [htIronSmithy], 0, 100);
                  BlockWare(wtBitinOre, [htIronSmithy], 0, 100);

                  {S.NotAcceptFlag[wtWater] := true;
                  S.NotAcceptFlag[wtBitin] := HasHouses([htIronFoundry]);
                  S.NotAcceptFlag[wtBitinOre] := HasHouses([htIronSmithy]);}

                  BlockWare(wtTimber, [], 0, 50);
                  BlockWare(wtStone, [], 0, 50);
                  BlockWare(wtGold, [], 0, 50);
                  BlockWare(wtTile, [], 0, 50);
                  BlockWare(wtApple, [], 0, 50);
                  BlockWare(wtVegetables, [], 0, 50);

                  BlockWare(wtWine, [], 0, 200);
                  BlockWare(wtBread, [], 0, 200);
                  BlockWare(wtSausage, [], 0, 200);
                  BlockWare(wtFish, [], 0, 200);

                  //We like to always keep a supply of these
                  {S.NotAcceptFlag[wtTimber] := S.CheckWareIn(wtTimber) > 50;
                  S.NotAcceptFlag[wtStone] := S.CheckWareIn(wtStone) > 50;
                  S.NotAcceptFlag[wtGold] := S.CheckWareIn(wtGold) > 50;
                  S.NotAcceptFlag[wtTile] := S.CheckWareIn(wtTile) > 50;
                  S.NotAcceptFlag[wtApple] := S.CheckWareIn(wtApple) > 50;
                  S.NotAcceptFlag[wtVegetables] := S.CheckWareIn(wtVegetables) > 50;}

                  //Storing these causes lots of congestion with very little gain
                  //Auto build AI aims for perfectly balanced village where these goods don't need storing
                  //Keep them only until we have the house which consumes them.
                  //cannot be blocked because it also produce seeds

                  BlockWare(wtTrunk, [htSawMill, htStoneWorkshop, htWoodBurner], 0, 20000 );
                  BlockWare(wtCoal, [htMetallurgists, htIronSmithy, htIronFoundry, htArmorSmithy, htWeaponSmithy], 0, 20000 );
                  BlockWare(wtIron, [htWeaponSmithy, htArmorSmithy], 0, 20000 );
                  BlockWare(wtIronOre, [htIronSmithy], 0, 20000 );
                  BlockWare(wtGoldOre, [htMetallurgists], 0, 20000 );
                  BlockWare(wtCorn, [htSwine, htStables, htWoodburner], 0, 50 );
                  BlockWare(wtSeed, [htMill, htHovel], 0, 50 );
                  BlockWare(wtFlour, [htBakery], 0, 20000 );
                  BlockWare(wtLeather, [htArmorWorkshop, htTailorsShop], 0, 20000);
                  BlockWare(wtStoneBolt, [htWatchTower], 50, 20000);

                end;
    end;
end;


//Demolish any exhausted mines, they will be rebuilt if needed
procedure TKMayor.CheckExhaustedMines;
var
  I: Integer;
  Houses: TKMHousesCollection;
  Loc: TKMPoint;
begin
  Houses := gHands[fOwner].Houses;

  //Wait until resource is depleted and output is empty
  for I := 0 to Houses.Count - 1 do
  if not Houses[I].IsDestroyed
  and Houses[I].ResourceDepleted
  and (Houses[I].CheckWareOut(wtAll) = 0) then
  begin
    //Set it so we can build over coal that was removed
    if Houses[I].HouseType = htCoalMine then
    begin
      Loc := Houses[I].Entrance;
      gAIFields.Influences.RemAvoidBuilding(KMRect(Loc.X-2, Loc.Y-2, Loc.X+3, Loc.Y+1));
    end;
    Houses[I].Demolish(fOwner);
  end;
end;
procedure TKMayor.CheckHouseCount;
var
  P: TKMHand;

  function MaxPlansForTowers: Integer;
  begin
    Result := GetMaxPlans;
    //Once there are 2 towers wip then allow balance to build something
    if (fBalance.Peek <> htNone) and (P.Stats.GetHouseWip(htWatchTower) >= 2) then
      Result := Result - 1;
    Result := Max(1, Result);
  end;

var
  H: TKMHouseType;
begin
  P := gHands[fOwner];

  //Try to express needs in terms of Balance = Production - Demand
  fBalance.Refresh;

  //Peek - see if we can build this house
  //Take - take this house into building
  //Reject - we can't build this house (that could affect other houses in queue)

  //Build towers if village is done, or peacetime is nearly over
  if P.Locks.HouseCanBuild(htWatchTower) then
    if ((fBalance.Peek = htNone) and (P.Stats.GetHouseWip(htAny) = 0)) //Finished building
    or ((gGame.Options.Peacetime <> 0) and gGame.CheckTime(600 * Max(0, gGame.Options.Peacetime - 15))) then
      PlanDefenceTowers;

  if fDefenceTowersPlanned then
    while (fDefenceTowers.Count > 0) and (P.Stats.GetHouseWip(htAny) < MaxPlansForTowers) do
      TryBuildDefenceTower;

  while P.Stats.GetHouseWip(htAny) < GetMaxPlans do
  begin
    H := fBalance.Peek;

    //There are no more suggestions
    if H = htNone then
      Break;
    If H = htPottery then
      H := htPottery;
    //See if we can build that
    if TryBuildHouse(H) then
    begin
      fBalance.Take;
      fBalance.Refresh; //Balance will be changed by the construction of this house
    end
    else
      fBalance.Reject;
  end;

  //Check if we need to demolish depleted mining houses
  CheckExhaustedMines;

  //Verify all plans are being connected with roads
  CheckHousePlans;
end;


procedure TKMayor.CheckRoadsCount;
const
  SHORTCUT_CHECKS_PER_UPDATE = 10;
var
  P: TKMHand;
  Store: TKMHouse;
  StoreLoc: TKMPoint;
  I, K: Integer;
  FromLoc, ToLoc: TKMPoint;
  NodeList: TKMPointList;
  RoadExists: Boolean;
begin
  P := gHands[fOwner];

  //This is one time task to build roads around Store
  //When town becomes larger add road around Store to make traffic smoother
  if not fRoadBelowStore and (P.Stats.GetHouseQty(htAny) > 14) then
  begin
    fRoadBelowStore := True;

    Store := P.Houses.FindHouse(htStore, 0, 0, 1);
    if Store = nil then Exit;
    StoreLoc := Store.Entrance;

    for I := Max(StoreLoc.Y - 3, 1) to Min(StoreLoc.Y + 2, gTerrain.MapY - 1) do
    for K := StoreLoc.X - 2 to StoreLoc.X + 2 do
    if P.CanAddFieldPlan(KMPoint(K, I), ftRoad) then
      P.Constructions.FieldworksList.AddField(KMPoint(K, I), ftRoad, rtStone);
  end;

  //Check if we need to connect separate branches of road network
  //Town has no plan and usually roadnetwork looks like a tree,
  //where we can improve it by connecting near branches with shortcuts.
  NodeList := TKMPointList.Create;
  try
    //See where our citizens are walking and build shortcuts where possible
    for I := 0 to gHands[fOwner].Units.Count - 1 do
    begin
      //Checking for shortcuts is slow, so skip some units randomly each update
      if KaMRandom(gHands[fOwner].Stats.GetUnitQty(utSerf), 'TKMayor.CheckRoadsCount') >= SHORTCUT_CHECKS_PER_UPDATE then
        Continue;
      if not gHands[fOwner].Units[I].IsDeadOrDying
      and (gHands[fOwner].Units[I].Action is TKMUnitActionWalkTo) then
        if ((gHands[fOwner].Units[I] is TKMUnitSerf) and (gHands[fOwner].Units[I].Task is TKMTaskDeliver)
                                                     and (TKMTaskDeliver(gHands[fOwner].Units[I].Task).DeliverKind <> dkToUnit))
        or ((gHands[fOwner].Units[I] is TKMUnitCitizen) and (gHands[fOwner].Units[I].Task is TKMTaskGoEat)) then
        begin
          FromLoc := TKMUnitActionWalkTo(gHands[fOwner].Units[I].Action).WalkFrom;
          ToLoc := TKMUnitActionWalkTo(gHands[fOwner].Units[I].Action).WalkTo;
          //Unit's route must be using road network, not f.e. delivering to soldiers
          if gTerrain.RouteCanBeMade(FromLoc, ToLoc, tpWalkRoad) then
          begin
            //Check for shortcuts we could build
            NodeList.Clear;
            RoadExists := fPathFindingRoadShortcuts.Route_Make(FromLoc, ToLoc, NodeList);

            if not RoadExists then
              Break;

            for K := 0 to NodeList.Count - 1 do
              //We must check if we can add the plan ontop of plans placed earlier in this turn
              if P.CanAddFieldPlan(NodeList[K], ftRoad) then
                P.Constructions.FieldworksList.AddField(NodeList[K], ftRoad, rtStone);
          end;
        end;
    end;
  finally
    NodeList.Free;
  end;
end;


procedure TKMayor.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
  fBalance.OwnerUpdate(aPlayer);
  fCityPlanner.OwnerUpdate(aPlayer);
  fPathFindingRoad.OwnerUpdate(aPlayer);
  fPathFindingRoadShortcuts.OwnerUpdate(aPlayer);
end;


//Tell Mayor what proportions of army is needed
//Input values are normalized
procedure TKMayor.SetArmyDemand(aDemand : TAIArmyDemand);

  function IsIronProduced: Boolean;
  begin
    Result := (  gHands[fOwner].Stats.GetHouseQty(htIronMine)
               + gHands[fOwner].Stats.GetHouseWip(htIronMine)
               + gHands[fOwner].Stats.GetHousePlans(htIronMine)) > 0;
  end;

  function GroupBlocked(aGT: TKMGroupType; aIron: Boolean): Boolean;
  begin
    if aIron then
      case aGT of
        gtMelee:     Result := gHands[fOwner].Locks.UnitUnlocked(utSwordFighter);
        gtAntiHorse: Result := gHands[fOwner].Locks.UnitUnlocked(utPikeman);
        gtRanged:    Result := gHands[fOwner].Locks.UnitUnlocked(utCrossbowman);
        gtMounted:   Result := gHands[fOwner].Locks.UnitUnlocked(utKnight);
        gtWreckers:   Result := gHands[fOwner].Locks.UnitUnlocked(utFlailFighter);
        else         Result := True;
      end
    else
      case aGT of
        gtMelee:     Result := (gHands[fOwner].Locks.UnitUnlocked(utMilitia)) and
                                (gHands[fOwner].Locks.UnitUnlocked(utAxeFighter));
        gtAntiHorse: Result := gHands[fOwner].Locks.UnitUnlocked(utLanceCarrier);
        gtRanged:    Result := gHands[fOwner].Locks.UnitUnlocked(utBowman);
        gtMounted:   Result := gHands[fOwner].Locks.UnitUnlocked(utScout);
        gtWreckers:   Result := gHands[fOwner].Locks.UnitUnlocked(utMaceFighter);
        else         Result := True;
      end;
  end;

  function GetUnitRatio(aUT: TKMUnitType): Byte;
  begin
    if not gHands[fOwner].Locks.UnitUnlocked(aUT) then
      Result := 0 //This warrior is blocked
    else
      if (fSetup.ArmyType = atIronAndLeather)
      and GroupBlocked(UNIT_TO_GROUP_TYPE[aUT], not (aUT in WARRIORS_IRON)) then
        Result := 2 //In mixed army type, if our compliment is blocked we need to make double
      else
        Result := 1;
  end;

var
  Summ: Single;
  IronPerMin, LeatherPerMin: Single;
  WT: TKMWareType;
  WarfarePerMinute: TWarfareDemands;
  demands : TAIArmyDemandF;
  GT : TKMGroupType;
begin
  Summ := 0;
  for GT := low(TKMGroupType) to High(TKMGroupType) do
    Summ := Summ + aDemand[GT];
  if Summ = 0 then
  begin
    for GT := low(TKMGroupType) to High(TKMGroupType) do
      demands[GT] := 0;
  end
  else
    for GT := low(TKMGroupType) to High(TKMGroupType) do
      demands[GT] := aDemand[GT] / Summ;

  demands[gtNone] := Summ;
  fArmyDemand := demands;
  //Store ratios localy in Mayor to place weapon orders
  //Leather
  WarfareRatios[wtLeatherArmor] := demands[gtMelee]  * GetUnitRatio(utAxeFighter)
                                 + demands[gtMounted] * GetUnitRatio(utScout)
                                 + demands[gtAntiHorse]  * GetUnitRatio(utLanceCarrier)
                                 + demands[gtRanged]  * GetUnitRatio(utBowman);

  WarfareRatios[wtPlateArmor] := demands[gtWreckers]  * GetUnitRatio(utMaceFighter);

  WarfareRatios[wtWoodenShield] :=     demands[gtMelee]  * GetUnitRatio(utAxeFighter)
                                 + demands[gtMounted] * GetUnitRatio(utScout);

  WarfareRatios[wtAxe] :=        demands[gtMelee]  * Max(GetUnitRatio(utAxeFighter), GetUnitRatio(utMilitia))
                                 + demands[gtMounted] * GetUnitRatio(utScout);
  WarfareRatios[wtLance] :=       demands[gtAntiHorse]  * GetUnitRatio(utLanceCarrier);
  WarfareRatios[wtBow] :=        demands[gtRanged]  * GetUnitRatio(utBowman);
  //Iron
  WarfareRatios[wtIronArmor] := demands[gtMelee]  * GetUnitRatio(utSwordFighter)
                                 + demands[gtMounted] * GetUnitRatio(utKnight)
                                 + demands[gtAntiHorse]  * GetUnitRatio(utPikeman)
                                 + demands[gtRanged]  * GetUnitRatio(utCrossbowman)
                                 + demands[gtWreckers]  * GetUnitRatio(utFlailFighter);

  WarfareRatios[wtIronShield] := demands[gtMelee]  * GetUnitRatio(utSwordFighter)
                                 + demands[gtMounted] * GetUnitRatio(utKnight);
  WarfareRatios[wtSword] :=      demands[gtMelee]  * GetUnitRatio(utSwordFighter)
                                 + demands[gtMounted] * GetUnitRatio(utKnight);
  WarfareRatios[wtPike] :=  demands[gtAntiHorse]  * GetUnitRatio(utPikeman);
  WarfareRatios[wtCrossbow] :=    demands[gtRanged]  * GetUnitRatio(utCrossbowman);

  WarfareRatios[wtHorse] := demands[gtMounted] * (GetUnitRatio(utKnight) + GetUnitRatio(utScout));

  WarfareRatios[wtMace] := demands[gtWreckers] * GetUnitRatio(utMaceFighter);
  WarfareRatios[wtPlateArmor] := demands[gtWreckers] * GetUnitRatio(utMaceFighter);

  WarfareRatios[wtFlail] := demands[gtWreckers] * GetUnitRatio(utFlailFighter);
  WarfareRatios[wtSteelE] := (demands[gtMachines] * (GetUnitRatio(utCatapult) + GetUnitRatio(utBallista) )
                              + demands[gtMachinesMelee] * (GetUnitRatio(utRam) + GetUnitRatio(utWoodenWall) )) * 3 ;
  WarfareRatios[wtLog] := (demands[gtMachines] * (GetUnitRatio(utCatapult) + GetUnitRatio(utBallista) )
                              + demands[gtMachinesMelee] * (GetUnitRatio(utRam) + GetUnitRatio(utWoodenWall) )) * 4 ;
  WarfareRatios[wtWheel] := (demands[gtMachines] * (GetUnitRatio(utCatapult) + GetUnitRatio(utBallista) )
                              + demands[gtMachinesMelee] * (GetUnitRatio(utRam) + GetUnitRatio(utWoodenWall) )) * 4 ;
  //How many warriors we would need to equip per-minute
  IronPerMin := fSetup.WarriorsPerMinute(atIron);
  LeatherPerMin := fSetup.WarriorsPerMinute(atLeather);

  //If the AI is meant to make both but runs out, we must make it up with leather
  if (fSetup.ArmyType = atIronAndLeather) and not IsIronProduced then
    LeatherPerMin := LeatherPerMin + IronPerMin; //Once iron runs out start making leather to replace it

  //Make only iron first then if it runs out make leather
  if (fSetup.ArmyType = atIronThenLeather) and IsIronProduced then
    LeatherPerMin := 0; //Don't make leather until the iron runs out

  for WT := WEAPON_MIN to WEAPON_MAX do
    if WT in WARFARE_IRON then
      WarfarePerMinute[WT] := WarfareRatios[WT] * IronPerMin
    else
      WarfarePerMinute[WT] := WarfareRatios[WT] * LeatherPerMin;

  //Horses require separate calculation
  WarfarePerMinute[wtHorse] := demands[gtMounted] * (  GetUnitRatio(utKnight) * IronPerMin
                                            + GetUnitRatio(utScout) * LeatherPerMin);

  //Update warfare needs accordingly
  fBalance.SetArmyDemand(WarfarePerMinute);
end;


procedure TKMayor.CheckAutoRepair;
var
  I: Integer;
begin
  with gHands[fOwner] do
  begin
    // Change repair mode for all houses only for rmRepairNever and rmRepairAlways
    if IsComputer and (fSetup.RepairMode in [rmRepairNever, rmRepairAlways]) then
      for I := 0 to Houses.Count - 1 do
      begin
        Houses[I].BuildingRepair := fSetup.IsRepairAlways;
        if fSetup.AutoBuild then
          if Houses[I].CanMakeUpgrade then
            if HouseConnectedToStore(Houses[I]) or (Houses[I].HouseType in WALL_HOUSES) then
              Houses[I].MakeUpgrade;

      end;

  end;
end;

function TKMayor.WareFromMinCount(aWare : TKMWareType) : Integer;
begin
  case aWare of
    wtTimber,
    wtStone : Result := 100;
    wtTile : Result := 30;

    wtWine : Result := IfThen(HasHouses([htVineyard]), 50, 500);
    wtBread,
    wtSausage,
    wtFish : Result := IfThen(HasHouses([htBakery, htFishermans, htButchers]), 100, 500);

    wtApple : Result := IfThen(HasHouses([htAppleTree]), 30, 150);//apple is only for some houses

    WARFARE_MIN..WARFARE_MAX : Result := 50;
    wtWater : Result := 50;// water is very important for many houses
    wtIron: Result := IfThen(HasHouses([htWeaponSmithy, htArmorSmithy, htIronFoundry]), 50, 0);
    wtGold,
    wtCoal,
    wtGoldOre,
    wtIronOre : Result := 200;
    wtJewerly : Result := IfThen(HasHouses([htPalace]), 1, 0);
    wtBitinE : Result := 30;
    wtBitin : Result := 20;
    wtSteelE : Result := 100;
    wtEgg : Result := 0;

    else Result := 30;
  end;
end;

function TKMayor.WareToMaxCount(aWare : TKMWareType) : Integer;
begin
  case aWare of
    wtTimber,
    wtStone : Result := 30;
    wtTile : Result := 10;

    wtBread,
    wtWine,
    wtSausage,
    wtFish : Result := 50;
    wtApple : Result := 20;

    WARFARE_MIN..WARFARE_MAX : Result := 5;
    wtWater : Result := 1;

    wtGold : Result := IfThen(HasHouses([htMetallurgists]), 20, 50);
    wtIron,
    wtBolt,
    wtCoal,
    wtGoldOre,
    wtIronOre,
    wtWheel,
    wtLog,
    wtStoneBolt,
    wtPig,
    wtSawDust,
    wtBitinE,
    wtBitinOre,
    wtSteelE : Result := 10;
    wtBitin : Result := 5;
    wtBoots : Result := 5;
    wtQuiver,
    wtTrunk : Result := 20;
    wtVegetables,
    wtLeather,
    wtFlour,
    wtSeed,
    wtCorn : Result := 20;
    else Result := 0;
  end;
end;

function TKMayor.NeedsWare(aWare : TKMWareType) : Boolean;

  function HasFood(aCount : integer) : Boolean;
  begin
    Result :=  (gHands[fOwner].Stats.Wares[wtWine].ActualCnt
              +gHands[fOwner].Stats.Wares[wtBread].ActualCnt
              +gHands[fOwner].Stats.Wares[wtSausage].ActualCnt
              +gHands[fOwner].Stats.Wares[wtFish].ActualCnt) >= aCount;
  end;

  function HasWare(aWare : TKMWareType; aCount : integer) : Boolean;
  begin
    Result :=  gHands[fOwner].Stats.Wares[aWare].ActualCnt >= aCount;
  end;

begin
  Result := false;
  case aWare of
    wtNone: ;

    wtTrunk: ;
    wtStone: Result := (fSetup.AutoBuild)
                        or HasHouses([htStoneWorkshop]);
    wtTimber: Result := fSetup.AutoBuild or HasHouses([htArmorWorkshop, htWeaponWorkshop]);
    wtIronOre: Result := HasHouses([htIronSmithy]);
    wtGoldOre: Result := HasHouses([htMetallurgists]);
    wtCoal: Result := HasHouses([htMetallurgists, htIronSmithy, htWeaponSmithy, htArmorSmithy, htWoodBurner]);
    wtIron: Result := HasHouses([htWeaponSmithy, htArmorSmithy, htIronFoundry]);

    wtWine: Result := not HasFood(400) and not HasHouses([htVineyard]);
    wtBread: Result := not HasFood(400) and not HasHouses([htBakery]);
    wtFish: Result := not HasFood(400) and not HasHouses([htFishermans]);
    wtSausage : Result := (not HasFood(400) or HasHouses([htCollectors])) and not HasHouses([htButchers]);

    wtGold: Result := HasHouses([htSchool, htTownhall, htPalace]){ and not HasHouses([htMertallurgist])};
    wtCorn: Result := HasHouses([htStables, htSwine, htWoodBurner]) and not HasHouses([htFarm]);
    wtFlour: Result := HasHouses([htBakery]) and not HasHouses([htMill]);
    wtLeather: Result := HasHouses([htArmorWorkshop, htTailorsShop]) and not HasHouses([htTannery]);
    wtPig: Result := HasHouses([htButchers]) and not HasHouses([htSwine]);
    wtSkin: Result := HasHouses([htTannery, htShipYard]);

    wtWoodenShield: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utAxeFighter, utScout])
                        and HasHouses([htBarracks]);
    wtIronShield: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utSwordFighter, utKnight])
                        and HasHouses([htBarracks]);
    wtLeatherArmor: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0)or (fArmyDemand[gtRanged] > 0)or (fArmyDemand[gtAntiHorse] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utAxeFighter, utScout, utLanceCarrier, utBowMan])
                        and HasHouses([htBarracks]);
    wtIronArmor: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0)or (fArmyDemand[gtRanged] > 0)or (fArmyDemand[gtAntiHorse] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utSwordFighter, utKnight, utPikeman, utCrossBowMan])
                        and HasHouses([htBarracks]);
    wtAxe: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utAxeFighter, utScout])
                        and HasHouses([htBarracks]);
    wtSword: Result := ((fArmyDemand[gtMelee] > 0) or (fArmyDemand[gtMounted] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utSwordFighter, utKnight])
                        and HasHouses([htBarracks]);
    wtLance: Result := ((fArmyDemand[gtAntiHorse] > 0) or (fArmyDemand[gtMounted] > 0))
                              and gHands[fOwner].Locks.UnitsUnlocked([utLanceCarrier])
                        and HasHouses([htBarracks]);
    wtPike: Result := (fArmyDemand[gtAntiHorse] > 0)
                              and gHands[fOwner].Locks.UnitsUnlocked([utPikeman])
                        and HasHouses([htBarracks]);
    wtBow: Result := (fArmyDemand[gtRanged] > 0)
                              and gHands[fOwner].Locks.UnitsUnlocked([utBowMan])
                        and HasHouses([htBarracks]);
    wtCrossbow: Result := (fArmyDemand[gtRanged] > 0)
                              and gHands[fOwner].Locks.UnitsUnlocked([utCrossBowMan])
                        and HasHouses([htBarracks]);
    wtHorse: Result := (fArmyDemand[gtMounted] > 0)
                        and gHands[fOwner].Locks.UnitsUnlocked([utScout, utKnight])
                        and HasHouses([htBarracks]);
    wtBitin: Result := HasHouses([htIronFoundry]);
    wtVegetables: Result := HasHouses([htSwine, htStables, htHovel]);
    //wtBitinOre: Result := gHands[fOwner].Stats.GetHouseQty([htIronSmithy]) > 0;  //don't trade bitin ore
    wtStoneBolt: Result := HasUnits([utRogue, utCatapult]) or HasHouses([htWatchTower]);

    wtBitinE: Result := ((fArmyDemand[gtMachines] > 0) or (fArmyDemand[gtMachinesMelee] > 0) or (fArmyDemand[gtShips] > 0))
                        and gHands[fOwner].Locks.UnitsUnlocked([utSwordFighter, utCrossbowman, utPikeman, utKnight, utCatapult, utBallista, utWarrior, utRam, utBattleShip])
                        and HasHouses([htSiegeWorkshop, htBarracks, htTownhall, htShipyard]);

    wtSteelE,
    wtLog: Result := ((fArmyDemand[gtMachines] > 0) or (fArmyDemand[gtMachinesMelee] > 0) or (fArmyDemand[gtShips] > 0))
                        and gHands[fOwner].Locks.UnitsUnlocked([utBallista, utCatapult, utRam, utBattleShip])
                        and not HasWare(aWare, 20)//no need to make a lot of it
                        and HasHouses([htSiegeWorkshop, htShipyard]);

    wtWheel: Result := ((fArmyDemand[gtMachines] > 0) or (fArmyDemand[gtMachinesMelee] > 0))
                        and gHands[fOwner].Locks.UnitsUnlocked([utBallista, utCatapult, utRam])
                        and not HasWare(aWare, 20)//no need to make a lot of it
                        and HasHouses([htSiegeWorkshop]);
    wtBolt: Result :=  HasUnits([utBallista, utBattleShip])
                        and not HasWare(aWare, 20);//no need to make a lot of it

    wtQuiver: Result := (HasUnits([utBowman, utCrossBowMan])
                        or HasHouses([htWallTower]))
                        and not HasWare(aWare, 20);//no need to make a lot of it

    wtWater: Result := HasHouses([htHovel, htBakery, htStables, htSwine, htAppletree]) and not HasHouses([htWell]);
    wtTile: Result := fSetup.AutoBuild;
    wtSeed: Result := HasHouses([htHovel, htMill]);
    wtSawDust: Result := HasHouses([htWoodBurner]);
    wtApple: Result := HasHouses([htCottage, htHouse, htPalace]);
    wtBoots: Result := HasHouses([htSchool, htPalace]);
    else
      Result := false;
  end;
end;


procedure TKMayor.CheckMarketTrades;
  function WarePortions(aWare : TKMWareType) : Word;
  begin
    case aWare of
      wtGold: Result := 20;
      wtBread,
      wtWine: Result := 15;
      wtSausage,
      wtFish: Result := 5;
      wtStone: Result := 50;
      wtTimber: Result := 10;
      wtTile: Result := 5;
      wtGoldOre,
      wtCoal: Result := 5;
      else Result := 1;
    end;
  end;
var I, J : Integer;
  HM : TKMHouseMarket;
  WT, W, toW : TKMWareType;
  Wares : array[WARE_MIN..WARE_MAX] of Integer;
  WaresTaken : array[WARE_MIN..WARE_MAX] of Integer;
begin
  //Exit;
  If gHands[fOwner].Houses.Markets.Count = 0 then
    Exit;
  for WT := WARE_MIN to WARE_MAX do
  begin
    Wares[WT] := gHands[fOwner].Stats.Wares[WT].ActualCnt;
    WaresTaken[WT] := Wares[WT];
  end;

  for I := 0 to gHands[fOwner].Houses.Markets.Count - 1 do
  begin
    HM := TKMHouseMarket(gHands[fOwner].Houses.Markets[I]);
    if (HM = nil) or HM.IsDestroyed or not HM.IsComplete then
      Continue;
    Try

      if (HM.WareOrder[0] > 0) then
      begin
        Inc(WaresTaken[HM.ResTo], HM.RatioTo * HM.WareOrder[0]);
        //WaresTaken := WaresTake + [HM.ResTo];
        Dec(Wares[HM.ResFrom], HM.RatioFrom * HM.WareOrder[0]);
      end;
    except

    End;

  end;


  for I := 0 to gHands[fOwner].Houses.Markets.Count - 1 do
  begin
    HM := TKMHouseMarket(gHands[fOwner].Houses.Markets[I]);
    if (HM = nil) or HM.IsDestroyed or not HM.IsComplete then
      Continue;
    try
      if (HM.WareOrder[0] > 0) then
      begin
        Continue;
      end;

      for WT := WARE_MIN to WARE_MAX do
        Inc(WaresTaken[WT], HM.CheckWareOut(WT));

      W := wtNone;
      toW := wtNone;

      for J := 0 to high(WARE_MARKET_IMPORTANCE) do
      begin
        WT := WARE_MARKET_IMPORTANCE[J];
        if WT = W then
          Continue;
        if not gHands[fOwner].Locks.AllowToTrade[WT] then
          Continue;
        if WaresTaken[WT] > WareToMaxCount(WT) then  //don't trade this ware if we have enough
          Continue;
        {if gHands[fOwner].Stats.Wares[WT].ActualCnt > WareToMaxCount(WT) then
          Continue;}
        if not NeedsWare(WT) then //don't trade this ware if we don't need it
          Continue;

        toW := WT;
        Break;
      end;
      if toW = wtNone then
        Continue;

      //get best ware
      for WT := WARE_MIN to WARE_MAX do
        if gHands[fOwner].Locks.AllowToTrade[WT] then
          if (gRes.Wares.RatioFrom(WT, toW) <= 20) {and (gRes.Wares.RatioTo(WT, toW) < 200)} then
            if HM.CheckWareOut(WT) = 0 then
              if (Wares[WT] > WareFromMinCount(WT)) then //take only resources that has min count
                if (W = wtNone) or ((W <> wtNone) and ((Wares[WT] * gRes.Wares[WT].MarketPrice) > (Wares[W] * gRes.Wares[W].MarketPrice) )) then
                  W := WT;

      if W = wtNone then
        Continue;

      HM.ResFrom := W;
      HM.ResTo := toW;
      HM.WareOrder[0] := Max(WarePortions(toW) div HM.RatioTo, 1);
      Inc(WaresTaken[toW], WarePortions(toW));
      {
      if toW = wtGold then
        HM.WareOrder[0] := 3
      else
      if HM.RatioTo < 10 then
        HM.WareOrder[0] := 1;  }

      Dec(Wares[W], HM.RatioFrom);
    except

    end;
  end;
end;

procedure TKMayor.CheckSilos;
var I, J : Integer;
  Silos, hList : TList<TKMHouse>;
  housesCnt : array[HOUSE_MIN..HOUSE_MAX] of Byte;
  HT, HT2 : TKMHouseType;
  WT : TKMWareType;
begin
  Silos := TList<TKMHouse>.Create;
  hList := TList<TKMHouse>.Create;
  if gHands[fOwner].Houses.GetHousesByType([htSmallStore], Silos) then
  begin


    for I := 0 to Silos.Count - 1 do
      if not Silos[I].WareSlotChangedByAI then
      begin
        gHands[fOwner].Houses.GetHousesInRect(KMRectGrow(Silos[I].Entrance, 15),hList);
        if hList.Count = 0 then
          Continue;

        for HT := HOUSE_MIN to HOUSE_MAX do
          housesCnt[HT] := 0;//clear

        for J := 0 to hList.Count - 1 do
          Inc(housesCnt[hList[J].HouseType]);

        HT2 := htNone;
        for HT := HOUSE_MIN to HOUSE_MAX do
          if not (HT in [htSmallStore, htStore, htTownhall, htPalace, htInn, htSchool, htWell, htWallTower, htWatchTower]) then
            if (housesCnt[HT] > 0) then
              If gRes.Houses[HT].ProducesWares then
                if (HT2 = htNone) or (housesCnt[HT] > housesCnt[HT2]) then  //get type of most houses
                  HT2 := HT;

        if HT2 = htNone then
          Continue;
        WT := wtNone;
        for J := 1 to 4 do
          if not (gRes.Houses[HT2].WareOutput[J] in [wtNone, wtAll, wtFood, wtWarfare]) and Silos[I].HasSlotWithWare(gRes.Houses[HT2].WareOutput[J]) then
          begin
            WT := gRes.Houses[HT2].WareOutput[J];
            Break;
          end;

        if WT = wtNone then
          Continue;

        Silos[I].SetWareSlot(WT);
        Silos[I].WareSlotChangedByAI := true;
      end;
  end;

  Silos.Free;
  hList.Free;
end;

procedure TKMayor.CheckMerchants;
var I, J : Integer;
  Merchants : TList<TKMHouse>;
  WT, W : TKMWareType;
  wareTaken : set of TKMWareType;
begin
  Merchants := TList<TKMHouse>.Create;

  if gHands[fOwner].Houses.GetHousesByType([htMerchant], Merchants) then
  begin
    wareTaken := [];
    //find best ware
    W := wtNone;
    for WT := WARE_MIN to WARE_MAX do
      if not (WT in wareTaken) then
        if gHands[fOwner].Locks.AllowToTrade[WT] then
          if gHands[fOwner].Stats.Wares[WT].ActualCnt > WareFromMinCount(WT) then //take only resources that has min count
            if (W = wtNone) or (gHands[fOwner].Stats.Wares[WT].ActualCnt * gRes.Wares[WT].MarketPrice > gHands[fOwner].Stats.Wares[W].ActualCnt * gRes.Wares[W].MarketPrice) then
              W := WT;


    if W <> wtNone then
      for I := 0 to Merchants.Count - 1 do
        if not Merchants[I].WareSlotChangedByAI then
          if Merchants[I].CanChangeWareInput(false) then
          begin
            Merchants[I].SetWareSlot(W);
            for J := 0 to gHands.Count - 1 do
              TKMHouseMerchant(Merchants[I]).SendToHand[J] := true;
            for J := 1 to 4 do
              If Merchants[I].WareInput[J] <> W then
                Merchants[I].ToggleAcceptWaresIn(Merchants[I].WareInput[J], 999)
              else
                Merchants[I].ToggleAcceptWaresIn(Merchants[I].WareInput[J], -999);

          end else
          if Merchants[I].CheckWareIn(wtAll) >= 10 then
          begin
            Merchants[I].ToggleAcceptWaresIn(wtAll, 999);
            //for J := 0 to gHands.Count - 1 do
            //  TKMHouseMerchant(Merchants[I]).SendToHand[J] := false;
          end;

  end;

  Merchants.Free;
end;

procedure TKMayor.CheckPearl;
var H : TKMHousePearl;
  PT : TKMPearlType;
begin
  If fPearlChecked then
    Exit;
  If gHands[fOwner].Stats.GetHouseQty(htPearl) > 0 then
  begin
    H := TKMHousePearl(gHands[fOwner].Houses.FindHouse(htPearl, 0, 0) );
    If H <> nil then
    begin
      PT := TKMPearlType(KaMRandom( ord(high(TKMPearlType)),  'TKMMayor.CheckPearl') + 1);
      H.SelectType(PT);
      H.ConfirmBuild;
      fPearlChecked := true;
    end;
  end;
end;

function TKMayor.BalanceText: UnicodeString;
begin
  Result := fBalance.BalanceText;
end;


procedure TKMayor.UpdateState(aTick: Cardinal);
begin
  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psAICityCls);
  {$ENDIF}
  try
    Recorder.UpdateState(aTick);//recorder works at every tick
    //Checking mod result against MAX_HANDS causes first update to happen ASAP
    if (aTick + Byte(fOwner)) mod (MAX_HANDS * 4) <> MAX_HANDS then Exit;

    CheckAutoRepair;

    //Train new units (citizens, serfs, workers and recruits) if needed
    CheckUnitCount;

    CheckArmyDemand;
    CheckWeaponOrderCount;
    CheckMarketTrades;
    CheckSilos;
    CheckMerchants;
    CheckPearl;
    if fSetup.AutoBuild then
    begin
      //CheckHouseCount;
      //Manage wares ratios and block stone to Store
      CheckWareFlow;


      {
        //CheckHouseCount;
      //Build more roads if necessary
      if not Recorder.HasRecording then
        CheckRoadsCount;
      }

    end;
  finally
    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psAICityCls);
    {$ENDIF}
  end;
end;


procedure TKMayor.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('AIMayor');
  SaveStream.Write(fOwner);
  SaveStream.Write(fRoadBelowStore);
  SaveStream.Write(fDefenceTowersPlanned);
  fDefenceTowers.SaveToStream(SaveStream);

  SaveStream.Write(WarfareRatios, SizeOf(WarfareRatios));
  SaveStream.Write(fArmyDemand, SizeOf(fArmyDemand));

  fBalance.Save(SaveStream);
  fCityPlanner.Save(SaveStream);
  fPathFindingRoad.Save(SaveStream);
  fPathFindingRoadShortcuts.Save(SaveStream);
  fRecorder.Save(SaveStream);
  SaveStream.Write(fPearlChecked);
end;



procedure TKMayor.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('AIMayor');
  LoadStream.Read(fOwner);
  LoadStream.Read(fRoadBelowStore);
  LoadStream.Read(fDefenceTowersPlanned);
  fDefenceTowers.LoadFromStream(LoadStream);

  LoadStream.Read(WarfareRatios, SizeOf(WarfareRatios));
  LoadStream.Read(fArmyDemand, SizeOf(fArmyDemand));

  fBalance.Load(LoadStream);
  fCityPlanner.Load(LoadStream);
  fPathFindingRoad.Load(LoadStream);
  fPathFindingRoadShortcuts.Load(LoadStream);
  fRecorder.Load(LoadStream);
  LoadStream.Read(fPearlChecked);
end;


end.
