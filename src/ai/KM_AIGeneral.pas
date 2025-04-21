unit KM_AIGeneral;
{$I KaM_Remake.inc}
interface
uses
  Generics.Collections,
  KM_AISetup, KM_AIAttacks, KM_AIDefensePos,
  KM_Units, KM_UnitGroup, KM_UnitWarrior,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_NavMeshDefences,
  KM_AITypes;


type
  TKMGeneral = class
  private
   // fLastEquippedTimeIron, fLastEquippedTimeLeather, fLastEquippedTimeTH: Cardinal;
    fUnitsEquipOrdered: TDictionary<Integer, TKMUnitWarrior>; //Units for which AIGeneral ordered to equip, but who did not exits the barracks yet

    fOwner: TKMHandID;
    fSetup: TKMHandAISetup;
    fAttacks: TKMAIAttacks;
    fDefencePositions: TAIDefencePositions;
    fDefendPositions: TAIDefendPositions;
    fDontRestockDP: Boolean;
    function HasPatrolAttack : Boolean;
    procedure CheckArmy;
    procedure CheckArmyCount;
    procedure CheckAttacks;
    procedure CheckAutoAttack;
    procedure CheckAutoDefend;
    procedure HaltArmy;
    procedure OrderAttack(aGroup: TKMUnitGroup; aTarget: TKMAIAttackTarget; const aCustomPos: TKMPoint);

    procedure RemoveEquipOrderedWarrior(aWarrior: TKMUnitWarrior);
  public
    fPatrolAttack : Boolean;
    constructor Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit;
    procedure OwnerUpdate(aPlayer: TKMHandID);
    property Attacks: TKMAIAttacks read fAttacks;
    property DefencePositions: TAIDefencePositions read fDefencePositions;
    property DPDontRestock : Boolean read fDontRestockDP write fDontRestockDP;
    property DefendPositions: TAIDefendPositions read fDefendPositions;

    procedure RetaliateAgainstThreat(aAttacker: TKMUnit);
    procedure WarriorEquipped(aWarrior: TKMUnitWarrior);
    procedure WarriorDied(aWarrior: TKMUnitWarrior);

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses
  Classes, Math,
  KM_Entity,
  KM_Game, KM_GameParams,
  KM_Hand, KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_Terrain, KM_AIFields,
  KM_Houses, KM_HouseBarracks,KM_HouseSiegeWorkshop, KM_HouseTownHall,
  KM_ResHouses, KM_CommonUtils, KM_DevPerfLog, KM_DevPerfLogTypes,
  KM_UnitGroupTypes,
  KM_ResTypes, KM_Resource, KM_ResUnits;


const
  //For compatibility with KaM these must be False. We can add a !REMAKE_AI command later
  //to make them more "intelligent", but for now these are required for the campaigns to be playable.

  //On the other hand, no need to reproduce KaM's flaws, so fill closest defence
  //positions for idle groups to avoid swapping on mission start. This shouldn't
  //cause incorrect behaviour in campaigns since either way the priority order
  //of positions will be respected.

  AI_FILL_CLOSEST_IDLE = True;
  AI_FILL_CLOSEST_EQUIPPED = False;
  AI_LINK_IDLE = False;


{ TKMGeneral }
constructor TKMGeneral.Create(aPlayer: TKMHandID; aSetup: TKMHandAISetup);

begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fAttacks := TKMAIAttacks.Create;
  fDefencePositions := TAIDefencePositions.Create(aPlayer);
  fDefendPositions := TAIDefendPositions.Create(aPlayer);
  fUnitsEquipOrdered := TDictionary<Integer, TKMUnitWarrior>.Create;
  //fPatrolAttack := HasPatrolAttack;
  fPatrolAttack := false
end;


destructor TKMGeneral.Destroy;
var
  U, U2: TKMUnit;
begin
  for U in fUnitsEquipOrdered.Values do
  begin
    U2 := U;
    gHands.CleanUpUnitPointer(U2);
  end;

  fUnitsEquipOrdered.Free;
  fDefencePositions.Free;
  fDefendPositions.Free;
  fAttacks.Free;

  inherited;
end;


procedure TKMGeneral.AfterMissionInit;
begin
  fPatrolAttack := HasPatrolAttack;
end;


procedure TKMGeneral.OwnerUpdate(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
end;


procedure TKMGeneral.Save(SaveStream: TKMemoryStream);
var
  uid: Integer;
  keyArray : TArray<Integer>;
begin
  SaveStream.PlaceMarker('AIGeneral');
  SaveStream.Write(fOwner);
  {SaveStream.Write(fLastEquippedTimeIron);
  SaveStream.Write(fLastEquippedTimeLeather);
  SaveStream.Write(fLastEquippedTimeTH);}
  SaveStream.Write(fPatrolAttack);
  SaveStream.Write(fDontRestockDP);
  fAttacks.Save(SaveStream);
  fDefencePositions.Save(SaveStream);
  fDefendPositions.Save(SaveStream);

  SaveStream.PlaceMarker('AIGeneral_unitsEquipOrdered');
  SaveStream.Write(fUnitsEquipOrdered.Count);
  keyArray := fUnitsEquipOrdered.Keys.ToArray;
  TArray.Sort<Integer>(keyArray);

  for uid in keyArray do
    SaveStream.Write(uid);
end;


procedure TKMGeneral.Load(LoadStream: TKMemoryStream);
var
  I, count, uid: Integer;
begin
  LoadStream.CheckMarker('AIGeneral');
  LoadStream.Read(fOwner);
  {LoadStream.Read(fLastEquippedTimeIron);
  LoadStream.Read(fLastEquippedTimeLeather);
  LoadStream.Read(fLastEquippedTimeTH);}
  LoadStream.Read(fPatrolAttack);
  LoadStream.Read(fDontRestockDP);
  fAttacks.Load(LoadStream);
  fDefencePositions.Load(LoadStream);
  fDefendPositions.Load(LoadStream);

  LoadStream.CheckMarker('AIGeneral_unitsEquipOrdered');
  fUnitsEquipOrdered.Clear;
  LoadStream.Read(count);
  for I := 0 to Count - 1 do
  begin
    LoadStream.Read(uid);
    fUnitsEquipOrdered.Add(uid, nil);
  end;
end;


procedure TKMGeneral.SyncLoad;
var
  uid: Integer;
begin
  fDefencePositions.SyncLoad;

  for uid in fUnitsEquipOrdered.Keys do
    fUnitsEquipOrdered[uid] := TKMUnitWarrior(gHands.GetUnitByUID(Integer(uid)));
end;

procedure TKMGeneral.CheckArmyCount;

var
  Barracks: array of TKMHouseBarracks;
  HB: TKMHouseBarracks;
  HTH: TKMHouseTownHall;
  GT: TKMGroupType;
  I,K, J: Integer;
  UT: TKMUnitType;
  GroupReq: TKMGroupTypeArray;
  warrior: TKMUnitWarrior;

  function CanEquipIron: Boolean;
  begin
    {Result := not (fSetup.ArmyType = atLeather)
              AND (fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeIron + fSetup.EquipRateIron div Max(gHands[fOwner].Stats.GetHouseQty(htBarracks), 1)));
     }
     Result := not (fSetup.ArmyType = atLeather)
              AND (fSetup.UnlimitedEquip or gGame.CheckTime(HB.EquipTimeI + fSetup.EquipRateIron div Max(gHands[fOwner].Stats.GetHouseQty(htBarracks), 1)));

  end;

  function CanEquipTH(aUnit : TKMUnitType): Boolean;
  begin

    Result := (not (fSetup.ArmyType = atIron) and not (aUnit in WARRIORS_IRON))
              or (not (fSetup.ArmyType = atLeather) and (aUnit in WARRIORS_IRON));


    Result := Result and (fSetup.UnlimitedEquip or gGame.CheckTime(HTH.EquipTime + fSetup.EquipRateTH));
  end;

  function CanEquipLeather: Boolean;
  begin
    {
    Result := not (fSetup.ArmyType = atIron)
              AND (fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeLeather + fSetup.EquipRateLeather div Max(gHands[fOwner].Stats.GetHouseQty(htBarracks), 1) ));
    }
    Result := not (fSetup.ArmyType = atIron)
          AND (fSetup.UnlimitedEquip or gGame.CheckTime(HB.EquipTimeL + fSetup.EquipRateLeather ));

  end;

  procedure EquipMachines;
  var I, J : Integer;
    HSW : TKMHouseSiegeWorkshop;
    UT2 : TKMUnitType;
    hasAdded : Boolean;
  begin
    for I := 0 to gHands[fOwner].Houses.SiegeWorkshops.Count - 1 do
    begin
      HSW := TKMHouseSiegeWorkshop(gHands[fOwner].Houses.SiegeWorkshops[I]);
      if HSW = nil then
        Continue;
      if HSW.IsDestroyed then
        Continue;
      if not HSW.IsComplete then
        Continue;

      if HSW.Queue[1] <> utNone then
        Continue;

      hasAdded := false;

      if GroupReq[gtMachines] > 0 then
        for J := 0 to high(AI_TROOP_TRAIN_ORDER_NEW[gtMachines]) do
        begin
          UT2 := AI_TROOP_TRAIN_ORDER_NEW[gtMachines][J];
          if not gHands[fOwner].Locks.UnitUnlocked(UT2) then
            Continue;
          if not (htSiegeWorkshop in gRes.Units[UT2].TrainingHouses) then
            Continue;

          if KamRandom(100, 'Add siege machines to queue') >= 100 / length(SIEGE_GAME_ORDER) then
            Continue;

          HSW.AddUnitToQueue(UT2, 1);
          hasAdded := true;
          Break;
        end;

      if not hasAdded then
        if GroupReq[gtMachinesMelee] > 0 then
          for J := 0 to high(AI_TROOP_TRAIN_ORDER_NEW[gtMachinesMelee]) do
          begin
            UT2 := AI_TROOP_TRAIN_ORDER_NEW[gtMachinesMelee][J];
            if not gHands[fOwner].Locks.UnitUnlocked(UT2) then
              Continue;
            if not (htSiegeWorkshop in gRes.Units[UT2].TrainingHouses) then
              Continue;
            if KamRandom(100, 'Add siege machines to queue') >= 100 / length(SIEGE_GAME_ORDER) then
              Continue;

            HSW.AddUnitToQueue(UT2, 1);
            Break;
          end;

    end;
  end;

  procedure RemoveMachines;
  var I : Integer;
    HSW : TKMHouseSiegeWorkshop;
  begin

    for I := 0 to gHands[fOwner].Houses.SiegeWorkshops.Count - 1 do
    begin
      If gHands[fOwner].Houses.SiegeWorkshops[I].HouseType <> htSiegeWorkshop then
        Exit;
      HSW := TKMHouseSiegeWorkshop(gHands[fOwner].Houses.SiegeWorkshops[I]);

      if not HSW.IsValid then
        Continue;
      if HSW.IsDestroyed then
        Continue;
      HSW.RemUnitFromQueue(1);
      HSW.RemUnitFromQueue(2);
      HSW.RemUnitFromQueue(3);
      HSW.RemUnitFromQueue(4);
      HSW.RemUnitFromQueue(5);

    end;
  end;

  procedure EquipFromPalace;
  var I, J, K : Integer;
    HP : TKMHousePalace;
    UT2 : TKMUnitType;
    HasOrders : Boolean;
  begin

    for I := 0 to gHands[fOwner].Houses.Palaces.Count - 1 do
    begin
      HP := TKMHousePalace(gHands[fOwner].Houses.Palaces[I]);

      if not HP.IsValid(htAny, false, true) then
        Continue;

      if HP.IsTraining then
        Continue;
      HasOrders := false;
      for J := 0 to High(PALACE_UNITS_ORDER) do
        if (HP.Orders[J] > 0) then
        begin
          HasOrders := true;
          Break;
        end;
      if HasOrders then
        Continue;
      //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
      K := 0;
      repeat
        GT := TKMGroupType(GROUP_TYPE_MIN_OFF + KaMRandom(GROUP_TYPES_CNT, 'TKMGeneral.CheckArmyCount')); //Pick random from overall count
        Inc(K);
      until (GroupReq[GT] > 0) or (K > 9); //Limit number of attempts to guarantee it doesn't loop forever

      if (GroupReq[GT] > 0) then
        for J := low(PALACE_UNITS_ORDER) to High(PALACE_UNITS_ORDER) do
        begin
          UT2 := PALACE_UNITS_ORDER[J];

          if UNIT_TO_GROUP_TYPE[UT2] <> GT then
            Continue;
          if not gHands[fOwner].Locks.UnitUnlocked(UT2) then
            Continue;
          if UT2 = utSpy then
            Continue;
          if (HP.Orders[J] > 0) then
            Continue;

          if HP.FullProgress[J] > 0 then
          begin
            Dec(GroupReq[GT]);
            Continue;
          end;

          if HP.IsTraining then
            Continue;

          if not HP.CanEquip(J) then
            Continue;

          HP.Orders[J] := 1;
          Break;//Train only one unit
        end;
    end;
  end;

  procedure EquipShips(aStopBuilding : Boolean);
  var I : Integer;
      houses : TKMArray<TKMHouse>;
      HS : TShipYard;
  begin
    houses := gHands[fOwner].Houses.GetHouses(htShipYard);
    for I := 0 to houses.Count - 1 do
    begin
      HS := TShipYard(houses[I]);

      HS.NextShipType := utBattleShip;//only produce battle ships. There is no need to have anything else
      HS.DoWork := not aStopBuilding;
    end;

  end;

var wP : Pointer;
begin
  if gGame.IsPeaceTime then Exit; //Do not train soldiers during peacetime

  //Don't train if we have reached our limit
  if (fSetup.MaxSoldiers <> -1) and (gHands[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers) then
    Exit;
  if gGame.Params.Tick < fSetup.RecruitDelay then Exit; //Do not train soldiers when there is recruit delay

  //if not CanEquipIron and not CanEquipLeather and not CanEquipTH  then Exit;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  for I := 0 to fDefencePositions.Count - 1 do
    with fDefencePositions[I] do
    if CurrentGroup = nil then
      Inc(GroupReq[GroupType], fDefencePositions.TroopFormations[GroupType].NumUnits)
    else
      Inc(GroupReq[GroupType], Max(fDefencePositions.TroopFormations[GroupType].NumUnits - CurrentGroup.Count, 0));

  //If we don't need anyone - Exit
  I := 0;
  for GT := Low(GroupReq) to High(GroupReq) do
    Inc(I, GroupReq[GT]);

  if (GroupReq[gtMachines] = 0) and (GroupReq[gtMachinesMelee] = 0)  then
    RemoveMachines;
  EquipShips(GroupReq[gtShips] = 0);

  if I = 0 then
    Exit;

  if (GroupReq[gtMachines] > 0) or (GroupReq[gtMachinesMelee] > 0) then
    EquipMachines;
  // Decrease group requirements on number of soldiers that are not walked out of barracks yet
  for warrior in fUnitsEquipOrdered.Values do
    if not warrior.IsDeadOrDying then
      // Group req could change while warrior was walking out of barracks, so we could get negative value here
      GroupReq[UNIT_TO_GROUP_TYPE[warrior.UnitType]] := Max(0, GroupReq[UNIT_TO_GROUP_TYPE[warrior.UnitType]] - 1);

  EquipFromPalace;

  //Train troops where possible in each barracks
  if gHands[fOwner].Houses.Barracks.Count > 0 then
    for I := 0 to gHands[fOwner].Houses.Barracks.Count - 1 do
    begin
      HB := TKMHouseBarracks(gHands[fOwner].Houses.Barracks[I]);
      if (HB = nil) or HB.IsDestroyed or (not HB.IsComplete) then
        Continue;
      //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
      K := 0;
      repeat
        GT := GROUP_TYPE_ORDER[KaMRandom(Length(GROUP_TYPE_ORDER), 'TKMGeneral.CheckArmyCount')]; //Pick random from overall count
        Inc(K);
      until (GroupReq[GT] > 0) or (K > 9); //Limit number of attempts to guarantee it doesn't loop forever

      if GroupReq[GT] = 0 then
        Break; // Don't train

      for K := Low(AI_TROOP_TRAIN_ORDER_NEW[GT]) to High(AI_TROOP_TRAIN_ORDER_NEW[GT]) do
      begin
        UT := AI_TROOP_TRAIN_ORDER_NEW[GT, K];

        if not (htBarracks in  gRes.Units[UT].TrainingHouses) then
          Continue;
        try
          if (UT <> utNone) then
            while ((CanEquipIron and (UT in WARRIORS_IRON)) or (CanEquipLeather and not (UT in WARRIORS_IRON)))
            and (GroupReq[GT] > 0)
            and ((fSetup.MaxSoldiers = -1) or (gHands[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers)) do
            begin
              If not HB.CanEquip(UT) then
                Break;
              wP := HB.EquipWarrior(UT);
              if (wP = nil) or (TKMUnit(wP) = nil) then
                Break;
              warrior := TKMUnitWarrior(wP);
              if warrior = nil then //We cant make unit of this type
                Break; //Continue with next UnitType

              //Save unit we just equipped in the list of units, that has to be equipped
              //Problem is that we could make several orders before all equipped units will exit the barracks
              //and register in the defence position groups
              //So we will order more units in that case, who will eventually exit the barracks and chill around,
              //because all defence groups are filled already
              fUnitsEquipOrdered.Add(warrior.UID, TKMUnitWarrior(warrior.GetPointer));

              Dec(GroupReq[GT]);
              //Only reset it when we actually trained something (in IronThenLeather mode we don't count them separately)

              if (UT in WARRIORS_IRON) or (fSetup.ArmyType = atIronThenLeather) then
                HB.EquipTimeI := gGameParams.Tick;

              if not (UT in WARRIORS_IRON) or (fSetup.ArmyType = atIronThenLeather) then
                HB.EquipTimeL := gGameParams.Tick;
            end;
        except

        end;
      end;
    end;

  //Train troops where possible in each TH
  for I := 0 to gHands[fOwner].Houses.TownHalls.Count - 1 do
  begin
    HTH := TKMHouseTownHall(gHands[fOwner].Houses.TownHalls[I]);
    if (HTH = nil) or HTH.IsDestroyed or (not HTH.IsComplete) then
      Continue;

    if HTH.CheckWareIn(wtGold) < 10 then
      Continue;
    J := 0;
    while (HTH.CheckWareIn(wtGold) > 2) and (J < 5) do
    begin
      Inc(J);
      K := 0;//Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
      repeat
        GT := TKMGroupType(GROUP_TYPE_MIN_OFF + KaMRandom(GROUP_TYPES_CNT, 'TKMGeneral.CheckArmyCount')); //Pick random from overall count
        Inc(K);
      until (GroupReq[GT] > 0) or (K > 9); //Limit number of attempts to guarantee it doesn't loop forever

      if GroupReq[GT] = 0 then
        Break; // Don't train
      for K := Low(AI_TROOP_TRAIN_ORDER_NEW[GT]) to High(AI_TROOP_TRAIN_ORDER_NEW[GT]) do
      begin

        UT := AI_TROOP_TRAIN_ORDER_NEW[GT][K];

        if not (htTownhall in  gRes.Units[UT].TrainingHouses) then
          Continue;

        if not gHands[fOwner].Locks.UnitUnlocked(UT) then
          Continue;

        if (UT <> utNone) then
          if HTH.CanEquip(UT)
            and (CanEquipTH(UT))
            and (GroupReq[GT] > 0)
            and ((fSetup.MaxSoldiers = -1) or (gHands[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers)) then
            begin
              Dec(GroupReq[GT], HTH.Equip(UT, 1));
              HTH.EquipTime := gGameParams.Tick;
            end;
      end;
    end;
  end;

end;


//Check army food level and positioning
procedure TKMGeneral.CheckArmy;
var
  I: Integer;
  GroupType: TKMGroupType;
  Group: TKMUnitGroup;
  NeedsLinkingTo: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMUnitGroup;
begin
  for GroupType := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    NeedsLinkingTo[GroupType] := nil;

  //Check: Hunger, (feed) formation, (units per row) position (from defence positions)
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
  begin
    Group := gHands[fOwner].UnitGroups[I];

    if not Group.IsDead
      and not Group.InFight(True)
      and (Group.Condition < UNIT_MIN_CONDITION) then
      //Check hunger and order food
      //Cheat for autobuild AI: Only feed hungry group members (food consumption lower and more predictable)
      //Do not clear offenders here, since we could follow attacker
      Group.OrderFood(False, fSetup.AutoBuild);

    if not Group.IsDead
      and Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder])
      and not gGame.IsPeaceTime then //Do not process attack or defence during peacetime
    begin
      //We already have a position, finished with this group
      if fDefencePositions.FindPositionOf(Group) <> nil then Continue;

      //Look for a new position to defend
      //In this case we choose the closest group, then move to a higher priority one later (see above)
      //This means at the start of the mission troops will take the position they are placed at rather than swapping around
      //With auto defence we reset defence positions regularly, so take closest rather than reshuffling all the time (newly equipped warriors still take highest priority)

      if fDefencePositions.FindPlaceForGroup(Group, AI_FILL_CLOSEST_IDLE or fSetup.AutoDefend) then Continue;

      //Just chill and link with other idle groups
      if AI_LINK_IDLE then
      begin
        GroupType := Group.GroupType; //Remember it because Group might get emptied
        if NeedsLinkingTo[GroupType] = nil then
        begin
          //If this group doesn't have enough members
          if (Group.Count < fDefencePositions.TroopFormations[GroupType].NumUnits) then
            NeedsLinkingTo[GroupType] := Group //Flag us as needing to be added to
        end
        else
        begin
          //Look for group that needs additional members
          fDefencePositions.RestockPositionWith(NeedsLinkingTo[GroupType], Group);
          if NeedsLinkingTo[GroupType].Count >= fDefencePositions.TroopFormations[GroupType].NumUnits then
            NeedsLinkingTo[GroupType] := nil; //Group is now full
        end;
      end;
    end;
  end;
end;


procedure TKMGeneral.CheckAttacks;
var
  MenAvailable: TKMGroupTypeArray; //Total number of warriors available to attack the enemy
  GroupsAvailable: TKMGroupTypeArray;
  MaxGroupsAvailable: Integer;
  AttackGroups: array [gtAny..GROUP_TYPE_MAX] of array of TKMUnitGroup;

  procedure AddAvailable(aGroup: TKMUnitGroup);
  var
    I : Integer;
    GT: TKMGroupType;
  begin
    GT := aGroup.GetMembersGroupType; //gtAny is only when there is no other unit that has different group types than gtAny
    if not (GT in GROUP_TYPES_VALID) then
      Exit;
    if GroupsAvailable[GT] >= Length(AttackGroups[GT]) then
      SetLength(AttackGroups[GT], GroupsAvailable[GT] + 10);

    AttackGroups[GT, GroupsAvailable[GT]] := aGroup;
    Inc(GroupsAvailable[GT]);
    MaxGroupsAvailable := Max(MaxGroupsAvailable, GroupsAvailable[GT]);

    //AttackGroups[GT, GroupsAvailable[GT]] := aGroup;
    //Inc(GroupsAvailable[GT]);
    //MaxGroupsAvailable := Max(MaxGroupsAvailable, GroupsAvailable[GT]);

    //check every member since there can be gtAny type
    for I := 0 to aGroup.Count - 1 do
    begin
      GT := UNIT_TO_GROUP_TYPE[aGroup.Members[I].UnitType];
      if GT = gtNone then
        Continue;
      Inc(MenAvailable[GT]);
    end;
  end;

  function CanMakePatrolAttack : Boolean;
  var I : Integer;
  begin
    Result := true;

    for I := fDefencePositions.Count-1 downto 0 do
      if (fDefencePositions[I].DefenceType = dtBackLine) then
      If not (fDefencePositions[I].CurrentGroup.IsIdle and (fDefencePositions[I].CurrentGroup.Position = fDefencePositions[I].PositionPatrol.Loc)) then
        Exit(false);
      {begin
        G := fDefencePositions[I].CurrentGroup;
        if not (G.Posi = fDefencePositions[I].PositionPatrol.Loc) then
          Exit(false);

      end;}

  end;

  function HasAllPatrolGroups : Boolean;
  var I : Integer;
  begin
    Result := true;
    for I := fDefencePositions.Count-1 downto 0 do
      if (fDefencePositions[I].DefenceType = dtBackLine) then
        if not ((fDefencePositions[I].CurrentGroup <> nil) //defence pos has group
        and not fDefencePositions[I].CurrentGroup.IsDead
        and (fDefencePositions[I].CurrentGroup.IsIdle or fDefencePositions[I].IsOnPatrol)//group is idle
        and not (fDefencePositions[I].CurrentGroup.GetMembersGroupType in [gtAny, gtNone])
        and (fDefencePositions[I].CurrentGroup.Count >= fDefencePositions.TroopFormations[fDefencePositions[I].CurrentGroup.GetMembersGroupType].NumUnits)) then  //group is full
          Exit(false);
  end;

var
  I, K, J: Integer;
  G: TKMGroupType;
  Group: TKMUnitGroup;
  DP: TAIDefencePosition;
  UnitsSent: Integer;
  AttackLaunched: Boolean;
  Looped: Integer;
begin
  //Do not process attack or defence during peacetime
  if gGame.IsPeaceTime then Exit;

  if fPatrolAttack then
  begin
    //1. Take all idling Groups that are not linked to any Defence positions
    //2. Make attack until they die
    for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
    begin
      Group := gHands[fOwner].UnitGroups[I];
      if not Group.IsDead
        and Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder]) then
      begin
        DP := fDefencePositions.FindPositionOf(Group);
        if DP = nil then
        begin
          OrderAttack(Group, Attacks[0].Target, KMPoint(0, 0));
          Group.IsOnPatrolAttack := true;
        end;
      end;
    end;


    //First check if Every defence pos has it's group and if it's full
    If HasAllPatrolGroups then
    begin
      for K := fDefencePositions.Count - 1 downto 0 do
        if (fDefencePositions[K].DefenceType = dtBackLine) then
        begin
          fDefencePositions[K].CurrentGroup.OrderWalk(fDefencePositions[K].PositionPatrol.Loc, true, wtokAIGotoDefencePos, fDefencePositions[K].PositionPatrol.Dir);
          fDefencePositions[K].IsOnPatrol := true; //change
        end;

      if CanMakePatrolAttack then
        for K := fDefencePositions.Count - 1 downto 0 do
          if (fDefencePositions[K].DefenceType = dtBackLine) then
          begin
            OrderAttack(fDefencePositions[K].CurrentGroup, Attacks[0].Target, KMPoint(0, 0));
            fDefencePositions[K].IsOnPatrol := false; //change
            fDefencePositions[K].CurrentGroup.IsOnPatrolAttack := true;
            fDefencePositions[K].CurrentGroup := nil; //remove this group from def pos
          end;


    end else
    begin
      for K := 0 to fDefencePositions.Count - 1 do
        fDefencePositions[K].IsOnPatrol := false;
    end;
    Exit;
  end;

  Looped := 0;
  repeat
    AttackLaunched := False;
    MaxGroupsAvailable := 0;
    for G := gtAny to GROUP_TYPE_MAX do
    begin
      GroupsAvailable[G] := 0;
      MenAvailable[G] := 0;
    end;

    //Order of units is prioritized:
    //1. Take all idling Groups that are not linked to any Defence positions
    for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
    begin
      Group := gHands[fOwner].UnitGroups[I];
      if not Group.IsDead
        and Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder]) then
      begin
        DP := fDefencePositions.FindPositionOf(Group);
        if DP = nil then
          AddAvailable(Group);
      end;
    end;
    //2. Take back line defence positions, lowest priority first
    for I := fDefencePositions.Count-1 downto 0 do
      if (fDefencePositions[I].DefenceType = dtBackLine)
      and (fDefencePositions[I].CurrentGroup <> nil)
      and not fDefencePositions[I].CurrentGroup.IsDead
      and fDefencePositions[I].CurrentGroup.IsIdleToAI([wtokFlagPoint, wtokHaltOrder, wtokAIGotoDefencePos]) then
        AddAvailable(fDefencePositions[I].CurrentGroup);

    //Now process AI attacks (we have compiled a list of warriors available to attack)
    for I := 0 to Attacks.Count - 1 do
    if Attacks.CanOccur(I, MenAvailable, GroupsAvailable, gGameParams.Tick) then //Check conditions are right
    begin
      AttackLaunched := True;
      //Order groups to attack
      UnitsSent := 0;
      if Attacks[I].RandomGroups then
      begin
        //Repeatedly send one of each group type until we have sent the required amount (mixed army)
        for K := 0 to MaxGroupsAvailable - 1 do
          for G := gtAny to GROUP_TYPE_MAX do
            if (UnitsSent < Attacks[I].TotalMen) and (K < GroupsAvailable[G]) then
            begin
              OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
              Inc(UnitsSent, AttackGroups[G, K].Count);
            end;
      end
      else
      begin

        //First send the number of each group as requested by the attack
        for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
          for K := 0 to Attacks[I].GroupAmounts[G] - 1 do
          begin
            OrderAttack(AttackGroups[G, K], Attacks[I].Target, Attacks[I].CustomPosition);
            Inc(UnitsSent, AttackGroups[G, K].Count);
          end;

        //send all gtAny groups first
        for K := 0 to high(AttackGroups[gtAny]) do
          if UnitsSent < Attacks[I].TotalMen then
          begin
            OrderAttack(AttackGroups[gtAny, K], Attacks[I].Target, Attacks[I].CustomPosition);
            Inc(UnitsSent, AttackGroups[gtAny, K].Count);
          end;

        //If we still haven't sent enough men, send more groups out of the types allowed until we have
        if UnitsSent < Attacks[I].TotalMen then
          for K := 0 to MaxGroupsAvailable - 1 do
            for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
            begin
              //Start index after the ones we've already sent above (ones required by attack)
              J := K + Attacks[I].GroupAmounts[G];
              if (Attacks[I].GroupAmounts[G] > 0) and (UnitsSent < Attacks[I].TotalMen)
              and (J < GroupsAvailable[G]) then
              begin
                OrderAttack(AttackGroups[G, J], Attacks[I].Target, Attacks[I].CustomPosition);
                Inc(UnitsSent, AttackGroups[G, J].Count);
              end;
            end;
      end;
      Attacks.HasOccured(I); //We can't set the flag to property record directly
      Break; //Only order 1 attack per update, since all the numbers of groups available need recalculating
    end;
    Inc(Looped);
  //if there's no target available we could loop forever ordering the same attack, so limit it
  until (not AttackLaunched) or (Looped > 10);
end;


procedure TKMGeneral.CheckAutoAttack;
var
  SimpleAttack: TKMAIAttack;
  H: TKMHouse;
begin
  //Simple test for now
  FillChar(SimpleAttack, SizeOf(SimpleAttack), #0);

  SimpleAttack.AttackType := aatRepeating;
  SimpleAttack.Target := attClosestBuildingFromStartPos;
  SimpleAttack.TotalMen := fDefencePositions.AverageUnitsPerGroup *
                           fDefencePositions.GetBacklineCount div 2;
  SimpleAttack.RandomGroups := True;

  Attacks.Clear;
  Attacks.AddAttack(SimpleAttack);

  //If start position isn't set, set it to first storehouse (used for targeting attacks)
  if (fSetup.StartPosition.X <= 1) and (fSetup.StartPosition.Y <= 1) then
  begin
    H := gHands[fOwner].Houses.FindHouse(htStore, 0, 0, 1);
    if H <> nil then
      fSetup.StartPosition := H.Entrance;
  end;

  //See how many soldiers we need to launch an attack

  //Check if we have enough troops we can take into attack (Backline formations)

  //Check if we can train more soldiers (ignoring EquipRate?)

  //Make decision about attack

  //Choose place to attack
end;


procedure TKMGeneral.CheckAutoDefend;

  //function EnsureWalkable(var Loc: TKMPoint): Boolean;
  //var
  //  IX, IY, BestDistSqr: Integer;
  //  Best: TKMPoint;
  //begin
  //  if gTerrain.CheckPassability(Loc, tpWalk) then
  //  begin
  //    Result := True;
  //    Exit;
  //  end;
  //  Result := False;
  //  BestDistSqr := High(Integer);
  //  for IY := Max(1, Loc.Y-2) to Min(gTerrain.MapY, Loc.Y+2) do
  //    for IX := Max(1, Loc.X-2) to Min(gTerrain.MapX, Loc.X+2) do
  //      if gTerrain.CheckPassability(KMPoint(IX, IY), tpWalk)
  //      and (KMLengthSqr(Loc, KMPoint(IX, IY)) < BestDistSqr) then
  //      begin
  //        BestDistSqr := KMLengthSqr(Loc, KMPoint(IX, IY));
  //        Best := KMPoint(IX, IY);
  //        Result := True;
  //      end;
  //  if Result then
  //    Loc := Best;
  //end;

const
  MIN_DEF_POS = 6;
var
  DefPosArr: TKMDefencePosArr;
  I: Integer;
  BestOwner: TKMHandID;
  Loc: TKMPoint;
  GT: TKMGroupType;
  DPT: TKMAIDefencePosType;
  //Outline1, Outline2: TKMWeightSegments;
  //Locs: TKMPointDirTagList;
  //LocI: TKMPoint;
  //FaceDir: TKMDirection;
  //SegLength, Ratio: Single;
  //DefCount: Byte;
  //Weight: Cardinal;
  //FirstLineCount,BacklineCount: Integer;
begin
  //Get defence Outline with weights representing how important each segment is
  if not gAIFields.NavMesh.Defences.FindDefensivePolygons(fOwner, DefPosArr) then
    Exit;

  fDefencePositions.Clear;
  for I := Low(DefPosArr) to High(DefPosArr) do
  begin
    BestOwner := gAIFields.Influences.GetBestOwner(DefPosArr[I].Polygon);
    if (BestOwner = fOwner) OR (BestOwner = HAND_NONE) OR (fDefencePositions.Count + Length(DefPosArr) <= MIN_DEF_POS) then
    begin
      if (DefPosArr[I].Line = 0) then
        DPT := dtFrontLine
      else
        DPT := dtBackLine;
      Loc := DefPosArr[I].DirPoint.Loc;
      case (Loc.X*2 + Loc.Y*2) mod 3 of
        0:   GT := gtAntiHorse;
        1:   GT := gtRanged;
        else GT := gtMelee;
      end;
      fDefencePositions.Add(DefPosArr[I].DirPoint, GT, 25, DPT);
    end;
  end;

  //BacklineCount := 0;
  //FirstLineCount := 0;

  //Locs := TKMPointDirTagList.Create;
  //try
    //Make list of defence positions

    //for I := 0 to High(Outline2) do
    //begin
    //  FaceDir := KMGetDirection(KMPointF(Outline2[I].A), KMPerpendecular(Outline2[I].A, Outline2[I].B));
    //
    //  //Longer segments will get several DefencePositions
    //  SegLength := KMLength(Outline2[I].A, Outline2[I].B);
    //  DefCount := Max(Trunc(SegLength / 5), 1); //At least 1, otherwise we might leave a bridge undefended
    //
    //  for K := 0 to DefCount - 1 do
    //  begin
    //    Ratio := (K + 1) / (DefCount + 1);
    //    Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
    //    Weight := Round(Outline2[I].Weight * 100);
    //    //Make sure each segment gets 1 defence position before filling others (in the middle of the segment line)
    //    if K = ((DefCount - 1) div 2) then
    //      Weight := Weight + 10000;
    //
    //    Locs.Add(KMPointDir(Loc, FaceDir), Weight);
    //  end;
    //end;

    //Sort according to positions weight
    //Locs.SortByTag;

    //Add defence positions
    //for I := Locs.Count - 1 downto 0 do
    //begin
    //  LocI := KMGetPointInDir(Locs.Items[I].Loc, Locs.Items[I].Dir, 1);
    //  Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    //  if not EnsureWalkable(Loc) then
    //    Continue;
    //
    //  //Mix group types deterministicly based on Loc, so they don't change for a given position
    //  case (Loc.X*2 + Loc.Y*2) mod 3 of
    //    0:   GT := gtAntiHorse;
    //    else GT := gtMelee;
    //  end;
    //  //Low weight positions are set to backline (when one segment has more than one position)
    //  if Locs.Tag[I] < 10000 then
    //    DPT := adtBackLine
    //  else
    //    DPT := adtFrontLine;
    //
    //  fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), GT, 25, DPT);
    //  if DPT = adtBackLine then Inc(BacklineCount);
    //
    //  LocI := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 4);
    //  Loc := gTerrain.EnsureTileInMapCoords(LocI.X, LocI.Y, 3);
    //  if not EnsureWalkable(Loc) then
    //    Continue;
    //
    //  fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), gtRanged, 25, DPT);
    //  if DPT = adtBackLine then Inc(BacklineCount);
    //end;

    //Add extra backline defence positions after adding all the front line ones so they are lower priority
    //for I := Locs.Count - 1 downto 0 do
    //if BacklineCount < 12 then
    //begin
    //  //Try to add backline defence positions behind front line, if there's space
    //  Loc := KMGetPointInDir(Locs[I].Loc, KMAddDirection(Locs[I].Dir, 4), 7);
    //  if gTerrain.TileInMapCoords(Loc.X, Loc.Y, 3) then
    //  begin
    //    if not EnsureWalkable(Loc) then
    //      Continue;
    //
    //    //Mix group types deterministicly based on Loc, so they don't change for a given position
    //    case (Loc.X*2 + Loc.Y*2) mod 3 of
    //      0:   GT := gtAntiHorse;
    //      1:   GT := gtRanged;
    //      else GT := gtMelee;
    //    end;
    //    fDefencePositions.Add(KMPointDir(Loc, Locs[I].Dir), GT, 35, adtBackLine);
    //    Inc(BacklineCount);
    //  end;
    //end;
  //finally
  //  Locs.Free;
  //end;

  //Compare existing defence positions with the sample
    //Get the ratio between sample and existing troops
    //Check all segments to have proportional troops count
    //Add or remove defence positions
end;


//See if we can attack our enemies
procedure TKMGeneral.OrderAttack(aGroup: TKMUnitGroup; aTarget: TKMAIAttackTarget; const aCustomPos: TKMPoint);
const
  TARGET_HOUSES: TKMHouseTypeSet = HOUSES_VALID;
var
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit  := nil;
  if aGroup = nil then
    Exit;
  //Find target
  case aTarget of
    attClosestUnit:                  TargetUnit := gHands.GetClosestUnit(aGroup.Position, fOwner, atEnemy);
    attClosestBuildingFromArmy:      TargetHouse := gHands.GetClosestHouse(aGroup.Position, fOwner, atEnemy, TARGET_HOUSES, False);
    attClosestBuildingFromStartPos:  TargetHouse := gHands.GetClosestHouse(fSetup.StartPosition, fOwner, atEnemy, TARGET_HOUSES, False);
    attCustomPosition:               begin
                                        TargetHouse := gHands.HousesHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetHouse <> nil) and
                                           (gHands.CheckAlliance(fOwner, TargetHouse.Owner) = atAlly) then
                                          TargetHouse := nil;

                                        TargetUnit := gTerrain.UnitsHitTest(aCustomPos.X, aCustomPos.Y);
                                        if (TargetUnit <> nil)
                                        and ((gHands.CheckAlliance(fOwner, TargetUnit.Owner) = atAlly)
                                            or TargetUnit.IsDeadOrDying) then
                                          TargetUnit := nil;
                                      end;
  end;

  //Choose best option
  if TargetHouse <> nil then
    aGroup.OrderAttackHouse(TargetHouse, True)
  else if TargetUnit <> nil then
    aGroup.OrderAttackUnit(TargetUnit, True)
  else if aTarget = attCustomPosition then
    aGroup.OrderWalk(aCustomPos, True, wtokAIAttackCustomPos);
end;


procedure TKMGeneral.RetaliateAgainstThreat(aAttacker: TKMUnit);
var
  I: Integer;
  Group: TKMUnitGroup;
  isInDefendArea : Boolean;
begin
  if gHands[fOwner].IsHuman then Exit;

  //Attacker may be already dying (e.g. killed by script)
  //We could retaliate against his whole group however
  if (aAttacker = nil) or aAttacker.IsDeadOrDying  then Exit;
  isInDefendArea := fDefendPositions.LocInDefendArea(aAttacker.Position);

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for I := 0 to fDefencePositions.Count - 1 do
  begin
    Group := fDefencePositions[I].CurrentGroup;
    if (Group <> nil)
      and not Group.IsDead
      and Group.IsIdle
    //Units walking to their defence position can retaliate (but not if pursuing an enemy)
    //@Lewin: Is it right that Group defends against attackers within the Rad
    //rather than defending property within the Rad?
    //Think of archer, he attacks property in AI defense radius, but stands utself outside of the radius
    //should AI retaliate against him or not?
    //@Krom: Yes it's right the way it is now. It should be the attacker not the victim.
    //Otherwise the AI sends much more groups when you shoot them with 1 bowmen in the campaigns.
    //Right now it seems to be working almost the same as in the original game.
    and ((KMLengthDiag(Group.Position, aAttacker.Position) <= fDefencePositions[I].Radius) or isInDefendArea)
    and not fDefencePositions[I].IsReturning then
    begin
      //if attacker is recruit in the tower, attack the tower
      if (aAttacker is TKMUnitRecruit) then
        Group.OrderAttackHouse(aAttacker.Home, True)
      else
        Group.OrderAttackUnit(aAttacker, True);
      fDefencePositions[I].IsDefending := true;
    end;
  end;
end;


procedure TKMGeneral.RemoveEquipOrderedWarrior(aWarrior: TKMUnitWarrior);
begin
  if fUnitsEquipOrdered.ContainsKey(aWarrior.UID) then
  begin
    fUnitsEquipOrdered[aWarrior.UID].ReleasePointer;
    fUnitsEquipOrdered.Remove(aWarrior.UID);
  end;
end;


//Trained warrior reports for duty
procedure TKMGeneral.WarriorEquipped(aWarrior: TKMUnitWarrior);
begin
  RemoveEquipOrderedWarrior(aWarrior);
  fDefencePositions.FindPlaceForGroup(TKMUnitGroup(aWarrior.Group), AI_FILL_CLOSEST_EQUIPPED);
end;

function TKMGeneral.HasPatrolAttack : Boolean;
var I : Integer;
begin

  Result := true;

  if (fDefencePositions.Count <= 0 ) then
    Exit(false);

  if not (Attacks.Count <= 1) then
    Exit(false);

  if (Attacks.Count = 0) then
    Exit(false);

  for I := fDefencePositions.Count - 1 downto 0 do
    if fDefencePositions[I].DefenceType = dtBackLine then
      if fDefencePositions[I].PositionPatrol.Loc.X <= 0 then
        Exit(false);
end;


//sometimes army doesn't come back after the battle so halting the group should fix it
procedure TKMGeneral.HaltArmy;
var I : Integer;
begin
  for I := fDefencePositions.Count - 1 downto 0 do
    if fDefencePositions[I].DefenceType = dtBackLine then
      if fDefencePositions[I].CurrentGroup <> nil then
        if fDefencePositions[I].CurrentGroup.IsDead then
          if fDefencePositions[I].CurrentGroup.Count > 0 then
          //if fDefencePositions[I].CurrentGroup.FlagBearer.IsIdle then
            fDefencePositions[I].CurrentGroup.OrderHalt(true, true);
end;


procedure TKMGeneral.WarriorDied(aWarrior: TKMUnitWarrior);
begin
  RemoveEquipOrderedWarrior(aWarrior);
end;


procedure TKMGeneral.UpdateState(aTick: Cardinal);
begin
  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psAIArmyCls);
  {$ENDIF}
  try
    //Update defence positions locations
    if fSetup.AutoDefend then
      //Checking mod result against MAX_HANDS causes first update to happen ASAP
      if (aTick + Byte(fOwner)) mod (MAX_HANDS * 50) = MAX_HANDS then
        CheckAutoDefend;

    //See if we can launch an attack
    if fSetup.AutoAttack then
      if (aTick + Byte(fOwner)) mod (MAX_HANDS * 50) = 1 then
        CheckAutoAttack;

      if aTick mod 3000 = 1 then //halt all idle army every 5 minutes
        HaltArmy;
    if (aTick + Byte(fOwner)) mod MAX_HANDS = 0 then
    begin
      fDefencePositions.UpdateState;
      CheckArmy; //Feed army, position defence, arrange/organise groups
      CheckAttacks;
      CheckArmyCount; //Train new soldiers if needed

      //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
      //CheckAndIssueAttack; //Attack enemy
      //Anything Else?
    end;
  finally
    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psAIArmyCls);
    {$ENDIF}
  end;
end;


end.




