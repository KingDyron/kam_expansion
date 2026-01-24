unit KM_HouseBarracks;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Houses,
  KM_CommonClasses, KM_Defaults,
  KM_ResTypes;


type
  // Barracks have 11 wares and Recruits
  TKMHouseBarracks = class(TKMHouseWFlagPoint)
  private

    fRecruitsList: TList;
    fResourceCount: array [WARE_MIN..WARE_MAX] of Word;
    procedure SetWareCnt(aWareType: TKMWareType; aValue: Word);
  public
    MapEdRecruitCount: Word; //Only used by MapEd
    NotAcceptFlag: array [WARE_MIN..WARE_MAX] of Boolean;
    NotAllowTakeOutFlag: array [WARE_MIN..WARE_MAX] of Boolean;
    NotAcceptRecruitFlag: Boolean;
    EquipTimeL, EquipTimeI : Cardinal;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;

    procedure Activate(aWasBuilt: Boolean); override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;

    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;
    procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function CheckWareIn(aWare: TKMWareType): Word; override;
    function CheckWareTotal(aWare: TKMWareType): Word; override;
    function WareCanAddToIn(aWare: TKMWareType): Boolean; override;

    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; override;
    function ShouldAbandonDeliveryFrom(aWareType: TKMWareType; aImmidiateCheck: Boolean = False): Boolean; override;
    function ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean; override;

    function GetTotalWaresCnt: Integer;
    function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;
    function CanEquip(aUnitType: TKMUnitType): Boolean;
    function RecruitsCount: Integer;
    procedure RecruitsAdd(aUnit: Pointer);
    procedure RecruitsRemove(aUnit: Pointer);
    procedure ToggleNotAcceptFlag(aWare: TKMWareType);
    procedure ToggleNotAllowTakeOutFlag(aWare: TKMWareType);
    procedure ToggleAcceptRecruits;
    function EquipWarrior(aUnitType: TKMUnitType): Pointer;
    function Equip(aUnitType: TKMUnitType; aCount: Integer): Integer;
    function CreateRecruitInside(aIsMapEd: Boolean) : Pointer;
    Function WareAllowedToTakeOut(aWare : TKMWareType) : Boolean;
    Function WareAccepted(aWare : TKMWareType) : Boolean;
    function RecruitAccepted : Boolean;
    function GetStats(aWares : Boolean) : TKMHouseStats; override;
  end;


implementation
uses
  Math, Types,
  KM_Entity,
  KM_Hand,
  KM_HandsCollection,
  KM_HandTypes,
  KM_HandEntity,
  KM_Units,
  KM_UnitWarrior,
  KM_ScriptingEvents,
  KM_ResUnits,
  KM_Resource;


{ TKMHouseBarracks }
constructor TKMHouseBarracks.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  inherited;

  fRecruitsList := TList.Create;
end;


constructor TKMHouseBarracks.Load(LoadStream: TKMemoryStream);
var
  I, newCount: Integer;
  U: TKMUnit;
begin
  inherited;
  LoadStream.CheckMarker('HouseBarracks');
  LoadStream.Read(fResourceCount, SizeOf(fResourceCount));
  fRecruitsList := TList.Create;
  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(U, 4); //subst on syncload
    fRecruitsList.Add(U);
  end;
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
  LoadStream.Read(NotAllowTakeOutFlag, SizeOf(NotAllowTakeOutFlag));
  LoadStream.Read(NotAcceptRecruitFlag);
  LoadStream.Read(EquipTimeL);
  LoadStream.Read(EquipTimeI);
end;


procedure TKMHouseBarracks.SyncLoad;
var
  I: Integer;
begin
  inherited;

  for I := 0 to RecruitsCount - 1 do
    fRecruitsList.Items[I] := gHands.GetUnitByUID(Integer(fRecruitsList.Items[I]));
end;


destructor TKMHouseBarracks.Destroy;
begin
  fRecruitsList.Free;

  inherited;
end;


procedure TKMHouseBarracks.Activate(aWasBuilt: Boolean);
var
  firstBarracks: TKMHouseBarracks;
  WT: TKMWareType;
begin
  inherited;
  //A new Barracks should inherit the accept properies of the first Barracks of that player,
  //which stops a sudden flow of unwanted wares to it as soon as it is created.
  firstBarracks := TKMHouseBarracks(gHands[Owner].FindHouse(htBarracks, 1));
  if (firstBarracks <> nil) and not firstBarracks.IsDestroyed then
  begin
    for WT in WARES_WARFARE do
    begin
      NotAcceptFlag[WT] := firstBarracks.NotAcceptFlag[WT];
      NotAllowTakeOutFlag[WT] := firstBarracks.NotAllowTakeOutFlag[WT];
    end;
  end;
end;


procedure TKMHouseBarracks.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  W: TKMWareType;
begin
  //Recruits are no longer under our control so we forget about them (UpdateVisibility will sort it out)
  //Otherwise it can cause crashes while saving under the right conditions when a recruit is then killed.
  fRecruitsList.Clear;

  for W in WARES_WARFARE do
    gHands[Owner].Stats.WareConsumed(W, fResourceCount[W]);

  inherited;
end;


procedure TKMHouseBarracks.RecruitsAdd(aUnit: Pointer);
begin
  fRecruitsList.Add(aUnit);
end;


function TKMHouseBarracks.RecruitsCount: Integer;
begin
  Result := fRecruitsList.Count;
end;


procedure TKMHouseBarracks.RecruitsRemove(aUnit: Pointer);
begin
  fRecruitsList.Remove(aUnit);
end;


procedure TKMHouseBarracks.SetWareCnt(aWareType: TKMWareType; aValue: Word);
var
  cntChange: Integer;
begin
  Assert(aWareType in WARES_WARFARE);

  cntChange := aValue - fResourceCount[aWareType];

  fResourceCount[aWareType] := aValue;

  if not gHands[Owner].IsHuman then
    if aWareType = wtBitinE then
      if fResourceCount[aWareType] >= 50 then
        NotAcceptFlag[aWareType] := true
      else
        NotAcceptFlag[aWareType] := false;

  if cntChange <> 0 then
    gScriptEvents.ProcHouseWareCountChanged(Self, aWareType, aValue, cntChange);
end;


procedure TKMHouseBarracks.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
var
  oldCnt: Integer;
begin
  Assert(aWare in WARES_WARFARE, 'Invalid ware added to barracks:' + gRes.Wares[aWare].Title);

  oldCnt := fResourceCount[aWare];
  SetWareCnt(aWare, EnsureRange(fResourceCount[aWare] + aCount, 0, High(Word)));
  gHands[Owner].Deliveries.Queue.AddOffer(Self, aWare, fResourceCount[aWare] - oldCnt);
end;


function TKMHouseBarracks.WareCanAddToIn(aWare: TKMWareType): Boolean;
begin
  Result := (aWare in WARES_WARFARE);
end;

function TKMHouseBarracks.WareAllowedToTakeOut(aWare: TKMWareType): Boolean;
begin
  Assert(aWare in WARES_WARFARE);

  Result := NotAllowTakeOutFlag[aWare];
end;

function TKMHouseBarracks.WareAccepted(aWare: TKMWareType): Boolean;
begin
  Assert(aWare in WARES_WARFARE);

  Result := NotAcceptFlag[aWare];
end;

function TKMHouseBarracks.CheckWareIn(aWare: TKMWareType): Word;
begin
  if aWare in WARES_WARFARE then
    Result := fResourceCount[aWare]
  else
    Result := 0; //Including Wood/stone in building stage
end;

function TKMHouseBarracks.CheckWareTotal(aWare: TKMWareType): Word;
begin
  Result := CheckWareIn(aWare);
end;

procedure TKMHouseBarracks.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  if aFromScript then
  begin
    aCount := Min(aCount, fResourceCount[aWare]);

    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;

  Assert(aCount <= fResourceCount[aWare]);
  SetWareCnt(aWare, fResourceCount[aWare] - aCount);
end;


function TKMHouseBarracks.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
begin
  Assert(aWare in WARES_WARFARE);

  Result := (fResourceCount[aWare] >= aCount);
end;


function TKMHouseBarracks.GetTotalWaresCnt: Integer;
var
  W: TKMWareType;
begin
  Result := 0;
  for W in WARES_WARFARE do
    Inc(Result, fResourceCount[W]);
end;

function TKMHouseBarracks.RecruitAccepted : Boolean;
begin
  Result := NotAcceptRecruitFlag or (  gHands[Owner].IsComputer and (fRecruitsList.Count >= gHands[Owner].AI.Setup.RecruitCount));

end;

procedure TKMHouseBarracks.ToggleNotAcceptFlag(aWare: TKMWareType);
begin
  Assert(aWare in WARES_WARFARE);
  NotAcceptFlag[aWare] := not NotAcceptFlag[aWare];
end;


procedure TKMHouseBarracks.ToggleNotAllowTakeOutFlag(aWare: TKMWareType);
begin
  Assert(aWare in WARES_WARFARE);

  NotAllowTakeOutFlag[aWare] := not NotAllowTakeOutFlag[aWare];
end;


function TKMHouseBarracks.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  Result := inherited
            or not (aWareType in WARES_WARFARE)
            or NotAcceptFlag[aWareType];
end;


function TKMHouseBarracks.ShouldAbandonDeliveryFrom(aWareType: TKMWareType; aImmidiateCheck: Boolean = False): Boolean;
begin
  Result := inherited or not (aWareType in WARES_WARFARE);
end;


function TKMHouseBarracks.ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean;
begin
  Result := inherited
              or (aToHouse = nil)
              //Do not allow delivery from Barracks to other houses except Market/Store/other Barracks
              or not (aToHouse.HouseType in [htMarket, htSchool, htStore, htBarracks])
              or ((aToHouse.HouseType <> htMarket) //allow delivery to Market with any mode
                //For other houses allow only when dmTakeOut and no flag NotAllowTakeOutFlag
                and ({(GetDeliveryModeForCheck(aImmidiateCheck) <> dmTakeOut)
                      or }not NotAllowTakeOutFlag[aWareType]
                      )



                      ); //Use NewDelivery here, since

end;


procedure TKMHouseBarracks.ToggleAcceptRecruits;
begin
  NotAcceptRecruitFlag := not NotAcceptRecruitFlag;
end;


function TKMHouseBarracks.CanEquip(aUnitType: TKMUnitType): Boolean;
var
  I: Integer;
begin
  Result := RecruitsCount > 0; //Can't equip anything without recruits
  Result := Result and gHands[Owner].Locks.UnitUnlocked(aUnitType);

  if Result then
    for I := 0 to high(gRes.Units[aUnitType].BarracksCost) do
      if gRes.Units[aUnitType].BarracksCost[I].W <> wtNone then
        Result := Result and (fResourceCount[gRes.Units[aUnitType].BarracksCost[I].W] >= gRes.Units[aUnitType].BarracksCost[I].C);

end;


function TKMHouseBarracks.EquipWarrior(aUnitType: TKMUnitType): Pointer;
var
  I: Integer;
  troopWareType: TKMWareType;
  soldier: TKMUnitWarrior;
  U : TKMUnit;
  condition : Integer;
  hadBoots : Boolean;
  wareCost : Integer;
begin
  Result := nil;
  //Make sure we have enough resources to equip a unit
  if not CanEquip(aUnitType) then Exit;

  hadBoots := TKMUnitRecruit(fRecruitsList.Items[0]).BootsAdded;
  condition := TKMUnitRecruit(fRecruitsList.Items[0]).Condition;
  //Take resources
  wareCost := 0;
  for I := 0 to high(gRes.Units[aUnitType].BarracksCost) do
    if gRes.Units[aUnitType].BarracksCost[I].W <> wtNone then
    begin
      troopWareType := gRes.Units[aUnitType].BarracksCost[I].W;
      SetWareCnt(troopWareType, fResourceCount[troopWareType] - gRes.Units[aUnitType].BarracksCost[I].C);
      Inc(wareCost);
      If gRes.Units[aUnitType].BarracksCost[I].W in [wtIronShield, wtIronArmor, wtSword, wtCrossbow, wtPike] then
        Inc(wareCost);

      gHands[Owner].Stats.WareConsumed(troopWareType, gRes.Units[aUnitType].BarracksCost[I].C);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, troopWareType, gRes.Units[aUnitType].BarracksCost[I].C);
    end;
  ProduceFestivalPoints(fptWarfare, wareCost);
  //don't use old one
  {for I := 1 to 4 do
  if TROOP_COST[aUnitType, I] <> wtNone then
  begin
    troopWareType := TROOP_COST[aUnitType, I];
    SetWareCnt(troopWareType, fResourceCount[troopWareType] - 1);

    gHands[Owner].Stats.WareConsumed(TROOP_COST[aUnitType, I]);
    gHands[Owner].Deliveries.Queue.RemOffer(Self, TROOP_COST[aUnitType, I], 1);

  end;}

  //Special way to kill the Recruit because it is in a house
  TKMUnitRecruit(fRecruitsList.Items[0]).KillInHouse;
  fRecruitsList.Delete(0); //Delete first recruit in the list

  if aUnitType in UNITS_CITIZEN then
  begin
    U := gHands[Owner].TrainUnit(aUnitType, Self);
    U.Visible := false;
    //U.Condition := condition{Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION)};
    U.SetActionGoIn(uaWalk, gdGoOutside, Self, true);
    if Assigned(U.OnUnitTrained) then
      U.OnUnitTrained(U);
    Result := U;
    U.Condition := condition;
    if gHands[Owner].VirtualWareTake('vtHerbs') or gHands[Owner].VirtualWareTake('vtAppleJuice') or gHands[Owner].VirtualWareTake('vtDishes') then
      U.Condition := UNIT_MAX_CONDITION;

    if hadBoots then
      U.GiveBoots(false);
    Result := U;
  end else
  if aUnitType in UNITS_WARRIORS then
  begin
    //Make new unit
    soldier := TKMUnitWarrior(gHands[Owner].TrainUnit(aUnitType, Self));
    soldier.Visible := False; //Make him invisible as he is inside the barracks
    //soldier.Condition := condition{Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION)}; //All soldiers start with 3/4, so groups get hungry at the same time
    //Soldier.OrderLoc := KMPointBelow(Entrance); //Position in front of the barracks facing north


    if gHands[Owner].VirtualWareTake('vtHerbs') or gHands[Owner].VirtualWareTake('vtAppleJuice') or gHands[Owner].VirtualWareTake('vtDishes') then
      Soldier.Condition := UNIT_MAX_CONDITION
    else
      Soldier.Condition := condition;

    soldier.SetActionGoIn(uaWalk, gdGoOutside, Self, true);
    if Assigned(soldier.OnUnitTrained) then
      soldier.OnUnitTrained(soldier);

    if hadBoots then
      soldier.GiveBoots(false)
    else
    if (CheckWareIn(wtBoots) > 0) and not (UNIT_TO_GROUP_TYPE[soldier.UnitType] in [gtMounted, gtMachines, gtMachinesMelee]) then
    if soldier.GiveBoots(false) then
        WareTakeFromOut(wtBoots, 1);




    //check if we can take quiver from baracks
    if (CheckWareIn(wtQuiver) > 0) and (gRes.Units[soldier.UnitType].AmmoType = uatArrow) and gRes.Units[soldier.UnitType].CanOrderAmmo then
    begin
      WareTakeFromOut(wtQuiver, 1);
      soldier.ReloadAmmo(wtQuiver);
    end else
    if gHands[Owner].IsComputer then
      if gRes.Units[soldier.UnitType].CanOrderAmmo then
        soldier.OrderAmmo;
    gHands[Owner].SetVirtualWareCnt('vtCertificate', 1);

    Result := soldier;
  end;

end;


// Equip a new soldier and make him walk out of the house
// Return the number of units successfully equipped
function TKMHouseBarracks.Equip(aUnitType: TKMUnitType; aCount: Integer): Integer;
var
  I: Integer;
  U: TKMUnit;
begin
  Result := 0;
  Assert(aUnitType in UNITS_HUMAN);

  for I := 0 to aCount - 1 do
  begin
    U := EquipWarrior(aUnitType);

    if U = nil then
      Exit;

    if aUnitType in WARRIOR_BITIN_EQUIPABLE then
      if CheckWareIn(wtBitinArmor) > 0 then
      begin
        TKMUnitWarrior(U).AddBitin;
        WareTakeFromOut(wtBitinArmor, 1);
      end;

    Inc(Result);
  end;
end;


function TKMHouseBarracks.CreateRecruitInside(aIsMapEd: Boolean) : Pointer;
var
  U: TKMUnit;
begin
  Result := nil;
  if aIsMapEd then
    Inc(MapEdRecruitCount)
  else
  begin
    U := gHands[Owner].TrainUnit(utRecruit, Self);
    U.Visible := False;
    U.Home := Self; //When walking out Home is used to remove recruit from barracks
    Result := U;
    RecruitsAdd(U);
    gHands[Owner].Stats.UnitCreated(utRecruit, False);
  end;
end;


procedure TKMHouseBarracks.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  inherited;

  SaveStream.PlaceMarker('HouseBarracks');
  SaveStream.Write(fResourceCount, SizeOf(fResourceCount));
  SaveStream.Write(RecruitsCount);
  for I := 0 to RecruitsCount - 1 do
    SaveStream.Write(TKMUnit(fRecruitsList.Items[I]).UID); //Store ID
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
  SaveStream.Write(NotAllowTakeOutFlag, SizeOf(NotAllowTakeOutFlag));
  SaveStream.Write(NotAcceptRecruitFlag);
  SaveStream.Write(EquipTimeL);
  SaveStream.Write(EquipTimeI);
end;

function TKMHouseBarracks.GetStats(aWares : Boolean) : TKMHouseStats;
var W : TKMWareType;
begin
  Result := Inherited;
  If aWares then
  begin
    for W in WARES_VALID do
      Result.Wares.AddWare(W, CheckWareIn(W));
  end;
end;

end.
