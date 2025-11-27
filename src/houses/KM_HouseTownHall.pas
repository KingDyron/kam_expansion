unit KM_HouseTownHall;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults,
  KM_Houses,
  KM_ResTypes;

const
  TH_MAX_GOLDMAX_VALUE = 120; //Max value for TownHall MaxGold parameter


type
  TKMHouseTownHall = class(TKMHouseWFlagPoint)
  private
  protected
  public
    EquipTime : Cardinal;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function Equip(aUnitType: TKMUnitType; aCount: Integer): Integer;
    function UnitCost(aUnitType: TKMUnitType) : Byte;
    function CanEquip(aUnitType: TKMUnitType): Boolean;

    procedure PostLoadMission; override;
    //procedure UpdateDemands; override;
  end;


implementation
uses
  Math,
  KM_Hand, KM_HandsCollection, KM_HandLogistics, KM_HandTypes, KM_HandEntity,
  KM_UnitWarrior, KM_ResUnits, KM_ScriptingEvents, KM_Resource,
  KM_InterfaceGame, KM_Units;


{ TKMHouseTownHall }
constructor TKMHouseTownHall.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  inherited;
  EquipTime := 0;
  SetAcceptWareIn(wtGold, 100);
  SetAcceptWareIn(wtBitinArmor, 115);
end;


constructor TKMHouseTownHall.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseTownHall');
  LoadStream.Read(EquipTime);
end;


procedure TKMHouseTownHall.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseTownHall');
  SaveStream.Write(EquipTime);
end;

function TKMHouseTownHall.UnitCost(aUnitType: TKMUnitType): Byte;
begin
  Result := gRes.Units[aUnitType].TownhallCost;

  If (aUnitType in UNITS_CITIZEN) and gHands[Owner].EconomyDevUnlocked(18) then
    Result := Result - 5;

  If (aUnitType = utWarrior) and gHands[Owner].ArmyDevUnlocked(12) then
    Result := Result - 1;
end;

function TKMHouseTownHall.CanEquip(aUnitType: TKMUnitType): Boolean;
begin
  Result := gHands[Owner].Locks.GetUnitBlocked(aUnitType, HouseType) = ulUnlocked;


  Result := Result and (CheckWareIn(wtGold) >= UnitCost(aUnitType));  //Can't equip if we don't have a required resource

  //Result := Result{ and (gHands[Owner].GetWorklessCount > 0)};
end;


//Equip a new soldier and make him walk out of the house
//Return the number of units successfully equipped
function TKMHouseTownHall.Equip(aUnitType: TKMUnitType; aCount: Integer): Integer;
var
  I, K, Index, count: Integer;
  U: TKMUnit;
  foundTPR: Boolean;
begin
  Result := 0;
  foundTPR := False;
  for I := Low(TH_GAME_ORDER) to High(TH_GAME_ORDER) do
    if TH_GAME_ORDER[I] = aUnitType then
    begin
      foundTPR := True;
      Break;
    end;

  Assert(foundTPR);
  
  for K := 0 to aCount - 1 do
  begin
    //Make sure we have enough resources to equip a unit
    if not CanEquip(aUnitType) then Exit;
    //gHands[Owner].TakeWorkless;
    //Take resources
    WareTakeFromIn(wtGold, UnitCost(aUnitType)); //Do the goldtaking

    gHands[Owner].Stats.WareConsumed(wtGold, UnitCost(aUnitType));
      
    //Make new unit
    U := gHands[Owner].TrainUnit(aUnitType, Self);
    U.Visible := False; //Make him invisible as he is inside the barracks
    U.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
    U.SetActionGoIn(uaWalk, gdGoOutside, Self, true);

    Index := gRes.Wares.VirtualWares.WareS['vtCertificate'].Index;
    if Index <> high(Word) then
    begin
      count := gRes.Wares.VirtualWares.Ware[Index].GetProdCount(HouseType, wtWarfare);
      gHands[Owner].VirtualWareTake(Index, -count);
    end;

    ProduceFestivalPoints(fptWarfare, (UnitCost(aUnitType) div 3) + 1);


    if gHands[Owner].IsComputer then
      if aUnitType in UNITS_WARRIORS then
        if gRes.Units[U.UnitType].CanOrderAmmo then
          TKMUnitWarrior(U).OrderAmmo;

    if Assigned(U.OnUnitTrained) then
      U.OnUnitTrained(U);

    if aUnitType in WARRIOR_BITIN_EQUIPABLE then
      if CheckWareIn(wtBitinArmor) > 0 then
      begin
        WareTakeFromIn(wtBitinArmor, 1);
        TKMUnitWarrior(U).AddBitin;
      end;
    If U.UnitType = utHouseBuilder then
      U.GiveBoots;
    Inc(Result);
  end;
end;



procedure TKMHouseTownHall.PostLoadMission;
begin
  // House could be destroyed on the game start if placed with 0 health in the MapEd
  if not IsDestroyed then
  begin
    SetAcceptWareIn(wtGold, 100);
    SetAcceptWareIn(wtBitinArmor, 115);
    UpdateDemands;
  end;
end;

end.
