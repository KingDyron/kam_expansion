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
    fGoldCnt: Word;
    fGoldMaxCnt: Word;
    fBitinArmorCnt: Word;
    fBitinArmorMaxCnt: Word;
    procedure SetGoldCnt(aValue: Word); overload;
    procedure SetGoldCnt(aValue: Word; aLimitMaxGoldCnt: Boolean); overload;

    procedure SetBitinCnt(aValue: Word); overload;
    procedure SetBitinCnt(aValue: Word; aLimitMaxGoldCnt: Boolean); overload;


    procedure SetGoldMaxCnt(aValue: Word);
    procedure SetBitinMaxCnt(aValue: Word);

    function GetGoldDeliveryCnt: Word;
    procedure SetGoldDeliveryCnt(aCount: Word);

    function GetBitinDeliveryCnt: Word;
    procedure SetBitinDeliveryCnt(aCount: Word);

    function GetGoldDemandsClosing: Word;
    procedure SetGoldDemandsClosing(aCount: Word);

    function GetBitinDemandsClosing: Word;
    procedure SetBitinDemandsClosing(aCount: Word);

    property GoldDeliveryCnt: Word read GetGoldDeliveryCnt write SetGoldDeliveryCnt;
    property GoldDemandsClosing: Word read GetGoldDemandsClosing write SetGoldDemandsClosing;

    property BitinDeliveryCnt: Word read GetBitinDeliveryCnt write SetBitinDeliveryCnt;
    property BitinDemandsClosing: Word read GetBitinDemandsClosing write SetBitinDemandsClosing;
  protected
    procedure AddDemandsOnActivate(aWasBuilt: Boolean); override;
    function GetWareIn(aI: Byte): Word; override;
    procedure SetWareIn(aI: Byte; aValue: Word); override;

    function TryDecWareDelivery(aWare: TKMWareType; aDeleteCanceled: Boolean): Boolean; override;
  public
    EquipTime : Cardinal;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;

    property GoldCnt: Word read fGoldCnt write SetGoldCnt;
    property GoldMaxCnt: Word read fGoldMaxCnt write SetGoldMaxCnt;

    property BitinArmorCnt: Word read fBitinArmorCnt write SetBitinCnt;
    property BitinArmorMaxCnt: Word read fBitinArmorMaxCnt write SetBitinMaxCnt;


    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; override;

    function Equip(aUnitType: TKMUnitType; aCount: Integer): Integer;
    function CanEquip(aUnitType: TKMUnitType): Boolean;

    procedure PostLoadMission; override;
    procedure UpdateDemands; override;

    procedure WareTake(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;
    procedure WareTakeFromIn(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function CheckWareIn(aWare: TKMWareType): Word; override;
    function CheckWareTotal(aWare: TKMWareType): Word; override;
    function WareCanAddToIn(aWare: TKMWareType): Boolean; override;
    function CanHaveWareType(aWare: TKMWareType): Boolean; override;
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

  fGoldCnt := 0;
  if gHands[aOwner].IsComputer then
   fGoldMaxCnt := 20
  else
   fGoldMaxCnt := 12;

   fBitinArmorMaxCnt := 5;
  inherited;
end;


constructor TKMHouseTownHall.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseTownHall');
  LoadStream.Read(fGoldCnt);
  LoadStream.Read(fGoldMaxCnt);
  LoadStream.Read(EquipTime);
  LoadStream.Read(fBitinArmorCnt);
  LoadStream.Read(fBitinArmorMaxCnt);
end;


procedure TKMHouseTownHall.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseTownHall');
  SaveStream.Write(fGoldCnt);
  SaveStream.Write(fGoldMaxCnt);
  SaveStream.Write(EquipTime);
  SaveStream.Write(fBitinArmorCnt);
  SaveStream.Write(fBitinArmorMaxCnt);
end;


procedure TKMHouseTownHall.SetGoldCnt(aValue: Word);
begin
  SetGoldCnt(aValue, True);
end;


procedure TKMHouseTownHall.SetGoldCnt(aValue: Word; aLimitMaxGoldCnt: Boolean);
var
  oldValue: Integer;
begin
  oldValue := fGoldCnt;

  fGoldCnt := EnsureRange(aValue, 0, IfThen(aLimitMaxGoldCnt, fGoldMaxCnt, High(Word)));

  SetWareInManageTakeOutDeliveryMode(wtGold, fGoldCnt - oldValue);

  if oldValue <> fGoldCnt then
    gScriptEvents.ProcHouseWareCountChanged(Self, wtGold, fGoldCnt, fGoldCnt - oldValue);
end;

procedure TKMHouseTownHall.SetBitinCnt(aValue: Word);
begin
  SetBitinCnt(aValue, True);
end;
procedure TKMHouseTownHall.SetBitinCnt(aValue: Word; aLimitMaxGoldCnt: Boolean);
var
  oldValue: Integer;
begin
  oldValue := fBitinArmorCnt;

  fBitinArmorCnt := EnsureRange(aValue, 0, IfThen(aLimitMaxGoldCnt, fBitinArmorMaxCnt, High(Word)));

  SetWareInManageTakeOutDeliveryMode(wtBitinArmor, fBitinArmorCnt - oldValue);

  if oldValue <> fBitinArmorCnt then
    gScriptEvents.ProcHouseWareCountChanged(Self, wtBitinArmor, fBitinArmorCnt, fBitinArmorCnt - oldValue);
end;


function TKMHouseTownHall.TryDecWareDelivery(aWare: TKMWareType; aDeleteCanceled: Boolean): Boolean;
begin
  Assert((GoldDemandsClosing > 0) or (BitinDemandsClosing > 0));
  Result := false;
  if aWare = wtBitinArmor then
  begin
    if not aDeleteCanceled then
      BitinDeliveryCnt := BitinDeliveryCnt - 1;
    BitinDemandsClosing := BitinDemandsClosing - 1;
    Exit;
  end;
  if not aDeleteCanceled then
    GoldDeliveryCnt := GoldDeliveryCnt - 1;
  GoldDemandsClosing := GoldDemandsClosing - 1;

  Result := True;
end;


procedure TKMHouseTownHall.SetGoldMaxCnt(aValue: Word);
begin
  fGoldMaxCnt := EnsureRange(aValue, 0, TH_MAX_GOLDMAX_VALUE);
  UpdateDemands;
end;

procedure TKMHouseTownHall.SetBitinMaxCnt(aValue: Word);
begin
  fBitinArmorMaxCnt := EnsureRange(aValue, 0, TH_MAX_GOLDMAX_VALUE);
  UpdateDemands;
end;


function TKMHouseTownHall.CanEquip(aUnitType: TKMUnitType): Boolean;
begin
  Result := gHands[Owner].Locks.GetUnitBlocked(aUnitType, HouseType) = ulUnlocked;


  Result := Result and (fGoldCnt >= gRes.Units[aUnitType].TownhallCost);  //Can't equip if we don't have a required resource

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
    GoldDeliveryCnt := Max(GoldDeliveryCnt - gRes.Units[aUnitType].TownhallCost, 0); //Compensation for GoldDeliveryCnt
    WareTakeFromIn(wtGold, gRes.Units[aUnitType].TownhallCost); //Do the goldtaking

    gHands[Owner].Stats.WareConsumed(wtGold, gRes.Units[aUnitType].TownhallCost);
      
    //Make new unit
    U := gHands[Owner].TrainUnit(aUnitType, Self);
    U.Visible := False; //Make him invisible as he is inside the barracks
    U.Condition := Round(TROOPS_TRAINED_CONDITION * UNIT_MAX_CONDITION); //All soldiers start with 3/4, so groups get hungry at the same time
    U.SetActionGoIn(uaWalk, gdGoOutside, Self);

    Index := gRes.Wares.VirtualWares.WareS['vtCertificate'].Index;
    if Index <> high(Word) then
    begin
      count := gRes.Wares.VirtualWares.Ware[Index].GetProdCount(HouseType, wtWarfare);
      Inc(gHands[Owner].VWaresCount[Index], count)
    end;

    if gHands[Owner].IsComputer then
      if aUnitType in UNITS_WARRIORS then
        if gRes.Units[U.UnitType].CanOrderAmmo then
          TKMUnitWarrior(U).OrderAmmo;

    if Assigned(U.OnUnitTrained) then
      U.OnUnitTrained(U);

    if aUnitType in WARRIOR_BITIN_EQUIPABLE then
      if CheckWareIn(wtBitinArmor) > 0 then
      begin
        BitinDeliveryCnt := Max(0, BitinDeliveryCnt - 1);
        WareTakeFromIn(wtBitinArmor, 1);
        TKMUnitWarrior(U).AddBitin;
      end;

    Inc(Result);
  end;
end;



procedure TKMHouseTownHall.PostLoadMission;
begin
  // House could be destroyed on the game start if placed with 0 health in the MapEd
  if not IsDestroyed then
    UpdateDemands;
end;


procedure TKMHouseTownHall.AddDemandsOnActivate(aWasBuilt: Boolean);
begin
  UpdateDemands;
end;


function TKMHouseTownHall.GetWareIn(aI: Byte): Word;
begin
  Result := 0;
  if aI = 1 then // Wares are 1 based
    Result := fGoldCnt;
  if aI = 2 then // Wares are 1 based
    Result := fBitinArmorCnt;
end;


procedure TKMHouseTownHall.SetWareIn(aI: Byte; aValue: Word);
begin
  if aI = 1 then
    GoldCnt := aValue;
  if aI = 2 then
    BitinArmorCnt := aValue;
end;


function TKMHouseTownHall.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  Result := inherited or not (aWareType in [wtGold, wtBitinArmor]);

  if not Result and (aWareType = wtGold) then
    Result := GoldCnt + gHands[Owner].Deliveries.Queue.GetDeliveriesToHouseCnt(Self, wtGold) > GoldMaxCnt;

  if not Result and (aWareType = wtBitinArmor) then
    Result := BitinArmorCnt + gHands[Owner].Deliveries.Queue.GetDeliveriesToHouseCnt(Self, wtBitinArmor) > BitinArmorMaxCnt;
end;


procedure TKMHouseTownHall.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
var
  ordersRemoved, plannedToRemove: Integer;
begin
  Assert(aWare in [wtGold, wtBitinArmor], 'Invalid ware added to TownHall');

  if aWare = wtBitinArmor then
  begin
    // Allow to enlarge GoldMaxCnt from script (either from .dat or from .script)
    if aFromScript and (fBitinArmorMaxCnt < fBitinArmorCnt + aCount) then
      SetBitinMaxCnt(fBitinArmorCnt + aCount);

    SetBitinCnt(fBitinArmorCnt + aCount, False);

    if aFromScript then
    begin
      BitinDeliveryCnt := BitinDeliveryCnt + aCount;
      ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount, plannedToRemove);
      BitinDeliveryCnt := BitinDeliveryCnt - ordersRemoved;
      BitinDemandsClosing := BitinDemandsClosing + plannedToRemove;
    end;
    UpdateDemands;
    Exit;
  end;
  // Allow to enlarge GoldMaxCnt from script (either from .dat or from .script)
  if aFromScript and (fGoldMaxCnt < fGoldCnt + aCount) then
    SetGoldMaxCnt(fGoldCnt + aCount);

  SetGoldCnt(fGoldCnt + aCount, False);

  if aFromScript then
  begin
    GoldDeliveryCnt := GoldDeliveryCnt + aCount;
    ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount, plannedToRemove);
    GoldDeliveryCnt := GoldDeliveryCnt - ordersRemoved;
    GoldDemandsClosing := GoldDemandsClosing + plannedToRemove;
  end;

  UpdateDemands;
end;


function TKMHouseTownHall.GetGoldDeliveryCnt: Word;
begin
  Result := WareDeliveryCnt[1];
end;


procedure TKMHouseTownHall.SetGoldDeliveryCnt(aCount: Word);
begin
  WareDeliveryCnt[1] := aCount;
end;

function TKMHouseTownHall.GetBitinDeliveryCnt: Word;
begin
  Result := WareDeliveryCnt[2];
end;


procedure TKMHouseTownHall.SetBitinDeliveryCnt(aCount: Word);
begin
  WareDeliveryCnt[2] := aCount;
end;

function TKMHouseTownHall.GetGoldDemandsClosing: Word;
begin
  Result := WareDemandsClosing[1];
end;


procedure TKMHouseTownHall.SetGoldDemandsClosing(aCount: Word);
begin
  WareDemandsClosing[1] := aCount;
end;

function TKMHouseTownHall.GetBitinDemandsClosing: Word;
begin
  Result := WareDemandsClosing[2];
end;

procedure TKMHouseTownHall.SetBitinDemandsClosing(aCount: Word);
begin
  WareDemandsClosing[2] := aCount;
end;

procedure TKMHouseTownHall.UpdateDemands;
const
  MAX_GOLD_DEMANDS = 20; //Limit max number of demands by townhall to not to overfill demands list
var
  goldToOrder, ordersRemoved, plannedToRemove, deliveringWare: Integer;
begin
  deliveringWare := GoldDeliveryCnt - GoldDemandsClosing;

  goldToOrder := Min(MAX_GOLD_DEMANDS - (deliveringWare - fGoldCnt), fGoldMaxCnt - deliveringWare);

  if goldToOrder > 0 then
  begin
    gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, wtGold, goldToOrder, dtOnce, diNorm);
    GoldDeliveryCnt := GoldDeliveryCnt + goldToOrder;
  end
  else
  if goldToOrder < 0 then
  begin
    ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, wtGold, -goldToOrder, plannedToRemove);
    GoldDeliveryCnt := GoldDeliveryCnt - ordersRemoved;
    GoldDemandsClosing := GoldDemandsClosing + plannedToRemove;
  end;


  deliveringWare := BitinDeliveryCnt - BitinDemandsClosing;

  goldToOrder := Min(MAX_GOLD_DEMANDS - (deliveringWare - fBitinArmorCnt), fBitinArmorMaxCnt - deliveringWare);

  if goldToOrder > 0 then
  begin
    gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, wtBitinArmor, goldToOrder, dtOnce, diNorm);
    BitinDeliveryCnt := BitinDeliveryCnt + goldToOrder;
  end
  else
  if goldToOrder < 0 then
  begin
    ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, wtBitinArmor, -goldToOrder, plannedToRemove);
    BitinDeliveryCnt := BitinDeliveryCnt - ordersRemoved;
    BitinDemandsClosing := BitinDemandsClosing + plannedToRemove;
  end;
end;


procedure TKMHouseTownHall.WareTake(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  if DeliveryMode = dmTakeOut then
    WareTakeFromOut(aWare, aCount, aFromScript)
  else
    WareTakeFromIn(aWare, aCount, aFromScript);
end;


procedure TKMHouseTownHall.WareTakeFromIn(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  Assert(aWare in [wtGold, wtBitinArmor], 'Invalid ware taken from TownHall');

  if aWare = wtBitinArmor then
  begin
    aCount := EnsureRange(aCount, 0, fBitinArmorCnt);
    if aFromScript then
    begin
      aCount := EnsureRange(aCount, 0, fBitinArmorCnt);
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
    end;

    SetBitinCnt(fBitinArmorCnt - aCount, False);
    Exit;
  end;

  aCount := EnsureRange(aCount, 0, fGoldCnt);
  if aFromScript then
  begin
    aCount := EnsureRange(aCount, 0, fGoldCnt);
    gHands[Owner].Stats.WareConsumed(aWare, aCount);
  end;

  SetGoldCnt(fGoldCnt - aCount, False);
  UpdateDemands;
end;


procedure TKMHouseTownHall.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
begin
  Assert(aWare in [wtGold, wtBitinArmor], 'Invalid ware taken from TownHall');

  if aFromScript then
  begin
    aCount := EnsureRange(aCount, 0, fGoldCnt);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;
  Assert(aCount <= fGoldCnt);
  SetGoldCnt(fGoldCnt - aCount, False);

  //Keep track of how many are ordered
  GoldDeliveryCnt := GoldDeliveryCnt - aCount;

  UpdateDemands;
end;


function TKMHouseTownHall.CheckWareIn(aWare: TKMWareType): Word;
begin
  Result := 0; //Including Wood/stone in building stage
  if aWare = wtGold then
    Result := fGoldCnt;
  if aWare = wtBitinArmor then
    Result := fBitinArmorCnt;
end;

function TKMHouseTownHall.CheckWareTotal(aWare: TKMWareType): Word;
begin
  Result := 0; //Including Wood/stone in building stage
  if aWare = wtGold then
    Result := fGoldCnt;
  if aWare = wtBitinArmor then
    Result := fBitinArmorCnt;
end;


function TKMHouseTownHall.WareCanAddToIn(aWare: TKMWareType): Boolean;
begin
  Result := ((aWare = wtGold) and (fGoldCnt < fGoldMaxCnt))
            or ((aWare = wtBitinArmor) and (fBitinArmorCnt < fBitinArmorMaxCnt));
end;


function TKMHouseTownHall.CanHaveWareType(aWare: TKMWareType): Boolean;
begin
  Result := (aWare in [wtGold, wtBitinArmor]);
end;

end.
