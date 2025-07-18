unit KM_HouseStore;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses,
  KM_ResTypes,
  KM_Defaults, KM_Points, KM_CommonClasses;

type
  // Storehouse keeps all the wares and flags for them
  TKMHouseStore = class(TKMHouse)
  private
    fMaxCount : Integer;
    fTotalCount : Integer;
    fWaresCount: array [WARE_MIN .. WARE_MAX] of Word;
    procedure SetWareCnt(aWareType: TKMWareType; aValue: Word);
  protected
    procedure Activate(aWasBuilt: Boolean); override;
  public
    NotAcceptFlag: array [TKMWareType] of Boolean;
    NotAllowTakeOutFlag: array [TKMWareType] of Boolean;
    WareAIBlockAouto: array [TKMWareType] of Boolean;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;

    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; override;
    function ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean; override;

    procedure ToggleNotAcceptFlag(aWare: TKMWareType);
    procedure SetToggleNotAcceptFlag(aWare: TKMWareType; aAccept : Boolean);
    procedure ToggleNotAcceptTakeOutFlag(aWare: TKMWareType);
    procedure SetToggleNotAcceptTakeOutFlag(aWare: TKMWareType; aAccept : Boolean);
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;
    function CheckWareIn(aWare: TKMWareType): Word; override;
    function CheckWareOut(aWare: TKMWareType): Word; override;
    function CheckWareTotal(aWare: TKMWareType): Word; override;

    function FoodCount : Integer;

    procedure SetWareInCnt(aWare : TKMWareType; aValue : Integer);
    procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
    function WareCanAddToIn(aWare: TKMWareType): Boolean; override;
    function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;
    procedure SetNewDeliveryMode(aValue: TKMDeliveryMode); override;
    function CanHaveWareType(aWare: TKMWareType): Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    property TotalCount : Integer read fTotalCount;
    property MaxCount : Integer read fMaxCount write fMaxCount;
    procedure BlockAll(aTakeOut, aBlocked : Boolean);

    {function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc : TKMPoint) : TKMPointDir; override;
    function Entrances : TKMPointDirArray; override;}
  end;
const
  MAX_STORE_CAPACITY = 4000;
  MAX_SMALL_STORE_CAPACITY = 1000;
  WARE_WEIGHT : array[WARE_MIN..WARE_MAX] of Byte =
  (
    6, 3, 2, 4, 4,
    4, 4, 3, 2, 2,
    1, 2, 1, 1, 12,
    1, 6, 6, 6, 6,
    6, 6, 6, 6, 6,
    6, 24, 1, 4, 1,//wtCorssbow..wtEgg
    4, 2, 6, 2, 2,//wtBitinOre..wtBitinE
    3, 2, 1, 2, 3,//wtWheel .. wtTile
    1, 1, 1, 1, 1,
    1, 6, 6, 1, 6,
    6, 2
  );

implementation
uses
  SysUtils, Math,
  KM_CommonExceptions,
  KM_Game,
  KM_GameParams,
  KM_GameTypes, KM_Resource,
  KM_HandsCollection, KM_HandEntity, KM_HandTypes,
  KM_ScriptingEvents;


{ TKMHouseStore }
procedure TKMHouseStore.Activate(aWasBuilt:boolean);
var
  FirstStore: TKMHouseStore;
  RT: TKMWareType;
begin
  inherited;
  //A new storehouse should inherrit the accept properies of the first storehouse of that player,
  //which stops a sudden flow of unwanted resources to it as soon as it is create.
  FirstStore := TKMHouseStore(gHands[Owner].FindHouse(htStore, 1));
  if (FirstStore <> nil) and not FirstStore.IsDestroyed then
    for RT := WARE_MIN to WARE_MAX do
      if RT in WARES_VALID then
      begin
        NotAcceptFlag[RT] := FirstStore.NotAcceptFlag[RT];
        NotAllowTakeOutFlag[RT] := FirstStore.NotAllowTakeOutFlag[RT];
        WareAIBlockAouto[RT] := false;
      end;

  if HouseType = htStore then
    fMaxCount := MAX_STORE_CAPACITY
  else
    fMaxCount := MAX_SMALL_STORE_CAPACITY;
  //ChangedByAIBuildScript := false;
end;



procedure TKMHouseStore.SetWareCnt(aWareType: TKMWareType; aValue: Word);
var
  cntChange: Integer;
  W: TKMWareType;
  storeWasFilled : Boolean;
begin
  Assert(aWareType in WARES_VALID);

  cntChange := aValue - fWaresCount[aWareType];

  fWaresCount[aWareType] := aValue;

  if cntChange <> 0 then
    gScriptEvents.ProcHouseWareCountChanged(Self, aWareType, aValue, cntChange);



  fTotalCount := 0;
  for W := Low(fWaresCount) to High(fWaresCount) do
    fTotalCount := (fWaresCount[W] * WARE_WEIGHT[W])  + fTotalCount;
  if gHands[Owner].IsHuman then
    if fTotalCount >= fMaxCount then
    begin
      StoreFilled := true;
      if DeliveryMode = dmDelivery then
        SetNewDeliveryMode(dmClosed);
    end else
    begin
      storeWasFilled := StoreFilled;
      StoreFilled := false;
      if storeWasFilled then
        if DeliveryMode = dmClosed then
            SetNewDeliveryMode(dmDelivery);
    end;
end;

function TKMHouseStore.CanHaveWareType(aWare: TKMWareType): Boolean;
begin
  Result := (aWare in WARES_VALID);

 // if HouseType = htSmallStore then
  //    Result := aWare = wtTrunk;

end;

constructor TKMHouseStore.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseStore');
  LoadStream.Read(fWaresCount, SizeOf(fWaresCount));
  LoadStream.Read(NotAcceptFlag, SizeOf(NotAcceptFlag));
  LoadStream.Read(NotAllowTakeOutFlag, SizeOf(NotAllowTakeOutFlag));
  LoadStream.Read(WareAIBlockAouto, SizeOf(WareAIBlockAouto));
  LoadStream.Read(fMaxCount);
  LoadStream.Read(fTotalCount);
end;


procedure TKMHouseStore.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
var
  W: TKMWareType;
begin
  case aWare of
    wtAll:     for W := Low(fWaresCount) to High(fWaresCount) do begin
                  SetWareCnt(W, EnsureRange(fWaresCount[W] + aCount, 0, High(Word)));
                  gHands[Owner].Deliveries.Queue.AddOffer(Self, W, aCount);
                end;
    WARE_MIN..
    WARE_MAX:   begin
                  SetWareCnt(aWare, EnsureRange(fWaresCount[aWare] + aCount, 0, High(Word)));
                  gHands[Owner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
                end;
    else        raise ELocError.Create('Cant''t add ' + gRes.Wares[aWare].Title, Position);
  end;

end;


function TKMHouseStore.WareCanAddToIn(aWare: TKMWareType): Boolean;
begin
  Result := (aWare in WARES_VALID);

  //if HouseType = htSmallStore then
  //  Result := aWare = wtTrunk;

  //Result := aWare = wtTrunk;
end;


function TKMHouseStore.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
begin
  Assert(aWare in WARES_VALID);
  Result := (fWaresCount[aWare] >= aCount);
end;


function TKMHouseStore.CheckWareIn(aWare: TKMWareType): Word;
begin
  Result := 0;
  if aWare in WARES_VALID then
    Result := fWaresCount[aWare]
  else
    if aWare <> wtNone then
      raise Exception.Create('Unexpected aWareType');
end;

function TKMHouseStore.CheckWareOut(aWare: TKMWareType): Word;
begin
  Result := CheckWareIn(aWare);
end;

function TKMHouseStore.CheckWareTotal(aWare: TKMWareType): Word;
begin
  Result := CheckWareIn(aWare);
end;

function TKMHouseStore.FoodCount: Integer;
var WT : TKMWareType;
begin
  Result := 0;
  for WT in WARES_HOUSE_FOOD do
    Result := Result + fWaresCount[WT];
end;

procedure TKMHouseStore.SetWareInCnt(aWare: TKMWareType; aValue: Integer);
begin
  if aWare in WARES_VALID then
    fWaresCount[aWare] := aValue;
end;

procedure TKMHouseStore.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  W: TKMWareType;
begin
  for W := WARE_MIN to WARE_MAX do
    if W in WARES_VALID then
      gHands[Owner].Stats.WareConsumed(W, fWaresCount[W]);

  inherited;
end;


procedure TKMHouseStore.WareTakeFromOut(aWare: TKMWareType; aCount: Word=1; aFromScript: Boolean = False);
begin
  if aFromScript then
  begin
    aCount := Min(aCount, fWaresCount[aWare]);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;
  Assert(aCount <= fWaresCount[aWare]);

  SetWareCnt(aWare, fWaresCount[aWare] - aCount);
end;


procedure TKMHouseStore.ToggleNotAcceptFlag(aWare: TKMWareType);
begin
  // Dunno why thats happening sometimes..
  Assert(aWare in WARES_VALID);

  if aWare in WARES_VALID then
  begin
    NotAcceptFlag[aWare] := not NotAcceptFlag[aWare];
    WareAIBlockAouto[aWare] := true;
  end;

end;

procedure TKMHouseStore.SetToggleNotAcceptFlag(aWare: TKMWareType; aAccept : Boolean);
begin
  NotAcceptFlag[aWare] := aAccept;
end;



procedure TKMHouseStore.ToggleNotAcceptTakeOutFlag(aWare: TKMWareType);
begin
  NotAllowTakeOutFlag[aWare] := not NotAllowTakeOutFlag[aWare];
end;

procedure TKMHouseStore.SetToggleNotAcceptTakeOutFlag(aWare: TKMWareType; aAccept : Boolean);
begin
  NotAllowTakeOutFlag[aWare] := aAccept;

end;

procedure TKMHouseStore.SetNewDeliveryMode(aValue: TKMDeliveryMode);
begin
  if aValue = dmDelivery then
    if StoreFilled and gHands[Owner].IsHuman then
      aValue := dmClosed;

  inherited;

end;

procedure TKMHouseStore.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('HouseStore');
  SaveStream.Write(fWaresCount, SizeOf(fWaresCount));
  SaveStream.Write(NotAcceptFlag, SizeOf(NotAcceptFlag));
  SaveStream.Write(NotAllowTakeOutFlag, SizeOf(NotAllowTakeOutFlag));
  SaveStream.Write(WareAIBlockAouto, SizeOf(WareAIBlockAouto));
  SaveStream.Write(fMaxCount);
  SaveStream.Write(fTotalCount);
end;


function TKMHouseStore.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  Result := inherited or NotAcceptFlag[aWareType]
                      or not CanHaveWareType(aWareType);
end;


//Check if we should abandon TakeOut delivery (evacuate) from this house to aToHouse
function TKMHouseStore.ShouldAbandonDeliveryFromTo(aToHouse: TKMHouse; aWareType: TKMWareType; aImmidiateCheck: Boolean): Boolean;
begin
  Result := inherited or ((aToHouse <> nil)
                           and (aToHouse is TKMHouseStore)
                           and aToHouse.IsComplete
                           and
                           (
                            (GetDeliveryModeForCheck(aImmidiateCheck) <> dmTakeOut)
                             or not NotAllowTakeOutFlag[aWareType]

                            )
                            );

end;

procedure TKMHouseStore.BlockAll(aTakeOut: Boolean; aBlocked: Boolean);
var WT : TKMWareType;
begin
  If aTakeOut then
  begin
    for WT := Low(NotAllowTakeOutFlag) to High(NotAllowTakeOutFlag) do
      NotAllowTakeOutFlag[WT] := aBlocked;

  end else
    for WT := Low(NotAcceptFlag) to High(NotAcceptFlag) do
      NotAcceptFlag[WT] := aBlocked;
end;

{
function TKMHouseStore.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHouseStore.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirS),
              KMPointDir(Entrance.X, Entrance.Y - 2, dirN)
            ];
end;


function TKMHouseStore.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
const  ENTRANCE_POS : array[1..2] of TKMPoint = ( (X : 0; Y : 0),
                                                (X : 0; Y : -2));
const  ENTRANCE_DIR : array[1..2] of TKMDirection = (dirS, dirN);

var I : Integer;
  lastDist, tmp : Single;
begin
  Result := Inherited;
  //return random for testing;

  lastDist := 99999;
  for I := low(ENTRANCE_POS) to High(ENTRANCE_POS) do
  begin
    tmp := KMLength(aLoc, Entrance + ENTRANCE_POS[I]);
    If tmp < lastDist then
    begin
      lastDist := tmp;
      Result.Loc := Entrance + ENTRANCE_POS[I];
      Result.Dir := ENTRANCE_DIR[I];
    end;
  end;

end;  }

end.
