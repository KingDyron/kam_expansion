unit KM_HouseSiegeWorkshop;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonTypes,
  KM_CommonClasses, KM_Defaults,
  KM_Houses,
  KM_ResTypes;

Const
  MAX_MACHINES_WAITING = 4;
type
  TKMSiegeMachine = record
    UT : TKMUnitType;
    BitinAdded : Byte;
  end;

  TKMHouseSiegeWorkshop = class(TKMHouseWFlagPoint)
  private
    fPhaseDuration : Integer;
    fUnitType : TKMUnitType;
    fPhase : Byte;
    fQueue: array [0..5] of TKMUnitType;
    fBitinAdded : Byte;
    fMachinesWaiting : array[0..MAX_MACHINES_WAITING - 1] of TKMSiegeMachine;
    function GetQueue(aIndex: Integer): TKMUnitType; //Used in UI. First item is the unit currently being trained, 1..5 are the actual queue
    procedure CreateUnit; //This should Create new unit and start training cycle
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure SetQueue(aIndex: Integer; aValue: TKMUnitType);
    property PrivateQueue[aIndex: Integer]: TKMUnitType read GetQueue write SetQueue;
    function GetPhasesCount : Byte;
    function HasEnoughResources : Boolean;
    procedure AddWaitingMachine(aUnitType : TKMUnitType; aBitinCount : Byte);
    function GetMachineWating(aIndex : Integer) : TKMSiegeMachine;
    procedure TryEquipMachines;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;
    procedure AddBitinToProd;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;
    function AddUnitToQueue(aUnit: TKMUnitType; aCount: Integer): Byte; //Should add unit to queue if there's a place
    procedure RemUnitFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure UnitTrainingComplete; //This should shift queue filling rest with utNone
    function GetTrainingProgress: Single;
    function QueueCount: Byte;
    function LastUnitPosInQueue: Integer;
    function QueueIsEmpty: Boolean;
    function QueueIsFull: Boolean;
    function QueueLength: Byte;
    property Queue[aIndex: Integer]: TKMUnitType read GetQueue;

    property MachineWaiting[aIndex : Integer] : TKMSiegeMachine read GetMachineWating;
    function GetMWGuiIcons : TKMWord2Array;
    function GetOperatorsInsideCnt : Byte;

    property PhasesCount : Byte read GetPhasesCount;
    function GetNeededWares : TKMWarePlan;
    class function GetTotalCost(aUnitType : TKMUnitType) : TKMWarePlan;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
  end;

const SIEGE_CYCLES = 30;// total equiping time is  Siege_Cycles * 15
      SIEGE_PHASE_COUNT = 4;
implementation
uses
  KM_Entity,
  KM_Units, KM_Resource, KM_ResUnits,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity, KM_GameInfo, KM_UnitWarrior;


{ TKMHouseSiegeWorkshop }
constructor TKMHouseSiegeWorkshop.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := 0 to High(fQueue) do
    fQueue[I] := utNone;
  fBitinAdded := 0;
  for I := 0 to High(fMachinesWaiting) do
    fMachinesWaiting[I].UT := utNone;
end;



procedure TKMHouseSiegeWorkshop.SyncLoad;
begin
  inherited;
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSiegeWorkshop.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  I: Integer;
begin
  for I := 1 to High(fQueue) do
    PrivateQueue[I] := utNone;
  RemUnitFromQueue(0); //Remove WIP unit

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSiegeWorkshop.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
begin
  inherited;
end;

function TKMHouseSiegeWorkshop.GetPhasesCount : Byte;
begin

  if gRes.Units[fUnitType].SiegePhasesCount > 0 then
  begin
    Result := gRes.Units[fUnitType].SiegePhasesCount;
  end else
  case fUnitType of
    utCatapult: Result := 4;
    utBallista: Result := 3;
    utRam:  Result := 6;
    else Result := 0;

  end;

end;

function TKMHouseSiegeWorkshop.HasEnoughResources : Boolean;
var I : Integer;
  warePlan : TKMWarePlan;
begin
  Result := true;

  warePlan := GetNeededWares;

  if warePlan[0].W = wtNone then
    Exit(false);

  for I := 0 to 3 do
    if warePlan[I].W <> wtNone then
      if not (CheckWareIn(warePlan[I].W) >= warePlan[I].C) then
        Exit(false);
end;

function TKMHouseSiegeWorkshop.GetNeededWares : TKMWarePlan;
var UT : TKMUnitType;
begin
  Result.SetCount(4);
  if Queue[0] = utNone then
    UT := Queue[1]
  else
    UT := fUnitType;

   Result[0].W := wtNone;
   Result[1].W := wtNone;
   Result[2].W := wtNone;
   Result[3].W := wtNone;

  if gRes.Units[UT].SiegePhasesCount > 0 then
  begin
    Result := gRes.Units[UT].SiegeCost;
  end else
  case UT of
    utCatapult: begin
                   Result[0].W := wtSteelE;
                   Result[0].C := 2;
                   Result[1].W := wtLog;
                   Result[1].C := 2;
                   Result[2].W := wtWheel;
                   Result[2].C := 1;
                end;

    utBallista: begin
                   Result[0].W := wtSteelE;
                   Result[0].C := 3;
                   Result[1].W := wtLog;
                   Result[1].C := 2;
                   Result[2].W := wtWheel;
                   Result[2].C := 1;
                end;

    utRam:  case fPhase of
              0, 1, 2, 3, 4:  begin
                                 Result[0].W := wtSteelE;
                                 Result[0].C := 2;
                                 Result[1].W := wtLog;
                                 Result[1].C := 2;
                                 Result[2].W := wtWheel;
                                 Result[2].C := 1;
                              end;
              5, 6:  begin
                       Result[0].W := wtSteelE;
                       Result[0].C := 3;
                       Result[1].W := wtNone;
                       Result[2].W := wtNone;
                       Result[3].W := wtNone;
                     end;
            end;
  end;

  if UT in WARRIOR_BITIN_EQUIPABLE then
    if CheckWareIn(wtBitinE) >= 1 then
    begin
      Result[3].W := wtBitinE;
      Result[3].C := 1;
      AddBitinToProd;
    end;

end;

class function TKMHouseSiegeWorkshop.GetTotalCost(aUnitType : TKMUnitType) : TKMWarePlan;
var I : Integer;
begin
   Result.SetCount(4);
   Result[0].W := wtNone;
   Result[1].W := wtNone;
   Result[2].W := wtNone;
   Result[3].W := wtNone;

  if gRes.Units[aUnitType].SiegePhasesCount > 0 then
  begin
    gRes.Units[aUnitType].SiegeCost.CopyTo(Result);
    for I := 1 to gRes.Units[aUnitType].SiegePhasesCount do
    begin
       Inc(Result[0].C, gRes.Units[aUnitType].SiegeCost[0].C);
       Inc(Result[1].C, gRes.Units[aUnitType].SiegeCost[1].C);
       Inc(Result[2].C, gRes.Units[aUnitType].SiegeCost[2].C);
       Inc(Result[3].C, gRes.Units[aUnitType].SiegeCost[3].C);
    end;
  end;

  case aUnitType of
    utCatapult: begin
                   Result[0].W := wtSteelE;
                   Result[0].C := 2 * 4;
                   Result[1].W := wtLog;
                   Result[1].C := 2 * 4;
                   Result[2].W := wtWheel;
                   Result[2].C := 1 * 4;
                end;

    utBallista: begin
                   Result[0].W := wtSteelE;
                   Result[0].C := 3 * 3;
                   Result[1].W := wtLog;
                   Result[1].C := 2 * 3;
                   Result[2].W := wtWheel;
                   Result[2].C := 1 * 3;
                end;

    utRam:  begin
               Result[0].W := wtSteelE;
               Result[0].C := 2 * 5 + 3;
               Result[1].W := wtLog;
               Result[1].C := 2 * 5;
               Result[2].W := wtWheel;
               Result[2].C := 1 * 5;
            end;
  end;

end;

procedure TKMHouseSiegeWorkshop.AddBitinToProd;
begin
  fBitinAdded := Min(fBitinAdded + 1, 4);
end;

//Add units to training queue
//aCount allows to add several units at once (but not more than Schools queue can fit)
//Returns the number of units successfully added to the queue
function TKMHouseSiegeWorkshop.AddUnitToQueue(aUnit: TKMUnitType; aCount: Integer): Byte;
var
  I, K: Integer;
begin
  Result := 0;
  if aCount <= 0 then Exit;
  //if not (aUnit in SIEGE_MACHINES) then Exit;

  for K := 1 to Min(aCount, Length(fQueue)) do
    for I := 1 to High(fQueue) do
    if fQueue[I] = utNone then
    begin
      Inc(Result);
      PrivateQueue[I] := aUnit;
      Break;
    end;
  StartTrainingUnit;
end;



procedure TKMHouseSiegeWorkshop.SetQueue(aIndex: Integer; aValue: TKMUnitType);
begin
  if fQueue[aIndex] <> utNone then
    gHands[Owner].Stats.UnitRemovedFromTrainingQueue(fQueue[aIndex]);

  if aValue <> utNone then
    gHands[Owner].Stats.UnitAddedToTrainingQueue(aValue);

  fQueue[aIndex] := aValue;

end;

//DoCancelTraining and remove untrained unit
procedure TKMHouseSiegeWorkshop.RemUnitFromQueue(aID: Byte);
var
  I: Integer;
begin
  if fQueue[aID] = utNone then Exit; //Ignore clicks on empty queue items

  if aID = 0 then
  begin
    fQueue[aID] := utNone;
    fPhase := 0;
    fUnitType := utNone;
    fBitinAdded := 0;
    self.CurrentAction.State := hstIdle;
    if Worker <> nil then
    begin
      TKMUnit(Worker).SetActionLockedStay(50, uaWalk);
      TKMUnit(Worker).CancelTask;
    end;

    StartTrainingUnit; //Start on the next unit in the queue
  end
  else
  begin
    for I := aID to High(fQueue) - 1 do
      PrivateQueue[I] := fQueue[I+1]; //Shift by one
    PrivateQueue[High(fQueue)] := utNone; //Set the last one empty
  end;
end;


procedure TKMHouseSiegeWorkshop.CreateUnit;
begin
  fUnitType := fQueue[0];

  WorkAnimStep := 0;
end;

procedure TKMHouseSiegeWorkshop.StartTrainingUnit;
  function HasNoWaitingSpace : Boolean;
  var I : Integer;
  begin
    Result := true;
    for I := 0 to High(fMachinesWaiting) do
      if fMachinesWaiting[I].UT = utNone then
        Exit(false);
  end;
var
  I: Integer;
begin
  if fQueue[0] <> utNone then exit; //If there's currently no unit in training
  if fQueue[1] = utNone then exit; //If there is a unit waiting to be trained
  //there must be enough resources to train machines

  if not HasEnoughResources then Exit;
  if not HasWorker then Exit;
  if HasNoWaitingSpace then Exit; //must have some space

  for I := 0 to High(fQueue) - 1 do
    PrivateQueue[I] := fQueue[I+1]; //Shift by one

  PrivateQueue[High(fQueue)] := utNone; //Set the last one empty

  CreateUnit;
end;
function TKMHouseSiegeWorkshop.GetMachineWating(aIndex: Integer): TKMSiegeMachine;
begin
  Assert(InRange(aIndex, 0, high(fMachinesWaiting)));

  Result := fMachinesWaiting[aIndex];
end;

function TKMHouseSiegeWorkshop.GetMWGuiIcons: TKMWord2Array;
var I : Integer;
begin
  SetLength(Result, 0);
  for I := 0 to High(fMachinesWaiting) do
    if fMachinesWaiting[I].UT <> utNone then
    begin
      SetLength(Result, length(Result) + 1);
      Result[high(Result)] := [gRes.Units[fMachinesWaiting[I].UT].GUIIcon];
    end;

end;
function TKMHouseSiegeWorkshop.GetOperatorsInsideCnt: Byte;
var I : Integer;
begin
  Result := 0;
  for I := 0 to high(fWorkers) do
    If (TKMUnit(fWorkers[I]).UnitType = utOperator) and (TKMUnit(fWorkers[I]).InHouse = self) then
      Inc(Result);
end;


procedure TKMHouseSiegeWorkshop.AddWaitingMachine(aUnitType: TKMUnitType; aBitinCount: Byte);
var I, aIndex : Integer;
begin
  aIndex := -1;
  for I := 0 to High(fMachinesWaiting) do
    if fMachinesWaiting[I].UT = utNone then
    begin
      aIndex := I;
      Break;
    end;
  Assert(aIndex <> -1, 'TKMHouseSiegeWorkshop.AddWaitingMachine, Siege workshop created too much machines');

  fMachinesWaiting[aIndex].UT := aUnitType;
  fMachinesWaiting[aIndex].BitinAdded := aBitinCount;
end;
//Unit reports back to School that it was trained
procedure TKMHouseSiegeWorkshop.UnitTrainingComplete;

begin
  inc(fPhase);

  fUnitType := PrivateQueue[0];

  if fPhase <> GetPhasesCount then
    Exit;

  PrivateQueue[0] := utNone; //Clear the unit in training
  AddWaitingMachine(fUnitType, fBitinAdded);
  //EquipWarrior(fUnitType);
  fUnitType := utNone;
  fBitinAdded := 0;
  fPhase := 0;
  //Attempt to start training next unit in queue
  StartTrainingUnit;
end;

procedure TKMHouseSiegeWorkshop.TryEquipMachines;
  procedure EquipWarrior(aMachine : TKMSiegeMachine);
  var
    U: TKMUnit;
  begin

    //Make new unit
    U := gHands[Owner].TrainUnit(aMachine.UT, Self);
    U.Visible := False; //Make him invisible as he is inside the barracks
    U.Condition := UNIT_MAX_CONDITION; //All soldiers start with 3/4, so groups get hungry at the same time
    //Soldier.OrderLoc := KMPointBelow(Entrance); //Position in front of the barracks facing north
    U.SetActionGoIn(uaWalk, gdGoOutside, Self, true);
    if Assigned(U.OnUnitTrained) then
      U.OnUnitTrained(U);

    if gHands[Owner].IsComputer then
      if aMachine.UT in UNITS_WARRIORS then
        if gRes.Units[U.UnitType].CanOrderAmmo then
          TKMUnitWarrior(U).OrderAmmo;

    if aMachine.UT in SIEGE_MACHINES then
      TKMUnitWarrior(U).AddBitin(aMachine.BitinAdded);

    ProduceFestivalPoints(fptWarfare, 30);
    If aMachine.BitinAdded > 0 then
      ProduceFestivalPoints(fptWarfare, 10 * aMachine.BitinAdded);
  end;
var I, operatorsTaken, aIndex : Integer;
  U : TKMUnit;
begin
  if WorkersCount(utOperator) < OPERATORS_PER_MACHINE then  Exit; //every machine need 4 operators
  aIndex := -1;
  for I := High(fMachinesWaiting) downto 0 do
    if fMachinesWaiting[I].UT <> utNone then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
    Exit;
  operatorsTaken := 0;
  for I := high(fWorkers) downto 0 do
  begin
    U := TKMUnit(fWorkers[I]);
    if (U <> nil)
    and (U.UnitType = utOperator)
    and (U.InHouse <> nil)
    and not U.IsDeadOrDying then
      Inc(operatorsTaken);
  end;
  if operatorsTaken < OPERATORS_PER_MACHINE then
    Exit;
  operatorsTaken := 0;
  for I := high(fWorkers) downto 0 do
  begin
    U := TKMUnit(fWorkers[I]);
    if operatorsTaken < OPERATORS_PER_MACHINE then
      if (U <> nil)
      and (U.InHouse <> nil)
      and (U.UnitType = utOperator)
      and not U.IsDeadOrDying then
      begin
        TKMCivilUnit(U).KillInHouse;
        Inc(operatorsTaken);
      end;
  end;
  EquipWarrior(fMachinesWaiting[aIndex]);
  fMachinesWaiting[aIndex].UT := utNone;

end;


//Return training progress of a unit in 0 - 1 range
function TKMHouseSiegeWorkshop.GetQueue(aIndex: Integer): TKMUnitType;
begin
  Result := fQueue[aIndex];
end;


function TKMHouseSiegeWorkshop.GetTrainingProgress: Single;
begin

  if fUnitType = utNone then
    Result := 0
  else
    Result := ((WorkingTime) + (fPhase * fPhaseDuration)) / Max(fPhaseDuration * GetPhasesCount, 1);

end;


function TKMHouseSiegeWorkshop.QueueIsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(fQueue) do
    Result := Result and (fQueue[I] = utNone);
end;


function TKMHouseSiegeWorkshop.QueueCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(fQueue) do
    if fQueue[I] <> utNone then
      Inc(Result);
end;


// Returns position of the last unit in queue.
// If queue is empty, return -1
function TKMHouseSiegeWorkshop.LastUnitPosInQueue: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(fQueue) do
    if fQueue[I] <> utNone then
      Result := I
end;


function TKMHouseSiegeWorkshop.QueueIsFull: Boolean;
begin
  Result := (fQueue[High(fQueue)] <> utNone);
end;


function TKMHouseSiegeWorkshop.QueueLength: Byte;
begin
  Result := Length(fQueue);
end;



constructor TKMHouseSiegeWorkshop.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseSiegeWorkShop');
  LoadStream.Read(fUnitType, SizeOf(fUnitType));
  LoadStream.Read(fQueue, SizeOf(fQueue));
  LoadStream.Read(fPhase);
  LoadStream.Read(fBitinAdded);
  LoadStream.Read(fMachinesWaiting, SizeOf(fMachinesWaiting));
end;

procedure TKMHouseSiegeWorkshop.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseSiegeWorkShop');
  SaveStream.Write(fUnitType,SizeOf(fUnitType)); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fQueue, SizeOf(fQueue));
  SaveStream.Write(fPhase);
  SaveStream.Write(fBitinAdded);
  SaveStream.Write(fMachinesWaiting, SizeOf(fMachinesWaiting));
end;

procedure TKMHouseSiegeWorkshop.UpdateState(aTick: Cardinal);
begin
  Inherited;

  if TotalWorkingTime > fPhaseDuration then
    fPhaseDuration := TotalWorkingTime;


  StartTrainingUnit;
  TryEquipMachines;
end;


end.
