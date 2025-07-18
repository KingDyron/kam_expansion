unit KM_HouseSchool;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,
  KM_Houses,
  KM_ResTypes;


type
  //School has one unique property - queue of units to be trained, 1 wip + 5 in line
  TKMHouseSchool = class(TKMHouse)
  private
    fUnitWip: Pointer;  //can't replace with TKMUnit since it will lead to circular reference in KM_House-KM_Units
    fHideOneGold: Boolean; //Hide the gold incase Player cancels the training, then we won't need to tweak DeliverQueue order
    fTrainProgress: Byte; //Was it 150 steps in KaM?
    fQueue: array [0..5] of TKMUnitType;
    function GetQueue(aIndex: Integer): TKMUnitType; //Used in UI. First item is the unit currently being trained, 1..5 are the actual queue
    procedure CreateUnit; //This should Create new unit and start training cycle
    procedure StartTrainingUnit; //This should Create new unit and start training cycle
    procedure CancelTrainingUnit;
    procedure SetQueue(aIndex: Integer; aValue: TKMUnitType);
    property PrivateQueue[aIndex: Integer]: TKMUnitType read GetQueue write SetQueue;
  protected
    function GetWareInLocked(aI: Byte): Word; override;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;
    function AddUnitToQueue(aUnit: TKMUnitType; aCount: Integer): Byte; //Should add unit to queue if there's a place
    procedure ChangeUnitTrainOrder(aNewPosition: Integer); overload; //Change last unit in queue training order
    procedure ChangeUnitTrainOrder(aOldPosition, aNewPosition: Integer); overload; //Change unit order in queue
    procedure RemUnitFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure UnitTrainingComplete(aUnit: Pointer); //This should shift queue filling rest with utNone
    function GetTrainingProgress: Single;
    function QueueCount: Byte;
    function LastUnitPosInQueue: Integer;
    function QueueIsEmpty: Boolean;
    function QueueIsFull: Boolean;
    function QueueLength: Byte;
    property HideOneGold: Boolean read fHideOneGold;
    property Queue[aIndex: Integer]: TKMUnitType read GetQueue;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick : Cardinal); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_Entity,
  KM_Points,
  KM_Units, KM_HandLogistics,
  KM_Resource,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_RenderPool;


{ TKMHouseSchool }
constructor TKMHouseSchool.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := 0 to High(fQueue) do
    fQueue[I] := utNone;
end;


constructor TKMHouseSchool.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseSchool');
  LoadStream.Read(fUnitWip, 4);
  LoadStream.Read(fHideOneGold);
  LoadStream.Read(fTrainProgress);
  LoadStream.Read(fQueue, SizeOf(fQueue));
end;


procedure TKMHouseSchool.SyncLoad;
begin
  inherited;

  fUnitWip := gHands.GetUnitByUID(Integer(fUnitWip));
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseSchool.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  I: Integer;
begin
  for I := 1 to High(fQueue) do
    PrivateQueue[I] := utNone;
  RemUnitFromQueue(0); //Remove WIP unit

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseSchool.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
begin
  inherited;

  if fUnitWip = nil then
    StartTrainingUnit;
end;


//Add units to training queue
//aCount allows to add several units at once (but not more than Schools queue can fit)
//Returns the number of units successfully added to the queue
function TKMHouseSchool.AddUnitToQueue(aUnit: TKMUnitType; aCount: Integer): Byte;
var
  I, K: Integer;
begin
  Result := 0;
  if aCount <= 0 then Exit;

  for K := 1 to Min(aCount, Length(fQueue)) do
  for I := 1 to High(fQueue) do
  if fQueue[I] = utNone then
  begin
    Inc(Result);
    PrivateQueue[I] := aUnit;
    if I = 1 then
      StartTrainingUnit; //If thats the first unit then start training it
    Break;
  end;
end;


procedure TKMHouseSchool.CancelTrainingUnit;
begin
  SetState(hstIdle);
  if fUnitWip <> nil then
  begin //Make sure unit started training
    TKMUnit(fUnitWip).CloseUnit(False); //Don't remove tile usage, we are inside the school
    fHideOneGold := False;
    //Add 1 gold to offer to take it out
    if DeliveryMode = dmTakeOut then
      gHands[Owner].Deliveries.Queue.AddOffer(Self, wtGold, 1);
  end;
  fUnitWip := nil;
  PrivateQueue[0] := utNone; //Removed the in training unit
end;


function TKMHouseSchool.GetWareInLocked(aI: Byte): Word;
begin
  Result := inherited GetWareInLocked(aI);

  if aI = 1 then //for gold only (1 - based)
    Inc(Result, Byte(HideOneGold)); //We Lock 1 gold if its hidden (while unit training)
end;


procedure TKMHouseSchool.SetQueue(aIndex: Integer; aValue: TKMUnitType);
begin
  if fQueue[aIndex] <> utNone then
    gHands[Owner].Stats.UnitRemovedFromTrainingQueue(fQueue[aIndex]);

  if aValue <> utNone then
    gHands[Owner].Stats.UnitAddedToTrainingQueue(aValue);
  fQueue[aIndex] := aValue;
end;


//Change unit priority in training queue
procedure TKMHouseSchool.ChangeUnitTrainOrder(aNewPosition: Integer);
begin
  ChangeUnitTrainOrder(Max(LastUnitPosInQueue, 1), aNewPosition);
end;


//Change unit priority in training queue
procedure TKMHouseSchool.ChangeUnitTrainOrder(aOldPosition, aNewPosition: Integer);
var
  tmpUnit: TKMUnitType;
  I: Integer;
begin
  Assert((aNewPosition >= 0) and (aOldPosition <= 5));

  if aOldPosition = 0 then Exit;

  // Do not cancel current training process, if unit type is the same.
  // Or set newPos to 1, if there is no training now (no gold, for example)
  if (aNewPosition = 0) and ((fQueue[aOldPosition] = fQueue[0]) or (fQueue[0] = utNone)) then
    aNewPosition := 1;

  if (fQueue[aOldPosition] = utNone) or (aOldPosition = aNewPosition) then Exit;

  Assert(aNewPosition < aOldPosition);

  tmpUnit := fQueue[aOldPosition];
  for I := aOldPosition downto Max(aNewPosition, 0) + 1 do
    PrivateQueue[I] := fQueue[I-1];

  if (aNewPosition = 0) then
    CancelTrainingUnit;

  PrivateQueue[aNewPosition] := tmpUnit;

  if (aNewPosition = 0) then
    CreateUnit;
end;


//DoCancelTraining and remove untrained unit
procedure TKMHouseSchool.RemUnitFromQueue(aID: Byte);
var
  I: Integer;
begin
  if fQueue[aID] = utNone then Exit; //Ignore clicks on empty queue items

  if aID = 0 then
  begin
    CancelTrainingUnit;
    StartTrainingUnit; //Start on the next unit in the queue
  end
  else
  begin
    for I := aID to High(fQueue) - 1 do
      PrivateQueue[I] := fQueue[I+1]; //Shift by one
    PrivateQueue[High(fQueue)] := utNone; //Set the last one empty
  end;
end;


procedure TKMHouseSchool.CreateUnit;
begin
  fHideOneGold := True;
  //Remove 1 gold from offer to take it out
  if DeliveryMode = dmTakeOut then
    gHands[Owner].Deliveries.Queue.RemOffer(Self, wtGold, 1);

  //Create the Unit
  fUnitWip := gHands[Owner].TrainUnit(fQueue[0], Self);
  TKMUnit(fUnitWip).InHouse := nil; // unit is not trained yet, so we should not set him as 'InHouse'
  TKMUnit(fUnitWip).TrainInHouse(Self); //Let the unit start the training task

  WorkAnimStep := 0;
end;


procedure TKMHouseSchool.StartTrainingUnit;
var
  I: Integer;
begin
  if fQueue[0] <> utNone then exit; //If there's currently no unit in training
  if fQueue[1] = utNone then exit; //If there is a unit waiting to be trained

  if CheckWareIn(wtGold) = 0 then exit; //There must be enough gold to perform training
  if gHands[Owner].GetWorklessCount = 0 then Exit;

  for I := 0 to High(fQueue) - 1 do
    PrivateQueue[I] := fQueue[I+1]; //Shift by one
  PrivateQueue[High(fQueue)] := utNone; //Set the last one empty

  CreateUnit;
end;


//Unit reports back to School that it was trained
procedure TKMHouseSchool.UnitTrainingComplete(aUnit: Pointer);
begin
  Assert(aUnit = fUnitWip, 'Should be called only by Unit itself when it''s trained');

  fUnitWip := nil;
  PrivateQueue[0] := utNone; //Clear the unit in training
  //Script command might have taken the gold while we were training, in which case ignore it (either way, gold is consumed)
  if CheckWareIn(wtGold) > 0 then
  begin
    WareTakeFromIn(wtGold); //Do the goldtaking
    gHands[Owner].Stats.WareConsumed(wtGold);
    gHands[Owner].TakeWorkless;
  end;
  fHideOneGold := False;
  fTrainProgress := 0;

  //Attempt to start training next unit in queue
  StartTrainingUnit;
end;


//Return training progress of a unit in 0 - 1 range
function TKMHouseSchool.GetQueue(aIndex: Integer): TKMUnitType;
begin
  Result := fQueue[aIndex];
end;


function TKMHouseSchool.GetTrainingProgress: Single;
begin

  if fUnitWip = nil then
    Result := 0
  else
    Result := WorkingTime / TotalWorkingTime

    {Result := (
              Byte(haWork2 in CurrentAction.SubAction) * 30 +
              Byte(haWork3 in CurrentAction.SubAction) * 60 +
              Byte(haWork4 in CurrentAction.SubAction) * 90 +
              Byte(haWork5 in CurrentAction.SubAction) * 120 +
              Byte(CurrentAction.State = hstWork) * WorkAnimStep
              ) / 150;}
end;


function TKMHouseSchool.QueueIsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(fQueue) do
    Result := Result and (fQueue[I] = utNone);
end;


function TKMHouseSchool.QueueCount: Byte;
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
function TKMHouseSchool.LastUnitPosInQueue: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(fQueue) do
    if fQueue[I] <> utNone then
      Result := I
end;


function TKMHouseSchool.QueueIsFull: Boolean;
begin
  Result := (fQueue[High(fQueue)] <> utNone);
end;


function TKMHouseSchool.QueueLength: Byte;
begin
  Result := Length(fQueue);
end;


procedure TKMHouseSchool.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseSchool');
  SaveStream.Write(TKMUnit(fUnitWip).UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fHideOneGold);
  SaveStream.Write(fTrainProgress);
  SaveStream.Write(fQueue, SizeOf(fQueue));
end;


procedure TKMHouseSchool.UpdateState(aTick : Cardinal);
begin
  Inherited;

  if fUnitWip = nil then
    StartTrainingUnit;



end;

procedure TKMHouseSchool.Paint;
var step : Integer;
begin
  Inherited;
  if not IsComplete then
    Exit;

  if (WorkingTime = 0) or (TotalWorkingTime = 0)  then
    step := 0
  else
    step := Trunc(gRes.Houses.School_Clock.Count * (WorkingTime / TotalWorkingTime));
  gRenderPool.AddHouseSchoolClock(Position, step);
end;


end.
