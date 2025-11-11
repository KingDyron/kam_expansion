unit KM_HouseQueue;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,
  KM_Houses,
  KM_ResTypes;

Const
  QUEUE_LENGTH = 6;
type
  TKMWareQueue = record
    W : TKMWareType;
    Qt : Byte;
  end;

  TKMHouseQueue = class(TKMHouse)
  private
    fQueue: array [0..QUEUE_LENGTH - 1] of TKMWareQueue;
    fNotRemLastPos : Boolean;
    function GetQueue(aIndex: Integer): TKMWareQueue; //Used in UI. First item is the unit currently being trained, 1..5 are the actual queue
    procedure MoveQueue; //This should Create new unit and start training cycle
    procedure SetQueue(aIndex: Integer; aValue: TKMWareQueue);
    property PrivateQueue[aIndex: Integer]: TKMWareQueue read GetQueue write SetQueue;
  protected
    function GetWareInLocked(aI: Byte): Word; override;
  public
    InProgress : Boolean;
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;

    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False); override;

    function AddWareToQueue(aWare : TKMWareType; aQty, aCount: Integer): Byte; //Should add unit to queue if there's a place
    procedure RemWareFromQueue(aID: Byte); //Should remove unit from queue and shift rest up
    procedure ProductionComplete; //This should shift queue filling rest with utNone
    function GetProgress : Single;
    function QueueCount: Byte;
    function LastWarePosInQueue: Integer;
    function QueueIsEmpty: Boolean;
    function QueueIsFull: Boolean;
    function QueueLength: Byte;
    property Queue[aIndex: Integer]: TKMWareQueue read GetQueue;
    property NotRemLastPos : Boolean read fNotRemLastPos write fNotRemLastPos;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
  end;

implementation
uses
  KM_Entity,
  KM_Units,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity, KM_GameInfo, KM_UnitWarrior;


{ TKMHouseSiegeWorkshop }
constructor TKMHouseQueue.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := 0 to High(fQueue) do
  begin
    fQueue[I].Qt := 0;
    fQueue[I].W := wtNone;
  end;
  fNotRemLastPos := false;
end;



procedure TKMHouseQueue.SyncLoad;
begin
  inherited;
end;


//Remove all queued units first, to avoid unnecessary shifts in queue
procedure TKMHouseQueue.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
var
  I: Integer;
  W : TKMWareQueue;
begin
  for I := 1 to High(fQueue) do
  begin
    W.W := wtNone;
    W.Qt := 0;
    PrivateQueue[I] := W;
  end;

  inherited;
end;


//Add resource as usual and initiate unit training
procedure TKMHouseQueue.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromScript: Boolean = False);
begin
  inherited;
  MoveQueue;
end;

//Add units to training queue
//aCount allows to add several units at once (but not more than Schools queue can fit)
//Returns the number of units successfully added to the queue
function TKMHouseQueue.AddWareToQueue(aWare : TKMWareType; aQty, aCount: Integer): Byte;
var
  I, K: Integer;
  W : TKMWareQueue;
begin
  Result := 0;
  if aWare = wtNone then Exit;
  if aCount <= 0 then Exit;
  if aQty = 0 then Exit;

  for K := 1 to Min(aCount, Length(fQueue)) do
    for I := 1 to High(fQueue) do
    if fQueue[I].W = wtNone then
    begin
      Inc(Result);
      W.W := aWare;
      W.Qt := aQty;
      PrivateQueue[I] := W;

      if I = 1 then
        MoveQueue;

      Break;
    end;
end;

function TKMHouseQueue.GetWareInLocked(aI: Byte): Word;
begin
  Result := inherited GetWareInLocked(aI);
end;

//DoCancelTraining and remove untrained unit
procedure TKMHouseQueue.RemWareFromQueue(aID: Byte);
var
  I: Integer;
begin
  if aID > high(fQueue) then Exit;
  
  if fQueue[aID].W = wtNone then Exit; //Ignore clicks on empty queue items

  if aID = 0 then
  begin
    fQueue[0].W := wtNone;
    fQueue[0].Qt := 0;
    MoveQueue; //Start on the next unit in the queue
  end
  else
  begin
    for I := aID to High(fQueue) - 1 do
      PrivateQueue[I] := fQueue[I+1]; //Shift by one

    fQueue[High(fQueue)].W := wtNone;
    fQueue[High(fQueue)].Qt := 0;
  end;
end;

procedure TKMHouseQueue.MoveQueue;
var
  I: Integer;
begin
  if fQueue[0].W <> wtNone then exit; //If there's currently no unit in training
  if fQueue[1].W = wtNone then exit; //If there is a unit waiting to be trained

  for I := 0 to High(fQueue) - 1 do
    PrivateQueue[I] := fQueue[I+1]; //Shift by one

  fQueue[High(fQueue)].W := wtNone;
  fQueue[High(fQueue)].Qt := 0;
end;


//Unit reports back to School that it was trained
procedure TKMHouseQueue.ProductionComplete;
begin
  If fNotRemLastPos then
    If (fQueue[1].W = wtNone) or (fQueue[1].Qt = 0) then
    Exit;
  fQueue[0].W := wtNone; //Clear the unit in training
  fQueue[0].Qt := 0; //Clear the unit in training
  //Attempt to start training next unit in queue
  MoveQueue;
end;


//Return training progress of a unit in 0 - 1 range
function TKMHouseQueue.GetQueue(aIndex: Integer): TKMWareQueue;
begin
  Result := fQueue[aIndex];
end;

procedure TKMHouseQueue.SetQueue(aIndex: Integer; aValue: TKMWareQueue);
begin
  fQueue[aIndex] := aValue;
end;


function TKMHouseQueue.QueueIsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to High(fQueue) do
    Result := Result and (fQueue[I].W = wtNone);
end;


function TKMHouseQueue.QueueCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(fQueue) do
    if fQueue[I].W <> wtNone then
      Inc(Result);
end;


// Returns position of the last unit in queue.
// If queue is empty, return -1
function TKMHouseQueue.LastWarePosInQueue: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(fQueue) do
    if fQueue[I].W <> wtNone then
      Result := I
end;


function TKMHouseQueue.QueueIsFull: Boolean;
begin
  Result := (fQueue[High(fQueue)].W <> wtNone);
end;


function TKMHouseQueue.QueueLength: Byte;
begin
  Result := Length(fQueue);
end;

function TKMHouseQueue.GetProgress: Single;
begin
  Result := 0;
  if fQueue[0].W = wtNone then
    Exit;
  Result := self.WorkingTime / TotalWorkingTime;
end;


constructor TKMHouseQueue.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseSiegeWorkShop');
  LoadStream.Read(fQueue, SizeOf(fQueue));
  LoadStream.Read(InProgress);
  LoadStream.Read(fNotRemLastPos);
end;

procedure TKMHouseQueue.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseSiegeWorkShop');
  SaveStream.Write(fQueue, SizeOf(fQueue));
  SaveStream.Write(InProgress);
  SaveStream.Write(fNotRemLastPos);
end;

procedure TKMHouseQueue.UpdateState(aTick: Cardinal);
begin
  Inherited;
  //MoveQueue;
end;


end.
