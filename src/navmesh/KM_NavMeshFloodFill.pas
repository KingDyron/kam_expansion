{
NavMesh - flood fill algorithm
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_NavMeshFloodFill;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_CommonTypes, KM_Points;

type
  TPolygonQueue = record
    Visited: Byte;
    Next: Word;
    Distance: Cardinal;
    DistPoint: TKMPoint;
  end;
  TPolygonsQueueArr = array of TPolygonQueue;

  // Flood-Fill with queue
  TNavMeshFloodFill = class
  private
  protected
    fSorted: Boolean; // Only for sorted case
    fVisitedIdx: Byte;
    fStartQueue, fEndQueue, fQueueCnt: Word;
    fQueueArray: TPolygonsQueueArr; // max 2000-4000 elements (polygons in navmesh) -> use array with fixed length instead of creating elements which have 10<->20 bytes

    procedure MakeNewQueue(); virtual;
    procedure ClearVisitIdx();
    function IsQueueEmpty(): Boolean; inline;
    function IsVisited(const aIdx: Word): Boolean; virtual;
    function CanBeExpanded(const aIdx: Word): Boolean; virtual;
    procedure MarkAsVisited(const aIdx: Word; const aDistance: Cardinal; const aPoint: TKMPoint); virtual;
    procedure InsertInQueue(const aIdx: Word); virtual;
    procedure InsertAndSort(const aIdx: Word); virtual; // Only for sorted case
    function RemoveFromQueue(var aIdx: Word): Boolean;

    procedure InitQueue(const aMaxIdx: Integer; var aInitIdxArray: TKMWordArray); virtual;
    procedure Flood(); virtual;
  public
    constructor Create(aSorted: Boolean = False); virtual;
    destructor Destroy(); override;

    function FillPolygons(const aMaxIdx: Word; var aInitIdxArray: TKMWordArray): Boolean; virtual;
  end;


implementation
uses
  KM_AIFields;


{ TNavMeshPathFinding }
constructor TNavMeshFloodFill.Create(aSorted: Boolean = False);
begin
  fQueueCnt := 0;
  fStartQueue := 0;
  fEndQueue := 0;
  fVisitedIdx := 0;
  fSorted := aSorted;
  inherited Create();
end;


destructor TNavMeshFloodFill.Destroy();
begin
  inherited;
end;


procedure TNavMeshFloodFill.MakeNewQueue();
begin
  fQueueCnt := 0;
  fVisitedIdx := fVisitedIdx + 1;
  if (Length(fQueueArray) < gAIFields.NavMesh.PolygonsCnt) then
  begin
    SetLength(fQueueArray, gAIFields.NavMesh.PolygonsCnt);
    ClearVisitIdx();
  end;
  if (fVisitedIdx = High(Byte)) then
    ClearVisitIdx();
end;


// Queue is realised inside of array (constant length) instead of interconnected elements
procedure TNavMeshFloodFill.ClearVisitIdx();
begin
  fVisitedIdx := 1;
  FillChar(fQueueArray[0], SizeOf(fQueueArray[0]) * Length(fQueueArray), #0);
end;


function TNavMeshFloodFill.IsQueueEmpty(): Boolean;
begin
  Result := fQueueCnt = 0;
end;


function TNavMeshFloodFill.IsVisited(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Visited = fVisitedIdx);
end;


function TNavMeshFloodFill.CanBeExpanded(const aIdx: Word): Boolean;
begin
  Result := True;
end;


procedure TNavMeshFloodFill.MarkAsVisited(const aIdx: Word; const aDistance: Cardinal; const aPoint: TKMPoint);
begin
  with fQueueArray[aIdx] do
  begin
    Visited := fVisitedIdx;
    DistPoint := aPoint;
    Distance := aDistance;
  end;
end;


procedure TNavMeshFloodFill.InsertInQueue(const aIdx: Word);
begin
  if IsQueueEmpty then
    fStartQueue := aIdx;
  fQueueArray[fEndQueue].Next := aIdx;
  fEndQueue := aIdx;
  fQueueCnt := fQueueCnt + 1;
end;


procedure TNavMeshFloodFill.InsertAndSort(const aIdx: Word);
var
  K, ActIdx, PrevIdx: Integer;
begin
  // Empty queue
  if IsQueueEmpty then
  begin
    fStartQueue := aIdx;
    fEndQueue := fStartQueue;
  end
  // Insert in sorted array
  else
  begin
    PrevIdx := fStartQueue;
    ActIdx := fStartQueue;
    // Find the right position
    K := 0;
    while (K < fQueueCnt) do // While must be here
    begin
      if (fQueueArray[aIdx].Distance < fQueueArray[ActIdx].Distance) then
        Break;
      PrevIdx := ActIdx;
      ActIdx := fQueueArray[ActIdx].Next;
      K := K + 1;
    end;
    // Change indexes of surrounding elements (= insert element into queue, no shift of other elements is required)
    if (K = 0) then
    begin
      fQueueArray[aIdx].Next := fStartQueue;
      fStartQueue := aIdx;
    end
    else if (K = fQueueCnt) then
    begin
      fQueueArray[fEndQueue].Next := aIdx;
      fEndQueue := aIdx;
    end
    else
    begin
      fQueueArray[PrevIdx].Next := aIdx;
      fQueueArray[aIdx].Next := ActIdx;
    end;
  end;
  fQueueCnt := fQueueCnt + 1;
end;


function TNavMeshFloodFill.RemoveFromQueue(var aIdx: Word): Boolean;
begin
  Result := not IsQueueEmpty;
  if Result then
  begin
    aIdx := fStartQueue;
    fStartQueue := fQueueArray[fStartQueue].Next;
    fQueueCnt := fQueueCnt - 1;
  end;
end;


// Init Queue
procedure TNavMeshFloodFill.InitQueue(const aMaxIdx: Integer; var aInitIdxArray: TKMWordArray);
const
  INIT_DISTANCE = 0;
var
  K, Idx: Integer;
begin
  MakeNewQueue();
  if (aMaxIdx >= 0) then
    for K := 0 to aMaxIdx do
    begin
      Idx := aInitIdxArray[K];
      if not IsVisited(Idx) then
      begin
        MarkAsVisited(Idx, INIT_DISTANCE, gAIFields.NavMesh.Polygons[ Idx ].CenterPoint);
        InsertInQueue(Idx);
      end;
    end;
end;


// Flood fill in NavMesh grid
procedure TNavMeshFloodFill.Flood();
var
  Idx: Word;
  K: Integer;
begin
  while RemoveFromQueue(Idx) do
    if CanBeExpanded(Idx) then
      with gAIFields.NavMesh.Polygons[Idx] do
        for K := 0 to NearbyCount-1 do
        begin
          if not IsVisited(Nearby[K]) then
          begin
            MarkAsVisited( Nearby[K],
                           fQueueArray[Idx].Distance + KMDistanceWalk(fQueueArray[Idx].DistPoint, NearbyPoints[K]),
                           NearbyPoints[K]);
            if fSorted then
              InsertAndSort(Nearby[K])
            else
              InsertInQueue(Nearby[K]);
          end;
        end;
end;


function TNavMeshFloodFill.FillPolygons(const aMaxIdx: Word; var aInitIdxArray: TKMWordArray): Boolean;
begin
  Result := (Length(aInitIdxArray) > 0);
  if Result then
  begin
    InitQueue(  Min( High(aInitIdxArray), aMaxIdx ), aInitIdxArray);
    Flood();
  end;
end;


end.
