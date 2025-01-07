unit KM_FloodFill;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Points;

type
  PElement = ^TElement;
  TElement = record
    X,Y: SmallInt;
    Next: PElement;
  end;


  // Fast flood algorithm (Queue + for cycle instead of recursion)
  TKMQuickFlood = class
  private
    fStartQueue, fEndQueue: PElement;
  protected
    fMinLimit, fMaxLimit: TKMPoint;
    fScanEightTiles: Boolean; // True = scan 8 tiles around, False = scan 4 tiles (no performance impact!)

    procedure MakeNewQueue; virtual;
    procedure InsertInQueue(const aX,aY: SmallInt); virtual;
    function RemoveFromQueue(var aX,aY: SmallInt): Boolean; virtual;
    function IsQueueEmpty: Boolean; inline;

    function CanBeVisited(const aX,aY: SmallInt): Boolean; virtual; abstract;
    function IsVisited(const aX,aY: SmallInt): Boolean; virtual; abstract;
    procedure MarkAsVisited(const aX,aY: SmallInt); virtual; abstract;
    procedure SearchAround(aX,aY: SmallInt); virtual;
  public
    constructor Create(aScanEightTiles: Boolean); virtual;
    destructor Destroy; override;
    procedure QuickFlood(aX,aY: SmallInt); virtual;
  end;


implementation


{ TKMQuickFlood }
constructor TKMQuickFlood.Create(aScanEightTiles: Boolean);
begin
  inherited Create;

  fScanEightTiles := aScanEightTiles;

  // Create first TElement and keep it all the time (queue is empty if fStartQueue = fEndQueue)
  New(fStartQueue);
  fStartQueue^.Next := nil;
  fEndQueue := fStartQueue;
end;


destructor TKMQuickFlood.Destroy;
var
  X,Y: SmallInt;
begin
  while not IsQueueEmpty do // Dispose everything except 1 last element
    RemoveFromQueue(X,Y);
  Dispose(fStartQueue); // Dispose the last element

  inherited;
end;


procedure TKMQuickFlood.MakeNewQueue;
var
  X,Y: SmallInt;
begin
  while not IsQueueEmpty do // Make sure that queue is empty
    RemoveFromQueue(X,Y);
end;


procedure TKMQuickFlood.InsertInQueue(const aX, aY: SmallInt);
begin
  fEndQueue^.X := aX;
  fEndQueue^.Y := aY;
  new(fEndQueue^.Next);
  fEndQueue := fEndQueue^.Next;
  fEndQueue^.Next := nil;
end;


function TKMQuickFlood.RemoveFromQueue(var aX, aY: SmallInt): Boolean;
var
  pom: PElement;
begin
  Result := True;
  if IsQueueEmpty then
  begin
    Result := False;
  end
  else
  begin
    aX := fStartQueue^.X;
    aY := fStartQueue^.Y;
    pom := fStartQueue;
    fStartQueue := fStartQueue^.Next;
    Dispose(pom);
  end;
end;


function TKMQuickFlood.IsQueueEmpty: Boolean;
begin
  Result := fStartQueue = fEndQueue;
end;


procedure TKMQuickFlood.SearchAround(aX,aY: SmallInt);

  procedure ScanInColumn(const aX,aY: SmallInt; var PreviousStep: Boolean);
  begin
    if PreviousStep then
      PreviousStep := CanBeVisited(aX,aY)
    else if not IsVisited(aX,aY) AND CanBeVisited(aX,aY) then
    begin
      PreviousStep := True;
      InsertInQueue(aX, aY);
    end;
  end;

  procedure ScanInRow(const aX,aY, Dir: SmallInt; const CheckCurrentPosition: Boolean = False);
  var
    X,Y_T,Y_D: SmallInt;
    Top,Down, TopCheck,DownCheck: Boolean;
  begin
    Top := aY > fMinLimit.Y;
    Down := aY < fMaxLimit.Y;
    Y_T := aY - 1;
    Y_D := aY + 1;
    TopCheck := False;
    DownCheck := False;
    if Top then
      TopCheck := CanBeVisited(aX,Y_T) AND not IsVisited(aX, Y_T);
    if Down then
      DownCheck := CanBeVisited(aX,Y_D) AND not IsVisited(aX, Y_D);
    if CheckCurrentPosition then
    begin
      if TopCheck then
        InsertInQueue(aX, Y_T);
      if DownCheck then
        InsertInQueue(aX, Y_D);
    end;
    X := aX + Dir;
    while (X >= fMinLimit.X) AND (X <= fMaxLimit.X) AND CanBeVisited(X,aY) do
    begin
      MarkAsVisited(X, aY);
      if Top then
        ScanInColumn(X,Y_T, TopCheck);
      if Down then
        ScanInColumn(X,Y_D, DownCheck);
      X := X + Dir;
    end;
    if fScanEightTiles AND (X >= fMinLimit.X) AND (X <= fMaxLimit.X) then
    begin
      if Top then
        ScanInColumn(X,Y_T, TopCheck);
      if Down then
        ScanInColumn(X,Y_D, DownCheck);
    end;
  end;

begin
  MarkAsVisited(aX, aY);
  ScanInRow(aX,aY, 1, True);
  ScanInRow(aX,aY, -1, False);
end;


procedure TKMQuickFlood.QuickFlood(aX,aY: SmallInt);
begin
  MakeNewQueue();
  if CanBeVisited(aX, aY) then
    InsertInQueue(aX, aY);
  while not IsQueueEmpty do
  begin
    RemoveFromQueue(aX, aY);
    if not IsVisited(aX, aY) then
      SearchAround(aX, aY);
  end;
end;


end.
