unit KM_Points;
{$I KaM_Remake.inc}
interface


type
  //* Facing direction
  TKMDirection = (dirNA, dirN, dirNE, dirE, dirSE, dirS, dirSW, dirW, dirNW);
  TKMDirection4 = (drNA, drN, drE, drS, drW);
  TKMDirection4Set = set of TKMDirection4;

const
  DIR_MIN = dirN;
  DIR_MAX = dirNW;
  DIRS_VALID = [DIR_MIN..DIR_MAX];
  DIR_TO_NEXT : array[TKMDirection] of TKMDirection = (dirNA, dirNE, dirE, dirSE, dirS, dirSW, dirW, dirNW, dirN);
  DIR_TO_PREV : array[TKMDirection] of TKMDirection = (dirNA, dirNW, dirN, dirNE, dirE, dirSE, dirS, dirSW, dirW);
  DIR_TO_NEXT2 : array[TKMDirection] of TKMDirection = (dirNA, dirE, dirSE, dirS, dirSW, dirW, dirNW, dirN, dirNE);
  DIR_TO_PREV2 : array[TKMDirection] of TKMDirection = (dirNA, dirW, dirNW, dirN, dirNE, dirE, dirSE, dirS, dirSW);

type
  //Records must be packed so they are stored identically in MP saves (padding bytes are unknown values)
  TKMPointF = record
    X,Y: Single;
    class operator Equal(const A, B: TKMPointF): Boolean;
    class operator NotEqual(const A, B: TKMPointF): Boolean;
    class operator Add(const A, B: TKMPointF): TKMPointF;
    class function New(aX, aY: Single): TKMPointF; static;
    function ToString: String;
    function RX : Integer;
    function RY : Integer;
  end;

  //* Point with integer coordinates X and Y
  TKMPoint = record
    X,Y: Integer;

    function ToString: String;

    class operator Equal(const A, B: TKMPoint): Boolean;
    class operator NotEqual(const A, B: TKMPoint): Boolean;
    class operator Add(const A, B: TKMPoint): TKMPoint;
    class operator Subtract(const A, B: TKMPoint): TKMPoint;
    class function New(aX, aY: Integer): TKMPoint; static;
    function Compare(const aPoint: TKMPoint): Integer;
    function ToFloat: TKMPointF;
  end;

  TKMPointDir = packed record
    Loc: TKMPoint;
    Dir: TKMDirection;
    function ToString: String;
    property X : Integer read Loc.X write Loc.X;
    property Y : Integer read Loc.Y write Loc.Y;
    function DirFaceLoc : TKMPoint;
    class operator Equal(const A, B: TKMPointDir): Boolean; overload;
    class operator Equal(const A : TKMPointDir; B: TKMPoint): Boolean; overload;
    class operator Equal(const A : TKMPoint; B: TKMPointDir): Boolean; overload;
  end;

  TKMPointExact = packed record Loc: TKMPoint; Exact: Boolean; end;

  TKMPointW = record
    X,Y: Word;
    function ToString: String;
  end; // For backwards compatibility with cmp files

  TKMPointArray = array of TKMPoint;
  TKMPoint2Array = array of array of TKMPoint;
  TKMPointDirArray = array of TKMPointDir;
  TKMTrisArray = array of array [0..2] of Integer;

  TKMPointArrayHelper = record helper for TKMPointArray
    procedure Clear;
    function Count : Integer;
    function Add(aLoc : TKMPoint) : Integer; Overload;
    function Add(aX, aY : Integer) : Integer; Overload;
    function Delete(aLoc : TKMPoint) : Boolean; Overload;
    function Delete(aIndex : Integer) : Boolean; Overload;
    function Contains(X, Y : Integer) : Boolean; Overload;
    function Contains(aLoc : TKMPoint) : Boolean; Overload;
    function IndexOf(aLoc : TKMPoint) : Integer;
  end;

  TKMPointDirArrayHelper = record helper for TKMPointDirArray
    function Add(aLoc : TKMPointDir) : Integer; Overload;
    function Add(aX, aY : Integer; aDir : TKMDirection) : Integer; Overload;
    function Delete(aLoc : TKMPointDir) : Boolean;
    function Contains(X, Y : Integer; aDir : TKMDirection) : Boolean; Overload;
    function Contains(aLoc : TKMPointDir) : Boolean; Overload;
    function IndexOf(aLoc : TKMPointDir) : Integer;
  end;


  TKMPointFArray = array of TKMPointF;

  TKMTriMesh = record
    Vertices: TKMPointArray;
    Polygons: TKMTrisArray;
  end;

  //We have our own TKMRect that consistently matches TKMPoint range
  //Rects are often used without range checking and include negative off-map coords
  TKMRect = packed record
    Left, Top, Right, Bottom: Integer;
    function ToString: String;
    function Width: Integer;
    function Height: Integer;
    procedure SetLeft(aLeft: Integer);
    procedure SetRight(aRight: Integer);
    procedure SetTop(aTop: Integer);
    procedure SetBottom(aBottom: Integer);
    procedure FitInMap(aX, aY : Integer);
    class operator Equal(const A, B: TKMRect): Boolean;
    class operator NotEqual(const A, B: TKMRect): Boolean;
  end;

  TKMRectF = packed record Left, Top, Right, Bottom: Single end;

  TKMRangeInt = record
    Min, Max: Integer;
    function ToString: String;
  end;

  TKMRangeSingle = record
    Min, Max: Single;
    function ToString: String;
  end;

  TKMPointFunction = function(aPoint: TKMPoint): Boolean of object;

  function KMPoint(X,Y: Integer): TKMPoint; overload;
  function KMPoint(A : TKMPointF): TKMPoint; overload;
  function KMPointF(X,Y: Single): TKMPointF; overload;
  function KMPointF(const P: TKMPoint):  TKMPointF; overload;
  function KMPointF(const P: TKMPointDir):  TKMPointF; overload;
  function KMPointDir(X,Y: Integer; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointDir(const P: TKMPoint; Dir: TKMDirection): TKMPointDir; overload;
  function KMPointX1Y1(const P:TKMPoint): TKMPoint;
  function KMPointBelow(const P: TKMPoint): TKMPoint;
  function KMPointAbove(const P: TKMPoint): TKMPoint;
  function KMPointLeft(const P: TKMPoint): TKMPoint;
  function KMPointRight(const P: TKMPoint): TKMPoint;
  function KMPointInCircle(const P, P2: TKMPoint ; aRadius : Integer): Boolean;
  function KMNormVector(const P: TKMPoint; R: Integer): TKMPoint;
  function KMPointInvert(const P : TKMPoint) : TKMPoint;

  function KMPointTrunc(const P: TKMPointF): TKMPoint;
  function KMPointRound(const P: TKMPointF): TKMPoint;
  function KMPointFRoundTo(const P: TKMPointF; aBase: Single): TKMPointF;
  function KMSamePoint(const P1,P2: TKMPoint): Boolean; overload;
  function KMSamePointF(const P1,P2: TKMPointF): Boolean; overload;
  function KMSamePointF(const P1,P2: TKMPointF; Epsilon: Single): Boolean; overload;
  function KMSamePointDir(const P1,P2: TKMPointDir): Boolean;


  function KMRect(aLeft, aTop, aRight, aBottom: SmallInt): TKMRect; overload;
  function KMRect(const aPoint: TKMPoint): TKMRect; overload;
  function KMRect(const aPoint: TKMPointF): TKMRect; overload;
  function KMRectF(const aRect: TKMRect): TKMRectF; overload;
  function KMRectF(const aPoint: TKMPointF): TKMRectF; overload;
  function KMRectF(aLeft, aTop, aRight, aBottom: SmallInt): TKMRectF; overload;
  function KMRectF(aLeft, aTop, aRight, aBottom: Single): TKMRectF; overload;
  function KMRectRound(const aRect: TKMRectF): TKMRect;
  function KMSameRect(const aRect1, aRect2: TKMRect): Boolean;
  function KMRectWidth(const aRect: TKMRect): Integer;
  function KMRectHeight(const aRect: TKMRect): Integer;
  function KMRectGrow(const aRect: TKMRect; aInset: Integer): TKMRect; overload;
  function KMRectGrowNoLimits(const aRect: TKMRect; aInset: Integer): TKMRect;
  function KMRectGrow(const aRect, aInsetRect: TKMRect): TKMRect; overload;
  function KMRectGrow(const aPoint : TKMPoint; aInset: Integer): TKMRect; overload;
  function KMRectGrow(const aRect: TKMRect; const aDir: TKMDirection; aInset: Integer = 1): TKMRect; overload;
  function KMRectGrowTopLeft(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
  function KMRectGrowTopRight(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
  function KMRectGrowBottomLeft(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
  function KMRectShinkTopLeft(const aRect: TKMRect): TKMRect;
  function KMRectGrowBottomRight(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
  function KMClipRect(const aRect: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect; overload;
  function KMClipRect(const aRect1, aRect2: TKMRect): TKMRect; overload;
  function KMRectIntersect(const aRect1: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect; overload;
  function KMRectIntersect(const aRect1, aRect2: TKMRect): TKMRect; overload;
  function KMRectCorners(const aRect: TKMRect): TKMPointArray;
  function KMInRect(const aPoint: TKMPoint; const aRect: TKMRect): Boolean; overload;
  function KMInRect(const aPoint: TKMPointF; const aRect: TKMRect): Boolean; overload;
  function KMInRect(const aPoint: TKMPointF; const aRect: TKMRectF): Boolean; overload;
  function KMRectFitInRect(const aInnerRect, aOuterRect: TKMRect): Boolean;
  function KMRectArea(const aRect: TKMRect): Integer;
  function KMRectMove(const aRect: TKMRect; X,Y: Integer): TKMRect;
  procedure KMRectIncludePoint(var aRect: TKMRect; X,Y: Integer); overload;
  procedure KMRectIncludePoint(var aRect: TKMRect; const aPoint: TKMPoint); overload;
  procedure KMRectIncludeRect(var aRect: TKMRect; aRect2: TKMRect);

  function KMGetDirection(aDirF: Single): TKMDirection; overload; inline;
  function KMGetDirection(X,Y: Single; aDirNAThreshold: Integer = 0): TKMDirection; overload;
  function KMGetDirection(const P: TKMPointF): TKMDirection; overload;
  function KMGetDirection(const FromPos, ToPos: TKMPoint): TKMDirection; overload;
  function KMGetDirection(const FromPos, ToPos: TKMPointF): TKMDirection; overload;
  function KMGetDirection(const FromPos, ToPos: TKMPointDir): TKMDirection; overload;
  function GetDirModifier(const aDir1, aDir2: TKMDirection): Byte;
  function GetDirDifference(const aDir1, aDir2: TKMDirection): byte;
  function KMGetVertexDir(X,Y: Integer): TKMDirection;
  function KMGetVertexTile(const P: TKMPoint; const Dir: TKMDirection): TKMPoint;
  function KMGetVertex(const aDir: TKMDirection): TKMPointF;
  function KMGetPointInDir(const aPoint: TKMPoint; const aDir: TKMDirection; aDist: Byte = 1): TKMPoint;

  function KMAddDirection(const aDir: TKMDirection; aAdd: Integer): TKMDirection;
  function KMNextDirection(const aDir: TKMDirection): TKMDirection;
  function KMPrevDirection(const aDir: TKMDirection): TKMDirection;

  function KMPointsAround(const P: TKMPoint; aIncludeSelf: Boolean = False): TKMPointArray;

  function KMGetDiagVertex(const P1,P2:TKMPoint): TKMPoint;
  function KMStepIsDiag(const P1,P2:TKMPoint): Boolean;

  function KMPointAverage(const A, B: TKMPoint): TKMPoint;
  function KMPointSubtract(const A, B: TKMPoint): TKMPoint;
  function KMPointAdd(const A, B: TKMPoint): TKMPoint; overload;
  function KMPointAdd(const A, B, C: TKMPoint): TKMPoint; overload;
  function KMPointFAdd(const A, B: TKMPointF): TKMPointF;
  function KMDotProduct(const A, B: TKMPoint): Single;
  function KMDistanceAbs(const A, B: TKMPoint): Integer;
  function KMDistanceWalk(const A, B: TKMPoint): Integer;
  function KMDistanceSqr(const A, B: TKMPoint): Single; overload;
  function KMDistanceSqr(const A, B: TKMPointF): Single; overload;

  function KMPerpendecular(const A,B: TKMPoint): TKMPointF;
  //Cross product of 2D vectors, pointed either Up or Down
  function KMNormal2Poly(const v1,v2,v3: TKMPoint): Single; overload;
  function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
  function KMSegmentsIntersect(const A, B, C, D: TKMPoint): Boolean;
  function KMSegmentsIntersectOrTouch(const A, B, C, D: TKMPoint): Boolean;

  function KMLength(A,B: Single): Single; overload;
  function KMLength(const A, B: TKMPoint): Single; overload;
  function KMLength(const A, B: TKMPointF): Single; overload;
  function KMLengthDiag(X, Y: Integer): Single; overload;
  function KMLengthDiag(const A, B: TKMPoint): Single; overload;
  function KMLengthDiag(const A, B: TKMPointF): Single; overload;
  function KMLengthDiag(X,Y: Integer; const B: TKMPoint): Single; overload;
  function KMLengthSqr(const A, B: TKMPoint): Integer; overload;
  function KMLengthSqr(const X1, Y1, X2, Y2: Integer): Integer; overload;
  function KMLengthSqr(const A, B: TKMPointF): Single; overload;
  function KMLengthSqr(const A: TKMPoint; const B: TKMPointF): Single; overload;

  function KMLerp(const A,B: TKMPoint; MixValue: Single): TKMPointF; overload;
  function KMLerp(const A,B: TKMPointF; MixValue: Single): TKMPointF; overload;
  function KMLerp(const A,B: TKMPointDir; MixValue: Single): TKMPointF; overload;

  procedure KMSwapPoints(var A,B: TKMPoint);
  procedure KMSwapPointDir(var A,B: TKMPointDir);

  function TypeToString(const P: TKMPoint): string; overload;
  function TypeToString(const P: TKMPointW): string; overload;
  function TypeToString(const P: TKMPointDir): string; overload;
  function TypeToString(const P: TKMPointF): string; overload;
  function TypeToString(const T: TKMDirection): string; overload;

  function StringToType(const Str: String): TKMPoint; overload;

  function KMRange(aMin, aMax: Integer): TKMRangeInt; overload;
  function KMRange(aMin, aMax: Single): TKMRangeSingle; overload;

  function KMInRange(aValue: Integer; const aRangeInt: TKMRangeInt): Boolean; overload;
  function KMInRange(aValue: Single; const aRangeSingle: TKMRangeSingle): Boolean; overload;

  function KMEnsureRange(aValue: Integer; const aRange: TKMRangeInt): Integer; overload;
  function KMEnsureRange(aValue: Single; const aRange: TKMRangeSingle): Single; overload;

  function KMEnlargeRange(const aRange: TKMRangeInt; aValue: Integer): TKMRangeInt; overload;
  function KMEnlargeRange(const aRange: TKMRangeSingle; aValue: Single): TKMRangeSingle; overload;

  function KMPointAffectDir(const aLoc : TKMPointF; aDir : TKMDirection; aAddDistance : Single) : TKMPointF;

  function KMPointRandomInRadius(const aLoc : TKMPoint; aRadius : Integer) : TKMPoint;
  function KMPointFRandomInRadius(const aLoc : TKMPointF; aRadius : Single) : TKMPointF;

const
  KMPOINT_ZERO: TKMPoint = (X: 0; Y: 0);
  KMPOINTF_ZERO: TKMPointF = (X: 0.0; Y: 0.0);
  KMPOINT_INVALID_TILE: TKMPoint = (X: -1; Y: -1);
  KMPOINTF_INVALID_TILE: TKMPointF = (X: -1; Y: -1);

  KMRECT_ZERO: TKMRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  KMRECT_INVALID_TILES: TKMRect = (Left: -1; Top: -1; Right: -1; Bottom: -1);

  DIAG_DIRECTION: array[TKMDirection] of Boolean = (False, False, True, False, True, False, True, False, True);


implementation
uses
  SysUtils, TypInfo, Math,
  KM_Defaults, KM_CommonUtils;




class function TKMPoint.New(aX, aY: Integer): TKMPoint;
begin
  Result.X := aX;
  Result.Y := aY;
end;


function TKMPoint.Compare(const aPoint: TKMPoint): Integer;
begin
  Result := (Y - aPoint.Y) * MAX_MAP_SIZE + (X - aPoint.X);
end;


class operator TKMPoint.Equal(const A, B: TKMPoint): Boolean;
begin
  Result := KMSamePoint(A,B);
end;


class operator TKMPoint.NotEqual(const A, B: TKMPoint): Boolean;
begin
  Result := not KMSamePoint(A,B);
end;


class operator TKMPoint.Add(const A, B: TKMPoint): TKMPoint;
begin
  Result := KMPoint(A.X + B.X,A.Y + B.Y);
end;

class operator TKMPoint.Subtract(const A, B: TKMPoint): TKMPoint;
begin
  Result := KMPoint(A.X - B.X,A.Y - B.Y);
end;


function TKMPoint.ToFloat: TKMPointF;
begin
  Result := KMPointF(X, Y);
end;


function TKMPoint.ToString: String;
begin
  Result := TypeToString(Self);
end;


class function TKMPointF.New(aX, aY: Single): TKMPointF;
begin
  Result.X := aX;
  Result.Y := aY;
end;


class operator TKMPointF.Equal(const A, B: TKMPointF): Boolean;
begin
  Result := KMSamePointF(A,B);
end;


class operator TKMPointF.NotEqual(const A, B: TKMPointF): Boolean;
begin
  Result := not KMSamePointF(A,B);
end;


class operator TKMPointF.Add(const A, B: TKMPointF): TKMPointF;
begin
  Result := KMPointF(A.X + B.X, A.Y + B.Y);
end;
{TKMPointArrayHelper}
procedure TKMPointArrayHelper.Clear;
begin
  SetLength(self, 0);
end;
function TKMPointArrayHelper.Count : Integer;
begin
  Result := length(self);
end;

function TKMPointArrayHelper.Add(aLoc: TKMPoint): Integer;
begin
  Result := length(self); SetLength(self, length(self) + 1); self[high(self)] := aLoc;
end;
function TKMPointArrayHelper.Add(aX: Integer; aY: Integer): Integer;
begin
  Result := Add(KMPoint(aX, aY));
end;
function TKMPointArrayHelper.Delete(aLoc: TKMPoint): Boolean;
var I, aIndex : Integer;
begin
  aIndex := -1;
  for I := 0 to High(self) do
    if self[I] = aLoc then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
    Exit(false);
  Result := true;
  for I := aIndex to High(self) - 1 do
    self[I] := self[I + 1];
  SetLength(self, high(self));
end;

function TKMPointArrayHelper.Delete(aIndex: Integer): Boolean;
var I : Integer;
begin
  if aIndex = -1 then
    Exit(false);
  Result := true;
  for I := aIndex to High(self) - 1 do
    self[I] := self[I + 1];
  SetLength(self, high(self));
end;


function TKMPointArrayHelper.Contains(aLoc: TKMPoint): Boolean;
var I : integer;
begin
  Result := false;
  for I := 0 to High(self) do
    if self[I] = aLoc then
      Exit(true);
end;
function TKMPointArrayHelper.Contains(X: Integer; Y: Integer): Boolean;
begin
  Result := Contains(KMPoint(X, Y));
end;
function TKMPointArrayHelper.IndexOf(aLoc: TKMPoint): Integer;
var I : Integer;
begin
  Result := -1;
  for I := 0 to High(self) do
    if self[I] = aLoc then
      Exit(I);
end;
{TKMPointDirArrayHelper}
function TKMPointDirArrayHelper.Add(aLoc: TKMPointDir): Integer;
begin
  Result := length(self); SetLength(self, length(self) + 1); self[high(self)] := aLoc;
end;
function TKMPointDirArrayHelper.Add(aX: Integer; aY: Integer; aDir: TKMDirection): Integer;
begin
  Result := Add(KMPointDir(aX, aY, aDir));
end;
function TKMPointDirArrayHelper.Delete(aLoc: TKMPointDir): Boolean;
var I, aIndex : Integer;
begin
  aIndex := -1;
  for I := 0 to High(self) do
    if (self[I].Loc = aLoc.Loc) and (self[I].Dir = aLoc.Dir) then
    begin
      aIndex := I;
      Break;
    end;
  if aIndex = -1 then
    Exit(false);
  Result := true;
  for I := aIndex to High(self) - 1 do
    self[I] := self[I + 1];
  SetLength(self, high(self));
end;

function TKMPointDirArrayHelper.Contains(aLoc: TKMPointDir): Boolean;
var I : integer;
begin
  Result := false;
  for I := 0 to High(self) do
    if (self[I].Loc = aLoc.Loc) and (self[I].Dir = aLoc.Dir) then
      Exit(true);
end;
function TKMPointDirArrayHelper.Contains(X: Integer; Y: Integer; aDir : TKMDirection): Boolean;
begin
  Result := Contains(KMPointDir(X, Y, aDir));
end;
function TKMPointDirArrayHelper.IndexOf(aLoc: TKMPointDir): Integer;
var I : Integer;
begin
  Result := -1;
  for I := 0 to High(self) do
    if (self[I].Loc = aLoc.Loc) and (self[I].Dir = aLoc.Dir) then
      Exit(I);
end;


function TKMPointF.ToString: String;
begin
  Result := TypeToString(Self);
end;

function TKMPointF.RX : Integer;
begin
  Result := Round(X);
end;
function TKMPointF.RY : Integer;
begin
  Result := Round(Y);
end;


function TKMPointW.ToString: String;
begin
  Result := TypeToString(Self);
end;


function TKMPointDir.ToString: String;
begin
  Result := TypeToString(Self);
end;

function TKMPointDir.DirFaceLoc : TKMPoint;
begin
  case Dir of
    dirNA : Result := Loc;
    dirN : Result := KMPoint(X, Y - 1);
    dirNE : Result := KMPoint(X + 1, Y - 1);
    dirE : Result := KMPoint(X + 1, Y);
    dirSE : Result := KMPoint(X + 1, Y + 1);
    dirS : Result := KMPoint(X, Y + 1);
    dirSW : Result := KMPoint(X - 1, Y + 1);
    dirW : Result := KMPoint(X - 1, Y);
    dirNW : Result := KMPoint(X, Y - 1);
  end;
end;

class operator TKMPointDir.Equal(const A, B: TKMPointDir): Boolean;
begin
  Result := (A.Loc = B.Loc) and (A.Dir = B.Dir);
end;

class operator TKMPointDir.Equal(const A : TKMPointDir; B: TKMPoint): Boolean;
begin
  Result := (A.Loc = B);
end;

class operator TKMPointDir.Equal(const A : TKMPoint; B: TKMPointDir): Boolean;
begin
  Result := B = A;
end;


function TKMRect.ToString: String;
begin
  Result := Format('(%d, %d, %d, %d)', [Left, Top, Right, Bottom]);;
end;


function TKMRect.Width: Integer;
begin
  Result := Right - Left + 1;
end;

function TKMRect.Height: Integer;
begin
  Result := Bottom - Top + 1;
end;

procedure TKMRect.SetLeft(aLeft: Integer);
begin
  Left := aLeft;
end;

procedure TKMRect.SetRight(aRight: Integer);
begin
  Right := aRight;
end;

procedure TKMRect.SetTop(aTop: Integer);
begin
  Top := aTop;
end;

procedure TKMRect.SetBottom(aBottom: Integer);
begin
  Bottom := aBottom;
end;

procedure TKMRect.FitInMap(aX, aY : Integer);
begin
  Left := EnsureRange(Left, 1, aX);
  Top := EnsureRange(Top, 1, aY);
  Right := EnsureRange(Right, 1, aX);
  Bottom := EnsureRange(Bottom, 1, aY);
end;

class operator TKMRect.Equal(const A, B: TKMRect): Boolean;
begin
  Result := KMSameRect(A,B);
end;

class operator TKMRect.NotEqual(const A, B: TKMRect): Boolean;
begin
  Result := not KMSameRect(A,B);
end;

function TKMRangeInt.ToString: String;
begin
  Result := Format('%d - %d', [Min, Max]);
end;

function TKMRangeSingle.ToString: String;
begin
  Result := Format('%.5f - %.5f', [Min, Max]);
end;


function KMPoint(X,Y: Integer): TKMPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function KMPoint(A : TKMPointF): TKMPoint;
begin
  Result.X := Round(A.X);
  Result.Y := Round(A.Y);
end;

function KMPointF(const P: TKMPoint): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function KMPointF(const P: TKMPointDir): TKMPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function KMPointF(X, Y: Single): TKMPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;


function KMPointDir(X,Y: Integer; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc.X := X;
  Result.Loc.Y := Y;
  Result.Dir := Dir;
end;


function KMPointDir(const P: TKMPoint; Dir: TKMDirection): TKMPointDir;
begin
  Result.Loc := P;
  Result.Dir := Dir;
end;


function KMPointX1Y1(const P: TKMPoint): TKMPoint;
begin
  Result.X := P.X + 1;
  Result.Y := P.Y + 1;
end;


function KMPointBelow(const P: TKMPoint): TKMPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y + 1;
end;


function KMPointAbove(const P: TKMPoint): TKMPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y - 1;
end;

function KMPointLeft(const P: TKMPoint): TKMPoint;
begin
  Result.X := P.X - 1;
  Result.Y := P.Y;
end;

function KMPointRight(const P: TKMPoint): TKMPoint;
begin
  Result.X := P.X + 1;
  Result.Y := P.Y;
end;

function KMPointInCircle(const P, P2: TKMPoint ; aRadius : Integer): Boolean;
begin

  Result := (Sqr(P2.X - P.X) + Sqr(P2.Y - P.Y)) <= Sqr(aRadius);

end;

function KMNormVector(const P: TKMPoint; R: Integer): TKMPoint;
begin
  Result.X := Round(R*P.X / sqrt(sqr(P.X) + sqr(P.Y)));
  Result.Y := Round(R*P.Y / sqrt(sqr(P.X) + sqr(P.Y)));
end;

function KMPointInvert(const P: TKMPoint): TKMPoint;
begin
  Result.X := -P.X;
  Result.Y := -P.Y;
end;

function KMPointTrunc(const P: TKMPointF): TKMPoint;
begin
  Result.X := Trunc(P.X);
  Result.Y := Trunc(P.Y);
end;

function KMPointRound(const P: TKMPointF): TKMPoint;
begin
  Result.X := Round(P.X);
  Result.Y := Round(P.Y);
end;


function KMPointFRoundTo(const P: TKMPointF; aBase: Single): TKMPointF;
begin
  Result.X := Round(P.X / aBase) * aBase;
  Result.Y := Round(P.Y / aBase) * aBase;
end;


function KMSamePoint(const P1,P2: TKMPoint): Boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;


function KMSamePointF(const P1,P2: TKMPointF): Boolean;
begin
  Result := ( P1.X = P2.X ) and ( P1.Y = P2.Y );
end;


function KMSamePointF(const P1,P2: TKMPointF; Epsilon: Single): Boolean;
begin
  Result := (abs(P1.X - P2.X) < Epsilon) and (abs(P1.Y - P2.Y) < Epsilon);
end;


function KMSamePointDir(const P1,P2: TKMPointDir): boolean;
begin
  Result := ( P1.Loc.X = P2.Loc.X ) and ( P1.Loc.Y = P2.Loc.Y ) and ( P1.Dir = P2.Dir );
end;


function KMRect(aLeft, aTop, aRight, aBottom: SmallInt): TKMRect;
begin
  Result.Left   := aLeft;
  Result.Right  := aRight;
  Result.Top    := aTop;
  Result.Bottom := aBottom;
end;


//Make rect with single point
function KMRect(const aPoint: TKMPoint): TKMRect;
begin
  Result.Left   := aPoint.X;
  Result.Right  := aPoint.X;
  Result.Top    := aPoint.Y;
  Result.Bottom := aPoint.Y;
end;


//Encompass PointF into fixed-point rect (2x2)
function KMRect(const aPoint: TKMPointF): TKMRect;
begin
  Result.Left   := Floor(aPoint.X) - Byte(Frac(aPoint.X) = 0);
  Result.Right  := Ceil(aPoint.X)  + Byte(Frac(aPoint.X) = 0);
  Result.Top    := Floor(aPoint.Y) - Byte(Frac(aPoint.Y) = 0);
  Result.Bottom := Ceil(aPoint.Y)  + Byte(Frac(aPoint.Y) = 0);
end;


function KMRectF(const aRect: TKMRect): TKMRectF;
begin
  Result.Left   := aRect.Left;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top;
  Result.Bottom := aRect.Bottom;
end;


function KMRectF(const aPoint: TKMPointF): TKMRectF;
begin
  Result.Left   := aPoint.X;
  Result.Right  := aPoint.X;
  Result.Top    := aPoint.Y;
  Result.Bottom := aPoint.Y;
end;


function KMRectF(aLeft, aTop, aRight, aBottom: SmallInt): TKMRectF;
begin
  Result.Left   := aLeft;
  Result.Right  := aRight;
  Result.Top    := aTop;
  Result.Bottom := aBottom;
end;


function KMRectF(aLeft, aTop, aRight, aBottom: Single): TKMRectF;
begin
  Result.Left   := aLeft;
  Result.Right  := aRight;
  Result.Top    := aTop;
  Result.Bottom := aBottom;
end;


function KMRectRound(const aRect: TKMRectF): TKMRect;
begin
  Result.Left   := Round(aRect.Left);
  Result.Right  := Round(aRect.Right);
  Result.Top    := Round(aRect.Top);
  Result.Bottom := Round(aRect.Bottom);
end;


function KMSameRect(const aRect1, aRect2: TKMRect): Boolean;
begin
  Result := (aRect1.Left = aRect2.Left)
        and (aRect1.Top = aRect2.Top)
        and (aRect1.Right = aRect2.Right)
        and (aRect1.Bottom = aRect2.Bottom);
end;


function KMRectWidth(const aRect: TKMRect): Integer;
begin
  Result := aRect.Right - aRect.Left;
end;


function KMRectHeight(const aRect: TKMRect): Integer;
begin
  Result := aRect.Bottom - aRect.Top;
end;


function KMRectGrow(const aRect: TKMRect; aInset: Integer): TKMRect;
begin
  Result.Left   := Math.Max(aRect.Left   - aInset, 0);
  Result.Right  := Math.Max(aRect.Right  + aInset, 0);
  Result.Top    := Math.Max(aRect.Top    - aInset, 0);
  Result.Bottom := Math.Max(aRect.Bottom + aInset, 0);
end;

function KMRectGrow(const aPoint : TKMPoint; aInset: Integer): TKMRect;
begin
 Result := KMRectGrow(KMRect(aPoint), aInset);
end;

function KMRectGrowNoLimits(const aRect: TKMRect; aInset: Integer): TKMRect;
begin
  Result.Left   := aRect.Left   - aInset;
  Result.Right  := aRect.Right  + aInset;
  Result.Top    := aRect.Top    - aInset;
  Result.Bottom := aRect.Bottom + aInset;
end;


function KMRectGrow(const aRect, aInsetRect: TKMRect): TKMRect;
begin
  Result.Left   := Math.Max(aRect.Left   + aInsetRect.Left,   0);
  Result.Right  := Math.Max(aRect.Right  + aInsetRect.Right,  0);
  Result.Top    := Math.Max(aRect.Top    + aInsetRect.Top,    0);
  Result.Bottom := Math.Max(aRect.Bottom + aInsetRect.Bottom, 0);
end;


function KMRectGrow(const aRect: TKMRect; const aDir: TKMDirection; aInset: Integer = 1): TKMRect; overload;
begin
  case aDir of
    dirNA: Result := KMRectGrow(aRect, aInset);
    dirNE: Result := KMRectGrowTopRight(aRect, aInset);
    dirSE: Result := KMRectGrowBottomRight(aRect, aInset);
    dirSW: Result := KMRectGrowBottomLeft(aRect, aInset);
    dirNW: Result := KMRectGrowTopLeft(aRect, aInset);
  else
    Result := aRect; //not implemented yet
  end;
end;


function KMRectGrowTopLeft(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
begin
  Result.Left   := aRect.Left - aInset;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top  - aInset;
  Result.Bottom := aRect.Bottom;
end;


function KMRectGrowTopRight(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
begin
  Result.Left   := aRect.Left;
  Result.Right  := aRect.Right + aInset;
  Result.Top    := aRect.Top - aInset;
  Result.Bottom := aRect.Bottom;
end;


function KMRectGrowBottomLeft(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
begin
  Result.Left   := aRect.Left - aInset;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top;
  Result.Bottom := aRect.Bottom + aInset;
end;


function KMRectShinkTopLeft(const aRect: TKMRect): TKMRect;
begin
  Result.Left   := aRect.Left + 1;
  Result.Right  := aRect.Right;
  Result.Top    := aRect.Top  + 1;
  Result.Bottom := aRect.Bottom;
end;


function KMRectGrowBottomRight(const aRect: TKMRect; aInset: Integer = 1): TKMRect;
begin
  Result.Left   := aRect.Left;
  Result.Right  := aRect.Right + aInset;
  Result.Top    := aRect.Top;
  Result.Bottom := aRect.Bottom + aInset;
end;


function KMClipRect(const aRect: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect;
begin
  Result.Left   := EnsureRange(aRect.Left, X1, X2);
  Result.Right  := EnsureRange(aRect.Right, X1, X2);
  Result.Top    := EnsureRange(aRect.Top, Y1, Y2);
  Result.Bottom := EnsureRange(aRect.Bottom, Y1, Y2);
end;


function KMClipRect(const aRect1, aRect2: TKMRect): TKMRect;
begin
  Result := KMClipRect(aRect1, aRect2.Left, aRect2.Top, aRect2.Right, aRect2.Bottom);
end;


function KMRectIntersect(const aRect1: TKMRect; X1,Y1,X2,Y2: Integer): TKMRect;
begin
  if (aRect1.Right  < X1) 
  or (aRect1.Left   > X2)
  or (aRect1.Bottom < Y1)
  or (aRect1.Top    > Y2) then
    Result := KMRECT_INVALID_TILES
  else
    Result := KMClipRect(aRect1, X1,Y1,X2,Y2);
end;


function KMRectIntersect(const aRect1, aRect2: TKMRect): TKMRect;
begin
  Result := KMRectIntersect(aRect1, aRect2.Left, aRect2.Top, aRect2.Right, aRect2.Bottom); 
end;


function KMRectCorners(const aRect: TKMRect): TKMPointArray;
begin
  SetLength(Result, 4);
  Result[0] := KMPoint(aRect.Left, aRect.Top);
  Result[1] := KMPoint(aRect.Right, aRect.Top);
  Result[2] := KMPoint(aRect.Right, aRect.Bottom);
  Result[3] := KMPoint(aRect.Left, aRect.Bottom);
end;


function KMInRect(const aPoint: TKMPoint; const aRect: TKMRect): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMInRect(const aPoint: TKMPointF; const aRect: TKMRect): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMInRect(const aPoint: TKMPointF; const aRect: TKMRectF): Boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and InRange(aPoint.Y, aRect.Top, aRect.Bottom);
end;


function KMRectFitInRect(const aInnerRect, aOuterRect: TKMRect): Boolean;
begin
  Result := (KMRectHeight(aInnerRect) <= KMRectHeight(aOuterRect))
        and (KMRectWidth(aInnerRect) <= KMRectWidth(aOuterRect))
end;


function KMRectArea(const aRect: TKMRect):Integer;
begin
  Result := (aRect.Right - aRect.Left) * (aRect.Bottom  - aRect.Top);
end;


function KMRectMove(const aRect: TKMRect; X,Y: Integer): TKMRect;
begin
  Result.Left   := aRect.Left + X;
  Result.Right  := aRect.Right + X;
  Result.Top    := aRect.Top + Y;
  Result.Bottom := aRect.Bottom + Y;
end;


procedure KMRectIncludePoint(var aRect: TKMRect; X,Y: Integer);
begin
  KMRectIncludePoint(aRect, KMPoint(X,Y));
end;


procedure KMRectIncludePoint(var aRect: TKMRect; const aPoint: TKMPoint);
begin
  if KMInRect(aPoint, aRect) then Exit;
  aRect.Left    := Min(aPoint.X, aRect.Left);
  aRect.Right   := Max(aPoint.X, aRect.Right);
  aRect.Top     := Min(aPoint.Y, aRect.Top);
  aRect.Bottom  := Max(aPoint.Y, aRect.Bottom);
end;


procedure KMRectIncludeRect(var aRect: TKMRect; aRect2: TKMRect);
begin
  KMRectIncludePoint(aRect, aRect2.Left, aRect2.Top);
  KMRectIncludePoint(aRect, aRect2.Right, aRect2.Top);
  KMRectIncludePoint(aRect, aRect2.Right, aRect2.Bottom);
  KMRectIncludePoint(aRect, aRect2.Left, aRect2.Bottom);
end;


function KMGetDirection(aDirF: Single): TKMDirection;
begin
  // Convert angle value to direction
  // 3600 is a lame way of ensuring we deal with 0..359 angle
  Result := TKMDirection((Round((aDirF / Pi * 180) + 3600 + 22.5) mod 360) div 45 + 1);
end;


function KMGetDirection(X, Y: Single; aDirNAThreshold: Integer = 0): TKMDirection;
var
  ang, distSqr: Single;
begin
  distSqr := Sqr(X) + Sqr(Y);

  if distSqr > Sqr(aDirNAThreshold) then
  begin
    ang := ArcTan2(Y/distSqr, X/distSqr);
    // We have North at zero
    Result := KMGetDirection(ang + Pi/2);
  end
  else
    Result := dirNA;
end;


function KMGetDirection(const P: TKMPointF): TKMDirection;
begin
  Result := KMGetDirection(P.X, P.Y);
end;


function KMGetDirection(const FromPos, ToPos: TKMPoint): TKMDirection;
begin
  Result := KMGetDirection(Integer(ToPos.X - FromPos.X), Integer(ToPos.Y - FromPos.Y));
end;


function KMGetDirection(const FromPos, ToPos: TKMPointF): TKMDirection;
begin
  Result := KMGetDirection(ToPos.X - FromPos.X, ToPos.Y - FromPos.Y);
end;

function KMGetDirection(const FromPos, ToPos: TKMPointDir): TKMDirection;
begin
  Result := KMGetDirection(ToPos.X - FromPos.X, ToPos.Y - FromPos.Y);
end;

//  -3 0 3
//  -2   2
//  -1 0 1
function GetDirDifference(const aDir1, aDir2: TKMDirection): byte;
var tmpDir : TKMDirection;
  tmpLeft : Byte;
begin
  Result := 0; //turn right}
  If (aDir2 = aDir1) or (aDir2 = dirNA) or (aDir1 = dirNA) then
    Exit(0);

  tmpDir := aDir1;
  tmpLeft := 0;
  while tmpDir <> aDir2 do
  begin
    tmpDir := DIR_TO_PREV[tmpDir];
    tmpLeft := tmpLeft + 1;
  end;
  if tmpLeft < 4 then
    Result := 1 //turn left;
end;

// How big is the difference between directions (in fights hit from behind is 5 times stronger)
//  1 0 1
//  2   2
//  3 4 3
function GetDirModifier(const aDir1, aDir2: TKMDirection): Byte;
begin
  Result := Abs(Ord(aDir1) - ((Ord(aDir2) + 4) mod 8));

  if Result > 4 then
    Result := 8 - Result; // Mirror it, as the difference must always be 0..4
end;


function KMGetVertexDir(X,Y: Integer): TKMDirection;
const
  DirectionsBitfield: array [-1..0, -1..0] of TKMDirection = ((dirSE, dirNE), (dirSW, dirNW));
begin
  Result := DirectionsBitfield[X,Y];
end;


function KMGetVertexTile(const P: TKMPoint; const Dir: TKMDirection): TKMPoint;
const
  XBitField: array [TKMDirection] of SmallInt = (0,0,1,0,1,0,0,0,0);
  YBitField: array [TKMDirection] of SmallInt = (0,0,0,0,1,0,1,0,0);
begin
  Result := KMPoint(P.X+XBitField[Dir], P.Y+YBitField[Dir]);
end;


function KMGetVertex(const aDir: TKMDirection): TKMPointF;
const
  XBitField: array [TKMDirection] of single = (0, 0, 0.7,1,0.7,0,-0.7,-1,-0.7);
  YBitField: array [TKMDirection] of single = (0,-1,-0.7,0,0.7,1, 0.7, 0,-0.7);
begin
  Result := KMPointF(XBitField[aDir], YBitField[aDir]);
end;


function KMGetPointInDir(const aPoint: TKMPoint; const aDir: TKMDirection; aDist: Byte = 1): TKMPoint;
const
  XBitField: array [TKMDirection] of SmallInt = (0, 0, 1, 1, 1, 0,-1,-1,-1);
  YBitField: array [TKMDirection] of SmallInt = (0,-1,-1, 0, 1, 1, 1, 0,-1);
begin
  Result.X := aPoint.X + XBitField[aDir] * aDist;
  Result.Y := aPoint.Y + YBitField[aDir] * aDist;
end;


function KMAddDirection(const aDir: TKMDirection; aAdd: Integer): TKMDirection;
begin
  Assert(aDir <> dirNA);
  Result := TKMDirection((Ord(aDir) + aAdd - 1 + 8) mod 8 + 1);
end;


function KMNextDirection(const aDir: TKMDirection): TKMDirection;
begin
  if aDir < dirNW then
    Result := Succ(aDir)
  else
    Result := dirN; //Rewind to start
end;


function KMPrevDirection(const aDir: TKMDirection): TKMDirection;
begin
  if aDir > dirN then
    Result := Pred(aDir)
  else
    Result := dirNW; //Rewind to end
end;


function KMPointsAround(const P: TKMPoint; aIncludeSelf: Boolean = False): TKMPointArray;
var
  I,J,K: Integer;
begin
  SetLength(Result, 8 + Ord(aIncludeSelf));

  K := 0;
  for I := -1 to 1 do
    for J := -1 to 1 do
      if aIncludeSelf or (I <> 0) or (J <> 0) then
      begin
        Result[K] := KMPoint(P.X + J, P.Y + I);
        Inc(K);
      end;
end;


function KMGetDiagVertex(const P1,P2: TKMPoint): TKMPoint;
begin
  // Returns the position of the vertex inbetween the two diagonal points (points must be diagonal)
  Result.X := Max(P1.X,P2.X);
  Result.Y := Max(P1.Y,P2.Y);
end;


function KMStepIsDiag(const P1,P2: TKMPoint): Boolean;
begin
  Result := (P2.X - P1.X <> 0) and (P2.Y - P1.Y <> 0);
end;


function KMPointAverage(const A, B: TKMPoint): TKMPoint;
begin
  Result.X := (A.X + B.X) shr 1;
  Result.Y := (A.Y + B.Y) shr 1;
end;


function KMPointSubtract(const A, B: TKMPoint): TKMPoint;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;


function KMPointAdd(const A, B: TKMPoint): TKMPoint;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;


function KMPointAdd(const A, B, C: TKMPoint): TKMPoint;
begin
  Result.X := A.X + B.X + C.X;
  Result.Y := A.Y + B.Y + C.Y;
end;


function KMPointFAdd(const A, B: TKMPointF): TKMPointF;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function KMDotProduct(const A, B: TKMPoint): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;


// Faster version of distance when we need just approximate information and Integer
function KMDistanceAbs(const A, B: TKMPoint): Integer;
begin
  Result := Abs(A.X - B.X) + Abs(A.Y - B.Y);
end;

// Walking distance in relation to time to get from point A to B (movement in a straight line, vertically / horizontally / diagonally)
function KMDistanceWalk(const A, B: TKMPoint): Integer;
begin
  Result := Max(Abs(A.X - B.X), Abs(A.Y - B.Y));
end;


function KMDistanceSqr(const A, B: TKMPoint): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMDistanceSqr(const A, B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMPerpendecular(const A,B: TKMPoint): TKMPointF;
var
  Tmp: TKMPointF;
  D: Single;
begin
  Tmp.X := B.X - A.X;
  Tmp.Y := B.Y - A.Y;

  D := Sqrt(Tmp.X * Tmp.X + Tmp.Y * Tmp.Y);

  Result.X := A.X - Tmp.Y / D;
  Result.Y := A.Y + Tmp.X / D;
end;


function KMNormal2Poly(const v1,v2,v3: TKMPoint): Single;
begin
  Result := (v1.Y - v2.Y) * (v1.X - v3.X) - (v1.X - v2.X) * (v1.Y - v3.Y);
end;


function KMPointInTriangle(const P, A, B, C: TKMPoint): Boolean;
begin
  Result := (KMNormal2Poly(A, B, P) > 0) and (KMNormal2Poly(B, C, P) > 0) and (KMNormal2Poly(C, A, P) > 0);
end;


//Segments intersect
function KMSegmentsIntersect(const A, B, C, D: TKMPoint): Boolean;
var
  ABx, ABy, CDx, CDy: Single;
  D2, S, T: Single;
begin
  ABx := B.x - A.x;     ABy := B.y - A.y;
  CDx := D.x - C.x;     CDy := D.y - C.y;

  D2 := -CDx * ABy + ABx * CDy;

  S := (-ABy * (A.x - C.x) + ABx * (A.y - C.y)) / D2;
  T := ( CDx * (A.y - C.y) - CDy * (A.x - C.x)) / D2;

  Result := (S > 0) and (S < 1) and (T > 0) and (T < 1)
            and not IsNaN(S) and not IsNaN(T)
            and not IsInfinite(S) and not IsInfinite(T);
end;


function KMSegmentsIntersectOrTouch(const A, B, C, D: TKMPoint): Boolean;
var
  ABx, ABy, CDx, CDy: Single;
  D2, S, T: Single;
begin
  ABx := B.x - A.x;     ABy := B.y - A.y;
  CDx := D.x - C.x;     CDy := D.y - C.y;

  //todo -cPractical: Cover with tests and rewrite to avoid / 0. In Delphi Rio behaviour will change
  // As said by Rey:
  //if S = -NaN and T = -NaN, then
  //Result := (S >= 0) and (S <= 1) and (T >= 0) and (T <= 1);
  // Delphi Berlin - False
  // Delphi Rio - True
  D2 := -CDx * ABy + ABx * CDy;

  S := (-ABy * (A.x - C.x) + ABx * (A.y - C.y)) / D2;
  T := ( CDx * (A.y - C.y) - CDy * (A.x - C.x)) / D2;

  Result := (S >= 0) and (S <= 1) and (T >= 0) and (T <= 1)
            and not IsNaN(S) and not IsNaN(T)
            and not IsInfinite(S) and not IsInfinite(T);
end;


function KMLength(A,B: Single): Single;
begin
  Result := Sqrt(Sqr(A) + Sqr(B));
end;


//True length between 2 points
function KMLength(const A,B: TKMPoint): Single;
begin
  Result := Sqrt(Sqr(A.X - B.X) + Sqr(A.Y - B.Y));
end;


function KMLength(const A,B: TKMPointF): Single;
begin
  Result := Sqrt(Sqr(A.X - B.X) + Sqr(A.Y - B.Y));
end;


//Rough and faster Length as combination of straight and diagonal
function KMLengthDiag(const A, B: TKMPoint): Single;
var
  absX, absY: Integer;
begin
  absX := Abs(A.X - B.X);
  absY := Abs(A.Y - B.Y);
  if absX > absY then
    Result := absX + absY * 0.41
  else
    Result := absY + absX * 0.41;
end;

//Rough and faster Length as combination of straight and diagonal
function KMLengthDiag(const A, B: TKMPointF): Single;
var
  absX, absY: Single;
begin
  absX := Abs(A.X - B.X);
  absY := Abs(A.Y - B.Y);
  if absX > absY then
    Result := absX + absY * 0.41
  else
    Result := absY + absX * 0.41;
end;


function KMLengthDiag(X, Y: Integer; const B: TKMPoint): Single;
begin
  if Abs(X - B.X) > Abs(Y - B.Y) then
    Result := Abs(X - B.X) + Abs(Y - B.Y) * 0.41
  else
    Result := Abs(Y - B.Y) + Abs(X - B.X) * 0.41;
end;


//Diag length to the KMPOINT_ZERO
function KMLengthDiag(X, Y: Integer): Single;
begin
  Result := KMLengthDiag(X, Y, KMPOINT_ZERO);
end;


//Squared length for cases where we need to compare two lengths
//or pick the best one and actual value is not that important
//we can save some cycles on ommitting SQRT
function KMLengthSqr(const A, B: TKMPoint): Integer;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLengthSqr(const X1, Y1, X2, Y2: Integer): Integer;
begin
  Result := Sqr(X1 - X2) + Sqr(Y1 - Y2);
end;


function KMLengthSqr(const A, B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLengthSqr(const A: TKMPoint; const B: TKMPointF): Single;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;


function KMLerp(const A,B: TKMPoint; MixValue: Single): TKMPointF;
begin
  Result.X := A.X + (B.X - A.X) * MixValue;
  Result.Y := A.Y + (B.Y - A.Y) * MixValue;
end;


function KMLerp(const A,B: TKMPointF; MixValue: Single): TKMPointF;
begin
  Result.X := A.X + (B.X - A.X) * MixValue;
  Result.Y := A.Y + (B.Y - A.Y) * MixValue;
end;

function KMLerp(const A,B: TKMPointDir; MixValue: Single): TKMPointF;
begin
  Result.X := A.X + (B.X - A.X) * MixValue;
  Result.Y := A.Y + (B.Y - A.Y) * MixValue;
end;


procedure KMSwapPoints(var A,B: TKMPoint);
var T: Integer;
begin
  T:=A.X; A.X:=B.X; B.X:=T;
  T:=A.Y; A.Y:=B.Y; B.Y:=T;
end;


procedure KMSwapPointDir(var A,B: TKMPointDir);
var
  T: TKMPointDir;
begin
  T := A;
  A := B;
  B := T;
end;


function TypeToString(const P: TKMPoint): string;
begin
  Result := '(' + IntToStr(P.X) + ';' + IntToStr(P.Y) + ')';
end;

function TypeToString(const P: TKMPointW): string;
begin
  Result := '(' + IntToStr(P.X) + ';' + IntToStr(P.Y) + ')';
end;


function TypeToString(const P: TKMPointDir): string;
begin
  Result := Format('%s Dir = %s', [TypeToString(P.Loc), GetEnumName(TypeInfo(TKMDirection), Integer(P.Dir))]);
end;


function TypeToString(const P: TKMPointF): string;
begin
  Result := Format('(%s;%s)', [FormatFloat('##0.##', P.X), FormatFloat('##0.##', P.Y)]);
end;


function StringToType(const Str: String): TKMPoint;
var
  DelimPos, X, Y: Integer;
begin
  Result := KMPOINT_INVALID_TILE;
  DelimPos := StrIndexOf(Str, ';');
  if DelimPos > 0 then
  begin
    if TryStrToInt(Copy(Str, 2, DelimPos - 1), X)
      and TryStrToInt(Copy(Str, DelimPos + 2, Length(Str) - DelimPos - 2), Y) then
      Result := KMPoint(X,Y);
  end;
end;


function TypeToString(const T: TKMDirection): string;
const
  S: array [TKMDirection] of string = ('N/A', 'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW');
begin
  Result := S[T];
end;


function KMRange(aMin, aMax: Integer): TKMRangeInt;
begin
  Result.Min := Min(aMin, aMax);
  Result.Max := Max(aMin, aMax);
end;


function KMRange(aMin, aMax: Single): TKMRangeSingle;
begin
  Result.Min := Min(aMin, aMax);
  Result.Max := Max(aMin, aMax);
end;


function KMInRange(aValue: Integer; const aRangeInt: TKMRangeInt): Boolean;
begin
  Result := InRange(aValue, aRangeInt.Min, aRangeInt.Max);
end;


function KMInRange(aValue: Single; const aRangeSingle: TKMRangeSingle): Boolean;
begin
  Result := InRange(aValue, aRangeSingle.Min, aRangeSingle.Max);
end;


function KMEnsureRange(aValue: Integer; const aRange: TKMRangeInt): Integer;
begin
  Result := EnsureRange(aValue, aRange.Min, aRange.Max);
end;


function KMEnsureRange(aValue: Single; const aRange: TKMRangeSingle): Single;
begin
  Result := EnsureRange(aValue, aRange.Min, aRange.Max);
end;


function KMEnlargeRange(const aRange: TKMRangeInt; aValue: Integer): TKMRangeInt;
begin
  Result.Min := Min(aRange.Min, aValue);
  Result.Max := Max(aRange.Max, aValue);
end;


function KMEnlargeRange(const aRange: TKMRangeSingle; aValue: Single): TKMRangeSingle;
begin
  Result.Min := Min(aRange.Min, aValue);
  Result.Max := Max(aRange.Max, aValue);
end;

function KMPointAffectDir(const aLoc : TKMPointF; aDir : TKMDirection; aAddDistance : Single) : TKMPointF;
begin
  case aDir of
    dirNA : Result := aLoc;
    dirN : Result := KMPointFAdd(aLoc, KMPointF(1 * aAddDistance, 0 * aAddDistance) );
    dirNE : Result := KMPointFAdd(aLoc, KMPointF(-1 * aAddDistance, -1 * aAddDistance) );
    dirE : Result := KMPointFAdd(aLoc, KMPointF(0 * aAddDistance, 1 * aAddDistance) );
    dirSE : Result := KMPointFAdd(aLoc, KMPointF(1 * aAddDistance, -1 * aAddDistance) );
    dirS : Result := KMPointFAdd(aLoc, KMPointF(1 * aAddDistance, 0 * aAddDistance) );
    dirSW : Result := KMPointFAdd(aLoc, KMPointF(-1 * aAddDistance, -1 * aAddDistance) );
    dirW : Result := KMPointFAdd(aLoc, KMPointF(0 * aAddDistance, 1 * aAddDistance) );
    dirNW : Result := KMPointFAdd(aLoc, KMPointF(-1 * aAddDistance, 1 * aAddDistance) );
  end;
end;

function KMPointRandomInRadius(const aLoc : TKMPoint; aRadius : Integer) : TKMPoint;
begin
  Result.X := aLoc.X + KamRandomI2(aRadius, 'KMPointRandomInRadius');
  Result.Y := aLoc.Y + KamRandomI2(aRadius, 'KMPointRandomInRadius');
end;

function KMPointFRandomInRadius(const aLoc : TKMPointF; aRadius : Single) : TKMPointF;
begin
  Result.X := aLoc.X + KamRandomS2(aRadius, 'KMPointFRandomInRadius');
  Result.Y := aLoc.Y + KamRandomS2(aRadius, 'KMPointFRandomInRadius');
end;




end.
