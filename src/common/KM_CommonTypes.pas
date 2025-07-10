unit KM_CommonTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_Points, Classes;

type
  TKMByteSet = set of Byte;
  //* Array of bytes
  //Legacy support for old scripts
  TByteSet = set of Byte;

  TBooleanArray = array of Boolean;
  TBoolean2Array = array of array of Boolean;
  TKMByteArray = array of Byte;
  TKMByte2Array = array of TKMByteArray;
  TKMByteSetArray = array of TKMByteSet;
  PKMByte2Array = ^TKMByte2Array;
  TKMWordArray = array of Word;
  TKMShortIntArray = array of ShortInt;
  TKMWord2Array = array of TKMWordArray;
  PKMWordArray = ^TKMWordArray;
  TKMCardinalArray = array of Cardinal;
  PKMCardinalArray = ^TKMCardinalArray;
  TSmallIntArray = array of SmallInt;
  //* array of integer values
  TIntegerArray = array of Integer;
  TInteger2Array = array of array of Integer;
  //* array of string values
  TAnsiStringArray = array of AnsiString;
  TSingleArray = array of Single;
  TSingle2Array = array of array of Single;
  TKMStringArray = array of string;
  TKMCharArray = array of Char;
  TRGB = record R,G,B: Byte end;
  TRGBArray = array of TRGB;
  TKMStaticByteArray = array [0..MaxInt - 1] of Byte;
  PKMStaticByteArray = ^TKMStaticByteArray;
  TKMVarRecArray = array of TVarRec;

  TEvent = procedure of object;
  TPointEvent = procedure (Sender: TObject; const X,Y: Integer) of object;
  TPointEventSimple = procedure (const X,Y: Integer) of object;
  TPointEventFunc = function (Sender: TObject; const X,Y: Integer): Boolean of object;
  TPointFEvent = procedure (const aPoint: TKMPointF) of object;
  TBooleanEvent = procedure (aValue: Boolean) of object;
  TBooleanObjEvent = procedure (Sender: TObject; aValue: Boolean) of object;
  TIntegerEvent = procedure (aValue: Integer) of object;
  TIntegerShiftEvent = procedure (aValue: Integer; Shift : TShiftState) of object;
  TIntBoolEvent = procedure (aIntValue: Integer; aBoolValue: Boolean) of object;
  TObjectIntegerEvent = procedure (Sender: TObject; X: Integer) of object;
  TSingleEvent = procedure (aValue: Single) of object;
  TAnsiStringEvent = procedure (const aData: AnsiString) of object;
  TUnicodeStringEvent = procedure (const aData: UnicodeString) of object;
  TUnicodeStringWDefEvent = procedure (const aData: UnicodeString = '') of object;
  TUnicodeStringEventProc = procedure (const aData: UnicodeString);
  TUnicode2StringEventProc = procedure (const aData1, aData2: UnicodeString);
  TUnicodeStringObjEvent = procedure (Obj: TObject; const aData: UnicodeString) of object;
  TUnicodeStringObjEventProc = procedure (Sender: TObject; const aData: UnicodeString);
  TUnicodeStringBoolEvent = procedure (const aData: UnicodeString; aBool: Boolean) of object;
  TGameStartEvent = procedure (const aData: UnicodeString; Spectating: Boolean) of object;
  TResyncEvent = procedure (aSender: ShortInt; aTick: cardinal) of object;
  TIntegerStringEvent = procedure (aValue: Integer; const aText: UnicodeString) of object;
  TBooleanFunc = function(Obj: TObject): Boolean of object;
  TBooleanWordFunc = function (aValue: Word): Boolean of object;
  TBooleanStringFunc = function (const aValue: String): Boolean of object;
  TBooleanFuncSimple = function: Boolean of object;
  TBoolIntFuncSimple = function (aValue: Integer): Boolean of object;
  TBoolCardFuncSimple = function (aValue: Cardinal): Boolean of object;
  TCardinalEvent = procedure (aValue: Cardinal) of object;
  TObjectIntBoolEvent = procedure (Sender: TObject; aIntValue: Integer; aBoolValue: Boolean) of object;
  TCoordDistanceFn = function (X, Y: Integer): Single;

  //local procedures
  TKMPointEventSimple = procedure (const X, Y: Integer);
  TKMPointEvent = procedure (const aLoc : TKMPoint);


  TPointerArray = array of Pointer;
  {$IFDEF FPC}
  TProc = procedure;
  {$ENDIF}

  TKMAnimLoop = packed record
    Step: array [1 .. 30] of SmallInt;
    Count: SmallInt;
    MoveX, MoveY: Integer;
    procedure Create(X, Y : Integer; Steps : Array of Integer; aOffset : Byte = 0);overload;
    procedure Create(X, Y : Integer; Steps : TIntegerArray; aOffset : Byte = 0);overload;
    procedure Create(X, Y : Integer; Steps : TKMWordArray; aOffset : Byte = 0);overload;
    procedure Create(Steps : TIntegerArray; aDuplicates : Byte = 1);overload;
    procedure Create(X, Y, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false);overload;
    procedure Clear;
    function IsLinear : Boolean;
  end;

  TKMAnimation = packed record
  private
    fSteps: TKMWordArray;
    function GetAnimation(aIndex : Integer) : Word;
    Procedure SetAnimation(aIndex : Integer; aValue : Word);
    function GetStep(aIndex : Integer) : Word;
    procedure SetStep(aIndex : Integer; aValue : Word);
  public
    X, Y : Integer;
    property Animation[aIndex : Integer] : Word read GetAnimation write SetAnimation; default;
    property Step[aIndex : Integer] : Word read GetStep write SetStep;
    property StepArray : TKMWordArray read fSteps;
    function Count : Word;
    procedure Clear;
    procedure Create(aX, aY : Integer; aAnimation : TKMWordArray; aOffset : Byte = 0); overload;
    procedure Create(aX, aY, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false);overload;
    procedure Create(aAnimation : TKMAnimLoop); overload;
    procedure Create(aName : String); overload;

    procedure Delete(aIndex : Integer);
    procedure Extend(aCount : Byte);
    procedure SaveToStream(SaveStream : TObject);
    procedure LoadFromStream(LoadStream : TObject);

    function IsLinear : Boolean;
  end;

  PKAnimation = ^TKMAnimation;

  function Anim(aX, aY : Integer; aAnimation : TKMWordArray; aOffset : Byte = 0) : TKMAnimation;overload;
  function Anim(aX, aY, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false) : TKMAnimation;overload;
  function Anim(aAnimation : TKMAnimLoop) : TKMAnimation;overload;
  function Anim(const aAnimation : TKMAnimation) : TKMAnimation;overload;
  function Anim(aName : String) : TKMAnimation;overload;

type
  TKMCursorDir = (cdNone = 0, cdForward = 1, cdBack = -1);

  TWonOrLost = (wolNone, wolWon, wolLost);

  //Menu load type - load / no load / load unsupported version
  TKMGameStartMode = (gsmNoStart, gsmStart, gsmStartWithWarn, gsmNoStartWithWarn);

  TKMCustomScriptParam = (cspTHTroopCosts, cspMarketGoldPrice);

  TKMCustomScriptParamData = record
    Added: Boolean;
    Data: UnicodeString;
  end;

  TKMImageType = (itJpeg, itPng, itBmp);

  TKMUserActionType = (uatNone, uatKeyDown, uatKeyUp, uatKeyPress, uatMouseDown, uatMouseUp, uatMouseMove, uatMouseWheel);
  TKMUserActionEvent = procedure (aActionType: TKMUserActionType) of object;


  TKMCustomScriptParamDataArray = array [TKMCustomScriptParam] of TKMCustomScriptParamData;

  TKMPlayerColorMode = (pcmNone, pcmDefault, pcmAllyEnemy, pcmTeams);

  TKMGameRevision = Word; // Word looks enough for now...

  TKMColor3f = record
    R,G,B: Single;
    function ToCardinal: Cardinal;
    class function Generic(aIndex: Integer): TKMColor3f; static;
    class function RandomWSeed(aSeed: Integer): TKMColor3f; static;
    class function New(aR,aG,aB: Single): TKMColor3f; static;
    class function NewB(aR,aG,aB: Byte): TKMColor3f; static;
    class function NewC(aRGB: Cardinal): TKMColor3f; static;
    function ToString: string;
    procedure SetColor(aR,aG,aB: Single);
  end;

  TKMColor3b = record
    R,G,B: Byte;
    function ToCardinal: Cardinal;
    function ToCardinal2: Cardinal;
    class function New(aR,aG,aB: Byte): TKMColor3b; static;
  end;

  TKMColor4f = record
    R,G,B,A: Single;
    class function New(aR,aG,aB,aA: Single): TKMColor4f; overload; static;
    class function New(aR,aG,aB: Byte): TKMColor4f; overload; static;
    class function NewB(aR,aG,aB,aA: Byte): TKMColor4f; overload; static;
    class function New(aCol: Cardinal): TKMColor4f; overload; static;
    class function New(aCol: TKMColor3f): TKMColor4f; overload; static;
    class function New(const aCol: TKMColor3f; aAlpha: Single): TKMColor4f; overload; static;
    class function White: TKMColor4f; static;
    class function Black: TKMColor4f; static;
    function Alpha50: TKMColor4f;
    function Alpha(aAlpha: Single): TKMColor4f;
    function ToColor3f: TKMColor3f;
    function ToCardinal: Cardinal;
  end;
  //settings for weather
  //it's used in few files, not just in KM_GameSettings
  TKMSettingsWeather = packed record
    Overwrite,
    Enabled : Boolean;
    MaxCount, MaxSpawnCount : Integer;
    MinInterval, MaxInterval, MaxLifeTime : Integer;
    MaxCloudSpeed : Single;
    DecParticles, NightTime : Integer;
    NightSpeed: Byte;
    DynamicLight : Boolean;
    procedure SetDefault;
    procedure SetRealism;
    function Copy : TKMSettingsWeather;
  end;



  TKMColor3bArray = array of TKMColor3b;
  TKMProgressBarOrientation = (pboLeft, pboRight, pboUp, pboDown);
const
  COLOR3F_WHITE: TKMColor3f = (R: 1; G: 1; B: 1);
  COLOR3F_BLACK: TKMColor3f = (R: 0; G: 0; B: 0);

  COLOR4F_WHITE: TKMColor4f = (R: 1; G: 1; B: 1; A: 1);
  COLOR4F_BLACK: TKMColor4f = (R: 0; G: 0; B: 0; A: 1);

const
  IMAGE_TYPE_EXT: array[TKMImageType] of string = ('.jpeg', '.png', '.bmp');

const
  WonOrLostText: array [TWonOrLost] of UnicodeString = ('None', 'Won', 'Lost');

  NO_SUCCESS_INT: Integer = -1;

implementation
uses
  Math, SysUtils, KM_CommonUtils, KM_CommonClasses, KM_Resource,
  KM_Defaults;


{ TKMColor3f }
class function TKMColor3f.New(aR, aG, aB: Single): TKMColor3f;
begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
end;


class function TKMColor3f.NewB(aR, aG, aB: Byte): TKMColor3f;
begin
  Result.R := aR / 255;
  Result.G := aG / 255;
  Result.B := aB / 255;
end;


class function TKMColor3f.NewC(aRGB: Cardinal): TKMColor3f;
begin
  Result.B := (aRGB and $FF) / 255;
  Result.G := (aRGB shr 8 and $FF) / 255;
  Result.R := (aRGB shr 16 and $FF) / 255;
end;


function TKMColor3f.ToCardinal: Cardinal;
begin
  Result := (Round(R * 255) + (Round(G * 255) shl 8) + (Round(B * 255) shl 16)) {or $FF000000};
end;


class function TKMColor3f.Generic(aIndex: Integer): TKMColor3f;
const
  MAX_GENERIC_COLORS = 6;
  GENERIC_COLORS: array [0..MAX_GENERIC_COLORS-1] of TKMColor3f = (
    (R:1.0; G:0.2; B:0.2),
    (R:1.0; G:1.0; B:0.2),
    (R:0.2; G:1.0; B:0.2),
    (R:0.2; G:1.0; B:1.0),
    (R:0.2; G:0.2; B:1.0),
    (R:1.0; G:0.2; B:1.0)
  );
begin
  Result := GENERIC_COLORS[aIndex mod MAX_GENERIC_COLORS];
end;

procedure TKMColor3f.SetColor(aR: Single; aG: Single; aB: Single);
begin
  R := aR;
  G := aG;
  B := aB;
end;

class function TKMColor3f.RandomWSeed(aSeed: Integer): TKMColor3f;
begin
  Result.R := KaMRandomWSeedS1(aSeed, 1);
  Result.G := KaMRandomWSeedS1(aSeed, 1);
  Result.B := KaMRandomWSeedS1(aSeed, 1);
end;


function TKMColor3f.ToString: string;
begin
  Result := Format('[%d:%d:%d]', [Round(255*R), Round(255*G), Round(255*B)]);
end;


{ TKMColor3b }
class function TKMColor3b.New(aR, aG, aB: Byte): TKMColor3b;
begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
end;


function TKMColor3b.ToCardinal: Cardinal;
begin
  Result := (R + (G shl 8) + (B shl 16)); {or $FF000000};
end;
function TKMColor3b.ToCardinal2: Cardinal;
begin
  Result := (R + (G shl 8) + (B shl 16)) or $FF000000;
end;


{ TKMColor4f }
class function TKMColor4f.New(aR,aG,aB,aA: Single): TKMColor4f;
begin
  Result.R := aR;
  Result.G := aG;
  Result.B := aB;
  Result.A := aA;
end;


class function TKMColor4f.New(aR,aG,aB: Byte): TKMColor4f;
begin
  Result.R := aR / 255;
  Result.G := aG / 255;
  Result.B := aB / 255;
  Result.A := 1;
end;


class function TKMColor4f.NewB(aR,aG,aB,aA: Byte): TKMColor4f;
begin
  Result.R := aR / 255;
  Result.G := aG / 255;
  Result.B := aB / 255;
  Result.A := aA / 255;
end;


class function TKMColor4f.New(aCol: Cardinal): TKMColor4f;
begin
  Result.R := (aCol and $FF)           / 255;
  Result.G := ((aCol shr 8) and $FF)   / 255;
  Result.B := ((aCol shr 16) and $FF)  / 255;
  Result.A := ((aCol shr 24) and $FF)  / 255;
end;


class function TKMColor4f.New(aCol: TKMColor3f): TKMColor4f;
begin
  New(aCol, 1);
end;


class function TKMColor4f.New(const aCol: TKMColor3f; aAlpha: Single): TKMColor4f;
begin
  Result.R := aCol.R;
  Result.G := aCol.G;
  Result.B := aCol.B;
  Result.A := aAlpha;
end;


class function TKMColor4f.White: TKMColor4f;
begin
  Result.R := 1;
  Result.G := 1;
  Result.B := 1;
  Result.A := 1;
end;


class function TKMColor4f.Black: TKMColor4f;
begin
  Result.R := 0;
  Result.G := 0;
  Result.B := 0;
  Result.A := 1;
end;


function TKMColor4f.Alpha50: TKMColor4f;
begin
  Result := Self;
  Result.A := 0.5;
end;


function TKMColor4f.Alpha(aAlpha: Single): TKMColor4f;
begin
  Result := Self;
  Result.A := aAlpha;
end;


function TKMColor4f.ToColor3f: TKMColor3f;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;


function TKMColor4f.ToCardinal: Cardinal;
begin
  Result := Round(R * 255) + (Round(G * 255) shl 8) + (Round(B * 255) shl 16) + (Round(A * 255) shl 24);
end;

procedure TKMAnimLoop.Create(X: Integer; Y: Integer; Steps: array of Integer; aOffset : Byte = 0);
var I : Integer;
begin
  MoveX := X;
  MoveY := Y;
  Count := length(Steps);
  for I := 1 to Count do
    Step[I] := Steps[(I - 1 + aOffset) mod Count];
end;

procedure TKMAnimLoop.Create(X: Integer; Y: Integer; Steps: TIntegerArray; aOffset : Byte = 0);
var I : Integer;
begin
  MoveX := X;
  MoveY := Y;
  Count := length(Steps);
  for I := 1 to 30 do
    Step[I] := 0;

  for I := 1 to Count do
    Step[I] := Steps[(I - 1 + aOffset) mod Count];
end;

procedure TKMAnimLoop.Create(X: Integer; Y: Integer; Steps: TKMWordArray; aOffset : Byte = 0);
var I : Integer;
begin
  MoveX := X;
  MoveY := Y;
  Count := length(Steps);
  for I := 1 to 30 do
    Step[I] := 0;

  for I := 1 to Count do
    Step[I] := Steps[(I - 1 + aOffset) mod Count];
end;

procedure TKMAnimLoop.Create(Steps: TIntegerArray; aDuplicates: Byte = 1);
var I, J, lastID: Integer;
begin
  MoveX := 0;
  MoveY := 0;
  Count := length(Steps);
  aDuplicates := Max(aDuplicates, 1);

  for I := 1 to 30 do
    Step[I] := 0;
  lastID := 1;
  for I := 1 to Count do
    for J := 1 to aDuplicates do
    begin
      Step[lastID] := Steps[I - 1];
      Inc(lastID);
    end;

  Count := lastID - 1;
end;

procedure TKMAnimLoop.Create(X, Y, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false);
var I : integer;
begin
  MoveX := X;
  MoveY := Y;
  Count := aCount;
  for I := 1 to 30 do
    Step[I] := 0;
  if doBackWard then
  begin
    for I := 0 to aCount - 1 do
      Step[I + 1] := StepStart + ((aCount - I - 1) + aOffset) mod aCount;
  end else
  for I := 0 to aCount - 1 do
    Step[I + 1] := StepStart + (I + aOffset) mod aCount;
end;

procedure TKMAnimLoop.Clear;
var I : Integer;
begin
  for I := Low(Step) to High(Step) do
    Step[I] := -1;
  Count := 0;
  MoveX := 0;
  MoveY := 0;
end;

function TKMAnimLoop.IsLinear : Boolean;
var I, J : Integer;
begin
  If Count = 0 then
    Exit(false);
  Result := true;
  J := Step[1];
  for I := 2 to Count do
    If Step[I] <> J + I then
      Exit(false);
end;

procedure TKMAnimation.Clear;
begin
  SetLength(fSteps, 0);
  X := 0;
  Y := 0;
end;

function TKMAnimation.GetAnimation(aIndex: Integer): Word;
begin
  if Count = 0 then
    Exit(0);
  Result := fSteps[aIndex mod length(fSteps)];
end;

procedure TKMAnimation.SetAnimation(aIndex: Integer; aValue: Word);
begin
  if aIndex >= length(fSteps) then
    SetLength(fSteps, aIndex + 1);
  fSteps[aIndex] := aValue;
end;

function TKMAnimation.GetStep(aIndex: Integer): Word;
begin
  Assert(InRange(aIndex, 0, high(fSteps)), 'TKMAnimation.NoAnimationFound: ' + IntToStr(aIndex));
  Result := fSteps[aIndex];
end;

procedure TKMAnimation.SetStep(aIndex: Integer; aValue: Word);
begin
  Assert(InRange(aIndex, 0, high(fSteps)), 'TKMAnimation.NoAnimationFound');
  fSteps[aIndex] := aValue;
end;

function TKMAnimation.Count: Word;
begin
  Result := length(fSteps);
end;

procedure TKMAnimation.Create(aX, aY, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false);
var I : integer;
begin
  Clear;
  X := aX;
  Y := aY;

  if doBackWard then
  begin
    for I := 0 to aCount - 1 do
      Animation[I] := StepStart + ((aCount - I - 1) + aOffset) mod aCount;
  end else
  for I := 0 to aCount - 1 do
    Animation[I] := StepStart + (I + aOffset) mod aCount;
end;

procedure TKMAnimation.Create(aX, aY : Integer; aAnimation : TKMWordArray; aOffset : Byte = 0);
var I, C : integer;
begin
  Clear;
  X := aX;
  Y := aY;
  C := length(aAnimation);

  for I := 0 to C - 1 do
    Animation[I] := aAnimation[(I + aOffset) mod C];
end;

procedure TKMAnimation.Create(aAnimation: TKMAnimLoop);
var I : Integer;
begin
  Clear;
  X := aAnimation.MoveX;
  Y := aAnimation.MoveY;
  SetLength(fSteps, 0);
  for I := 1 to aAnimation.Count do
    if aAnimation.Step[I] > 0 then
      Animation[I - 1] := aAnimation.Step[I]
    else
      Break;
end;

procedure TKMAnimation.Create(aName: string);
begin
  self := gRes.JsonData.Animation(aName);
end;

procedure TKMAnimation.Delete(aIndex : Integer);
var I : Integer;
begin
  If not InRange(aIndex, 0, Count - 1) then
    Exit;
  for I := aIndex to Count - 2 do
    fSteps[I] := fSteps[I + 1];
  SetLength(fSteps, high(fSteps));
end;

procedure TKMAnimation.Extend(aCount: Byte);
var I, J, lastID : Integer;
  aArr : TKMWordArray;
begin
  if aCount = 0 then
    Exit;
  SetLength(aArr, length(fSteps) * (aCount + 1));

  //SetLength(fSteps, length(tmp) * aCount )
  lastID := 0;
  for I := 0 to High(fSteps) do
    for J := 0 to aCount do
    begin
      aArr[lastID] := fSteps[I];
      inc(lastID);
    end;
  SetLength(aArr, lastID);
  fSteps := aArr;

end;

procedure TKMAnimation.SaveToStream(SaveStream: TObject);
var I, newCount : Integer;
begin
  if not (SaveStream is TKMemoryStream) then
    Exit;

  TKMemoryStream(SaveStream).Write(X);
  TKMemoryStream(SaveStream).Write(Y);
  newCount := length(fSteps);
  TKMemoryStream(SaveStream).Write(newCount);
  for I := 0 to newCount - 1 do
    TKMemoryStream(SaveStream).Write(fSteps[I]);
end;

procedure TKMAnimation.LoadFromStream(LoadStream: TObject);
var I, newCount : Integer;
begin
  if not (LoadStream is TKMemoryStream) then
    Exit;

  TKMemoryStream(LoadStream).Read(X);
  TKMemoryStream(LoadStream).Read(Y);
  TKMemoryStream(LoadStream).Read(newCount);
  SetLength(fSteps, newCount);
  for I := 0 to newCount - 1 do
    TKMemoryStream(LoadStream).Read(fSteps[I]);
end;

function TKMAnimation.IsLinear : Boolean;
var I, J : Integer;
begin
  If Count = 0 then
    Exit(false);
  Result := true;
  J := fSteps[0];
  for I := 1 to Count - 1 do
    If fSteps[I] <> J + I then
      Exit(false);
end;


function Anim(aX, aY : Integer; aAnimation : TKMWordArray; aOffset : Byte = 0) : TKMAnimation;
begin
  Result.Create(aX, aY, aAnimation, aOffset);
end;

function Anim(aX, aY, StepStart, aCount : Integer; aOffset : Byte = 0; doBackWard : Boolean = false) : TKMAnimation;
begin
  Result.Create(aX, aY, StepStart, aCount, aOffset, doBackWard);
end;

function Anim(aAnimation : TKMAnimLoop) : TKMAnimation;
begin
  Result.Create(aAnimation);
end;

function Anim(const aAnimation : TKMAnimation) : TKMAnimation;
begin
  Result.Create(aAnimation.X, aAnimation.Y, aAnimation.fSteps);
end;

function Anim(aName : String) : TKMAnimation;
begin
  Result.Create(aName);
end;

procedure TKMSettingsWeather.SetDefault;
begin
  Overwrite := false;
  Enabled := false;
  MaxCount := 10;
  MaxSpawnCount := 4;
  MinInterval := 100;
  MaxInterval := 600;
  MaxLifeTime := 600;
  MaxCloudSpeed := 0.05;
  DecParticles := 0;
  NightSpeed := 10;
  NightTime := 7;
  DynamicLight := true;
end;

procedure TKMSettingsWeather.SetRealism;
begin
  Overwrite := false;
  Enabled := true;
  MaxCount := 10;
  MaxSpawnCount := 4;
  MinInterval := 100;
  MaxInterval := 1200;
  MaxLifeTime := 1200;
  MaxCloudSpeed := 0.05;
  DecParticles := 5;
  NightSpeed := 10;
  NightTime := 7;
  DynamicLight := true;
end;

function TKMSettingsWeather.Copy: TKMSettingsWeather;
begin
  Result.Overwrite := Overwrite;
  Result.Enabled := Enabled;
  Result.MaxCount := MaxCount;
  Result.MaxSpawnCount := MaxSpawnCount;
  Result.MinInterval := MinInterval;
  Result.MaxInterval := MaxInterval;
  Result.MaxLifeTime := MaxLifeTime;
  Result.MaxCloudSpeed := MaxCloudSpeed;
  Result.DecParticles := DecParticles;
  Result.NightSpeed := NightSpeed;
  Result.NightTime := NightTime;
  Result.DynamicLight := DynamicLight;
end;

end.
