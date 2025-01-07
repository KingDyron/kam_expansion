unit KM_DevPerfLogSingle;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, StrUtils, SysUtils, KM_CommonTypes;


type
  // Log how much time each section takes and write results to a file or HUD
  // It is simpler to keep CPU/GPU separate, so we can enable/show them individually
  TKMPerfLogClass = class of TKMPerfLogSingle;
  TKMPerfLogSingle = class
  private
    fCount: Integer;
    fTimes: array of record
      Time: Int64;
      Tag: Integer;
      TagS: string;
      Count: Integer;
    end;
    function GetCount: Integer;
    function GetEnterTick: Integer;
  protected
    fEnterTick: Integer;
    fEnterTag: Integer;
    procedure DoSectionEnter; virtual; abstract;
    procedure DoSectionLeave; virtual; abstract;
  public
    Enabled: Boolean;
    Display: Boolean;
    Color: TKMColor4f;
    constructor Create; virtual; // Needs to be virtual (see more info inside)
    property Count: Integer read GetCount;
    property EnterTick: Integer read GetEnterTick;
    procedure SectionEnter(aTick: Integer = -1; aTag: Integer = 0);
    procedure SectionLeave;
    procedure SectionAddValue(aValue: Int64; aTick: Integer = -1; aTagS: string = '');
    procedure Clear;
    procedure Render(aLeft, aWidth, aHeight, aScaleY: Integer; aEmaAlpha: Single; aScale: Integer; aSmoothing: Boolean);
    procedure SaveToFile(const aFilename: string; aSaveThreshold: Integer);
    procedure SaveToStringList(aStringList: TStringList; aSaveThreshold: Integer);
  end;

  // CPU logging
  TKMPerfLogSingleCPU = class(TKMPerfLogSingle)
  private
    fEnterTime: Int64;
  protected
    procedure DoSectionEnter; override;
    procedure DoSectionLeave; override;
  public
    HighPrecision: Boolean;
  end;

  // GPU logging
  // Async through gRenderLow.Queries
  // Largely replaced with TKMPerfLogGFXStack
  // Do not delete it, we might need it for non-frame dependant things (e.g. measure GPU cost of Buffers updates)
  TKMPerfLogSingleGFX = class(TKMPerfLogSingle)
  private
    fGPUQueryId: Integer;
  protected
    procedure DoSectionEnter; override;
    procedure DoSectionLeave; override;
  public
    constructor Create; override; // Needs to be overriden (see TKMPerfLogSingle.Create for info)
    destructor Destroy; override;
  end;


implementation
uses
  KM_CommonUtils, KM_Points,
  KM_Resource, KM_ResFonts,
  KM_RenderAux, KM_Render, KM_RenderUI;


{ TKMPerfLogSingle }
constructor TKMPerfLogSingle.Create;
begin
  inherited;

  // Need a virtual constructor, so child classes could override it, so their constructors get called
  // when referenced as ClassName: TKMPerfLogClass; ClassName.Create
  // Otherwise, only base TObject class constructor gets called, yet objects are instances of ClassName
end;


function TKMPerfLogSingle.GetCount: Integer;
begin
  if Self = nil then Exit(0);

  Result := fCount;
end;


function TKMPerfLogSingle.GetEnterTick: Integer;
begin
  if Self = nil then Exit(0);

  Result := fEnterTick;
end;


// aTick - Tick to which the result gets appended (for example multiple FOW calls can be stacked within the same tick)
// aTag - mark the Tick (e.g. render frame can be marked with a game tick it displayed)
procedure TKMPerfLogSingle.SectionEnter(aTick: Integer = -1; aTag: Integer = 0);
begin
  // This easy check allows us to exit if the Log was not initialized, e.g. in untils
  if Self = nil then Exit;
  if not Enabled then Exit;

  fEnterTick := aTick;
  fEnterTag := aTag;

  DoSectionEnter;
end;


procedure TKMPerfLogSingle.SectionLeave;
begin
  // This easy check allows us to exit if the Log was not initialized, e.g. in untils
  if Self = nil then Exit;
  if not Enabled then Exit;

  DoSectionLeave;
end;


procedure TKMPerfLogSingle.SectionAddValue(aValue: Int64; aTick: Integer = -1; aTagS: string = '');
begin
  if Self = nil then Exit;
  if not Enabled then Exit;

  fEnterTick := aTick;
  fEnterTag := 0;

  if aTick = -1 then
    Inc(fCount)
  else
    fCount := aTick;

  if fCount-1 >= Length(fTimes) then
    SetLength(fTimes, fCount + 1024);

  fTimes[fCount-1].Tag := 0;
  fTimes[fCount-1].TagS := aTagS;
  if fEnterTick = -1 then
    fTimes[fCount-1].Time := aValue
  else
    fTimes[fCount-1].Time := fTimes[fCount-1].Time + aValue;
end;


procedure TKMPerfLogSingle.Clear;
begin
  if Self = nil then Exit;

  fCount := 0;
  SetLength(fTimes, 0);
end;


procedure TKMPerfLogSingle.Render(aLeft, aWidth, aHeight, aScaleY: Integer; aEmaAlpha: Single; aScale: Integer; aSmoothing: Boolean);
const
  TAG_POS = 10;
  TAG_PAD = 2;
  TAG_FONT = fntMonospaced;
var
  I, K: Integer;
  vaChart: TKMPointFArray;
  cCount: Integer;
  accum: Single;
  tagStr: string;
  txtSize: TKMPoint;
begin
  if Self = nil then Exit;
  if not Display then Exit;

  cCount := Min(fCount, aWidth);
  SetLength(vaChart, cCount);

  accum := aHeight;

  for I := cCount - 1 downto 0 do
  begin
    // Instant reading
    K := fCount - 1 - I;
    vaChart[I] := TKMPointF.New(aLeft + I + 0.5, aHeight + 0.5 - fTimes[K].Time / 1000 / aScale * aScaleY);

    if aSmoothing then
    begin
      // Exponential Moving Average
      accum := aEmaAlpha * vaChart[I].Y + (1 - aEmaAlpha) * accum;
      vaChart[I].Y := accum;
    end;
  end;

  // Chart
  gRenderAux.Line(vaChart, Color);

  I := TAG_POS;
  K := fCount - 1 - I;

  // Tags
  if InRange(K, Low(fTimes), High(fTimes)) then
  begin
    tagStr := '';
    if fTimes[K].TagS <> '' then
      tagStr := fTimes[K].TagS
    else
    if (fTimes[K].Tag <> 0) then
      tagStr := IntToStr(fTimes[K].Tag);

    if (tagStr <> '') then
    begin
      txtSize := gRes.Fonts[TAG_FONT].GetTextSize(tagStr);
      gRenderAux.Square(KMRect(Round(vaChart[I].X) - (txtSize.X div 2) - TAG_PAD,
                               Round(vaChart[I].Y) - (txtSize.Y div 2) - TAG_PAD,
                               Round(vaChart[I].X) + (txtSize.X div 2) + TAG_PAD,
                               Round(vaChart[I].Y) + (txtSize.Y div 2) + TAG_PAD), Color);
      TKMRenderUI.WriteText(Round(vaChart[I].X) - txtSize.X div 2, Round(vaChart[I].Y) - 7, txtSize.X, tagStr, TAG_FONT, taLeft);
    end;
  end;
end;


// Save to file for standalone version running without TKMPerfLogs
procedure TKMPerfLogSingle.SaveToFile(const aFilename: string; aSaveThreshold: Integer);
var
  sl: TStringList;
begin
  if Self = nil then Exit;

  sl := TStringList.Create;
  SaveToStringList(sl, aSaveThreshold);
  sl.SaveToFile(aFilename);
  sl.Free;
end;


procedure TKMPerfLogSingle.SaveToStringList(aStringList: TStringList; aSaveThreshold: Integer);
var
  I: Integer;
  total: Int64;
begin
  if Self = nil then Exit;

  total := 0;

  // Times (when Disabled Count is 0)
  for I := 0 to fCount - 1 do
  begin
    Inc(total, fTimes[I].Time);

    if fTimes[I].Time >= aSaveThreshold then
      aStringList.Append(Format('Tick: %d'#9'%d'#9'%d', [I, fTimes[I].Count, fTimes[I].Time]));
  end;

  aStringList.Append('Total ' + FloatToStr(total/1000) + 'msec');
end;


{ TKMPerfLogSingleCPU }
procedure TKMPerfLogSingleCPU.DoSectionEnter;
begin
  if HighPrecision then
    fEnterTime := TimeGetUsec
  else
    fEnterTime := TimeGet;
end;


procedure TKMPerfLogSingleCPU.DoSectionLeave;
var
  tgt: Integer;
  T: Int64;
begin
  if HighPrecision then
    T := TimeSinceUSec(fEnterTime)
  else
    T := TimeSince(fEnterTime) * 1000;

  if fEnterTick = -1 then
    tgt := fCount + 1
  else
    tgt := fEnterTick;

  if tgt >= Length(fTimes) then
    SetLength(fTimes, tgt + 1024);

  fTimes[tgt].Tag := fEnterTag;
  if fEnterTick = -1 then
    fTimes[tgt].Time := T
  else
  begin
    fTimes[tgt].Time := fTimes[tgt].Time + T;
    Inc(fTimes[tgt].Count);
  end;

  fCount := Max(fCount, tgt);
end;


{ TKMPerfLogSingleGFX }
constructor TKMPerfLogSingleGFX.Create;
begin
  inherited;

  fGPUQueryId := -1;
end;


destructor TKMPerfLogSingleGFX.Destroy;
begin
  if (gRender <> nil) and (fGPUQueryId <> -1) then
    gRender.Query.QueriesDelete(fGPUQueryId);

  inherited;
end;


procedure TKMPerfLogSingleGFX.DoSectionEnter;
begin
  if fGPUQueryId = -1 then
    fGPUQueryId := gRender.Query.QueriesGen;

  gRender.Query.QueriesBegin(fGPUQueryId);
end;


procedure TKMPerfLogSingleGFX.DoSectionLeave;
var
  T: Int64;
begin
  gRender.Query.QueriesEnd(fGPUQueryId);

  // Get us time from previous frame
  T := gRender.Query.QueriesTime(fGPUQueryId);
  T := Round(T / 1000);

  if fEnterTick = -1 then
    Inc(fCount)
  else
    fCount := fEnterTick;

  if fCount-1 >= Length(fTimes) then
    SetLength(fTimes, fCount + 1024);

  fTimes[fCount-1].Tag := fEnterTag;
  if fEnterTick = -1 then
    fTimes[fCount-1].Time := T
  else
    fTimes[fCount-1].Time := fTimes[fCount-1].Time + T;
end;


end.
