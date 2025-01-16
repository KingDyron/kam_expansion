unit KM_ControlsTrackBar;
{$I KaM_Remake.inc}
interface
uses
  Classes, Vcl.Controls,
  KM_Controls,
  KM_ResFonts,
  KM_Points;

type
  TKMTrackBar = class(TKMControl)
  private
    fTrackTop: Byte; //Offset trackbar from top (if Caption <> '')
    fTrackHeight: Byte; //Trackbar height
    fMinValue: Word;
    fMaxValue: Word;
    fOnChange: TNotifyEvent;
    fCaption: UnicodeString;
    fPosition: Word;
    fFont: TKMFont;
    fRange: TKMRangeInt;
    fThumbText: UnicodeString;
    fAutoThumbWidth: Boolean;
    fFixedThumbWidth: Boolean;
    procedure SetCaption(const aValue: UnicodeString);
    procedure SetPosition(aValue: Word);
    procedure SetRange(const aRange: TKMRangeInt);
    procedure UpdateThumbWidth;
    procedure SetThumbText(const Value: UnicodeString);
    procedure SetAutoThumbWidth(const aValue: Boolean);
    procedure SetMaxValue(aValue : Word);
    procedure SetMinValue(aValue : Word);
  protected
    function DoHandleMouseWheelByDefault: Boolean; override;
  public
    Step: Byte; //Change Position by this amount each time
    ThumbWidth: Word;
    CaptionWidth: Integer;
    SliderFont: TKMFont;

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aMin, aMax: Word);

    property Caption: UnicodeString read fCaption write SetCaption;
    property Position: Word read fPosition write SetPosition;
    property Range: TKMRangeInt read fRange write SetRange;
    property Font: TKMFont read fFont write fFont;
    property MinValue: Word read fMinValue write SetMinValue;
    property MaxValue: Word read fMaxValue write SetMaxValue;
    property ThumbText: UnicodeString read fThumbText write SetThumbText;
    property AutoThumbWidth: Boolean read fAutoThumbWidth write SetAutoThumbWidth;
    property FixedThumbWidth: Boolean read fAutoThumbWidth write fFixedThumbWidth;
    procedure ResetRange;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean); override;
    procedure Paint; override;
  const
    THUMB_WIDTH_ADD = 24;
  end;


implementation
uses
  SysUtils, Math,
  KromUtils, KromOGLUtils,
  KM_RenderUI,
  KM_Resource, KM_ResTypes;


{ TKMTrackBar }
constructor TKMTrackBar.Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aMin, aMax: Word);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,0);
  Assert(aMax > aMin, 'TKMTrackBar''s minValue >= maxValue');
  fMinValue := aMin;
  fMaxValue := aMax;
  fTrackHeight := 20;
  fRange := KMRange(aMin, aMax); //set Range before position
  Position := (fMinValue + fMaxValue) div 2;
  Caption := '';
  Font := fntMetal;
  SliderFont := fntMetal;
  CaptionWidth := -1;

  Step := 1;
  fAutoThumbWidth := False;
  fFixedThumbWidth := False;

  UpdateThumbWidth;
end;


procedure TKMTrackBar.UpdateThumbWidth;
begin
  if fFixedThumbWidth then Exit;

  if AutoThumbWidth then
    ThumbWidth := Max(gRes.Fonts[SliderFont].GetTextSize(IntToStr(MaxValue)).X,
                      gRes.Fonts[SliderFont].GetTextSize(ThumbText).X)
  else
    ThumbWidth := gRes.Fonts[SliderFont].GetTextSize(IntToStr(MaxValue)).X;

  ThumbWidth := ThumbWidth + THUMB_WIDTH_ADD;
end;


procedure TKMTrackBar.SetAutoThumbWidth(const aValue: Boolean);
begin
  fAutoThumbWidth := aValue;
  UpdateThumbWidth;
end;

procedure TKMTrackBar.SetMaxValue(aValue : Word);
var newPos : Word;
begin
  newPos := EnsureRange(Position, MinValue, aValue);
  fMaxValue := aValue;
  ResetRange;//set range before position
  Position := newPos;
  UpdateThumbWidth;
end;

procedure TKMTrackBar.SetMinValue(aValue : Word);
var newPos : Word;
begin
  newPos := EnsureRange(Position, aValue, MaxValue);
  fMinValue := aValue;
  ResetRange;//set range before position
  Position := newPos;
  UpdateThumbWidth;
end;


procedure TKMTrackBar.SetCaption(const aValue: UnicodeString);
begin
  fCaption := aValue;

  if Trim(fCaption) <> '' then
  begin
    SetHeightSilently(20 + fTrackHeight);
    fTrackTop := 20;
  end
  else
  begin
    SetHeightSilently(fTrackHeight);
    fTrackTop := 0;
  end;
end;


procedure TKMTrackBar.SetPosition(aValue: Word);
begin
  fPosition := KMEnsureRange(aValue, Range);
  ThumbText := IntToStr(fPosition);
end;


procedure TKMTrackBar.SetRange(const aRange: TKMRangeInt);
begin
  fRange.Min := EnsureRange(aRange.Min, MinValue, MaxValue);
  fRange.Max := EnsureRange(aRange.Max, MinValue, MaxValue);
  Position := fPosition; //Update position due to range change
end;


procedure TKMTrackBar.SetThumbText(const Value: UnicodeString);
begin
  fThumbText := Value;
  UpdateThumbWidth;
end;


procedure TKMTrackBar.ResetRange;
begin
  fRange.Min := MinValue;
  fRange.Max := MaxValue;
end;


procedure TKMTrackBar.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X, Y, Shift);
end;


procedure TKMTrackBar.MouseMove(X,Y: Integer; Shift: TShiftState);
var
  newPos: Integer;
  posX, stepX: Single;
begin
  inherited;

  newPos := Position;
  if (ssLeft in Shift)
    and InRange(Y - AbsTop - fTrackTop, 0, fTrackHeight) then
  begin
    posX := 0;
    if AutoThumbWidth then
    begin
      stepX := (Width - ThumbWidth) / (fMaxValue - fMinValue) / Step; // width between marks
      posX := stepX * (Position / Step) + (ThumbWidth div 2); // actual marks positions on track
    end;
    if not AutoThumbWidth
      or not InRange(X - AbsLeft, posX - ThumbWidth div 2, posX + ThumbWidth div 2) then // do not update pos, if we hover over thumb
      newPos := EnsureRange(fMinValue + Round(((X-AbsLeft-ThumbWidth div 2) / (Width - ThumbWidth - 4))*(fMaxValue - fMinValue)/Step)*Step, fMinValue, fMaxValue);
  end;

  if newPos <> Position then
  begin
    Position := newPos;

    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


function TKMTrackBar.DoHandleMouseWheelByDefault: Boolean;
begin
  Result := False;
end;


procedure TKMTrackBar.MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
var
  newPos: Integer;
begin
  inherited;

  if aHandled or (WheelSteps = 0) then Exit;

  aHandled := WheelSteps <> 0;

  Focus;

  newPos := Position;

  if WheelSteps <> 0 then
    newPos := EnsureRange(newPos - Step*fMouseWheelStep*WheelSteps, fMinValue, fMaxValue);

  if newPos <> Position then
  begin
    Position := newPos;

    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


procedure TKMTrackBar.Paint;
const
  TEXT_COLOR: array [Boolean] of TColor4 = ($FF888888, $FFFFFFFF);
var
  thumbPos, thumbHeight, rangeMinPos, rangeMaxPos: Word;
  capWidth: Integer;
begin
  inherited;

  if fCaption <> '' then
  begin
    if CaptionWidth = -1 then
      capWidth := Width
    else
      capWidth := CaptionWidth;

    TKMRenderUI.WriteText(AbsLeft, AbsTop, capWidth, fCaption, fFont, taLeft, TEXT_COLOR[Enabled]);
  end;

  rangeMinPos := Round(Width*(fRange.Min-fMinValue) / (fMaxValue - fMinValue));
  rangeMaxPos := Round(Width*(fRange.Max-fMinValue) / (fMaxValue - fMinValue));

  TKMRenderUI.WriteBevel(AbsLeft,               AbsTop+fTrackTop+1, rangeMinPos,               fTrackHeight-2, 0.3, 0.4);
  TKMRenderUI.WriteBevel(AbsLeft + rangeMinPos, AbsTop+fTrackTop+2, rangeMaxPos - rangeMinPos, fTrackHeight-4);
  TKMRenderUI.WriteBevel(AbsLeft + rangeMaxPos, AbsTop+fTrackTop+1, Width - rangeMaxPos,       fTrackHeight-2, 0, 0.3);

  thumbPos := Round(Mix (0, Width - ThumbWidth, 1-(Position-fMinValue) / (fMaxValue - fMinValue)));
  thumbHeight := gRes.Sprites[rxGui].RXData.Size[132].Y;

  TKMRenderUI.WritePicture(AbsLeft + thumbPos, AbsTop+fTrackTop, ThumbWidth, thumbHeight, [anLeft,anRight], rxGui, 132);
  TKMRenderUI.WriteText(AbsLeft + thumbPos + ThumbWidth div 2, AbsTop+fTrackTop+3, 0, ThumbText, SliderFont, taCenter, TEXT_COLOR[Enabled]);
end;


end.

