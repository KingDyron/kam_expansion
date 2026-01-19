unit KM_ControlsBase;
{$I KaM_Remake.inc}
interface
uses
  Classes, Vcl.Controls,
  KromOGLUtils,
  KM_Controls,
  KM_RenderUI,
  KM_ResFonts, KM_ResTypes,
  KM_CommonTypes, KM_Points;


type
  // Beveled area
  TKMBevel = class(TKMControl)
  const
    DEF_BACK_ALPHA = 0.4;
    DEF_EDGE_ALPHA = 0.75;
  public
    BackAlpha: Single;
    EdgeAlpha: Single;
    Color: TKMColor3f;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer); overload;
    constructor Create(aParent: TKMPanel; aSizeOf : TKMControl); overload;

    procedure SetDefBackAlpha;
    procedure SetDefEdgeAlpha;
    procedure SetDefColor;

    procedure Paint; override;
  end;


  // Rectangle area
  TKMShape = class(TKMControl)
  public
    FillColor: TColor4;
    LineColor: TColor4; //color of outline
    LineWidth: Byte;
  public
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
    procedure Paint; override;
  end;


  // Text Label
  TKMLabel = class(TKMControl)
  private
    fWordWrap: Boolean;
    fFont: TKMFont;
    fFontColor: TColor4; //Usually white (self-colored)
    fCaption: UnicodeString; //Original text
    fText: UnicodeString; //Reformatted text
    fTextAlign: TKMTextAlign;
    fTextVAlign: TKMTextVAlign;
    fTextSize: TKMPoint;
    fStrikethrough: Boolean;
    fTabWidth: Integer;
    procedure SetWordWrap(aValue: Boolean);
    procedure ReformatText;
    procedure SetFont(const Value: TKMFont);
  protected
    procedure SetCaption(const aCaption: UnicodeString); virtual;
    procedure SetWidth(aValue: Integer); override;

    function GetIsPainted: Boolean; override;
  public
    MaxLines: Integer;

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                       aFont: TKMFont; aTextAlign: TKMTextAlign); overload;
    constructor Create(aParent: TKMPanel; aLeft,aTop: Integer; const aCaption: UnicodeString; aFont: TKMFont;
                       aTextAlign: TKMTextAlign); overload;

    function HitTest(X, Y: Integer; aIncludeDisabled: Boolean = False; aIncludeNotHitable: Boolean = False): Boolean; override;
    procedure SetColor(aColor: Cardinal);
    property WordWrap: Boolean read fWordWrap write SetWordWrap;  //Whether to automatically wrap text within given text area width
    property Caption: UnicodeString read fCaption write SetCaption;
    property WordWrapedText : UnicodeString read fText;
    property FontColor: TColor4 read fFontColor write fFontColor;
    property Strikethrough: Boolean read fStrikethrough write fStrikethrough;
    property TabWidth: Integer read fTabWidth write fTabWidth;
    property TextSize: TKMPoint read fTextSize;
    function TextLeft: Integer;
    property TextVAlign: TKMTextVAlign read fTextVAlign write fTextVAlign;
    property TextHAlign: TKMTextAlign read fTextAlign write fTextAlign;
    property Font: TKMFont read fFont write SetFont;
    procedure Paint; override;
  end;

  TKMOverlayLabel = class(TKMLabel)
    protected
      procedure SetCaption(const aCaption: UnicodeString); override;
    public
      FromBottom,
      AddBevel : Boolean;

      procedure Paint;override;
  end;

  TKMHintLabel = class(TKMLabel)
    protected
    procedure SetCaption(const aCaption: UnicodeString); override;
    public
      CursorY : Integer;
      procedure Paint; override;
  end;

  // Label that is scrolled within an area. Used in Credits
  TKMLabelScroll = class(TKMLabel)
  public
    SmoothScrollToTop: Cardinal; //Delta between this and TimeGetTime affects vertical position
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aTextAlign: TKMTextAlign);
    procedure Paint; override;
  end;


  // Image
  TKMImage = class(TKMControl)
  private
    fRX: TRXType;
    fTexID: Word;
    fFlagColor: TColor4;
  protected
    function GetIsPainted: Boolean; override;
  public
    ImageAnchors: TKMAnchorsSet;
    Highlight: Boolean;
    HighlightOnMouseOver: Boolean;
    HighlightCoef: Single;
    Lightness: Single;
    ClipToBounds: Boolean;
    Tiled: Boolean;
    AlphaStep : Single;
    TexID2 : Word;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui;
                       aImageAnchors: TKMAnchorsSet = [anLeft, anTop]);
    property RX: TRXType read fRX write fRX;
    property TexID: Word read fTexID write fTexID;
    property FlagColor: TColor4 read fFlagColor write fFlagColor;
    function Click: Boolean;
    procedure ImageStretch;
    procedure ImageCenter;
    procedure Paint; override;
  end;

  TKMGuiImage = class(TKMImage)
    protected
      function GetRX : TRXType;virtual;
      function GetTexID : Word;virtual;
    public
      constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer;
                         aImageAnchors: TKMAnchorsSet = [anLeft, anTop]);
      procedure Paint; override;
  end;

  TKMImageBackGround = class(TKMGuiImage)
    protected
      function GetTexID : Word;override;
  end;

  TKMImageMain = class(TKMGuiImage)
    protected
      function GetTexID : Word;override;
  end;

  TKMImageSideBar = class(TKMGuiImage)
    protected
      function GetRX : TRXType; override;
      function GetTexID : Word;override;
  end;

  TKMImageMinimap = class(TKMGuiImage)
    protected
      function GetRX : TRXType;override;
      function GetTexID : Word;override;
  end;

  TKMImageMainLeft = class(TKMGuiImage)
    protected
      function GetTexID : Word;override;
  end;

  TKMImageMainRight = class(TKMGuiImage)
    protected
      function GetTexID : Word;override;
  end;

  TKMImageMainText = class(TKMGuiImage)
    protected
      function GetTexID : Word;override;
  end;



  // Image stack - for army formation view
  TKMImageStack = class(TKMControl)
  private
    fRX: TRXType;
    fTexID1, fTexID2: Word; //Normal and commander
    fCount: Integer;
    fColumns: Integer;
    fDrawWidth: Integer;
    fDrawHeight: Integer;
    fHighlightID: Integer;
  public
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID1, aTexID2: Word; aRX: TRXType = rxGui);
    procedure SetCount(aCount, aColumns, aHighlightID: Word);
    procedure Paint; override;
  end;

  TKMImageAnimation = class(TKMImage)
  public
    AnimStep: Integer;
    Animation : TKMAnimLoop;
    AddBevel,
    StopAnim : Boolean;
    procedure Paint; override;
    Procedure UpdateState(aTickCount: Cardinal); override;
  end;

  // 3D Button
  TKMButton = class(TKMControl)
  private
    fCaption: UnicodeString;
    fTextAlign: TKMTextAlign;
    fStyle: TKMButtonStyle;
    fRX: TRXType;
    fAutoHeight: Boolean; //Set button height automatically depending text size (height)
    procedure InitCommon(aStyle: TKMButtonStyle);
    procedure SetCaption(const aCaption: UnicodeString);
    procedure SetAutoHeight(aValue: Boolean);
    procedure UpdateHeight;
  public
    FlagColor: TColor4; //When using an image
    CapColor: TColor4;
    Font: TKMFont;
    MakesSound: Boolean;
    TexID: Word;
    CapOffsetX: Shortint;
    CapOffsetY: Shortint;
    ShowImageEnabled: Boolean; // show picture as enabled or not (normal or darkened)
    TextVAlign: TKMTextVAlign;
    AutoTextPadding: Byte;      //text padding for autoHeight
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType;
                       aStyle: TKMButtonStyle); overload;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                       aStyle: TKMButtonStyle); overload;
    function Click: Boolean; //Try to click a button and return TRUE if succeded

    property Caption: UnicodeString read fCaption write SetCaption;
    property AutoHeight: Boolean read fAutoHeight write SetAutoHeight;
    property RX: TRXType read fRX write fRX;

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  // Common Flat Button
  TKMButtonFlatCommon = class abstract(TKMControl)
  protected
    fCaption : UnicodeString;
    procedure SetCaption(aValue : String); virtual;
  public
    RX: TRXType;
    TexID: Word;
    TexOffsetX: Shortint;
    TexOffsetY: Shortint;
    CapOffsetX: SmallInt;
    CapOffsetY: SmallInt;
    //Caption: UnicodeString;
    CapColor: TColor4;
    FlagColor: TColor4;
    Font: TKMFont;
    HideHighlight: Boolean;
    Clickable: Boolean; //Disables clicking without dimming
    EdgeAlpha: Single;
    BackAlpha: Single;
    HighLightColor : TColor4;
    BackBevelColor : TColor4;

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui); virtual;

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    property Caption : UnicodeString read fCaption write SetCaption;

    procedure Paint; override;
  end;


  // FlatButton
  TKMButtonFlat = class(TKMButtonFlatCommon)
  public
    Down: Boolean;
    LineWidth : Byte;
    DownColor : TColor4;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui); override;

    procedure Paint; override;
  end;

  // FlatButtonFitImage
  TKMButtonFlatFit = class(TKMButtonFlatCommon)
  private
    fImgWidth, fImgHeight : Integer;
  public
    Down: Boolean;
    LineWidth : Byte;
    DownColor : TColor4;
    DoFitImage : Boolean;
    ImgAnchors : TKMAnchorsSet;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui); override;
    procedure SetNewTexID(aID : Word);

    procedure Paint; override;
  end;

  // FlatButton
  TKMButtonFlatBlock = class(TKMButtonFlat)
  private
    fBlock : Integer;
  public
    Tag3 : Integer;
    procedure SetBlock(aValue : Integer);
    property Block : Integer read fBlock write SetBlock;
    procedure Paint; override;
  end;

  // FlatButton
  TKMButtonFlatStack = class(TKMButtonFlat)
  public
    TextIds : TKMWordArray;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexIDs: TKMWordArray; aRX: TRXType = rxGui); reintroduce;
    procedure Paint; override;
  end;

  TKMButtonFlatPin = class(TKMButtonFlat)
  public
    procedure Paint; override;
  end;

  // FlatButton with Shape on it
  TKMFlatButtonShape = class(TKMControl)
  private
    fCaption: UnicodeString;
    fFont: TKMFont;
    fFontHeight: Byte;
  public
    Down: Boolean;
    FontColor: TColor4;
    ShapeColor: TColor4;

    LineWidth : Byte;
    DownColor : TColor4;
    property Caption : UnicodeString read fCaption write fCaption;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aShapeColor: TColor4);
    procedure Paint; override;
  end;


implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, Math,
  KM_ControlsTypes,
  KM_Sound,
  KM_Resource, KM_ResSound, KM_ResSprites,
  KM_Defaults, KM_CommonUtils,
  KM_RenderTypes;


{ TKMBevel }
constructor TKMBevel.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  SetDefBackAlpha;
  SetDefEdgeAlpha;
end;

constructor TKMBevel.Create(aParent: TKMPanel; aSizeOf: TKMControl);
begin
  Inherited Create(aParent, aSizeOf.Left, aSizeOf.Top, aSizeOf.Width, aSizeOf.Height);
  SetDefBackAlpha;
  SetDefEdgeAlpha;
end;


procedure TKMBevel.SetDefBackAlpha;
begin
  BackAlpha := DEF_BACK_ALPHA; //Default value
end;


procedure TKMBevel.SetDefEdgeAlpha;
begin
  EdgeAlpha := DEF_EDGE_ALPHA; //Default value
end;


procedure TKMBevel.SetDefColor;
begin
  Color := COLOR3F_BLACK; //Default value
end;


procedure TKMBevel.Paint;
begin
  inherited;
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, Color, EdgeAlpha, BackAlpha);
end;


{ TKMShape }
constructor TKMShape.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  LineWidth := 2;
end;


procedure TKMShape.Paint;
begin
  inherited;
  TKMRenderUI.WriteShape(AbsLeft, AbsTop, Width, Height, FillColor);
  TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, LineColor);
end;


{ TKMLabel }
constructor TKMLabel.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                            aFont: TKMFont; aTextAlign: TKMTextAlign);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fFont := aFont;
  fFontColor := $FFFFFFFF;
  fTextAlign := aTextAlign;
  fTextVAlign := tvaTop;
  fWordWrap := False;
  fTabWidth := FONT_TAB_WIDTH;
  SetCaption(aCaption);
end;


//Same as above but with width/height ommitted, as in most cases we don't know/don't care
constructor TKMLabel.Create(aParent: TKMPanel; aLeft, aTop: Integer; const aCaption: UnicodeString; aFont: TKMFont;
                            aTextAlign: TKMTextAlign);
begin
  Create(aParent, aLeft, aTop, 0, 0, aCaption, aFont, aTextAlign);
end;


function TKMLabel.TextLeft: Integer;
begin
  case fTextAlign of
    taLeft:   Result := AbsLeft;
    taCenter: Result := AbsLeft + Round((Width - fTextSize.X) / 2);
    taRight:  Result := AbsLeft + (Width - fTextSize.X);
    else      Result := AbsLeft;
  end;
end;


procedure TKMLabel.SetCaption(const aCaption: UnicodeString);
begin
  fCaption := aCaption;
  ReformatText;
end;


procedure TKMLabel.SetWordWrap(aValue: Boolean);
begin
  fWordWrap := aValue;
  ReformatText;
end;


//Override usual hittest with regard to text alignment
function TKMLabel.HitTest(X, Y: Integer; aIncludeDisabled: Boolean = False; aIncludeNotHitable: Boolean = False): Boolean;
begin
  Result := (Hitable or aIncludeNotHitable)
            and InRange(X, TextLeft, TextLeft + fTextSize.X)
            and InRange(Y, AbsTop, AbsTop + Height);
end;


procedure TKMLabel.SetColor(aColor: Cardinal);
begin
  fCaption := StripColor(fCaption);
  Caption := WrapColor(fCaption, aColor);
end;

procedure TKMLabel.SetFont(const Value: TKMFont);
begin
  fFont := Value;
  ReformatText;
end;


// Existing EOLs should be preserved, and new ones added where needed
// Keep original intact incase we need to Reformat text once again
procedure TKMLabel.ReformatText;
var
  I: Integer;
  stake: Integer;
begin
  if fWordWrap then
    fText := gRes.Fonts[fFont].WordWrap(fCaption, Width, True, False)
  else
    fText := fCaption;

  // We may need to display only N lines
  if MaxLines > 0 then
  begin
    stake := 0;
    for I := 1 to MaxLines do
    begin
      stake := PosEx(#124, fText, stake+1);
      if stake = 0 then
        Break;
    end;

    if stake > 0 then
      fText := LeftStr(fText, stake - 1);
  end;

  fTextSize := gRes.Fonts[fFont].GetTextSize(fText);
end;


procedure TKMLabel.SetWidth(aValue: Integer);
begin
  inherited;

  if fWordWrap then
    ReformatText;
end;


function TKMLabel.GetIsPainted: Boolean;
begin
  Result := inherited and (Length(fCaption) > 0 );
end;


// Send caption to render
procedure TKMLabel.Paint;
var
  t: Integer;
  col: Cardinal;
begin
  inherited;

  if Enabled then
    col := FontColor
  else
    col := $FF888888;

  t := 0;
  if Height > 0 then
  begin
    case fTextVAlign of
      tvaNone,
      tvaTop:     ;
      tvaMiddle:  t := (Height - fTextSize.Y) div 2;
      tvaBottom:  t := Height - fTextSize.Y;
    end;
  end;

  TKMRenderUI.WriteText(AbsLeft, AbsTop + t, Width, fText, fFont, fTextAlign, col, False, False, False, false, fTabWidth);

  if fStrikethrough then
    TKMRenderUI.WriteShape(TextLeft, AbsTop + fTextSize.Y div 2 - 2, fTextSize.X, 3, col, $FF000000);
end;

procedure TKMOverlayLabel.SetCaption(const aCaption: UnicodeString);
//var tmp : Integer;
begin
  Inherited;
  //Height := gRes.Fonts[fFont].GetTextSize(fText, tmp).Y + 10;
end;
// Send caption to render
procedure TKMOverlayLabel.Paint;
var
  t: Integer;
  col: Cardinal;
begin
  inherited;

  if Enabled then
    col := FontColor
  else
    col := $FF888888;

  t := 0;
  if Height > 0 then
  begin
    case fTextVAlign of
      tvaNone,
      tvaTop:     ;
      tvaMiddle:  t := (Height - fTextSize.Y) div 2;
      tvaBottom:  t := Height - fTextSize.Y;
    end;
  end;
  If AddBevel then
    TKMRenderUI.WriteBevel(AbsLeft - 3, AbsTop - 3, Width + 6, fTextSize.Y + 6);

  TKMRenderUI.WriteText(AbsLeft, AbsTop + t, Width, fText, fFont, fTextAlign, col, False, False, False, false, fTabWidth);

  if fStrikethrough then
    TKMRenderUI.WriteShape(TextLeft, AbsTop + fTextSize.Y div 2 - 2, fTextSize.X, 3, col, $FF000000);
end;


procedure TKMHintLabel.SetCaption(const aCaption: UnicodeString);
var tmp : Integer;
begin
  Inherited;
  Height := gRes.Fonts[fFont].GetTextSize(fText, tmp).Y + 10;
end;
// Send caption to render
procedure TKMHintLabel.Paint;
var
  tmp, I: Integer;
  col: Cardinal;
  bevelSize : TKMPoint;
  R: TRXData;
begin
  if not Visible then
    Exit;
  bevelSize := gRes.Fonts[fFont].GetTextSize(fText, tmp);
  IF bevelSize.X <= 10 then
    Exit;
  if Enabled then
    col := FontColor
  else
    col := $FF888888;
  R := gRes.Sprites[rxGui].RXData;
  tmp := (bevelSize.X + 50) div R.Size[884].X;

  bevelSize.X := (tmp + 1) * R.Size[884].X - 40;

  TKMRenderUI.WriteBevel(AbsLeft + 40, AbsTop + 5, bevelSize.X + 10, bevelSize.Y + 10, 1, 0.75);
  //TKMRenderUI.WriteBevel(AbsLeft, AbsTop - 1, bevelSize.X + 60, 5,TKMColor3f.New(0.82, 0.5, 0), 1, 1);

  for I := 0 to tmp do
    TKMRenderUI.WritePicture(AbsLeft + R.Size[884].X * I, CursorY, R.Size[884].X, R.Size[884].Y, [], rxGui, 884, true);

  TKMRenderUI.WritePicture(AbsLeft + 50, CursorY - 2, R.Size[885].X, R.Size[885].Y, [], rxGui, 885, true);//left
  TKMRenderUI.WritePicture(AbsLeft - 10 + (tmp + 1) * R.Size[884].X, CursorY - 2, R.Size[885].X, R.Size[885].Y, [], rxGui, 885, true);//right

  TKMRenderUI.WriteText(AbsLeft + 45, AbsTop + 10, bevelSize.X, fText, fFont, fTextAlign, col, False, False, False, false, fTabWidth);
end;

{ TKMLabelScroll }
constructor TKMLabelScroll.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aTextAlign: TKMTextAlign);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight, aCaption, aFont, aTextAlign);
  SmoothScrollToTop := 0; //Disabled by default
end;


procedure TKMLabelScroll.Paint;
var
  newTop: Integer;
  col: Cardinal;
begin
  TKMRenderUI.SetupClipY(AbsTop, AbsTop + Height);
  newTop := EnsureRange(AbsTop + Height - TimeSince(SmoothScrollToTop) div 20, -MINSHORT, MAXSHORT); //Compute delta and shift by it upwards (Credits page)

  if Enabled then
    col := FontColor
  else
    col := $FF888888;

  TKMRenderUI.WriteText(AbsLeft, newTop, Width, fCaption, fFont, fTextAlign, col);
  TKMRenderUI.ReleaseClipY;
end;


{ TKMImage }
constructor TKMImage.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui;
                            aImageAnchors: TKMAnchorsSet = [anLeft, anTop]);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fRX := aRX;
  fTexID := aTexID;
  fFlagColor := $FFFF00FF;
  ImageAnchors := aImageAnchors;
  Highlight := False;
  HighlightOnMouseOver := False;
  HighlightCoef := CTRL_HIGHLIGHT_COEF_DEF;
  AlphaStep := -1;
  TexID2 := 0;
end;


function TKMImage.GetIsPainted: Boolean;
begin
  Result := inherited and (TexID <> 0);
end;


//DoClick is called by keyboard shortcuts
//It's important that Control must be:
// IsVisible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMImage.Click: Boolean;
begin
  if Visible and Enabled then
  begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    Parent.MasterControl.CtrlOver := Self;
    Parent.MasterControl.CtrlUp := Self;
    if Assigned(OnClick) then OnClick(Self);
    Result := True; //Click has happened
  end
  else
    Result := False; //No, we couldn't click for Control is unreachable
end;


procedure TKMImage.ImageStretch;
begin
  ImageAnchors := [anLeft, anRight, anTop, anBottom]; //Stretch image to fit
end;


procedure TKMImage.ImageCenter; //Render image from center
begin
  ImageAnchors := [];
end;


{If image area is bigger than image - do center image in it}
procedure TKMImage.Paint;
var
  x, y: Integer;
  col, row: Integer;
  paintLightness: Single;
  drawLeft, drawTop: Integer;
  drawWidth, drawHeight: Integer;
begin
  inherited;
  if fTexID > 0 then
  begin //No picture to draw

    if ClipToBounds then
    begin
      TKMRenderUI.SetupClipX(AbsLeft, AbsLeft + Width);
      TKMRenderUI.SetupClipY(AbsTop,  AbsTop + Height);
    end;

    paintLightness := Lightness + HighlightCoef * (Byte(HighlightOnMouseOver and (csOver in State)) + Byte(Highlight));

    if Tiled then
    begin
      drawWidth := gGFXData[fRX, fTexID].PxWidth;
      drawHeight := gGFXData[fRX, fTexID].PxHeight;
      drawLeft := AbsLeft + Width div 2 - drawWidth div 2;
      drawTop := AbsTop + Height div 2 - drawHeight div 2;

      col := Width div drawWidth + 1;
      row := Height div drawHeight + 1;
      for x := -col div 2 to col div 2 do
        for y := -row div 2 to row div 2 do
          TKMRenderUI.WritePicture(drawLeft + x * drawWidth, drawTop + y * drawHeight, drawWidth, drawHeight, ImageAnchors, fRX, fTexID, Enabled, fFlagColor, paintLightness, AlphaStep);
    end
    else
      TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, ImageAnchors, fRX, fTexID, Enabled, fFlagColor, paintLightness, AlphaStep);

    if TexID2 > 0 then
      TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, ImageAnchors, fRX, TexID2, Enabled, fFlagColor, paintLightness, AlphaStep);
    if ClipToBounds then
    begin
      TKMRenderUI.ReleaseClipX;
      TKMRenderUI.ReleaseClipY;
    end;
  end;
end;


constructor TKMGuiImage.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer; aImageAnchors: TKMAnchorsSet = [anLeft,anTop]);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight, 0, rxGuiMain, aImageAnchors);
  Hitable := false;
end;

function TKMGuiImage.GetRX: TRXType;
begin
  Result := rxGuiMain;
end;

function TKMGuiImage.GetTexID: Word;
begin
  Result := 0;
end;

procedure TKMGuiImage.Paint;
var
  x, y: Integer;
  col, row: Integer;
  paintLightness: Single;
  drawLeft, drawTop: Integer;
  drawWidth, drawHeight: Integer;
begin
  if GetTexID > 0 then
  begin //No picture to draw

    if ClipToBounds then
    begin
      TKMRenderUI.SetupClipX(AbsLeft, AbsLeft + Width);
      TKMRenderUI.SetupClipY(AbsTop,  AbsTop + Height);
    end;

    paintLightness := Lightness + HighlightCoef * (Byte(HighlightOnMouseOver and (csOver in State)) + Byte(Highlight));

    if self is TKMImageBackGround then
    begin
      drawWidth := gGFXData[GetRX, GetTexID].PxWidth;
      drawHeight := gGFXData[GetRX, GetTexID].PxHeight;
      drawLeft := AbsLeft + Width div 2 - drawWidth div 2;
      drawTop := AbsTop + Height div 2 - drawHeight div 2;

      col := Width div drawWidth + 1;
      row := Height div drawHeight + 1;
      for x := -col div 2 to col div 2 do
        for y := -row div 2 to row div 2 do
          TKMRenderUI.WritePicture(drawLeft + x * drawWidth, drawTop + y * drawHeight, drawWidth, drawHeight, ImageAnchors, GetRX, GetTexID, Enabled, fFlagColor, paintLightness, AlphaStep);
    end
    else
      TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, ImageAnchors, GetRX, GetTexID, Enabled, fFlagColor, paintLightness, AlphaStep);

    if ClipToBounds then
    begin
      TKMRenderUI.ReleaseClipX;
      TKMRenderUI.ReleaseClipY;
    end;
  end;
end;

function TKMImageBackGround.GetTexID: Word; begin  Result := gRes.Cosmetics.CurrentGuiStyle.Background; end;
function TKMImageMain.GetTexID: Word; begin  Result := gRes.Cosmetics.CurrentGuiStyle.MainImage; end;

function TKMImageSideBar.GetRX: TRXType; begin Result := rxGui; end;
function TKMImageSideBar.GetTexID: Word; begin Result := gRes.Cosmetics.CurrentGuiStyle.LeftPanel; end;

function TKMImageMinimap.GetRX: TRXType; begin Result := rxGui; end;
function TKMImageMinimap.GetTexID: Word; begin Result := gRes.Cosmetics.CurrentGuiStyle.MiniMap; end;

function TKMImageMainLeft.GetTexID: Word; begin  Result := gRes.Cosmetics.CurrentGuiStyle.LeftTex; end;
function TKMImageMainRight.GetTexID: Word; begin  Result := gRes.Cosmetics.CurrentGuiStyle.RightTex; end;
function TKMImageMainText.GetTexID: Word; begin  Result := gRes.Cosmetics.CurrentGuiStyle.MainText; end;

{ TKMImageStack }
constructor TKMImageStack.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID1, aTexID2: Word; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fRX  := aRX;
  fTexID1 := aTexID1;
  fTexID2 := aTexID2;
end;


procedure TKMImageStack.SetCount(aCount, aColumns, aHighlightID: Word);
var
  aspect: Single;
begin
  fCount := aCount;
  fColumns := Math.max(1, aColumns);
  fHighlightID := aHighlightID;

  fDrawWidth  := EnsureRange(Width div fColumns, 8, gGFXData[fRX, fTexID1].PxWidth);
  fDrawHeight := EnsureRange(Height div Ceil(fCount/fColumns), 6, gGFXData[fRX, fTexID1].PxHeight);

  aspect := gGFXData[fRX, fTexID1].PxWidth / gGFXData[fRX, fTexID1].PxHeight;
  if fDrawHeight * aspect <= fDrawWidth then
    fDrawWidth  := Round(fDrawHeight * aspect)
  else
    fDrawHeight := Round(fDrawWidth / aspect);
end;


// If image area is bigger than image - do center image in it
procedure TKMImageStack.Paint;
var
  I: Integer;
  offsetX, offsetY, centerX, centerY: SmallInt; //variable parameters
  texID: Word;
begin
  inherited;
  if fTexID1 = 0 then Exit; //No picture to draw

  offsetX := Width div fColumns;
  offsetY := Height div Ceil(fCount / fColumns);

  centerX := (Width - offsetX * (fColumns-1) - fDrawWidth) div 2;
  centerY := (Height - offsetY * (Ceil(fCount/fColumns) - 1) - fDrawHeight) div 2;

  for I := 0 to fCount - 1 do
  begin
    texID := IfThen(I = fHighlightID, fTexID2, fTexId1);

    TKMRenderUI.WritePicture(AbsLeft + centerX + offsetX * (I mod fColumns),
                            AbsTop + centerY + offsetY * (I div fColumns),
                            fDrawWidth, fDrawHeight, [anLeft, anTop, anRight, anBottom], fRX, texID, Enabled);
  end;
end;


{ TKMButton }
constructor TKMButton.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType;
                             aStyle: TKMButtonStyle);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  InitCommon(aStyle);
  fRX   := aRX;
  TexID := aTexID;
end;


{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                             aStyle: TKMButtonStyle);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  InitCommon(aStyle);
  Caption := aCaption;
end;


procedure TKMButton.InitCommon(aStyle: TKMButtonStyle);
begin
  TexID             := 0;
  Caption           := '';
  FlagColor         := $FFFF00FF;
  Font              := fntMetal;
  fTextAlign        := taCenter; //Thats default everywhere in KaM
  TextVAlign        := tvaMiddle;//tvaNone;
  fStyle            := aStyle;
  MakesSound        := True;
  ShowImageEnabled  := True;
  AutoHeight        := False;
  AutoTextPadding   := 5;
  CapColor          := icWhite;
end;


procedure TKMButton.UpdateHeight;
var
  textY: Integer;
begin
  if fAutoHeight then
  begin
    textY := gRes.Fonts[Font].GetTextSize(Caption).Y;
    if textY + AutoTextPadding > Height then
      Height := textY + AutoTextPadding;
  end;
end;


procedure TKMButton.SetCaption(const aCaption: UnicodeString);
begin
  fCaption := aCaption;
  UpdateHeight;
end;


procedure TKMButton.SetAutoHeight(aValue: Boolean);
begin
  fAutoHeight := aValue;
  UpdateHeight;
end;


//DoClick is called by keyboard shortcuts
//It puts a focus on the button and depresses it if it was DoPress'ed
//It's important that Control must be:
// Visible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.Click: Boolean;
begin
  if Visible and Enabled then
  begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    Parent.MasterControl.CtrlOver := Self;
    Parent.MasterControl.CtrlUp := Self;
    if Assigned(OnClick) then
      OnClick(Self);
    Result := True; //Click has happened
  end
  else
    Result := False; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if Enabled and MakesSound and (csDown in State) then
    gSoundPlayer.Play(sfxnButtonClick);

  inherited;
end;


procedure TKMButton.Paint;
var
  col: TColor4;
  stateSet: TKMButtonStateSet;
  textY, top: Integer;
begin
  inherited;
  stateSet := [];
  if (csOver in State) and Enabled then
    stateSet := stateSet + [bsOver];
  if (csOver in State) and (csDown in State) then
    stateSet := stateSet + [bsDown];
  if not Enabled then
    stateSet := stateSet + [bsDisabled];

  TKMRenderUI.Write3DButton(AbsLeft, AbsTop, Width, Height, fBaseLeft, fBaseTop, fRX, TexID, FlagColor, stateSet, fStyle, ShowImageEnabled);

  if TexID <> 0 then Exit;

  //If disabled then text should be faded
  col := IfThen(Enabled, CapColor, icGray);

  top := AbsTop + Byte(bsDown in stateSet) + CapOffsetY;

  textY := gRes.Fonts[Font].GetTextSize(Caption).Y;
  case TextVAlign of
    tvaNone:    Inc(top, (Height div 2) - 7);
    tvaTop:     Inc(top, 2);
    tvaMiddle:  Inc(top, (Height div 2) - (textY div 2) + 2);
    tvaBottom:  Inc(top, Height - textY);
  end;
  TKMRenderUI.WriteText(AbsLeft + Byte(bsDown in stateSet) + CapOffsetX, top,
                        Width, Caption, Font, fTextAlign, col);
end;


{TKMButtonFlatCommon}
constructor TKMButtonFlatCommon.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  RX        := aRX;
  TexID     := aTexID;
  FlagColor := $FFFF00FF;
  CapColor  := $FFFFFFFF;
  Font      := fntGame;
  Clickable := True;
  EdgeAlpha := BEVEL_EDGE_ALPHA_DEF;
  BackAlpha := BEVEL_BACK_ALPHA_DEF;
  BackBevelColor := $00000000;
  HighLightColor := $40FFFFFF;
end;


procedure TKMButtonFlatCommon.SetCaption(aValue: string);
begin
  fCaption := aValue;
end;

procedure TKMButtonFlatCommon.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if not Clickable then Exit;
  if Enabled and (csDown in State) then
    gSoundPlayer.Play(sfxClick);

  inherited;
end;


procedure TKMButtonFlatCommon.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, EdgeAlpha, BackAlpha);

  TKMRenderUI.WriteShape(AbsLeft+1, AbsTop+1, Width-2, Height-2, BackBevelColor);

  if (csOver in State) and Enabled and not HideHighlight then
    TKMRenderUI.WriteShape(AbsLeft+1, AbsTop+1, Width-2, Height-2, HighLightColor);
end;


//Simple version of button, with a caption and image
{TKMButtonFlat}
constructor TKMButtonFlat.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer; aTexID: Integer; aRX: TRXType = rxGui);
begin
  Inherited;

  LineWidth := 1;
  DownColor :=$FFFFFFFF;
end;

procedure TKMButtonFlat.Paint;
var
  textCol: TColor4;
begin
  inherited;

  if TexID <> 0 then
    TKMRenderUI.WritePicture(AbsLeft + TexOffsetX,
                             AbsTop + TexOffsetY - 6 * Byte(Caption <> ''),
                             Width, Height, [], RX, TexID, Enabled or fEnabledVisually, FlagColor, 0, -1);

  textCol := IfThen(Enabled or fEnabledVisually, CapColor, $FF888888);


  TKMRenderUI.WriteText(AbsLeft + CapOffsetX, AbsTop + (Height div 2) + 4 + CapOffsetY, Width, Caption, Font, taCenter, textCol, false, false, not Enabled);

  if Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, DownColor);
  {if not fEnabled then
    TKMRenderUI.WriteShape(Left, Top, Width, Height, $80000000);}
end;

//Simple version of button, with a caption and image
{TKMButtonFlatFit}
constructor TKMButtonFlatFit.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer; aTexID: Integer; aRX: TRXType = rxGui);
begin
  Inherited;

  LineWidth := 1;
  DownColor :=$FFFFFFFF;
  DoFitImage := true;
  SetNewTexID(aTexID);
end;

procedure TKMButtonFlatFit.SetNewTexID(aID: Word);
var
  texSizeX, texSizeY : Word;
  scale : Single;
begin
  TexID := aID;
  texSizeX := gRes.Sprites[RX].RXData.Size[TexID].X;
  texSizeY := gRes.Sprites[RX].RXData.Size[TexID].Y;
  ImgAnchors := [];
  fImgWidth := Width;
  fImgHeight := Height;
  //scale image down
  If (texSizeX > fImgWidth) or (texSizeY > fImgHeight) then
  begin
    ImgAnchors := [anLeft, anRight, anTop, anBottom];

    If texSizeX - fImgWidth >= texSizeY - fImgHeight then
      scale := fImgWidth / texSizeX
    else
      scale := fImgHeight / texSizeY;

    fImgWidth := Round(texSizeX * scale);
    fImgHeight := Round(texSizeY * scale);
    TexOffsetX := (Width - fImgWidth) div 2;
    TexOffsetY := (Height - fImgHeight) div 2;
  end;
end;

procedure TKMButtonFlatFit.Paint;
var
  textCol: TColor4;
begin
  inherited;

  if TexID <> 0 then
  begin
    TKMRenderUI.WritePicture(AbsLeft + TexOffsetX,
                             AbsTop + TexOffsetY - 6 * Byte(Caption <> ''),
                             fImgWidth, fImgHeight, ImgAnchors, RX, TexID, Enabled or fEnabledVisually, FlagColor, 0, -1);
  end;

  textCol := IfThen(Enabled or fEnabledVisually, CapColor, $FF888888);


  TKMRenderUI.WriteText(AbsLeft + CapOffsetX, AbsTop + (Height div 2) + 4 + CapOffsetY, Width, Caption, Font, taCenter, textCol, false, false, not Enabled);

  if Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, DownColor);
  {if not fEnabled then
    TKMRenderUI.WriteShape(Left, Top, Width, Height, $80000000);}
end;


constructor TKMButtonFlatStack.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer;
                                      aTexIDs: TKMWordArray; aRX: TRXType = rxGui);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight, 0, aRX);
  TextIds := aTexIDs
end;

procedure TKMButtonFlatStack.Paint;
var I : Integer;
  textCol: TColor4;
begin
  Inherited;
  for I := Low(TextIds) to High(TextIds) do
    if TextIds[I] <> 0 then
      TKMRenderUI.WritePicture(AbsLeft + TexOffsetX,
                               AbsTop + TexOffsetY - 6 * Byte(Caption <> ''),
                               Width, Height, [], RX, TextIds[I], Enabled or fEnabledVisually, FlagColor);

  textCol := IfThen(Enabled or fEnabledVisually, CapColor, $FF888888);
  TKMRenderUI.WriteText(AbsLeft + CapOffsetX, AbsTop + (Height div 2) + 4 + CapOffsetY, Width,
                        Caption, Font, taCenter, textCol, false, false, not Enabled);

  if Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, DownColor);

end;

procedure TKMButtonFlatPin.Paint;
begin
  Inherited;

  TKMRenderUI.WriteOutline(AbsLeft+1, AbsTop+1, Width-2, Height-2, 2, icGoldenYellow); //yellow color
  TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 1, icAmberBrown); //brown color

  //TKMRenderUI.WriteBevel(AbsLeft + 1, AbsTop + 1, Width - 2, Height - 2, 1, 0);
  //TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, 0.2);

  TKMRenderUI.WritePicture(AbsLeft + Width div 2,
                           AbsTop - 6,
                           0, 0, [], rxGui, 755, Enabled or fEnabledVisually, FlagColor);
end;


{ TKMFlatButtonShape }
constructor TKMFlatButtonShape.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aShapeColor: TColor4);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fCaption    := aCaption;
  ShapeColor  := aShapeColor;
  fFont       := aFont;
  fFontHeight := gRes.Fonts[fFont].BaseHeight + 2;
  FontColor   := icWhite;
  LineWidth := 1;
  DownColor :=$FFFFFFFF;
end;


procedure TKMFlatButtonShape.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);

  //Shape within bevel
  TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Height - 2, ShapeColor);

  TKMRenderUI.WriteText(AbsLeft, AbsTop + (Height - fFontHeight) div 2,
                      Width, fCaption, fFont, taCenter, FontColor);

  if (csOver in State) and Enabled then
    TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Height - 2, $40FFFFFF);

  if (csDown in State) or Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, DownColor);
end;

procedure TKMImageAnimation.UpdateState(aTickCount: Cardinal);
begin
  Inherited;
  if not StopAnim then
    Inc(AnimStep);
end;

procedure TKMImageAnimation.Paint;
var id : Integer;
  paintLightness : Single;
begin
  if AddBevel then
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);


  Inherited;

  if Animation.Count > 0 then //No picture to draw
  begin
    if ClipToBounds then
    begin
      TKMRenderUI.SetupClipX(AbsLeft, AbsLeft + Width);
      TKMRenderUI.SetupClipY(AbsTop,  AbsTop + Height);
    end;
    id := Animation.Step[AnimStep mod Animation.Count + 1];

    paintLightness := Lightness + HighlightCoef * (Byte(HighlightOnMouseOver and (csOver in State)) + Byte(Highlight));

    TKMRenderUI.WritePicture(AbsLeft, AbsTop, Width, Height, ImageAnchors, fRX, id, Enabled, fFlagColor, paintLightness, AlphaStep);

    if ClipToBounds then
    begin
      TKMRenderUI.ReleaseClipX;
      TKMRenderUI.ReleaseClipY;
    end;
  end;

end;

procedure TKMButtonFlatBlock.SetBlock(aValue: Integer);
begin
  fBlock := aValue;
end;

procedure TKMButtonFlatBlock.Paint;
var aID : Integer;
begin
  Inherited;

  case fBlock of
    2 : aID := 32;
    3 : aID := 33;
    4 : aID := 91;
    else aID := 0;
  end;
  if aID > 0 then
    TKMRenderUI.WritePicture(AbsRight - 16, AbsBottom - 16, 16, 16, [], rxGuiMain, aId, Enabled);
end;


end.

