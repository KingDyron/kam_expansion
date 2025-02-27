unit KM_ControlsWaresRow;
{$I KaM_Remake.inc}
interface
uses
  Classes, Vcl.Controls,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch,
  KM_ResTypes,
  KM_CommonTypes, KM_Defaults;


type
  // Row with resource name and icons
  TKMWaresRow = class(TKMButtonFlatCommon)
  private
    fFinCap : UnicodeString;
  protected
    procedure SetWidth(aValue: Integer); override;
    procedure SetCaption(aValue : String); override;
    function GetMobilHint : Boolean;override;
    function GetHint : UnicodeString;override;
  public
    TextOffset, TextMarginX : SmallInt;
    TxtOffset : SmallInt;
    ShowName : Boolean;
    Spacing : Word;
    WareCount: Word;
    WareCntAsNumber: Boolean;
    MaxWares : Word;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer); reintroduce; overload;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aClickable: Boolean); reintroduce; overload;
    procedure Paint; override;
  end;


  // Ware order bar
  TKMWareOrderRow = class(TKMControl)
  private
    fWaresRow: TKMWaresRow;
    fOrderAdd: TKMButton;
    fOrderLab: TKMLabel;
    fOrderRem: TKMButton;
    fOrderCheckbox: TKMCheckBoxFlat;
    fOrderCount: Integer;
    fAsCheckBox : Boolean;
    fClcickHoldTimer: Byte;
    fImmidiateOrder: Boolean; //Order count should be changed immidiately in control. Should be False usually
    procedure ButtonClick(Sender: TObject; Shift: TShiftState);
    procedure ClickHold(Sender: TObject; Button: TMouseButton; var aHandled: Boolean);
    procedure SetOrderRemHint(const aValue: UnicodeString);
    procedure SetOrderAddHint(const aValue: UnicodeString);
    procedure SetOrderCount(aValue: Integer);
    procedure SetAsCheckBox(aValue : Boolean);
  protected
    procedure SetTop(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
    function DoHandleMouseWheelByDefault: Boolean; override;
  public
    ReplaceCaption: String;
    OrderCntMin: Integer;
    OrderCntMax: Integer;
    Orderable : Boolean;
    OnChange: TObjectIntegerEvent;
    property AsCheckBox : Boolean read fAsCheckBox write SetAsCheckBox;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aOrderCntMax: Integer = MAX_WARES_IN_HOUSE;
                       aOrderCntMin: Integer = 0; aImmidiateOrder: Boolean = False);
    property WareRow: TKMWaresRow read fWaresRow;
    property OrderCount: Integer read fOrderCount write SetOrderCount;
    property OrderRemHint: UnicodeString write SetOrderRemHint;
    property OrderAddHint: UnicodeString write SetOrderAddHint;
    procedure MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean); override;
    procedure Paint; override;
  end;


  // Production cost bar
  TKMCostsRow = class(TKMControl)
  public
    RX: TRXType;
    TexArr : TIntegerArray;
    TexID1, TexID2, TexID3: Word;
    Count: Byte;
    Caption: UnicodeString;
    MaxCount: Byte;
    AddBevel : Boolean;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aMaxCount: Byte = 6);
    procedure Paint; override;
  end;

  TKMCostsRowMulti = class(TKMControl)
  public
    WarePlan : TKMWarePlan;
    Caption : UnicodeString;
    procedure MouseOver(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
    const MARGIN = 5;
  end;

  TKMWaresButtonsMulti = class(TKMControl)
  public
    WarePlan : TKMWarePlan;
    Caption : UnicodeString;
    procedure MouseOver(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;

  TKMUnitsButtonsMulti = class(TKMControl)
  public
    UnitPlan : TKMUnitPlan;
    Caption : UnicodeString;
    procedure MouseOver(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;


  TKMWaresBlockRow = class(TKMControl)
  public
    RX: TRXType;
    Count, MaxCount: Word;
    TexID1, Spacing: Word;
    FromRight : Boolean;
    CntAsNumber : Boolean;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
    procedure Paint; override;

  end;

  TKMIconsRow = class(TKMControl)
    private

      fTags: TIntegerArray;
      fIcons: TKMWordArray;
      fIconOver: Byte;
      fIconClicked: Byte;
      fOnIconClicked : TIntegerEvent;
      fOnIconClickedShift :TIntegerShiftEvent;
    protected
      procedure SetVisible(aValue: Boolean); override;
    public
      RX: TRXType;
      MaxCountInRow: Byte;
      VSpacing,
      HSpacing : SmallInt;
      StillSize : Boolean;
      Caption : String;
      AddBevel : Boolean;
      BackBevel : Single;
      procedure SetIcons(aIcons : TKMWordArray);
      procedure Clear;
      procedure AddIcon(aIcon : Word; aTag : Integer);
      function GetTag(aIndex : Integer) : Integer;

      property OnIconClicked : TIntegerEvent read fOnIconClicked write fOnIconClicked;
      property OnIconClickedShift : TIntegerShiftEvent read fOnIconClickedShift write fOnIconClickedShift;


      procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
      procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
      procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

      constructor Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aHSpacing: Integer = 20; aVSpacing: Integer = 20);
      procedure Paint; override;
  end;
  TKMIconProgressBar = class(TKMControl)
    private
    protected
    public
      RX: TRXType;
      TexID : TKMWord2Array;
      Shape : (stSquare, stRound);
      Progress : TSingleArray;
      Colors : TKMCardinalArray;
      ColumnCount : Byte;
      Center : Boolean;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aRound : Boolean = false; aTexID : TKMWord2Array = []);
    procedure Paint; override;
  end;

implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  SysUtils, Math,
  KM_RenderUI,
  KM_Resource,
  KM_ResFonts,
  KM_UtilsExt;

const
  WARE_ROW_HEIGHT = 21;


{ TKMWaresRow }
constructor TKMWaresRow.Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer);
begin
  Create(aParent, aLeft, aTop, aWidth, False);
  Spacing := 14;
  TextOffset := 0;
  TxtOffset := 0;
  ShowName := true;
end;


constructor TKMWaresRow.Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aClickable: Boolean);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, WARE_ROW_HEIGHT, 0);
  Clickable := aClickable;
  HideHighlight := not aClickable;
  Spacing := 14;
  TextOffset := 0;
  TxtOffset := 0;
  ShowName := true;
  MaxWares := 6;
  TextMarginX := -20;
end;

procedure TKMWaresRow.SetCaption(aValue: string);
var clipText : Single;
begin
  Inherited;
  clipText := (Width + TextMarginX) / gRes.Fonts[fntGame].GetTextSize(Caption).X;

  if clipText < 1 then
  begin
    fFinCap := copy(Caption, 1, Round(high(Caption) * clipText));
    if fFinCap[high(fFinCap)] = #32 then  //delete space if it's the last char
      SetLength(fFinCap, high(fFinCap) - 1);
    fFinCap := fFinCap + '...';

  end
  else
    fFinCap := Caption;
end;

procedure TKMWaresRow.SetWidth(aValue: Integer);
begin
  Inherited;
  SetCaption(Caption);
end;
function TKMWaresRow.GetMobilHint: Boolean;
begin
  if Caption <> fFinCap then
    Result := True
  else
    Result := fMobilHint;
end;

function TKMWaresRow.GetHint: UnicodeString;
begin
  if Caption <> fFinCap then
    Result := fCaption
  else
    Result := fHint;
end;


procedure TKMWaresRow.Paint;
var
  I: Integer;
begin
  inherited;

  if ShowName then
  begin
    TKMRenderUI.WriteText(AbsLeft + 4 + TextOffset, AbsTop + 3, Width-8, fFinCap, fntGame, taLeft, CapColor);
  end;


  //Render in reverse order so the rightmost resource is on top (otherwise lighting looks wrong)
  if WareCntAsNumber then
  begin
    TKMRenderUI.WriteText(AbsLeft + Width - 18 - 70 + 24 + TextOffset, AbsTop + 3, 40, IntToKStr(WareCount, 1000), fntGame, taRight, $FFE0E0E0);
    TKMRenderUI.WritePicture(AbsLeft + Width - 18 + TxtOffset, AbsTop + 3, 14, 14, [], RX, TexID);
  end else
    for I := Min(WareCount - 1, MaxWares) downto 0 do
      TKMRenderUI.WritePicture(AbsLeft + Width - 18 - I * Spacing + TxtOffset, AbsTop + 3, 14, 14,
                                [], RX, TexID, Enabled, $FFFF00FF, 0.25 * (I mod 2));
end;


{ TKMWareOrderRow }
constructor TKMWareOrderRow.Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aOrderCntMax: Integer = MAX_WARES_IN_HOUSE;
                                   aOrderCntMin: Integer = 0; aImmidiateOrder: Boolean = False);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, WARE_ROW_HEIGHT);

  fWaresRow := TKMWaresRow.Create(aParent, aLeft + 68, aTop, aWidth - 68);

  fImmidiateOrder := aImmidiateOrder;

  OrderCntMin := aOrderCntMin;
  OrderCntMax := aOrderCntMax;
  ReplaceCaption := '';
  fOrderRem := TKMButton.Create(aParent, aLeft,   0, 20, Height - 2, '-', bsGame);
  fOrderAdd := TKMButton.Create(aParent, aLeft + 46,  0, 20, Height - 2, '+', bsGame);
  fOrderCheckbox := TKMCheckBoxFlat.Create(aParent, fWaresRow.Right - 20, aTop, WARE_ROW_HEIGHT, WARE_ROW_HEIGHT, '', fntMetal);
  fOrderCheckbox.OnClickShift := ButtonClick;
  fOrderCheckbox.Checked := false;
  //Label after buttons, to be sure it will be on top of them, to let player read value from it, if too long (more then 3 symbols)
  fOrderLab := TKMLabel.Create (aParent, aLeft + 33,  0, '', fntGrey, taCenter);
  fOrderLab.Hitable := false;
  fOrderAdd.CapOffsetY := 1;

  fOrderRem.OnClickShift := ButtonClick;
  fOrderAdd.OnClickShift := ButtonClick;
  fOrderRem.OnMouseWheel := MouseWheel;
  fOrderAdd.OnMouseWheel := MouseWheel;
  fWaresRow.OnMouseWheel := MouseWheel;
  fOrderRem.OnClickHold := ClickHold;
  fOrderAdd.OnClickHold := ClickHold;
  Orderable := True;
  AsCheckBox := false;
end;


procedure TKMWareOrderRow.ButtonClick(Sender: TObject; Shift: TShiftState);
var
  amt: Integer;
begin
  fClcickHoldTimer := 0;
  if Sender = fOrderCheckbox then
    amt := IfThen(fOrderCount > 0, -1000, 1000)
  else
    amt := GetMultiplicator(Shift, 10, true);
  if Sender = fOrderRem then
    amt := -amt;

  if amt = 0 then Exit;

  if fImmidiateOrder then
    OrderCount := fOrderCount + amt;

  Focus;

  if Assigned(OnChange) then
    OnChange(Self, amt);
end;


function TKMWareOrderRow.DoHandleMouseWheelByDefault: Boolean;
begin
  Result := False;
end;


procedure TKMWareOrderRow.MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
var
  amt: Integer;
begin
  inherited;

  if aHandled then Exit;
  if not fOrderRem.Visible then
    Exit;
  if not fOrderAdd.Visible then
    Exit;

  amt := MouseWheelStep * WheelSteps;
  if GetKeyState(VK_SHIFT) < 0 then
    amt := amt * 10;

  if fImmidiateOrder then
    OrderCount := fOrderCount + amt;

  aHandled := amt <> 0;

  Focus;

  if Assigned(OnChange) then
    OnChange(Self, amt);
end;


procedure TKMWareOrderRow.ClickHold(Sender: TObject; Button: TMouseButton; var aHandled: Boolean);
var
  amt: Integer;
begin
  inherited;
  aHandled := True;
  fClcickHoldTimer := EnsureRange(fClcickHoldTimer + 1, 0, high(byte));

  amt := GetMultiplicator(Button) * Round(sqr(fClcickHoldTimer / 30 + 1));

  if Sender = fOrderRem then
    amt := -amt;

  if amt = 0 then Exit;

  if fImmidiateOrder then
    OrderCount := fOrderCount + amt;

  if Assigned(OnChange) then
    OnChange(Self, amt);
end;

procedure TKMWareOrderRow.SetAsCheckBox(aValue: Boolean);
begin
  fAsCheckBox := aValue;
end;

procedure TKMWareOrderRow.SetOrderCount(aValue: Integer);
begin
  fOrderCount := EnsureRange(aValue, OrderCntMin, OrderCntMax);
  fOrderCheckbox.Checked := fOrderCount > 0;
end;


procedure TKMWareOrderRow.SetOrderRemHint(const aValue: UnicodeString);
begin
  fOrderRem.Hint := aValue;
end;


procedure TKMWareOrderRow.SetOrderAddHint(const aValue: UnicodeString);
begin
  fOrderAdd.Hint := aValue;
end;


procedure TKMWareOrderRow.SetTop(aValue: Integer);
begin
  inherited;
  fWaresRow.Top := aValue;
end;

//Copy property to buttons
procedure TKMWareOrderRow.SetEnabled(aValue: Boolean);
begin
  inherited;
  fWaresRow.Enabled := Enabled;
  fOrderRem.Enabled := Enabled;
  fOrderLab.Enabled := Enabled;
  fOrderAdd.Enabled := Enabled;
  fOrderCheckbox.Visible := Enabled;
end;


//Copy property to buttons. Otherwise they won't be rendered
procedure TKMWareOrderRow.SetVisible(aValue: Boolean);
begin
  inherited;
  fWaresRow.Visible := IsSetVisible;
  fOrderRem.Visible := IsSetVisible;
  fOrderLab.Visible := IsSetVisible;
  fOrderAdd.Visible := IsSetVisible;
  fOrderCheckbox.Visible := IsSetVisible;
end;


procedure TKMWareOrderRow.Paint;
begin
  inherited;
  fOrderRem.Visible := Orderable and not fAsCheckBox;
  fOrderLab.Visible :=  (Width > 100) and not fAsCheckBox;
  fOrderAdd.Visible := Orderable and not fAsCheckBox;
  fOrderCheckbox.Visible := Orderable and fAsCheckBox;
  //fOrderCheckbox.Checked := fOrderCount > 0;

  fOrderRem.Top := Top + 1; //Use internal fTop instead of GetTop (which will return absolute value)
  fOrderLab.Top := Top + 4;
  fOrderAdd.Top := Top + 1;
  fOrderCheckbox.Top := Top;

  fOrderRem.Left := IfThen(fOrderLab.Visible, Left, Left);
  fOrderAdd.Left := IfThen(fOrderLab.Visible, Left + 46, fOrderRem.Right);
  if fAsCheckBox then
  begin
    fWaresRow.Left := Left;
    fWaresRow.Width := Width - 22;
  end else
  begin
    fWaresRow.Left := IfThen(fOrderLab.Visible, Left + 68, fOrderAdd.Right);
    fWaresRow.Width := IfThen(fOrderLab.Visible, Width - 68, Width - 40);
  end;
  fOrderLab.Visible := fOrderLab.Visible and Orderable;//don't show the number if bar is not to long

  fOrderLab.Caption := IntToKStr(OrderCount, 10000);
  if ReplaceCaption <> '' then
    fOrderLab.Caption := ReplaceCaption;


end;


{ TKMCostsRow }
constructor TKMCostsRow.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aMaxCount: Byte = 6);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  MaxCount := aMaxCount;
  TexArr := [];
  AddBevel := true;
end;


procedure TKMCostsRow.Paint;
var
  I, gap, baseLeft: Integer;
begin
  inherited;
  baseLeft := 0;
  if AddBevel then
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, 40, 1, 0.35);
    TKMRenderUI.WriteText(AbsLeft + 3, AbsTop + 2, Width - 6, Caption, fntGrey, taCenter, $FFFFFFFF);
  end;

  if Count > 0 then
  begin
    if Count <= MaxCount then
      gap := 20
    else
      gap := Trunc(MaxCount * 20 / Count);

    baseLeft := gap * Count;
    baseLeft := AbsLeft + (Width - baseLeft) div 2;//Centerize
    if TexID1 <> 0 then
      for I := Count - 1 downto 0 do
        TKMRenderUI.WritePicture(baseLeft + gap*(I), AbsTop + 15, 20, Height, [], RX, TexID1);
  end else
  if length(TexArr) > 0 then
  begin
    if Count <= MaxCount then
      gap := 20
    else
      gap := Trunc(MaxCount * 20 / Count);
    baseLeft := gap * length(TexArr);
    baseLeft := AbsLeft + (Width - baseLeft) div 2;//Centerize

    for I := High(TexArr) downto 0 do
      if TexArr[I] <> 0 then      
        TKMRenderUI.WritePicture(baseLeft+gap*(I), AbsTop + 15, 20, Height, [], RX, TexArr[I]);
  end else
  begin
    Inc(baseLeft, byte(TexID1 > 0) * 20);
    Inc(baseLeft, byte(TexID2 > 0) * 20);
    Inc(baseLeft, byte(TexID3 > 0) * 20);
    baseLeft := AbsLeft + (Width - baseLeft) div 2;//Centerize

    if TexID1 <> 0 then
      TKMRenderUI.WritePicture(baseLeft, AbsTop + 15, 20, Height, [], RX, TexID1);
    if TexID2 <> 0 then
      TKMRenderUI.WritePicture(baseLeft+20, AbsTop + 15, 20, Height, [], RX, TexID2);
    if TexID3 <> 0 then
      TKMRenderUI.WritePicture(baseLeft+40, AbsTop + 15, 20, Height, [], RX, TexID3);
  end;
end;

constructor TKMWaresBlockRow.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  Count := 0;
  RX := rxGui;
  TexID1 := 49;
  FromRight := false;
  Spacing := 17;
end;

procedure TKMWaresBlockRow.Paint;
var
  I, gap, sWidth: Integer;
begin
  inherited;

  if Count > 0 then
  begin
    if not CntAsNumber then
    begin
    gap := Spacing;

    if FromRight then
    begin
      sWidth := Width;
      gap := -gap;
    end else
      sWidth := 0;

    if TexID1 <> 0 then
      for I := Count-1 downto 0 do
        TKMRenderUI.WritePicture(AbsLeft+sWidth+gap*(I+1), AbsTop, 20, Height, [], RX, TexID1);

    end else
    begin
      if Count = MaxCount then
        TKMRenderUI.WritePicture(AbsLeft, AbsTop, 20, Height, [], RX, TexID1);
      //TKMRenderUI.WriteText(AbsLeft, AbsTop, 30,'('+IntToStr(MaxCount - Count) + ')', fntGame, taRight, $FFE0E0E0);


    end;


  end;
end;

constructor TKMIconsRow.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aHSpacing: Integer = 20; aVSpacing: Integer = 20);
begin
  inherited Create(aParent, aLeft, aTop, 20, 20);

  fIcons := [];
  RX := rxGui;
  MaxCountInRow := 5;
  VSpacing := aVSpacing;
  HSpacing := aHSpacing;
  fIconClicked := 255;
  fIconOver := 255;//no icon over
  AddBevel := false;
  BackBevel := 0.8;
end;

procedure TKMIconsRow.SetIcons(aIcons: TKMWordArray);
begin
  fIcons := aIcons;
  if StillSize then
    Exit;
  Width := Min(length(fIcons), MaxCountInRow)  * HSpacing;
  Height := ((length(fIcons) + MaxCountInRow) div MaxCountInRow) * VSpacing;
  SetLength(fTags, length(fIcons));
  FillChar(fTags, SizeOf(fTags), #0);
end;

procedure TKMIconsRow.AddIcon(aIcon: Word; aTag: Integer);
var I : Integer;
begin
  I := length(fIcons);
  Setlength(fIcons, I + 1);
  Setlength(fTags, I + 1);
  fIcons[I] := aIcon;
  fTags[I] := aTag;
  if StillSize then
    Exit;

  Width := Min(length(fIcons), MaxCountInRow)  * HSpacing;
  Height := ((length(fIcons) + MaxCountInRow) div MaxCountInRow) * VSpacing;
end;

function TKMIconsRow.GetTag(aIndex: Integer): Integer;
begin
  Result := fTags[aIndex];
end;

procedure TKMIconsRow.Clear;
begin
  SetLength(fIcons, 0);
  SetLength(fTags, 0);
end;

procedure TKMIconsRow.SetVisible(aValue: Boolean);
var oldVisible: Boolean;
begin
  oldVisible := Visible;
  Inherited;

  If oldVisible <> Visible then
  begin
    fIconClicked := 255;
    fIconOver := 255;//no icon over
  end;
end;

procedure TKMIconsRow.MouseMove(X: Integer; Y: Integer; Shift: TShiftState);
var pX, pY : Integer;
    I : Integer;
begin
  Inherited;
  pX := X - AbsLeft;
  pY := Y - AbsTop;

  fIconOver := 255;//no icon over

  for I := 0 to High(fIcons) do
    if InRange(pX, HSpacing * (I mod MaxCountInRow), HSpacing * (I mod MaxCountInRow) + HSpacing) then
      if InRange(pY, VSpacing * (I div MaxCountInRow), HSpacing * (I div MaxCountInRow) + VSpacing) then
        fIconOver := I;
end;

procedure TKMIconsRow.MouseDown(X: Integer; Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  Inherited;
  fIconClicked := 255;
  if fIconOver = 255 then
    Exit;
  fIconClicked := fIconOver;
end;

procedure TKMIconsRow.MouseUp(X: Integer; Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  Inherited;

  if fIconClicked <> 255 then
  begin
    if Assigned(fOnIconClicked) then
      fOnIconClicked(fIconClicked);
    If Assigned(fOnIconClickedShift) then
      fOnIconClickedShift(fIconClicked, Shift);
  end;

  fIconClicked := 255;
end;


procedure TKMIconsRow.Paint;
var I, C, top, lWidth : integer;
begin
  inherited;
  C := length(fIcons);
  if C = 0 then Exit;

  if Caption <> '' then
    lWidth := Max(C div MaxCountInRow * VSpacing, gRes.Fonts[fntMetal].GetTextSize(Caption, I).X + 10)
  else
    lWidth := Width;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, lWidth, IfThen(Caption <> '', Height + 20, Height), 1, BackBevel);

  top := IfThen(Caption <> '', 20, 0);
  TKMRenderUI.WriteText(AbsLeft, AbsTop, lWidth, Caption, fntMetal, taCenter);
  for I := 0 to C - 1 do
  begin
    If AddBevel then
      TKMRenderUI.WriteBevel(AbsLeft + HSpacing * (I mod MaxCountInRow) + 2,
                              AbsTop + top + VSpacing * (I div MaxCountInRow) + 2,
                              HSpacing - 4, VSpacing -4, 1, BackBevel);
    TKMRenderUI.WritePicture(AbsLeft + HSpacing * (I mod MaxCountInRow), AbsTop + top + VSpacing * (I div MaxCountInRow), HSpacing, VSpacing,
                              [], RX, fIcons[I], not (fIconClicked = I), $FFFF00FF, 0.4 * ord(fIconOver = I));
  end;
end;

constructor TKMIconProgressBar.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aRound : Boolean = false; aTexID : TKMWord2Array = []);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  RX := rxGui;
  if aRound then
    Shape := stRound
  else
    Shape := stSquare;
  TexID := aTexID;
  Colors := [];
  ColumnCount := 5;
  Center := true;
end;

procedure TKMIconProgressBar.Paint;
var I, K, id : Integer;
  A : Single;
  C : Cardinal;
  lS : Integer;
begin
  Inherited;
  if not Visible then
    Exit;

  if Center  then
    lS := AbsLeft + (Width div 2) - (length(Progress) * 36) div 2
  else
    ls := AbsLeft;

  for I := 0 to High(Progress) do
  begin
    A := Progress[I];

    //render frame first
    case Shape of
      stSquare:   TKMRenderUI.WritePicture(ls + I mod ColumnCount * 36, AbsTop + I div ColumnCount * 36, 30, 30,
                              [], rxGui, 874, Enabled, $FFFF00FF);
      stRound:   TKMRenderUI.WritePicture(ls + I mod ColumnCount * 36, AbsTop + I div ColumnCount * 36, 30, 30,
                              [], rxGui, 879, Enabled, $FFFF00FF);
    end;

    K := I;
    if I > high(TexID) then
      K := high(TexID);

    if (length(TexID[K]) = 0) then
      id := 0
    else
    if length(TexID[K]) = 1 then
      id := TexID[K, 0]
    else
      id := TexID[K, Min(Round(A * high(TexID[K])), high(TexID[K]))];

    C := $FFFF00FF;
    if length(Colors) > 0 then
      C := Colors[EnsureRange(I, 0, high(Colors))];
    //render bar in cpolor
    case Shape of
      stSquare:   TKMRenderUI.WritePicture(ls + I mod ColumnCount * 36, AbsTop + I div ColumnCount * 36, 30, 30,
                              [], rxGui, 878, Enabled, C, 0, A);
      stRound:   TKMRenderUI.WritePicture(ls + I mod ColumnCount * 36, AbsTop + I div ColumnCount * 36, 30, 30,
                              [], rxGui, 880, Enabled, C, 0, A);
    end;


    TKMRenderUI.WritePicture(ls + I mod ColumnCount * 36, AbsTop + I div ColumnCount * 36, 30, 30,
                              [], RX, id, Enabled);

  end;

end;

procedure TKMCostsRowMulti.MouseOver(X: Integer; Y: Integer; Shift: TShiftState);
var I, J, lX: Integer;
  wareWidth : Integer;
begin
  Inherited;

  J := WarePlan.Count;

  Hint := '';
  if J = 0 then
  begin
  end else
  if J = 1 then
    Hint := gRes.Wares[WarePlan[0].W].Title
  else
  begin
    wareWidth := (Width div J) - MARGIN;

    lX := AbsLeft;
    for I := 0 to J - 1 do
      if InRange(X, lX + (wareWidth + MARGIN) * I, lX + (wareWidth + MARGIN) * I + wareWidth) then
      begin
        Hint := gRes.Wares[WarePlan[I].W].Title + ': x' + IntToStr(WarePlan[I].C);
        Exit;
      end;

  end;

end;

procedure TKMCostsRowMulti.Paint;
var I, J, id, wareWidth : Integer;
begin

  TKMRenderUI.WriteText(AbsLeft, AbsTop - 17, Width, Caption, fntMetal, taLeft);

  J := WarePlan.Count;
  if J = 0 then
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, 0.35); //render only bevel
    Exit;
  end;
  wareWidth := (Width div J);
  if J > 1 then
    wareWidth := wareWidth - MARGIN;


  for I := 0 to J - 1 do
  begin
    TKMRenderUI.WriteBevel(AbsLeft + (wareWidth + MARGIN) * I, AbsTop, wareWidth, Height, 1, 0.35); //render bevel for each ware

    id := gRes.Wares[WarePlan[I].W].GUIIcon;
    TKMRenderUI.WritePicture(AbsLeft + (wareWidth + MARGIN) * I, AbsTop - 1, 30, Height,
                              [], rxGui, id, Enabled);

    TKMRenderUI.WriteText(AbsLeft + (wareWidth + MARGIN) * I, AbsTop + 5, wareWidth - 5,'x' + IntToStr(WarePlan[I].C), fntGame, taRight);

  end;
    

end;

procedure TKMWaresButtonsMulti.MouseOver(X: Integer; Y: Integer; Shift: TShiftState);
var I, J, lX, lY : Integer;
begin
  Inherited;
  Hint := '';
  lX := AbsLeft;
  lY := AbsTop;
  J := 0;
  for I := 0 to WarePlan.Count - 1 do
    if (WarePlan[I].W <> wtNone) and (WarePlan[I].C > 0) then
    begin
      if InRange(X, lX + (J mod 5) * 37, lX + (J mod 5) * 37 + 32)
      and InRange(Y, lY + (J div 5) * 37, lY + (J div 5) * 37 + 36) then
      begin
        Hint := gRes.Wares[WarePlan[I].W].Title;
        Exit;
      end;

      Inc(J);
    end;

end;

procedure TKMWaresButtonsMulti.Paint;
var I, J, id : Integer;
begin
  TKMRenderUI.WriteText(AbsLeft, AbsTop - 17, Width, Caption, fntMetal, taLeft);

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, 5 * 37, 40 + ((WarePlan.Count - 1) div 5) * 37, 1, 0.5); //render bevel for each ware
  J := 0;
  for I := 0 to WarePlan.Count - 1 do
    if (WarePlan[I].W <> wtNone) and (WarePlan[I].C > 0) then
    begin

      TKMRenderUI.WriteBevel(AbsLeft + (J mod 5) * 37, AbsTop + (J div 5) * 37, 32, 36, 1, 0.35); //render bevel for each ware

      id := gRes.Wares[WarePlan[I].W].GUIIcon;
      TKMRenderUI.WritePicture(AbsLeft + (J mod 5) * 37, AbsTop + (J div 5) * 37, 32, 32,
                                [], rxGui, id, Enabled);

      TKMRenderUI.WriteText(AbsLeft + (J mod 5) * 37, AbsTop + 22 + ((J div 5) * 37), 32,'x' + IntToStr(WarePlan[I].C), fntGame, taCenter);


      Inc(J);
    end;

end;

procedure TKMUnitsButtonsMulti.MouseOver(X: Integer; Y: Integer; Shift: TShiftState);
var I, J, lX, lY : Integer;
begin
  Inherited;
  Hint := '';
  lX := AbsLeft;
  lY := AbsTop;
  J := 0;
  for I := 0 to high(UnitPlan) do
    if (UnitPlan[I].UnitType <> utNone) and (UnitPlan[I].Count > 0) then
    begin
      if InRange(X, lX + (J mod 5) * 37, lX + (J mod 5) * 37 + 32)
      and InRange(Y, lY + (J div 5) * 37, lY + (J div 5) * 37 + 36) then
      begin
        Hint := gRes.Units[UnitPlan[I].UnitType].GUIName;
        Exit;
      end;

      Inc(J);
    end;

end;

procedure TKMUnitsButtonsMulti.Paint;
var I, J, id : Integer;
begin
  TKMRenderUI.WriteText(AbsLeft, AbsTop - 17, Width, Caption, fntMetal, taLeft);

  J := 0;
  for I := 0 to high(UnitPlan) do
    if (UnitPlan[I].UnitType <> utNone) and (UnitPlan[I].Count > 0) then
    begin

      TKMRenderUI.WriteBevel(AbsLeft + (J mod 5) * 37, AbsTop + (J div 5) * 37, 32, 36, 1, 0.35); //render bevel for each ware

      id := gRes.Units[UnitPlan[I].UnitType].GUIIcon;
      TKMRenderUI.WritePicture(AbsLeft + (J mod 5) * 37, AbsTop + (J div 5) * 37, 32, 32,
                                [], rxGui, id, Enabled);

      TKMRenderUI.WriteText(AbsLeft + (J mod 5) * 37, AbsTop + 22 + ((J div 5) * 37), 32,'x' + IntToStr(UnitPlan[I].Count), fntGame, taCenter);
      Inc(J);
    end;

end;
end.

