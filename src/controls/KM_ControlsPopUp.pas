unit KM_ControlsPopUp;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  Vcl.Controls,
  KromOGLUtils,
  KM_ControlsTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsList, KM_ControlsTrackBar, KM_ControlsEdit,
  KM_ResFonts,
  KM_RenderUI,
  KM_Points,
  KM_CommonTypes;


type
  TKMPopUpRectPanel = class(TKMPanel)
    private
      Button_Shape : TKMButton;
      Button_Back : TKMButton;
      Bevel_Back : TKMBevel;
      Bevel_Big : TKMBevel;
      fStyle : TKMButtonStyle;
      fSkipChilds : Byte;
    protected
      procedure SetHeight(aValue: Integer); override;
    public
      constructor Create(aParent: TKMPanel; aWidth, aHeight: Integer; aStyle : TKMButtonStyle = bsPaper);

      procedure SetHeightToChilds(aMargin : Integer = 5); override;
      procedure SetWidthToChilds(aMargin : Integer = 5);override;

      property Style : TKMButtonStyle read fStyle write fStyle;
  end;

  TKMPopUpMenu = class(TKMPanel)
  private
    fShapeBG: TKMShape;
    fList: TKMColumnBox;
    procedure MenuHide(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure SetItemIndex(aValue: Integer);
    function GetItemIndex: Integer;
    function GetItemTag(aIndex: Integer): Integer;
  public
    constructor Create(aParent: TKMPanel; aWidth: Integer);
    procedure AddItem(const aCaption: UnicodeString; aTag: Integer = 0);
    procedure UpdateItem(aIndex: Integer; const aCaption: UnicodeString);
    procedure Clear;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemTags[aIndex: Integer]: Integer read GetItemTag;
    procedure ShowAt(X,Y: Integer);
    procedure HideMenu;
  end;


  TKMPopUpBGImageType = (pbGray, pbYellow, pbScroll, pbPaper);

  TKMPopUpPanel = class(TKMPanel)
  const
    DEF_FONT: TKMFont = fntOutline;
  private
    fDragging: Boolean;
    fDragStartPos: TKMPoint;
    fBGImageType: TKMPopUpBGImageType;
    fHandleCloseKey: Boolean;
    fCapOffsetY: Integer;

    fOnClose: TEvent;
    procedure UpdateSizes;
    procedure Close(Sender: TObject);

    function GetLeftRightMargin: Integer;
    function GetTopMargin: Integer;
    function GetBottomMargin: Integer;
    function GetCrossTop: Integer;
    function GetCrossRight: Integer;

    function GetActualWidth: Integer;
    procedure SetActualWidth(aValue: Integer);
    function GetActualHeight: Integer;
    procedure SetActualHeight(aValue: Integer);

    procedure SetHandleCloseKey(aValue: Boolean);
    procedure SetCapOffsetY(aValue: Integer);
  protected
    BevelBG: TKMBevel;
    BevelShade: TKMBevel;
    procedure SetWidth(aValue: Integer); override;
    procedure SetHeight(aValue: Integer); override;
  public
    ItemsPanel: TKMPanel;
    DragEnabled: Boolean;
    ImageBG, ImageClose: TKMImage;

    CaptionLabel: TKMLabel;

    constructor Create(aParent: TKMPanel; aWidth, aHeight: Integer; const aCaption: UnicodeString = '';
                       aImageType: TKMPopUpBGImageType = pbYellow; aWithCrossImg: Boolean = False;
                       aShowBevel: Boolean = True; aShowShadeBevel: Boolean = True);

    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove (X,Y: Integer; Shift: TShiftState); override;
    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    function KeyUp(Key: Word; Shift: TShiftState): Boolean; override;

    procedure ControlMouseMove(Sender: TObject; X,Y: Integer; Shift: TShiftState);

    procedure ControlMouseDown(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
    procedure ControlMouseUp(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);

    property OnClose: TEvent read fOnClose write fOnClose;

    property ActualHeight: Integer read GetActualHeight write SetActualHeight;
    property ActualWidth: Integer read GetActualWidth write SetActualWidth;
    property CapOffsetY: Integer read fCapOffsetY write SetCapOffsetY;

    property HandleCloseKey: Boolean read fHandleCloseKey write SetHandleCloseKey;
  end;


  //Form that can be dragged around (and resized?)
//  TKMForm = class(TKMPanel)
//  private
//    fHeaderHeight: Integer;
//    fButtonClose: TKMButtonFlat;
//    fLabelCaption: TKMLabel;
//
//    fDragging: Boolean;
//    fOffsetX: Integer;
//    fOffsetY: Integer;
//    function HitHeader(X, Y: Integer): Boolean;
//    procedure FormCloseClick(Sender: TObject);
//    function GetCaption: UnicodeString;
//    procedure SetCaption(const aValue: UnicodeString);
//  public
//    OnMove: TNotifyEvent;
//    OnClose: TNotifyEvent;
//
//    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
//    property Caption: UnicodeString read GetCaption write SetCaption;
//    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
//    procedure MouseMove (X,Y: Integer; Shift: TShiftState); override;
//    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
//    procedure PaintPanel(aPaintLayer: TKMPaintLayer); override;
//  end;
  TKMPopUpConfirm = class (TKMPopUpMenu)
  protected
    fOnConfirm, fOnCancel : TNotifyEvent;
    Procedure ButtonClick(Sender : TObject);
  public
    LabelTop, LabelCenter : TKMLabel;
    Button_Confirm, Button_Cancel : TKMButton;
    constructor Create(aParent : TKMPanel; aLeft ,aTop, aWidth : Integer); overload;
    property OnConfirm : TNotifyEvent read fOnConfirm write fOnConfirm;
    property OnCancel : TNotifyEvent read fOnCancel write fOnCancel;

  end;

  TKMPopUpPanelRGB = class(TKMPopUpPanel)
  private
    fTrackBar_R,
    fTrackBar_G,
    fTrackBar_B : TKMTrackBar;
    Button_Confirm, Button_Cancel : TKMButton;
    fShape_Color: TKMFlatButtonShape;
    fColor : Cardinal;
    fOnConfirm : TNotifyEvent;
    Edit_Color : TKMEdit;
    procedure TrackBarChange(Sender : TObject);
    procedure ButtonClicked(Sender : TObject);
    procedure SetColor(aValue : Cardinal);
  protected
  public
    property TracBar_R : TKMTrackBar read fTrackBar_R;
    property TracBar_G : TKMTrackBar read fTrackBar_G;
    property TracBar_B : TKMTrackBar read fTrackBar_B;
    property OnConfirm : TNotifyEvent read fOnConfirm write fOnConfirm;
    property Color : Cardinal read fColor write SetColor;

    constructor Create(aParent: TKMPanel);
  end;


implementation
uses
  Math, SysUtils,
  KM_Resource, KM_ResTexts, KM_ResKeys, KM_ResTypes,
  KM_Defaults,
  KM_CommonUtils;

constructor TKMPopUpRectPanel.Create(aParent: TKMPanel; aWidth: Integer; aHeight: Integer; aStyle : TKMButtonStyle = bsPaper);
begin
  inherited Create(aParent, 0, 0, aWidth, aHeight);
  fStyle := aStyle;
  Centerize;

  Bevel_Big := TKMBevel.Create(self, -2000, -2000, 5000, 5000);
  Bevel_Big.HideParentOnClick;
  Bevel_Back := TKMBevel.Create(self, -5, -5, Width + 10, Height + 10);

  Button_Shape := TKMButton.Create(self, -5, -5, Width + 10, Height + 10, '', fStyle);
  Button_Shape.Hitable := false;
  Button_Shape.AnchorsStretch;



  Button_Back := TKMButton.Create(self, 0, Height - 30, 150, 30, gResTexts[106], bsMenu);
  Button_Back.HideParentOnClick;
  fSkipChilds := ChildCount;

  Hide;
end;

procedure TKMPopUpRectPanel.SetHeight(aValue: Integer);
begin
  Button_Back.Top := Height - 30;
end;

procedure TKMPopUpRectPanel.SetHeightToChilds(aMargin: Integer = 5);
var I, tmp : Integer;
begin
  tmp := 0;
  for I := fSkipChilds to ChildCount - 1 do
    tmp := Max(tmp, Childs[I].Bottom);
  Height := tmp + aMargin;


  Centerize;
end;

procedure TKMPopUpRectPanel.SetWidthToChilds(aMargin: Integer = 5);
var I, tmp : Integer;
begin
  tmp := 0;
  for I := fSkipChilds to ChildCount - 1 do
    tmp := Max(tmp, Childs[I].Right);
  Width := tmp + aMargin;
  Centerize;
end;

{ TKMPopUpMenu }
constructor TKMPopUpMenu.Create(aParent: TKMPanel; aWidth: Integer);
begin
  inherited Create(aParent, 0, 0, aWidth, 0);

  fShapeBG := TKMShape.Create(Self, 0, 0, aParent.Width, aParent.Height);
  fShapeBG.AnchorsStretch;
  fShapeBG.OnClick := MenuHide;
  fShapeBG.Hide;

  fList := TKMColumnBox.Create(Self, 0, 0, aWidth, 0, fntGrey, bsMenu);
  fList.AnchorsStretch;
  fList.BackAlpha := 0.8;
  fList.Focusable := False;
  fList.SetColumns(fntGrey, [''], [0]);
  fList.ShowHeader := False;
  fList.OnClick := MenuClick;
  fList.Hide;

  Hide;
end;


procedure TKMPopUpMenu.Clear;
begin
  fList.Clear;
end;


function TKMPopUpMenu.GetItemIndex: Integer;
begin
  Result := fList.ItemIndex;
end;


function TKMPopUpMenu.GetItemTag(aIndex: Integer): Integer;
begin
  Result := fList.Rows[aIndex].Tag;
end;


procedure TKMPopUpMenu.SetItemIndex(aValue: Integer);
begin
  fList.ItemIndex := aValue;
end;


procedure TKMPopUpMenu.AddItem(const aCaption: UnicodeString; aTag: Integer = 0);
begin
  fList.AddItem(MakeListRow([aCaption], aTag));
  Height := fList.ItemHeight * fList.RowCount;
end;


procedure TKMPopUpMenu.UpdateItem(aIndex: Integer; const aCaption: UnicodeString);
begin
  fList.Rows[aIndex].Cells[0].Caption := aCaption;
end;


procedure TKMPopUpMenu.MenuClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);

  MenuHide(Self);
end;


procedure TKMPopUpMenu.MenuHide(Sender: TObject);
begin
  Hide;
  fList.Hide;
  fShapeBG.Hide;
end;


procedure TKMPopUpMenu.HideMenu;
begin
  MenuHide(nil);
end;


procedure TKMPopUpMenu.ShowAt(X, Y: Integer);
begin
  fList.AbsLeft := X;
  fList.AbsTop := Y;

  //Reset previously selected item
  fList.ItemIndex := -1;

  Show;
  fShapeBG.Show;
  fList.Show;
end;


{ TKMPopUpPanel }
// aWidth / aHeight represents not TKMPopUpPanel sizes, but its internal panel: ItemsPanel
// PopUpPanel draw bigger image behind it
constructor TKMPopUpPanel.Create(aParent: TKMPanel; aWidth, aHeight: Integer; const aCaption: UnicodeString = '';
                                 aImageType: TKMPopUpBGImageType = pbYellow; aWithCrossImg: Boolean = False;
                                 aShowBevel: Boolean = True; aShowShadeBevel: Boolean = True);
var
  margin, l, t, topMarg, baseW, baseH, w, h: Integer;
begin
  fBGImageType := aImageType;

  margin := GetLeftRightMargin;

  baseW := aWidth + 2*margin;
  topMarg := GetTopMargin;
  baseH := aHeight + GetBottomMargin + topMarg;
  w := Min(aParent.Width, baseW);
  h := Min(aParent.Height, baseH);
  l := Max(0, (aParent.Width - w) div 2);
  t := Max(0, (aParent.Height - h) div 2);

  // Create panel with calculated sizes
  inherited Create(aParent, l, t, w, h);

  // Fix its base sizes as a desired one
  BaseWidth := baseW;
  BaseHeight := baseH;

  FitInParent := True;
  DragEnabled := False;
  fHandleCloseKey := False;
  fCapOffsetY := 0;

  if aShowShadeBevel then
    BevelShade := TKMBevel.Create(Self, -2000,  -2000, 5000, 5000);
  If fBGImageType = pbYellow then
    ImageBG := TKMImageMain.Create(Self, 0, 0, w, h{, 15, rxGuiMain})
  else
    ImageBG := TKMImage.Create(Self, 0, 0, w, h, 15, rxGuiMain);

  if fBGImageType = pbPaper then
    with TKMBevel.Create(self, 0, 0, w, h) do
      BackAlpha := 0;


  ItemsPanel := TKMPanel.Create(Self, margin, topMarg, Width - 2*margin, Height - topMarg - GetBottomMargin);

  case fBGImageType of
    pbGray:   ImageBG.TexId := 15;
    //pbYellow: ImageBG.TexId := gRes.Cosmetics.CurrentGuiStyle.MainImage;
    pbPaper: ImageBG.TexId := 102;
    pbScroll: begin
                ImageBG.Rx := rxGui;
                ImageBG.TexId := 409;
              end;
  end;

  if aWithCrossImg then
  begin
    ImageClose := TKMImage.Create(Self, Width - GetCrossRight, GetCrossTop, 31, 30, 52);
    ImageClose.Anchors := [anTop, anRight];
    ImageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    ImageClose.OnClick := Close;
    ImageClose.HighlightOnMouseOver := True;
  end;

  ItemsPanel.AnchorsStretch;
  if aShowBevel then
  begin
    BevelBG := TKMBevel.Create(ItemsPanel, 0, 0, ItemsPanel.Width, ItemsPanel.Height);
    BevelBG.AnchorsStretch;
  end;

  ImageBG.ImageStretch;

  CaptionLabel := TKMLabel.Create(ItemsPanel, 0, IfThen(fBGImageType = pbPaper, -18, -25), ItemsPanel.Width, 20, aCaption, DEF_FONT, taCenter);

  AnchorsCenter;
  Hide;

  // Subscribe to get other controls mouse move events
  fMasterControl.AddMouseMoveCtrlSub(ControlMouseMove);

  // Subscribe to get other controls mouse down events
  fMasterControl.AddMouseDownCtrlSub(ControlMouseDown);

    // Subscribe to get other controls mouse up events
  fMasterControl.AddMouseUpCtrlSub(ControlMouseUp);
end;


function TKMPopUpPanel.GetLeftRightMargin: Integer;
const
  MARGIN_SIDE: array [TKMPopUpBGImageType] of Byte = (20, 35, 20, 3);
begin
  Result := MARGIN_SIDE[fBGImageType];
end;


function TKMPopUpPanel.GetTopMargin: Integer;
const
  MARGIN_TOP: array [TKMPopUpBGImageType] of Byte = (40, 80, 50, 20);
begin
  Result := MARGIN_TOP[fBGImageType];
end;


function TKMPopUpPanel.GetBottomMargin: Integer;
const
  MARGIN_BOTTOM: array [TKMPopUpBGImageType] of Byte = (20, 50, 20, 5);
begin
  Result := MARGIN_BOTTOM[fBGImageType];
end;


function TKMPopUpPanel.GetCrossTop: Integer;
const
  CROSS_TOP: array [TKMPopUpBGImageType] of Integer = (24, 40, 24, -15);
begin
  Result := CROSS_TOP[fBGImageType];
end;


function TKMPopUpPanel.GetCrossRight: Integer;
const
  // We probably should calc those sizes as dependant of the Width
  CROSS_RIGHT: array [TKMPopUpBGImageType] of Byte = (50, 130, 55, 20);
begin
  Result := CROSS_RIGHT[fBGImageType];
end;


procedure TKMPopUpPanel.Close(Sender: TObject);
begin
  Hide;

  if Assigned(fOnClose) then
    fOnClose;
end;


procedure TKMPopUpPanel.ControlMouseDown(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if Sender = ImageBG then
    MouseDown(X, Y, Shift, Button);
end;


procedure TKMPopUpPanel.ControlMouseMove(Sender: TObject; X, Y: Integer; Shift: TShiftState);
begin
  inherited;

  MouseMove(X, Y, Shift);
end;


procedure TKMPopUpPanel.ControlMouseUp(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  MouseUp(X, Y, Shift, Button);
end;


procedure TKMPopUpPanel.MouseDown(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  if not DragEnabled then Exit;

  fDragging := True;
  fDragStartPos := TKMPoint.New(X,Y);
end;

procedure TKMPopUpPanel.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited;

  if not DragEnabled or not fDragging then Exit;

  Left := EnsureRange(Left + X - fDragStartPos.X, 0, fMasterControl.MasterPanel.Width - Width);
  Top := EnsureRange(Top + Y - fDragStartPos.Y, -ImageBG.Top, fMasterControl.MasterPanel.Height - Height);

  fDragStartPos := TKMPoint.New(X,Y);
end;

procedure TKMPopUpPanel.MouseUp(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  if not DragEnabled then Exit;

  fDragging := False;
end;


function TKMPopUpPanel.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := inherited;
  if Result then Exit; // Key already handled

  if not fHandleCloseKey then Exit;

  if Key = gResKeys[kfCloseMenu] then
  begin
    Close(nil);
    Result := True;
  end;
end;


procedure TKMPopUpPanel.SetHeight(aValue: Integer);
begin
  inherited;

  UpdateSizes;
end;


procedure TKMPopUpPanel.SetWidth(aValue: Integer);
begin
  inherited;

  UpdateSizes;
end;


procedure TKMPopUpPanel.UpdateSizes;
begin
  ImageBG.Width := Width;
  ImageBG.Height := Height;
end;


function TKMPopUpPanel.GetActualWidth: Integer;
begin
  Result := ItemsPanel.Width;
end;


procedure TKMPopUpPanel.SetActualWidth(aValue: Integer);
var
  baseW: Integer;
begin
  baseW := aValue + GetLeftRightMargin*2;
  SetWidth(Min(Parent.Width, baseW));
end;


function TKMPopUpPanel.GetActualHeight: Integer;
begin
  Result := ItemsPanel.Height;
end;


procedure TKMPopUpPanel.SetActualHeight(aValue: Integer);
var
  baseH, h: Integer;
begin
  baseH := aValue + GetBottomMargin + GetTopMargin;
  h := Min(Parent.Height, baseH);
  SetHeight(h);
end;


procedure TKMPopUpPanel.SetHandleCloseKey(aValue: Boolean);
begin
  fHandleCloseKey := aValue;
  Focusable := aValue;
end;


procedure TKMPopUpPanel.SetCapOffsetY(aValue: Integer);
begin
  CaptionLabel.Top := CaptionLabel.Top + aValue - fCapOffsetY;

  fCapOffsetY := aValue;
end;


{ TKMForm }
//constructor TKMForm.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
//begin
//  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
//
//  fHeaderHeight := 24;
//
//  fButtonClose := TKMButtonFlat.Create(Self, aWidth - fHeaderHeight + 2, 2, fHeaderHeight-4, fHeaderHeight-4, 340, rxGui);
//  fButtonClose.OnClick := FormCloseClick;
//  fLabelCaption := TKMLabel.Create(Self, 0, 5, aWidth, fHeaderHeight, 'Form1', fntOutline, taCenter);
//  fLabelCaption.Hitable := False;
//end;
//
//
//procedure TKMForm.FormCloseClick(Sender: TObject);
//begin
//  Hide;
//
//  if Assigned(OnClose) then
//    OnClose(Self);
//end;
//
//
//function TKMForm.GetCaption: UnicodeString;
//begin
//  Result := fLabelCaption.Caption;
//end;
//
//
//procedure TKMForm.SetCaption(const aValue: UnicodeString);
//begin
//  fLabelCaption.Caption := aValue;
//end;
//
//
//function TKMForm.HitHeader(X, Y: Integer): Boolean;
//begin
//  Result := InRange(X - AbsLeft, 0, Width) and InRange(Y - AbsTop, 0, fHeaderHeight);
//end;
//
//
//procedure TKMForm.MouseDown(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
//begin
//  inherited;
//
//  if HitHeader(X,Y) then
//  begin
//    fDragging := True;
//    fOffsetX := X - AbsLeft;
//    fOffsetY := Y - AbsTop;
//  end;
//
//  MouseMove(X, Y, Shift);
//end;
//
//
//procedure TKMForm.MouseMove(X,Y: Integer; Shift: TShiftState);
//begin
//  inherited;
//
//  if fDragging and (csDown in State) then
//  begin
//    AbsLeft := EnsureRange(X - fOffsetX, 0, MasterParent.Width - Width);
//    AbsTop := EnsureRange(Y - fOffsetY, 0, MasterParent.Height - Height);
//
//    if Assigned(OnMove) then
//      OnMove(Self);
//  end;
//end;
//
//
//procedure TKMForm.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
//begin
//  inherited;
//  MouseMove(X,Y,Shift);
//
//end;
//
//
//procedure TKMForm.PaintPanel(aPaintLayer: TKMPaintLayer);
//begin
//  TKMRenderUI.WriteShadow(AbsLeft, AbsTop, Width, Height, 15, $40000000);
//
//  TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 3, $FF000000);
//  TKMRenderUI.WriteOutline(AbsLeft+1, AbsTop+1, Width-2, Height-2, 1, $FF80FFFF);
//
//  inherited;
//end;
constructor TKMPopUpConfirm.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer);
begin
  Inherited Create(aParent, aWidth);
  Self.Left := aLeft;
  Self.Top := aTop;
  Self.Height := 200;

  TKMBevel.Create(Self, -2000,  -2000, 5000, 5000);

  TKMImage.Create(Self, 0, 0, Self.Width, Self.Height, 15, rxGuiMain).ImageStretch;

  LabelTop := TKMLabel.Create(self, 25, 40, self.Width - 50, 25, '', fntOutline, taCenter );
  LabelTop.Hitable := false;
  LabelCenter := TKMLabel.Create(self, 25, 85, self.Width - 50, 25, '', fntOutline, taCenter );
  LabelCenter.Hitable := false;

  Button_Confirm := TKMButton.Create(self, 20, 155, 150, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
  Button_Confirm.Anchors:= [anLeft, anBottom];
  Button_Confirm.OnClick := ButtonClick;

  Button_Cancel := TKMButton.Create(self, self.Width - 20 - 150, 155, 150, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
  Button_Cancel.Anchors:= [anLeft, anBottom];
  Button_Cancel.OnClick := ButtonClick;
end;

procedure TKMPopUpConfirm.ButtonClick(Sender: TObject);
begin
  if (Sender = Button_Confirm) and Assigned(fOnConfirm) then
    fOnConfirm(Button_Confirm)
  else
  if (Sender = Button_Cancel) and Assigned(fOnCancel) then
    fOnCancel(Button_Cancel);
end;

constructor TKMPopUpPanelRGB.Create(aParent: TKMPanel);
begin
  Inherited Create(aParent, 250, 250, gResTexts[1953]);

  fTrackBar_R := TKMTrackbar.Create(ItemsPanel, 5, 10, ItemsPanel.Width - 10, 0, 255);
  fTrackBar_G := TKMTrackbar.Create(ItemsPanel, 5, 40, ItemsPanel.Width - 10, 0, 255);
  fTrackBar_B := TKMTrackbar.Create(ItemsPanel, 5, 70, ItemsPanel.Width - 10, 0, 255);
  fShape_Color := TKMFlatButtonShape.Create(ItemsPanel, 5, 100,  ItemsPanel.Width - 10, 40, '', fntMini, 0);
  fShape_Color.Hitable := false;

  fTrackBar_R.OnChange := TrackBarChange;
  fTrackBar_G.OnChange := TrackBarChange;
  fTrackBar_B.OnChange := TrackBarChange;

  Button_Confirm := TKMButton.Create(ItemsPanel, 5, ItemsPanel.Height - 30, 100, 25, gResTexts[836], bsGame);
  Button_Confirm.OnClick := ButtonClicked;
  Button_Cancel := TKMButton.Create(ItemsPanel, ItemsPanel.Width - 10 - 100, ItemsPanel.Height - 30, 100, 25, gResTexts[991], bsGame);
  Button_Cancel.OnClick := ButtonClicked;

  Edit_Color := TKMEdit.Create(ItemsPanel, 5, fShape_Color.Bottom + 5, 100, 20, fntMetal);
  Edit_Color.AutoFocusable := False; // No need to make too much attention on that field
  Edit_Color.Anchors := [anLeft, anTop, anRight];
  Edit_Color.AllowedChars := acHex;
  Edit_Color.MaxLen := 6;
  Edit_Color.OnChange := TrackBarChange;

  fColor := 0 and $FF000000;
  Self.Hide;
end;

procedure TKMPopUpPanelRGB.TrackBarChange(Sender: TObject);
var S : String;
  R,G,B : Byte;
  C : Cardinal;
begin
  if sender = Edit_Color then
  begin
    if length(Edit_Color.Text) = 0 then
      Exit;
    S := '$' + Edit_Color.Text;
    C := StrToInt(S);
    C := C or $FF000000;
    fColor := C;
    ExtractRGB(fColor, R, G, B);
    fShape_Color.ShapeColor := fColor;
    fTrackBar_B.Position := B;
    fTrackBar_G.Position := G;
    fTrackBar_R.Position := R;
    Exit;
  end;
  //S := '$';
  //S := S + IntToHex(fTrackBar_R.Position)+ IntToHex(fTrackBar_G.Position)+ IntToHex(fTrackBar_B.Position);

  B := StrToInt64('$' + IntToHex(fTrackBar_R.Position));
  G := StrToInt64('$' + IntToHex(fTrackBar_G.Position));
  R := StrToInt64('$' + IntToHex(fTrackBar_B.Position));

  S := '$FF' + IntToHex(R) + IntToHex(G) + IntToHex(B);
  fColor := StrToInt64(S);
  fShape_Color.ShapeColor := fColor;
  Edit_Color.Text := Format('%.6x', [fColor and $FFFFFF]);
end;

procedure TKMPopUpPanelRGB.ButtonClicked(Sender: TObject);
begin
  if Sender = Button_Confirm then
    if Assigned(fOnConfirm) then
      fOnConfirm(Self);

  Hide;
  
end;
procedure TKMPopUpPanelRGB.SetColor(aValue: Cardinal);
var R,G,B : Byte;
begin
  fColor := aValue or $FF000000;

  ExtractRGB(fColor, R, G, B);
  fShape_Color.ShapeColor := fColor;
  Edit_Color.Text := Format('%.6x', [fColor and $FFFFFF]);
  fTrackBar_B.Position := B;
  fTrackBar_G.Position := G;
  fTrackBar_R.Position := R;
end;

end.

