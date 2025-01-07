﻿unit KM_ControlsScroll;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls,
  KM_Controls, KM_ControlsBase,
  KM_RenderUI,
  KM_Points;


type
  TKMScrollAxis = (saVertical, saHorizontal);
  TKMScrollStyle = (ssGame, ssCommon);
  TKMScrollAxisSet = set of TKMScrollAxis;

  TKMScrollBar = class(TKMPanel)
  private
    fScrollAxis: TKMScrollAxis;
    fStyle: TKMButtonStyle;
    fMinValue: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fThumbPos: Integer; //Position of the thumb
    fThumbSize: Word; //Length of the thumb
    fOffset: Integer;
    fScrollDec: TKMButton;
    fScrollInc: TKMButton;
    fOnChange: TNotifyEvent;
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure IncPosition(Sender: TObject);
    procedure DecPosition(Sender: TObject);
    procedure UpdateThumbPos;
    procedure UpdateThumbSize;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    function DoHandleMouseWheelByDefault: Boolean; override;
  public
    BackAlpha: Single; //Alpha of background (usually 0.5, dropbox 1)
    EdgeAlpha: Single; //Alpha of background outline (usually 1)
    WheelStep: Word;

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aScrollAxis: TKMScrollAxis;
                       aStyle: TKMButtonStyle; aScrollStyle: TKMScrollStyle = ssGame);
    property MinValue: Integer read fMinValue write SetMinValue;
    property MaxValue: Integer read fMaxValue write SetMaxValue;
    property Position: Integer read fPosition write SetPosition;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Paint; override;
  end;


  TKMScrollPanel = class(TKMPanel)
  private
    fChildsPanel: TKMPanel;

    fClipRect: TKMRect;
    fScrollBarH: TKMScrollBar;
    fScrollBarV: TKMScrollBar;
    fScrollAxisSet: TKMScrollAxisSet;

    fPadding: TKMRect;
    fScrollV_PadTop: Integer;
    fScrollV_PadBottom: Integer;
    fScrollV_PadLeft: Integer;
    fHideScrolls : Boolean;

    procedure UpdateScrolls(Sender: TObject; aValue: Boolean); overload;
    procedure UpdateScrolls(Sender: TObject); overload;
    procedure UpdateScrollV; overload;
    procedure UpdateScrollV(aChildsRect: TKMRect); overload;
    procedure UpdateScrollV(Sender: TObject; aValue: Integer); overload;
    procedure UpdateScrollH; overload;
    procedure UpdateScrollH(aChildsRect: TKMRect); overload;
    procedure UpdateScrollH(Sender: TObject; aValue: Integer); overload;
    procedure ScrollChanged(Sender: TObject);
    function GetChildsRect: TKMRect;

    function AllowScrollV: Boolean;
    function AllowScrollH: Boolean;
    procedure SetScrollVPadTop(const Value: Integer);
    procedure SetScrollVPadBottom(const Value: Integer);

    procedure SetScrollVPadLeft(const Value: Integer);
    procedure SetHideScrolls(aValue : Boolean);
  protected
    procedure SetVisible(aValue: Boolean); override;
    procedure SetHitable(const aValue: Boolean); override;

    procedure SetLeft(aValue: Integer); override;
    procedure SetTop(aValue: Integer); override;
    procedure SetHeight(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;

    function GetDrawRect: TKMRect; override;

    function GetAbsDrawLeft: Integer; override;
    function GetAbsDrawTop: Integer; override;
    function GetAbsDrawRight: Integer; override;
    function GetAbsDrawBottom: Integer; override;

    procedure UpdateVisibility; override;
  public
    DoScrollUpdate : Boolean;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aScrollAxisSet: TKMScrollAxisSet;
                       aStyle: TKMButtonStyle; aScrollStyle: TKMScrollStyle; aEnlargeParents: Boolean = False);

    property ScrollH: TKMScrollBar read fScrollBarH;
    property ScrollV: TKMScrollBar read fScrollBarV;

    function AddChild(aChild: TKMControl): Integer; override;
    property ClipRect: TKMRect read fClipRect;
    property Padding: TKMRect read fPadding write fPadding;
    property ScrollV_PadTop: Integer read fScrollV_PadTop write SetScrollVPadTop;
    property ScrollV_PadLeft: Integer read fScrollV_PadLeft write SetScrollVPadLeft;
    property ScrollV_PadBottom: Integer read fScrollV_PadBottom write SetScrollVPadBottom;
    property ChildPanel : TKMPanel read fChildsPanel;
    property HideScrolls : Boolean read fHideScrolls write SetHideScrolls;
    procedure Show; override;

    procedure MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean); override;

    procedure ClipY;
    procedure UnClipY;

    procedure UpdateScrolls; overload;

    procedure Paint; override;
  end;

  TKMExpandPanel = class(TKMPanel)
  private
    fOnRefresh : TNotifyEvent;
    fOnRefresh2 : TNotifyEvent;
    fHidden : Boolean;
    procedure ButtonExpandClick(Sender : TObject);
    procedure Refresh;
    function GetHidden : Boolean;
    function GetExpanded : Boolean;
    procedure SetHidden(aValue: Boolean);
  protected
    procedure SetTop(aValue: Integer); override;
    procedure SetLeft(aValue: Integer); override;
  public
    ShowInCollection : Boolean;
    Button_Expand : TKMButton;
    Label_Expand : TKMLabel;
    AddToTop : Integer;
    function AddChild(aChild: TKMControl): Integer; override;


    constructor Create(aParent: TKMPanel; aX, aY, aWidth, Height : integer; const aCaption : String; const IsHidden : Boolean = false); overload;

    Function GetBottom : integer;
    Function GetHeight : integer;
    procedure SetPos(aX, aY : Integer);
    Property OnRefresh : TNotifyEvent read fOnRefresh write fOnRefresh;
    Property OnRefresh2 : TNotifyEvent read fOnRefresh2 write fOnRefresh2;
    property Hidden : Boolean read GetHidden write SetHidden;
    property Expanded : Boolean read GetExpanded;

  end;
  TKMExpandPanelCollection = class(TKMPanel)
    private
      fPanels : array of TKMExpandPanel;
      fCount : Integer;
      fOnExpand : TNotifyEvent;
      procedure RefreshPanels(sender : TObject);
      function GetPanel(aIndex : Integer) : TKMExpandPanel;
      procedure SetOnExpand(aProc : TNotifyEvent);
    protected
      procedure SetLeft(aValue : Integer); override;
      procedure SetTop(aValue : Integer); override;
    public
      constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth : Integer);overload;
      property Count : Integer read fCount;
      Property Panel[aIndex : Integer] : TKMExpandPanel read GetPanel; default;
      function AddPanel(aHeight : integer; const aCaption : String; const IsHidden : Boolean = false) : Integer;
      Property OnExpand : TNotifyEvent read fOnExpand write SetOnExpand;
      procedure Refresh;

  end;


implementation
uses
  Math, SysUtils,
  KM_ResTypes, KM_ResFonts;


const
  EXPAND_BUTTON_SIZE = 20;

{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aScrollAxis: TKMScrollAxis;
                                aStyle: TKMButtonStyle; aScrollStyle: TKMScrollStyle = ssGame);
var
  decId, incId: Integer;
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  BackAlpha := 0.5;
  EdgeAlpha := 0.75;
  fScrollAxis := aScrollAxis;
  fMinValue := 0;
  fMaxValue := 10;
  fPosition := 0;
  fStyle    := aStyle;
  WheelStep := 1;

  if aScrollAxis = saVertical then
  begin
    fScrollDec := TKMButton.Create(Self, 0, 0, aWidth, aWidth, 591, rxGui, aStyle);
    fScrollInc := TKMButton.Create(Self, 0, aHeight-aWidth, aWidth, aWidth, 590, rxGui, aStyle);
    fScrollDec.Anchors := [anLeft, anTop, anRight];
    fScrollInc.Anchors := [anLeft, anRight, anBottom];
  end;
  if aScrollAxis = saHorizontal then
  begin
    if aScrollStyle = ssGame then
    begin
      decId := 2;
      incId := 3;
    end else begin
      decId := 674;
      incId := 675;
    end;
    fScrollDec := TKMButton.Create(Self, 0, 0, aHeight, aHeight, decId, rxGui, aStyle);
    fScrollInc := TKMButton.Create(Self, aWidth-aHeight, 0, aHeight, aHeight, incId, rxGui, aStyle);
    fScrollDec.Anchors := [anLeft, anTop, anBottom];
    fScrollInc.Anchors := [anTop, anRight, anBottom];
  end;
  fScrollDec.OnClick := DecPosition;
  fScrollDec.OnMouseWheel := MouseWheel;
  fScrollInc.OnClick := IncPosition;
  fScrollInc.OnMouseWheel := MouseWheel;
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetHeight(aValue: Integer);
begin
  inherited;

  //Update Thumb size
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetWidth(aValue: Integer);
begin
  inherited;

  //Update Thumb size
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetEnabled(aValue: Boolean);
begin
  inherited;
  fScrollDec.Enabled := Enabled;
  fScrollInc.Enabled := Enabled;
end;


procedure TKMScrollBar.SetMinValue(Value: Integer);
begin
  fMinValue := Max(0, Value);
  Enabled := (fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetMaxValue(Value: Integer);
begin
  fMaxValue := Max(0, Value);
  Enabled := (fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetPosition(Value: Integer);
begin
  fPosition := EnsureRange(Value, fMinValue, fMaxValue);
  UpdateThumbPos;
end;


procedure TKMScrollBar.IncPosition(Sender: TObject);
begin
  SetPosition(fPosition + WheelStep);

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender: TObject);
begin
  SetPosition(fPosition - WheelStep);

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMScrollBar.UpdateThumbPos;
begin
  fThumbPos := 0;

  if fMaxValue > fMinValue then
    case fScrollAxis of
      saVertical:   fThumbPos := (fPosition-fMinValue)*(Height-Width*2-fThumbSize) div (fMaxValue-fMinValue);
      saHorizontal: fThumbPos := (fPosition-fMinValue)*(Width-Height*2-fThumbSize) div (fMaxValue-fMinValue);
    end
  else
    case fScrollAxis of
      saVertical:   fThumbPos := Math.max((Height-Width*2-fThumbSize),0) div 2;
      saHorizontal: fThumbPos := Math.max((Width-Height*2-fThumbSize),0) div 2;
    end;
end;


procedure TKMScrollBar.UpdateThumbSize;
begin
  case fScrollAxis of
    saVertical:   fThumbSize := Math.max(0, (Height-2*Width)) div 4;
    saHorizontal: fThumbSize := Math.max(0, (Width-2*Height)) div 4;
  end;

  //If size has changed, then Pos needs to be updated as well (depends on it)
  UpdateThumbPos;
end;


procedure TKMScrollBar.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var
  T: Integer;
begin
  inherited;

  fOffset := 0;
  case fScrollAxis of
    saVertical:    begin
                      T := Y - AbsTop - Width - fThumbPos;
                      if InRange(T, 0, fThumbSize) then
                        fOffset := T - fThumbSize div 2;
                    end;
    saHorizontal:  begin
                      T := X - AbsLeft - Height - fThumbPos;
                      if InRange(T, 0, fThumbSize) then
                        fOffset := T - fThumbSize div 2;
                    end;
  end;

  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMScrollBar.MouseMove(X,Y: Integer; Shift: TShiftState);
var
  newPos: Integer;
  T: Integer;
begin
  inherited;
  if not (ssLeft in Shift) then Exit;

  newPos := fPosition;

  case fScrollAxis of
    saVertical:
      begin
        T := Y - fOffset - AbsTop - Width;
        if InRange(T, 0, Height - Width * 2) then
          newPos := Round(fMinValue+((T - fThumbSize / 2) / (Height-Width*2-fThumbSize)) * (fMaxValue - fMinValue) );
      end;

    saHorizontal:
      begin
        T := X - fOffset - AbsLeft - Height;
        if InRange(T, 0, Width - Height * 2) then
          newPos := Round(fMinValue+((T - fThumbSize / 2) / (Width-Height*2-fThumbSize)) * (fMaxValue - fMinValue) );
      end;
  end;

  if newPos <> fPosition then
  begin
    SetPosition(newPos);
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


function TKMScrollBar.DoHandleMouseWheelByDefault: Boolean;
begin
  Result := False;
end;


procedure TKMScrollBar.MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
begin
  inherited;

  if aHandled then Exit;

  aHandled := WheelSteps <> 0;

  if aHandled then
  begin
    Position := Position - WheelStep * WheelSteps;

    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


procedure TKMScrollBar.Paint;
var
  buttonState: TKMButtonStateSet;
begin
  inherited;

  case fScrollAxis of
    saVertical:   TKMRenderUI.WriteBevel(AbsLeft, AbsTop+Width, Width, Height - Width*2, EdgeAlpha, BackAlpha);
    saHorizontal: TKMRenderUI.WriteBevel(AbsLeft+Height, AbsTop, Width - Height*2, Height, EdgeAlpha, BackAlpha);
  end;

  if fMaxValue > fMinValue then
    buttonState := []
  else
    buttonState := [bsDisabled];

  if not (bsDisabled in buttonState) then //Only show thumb when usable
    case fScrollAxis of
      saVertical:   TKMRenderUI.Write3DButton(AbsLeft,AbsTop+Width+fThumbPos,Width,fThumbSize, fBaseLeft, fBaseTop,rxGui,0,$FFFF00FF,buttonState,fStyle);
      saHorizontal: TKMRenderUI.Write3DButton(AbsLeft+Height+fThumbPos,AbsTop,fThumbSize,Height, fBaseLeft, fBaseTop,rxGui,0,$FFFF00FF,buttonState,fStyle);
    end;
end;


{ TKMScrollPanel }
constructor TKMScrollPanel.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aScrollAxisSet: TKMScrollAxisSet;
                                  aStyle: TKMButtonStyle; aScrollStyle: TKMScrollStyle; aEnlargeParents: Boolean = False);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fHideScrolls := false;
  fPadding := KMRect(0, 0, 0, 0);

  fScrollAxisSet := aScrollAxisSet;

  fChildsPanel := TKMPanel.Create(Self, aLeft, aTop, aWidth, aHeight);
  fChildsPanel.AnchorsStretch;
  fChildsPanel.IsHitTestUseDrawRect := True; // We want DrawRect to be used on the ScrollPanel

  // We should create scroll bars on Parent panel, since we move panel there and back and scroll should not be scrolled as well
  fScrollBarH := TKMScrollBar.Create(aParent, aLeft, aTop + aHeight - 20, aWidth, 20, saHorizontal, aStyle, aScrollStyle);
  fScrollBarH.Hide;
  fScrollBarH.OnChange := ScrollChanged;
  fScrollBarH.WheelStep := 10;

  fScrollBarV := TKMScrollBar.Create(aParent, aLeft + aWidth - 20, aTop, 20, aHeight, saVertical, aStyle, aScrollStyle);
  fScrollBarV.Hide;
  fScrollBarV.OnChange := ScrollChanged;
  fScrollBarV.WheelStep := 10;

  IsHitTestUseDrawRect := True; // We want DrawRect to be used on the ScrollPanel

  fScrollV_PadTop := 0;
  fScrollV_PadBottom := 0;

//  if aEnlargeParents then
//  begin
//    if saHorizontal in aScrollAxisSet then
//      Enlarge(fScrollBarH);
//
//    if saVertical in aScrollAxisSet then
//      Enlarge(fScrollBarV);
//  end;
  DoScrollUpdate := true;
  fClipRect := KMRect(Left, Top, Right, Bottom);
end;


function TKMScrollPanel.AddChild(aChild: TKMControl): Integer;
begin
  aChild.IsHitTestUseDrawRect := True;
  if fChildsPanel = nil then
    Result := inherited AddChild(aChild)
  else
  begin
    Result := fChildsPanel.AddChild(aChild);
    if DoScrollUpdate then
    begin
      aChild.OnHeightChange := UpdateScrollV;
      aChild.OnWidthChange := UpdateScrollH;
      aChild.OnPositionSet := UpdateScrolls;
      aChild.OnChangeVisibility := UpdateScrolls;
      aChild.OnChangeEnableStatus := UpdateScrolls;
    end;
  end;
end;


//Update View is usefull when use Padding Left or Top
//To show panel properly initially. Beeter to invoke from Show Element
procedure TKMScrollPanel.UpdateVisibility;
begin
  inherited;
  if Visible
    and ((fPadding.Left > 0) or (fPadding.Top > 0)) then //Update visibility only when left/top padding > 0
  begin
    if saHorizontal in fScrollAxisSet then
    begin
      fScrollBarH.Position := fPadding.Left;
      ScrollChanged(fScrollBarH);
    end;

    if saVertical in fScrollAxisSet then
    begin
      fScrollBarV.Position := fPadding.Top;
      ScrollChanged(fScrollBarV);
    end;
  end;
end;


procedure TKMScrollPanel.MouseWheel(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
begin
  if (saVertical in fScrollAxisSet) and fScrollBarV.Visible then
    fScrollBarV.MouseWheel(Sender, WheelSteps, aHandled)
  else if (saHorizontal in fScrollAxisSet) and fScrollBarH.Visible then
    fScrollBarH.MouseWheel(Sender, WheelSteps, aHandled)
  else
    inherited;
end;


procedure TKMScrollPanel.ScrollChanged(Sender: TObject);
var
  oldValue: Integer;
begin
  if Sender = fScrollBarH then
  begin
    oldValue := Left;
    Left := fClipRect.Left - fScrollBarH.Position + fPadding.Left;
    //To compensate changes in SetLeft
    Inc(fClipRect.Left, oldValue - Left);
    Inc(fClipRect.Right, oldValue - Left);
  end else
  if Sender = fScrollBarV then
  begin
    oldValue := Top;
    Top := fClipRect.Top - fScrollBarV.Position + fPadding.Top;
    //To compensate changes in SetTop
    Inc(fClipRect.Top, oldValue - Top);
    Inc(fClipRect.Bottom, oldValue - Top);
  end;
end;

procedure TKMScrollPanel.SetHideScrolls(aValue: Boolean);
begin
  fHideScrolls := aValue;
  UpdateScrolls(nil);
end;
procedure TKMScrollPanel.UpdateScrolls(Sender: TObject; aValue: Boolean);
begin
  // No need to update scrolls if panel is not visible
  // In case we've just hide panel then DrawRect is RECT_ZERO and scrools will be set to 0 position
  if not Visible then Exit;

  if (Sender <> fScrollBarH) and (Sender <> fScrollBarV) then
    UpdateScrolls(nil);
end;


procedure TKMScrollPanel.UpdateScrollH(Sender: TObject; aValue: Integer);
begin
  if (Sender <> fScrollBarH) then
    UpdateScrollH;
end;


procedure TKMScrollPanel.UpdateScrollH;
begin
  if not (saHorizontal in fScrollAxisSet) then Exit;

  UpdateScrollH(GetChildsRect);
end;


procedure TKMScrollPanel.UpdateScrollH(aChildsRect: TKMRect);
var
  newPos: Integer;
  showScroll: Boolean;
begin
  if not (saHorizontal in fScrollAxisSet) then Exit;

  if fHideScrolls then
  begin
    fScrollBarH.Visible := false;
    Exit;
  end;

  showScroll := False;

  if aChildsRect.Width + fPadding.Left + fPadding.Right > fClipRect.Width then
  begin
    fScrollBarH.MaxValue := aChildsRect.Width - fClipRect.Width + fPadding.Left + fPadding.Right;
    newPos := fClipRect.Left - Left;

    if newPos > fScrollBarH.MaxValue then
      SetLeftSilently(Left + newPos - fScrollBarH.MaxValue); //Slightly move panel to the top, when resize near maxvalue position
    showScroll := True;
  end else begin
    fScrollBarH.Position := 0;
    if Left <> fClipRect.Left then
      SetLeftSilently(fClipRect.Left); //Set directrly to avoid SetLeft call
  end;

  fScrollBarH.Width := Width;
  if showScroll <> fScrollBarH.Visible then
  begin
    fScrollBarH.Visible := showScroll;
    fChildsPanel.Height := Height - 20*Byte(showScroll);
  end;
end;


procedure TKMScrollPanel.UpdateScrollV(Sender: TObject; aValue: Integer);
begin
  if (Sender <> fScrollBarV) then
    UpdateScrollV;
end;


procedure TKMScrollPanel.UpdateScrollV;
begin
  if not (saVertical in fScrollAxisSet) then Exit;

  UpdateScrollV(GetChildsRect);
end;


procedure TKMScrollPanel.UpdateScrollV(aChildsRect: TKMRect);
var
  newPos: Integer;
  showScroll: Boolean;
begin
  if not (saVertical in fScrollAxisSet) then Exit;

  if fHideScrolls then
  begin
    fScrollBarV.Visible := false;
    Exit;
  end;

  //Do not set Visible, avoid trigger OnChangeVisibility
  showScroll := False;

  if aChildsRect.Height + fPadding.Top + fPadding.Bottom > fClipRect.Height then
  begin
    fScrollBarV.MaxValue := aChildsRect.Height - fClipRect.Height + fPadding.Top + fPadding.Bottom;
    newPos := fClipRect.Top - Top;

    if newPos > fScrollBarV.MaxValue then
      SetTopSilently(Top + newPos - fScrollBarV.MaxValue); //Slightly move panel to the top, when resize near maxvalue position
    showScroll := True;
  end else
  begin
    fScrollBarV.Position := 0;
    if Top <> fClipRect.Top then
      SetTopSilently(fClipRect.Top); //Set directrly to avoid SetTop call
  end;
  //fScrollBarV.Left := self.Width - fScrollBarV.Width;
  fScrollBarV.Left := fClipRect.Left + Width + fScrollV_PadLeft;
  fScrollBarV.Height := Height - fScrollV_PadTop - fScrollV_PadBottom;
  if showScroll <> fScrollBarV.Visible then
  begin
    fScrollBarV.Visible := showScroll;
    fChildsPanel.Width := Width - 20*Byte(showScroll);
  end;
end;


procedure TKMScrollPanel.UpdateScrolls;
begin
  UpdateScrolls(nil);
end;


procedure TKMScrollPanel.UpdateScrolls(Sender: TObject);
var
  childsRect: TKMRect;
begin
  childsRect := GetChildsRect;

  UpdateScrollV(childsRect);
  UpdateScrollH(childsRect);
end;


procedure TKMScrollPanel.SetLeft(aValue: Integer);
var
  oldValue: Integer;
begin
  oldValue := Left;
  inherited;

  Inc(fClipRect.Left, Left - oldValue);
  Inc(fClipRect.Right, Left - oldValue);

  UpdateScrolls(nil);
end;


procedure TKMScrollPanel.SetScrollVPadBottom(const Value: Integer);
begin
  fScrollV_PadBottom := Value;
  fScrollBarV.Height := fScrollBarV.Height - Value;
end;


procedure TKMScrollPanel.SetScrollVPadLeft(const Value: Integer);
begin
  fScrollV_PadLeft := Value;
end;


procedure TKMScrollPanel.SetScrollVPadTop(const Value: Integer);
begin
  fScrollV_PadTop := Value;
  fScrollBarV.Top := fScrollBarV.Top + Value;
  fScrollBarV.Height := fScrollBarV.Height - Value;
end;


procedure TKMScrollPanel.SetTop(aValue: Integer);
var
  oldValue: Integer;
begin
  oldValue := Top;
  inherited;

  Inc(fClipRect.Top, Top - oldValue);
  Inc(fClipRect.Bottom, Top - oldValue);

  UpdateScrolls(nil);
end;


procedure TKMScrollPanel.SetVisible(aValue: Boolean);
begin
  inherited;

  // Hide ScrollBars with if we hide ScrollPanel
  if not aValue then
  begin
    fScrollBarH.Visible := aValue;
    fScrollBarV.Visible := aValue;
  end;
end;


procedure TKMScrollPanel.SetWidth(aValue: Integer);
var
  oldValue: Integer;
begin
  oldValue := Width;
  inherited;

  Inc(fClipRect.Right, Width - oldValue);

  fScrollBarV.Left := fClipRect.Left + Width + fScrollV_PadLeft;

  UpdateScrolls(nil);
end;


procedure TKMScrollPanel.Show;
begin
  inherited;

  // We can use Visible here, since Self is he parent of ChildsPanel
  fChildsPanel.Visible := True;
end;


procedure TKMScrollPanel.SetHeight(aValue: Integer);
var
  oldValue: Integer;
begin
  oldValue := Height;
  inherited;

  Inc(fClipRect.Bottom, Height - oldValue);

  fScrollBarH.Top := fClipRect.Top + Height;

  UpdateScrolls(nil);
end;


procedure TKMScrollPanel.SetHitable(const aValue: Boolean);
begin
  inherited;

  fChildsPanel.Hitable := aValue;
end;


function TKMScrollPanel.GetChildsRect: TKMRect;
var aRec : TKMRect;
  procedure AddPanelRect(aPanel : TKMPanel; aLeft, aTop : Integer );
  var I : Integer;
  begin
    if aPanel is TKMScrollPanel then
    begin
      if TKMScrollPanel(aPanel).ClipRect.Left + aLeft < aRec.Left then
        aRec.Left := TKMScrollPanel(aPanel).ClipRect.Left + aLeft;

      if TKMScrollPanel(aPanel).ClipRect.Top + aTop < aRec.Top then
        aRec.Top := TKMScrollPanel(aPanel).ClipRect.Top + aTop;

      if TKMScrollPanel(aPanel).ClipRect.Right + aLeft > aRec.Right then
        aRec.Right := TKMScrollPanel(aPanel).ClipRect.Right + aLeft;

      if TKMScrollPanel(aPanel).ClipRect.Bottom + aTop > aRec.Bottom then
        aRec.Bottom := TKMScrollPanel(aPanel).ClipRect.Bottom + aTop;
      Exit;
    end;

    for I := 0 to aPanel.ChildCount - 1 do
    begin
      if not aPanel.Childs[I].Visible then
        Continue;

      if aPanel.Childs[I] is TKMPanel then
      begin
        AddPanelRect(TKMPanel(aPanel.Childs[I]), aLeft + aPanel.Childs[I].Left, aTop + aPanel.Childs[I].Top);
        Continue;
      end;

      if aPanel.Childs[I].Left + aLeft < aRec.Left then
        aRec.Left := aPanel.Childs[I].Left + aLeft;

      if aPanel.Childs[I].Top + aTop < aRec.Top then
        aRec.Top := aPanel.Childs[I].Top + aTop;

      if aPanel.Childs[I].Right + aLeft > aRec.Right then
        aRec.Right := aPanel.Childs[I].Right + aLeft;

      if aPanel.Childs[I].Bottom + aTop > aRec.Bottom then
        aRec.Bottom := aPanel.Childs[I].Bottom + aTop;

    end;
  end;

{var
  I: Integer;}
begin
  aRec := KMRECT_ZERO;

  AddPanelRect(fChildsPanel, 0, 0);
  Result := aRec; //Zero rect, by default
  {for I := 0 to fChildsPanel.ChildCount - 1 do
  begin
    if not fChildsPanel.Childs[I].IsPainted then
      Continue;

    if fChildsPanel.Childs[I] is TKMPanel then
    begin
      AddPanelRect(fChildsPanel.Childs[I]);
      Continue;
    end;

    if fChildsPanel.Childs[I].Left < Result.Left then
      Result.Left := fChildsPanel.Childs[I].Left;

    if fChildsPanel.Childs[I].Top < Result.Top then
      Result.Top := fChildsPanel.Childs[I].Top;

    if fChildsPanel.Childs[I].Right > Result.Right then
      Result.Right := fChildsPanel.Childs[I].Right;

    if fChildsPanel.Childs[I].Bottom > Result.Bottom then
      Result.Bottom := fChildsPanel.Childs[I].Bottom;
  end;}
end;


function TKMScrollPanel.AllowScrollV: Boolean;
begin
  Result := saVertical in fScrollAxisSet;
end;


function TKMScrollPanel.AllowScrollH: Boolean;
begin
  Result := saHorizontal in fScrollAxisSet;
end;


function TKMScrollPanel.GetDrawRect: TKMRect;
begin
  Result := KMRect(AbsDrawLeft, AbsDrawTop, AbsDrawRight, AbsDrawBottom);
end;


function TKMScrollPanel.GetAbsDrawLeft: Integer;
begin
  Result := Parent.AbsLeft + fClipRect.Left;
end;


function TKMScrollPanel.GetAbsDrawTop: Integer;
begin
  Result := Parent.AbsTop + fClipRect.Top;
end;


function TKMScrollPanel.GetAbsDrawRight: Integer;
begin
  Result := Parent.AbsLeft + fClipRect.Right + 20*Byte(AllowScrollV and not fScrollBarV.Visible);
end;


function TKMScrollPanel.GetAbsDrawBottom: Integer;
begin
  Result := Parent.AbsTop + fClipRect.Bottom + 20*Byte(AllowScrollH and not fScrollBarH.Visible);
end;


procedure TKMScrollPanel.Paint;
begin
  TKMRenderUI.SetupClipX(AbsDrawLeft, AbsDrawRight);
  TKMRenderUI.SetupClipY(AbsDrawTop, AbsDrawBottom);

  inherited;

  TKMRenderUI.ReleaseClipY;
  TKMRenderUI.ReleaseClipX;
end;


procedure TKMScrollPanel.ClipY;
begin
   TKMRenderUI.SetupClipY(AbsDrawTop, AbsDrawBottom);
end;


procedure TKMScrollPanel.UnClipY;
begin
  TKMRenderUI.ReleaseClipY;
end;

constructor TKMExpandPanel.Create(aParent: TKMPanel; aX, aY, aWidth, Height : integer; const aCaption : String; const IsHidden : Boolean = false);
begin
  Inherited Create(aParent, aX, aY + EXPAND_BUTTON_SIZE, aWidth, Height);
  AddToTop := 0;
  Button_Expand := TKMButton.Create(aParent, aX, aY, EXPAND_BUTTON_SIZE, EXPAND_BUTTON_SIZE, '-', bsGame);
  Button_Expand.OnClick := ButtonExpandClick;
  fHidden := IsHidden;

  Label_Expand := TKMlabel.Create(aParent, aX + EXPAND_BUTTON_SIZE + 2, aY + 2, aWidth - EXPAND_BUTTON_SIZE, EXPAND_BUTTON_SIZE, aCaption,fntOutline,taLeft);
  Label_Expand.TextVAlign := tvaMiddle;
  Label_Expand.Hitable := false;
  fOnRefresh := nil;
  Refresh;
  ShowInCollection := true;
end;

function TKMExpandPanel.AddChild(aChild: TKMControl): Integer;
begin
  Result := Inherited;
  if aChild.Bottom > self.Height then
    self.Height := aChild.Bottom + 3;
end;

Procedure TKMExpandPanel.ButtonExpandClick(Sender: TObject);
begin
  fHidden := not fHidden;
  Refresh;
  if Assigned(fOnRefresh) then
    fOnRefresh(Sender);

  if Assigned(fOnRefresh2) then
    fOnRefresh2(Sender);
end;

procedure TKMExpandPanel.SetLeft(aValue: Integer);
begin
  Inherited;

  Button_Expand.Left := aValue;
  Label_Expand.Left := aValue + EXPAND_BUTTON_SIZE + 2;

end;

procedure TKMExpandPanel.SetTop(aValue: Integer);
begin
  Button_Expand.Top := aValue;
  Label_Expand.Top := aValue + 2;
  aValue := aValue + EXPAND_BUTTON_SIZE + AddToTop;
  Inherited;
end;

procedure TKMExpandPanel.Refresh;
begin

  if not fHidden then
    Button_Expand.Caption := '-'
  else
    Button_Expand.Caption := '+';

  if fHidden then
    self.Hide
  else
    self.Show;
end;

function TKMExpandPanel.GetHidden: Boolean;
begin
  Result := not (self.Visible or Button_Expand.Visible or Label_Expand.Visible)
end;

function TKMExpandPanel.GetExpanded: Boolean;
begin
  Result := not fHidden;
end;

procedure TKMExpandPanel.SetHidden(aValue: Boolean);

begin
  if aValue then
  begin
    //Self.Hide;
    Button_Expand.Hide;
    Label_Expand.Hide;
  end else
  begin
    //Self.Show;
    Button_Expand.Show;
    Label_Expand.Show;

    Refresh;

  end;
end;

function TKMExpandPanel.GetBottom: Integer;
begin
  if fHidden then
    Result :=  2 + self.Top + Abs(AddToTop)
  else
    Result := self.Top + self.Height;
end;

function TKMExpandPanel.GetHeight: Integer;
begin
  if fHidden then
    Result := 2
  else
    Result := self.Height;
end;

procedure TKMExpandPanel.SetPos(aX: Integer; aY: Integer);
begin
  if aX <> -1 then
    SetLeft(aX);

  if aY <> -1 then
    SetTop(aY);

end;

constructor TKMExpandPanelCollection.Create(aParent : TKMPanel; aLeft, aTop, aWidth : Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aParent.Height);

  fOnExpand := nil;
end;

procedure TKMExpandPanelCollection.SetOnExpand(aProc: TNotifyEvent);
var I : Integer;
begin
  fOnExpand := aProc;
  for I := 0 to High(fPanels) do
    fPanels[I].OnRefresh2 := fOnExpand;
end;

procedure TKMExpandPanelCollection.RefreshPanels(Sender : TObject);
var I, lastID : Integer;
begin
  if Length(fPanels) = 0 then
    Exit;

  lastID := -1;

  for I := 0 to High(fPanels) do
  begin
    if not fPanels[I].ShowInCollection then
      Continue;
    if fPanels[I].Hidden then
      Continue;

    if lastID = -1 then
    begin
      fPanels[I].Top := 0;
      LastID := I;
    end else
    begin
      fPanels[I].Top := fPanels[LastID].GetBottom;
      LastID := I;
    end;

  end;


end;

function TKMExpandPanelCollection.AddPanel(aHeight : integer; const aCaption : String; const IsHidden : Boolean = false) : Integer;
begin
  fCount := fCount + 1;
  SetLength(fPanels, fCount);
  fPanels[fCount - 1] := TKMExpandPanel.Create(self, Left, Top, Width, aHeight, aCaption, IsHidden);
  fPanels[fCount - 1].OnRefresh := RefreshPanels;

  if Assigned(fOnExpand) then
    fPanels[fCount - 1].OnRefresh2 := fOnExpand;

  Result := fCount - 1;
  RefreshPanels(nil);
end;

function TKMExpandPanelCollection.GetPanel(aIndex: Integer): TKMExpandPanel;
begin
  Assert(aIndex < length(fPanels), 'Can''t get panel group item for index ' + IntToStr(aIndex));

  Result := fPanels[aIndex];
end;

procedure TKMExpandPanelCollection.SetLeft(aValue : Integer);
begin
  Inherited;
  RefreshPanels(nil);

end;

procedure TKMExpandPanelCollection.SetTop(aValue : Integer);
begin
  Inherited;

  RefreshPanels(self);

end;

procedure TKMExpandPanelCollection.Refresh;
begin
  RefreshPanels(nil);
end;

end.

