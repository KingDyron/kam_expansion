unit KM_GUIMapEdPlayerBlockTrade;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsScroll,
   KM_Pics, KM_InterfaceGame, KM_ResWares, KM_HandTypes;

type
  TKMButtonFlatBlockWare = class(TKMButtonFlat)
  public
    Block : TKMHandWareTradeLock;
    procedure Paint; override;
  end;

  TKMMapEdPlayerBlockTrade = class
  private
    procedure Player_BlockTradeClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_BlockTradeOver(Sender: TObject; Shift: TShiftState);
    procedure Player_BlockTradeRefresh;

  protected
    Panel_BlockTrade: TKMScrollPanel;
    Button_BlockTrade: array of TKMButtonFlatBlockWare;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  Math,
  KM_HandsCollection, KM_ResTexts,
  KM_Resource, KM_RenderUI, KM_ResFonts,
  KM_ResTypes,
  KM_CommonUtils;

{ TKMMapEdPlayerBlockTrade }
constructor TKMMapEdPlayerBlockTrade.Create(aParent: TKMPanel);
var
  I, J, K, C, top: Integer;
begin
  inherited Create;

  Panel_BlockTrade := TKMScrollPanel.Create(aParent, 9, 28 + 30, aParent.Width - 9, aParent.Height - 40, [saVertical], bsMenu, ssCommon);
  Panel_BlockTrade.ScrollV.Left := Panel_BlockTrade.ScrollV.Left + 20;


  with TKMLabel.Create(Panel_BlockTrade, 0, PAGE_TITLE_Y, Panel_BlockTrade.Width, 0, gResTexts[TX_MAPED_BLOCK_TRADE], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  J := 0;
  K := 0;
  top := 30;
  C := 0;
  for I := 1 to STORE_RES_COUNT do
  begin

    if StoreResType[I] = wtNone then
    begin
      C := 0;
      if J > 0 then
        top := Button_BlockTrade[J - 1].Bottom;

      with TKMLabel.Create(Panel_BlockTrade, 0, top, 9 + 37 * 5, 15, gResTexts[1657 + K], fntOutline, taCenter) do
        Hitable := false;
      Inc(top, 17);
      Inc(K);
      Continue;
    end;
    If J >= length(Button_BlockTrade) then
    SetLength(Button_BlockTrade, J + 20);

    Button_BlockTrade[J] := TKMButtonFlatBlockWare.Create(Panel_BlockTrade, 9 + (C mod 5)*37, top + (C div 5)*37,32,36, 0);
    Button_BlockTrade[J].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_BlockTrade[J].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_BlockTrade[J].OnMouseDown := Player_BlockTradeClick;
    Button_BlockTrade[J].Tag := byte(StoreResType[I]);
    Button_BlockTrade[J].Tag2 := BUTTON_BLOCK_WARE_TRADE_TAG_2;
    Button_BlockTrade[J].OnMouseOver := Player_BlockTradeOver;

    Inc(J);
    Inc(C);
  end;
  SetLength(Button_BlockTrade, J);
end;


procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I: Integer;
  WT: TKMWareType;
begin
  I := TKMButtonFlat(Sender).Tag;
  WT := TKMWareType(I);
  I := byte(gMySpectator.Hand.Locks.WareTradeLock[WT]);
  IncLoop(I, 0, Byte(high(TKMHandWareTradeLock)), IfThen(ssRight in Shift, -1, 1) );
  gMySpectator.Hand.Locks.WareTradeLock[WT] := TKMHandWareTradeLock(I);

  Player_BlockTradeRefresh;
end;

procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeOver(Sender: TObject; Shift: TShiftState);
var CtrlDown : TKMControl;
  W1, W2 : TKMWareType;
begin
  CtrlDown := Panel_BlockTrade.MasterControl.CtrlDown;

  if CtrlDown = nil then
    Exit;
  if not (CtrlDown is TKMButtonFlat) then
    Exit;
  if Sender = nil then
    Exit;

  if (CtrlDown.Tag2 = BUTTON_BLOCK_WARE_TRADE_TAG_2) and (TKMButtonFlat(Sender).Tag2 = BUTTON_BLOCK_WARE_TRADE_TAG_2) then
  begin
    W1 := TKMWareType(CtrlDown.Tag);
    W2 := TKMWareType(TKMButtonFlat(Sender).Tag);

    if gMySpectator.Hand.Locks.WareTradeLock[W1] <> gMySpectator.Hand.Locks.WareTradeLock[W2] then
    begin
      gMySpectator.Hand.Locks.WareTradeLock[W2] := gMySpectator.Hand.Locks.WareTradeLock[W1];
      Player_BlockTradeRefresh;
    end;

  end;




end;

procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeRefresh;
var
  I: Integer;
  WT: TKMWareType;
begin
  for I := 0 to high(Button_BlockTrade) do
  begin
    WT := TKMWareType(Button_BlockTrade[I].Tag);
    if not( WT in WARES_VALID) then
      Continue;
    Button_BlockTrade[I].Block := gMySpectator.Hand.Locks.WareTradeLock[WT];
  end;
end;


procedure TKMMapEdPlayerBlockTrade.Hide;
begin
  Panel_BlockTrade.Hide;
end;


procedure TKMMapEdPlayerBlockTrade.Show;
begin
  Player_BlockTradeRefresh;
  Panel_BlockTrade.Show;
end;


function TKMMapEdPlayerBlockTrade.Visible: Boolean;
begin
  Result := Panel_BlockTrade.Visible;
end;

procedure TKMButtonFlatBlockWare.Paint;
const
  MARGIN = 0;
begin

  case Block of
    wlBothWays: begin
                  TKMRenderUI.WritePicture(AbsLeft + MARGIN, AbsTop + MARGIN, 0, 0, [anLeft, anTop], rxGuiMain, 128, Enabled);
                  TKMRenderUI.WritePicture(AbsRight - MARGIN, AbsTop + MARGIN, 0, 0, [anRight, anTop], rxGuiMain, 129, Enabled);
                end;
    wlFromOnly: TKMRenderUI.WritePicture(AbsLeft + MARGIN, AbsTop + MARGIN, 0, 0, [anLeft, anTop], rxGuiMain, 128, Enabled);
    wlToOnly: TKMRenderUI.WritePicture(AbsRight - MARGIN, AbsTop + MARGIN, 0, 0, [anRight, anTop], rxGuiMain, 129, Enabled);
  end;
  Inherited;
  case Block of
    wlNotVisible: TKMRenderUI.WritePicture(AbsRight - 16, AbsBottom - 16, 16, 16, [], rxGuiMain, 91, Enabled);
    wlBlocked: TKMRenderUI.WritePicture(AbsRight - 16, AbsBottom - 16, 16, 16, [], rxGuiMain, 32, Enabled);
  end;
end;


end.
