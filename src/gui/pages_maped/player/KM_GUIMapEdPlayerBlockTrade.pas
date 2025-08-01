unit KM_GUIMapEdPlayerBlockTrade;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_Controls, KM_ControlsBase, KM_ControlsScroll,
   KM_Pics, KM_InterfaceGame, KM_ResWares;

type
  TKMMapEdPlayerBlockTrade = class
  private
    procedure Player_BlockTradeClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
    procedure Player_BlockTradeOver(Sender: TObject; Shift: TShiftState);
    procedure Player_BlockTradeRefresh;

  protected
    Panel_BlockTrade: TKMScrollPanel;
    Button_BlockTrade: array of TKMButtonFlat;
    Image_BlockTrade: array of TKMImage;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts,
  KM_Resource, KM_RenderUI, KM_ResFonts,
  KM_ResTypes;


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

    SetLength(Button_BlockTrade, J + 1);
    SetLength(Image_BlockTrade, J + 1);

    Button_BlockTrade[J] := TKMButtonFlat.Create(Panel_BlockTrade, 9 + (C mod 5)*37, top + (C div 5)*37,33,33, 0);
    Button_BlockTrade[J].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_BlockTrade[J].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_BlockTrade[J].OnMouseDown := Player_BlockTradeClick;
    Button_BlockTrade[J].Tag := byte(StoreResType[I]);
    Button_BlockTrade[J].Tag2 := BUTTON_BLOCK_WARE_TRADE_TAG_2;
    Button_BlockTrade[J].OnMouseOver := Player_BlockTradeOver;


    Image_BlockTrade[J] := TKMImage.Create(Panel_BlockTrade, 9 + (C mod 5)*37 + 15, top + (C div 5)*37 + 15, 16, 16, 0, rxGuiMain);
    Image_BlockTrade[J].Hitable := False;
    Image_BlockTrade[J].ImageCenter;

    Inc(J);
    Inc(C);

  end;
end;


procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeClick(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var
  I: Integer;
  WT: TKMWareType;
begin
  I := TKMButtonFlat(Sender).Tag;
  WT := TKMWareType(I);

  gMySpectator.Hand.Locks.AllowToTrade[WT] := not gMySpectator.Hand.Locks.AllowToTrade[WT];

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

    if gMySpectator.Hand.Locks.AllowToTrade[W1] <> gMySpectator.Hand.Locks.AllowToTrade[W2] then
    begin
      gMySpectator.Hand.Locks.AllowToTrade[W2] := gMySpectator.Hand.Locks.AllowToTrade[W1];
      Player_BlockTradeRefresh;
    end;

  end;




end;

procedure TKMMapEdPlayerBlockTrade.Player_BlockTradeRefresh;
var
  I: Integer;
  WT: TKMWareType;
begin
  for I := 0 to high(Image_BlockTrade) do
  begin
    WT := TKMWareType(Button_BlockTrade[I].Tag);
    if not( WT in WARES_VALID) then
      Continue;

    if gMySpectator.Hand.Locks.AllowToTrade[WT] then
      Image_BlockTrade[I].TexID := 0
    else
      Image_BlockTrade[I].TexID := 32; //Red cross
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


end.
