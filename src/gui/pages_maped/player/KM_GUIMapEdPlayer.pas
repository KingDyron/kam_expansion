unit KM_GUIMapEdPlayer;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_ControlsBase,
   KM_Defaults, KM_Pics,
   KM_InterfaceDefaults,
   KM_GUIMapEdPlayerBlockHouse,
   KM_GUIMapEdPlayerBlockTrade,
   KM_GUIMapEdPlayerBlockUnit,
   KM_GUIMapEdPlayerColors,
   KM_GUIMapEdPlayerGoals,
   KM_GUIMapEdPlayerView,
   KM_GUIMapEdPlayerBlockDev;

type
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptBlockUnit, ptBlockDev, ptView, ptAdd);

  TKMMapEdPlayer = class(TKMMapEdMenuPage)
  private
    fOnPageChange: TNotifyEvent;

    fGuiPlayerBlockHouse: TKMMapEdPlayerBlockHouse;
    fGuiPlayerBlockTrade: TKMMapEdPlayerBlockTrade;
    fGuiPlayerBlockUnit: TKMMapEdPlayerBlockUnit;
    fGuiPlayerColors: TKMMapEdPlayerColors;
    fGuiPlayerView: TKMMapEdPlayerView;
    fGuiPlayerAdditional: TKMMapEdPlayerAdditional;
    fGuiPlayerBlockDevs: TKMMapEdPlayerBlockDevs;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Player: TKMPanel;
    Button_Player: array [TKMPlayerTab] of TKMButton;
    procedure DoShowSubMenu(aIndex: Byte); override;
  public
    GuiPlayerGoals: TKMMapEdPlayerGoals;
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure Show(aPage: TKMPlayerTab);
    function IsVisible(aPage: TKMPlayerTab): Boolean;
    function Visible: Boolean; override;
    procedure ChangePlayer;
    procedure UpdatePlayerColor;
    procedure UpdateHotkeys;
    procedure UpdateState;
  end;


implementation
uses
  KM_HandsCollection,
  KM_ResTexts, KM_ResTypes,
  KM_Cursor,
  KM_RenderUI, KM_InterfaceGame, KM_Utils;


{ TKMMapEdPlayer }
constructor TKMMapEdPlayer.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TAB_GLYPH: array [TKMPlayerTab] of Word    = (8,         1159,     38,    327,   141,     141,   393, 754);
  TAB_RXX  : array [TKMPlayerTab] of TRXType = (rxGuiMain, rxHouses, rxGui, rxGui, rxGui,   rxGui, rxGui, rxGui);
var
  PT: TKMPlayerTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Player := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Player.Anchors := [anLeft, anTop, anBottom];

  for PT := Low(TKMPlayerTab) to High(TKMPlayerTab) do
  begin
    Button_Player[PT] := TKMButton.Create(Panel_Player, 9 + 45 + SMALL_PAD_W * (Byte(PT) mod 4), (byte(PT) div 4) * SMALL_PAD_W, SMALL_PAD_W, SMALL_TAB_H,  TAB_GLYPH[PT], TAB_RXX[PT], bsPaper);
    Button_Player[PT].OnClick := PageChange;
  end;

  GuiPlayerGoals := TKMMapEdPlayerGoals.Create(Panel_Player);
  fGuiPlayerColors := TKMMapEdPlayerColors.Create(Panel_Player);
  fGuiPlayerBlockHouse := TKMMapEdPlayerBlockHouse.Create(Panel_Player);
  fGuiPlayerBlockTrade := TKMMapEdPlayerBlockTrade.Create(Panel_Player);
  fGuiPlayerBlockUnit := TKMMapEdPlayerBlockUnit.Create(Panel_Player);
  fGuiPlayerView := TKMMapEdPlayerView.Create(Panel_Player);
  fGuiPlayerAdditional := TKMMapEdPlayerAdditional.Create(Panel_Player);
  fGuiPlayerBlockDevs := TKMMapEdPlayerBlockDevs.Create(Panel_Player);
end;


destructor TKMMapEdPlayer.Destroy;
begin
  GuiPlayerGoals.Free;
  fGuiPlayerColors.Free;
  fGuiPlayerBlockHouse.Free;
  fGuiPlayerBlockTrade.Free;
  fGuiPlayerBlockUnit.Free;
  fGuiPlayerView.Free;
  fGuiPlayerAdditional.Free;
  //fGuiPlayerBlockDevs.Free;//it's destroyed in panels
  inherited;
end;


procedure TKMMapEdPlayer.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gCursor.Mode := cmNone;

  //Hide existing pages
  GuiPlayerGoals.Hide;
  fGuiPlayerColors.Hide;
  fGuiPlayerBlockHouse.Hide;
  fGuiPlayerBlockTrade.Hide;
  fGuiPlayerBlockUnit.Hide;
  fGuiPlayerView.Hide;
  fGuiPlayerAdditional.Hide;
  fGuiPlayerBlockDevs.Hide;

  if (Sender = Button_Player[ptGoals]) then
    GuiPlayerGoals.Show
  else
  if (Sender = Button_Player[ptColor]) then
    fGuiPlayerColors.Show
  else
  if (Sender = Button_Player[ptBlockHouse]) then
    fGuiPlayerBlockHouse.Show
  else
  if (Sender = Button_Player[ptBlockTrade]) then
    fGuiPlayerBlockTrade.Show
  else
  if (Sender = Button_Player[ptBlockUnit]) then
    fGuiPlayerBlockUnit.Show
  else
  if (Sender = Button_Player[ptBlockDev]) then
    fGuiPlayerBlockDevs.Show
  else
  if (Sender = Button_Player[ptView]) then
    fGuiPlayerView.Show
  else
  if (Sender = Button_Player[ptAdd]) then
    fGuiPlayerAdditional.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdPlayer.Show(aPage: TKMPlayerTab);
begin
  case aPage of
    ptGoals:      GuiPlayerGoals.Show;
    ptColor:      fGuiPlayerColors.Show;
    ptBlockHouse: fGuiPlayerBlockHouse.Show;
    ptBlockTrade: fGuiPlayerBlockTrade.Show;
    ptBlockUnit:  fGuiPlayerBlockUnit.Show;
    ptBlockDev:   fGuiPlayerBlockDevs.Show;
    ptView:       fGuiPlayerView.Show;
    ptAdd:       fGuiPlayerAdditional.Show;

  end;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdPlayer.DoShowSubMenu(aIndex: Byte);
begin
  inherited;

  if (aIndex in [Byte(Low(TKMPlayerTab))..Byte(High(TKMPlayerTab))])
    and Button_Player[TKMPlayerTab(aIndex)].Enabled then
  begin
    PageChange(nil); //Hide existing pages
    Show(TKMPlayerTab(aIndex));
  end;
end;


function TKMMapEdPlayer.Visible: Boolean;
begin
  Result := Panel_Player.Visible;
end;


function TKMMapEdPlayer.IsVisible(aPage: TKMPlayerTab): Boolean;
begin
  case aPage of
    ptGoals:      Result := GuiPlayerGoals.Visible;
    ptColor:      Result := fGuiPlayerColors.Visible;
    ptBlockHouse: Result := fGuiPlayerBlockHouse.Visible;
    ptBlockTrade: Result := fGuiPlayerBlockTrade.Visible;
    ptBlockUnit:  Result := fGuiPlayerBlockUnit.Visible;
    ptBlockDev:   Result := fGuiPlayerBlockDevs.Visible;
    ptView:       Result := fGuiPlayerView.Visible;
    ptAdd:       Result := fGuiPlayerAdditional.Visible;
    else          Result := False;
  end;
end;


procedure TKMMapEdPlayer.ChangePlayer;
begin
  if GuiPlayerGoals.Visible then GuiPlayerGoals.Show;
  if fGuiPlayerColors.Visible then fGuiPlayerColors.UpdatePlayer;
  if fGuiPlayerBlockHouse.Visible then fGuiPlayerBlockHouse.Show;
  if fGuiPlayerBlockTrade.Visible then fGuiPlayerBlockTrade.Show;
  if fGuiPlayerBlockUnit.Visible then fGuiPlayerBlockUnit.Show;
  if fGuiPlayerView.Visible then fGuiPlayerView.Show;
  if fGuiPlayerAdditional.Visible then fGuiPlayerAdditional.Show;
  if fGuiPlayerBlockDevs.Visible then fGuiPlayerBlockDevs.Show;
  UpdatePlayerColor;
end;


procedure TKMMapEdPlayer.UpdateHotkeys;
const
  TAB_HINT : array [TKMPlayerTab] of Word = (
    TX_MAPED_GOALS,
    TX_MAPED_PLAYER_COLORS,
    TX_MAPED_BLOCK_HOUSES,
    TX_MAPED_BLOCK_TRADE,
    TX_MAPED_BLOCK_UNITS,
    TX_MAPED_BLOCK_UNITS,
    TX_MAPED_FOG,
    1791);
var
  PT: TKMPlayerTab;
begin
  for PT := Low(TKMPlayerTab) to High(TKMPlayerTab) do
    If ord(PT) <= 6 then
    Button_Player[PT].Hint := GetHintWHotkey(TAB_HINT[PT], MAPED_SUBMENU_HOTKEYS[Ord(PT)]);
end;


procedure TKMMapEdPlayer.UpdatePlayerColor;
begin
  Button_Player[ptColor].FlagColor := gMySpectator.Hand.FlagColor;
  Button_Player[ptBlockUnit].FlagColor := gMySpectator.Hand.FlagColor;

  fGuiPlayerView.UpdatePlayerColor;
  fGuiPlayerBlockUnit.UpdatePlayerColor;
end;


procedure TKMMapEdPlayer.UpdateState;
begin
  fGuiPlayerView.UpdateState;
end;


end.
