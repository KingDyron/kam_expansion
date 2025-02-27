unit KM_GUIMapEdTown;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_Controls, KM_ControlsBase,
  KM_Defaults, KM_Pics,
  KM_InterfaceDefaults,
  KM_GUIMapEdTownHouses,
  KM_GUIMapEdTownUnits,
  KM_GUIMapEdTownScript,
  KM_GUIMapEdTownDefence,
  KM_GUIMapEdTownOffence,
  KM_GUIMapEdTownAnimals;

type
  TKMTownTab = (ttHouses, ttUnits, ttWarriors, ttAnimals, ttScript, ttDefences, ttOffence);

  TKMMapEdTown = class(TKMMapEdMenuPage)
  private
    fOnPageChange: TNotifyEvent;

    fGuiHouses: TKMMapEdTownHouses;
    fGuiUnits: TKMMapEdTownUnits;
    fGuiWarriors: TKMMapEdTownWarriors;
    fGuiScript: TKMMapEdTownScript;
    fGuiDefence: TKMMapEdTownDefence;
    fGuiOffence: TKMMapEdTownOffence;
    fGuiAnimals: TKMMapEdTownAnimals;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Town: TKMPanel;
    Button_Town: array [TKMTownTab] of TKMButton;
    procedure DoShowSubMenu(aIndex: Byte); override;
    procedure DoExecuteSubMenuAction(aIndex: Byte; var aHandled: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    property GuiHouses: TKMMapEdTownHouses read fGuiHouses;
    property GuiDefence: TKMMapEdTownDefence read fGuiDefence;
    property GuiOffence: TKMMapEdTownOffence read fGuiOffence;

    procedure Show(aPage: TKMTownTab);
    function IsVisible(aPage: TKMTownTab): Boolean;
    function Visible: Boolean; override;
    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
    procedure ChangePlayer;
    procedure UpdatePlayerColor;
    procedure UpdateHotkeys;
    procedure UpdateState;
    procedure UpdateStateIdle;
  end;


implementation
uses
  KM_Game,
  KM_HandsCollection,
  KM_InterfaceGame,
  KM_ResTexts, KM_ResTypes,
  KM_Cursor,
  KM_RenderUI,
  KM_Utils;

{ TKMMapEdTown }
constructor TKMMapEdTown.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TAB_GLYPH: array [TKMTownTab] of Word    = (391,   141,   61,    915,    62,        43,    53);
  TAB_RXX  : array [TKMTownTab] of TRXType = (rxGui, rxGui, rxGui, rxGui, rxGuiMain, rxGui, rxGui);
var
  TT: TKMTownTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Town := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Town.Anchors := [anLeft, anTop, anBottom];

  for TT := Low(TKMTownTab) to High(TKMTownTab) do
  begin
    Button_Town[TT] := TKMButton.Create(Panel_Town, 9 + SMALL_PAD_W * Byte(TT), 0, SMALL_TAB_W, SMALL_TAB_H, TAB_GLYPH[TT], TAB_RXX[TT], bsPaper);
    Button_Town[TT].OnClick := PageChange;
  end;

  fGuiHouses := TKMMapEdTownHouses.Create(Panel_Town);
  fGuiUnits := TKMMapEdTownUnits.Create(Panel_Town);
  fGuiWarriors := TKMMapEdTownWarriors.Create(Panel_Town);
  fGuiScript := TKMMapEdTownScript.Create(Panel_Town);
  fGuiDefence := TKMMapEdTownDefence.Create(Panel_Town);
  fGuiOffence := TKMMapEdTownOffence.Create(Panel_Town);
  fGuiAnimals := TKMMapEdTownAnimals.Create(Panel_Town);
end;


destructor TKMMapEdTown.Destroy;
begin
  fGuiHouses.Free;
  fGuiUnits.Free;
  fGuiWarriors.Free;
  fGuiScript.Free;
  fGuiDefence.Free;
  fGuiOffence.Free;
  fGuiAnimals.Free;
  inherited;
end;


procedure TKMMapEdTown.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gCursor.Mode := cmNone;

  //Hide existing pages
  fGuiHouses.Hide;
  fGuiUnits.Hide;
  fGuiWarriors.Hide;
  fGuiScript.Hide;
  fGuiDefence.Hide;
  fGuiOffence.Hide;
  fGuiAnimals.Hide;

  if (Sender = Button_Town[ttHouses]) then
    fGuiHouses.Show
  else
  if (Sender = Button_Town[ttUnits]) then
    fGuiUnits.Show
  else
  if (Sender = Button_Town[ttWarriors]) then
    fGuiWarriors.Show
  else
  if (Sender = Button_Town[ttScript]) then
    fGuiScript.Show
  else
  if (Sender = Button_Town[ttDefences]) then
    fGuiDefence.Show
  else
  if (Sender = Button_Town[ttOffence]) then
    fGuiOffence.Show
  else
  if (Sender = Button_Town[ttAnimals]) then
    fGuiAnimals.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdTown.Show(aPage: TKMTownTab);
begin
  case aPage of
    ttHouses:   fGuiHouses.Show;
    ttUnits:    fGuiUnits.Show;
    ttWarriors: fGuiWarriors.Show;
    ttScript:   fGuiScript.Show;
    ttDefences: fGuiDefence.Show;
    ttOffence:  fGuiOffence.Show;
    ttAnimals:  fGuiAnimals.Show;
  end;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdTown.DoShowSubMenu(aIndex: Byte);
begin
  inherited;

  if (aIndex in [Byte(Low(TKMTownTab))..Byte(High(TKMTownTab))])
    and Button_Town[TKMTownTab(aIndex)].Enabled then
  begin
    PageChange(nil); //Hide existing pages
    Show(TKMTownTab(aIndex));
  end;
end;


procedure TKMMapEdTown.DoExecuteSubMenuAction(aIndex: Byte; var aHandled: Boolean);
begin
  inherited;

  fGuiHouses.ExecuteSubMenuAction(aIndex, aHandled);
  fGuiUnits.ExecuteSubMenuAction(aIndex, aHandled);
  fGuiWarriors.ExecuteSubMenuAction(aIndex, aHandled);
  fGuiScript.ExecuteSubMenuAction(aIndex, aHandled);
  fGuiDefence.ExecuteSubMenuAction(aIndex, aHandled);
  fGuiOffence.ExecuteSubMenuAction(aIndex, aHandled);
  //fGuiAnimals.ExecuteSubMenuAction(aIndex, aHandled);
end;


function TKMMapEdTown.Visible: Boolean;
begin
  Result := Panel_Town.Visible;
end;


procedure TKMMapEdTown.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  fGuiScript.KeyDown(Key, Shift, aHandled);
end;


function TKMMapEdTown.IsVisible(aPage: TKMTownTab): Boolean;
begin
  case aPage of
    ttHouses:   Result := fGuiHouses.Visible;
    ttUnits:    Result := fGuiUnits.Visible;
    ttScript:   Result := fGuiScript.Visible;
    ttDefences: Result := fGuiDefence.Visible;
    ttOffence:  Result := fGuiOffence.Visible;
    ttAnimals:  Result := fGuiAnimals.Visible;
    else        Result := False;
  end;
end;


procedure TKMMapEdTown.ChangePlayer;
var
  isAI: Boolean;
begin
  isAI := gGame.MapEditor.PlayerClassicAI[gMySpectator.HandID] or gGame.MapEditor.PlayerAdvancedAI[gMySpectator.HandID];

  Button_Town[ttScript].Enabled := isAI;
  Button_Town[ttDefences].Enabled := isAI;
  Button_Town[ttOffence].Enabled := isAI;

  if not isAi and (fGuiScript.Visible or fGuiDefence.Visible or fGuiOffence.Visible) then
    PageChange(Button_Town[ttHouses]);

  if fGuiScript.Visible then fGuiScript.Show;
  if fGuiDefence.Visible then fGuiDefence.Show;
  if fGuiOffence.Visible then fGuiOffence.Show;

  UpdatePlayerColor;
end;


procedure TKMMapEdTown.UpdateHotkeys;
const
  TAB_HINT : array [TKMTownTab] of Word = (
    TX_MAPED_VILLAGE,
    TX_MAPED_UNITS,
    2028,
    2027,
    TX_MAPED_AI_TITLE,
    TX_MAPED_AI_DEFENSE_OPTIONS,
    TX_MAPED_AI_ATTACK);
var
  TT: TKMTownTab;
begin
  for TT := Low(TKMTownTab) to High(TKMTownTab) do
    Button_Town[TT].Hint := GetHintWHotkey(TAB_HINT[TT], MAPED_SUBMENU_HOTKEYS[Ord(TT)]);

  fGuiHouses.UpdateHotkeys;
  fGuiUnits.UpdateHotkeys;
  fGuiDefence.UpdateHotkeys;
  fGuiOffence.UpdateHotkeys;
  fGuiScript.UpdateHotkeys;
end;


procedure TKMMapEdTown.UpdatePlayerColor;
begin
  //Update colors
  Button_Town[ttUnits].FlagColor := gMySpectator.Hand.FlagColor;
  Button_Town[ttWarriors].FlagColor := gMySpectator.Hand.FlagColor;
  fGuiUnits.UpdatePlayerColor;
  fGuiWarriors.UpdatePlayerColor;
end;


procedure TKMMapEdTown.UpdateState;
begin
  fGuiHouses.UpdateState;
  fGuiUnits.UpdateState;
  fGuiWarriors.UpdateState;
  fGuiScript.UpdateState;
  fGuiDefence.UpdateState;
end;


procedure TKMMapEdTown.UpdateStateIdle;
begin
  fGuiHouses.UpdateStateIdle;
end;


end.
