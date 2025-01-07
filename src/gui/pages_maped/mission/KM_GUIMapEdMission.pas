unit KM_GUIMapEdMission;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_ControlsBase,
   KM_InterfaceDefaults,
   KM_GUIMapEdMissionMode,
   KM_GUIMapEdMissionAlliances,
   KM_GUIMapEdMissionPlayers;

type
  TKMMissionTab = (mtMode, mtPlayers, mtAlliances);

  TKMMapEdMission = class(TKMMapEdMenuPage)
  private
    fOnPageChange: TNotifyEvent;

    fGuiMissionMode: TKMMapEdMissionMode;
    fGuiMissionAlliances: TKMMapEdMissionAlliances;
    fGuiMissionPlayers: TKMMapEdMissionPlayers;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Mission: TKMPanel;
    Button_Mission: array [TKMMissionTab] of TKMButton;
    procedure DoShowSubMenu(aIndex: Byte); override;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);

    property GuiMissionPlayers: TKMMapEdMissionPlayers read fGuiMissionPlayers;
    procedure Show(aPage: TKMMissionTab);
    function IsVisible(aPage: TKMMissionTab): Boolean;
    function Visible: Boolean; override;

    procedure UpdateHotkeys;
  end;


implementation
uses
  KM_ResTexts, KM_ResTypes,
  KM_Cursor, KM_RenderUI, KM_InterfaceGame, KM_Defaults, KM_Utils;

{ TKMMapEdMission }
constructor TKMMapEdMission.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TAB_GLYPH: array [TKMMissionTab] of Word    = (41, 656, 386);
var
  MT: TKMMissionTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Mission := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Mission.Anchors := [anLeft, anTop, anBottom];

  for MT := Low(TKMMissionTab) to High(TKMMissionTab) do
  begin
    Button_Mission[MT] := TKMButton.Create(Panel_Mission, 9 + SMALL_PAD_W * Byte(MT), 0, SMALL_TAB_W, SMALL_TAB_H,  TAB_GLYPH[MT], rxGui, bsPaper);
    Button_Mission[MT].OnClick := PageChange;
  end;

  fGuiMissionMode := TKMMapEdMissionMode.Create(Panel_Mission);
  fGuiMissionAlliances := TKMMapEdMissionAlliances.Create(Panel_Mission);
  fGuiMissionPlayers := TKMMapEdMissionPlayers.Create(Panel_Mission);
end;


destructor TKMMapEdMission.Destroy;
begin
  fGuiMissionMode.Free;
  fGuiMissionAlliances.Free;
  fGuiMissionPlayers.Free;

  inherited;
end;


procedure TKMMapEdMission.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  fGuiMissionPlayers.KeyDown(Key, Shift, aHandled);
  fGuiMissionAlliances.KeyDown(Key, Shift, aHandled);
end;


procedure TKMMapEdMission.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gCursor.Mode := cmNone;

  //Hide existing pages
  fGuiMissionMode.Hide;
  fGuiMissionAlliances.Hide;
  fGuiMissionPlayers.Hide;

  if (Sender = Button_Mission[mtMode]) then
    fGuiMissionMode.Show
  else
  if (Sender = Button_Mission[mtAlliances]) then
    fGuiMissionAlliances.Show
  else
  if (Sender = Button_Mission[mtPlayers]) then
    fGuiMissionPlayers.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdMission.Show(aPage: TKMMissionTab);
begin
  case aPage of
    mtMode:       fGuiMissionMode.Show;
    mtAlliances:  fGuiMissionAlliances.Show;
    mtPlayers:    fGuiMissionPlayers.Show;
  end;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdMission.UpdateHotkeys;
const
  TAB_HINT : array [TKMMissionTab] of Word = (
    TX_MAPED_MISSION_MODE,
    TX_MAPED_PLAYERS_TYPE,
    TX_MAPED_ALLIANCE);
var
  MT: TKMMissionTab;
begin
  for MT := Low(TKMMissionTab) to High(TKMMissionTab) do
    Button_Mission[MT].Hint := GetHintWHotkey(TAB_HINT[MT], MAPED_SUBMENU_HOTKEYS[Ord(MT)]);
end;


procedure TKMMapEdMission.DoShowSubMenu(aIndex: Byte);
begin
  inherited;

  if (aIndex in [Byte(Low(TKMMissionTab))..Byte(High(TKMMissionTab))])
    and Button_Mission[TKMMissionTab(aIndex)].Enabled then
  begin
    PageChange(nil); //Hide existing pages
    Show(TKMMissionTab(aIndex));
  end;
end;


function TKMMapEdMission.Visible: Boolean;
begin
  Result := Panel_Mission.Visible;
end;


function TKMMapEdMission.IsVisible(aPage: TKMMissionTab): Boolean;
begin
  case aPage of
    mtMode:       Result := fGuiMissionMode.Visible;
    mtAlliances:  Result := fGuiMissionAlliances.Visible;
    mtPlayers:    Result := fGuiMissionPlayers.Visible;
    else          Result := False;
  end;
end;


end.
