unit KM_GUIMapEdMenuQuit;
{$I KaM_Remake.inc}
interface
uses
   Classes, SysUtils,
   KM_Controls, KM_ControlsBase,
   KM_Defaults;

type
  TKMMapEdMenuQuit = class
  private
    fOnDone: TNotifyEvent;
    procedure Menu_QuitClick(Sender: TObject);
  protected
    Panel_Quit: TKMPanel;
    Button_Quit_Yes: TKMButton;
    Button_Quit_No: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnDone: TNotifyEvent);

    procedure Show;
    procedure Hide;
  end;


implementation
uses
  KM_GameApp, KM_ResTexts, KM_RenderUI, KM_ResFonts;


{ TKMMapEdMenuQuit }
constructor TKMMapEdMenuQuit.Create(aParent: TKMPanel; aOnDone: TNotifyEvent);
begin
  inherited Create;

  fOnDone := aOnDone;

  Panel_Quit := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Quit.Anchors := [anLeft, anTop, anBottom];

  with TKMLabel.Create(Panel_Quit, 0, 40, Panel_Quit.Width, 60, gResTexts[TX_MAPED_LOAD_UNSAVED], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  Button_Quit_Yes := TKMButton.Create(Panel_Quit, 9, 100, Panel_Quit.Width - 9, 30, gResTexts[TX_MENU_QUIT_MAPED], bsGame);
  Button_Quit_Yes.Anchors := [anLeft, anTop, anRight];
  Button_Quit_No  := TKMButton.Create(Panel_Quit, 9, 140, Panel_Quit.Width - 9, 30, gResTexts[TX_MENU_DONT_QUIT_MISSION], bsGame);
  Button_Quit_No.Anchors := [anLeft, anTop, anRight];
  Button_Quit_Yes.Hint    := gResTexts[TX_MENU_QUIT_MAPED];
  Button_Quit_No.Hint     := gResTexts[TX_MENU_DONT_QUIT_MISSION];
  Button_Quit_Yes.OnClick := Menu_QuitClick;
  Button_Quit_No.OnClick  := Menu_QuitClick;
end;


procedure TKMMapEdMenuQuit.Menu_QuitClick(Sender: TObject);
begin
  if Sender = Button_Quit_Yes then
  begin
    gGameApp.StopGame(grMapEdEnd);
    Exit; //Must exit immediately since Self is destroyed
  end;
  if Sender = Button_Quit_No then
    fOnDone(Self);
end;


procedure TKMMapEdMenuQuit.Hide;
begin
  Panel_Quit.Hide;
end;


procedure TKMMapEdMenuQuit.Show;
begin
  Panel_Quit.Show;
end;


end.
