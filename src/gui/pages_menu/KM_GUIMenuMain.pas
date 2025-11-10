unit KM_GUIMenuMain;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsEdit,{  KM_ControlsSwitch,}
  KM_Pics, KM_InterfaceDefaults, KM_InterfaceTypes;


type
  TKMMenuMain = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText;
    fChangeLog : Boolean;
    procedure ButtonClick(Sender: TObject);
  protected
    Panel_MainMenu: TKMPanel;
    Panel_MMButtons: TKMPanel;
    Button_MM_SinglePlayer: TKMButton;
    Button_MM_MultiPlayer: TKMButton;
    Button_MM_MapEd: TKMButton;
    Button_MM_Replays: TKMButton;
    Button_MM_Options: TKMButton;
    Button_MM_Achievements: TKMButton;
    Image_MM_HasAch : TKMImage;
    Button_MM_Credits: TKMButton;
    Button_MM_Quit: TKMButton;
    Button_MM_Debug: TKMButton;
    Image_ChangeLog : TKMImage;
    //Switch_Test : TKMSwitch;
  public
    constructor Create(aParent: TKMPanel; aWithChangeLog : Boolean; aOnPageChange: TKMMenuChangeEventText);
    procedure Show;
    procedure HideChangeLog;
  end;


implementation
uses
  KM_Main,
  KM_GameApp,
  KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI,
  KM_Achievements;


{ TKMGUIMenuMain }
constructor TKMMenuMain.Create(aParent: TKMPanel; aWithChangeLog : Boolean;  aOnPageChange: TKMMenuChangeEventText);
begin
  inherited Create(gpMainMenu);

  fOnPageChange := aOnPageChange;
  fChangeLog := aWithChangeLog;
  //Without anchors this page is centered on resize
  Panel_MainMenu := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MainMenu.AnchorsCenter;
    TKMImageMainText.Create(Panel_MainMenu, 0, 90, Panel_MainMenu.Width, Panel_MainMenu.Height - 90{, 4, rxGuiMain}, [anTop]);
    //with TKMImageMainText.Create(Panel_MainMenu, 300, 90, 423, 164, 4, rxGuiMain) do
    //  ImageStretch;

    TKMImageMainLeft.Create(Panel_MainMenu,  50, 220, round(218*1.3), round(291*1.3){, 5, rxGuiMain}, []);
      //ImageStretch;
    TKMImageMainRight.Create(Panel_MainMenu, 705, 220, round(207*1.3), round(295*1.3){, 6, rxGuiMain}, []);
      //ImageStretch;
    //TKMLabel.Create(Panel_MainMenu, 512, 300, 'E X P A N S I O N', fntOutline, taCenter);

    Panel_MMButtons := TKMPanel.Create(Panel_MainMenu, 337, 340, 350, 400);
      Button_MM_SinglePlayer := TKMButton.Create(Panel_MMButtons,0,  0,350,30,gResTexts[TX_MENU_SINGLEPLAYER],bsMenu);
      Button_MM_MultiPlayer  := TKMButton.Create(Panel_MMButtons,0, 40,350,30,gResTexts[TX_MENU_MULTIPLAYER],bsMenu);
      Button_MM_MapEd        := TKMButton.Create(Panel_MMButtons,0, 80,350,30,gResTexts[TX_MENU_MAP_EDITOR],bsMenu);
      Button_MM_Replays      := TKMButton.Create(Panel_MMButtons,0,120,350,30,gResTexts[TX_MENU_REPLAYS],bsMenu);
      Button_MM_Options      := TKMButton.Create(Panel_MMButtons,0,160,350,30,gResTexts[TX_MENU_OPTIONS],bsMenu);
      Button_MM_Achievements := TKMButton.Create(Panel_MMButtons,0,200,350,30,gResTexts[2108],bsMenu);
      Button_MM_Credits      := TKMButton.Create(Panel_MMButtons,0,240,350,30,gResTexts[TX_MENU_CREDITS],bsMenu);
      Button_MM_Quit         := TKMButton.Create(Panel_MMButtons,0,300,350,30,gResTexts[TX_MENU_QUIT],bsMenu);
      Button_MM_Debug        := TKMButton.Create(Panel_MMButtons,0,350,350,30,'Debug Houses',bsMenu);

      Image_ChangeLog         := TKMImage.Create(Panel_MMButtons.MasterPanel, 15, 30, 50, 50, 105, rxGuiMain);
      Image_ChangeLog.Center;
      Image_ChangeLog.HighlightOnMouseOver := true;
      Image_ChangeLog.Visible := aWithChangeLog;

      Button_MM_SinglePlayer.OnClick := ButtonClick;
      Button_MM_MultiPlayer.OnClick  := ButtonClick;
      Button_MM_MapEd.OnClick        := ButtonClick;
      Button_MM_Replays.OnClick      := ButtonClick;
      Button_MM_Options.OnClick      := ButtonClick;
      Button_MM_Achievements.OnClick := ButtonClick;
      Button_MM_Credits.OnClick      := ButtonClick;
      Button_MM_Quit.OnClick         := ButtonClick;
      Button_MM_Debug.OnClick         := ButtonClick;
      Image_ChangeLog.OnClick         := ButtonClick;
      Image_MM_HasAch := TKMImage.Create(Panel_MMButtons, Button_MM_Achievements.Right - 15, Button_MM_Achievements.Top - 10, 35, 33, 105, rxGuiMain);
      Image_MM_HasAch.Hitable := false;
      Image_MM_HasAch.Hide;
      {$IFNDEF DEBUG}
        Button_MM_Debug.Hide;
      {$ENDIF}
      {Switch_Test := TKMSwitch.Create(Panel_MMButtons, 0, 340, 110, 30);
      Switch_Test.Offset := 35;
      Switch_Test.TexID := [383, 384, 385];}
end;


procedure TKMMenuMain.ButtonClick(Sender: TObject);
begin
  if Sender = Button_MM_SinglePlayer then
    fOnPageChange(gpSinglePlayer);

  if Sender = Button_MM_MultiPlayer then
  begin
    if gMain.LockMutex then
    begin
      if not gGameApp.CheckDATConsistency then
      begin
        fOnPageChange(gpError, gResTexts[TX_ERROR_MODS]);
        gMain.UnlockMutex;
      end
      else
        fOnPageChange(gpMultiplayer);
    end
    else
      fOnPageChange(gpError, gResTexts[TX_MULTIPLE_INSTANCES]);
  end;

  if Sender = Button_MM_MapEd then
    fOnPageChange(gpMapEditor);

  if Sender = Button_MM_Replays then
    fOnPageChange(gpReplays);

  if Sender = Button_MM_Options then
    fOnPageChange(gpOptions);

  if Sender = Button_MM_Achievements then
    fOnPageChange(gpAchievements);

  if Sender = Button_MM_Credits then
    fOnPageChange(gpCredits);

  if Sender = Button_MM_Debug then
    fOnPageChange(gpDebug);

  If Sender = Image_ChangeLog then
    fOnPageChange(gpChangeLog);



  if Sender = Button_MM_Quit then
    gMain.Stop(Self);
end;


procedure TKMMenuMain.Show;
begin
  Panel_MainMenu.Show;
  Image_MM_HasAch.Visible := gAchievements.AnythingToShow;
  Image_ChangeLog.Visible := fChangeLog;
end;

procedure TKMMenuMain.HideChangeLog;
begin
  Image_ChangeLog.Hide;
end;


end.
