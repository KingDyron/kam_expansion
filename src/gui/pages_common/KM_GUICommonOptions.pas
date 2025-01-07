unit KM_GUICommonOptions;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsSwitch, KM_ControlsTrackBar,
  KM_ControlsEdit,
  KromOGLUtils,
  KM_MainSettings,
  KM_Pics, KM_Resolutions, KM_ResKeyFuncs, KM_GUICommonKeys,
  KM_InterfaceDefaults, KM_InterfaceTypes, KM_CommonTypes;


type
  TKMGUIOptionsKind = (guiOptMenu, guiOptGame);

  TKMGUICommonOptions = class
  private
    fGuiCommonKeys: TKMGUICommonKeys;
    fOptionsKind: TKMGUIOptionsKind;

    fOnClose: TEvent;

    fLastAlphaShadows: Boolean;

    fResolutions: TKMResolutions;

    // We remember old values to enable/disable "Apply" button dynamicaly
    fPrevResolutionId: TKMScreenResIndex;
    // Try to pick the same refresh rate on resolution change
    fDesiredRefRate: Integer;

    procedure ApplyResolution(Sender: TObject);
    procedure TestVideo_Click(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure ChangeResolution(Sender: TObject);
    procedure BackClick(Sender: TObject);
//    procedure EscKeyDown(Sender: TObject);
    procedure FlagClick(Sender: TObject);
    procedure RefreshResolutions;
    procedure KeysClick(Sender: TObject);
    procedure ShowEnviromentSettings(Sender: TObject);

    procedure CreateRes(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateGraphics(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateVideos(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateSound(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateControls(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateGameplay(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateReplay(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateMods(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateLanguages(var aTopBlock: Integer; var aLeftBlock: Integer);
    procedure CreateWeather;

    function NextBlock(var aTop: Integer; aCtrl: TKMControl; aAdj: Integer = 0): Integer;
    function NextTop(var aTop: Integer; const aInc: Integer = 20): Integer;

    procedure Init;

    function IsMenu: Boolean;
    function IsGame: Boolean;
  protected
    Panel_Options: TKMPanel;
      Panel_GFX: TKMPanel;
        Bevel_Options_GFX: TKMBevel;
        CheckBox_LerpRender: TKMCheckBox;
        CheckBox_LerpAnims: TKMCheckBox;
        CheckBox_VSync: TKMCheckBox;
        CheckBox_ShadowQuality: TKMCheckBox;
        TrackBar_Brightness: TKMTrackBar;
      Panel_Video: TKMPanel;
        Bevel_Video: TKMBevel;
        CheckBox_VideoEnable: TKMCheckBox;
        CheckBox_VideoStartup: TKMCheckBox;
        CheckBox_VideoStretch: TKMCheckBox;
        TrackBar_VideoVolume: TKMTrackBar;
        Button_VideoTest: TKMButton;

      Panel_Fonts: TKMPanel;
        CheckBox_FullFonts: TKMCheckBox;
      Panel_Ctrl: TKMPanel;
        TrackBar_ScrollSpeed: TKMTrackBar;
        Button_OptionsKeys: TKMButton;
      Panel_Game: TKMPanel;
        Bevel_Game: TKMBevel;
        CheckBox_Autosave: TKMCheckBox;
        CheckBoxs_AutosaveAtGameEnd: TKMCheckBox;
        CheckBox_MakeSavePoints: TKMCheckBox;
        CheckBox_SpecShowBeacons: TKMCheckBox;
        Label_PlayersColorMode: TKMLabel;
        Radio_PlayersColorMode: TKMRadioGroup;
      Panel_Replays: TKMPanel;
        Bevel_Replays: TKMBevel;
        CheckBox_ReplayAutopause: TKMCheckBox;
        CheckBox_ReplayShowBeacons: TKMCheckBox;
      Panel_Mods: TKMPanel;
        CheckBox_SnowHouses: TKMCheckBox;
        CheckBox_SnowObjects: TKMCheckBox;

      Button_Weather : TKMButton;
        Panel_Weather: TKMPanel;
          CheckBox_Enabled: TKMCheckBox;
          TrackBar_MaxCount: TKMNumericEdit;
          TrackBar_MaxSpawnCount: TKMNumericEdit;
          TrackBar_MinInterval: TKMNumericEdit;
          TrackBar_MaxInterval: TKMNumericEdit;
          TrackBar_MaxLifeTime: TKMNumericEdit;
          TrackBar_MaxCloudSpeed: TKMNumericEdit;
          TrackBar_DecParticles: TKMNumericEdit;
          TrackBar_NightSpeed, TrackBar_NightTime: TKMTrackBar;
          CheckBox_DynamicLight: TKMCheckBox;


      Panel_Sound: TKMPanel;
        Label_MusicOff: TKMLabel;
        TrackBar_SFX, TrackBar_Music: TKMTrackBar;
        CheckBox_MusicOff: TKMCheckBox;
        CheckBox_ShuffleOn: TKMCheckBox;
      Panel_Lang: TKMPanel;
        Radio_Lang: TKMRadioGroup;
        Image_Lang_Flags: array of TKMImage;
      Panel_Res: TKMPanel;
        CheckBox_FullScreen: TKMCheckBox;
        DropBox_Resolution: TKMDropList;
        DropBox_RefreshRate: TKMDropList;
        Button_ResApply: TKMButton;
      Button_OptionsBack: TKMButton;
  public
    OnToggleLocale: TKMToggleLocaleEvent;
    OnOptionsChange: TEvent;
    OnPreloadGameResources: TEvent;

    constructor Create(aParent: TKMPanel; aOptionsKind: TKMGUIOptionsKind; aOnClose, aOnKeysUpdated: TEvent);
    destructor Destroy; override;

    property GuiCommonKeys: TKMGUICommonKeys read fGuiCommonKeys;

    procedure Refresh;
    function Visible: Boolean;
    procedure Show;
  end;


implementation
uses
  KM_Main, KM_Music, KM_Sound, KM_RenderUI, KM_Resource, KM_ResTexts, KM_ResLocales, KM_ResFonts, KM_ResSound, KM_Video,
  KM_ResTypes,
  KM_Defaults,
  KM_Game, KM_GameSettings, KM_GameParams, KM_GameTypes,
  KM_GameAppSettings;

const
  SCROLL_SPEED_MULTIPLIER = 2.5;
  BLOCK_SPAN = 3;


{ TKMGUIMainOptions }
constructor TKMGUICommonOptions.Create(aParent: TKMPanel; aOptionsKind: TKMGUIOptionsKind; aOnClose, aOnKeysUpdated: TEvent);
var
  topBlock, leftBlock, bottomLine, panelTop, wid: Integer;
  backStr: String;
begin
  // We cant pass pointers to Settings in here cos on GUI creation fMain/gGameApp are not initialized yet
  fOptionsKind := aOptionsKind;
  fOnClose := aOnClose;

  case fOptionsKind of
    guiOptMenu: begin
                  wid := 880;
                  panelTop := (aParent.Height - 620) div 2 - 20;
                  backStr := gResTexts[TX_MENU_BACK];
                end;
    guiOptGame: begin
                  wid := 600;
                  panelTop := 0;
                  backStr := gResTexts[TX_WORD_CLOSE];
                end;
    else        begin
                  wid := 0;
                  panelTop := 0;
                end;
  end;

  Panel_Options := TKMPanel.Create(aParent,(aParent.Width - wid) div 2, panelTop, wid, aParent.Height - panelTop);
  Panel_Options.AnchorsStretch;
  bottomLine := Panel_Options.Height - 30 - 20;

  if IsMenu then
    with TKMImage.Create(Panel_Options, 705 - Panel_Options.Left, 220 - Panel_Options.Top, Round(207*1.3), Round(295*1.3),6,rxGuiMain) do
    begin
      ImageStretch;
      Anchors := [anLeft];
    end;

    case fOptionsKind of
      guiOptMenu: begin
                    //--- Column 1 --------------------------------------------------------------
                    topBlock := 0;
                    leftBlock := 0;

                    CreateRes(topBlock, leftBlock);
                    CreateGraphics(topBlock, leftBlock);
                    CreateVideos(topBlock, leftBlock);


                    //--- Column 2 --------------------------------------------------------------
                    topBlock := 0;
                    leftBlock := 300;

                    CreateSound(topBlock, leftBlock);
                    CreateControls(topBlock, leftBlock);
                    CreateGameplay(topBlock, leftBlock);
                    CreateReplay(topBlock, leftBlock);
                    CreateMods(topBlock, leftBlock);

                    //--- Column 3 --------------------------------------------------------------
                    topBlock := 0;
                    leftBlock := 600;

                    CreateLanguages(topBlock, leftBlock);
                    CreateWeather;
                  end;
      guiOptGame: begin
                    //--- Column 1 --------------------------------------------------------------
                    topBlock := 0;
                    leftBlock := 0;

                    CreateGraphics(topBlock, leftBlock);
                    CreateSound(topBlock, leftBlock);
                    CreateVideos(topBlock, leftBlock);

                    //--- Column 2 --------------------------------------------------------------
                    topBlock := 0;
                    leftBlock := 300;

                    CreateControls(topBlock, leftBlock);
                    CreateGameplay(topBlock, leftBlock);
                    CreateReplay(topBlock, leftBlock);
                    CreateMods(topBlock, leftBlock);
                  end;
    end;

    // Back button
    Button_OptionsBack := TKMButton.Create(Panel_Options, 0, bottomLine, 280, 30, backStr, bsMenu);
    Button_OptionsBack.Anchors := [anLeft];
    Button_OptionsBack.OnClick := BackClick;

    Button_Weather.Visible := fOptionsKind = guiOptMenu;

    // Panel_Options_Keys
    // Last, to be above all other panels
    fGuiCommonKeys := TKMGUICommonKeys.Create(aParent, aOnKeysUpdated);
end;


destructor TKMGUICommonOptions.Destroy;
begin
  fGuiCommonKeys.Free;
  inherited;
end;


function TKMGUICommonOptions.NextBlock(var aTop: Integer; aCtrl: TKMControl; aAdj: Integer = 0): Integer;
begin
  aTop := aCtrl.Bottom + BLOCK_SPAN + aAdj;
  Result := aTop;
end;


function TKMGUICommonOptions.NextTop(var aTop: Integer; const aInc: Integer = 20): Integer;
begin
  Inc(aTop, aInc);
  Result := aTop;
end;


procedure TKMGUICommonOptions.CreateRes(var aTopBlock: Integer; var aLeftBlock: Integer);
begin
  if not IsMenu then Exit;

  // Resolutions section
  Panel_Res := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 175);
  NextBlock(aTopBlock, Panel_Res);
  Panel_Res.Anchors := [anLeft];
    TKMLabel.Create(Panel_Res, 6, 0, 270, 20, gResTexts[TX_MENU_OPTIONS_RESOLUTION], fntOutline, taLeft);
    TKMBevel.Create(Panel_Res, 0, 20, 280, Panel_Res.Height - 20);

    CheckBox_FullScreen := TKMCheckBox.Create(Panel_Res, 10, 30, 260, 20, gResTexts[TX_MENU_OPTIONS_FULLSCREEN], fntMetal);
    CheckBox_FullScreen.OnClick := ChangeResolution;

    DropBox_Resolution := TKMDropList.Create(Panel_Res, 10, 50, 260, 20, fntMetal, '', bsMenu);
    DropBox_Resolution.OnChange := ChangeResolution;

    DropBox_RefreshRate := TKMDropList.Create(Panel_Res, 10, 85, 260, 20, fntMetal, '', bsMenu);
    DropBox_RefreshRate.OnChange := ChangeResolution;

    Button_ResApply := TKMButton.Create(Panel_Res, 10, 125, 260, 30, gResTexts[TX_MENU_OPTIONS_APPLY], bsMenu);
    Button_ResApply.OnClick := ApplyResolution;
end;


procedure TKMGUICommonOptions.CreateGraphics(var aTopBlock: Integer; var aLeftBlock: Integer);
var
  top: Integer;
begin
  // Graphics section
  Panel_GFX := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 165);
  Panel_GFX.Anchors := [anLeft];
    TKMLabel.Create(Panel_GFX,6,0,270,20,gResTexts[TX_MENU_OPTIONS_GRAPHICS],fntOutline,taLeft);
    Bevel_Options_GFX := TKMBevel.Create(Panel_GFX,0,20,280,145);
    CheckBox_LerpRender := TKMCheckBox.Create(Panel_GFX, 10, 30, 260, 20, gResTexts[TX_MENU_OPTIONS_LERP_RENDER], fntMetal);
    CheckBox_LerpRender.Hint := gResTexts[TX_SETTINGS_LERP_RENDER_HINT];
    CheckBox_LerpRender.OnClick := Change;

    CheckBox_LerpAnims := TKMCheckBox.Create(Panel_GFX, 10, 50, 260, 20, gResTexts[TX_MENU_OPTIONS_LERP_ANIMS], fntMetal);
    CheckBox_LerpAnims.Hint := gResTexts[TX_SETTINGS_LERP_ANIMS_HINT];
    CheckBox_LerpAnims.Checked := false;
    CheckBox_LerpAnims.Enabled := false;
    CheckBox_LerpAnims.Visible := false;
    CheckBox_LerpAnims.OnClick := Change;
    top := 70;

    if IsMenu then
    begin
      CheckBox_VSync := TKMCheckBox.Create(Panel_GFX, 10, 70, 260, 20, gResTexts[TX_MENU_OPTIONS_VSYNC], fntMetal);
      CheckBox_VSync.OnClick := Change;

      CheckBox_ShadowQuality := TKMCheckBox.Create(Panel_GFX, 10, 90, 260, 20, gResTexts[TX_MENU_OPTIONS_SHADOW_QUALITY], fntMetal);
      CheckBox_ShadowQuality.OnClick := Change;
      Inc(top, 40);
    end;

    TrackBar_Brightness := TKMTrackBar.Create(Panel_GFX, 10, top, 256, OPT_SLIDER_MIN,OPT_SLIDER_MAX);
    TrackBar_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Brightness.OnChange := Change;

  Panel_GFX.Height := TrackBar_Brightness.Bottom + 15;
  Bevel_Options_GFX.Height := Panel_GFX.Height - 20;
  NextBlock(aTopBlock, Panel_GFX);
end;


procedure TKMGUICommonOptions.CreateVideos(var aTopBlock: Integer; var aLeftBlock: Integer);
var
  top: Integer;
begin
  top := 10;
  // Videos
  Panel_Video := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 195);
  Panel_Video.Anchors := [anLeft];
    TKMLabel.Create(Panel_Video,6,0,270,20,gResTexts[TX_MENU_OPTIONS_VIDEOS],fntOutline,taLeft);
    Bevel_Video := TKMBevel.Create(Panel_Video,0,20,280,Panel_Video.Height - 20);
    CheckBox_VideoEnable := TKMCheckBox.Create(Panel_Video, 10, NextTop(top), 260, 20, gResTexts[TX_MENU_OPTIONS_VIDEOS_ENABLE], fntMetal);
    CheckBox_VideoEnable.OnClick := Change;
    CheckBox_VideoStretch := TKMCheckBox.Create(Panel_Video, 10, NextTop(top), 260, 20, gResTexts[TX_MENU_OPTIONS_VIDEOS_STRETCH], fntMetal);
    CheckBox_VideoStretch.OnClick := Change;
    CheckBox_VideoStartup := TKMCheckBox.Create(Panel_Video, 10, NextTop(top), 260, 20, gResTexts[TX_MENU_OPTIONS_VIDEOS_STARTUP], fntMetal);
    CheckBox_VideoStartup.OnClick := Change;

    if IsMenu then
    begin
      TrackBar_VideoVolume := TKMTrackBar.Create(Panel_Video, 10, NextTop(top), 256, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      TrackBar_VideoVolume.Caption := gResTexts[TX_MENU_OPTIONS_VIDEOS_VOLUME];
      TrackBar_VideoVolume.OnChange := Change;

      Button_VideoTest := TKMButton.Create(Panel_Video, 10, NextTop(top, 60), 260, 30, gResTexts[TX_MENU_OPTIONS_VIDEOS_TEST], bsMenu);
      Button_VideoTest.OnClick := TestVideo_Click;

      Inc(top, 15);
    end;

  Panel_Video.Height := top + 25;
  Bevel_Video.Height := Panel_Video.Height - 20;
  NextBlock(aTopBlock, Panel_Video);

  Panel_Video.Visible := gVideoPlayer.PlayerEnabled;

  {$IFNDEF VIDEOS}
  Panel_Video.Hide; //Hide panel when no videos defined
  {$ENDIF}
end;


procedure TKMGUICommonOptions.CreateSound(var aTopBlock: Integer; var aLeftBlock: Integer);
begin
  // SFX section
  Panel_Sound := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 175);
  NextBlock(aTopBlock, Panel_Sound);
  Panel_Sound.Anchors := [anLeft];
    TKMLabel.Create(Panel_Sound,6,0,270,20,gResTexts[TX_MENU_OPTIONS_SOUND],fntOutline,taLeft);
    TKMBevel.Create(Panel_Sound,0,20,280,Panel_Sound.Height - 20);

    TrackBar_SFX       := TKMTrackBar.Create(Panel_Sound, 10, 27, 256, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
    TrackBar_Music     := TKMTrackBar.Create(Panel_Sound, 10, 77, 256, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
    CheckBox_MusicOff  := TKMCheckBox.Create(Panel_Sound, 10, 127, 256, 20, gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE], fntMetal);
    CheckBox_ShuffleOn := TKMCheckBox.Create(Panel_Sound, 10, 147, 256, 20, gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE], fntMetal);
    TrackBar_SFX.Caption   := gResTexts[TX_MENU_SFX_VOLUME];
    TrackBar_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
    TrackBar_SFX.OnChange      := Change;
    TrackBar_Music.OnChange    := Change;
    CheckBox_MusicOff.OnClick  := Change;
    CheckBox_ShuffleOn.OnClick := Change;
end;


procedure TKMGUICommonOptions.CreateControls(var aTopBlock: Integer; var aLeftBlock: Integer);
begin
  // Controls section
  Panel_Ctrl := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 115);
  NextBlock(aTopBlock, Panel_Ctrl);
  Panel_Ctrl.Anchors := [anLeft];
    TKMLabel.Create(Panel_Ctrl,6,0,270,20,gResTexts[TX_MENU_OPTIONS_CONTROLS],fntOutline,taLeft);
    TKMBevel.Create(Panel_Ctrl,0,20,280,Panel_Ctrl.Height - 20);

    TrackBar_ScrollSpeed := TKMTrackBar.Create(Panel_Ctrl,10,27,256,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
    TrackBar_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_ScrollSpeed.OnChange := Change;

    // Keybindings button
    Button_OptionsKeys := TKMButton.Create(Panel_Ctrl, 10, 77, 260, 30, gResTexts[TX_MENU_OPTIONS_KEYBIND], bsMenu);
    Button_OptionsKeys.Anchors := [anLeft];
    Button_OptionsKeys.OnClick := KeysClick;
end;


procedure TKMGUICommonOptions.CreateGameplay(var aTopBlock: Integer; var aLeftBlock: Integer);
var
  strSavePT, strAutosavePTEnd: String;
  linesSavePT, linesAutosavePTEnd, top: Integer;
begin
  // Gameplay section

  strSavePT := gResTexts[TX_MENU_OPTIONS_MAKE_SAVEPOINTS];
  gRes.Fonts[fntMetal].GetTextSize(strSavePT, linesSavePT);

  strAutosavePTEnd := gResTexts[TX_MENU_OPTIONS_AUTOSAVE_AT_GAME_END];
  gRes.Fonts[fntMetal].GetTextSize(strAutosavePTEnd, linesAutosavePTEnd);

  {if IsMenu then
    Inc(aTopBlock, 40 - 20*(linesSavePT + linesAutosavePTEnd - 2));}

  Panel_Game := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 50 + 20*(linesSavePT + linesAutosavePTEnd));

  Panel_Game.Anchors := [anLeft];

    TKMLabel.Create(Panel_Game,6,0,270,20,gResTexts[TX_MENU_OPTIONS_GAMEPLAY],fntOutline,taLeft);
    Bevel_Game := TKMBevel.Create(Panel_Game,0,20,280,Panel_Game.Height - 20);
    //Bevel_Game.OnClick := BackClick;
    top := 7;
    // Menu, Game
    if IsMenu or gGameParams.IsGame then
    begin
      CheckBox_Autosave := TKMCheckBox.Create(Panel_Game,10,NextTop(top),256,20,gResTexts[TX_MENU_OPTIONS_AUTOSAVE], fntMetal);
      CheckBox_Autosave.OnClick := Change;

      CheckBoxs_AutosaveAtGameEnd := TKMCheckBox.Create(Panel_Game,10,NextTop(top),256,20,strAutosavePTEnd, fntMetal);
      CheckBoxs_AutosaveAtGameEnd.OnClick := Change;
      NextTop(top, 20*(linesAutosavePTEnd - 1));

      CheckBox_MakeSavePoints := TKMCheckBox.Create(Panel_Game, 10, NextTop(top),256,20,strSavePT, fntMetal);
      CheckBox_MakeSavePoints.OnClick := Change;
      NextTop(top, 20*(linesSavePT - 1));
    end;

    // Spectator
    if IsGame and (gGameParams.Mode = gmMultiSpectate) then
    begin
      CheckBox_SpecShowBeacons := TKMCheckBox.Create(Panel_Game, 10, NextTop(top), 256, 20, gResTexts[TX_GAME_SETTINGS_SHOW_BEACONS], fntMetal);
      CheckBox_SpecShowBeacons.Hint := gResTexts[TX_GAME_SETTINGS_SHOW_BEACONS_HINT];
      CheckBox_SpecShowBeacons.OnClick := Change;
    end;

    if IsGame then
    begin
      // Do not make space if its the 1st element in Gameplay section
      if top > 10 then
        Inc(top, 15);

      Label_PlayersColorMode := TKMLabel.Create(Panel_Game, 10, NextTop(top), 256, 20, gResTexts[TX_PLAYERS_COLOR_MODE_CAPTION], fntMetal, taLeft);

      Radio_PlayersColorMode := TKMRadioGroup.Create(Panel_Game,10,NextTop(top),256,60,fntMetal);
//        Radio_PlayersColorMode.Anchors := [anLeft, anBottom];
      Radio_PlayersColorMode.ItemIndex := 0;
      Radio_PlayersColorMode.Add(gResTexts[TX_PLAYERS_COLOR_MODE_DEFAULT], gResTexts[TX_PLAYERS_COLOR_MODE_DEFAULT_HINT]);
      Radio_PlayersColorMode.Add(gResTexts[TX_PLAYERS_COLOR_MODE_ALLY_ENEMY], gResTexts[TX_PLAYERS_COLOR_MODE_ALLY_ENEMY_HINT]);
      Radio_PlayersColorMode.Add(gResTexts[TX_PLAYERS_COLOR_MODE_TEAMS], gResTexts[TX_PLAYERS_COLOR_MODE_TEAMS_HINT]);
      Radio_PlayersColorMode.OnChange := Change;
      Inc(top, 40);
    end;

  Panel_Game.Height := top + 20 + 3;
  Bevel_Game.Height := Panel_Game.Height - 20;
  NextBlock(aTopBlock, Panel_Game);
end;


procedure TKMGUICommonOptions.CreateReplay(var aTopBlock: Integer; var aLeftBlock: Integer);
var
  height: Integer;
begin
  // Show only in Menu and Replay
  if not IsMenu and not gGameParams.IsReplay then Exit;

  if IsMenu then
    Inc(aTopBlock, 4);

  //Replays section
  Panel_Replays := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 50);

  Panel_Replays.Anchors := [anLeft];
    TKMLabel.Create(Panel_Replays,6,0,270,20,gResTexts[TX_WORD_REPLAY] + ':',fntOutline,taLeft);
    Bevel_Replays := TKMBevel.Create(Panel_Replays,0,20,280,Panel_Replays.Height - 20);

    CheckBox_ReplayAutopause := TKMCheckBox.Create(Panel_Replays,10,27,256,20,gResTexts[TX_SETTINGS_PAUSE_AT_PT_END], fntMetal);
    CheckBox_ReplayAutopause.OnClick := Change;

    height := 50;

  if not IsMenu and gGameParams.IsReplay then
  begin
    CheckBox_ReplayShowBeacons := TKMCheckBox.Create(Panel_Replays, 10, 47, 256, 20, gResTexts[TX_GAME_SETTINGS_SHOW_BEACONS], fntMetal);
    CheckBox_ReplayShowBeacons.Hint := gResTexts[TX_GAME_SETTINGS_SHOW_BEACONS_HINT];
    CheckBox_ReplayShowBeacons.OnClick := Change;

    Inc(height, 20);
  end;

  Panel_Replays.Height := height;
  Bevel_Replays.Height := Panel_Replays.Height - 20;

  NextBlock(aTopBlock, Panel_Replays);
end;


procedure TKMGUICommonOptions.CreateMods(var aTopBlock: Integer; var aLeftBlock: Integer);
begin
  if IsMenu then
    Inc(aTopBlock, 4);

  // Mods
//  Panel_Options_Mods := TKMPanel.Create(Panel_Options,300,bottomLine-20,280,50);
  Panel_Mods := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 120);
  Panel_Mods.Anchors := [anLeft];
  NextBlock(aTopBlock, Panel_Mods);
    TKMLabel.Create(Panel_Mods,6,0,270,20,gResTexts[TX_MENU_OPTIONS_MODS] + ':',fntOutline,taLeft);
    TKMBevel.Create(Panel_Mods,0,20,280,Panel_Mods.Height - 20);

    CheckBox_SnowHouses := TKMCheckBox.Create(Panel_Mods,10,27,256,20,gResTexts[TX_MENU_OPTIONS_MODS_SNOW_HOUSES], fntMetal);
    CheckBox_SnowHouses.OnClick := Change;

    CheckBox_SnowObjects := TKMCheckBox.Create(Panel_Mods,10,47,256,20,gResTexts[2052], fntMetal);
    CheckBox_SnowObjects.OnClick := Change;

  Button_Weather := TKMButton.Create(Panel_Mods, 10, 75, Panel_Mods.Width - 20, 30, gResTexts[2069], bsMenu);
  Button_Weather.OnClick := ShowEnviromentSettings;
  Button_Weather.Hide;
end;

procedure TKMGUICommonOptions.CreateWeather;
begin
  Panel_Weather := TKMPanel.Create(Panel_Options, 0, 0, 580, 300);
  Panel_Weather.Centerize;
  Panel_Weather.Anchors := [anLeft];

  with TKMBevel.Create(Panel_Weather, -2000, -2000, 6000, 6000) do
    HideParentOnClick;
  TKMBevel.Create(Panel_Weather, 0, 0, Panel_Weather.Width, Panel_Weather.Height);
  with TKMButton.Create(Panel_Weather, -5, -5, Panel_Weather.Width + 10, Panel_Weather.Height + 10, '', bsPaper) do
    Hitable := false;



  with TKMButton.Create(Panel_Weather, 10, Panel_Weather.Height - 40, 100, 30, gResTexts[106], bsMenu) do
    HideParentOnClick;

    TKMBevel.Create(Panel_Weather,0,20,Panel_Weather.Width div 2 - 3,100);
    TKMBevel.Create(Panel_Weather,Panel_Weather.Width div 2 + 2,20,Panel_Weather.Width div 2 - 5, 100);

    CheckBox_Enabled := TKMCheckBox.Create(Panel_Weather,0,0,256,20,gResTexts[2053], fntOutline);
    CheckBox_Enabled.OnClick := Change;
    //CheckBox_Enabled.MobilHint := true;
    CheckBox_Enabled.Hint := gResTexts[2054];

    TKMLabel.Create(Panel_Weather,6,23,Panel_Weather.Width div 2,20,gResTexts[2063] + ':', fntMetal,taLeft);

    TrackBar_MaxCount := TKMNumericEdit.Create(Panel_Weather, 10, 40, 1, 20);
    TrackBar_MaxCount.OnChange := Change;
    TrackBar_MaxCount.Width := 75;
    TrackBar_MaxCount.TextAlign := taCenter;
    TrackBar_MaxCount.Hint := gResTexts[2055];

    TrackBar_MaxSpawnCount := TKMNumericEdit.Create(Panel_Weather, 100, 40, 1, 10);
    TrackBar_MaxSpawnCount.OnChange := Change;
    TrackBar_MaxSpawnCount.Width := 75;
    TrackBar_MaxSpawnCount.TextAlign := taCenter;
    TrackBar_MaxSpawnCount.Hint := gResTexts[2056];

    TKMLabel.Create(Panel_Weather,6,65,Panel_Weather.Width div 2,20,gResTexts[2062] + ':', fntMetal,taLeft);

    TrackBar_MinInterval := TKMNumericEdit.Create(Panel_Weather, 10, 85, 1, 60);
    TrackBar_MinInterval.OnChange := Change;
    TrackBar_MinInterval.Width := 75;
    TrackBar_MinInterval.TextAlign := taCenter;
    TrackBar_MinInterval.Hint := gResTexts[2057];

    TrackBar_MaxInterval := TKMNumericEdit.Create(Panel_Weather, 100, 85, 1, 300);
    TrackBar_MaxInterval.OnChange := Change;
    TrackBar_MaxInterval.Width := 75;
    TrackBar_MaxInterval.TextAlign := taCenter;
    TrackBar_MaxInterval.Hint := gResTexts[2058];
    TrackBar_MaxCount.Width := 75;

    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 23,Panel_Weather.Width div 2,20,gResTexts[2059] + ':', fntMetal,taLeft);

    TrackBar_MaxLifeTime := TKMNumericEdit.Create(Panel_Weather, 490, 23, 1, 120);
    TrackBar_MaxLifeTime.OnChange := Change;
    TrackBar_MaxLifeTime.Width := 75;
    TrackBar_MaxLifeTime.TextAlign := taCenter;
    TrackBar_MaxLifeTime.Hint := gResTexts[2064];
    TrackBar_MaxLifeTime.Width := 75;

    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 53,Panel_Weather.Width div 2,20,gResTexts[2060] + ':', fntMetal,taLeft);
    TrackBar_MaxCloudSpeed := TKMNumericEdit.Create(Panel_Weather, 490, 50, 1, 20);
    TrackBar_MaxCloudSpeed.OnChange := Change;
    TrackBar_MaxCloudSpeed.Hint := gResTexts[2060];
    TrackBar_MaxCloudSpeed.Width := 75;
    TrackBar_MaxCloudSpeed.TextAlign := taCenter;
    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 83,Panel_Weather.Width div 2,20,gResTexts[2061] + ':', fntMetal,taLeft);

    TrackBar_DecParticles := TKMNumericEdit.Create(Panel_Weather, 490, 80, 1, 10);
    TrackBar_DecParticles.OnChange := Change;
    TrackBar_DecParticles.Hint := gResTexts[2068];
    TrackBar_DecParticles.Width := 75;
    TrackBar_DecParticles.TextAlign := taCenter;

    TKMBevel.Create(Panel_Weather,0,145,Panel_Weather.Width div 2 - 3,90);

    TrackBar_NightSpeed := TKMTrackBar.Create(Panel_Weather, 5, 150, Panel_Weather.Width div 2 - 13, 0, MAX_NIGHT_SPEED - 1);
    TrackBar_NightSpeed.Position := 10;
    TrackBar_NightSpeed.OnChange := Change;
    TrackBar_NightSpeed.Caption := gResTexts[2070];
    TrackBar_NightSpeed.Hint := gResTexts[2072];

    TrackBar_NightTime := TKMTrackBar.Create(Panel_Weather, 5, 190, Panel_Weather.Width div 2 - 13, 0, 24);
    TrackBar_NightTime.Position := 12;
    TrackBar_NightTime.OnChange := Change;
    TrackBar_NightTime.Caption := gResTexts[2071];
    TrackBar_NightTime.Hint := gResTexts[2073];

    TKMBevel.Create(Panel_Weather,Panel_Weather.Width div 2 + 2,145,Panel_Weather.Width div 2 - 5, 90);

    CheckBox_DynamicLight := TKMCheckBox.Create(Panel_Weather,Panel_Weather.Width div 2 + 6,150,200,20,gResTexts[2099], fntGrey);
    CheckBox_DynamicLight.OnClick := Change;
    CheckBox_DynamicLight.Hint := gResTexts[2100];

  Panel_Weather.Hide;
end;


procedure TKMGUICommonOptions.CreateLanguages(var aTopBlock: Integer; var aLeftBlock: Integer);
var
  I: Integer;
begin
  // Only in Menu
  if not IsMenu then Exit;

  // Language section
  Panel_Lang := TKMPanel.Create(Panel_Options, aLeftBlock, aTopBlock, 280, 30 + gResLocales.Count*20);
  NextBlock(aTopBlock, Panel_Lang);
  Panel_Lang.Anchors := [anLeft];
    TKMLabel.Create(Panel_Lang,6,0,242,20,gResTexts[TX_MENU_OPTIONS_LANGUAGE],fntOutline,taLeft);
    TKMBevel.Create(Panel_Lang,0,20,280,10+gResLocales.Count*20);

    Radio_Lang := TKMRadioGroup.Create(Panel_Lang, 28, 27, 220, 20*gResLocales.Count, fntMetal);
    SetLength(Image_Lang_Flags,gResLocales.Count);
    for I := 0 to gResLocales.Count - 1 do
    begin
      Radio_Lang.Add(gResLocales[I].Title);
      Image_Lang_Flags[I] := TKMImage.Create(Panel_Lang,6,28+(I*20),16,11, gResLocales[I].FlagSpriteID, rxGuiMain);
      Image_Lang_Flags[I].Tag := I;
      Image_Lang_Flags[I].OnClick := FlagClick;
    end;
    Radio_Lang.OnChange := Change;

  // Language Fonts section

    TKMBevel.Create(Panel_Lang,0,30+gResLocales.Count*20+10,280,30);

    CheckBox_FullFonts := TKMCheckBox.Create(Panel_Lang, 10,30+gResLocales.Count*20+17,260,20, gResTexts[TX_MENU_OPTIONS_FONTS], fntMetal);
    CheckBox_FullFonts.OnClick := Change;
end;


// This is called when the options page is shown, so update all the values
// Note: Options can be required to fill before gGameApp is completely initialized,
// hence we need to pass either gGameApp.Settings or a direct Settings link
procedure TKMGUICommonOptions.Refresh;
begin
  Init;

  TrackBar_Brightness.Position  := gGameSettings.GFX.Brightness;
  CheckBox_LerpRender.Checked   := gGameSettings.GFX.InterpolatedRender;
  CheckBox_LerpAnims.Enabled    := false;
  CheckBox_LerpAnims.Checked    := false;
  TrackBar_ScrollSpeed.Position := Round(gGameSettings.ScrollSpeed / SCROLL_SPEED_MULTIPLIER);
  TrackBar_SFX.Position         := Round(gGameSettings.SFX.SoundFXVolume * TrackBar_SFX.MaxValue);
  TrackBar_Music.Position       := Round(gGameSettings.SFX.MusicVolume * TrackBar_Music.MaxValue);
  CheckBox_MusicOff.Checked     := not gGameSettings.SFX.MusicEnabled;
  TrackBar_Music.Enabled        := not CheckBox_MusicOff.Checked;
  CheckBox_ShuffleOn.Checked    := gGameSettings.SFX.ShuffleOn;
  CheckBox_ShuffleOn.Enabled    := not CheckBox_MusicOff.Checked;
  CheckBox_SnowHouses.Checked   := gGameSettings.GFX.AllowSnowHouses;
  CheckBox_SnowObjects.Checked   := gGameSettings.GFX.AllowSnowObjects;

  CheckBox_VideoEnable.Checked   := gGameSettings.Video.Enabled;
  CheckBox_VideoStretch.Checked  := gGameSettings.Video.VideoStretch;
  CheckBox_VideoStretch.Enabled  := gGameSettings.Video.Enabled;
  CheckBox_VideoStartup.Checked  := gGameSettings.Video.PlayOnStartup;
  CheckBox_VideoStartup.Enabled  := gGameSettings.Video.Enabled;

  // Only in Menu
  if IsMenu then
  begin
    CheckBox_VSync.Checked         := gMainSettings.VSync;
    CheckBox_FullFonts.Enabled     := not gResLocales.LocaleByCode(gGameSettings.Locale).NeedsFullFonts;
    CheckBox_FullFonts.Checked     := gGameSettings.GFX.LoadFullFonts or not CheckBox_FullFonts.Enabled;
    CheckBox_ShadowQuality.Checked := gGameSettings.GFX.AlphaShadows;

    Button_VideoTest.Enabled       := gGameSettings.Video.Enabled;
    TrackBar_VideoVolume.Position  := Round(gGameSettings.Video.VideoVolume * TrackBar_VideoVolume.MaxValue);
    //Disable Video volume util we will fix it
    //Video volume is set via windows mixer now, and it affect all other game sounds/music after the end of video playback
    TrackBar_VideoVolume.Enabled     := False; //gGameSettings.VideoOn;

    Radio_Lang.ItemIndex := gResLocales.IndexByCode(gGameSettings.Locale);

    // We need to reset dropboxes every time we enter Options page
    RefreshResolutions;

    CheckBox_Enabled.Checked := gGameSettings.Weather.Enabled;
    TrackBar_MaxCount.Value := gGameSettings.Weather.MaxCount;
    TrackBar_MaxSpawnCount.Value := gGameSettings.Weather.MaxSpawnCount;
    TrackBar_MinInterval.Value := gGameSettings.Weather.MinInterval div 10;
    TrackBar_MaxInterval.Value := gGameSettings.Weather.MaxInterval div 10;
    TrackBar_MaxLifeTime.Value := gGameSettings.Weather.MaxLifeTime div 10;
    TrackBar_MaxCloudSpeed.Value := Round(gGameSettings.Weather.MaxCloudSpeed * 200);
    TrackBar_DecParticles.Value := gGameSettings.Weather.DecParticles + 1;
    TrackBar_NightSpeed.Position := gGameSettings.Weather.NightSpeed;
    TrackBar_NightTime.Position := gGameSettings.Weather.NightTime;
    CheckBox_DynamicLight.Checked  := gGameSettings.Weather.DynamicLight;
  end
  else
  // Only in Game
  if IsGame then
  begin
    if gGameParams.IsReplay then
      CheckBox_ReplayShowBeacons.Checked := gGameSettings.ReplayShowBeacons
    else
    if gGameParams.Mode = gmMultiSpectate then
      CheckBox_SpecShowBeacons.Checked := gGameSettings.SpecShowBeacons;

    Radio_PlayersColorMode.ItemIndex := Byte(gGameSettings.PlayersColorMode) - 1;
  end;

  // In Menu and in Game (not Replay)
  if IsMenu or gGameParams.IsGame then
  begin
    CheckBox_Autosave.Checked           := gGameSettings.Autosave;
    CheckBoxs_AutosaveAtGameEnd.Checked := gGameSettings.AutosaveAtGameEnd;
    CheckBox_MakeSavePoints.Checked     := gGameSettings.SaveCheckpoints;
  end;

  // In Menu or Replay
  if IsMenu or gGameParams.IsReplay then
    CheckBox_ReplayAutopause.Checked := gGameSettings.ReplayAutopause;

  // In Replay
  if (gGameParams <> nil) and gGameParams.IsReplay then
    CheckBox_ReplayAutopause.Enabled := (gGameParams.Mode = gmReplayMulti) and gGame.IsPeaceTime;




end;


// Changed options are saved immediately (cos they are easy to restore/rollback)
procedure TKMGUICommonOptions.Change(Sender: TObject);
var
  musicToggled, shuffleToggled: Boolean;
begin
  // Change these options only if they changed state since last time
  musicToggled := (gGameSettings.SFX.MusicEnabled <> not CheckBox_MusicOff.Checked);
  shuffleToggled := (gGameSettings.SFX.ShuffleOn <> CheckBox_ShuffleOn.Checked);

  gGameSettings.GFX.Brightness         := TrackBar_Brightness.Position;
  gGameSettings.GFX.InterpolatedRender := CheckBox_LerpRender.Checked;
  gGameSettings.GFX.InterpolatedAnimations := false;

  CheckBox_LerpAnims.Enabled       := false;

  gGameSettings.ScrollSpeed        := TrackBar_ScrollSpeed.Position * SCROLL_SPEED_MULTIPLIER;
  gGameSettings.SFX.SoundFXVolume      := TrackBar_SFX.Position / TrackBar_SFX.MaxValue;
  gGameSettings.SFX.MusicVolume        := TrackBar_Music.Position / TrackBar_Music.MaxValue;
  gGameSettings.SFX.MusicEnabled       := not CheckBox_MusicOff.Checked;
  gGameSettings.SFX.ShuffleOn          := CheckBox_ShuffleOn.Checked;
  gGameSettings.GFX.AllowSnowHouses    := CheckBox_SnowHouses.Checked;
  gGameSettings.GFX.AllowSnowObjects    := CheckBox_SnowObjects.Checked;

  TrackBar_Music.Enabled      := not CheckBox_MusicOff.Checked;
  CheckBox_ShuffleOn.Enabled  := not CheckBox_MusicOff.Checked;

  gSoundPlayer.UpdateSoundVolume(gGameSettings.SFX.SoundFXVolume);
  gMusic.Volume := gGameSettings.SFX.MusicVolume;

  gGameSettings.Video.Enabled         := CheckBox_VideoEnable.Checked;
  gGameSettings.Video.VideoStretch    := CheckBox_VideoStretch.Checked;
  gGameSettings.Video.PlayOnStartup   := CheckBox_VideoStartup.Checked;

  if IsMenu then
    gGameSettings.Video.VideoVolume      := TrackBar_VideoVolume.Position / TrackBar_VideoVolume.MaxValue;

  if Sender = CheckBox_VideoEnable then
  begin
    CheckBox_VideoStartup.Enabled := CheckBox_VideoEnable.Checked;
    CheckBox_VideoStretch.Enabled := CheckBox_VideoEnable.Checked;

    if IsMenu then
    begin
      //Disable Video volume util we will fix it
      //Video volume is set via windows mixer now, and it affect all other game sounds/music after the end of video playback
      TrackBar_VideoVolume.Enabled  := False; //CheckBox_Options_VideoEnable.Checked;
      Button_VideoTest.Enabled := CheckBox_VideoEnable.Checked;
    end;
  end;

  // Only in Menu
  if IsMenu then
  begin
    gMainSettings.VSync        := CheckBox_VSync.Checked;
    gGameSettings.GFX.AlphaShadows := CheckBox_ShadowQuality.Checked;

    // Menu options
    SetupVSync(gMainSettings.VSync);

    if Sender = CheckBox_FullFonts then
    begin
      gGameSettings.GFX.LoadFullFonts := CheckBox_FullFonts.Checked;
      if CheckBox_FullFonts.Checked and (gRes.Fonts.LoadLevel <> fllFull) then
      begin
        // When enabling full fonts, use ToggleLocale reload the entire interface
        if Assigned(OnToggleLocale) then
          OnToggleLocale(gResLocales[Radio_Lang.ItemIndex].Code, gpOptions);
        Exit; // Exit ASAP because whole interface will be recreated
      end;
    end;

    if Sender = Radio_Lang then
    begin
      if Assigned(OnToggleLocale) then
        OnToggleLocale(gResLocales[Radio_Lang.ItemIndex].Code, gpOptions);
      Exit; // Exit ASAP because whole interface will be recreated
    end;

    gGameSettings.Weather.Enabled := CheckBox_Enabled.Checked;
    gGameSettings.Weather.MaxCount := TrackBar_MaxCount.Value;
    gGameSettings.Weather.MaxSpawnCount := TrackBar_MaxSpawnCount.Value;
    gGameSettings.Weather.MinInterval := TrackBar_MinInterval.Value * 10;
    gGameSettings.Weather.MaxInterval := TrackBar_MaxInterval.Value * 10;
    gGameSettings.Weather.MaxLifeTime := TrackBar_MaxLifeTime.Value * 10;
    gGameSettings.Weather.MaxCloudSpeed := TrackBar_MaxCloudSpeed.Value / 200;
    gGameSettings.Weather.DecParticles := TrackBar_DecParticles.Value - 1;
    gGameSettings.Weather.NightSpeed := TrackBar_NightSpeed.Position;
    gGameSettings.Weather.NightTime := TrackBar_NightTime.Position;
    gGameSettings.Weather.DynamicLight := CheckBox_DynamicLight.Checked;


    TrackBar_MaxInterval.ValueMin := TrackBar_MinInterval.Value + 1;

    {for I := 0 to Panel_Weather.ChildCount - 1 do
      if Panel_Weather.Childs[I] <> CheckBox_Enabled  then
        Panel_Weather.Childs[I].Enabled := CheckBox_Enabled.Checked;}
  end
  else
  // Only in Game
  if IsGame then
  begin
    // Game options
    if gGameParams.IsReplay then
      gGameSettings.ReplayShowBeacons := CheckBox_ReplayShowBeacons.Checked
    else if gGameParams.Mode = gmMultiSpectate then
      gGameSettings.SpecShowBeacons   := CheckBox_SpecShowBeacons.Checked;

    gGameSettings.PlayersColorMode := TKMPlayerColorMode(Radio_PlayersColorMode.ItemIndex + 1);
  end;

  // In Menu or in Game (not Replay)
  if IsMenu or gGameParams.IsGame then
  begin
    gGameSettings.Autosave          := CheckBox_Autosave.Checked;
    gGameSettings.AutosaveAtGameEnd := CheckBoxs_AutosaveAtGameEnd.Checked;
    gGameSettings.SaveCheckpoints    := CheckBox_MakeSavePoints.Checked;
  end;

  // In Menu or Replay
  if IsMenu or gGameParams.IsReplay then
    gGameSettings.ReplayAutopause := CheckBox_ReplayAutopause.Checked;

  if musicToggled then
  begin
    gMusic.ToggleEnabled(gGameSettings.SFX.MusicEnabled);
    if gGameSettings.SFX.MusicEnabled then
      shuffleToggled := True; // Re-shuffle songs if music has been enabled
  end;

  if shuffleToggled then
    gMusic.ToggleShuffle(gGameSettings.SFX.ShuffleOn);

  if Assigned(OnOptionsChange) then
    OnOptionsChange();
end;


// Apply resolution changes
procedure TKMGUICommonOptions.ChangeResolution(Sender: TObject);
var
  I: Integer;
  resID, refID: Integer;
begin
  if fResolutions.Count = 0 then Exit;

  DropBox_Resolution.Enabled := CheckBox_FullScreen.Checked;
  DropBox_RefreshRate.Enabled := CheckBox_FullScreen.Checked;

  // Repopulate RefreshRates list
  if Sender = DropBox_Resolution then
  begin
    resID := DropBox_Resolution.ItemIndex;

    // Reset refresh rates, because they are different for each resolution
    DropBox_RefreshRate.Clear;
    for I := 0 to fResolutions.Items[resID].RefRateCount - 1 do
    begin
      DropBox_RefreshRate.Add(Format('%d Hz', [fResolutions.Items[resID].RefRate[I]]));
      // Make sure to select something. SelectedRefRate is prefered, otherwise select first
      if (I = 0) or (fResolutions.Items[resID].RefRate[I] = fDesiredRefRate) then
        DropBox_RefreshRate.ItemIndex := I;
    end;
  end;

  // Make button enabled only if new resolution/mode differs from old
  resID := DropBox_Resolution.ItemIndex;
  refID := DropBox_RefreshRate.ItemIndex;
  Button_ResApply.Enabled :=
      (gMainSettings.FullScreen <> CheckBox_FullScreen.Checked) or
      (CheckBox_FullScreen.Checked and ((fPrevResolutionId.ResID <> resID) or
                                                (fPrevResolutionId.RefID <> refID)));
  // Remember which one we have selected so we can reselect it if the user changes resolution
  fDesiredRefRate := fResolutions.Items[resID].RefRate[refID];
end;


procedure TKMGUICommonOptions.ApplyResolution(Sender: TObject);
var
  ResID, RefID: Integer;
  NewResolution: TKMScreenRes;
begin
  if fResolutions.Count = 0 then Exit;

  gMainSettings.FullScreen := CheckBox_FullScreen.Checked;

  ResID := DropBox_Resolution.ItemIndex;
  RefID := DropBox_RefreshRate.ItemIndex;
  NewResolution.Width := fResolutions.Items[ResID].Width;
  NewResolution.Height := fResolutions.Items[ResID].Height;
  NewResolution.RefRate := fResolutions.Items[ResID].RefRate[RefID];

  gMainSettings.Resolution := NewResolution;
  gMain.ReinitRender(True);
end;


procedure TKMGUICommonOptions.TestVideo_Click(Sender: TObject);
begin
  gVideoPlayer.AddVideo('Victory');
  gVideoPlayer.AddVideo('Campaigns\The Shattered Kingdom\Intro');
  gVideoPlayer.AddVideo('Defeat');
  gVideoPlayer.AddVideo('KaM');
  gVideoPlayer.Play;
end;


function TKMGUICommonOptions.Visible: Boolean;
begin
  Result := Panel_Options.Visible;
end;


procedure TKMGUICommonOptions.FlagClick(Sender: TObject);
begin
  Assert(Sender is TKMImage);
  Radio_Lang.ItemIndex := TKMImage(Sender).Tag;
  Change(Radio_Lang);
end;


// Resets dropboxes, they will have correct values
procedure TKMGUICommonOptions.RefreshResolutions;
var
  I: Integer;
  R: TKMScreenResIndex;
begin
  DropBox_Resolution.Clear;
  DropBox_RefreshRate.Clear;

  R := fResolutions.GetResolutionIDs(gMainSettings.Resolution);

  if fResolutions.Count > 0 then
  begin
    for I := 0 to fResolutions.Count - 1 do
    begin
      DropBox_Resolution.Add(Format('%dx%d', [fResolutions.Items[I].Width, fResolutions.Items[I].Height]));
      if (I = 0) or (I = R.ResID) then
        DropBox_Resolution.ItemIndex := I;
    end;

    for I := 0 to fResolutions.Items[R.ResID].RefRateCount - 1 do
    begin
      DropBox_RefreshRate.Add(Format('%d Hz', [fResolutions.Items[R.ResID].RefRate[I]]));
      if (I = 0) or (I = R.RefID) then
      begin
        DropBox_RefreshRate.ItemIndex := I;
        fDesiredRefRate := fResolutions.Items[R.ResID].RefRate[I];
      end;
    end;
  end
  else
  begin
    // No supported resolutions
    DropBox_Resolution.Add(gResTexts[TX_MENU_OPTIONS_RESOLUTION_NOT_SUPPORTED]);
    DropBox_RefreshRate.Add(gResTexts[TX_MENU_OPTIONS_REFRESH_RATE_NOT_SUPPORTED]);
    DropBox_Resolution.ItemIndex := 0;
    DropBox_RefreshRate.ItemIndex := 0;
  end;

  CheckBox_FullScreen.Checked := gMainSettings.FullScreen;
  // Controls should be disabled, when there is no resolution to choose
  CheckBox_FullScreen.Enabled := fResolutions.Count > 0;
  DropBox_Resolution.Enabled  := (gMainSettings.FullScreen) and (fResolutions.Count > 0);
  DropBox_RefreshRate.Enabled := (gMainSettings.FullScreen) and (fResolutions.Count > 0);

  fPrevResolutionId := R;
  Button_ResApply.Disable;
end;


procedure TKMGUICommonOptions.Init;
begin
  // Remember what we are working with
  // (we do that on Show because Create gets called from Main/Game constructor and fMain/gGameApp are not yet assigned)
  // Ideally we could pass them as parameters here
  // Only in Menu
  if IsMenu then
  begin
    fResolutions := gMain.Resolutions;
    fLastAlphaShadows := gGameSettings.GFX.AlphaShadows;
  end;
end;


function TKMGUICommonOptions.IsGame: Boolean;
begin
  Result := fOptionsKind = guiOptGame;
end;


function TKMGUICommonOptions.IsMenu: Boolean;
begin
  Result := fOptionsKind = guiOptMenu;
end;


procedure TKMGUICommonOptions.Show;
begin
  Refresh;
  Panel_Options.Show;
end;


procedure TKMGUICommonOptions.KeysClick(Sender: TObject);
begin
    fGuiCommonKeys.Show;
end;

procedure TKMGUICommonOptions.ShowEnviromentSettings(Sender: TObject);
begin
  Panel_Weather.Show;
end;


procedure TKMGUICommonOptions.BackClick(Sender: TObject);
begin
  // Return to MainMenu and restore resolution changes
  gGameAppSettings.SaveSettings;

  if IsMenu and (fLastAlphaShadows <> gGameSettings.GFX.AlphaShadows)
    and Assigned(OnPreloadGameResources) then
    OnPreloadGameResources;  //Update loaded game resources, if we changed alpha shadow setting

  if Assigned(fOnClose) then
    fOnClose;
end;


//procedure TKMGUICommonOptions.EscKeyDown(Sender: TObject);
//begin
//  if not fGuiCommonKeys.Visible then
//    BackClick(nil);
//end;


end.

