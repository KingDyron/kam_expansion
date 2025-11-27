unit KM_GUIMapEdMissionMode;
{$I KaM_Remake.inc}
interface
uses
   Classes,
   KM_MapTypes,
   KM_Controls, KM_ControlsBase, KM_ControlsEdit, KM_ControlsMemo, KM_ControlsPopUp, KM_ControlsSwitch, KM_ControlsTrackBar,
   KM_ControlsDrop,
   KM_Defaults;

type
  TKMMapEdMissionMode = class
  private
    fUpdating: Boolean;
    fWeatherStartIndex : Integer;

    procedure CreateMissionParams(aParent: TKMPopUpPanel);

    procedure Mission_ModeChange(Sender: TObject);
    procedure Mission_ModeUpdate;
    procedure AIBuilderChange(Sender: TObject);

    procedure MissionParams_Click(Sender: TObject);
    procedure MissionParams_CloseClick(Sender: TObject);
    function MissionParams_OnKeyDown(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
    procedure RadioMissionDesc_Changed(Sender: TObject);
    procedure UpdateMapTxtInfo(Sender: TObject);
    procedure UpdateMapParams;

    procedure ShowPanels(Sender: TObject);
    procedure WeatherChange(Sender: TObject);
    procedure RefreshWeather;

    procedure CreateMessages;

    procedure MessagesChange(Sender: TObject);
    procedure MessagesChange2(Sender: TObject);
  protected
    Panel_Mode: TKMPanel;
      Radio_MissionMode: TKMRadioGroup;

      Button_MissionParams: TKMButton;
      PopUp_MissionParams: TKMPopUpPanel;
        Panel_MissionParams: TKMPanel;
          Edit_Author: TKMEdit;
          Edit_Version: TKMEdit;
          Radio_NameType: TKMRadioGroup;
          NumEdit_MapName: TKMNumericEdit;
          Memo_MapName: TKMMemo;
          Radio_SmallDescType: TKMRadioGroup;
          Edit_SmallDesc: TKMEdit;
          NumEdit_SmallDesc: TKMNumericEdit;
          Panel_CheckBoxes: TKMPanel;
            CheckBox_Coop, CheckBox_Special, CheckBox_RMG, CheckBox_PlayableAsSP,
            CheckBox_BlockTeamSelection, CheckBox_BlockPeacetime,
            CheckBox_BlockFullMapPreview, CheckBox_BlockColorSelection: TKMCheckBox;

          Button_Difficulty : TKMButton;
          Panel_Difficulty: TKMPanel;
            CheckBox_Difficulty: array [MISSION_DIFFICULTY_MIN..MISSION_DIFFICULTY_MAX] of TKMCheckBox;

          Button_Weather : TKMButton;
          Panel_Weather: TKMPanel;
            CheckBox_Enabled, CheckBox_Overwrite: TKMCheckBox;
            TrackBar_MaxCount: TKMNumericEdit;
            TrackBar_MaxSpawnCount: TKMNumericEdit;
            TrackBar_MinInterval: TKMNumericEdit;
            TrackBar_MaxInterval: TKMNumericEdit;
            TrackBar_MaxLifeTime: TKMNumericEdit;
            TrackBar_MaxCloudSpeed: TKMNumericEdit;
            TrackBar_DecParticles: TKMNumericEdit;
            TrackBar_NightSpeed, TrackBar_NightTime: TKMTrackBar;
            CheckBox_DynamicLight: TKMCheckBox;
            CheckBox_DynamicShadow: TKMCheckBox;

          Button_Texts : TKMButton;
          PopUp_Texts : TKMPanel;
            DropList_Locales : TKMDropList;
            Edit_TextID : TKMNumericEdit;
            Edit_Text : TKMEdit;
            Memo_Text : TKMMemo;
            DropList_Locales2 : TKMDropList;
            Memo_Text2 : TKMMemo;

          Radio_BigDescType: TKMRadioGroup;
          Edit_BigDesc: TKMEdit;
          NumEdit_BigDesc: TKMNumericEdit;
          Memo_BigDesc: TKMMemo;
          Label_BigDesc: TKMLabel;
          Button_Close: TKMButton;

      Button_AIBuilderSetup: TKMButton;
      Button_AIBuilderWarn: TKMLabel;
      Button_AIBuilderOK, Button_AIBuilderCancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;
  end;


implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Math,
  KM_ResTexts, KM_Game, KM_GameParams,
  KM_ControlsTypes,
  KM_RenderUI, KM_ResFonts,
  KM_ResLocales,
  KM_InterfaceGame, KM_HandsCollection, KM_Hand,
  SysUtils;


{ TKMMapEdMissionMode }
constructor TKMMapEdMissionMode.Create(aParent: TKMPanel);
const
  POPUP_H = 700;
begin
  inherited Create;

  Panel_Mode := TKMPanel.Create(aParent, 0, 28, aParent.Width, 400);
  TKMLabel.Create(Panel_Mode, 0, PAGE_TITLE_Y, TB_MAP_ED_WIDTH, 0, gResTexts[TX_MAPED_MISSION_MODE], fntOutline, taCenter).Anchors := [anLeft, anTop, anRight];
  TKMBevel.Create(Panel_Mode, 9, 25, Panel_Mode.Width - 9, 45).Anchors := [anLeft, anTop, anRight];

  Radio_MissionMode := TKMRadioGroup.Create(Panel_Mode, 14, 30, Panel_Mode.Width - 28, 40, fntMetal);
  Radio_MissionMode.Anchors := [anLeft, anTop, anRight];
  Radio_MissionMode.Add(gResTexts[TX_LOBBY_MAP_BUILD]);
  Radio_MissionMode.Add(gResTexts[TX_LOBBY_MAP_FIGHT]);
  Radio_MissionMode.OnChange := Mission_ModeChange;

  Button_MissionParams := TKMButton.Create(Panel_Mode, 9, 80, Panel_Mode.Width - 9, 45, gResTexts[TX_MAPED_MISSION_PARAMETERS_BTN], bsGame);
  Button_MissionParams.Anchors := [anLeft, anTop, anRight];
  Button_MissionParams.Hint := gResTexts[TX_MAPED_MISSION_PARAMETERS_BTN_HINT];
  Button_MissionParams.OnClick := MissionParams_Click;

  PopUp_MissionParams := TKMPopUpPanel.Create(aParent.MasterParent, 700, POPUP_H, gResTexts[TX_MAPED_MISSION_PARAMETERS_TITLE], pbYellow, False, False);
  PopUp_MissionParams.CapOffsetY := -5;
  PopUp_MissionParams.OnKeyDown := MissionParams_OnKeyDown;

  CreateMissionParams(PopUp_MissionParams);

  TKMLabel.Create(Panel_Mode, 0, 140, Panel_Mode.Width, 0, gResTexts[TX_MAPED_AI_DEFAULTS_HEADING], fntOutline, taCenter).Anchors := [anLeft, anTop, anRight];

  Button_AIBuilderSetup := TKMButton.Create(Panel_Mode, 9, 170, Panel_Mode.Width - 9, 30, gResTexts[TX_MAPED_AI_DEFAULTS_MP_BUILDER], bsGame);
  Button_AIBuilderSetup.Anchors := [anLeft, anTop, anRight];
  Button_AIBuilderSetup.Hint := gResTexts[TX_MAPED_AI_DEFAULTS_MP_BUILDER_HINT];
  Button_AIBuilderSetup.OnClick := AIBuilderChange;

  Button_AIBuilderWarn := TKMLabel.Create(Panel_Mode, 9, 160, Panel_Mode.Width - 9, 0, gResTexts[TX_MAPED_AI_DEFAULTS_CONFIRM], fntGrey, taLeft);
  Button_AIBuilderWarn.Anchors := [anLeft, anTop, anRight];
  Button_AIBuilderWarn.WordWrap := True;
  Button_AIBuilderWarn.Hide;
  Button_AIBuilderOK := TKMButton.Create(Panel_Mode, 9, 250, 88, 20, gResTexts[TX_MAPED_OK], bsGame);
  Button_AIBuilderOK.OnClick := AIBuilderChange;
  Button_AIBuilderOK.Hide;
  Button_AIBuilderCancel := TKMButton.Create(Panel_Mode, Panel_Mode.Width - 88, 250, 88, 20, gResTexts[TX_MAPED_CANCEL], bsGame);
  Button_AIBuilderCancel.Anchors := [anTop, anRight];
  Button_AIBuilderCancel.OnClick := AIBuilderChange;
  Button_AIBuilderCancel.Hide;
end;


procedure TKMMapEdMissionMode.CreateMissionParams(aParent: TKMPopUpPanel);
const
  CHK_W = 300;
  RADIO_W = 250;
var
  Top: Integer;
  MD: TKMMissionDifficulty;
begin
  Panel_MissionParams := TKMPanel.Create(aParent.ItemsPanel, 5, 5, aParent.ItemsPanel.Width - 10, aParent.ItemsPanel.Height - 10);
  Panel_MissionParams.AnchorsStretch;

  Top := 0;
  TKMLabel.Create(Panel_MissionParams, 0, Top, gResTexts[TX_MAPED_MISSION_AUTHOR], fntMetal, taLeft);
  TKMLabel.Create(Panel_MissionParams, (Panel_MissionParams.Width div 2) + 5, Top, gResTexts[TX_MAPED_MISSION_VERSION], fntMetal, taLeft);
  Inc(Top, 20);
  Edit_Author := TKMEdit.Create(Panel_MissionParams, 0, Top, (Panel_MissionParams.Width div 2) - 5, 20, fntArial);
  Edit_Author.ShowColors := True;
  Edit_Version := TKMEdit.Create(Panel_MissionParams, (Panel_MissionParams.Width div 2) + 5, Top,
                                 (Panel_MissionParams.Width div 2) - 5, 20, fntArial);
  Edit_Version.ShowColors := True;

  Inc(Top, 30);
  TKMLabel.Create(Panel_MissionParams, 0, Top, gResTexts[1985], fntMetal, taLeft);

  Inc(Top, 20);
  Radio_NameType := TKMRadioGroup.Create(Panel_MissionParams, 5, Top + 5, RADIO_W, 40, fntMetal);
  Radio_NameType.Add(gResTexts[TX_MAPED_MISSION_LIBX_TEXT_ID]);
  Radio_NameType.AllowUncheck := false;
  Radio_NameType.ItemIndex := 0;
  NumEdit_MapName := TKMNumericEdit.Create(Panel_MissionParams, RADIO_W + 20, Top, -1, 999, fntGrey);
  NumEdit_MapName.OnChange := UpdateMapTxtInfo;
  Memo_MapName := TKMMemo.Create(Panel_MissionParams, NumEdit_MapName.Right + 20, Top, Panel_MissionParams.Width - NumEdit_MapName.Right - 20, 30, fntMetal, bsGame);
  //Memo_MapName.AnchorsStretch;
  Memo_MapName.WordWrap := false;
  Memo_MapName.ScrollDown := false;
  Memo_MapName.Hitable := false;
  Inc(Top, 30);
  TKMLabel.Create(Panel_MissionParams, 0, Top, gResTexts[TX_MAPED_MISSION_SMALL_DESC], fntMetal, taLeft);
  Inc(Top, 20);
  TKMBevel.Create(Panel_MissionParams, 0, Top, RADIO_W + 10, 45);

  Radio_SmallDescType := TKMRadioGroup.Create(Panel_MissionParams, 5, Top + 5, RADIO_W, 40, fntMetal);
  Radio_SmallDescType.Add(gResTexts[TX_WORD_TEXT]);
  Radio_SmallDescType.Add(gResTexts[TX_MAPED_MISSION_LIBX_TEXT_ID]);
  Radio_SmallDescType.OnChange := RadioMissionDesc_Changed;

  Edit_SmallDesc := TKMEdit.Create(Panel_MissionParams, RADIO_W + 20, Top, Panel_MissionParams.Width - RADIO_W - 25, 20, fntGame);
  Edit_SmallDesc.ShowColors := True;
  NumEdit_SmallDesc := TKMNumericEdit.Create(Panel_MissionParams, RADIO_W + 20, Top, -1, 999, fntGrey);

  Inc(Top, 55);
  TKMLabel.Create(Panel_MissionParams, 0, Top, gResTexts[TX_MAPED_MISSION_PARAMETERS_TITLE], fntMetal, taLeft);
  Inc(Top, 25);
  TKMBevel.Create(Panel_MissionParams, 0, Top, Panel_MissionParams.Width, 150);

  Inc(Top, 5);
  Panel_CheckBoxes := TKMPanel.Create(Panel_MissionParams, 5, Top, Panel_MissionParams.Width - 10, 110);

    CheckBox_Coop := TKMCheckBox.Create(Panel_CheckBoxes, 0, 0,  CHK_W, 20, gResTexts[TX_LOBBY_MAP_COOP], fntMetal);
    CheckBox_Coop.Hint := gResTexts[TX_LOBBY_MAP_COOP];

    CheckBox_Special := TKMCheckBox.Create(Panel_CheckBoxes, 0, 20, CHK_W, 20, gResTexts[TX_LOBBY_MAP_SPECIAL], fntMetal);
    CheckBox_Special.Hint := gResTexts[TX_LOBBY_MAP_SPECIAL];

    CheckBox_PlayableAsSP := TKMCheckBox.Create(Panel_CheckBoxes, 0, 40, CHK_W, 20, gResTexts[TX_MENU_MAP_PLAYABLE_AS_SP],  fntMetal);
    CheckBox_PlayableAsSP.Hint := gResTexts[TX_MAPED_MISSION_PLAYABLE_AS_SP_HINT];

    CheckBox_RMG := TKMCheckBox.Create(Panel_CheckBoxes, 0, 60, CHK_W, 20, gResTexts[TX_LOBBY_MAP_RANDOM], fntMetal);
    CheckBox_RMG.Hint := gResTexts[TX_LOBBY_MAP_RANDOM_HINT];

    CheckBox_BlockColorSelection := TKMCheckBox.Create(Panel_CheckBoxes, CHK_W + 10, 0, CHK_W, 20, gResTexts[TX_MAPED_MISSION_BLOCK_COLOR_SEL], fntMetal);
    CheckBox_BlockColorSelection.Hint := gResTexts[TX_MAPED_MISSION_BLOCK_COLOR_SEL_HINT];

    CheckBox_BlockTeamSelection := TKMCheckBox.Create(Panel_CheckBoxes, CHK_W + 10, 20, CHK_W, 20, gResTexts[TX_MAPED_MISSION_BLOCK_TEAM_SEL],  fntMetal);
    CheckBox_BlockTeamSelection.Hint := gResTexts[TX_MAPED_MISSION_BLOCK_TEAM_SEL_HINT];

    CheckBox_BlockPeacetime := TKMCheckBox.Create(Panel_CheckBoxes, CHK_W + 10, 40, CHK_W, 20, gResTexts[TX_MAPED_MISSION_BLOCK_PT], fntMetal);
    CheckBox_BlockPeacetime.Hint := gResTexts[TX_MAPED_MISSION_BLOCK_PT_HINT];

    CheckBox_BlockFullMapPreview := TKMCheckBox.Create(Panel_CheckBoxes, CHK_W + 10, 60, CHK_W, 20, gResTexts[TX_MAPED_MISSION_BLOCK_FULL_MAP_PREVIEW], fntMetal);
    CheckBox_BlockFullMapPreview.Hint := gResTexts[TX_MAPED_MISSION_BLOCK_FULL_MAP_PREVIEW_HINT];


  Inc(Top, 80);
  {with TKMLabel.Create(Panel_MissionParams, 0, Top, Panel_MissionParams.Width, 20, gResTexts[TX_MAPED_MISSION_DIFFICULTY_LEVELS], fntMetal, taLeft) do
    Hint := gResTexts[TX_MAPED_MISSION_DIFFICULTY_LEVELS_HINT];
  Inc(Top, 20);
  //TKMBevel.Create(Panel_MissionParams, 0, Top, 100, 65);

  Inc(Top, 5);
  difW := (aParent.ItemsPanel.Width - 20) div 3;}

  Button_Difficulty := TKMButton.Create(Panel_MissionParams, 5, Top, 270, 25, gResTexts[TX_MAPED_MISSION_DIFFICULTY_LEVELS] + ' \/', bsMenu);
  Button_Difficulty.OnClick := ShowPanels;

  Button_Weather := TKMButton.Create(Panel_MissionParams, Panel_MissionParams.Width - 275, Top, 270, 25, gResTexts[2069], bsMenu);
  Button_Weather.OnClick := ShowPanels;

  Inc(Top, 33);

  Button_Texts := TKMButton.Create(Panel_MissionParams, 5, Top, 270, 25, gResTexts[1843], bsMenu);
  Button_Texts.OnClick := ShowPanels;

  {for MD := MISSION_DIFFICULTY_MIN to mdEasy1 do
    CheckBox_Difficulty[MD] := TKMCheckBox.Create(Panel_MissionParams,
                                     5, Top + (Integer(MD) - Integer(mdEasy3))*20,
                                     difW, 20, gResTexts[DIFFICULTY_LEVELS_TX[MD]], fntMetal);
  CheckBox_Difficulty[mdNormal] := TKMCheckBox.Create(Panel_MissionParams,
                                     5 + difW + 5, Top + 20, difW, 20,
                                     gResTexts[DIFFICULTY_LEVELS_TX[mdNormal]], fntMetal);

  for MD := mdHard1 to MISSION_DIFFICULTY_MAX do
    CheckBox_Difficulty[MD] := TKMCheckBox.Create(Panel_MissionParams,
                                     5 + 2*difW + 5, Top + (Integer(MD) - Integer(mdHard1))*20,
                                     difW, 20, gResTexts[DIFFICULTY_LEVELS_TX[MD]], fntMetal);}

  Inc(Top, 115);
  TKMLabel.Create(Panel_MissionParams, 0, Top, gResTexts[TX_MAPED_MISSION_BIG_DESC], fntMetal, taLeft);
  Inc(Top, 20);
  TKMBevel.Create(Panel_MissionParams, 0, Top, RADIO_W + 10, 45);

  Radio_BigDescType := TKMRadioGroup.Create(Panel_MissionParams, 5, Top + 5, RADIO_W, 40, fntMetal);
  Radio_BigDescType.Add(gResTexts[TX_WORD_TEXT]);
  Radio_BigDescType.Add(gResTexts[TX_MAPED_MISSION_LIBX_TEXT_ID]);
  Radio_BigDescType.OnChange := RadioMissionDesc_Changed;

  Edit_BigDesc := TKMEdit.Create(Panel_MissionParams, RADIO_W + 20, Top, Panel_MissionParams.Width - RADIO_W - 25, 20, fntGame);
  Edit_BigDesc.MaxLen := 4096;
  Edit_BigDesc.AllowedChars := acAll;
  Edit_BigDesc.ShowColors := True;
  NumEdit_BigDesc := TKMNumericEdit.Create(Panel_MissionParams, RADIO_W + 20, Top, -1, 999, fntGrey);

  Inc(Top, 55);

  Memo_BigDesc := TKMMemo.Create(Panel_MissionParams, 0, Top, Panel_MissionParams.Width, Panel_MissionParams.Height - Top - 40, fntMetal, bsGame);
  Memo_BigDesc.AnchorsStretch;
  Memo_BigDesc.WordWrap := True;
  Memo_BigDesc.ScrollDown := True;

  Top := Button_Difficulty.Bottom + 3;
  Panel_Difficulty := TKMPanel.Create(Panel_MissionParams, 5, Top, Button_Difficulty.Width, 180);
  with TKMButton.Create(Panel_Difficulty, 0, 0, Panel_Difficulty.Width, Panel_Difficulty.Height, '', bsPaper) do
    Hitable := false;
  for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
    CheckBox_Difficulty[MD] := TKMCheckBox.Create(Panel_Difficulty,
                                     5, 8 + (Integer(MD) - Integer(mdEasy3))*25,
                                     Panel_Difficulty.Width - 20, 20, gResTexts[DIFFICULTY_LEVELS_TX[MD]], fntMetal);
  Panel_Difficulty.Hide;


  Edit_Author.OnChange                 := UpdateMapTxtInfo;
  Edit_Version.OnChange                := UpdateMapTxtInfo;
  Edit_SmallDesc.OnChange              := UpdateMapTxtInfo;
  NumEdit_SmallDesc.OnChange           := UpdateMapTxtInfo;
  Edit_BigDesc.OnChange                := UpdateMapTxtInfo;
  NumEdit_BigDesc.OnChange             := UpdateMapTxtInfo;
  CheckBox_Coop.OnClick                := UpdateMapTxtInfo;
  CheckBox_Special.OnClick             := UpdateMapTxtInfo;
  CheckBox_RMG.OnClick                 := UpdateMapTxtInfo;
  CheckBox_PlayableAsSP.OnClick        := UpdateMapTxtInfo;
  CheckBox_BlockTeamSelection.OnClick  := UpdateMapTxtInfo;
  CheckBox_BlockColorSelection.OnClick := UpdateMapTxtInfo;
  CheckBox_BlockPeacetime.OnClick      := UpdateMapTxtInfo;
  CheckBox_BlockFullMapPreview.OnClick := UpdateMapTxtInfo;

  for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
    CheckBox_Difficulty[MD].OnClick := UpdateMapTxtInfo;

  Button_Close := TKMButton.Create(Panel_MissionParams, 0, Panel_MissionParams.Height - 35, 120, 30, gResTexts[TX_WORD_CLOSE], bsGame);
  Button_Close.SetPosCenterW;
  Button_Close.OnClick := MissionParams_CloseClick;
  Button_Close.Anchors := [anLeft, anRight, anBottom];

  // Shrink panel height after all childs were added, so they will fit in according to their anchors
  Panel_MissionParams.Height := aParent.ItemsPanel.Height - 10;


  Panel_Weather := TKMPanel.Create(Panel_MissionParams, 0, 0, Panel_MissionParams.Width, 300);
  Panel_Weather.Centerize;

    with TKMBevel.Create(Panel_Weather, -2000, -2000, 6000, 6000) do
      HideParentOnClick;

    TKMBevel.Create(Panel_Weather, 0, 0, Panel_Weather.Width, Panel_Weather.Height);
    with TKMButton.Create(Panel_Weather, -5, -5, Panel_Weather.Width + 10, Panel_Weather.Height + 10, '', bsPaper) do
      Hitable := false;

    with TKMButton.Create(Panel_Weather, 0, Panel_Weather.Height - 35, 100, 30, gResTexts[106], bsMenu) do
      HideParentOnClick;


    TKMBevel.Create(Panel_Weather,0,40,Panel_Weather.Width div 2 - 3, 100);
    TKMBevel.Create(Panel_Weather,Panel_Weather.Width div 2 + 2,40,Panel_Weather.Width div 2 - 5, 100);


    CheckBox_Overwrite := TKMCheckBox.Create(Panel_Weather,0,0,150,20,gResTexts[2066], fntMetal);
    CheckBox_Overwrite.OnClick := WeatherChange;
    CheckBox_Overwrite.Hint := gResTexts[2067];
    CheckBox_Overwrite.MobilHint := true;

    fWeatherStartIndex := Panel_Weather.ChildCount - 1;

    TKMLabel.Create(Panel_Weather,0,20,Panel_Weather.Width div 2,20,gResTexts[2053] + ':', fntMetal,taLeft);

    CheckBox_Enabled := TKMCheckBox.Create(Panel_Weather,100,20,150,20,gResTexts[2065], fntMetal);
    CheckBox_Enabled.OnClick := WeatherChange;
    CheckBox_Enabled.Hint := gResTexts[2054];
    CheckBox_Enabled.MobilHint := true;


    TKMLabel.Create(Panel_Weather,6,43,Panel_Weather.Width div 2,20,gResTexts[2063] + ':', fntMetal,taLeft);

    TrackBar_MaxCount := TKMNumericEdit.Create(Panel_Weather, 10, 60, 1, 20);
    TrackBar_MaxCount.OnChange := WeatherChange;
    TrackBar_MaxCount.Width := 75;
    TrackBar_MaxCount.TextAlign := taCenter;
    TrackBar_MaxCount.Hint := gResTexts[2055];
    TrackBar_MaxCount.MobilHint := true;

    TrackBar_MaxSpawnCount := TKMNumericEdit.Create(Panel_Weather, 100, 60, 1, 10);
    TrackBar_MaxSpawnCount.OnChange := WeatherChange;
    TrackBar_MaxSpawnCount.Width := 75;
    TrackBar_MaxSpawnCount.TextAlign := taCenter;
    TrackBar_MaxSpawnCount.Hint := gResTexts[2056];
    TrackBar_MaxSpawnCount.MobilHint := true;

    TKMLabel.Create(Panel_Weather,6,85,Panel_Weather.Width div 2,20,gResTexts[2062] + ':', fntMetal,taLeft);

    TrackBar_MinInterval := TKMNumericEdit.Create(Panel_Weather, 10, 105, 1, 60);
    TrackBar_MinInterval.OnChange := WeatherChange;
    TrackBar_MinInterval.Width := 75;
    TrackBar_MinInterval.TextAlign := taCenter;
    TrackBar_MinInterval.Hint := gResTexts[2057];
    TrackBar_MinInterval.MobilHint := true;

    TrackBar_MaxInterval := TKMNumericEdit.Create(Panel_Weather, 100, 105, 1, 300);
    TrackBar_MaxInterval.OnChange := WeatherChange;
    TrackBar_MaxInterval.Width := 75;
    TrackBar_MaxInterval.TextAlign := taCenter;
    TrackBar_MaxInterval.Hint := gResTexts[2058];
    TrackBar_MaxCount.Width := 75;
    TrackBar_MaxInterval.MobilHint := true;

    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 43,Panel_Weather.Width div 2,20,gResTexts[2059] + ':', fntMetal,taLeft);

    TrackBar_MaxLifeTime := TKMNumericEdit.Create(Panel_Weather, 560, 43, 1, 120);
    TrackBar_MaxLifeTime.OnChange := WeatherChange;
    TrackBar_MaxLifeTime.Width := 75;
    TrackBar_MaxLifeTime.TextAlign := taCenter;
    TrackBar_MaxLifeTime.Hint := gResTexts[2064];
    TrackBar_MaxLifeTime.Width := 75;
    TrackBar_MaxLifeTime.MobilHint := true;

    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 73,Panel_Weather.Width div 2,20,gResTexts[2060] + ':', fntMetal,taLeft);
    TrackBar_MaxCloudSpeed := TKMNumericEdit.Create(Panel_Weather, 560, 73, 1, 20);
    TrackBar_MaxCloudSpeed.OnChange := WeatherChange;
    TrackBar_MaxCloudSpeed.Hint := gResTexts[2060];
    TrackBar_MaxCloudSpeed.Width := 75;
    TrackBar_MaxCloudSpeed.TextAlign := taCenter;
    TrackBar_MaxCloudSpeed.MobilHint := true;

    TKMLabel.Create(Panel_Weather, Panel_Weather.Width div 2 + 6, 103,Panel_Weather.Width div 2,20,gResTexts[2061] + ':', fntMetal,taLeft);
    TrackBar_DecParticles := TKMNumericEdit.Create(Panel_Weather, 560, 103, 1, 10);
    TrackBar_DecParticles.OnChange := WeatherChange;
    TrackBar_DecParticles.Hint := gResTexts[2068];
    TrackBar_DecParticles.Width := 75;
    TrackBar_DecParticles.TextAlign := taCenter;
    TrackBar_DecParticles.MobilHint := true;

    TKMBevel.Create(Panel_Weather,0,165,Panel_Weather.Width div 2 - 3,90);

    TrackBar_NightSpeed := TKMTrackBar.Create(Panel_Weather, 5, 170, Panel_Weather.Width div 2 - 13, 0, MAX_NIGHT_SPEED - 1);
    TrackBar_NightSpeed.Position := 10;
    TrackBar_NightSpeed.OnChange := WeatherChange;
    TrackBar_NightSpeed.Caption := gResTexts[2070];
    TrackBar_NightSpeed.Hint := gResTexts[2072];
    TrackBar_NightSpeed.MobilHint := true;

    TrackBar_NightTime := TKMTrackBar.Create(Panel_Weather, 5, 210, Panel_Weather.Width div 2 - 13, 0, 24);
    TrackBar_NightTime.Position := 12;
    TrackBar_NightTime.OnChange := WeatherChange;
    TrackBar_NightTime.Caption := gResTexts[2071];
    TrackBar_NightTime.Hint := gResTexts[2073];
    TrackBar_NightTime.MobilHint := true;

    TKMBevel.Create(Panel_Weather,Panel_Weather.Width div 2 + 2,145,Panel_Weather.Width div 2 - 5, 90);

    CheckBox_DynamicLight := TKMCheckBox.Create(Panel_Weather,Panel_Weather.Width div 2 + 6,150,200,20,gResTexts[2099], fntGrey);
    CheckBox_DynamicLight.OnClick := WeatherChange;
    CheckBox_DynamicLight.Hint := gResTexts[2100];

    CheckBox_DynamicShadow := TKMCheckBox.Create(Panel_Weather,Panel_Weather.Width div 2 + 6,175,250,20,gResTexts[2310], fntGrey);
    CheckBox_DynamicShadow.OnClick := WeatherChange;
    CheckBox_DynamicShadow.Hint := gResTexts[2311];



  Panel_Weather.Hide;

  CreateMessages;
end;

procedure TKMMapEdMissionMode.CreateMessages;
var I : Integer;
begin
  PopUp_Texts := TKMPanel.Create(Panel_MissionParams, 0, 0, Panel_MissionParams.Width, 500);
  PopUp_Texts.Centerize;

  with TKMBevel.Create(PopUp_Texts, -2000, -2000, 6000, 6000) do
    HideParentOnClick;

  TKMBevel.Create(PopUp_Texts, 0, 0, PopUp_Texts.Width, PopUp_Texts.Height);
  with TKMButton.Create(PopUp_Texts, -5, -5, PopUp_Texts.Width + 10, PopUp_Texts.Height + 10, '', bsPaper) do
    Hitable := false;

  with TKMButton.Create(PopUp_Texts, 0, PopUp_Texts.Height - 35, 100, 30, gResTexts[106], bsMenu) do
    HideParentOnClick;

  DropList_Locales := TKMDropList.Create(PopUp_Texts, 10, 10, 200, 20, fntGrey, gResTexts[310], bsGame);
  DropList_Locales.Hint := gResTexts[310];
  for I := 0 to gResLocales.Count - 1 do
  begin
    DropList_Locales.Add(gResLocales.Locales[I].Title, I);
    if gResLocales.Locales[I].Code = gResLocales.UserLocale then
      DropList_Locales.ItemIndex := I;
  end;
  DropList_Locales.OnChange := MessagesChange;

  Edit_TextID := TKMNumericEdit.Create(PopUp_Texts, DropList_Locales.Right + 10, 10, 0, 500);
  Edit_TextID.Width := 100;
  Edit_TextID.Hint := gResTexts[1151];
  Edit_TextID.OnChange := MessagesChange;

  TKMLabel.Create(PopUp_Texts, Edit_TextID.Right + 10, 12, 200, 17, gResTexts[1151], fntMetal, taLeft);

  Edit_Text := TKMEdit.Create(PopUp_Texts, 10, 40, PopUp_Texts.Width - 20, 20, fntGrey);
  Edit_Text.OnChange := MessagesChange;
  Edit_Text.MaxLen := high(word);
  Memo_Text := TKMMemo.Create(PopUp_Texts, 10, Edit_Text.Bottom + 20,
                              PopUp_Texts.Width - 20, 175,
                              fntGrey, bsMenu, false);
  Memo_Text.WordWrap := true;

  DropList_Locales2 := TKMDropList.Create(PopUp_Texts, 10, Memo_Text.Bottom + 10, 200, 20, fntGrey, gResTexts[310], bsGame);
  DropList_Locales2.Hint := gResTexts[310];
  for I := 0 to gResLocales.Count - 1 do
  begin
    DropList_Locales2.Add(gResLocales.Locales[I].Title, I);
    if gResLocales.Locales[I].Code = gResLocales.UserLocale then
      DropList_Locales2.ItemIndex := I;
  end;
  DropList_Locales2.OnChange := MessagesChange2;

  TKMLabel.Create(PopUp_Texts, DropList_Locales2.Right + 10, DropList_Locales2.Top + 2, 350, 17, gResTexts[2117], fntMetal, taLeft);

  Memo_Text2 := TKMMemo.Create(PopUp_Texts, 10, DropList_Locales2.Bottom + 10,
                              PopUp_Texts.Width - 20, 165,
                              fntGrey, bsMenu, true);
  Memo_Text2.WordWrap := true;



  PopUp_Texts.Hide;
end;


function TKMMapEdMissionMode.MissionParams_OnKeyDown(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True; // We want to handle all keys here

  if (Key = VK_ESCAPE) and Button_Close.IsClickable then
    MissionParams_CloseClick(Button_Close);
end;


procedure TKMMapEdMissionMode.MissionParams_Click(Sender: TObject);
begin
  PopUp_MissionParams.Show;
end;


procedure TKMMapEdMissionMode.MissionParams_CloseClick(Sender: TObject);
begin
  PopUp_MissionParams.Hide;
end;

procedure TKMMapEdMissionMode.ShowPanels(Sender: TObject);
begin
  if Sender = Button_Weather then
  begin
    Panel_Weather.Show;
    Exit;
  end;
  if Sender = Button_Texts then
  begin
    MessagesChange(DropList_Locales);
    MessagesChange2(nil);
    PopUp_Texts.Show;
    Exit;
  end;

  Panel_Difficulty.Visible := not Panel_Difficulty.Visible;
  if Panel_Difficulty.Visible then
    Button_Difficulty.Caption := gResTexts[TX_MAPED_MISSION_DIFFICULTY_LEVELS] + ' /\'
  else
    Button_Difficulty.Caption := gResTexts[TX_MAPED_MISSION_DIFFICULTY_LEVELS] + ' \/';
end;

procedure TKMMapEdMissionMode.WeatherChange(Sender: TObject);
begin
  UpdateMapTxtInfo(nil);
  RefreshWeather;
end;

procedure TKMMapEdMissionMode.RefreshWeather;
var I : Integer;
begin
  for I := fWeatherStartIndex to Panel_Weather.ChildCount - 1 do
    if Panel_Weather.Childs[I] <> CheckBox_Overwrite  then
      if Panel_Weather.Childs[I] = CheckBox_Enabled then
        Panel_Weather.Childs[I].Enabled := CheckBox_Overwrite.Checked
      else
        Panel_Weather.Childs[I].Enabled := CheckBox_Overwrite.Checked and CheckBox_Enabled.Checked;

  TrackBar_MaxInterval.ValueMin := TrackBar_MinInterval.Value + 1;
  TrackBar_MaxInterval.Value := TrackBar_MaxInterval.Value;
  TrackBar_NightSpeed.Enabled := CheckBox_Overwrite.Checked;
  TrackBar_NightTime.Enabled := CheckBox_Overwrite.Checked;

  CheckBox_DynamicLight.Checked := gGame.MapTxtInfo.Weather.DynamicLight;
  CheckBox_DynamicShadow.Checked := gGame.MapTxtInfo.Weather.DynamicShadow;
end;


procedure TKMMapEdMissionMode.Mission_ModeChange(Sender: TObject);
begin
  gGameParams.MissionMode := TKMissionMode(Radio_MissionMode.ItemIndex);
end;


procedure TKMMapEdMissionMode.AIBuilderChange(Sender: TObject);
var
  I: Integer;
begin
  if Sender = Button_AIBuilderSetup then
  begin
    Button_AIBuilderOK.Show;
    Button_AIBuilderCancel.Show;
    Button_AIBuilderWarn.Show;
    Button_AIBuilderSetup.Hide;
  end;

  if Sender = Button_AIBuilderOK then
    for I := 0 to gHands.Count-1 do
      gGame.MapEditor.ApplyAIMultiplayerSetup(I);

  if (Sender = Button_AIBuilderOK) or (Sender = Button_AIBuilderCancel) then
  begin
    Button_AIBuilderOK.Hide;
    Button_AIBuilderCancel.Hide;
    Button_AIBuilderWarn.Hide;
    Button_AIBuilderSetup.Show;
  end;
end;


procedure TKMMapEdMissionMode.Mission_ModeUpdate;
begin
  Radio_MissionMode.ItemIndex := Ord(gGameParams.MissionMode);
end;


procedure TKMMapEdMissionMode.Hide;
begin
  Panel_Mode.Hide;
end;


procedure TKMMapEdMissionMode.RadioMissionDesc_Changed(Sender: TObject);
begin
  Edit_SmallDesc.Visible := Radio_SmallDescType.ItemIndex = 0;
  NumEdit_SmallDesc.Visible := Radio_SmallDescType.ItemIndex = 1;

  Edit_BigDesc.Visible := Radio_BigDescType.ItemIndex = 0;
  //Memo_BigDesc.Enabled := Radio_BigDescType.ItemIndex = 0;
  NumEdit_BigDesc.Visible := Radio_BigDescType.ItemIndex = 1;

  UpdateMapTxtInfo(nil);
end;


procedure TKMMapEdMissionMode.UpdateMapTxtInfo(Sender: TObject);
var
  MD: TKMMissionDifficulty;
begin
  if fUpdating then Exit;

  if CheckBox_Coop.Checked then
  begin
    CheckBox_BlockTeamSelection.Check;
    CheckBox_BlockPeacetime.Check;
    CheckBox_BlockFullMapPreview.Check;
    CheckBox_BlockTeamSelection.Disable;
    CheckBox_BlockPeacetime.Disable;
    CheckBox_BlockFullMapPreview.Disable;
  end else
  begin
    CheckBox_BlockTeamSelection.Enable;
    CheckBox_BlockPeacetime.Enable;
    CheckBox_BlockFullMapPreview.Enable;
  end;

 If NumEdit_MapName.Value >= 0 then
    Memo_MapName.Text := gGame.TextMission.ParseTextMarkup(UnicodeString('<$' + IntToStr(NumEdit_MapName.Value) + '>'))
  else
    Memo_MapName.Text := '';

  gGame.MapTxtInfo.SetNameLibxAndTranslation(NumEdit_MapName.Value, '');

  Memo_BigDesc.Text := Edit_BigDesc.Text;
  gGame.MapTxtInfo.Author := Edit_Author.Text;
  gGame.MapTxtInfo.Version := Edit_Version.Text;

  case Radio_SmallDescType.ItemIndex of
    0: gGame.MapTxtInfo.SmallDesc := Edit_SmallDesc.Text;
    1: gGame.MapTxtInfo.SetSmallDescLibxAndTranslation(NumEdit_SmallDesc.Value, '');
  end;

  case Radio_BigDescType.ItemIndex of
    0: gGame.MapTxtInfo.BigDesc := Edit_BigDesc.Text;
    1: gGame.MapTxtInfo.SetBigDescLibxAndTranslation(NumEdit_BigDesc.Value, '');
  end;

  case Radio_BigDescType.ItemIndex of
    0: gGame.MapTxtInfo.BigDesc := Edit_BigDesc.Text;
    1:  If NumEdit_BigDesc.Value >= 0 then
          Memo_BigDesc.Text := gGame.TextMission.ParseTextMarkup(UnicodeString('<$' + IntToStr(NumEdit_BigDesc.Value) + '>'))
        else
          Memo_BigDesc.Text := '';
  end;

  gGame.MapTxtInfo.IsCoop         := CheckBox_Coop.Checked;
  gGame.MapTxtInfo.IsSpecial      := CheckBox_Special.Checked;
  gGame.MapTxtInfo.IsRMG          := CheckBox_RMG.Checked;
  gGame.MapTxtInfo.IsPlayableAsSP := CheckBox_PlayableAsSP.Checked;

  gGame.MapTxtInfo.BlockTeamSelection  := CheckBox_BlockTeamSelection.Checked;
  gGame.MapTxtInfo.BlockColorSelection := CheckBox_BlockColorSelection.Checked;
  gGame.MapTxtInfo.BlockPeacetime      := CheckBox_BlockPeacetime.Checked;
  gGame.MapTxtInfo.BlockFullMapPreview := CheckBox_BlockFullMapPreview.Checked;

  gGame.MapTxtInfo.DifficultyLevels := [];

  gGame.MapTxtInfo.Weather.Overwrite := CheckBox_Overwrite.Checked;
  gGame.MapTxtInfo.Weather.Enabled := CheckBox_Enabled.Checked;
  gGame.MapTxtInfo.Weather.MaxCount := TrackBar_MaxCount.Value;
  gGame.MapTxtInfo.Weather.MaxSpawnCount := TrackBar_MaxSpawnCount.Value;
  gGame.MapTxtInfo.Weather.MinInterval := TrackBar_MinInterval.Value * 10;
  gGame.MapTxtInfo.Weather.MaxInterval := TrackBar_MaxInterval.Value * 10;
  gGame.MapTxtInfo.Weather.MaxLifeTime := TrackBar_MaxLifeTime.Value * 10;
  gGame.MapTxtInfo.Weather.MaxCloudSpeed := TrackBar_MaxCloudSpeed.Value / 200;
  gGame.MapTxtInfo.Weather.DecParticles := TrackBar_DecParticles.Value - 1;
  gGame.MapTxtInfo.Weather.NightSpeed := TrackBar_NightSpeed.Position;
  gGame.MapTxtInfo.Weather.NightTime := TrackBar_NightTime.Position;
  gGame.MapTxtInfo.Weather.DynamicLight := CheckBox_DynamicLight.Checked;
  gGame.MapTxtInfo.Weather.DynamicShadow := CheckBox_DynamicShadow.Checked;


  for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
    if CheckBox_Difficulty[MD].Checked then
      Include(gGame.MapTxtInfo.DifficultyLevels, MD);
end;


procedure TKMMapEdMissionMode.UpdateMapParams;
var
  MD: TKMMissionDifficulty;
begin
  fUpdating := True;
  try
    Edit_Author.Text := gGame.MapTxtInfo.Author;
    Edit_Version.Text := gGame.MapTxtInfo.Version;

    Radio_SmallDescType.ItemIndex := Ord(gGame.MapTxtInfo.IsSmallDescLibxSet);
    Radio_BigDescType.ItemIndex := Ord(gGame.MapTxtInfo.IsBigDescLibxSet);

    Edit_SmallDesc.Text     := gGame.MapTxtInfo.SmallDesc; // If there's Libx it won't show up
    NumEdit_SmallDesc.Value := gGame.MapTxtInfo.SmallDescLibx;
    Edit_BigDesc.Text       := gGame.MapTxtInfo.BigDescToDisplay;
    NumEdit_BigDesc.Value   := gGame.MapTxtInfo.BigDescLibx;
    NumEdit_MapName.Value   := gGame.MapTxtInfo.NameLibx;
    Memo_BigDesc.Text       := Edit_BigDesc.Text;

    CheckBox_Coop.Checked         := gGame.MapTxtInfo.IsCoop;
    CheckBox_Special.Checked      := gGame.MapTxtInfo.IsSpecial;
    CheckBox_RMG.Checked          := gGame.MapTxtInfo.IsRMG;
    CheckBox_PlayableAsSP.Checked := gGame.MapTxtInfo.IsPlayableAsSP;

    CheckBox_BlockTeamSelection.Checked   := gGame.MapTxtInfo.BlockTeamSelection;
    CheckBox_BlockColorSelection.Checked  := gGame.MapTxtInfo.BlockColorSelection;
    CheckBox_BlockPeacetime.Checked       := gGame.MapTxtInfo.BlockPeacetime;
    CheckBox_BlockFullMapPreview.Checked  := gGame.MapTxtInfo.BlockFullMapPreview;

    for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
      CheckBox_Difficulty[MD].Checked := MD in gGame.MapTxtInfo.DifficultyLevels;

    CheckBox_Overwrite.Checked    := gGame.MapTxtInfo.Weather.Overwrite;
    CheckBox_Enabled.Checked      := gGame.MapTxtInfo.Weather.Enabled;
    TrackBar_MaxCount.Value       := gGame.MapTxtInfo.Weather.MaxCount;
    TrackBar_MaxSpawnCount.Value  := gGame.MapTxtInfo.Weather.MaxSpawnCount;
    TrackBar_MinInterval.Value    := gGame.MapTxtInfo.Weather.MinInterval div 10;
    TrackBar_MaxInterval.Value    := gGame.MapTxtInfo.Weather.MaxInterval div 10;
    TrackBar_MaxLifeTime.Value    := gGame.MapTxtInfo.Weather.MaxLifeTime div 10;
    TrackBar_MaxCloudSpeed.Value  := Round(gGame.MapTxtInfo.Weather.MaxCloudSpeed * 200);
    TrackBar_DecParticles.Value   := gGame.MapTxtInfo.Weather.DecParticles + 1;
    TrackBar_NightSpeed.Position  := gGame.MapTxtInfo.Weather.NightSpeed;
    TrackBar_NightTime.Position   := gGame.MapTxtInfo.Weather.NightTime;

    RefreshWeather;
  finally
    fUpdating := False;
  end;

  RadioMissionDesc_Changed(nil);
end;

procedure TKMMapEdMissionMode.MessagesChange(Sender: TObject);
var id, locId : Integer;
begin
  if gGame.TextMission = nil then
    Exit;
  id := Edit_TextID.Value;
  locId := Max(DropList_Locales.ItemIndex, 0);
  if Sender = Edit_TextID then
  begin
    Edit_Text.SetTextSilently(gGame.TextMission.GetText(locId, id) );
    Edit_Text.CursorPos := 0;
    Memo_Text.Text := gGame.TextMission.GetText(locId, id);
    MessagesChange2(nil);
  end else
  if sender = Edit_Text then
  begin
    gGame.TextMission.SetText(locId, id, Edit_Text.Text);
    Memo_Text.Text := Edit_Text.Text;
  end else
  if sender = DropList_Locales then     
  begin
    Edit_Text.SetTextSilently(gGame.TextMission.GetText(locId, id) );
    Edit_Text.CursorPos := 0;
    Memo_Text.Text := gGame.TextMission.GetText(locId, id);
  end;

end;

procedure TKMMapEdMissionMode.MessagesChange2(Sender: TObject);
var id, locId : Integer;
begin
  if gGame.TextMission = nil then
    Exit;
  id := Edit_TextID.Value;
  locId := Max(DropList_Locales2.ItemIndex, 0);

  Memo_Text2.Text := gGame.TextMission.GetText(locId, id);
end;


procedure TKMMapEdMissionMode.Show;
begin
  Mission_ModeUpdate;
  Panel_Mode.Show;
  AIBuilderChange(Button_AIBuilderCancel); //Hide confirmation
  UpdateMapParams;
end;


function TKMMapEdMissionMode.Visible: Boolean;
begin
  Result := Panel_Mode.Visible;
end;


end.
