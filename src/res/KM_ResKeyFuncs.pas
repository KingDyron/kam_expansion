unit KM_ResKeyFuncs;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, Math, Generics.Collections,
  KM_ResTexts,
  KM_ResTypes,
  KM_KeysSettings;


type
  TKMKeyFuncSpec = record
    TextId: Word;     // Text description of the function
    Area: TKMKeyFuncArea; // Area of effect for the function (common, game, maped)
    IsChangableByPlayer: Boolean; // Hide debug key and its function from UI
  end;

  TKMResKeyFuncs = class
  private
    fFuncs: array [TKMKeyFunction] of TKMKeyFuncSpec;
    function GetFunc(aKeyFunc: TKMKeyFunction): TKMKeyFuncSpec;
    procedure SetFunc(aKeyFunc: TKMKeyFunction; const aFuncInfo: TKMKeyFuncSpec);
    function GetCount: Integer;
  public
    constructor Create;
    function GetKeyFunctionName(aKeyFunc: TKMKeyFunction): string;

    property Count: Integer read GetCount;
    property Funcs[aKeyFunc: TKMKeyFunction]: TKMKeyFuncSpec read GetFunc write SetFunc; default;
  end;

var
  // All Keys accessible from everywhere
  gResKeyFuncs: TKMResKeyFuncs;

implementation
uses
  TypInfo{, RTTI};

const
  // Function text values
  KEY_FUNC_TX: array [TKMKeyFunction] of Word = (
    //Common Keys
    0,
    TX_KEY_FUNC_SCROLL_LEFT, TX_KEY_FUNC_SCROLL_RIGHT, TX_KEY_FUNC_SCROLL_UP, TX_KEY_FUNC_SCROLL_DOWN,    // Scroll Left, Right, Up, Down
    TX_KEY_FUNC_MAP_DRAG_SCROLL,                                                                          // Map drag scroll
    TX_KEY_FUNC_ZOOM_IN, TX_KEY_FUNC_ZOOM_OUT, TX_KEY_FUNC_ZOOM_RESET,                                    // Zoom In/Out/Reset
    TX_KEY_FUNC_CLOSE_MENU,                                                                               // Close opened menu
    TX_KEY_FUNC_MUSIC_PREV_TRACK, TX_KEY_FUNC_MUSIC_NEXT_TRACK,                                           // Music track prev / next
    TX_KEY_FUNC_MUSIC_DISABLE, TX_KEY_FUNC_MUSIC_SHUFFLE,                                                 // Music disable / shuffle
    TX_KEY_FUNC_MUSIC_VOLUME_UP, TX_KEY_FUNC_MUSIC_VOLUME_DOWN, TX_KEY_FUNC_MUSIC_MUTE,                   // Music volume up / down / mute
    TX_KEY_FUNC_SOUND_VOLUME_UP, TX_KEY_FUNC_SOUND_VOLUME_DOWN, TX_KEY_FUNC_SOUND_MUTE,                   // Sound volume up / down / mute
    TX_KEY_FUNC_MUTE_ALL,                                                                                 // Mute music and sound

    TX_KEY_FUNC_DBG_WINDOW,                                                                               // Debug window

    // These keys are not changable by Player in Options menu
    TX_KEY_FUNC_DBG_MAP, TX_KEY_FUNC_DBG_VICTORY, TX_KEY_FUNC_DBG_DEFEAT, TX_KEY_FUNC_DBG_SCOUT,          // Debug (Show Map, Victory, Defeat, Add Scout)

    // Game Keys
    TX_KEY_FUNC_MENU_BUILD, TX_KEY_FUNC_MENU_RATIO, TX_KEY_FUNC_MENU_STATS, TX_KEY_FUNC_MENU_MAIN,        // Game menus
    TX_KEY_FUNC_GAME_SPEED_1,TX_KEY_FUNC_GAME_SPEED_2,TX_KEY_FUNC_GAME_SPEED_3,TX_KEY_FUNC_GAME_SPEED_4,  // Speed ups
    TX_KEY_FUNC_BEACON, TX_KEY_FUNC_PAUSE, TX_KEY_FUNC_SHOW_TEAMS,                                        // Beacon/Pause/Show team in MP
    TX_KEY_FUNC_CENTER_ALERT, TX_KEY_FUNC_DELETE_MSG, TX_KEY_FUNC_SHOW_GAME_CHAT,                         // Center to alert/Delete message/Show chat
    TX_KEY_FUNC_SEL_NXT_BLD_UNIT_SAME_TYPE,                                                               // Select next building/unit/group with same type
    TX_KEY_FUNC_PLAYER_COLOR_MODE,                                                                        // Player color mode
    TX_KEY_FUNC_PLAN_ROAD, TX_KEY_FUNC_PLAN_FIELD, TX_KEY_FUNC_PLAN_WINE, TX_KEY_FUNC_ERASE_PLAN,         // Plan road/corn/wine/erase plan(building)
    TX_KEY_FUNC_SELECT_1, TX_KEY_FUNC_SELECT_2, TX_KEY_FUNC_SELECT_3, TX_KEY_FUNC_SELECT_4, TX_KEY_FUNC_SELECT_5,   // Dynamic selection groups 1-5
    TX_KEY_FUNC_SELECT_6, TX_KEY_FUNC_SELECT_7, TX_KEY_FUNC_SELECT_8, TX_KEY_FUNC_SELECT_9, TX_KEY_FUNC_SELECT_10,  // Dynamic selection groups 6-10
    TX_KEY_FUNC_SELECT_11,TX_KEY_FUNC_SELECT_12,TX_KEY_FUNC_SELECT_13,TX_KEY_FUNC_SELECT_14,TX_KEY_FUNC_SELECT_15,  // Dynamic selection groups 11-15
    TX_KEY_FUNC_SELECT_16,TX_KEY_FUNC_SELECT_17,TX_KEY_FUNC_SELECT_18,TX_KEY_FUNC_SELECT_19,TX_KEY_FUNC_SELECT_20,  // Dynamic selection groups 16-20
    //Unit keys
    TX_KEY_FUNC_HALT, TX_KEY_FUNC_SPLIT, TX_KEY_FUNC_LINKUP, TX_KEY_FUNC_FOOD, TX_KEY_FUNC_STORM,         // Army commands
    TX_KEY_FUNC_FORM_INCREASE, TX_KEY_FUNC_FORM_DECREASE, TX_KEY_FUNC_TURN_CW, TX_KEY_FUNC_TURN_CCW,      // Army commands
    //House keys
    TX_KEY_FUNC_TRAIN_PREV, TX_KEY_FUNC_TRAIN_EQUIP, TX_KEY_FUNC_TRAIN_NEXT,                              // School/Barracks/TH commands

    // Spectate MP game/Replay view Keys
    TX_KEY_FUNC_SPECPANEL_DROPBOX_OPEN_CLOSE,
    TX_KEY_FUNC_REPLAY_PLAY_NEXT_TICK,
    TX_KEY_FUNC_SPECTATE_PLAYER_1, TX_KEY_FUNC_SPECTATE_PLAYER_2, TX_KEY_FUNC_SPECTATE_PLAYER_3, TX_KEY_FUNC_SPECTATE_PLAYER_4,    // Spectator/Replay player switch
    TX_KEY_FUNC_SPECTATE_PLAYER_5, TX_KEY_FUNC_SPECTATE_PLAYER_6, TX_KEY_FUNC_SPECTATE_PLAYER_7, TX_KEY_FUNC_SPECTATE_PLAYER_8,    // Spectator/Replay player switch
    TX_KEY_FUNC_SPECTATE_PLAYER_9, TX_KEY_FUNC_SPECTATE_PLAYER_10, TX_KEY_FUNC_SPECTATE_PLAYER_11, TX_KEY_FUNC_SPECTATE_PLAYER_12, // Spectator/Replay player switch

    // Map Editor Keys
    TX_KEY_FUNC_MAPEDIT_EXTRA,                                                                            // Map Editor Extra's menu
    TX_KEY_FUNC_MAPEDIT_SAVE,                                                                             // Map Editor Save map
    TX_KEY_FUNC_MAPEDIT_TERAIN_EDIT, TX_KEY_FUNC_MAPEDIT_VILLAGE_PLAN,                                    // Map Editor menus
    TX_KEY_FUNC_MAPEDIT_VISUAL_SCRIPT, TX_KEY_FUNC_MAPEDIT_GLOBAL_SCRIPT, TX_KEY_FUNC_MAPEDIT_MENU_MAIN,  // Map Editor menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_1, TX_KEY_FUNC_MAPEDIT_SUBMENU_2, TX_KEY_FUNC_MAPEDIT_SUBMENU_3,          // Map Editor sub-menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_4, TX_KEY_FUNC_MAPEDIT_SUBMENU_5, TX_KEY_FUNC_MAPEDIT_SUBMENU_6,          // Map Editor sub-menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_7,          // Map Editor sub-menus
    TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_1, TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_2,   // Map Editor sub-menu actions
    TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_3, TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_4,   // Map Editor sub-menu actions
    TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_5, TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_6,   // Map Editor sub-menu actions
    TX_KEY_FUNC_MAPEDIT_SUBMENU_ACTION_7,                                         // Map Editor sub-menu actions
    TX_KEY_FUNC_MAPEDIT_OBJ_PALETTE,                                              // Map Editor show objects palette
    TX_KEY_FUNC_MAPEDIT_TILES_PALETTE,                                            // Map Editor show tiles palette
    TX_KEY_FUNC_MAPEDIT_UNIV_ERASOR,                                              // Map Editor universal erasor
    TX_KEY_FUNC_MAPEDIT_PAINT_BUCKET,                                             // Map Editor paint bucket
    TX_KEY_FUNC_MAPEDIT_HISTORY,                                                  // Map Editor history
    TX_KEY_FUNC_MAPEDIT_FLAT_TERRAIN,                                             // Map Editor flat terrain
    TX_KEY_FUNC_MAPEDIT_TILES_GRID                                                // Map Editor tiles grid
  );

{ TKMResKeys }
constructor TKMResKeyFuncs.Create;
var
  KF: TKMKeyFunction;
begin
  inherited;

  for KF := KEY_FUNC_LOW to High(TKMKeyFunction) do
  begin
    fFuncs[KF].TextId := KEY_FUNC_TX[KF];

    case KF of
      kfScrollLeft..kfDebugAddscout:    fFuncs[KF].Area := faCommon;
      kfMenuBuild..kfSelect20:          fFuncs[KF].Area := faGame;
      kfArmyHalt..kfArmyRotateCcw:      fFuncs[KF].Area := faUnit;
      kfTrainGotoPrev..kfTrainGotoNext: fFuncs[KF].Area := faHouse;
      kfSpecpanelSelectDropbox..kfSpectatePlayer12: fFuncs[KF].Area := faSpecReplay;
      else    fFuncs[KF].Area := faMapEdit;
    end;

    fFuncs[KF].IsChangableByPlayer := (KF in [kfDebugRevealmap..kfDebugAddscout]);
  end;
end;


function TKMResKeyFuncs.GetCount: Integer;
begin
  Result := Integer(High(TKMKeyFunction));
end;


function TKMResKeyFuncs.GetFunc(aKeyFunc: TKMKeyFunction): TKMKeyFuncSpec;
begin
  Result := fFuncs[aKeyFunc];
end;


procedure TKMResKeyFuncs.SetFunc(aKeyFunc: TKMKeyFunction; const aFuncInfo :TKMKeyFuncSpec);
begin
  fFuncs[aKeyFunc] := aFuncInfo;
end;


function TKMResKeyFuncs.GetKeyFunctionName(aKeyFunc: TKMKeyFunction): string;
begin
  Result := gResTexts[KEY_FUNC_TX[aKeyFunc]];
end;


end.
