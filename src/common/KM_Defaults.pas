unit KM_Defaults;
{$I KaM_Remake.inc}
interface
uses
  SysUtils;

//Global constants
const
//|===================| <- constant name length
  MAX_MAP_SIZE          = 512;
  MIN_MAP_SIZE          = 32;
  CELL_SIZE_PX          = 40;           //Single cell size in pixels (width)
  CELL_HEIGHT_DIV       = 33.333;       //Height divider, controlls terrains pseudo-3d look
  TOOLBAR_WIDTH         = 224;          //Toolbar width in game
  MAPED_TOOLBAR_WIDTH   = 250;          //Toolbar width in map editor
  TERRAIN_PACE          = 200;          //Each tile gets updated once per ** ticks (100 by default), Warning, it affects field/tree growth rate
  FOW_PACE              = 10;           //Each tile gets updated once per ** ticks (10 by default)

  MIN_FPS_CAP           = 10;           //Minimum FPS Cap - limit fps
  DEF_FPS_CAP           = 60;           //Default FPS Cap
  MAX_FPS_CAP           = 1000;         //Maximum FPS Cap (means no CAP at all)
  FPS_INTERVAL          = 1000;         //Time in ms between FPS measurements, bigger value = more accurate result
  MENU_DESIGN_X         = 1024;         //Thats the size menu was designed for. All elements are placed in this size
  MENU_DESIGN_Y         = 768;          //Thats the size menu was designed for. All elements are placed in this size
  MIN_RESOLUTION_WIDTH  = 1024;         //Lowest supported resolution X
  MIN_RESOLUTION_HEIGHT = 800;          //Lowest supported resolution Y
  MIN_NIGHT_DARKNESS    = 0.25;
  NIGHT_SPEED           = 360;
  MAX_NIGHT_SPEED       = 21;       //21 * 360
  MAX_X_RESOLUTION      = 1920;
  MAX_Y_RESOLUTION      = 1080;
  SCALE_INTERFACE       = false;
  // It would be nice to have the declaration as:
  // GAME_REVISION_NUM = {$I KM_Revision.inc}
  // This would also allow to disuse the initialization section down below
  // However for some odd reason this kills Reys IDE ..
  // kills mine too :D -> Dyron
  {$I KM_Revision.inc};
  {$I KM_NetProtocolRevision.inc};

var
  GAME_VERSION_POSTFIX: AnsiString = '';
  TERRAIN_DARK: Single;
const
  GAME_VERSION_CUSTOM_POSTFIX = ''; // Custom postfix for the test builds
  GAME_VERSION_PREFIX   = 'KaM Expansion: KamStar United'; //Game version string displayed in menu corner
  VERSION_NAME = '';
var
  //Game revision is set in initialisation block
  GAME_REVISION: AnsiString; //Should be updated for every release (each time save format is changed)
  GAME_VERSION: AnsiString;
  NET_PROTOCOL_REVISON: AnsiString; //Clients of this version may connect to the dedicated server
const
  GAME_TITLE            = 'Knights and Merchants Remake';
  SETTINGS_FILE         = 'KaM Remake Settings KingDyronMod.xml';
  SERVER_SETTINGS_FILE  = 'KaM Remake Server Settings.ini';
  DEFAULT_LOCALE: AnsiString = 'eng';

  MAX_NICKNAME_LENGTH = 16;

  DEL_LOGS_OLDER_THAN   = 14;           //in days

  TEMPLATE_LIBX_FILE_TEXT = 'text.%s.libx';

  DEFAULT_WATER_LIGHT_MULTIPLIER = 1.3; // Default multiplier for terrain water light
const
  //Max number of ticks, played on 1 game update.
  //We must limit number of ticks per update to be able to leave update cycle fast (when turn off ultra fast speedup, f.e.)
  //Also there is a technical limit, of how many ticks we can calculate per update
  MAX_TICKS_PER_GAME_UPDATE = 100;
{$IFDEF DEBUG}
  DEBUG_CFG = True; //Debug preset for most usable debug options. ON for Debug build configuration
{$ELSE}
  DEBUG_CFG = false; //Debug preset for most usable debug options. OFF for Release build configuration
{$ENDIF}
var
  // These should be True (we can occasionally turn them Off to speed up the debug)
  // We keep them as `var` to keep compiler happy (otherwise it sees a lot of "unused var usage" around)
  CALC_EXPECTED_TICK    :Boolean = not DEBUG_CFG;  //Do we calculate expected tick and try to be in-time (send as many tick as needed to get to expected tick)
  MAKE_ANIM_TERRAIN     :Boolean = True;  //Should we animate water and swamps
  MAKE_TEAM_COLORS      :Boolean = True;  //Whenever to make team colors or not, saves RAM for debug
  DYNAMIC_TERRAIN       :Boolean = True;  //Update terrain each tick to grow things
  KAM_WATER_DRAW        :Boolean = True;  //Render underwater sand
  CHEATS_SP_ENABLED     :Boolean = True;  //Enable cheats in game (add_resource, instant_win, etc)
  FREE_POINTERS         :Boolean = True;  //If True, units/houses will be freed and removed from the list once they are no longer needed
  CAP_MAX_FPS           :Boolean = True;  //Should limit rendering performance to avoid GPU overheating (disable to measure debug performance)
  CRASH_ON_REPLAY       :Boolean = True;  //Crash as soon as replay consistency fails (random numbers mismatch)
  BLOCK_DUPLICATE_APP   :Boolean = not DEBUG_CFG; //Do not allow to run multiple games at once (to prevent MP cheating)
  DO_NETWORK_AUTH       :Boolean = not DEBUG_CFG; //Do network auth. If no its equivalent to use KM_NetAuthUnsecure
  QUERY_ON_FORM_CLOSE   :Boolean = not DEBUG_CFG; //Do we ask player about lost changes on game exit ?
  SHOW_DISMISS_UNITS_BTN:Boolean = True; //The button to order citizens go back to school
  RESET_DEBUG_CONTROLS  :Boolean = not DEBUG_CFG; //Reset Debug controls (F11) on game start
  SKIP_LOG_TEMP_COMMANDS:Boolean = True;
  BLOCK_GAME_ON_PAUSE   :Boolean = not DEBUG_CFG; // Should we block game input, viewport scrolling etc on game pause?
  DELETE_OLD_LOGS       :Boolean = not DEBUG_CFG; // Should we delete old logs?

  // Implemented
  FEAT_SETTINGS_IN_MYDOC  :Boolean = True; // Save settings in the C:\Users\Username\My Documents\My Games\GAME_TITLE\ folder
  DO_UNIT_INTERACTION     :Boolean = True; // Debug for unit interaction
  DO_WEIGHT_ROUTES        :Boolean = True; // Add additional cost to tiles in A* if they are occupied by other units (IsUnit=1)
  CUSTOM_RANDOM           :Boolean = True; // Use our custom random number generator or the built in "Random()"
  USE_WALKING_DISTANCE    :Boolean = True; // Use the walking distance for deciding place to mine rather than direct distance
  RANDOM_TARGETS          :Boolean = True; // Archers use random targets instead of closest
  DISPLAY_CHARTS_RESULT   :Boolean = True; // Show charts in game results screen
  HUNGARIAN_GROUP_ORDER   :Boolean = True; // Use Hungarian algorithm to reorder warrior groups when walking
  AI_GEN_NAVMESH          :Boolean = True; // Generate navmesh for AI to plan attacks/defenses
  AI_GEN_INFLUENCE_MAPS   :Boolean = True; // Generate influence maps for AI to plan attacks/defenses
  // Not fully implemented yet
  FEAT_CCL_WALKCONNECT    :Boolean = False; // Use CCL instead of FloodFill for walk-connect (CCL is generaly worse. It's a bit slower, counts 1 tile areas and needs more AreaIDs to work / makes sparsed IDs)
  FEAT_DYNAMIC_FOG_OF_WAR :Boolean = False; // Whenever dynamic fog of war is enabled or not
  FEAT_DISMISS_GROUP_BTN  :Boolean = DEBUG_CFG; // The button to kill group
  CHECK_8087CW            :Boolean = False; // Check that 8087CW (FPU flags) are set correctly each frame, in case some lib/API changed them
  FEAT_SCROLL_ACCEL       :Boolean = False; // Acceleration for viewport scrolling
  PATHFINDER_TO_USE       :Byte = 1;       // Use TKMPathfindingAStarNew

  ENABLE_VIDEOS_UNDER_WINE: Boolean = DEBUG_CFG; //Do we enable videos under wine

  //Cache / delivery / pathfinding
  CACHE_PATHFINDING                       :Boolean = True; //Cache routes incase they are needed soon (Vortamic PF runs x4 faster even with lame approach)
  CACHE_PATHFINDING_AVOID_LOCKED          :Boolean = True; //Cache unsuccesfull tries to find route with avoid locked (as non walkable)
  DELIVERY_BID_CALC_USE_PATHFINDING       :Boolean = True; //Do we use simple distance on map or pathfinding for calc delivery bids cost?
  {$IFDEF WDC} //Work only in Delphi
  CACHE_DELIVERY_BIDS: Boolean = True; //Cache delivery bids cost. Must be turned ON if we want to use pathfinding for bid calc, huge impact on performance in that case
  {$ENDIF}

  WARFARE_ORDER_SEQUENTIAL    :Boolean = True; //Pick weapon orders like KaM did
  WARFARE_ORDER_PROPORTIONAL  :Boolean = False; //New proportional way (looks like a bad idea)

  //These are debug things, should be False
  {Runner}
  {$IFDEF PARALLEL_RUNNER}
    THREAD_NUMBER         :Word = 1; // Thread number for parallel runner and saves
  {$ENDIF}
  DEFAULT_PEACE_TIME      :Word = 0; //Default peacetime for SP games when SP_DEFAULT_ADVANCED_AI set to True
  {AI}
  SP_BOOST_AI_BUILD       :Boolean = False; //Boost build algorithm of the new AI (performance impact)
  SP_DEFAULT_ADVANCED_AI  :Boolean = False; //Set advanced AI as default for SP games
  {User interface options}
  DEBUG_SPEEDUP_SPEED     :Integer = 600;   //Speed for speedup from debug menu
  DEBUG_LOGS              :Boolean = True;  //Log debug info
  DEBUG_SCRIPTING_EXEC    :Boolean = False; //Use slow debug executor (about 3 times slower! never use on release version). Using it we can find exact position of execution time error (row/col/pos/module)
  USE_KMR_DIR_FOR_SETTINGS:Boolean = DEBUG_CFG; // Do we use KMR local directory for settings?
  SKIP_RNG_CHECKS_FOR_SOME_GIC: Boolean = True; //Skip rng checks for Autosave and few other commands to have same AI city with predefined seed + mapconfig
  ALLOW_SELECT_ALLIES     :Boolean = True;  //Do we allow to select ally units or groups
  ALLOW_SELECT_ALL        :Boolean = DEBUG_CFG; //Do we allow to select all entities (allies and enemies)
  SHOW_RES_CNT_K_FOR_10000:Boolean = not DEBUG_CFG; //Do we show resource amount with K postfix (10123 -> 10k)
  GAME_NO_UPDATE_ON_TIMER :Boolean = False; //Block game update by timer (only allow to update it manually)
  GAME_SAVE_STRIP_FOR_CRC :Boolean = False; //Strip unsynced data from Game saves, to compare saves CRC
  ALLOW_LOAD_UNSUP_VERSION_SAVE:
                           Boolean = DEBUG_CFG; //Allow to try load saves / replay with unsupported version
  SHOW_ENEMIES_STATS      :Boolean = False; //Do we allow to show enemies stats during the game
  SHOW_CONTROLS_OVERLAY   :Boolean = False; //Draw colored overlays ontop of controls! always Off here
  SHOW_CONTROLS_ID        :Boolean = False; //Draw controls ID
  SHOW_FOCUSED_CONTROL    :Boolean = False; //Outline focused control
  SHOW_CONTROL_OVER       :Boolean = False; //Outline control with mouse over
  SHOW_TEXT_OUTLINES      :Boolean = False; //Display text areas outlines
  SKIP_RENDER_TEXT        :Boolean = False; //Skip painting labels
  ENABLE_DESIGN_CONTORLS  :Boolean = False; //Enable special mode to allow to move/edit controls
  MODE_DESIGN_CONTROLS    :Boolean = False; //Special mode to move/edit controls activated by F7, it must block OnClick events! always Off here
  DBG_UI_HINT_POS         :Boolean = False; //Show coordinates near cursor, with axis

  OVERLAY_RESOLUTIONS     :Boolean = False; //Render constraining frame
  LOCAL_SERVER_LIST       :Boolean = False; //Instead of loading server list from master server, add localhost:56789 (good for testing)
  SHOW_LOG_IN_CHAT        :Boolean = False; //Show log messages in MP game chat
  LOG_GAME_TICK           :Boolean = False; //Log game tick
  SAVE_RANDOM_CHECKS      :Boolean = True; //Save random checks data to separate file
  MAPED_SHOW_CONDITION_UNIT_BTNS: Boolean = True; //Show condition Inc/Dec buttons for citizen units in MapEd
  {Gameplay display}
  SKIP_RENDER             :Boolean = False; //Skip all the rendering in favor of faster logic
  DO_NOT_SKIP_LOAD_TILESET:Boolean = False; //Do not skip load tileset even if SKIP_RENDER is set
  SKIP_SOUND              :Boolean = False; //Skip all the sounds in favor of faster logic
  SKIP_LOADING_CURSOR     :Boolean = False; //Skip loading and setting cursor
  SKIP_SETTINGS_SAVE      :Boolean = false; //Skip save main/game settings into the ini file
//  SKIP_POINTER_REF_CHECK  :Boolean = False; //Skip Pointer reference check (gGame.AllowGetPointer)
  SKIP_SAVE_SAVPTS_TO_FILE:Boolean = False; //Skip save game savepoints (into .spt file)
  AGGRESSIVE_REPLAYS      :Boolean = True; //Write a command gicTempDoNothing every tick in order to find exactly when a replay mismatch occurs
  SHOW_GAME_TICK          :Boolean = false; //Show game tick next to game time
  SHOW_FPS                :Boolean = False; //Show FPS
  SHOW_TERRAIN_IDS        :Boolean = False; //Show number of every tile terrain on it (also show layers terrain ids)
  SKIP_TER_RENDER_ANIMS   :Boolean = False; //Skip render terrain animations
  SKIP_TER_RENDER_LIGHT   :Boolean = False; //Skip render terrain lighting
  SKIP_TER_RENDER_SHADOW  :Boolean = False; //Skip render terrain shadows
  DO_DEBUG_TER_RENDER     :Boolean = False; //Do we do debug terrain layers? If yes, then only chosen terrain layers will be rendered
  DEBUG_TERRAIN_LAYERS    :set of Byte = [0,1,2,3]; //Terrain layers to render while debugging with DO_DEBUG_LAYERS
  SKIP_RENDER_TER_LAYERS  :Boolean = False; //Do not render terrain layers
  SHOW_TERRAIN_KINDS      :Boolean = False; //Show terrain kind ids on every tile corner
  SHOW_TERRAIN_OVERLAYS   :Boolean = False; //Show terrain tile overlays
  SHOW_TERRAIN_HEIGHT     :Boolean = False; //Show terrain tile overlays
  SHOW_TERRAIN_TILES_GRID :Boolean = False; //Show terrain tiles grid
  SHOW_BRUSH_APPLY_AREA   :Boolean = False; //Show brushes apply area
  SHOW_TERRAIN_WIRES      :Boolean = False; //Makes terrain height visible
  SHOW_TERRAIN_PASS       :Byte = 0; //Byte(TKMTerrainPassability)
  SHOW_UNIT_ROUTES        :Boolean = False; //Draw unit routes
  SHOW_UNIT_ROUTES_STEPS  :Boolean = False; //Draw unit routes step numbers
  SHOW_SEL_BUFFER         :Boolean = False; //Display selection buffer
  SHOW_PROJECTILES        :Boolean = False; //Shows projectiles trajectory
  SHOW_POINTER_DOTS       :Boolean = False; //Show pointer count as small dots below unit/houses
  SHOW_GROUND_LINES       :Boolean = False; //Show a line below all sprites to mark the ground height used in Z-Order
  SHOW_UNIT_MOVEMENT      :Boolean = False; //Draw unit movement overlay (occupied tile), Only if unit interaction enabled
  SHOW_VIEWPORT_POS       :Boolean = False; //Draw viewport position
  SHOW_JAM_METER          :Boolean = False; //Show jam meter value on terrain
  SHOW_TILE_OBJECT_ID     :Boolean = False; //Show tiles object ID
  SHOW_TILES_OWNER        :Boolean = False; //Show tiles owner
  SHOW_TREE_AGE           :Boolean = False; //Show tiles tree age
  SHOW_FIELD_AGE          :Boolean = False; //Show tiles field age
  SHOW_TILE_LOCK          :Boolean = False; //Show tiles lock
  SHOW_TILE_UNIT          :Boolean = False; //Show tiles units
  SHOW_VERTEX_UNIT        :Boolean = False; //Show vertex unit (if vertex is occupied)
  SHOW_UIDs               :Boolean = False; //Show units/groups/houses UIDs
  SHOW_WALK_CONNECT       :Boolean = False; //Show floodfill areas of interconnected areas
  SHOW_GROUP_MEMBERS_POS  :Boolean = False; //Show group members position
  TEST_VIEW_CLIP_INSET    :Boolean = False; //Renders smaller area to see if everything gets clipped well
  OUTLINE_ALL_SPRITES     :Boolean = False; //Render outline around every sprite
  SHOW_ATTACK_RADIUS      :Boolean = False; //Render towers/archers attack radius
  DISPLAY_SOUNDS          :Boolean = False; //Display sounds on map
  RENDER_3D               :Boolean = False; //Experimental 3D render
  LINEAR_FILTER_SPRITES   :Boolean = False; //To check pixel sampling alignment issues (bouncing) at 100% zoom
  HOUSE_BUILDING_STEP     :Single = 0;
  WATER_LIGHT_MULTIPLIER  :Single = DEFAULT_WATER_LIGHT_MULTIPLIER; //Terrain light multiplier
  OVERLAY_NAVMESH         :Boolean = False; //Show navmesh
  OVERLAY_HIGHLIGHT_POLY  :Integer = 0;     //Highlight plygon in navmesh
  OVERLAY_DEFENCES        :Boolean = False; //Show AI defence perimeters
  OVERLAY_DEFENCES_A      :Boolean = False; //Show AI defence perimeters (Animation)
  OVERLAY_INFLUENCE       :Boolean = False; //Show influence map
  OVERLAY_OWNERSHIP       :Boolean = False; //Show ownership map
  OVERLAY_AVOID           :Boolean = False; //Show avoidance map
  OVERLAY_AI_BUILD        :Boolean = False; //Show build progress of new AI
  OVERLAY_AI_COMBAT       :Boolean = False; //Show combat marks of new AI
  OVERLAY_AI_PATHFINDING  :Boolean = False; //Show combat marks of new AI
  OVERLAY_AI_EYE          :Boolean = False; //Show Eye vision of new AI
  OVERLAY_AI_SOIL         :Boolean = False; //Show Soil vision of new AI
  OVERLAY_AI_FLATAREA     :Boolean = False; //Show FlatArea vision of new AI
  OVERLAY_AI_ROUTES       :Boolean = False; //Show Routes to resources vision of new AI
  OVERLAY_AI_SUPERVISOR   :Boolean = False; //Show Supervisor vision of new AI
  OVERLAY_AI_VEC_FLD_ENEM :Boolean = False; //Show Vector field of enemy (combat AI)
  OVERLAY_AI_VEC_FLD_ALLY :Boolean = False; //Show Vector field of ally (combat AI)
  OVERLAY_AI_CLUSTERS     :Boolean = False; //Show combat clusters (combat AI)
  OVERLAY_AI_ALLIEDGROUPS :Boolean = False; //Show show allied groups (combat AI)
  {Render}
  SAVE_MAP_TO_FBO_RENDER  :Boolean = False; //Do Render is performed into FBO off-screen buffer?
  {Stats}
  SHOW_SPRITE_COUNT       :Boolean = False; //display rendered controls/sprites count
  SHOW_POINTER_COUNT      :Boolean = False; //Show debug total count of unit/house pointers being tracked
  SHOW_CMDQUEUE_COUNT     :Boolean = False; //Show how many commands were processed and stored by TGameInputProcess
  SHOW_NETWORK_DELAY      :Boolean = False; //Show the current delay in multiplayer game
  SHOW_ARMYEVALS          :Boolean = False; //Show result of enemy armies evaluation
  SHOW_AI_WARE_BALANCE    :Boolean = False; //Show wares balance (Produced - Consumed)
  SHOW_NET_PACKETS_STATS  :Boolean = False; //Show network packet statistics
  SHOW_NET_PACKETS_LIMIT  :Integer = 1;
  SHOW_SELECTED_OBJ_INFO  :Boolean = False; //Show selected object (Unit/Group + Unit/House) data (UID/order/action etc)
  SHOW_HANDS_INFO         :Boolean = False; //Show hands info
  SHOW_VIEWPORT_INFO      :Boolean = False; //Show viewport info
  SHOW_GIP                :Boolean = False; //Show GIP commands
  SHOW_GIP_AS_BYTES       :Boolean = False; //Show GIP commands as bytes (or as 'parsed type' if False)
  INI_HITPOINT_RESTORE    :Boolean = False; //Use the hitpoint restore rate from the INI file to compare with KaM
  SLOW_MAP_SCAN           :Boolean = False; //Scan maps with a pause to emulate uncached file access
  SLOW_CAMPAIGN_SCAN      :Boolean = False; //Scan campaigns with a pause to emulate uncached file access
  SLOW_SAVE_SCAN          :Boolean = False; //Scan saves with a pause to emulate uncached file access
  SLOW_GAME_SAVE_ASYNC    :Boolean = False; //Emulate slow game save (in the async save thread)
  SLOW_MAP_SAVE_LOAD      :Boolean = False; //Load map or save to emulate slow network
  SLOW_ASYNC_RES_LOADER   :Boolean = False; //Emulate slow async resource loader (slow res load in async thread)
  DO_PERF_LOGGING         :Boolean = False; //Write each ticks time to log (DEPRECATED PERF_LOGGER)
  MP_RESULTS_IN_SP        :Boolean = False; //Display each players stats in SP
  SHOW_DEBUG_OVERLAY_BEVEL:Boolean = True;  //Show debug text overlay Bevel (for better text readability)
  SHOW_LOG_IN_GUI         :Boolean = False; //Show log in GUI
  UPDATE_LOG_FOR_GUI      :Boolean = False; //Update log to be shown in GUI
  DEBUG_TEXT_FONT_ID      :Integer = 7;     //Debug font ID (7 is fntMonospaced)

  {Gameplay}
  LOBBY_SET_SPECS_DEFAULT :Boolean = DEBUG_CFG; //Set 'Allow spectators' flag in the lobby by default
  LOBBY_HOST_AS_SPECTATOR :Boolean = DEBUG_CFG; //Host lobby as spectator by default
  NO_SAVE_COMPRESSION     :Boolean = False; //Do not apply compression on save files, to make them more readable
  CUSTOM_SEED_VALUE       :Integer = 0;     //Custom seed value. Not applied if set to 0
  PAUSE_GAME_BEFORE_TICK  :Integer = -1;    //Pause after specified game tick
  MAKE_SAVEPT_BEFORE_TICK :Integer = -1;    //Make savepoint after a certain tick (for both game and replay)
  ALLOW_SAVE_IN_REPLAY    :Boolean = DEBUG_CFG; //Allow to save game from replay, good for debug
  SAVE_GAME_AS_TEXT       :Boolean = True; {Save game serialized} //todo: DEBUG. set to False before releases

  DEBUG_TEXT              :String = '';    //Debug text
  DEBUG_VALUE             :Integer = 0;    //Debug value
  {Gameplay cheats}
  UNLOCK_CAMPAIGN_MAPS    :Boolean = False; //Unlock more maps for debug
  REDUCE_SHOOTING_RANGE   :Boolean = False; //Reduce shooting range for debug
  MULTIPLAYER_CHEATS      :Boolean = DEBUG_CFG; //Allow cheats and debug overlays (e.g. CanWalk) in Multiplayer
  DEBUG_CHEATS            :Boolean = DEBUG_CFG; //Cheats for debug (place scout and reveal map) which can be turned On from menu
  MULTIPLAYER_SPEEDUP     :Boolean = DEBUG_CFG; //Allow you to use F8 to speed up multiplayer for debugging (only effects local client)
  SKIP_EXE_CRC            :Boolean = False; //Don't check KaM_Remake.exe CRC before MP game (useful for testing with different versions)
  ALLOW_MP_MODS           :Boolean = DEBUG_CFG; //Don't let people enter MP mode if they are using mods (unit.dat, house.dat, etc.)
  ALLOW_TAKE_AI_PLAYERS   :Boolean = False; //Allow to load SP maps without Human player (usefull for AI testing)
  {Data output}
  BLOCK_SAVE              :Boolean = False; //Block saving game (used in parallel Runner)
  BLOCK_FILE_WRITE        :Boolean = False; //Block to write into txt file (used in parallel Runner)
  WRITE_DECODED_MISSION   :Boolean = False; //Save decoded mission as txt file
  WRITE_WALKTO_LOG        :Boolean = False; //Write even more output into log + slows down game noticably
  WriteResourceInfoToTXT  :Boolean = False; //Whenever to write txt files with defines data properties on loading
  EXPORT_SPRITE_ATLASES   :Boolean = False; //Whenever to write all generated textures to PNG on loading (extremely time consuming)
  EXPORT_SPRITE_ATLASES_RXA:Boolean = False; //Whenever to write all loaded from .RXA files textures to PNG on loading (super extremely time consuming)
  EXPORT_INFLUENCE        :Boolean = False;
  LOG_FONTS_RAM_USAGE     :Boolean = False;
  CONNECT_ROAD_TO_WALLS     :Boolean = true;
const
  MAX_WARES_IN_HOUSE     = 5;    //Maximum resource items allowed to be in house
  MAX_WARES_OUT_WORKSHOP = 5;    //Maximum sum resource items allowed to output in workshops. Value: 5 - 20;
  MAX_WARES_ORDER        = 1000;  //Number of max allowed items to be ordered in production houses (Weapon/Armor/etc) 1000 = Infinity

  MAX_UNITS_AROUND_HOUSE = 50;
  USE_UNIT_TURN_AROUND = true;

const
  MAX_WOODCUTTER_CUT_PNT_DISTANCE = 5; //Max distance for woodcutter new cutting point from his house

const
  MAX_HANDS            = 27; // Maximum players (human or AI) per map
  MAX_LOBBY_PLAYERS    = 12; // Maximum number of players (not spectators) allowed in the lobby. Map can have additional AI locations up to MAX_HANDS (for co-op).
  MAX_LOBBY_SPECTATORS = 2;  // Slots available in lobby. Additional slots can be used by spectators
  MAX_LOBBY_SLOTS      = MAX_LOBBY_PLAYERS + MAX_LOBBY_SPECTATORS;
  MAX_TEAMS            = MAX_LOBBY_PLAYERS div 2;

  GAME_SPEED_NORMAL = 1;
  GAME_SP_SPEED_MAX = 10;
  GAME_MP_SPEED_MAX = 2.5;

  SPEED_PACE_DEFAULT = 100; // ms, frequency of game updates


  AUTOSAVE_COUNT_DEF      = 5;    //How many autosaves to backup by default - this MUST be variable (Parallel Runner)
  AUTOSAVE_COUNT_MIN      = 1;
  AUTOSAVE_COUNT_MAX      = 50;  // 99 max, since it should be 2-digit width to cope with our rename algo
  AUTOSAVE_FREQUENCY_MIN  = 300;  // 30 sec
  AUTOSAVE_FREQUENCY_MAX  = 6000; // 10 minutes
  AUTOSAVE_FREQUENCY_DEFAULT = 600; // 1 min, How often to do autosave, every N ticks
  BASESAVE_NAME = 'basesave';
  AUTOSAVE_SAVE_NAME = 'autosave';
  AUTOSAVE_AFTER_PT_END_SAVE_NAME = 'autosave_after_pt_end';
  CRASHREPORT_SAVE_NAME = 'crashreport';

  // Checkpoint, which are made in the memory while watching replay
  REPLAY_SAVEPOINT_FREQUENCY_MIN = 30*10; //30 sec
  REPLAY_SAVEPOINT_FREQUENCY_MAX = 60*60*10; // 1 hour
  REPLAY_SAVEPOINT_FREQUENCY_DEF = 5*60*10; // 5 min
  REPLAY_SAVEPOINT_CNT_MAX       = 60; // Max number of replay autosaves

  // Checkpoints, which are made during the game and saved in the .spt file
{$IFDEF DEBUG}
var
{$ENDIF}
  GAME_SAVE_CHECKPOINT_FREQ_MIN: Integer = 5*60*10;   // 5 min
  GAME_SAVE_CHECKPOINT_FREQ_MAX: Integer = 2*60*60*10; // 2 hours
  GAME_SAVE_CHECKPOINT_FREQ_DEF: Integer = 15*60*10; // 15 minutes
  GAME_SAVE_CHECKPOINT_CNT_LIMIT_MIN: Integer  = 0;  // Min limit for number of game checkpoints
  GAME_SAVE_CHECKPOINT_CNT_LIMIT_MAX: Integer  = 60; // Max limit for number of game checkpoints
  GAME_SAVE_CHECKPOINT_CNT_LIMIT_DEF: Integer  = 30; // Def limit for number of game checkpoints
{$IFDEF DEBUG}
const
{$ENDIF}

  BEACON_COOLDOWN         = 400;  //Minimum time in milliseconds between beacons

  DYNAMIC_HOTKEYS_NUM  = 20; // Number of dynamic hotkeys

  GLOBAL_TICK_UPDATE_FREQ = 100; // In ms.

var
  HITPOINT_RESTORE_PACE: Word = 100;         //1 hitpoint is restored to units every X ticks (using Humbelum's advice)

const
  TERRAIN_MAX_BLENDING_LEVEL = 15;
  TERRAIN_DEF_BLENDING_LVL = (TERRAIN_MAX_BLENDING_LEVEL + 1) div 2;
  MAPED_BRUSH_MAX_SIZE = 32;

  //Here we store options that are hidden somewhere in code
  //Unit condition
  CONDITION_PACE             = 10;         //Check unit conditions only once per 10 ticks
  PALISADE_PACE             = 20;         //Check unit on palisade only once per 20ticks
  UNIT_STUFFED_CONDITION_LVL = 0.9;        //Unit condition level, until which we allow unit to eat foods
  UNIT_MAX_CONDITION         = 60*60;      //Minutes of life. In KaM it's 45min
  UNIT_MIN_CONDITION         = 10*60;       //If unit condition is less it will look for Inn. In KaM it's 6min
  TROOPS_FEED_MAX            = 0.65;       //Maximum amount of condition a troop can have to order food (more than this means they won't order food)
  UNIT_CONDITION_BASE        = 0.7;        //Base amount of health a unit starts with (measured in KaM)
  UNIT_CONDITION_RANDOM      = 0.2;        //Random jitter of unit's starting health (KaM did not have this, all units started the same)
  TROOPS_TRAINED_CONDITION   = 0.7;        //Condition troops start with when trained (measured from KaM)
  SHIP_MAX_CAPACITY          = 30;
  //Units are fed acording to this: (from knightsandmerchants.de tips and tricks)
  //Bread    = +40%
  //Sausages = +60%
  //Wine     = +20% (We changed this to +30% for balance)
  //Fish     = +50%
  BREAD_RESTORE = 0.50;
  SAUSAGE_RESTORE = 0.35;
  WINE_RESTORE = 0.25;
  FISH_RESTORE = 0.7;
  APPLE_RESTORE = 0.15;
  VEGE_RESTORE = 0.20;

  FISHERMAN_FISH_MAX_DISTANCE = 25;

  DEFAULT_HITPOINT_RESTORE  = 100;        //1 hitpoint is restored to units every X ticks (using Humbelum's advice)
  TIME_BETWEEN_MESSAGES     = 5*600;      //Time between messages saying house is unoccupied or unit is hungry. In KaM it's 4 minutes

  TIME_BETWEEN_RE_REQUEST_FOOD     = 15*600;//Time between requesting food. 15 minutes

  RANGE_WATCHTOWER_MAX  = 13.99; //Measured in KaM. Distance from the doorway of tower
  RANGE_WATCHTOWER_MIN  = 7; //In KaM towers have no minimum range, they will shoot any unit less than the range

  RANGE_WALLTOWER_MAX  = 9.5; //Measured in KaM. Distance from the doorway of tower
  RANGE_WATLLTOWER_MIN  = 2; //In KaM towers have no minimum range, they will shoot any unit less than the range

  LINK_RADIUS = 5; //Radius to search for groups to link to after being trained at the barracks (measured from KaM)

  FRIENDLY_FIRE = True; //Whenever archers could kill fellow men with their arrows

  NET_DROP_PLAYER_MIN_WAIT = 15; // in seconds. Host must wait at least this long before dropping disconnected players
  ANNOUNCE_BUILD_MAP = 30*60*10; //30 minutes
  ANNOUNCE_BATTLE_MAP = 2*60*10; //2 minutes

  RESULTS_UPDATE_RATE = 15;          //Each 1.5 sec
  CHARTS_SAMPLING_FOR_ECONOMY = 150; //Each 15sec
  CHARTS_SAMPLING_FOR_TACTICS = 30;  //Each 3sec, cos average game length is much shorter

  RETURN_TO_LOBBY_SAVE = 'paused';
  DOWNLOADED_LOBBY_SAVE = 'downloaded';

  SERVER_DEFAULT_UDP_SCAN_PORT = 56788;
  SERVER_DEFAULT_UDP_ANNOUNCE_PORT = 56789;
  EMPTY_ROOM_DEFAULT_GAME_REVISION = 0; //Placeholder for game revision in room

  LOC_RANDOM = 0;
  LOC_SPECTATE = -1;
  LOC_ANY = -1000;

  MIN_PLAYER_COLOR_DIST = 0.15; // Minimun color distance between players

  EXT_SAVE_TXT_DOT = '.txt';

  EXT_SAVE_REPLAY = 'rpl';
  EXT_SAVE_GAME_SAVEPTS = 'spt';
  EXT_SAVE_MAIN = 'sav';
  EXT_SAVE_MAIN_TXT = EXT_SAVE_MAIN + EXT_SAVE_TXT_DOT;
  EXT_SAVE_BASE = 'bas';
  EXT_SAVE_MP_LOCAL = 'sloc';
  EXT_SAVE_RNG_LOG = 'rng';

  EXT_FILE_SCRIPT = 'script';

  EXT_SAVE_REPLAY_DOT = '.' + EXT_SAVE_REPLAY;
  EXT_SAVE_GAME_SAVEPTS_DOT = '.' + EXT_SAVE_GAME_SAVEPTS;
  EXT_SAVE_MAIN_DOT = '.' + EXT_SAVE_MAIN;
  EXT_SAVE_MAIN_TXT_DOT = '.' + EXT_SAVE_MAIN_TXT;
  EXT_SAVE_BASE_DOT = '.' + EXT_SAVE_BASE;
  EXT_SAVE_MP_LOCAL_DOT = '.' + EXT_SAVE_MP_LOCAL;
  EXT_SAVE_RNG_LOG_DOT = '.' + EXT_SAVE_RNG_LOG;

  EXT_FILE_SCRIPT_DOT = '.' + EXT_FILE_SCRIPT;

  MAX_HOUSE_SIZE = 5;
  WARES_IN_OUT_COUNT = 6;

type
  TKMHandID = {type} ShortInt;
  TKMHandIDArray = array of TKMHandID;
  TKMHandID2Array = array of TKMHandIDArray;
  TKMHandEnabledArray = array [0..MAX_HANDS-1] of Boolean;

const
  // Used to reset on new game start
  OWN_MARGIN_DEF   :Byte = 190;
  OWN_THRESHOLD_DEF:Byte = 126;

var
  //Values are empirical
  //todo: Can move this to AIInfluences as parameter
  OWN_MARGIN   :Byte = 190;
  OWN_THRESHOLD:Byte = 126;


const
  LAST_SENT_COMMANDS_TICK_NONE = 0;

{Cursors}
type
  TKMCursorMode = (
    cmNone,
    cmErase, //Remove player controlled assets (plans, houses) with a confirmation dialog
    cmRoad,
    cmField,
    cmGrassLand,
    cmVegeField,
    cmWine,
    cmHouses, // Gameplay

    //Map Editor
    cmElevate, //Height elevation
    cmEqualize, //Height equalization
    cmConstHeight, //Constant height brush
    cmElevateAll, //Terrain kind elevation
    cmBrush, //Terrain brush
    cmTiles, // Individual tiles
    cmObjects, //Terrain objects
    cmObjectsBrush, //Objects brush
    cmMagicWater, //Magic water
    cmSelection, //Selection manipulations
    cmUnits, //Units
    cmMarkers, //CenterScreen, FOW, Defence, AIStart, Rally/Cutting Point markers
    cmEyedropper, //Terrain eyedropper
    cmPaintBucket, //PaintBucket - change color(team) for map objects
    cmUniversalEraser, //Universal eraser for units/groups/houses/terrain objects/roads and fields (corn/wine)
    cmRotateTile,  //Rotate terrain tile
    cmOverlays,
    cmPalisade,
    cmTileSelection,
    cmBridges,
    cmAssignToShip,
    cmDecorations,
    cmCustom,
    cmWaresOnGround,
    cmChangeResCount,
    cmPearlRepair);  //Tile overlays

type
  // How cursor field placing will act (depends on which tile LMB was pressed)
  TKMCursorFieldMode = (
    cfmNone, // Disabled
    cfmPlan, // Placing plans
    cfmErase // Erasing plans
  );
  TKMCursorRenderType = (crtNone, crtWireTile, crtTile, crtObject, crtUnit, crtHouseSite, crtHouse, crtX, crtDelete);

  // Shape types for MapEditor
  TKMMapEdShape = (hsCircle, hsSquare);


const
  MARKER_REVEAL = 1;
  MARKER_DEFENCE = 2;
  MARKER_CENTERSCREEN = 3;
  MARKER_AISTART = 4;
  MARKER_RALLY_POINT = 5;
  MARKER_DEFEND = 6;
  MARKER_ANIMALS = 7;

const
  DATE_TIME_ZERO: TDateTime = 0; // DateTime as 0, for simplicity. Some compilers (Delphi Berlin, f.e.) can't handle TDateTime(0)


const
  DIR_CURSOR_CIRCLE_RAD  = 32; //Radius of the direction selector cursor restriction area
  DIR_CURSOR_NA_RAD = 20;  //Radius of centeral part that has no direction


type
  TKMGameResultMsg = (//Game result
        grWin,           //Player has won the game
        grDefeat,        //Player was defeated
        grCancel,        //Game was cancelled (unfinished)
        grError,         //Some known error occured
        grDisconnect,    //Disconnected from multiplayer game
        grSilent,        //Used when loading savegame from running game (show no screens)
        grReplayEnd,     //Replay was cancelled - return to menu without screens
        grMapEdEnd,      //Map Editor was closed - return to menu without screens
        grGameContinues);//Game is not finished yet, it is continious


type
  TKMAllianceType = (atEnemy, atAlly);

const
  FOG_OF_WAR_MIN  = 80;           //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  FOG_OF_WAR_ACT  = 160;          //Until this value FOW is not rendered at all
  FOG_OF_WAR_MAX  = 255;          //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  FOG_OF_WAR_INC  = 128;          //Increment for FOW
  FOG_OF_WAR_DEC  = 4;           //Decrement for FOW

const
  MAPED_HISTORY_DEPTH_MIN = 20;
  MAPED_HISTORY_DEPTH_MAX = 1000;
  MAPED_HISTORY_DEPTH_DEF = 250;


const
  MAPS_FOLDER_NAME = 'Maps';
  MAPS_MP_FOLDER_NAME = 'MapsMP';
  MAPS_DL_FOLDER_NAME = 'MapsDL';
  MAPS_RMG_NAME = 'Randomly generated map';
  TUTORIALS_FOLDER_NAME = 'Tutorials';
  BATTLE_TUTORIALS_FOLDER_NAME = 'Battle Tutorials';
  CAMPAIGNS_FOLDER_NAME = 'Campaigns';
  CAMPAIGNSMP_FOLDER_NAME = 'CampaignsMP';
  SAVES_FOLDER_NAME = 'Saves';
  SAVES_MP_FOLDER_NAME = 'SavesMP';


{ Terrain }
type
  //* terrain passability
  TKMTerrainPassability = (
    tpNone,
    tpWalk,        // General passability of tile for any walking units
    tpWalkRoad,    // Type of passability for Serfs when transporting wares, only roads have it
    tpBuildNoObj,  // Can we build a house on this tile after removing an object on the tile or house near it?
    tpBuild,       // Can we build a house on this tile?
    tpMakeRoads,   // Thats less strict than house building, roads Can be placed almost everywhere where units Can walk, except e.g. bridges
    tpCutTree,     // Can tree be cut
    tpFish,        // Water tiles where fish Can move around
    tpCrab,        // Sand tiles where crabs Can move around
    tpWolf,        // Soil tiles where wolfs Can move around
    tpElevate,     // Nodes which are forbidden to be elevated by workers (house basements, water, etc..)
    tpWorker,      // Like CanWalk but allows walking on building sites
    tpOwn,         // For AI ownership
    tpFactor,       // Allows vertex (top left) to be factored as a neighbour in flattening algorithm (it is to stop mines flattening strangely due to surrounding hills)
    tpWall,
    tpWallGate,
    tpPolarBear,
    tpFox
  );
  TKMTerrainPassabilitySet = set of TKMTerrainPassability;

  TKMHeightPass = (hpWalking, hpBuilding, hpBuildingMines, hpFish);

const
  PASSABILITY_GUI_TEXT: array [TKMTerrainPassability] of UnicodeString = (
    'Unused',
    'Can walk',
    'Can walk road',
    'Can build without|object or house',
    'Can build',
    'Can make roads',
    'Can cut tree',
    'Can fish',
    'Can crab',
    'Can wolf',
    'Can elevate',
    'Can worker',
    'Can own',
    'Can factor',
    'Can Wall',
    'Is WallGate',
    'Can polar bear',
    'Can Deer'
  );


type
  TKMWalkConnect = (
    wcWalk,
    wcRoad,
    wcFish, //Required for fisherman finding fish in a pond, NOT for fish movement (uses steering). Updated ONLY on load because water doesn't change.
    wcWork  //CanWorker areas
  );

  TKMWalkConnectSet = set of TKMWalkConnect;


const
  UID_NONE: Integer = -1; // Would be better to have it 0. But now it's -1 for backwards compatibility


{Units}
type
  //* Unit type
  TKMUnitType = (utNone, utAny,
    utSerf,         utWoodcutter,   utMiner,         utAnimalBreeder,
    utFarmer,       utCarpenter,    utBaker,         utButcher,
    utFisher,       utBuilder,      utStonemason,    utSmith,
    utMetallurgist, utRecruit,      utOperator,      utClayPicker,
    utFeeder,       utHouseBuilder,

    utMilitia,      utAxeFighter,   utSwordFighter,  utBowman,
    utCrossbowman,  utLanceCarrier, utPikeman,       utScout,
    utKnight,       utBarbarian,

    utRebel,        utRogue,        utWarrior,       utVagabond,
    utCatapult,     utBallista,     utRam,           utGolem,
    utGiant,        utPaladin,      utArcher,        utSpy,
    utTrainedWolf,  utAmmoCart,     utPikeMachine,   utShip,
    utClubMan,      utMaceFighter,  utFlailFighter,  utShieldBearer,
    utFighter,      utSpikedTrap,   utWoodenWall,    utTorchMan,
    utMedic,        utBattleShip,   utBoat,          utPyro,
    utLekter,       utMobileTower,

    utWolf,         utFish,         utWatersnake,    utSeastar,
    utCrab,         utWaterflower,  utWaterleaf,     utDuck,
    utDeerMale,     utDeerFemale,   utFox,           utBoar,
    utBear,         utLandDuck,     utRabbit,        utWhiteBear,
    utSandSnake,    utSpider);
  //* Unit type set
  TKMUnitTypeSet = set of TKMUnitType;
  TKMUnitTypeArray = array of TKMUnitType;

  TKMUnitTypeArrayHelper = record helper for TKMUnitTypeArray
    class operator Add(const A, B : TKMUnitTypeArray) : TKMUnitTypeArray;
  end;

const
  UNIT_MIN = utSerf;
  UNIT_MAX = utSpider;
  CITIZEN_MIN = utSerf;
  CITIZEN_MAX = utHouseBuilder;
  WARRIOR_MIN = utMilitia;
  WARRIOR_MAX = utMobileTower;
  WARRIOR_EQUIPABLE_BARRACKS_MIN = utMilitia; //Available from barracks
  WARRIOR_EQUIPABLE_BARRACKS_MAX = utKnight;
  WARRIOR_EQUIPABLE_TH_MIN = utBarbarian; //Available from Townhall
  WARRIOR_EQUIPABLE_TH_MAX =  utWarrior;
  HUMANS_MIN = utSerf;
  HUMANS_MAX =  utMobileTower;
  ANIMAL_MIN = utWolf;
  ANIMAL_MAX = utSpider;
  WARRIOR_BITIN_EQUIPABLE = [ utSwordFighter, utCrossbowman, utPikeman, utKnight, utCatapult, utShieldBearer,
                              utBallista, utWarrior, utRam, utFlailFighter, utSpikedTrap, utWoodenWall];
  UNITS_VALID = [UNIT_MIN..UNIT_MAX];
  UNITS_ANIMALS = [ANIMAL_MIN..ANIMAL_MAX];
  UNITS_CITIZEN = [CITIZEN_MIN..CITIZEN_MAX];
  UNITS_WARRIORS = [WARRIOR_MIN..WARRIOR_MAX];
  UNITS_HUMAN = [HUMANS_MIN..HUMANS_MAX];
  {UNITS_NEW = [utOperator, utFeeder, utClayPicker, utHouseBuilder, utRam .. HUMANS_MAX];}
  WARRIORS_IRON = [utSwordFighter, utCrossbowman, utPikeman, utKnight, utWarrior, utFlailFighter, utShieldBearer];
  SPECIAL_UNITS = [utPaladin, utTrainedWolf, utArcher, utSpy, utAmmoCart, utShip, utBoat, utBattleShip, utPyro, utLekter];

  CITIZENS_CNT = Integer(CITIZEN_MAX) - Integer(CITIZEN_MIN) + 1;
  WARRIORS_CNT = Integer(WARRIOR_MAX) - Integer(WARRIOR_MIN) + 1;
  SIEGE_MACHINES = [utBallista, utCatapult, utRam, utAmmoCart, utWoodenWall, utSpikedTrap, utPikeMachine];
  UNITS_SHIPS = [utBoat, utShip, utBattleShip];

  OPERATORS_PER_MACHINE = 4;

type
  TKMUnitAmmoType = (uatNone, uatArrow, uatRogueStone, uatStoneBolt, uatBolt, uatAxe);

const
  AMMO_GUI_ICON : array[TKMUnitAmmoType] of Word = (0, 699, 695, 695, 696, 371);
  AMMO_GUI_TEXT : array[TKMUnitAmmoType] of Word = (0, 1874, 1874, 1875, 1876, 2098);
  AMMOCART_AMMO : set of TKMUnitAmmoType = [uatStoneBolt, uatBolt];
  AmmoCart_AmmoOrder : array[0..1] of TKMUnitAmmoType = (uatStoneBolt, uatBolt);

type
  TKMCheckAxis = (axX, axY);

// Used for AI defence and linking troops
type
  //* Group type
  TKMGroupType = (gtNone, gtAny, gtMelee, gtAntiHorse, gtRanged, gtMounted, gtMachines, gtMachinesMelee, gtWreckers, gtShips);
  //* Group type set
  TKMGroupTypeSet = set of TKMGroupType;
const
  GROUP_TYPE_MIN = gtMelee;
  GROUP_TYPE_MAX = gtShips;
  GROUP_TYPE_MIN_OFF = Ord(GROUP_TYPE_MIN);
  GROUP_TYPES: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Byte = (0, 1, 2, 3, 4, 5, 6, 7);
  GROUP_TYPES_VALID = [gtAny, GROUP_TYPE_MIN..GROUP_TYPE_MAX];
  GROUP_TYPES_CNT = Integer(GROUP_TYPE_MAX) - Integer(GROUP_TYPE_MIN) + 1;
  GROUP_TYPE_ORDER : array[0..GROUP_TYPES_CNT - 1] of TKMGroupType = (gtMelee, gtAntiHorse, gtRanged, gtMounted, gtMachines, gtMachinesMelee, gtWreckers, gtShips);
  GROUP_TYPE_GUI_ICON : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Word = (371, 374, 376, 377, 705, 734, 891, 966);
  GROUP_TYPE_GUI_TEXT : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Word = (473, 472, 471, 470, 1643, 1883, 1963, 2022);
type
  TKMGroupTypeArray = array [gtAny..GROUP_TYPE_MAX] of Word;
  TKMGroupTypeValidArray = array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Word;
  //* AI army type
  TKMArmyType = (atIronThenLeather, atLeather, atIron, atIronAndLeather);
  TAIArmyDemand = array [TKMGroupType] of Integer;
  TAIArmyDemandF = array [TKMGroupType] of Single;

const
  UNIT_TO_GROUP_TYPE: array [WARRIOR_MIN..WARRIOR_MAX] of TKMGroupType = (
    gtMelee,gtMelee,gtMelee, //utMilitia, utAxeFighter, utSwordsman
    gtRanged,gtRanged,        //utBowman, utArbaletman
    gtAntiHorse,gtAntiHorse,  //utPikeman, utHallebardman,
    gtMounted,gtMounted,      //utHorseScout, utCavalry,
    gtMelee,                   //utBarbarian
    //TPR Army
    gtAntiHorse,        //utPeasant
    gtRanged,           //utSlingshot
    gtMelee,            //utMetalBarbarian
    gtMounted,           //utHorseman
    gtMachines,gtMachines,gtMachinesMelee, //utCatapult, utBallista, utRam
    gtRanged, gtMelee,
    gtMelee,
    gtRanged,
    gtMelee,
    gtMounted,//utTrainedWolf
    gtMachines,//AmmoCart
    gtAntiHorse,//PikeMachine
    gtShips,//utShip
    gtWreckers,//clubman
    gtWreckers,//utMaceFighter
    gtWreckers,//utFlailfighter
    gtMachinesMelee,//itShieldBearer
    gtAny,//utFighter
    gtMachinesMelee,
    gtMachinesMelee,
    gtWreckers,
    gtAny,
    gtShips,
    gtShips,
    gtWreckers,
    gtMelee,
    gtMachines
    );

type
  TKMGoInDirection = (gdGoOutside=-1, gdGoInside=1); //Switch to set if unit goes into house or out of it

type
  TKMUnitThought = (thNone, thEat, thHome, thBuild, thStone, thWood, thDeath, thQuest,
                    thDismiss, thArmor, thSpy, thTile, thExclusive, thImportant,
                    thBucket, thBoots, thDiamond, thBow);

const //Corresponding indices in units.rx
  {THOUGHT_BOUNDS: array [TKMUnitThought, 1..2] of Word = (
  (0,0), (6250,6257), (6258,6265), (6266,6273), (6274,6281), (6282,6289), (6290,6297), (6298,6305), (6314,6321), (9603,9609), (10558,10564)
  );}
  THOUGHT_ICON: array[TKMUnitThought] of Word = (0, 15222, 15223, 15224, 15225, 15226, 15227,
                                                  15228, 15229, 15230, 15231, 15232, 15233, 15234,
                                                  15235, 15236, 15237, 15238);

  UNIT_OFF_X = -0.5;
  UNIT_OFF_Y = -0.4;

  //Offsetting layers of units we control what goes above or below
  //using smaller values to minimize impact on other objects and keeping withing map bounds
  FLAG_X_OFFSET = 0.01; //Flag is offset to be rendered above/below the flag carrier
  THOUGHT_X_OFFSET = 0.04; //Thought is offset to be rendered always above the flag

  //TileCursors
  TC_OUTLINE = 0;
  TC_BLOCK = 479;
  TC_BLOCK_MINE = 480;
  TC_ENTRANCE = 481;
  TC_BLOCK_ENTRANCE = 482;

type
  TKMUnitTaskType = ( uttUnknown, //Uninitialized task to detect bugs
        uttSelfTrain, uttDeliver,         uttBuildRoad,  uttBuildWine,        uttBuildField,
        uttBuildHouseArea, uttBuildHouse, uttBuildHouseRepair, uttGoHome,    uttDismiss,
        uttGoEat,     uttMining,          uttDie,        uttGoOutShowHungry,  uttAttackHouse,
        uttThrowRock, uttBuildPalisade, uttBuildRemove, uttBuildHouseUpgrade, uttGoGetBoots, uttBuildStructure,
        uttGoToShip, uttBuildGrassLand, uttUnloadFromShip, uttGoToStore, uttGoToLoc, uttCollectWares, uttUnloadWares,
        uttGoToWell, uttTakeOverHouse, uttMerchant, uttCartographer, uttBuildPearl, uttPearlRally, uttGoToPearl,
        uttForestCutter, uttFeedGroup, uttShootAtSpot);

  TKMUnitActionName = (uanStay, uanWalkTo, uanGoInOut, uanAbandonWalk, uanFight, uanStormAttack, uanSteer);

  TKMUnitActionType = (uaWalk=120, uaWork, uaSpec, uaDie, uaWork1,
                       uaWork2, uaWorkEnd, uaEat, uaWalkArm, uaWalkTool,
                       uaWalkBooty, uaWalkTool2, uaWalkBooty2, uaStay, uaUnknown);
  TKMUnitActionTypeSet = set of TKMUnitActionType;

const
  UNIT_ACT_MIN = uaWalk;
  UNIT_ACT_MAX = uaStay;
  UNIT_ACT_STR: array [TKMUnitActionType] of string = ('uaWalk', 'uaWork', 'uaSpec', 'uaDie', 'uaWork1',
             'uaWork2', 'uaWorkEnd', 'uaEat', 'uaWalkArm', 'uaWalkTool',
             'uaWalkBooty', 'uaWalkTool2', 'uaWalkBooty2', 'uaStay', 'uaUnknown');


const
  // Fish counts
  FISH_CNT_DEFAULT = 10;
  FISH_CNT_MAX = 255;


type
  TKMUnitMainData = record
    UnitType : TKMUnitType;
    Condition : Integer;
    BoltCount : Word;
    Columns, Count : Byte;
    function Valid : Boolean;
    procedure TakeUnit;
  end;
  TKMUnitPlan = array of record
    UnitType : TKMUnitType;
    Count : Word;
  end;

  TKMGatheringScript = (
    gsNone,
    gsWoodCutterCut, gsWoodCutterPlant,
    gsFarmerSow, gsFarmerCorn, gsFarmerWine,
    gsFisherCatch,
    gsStoneCutter,
    gsCoalMiner, gsGoldMiner, gsIronMiner,
    gsHorseBreeder, gsSwineBreeder, gsBitinMiner, gsClayMiner,
    gsWoodBurner, gsCarpenter, gsHovel, gsMerchant, gsSiegeCarpenter, gsIronSmithy, gsCollector,
    gsAppleTree, gsMetallurgists, gsShipyard, gsHunter);

{Houses in game}
type
  //House has 3 basic states: no owner inside, owner inside, owner working inside
  TKMHouseState = (hstEmpty, hstIdle, hstWork);
  //These are house building states
  TKMHouseBuildState = (hbsNoGlyph, hbsWood, hbsStone, hbsDone);

  TKMHouseActionType = (
    haWork1, haWork2, haWork3, haWork4, haWork5, //Start, InProgress, .., .., Finish
    haSmoke, haFlagpole, haIdle,
    haFlag1, haFlag2, haFlag3,
    haFire1, haFire2, haFire3, haFire4, haFire5, haFire6, haFire7, haFire8);
  TKMHouseActionSet = set of TKMHouseActionType;

const
  HOUSE_ACTION_STR
  : array [TKMHouseActionType] of string = (
    'ha_Work1', 'ha_Work2', 'ha_Work3', 'ha_Work4', 'ha_Work5', //Start, InProgress, .., .., Finish
    'ha_Smoke', 'ha_FlagShtok', 'ha_Idle',
    'ha_Flag1', 'ha_Flag2', 'ha_Flag3',
    'ha_Fire1', 'ha_Fire2', 'ha_Fire3', 'ha_Fire4', 'ha_Fire5', 'ha_Fire6', 'ha_Fire7', 'ha_Fire8'
  );


{Terrain}
type
  //* Field type
  TKMFieldType = (
    ftNone,
    ftRoad,
    ftCorn,
    ftWine,
    ftInitWine, //Reset rotation and set grapes ground, but without Grapes yet
    ftPalisade,
    ftRemove,
    ftGrassland,
    ftVegeField
  );
  TKMLockFieldType = (lftRoadStone, lftRoadWooden, lftRoadClay, lftRoadExclusive, lftPalisade,
                      lftField, lftGrassField, lftVegetablesField, lftWineField, lftRemove);
  TKMRoadType = (rtNone, rtStone, rtWooden, rtClay, rtExclusive);

Type
  TKMGrainType = (gftNone, gftWheat, gftRye, gftOat, gftRice, gftCorn, gftSunflower, gftStrawBush, gftCapvioli,
                    gftGrass, gftClover, gftLucerne, gftReed,
                    gftPumpkin, gftCarrots, gftCabbages, gftTomatos, gftPotatos, gftPalm,
                    gftWinePurple, gftWineWhite, gftWineBlack, gftWineRed, gftRaspberry, gftBlackberry,
                    gftWildRose, gftWildStrawberry, gftPomegranate, gftCactus, gftIceSticks, gftRandom);
  TKMGrainTypeSet = set of TKMGrainType;
  TKMGrainTypeArray = array of TKMGrainType;
  TKMGrainFarmSet = array[0..2] of TKMGrainType;
const
  GRAIN_FARM_SET_NONE : TKMGrainFarmSet = (gftNone, gftNone, gftNone);
  ROAD_GUI_PIC : array[TKMRoadType] of Word = (0, 335, 822, 823, 824);
  GRAIN_GUI_PIC : array[TKMGrainType] of Word = (967,//None
                                                825, 826, 827, 828, 829, 830, 931, 1034,//grain
                                                832, 833, 834, 1036,//grass
                                                831, 886, 887, 888, 889, 1035,//vege
                                                981, 982, 983, 984, 985, 986, 987, 988, 989,//wine
                                                926, 927,//wine
                                                968);//random

  GRAIN_GUI_HINT : array[TKMGrainType] of Word = (2139,//none
                                                2000, 2001, 2002, 2003, 2004, 2005, 2051, 2262,//grain
                                                2007, 2008, 2009, 2263,//grass
                                                2006, 2012, 2013, 2014, 2015, 2264,//vege
                                                2265, 2266, 2267, 2268, 2269, 2270, 2271, 2272, 2273, 2274, 2275,//wine
                                                2140);//random
  GRAIN_GRAIN_MIN = gftWheat;
  GRAIN_GRAIN_MAX = gftCapvioli;
  GRAIN_GRASS_MIN = gftGrass;
  GRAIN_GRASS_MAX = gftReed;
  GRAIN_VEGE_MIN = gftPumpkin;
  GRAIN_VEGE_MAX = gftPalm;
  GRAIN_WINE_MIN = gftWinePurple;
  GRAIN_WINE_MAX = gftIceSticks;
  GRAIN_MIN = gftWheat;
  GRAIN_MAX = gftIceSticks;
  GRAIN_VALID = [GRAIN_MIN..GRAIN_MAX];

  GRAIN_GUI_ORDER : array[0..9] of TKMGrainType = (gftWheat, gftRye, gftOat, gftStrawBush, gftRice, gftCorn, gftSunflower, gftCapvioli, gftRandom, gftNone);
  GRASS_GUI_ORDER : array[0..5] of TKMGrainType = (gftGrass, gftClover, gftLucerne, gftReed, gftRandom, gftNone);
  VEGE_GUI_ORDER : array[0..7] of TKMGrainType = (gftPumpkin, gftCarrots, gftCabbages, gftTomatos, gftPotatos, gftPalm, gftRandom, gftNone);

  GRAIN_GRAIN : set of TKMGrainType = [GRAIN_GRAIN_MIN..GRAIN_GRAIN_MAX];
  GRAIN_WINE : set of TKMGrainType = [GRAIN_WINE_MIN..GRAIN_WINE_MAX];
  GRAIN_GRASS : set of TKMGrainType = [GRAIN_GRASS_MIN..GRAIN_GRASS_MAX];
  GRAIN_VEGE : set of TKMGrainType = [GRAIN_VEGE_MIN..GRAIN_VEGE_MAX];

  LOCK_FIELD_GUI : array[TKMLockFieldType] of Word = (335, 822, 823, 824, 373, //rtStone, rtWooden, rtClay, rtExclusive, ftPalisade
                                                      337, 875, 876, 336, 340);//ftCorn, ftGrassLand, ftVegetables, ftWinefield, ftRemove

type

  TKMHouseStage = (
    hsNone,        //Nothing, clear area
    hsFence,       //Wooden fence, partially walkable as workers digg it up
    hsBuilt        //Done
  );

  //There are 4 steps in tile blocking scheme:
  // 0. Tile is normally walkable
  // 1. Set the tile as CantBuild
  // 2. Sets the tile as CantWalk to anyone except workers who are performing
  //    the digging task, so they could escape the area.
  //    The Worker will push out any unit on his way.
  //    sidenote: CanElevate is per-vertex property, hence it's not identical to CanWorker
  // 3. Set the tile as fully blocked
  TKMTileLock = (   // CanBuild CanWalk CanWorker CanElevate House Digged Fenced
        tlNone,     // X        X         X       X          -     -      -
        tlFenced,   // -        X         X       X          X     -      X
        tlDigged,   // -        -         X       X          X     X      X
        tlHouse,    // -        -         -       -          X     X      -
        tlWall, tlWallFence, tlWallGate, tlWallEmpty,
        tlStructure,
        //Used by workers making roads/fields to prevent you from building over them
        tlFieldWork,// -        X         X       X          -     X      -
        tlRoadWork  // -        X         X       X          -     X      -
        );

//Indexes KM_FormMain.StatusBar
const
  SB_ID_KMR_VER      = 0;
  SB_ID_MAP_SIZE     = 1;
  SB_ID_CURSOR_COORD = 2;
  SB_ID_TILE         = 3;
  SB_ID_TIME         = 4;
  SB_ID_FPS          = 5;
  SB_ID_OBJECT       = 6;
  SB_ID_CTRL_ID      = 7;

type
  TKMMapSize = (msNone, msXS, msS, msM, msL, msXL, msXXL, msG);

const
  MAP_SIZE_ENUM_MIN = msXS;
  MAP_SIZE_ENUM_MAX = msXXL;

type
  //Enum representing map visible layers
  TKMGameVisibleLayer = (
    mlObjects,
    mlHouses,
    mlUnits,
    mlOverlays,
    mlMiningRadius,
    mlTowersAttackRadius,
    mlUnitsAttackRadius,
    mlDefencesAll,
    mlFlatTerrain,
    mlSpawners
  );

  TKMMapVisibleLayerSet = set of TKMGameVisibleLayer; //Set of above enum

  //Enum representing mapEd visible layers
  TKMMapEdVisibleLayers = (
    melDeposits,
    melRevealFOW,
    melCenterScreen,
    melAIStart,
    melSelection,
    melWaterFlow,
    melMapResize,
    melDefences,
    melSpawners
  );

  TKMMapEdVisibleLayerSet = set of TKMMapEdVisibleLayers; //Set of above enum

  TKMDebugControls = (dcNone, dcFlatTerrain);

const
  DEV_SETTINGS_XML_FILENAME = 'kmr_dev.xml';


const
  //Colors available for selection in multiplayer
  MP_COLOR_COUNT = 26;
  MP_PLAYER_COLORS: array [1..MP_COLOR_COUNT] of Cardinal = (
    $FF0000EB, // 1 Red
    $FF076CF8, // 2 Orange
    $FF00B5FF, // 3 Gold
    $FF07FFFF, // 4 Lauenburg yellow
    $FF0EC5A2, // 5 Lime green
    $FF88FF88, // 6 Light green
    $FF07FF07, // 7 Neon green
    $FF00A100, // 8 Bright green
    $FF134B00, // 9 Dark green
    $FF7A9E00, // 10 Teal
    $FFFACE64, // 11 Sky blue
    $FFFB886D, // 12 Light violet blue
    $FFCC6600, // 13 Medium blue
    $FF581F00, // 14 Dark blue
    $FFCB3972, // 15 Violet (Amethyst)
    $FF720468, // 16 Purple
    $FFDE8FFB, // 17 Pink
    $FFFF07FF, // 18 Magenta
    $FF4A00A8, // 19 Dark pink
    $FF00005E, // 20 Maroon
    $FF666690, // 21 Light brown
    $FF103C52, // 22 Brown
    $FF519EC9, // 23 Tan
    $FFFFFFFF, // 24 White
    $FF838383, // 25 Grey
    $FF1B1B1B  // 26 Black
  );

  //Players colors, as they appear in KaM when the color is not specified in the script, copied from palette values.
  //Using these as the defaults when loading the script means the colors will be the same as KaM when not defined.
  //Default IDs from KaM:
  {229, //Red
  36,  //Cyan
  106, //Green
  20,  //Magenta
  233, //Yellow
  213, //Grey
  3,   //Black
  3,   //Black
  255  //White}
  DEFAULT_PLAYERS_COLORS: array [0..MAX_HANDS-1] of Cardinal = (
    $FF0000EB, // 1 Red
    $FF076CF8, // 2 Orange
    $FF00B5FF, // 3 Gold
    $FF07FFFF, // 4 Lauenburg yellow
    $FF0EC5A2, // 5 Lime green
    $FF88FF88, // 6 Light green
    $FF07FF07, // 7 Neon green
    $FF00A100, // 8 Bright green
    $FF134B00, // 9 Dark green
    $FF7A9E00, // 10 Teal
    $FFFACE64, // 11 Sky blue
    $FFFB886D, // 12 Light violet blue
    $FFCC6600, // 13 Medium blue
    $FF581F00, // 14 Dark blue
    $FFCB3972, // 15 Violet (Amethyst)
    $FF720468, // 16 Purple
    $FFDE8FFB, // 17 Pink
    $FFFF07FF, // 18 Magenta
    $FF9DB4FB, // 27 Light Light Red
    $FF4A00A8, // 19 Dark pink
    $FF00005E, // 20 Maroon
    $FF666690, // 21 Light brown
    $FF103C52, // 22 Brown
    $FF519EC9, // 23 Tan
    $FFFFFFFF, // 24 White
    $FF838383, // 25 Grey
    $FF1B1B1B  // 26 Black
  );

  // Colors which are used for an MP teams
  MP_TEAM_COLORS: array[0..MAX_TEAMS - 1] of Cardinal = (
    $FF0000EB, // 1 Red
    $FF076CF8, // 2 Orange
    $FF07FFFF, // 3 Lauenburg yellow
    $FF07FF07, // 4 Neon green
    $FFFACE64, // 5 Sky blue
    $FFDE8FFB  // 6 Pink
  );

  // DEBUG colors (transparent color - opacity will be added by debug tools)
  tcBlack      = $000000;
  tcBlue       = $FF0000;
  tcCream      = $F0FBFF;
  tcCyan       = $FFFF00;
  tcDarkGrey   = $808080;
  tcFuchsia    = $FF00FF;
  tcGreen      = $008000;
  tcGrey       = $808080;
  tcLightGrey  = $C0C0C0;
  tcLimeGreen  = $00FF00;
  tcMaroon     = $000080;
  tcMediumGrey = $A4A0A0;
  tcMintGreen  = $C0DCC0;
  tcNavyBlue   = $800000;
  tcOliveGreen = $008080;
  tcPurple     = $800080;
  tcRed        = $0000FF;
  tcSilver     = $C0C0C0;
  tcSkyBlue    = $F0CAA6;
  tcTeal       = $808000;
  tcWhite      = $FFFFFF;
  tcYellow     = $00FFFF;

  //Interface colors
  icGreen  = $FF00C000;
  icYellow = $FF07FFFF;
  icOrange = $FF0099FF;
  icRed    = $FF0707FF;
  icBlue   = $FFFF0707;
  icCyan   = $FFFFFF00;

  icTransparent = $00;
  icDarkGray = $FF606060;
  icDarkGrayTrans = $60606060;
  icDarkestGrayTrans = $20303030;
  icGray = $FF808080;
  icGray2 = $FF888888;
  icLightGray = $FFA0A0A0;
  icLightGray2 = $FFD0D0D0;
  icLightGrayTrans = $80A0A0A0;
  icWhite = $FFFFFFFF;
  icBlack = $FF000000;
  icLightCyan   = $FFFFFF80;
  icLightOrange = $FF80CCFF;
  icLightRed   = $FF7070FF;
  icLight2Red   = $FFB0B0FF;
  icLightLightRed = $FF9DB4FB;
  icDarkOrange = $FF0060FF;
  icDarkCyan   = $FFB0B000;
  icLightGreen = $FF00F000;
  icDeepGreen = $FF008000;
  icGreenYellow = $FF00FFBB;
  icTeal = $FF808000;

  icPink = $FFFF00FF;
  icDarkPink = $FFAA00AA;

  icSteelBlue = $FFA56D53;
  icDarkBlue   = $FF770707;

  icRoyalYellow = $FF4AC7FF;
  icGoldenYellow = $FF00B0FF;
  icAmberBrown = $FF006797;
  icDarkGoldenRod = $FF0080B0; // brown shade color

  icBarColorGreen = $FF00AA26;
  icBarColorBlue = $FFBBAA00;

  icBlackish = $FF333333;

  // Interface colors (by usage)
  clWhiteText = icWhite;
  clBlackText = icBlackish;

  clPingLow = icGreen;
  clPingNormal = icYellow;
  clPingHigh = icOrange;
  clPingCritical = icRed;

  clFpsCritical = icRed;
  clFpsLow = icOrange;
  clFpsNormal = icYellow;
  clFpsHigh = icGreen;

  clTextSelection = icSteelBlue;

  clMPSrvDetailsGameInfoFont = icLightGray;
  clStatsUnitDefault = icWhite;
  clStatsUnitMissingHL = $FF0080FF;

  clMessageUnitUnread = icGoldenYellow;
  clMessageUnitUnreadHL = icRoyalYellow;
  clMessageUnitRead = icDarkGoldenRod;
  clMessageUnitReadHL = icAmberBrown;

  clLobbyOpponentAll = icRoyalYellow;
  clLobbyOpponentAllHL = icAmberBrown;

  clListSelShape = $88888888;
  clListSelOutline = icWhite;
  clListSelShapeUnfocused = $66666666;
  clListSelOutlineUnfocused = icLightGray;
  clListSeparatorShape = icDarkGray;

  clMapEdBtnField = icYellow;
  clMapEdBtnWine = icYellow;

  clChartHighlight = icPink;
  clChartHighlight2 = icDarkPink;
  clChartDashedHLn = icDarkGray;
  clChartDashedVLn = icDarkGrayTrans;
  clChartPeacetimeLn = icDarkGoldenRod;
  clChartPeacetimeLbl = icGoldenYellow;
  clChartSeparator = icTransparent;

  clChkboxOutline = icLightGrayTrans;

  clPlayerSelf = icRed;
  clPlayerAlly = icYellow;
  clPlayerEnemy = icCyan;

  clScriptCmdName = icYellow;
  clScriptCmdParam = icLightGray;

  clSaveLoadOk = icWhite;
  clSaveLoadTry = icLightLightRed;
  clSaveLoadError = icLightRed;

//  clGameSelf = icRed;
//  clGameAlly = icYellow;
//  clGameEnemy = icCyan;

  GROUP_IMG: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Word = (
    371, 374,
    376, 377,
    684, 734,
    891, 966);

  GROUP_TXT_COLOR: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Cardinal = (
    icWhite,
    icGreen,
    icPink,
    icRed,
    icCyan,
    icOrange,
    icYellow,
    icYellow
    );
  type
    TKMDirType = (dtOldHouses, dtNewHouses, dtUnits, dtWares, dtModding);
    TKMFogRevealType = (frtScript, frtUnit, frtHouse);
var
  ExeDir: UnicodeString;

const
  WARRIORS_POWER_RATES: array [WARRIOR_MIN..WARRIOR_MAX] of Single = (
    1, 2.4, 5.2,    // utMilitia, utAxeFighter, utSwordsman
    2.2, 4,         // utBowman, utArbaletman
    2, 4,           // utPikeman, utHallebardman
    3.3, 6,         // utHorseScout, utCavalry
    5.3, 1.5, 1.5,  // utBarbarian, utPeasant, utSlingshot
    5.3, 2.1,        // utMetalBarbarian, utHorseman
    8, 8, 8,
    8, 16,10,
    10, 10,
    1.5,//utTrainedWolf
    5, 5,
    0,
    1, 2.4, 6, 6,
    0.5, 10, 10,
    2, 2, 2, 2, 10, 10,
    10
  );

const
  GITHUB_CRC_LINK = 'https://gist.githubusercontent.com/KingDyron/a4546ae43e2055eb640e8fd59d932197/raw';


implementation

class operator TKMUnitTypeArrayHelper.Add(const A, B : TKMUnitTypeArray) : TKMUnitTypeArray;
var I : Integer;
begin
  Result := A;
  SetLength(Result, length(A) + length(B));
  for I := 0 to High(B) do
    Result[I + length(A)] := B[I];
end;

function TKMUnitMainData.Valid : Boolean;
begin
  Result := (UnitType <> utNone)
            and (Count > 0)
            and (Condition > 0);
end;

procedure TKMUnitMainData.TakeUnit;
begin
  Dec(Count);
end;

initialization
begin
  {$IFNDEF USESECUREAUTH}
  //GAME_VERSION_POSTFIX := GAME_VERSION_POSTFIX + ' [ UNSECURE ]';
  {$ENDIF}

  {$IFDEF DEBUG}
  //GAME_VERSION_POSTFIX := GAME_VERSION_POSTFIX + ' [ DEBUG ]';
  {$ENDIF}

  {$IFDEF WDC64}
  GAME_VERSION_POSTFIX := GAME_VERSION_POSTFIX + ' [x64]';
  {$ENDIF}

  GAME_REVISION := AnsiString('r' + IntToStr(GAME_REVISION_NUM));
  GAME_VERSION := GAME_VERSION_PREFIX + VERSION_NAME + GAME_VERSION_POSTFIX + GAME_VERSION_CUSTOM_POSTFIX;
  //Clients of this net protocol version may connect to the dedicated server
  NET_PROTOCOL_REVISON := AnsiString('r' + IntToStr(NET_PROTOCOL_REVISION_NUM));
end;

end.
