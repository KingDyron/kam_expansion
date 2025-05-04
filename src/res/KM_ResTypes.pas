unit KM_ResTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_ResTilesetTypes, KM_TerrainTypes, KM_CommonClasses, KM_Defaults, KM_CommonTypes;

type
  TKMChopableAge = (caAge1, caAge2, caAge3, caAgeFull, caAgeFall, caAgeStump);
  TKMChopableAgeSet = set of TKMChopableAge;

  //* Ware type
  TKMWareType = (
    wtNone,
    wtTrunk,    wtStone,         wtTimber,     wtIronOre,      wtGoldOre,
    wtCoal,     wtIron,          wtGold,       wtWine,         wtCorn,
    wtBread,    wtFlour,         wtLeather,    wtSausage,      wtPig,
    wtSkin,     wtWoodenShield,  wtIronShield, wtLeatherArmor, wtIronArmor,
    wtAxe,      wtSword,         wtLance,      wtPike,         wtBow,
    wtCrossbow, wtHorse,         wtFish,       wtBitin,        wtVegetables,

    wtBitinOre, wtStoneBolt,     wtLog,        wtSteelE,       wtBitinE,
    wtWheel,    wtBolt,          wtQuiver,     wtWater,        wtTile,
    wtSeed,     wtSawDust,       wtApple,      wtJewerly,      wtBoots,
    wtHay,      wtMace,          wtFlail,      wtFeathers,     wtPlateArmor,
    wtBitinArmor, wtEgg,
    // Special ware types
    wtAll, wtWarfare, wtFood
  );

  //* Ware type set
  TKMWareTypeSet = set of TKMWareType;
  TKMWareTypeArray = array of TKMWareType;

const
  WARE_MIN = wtTrunk;
  WARE_MAX = wtEgg;
  WARE_MAX_ALL = wtFood;
  WARFARE_MIN = wtWoodenShield;
  WEAPON_MIN = wtWoodenShield;
  WEAPON_MAX = wtCrossbow;
  WARFARE_MAX = wtHorse;

  WARES_VALID = [WARE_MIN..WARE_MAX];

  WARES_WARFARE = [WARFARE_MIN..WARFARE_MAX, wtMace, wtFlail, wtPlateArmor, wtBitinArmor, wtBoots, wtQuiver];
  WARES_FOOD = [wtBread, wtSausage, wtFish, wtWine];
  WARES_HOUSE_FOOD = [wtBread, wtSausage, wtFish, wtWine, wtApple, wtVegetables];

  WARE_CNT = Integer(WARE_MAX) - Integer(WARE_MIN) + 1;
  WARFARE_CNT = Integer(WARFARE_MAX) - Integer(WEAPON_MIN) + 1;

  WARFARE_IRON = [wtIronShield, wtIronArmor, wtSword, wtPike, wtCrossbow, wtFlail];


type
  //* House type
  TKMHouseType = (
    htNone, htAny,
    htArmorSmithy,     htArmorWorkshop,   htBakery,        htBarracks,      htButchers,
    htCoalMine,        htFarm,            htFishermans,    htGoldMine,      htInn,
    htIronMine,        htIronSmithy,      htMarket,        htMetallurgists, htMill,
    htQuarry,          htSawmill,         htSchool,        htSiegeWorkshop, htStables,
    htStore,           htSwine,           htTannery,       htTownHall,      htWatchTower,
    htWeaponSmithy,    htWeaponWorkshop,  htVineyard,      htWoodcutters,
    // Added By me
    htWall,            htWall2,           htWall3,         htWall4,         htWall5,
    htHovel,           htSign,            htBitinMine,     htWallTower,     htWell,
    htStoneWorkshop,   htIronFoundry,     htMerchant,      htPottery,       htWoodBurner,
    htAppleTree,       htSmallStore,      htCollectors,    htTailorsShop,   htCottage,
    htHouse,           htPalace,          htStall,         htProductionThatch,
    htShipYard,        htCartographers,   htPearl
  );

  //* House type set
  TKMHouseTypeSet = set of TKMHouseType;
  TKMHouseTypeArray = array of TKMHouseType;
  TKMHouseTypeArray2 = array of TKMHouseTypeArray;

  TKMWareDistributionType = array of Record
    WareType : TKMWareType;
    Houses : array of Record
      House : TKMHouseType;
      Qty : Byte;
    end;
  end;
  //shared house types
  TKMHouseArea = array [1..4, 1..4] of Byte;
  TKMWareType4 = array [1..4] of TKMWareType;
  TKMWareType8 = array [1..WARES_IN_OUT_COUNT] of TKMWareType;//now lets make it more wares
  TKMHouseAreaNew = array [1..MAX_HOUSE_SIZE, 1..MAX_HOUSE_SIZE] of Byte;
  THouseSupply8 = array [1..WARES_IN_OUT_COUNT, 1..5] of SmallInt;

  TKMHouseSound = record
    ID : Integer;
    Steps : TByteSet;
  end;
  TKMHouseWorkAnim = record
    Action : TKMHouseActionType;
    Cycles : Single;
  end;
  TKMHouseWareSlot = record
    WareInput : TKMWareType8;
    WareOutput : TKMWareType8;
    Icon : Word;
  end;

  TKMHouseStyle = record
    StonePic : Word;
    SnowPic : Word;
    Icon : Word;
    HideSupplies: Boolean;
  end;


  THouseBuildSupplyOld = array [1..2,1..6] of packed record MoveX, MoveY: Integer; end;
  THouseBuildSupply = array [1..3] of packed record MoveX, MoveY: Integer; end;
  THouseSupply = array [1..4, 1..5] of SmallInt;
  //* Woodcutting mode
  TKMWoodcutterMode = (wmChopAndPlant, wmChop, wmPlant);
  TKMCollectorsMode = (cmCollector, cmHunter);
  TKMPearlType = (ptNone, ptValtaria, ptArium, ptAgros, ptRalender);

  TKMProdThatchAnimType = (ptaSawDust, ptaStoneDust, ptaSmokeBakery, ptaSmokeIron, ptaSmokeGold, ptaWindows, ptaWine, ptaCorn);
  TKMPTAnim = record
    StartTick, EndTick : Cardinal;
  end;
  TKMKeyFunction = (
    kfNone,

    // htWall
    kfScrollLeft,   // Scroll Left
    kfScrollRight,  // Scroll Right
    kfScrollUp,     // Scroll Up
    kfScrollDown,   // Scroll Down

    kfMapDragScroll, // Map drag scroll

    kfZoomIn,     // Zoom In
    kfZoomOut,    // Zoom Out
    kfZoomReset,  // Reset Zoom

    kfCloseMenu,  // Close opened menu

    kfMusicPrevTrack,   // Music previous track
    kfMusicNextTrack,   // Music next track
    kfMusicDisable,     // Music disable
    kfMusicShuffle,     // Music shuffle
    kfMusicVolumeUp,    // Music volume up
    kfMusicVolumeDown,  // Music volume down
    kfMusicMute,        // Music mute

    kfSoundVolumeUp,    // Sound volume up
    kfSoundVolumeDown,  // Sound volume down
    kfSoundMute,        // Sound mute

    kfMuteAll,          // Mute music and sound

    kfDebugWindow,    // Debug window
    kfDebugRevealmap, // Debug Menu Reveal Map
    kfDebugVictory,   // Debug Menu Victory
    kfDebugDefeat,    // Debug Menu Defeat
    kfDebugAddscout,  // Debug Menu Add Scout

    // faGame
    kfMenuBuild,  // Build Menu
    kfMenuRatio,  // Ratio Menu
    kfMenuStats,  // Stats Menu
    kfMenuMenu,   // Main Menu

    kfSpeedup1, // Speed up 1
    kfSpeedup2, // Speed up 2
    kfSpeedup3, // Speed up 3
    kfSpeedup4, // Speed up 4

    kfBeacon,     // Beacon
    kfPause,      // Pause
    kfShowTeams,  // Show teams in MP

    kfCenterAlert,  // Center to alert
    kfDeleteMsg,    // Delete message
    kfChat,         // Show the chat

    kfNextEntitySameType,  // Select next building/unit with same type
    kfPlayerColorMode,     // Switch player color mode

    kfPlanRoad,   // Plan road
    kfPlanField,  // Plan field
    kfPlanWine,   // Plan wine
    kfErasePlan,  // Erase plan

    kfSelect1,  // Select 1
    kfSelect2,  // Select 2
    kfSelect3,  // Select 3
    kfSelect4,  // Select 4
    kfSelect5,  // Select 5
    kfSelect6,  // Select 6
    kfSelect7,  // Select 7
    kfSelect8,  // Select 8
    kfSelect9,  // Select 9
    kfSelect10, // Select 10
    kfSelect11, // Select 11
    kfSelect12, // Select 12
    kfSelect13, // Select 13
    kfSelect14, // Select 14
    kfSelect15, // Select 15
    kfSelect16, // Select 16
    kfSelect17, // Select 17
    kfSelect18, // Select 18
    kfSelect19, // Select 19
    kfSelect20, // Select 20

    // faUnit
    kfArmyHalt,       // Halt Command
    kfArmySplit,      // Split up Command
    kfArmyLink,       // Linkup Command
    kfArmyFood,       // Food Command
    kfArmyStorm,      // Storm Command
    kfArmyAddLine,    // Formation Increase Line Command
    kfArmyDelLine,    // Formation Shrink Line Command
    kfArmyRotateCw,   // Turn Right Command
    kfArmyRotateCcw,  // Turn Left Command

    // faHouse
    kfTrainGotoPrev,  // Goto previuos unit
    kfTrainEquipUnit, // Train or Equip unit
    kfTrainGotoNext,  // Goto next unit

    // faSpecReplay
    kfSpecpanelSelectDropbox, // Select dropbox on spectator panel
    kfReplayPlayNextTick,     // Play next tick in replay
    kfSpectatePlayer1,  // Spectate player 1
    kfSpectatePlayer2,  // Spectate player 2
    kfSpectatePlayer3,  // Spectate player 3
    kfSpectatePlayer4,  // Spectate player 4
    kfSpectatePlayer5,  // Spectate player 5
    kfSpectatePlayer6,  // Spectate player 6
    kfSpectatePlayer7,  // Spectate player 7
    kfSpectatePlayer8,  // Spectate player 8
    kfSpectatePlayer9,  // Spectate player 9
    kfSpectatePlayer10, // Spectate player 10
    kfSpectatePlayer11, // Spectate player 11
    kfSpectatePlayer12, // Spectate player 12

    // faMapEdit
    kfMapedExtra,     // Maped Extra's menu
    kfMapedSaveMap,   // Maped Save map
    kfMapedTerrain,   // Maped Terrain Editing
    kfMapedVillage,   // Maped Village Planning
    kfMapedVisual,    // Maped Visual Scripts
    kfMapedGlobal,    // Maped Global Scripting
    kfMapedMainMenu,  // Maped Main Menu
    kfMapedSubMenu1,  // Maped Sub-menu 1
    kfMapedSubMenu2,  // Maped Sub-menu 2
    kfMapedSubMenu3,  // Maped Sub-menu 3
    kfMapedSubMenu4,  // Maped Sub-menu 4
    kfMapedSubMenu5,  // Maped Sub-menu 5
    kfMapedSubMenu6,  // Maped Sub-menu 6
    kfMapedSubMenu7,  // Maped Sub-menu 6
    kfMapedSubMenuAction1,  // Maped Sub-menu Action 1
    kfMapedSubMenuAction2,  // Maped Sub-menu Action 2
    kfMapedSubMenuAction3,  // Maped Sub-menu Action 3
    kfMapedSubMenuAction4,  // Maped Sub-menu Action 4
    kfMapedSubMenuAction5,  // Maped Sub-menu Action 5
    kfMapedSubMenuAction6,  // Maped Sub-menu Action 6
    kfMapedSubMenuAction7,  // Maped Sub-menu Action 6
    kfMapedObjPalette,      // Maped Objects palette
    kfMapedTilesPalette,    // Maped Tiles palette
    kfMapedUnivErasor,      // Maped Universal erasor
    kfMapedPaintBucket,     // Maped Paint bucket
    kfMapedHistory,         // Maped History
    kfMapedFlatTerrain,     // Maped Flat terrain
    kfMapedTilesGrid        // Maped Tiles grid
  );

  TKMKeyFunctionSet = set of TKMKeyFunction;

  TKMKeyFuncArea = (faCommon,
                      faGame,
                        faUnit,
                        faHouse,
                      faSpecReplay,
                      faMapEdit);

  TKMKeyFuncAreaSet = set of TKMKeyFuncArea;

const
  KEY_FUNC_LOW = Succ(kfNone); // 1st key function

  KEY_FUNCS_ALL = [KEY_FUNC_LOW..High(TKMKeyFunction)];

type
  // Cursors
  TKMCursorImageType = (
    kmcDefault, kmcInfo, kmcAttack, kmcJoinYes, kmcJoinNo, kmcEdit, kmcDragUp,
    kmcDir0, kmcDir1, kmcDir2, kmcDir3, kmcDir4, kmcDir5, kmcDir6, kmcDir7, kmcDirNA,
    kmcScroll0, kmcScroll1, kmcScroll2, kmcScroll3, kmcScroll4, kmcScroll5, kmcScroll6, kmcScroll7,
    kmcBeacon, kmcDrag,
    kmcInvisible, // for some reason kmcInvisible should be at its current position in enum. Otherwise 1px dot will appear while TroopSelection is on
    kmcPaintBucket,
    kmcChangeResCount,
    kmcAnimatedDirSelector
  );

const
  // Indexes of cursor images in GUI.RX
  CURSOR_SPRITE_INDEX: array [TKMCursorImageType] of Word = (
    1, 452, 457, 460, 450, 453, 449,
    511,  512, 513, 514, 515, 516, 517, 518, 519,
    4, 7, 3, 9, 5, 8, 2, 6,
    456, 451, 999, 661, 952, 0);

type
  TRXUsage = (ruMenu, ruGame, ruCustom); //Where sprites are used

  TKMSpriteAtlasType = (saBase, saMask);

  TRXInfo = record
    FileName: string; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    LoadingTextID: Word;
  end;

  TKMGenTerrainInfo = record
    TerKind: TKMTerrainKind;
    Mask: TKMMaskFullType;
    CoalOverlay : TKMTileOverlay;
  end;

// ResSprites Types
type
  TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxCustom, //Used for loading stuff like campaign maps (there is no main RXX file)
    rxTiles //Tiles
  );
  TRXTypeSet = set of TRXType;

const
  EXPORT_SPRITE_ATLASES_LIST: TRXTypeSet = [
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxCustom,
    rxTiles
  ];

  //Colors to paint beneath player color areas (flags)
  //The blacker/whiter - the more contrast player color will be
  FLAG_COLOR_DARK = $FF101010;   //Dark-grey (Black)
  FLAG_COLOR_LITE = $FFFFFFFF;   //White

type
  // Original KaM format for RX files
  {
  TRXData = record
    Count: Integer;
    Flag: array of Byte; // Sprite is valid
    Size: array of record X,Y: Word; end;
    Pivot: array of record X,Y: Integer; end;
    Data: array of array of Byte;
  end;}

  // RXX and RXX1 file format, with Pivot type changed and SizeNoShadow added
  TRXData = record
    Count: Integer;
    Flag: array of Byte; //Sprite is valid
    Size: array of record X,Y: Word; end;
    Pivot: array of record X,Y: SmallInt; end;
    SizeNoShadow: array of record Left, Top, Right, Bottom: SmallInt; end; //Image object (without shadow) rect in the image sizes
    {unused in RXX} Data: array of array of Byte; //Used for RXX utils (Packer / Editor)
    RGBA: array {Index} of array {YX} of Cardinal; //Expanded image
    Mask: array of array of Byte; //Mask for team colors
    HasMask: array of Boolean; //Flag if Mask for team colors is used
  end;
  PRXData = ^TRXData;

  TKMWarePlanSingle = record
      W : TKMWareType;
      C : Word;
    end;
  TKMWarePlan = array of TKMWarePlanSingle;
  PKMWarePlan = ^TKMWarePlan;

  TKMWarePlanHelper = record helper for TKMWarePlan
    function Count : Byte;
    function HasWare(aWare : TKMWareType) : Word;
    function HasWares(aWares : TKMWareTypeSet) : Boolean;
    function HasAnyWares(aWares : TKMWareTypeSet) : Boolean;
    procedure SetCount(aCount : Byte; aDoClear : Boolean = false);
    procedure AddWare(aWare : TKMWareType; aCount: Integer = 1; aKeepZero : Boolean = false);
    function IndexOf(aWare : TKMWareType; aMinCount : Integer = 0) : Integer;
    procedure Remove(aIndex : Integer);
    procedure CopyFrom(aWare : TKMWarePlan);
    procedure CopyTo(var aWare : TKMWarePlan);
    procedure Clear;
    procedure Reset;
    procedure Save(SaveStream : TKMemoryStream);
    procedure Load(LoadStream : TKMemoryStream);
  end;

  TKMVWarePlanCommon = array of record
    W : String;
    C : Word;
  end;


  TKMVWarePlan = record
    PhaseCount : Integer;
    PhaseDuration : Integer;
    Plan : TKMWarePlan;
    Wares : array of record
      W : String;
      Index: Byte;
      C : Byte;
    end;
  end;
  TKMBuildCost = record
    W : TKMWareType;
    C : Byte;
  end;

  TKMBridgeTile = record
    Obj, Rot, Tile, Order, Point : Word;
  end;

  TKMBridgeTileRect = array of array of TKMBridgeTile;

const
  {$I KM_TextIDs.inc}

  RX_INFO: array [TRXType] of TRXInfo = (
    (FileName: 'Trees';      TeamColors: False; Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_TREES;),
    (FileName: 'Houses';     TeamColors: True;  Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_HOUSES;),
    (FileName: 'Units';      TeamColors: True;  Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_UNITS;),
    (FileName: 'GUI';        TeamColors: True;  Usage: ruMenu;   LoadingTextID: 0;),
    (FileName: 'GUIMain';    TeamColors: False; Usage: ruMenu;   LoadingTextID: 0;),
    (FileName: 'Custom';     TeamColors: False; Usage: ruCustom; LoadingTextID: 0;),
    (FileName: 'Tileset';    TeamColors: False; Usage: ruMenu;   LoadingTextID: TX_MENU_LOADING_TILESET;));

  function GetKeyFunctionStr(aKeyFun: TKMKeyFunction): string;
  procedure ClearWarePlan(var aPlan : TKMWarePlan);

implementation
uses
  TypInfo;


function GetKeyFunctionStr(aKeyFun: TKMKeyFunction): string;
begin
//  Result := TRttiEnumerationType.GetName(aKeyFun);
  Result := GetEnumName(TypeInfo(TKMKeyFunction), Integer(aKeyFun));
end;

procedure ClearWarePlan(var aPlan : TKMWarePlan);
var I : integer;
begin
  for I := Low(aPlan) to High(aPlan) do
  begin
    aPlan[I].W := wtNone;
    aPlan[I].C := 0;
  end;
end;

function TKMWarePlanHelper.Count: Byte;
begin
  Result := length(self);
end;

function TKMWarePlanHelper.HasWare(aWare: TKMWareType): Word;
var I : integer;
begin
  Result := 0;
  for I := 0 to High(self) do
    if Self[I].W = aWare then
      Exit(Self[I].C);
end;

function TKMWarePlanHelper.HasWares(aWares: TKMWareTypeSet): Boolean;
var I : Integer;
begin

  for I := 0 to High(self) do
    if (Self[I].W in aWares) and (Self[I].C > 0) then
    begin
      aWares := aWares - [Self[I].W];
      if aWares = [] then
        Exit(true);
    end;

  Result := aWares = [];
end;

function TKMWarePlanHelper.HasAnyWares(aWares: TKMWareTypeSet): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to High(self) do
    if (Self[I].W in aWares) and (Self[I].C > 0) then
    begin
      Exit(true);
    end;

end;


procedure TKMWarePlanHelper.SetCount(aCount : Byte; aDoClear : Boolean = false);
begin
  if aDoClear then
    SetLength(self, 0); //clear everything
  SetLength(self, aCount);
end;

function TKMWarePlanHelper.IndexOf(aWare : TKMWareType; aMinCount : Integer = 0) : Integer;
var I : Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (self[I].W = aWare) and (self[I].C >= aMinCount) then
      Exit(I);
end;

procedure TKMWarePlanHelper.AddWare(aWare : TKMWareType; aCount: Integer = 1; aKeepZero : Boolean = false);
var I : Integer;
begin
  if ((aWare = wtNone) or (aCount = 0)) and not aKeepZero then
    Exit;
  for I := 0 to High(self) do
    if (self[I].W = wtNone) or ((self[I].C = 0) and not aKeepZero) then
    begin
      self[I].W := aWare;
      self[I].C := aCount;
      Exit;
    end else
    if self[I].W = aWare then
    begin
      self[I].C := self[I].C + aCount;
      Exit;
    end;
  //no free space found, so add one

  I := Count;
  SetLength(self, I + 1);
  self[I].W := aWare;
  self[I].C := aCount;
end;

procedure TKMWarePlanHelper.Clear;
var I : Integer;
begin
  for I := 0 to High(self) do
  begin
    self[I].W := wtNone;
    self[I].C := 0;
  end;
end;

procedure TKMWarePlanHelper.Reset;
begin
  SetLength(self, 0);
end;

procedure TKMWarePlanHelper.Remove(aIndex: Integer);
var I : integer;
begin
  for I := aIndex to High(self) - 1 do
    self[I] := self[I+1];

  SetLength(self, High(self));
end;

procedure TKMWarePlanHelper.CopyFrom(aWare: TKMWarePlan);
var I : Integer;
begin
  self.Clear;
  for I := 0 to High(aWare) do
    self.AddWare(aWare[I].W, aWare[I].C);
end;

procedure TKMWarePlanHelper.CopyTo(var aWare: TKMWarePlan);
var I : Integer;
begin
  aWare.Clear;
  for I := 0 to High(self) do
    aWare.AddWare(self[I].W, self[I].C);
end;

procedure TKMWarePlanHelper.Save(SaveStream: TKMemoryStream);
var newCount, I : Integer;
begin
  newCount := self.Count;

  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(self[I].W, sizeOf(self[I].W));
    SaveStream.Write(self[I].C);
  end;
end;

procedure TKMWarePlanHelper.Load(LoadStream: TKMemoryStream);
var newCount, I : Integer;
begin
  LoadStream.Read(newCount);
  self.SetCount(newCount);

  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(self[I].W, sizeOf(self[I].W));
    LoadStream.Read(self[I].C);
  end;
end;

end.
