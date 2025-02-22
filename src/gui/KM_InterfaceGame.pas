unit KM_InterfaceGame;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Classes, Math,
  Vcl.Controls,
  KM_Defaults,
  KM_Controls, KM_ControlsBase,
  KM_Points,
  KM_InterfaceDefaults, KM_CommonTypes, KM_AIDefensePos,
  KM_Cursor, KM_Render, KM_MinimapGame, KM_Viewport, KM_ResFonts,
  KM_ResTypes, KM_CommonClassesExt, KM_AITypes;


type
  // Common class for ingame interfaces (Gameplay, MapEd)
  TKMUserInterfaceGame = class abstract(TKMUserInterfaceCommon)
  private
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;
    fOnUserAction: TKMUserActionEvent;

    fLogStringList: TKMLimitedList<string>;

    function GetLogString(aLimit: Integer): string;
    procedure LogMessageHappened(const aLogMessage: UnicodeString);

    procedure ResetDragScrolling;
    procedure PaintDefences;
  protected
    fMinimap: TKMMinimapGame;
    fViewport: TKMViewport;
    fDragScrolling: Boolean;

    fPaintDefences: Boolean;

    Bevel_DebugInfo: TKMBevel;
    Label_DebugInfo: TKMLabel;

    function ZoomChangeBlocked: Boolean; virtual;

    function IsDragScrollingAllowed: Boolean; virtual;
    function GetHintPositionBase: TKMPoint; override;
    function GetHintFont: TKMFont; override;
    function GetHintKind: TKMHintKind; override;

    function GetDebugInfo: string; virtual;

    procedure InitDebugControls;

    procedure ViewportPositionChanged(const aPos: TKMPointF);
    procedure OptionsChanged; virtual; abstract;

    procedure HandleScrollKeysDown(Key: Word; var aHandled: Boolean);
    procedure HandleScrollKeysUp(Key: Word; var aHandled: Boolean);
  public
    constructor Create(aRender: TKMRender); reintroduce;
    destructor Destroy; override;

    property Minimap: TKMMinimapGame read fMinimap;
    property Viewport: TKMViewport read fViewport;
    property OnUserAction: TKMUserActionEvent read fOnUserAction write fOnUserAction;

    function CursorToMapCoord(X, Y: Integer): TKMPointF;

    procedure DebugControlsUpdated(aSenderTag: Integer); override;

    procedure KeyDown(Key: Word; Shift: TShiftState; aIsFirst: Boolean; var aHandled: Boolean); override;
    procedure KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean); override;
    procedure KeyPress(Key: Char); override;
    procedure MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer; var aHandled: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure ShowDebugInfo;

    procedure GameSpeedChanged(aFromSpeed, aToSpeed: Single);
    procedure SyncUI(aMoveViewport: Boolean = True); virtual;
    procedure SyncUIView(const aCenter: TKMPointF); overload;
    procedure SyncUIView(const aCenter: TKMPointF; aZoom: Single); overload;
    procedure UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
    procedure UpdateStateIdle(aFrameTime: Cardinal); virtual; abstract;
    procedure UpdateState(aGlobalTickCount: Cardinal); override;

    procedure Paint; override;
  end;


const
  // Toolbar pads
  TB_PAD = 9; // Picked up empirically
  TB_WIDTH = 180; // Max width of sidebar elements
  //TB_MAP_ED_WIDTH = 214; //Max width of sidebar elements in Map Editor
  TB_MAP_ED_WIDTH = 214; //Max width of sidebar elements in Map Editor
  PAGE_TITLE_Y = 5; // Page title offset
  TERRAIN_PAGE_TITLE_Y = PAGE_TITLE_Y + 2; // Terrain pages title offset
  STATS_LINES_CNT = 13; //Number of stats (F3) lines

  DEFENCE_LINE_TYPE_COL: array [TKMAIDefencePosType] of Cardinal = ($FF80FF00, $FFFF8000, icOrange);

  // Shortcuts
  // All shortcuts are in English and are the same for all languages to avoid
  // naming collisions and confusion in discussions
  {GUI_HOUSE_COUNT = 29;

  GUIHouseOrder: array [1..GUI_HOUSE_COUNT] of TKMHouseType = (
    htSchool, htInn, htQuarry, htWoodcutters, htSawmill,
    htFarm, htMill, htBakery, htSwine, htButchers,
    htVineyard, htGoldMine, htCoalMine, htMetallurgists, htWeaponWorkshop,
    htTannery, htArmorWorkshop, htStables, htIronMine, htIronSmithy,
    htWeaponSmithy, htArmorSmithy, htBarracks, htStore, htWatchTower,
    htFishermans, htMarket, htTownHall, htPottery);}

  //Gui house order has now expand panels, every // means it will create new line
  GUIHouseOrderFull: array [1..54] of TKMHouseType = (
    htSchool, htInn, htCottage, htHouse,  htStore,
    htQuarry, htWoodcutters, htSawmill, htPottery, htCollectors,//

    htWell, htAppleTree, htFarm, htVineyard, htFishermans,
    htSmallStore, htGoldMine, htIronMine, htBitinMine, htCoalMine,//
    htMarket, htMerchant, htStall, htSign,
    htMill, htBakery, htSwine, htButchers, htTannery,
    htStables, htHovel, htWeaponWorkshop, htArmorWorkshop, htTailorsShop,
    htMetallurgists, htIronSmithy, htWeaponSmithy, htArmorSmithy, htIronFoundry,
    htStoneWorkshop, htWoodBurner, htProductionThatch,
    htBarracks, htTownhall, htPalace, htSiegeWorkshop, htShipYard,
    htWall, htWall2, htWall3, htWall4, htWall5,
    htWallTower, htWatchTower
    );

  //GUIHouseOrderPageStart: set of TKMHouseType = [htSchool, htQuarry, htMill, htGoldMine, htWeaponWorkshop, htWall, htMarket];
  //GUIHouseOrderPageHint: array[0..6] of Integer = (1649, 1650, 1651, 1652, 1653, 1654, 1655);

  // Layout of resources in Store
  STORE_RES_COUNT = 59;


  {StoreResType: array [1..STORE_RES_COUNT] of TKMWareType =
    (wtTrunk,    wtStone,   wtTimber,        wtIronOre,   wtGoldOre,
     wtCoal,     wtIron,   wtGold,        wtWine,      wtCorn,
     wtBread,    wtFlour,   wtLeather,     wtSausage,  wtPig,
     wtSkin,     wtWoodenShield,  wtIronShield, wtLeatherArmor,     wtIronArmor,
     wtAxe,      wtSword,   wtLance,        wtPike, wtBow,
     wtCrossbow,  wtHorse,   wtFish,        wtEgg, wtBitinOre,
     wtBitin, wtStoneBolt,     wtLog,        wtSteelE,       wtBitinE,
    wtWheel,    wtBolt,          wtQuiver
     );}
  //Resources has also expanding panels
  StoreResType: array [1..STORE_RES_COUNT] of TKMWareType =
    (
     //new line
     wtNone,
     wtTrunk,         wtStone,          wtTimber,         wtTile,   wtApple, //basic


     //new line
     wtNone,
     wtVegetables,   wtWine,           wtBread,          wtSausage,        wtFish,//food


     //new line
     wtNone,
     wtWater,         wtCorn,            wtSeed,          wtHay,            wtFlour,     //farm/animal products
     wtPig,           wtSkin,            wtLeather,       wtFeathers,       wtSawDust,

     //new line
     wtNone,
     wtGoldOre,       wtIronOre,        wtBitinOre,       wtCoal,//ores
     //new line
     wtNone,
     wtIron,          wtGold,           wtBitin,          wtEgg,        wtJewerly,

     //new line
     wtNone,
     wtWoodenShield,  wtLeatherArmor,   wtAxe,            wtLance,      wtBow,
     wtIronShield,    wtIronArmor,      wtSword,          wtPike,       wtCrossbow,
     wtMace,          wtPlateArmor,     wtFlail,          wtHorse,
     wtQuiver,         wtBoots,         wtBitinArmor,


     wtNone,//new line
     wtWheel,         wtSteelE,         wtBitinE,        wtLog,         wtStoneBolt,
     wtBolt
     );
  // Template for how resources are shown in Barracks
  BarracksResOrder: array[0..21] of TKMWareType =
  (
    //leather weapons
    wtNone,//new line
    wtAxe, wtBow, wtLance, wtMace,

    wtNone,//new line
    wtLeatherArmor, wtPlateArmor, wtWoodenShield,

    //iron weapons
    wtNone,//new line
    wtSword, wtCrossbow, wtPike, wtFlail,

    wtNone,//new line
    wtIronArmor, wtIronShield,

    wtNone,//last line = icons on the right
    wtHorse, wtQuiver, wtBoots, wtBitinArmor

  );

  School_Order: array [0..15] of TKMUnitType = (
    utSerf, utBuilder, utStonemason, utWoodcutter, utCarpenter,
    utClayPicker, utFisher, utFarmer, utBaker, utAnimalBreeder, utButcher,
    utMiner, utMetallurgist, utSmith, utOperator, utRecruit);

  SiegeWorkshop_Order: array [0..4] of TKMUnitType = (
    utCatapult, utBallista, utRam, utWoodenWall, utSpikedTrap);

  Barracks_Order: array [0..11] of TKMUnitType = (
    utFighter, utMilitia, utAxeFighter, utSwordFighter, utBowman, utCrossbowman,
    utLanceCarrier, utPikeman, utMaceFighter, utFlailFighter, utScout, utKnight);

  TownHall_Order: array [0..6] of TKMUnitType = (
    utRebel, utRogue, utClubMan, utVagabond, utShieldBearer, utBarbarian, utWarrior);

  Soldiers_Order: array[0..36] of TKMUnitType = (
    utFighter, utMilitia, utAxeFighter, utSwordFighter, utBarbarian, utWarrior, utPaladin,
    utRogue, utBowman, utCrossbowMan, utArcher,
    utRebel, utLanceCarrier, utPikeman, utPikeMachine,
    utClubMan, utMaceFighter, utFlailFighter,
    utVagabond, utScout, utKnight, utTrainedWolf,
    utCatapult, utBallista, utAmmoCart, utShieldBearer, utWoodenWall, utRam,
    utPyro, utTorchMan, utLekter, utMedic, utSpy, utSpikedTrap, utBoat, utShip, utBattleShip);

  // Stats get stacked by UI logic (so that on taller screens they all were
  // in nice pairs, and would stack up only on short screens)
  StatUnitOrder : array[0..13] of TKMUnitType =
  (
    utRecruit,
    utOperator,
    utStoneMason,
    utWoodcutter,
    utCarpenter,
    utClayPicker,
    utFarmer,
    utBaker,
    utAnimalBreeder,
    utButcher,
    utMiner,
    utMetallurgist,
    utSmith,
    utFisher
  );
  StatNonWorkerHouse: array[0..7] of TKMHouseType = (htStore, htSchool, htInn, htTownhall, htPalace, htCottage, htHouse, htWell);
  {
  StatPlan: array [0..STATS_LINES_CNT-1] of record
    HouseType: array [0..4] of TKMHouseType;
    UnitType: array [0..1] of TKMUnitType;
  end = (
    (HouseType: (htQuarry, htStoneWorkshop, htNone, htNone, htNone);                               UnitType: (utStonemason, utNone)),
    (HouseType: (htWoodcutters, htNone, htNone, htNone, htNone);                          UnitType: (utWoodcutter, utNone)),
    (HouseType: (htFishermans, htNone, htNone, htNone, htNone);                           UnitType: (utFisher, utNone)),
    (HouseType: (htFarm, htVineyard, htNone, htNone, htNone);                            UnitType: (utFarmer, utNone)),
    (HouseType: (htMill, htBakery, htNone, htNone, htNone);                               UnitType: (utBaker, utNone)),
    (HouseType: (htSwine, htStables, htHovel, htNone, htNone);                            UnitType: (utAnimalBreeder, utNone)),
    (HouseType: (htButchers, htTannery, htNone, htNone, htNone);                          UnitType: (utButcher, utNone)),
    (HouseType: (htMetallurgists, htIronSmithy, htNone, htNone, htNone);                  UnitType: (utMetallurgist, utNone)),
    (HouseType: (htArmorSmithy, htWeaponSmithy, htIronFoundry, htNone, htNone);                  UnitType: (utSmith, utNone)),
    (HouseType: (htCoalMine, htIronMine, htGoldMine, htBitinMine, htNone);                     UnitType: (utMiner, utNone)),
    (HouseType: (htSawmill, htWeaponWorkshop, htArmorWorkshop, htSiegeWorkshop, htNone);  UnitType: (utCarpenter, utNone)),
    (HouseType: (htBarracks, htTownHall, htWatchTower, htWallTower, htNone);              UnitType: (utRecruit, utNone)),
    (HouseType: (htStore, htSchool, htInn, htMarket, htNone);                             UnitType: (utSerf, utBuilder))
    ); }

  MapEd_Order: array [0..36] of TKMUnitType = (
    utFighter, utMilitia, utAxeFighter, utSwordFighter, utBowman, utCrossbowman,
    utLanceCarrier, utPikeman, utMaceFighter, utFlailFighter,
    utScout, utKnight,
    utRebel, utRogue, utClubMan, utVagabond,utShieldBearer, utBarbarian, utWarrior,
    utCatapult, utBallista, utRam, utWoodenWall,
    utAmmoCart, utPikeMachine,
    utTorchMan, utPyro, utLekter, utMedic, utPaladin, utArcher, utSpy, utTrainedWolf, utSpikedTrap,
    utBoat, utShip, utBattleShip{, utGolem, utGiant});

  MapEd_Icon: array [0..26] of Word = (
    61, 62, 63, 64, 65,
    66, 67, 68, 69, 70,
    79, 80, 81, 82, 684, 683, 734, 683, 683, 738, 751, 141, 142, 71, 71, 142, 142);

  Animal_Order: array [0..17] of TKMUnitType = (
    utFish, utWatersnake, utSeastar,
    utWaterflower, utWaterleaf,  utDuck,
    utCrab, utSandSnake, utSpider, utLandDuck, utRabbit,
    utWolf, utFox, utDeerMale, utDeerFemale,
    utBoar, utBear, utWhiteBear);

  Animal_Icon: array [0..7] of word = (
    71, 72, 73, 74,
    75, 76, 77, 78);

  MARKET_RES_HEIGHT = 35;

  // Big tab buttons in MapEd
  BIG_TAB_W = 41;
  BIG_PAD_W = 41;
  BIG_TAB_H = 36;
  // Small sub-tab buttons in MapEd
  SMALL_TAB_W = 30;
  SMALL_PAD_W = 29;
  SMALL_TAB_H = 30;

  MESSAGE_AREA_HEIGHT = 173+17; // Image_ChatHead + Image_ChatBody
  MESSAGE_AREA_RESIZE_Y = 200; // How much can we resize it

  BUTTON_BLOCK_HOUSE_TAG_2 = 2;
  BUTTON_STORE_TAG_2 = 3;
  BUTTON_BLOCK_WARE_TRADE_TAG_2 = 4;
  BUTTON_BLOCK_UNIT_TAG_2 = 5;
  BUTTON_BLOCK_FIELD_TAG_2 = 6;
  BUTTON_BLOCK_STRUCT_TAG_2 = 7;
  BUTTON_BLOCK_DECOR_TAG_2 = 8;
  BUTTON_BLOCK_SPAWNER_TAG_2 = 9;

implementation
uses
  StrUtils, KromUtils,
  KM_Main, KM_System, 
  KM_GameParams, KM_GameSettings,
  KM_HandsCollection, 
  KM_Terrain, 
  KM_RenderPool, KM_RenderUI, KM_RenderTerrain, KM_Pics,
  KM_Resource, KM_ResKeys,
  KM_Sound, KM_ScriptSound,
  KM_CommonUtils, KM_Log;


{ TKMUserInterfaceGame }
constructor TKMUserInterfaceGame.Create(aRender: TKMRender);
begin
  inherited Create(aRender.ScreenX, aRender.ScreenY);

  fMinimap := TKMMinimapGame.Create(False);
  fViewport := TKMViewport.Create(GetToolbarWidth, aRender.ScreenX, aRender.ScreenY, ViewportPositionChanged);

  gLog.AddOnLogEventSub(LogMessageHappened);
  fLogStringList := TKMLimitedList<string>.Create(80); // 50 lines max

  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos := KMPOINTF_ZERO;

  fPaintDefences := False;

  gRenderPool := TKMRenderPool.Create(fViewport, aRender);
end;


destructor TKMUserInterfaceGame.Destroy;
begin
  gLog.RemoveOnLogEventSub(LogMessageHappened);
  fLogStringList.Free;

  FreeAndNil(fMinimap);
  FreeAndNil(fViewport);
  FreeAndNil(gRenderPool);
  Inherited;
end;


procedure TKMUserInterfaceGame.DebugControlsUpdated(aSenderTag: Integer);
begin
  inherited;

  if aSenderTag = Ord(dcFlatTerrain) then
    gTerrain.UpdateLighting;
end;


function TKMUserInterfaceGame.GetHintPositionBase: TKMPoint;
begin
  Result := KMPoint(GetToolbarWidth + 35, Panel_Main.Height);
end;


function TKMUserInterfaceGame.GetHintFont: TKMFont;
begin
  Result := fntOutline;
end;


function TKMUserInterfaceGame.GetHintKind: TKMHintKind;
begin
  Result := hkStatic;
end;


procedure TKMUserInterfaceGame.LogMessageHappened(const aLogMessage: UnicodeString);
begin
  if Self = nil then Exit;

  if SHOW_LOG_IN_GUI or UPDATE_LOG_FOR_GUI then
    fLogStringList.Add(ReplaceStr(DeleteDoubleSpaces(aLogMessage), EolW, '|'));
end;



function TKMUserInterfaceGame.GetLogString(aLimit: Integer): string;
var
  I: Integer;
begin
  Result := '';
  // attach from the end, the most fresh log lines, till the limit
  for I := fLogStringList.Count - 1 downto Max(fLogStringList.Count - aLimit - 1, 0) do
    Result := fLogStringList[I] + '|' + Result;
end;


function TKMUserInterfaceGame.GetDebugInfo: string;
begin
  Result :='';

  if SHOW_VIEWPORT_INFO then
    Result := Result + 'Viewport: ' + fViewport.ToStr + '|';

  if SHOW_FPS then
    Result := Result + gMain.FPSString + '|';
end;


procedure TKMUserInterfaceGame.ShowDebugInfo;
const
  BEVEL_PAD = 20;
var
  S: string;
  linesCnt: Integer;
  textSize: TKMPoint;
begin
  S := GetDebugInfo;

  // Add logs at the end
  if SHOW_LOG_IN_GUI then
  begin
    linesCnt := (Bevel_DebugInfo.Parent.Height
                  - Bevel_DebugInfo.Top
                  - BEVEL_PAD
                  - gRes.Fonts[Label_DebugInfo.Font].GetTextSize(S).Y)
                div gRes.Fonts[Label_DebugInfo.Font].LineHeight;
    S := S + '|Logs:|' + GetLogString(linesCnt - 1);
  end;

  Label_DebugInfo.Caption := S;
  Label_DebugInfo.Visible := (Trim(S) <> '');

  Assert(InRange(DEBUG_TEXT_FONT_ID, Ord(Low(TKMFont)), Ord(High(TKMFont))));
  Label_DebugInfo.Font := TKMFont(DEBUG_TEXT_FONT_ID);

  textSize := gRes.Fonts[Label_DebugInfo.Font].GetTextSize(S);

  Bevel_DebugInfo.Width := IfThen(textSize.X <= 1, 0, textSize.X + BEVEL_PAD);
  Bevel_DebugInfo.Height := IfThen(textSize.Y <= 1, 0, textSize.Y + BEVEL_PAD);

  Bevel_DebugInfo.Visible := SHOW_DEBUG_OVERLAY_BEVEL and (Trim(S) <> '') ;
end;


procedure TKMUserInterfaceGame.ViewportPositionChanged(const aPos: TKMPointF);
begin
  gSoundPlayer.UpdateListener(aPos.X, aPos.Y);
  if gScriptSounds <> nil then
    gScriptSounds.UpdateListener(aPos.X, aPos.Y);
end;


procedure TKMUserInterfaceGame.KeyPress(Key: Char);
begin
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyPress);

  inherited;
end;


procedure TKMUserInterfaceGame.HandleScrollKeysDown(Key: Word; var aHandled: Boolean);
{$IFDEF MSWindows}
var
  windowRect: TRect;
{$ENDIF}
begin
  aHandled := True;

  if gMySpectator.Hand.InCinematic then
  begin
    aHandled := False;
    Exit;
  end;

  //Scrolling
  if Key = gResKeys[kfScrollLeft]       then
    fViewport.ScrollKeyLeft  := True
  else if Key = gResKeys[kfScrollRight] then
    fViewport.ScrollKeyRight := True
  else if Key = gResKeys[kfScrollUp]    then
    fViewport.ScrollKeyUp    := True
  else if Key =  gResKeys[kfScrollDown] then
    fViewport.ScrollKeyDown  := True
  else if Key = gResKeys[kfZoomIn]      then
    fViewport.ZoomKeyIn      := True
  else if Key = gResKeys[kfZoomOut]     then
    fViewport.ZoomKeyOut     := True
  else if Key = gResKeys[kfZoomReset]   then
    fViewport.ResetZoom
  else if (Key = gResKeys[kfMapDragScroll])
      and IsDragScrollingAllowed then
  begin
    fDragScrolling := True;
   // Restrict the cursor to the window, for now.
   //todo: Allow one to drag out of the window, and still capture.
   {$IFDEF MSWindows}
     windowRect := gMain.ClientRect(1); //Reduce ClientRect by 1 pixel, to fix 'jump viewport' bug when dragscrolling over the window border
     ClipCursor(@windowRect);
   {$ENDIF}
   fDragScrollingCursorPos.X := gCursor.Pixel.X;
   fDragScrollingCursorPos.Y := gCursor.Pixel.Y;
   fDragScrollingViewportPos.X := fViewport.Position.X;
   fDragScrollingViewportPos.Y := fViewport.Position.Y;
   gSystem.Cursor := kmcDrag;
  end
  else
    aHandled := False;
end;


// This event happens every ~33ms if the Key is Down and holded
procedure TKMUserInterfaceGame.KeyDown(Key: Word; Shift: TShiftState; aIsFirst: Boolean; var aHandled: Boolean);
begin
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyDown);

  if aHandled then Exit;

  inherited;

  // Update game options in case we used sounds hotkeys
  if aHandled then
  begin
    OptionsChanged;
    Exit;
  end;

  HandleScrollKeysDown(Key, aHandled);
end;


procedure TKMUserInterfaceGame.HandleScrollKeysUp(Key: Word; var aHandled: Boolean);
begin
  aHandled := True;

  if gMySpectator.Hand.InCinematic then
  begin
    aHandled := False;
    Exit;
  end;

  //Scrolling
  if Key = gResKeys[kfScrollLeft]       then
    fViewport.ScrollKeyLeft := False
  else if Key = gResKeys[kfScrollRight] then
    fViewport.ScrollKeyRight := False
  else if Key = gResKeys[kfScrollUp]    then
    fViewport.ScrollKeyUp := False
  else if Key =  gResKeys[kfScrollDown] then
    fViewport.ScrollKeyDown  := False
  else if Key = gResKeys[kfZoomIn]      then
    fViewport.ZoomKeyIn := False
  else if Key = gResKeys[kfZoomOut]     then
    fViewport.ZoomKeyOut := False
  else if Key = gResKeys[kfZoomReset]   then
    fViewport.ResetZoom
  else if Key = gResKeys[kfMapDragScroll] then
  begin
    if fDragScrolling then
      ResetDragScrolling;
  end
  else
    aHandled := False;
end;


procedure TKMUserInterfaceGame.KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyUp);

  if aHandled then Exit;

  inherited;

  // Update game options in case we used sounds hotkeys
  if aHandled then
  begin
    OptionsChanged;
    Exit;
  end;

  HandleScrollKeysUp(Key, aHandled);
end;


procedure TKMUserInterfaceGame.ResetDragScrolling;
begin
  fDragScrolling := False;
  gSystem.Cursor := kmcDefault; //Reset cursor
  gMain.ApplyCursorRestriction;
end;


procedure TKMUserInterfaceGame.InitDebugControls;
begin
  // Debugging displays
  Bevel_DebugInfo := TKMBevel.Create(Panel_Main, ToolbarWidth + 8 - 10, 133 - 10, Panel_Main.Width - ToolbarWidth - 8, 0);
  Bevel_DebugInfo.BackAlpha := 0.5;
  Bevel_DebugInfo.Hitable := False;
  Bevel_DebugInfo.Hide;
  Label_DebugInfo := TKMLabel.Create(Panel_Main, ToolbarWidth + 8, 133, '', fntMonospaced, taLeft);
  Label_DebugInfo.Hide;
end;


function TKMUserInterfaceGame.IsDragScrollingAllowed: Boolean;
begin
  Result := True; // Allow drag scrolling by default
end;


procedure TKMUserInterfaceGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseUp);
end;


procedure TKMUserInterfaceGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;

  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseDown);
end;


procedure TKMUserInterfaceGame.MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean);
var
  VP: TKMPointF;
begin
  inherited;

  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseMove);

  aHandled := False;
  if fDragScrolling then
  begin
    if GetKeyState(gResKeys[kfMapDragScroll]) < 0 then
    begin
      UpdateGameCursor(X, Y, Shift);
      VP.X := fDragScrollingViewportPos.X + (fDragScrollingCursorPos.X - X) / (CELL_SIZE_PX * fViewport.Zoom);
      VP.Y := fDragScrollingViewportPos.Y + (fDragScrollingCursorPos.Y - Y) / (CELL_SIZE_PX * fViewport.Zoom);
      fViewport.Position := VP;
      aHandled := True;
    end else
      ResetDragScrolling;
  end;
end;


function TKMUserInterfaceGame.ZoomChangeBlocked: Boolean;
begin
  Result := False;
end;


procedure TKMUserInterfaceGame.MouseWheel(Shift: TShiftState; WheelSteps, X, Y: Integer; var aHandled: Boolean);
var
  prevCursor: TKMPointF;
begin
  inherited;

  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseWheel);
  //first check if we hit minimap  : MinimapView := TKMMinimapView.Create(fMinimap, Panel_Main, 10, 10, 176, 176);
  if  not (InRange(X, 10, 186) and InRange(Y, 10, 186)) then
    if (X < GetToolbarWidth) or (Y < 0) then Exit; // This happens when you use the mouse wheel on the window frame

  // Allow to zoom only when cursor is over map. Controls handle zoom on their own
  if aHandled or ZoomChangeBlocked then Exit;

  UpdateGameCursor(X, Y, Shift); // Make sure we have the correct cursor position to begin with
    prevCursor := gCursor.Float;
  // +1 for ScrollSpeed = 0.
  // Sqrt to reduce Scroll speed importance
  // 11 = 10 + 1, 10 is default scroll speed
  fViewport.Zoom := fViewport.Zoom * (1 + WheelSteps * Sqrt((gGameSettings.ScrollSpeed + 1) / 11) / 12);
  UpdateGameCursor(X, Y, Shift); // Zooming changes the cursor position
  // Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
  if not (InRange(X, 10, 186) and InRange(Y, 10, 186)) then
  fViewport.Position := KMPointF(fViewport.Position.X + prevCursor.X-gCursor.Float.X,
                                 fViewport.Position.Y + prevCursor.Y-gCursor.Float.Y);
  UpdateGameCursor(X, Y, Shift); // Recentering the map changes the cursor position
  aHandled := True;
end;


procedure TKMUserInterfaceGame.PaintDefences;
var
  I, K: Integer;
  DP: TAIDefencePosition;
  DD: TAIDefendPosition;
  locF: TKMPointF;
  screenLoc: TKMPoint;
begin
  for I := 0 to gHands.Count - 1 do
  begin
    for K := 0 to gHands[I].AI.General.DefencePositions.Count - 1 do
    begin
      DP := gHands[I].AI.General.DefencePositions[K];
      locF := gTerrain.FlatToHeight(KMPointF(DP.Position.Loc.X-0.5, DP.Position.Loc.Y-0.5));
      screenLoc := fViewport.MapToScreen(locF);

      if KMInRect(screenLoc, fViewport.ViewRect) then
      begin
        //Dir selector
        TKMRenderUI.WritePicture(screenLoc.X, screenLoc.Y, 0, 0, [], rxGui,  510 + Byte(DP.Position.Dir));
        TKMRenderUI.WriteTextInShape(IntToStr(K+1), screenLoc.X, screenLoc.Y - 28, DEFENCE_LINE_TYPE_COL[DP.DefenceType],
                                     FlagColorToTextColor(GROUP_TXT_COLOR[DP.GroupType]), $80000000, IntToStr(I + 1), gHands[I].FlagColor, icWhite);
        //GroupType icon
        TKMRenderUI.WritePicture(screenLoc.X, screenLoc.Y, 0, 0, [], rxGui, GROUP_IMG[DP.GroupType]);

      end;
    end;

    for K := 0 to gHands[I].AI.General.DefenDPositions.Count - 1 do
    begin
      DD := gHands[I].AI.General.DefenDPositions[K];
      locF := gTerrain.FlatToHeight(KMPointF(DD.Position.X-0.5, DD.Position.Y-0.5));
      screenLoc := fViewport.MapToScreen(locF);

      if KMInRect(screenLoc, fViewport.ViewRect) then
        TKMRenderUI.WritePicture(screenLoc.X, screenLoc.Y, 0, 0, [], rxGui,  955);
    end;
  end;
end;


procedure TKMUserInterfaceGame.Paint;
begin
  if (mlDefencesAll in gGameParams.VisibleLayers) then
    fPaintDefences := True;

  if fPaintDefences then
    PaintDefences;

  fPaintDefences := False;

  inherited;
end;


procedure TKMUserInterfaceGame.GameSpeedChanged(aFromSpeed, aToSpeed: Single);
begin
  fViewport.GameSpeedChanged(aFromSpeed, aToSpeed);
end;


procedure TKMUserInterfaceGame.SyncUI(aMoveViewport: Boolean = True);
begin
  fMinimap.LoadFromTerrain;
  fMinimap.Update;

  gTerrain.OnTopHillChanged := fViewport.TopHillChanged;

  if aMoveViewport then
  begin
    fViewport.ResizeMap(gTerrain.MapX, gTerrain.MapY, gTerrain.TopHill / CELL_SIZE_PX);
    fViewport.ResetZoom;
  end;

  UpdateHotkeys;
end;


procedure TKMUserInterfaceGame.SyncUIView(const aCenter: TKMPointF);
begin
  fViewport.Zoom := gGameSettings.DefaultZoom; // Set Zoom first, since it can apply restrictions on Position near map borders
  fViewport.Position := aCenter;
end;


procedure TKMUserInterfaceGame.SyncUIView(const aCenter: TKMPointF; aZoom: Single);
begin
  fViewport.Zoom := aZoom; // Set Zoom first, since it can apply restrictions on Position near map borders
  fViewport.Position := aCenter;
end;


function TKMUserInterfaceGame.CursorToMapCoord(X, Y: Integer): TKMPointF;
begin
  Result.X := fViewport.Position.X + (X-fViewport.ViewRect.Right/2-GetToolbarWidth/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := fViewport.Position.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := gTerrain.ConvertCursorToMapCoord(Result.X, Result.Y);
end;


// Compute cursor position and store it in global variables
procedure TKMUserInterfaceGame.UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
begin
  UpdateCursor(X, Y, Shift);

  with gCursor do
  begin
    Float := CursorToMapCoord(X, Y);

    PrevCell := Cell; //Save previous cell

    // Cursor cannot reach row MapY or column MapX, they're not part of the map (only used for vertex height)
    Cell.X := EnsureRange(Round(Float.X+0.5), 1, gTerrain.MapX-1); // Cell below cursor in map bounds
    Cell.Y := EnsureRange(Round(Float.Y+0.5), 1, gTerrain.MapY-1);

    ObjectUID := gRenderPool.RenderList.GetSelectionUID(Float);
  end;
end;


procedure TKMUserInterfaceGame.UpdateState(aGlobalTickCount: Cardinal);
begin
  inherited;

  ShowDebugInfo;
end;


end.
