unit KM_GUIGameHouse;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Controls, KM_ControlsBase, KM_ControlsProgressBar, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Pics, KM_ControlsPopUp, KM_ControlsEdit, KM_ControlsScroll,
  KM_GUIGameHouseCartographer, KM_GuiGameHousePearl, KM_GuiGameHousePasture, KM_GuiGameHouseForest,
  KM_GuiGameHouseArena, KM_GUIGameHouseST,
  KM_InterfaceGame, KM_Houses, KM_HouseMarket, KM_HouseQueue, KM_ResWares, KM_ResTypes;

const LINE_HEIGHT = 25; //Each new Line is placed ## pixels after previous

type
  TKMGUIGameHouse = class
  private
    fHouse: TKMHouse;
    fAnimStep: Cardinal;
    fLastWareOutput : Byte;
    fLastSiegeUnit : Byte;
    fLastSchoolUnit: Byte;  //Last unit that was selected in School, global for all schools player owns
    fLastBarracksUnit: Byte; //Last unit that was selected in Barracks, global for all barracks player owns
    fLastTHUnit: Byte; //Last unit that was selected in Townhall, global for all townhalls player owns
    fLastPalaceUnit: Byte;

    fSetViewportEvent: TPointFEvent;
    fSelectNextHouse : TBooleanEvent;
    fStoreWareOrder : array of array of TKMWareType;
    function HasAnyUnit(unitArr : array of TKMUnitType) : Boolean; overload;
    function HasAnyUnit(unitArr : array of TKMUnitType; OnlyVisible : Boolean) : Boolean;overload;
    function NextUnit(startFrom : Byte; unitArr : array of TKMUnitType; toLast : Boolean = false) : Integer;
    function PreviousUnit(startFrom : Byte; unitArr : array of TKMUnitType; toLast : Boolean = false) : Integer;
    procedure CheckLastSelected(var A : Byte; unitArr : array of TKMUnitType);
    procedure Create_HouseTownhall;
    procedure Create_HouseBarracks;
    procedure Create_HouseMarket;
    procedure Create_HouseSchool;
    procedure Create_HouseSiege;
    procedure Create_HouseStore;
    procedure Create_HouseWoodcutter;
    procedure Create_HouseSign(aParent : TKMPanel);
    procedure Create_HouseMerchant;
    Procedure Create_HouseWoodBurner;
    procedure Create_HouseCottage; 
    procedure Create_HousePalace;
    procedure Create_HouseStall;
    Procedure ShowSignMSG;
    procedure House_Demolish(Sender: TObject; Shift: TShiftState);
    procedure House_RepairToggle(Sender: TObject);
    procedure HouseSignClose(Sender: TObject);
    procedure House_OrderChange(Sender: TObject; aValue: Integer);
    procedure House_DeliveryModeToggle(Sender: TObject; Shift: TShiftState);
    procedure House_ClosedForWorkerToggle(Sender: TObject; Shift: TShiftState);
    procedure HandleHouseClosedForWorker(aHouse: TKMHouse);

    procedure HouseLogo_Click(Sender: TObject; Shift: TShiftState);

    procedure House_BarracksItemClickShift(Sender: TObject; Shift: TShiftState);
    procedure House_BarracksUnitChange(Sender: TObject; Shift: TShiftState);

    procedure House_TH_UnitChange(Sender: TObject; Shift: TShiftState);

    procedure House_MarketFill(aMarket: TKMHouseMarket);
    procedure House_MarketOrderClick(Sender: TObject; Shift: TShiftState);
    procedure House_MarketSelect(Sender: TObject; Shift: TShiftState);

    procedure House_SchoolUnitChange(Sender: TObject; Shift: TShiftState);
    procedure House_SchoolUnitQueueClick(Sender: TObject; Shift: TShiftState);
    //procedure House_SchoolIconClicked(aValue : Integer; Shift : TShiftState);

    procedure House_StoreItemClickShift(Sender: TObject;X, Y : Integer; Shift: TShiftState);
    procedure House_StoreItemMouseOver(Sender: TObject; Shift: TShiftState);
    procedure House_StoreFill;

    procedure House_WoodcutterClick(Sender: TObject; Shift: TShiftState);
    procedure House_WoodcutterChange(Sender: TObject);

    procedure House_FarmClick(Sender: TObject; Shift: TShiftState);
    procedure House_FarmChange(Sender: TObject);

    procedure ShowCommonDemand(aHouse: TKMHouse; Base: Integer; var Line: Integer; var RowRes: Integer);
    procedure ShowCommonOutput(aHouse: TKMHouse; Base: Integer; var Line: Integer; var RowRes: Integer);
    procedure ShowCommonOrders(aHouse: TKMHouse; Base: Integer; var Line: Integer; var RowRes: Integer);
    procedure ShowCommonCost(aHouse: TKMHouse; Base: Integer; var Line: Integer; var RowRes: Integer);
    procedure ShowCommonDemandSingle(aHouse: TKMHouse; aID : TByteSet; Base: Integer; var Line, RowRes: Integer);
    procedure ShowTownHall(aHouse: TKMHouse);
    function GetEquipAmount(Shift: TShiftState): Integer;
    procedure ToggleHouseAcceptWare(Sender: TObject; Shift: TShiftState);
    procedure ToggleMerchantWareInput(Sender : TObject);
    Procedure House_MerchantPlayer(Sender : TObject);
    procedure House_WoodBurnerChange;
    procedure House_Queue_ChangeWare(Sender : TObject);
    procedure House_Queue_Click(Sender : TObject; Shift: TShiftState);
    procedure House_Palace_Click(Sender : TObject; Shift: TShiftState);
    procedure House_UpgradeClick(Sender : TObject);
    procedure House_SmallStoreTransfer(Sender : TObject);
    procedure House_PalaceRefresh(aHouse : TKMHouse);
    procedure House_StallClick(Sender: TObject; Shift: TShiftState);
    procedure House_NotAcceptWorkersClick(aValue : Integer);
    procedure House_FlagClicked(Sender : TObject);
    procedure House_FarmToggleGrain(Sender : TObject; Shift: TShiftState);
    procedure House_FruitTreeToggle(Sender : TObject; Shift: TShiftState);
    procedure Store_BellClick(Sender : TObject);
    procedure Ship_Clicked(Sender: TObject; Shift : TShiftState);
    procedure CollectorsClicked(Sender: TObject; Shift : TShiftState);
    procedure SetHouseStyleClicked(Sender: TObject; Shift : TShiftState);
  protected
    Panel_House: TKMScrollPanel;
      Label_House: TKMLabel;
      Button_HouseDeliveryMode: TKMButton;
      Button_HouseRepair: TKMButton;
      Image_House_Logo: TKMImage;
      Image_House_Worker: TKMImage;
      Image_House_Worker_Closed: TKMImage;
      Button_House_Worker: TKMButton;
      HealthBar_House: TKMPercentBar;
      Button_UpgradeHouse : TKMButton;
      Button_ForceWork : TKMButton;
      Image_WorkProgress : TKMImageAnimation;
      Icons_Workers: TKMIconsRow;
      Button_Bell : TKMButton;

    Panel_House_Common: TKMPanel;
      Image_PlayerFlag: TKMImage;
      Label_Common_Demand,Label_Common_Offer,Label_Common_Costs,
      Label_House_UnderConstruction,Label_House_Demolish: TKMLabel;
      Image_HouseConstructionWood, Image_HouseConstructionStone, Image_HouseConstructionTile: TKMImage;
      Label_HouseConstructionWood, Label_HouseConstructionStone, Label_HouseConstructionTile: TKMLabel;
      Button_SetHouseStyle : TKMButton;

      Button_House_DemolishYes,Button_House_DemolishNo: TKMButton;
      WaresProdCt_Common: array [1..WARES_IN_OUT_COUNT * 2] of TKMLabel;
      WaresRow_Common: array [1..WARES_IN_OUT_COUNT * 2] of TKMWaresRow;
      WaresRow_Max: array [1..WARES_IN_OUT_COUNT] of TKMNumericEdit;
      Image_WareIn_Accept: array [1..WARES_IN_OUT_COUNT * 2] of TKMWaresBlockRow;
      Button_TransferWare: array[1..WARES_IN_OUT_COUNT * 2] of TKMButton;

      WareOrderRow_Order: array [1..WARES_IN_OUT_COUNT] of TKMWareOrderRow; //4 bars is the maximum
      CostsRow_Costs: array [1..WARES_IN_OUT_COUNT] of TKMCostsRow; //3 bars is the maximum
      Label_DepletedMsg: TKMLabel;
      VirtualWares_Row: array [1..9] of TKMWaresRow; //Virtual wares show max 9 wares
      ProgressBar_BigWare: TKMImageBar;
      Progress_BigWare, Progress_BigWare2 : TKMImageAnimation;
      Progress_Beasts : TKMIconProgressBar;
      CostsRow_Common : TKMCostsRowMulti;

    Panel_HouseMarket: TKMPanel;
      Button_Market: array of TKMButtonFlat;
      Label_Market_In, Label_Market_Out: TKMLabel;
      Button_Market_In, Button_Market_Out: TKMButtonFlat;
      Button_Market_Add,Button_Market_Remove: TKMButton;
      Label_Market_FromAmount,Label_Market_ToAmount: TKMLabel;

    Panel_HouseStore: TKMPanel;
      Image_TotalCount : TKMImage;
      Bar_TotalCount   : TKMPercentBar;
      Panel_StoreWares : TKMPanel;
      Button_Store: array of TKMButtonFlat;
      Image_Store_NotAccept: array of TKMImage;
      Image_Store_NotAllowTakeOut: array of TKMImage;
      Label_Wares : array of TKMLabel;
      CheckBox_Store : TKMCheckBoxTex;

    Panel_House_School: TKMPanel;
      //WaresRow_School_Gold, WaresRow_School_Boots: TKMWaresRow;
      Button_Workless: TKMButtonFlat;
      Button_School_UnitWIP: TKMButton;
      Button_School_UnitWIPBar: TKMPercentBar;
      Button_School_UnitPlan: array [1..5] of TKMButtonFlat;
      Label_School_Unit: TKMLabel;
      Image_School_Right,Image_School_Train,Image_School_Left: TKMImage;
      Button_School_Right,Button_School_Train,Button_School_Left: TKMButton;
      //Icons_AllWorkers: TKMIconsRow;

    Panel_House_Siege: TKMPanel;
      Button_Siege_UnitWIP: TKMButton;
      Button_Siege_UnitWIPBar: TKMPercentBar;
      Button_Siege_UnitPlan: array [1..5] of TKMButtonFlat;
      Label_Siege_Unit: TKMLabel;
      Image_Siege_Right,Image_Siege_Train,Image_Siege_Left: TKMImage;
      Button_Siege_Right,Button_Siege_Train,Button_Siege_Left: TKMButton;
      Button_OperatorsCount : TKMButtonFlat;
      
    Panel_House_Palace: TKMPanel;
      Button_PalaceLeft, Button_PalaceRight, Button_PalaceTrain : TKMButton;
      Button_PalaceVWaresCost,
      Button_PalaceVWares: array of TKMButtonFlat;
      Button_Palace_PreviousUnit,
      Button_Palace_NextUnit,
      Button_Palace_UnitPlan: TKMButtonFlat;
      Image_CancelUnit, Image_OrderCount: TKMButtonFlat;
      Bar_Palace_ProgressLeft,
      Bar_Palace_ProgressRight : TKMPercentBar;
      Image_Ornament : TKMimage;
      Label_Palace_Unit : TKMLabel;

    Panel_HouseQueue: TKMPanel;
      Bar_QueueProgress : TKMShape;
      Button_Queue_WarePlan: array [0..QUEUE_LENGTH - 1] of TKMButtonFlat;
      Button_Queue_Wares: array [0..3] of TKMButtonFlat;
      Button_Queue_Right,Button_Queue_Left : TKMButton;
      CheckBox_NotRem : TKMCheckBox;

    Panel_HouseCottage: TKMPanel;
      Button_CottageWorkless : TKMButtonFlat;
      //Bar_FamilyProgress: TKMPercentBar;
      //Bar_KidsProgress: array[0..5] of TKMPercentBar;
      Button_FamilyQty: TKMButtonFlat;
      Button_KidsQty : array[0..2] of TKMButtonFlat;
      Image_FamilyProgress : array[1..5] of TKMImage;
      Image_KidsProgress: array[0..2] of array[1..8] of TKMImage;

    Panel_HouseTownHall: TKMPanel;
      Label_TH_Costs: TKMLabel;
      Label_TH_Unit: TKMLabel;
      Image_TH_Right,Image_TH_Train,Image_TH_Left: TKMImage;
      Button_TH_Right,Button_TH_Train,Button_TH_Left: TKMButton;
      CostsRow_TH_Cost: TKMCostsRow;

    Panel_HouseBarracks: TKMPanel;
      Button_Barracks: array of TKMButtonFlat;
      Image_Barracks_NotAccept: array of TKMImage;
      Image_Barracks_NotAllowTakeOut: array of TKMImage;
      Button_BarracksRecruit: TKMButtonFlat;
      Image_Barracks_NotAcceptRecruit: TKMImage;
      Label_Barracks_Unit: TKMLabel;
      Image_Barracks_Right, Image_Barracks_Train, Image_Barracks_Left: TKMImage;
      Button_Barracks_Right, Button_Barracks_Train, Button_Barracks_Left: TKMButton;

    Panel_HouseWoodcutter: TKMPanel;
      Radio_Woodcutter: TKMRadioGroup;
      Button_Woodcutter: TKMButtonFlat;

    Panel_HouseFarm: TKMPanel;
      Radio_Farm: TKMRadioGroup;
      Button_Farm: TKMButtonFlat;

    Panel_HouseSign : TKMPopUpMenu;
      Label_HouseSignTop,
      Label_HouseSign : TKMlabel;
      Image_HouseSign : TKMImage;
      Bevel_HouseSign : TKMBevel;
      Bevel_HouseSignBehind : TKMBevel;
      Image_HouseSignClose: TKMImage;

    Panel_HouseWoodBurner: TKMPanel;
      Fuel_Bar : TKMPercentBar;
      Wood_Bar : array[1..5] of TKMPercentBar;
      Coal_Image : array[1..5] of TKMCostsRow;

    Panel_House_Stall: TKMExpandPanelCollection;
        Button_VWares:array[0..5] of TKMButtonFlat;
        Label_VWaresRArrow: TKMLabel;
        Button_NotAcceptWares,
        Button_Wares :array[1..WARES_IN_OUT_COUNT] of TKMButtonFlat;//default wares
        Button_Coin, Button_CoinV, Button_ResToV: TKMButtonFlat;

    Button_ToggleFruitType,
    Button_ToggleGrainType, Button_ToggleGrassType,
    Button_ToggleVegeType,
    Button_CollectorsMode: TKMButton;
    Button_MerchantType : array[0..9] of TKMButton;
    Button_PlayerSelect: array [0..MAX_HANDS-1] of TKMFlatButtonShape;//select player in merchant

    Ship_ShipType, Ship_DoWork : TKMButton;
    WaresOut_ShipYard : TKMWaresButtonsMulti;

    Button_ATBoltCount : TKMButtonFlat;

    Pottery_ClayCount : TKMLabel;
    Pottery_ClayTitle : TKMLabel;

    Panel_Cartographers : TKMGuiGameCartographer;
    Panel_Pearl : TKMGuiGamePearl;
    Panel_Pasture : TKMGuiGamePasture;
    Panel_Forest : TKMGuiGameForest;
    Panel_Arena : TKMGuiGameArena;
    Panel_SiegeTower : TKMGuiGameSiegeTower;

  public
    AskDemolish: Boolean;
    OnHouseDemolish: TNotifyEventShift;

    constructor Create(aParent: TKMPanel; aSetViewportEvent: TPointFEvent; aSelectNextEvent : TBooleanEvent);

    procedure Show(aHouse: TKMHouse); overload;
    procedure Show(aHouse: TKMHouse; aAskDemolish: Boolean); overload;
    procedure Hide;
    function Visible: Boolean;

    procedure KeyDown(Key: Word; aShift: TShiftState; var aHandled: Boolean);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateHotkeys;
  end;


implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  KM_Game, KM_GameInputProcess, KM_GameParams,
  KM_GameSettings,
  KM_Hand,
  KM_InterfaceTypes,
  KM_CommonUtils, KM_CommonHelpers,
  KM_HouseBarracks, KM_HouseSchool, KM_HouseTownHall, KM_HouseWoodcutters, KM_HouseStore,
  KM_HouseArmorWorkshop, KM_HouseSiegeWorkshop, KM_HouseWoodBurner, KM_HouseCottage,
  KM_HouseSwineStable, KM_HouseCartographers,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_RenderUI, KM_ResKeys, KM_ResMapElements,
  KM_Resource, KM_ResFonts, KM_ResHouses, KM_ResTexts, KM_ResUnits, KM_Utils, KM_UtilsExt, KM_Points,
  KM_Cursor,
  KM_GameTypes, KM_MainSettings;

const
  MAX_UNITS_TO_EQUIP = 100;
  HOUSE_FLAG_TEX_ID = 1159;
  HOUSE_FLAG_TEX_ID_FRAME = 5;
  HOUSE_ORDER_ROW_MOUSEWHEEL_STEP = 5;

  SCHOOL_CH_ORDER_TO_0_SHIFT = ssShift; // Shift state to change Unit order in queue to 0 in School
  SCHOOL_CH_ORDER_TO_1_SHIFT = ssCtrl;  // Shift state to change Unit order in queue to 1 in School


constructor TKMGUIGameHouse.Create(aParent: TKMPanel; aSetViewportEvent: TPointFEvent; aSelectNextEvent : TBooleanEvent);
var
  I: Integer;
begin
  inherited Create;
  fLastWareOutput := 1;
  fSetViewportEvent := aSetViewportEvent;
  self.fSelectNextHouse := aSelectNextEvent;
  fAnimStep := 0;
  Panel_House := TKMScrollPanel.Create(aParent, TB_PAD, 44, TB_WIDTH + 9, aParent.Height - 50, [saVertical], bsMenu, ssCommon);
  Panel_House.AnchorsStretch;
  Panel_House.DoScrollUpdate := false;
    //Thats common things

    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    Image_PlayerFlag := TKMImage.Create(Panel_House, 0, 17, 20, 13, 1159, rxHouses); // before house name label
    Image_PlayerFlag.OnClick := House_FlagClicked;
    Image_PlayerFlag.HighlightOnMouseOver := true;

    Label_House := TKMLabel.Create(Panel_House, 0, 0, TB_WIDTH, 0, '', fntOutline, taCenter);
    Button_HouseDeliveryMode := TKMButton.Create(Panel_House,0,42,30,30,37, rxGui, bsGame);
    Button_HouseDeliveryMode.Hint := gResTexts[TX_HOUSE_TOGGLE_DELIVERS_HINT];
    Button_HouseDeliveryMode.OnClickShift := House_DeliveryModeToggle;
    Button_HouseRepair := TKMButton.Create(Panel_House,30,42,30,30,40, rxGui, bsGame);
    Button_HouseRepair.Hint := gResTexts[TX_HOUSE_TOGGLE_REPAIR_HINT];
    Button_HouseRepair.OnClick := House_RepairToggle;
    Image_WorkProgress := TKMImageAnimation.Create(Panel_House, Panel_House.Width - 35, 70, 20, 20, 0, rxGui, [anTop, anRight]);
    Image_WorkProgress.Animation.Create([811, 838, 805, 839], 3);
    Image_House_Worker := TKMImage.Create(Panel_House,60,41,32,32,141);
    Image_House_Worker.ImageCenter;

    Button_House_Worker := TKMButton.Create(Panel_House,60,42,30,30,141, rxGui, bsGame);
    Button_House_Worker.OnClickShift := House_ClosedForWorkerToggle; //Clicking the button cycles it

    Button_Bell := TKMButton.Create(Panel_House,60,42,30,30,883, rxGui, bsGame);
    Button_Bell.OnClick := Store_BellClick;
    Button_Bell.Hint := gResTexts[2011];
    //Button_Bell.MobilHint := true;

    Image_House_Worker_Closed := TKMImage.Create(Panel_House,78,42,12,12,49); //Red triangle for house closed for worker
    Image_House_Worker_Closed.Hitable := False;
    Image_House_Worker_Closed.Hide;

    Button_Workless := TKMButtonFlat.Create(Panel_House, 62, 41, 28, 38, 808);
    Button_Workless.TexOffsetX := 1;
    Button_Workless.TexOffsetY := 1;
    Button_Workless.CapOffsetY := 2;
    Button_Workless.Hint := gResTexts[1682];

    Image_House_Logo := TKMImage.Create(Panel_House,93,41,32,32,338);
    Image_House_Logo.ImageCenter;
    Image_House_Logo.HighlightOnMouseOver := True;
    Image_House_Logo.OnClickShift := HouseLogo_Click;
    Image_House_Logo.Hint := gResTexts[TX_HOUSE_LOGO_HINT];
    HealthBar_House := TKMPercentBar.Create(Panel_House,126,50,55,15);
    Label_House_UnderConstruction := TKMLabel.Create(Panel_House,0,110,TB_WIDTH,0,gResTexts[TX_HOUSE_UNDER_CONSTRUCTION],fntGrey,taCenter);

    Button_UpgradeHouse := TKMButton.Create(Panel_House, TB_WIDTH - 30, 14, 25, 25, 716, rxGui, bsGame);
    Button_UpgradeHouse.OnClick := House_UpgradeClick;

    Button_ForceWork := TKMButton.Create(Panel_House, 0, 42 + 30, 30, 25, 716, rxGui, bsGame);
    Button_ForceWork.OnClick := House_UpgradeClick;
    Button_ForceWork.MobilHint := true;


    Button_ToggleGrainType := TKMButton.Create(Panel_House, 0, 42 + 60, 30, 25, GRAIN_GUI_PIC[gftWheat], rxGui, bsGame );
    Button_ToggleGrainType.OnClickShift := House_FarmToggleGrain;
    Button_ToggleGrainType.MobilHint := true;
    Button_ToggleGrainType.Tag := 0;

    Button_ToggleGrassType := TKMButton.Create(Panel_House, Button_ToggleGrainType.Right, 42 + 60, 30, 25, GRAIN_GUI_PIC[gftWheat], rxGui, bsGame );
    Button_ToggleGrassType.OnClickShift := House_FarmToggleGrain;
    Button_ToggleGrassType.MobilHint := true;
    Button_ToggleGrassType.Tag := 1;

    Button_ToggleVegeType := TKMButton.Create(Panel_House, Button_ToggleGrassType.Right, 42 + 60, 30, 25, GRAIN_GUI_PIC[gftWheat], rxGui, bsGame );
    Button_ToggleVegeType.OnClickShift := House_FarmToggleGrain;
    Button_ToggleVegeType.MobilHint := true;
    Button_ToggleVegeType.Tag := 2;

    Button_ToggleFruitType := TKMButton.Create(Panel_House, 0, 42 + 60, 30, 25, GRAIN_GUI_PIC[gftWheat], rxGui, bsGame );
    Button_ToggleFruitType.OnClickShift := House_FruitTreeToggle;
    Button_ToggleFruitType.MobilHint := true;
    Button_ToggleFruitType.Tag := 0;

    Button_CollectorsMode := TKMButton.Create(Panel_House, 0, 42 + 30, 30, 25, 0, rxGui, bsGame );
    Button_CollectorsMode.OnClickShift := CollectorsClicked;
    Button_CollectorsMode.MobilHint := true;
    Button_CollectorsMode.Tag := 0;

    Ship_ShipType := TKMButton.Create(Panel_House, TB_WIDTH - 100, 42 + 30, 30, 25, gRes.Units[utBoat].GUIIcon, rxGui, bsGame );
    Ship_ShipType.MobilHint := true;
    Ship_ShipType.OnClickShift := Ship_Clicked;

    Ship_DoWork := TKMButton.Create(Panel_House, TB_WIDTH - 70, 42 + 30, 30, 25, 32, rxGuiMain, bsGame );
    Ship_DoWork.MobilHint := true;
    Ship_DoWork.OnClickShift := Ship_Clicked;

    Image_HouseConstructionWood  := TKMImage.Create(Panel_House,20,170,40,40,655);
    Image_HouseConstructionWood.ImageCenter;
    Image_HouseConstructionStone := TKMImage.Create(Panel_House,70,170,40,40,654);
    Image_HouseConstructionStone.ImageCenter;
    Image_HouseConstructionTile := TKMImage.Create(Panel_House,120,170,40,40,709);
    Image_HouseConstructionTile.ImageCenter;

    Label_HouseConstructionWood  := TKMLabel.Create(Panel_House,40,210,gRes.Wares[wtTimber].Title,fntGrey,taCenter);
    Label_HouseConstructionStone := TKMLabel.Create(Panel_House,90,210,gRes.Wares[wtStone].Title,fntGrey,taCenter);
    Label_HouseConstructionTile := TKMLabel.Create(Panel_House,140,210,gRes.Wares[wtStone].Title,fntGrey,taCenter);

    Button_SetHouseStyle := TKMButton.Create(Panel_House,71,230,40,30,389, rxGui,bsGame);
    Button_SetHouseStyle.Hint := gResTexts[1814];
    Button_SetHouseStyle.OnClickShift := SetHouseStyleClicked;

    Label_House_Demolish := TKMLabel.Create(Panel_House,0,130,TB_WIDTH,0,gResTexts[TX_HOUSE_DEMOLISH],fntGrey,taCenter);
    Label_House_Demolish.WordWrap := True;
    Button_House_DemolishYes := TKMButton.Create(Panel_House,0,185,TB_WIDTH,30,gResTexts[TX_HOUSE_DEMOLISH_YES],bsGame);
    Button_House_DemolishNo  := TKMButton.Create(Panel_House,0,220,TB_WIDTH,30,gResTexts[TX_HOUSE_DEMOLISH_NO],bsGame);
    Button_House_DemolishYes.Hint := gResTexts[TX_HOUSE_DEMOLISH_YES_HINT];
    Button_House_DemolishNo.Hint  := gResTexts[TX_HOUSE_DEMOLISH_NO];
    Button_House_DemolishYes.OnClickShift := House_Demolish;
    Button_House_DemolishNo.OnClickShift  := House_Demolish;

    Icons_Workers := TKMIconsRow.Create(Panel_House, Button_House_Worker.Right, Button_House_Worker.Top, 21, 30);
    Icons_Workers.OnIconClicked := House_NotAcceptWorkersClick;
    Icons_Workers.MaxCountInRow := 5;
    Icons_Workers.StillSize := true;
    Icons_Workers.Width := 5 * Icons_Workers.HSpacing;
    Icons_Workers.Height := 1 * Icons_Workers.VSpacing;



  Create_HouseMarket;
  Create_HouseStore;
  Create_HouseSchool;
  Create_HouseSiege;
  Create_HouseBarracks;
  Create_HouseTownhall;
  Create_HouseWoodcutter;
  Create_HouseSign(aParent);
  Create_HouseMerchant;
  Create_HouseWoodBurner;
  Create_HouseCottage;
  Create_HousePalace;
  Create_HouseStall;


  //Panel_House_Common must be created after because of the siege workshop (machines icons must be above the Button_OperatorsCount button)


    Panel_House_Common := TKMPanel.Create(Panel_House,0,76,200,Panel_House.Height - 80);
    Panel_House_Common.Hitable := false;
      Label_Common_Demand := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,gResTexts[TX_HOUSE_NEEDS],fntGrey,taCenter);
      Label_Common_Demand.Hitable := false;
      Label_Common_Offer  := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,'',fntGrey,taCenter);
      Label_Common_Offer.Hitable := false;
      Label_Common_Costs  := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,gResTexts[TX_HOUSE_WARE_COSTS],fntGrey,taCenter);
      Label_Common_Costs.Hitable := false;
      //They get repositioned on display
      for I := 1 to high(WaresRow_Common) do
      begin
        WaresRow_Common[I] := TKMWaresRow.Create(Panel_House_Common, 0, 0, TB_WIDTH - 20);
        WaresRow_Common[I].RX := rxGui;
        WaresRow_Common[I].Tag := I;
        WaresRow_Common[I].OnClickShift := ToggleHouseAcceptWare;
        WaresRow_Common[I].Hitable := true;
        WaresRow_Common[I].HideHighlight := false;
        WaresRow_Common[I].Clickable := true;
        WaresRow_Common[I].MobilHint := true;
        If I <= WARES_IN_OUT_COUNT then
        begin
          WaresRow_Max[I] := TKMNumericEdit.Create(Panel_House_Common, 0, 0, 0, 120, fntGrey, false);
          WaresRow_Max[I].Hide;
          WaresRow_Max[I].AutoFocusable := false;
          WaresRow_Max[I].Focusable := false;
          WaresRow_Max[I].TextAlign := taCenter;
          WaresRow_Max[I].ButtonInc.OnClickShift := ToggleHouseAcceptWare;
          WaresRow_Max[I].ButtonDec.OnClickShift := ToggleHouseAcceptWare;
        end;
        WaresProdCt_Common[I] := TKMLabel.Create(Panel_House_Common, WaresRow_Common[I].Right + 3, 0, TB_WIDTH - 20, 0, '',fntGrey,taLeft);

        Image_WareIn_Accept[I] := TKMWaresBlockRow.Create(Panel_House_Common, 0, 0, 100, 12);
        Image_WareIn_Accept[I].Count := 5;
        Image_WareIn_Accept[I].Hitable := False;
        Image_WareIn_Accept[I].Spacing :=  WaresRow_Common[I].Spacing;

        Button_TransferWare[I] := TKMButton.Create(Panel_House_Common, WaresRow_Common[I].Right + 3, 0, 20, 20, 716, rxGui, bsGame);
        Button_TransferWare[I].OnClick := House_SmallStoreTransfer;
        Button_TransferWare[I].Hint := 'Transfer Ware'
        //Image_WareIn_Accept[I].Visible := false;
      end;
      //They get repositioned on display
      for I := 1 to WARES_IN_OUT_COUNT do
      begin
        WareOrderRow_Order[I] := TKMWareOrderRow.Create(Panel_House_Common, 0, 0, TB_WIDTH, 999);
        WareOrderRow_Order[I].MouseWheelStep := HOUSE_ORDER_ROW_MOUSEWHEEL_STEP;
        WareOrderRow_Order[I].WareRow.RX := rxGui;
        WareOrderRow_Order[I].OnChange := House_OrderChange;
        WareOrderRow_Order[I].OrderRemHint := gResTexts[TX_HOUSE_ORDER_DEC_HINT];
        WareOrderRow_Order[I].OrderAddHint := gResTexts[TX_HOUSE_ORDER_INC_HINT];
        WareOrderRow_Order[I].WareRow.MobilHint := true;
        CostsRow_Costs[I] := TKMCostsRow.Create(Panel_House_Common, 0, 0, TB_WIDTH, 21);
        CostsRow_Costs[I].RX := rxGui;

      end;
      Label_DepletedMsg := TKMLabel.Create(Panel_House_Common,0,0,TB_WIDTH,0,'',fntGrey,taLeft);
      Label_DepletedMsg.WordWrap := True;
      Label_DepletedMsg.Hide;
  for I := 0 to 8 do
  begin
    VirtualWares_Row[I + 1] := TKMWaresRow.Create(Panel_House, 0, 76 + I * 25, TB_WIDTH);
    VirtualWares_Row[I + 1].RX := rxGui;
    VirtualWares_Row[I + 1].Tag := I;
    VirtualWares_Row[I + 1].Hitable := false;
    VirtualWares_Row[I + 1].WareCntAsNumber := true;
  end;


  ProgressBar_BigWare := TKMImageBar.Create(Panel_House_Common, 0, 0, TB_WIDTH, 40, 928);
  ProgressBar_BigWare.LinesCount := 4;
  Progress_BigWare := TKMImageAnimation.Create(Panel_House_Common, 0, 0, TB_WIDTH, 40, 0, rxGui, []);
  Progress_BigWare.Hitable := false;
  Progress_BigWare.StopAnim := true;
  Progress_BigWare.AddBevel := false;

  Progress_BigWare2 := TKMImageAnimation.Create(Panel_House_Common, 0, 0, TB_WIDTH, 40, 0, rxGui, []);
  Progress_BigWare2.Hitable := false;
  Progress_BigWare2.StopAnim := true;
  Progress_BigWare2.AddBevel := false;


  CostsRow_Common := TKMCostsRowMulti.Create(Panel_House_Common, 0, 0, TB_WIDTH, 21);
  CostsRow_Common.MobilHint := true;
  CostsRow_Common.WarePlan.Reset;
  CostsRow_Common.Caption := gResTexts[2032];

  WaresOut_ShipYard := TKMWaresButtonsMulti.Create(Panel_House_Common, 0, 0, Panel_House.Width - 18, 150);
  WaresOut_ShipYard.WarePlan.Reset;
  WaresOut_ShipYard.MobilHint := true;
  WaresOut_ShipYard.Caption := gResTexts[2120] + ':';


  Pottery_ClayTitle := TKMLabel.Create(Panel_House_Common, 0, 0, Panel_House_Common.Width, 15, gResTexts[2184], fntMetal, taLeft);
  Pottery_ClayCount := TKMLabel.Create(Panel_House_Common, 0, 0, Panel_House_Common.Width, 20, gResTexts[2184], fntGrey, taRight);

  Button_ATBoltCount := TKMButtonFlat.Create(Panel_House_Common, 0, 0, 34, 38, gRes.Wares[wtQuiver].GUIIcon);
  Button_ATBoltCount.Hitable := false;

  Panel_Cartographers := TKMGuiGameCartographer.Create(Panel_House);
  Panel_Pearl := TKMGuiGamePearl.Create(Panel_House);
  Panel_Pasture := TKMGuiGamePasture.Create(Panel_House);
  Panel_Forest := TKMGuiGameForest.Create(Panel_House);
  Panel_Arena := TKMGuiGameArena.Create(Panel_House);
  Panel_SiegeTower := TKMGuiGameSiegeTower.Create(Panel_House);

  Progress_Beasts := TKMIconProgressBar.Create(Panel_House_Common, 0, 0, TB_WIDTH, 30, false, [[490, 569, 403], [490, 569, 403], [490, 569, 403], [490, 569, 403], [490, 569, 403]]);
  Progress_Beasts.RX := rxHouses;
end;


// Market page
procedure TKMGUIGameHouse.Create_HouseMarket;
var
  I: Integer;
  lineH: Integer;

  J, K: Integer;
  top, C: Integer;

begin
  Panel_HouseMarket := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 650);

  lineH := 0;
  Label_Market_In  := TKMLabel.Create(Panel_HouseMarket, 0,lineH,85,0,'',fntGrey,taLeft);
  Label_Market_Out := TKMLabel.Create(Panel_HouseMarket, TB_WIDTH - 85,lineH,85,0,'',fntGrey,taRight);

  Inc(lineH, 20);
  Button_Market_In  := TKMButtonFlat.Create(Panel_HouseMarket,  0, lineH, 36, 40, 0);
  Button_Market_In.HideHighlight := True;
  Button_Market_In.Clickable := False;
  Button_Market_In.Hint := gResTexts[TX_HOUSES_MARKET_SELECT_LEFT];
  Button_Market_Out := TKMButtonFlat.Create(Panel_HouseMarket, TB_WIDTH - 36, lineH, 36, 40, 0);
  Button_Market_Out.HideHighlight := True;
  Button_Market_Out.Clickable := False;
  Button_Market_Out.Hint := gResTexts[TX_HOUSES_MARKET_SELECT_RIGHT];

  with TKMShape.Create(Panel_HouseMarket,  0, lineH, 36, 40) do
  begin
    LineColor := $FF00B000;
    LineWidth := 2;
    Hitable := False;
  end;
  with TKMShape.Create(Panel_HouseMarket, TB_WIDTH - 36, lineH, 36, 40) do
  begin
    LineColor := $FF0000B0;
    LineWidth := 2;
    Hitable := False;
  end;

  Inc(lineH, 10);

  Button_Market_Remove := TKMButton.Create(Panel_HouseMarket, TB_WIDTH div 2 - 20, lineH, 20, 20, '-', bsGame);
  Button_Market_Remove.Hint := gResTexts[TX_HOUSES_MARKET_HINT_REM];
  Button_Market_Remove.OnClickShift := House_MarketOrderClick;

  Button_Market_Add := TKMButton.Create(Panel_HouseMarket, TB_WIDTH div 2, lineH, 20, 20, '+', bsGame);
  Button_Market_Add.Hint := gResTexts[TX_HOUSES_MARKET_HINT_ADD];
  Button_Market_Add.OnClickShift := House_MarketOrderClick;

  Label_Market_FromAmount := TKMLabel.Create(Panel_HouseMarket,  53, lineH, '', fntGrey, taCenter);
  Label_Market_ToAmount   := TKMLabel.Create(Panel_HouseMarket, 127, lineH, '', fntGrey, taCenter);

  J := 0;
  C := 0;
  K := 0;
  top := Button_Market_Out.Bottom + 10;
  SetLength(Button_Market, 0);
  for I := 1 to STORE_RES_COUNT do
  begin
    if StoreResType[I] = wtNone then
    begin
      C := 0;
      if J > 0 then
        top := Button_Market[J - 1].Bottom + 1;
      with TKMLabel.Create(Panel_HouseMarket, 0, top, Panel_HouseMarket.Width, 15, gResTexts[1657 + K], fntOutline, taCenter) do
        Hitable := false;

      Inc(top, 17);
      Inc(K);
      Continue;
    end;
    SetLength(Button_Market, J + 1);

    Button_Market[J] := TKMButtonFlat.Create(Panel_HouseMarket, C mod 5 * 37, C div 5 * 37 + top, 32, 36, 0);

    Button_Market[J].TexOffsetY := 1;
    Button_Market[J].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_Market[J].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_Market[J].Tag := Byte(StoreResType[I]);
    Button_Market[J].OnClickShift := House_MarketSelect;
    Button_Market[J].LineWidth := 2;

    Inc(C);
    Inc(J);
  end;
end;


// Store page
procedure TKMGUIGameHouse.Create_HouseStore;
const BAR_WIDTH = 90;
var
  I, J, K: Integer;
  top, C: Integer;
begin
  Panel_HouseStore := TKMPanel.Create(Panel_House, 0, 73, TB_WIDTH + 25, Panel_House.Height);
  Panel_HouseStore.Hitable := false;
  Image_TotalCount := TKMImage.Create(Panel_HouseStore, 0, -2, 20, 20, 717);
  Bar_TotalCount     := TKMPercentBar.Create(Panel_HouseStore, 20, 0,BAR_WIDTH,15);

  CheckBox_Store := TKMCheckBoxTex.Create(Panel_HouseStore, Bar_TotalCount.Right + 2, 0, 100, 15, gResTexts[1999], fntMetal);
  CheckBox_Store.RX := rxGuiMain;
  CheckBox_Store.TexID := 33;
  //CheckBox_Store.TexID := 91;
  CheckBox_Store.TexID2 := 32;
  CheckBox_Store.Hint := gResTexts[1998];
  CheckBox_Store.Checked := true;


  top := Bar_TotalCount.Bottom + 5;
  C := 0;
  K := 0;
  SetLength(Button_Store, 0);
  SetLength(Image_Store_NotAccept, 0);
  SetLength(Image_Store_NotAllowTakeOut, 0);
  SetLength(Label_Wares, 0);
  SetLength(fStoreWareOrder, 0);
  //convert StoreResType to double array
  J := 0;
  for I := 1 to STORE_RES_COUNT do
  begin
    if StoreResType[I] in [wtNone] then //new array
    begin
      K := length(fStoreWareOrder);
      SetLength(fStoreWareOrder, K + 1);
      J := 0;
    end else
    begin
      SetLength(fStoreWareOrder[K], J + 1);

      fStoreWareOrder[K, J] := StoreResType[I];
      Inc(J);
    end;


  end;


  J := 0;
  K := 0;

  for I := 1 to STORE_RES_COUNT do
  begin
    if StoreResType[I] in [wtNone] then
    begin
      C := 0;
      if J > 0 then
        top := Button_Store[J - 1].Bottom + 1;
      SetLength(Label_Wares, K + 1);
      Label_Wares[K] := TKMLabel.Create(Panel_HouseStore, 0, top, TB_WIDTH, 15, gResTexts[1657 + K], fntOutline, taCenter);
      Label_Wares[K].Hitable := false;

      Inc(top, 17);
      Inc(K);
      Continue;
    end;
    SetLength(Button_Store, J + 1);
    SetLength(Image_Store_NotAccept, J + 1);
    SetLength(Image_Store_NotAllowTakeOut, J + 1);

    Button_Store[J] := TKMButtonFlat.Create(Panel_HouseStore, C mod 5 * 37, top + C div 5 * 37, 32, 36, 0);
    Button_Store[J].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_Store[J].Tag := byte(StoreResType[I]);
    Button_Store[J].Tag2 := BUTTON_STORE_TAG_2;
    Button_Store[J].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_Store[J].OnMouseDown := House_StoreItemClickShift;
    Button_Store[J].OnMouseOver := House_StoreItemMouseOver;

    Image_Store_NotAccept[J] := TKMImage.Create(Panel_HouseStore, Button_Store[J].Left + 20, Button_Store[J].Top, 12, 12, 49);
    Image_Store_NotAccept[J].Hitable := False;
    Image_Store_NotAccept[J].Hint := Format(gResTexts[TX_HOUSE_DELIVERY_PROHIBITED_HINT], [gRes.Wares[StoreResType[I]].Title]);

    Image_Store_NotAllowTakeOut[J] := TKMImage.Create(Panel_HouseStore, Button_Store[J].Left, Button_Store[J].Top, 12, 12, 676);
    Image_Store_NotAllowTakeOut[J].Hitable := False;
    Image_Store_NotAllowTakeOut[J].Hint := Format(gResTexts[TX_HOUSE_TAKEOUT_PROHIBITED_HINT], [gRes.Wares[StoreResType[I]].Title]);

    Inc(C);
    Inc(J);
  end;
  Panel_HouseStore.SetHeightToChilds;
end;


// School page
procedure TKMGUIGameHouse.Create_HouseSchool;
var
  I: Integer;
begin
  Panel_House_School := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, Panel_House.Height);

    //TKMLabel.Create(Panel_House_School,0,2,TB_WIDTH,30,gResTexts[TX_HOUSE_NEEDS],fntGrey,taCenter);

   { WaresRow_School_Gold := TKMWaresRow.Create(Panel_House_School, 0, 21, TB_WIDTH);
    WaresRow_School_Gold.RX := rxGui;
    WaresRow_School_Gold.TexID := gRes.Wares[wtGold].GUIIcon;
    WaresRow_School_Gold.Caption := gRes.Wares[wtGold].Title;
    WaresRow_School_Gold.Hint := gRes.Wares[wtGold].Title;
    WaresRow_School_Gold.Hide;

    WaresRow_School_Boots := TKMWaresRow.Create(Panel_House_School, 0, 42, TB_WIDTH);
    WaresRow_School_Boots.RX := rxGui;
    WaresRow_School_Boots.TexID := gRes.Wares[wtBoots].GUIIcon;
    WaresRow_School_Boots.Caption := gRes.Wares[wtBoots].Title;
    WaresRow_School_Boots.Hint := gRes.Wares[wtBoots].Title;
    WaresRow_School_Boots.Hide;}


    Button_School_UnitWIP := TKMButton.Create(Panel_House_School,  0,0,32,32,0, rxGui, bsGame);
    Button_School_UnitWIP.Hint := gResTexts[TX_HOUSE_SCHOOL_WIP_HINT];
    Button_School_UnitWIP.Tag := 0;
    Button_School_UnitWIP.OnClickShift := House_SchoolUnitQueueClick;
    Button_School_UnitWIPBar := TKMPercentBar.Create(Panel_House_School,34,6,146,20);
    for I := 1 to 5 do
    begin
      Button_School_UnitPlan[i] := TKMButtonFlat.Create(Panel_House_School, (I-1) * 36, 80 - 48, 32, 32, 0);
      Button_School_UnitPlan[i].Tag := I;
      Button_School_UnitPlan[i].OnClickShift := House_SchoolUnitQueueClick;
    end;

    Label_School_Unit := TKMLabel.Create(Panel_House_School,   0,116 - 48,TB_WIDTH,30,'',fntOutline,taCenter);
    Image_School_Left := TKMImage.Create(Panel_House_School,   0,136 - 48,54,80,521);
    Image_School_Train := TKMImage.Create(Panel_House_School, 62,136 - 48,54,80,522);
    Image_School_Right := TKMImage.Create(Panel_House_School,124,136 - 48,54,80,523);
    Image_School_Left.Disable;
    Image_School_Right.Disable;
    Button_School_Left  := TKMButton.Create(Panel_House_School,  0,222 - 48,54,40,35, rxGui, bsGame);
    Button_School_Train := TKMButton.Create(Panel_House_School, 62,222 - 48,54,40,42, rxGui, bsGame);
    Button_School_Right := TKMButton.Create(Panel_House_School,124,222 - 48,54,40,36, rxGui, bsGame);
    Button_School_Left.OnClickShift  := House_SchoolUnitChange;
    Button_School_Train.OnClickShift := House_SchoolUnitChange;
    Button_School_Right.OnClickShift := House_SchoolUnitChange;

    {Icons_AllWorkers := TKMIconsRow.Create(Panel_House_School, 5, Button_School_Right.Bottom + 5, 30, 30);
    Icons_AllWorkers.OnIconClickedShift := House_SchoolIconClicked;
    Icons_AllWorkers.AddBevel := true;
    Icons_AllWorkers.BackBevel := 0.4;
    Icons_AllWorkers.MaxCountInRow := 6;}
end;

procedure TKMGUIGameHouse.Create_HouseSiege;
var
  I, Top: Integer;
begin
  Panel_House_Siege := TKMPanel.Create(Panel_House, 0, 160, TB_WIDTH, 300);
  Panel_House_Siege.Hitable := false;

    Button_Siege_UnitWIP := TKMButton.Create(Panel_House_Siege,  0,48,32,32,0, rxGui, bsGame);
    Button_Siege_UnitWIP.Hint := gResTexts[TX_HOUSE_SCHOOL_WIP_HINT];
    Button_Siege_UnitWIP.Tag := 0;
    Button_Siege_UnitWIP.OnClickShift := House_SchoolUnitQueueClick;

    Button_Siege_UnitWIPBar := TKMPercentBar.Create(Panel_House_Siege,34,54,146,20);
    for I := 1 to 5 do
    begin
      Button_Siege_UnitPlan[i] := TKMButtonFlat.Create(Panel_House_Siege, (I-1) * 36, 80, 32, 32, 0);
      Button_Siege_UnitPlan[i].Tag := I;
      Button_Siege_UnitPlan[i].OnClickShift := House_SchoolUnitQueueClick;
    end;

    Label_Siege_Unit := TKMLabel.Create(Panel_House_Siege,   0,116,TB_WIDTH,30,'',fntOutline,taCenter);
    Image_Siege_Left := TKMImage.Create(Panel_House_Siege,   0,136,54,80,521);
    Image_Siege_Train := TKMImage.Create(Panel_House_Siege, 62,136,54,80,522);
    Image_Siege_Right := TKMImage.Create(Panel_House_Siege,124,136,54,80,523);
    Image_Siege_Left.Disable;
    Image_Siege_Right.Disable;
    Button_Siege_Left  := TKMButton.Create(Panel_House_Siege,  0,252,54,40,35, rxGui, bsGame);
    Button_Siege_Train := TKMButton.Create(Panel_House_Siege, 62,252,54,40,42, rxGui, bsGame);
    Button_Siege_Right := TKMButton.Create(Panel_House_Siege,124,252,54,40,36, rxGui, bsGame);

    Button_Siege_Left.OnClickShift  := House_SchoolUnitChange;
    Button_Siege_Train.OnClickShift := House_SchoolUnitChange;
    Button_Siege_Right.OnClickShift := House_SchoolUnitChange;

    Button_OperatorsCount := TKMButtonFlat.Create(Panel_House_Siege, 0, Button_Siege_Left.Bottom + 5, TB_WIDTH, 36, gRes.Units[utOperator].GUIIcon);
    Button_OperatorsCount.TexOffsetX := -TB_WIDTH div 2 + 15;
    Button_OperatorsCount.CapOffsetX := -TB_WIDTH div 2 + 15;

  Panel_HouseQueue := TKMPanel.Create(Panel_House, 0, 0, TB_WIDTH, 266);
  Panel_HouseQueue.Hitable := false;

    Bar_QueueProgress := TKMShape.Create(Panel_HouseQueue, 0, 80 + 32, 28, 0);
    Bar_QueueProgress.FillColor := icBarColorGreen;
    Bar_QueueProgress.Hitable := false;

    Button_Queue_WarePlan[0] := TKMButtonFlat.Create(Panel_HouseQueue, 0, 80, 28, 32, 0);
    Button_Queue_WarePlan[0].Tag := 0;
    Button_Queue_WarePlan[0].OnClickShift  := House_Queue_Click;
    Button_Queue_WarePlan[0].Down := true;
    Button_Queue_WarePlan[0].LineWidth := 2;
    Button_Queue_WarePlan[0].BackAlpha := 0.2;

    for I := 0 to QUEUE_LENGTH - 2 do
    begin
      Button_Queue_WarePlan[I+1] := TKMButtonFlat.Create(Panel_HouseQueue, 30 + (I mod 5) * 30, 80 + 34 * (I div 5), 28, 32, 0);
      Button_Queue_WarePlan[I+1].Tag := I + 1;
      Button_Queue_WarePlan[I+1].OnClickShift  := House_Queue_Click;
    end;

    Top := Button_Queue_WarePlan[QUEUE_LENGTH - 1].Bottom + 5;


    Button_Queue_Left := TKMButton.Create(Panel_HouseQueue, 0, Top, 32, 32, 2, rxGui, bsGame);
    for I := 0 to 3 do
    begin
      Button_Queue_Wares[I] := TKMButtonFlat.Create(Panel_HouseQueue,35 + I * 28, Top, 26, 32, 0);
      Button_Queue_Wares[I].Tag := I + 100;
      Button_Queue_Wares[I].OnClickShift  := House_Queue_Click;
      case I of
        0 : Button_Queue_Wares[I].Caption := '1';
        1 : Button_Queue_Wares[I].Caption := '2';
        2 : Button_Queue_Wares[I].Caption := '3';
        3 : Button_Queue_Wares[I].Caption := '5';
      end;
    end;

    Button_Queue_Right := TKMButton.Create(Panel_HouseQueue, TB_WIDTH - 32, Top, 32, 32, 3, rxGui, bsGame);

    Button_Queue_Left.OnClick  := House_Queue_ChangeWare;
    Button_Queue_Right.OnClick := House_Queue_ChangeWare;

    CheckBox_NotRem := TKMCheckBox.Create(Panel_HouseQueue, 0, Button_Queue_Left.Bottom + 3, TB_WIDTH, 20, gResTexts[2318], fntMetal);
    CheckBox_NotRem.Hint := gResTexts[2317];
    CheckBox_NotRem.OnClickShift := House_Queue_Click;


end;

procedure TKMGUIGameHouse.Create_HouseSign(aParent : TKMPanel);
var  LABEL_DEC_SIZE : Integer;
var W, H : Integer;
begin

  if gMainSettings.Resolution.Width >= 1600 then
    LABEL_DEC_SIZE := 40
  else
  if gMainSettings.Resolution.Width > 1300 then
    LABEL_DEC_SIZE := 35
  else
    LABEL_DEC_SIZE := 30;

  W := Round(aParent.MasterParent.Width - (aParent.MasterParent.Width * 0.75));
  H := Round(aParent.MasterParent.Height - (aParent.MasterParent.Height * 0.8));
  Panel_HouseSign := TKMPopUpMenu.Create(aParent.MasterParent, aParent.MasterParent.Width - W*2);
  Panel_HouseSign.Height := aParent.MasterParent.Height - (H*2);
  Panel_HouseSign.AnchorsCenter;
  Panel_HouseSign.Left := W;
  Panel_HouseSign.Top := H;
  Bevel_HouseSignBehind := TKMBevel.Create(Panel_HouseSign, -2000,  -2000 ,5000,5000);
  Bevel_HouseSignBehind.BackAlpha := 0.3;
  Bevel_HouseSignBehind.EdgeAlpha := 0.3;
  Bevel_HouseSignBehind.OnClick := HouseSignClose;

  Image_HouseSign := TKMImage.Create(Panel_HouseSign, 0, 0, Panel_HouseSign.Width, Panel_HouseSign.Height, 18, rxGuiMain);
  Image_HouseSign.ImageStretch;

  Bevel_HouseSign := TKMBevel.Create(Panel_HouseSign, LABEL_DEC_SIZE,  LABEL_DEC_SIZE ,Panel_HouseSign.Width - (LABEL_DEC_SIZE * 2),Panel_HouseSign.Height -(LABEL_DEC_SIZE * 2));
  Bevel_HouseSign.BackAlpha := 0.7;
  Bevel_HouseSign.EdgeAlpha := 0.9;



  Label_HouseSignTop := TKMLabel.Create(Panel_HouseSign, LABEL_DEC_SIZE , LABEL_DEC_SIZE , Panel_HouseSign.Width - (LABEL_DEC_SIZE * 2),25 ,'',fntOutline,taCenter);
  Label_HouseSignTop.Caption := gResTexts[1671];

  Label_HouseSign := TKMLabel.Create(Panel_HouseSign, LABEL_DEC_SIZE * 2, LABEL_DEC_SIZE + 25 ,Panel_HouseSign.Width - (LABEL_DEC_SIZE * 4),Panel_HouseSign.Height -(LABEL_DEC_SIZE * 4) - 25,'',fntOutline,taCenter);
  Label_HouseSign.Caption := '';
  Label_HouseSign.TextVAlign := tvaMiddle;
  Label_HouseSign.WordWrap := true;

  Image_HouseSignClose := TKMImage.Create(Panel_HouseSign, Panel_HouseSign.Width - 50, 7, 32, 32, 52);
  Image_HouseSignClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
  Image_HouseSignClose.HighlightOnMouseOver := True;
  Image_HouseSignClose.OnClick := HouseSignClose;

  Panel_HouseSign.Hide;

end;

procedure TKMGuiGameHouse.Create_HouseMerchant;
var I : Integer;
begin
  for I := 0 to high(Button_MerchantType) do
  begin
    Button_MerchantType[I] := TKMButton.Create(Panel_House,29 + 25 * (I mod 5), 90 + 25 * (I div 5), 22, 22, '', bsGame);
    Button_MerchantType[I].Tag := I;
    Button_MerchantType[I].OnClick := ToggleMerchantWareInput;
    Button_MerchantType[I].RX := rxGui;
  end;

  for I := 0 to MAX_HANDS - 1 do
  begin
    Button_PlayerSelect[I] := TKMFlatButtonShape.Create(Panel_House, 9 + 25 * (I mod 5), 300 + 25*(I div 5), 25, 25, IntToStr(I+1), fntGrey, $FF0000FF);
    Button_PlayerSelect[I].LineWidth := 3;
    Button_PlayerSelect[I].Tag := I;
    Button_PlayerSelect[I].OnClick := House_MerchantPlayer;
  end;

end;


// Barracks page
procedure TKMGUIGameHouse.Create_HouseTownhall;
var
  dy: Integer;
begin
  Panel_HouseTownhall := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 350);

    dy := 8;

    Label_TH_Unit := TKMLabel.Create(Panel_HouseTownhall, 0, dy, TB_WIDTH, 0, '', fntOutline, taCenter);
    Inc(dy, 20);

    Image_TH_Left  := TKMImage.Create(Panel_HouseTownhall,  0 + 10,dy + 10,54 - 10,106 - 20,535);
    Image_TH_Left.Disable;
    Image_TH_Left.ImageStretch;
    Image_TH_Train := TKMImage.Create(Panel_HouseTownhall, 62,dy,54,106,536);
    Image_TH_Train.ImageCenter;
    Image_TH_Right := TKMImage.Create(Panel_HouseTownhall,124 + 5,dy + 10,54 - 10,106 - 20,537);
    Image_TH_Right.ImageStretch;
    Image_TH_Right.Disable;
    Inc(dy, 106);

    Button_TH_Left  := TKMButton.Create(Panel_HouseTownhall,  0,dy,54,40,35, rxGui, bsGame);
    Button_TH_Train := TKMButton.Create(Panel_HouseTownhall, 62,dy,54,40,42, rxGui, bsGame);
    Button_TH_Right := TKMButton.Create(Panel_HouseTownhall,124,dy,54,40,36, rxGui, bsGame);
    Button_TH_Left.OnClickShift := House_TH_UnitChange;
    Button_TH_Train.OnClickShift := House_TH_UnitChange;
    Button_TH_Right.OnClickShift := House_TH_UnitChange;
    Button_TH_Train.Disable;

    Inc(dy, 46);
    Label_TH_Costs  := TKMLabel.Create(Panel_HouseTownhall,0,dy,TB_WIDTH,0,gResTexts[TX_HOUSE_WARE_COSTS],fntGrey,taCenter);
    Inc(dy, 20);
    CostsRow_TH_Cost := TKMCostsRow.Create(Panel_HouseTownhall, 0, dy, TB_WIDTH, 40, 8);
    CostsRow_TH_Cost.RX := rxGui;
    CostsRow_TH_Cost.Visible := True;
    CostsRow_TH_Cost.Caption := gRes.Wares[wtGold].Title;
    CostsRow_TH_Cost.TexID1 := gRes.Wares[wtGold].GUIIcon;
    CostsRow_TH_Cost.AsNumber := true;
end;


{Barracks page}
procedure TKMGUIGameHouse.Create_HouseBarracks;
var
  I, J, LastID, rightIconStart: Integer;
  dX, dY, top: Integer;
begin
  Panel_HouseBarracks := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 266);
  top := 0;
  J := 0;
  LastID := 0;
  dY := -35;
  rightIconStart := 0;
    for I := 0 to high(BarracksResOrder) do
    begin
      if BarracksResOrder[I] = wtNone then //make new line
      begin
        //Inc(top, (LastID) div 5) * 42);
        rightIconStart := J;
        top := dY + 42;
        LastID := 0;
        Continue;
      end;

      SetLength(Button_Barracks, J + 1);
      SetLength(Image_Barracks_NotAccept, J + 1);
      SetLength(Image_Barracks_NotAllowTakeOut, J + 1);
      dX := ((LastID) mod 5) * 31;
      dY := top + ((LastID) div 5) * 42;
      Button_Barracks[J] := TKMButtonFlat.Create(Panel_HouseBarracks, dX, dY, 28, 38, 0);
      Button_Barracks[J].TexOffsetX := 1;
      Button_Barracks[J].TexOffsetY := 1;
      Button_Barracks[J].CapOffsetY := 2;
      Button_Barracks[J].Tag := I;
      Button_Barracks[J].TexID := gRes.Wares[BarracksResOrder[I]].GUIIcon;
      Button_Barracks[J].Hint := gRes.Wares[BarracksResOrder[I]].Title;
      Button_Barracks[J].OnClickShift := House_BarracksItemClickShift;
      Button_Barracks[J].LineWidth := 2;

      Image_Barracks_NotAccept[J] := TKMImage.Create(Panel_HouseBarracks, dX+16, dY, 12, 12, 49);
      Image_Barracks_NotAccept[J].Hitable := False;
      Image_Barracks_NotAccept[J].Hint := Format(gResTexts[TX_HOUSE_DELIVERY_PROHIBITED_HINT], [gRes.Wares[BarracksResOrder[I]].Title]);
      Image_Barracks_NotAllowTakeOut[J] := TKMImage.Create(Panel_HouseBarracks, dX, dY, 12, 12, 676);
      Image_Barracks_NotAllowTakeOut[J].Hitable := False;
      Image_Barracks_NotAllowTakeOut[J].Hint := Format(gResTexts[TX_HOUSE_TAKEOUT_PROHIBITED_HINT], [gRes.Wares[BarracksResOrder[I]].Title]);
      Inc(J);
      Inc(LastID);
    end;
    LastID := 0;
    for I := rightIconStart to High(Button_Barracks) do
    begin
      dX := Panel_HouseBarracks.Width - 35;
      dY := 7 + LastID * 42;
      Button_Barracks[I].Left := dX;
      Button_Barracks[I].Top := dY;

      Image_Barracks_NotAccept[I].Left := dX + 16;
      Image_Barracks_NotAccept[I].Top := dY;

      Image_Barracks_NotAllowTakeOut[I].Left := dX;
      Image_Barracks_NotAllowTakeOut[I].Top := dY;
      Inc(LastID);
    end;


    Button_BarracksRecruit := TKMButtonFlat.Create(Panel_House, Button_HouseRepair.Right, Button_HouseRepair.Top - 3, 28, 38, 0);
    Button_BarracksRecruit.TexOffsetX := 1;
    Button_BarracksRecruit.TexOffsetY := 1;
    Button_BarracksRecruit.CapOffsetY := 2;
    Button_BarracksRecruit.TexID := gRes.Units[utRecruit].GUIIcon;
    Button_BarracksRecruit.Hint := gRes.Units[utRecruit].GUIName;
    Button_BarracksRecruit.OnClickShift := House_BarracksItemClickShift;
    Image_Barracks_NotAcceptRecruit := TKMImage.Create(Panel_House, Button_BarracksRecruit.Left+16, Button_BarracksRecruit.Top, 12, 12, 49);
    Image_Barracks_NotAcceptRecruit.Hitable := False;
    Image_Barracks_NotAcceptRecruit.Hint := gResTexts[TX_HOUSE_BARRACKS_NOT_ACCEPT_RECRUIT_HINT];

    top := Button_Barracks[rightIconStart - 1].Bottom - 96;
    Label_Barracks_Unit := TKMLabel.Create(Panel_HouseBarracks, 0, top + 96, TB_WIDTH, 0, '', fntOutline, taCenter);

    Image_Barracks_Left  := TKMImage.Create(Panel_HouseBarracks,  0 + 5,top + 116 + 10,54 - 10,106 - 20,535);
    Image_Barracks_Left.ImageStretch;
    Image_Barracks_Left.Disable;
    Image_Barracks_Train := TKMImage.Create(Panel_HouseBarracks, 62,top + 116,54,106,536);
    Image_Barracks_Right := TKMImage.Create(Panel_HouseBarracks,124 + 5,top + 116 + 10,54 - 10,106 - 20,537);
    Image_Barracks_Right.ImageStretch;
    Image_Barracks_Right.Disable;

    Button_Barracks_Left  := TKMButton.Create(Panel_HouseBarracks,  0,top + 222,54,40,35, rxGui, bsGame);
    Button_Barracks_Train := TKMButton.Create(Panel_HouseBarracks, 62,top + 222,54,40,42, rxGui, bsGame);
    Button_Barracks_Right := TKMButton.Create(Panel_HouseBarracks,124,top + 222,54,40,36, rxGui, bsGame);
    Button_Barracks_Left.OnClickShift := House_BarracksUnitChange;
    Button_Barracks_Train.OnClickShift := House_BarracksUnitChange;
    Button_Barracks_Right.OnClickShift := House_BarracksUnitChange;

    Button_Barracks_Train.Disable;
    Panel_HouseBarracks.Height := Button_Barracks_Left.Bottom;
end;


{Woodcutter page}
procedure TKMGUIGameHouse.Create_HouseWoodcutter;
begin
  Panel_HouseWoodcutter := TKMPanel.Create(Panel_House,TB_PAD,76,TB_WIDTH,266);
    Button_Woodcutter := TKMButtonFlat.Create(Panel_HouseWoodcutter,0,64,32,32,51,rxGui);
    Button_Woodcutter.OnClickShift := House_WoodcutterClick; //Clicking the button cycles it

    Radio_Woodcutter := TKMRadioGroup.Create(Panel_HouseWoodcutter,38,64,TB_WIDTH - 38,48,fntGrey);
    Radio_Woodcutter.ItemIndex := 0;
    Radio_Woodcutter.Add(gResTexts[TX_HOUSES_WOODCUTTER_PLANT_CHOP]);
    Radio_Woodcutter.Add(gResTexts[TX_HOUSES_WOODCUTTER_CHOP_ONLY]);
    Radio_Woodcutter.Add(gResTexts[TX_HOUSES_WOODCUTTER_PLANT_ONLY]);
    Radio_Woodcutter.OnChange := House_WoodcutterChange;

  Panel_HouseFarm := TKMPanel.Create(Panel_House,TB_PAD,76,TB_WIDTH,266);
    Button_Farm := TKMButtonFlat.Create(Panel_HouseFarm,0,64,32,32,51,rxGui);
    Button_Farm.OnClickShift := House_FarmClick; //Clicking the button cycles it

    Radio_Farm := TKMRadioGroup.Create(Panel_HouseFarm,38,64,TB_WIDTH - 38,48,fntGrey);
    Radio_Farm.ItemIndex := 0;
    Radio_Farm.Add(gResTexts[2127]);
    Radio_Farm.Add(gResTexts[2128]);
    Radio_Farm.Add(gResTexts[2129]);
    Radio_Farm.OnChange := House_FarmChange;
end;

Procedure TKMGUIGameHouse.Create_HouseWoodBurner;
var I : Integer;
begin
  Panel_HouseWoodBurner :=  TKMPanel.Create(Panel_House, 0, 275, TB_WIDTH, LINE_HEIGHT*7);

  Fuel_Bar := TKMPercentBar.Create(Panel_HouseWoodBurner, 0, 0, TB_WIDTH, LINE_HEIGHT);
  Fuel_Bar.Caption := ' Fuel Level';

  for I := Low(Wood_Bar) to High(Wood_Bar) do
  begin
    Wood_Bar[I] := TKMPercentBar.Create(Panel_HouseWoodBurner, 0, 25 + 22 * I, TB_WIDTH, 20);
    Coal_Image[I] := TKMCostsRow.Create(Panel_HouseWoodBurner, 0, Wood_Bar[I].Top - 15, TB_WIDTH - 5, 20);
    Coal_Image[I].Count := 1;
    Coal_Image[I].AddBevel := false;
    case I of
      3 : Coal_Image[I].Count := 2;
      4 : Coal_Image[I].Count := 3;
      5, 6 : Coal_Image[I].Count := 4;
      else Coal_Image[I].Count := 1;
    end;
    Coal_Image[I].RX := rxGui;
    Coal_Image[I].TexID1 := gRes.Wares[wtCoal].GUIIcon;

  end;


end;

Procedure TKMGUIGameHouse.Create_HouseCottage;
var I : Integer;
begin
  Panel_HouseCottage :=  TKMPanel.Create(Panel_House, 0, 135, TB_WIDTH, LINE_HEIGHT*7);

  //Bar_CottageProgress := TKMPercentBar.Create(Panel_HouseCottage, 0, 37, TB_WIDTH - 28, LINE_HEIGHT);

  {Bar_FamilyProgress := TKMPercentBar.Create(Panel_HouseCottage, 0, 37, TB_WIDTH - 28, 15);
  for I := 0 to High(Bar_KidsProgress) do
  begin
    Bar_KidsProgress[I] := TKMPercentBar.Create(Panel_HouseCottage, 0, 57 + (I * 13), TB_WIDTH - 28, 10);
  end;}

  //Bar_KidsProgress: array[0..5] of TKMPercentBar;
  TKMBevel.Create(Panel_HouseCottage, 9, 0, TB_WIDTH - 9, 38);
  TKMBevel.Create(Panel_HouseCottage, 9, 40, TB_WIDTH - 9, 38);
  TKMBevel.Create(Panel_HouseCottage, 9, 80, TB_WIDTH - 9, 38);
  TKMBevel.Create(Panel_HouseCottage, 9, 120, TB_WIDTH - 9, 38);
  Button_FamilyQty := TKMButtonFlat.Create(Panel_HouseCottage, 9, 0, 28, 38, 803);
  Button_KidsQty[0] := TKMButtonFlat.Create(Panel_HouseCottage, 9, 40, 28, 38, 804);
  Button_KidsQty[1] := TKMButtonFlat.Create(Panel_HouseCottage, 9, 80, 28, 38, 806);
  Button_KidsQty[2] := TKMButtonFlat.Create(Panel_HouseCottage, 9, 120, 28, 38, 807);

  Button_FamilyQty.Hint := gResTexts[2236];
  Button_KidsQty[0].Hint := gResTexts[2237];
  Button_KidsQty[1].Hint := gResTexts[2238];
  Button_KidsQty[2].Hint := gResTexts[2239];

  for I := Low(Image_FamilyProgress) to High(Image_FamilyProgress) do
  begin
    Image_FamilyProgress[I] := TKMImage.Create(Panel_HouseCottage, 18 + I * 20, 7, 20, 20, 799);
    Image_FamilyProgress[I].AlphaStep := 0;
  end;

  for I := 1 to 8 do
  begin
    Image_KidsProgress[0, I] := TKMImage.Create(Panel_HouseCottage, 18 + I * 20, 50, 20, 20, 801);
    Image_KidsProgress[1, I] := TKMImage.Create(Panel_HouseCottage, 18 + I * 20, 85, 20, 20, 802);
    Image_KidsProgress[2, I] := TKMImage.Create(Panel_HouseCottage, 18 + I * 20, 130, 20, 20, 800);
    Image_KidsProgress[0, I].AlphaStep := 0;
    Image_KidsProgress[1, I].AlphaStep := 0;
    Image_KidsProgress[2, I].AlphaStep := 0;
  end;
end;   

Procedure TKMGUIGameHouse.Create_HousePalace;
const MAX_ROW_COUNT = 2;
var I, K : Integer;
begin

  Panel_House_Palace :=  TKMPanel.Create(Panel_House, 0, 76 + 135, TB_WIDTH, 500);

  SetLength(Button_PalaceVWares, length(gRes.Wares.VirtualWares.PALACE_WARES));
  SetLength(Button_PalaceVWaresCost, length(gRes.Wares.VirtualWares.PALACE_WARES));

  for I := 0 to High(Button_PalaceVWares) do
  begin
    K := gRes.Wares.VirtualWares.PALACE_WARES[I];

    Button_PalaceVWares[I] := TKMButtonFlat.Create(Panel_House_Palace, 0, 0, 28, 32, 0, rxGui);
    //Button_PalaceVWares[I].HideHighlight := true;
    Button_PalaceVWares[I].LineWidth := 1;
    Button_PalaceVWares[I].Clickable := false;

    Button_PalaceVWares[I].TexID := gRes.Wares.VirtualWares[K].GUIIcon;
    Button_PalaceVWares[I].Hint := gResTexts[gRes.Wares.VirtualWares[K].TextID];
    Button_PalaceVWares[I].Left := I mod 6 * 30 + 1;
    Button_PalaceVWares[I].Top := I div 6 * 36;

    Button_PalaceVWaresCost[I] := TKMButtonFlat.Create(Panel_House_Palace, 0, 0, 28, 32, 0, rxGui);
    Button_PalaceVWaresCost[I].LineWidth := 2;
    Button_PalaceVWaresCost[I].Hitable := true;
    Button_PalaceVWaresCost[I].Clickable := false;
  end;


  Button_Palace_UnitPlan := TKMButtonFlat.Create(Panel_House_Palace, TB_WIDTH div 2 - 35, 140, 70, 120, 0);
  Button_Palace_UnitPlan.Caption := '';
  Button_Palace_UnitPlan.CapOffsetY := 40;
  Button_Palace_UnitPlan.BackAlpha := 0.50;
  Button_Palace_UnitPlan.OnClickShift := House_Palace_Click;
  Button_Palace_UnitPlan.Font := fntGrey;
  Button_Palace_UnitPlan.CapColor := $FFFF0000;

  Label_Palace_Unit := TKMLabel.Create(Panel_House_Palace, 9, Button_Palace_UnitPlan.Top - 20, TB_WIDTH, 20, '', fntOutLine, taCenter);
  Label_Palace_Unit.Hitable := false;

  Image_CancelUnit := TKMButtonFlat.Create(Panel_House_Palace, Button_Palace_UnitPlan.Left + 35 - 5, Button_Palace_UnitPlan.Top, 20, 20, 0, rxGuiMain);
  Image_CancelUnit.OnClickShift := House_Palace_Click;
  Image_CancelUnit.HighLightColor := $FF0000FF;

  Image_OrderCount := TKMButtonFlat.Create(Panel_House_Palace, Button_Palace_UnitPlan.Right - 20, Button_Palace_UnitPlan.Top, 20, 20, 0, rxGuiMain);
  Image_OrderCount.Hitable := false;
  Image_OrderCount.CapOffsetY := -12;
  Image_OrderCount.Font := fntGrey;

  Bar_Palace_ProgressLeft := TKMPercentBar.Create(Panel_House_Palace, Button_Palace_UnitPlan.Left - 40, 15, 15, 120);
  Bar_Palace_ProgressLeft.MainColor := icGoldenYellow;
  Bar_Palace_ProgressLeft.Hitable := false;
  Bar_Palace_ProgressLeft.Orientation := pboUp;

  Bar_Palace_ProgressRight := TKMPercentBar.Create(Panel_House_Palace, Button_Palace_UnitPlan.Right + 25, 15, 15, 120);
  Bar_Palace_ProgressRight.MainColor := icGoldenYellow;
  Bar_Palace_ProgressRight.Hitable := false;
  Bar_Palace_ProgressRight.Orientation := pboUp;

  Button_PalaceLeft := TKMButton.Create(Panel_House_Palace, TB_WIDTH div 2 - 55, 180, 25, 30, 2, rxGui, bsGame);
  Button_PalaceLeft.OnClickShift := House_Palace_Click;
  Button_PalaceLeft.CanChangeEnable := false;

  Button_PalaceRight := TKMButton.Create(Panel_House_Palace, TB_WIDTH div 2 + 30, 180, 25, 30, 3, rxGui, bsGame);
  Button_PalaceRight.OnClickShift := House_Palace_Click;
  Button_PalaceTrain := TKMButton.Create(Panel_House_Palace, TB_WIDTH div 2 - 25, 177, 50, 36, 42, rxGui, bsGame);
  Button_PalaceTrain.OnClickShift := House_Palace_Click;

  Button_Palace_PreviousUnit := TKMButtonFlat.Create(Panel_House_Palace, Button_PalaceLeft.Left - 30, 0, 25, 30, 0);
  Button_Palace_PreviousUnit.Hitable := false;

  Button_Palace_NextUnit := TKMButtonFlat.Create(Panel_House_Palace, Button_PalaceRight.Right + 5, 0, 25, 30, 0);
  Button_Palace_NextUnit.Hitable := false;

  Image_Ornament := TKMImage.Create(Panel_House_Palace, Bar_Palace_ProgressLeft.Left - 9, Bar_Palace_ProgressLeft.Top - 17, 167, 149, 782, rxGui);
  Image_Ornament.Hitable := false;

end;

procedure TKMGUIGameHouse.Create_HouseStall;
var I : Integer;
  panel : TKMPanel;
begin
  Panel_House_Stall := TKMExpandPanelCollection.Create(Panel_House, 0, 76, Panel_House.Width, 370);

  panel := Panel_House_Stall[Panel_House_Stall.AddPanel(80, gResTexts[1958], false)];

  for I := low(Button_Wares) to High(Button_Wares) do
  begin
    Button_Wares[I] := TKMButtonFlat.Create(panel, (I - 1) mod 4 * 32, (I - 1) div 4 * 36, 31, 34, 0, rxGui);
    Button_Wares[I].OnClickShift := House_StallClick;
    Button_NotAcceptWares[I] := TKMButtonFlat.Create(panel, (I - 1) mod 4 * 32, (I - 1) div 4 * 36, 15, 15, 0, rxGuiMain);
    Button_NotAcceptWares[I].OnClickShift := House_StallClick;
  end;

  Button_Coin :=  TKMButtonFlat.Create(panel, Panel_House.Width - 31, 0, 28, 34, 0, rxGui);
  Button_Coin.TexID := gRes.Wares.VirtualWares.WareS['vtCoin'].GUIIcon;
  Button_Coin.OnClickShift := House_StallClick;

  panel.Height := Button_Wares[high(Button_Wares)].Bottom + 3;

  panel := Panel_House_Stall[Panel_House_Stall.AddPanel(300, gResTexts[1959], false)];


  for I := low(Button_VWares) to High(Button_VWares) do
  begin
    Button_VWares[I] := TKMButtonFlat.Create(panel, I * 31 + 3,0, 28, 34, 0, rxGui);
    Button_VWares[I].OnClickShift := House_StallClick;
    end;


  Button_CoinV :=  TKMButtonFlat.Create(panel, 43, 40, 28, 34, 0, rxGui);
  Button_CoinV.TexID := gRes.Wares.VirtualWares.WareS['vtCoin'].GUIIcon;
  Button_CoinV.Clickable := false;

  Button_ResToV :=  TKMButtonFlat.Create(panel, panel.Width - 70, 40, 28, 34, 0, rxGui);
  Button_ResToV.TexID := gRes.Wares.VirtualWares.WareS['vtCoin'].GUIIcon;
  Button_ResToV.Clickable := false;
  Label_VWaresRArrow := TKMLabel.Create(panel, 0, 50, panel.Width, 20, '->', fntMetal, taCenter);

end;

procedure TKMGUIGameHouse.ShowSignMSG;
var H  : TKMHouse;
    T:String;
begin
  H := fHouse{TKMHouse(gMySpectator.Selected)};
  if H.Text = '' then
    Exit;

  T := gGame.TextMission.ParseTextMarkup(UnicodeString(H.Text));
  gCursor.Hint := '';
  Panel_HouseSign.Show;
  Label_HouseSign.Caption := T;
  gMySpectator.Selected := nil;
end;


procedure TKMGUIGameHouse.Show(aHouse: TKMHouse);
begin
  if not AskDemolish then
    if aHouse.HouseType = htSign then
    begin
      ShowSignMSG;
      Exit;
    end;

  fHouse := aHouse;
  Show(aHouse, AskDemolish);
end;


procedure TKMGUIGameHouse.Show(aHouse: TKMHouse; aAskDemolish: Boolean);
const
  DO_NOT_DISABLE_CTRLS: array[0..1] of TKMControlClass = (TKMLabel, TKMImage);
var
  I, K, rowRes, base, line, hLabelWidth, demandTop, tmp: Integer;
  vWare : TKMVirtualWare;
begin
  fHouse := aHouse;
  if not aAskDemolish then
    if aHouse.HouseType = htSign then
    begin
      ShowSignMSG;
      Exit;
    end;

  AskDemolish := aAskDemolish;

  Inc(fAnimStep);
  Image_PlayerFlag.TexID := HOUSE_FLAG_TEX_ID + fAnimStep mod HOUSE_FLAG_TEX_ID_FRAME;

  //Hide all House sub-pages
  for I := 0 to Panel_House.ChildPanel.ChildCount - 1 do
    if Panel_House.ChildPanel.Childs[I] is TKMPanel then
      Panel_House.ChildPanel.Childs[I].Hide;

  for I := 0 to high(Button_MerchantType) do
    Button_MerchantType[I].Hide;

  for I := 0 to high(Button_PlayerSelect) do
      Button_PlayerSelect[I].Hide;

  Panel_House.SetCanChangeEnable(gMySpectator.IsSelectedMyObj, DO_NOT_DISABLE_CTRLS);

  if aHouse = nil then
  begin
    Hide;
    Exit;
  end;

  {Common data}
  Label_House.Caption        := gRes.Houses[aHouse.HouseType].HouseName;
  //Calc House caption position
  hLabelWidth := gRes.Fonts[fntOutline].GetTextSize(Label_House.Caption).X;
  if hLabelWidth <= TB_WIDTH - 2*Image_PlayerFlag.Width then
    Label_House.Left := 0
  else if hLabelWidth <= TB_WIDTH - Image_PlayerFlag.Width then
    Label_House.Left := Image_PlayerFlag.Width
  else
    Label_House.Left := Max(TB_WIDTH - hLabelWidth, 0);

  Label_House.Width := TB_WIDTH - Label_House.Left;

  Image_PlayerFlag.FlagColor := gHands[aHouse.Owner].FlagColor;
  Image_PlayerFlag.Hint      := Format(gResTexts[TX_PLAYER_FLAG_HINT], [gHands[aHouse.Owner].OwnerName]);
  Image_House_Logo.TexID     := gRes.Houses[aHouse.HouseType].GUIIcon;
  Image_House_Worker.TexID   := gRes.Units[utSerf].GUIIcon;
  Image_House_Worker.Hint    := gRes.Units[utSerf].GUIName;
  Image_House_Worker.FlagColor := gHands[aHouse.Owner].FlagColor;

  if aHouse.HouseType = htSiegeTower then
  begin
    Button_House_Worker.TexID  := 665;
    Button_House_Worker.Hint := Format(gResTexts[TX_HOUSES_CLOSED_FOR_WORKER_HINT], [gResTexts[471]]);
  end else
  if aHouse.HSpec.GUIWorkerType = utAny then
  begin
    Button_House_Worker.TexID  := 810;
    Button_House_Worker.Hint := Format(gResTexts[TX_HOUSES_CLOSED_FOR_WORKER_HINT], [gResTexts[1960]]);
  end else
  if aHouse.HSpec.GUIWorkerType = utNone then
  begin
    Button_House_Worker.Hide;
  end
  else
  begin
    Button_House_Worker.Hint := Format(gResTexts[TX_HOUSES_CLOSED_FOR_WORKER_HINT], [gRes.Units[gRes.Houses[aHouse.HouseType].GUIWorkerType].GUIName]);
    Button_House_Worker.TexID  := gRes.Units[aHouse.HSpec.GUIWorkerType].GUIIcon
  end;

  HandleHouseClosedForWorker(aHouse);
  Button_House_Worker.FlagColor := gHands[aHouse.Owner].FlagColor;

  //HealthBar_House.Caption   := IntToStr(aHouse.GetHealth) + '/' + IntToStr(aHouse.MaxHealth);
  HealthBar_House.SetFromDivByMax(aHouse.GetHealth, aHouse.MaxHealth);
  Button_UpgradeHouse.Hide;
  Button_ForceWork.Hide;
  Image_WorkProgress.Hide;
  Button_UpgradeHouse.Enabled := aHouse.CanMakeUpgrade or aHouse.IsUpgrading;

  if gRes.Houses[aHouse.HouseType].CanForceWork then
  begin
    Button_ForceWork.Show;
    Button_ForceWork.TexID := IfThen(aHouse.ForceWorking, 770, 769);
    Button_ForceWork.Hint := IfThen(aHouse.ForceWorking, gResTexts[1817], gResTexts[1816]);
  end;
  if length(gRes.Houses[aHouse.HouseType].Levels) > 0 then
  begin
    if aHouse.IsMaxLevel then
    begin
      Button_UpgradeHouse.TexID := 746;
      //Button_UpgradeHouse.Hitable := false;
      Button_UpgradeHouse.Hint := gResTexts[1721];
    end else
    if aHouse.IsUpgrading then
    begin
      Button_UpgradeHouse.TexID := 747;
      Button_UpgradeHouse.Hint := gResTexts[1720];
      Button_UpgradeHouse.Enabled :=  aHouse.CanCancelUpgrade;
    end
    else
    begin
      Button_UpgradeHouse.Hitable := true;
      Button_UpgradeHouse.TexID := 748;
      Button_UpgradeHouse.Hint := gResTexts[1719];
    end;
    Button_UpgradeHouse.Show;

  end;


  if AskDemolish then
  begin
    for I := 0 to Panel_House.ChildPanel.ChildCount - 1 do
      Panel_House.ChildPanel.Childs[I].Hide; //hide all
    Label_House_Demolish.Show;
    Button_House_DemolishYes.Show;
    Button_House_DemolishNo.Show;
    Label_House.Show;
    Image_PlayerFlag.Show;
    Image_House_Logo.Show;
    Image_House_Worker.Show;
    Button_House_Worker.Hide;
    Button_Bell.Hide;
    HealthBar_House.Show;
    Panel_House.Show;
    Exit;
  end;
  if not aHouse.IsComplete  or aHouse.IsUpgrading then
  begin
    for I := 0 to Panel_House.ChildPanel.ChildCount - 1 do
      Panel_House.ChildPanel.Childs[I].Hide; //hide all
    if aHouse.IsUpgrading then
      Button_UpgradeHouse.Show;
    Label_House_UnderConstruction.Show;
    Image_HouseConstructionWood.Show;
    Image_HouseConstructionStone.Show;
    Image_HouseConstructionTile.Show;
    Label_HouseConstructionWood.Show;
    Label_HouseConstructionStone.Show;
    Label_HouseConstructionTile.Show;
    Button_SetHouseStyle.Visible := (Length(fHouse.HSpec.Styles) > 0) and (Length(fHouse.HSpec.Levels) = 0);
    Button_SetHouseStyle.TexID := fHouse.GetStyleGuiIcon;
    Label_HouseConstructionWood.Caption := IntToStr(aHouse.GetBuildWoodDelivered) + ' / ' + IntToStr(aHouse.WoodCost);
    Label_HouseConstructionStone.Caption := IntToStr(aHouse.GetBuildStoneDelivered) + ' / ' + IntToStr(aHouse.StoneCost);
    Label_HouseConstructionTile.Caption := IntToStr(aHouse.TileDelivered) + ' / ' + IntToStr(aHouse.TileCost);

    Label_House.Show;
    Image_PlayerFlag.Show;
    Image_House_Logo.Show;
    Image_House_Worker.Visible := gRes.Houses[aHouse.HouseType].CanHasWorker;
    Button_House_Worker.Hide;
    HealthBar_House.Show;
    Panel_House.Show;
    Exit;
  end;

  Image_House_Worker.Hide;
  if not (aHouse.HouseType in [htBarracks, htSchool, htCottage, htHouse]) then
    Button_House_Worker.Visible := gRes.Houses[aHouse.HouseType].CanHasWorker or (aHouse.HouseType in [htSiegeTower])
  else
    Button_House_Worker.Visible := false;
  Button_Bell.Hide;
  if aHouse.HouseType in [htStore, htTownhall] then
    Button_Bell.Show;

  Button_HouseDeliveryMode.Enabled := aHouse.AllowDeliveryModeChange;
  Button_HouseDeliveryMode.Show;
  Button_HouseRepair.Show;


  Button_HouseRepair.TexID := IfThen(aHouse.BuildingRepair, 39, 40);

  // We use aHouse.NewDeliveryMode, so that player could see the effect immediately
  // (instead of DeliveryMode which goes through GIP)
  Button_HouseDeliveryMode.TexID := DELIVERY_MODE_SPRITE[aHouse.NewDeliveryMode];
  if (aHouse.WorkingTime > 0) and (gRes.Houses[aHouse.HouseType].MaxWorkersCount = 1) then
  begin
    Image_WorkProgress.Show;
    Image_WorkProgress.AlphaStep := aHouse.WorkingTime / aHouse.TotalWorkingTime;
  end;

  Label_House_UnderConstruction.Hide;
  Image_HouseConstructionWood.Hide;
  Image_HouseConstructionStone.Hide;
  Image_HouseConstructionTile.Hide;
  Label_HouseConstructionWood.Hide;
  Label_HouseConstructionStone.Hide;
  Label_HouseConstructionTile.Hide;
  Button_SetHouseStyle.Hide;
  Label_House_Demolish.Hide;
  Button_House_DemolishYes.Hide;
  Button_House_DemolishNo.Hide;
  Panel_House.Show;
  if not (aHouse.HouseType in [htSchool, htPalace, htBarracks, htCottage, htHouse, htStore]) then
    Image_House_Worker.Show;
  //Image_House_Worker.Visible := not (aHouse.HouseType in [htTownHall, htSchool, htPalace]);

  Button_ToggleGrainType.Visible := aHouse.HouseType in [htFarm, htProductionThatch];
  Button_ToggleGrassType.Visible := aHouse.HouseType in [htFarm, htProductionThatch];
  Button_ToggleVegeType.Visible := aHouse.HouseType in [htFarm, htProductionThatch];
  Button_ToggleFruitType.Visible := aHouse.HouseType = htAppleTree;
  Button_CollectorsMode.Visible := aHouse.HouseType = htCollectors;

  if Button_CollectorsMode.Visible then
  begin
    Button_CollectorsMode.TexID := IfThen(TKMHouseCollectors(aHouse).Mode = cmCollector, 359, 375);
    Button_CollectorsMode.Hint := gResTexts[IfThen(TKMHouseCollectors(aHouse).Mode = cmCollector, 2141, 2142)];
  end;

  if Button_ToggleFruitType.Visible then
  begin
    Button_ToggleFruitType.TexID := gFruitTrees[TKMHouseAppleTree(aHouse).GetFruitType].GuiIcon;
    Button_ToggleFruitType.Hint := gResTexts[gFruitTrees[TKMHouseAppleTree(aHouse).GetFruitType].HintID];
  end;

  if Button_ToggleGrassType.Visible then
    if aHouse is TKMHouseFarm then
    begin
      Button_ToggleGrassType.TexID := GRAIN_GUI_PIC[TKMHouseFarm(aHouse).GrassType];
      Button_ToggleGrassType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseFarm(aHouse).GrassType]];

      Button_ToggleGrainType.TexID := GRAIN_GUI_PIC[TKMHouseFarm(aHouse).GrainType] ;
      Button_ToggleGrainType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseFarm(aHouse).GrainType]];

      Button_ToggleVegeType.TexID := GRAIN_GUI_PIC[TKMHouseFarm(aHouse).VegeType] ;
      Button_ToggleVegeType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseFarm(aHouse).VegeType]];
    end
    else
    if aHouse is TKMHouseProdThatch then
    begin
      Button_ToggleGrassType.TexID := GRAIN_GUI_PIC[TKMHouseProdThatch(aHouse).GrassType];
      Button_ToggleGrassType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseProdThatch(aHouse).GrassType]];

      Button_ToggleGrainType.TexID := GRAIN_GUI_PIC[TKMHouseProdThatch(aHouse).GrainType];
      Button_ToggleGrainType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseProdThatch(aHouse).GrainType]];

      Button_ToggleVegeType.TexID := GRAIN_GUI_PIC[TKMHouseProdThatch(aHouse).VegeType] ;
      Button_ToggleVegeType.Hint := gResTexts[GRAIN_GUI_HINT[TKMHouseProdThatch(aHouse).VegeType]];
    end;

  demandTop := 10;
  if Button_ForceWork.Visible then
    Inc(demandTop, 30);
  if Button_ToggleVegeType.Visible or Button_ToggleFruitType.Visible or Button_CollectorsMode.Visible then
    Inc(demandTop, 30);

  SortVisibleControls(0, Button_ForceWork.Top, Panel_House.Width, 1, [Button_ForceWork]);

  Ship_ShipType.Visible := (aHouse.HouseType = htShipYard) and HasAnyUnit(SHIPYARD_ORDER, true);
  Ship_DoWork.Visible := Ship_ShipType.Visible;
  //WaresOut_ShipYard.Visible := aHouse.HouseType = htShipYard;



  if Length(gRes.Houses[aHouse.HouseType].WareInputSlots) > 0 then
  begin
    for I := 0 to high(Button_MerchantType) do
    begin
      if I < Length(gRes.Houses[aHouse.HouseType].WareInputSlots) then
      begin
        Button_MerchantType[I].Show;
        Button_MerchantType[I].Enabled := aHouse.CanChangeWareInput or (GetKeyState(VK_SHIFT) < 0);
        Button_MerchantType[I].TexID := gRes.Houses[aHouse.HouseType].WareInputSlots[I].Icon;
        Button_MerchantType[I].ShowImageEnabled := (aHouse.WareInputSlot = I);
        Button_MerchantType[I].Top := 65 + 25 * (I div 5) + demandTop;
      end else
      begin
        Button_MerchantType[I].Disable;
        Button_MerchantType[I].Hide;
      end;
    end;
    demandTop :=demandTop + 30 * ( high(gRes.Houses[aHouse.HouseType].WareInputSlots) div 5);
  end;

  for I := 1 to 9 do
    VirtualWares_Row[I].Hide;


  K := 1;
  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    with gRes.Wares.VirtualWares[I] do
      if aHouse.HouseType in ShowInHouses then
      begin

        VirtualWares_Row[K].Top := 76 + demandTop;
        VirtualWares_Row[K].TexID := GuiIcon;
        VirtualWares_Row[K].Caption := gResTexts[TextID];
        VirtualWares_Row[K].Show;
        VirtualWares_Row[K].WareCount := gHands[aHouse.Owner].VWaresCount[I];
        inc(K);
        if K = 10  then
          Break;
        Inc(demandTop, 25);
      end;


  Button_Workless.Visible := ((aHouse.HouseType in [htSchool, htPalace]) and (gHands[aHouse.Owner].GetWorklessCount <= 999))
                              or (aHouse.HouseType in [htCottage, htHouse]);
  Button_Workless.Caption := IntToStr(gHands[aHouse.Owner].GetWorklessCount);


  Icons_Workers.Visible := (aHouse.HSpec.MaxWorkersCount > 1) and ((Icons_Workers.Parent.MasterControl.CtrlOver = Button_House_Worker)
                          or (Icons_Workers.Parent.MasterControl.CtrlOver = Icons_Workers));

  if Icons_Workers.Visible then
  begin
    Icons_Workers.SetIcons(aHouse.GetWorkersIcons);
    Icons_Workers.Top := Button_House_Worker.Center.Y - (Icons_Workers.Height div 2);
  end;
  Progress_Beasts.ColumnCount := 5;
  Progress_Beasts.Left := 0;
  Progress_Beasts.Width := TB_WIDTH;
  Progress_Beasts.Hide;
  Progress_BigWare.Hide;
  ProgressBar_BigWare.Hide;

  Button_BarracksRecruit.Hide;
  Image_Barracks_NotAcceptRecruit.Hide;

  rowRes := 1;
  line := 0;
  base := 2;

  //check if last selected unit is unlocked
  case fHouse.HouseType of
    htSchool        : CheckLastSelected(fLastSchoolUnit, SCHOOL_GAME_ORDER);
    htSiegeWorkshop : CheckLastSelected(fLastSiegeUnit, SIEGE_GAME_ORDER);
    htBarracks      : CheckLastSelected(fLastBarracksUnit, BARRACKS_GAME_ORDER);
    htTownhall      : CheckLastSelected(fLastTHUnit, TH_GAME_ORDER);
    htPalace        : CheckLastSelected(fLastPalaceUnit, PALACE_UNITS_ORDER);
  end;
  Button_ATBoltCount.Visible := aHouse.HouseType = htWallTower;
  case aHouse.HouseType of
    htMarket:         begin
                        House_MarketFill(TKMHouseMarket(aHouse));
                        Panel_HouseMarket.Show;
                      end;

    htStore:          begin
                        House_StoreFill;
                        Panel_HouseStore.Show;

                      end;
    htSchool:         begin
                        {WaresRow_School_Gold.WareCount := aHouse.CheckWareIn(wtGold) - Ord(TKMHouseSchool(aHouse).HideOneGold);
                        WaresRow_School_Boots.WareCount := aHouse.CheckWareIn(wtBoots);}

                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;
                        rowRes := 1;
                        line := 0;
                        base := 2 + demandTop ;
                        ShowCommonDemand(aHouse, base, line, rowRes);

                        Panel_House_School.Top := 76 + base + line * 25;

                        Button_School_UnitWIP.FlagColor := gHands[aHouse.Owner].FlagColor;
                        for I := 1 to 5 do
                          Button_School_UnitPlan[I].FlagColor := gHands[aHouse.Owner].FlagColor;
                        Image_School_Left.FlagColor  := gHands[aHouse.Owner].FlagColor;
                        Image_School_Right.FlagColor := gHands[aHouse.Owner].FlagColor;
                        Image_School_Train.FlagColor := gHands[aHouse.Owner].FlagColor;
                        House_SchoolUnitChange(nil, []);
                        {Icons_AllWorkers.Clear;
                        for I := 0 to High(SCHOOL_GAME_ORDER) do
                          If gHands[fHouse.Owner].Locks.UnitUnlocked(SCHOOL_GAME_ORDER[I], htSchool) then
                            Icons_AllWorkers.AddIcon(gRes.Units[SCHOOL_GAME_ORDER[I]].GUIIcon, ord(SCHOOL_GAME_ORDER[I]));}

                        //Panel_House_School.Show;
                      end;
    htBarracks:       begin
                        House_BarracksUnitChange(nil, []);
                        Panel_HouseBarracks.Show;
                      end;
    htWoodcutters:    begin
                        House_WoodcutterChange(nil);
                        Panel_HouseWoodcutter.Show;
                        Panel_HouseWoodcutter.Top := 76 + demandTop;
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;

                        {Label_Common_Offer.Show;
                        Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS]+':';
                        //Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS]+'(x'+inttostr(gRes.Houses[aHouse.HouseType].ResProductionX)+'):';
                        Label_Common_Offer.Top := 8 + demandTop;}

                        rowRes := 1;
                        line := 0;
                        base := demandTop;
                        ShowCommonOutput(aHouse, base, line, rowRes);

                        {WaresRow_Common[1].TexID := gRes.Wares[aHouse.WareOutput[1]].GUIIcon;
                        WaresRow_Common[1].WareCount := aHouse.CheckWareOut(aHouse.WareOutput[1]);
                        WaresRow_Common[1].Caption := gRes.Wares[aHouse.WareOutput[1]].Title;
                        WaresRow_Common[1].Hint := gRes.Wares[aHouse.WareOutput[1]].Title;
                        WaresRow_Common[1].Show;
                        WaresRow_Common[1].Top := 2 + LINE_HEIGHT + demandTop;;
                        WaresRow_Common[1].WareCntAsNumber := false;}

                        Label_DepletedMsg.Top := Panel_HouseWoodcutter.Top + Radio_Woodcutter.Bottom + 5;
                        Label_DepletedMsg.Visible := aHouse.ResourceDepleted;
                        if aHouse.ResourceDepleted then
                          Label_DepletedMsg.Caption := gResTexts[aHouse.GetResourceDepletedMessageId];
                      end;
    {htTownHall:       begin
                        //Now show only what we need
                        rowRes := 1;
                        line := 0;
                        base := 2;

                        //Show Demand
                        ShowCommonDemand(aHouse, base, line, rowRes);
                        Panel_House_Common.Show;
                        ShowTownHall(aHouse);
                        Panel_HouseTownHall.Top := 100 + demandTop;
                      end;}
    htSiegeWorkShop : begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;

                        //Now show only what we need
                        rowRes := 1;
                        line := 0;
                        base := 2;

                        //Show Demand
                        ShowCommonDemand(aHouse, base, line, rowRes);


                        Panel_House_Common.Show;

                        Button_Siege_UnitWIP.FlagColor := gHands[aHouse.Owner].FlagColor;
                        Button_OperatorsCount.FlagColor := gHands[aHouse.Owner].FlagColor;

                        WaresProdCt_Common[1].Caption := '';
                        WaresProdCt_Common[2].Caption := '';
                        WaresProdCt_Common[3].Caption := '';
                        WaresProdCt_Common[4].Caption := '';

                        for I := 0 to 3 do
                        begin

                          case TKMHouseSiegeWorkshop(aHouse).GetNeededWares[I].W of
                            wtSteelE: WaresProdCt_Common[2].Caption := 'x' + IntToStr(TKMHouseSiegeWorkshop(aHouse).GetNeededWares[I].C);
                            wtBitinE: WaresProdCt_Common[4].Caption := 'x' + IntToStr(TKMHouseSiegeWorkshop(aHouse).GetNeededWares[I].C);
                            wtWheel: WaresProdCt_Common[3].Caption := 'x' + IntToStr(TKMHouseSiegeWorkshop(aHouse).GetNeededWares[I].C);
                            wtLog: WaresProdCt_Common[1].Caption := 'x' + IntToStr(TKMHouseSiegeWorkshop(aHouse).GetNeededWares[I].C);
                          end;
                          WaresProdCt_Common[I + 1].Show;
                        end;

                        for I := 1 to 5 do
                          Button_Siege_UnitPlan[I].FlagColor := gHands[aHouse.Owner].FlagColor;

                        Image_Siege_Left.FlagColor  := gHands[aHouse.Owner].FlagColor;
                        Image_Siege_Right.FlagColor := gHands[aHouse.Owner].FlagColor;
                        Image_Siege_Train.FlagColor := gHands[aHouse.Owner].FlagColor;

                        //Panel_House_Siege.Show;
                        Panel_House_Siege.Top := 160 + demandTop;

                        House_SchoolUnitChange(nil, []);
                        Progress_Beasts.Top :=  Button_Siege_Left.Bottom + 90 + demandTop{ + Panel_House_Siege.Top};
                        Progress_Beasts.RX := rxGui;
                        Progress_Beasts.TexID := TKMHouseSiegeWorkshop(aHouse).GetMWGuiIcons;
                        Progress_Beasts.Width := TB_WIDTH - 36;
                        Progress_Beasts.Left := 36;
                        SetLength(Progress_Beasts.Progress, length(Progress_Beasts.TexID));
                        If length(Progress_Beasts.Progress) > 0 then
                        begin
                          for I := 0 to High(Progress_Beasts.Progress) do
                            Progress_Beasts.Progress[I] := 0;

                          Progress_Beasts.Progress[0] := TKMHouseSiegeWorkshop(aHouse).GetOperatorsInsideCnt / 4;
                        end;
                        Button_OperatorsCount.Caption := TKMHouseSiegeWorkshop(aHouse).GetOperatorsInsideCnt.ToString;

                        Progress_Beasts.Colors := [icLightGreen];
                        Progress_Beasts.Show;


                      end;
    htWoodBurner :    Begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;

                        Panel_House_Common.Show;
                        rowRes := 1;

                        line := 0;
                        base := 2;
                        //ShowCommonDemand(aHouse, base, line, rowRes);

                        ShowCommonDemandSingle(aHouse, [1],base, line, rowRes);
                        //dec(line);
                        base := 5;

                        ShowCommonDemandSingle(aHouse, [2, 3, 4],base, line, rowRes);


                        ShowCommonOutput(aHouse, base, line, rowRes);
                        House_WoodBurnerChange;
                        Panel_HouseWoodBurner.Show;
                        Panel_HouseWoodBurner.Top := 275 + demandTop;
                      End;
    htMerchant:       begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;

                        Panel_House_Common.Show;

                        rowRes := 1;
                        line := 0;
                        base := 2 + demandTop;

                        ShowCommonDemand(aHouse, base, line, rowRes);
                        rowRes := 1;
                        line := 0;

                        for I := 0 to high(Button_PlayerSelect) do
                          if (I < gHands.Count)
                          and (gHands[I].Houses.Stores.Count > 0)
                          and (gHands[aHouse.Owner].Alliances[I] = atAlly)
                          and (I <> aHouse.Owner) then
                          begin
                            Button_PlayerSelect[I].Top := 300 + 30 * (Line div 6);
                            Button_PlayerSelect[I].Left := 30 * (line mod 6);
                            Button_PlayerSelect[I].Show;
                            Button_PlayerSelect[I].Down := TKMHouseMerchant(aHouse).SendToHand[I];
                            Button_PlayerSelect[I].ShapeColor := gHands[I].FlagColor;
                            Button_PlayerSelect[I].Hint := gResTexts[1718] + gHands[I].OwnerName;
                            Inc(line);
                          end else
                            Button_PlayerSelect[I].Hide;

                      end;
    htCollectors:       begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;

                        rowRes := 1;
                        line := 0;
                        base := 0 + demandTop;
                        ShowCommonDemand(aHouse, base, line, rowRes);

                        //rowRes := 2;
                        //line := 1;
                        ShowCommonOutput(aHouse, base, line, rowRes);

                        WaresOut_ShipYard.Top := base + line * 25 + 20;
                        WaresOut_ShipYard.WarePlan := TKMHouseCollectors(fHouse).WaresOut;
                        WaresOut_ShipYard.Show;

                        Panel_House_Common.Show;
                      end; 
                      
    htPalace:       begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;
                        rowRes := 1;
                        line := 0;
                        base := 0 + demandTop;

                        ShowCommonDemand(aHouse, base, line, rowRes);
                        House_PalaceRefresh(aHouse);
                        //Panel_House_Palace.Show;

                      end;
                      
    htCottage, htHouse:begin
                        //First thing - hide everything
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;
                        Panel_HouseCottage.Show;

                        rowRes := 1;
                        line := 0;
                        base := 0 + demandTop;

                        ShowCommonDemand(aHouse, base, line, rowRes);
                        //ResRow_Cottage_Apples.WareCount := aHouse.CheckWareIn(wtApple);
                        //Button_CottageWorkless.Caption := IntToStr(TKMHouseCottage(aHouse).Workless);

                        Button_FamilyQty.Caption := IntToStr(TKMHouseCottage(aHouse).FamilyQty);
                        Button_KidsQty[0].Caption := IntToStr(TKMHouseCottage(aHouse).KidAgeCount[waBaby]);
                        Button_KidsQty[1].Caption := IntToStr(TKMHouseCottage(aHouse).KidAgeCount[waTeenager]);
                        Button_KidsQty[2].Caption := IntToStr(TKMHouseCottage(aHouse).KidAgeCount[waAdult]);

                        Button_Workless.Caption := IntToStr(TKMHouseCottage(aHouse).Workless);

                        for I := 1 to high(Image_FamilyProgress) do
                          Image_FamilyProgress[I].Hide;

                        for I := 1 to high(Image_KidsProgress[0]) do
                        begin
                          Image_KidsProgress[0, I].Hide;
                          Image_KidsProgress[1, I].Hide;
                          Image_KidsProgress[2, I].Hide;
                        end;
                        for I := 1 to Min(TKMHouseCottage(aHouse).KidAgeCount[waBaby], 7) do
                        begin
                          Image_KidsProgress[0, I].Show;
                          Image_KidsProgress[0, I].AlphaStep := TKMHouseCottage(aHouse).GetKidProgress(waBaby, I - 1);
                        end;

                        for I := 1 to Min(TKMHouseCottage(aHouse).KidAgeCount[waTeenager], 7) do
                        begin
                          Image_KidsProgress[1, I].Show;
                          Image_KidsProgress[1, I].AlphaStep := TKMHouseCottage(aHouse).GetKidProgress(waTeenager, I - 1);
                        end;
                        for I := 1 to Min(TKMHouseCottage(aHouse).KidAgeCount[waAdult], 7) do
                        begin
                          Image_KidsProgress[2, I].Show;
                          Image_KidsProgress[2, I].AlphaStep := TKMHouseCottage(aHouse).GetKidProgress(waAdult, I - 1);
                        end;

                        for I := 1 to TKMHouseCottage(aHouse).FamilyQty do
                        begin
                          Image_FamilyProgress[I].Show;
                          Image_FamilyProgress[I].AlphaStep := TKMHouseCottage(aHouse).GetFamilyProgress(I - 1);
                        end;

                      end;

    htStall:          begin
                        for I := 0 to Panel_House_Common.ChildCount - 1 do
                          Panel_House_Common.Childs[I].Hide;
                        tmp := 0;
                        for I := Low(Button_Wares) to High(Button_Wares) do
                        begin
                          Button_Wares[I].Visible := gRes.Wares[aHouse.WareInput[I]].IsValid;
                          Button_Wares[I].Enabled := aHouse.HasWorkerInside;

                          Button_Wares[I].TexID := gRes.Wares[aHouse.WareInput[I]].GUIIcon;
                          Button_Wares[I].Caption := IntToStr(aHouse.ResIn[I]);
                          Button_NotAcceptWares[I].Visible := gRes.Wares[aHouse.WareInput[I]].IsValid;
                          Button_NotAcceptWares[I].TexID := IfThen(aHouse.GetAcceptWareIn(aHouse.WareInput[I]) > 0, 32, 33);
                          K := 0;
                          if gRes.Wares[aHouse.WareInput[I]].IsValid then
                            K := TKMHouseStall(aHouse).WareRatioTo(I);

                          Button_Wares[I].Hint := Format(gResTexts[1987], [gRes.Wares[aHouse.WareInput[I]].Title, 1,
                                                                            gResTexts[gRes.Wares.VirtualWares.WareS['vtCoin'].TextID],
                                                                            K
                                                                            ]);
                          Inc(tmp, K);

                        end;
                        K := 0;
                        for I := Low(Button_Wares) to High(Button_Wares) do
                          if gRes.Wares[aHouse.WareInput[I]].IsValid then
                            inc(K, TKMHouseStall(aHouse).WareRatioTo(I) * aHouse.ResIn[I]);

                        Button_Coin.Caption := IntToStr(gHands[aHouse.Owner].VirtualWare['vtCoin']);

                        Button_Coin.Hint := Format(gResTexts[1986], [tmp, K]);
                        Button_Coin.Enabled := aHouse.HasWorkerInside;
                        Button_CoinV.Caption := Button_Coin.Caption;



                          Button_CoinV.Hide;
                          Button_ResToV.Hide;
                          Label_VWaresRArrow.Hide;
                        //Label_VWaresRatios.Caption := '';
                        for I := Low(Button_VWares) to High(Button_VWares) do
                        begin
                          vWare := gRes.Wares.VirtualWares.WareS[TKMHouseStall(aHouse).VWare[I]];
                          //Button_VWares[I].TexID := vWare.GUIIcon;
                          Button_VWares[I].TexID := IfThen(TKMHouseStall(aHouse).VWareCount[I] > 0, vWare.GUIIcon, 0);
                          Button_VWares[I].Hint := gResTexts[vWare.TextID];
                          Button_VWares[I].Caption := IfThen(TKMHouseStall(aHouse).VWareCount[I] > 0, IntToStr(TKMHouseStall(aHouse).VWareCount[I]), '-');
                          Button_VWares[I].Enabled := TKMHouseStall(aHouse).VWareCount[I] > 0;
                          Button_VWares[I].Down := Button_VWares[I].Enabled;
                          if not TKMHouseStall(aHouse).CanBuyItem(I) then
                            Button_VWares[I].DownColor := $FF0000FF
                          else
                            Button_VWares[I].DownColor := $FFFFFFFF;
                          Button_VWares[I].Hint := Format(gResTexts[1988], [gResTexts[gRes.Wares.VirtualWares.WareS['vtCoin'].TextID],
                                                                                                    TKMHouseStall(aHouse).RatioFrom(I),
                                                                                                    gResTexts[vWare.TextID],
                                                                                                    TKMHouseStall(aHouse).RatioTo(I)
                                                                                                  ]);
                          if Button_VWares[I].Parent.MasterControl.CtrlOver = Button_VWares[I] then
                          begin
                            Button_CoinV.Show;
                            Button_ResToV.Show;
                            Label_VWaresRArrow.Show;
                            Button_CoinV.Caption := 'x' + IntToStr(TKMHouseStall(aHouse).RatioFrom(I));
                            Button_ResToV.Caption := 'x' + IntToStr(TKMHouseStall(aHouse).RatioTo(I));
                            Button_ResToV.TexID := Button_VWares[I].TexID;
                          end;

                          //Label_VWaresRatios[I].Caption := Format('x%d', [TKMHouseStall(aHouse).RatioTo(I)]);
                        end;

                        //Button_VWares:array[0..6] of TKMButtonFlat;
                        Panel_House_Stall.Show;
                      end;

    htPearl : Panel_Pearl.Show(fHouse, base + line * 25 + 20 + 50);

  else
    //First thing - hide everything
      for I := 0 to Panel_House_Common.ChildCount - 1 do
        Panel_House_Common.Childs[I].Hide;

    if aHouse is TKMHouseQueue then
    begin
      //Now show only what we need
      rowRes := 1;
      line := 0;
      base := 2 + demandTop;

      //Show Demand
      ShowCommonDemand(aHouse, base, line, rowRes);
      Button_Queue_Left.Enabled := not (aHouse.WareOutput[2] = wtNone);
      Button_Queue_Right.Enabled := not (aHouse.WareOutput[2] = wtNone);

      for I := 0 to High(Button_Queue_Wares) do
      begin
        Button_Queue_Wares[I].TexID := gRes.Wares[aHouse.WareOutput[fLastWareOutput]].GUIIcon;
      end;

      if (TKMHouseQueue(aHouse).InProgress) and (TKMHouseQueue(aHouse).Queue[0].W <> wtNone) then
      begin
        Bar_QueueProgress.Height := Trunc(-Button_Queue_WarePlan[0].Height * aHouse.WorkingTime / aHouse.TotalWorkingTime);
        Button_Queue_WarePlan[0].DownColor := $FF0707FF;
        //Button_Queue_WarePlan[0].Enabled := false;
        Button_Queue_WarePlan[0].Hitable := false;
        Button_Queue_WarePlan[0].HideHighlight := true;
      end else
      begin
        Bar_QueueProgress.Height := 0;
        Button_Queue_WarePlan[0].DownColor := $FFFFFFFF;
        //Button_Queue_WarePlan[0].Enabled := true;
        Button_Queue_WarePlan[0].Hitable := true;
        Button_Queue_WarePlan[0].HideHighlight := false;
      end;

      for I := 0 to High(Button_Queue_WarePlan) do
        if TKMHouseQueue(aHouse).Queue[I].W <> wtNone then
        begin
          Button_Queue_WarePlan[I].TexID := gRes.Wares[TKMHouseQueue(aHouse).Queue[I].W].GUIIcon;
          Button_Queue_WarePlan[I].Caption := IntToStr(TKMHouseQueue(aHouse).Queue[I].Qt);
        end else
        begin
          Button_Queue_WarePlan[I].TexID := 0;
          Button_Queue_WarePlan[I].Caption := '';
        end;
      CheckBox_NotRem.Checked := TKMHouseQueue(aHouse).NotRemLastPos;
      Panel_HouseQueue.Top := base + line * 25 + 10;

      base := 75 + 34 * (QUEUE_LENGTH div 6) + demandTop;
      ShowCommonOutput(aHouse, base, line, rowRes);
      ShowCommonCost(aHouse, base, line, rowRes);

      Panel_HouseQueue.Show;
      Panel_House_Common.Show;

    end else
    begin

      //Now show only what we need
      rowRes := 1;
      line := 0;
      base := 5 + demandTop;

      //Show Demand
      ShowCommonDemand(aHouse, base, line, rowRes);
      //Show Output
      ShowCommonOutput(aHouse, base, line, rowRes);
      //Show Orders
      ShowCommonOrders(aHouse, base, line, rowRes);

      Progress_Beasts.Shape := stSquare;
      Progress_Beasts.Top := base + line * 25;
      Progress_Beasts.RX := rxGui;
      Progress_Beasts.Visible := aHouse.HouseType in [htSwine, htHovel, htStables, htFarm, htVineyard];
      case aHouse.HouseType of
        htSwine :   begin
                      Progress_Beasts.Progress := TKMHouseSwineStable(aHouse).GetBeastsProgresses;
                      Progress_Beasts.Colors := TKMHouseSwineStable(aHouse).GetBeastsProgressColors;
                      Progress_Beasts.TexID := [[0, 912, 912, 912, 912, 912]];
                    end;
        htStables : begin
                      Progress_Beasts.Progress := TKMHouseSwineStable(aHouse).GetBeastsProgresses;
                      Progress_Beasts.Colors := TKMHouseSwineStable(aHouse).GetBeastsProgressColors;
                      Progress_Beasts.TexID := [[0, 913, 913, 913, 913]];
                    end;
        htHovel :   begin
                      Progress_Beasts.Progress := TKMHouseHovel(aHouse).GetBeastsProgresses;
                      Progress_Beasts.Colors := TKMHouseHovel(aHouse).GetBeastsProgressColors;

                      Progress_Beasts.TexID := [[0, 680, 680, 680, 680, 680, 680]];
                      Progress_Beasts.ColumnCount := 5;
                      Progress_BigWare.Show;
                      Progress_BigWare.Top := Progress_Beasts.Top;
                      Progress_BigWare.Left := 0;
                      Progress_BigWare.Width := TB_WIDTH div 2;
                      Progress_BigWare.Animation.Create(0, 0, 901, 11);
                      Progress_BigWare.AnimStep := EnsureRange(Round(Progress_BigWare.Animation.Count * TKMHouseHovel(aHouse).FeathersProgress) - 1, 0, 10);

                      Progress_BigWare2.Show;
                      Progress_BigWare2.Top := Progress_Beasts.Top;
                      Progress_BigWare2.Left := TB_WIDTH div 2;
                      Progress_BigWare2.Width := TB_WIDTH div 2;
                      Progress_BigWare2.Animation.Create(0, 0, 937, 8);
                      Progress_BigWare2.AnimStep := EnsureRange(Round(Progress_BigWare2.Animation.Count * TKMHouseHovel(aHouse).EggsProgress) - 1, 0, 7);


                      Progress_Beasts.Top := Progress_BigWare.Bottom + 3;
                    end;
        htFarm :    begin
                      House_FarmChange(nil);
                      Progress_Beasts.Progress := TKMHouseFarm(aHouse).GetProgressArray;
                      Progress_Beasts.Colors := [icLightGreen];
                      Progress_Beasts.TexID := TKMHouseFarm(aHouse).GetTexIDsArray;
                      Panel_HouseFarm.Show;
                      Panel_HouseFarm.Top := Progress_Beasts.Bottom + 18;
                    end;
        htVineyard :  begin
                        Progress_BigWare.Show;
                        Progress_BigWare.Top := Progress_Beasts.Top;
                        Progress_BigWare.Left := 0;
                        Progress_BigWare.Width := TB_WIDTH;
                        Progress_BigWare.Animation.Create(0, 0, 892, 9);
                        Progress_BigWare.AnimStep := EnsureRange(Round(Progress_BigWare.Animation.Count * (TKMHouseVineyard(aHouse).WineToProduce / 5)) - 1, 0, 8);

                        ProgressBar_BigWare.LinesCount := 4;
                        ProgressBar_BigWare.SetTextures(0, 928);
                        ProgressBar_BigWare.Top := Progress_BigWare.Top;
                        ProgressBar_BigWare.AddBevel := true;
                        ProgressBar_BigWare.Position := TKMHouseVineyard(aHouse).WineProgress / 5;
                        ProgressBar_BigWare.Show;

                        Progress_Beasts.Hide;
                      end;
          htShipYard: begin
                        CostsRow_Common.Top := base + line * 25 + 20;
                        CostsRow_Common.WarePlan := TShipyard(fHouse).GetWarePlan;
                        CostsRow_Common.Visible := Ship_ShipType.Visible;
                        WaresOut_ShipYard.Top := CostsRow_Common.Bottom + 15;
                        WaresOut_ShipYard.WarePlan := TShipyard(fHouse).WaresOut;
                        WaresOut_ShipYard.Show;
                        if Ship_DoWork.Visible then
                        begin
                          Ship_ShipType.TexID := gRes.Units[TKMHouseShipYard(aHouse).NextShipType].GUIIcon;
                          Ship_ShipType.Hint := gRes.Units[TKMHouseShipYard(aHouse).NextShipType].GUIName;
                          Ship_DoWork.TexID := IfThen(TKMHouseShipYard(aHouse).DoWork, 33, 32);
                          Ship_DoWork.Hint := IfThen(TKMHouseShipYard(aHouse).DoWork, gResTexts[2034], gResTexts[2033]);
                        end;
                      end;
          htTownhall: begin
                        ShowTownHall(aHouse);
                        Panel_HouseTownHall.Top := 50 + base + line * 25 + 20;
                      end;
          htPottery : begin
                        ProgressBar_BigWare.LinesCount := 2;
                        ProgressBar_BigWare.SetTextures(979, 980);
                        ProgressBar_BigWare.Top := Progress_Beasts.Top + 60;
                        ProgressBar_BigWare.Position := TKMHousePottery(aHouse).FilledClay;
                        ProgressBar_BigWare.AddBevel := false;
                        ProgressBar_BigWare.Show;

                        Pottery_ClayTitle.Top := ProgressBar_BigWare.Top - 20;
                        Pottery_ClayCount.Top := ProgressBar_BigWare.Bottom;
                        Pottery_ClayCount.Width := ProgressBar_BigWare.Width - 3;
                        Pottery_ClayCount.Caption := IntToStr(TKMHousePottery(aHouse).StoredClay) + '/' + IntToStr(TKMHousePottery(aHouse).MAX_CLAY_TO_STORE);
                        Pottery_ClayTitle.Show;
                        Pottery_ClayCount.Show;
                      end;

          htWallTower : begin
                         Button_ATBoltCount.Show;
                          Button_ATBoltCount.Top := base + line * 25;
                          Button_ATBoltCount.Caption := TKMHouseWallTower(fHouse).Bolts.ToString;
                        end;
          htCartographers : Panel_Cartographers.Show(fHouse, base + line * 25 + 76);
          htPasture : Panel_Pasture.Show(fHouse, base + line * 25 + 76);
          htForest : Panel_Forest.Show(fHouse, base + line * 25 + 76);
          htArena : Panel_Arena.Show(fHouse, base + 76);
          htSiegeTower : Panel_SiegeTower.Show(fHouse, base + 76);
      end;


      Panel_House_Common.Show;
    end;
  end;
  if Panel_House_Common.Visible then
    Panel_House_Common.SetHeightToChilds;

  if Panel_House.Visible then
    Panel_House.UpdateScrolls;
end;


procedure TKMGUIGameHouse.ShowCommonDemand(aHouse: TKMHouse; Base: Integer; var Line, RowRes: Integer);
begin
  if not aHouse.AcceptsWares then
    Exit;
  ShowCommonDemandSingle(aHouse, [1..WARES_IN_OUT_COUNT], Base, Line, RowRes);
end;

procedure TKMGUIGameHouse.ShowCommonDemandSingle(aHouse: TKMHouse; aID : TByteSet; Base: Integer; var Line, RowRes: Integer);
const MAX_WARE_EDIT_WIDTH = 75;
var
  I: Integer;
  hSpec: TKMHouseSpec;
  WT : TKMWareType;
begin
  if not aHouse.AcceptsWares then
    Exit;
  hSpec := gRes.Houses[aHouse.HouseType];
  //Show Demand
  if hSpec.AcceptsWares then
  begin
    Label_Common_Demand.Show;
    if Line = 0 then
      Label_Common_Demand.Top := Base+Line*LINE_HEIGHT+6;
    Inc(Line);

    for I := 1 to WARES_IN_OUT_COUNT do
    begin
      if I in aID then
        If aHouse.WareInput[I] in [WARE_MIN..WARE_MAX, wtFood, wtWarfare, wtValuable] then
        //if gRes.Wares[aHouse.WareInput[I]].IsValid then
        begin
          WT := aHouse.WareInput[I];

          WaresRow_Common[RowRes].Spacing := 14;
          WaresRow_Common[RowRes].Tag := I;
          WaresRow_Common[RowRes].WareCntAsNumber := (aHouse.GetMaxInWare >= 10) or (WT in [wtFood, wtWarfare, wtValuable]);
          WaresRow_Common[RowRes].ShowName := not WaresRow_Common[RowRes].WareCntAsNumber;

          WaresRow_Common[RowRes].TexID := gRes.Wares[WT].GUIIcon;


          WaresRow_Common[RowRes].Hint := gRes.Wares[WT].Title;
          WaresRow_Common[RowRes].WareCount := aHouse.CheckWareIn(aHouse.WareInput[I]);
          WaresRow_Common[RowRes].Top := Base + Line * LINE_HEIGHT;
          WaresRow_Common[RowRes].Hitable := true;
          WaresRow_Common[RowRes].HideHighlight := false;
          WaresRow_Common[RowRes].Clickable := true;
          WaresRow_Common[RowRes].MaxWares := aHouse.GetMaxInWare;

          if WaresRow_Common[RowRes].WareCntAsNumber then
          begin
            //WaresRow_Common[RowRes].Caption := IntToStr(aHouse.GetMaxInWare - aHouse.GetAcceptWareIn(WT)) +  '/'  + IntToStr(aHouse.GetMaxInWare);
            WaresRow_Common[RowRes].Left := MAX_WARE_EDIT_WIDTH;
            WaresRow_Common[RowRes].Width := TB_WIDTH - (MAX_WARE_EDIT_WIDTH);
            WaresRow_Max[RowRes].Top := WaresRow_Common[RowRes].Top;
            WaresRow_Max[RowRes].Width := MAX_WARE_EDIT_WIDTH;
            WaresRow_Max[RowRes].Value := aHouse.GetMaxInWare - aHouse.GetAcceptWareIn(WT);
            WaresRow_Max[RowRes].SetRange(0, aHouse.GetMaxInWare);
            WaresRow_Max[RowRes].Hint := Format(gResTexts[2182], [WaresRow_Max[RowRes].Value, aHouse.GetMaxInWare]);
            WaresRow_Max[RowRes].Tag := I;
            WaresRow_Max[RowRes].Show;
          end else
          begin
            WaresRow_Common[RowRes].Caption := gRes.Wares[WT].Title;
            WaresRow_Common[RowRes].Left := 0;
            WaresRow_Common[RowRes].Width := TB_WIDTH;
            WaresRow_Max[RowRes].Hide;
          end;

          WaresRow_Common[RowRes].Show;

          WaresProdCt_Common[RowRes].Top := Base + 3 + Line * LINE_HEIGHT;


          Image_WareIn_Accept[RowRes].Spacing := WaresRow_Common[RowRes].Spacing;
          Image_WareIn_Accept[RowRes].CntAsNumber := WaresRow_Common[RowRes].WareCntAsNumber;
          Image_WareIn_Accept[RowRes].MaxCount := aHouse.GetMaxInWare;
          Image_WareIn_Accept[RowRes].Top := WaresRow_Common[RowRes].Top;
          Image_WareIn_Accept[RowRes].Count := aHouse.GetAcceptWareIn(aHouse.WareInput[I]);

          if Image_WareIn_Accept[RowRes].CntAsNumber then
            Image_WareIn_Accept[RowRes].Left := WaresRow_Common[RowRes].Right - 18
          else
            Image_WareIn_Accept[RowRes].Left := WaresRow_Common[RowRes].Right - 5 * Image_WareIn_Accept[I].Spacing - 18;
          Image_WareIn_Accept[RowRes].Show;
          WaresProdCt_Common[RowRes].Caption := '';
          case aHouse.HouseType of
            htCartographers: If TKMHouseCartographers(aHouse).NeedsWare(WT) > 0 then
                              WaresProdCt_Common[RowRes].Caption := 'x' + TKMHouseCartographers(aHouse).NeedsWare(WT).ToString;
          end;
          WaresProdCt_Common[RowRes].Visible := WaresProdCt_Common[RowRes].Caption <> '';
          Inc(Line);
          Inc(RowRes);
        end;
    end;
  end;
  ToggleHouseAcceptWare(nil, []);
end;

procedure TKMGUIGameHouse.ShowCommonOutput(aHouse: TKMHouse; Base: Integer; var Line, RowRes: Integer);
var
  I, m1, m2: Integer;
  hSpec: TKMHouseSpec;
  W : TKMWareType;
begin
  hSpec := gRes.Houses[aHouse.HouseType];

  if not aHouse.ProducesWares then
    Exit;
  //Show Output
  if not hSpec.DoesOrders then
    if aHouse.HasOutput then
    begin
      Label_Common_Offer.Show;
      Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS] + ':';
      //Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS] + '(x' + IntToStr(hSpec.ResProductionX) + '):';
      Label_Common_Offer.Top := Base+Line * LINE_HEIGHT + 6;
      Inc(Line);

      for I := 1 to WARES_IN_OUT_COUNT do
        if gRes.Wares[aHouse.WareOutput[I]].IsValid then
        begin
          W := aHouse.WareOutput[I];
          WaresRow_Common[RowRes].Left := 0;
          WaresRow_Common[RowRes].Width := TB_WIDTH - 20;
          WaresRow_Common[RowRes].TexID     := gRes.Wares[W].GUIIcon;
          WaresRow_Common[RowRes].WareCount := aHouse.CheckWareOut(W);
          WaresRow_Common[RowRes].WareCntAsNumber := aHouse.GetMaxOutWare > 5;
          WaresRow_Common[RowRes].ShowName := aHouse.GetMaxOutWare <= 5;
          WaresRow_Common[RowRes].Caption   := gRes.Wares[W].Title;
          WaresRow_Common[RowRes].Hint      := gRes.Wares[W].Title;
          WaresRow_Common[RowRes].Top       := Base + Line * LINE_HEIGHT;
          WaresRow_Common[RowRes].Hitable := true;
          WaresRow_Common[RowRes].HideHighlight := false;
          WaresRow_Common[RowRes].Clickable := true;
          WaresRow_Common[RowRes].Show;

          if aHouse.WareOutput[I] = aHouse.WareInput[I] then
          begin
            Button_TransferWare[RowRes].Top := Base + Line * LINE_HEIGHT;
            Button_TransferWare[RowRes].Tag := byte(W);
            Button_TransferWare[RowRes].TexID := IfThen(aHouse.TransferWare[I], 718, 717);
            Button_TransferWare[RowRes].Hint := IfThen(aHouse.TransferWare[I], gResTexts[1717], gResTexts[1716]);
            Button_TransferWare[RowRes].Show;
          end;

          m1 := gRes.Wares[W].MinProduction(aHouse.HouseType);
          m2 := gRes.Wares[W].MaxProduction(aHouse.HouseType);
          if m1 = m2 then
            WaresProdCt_Common[RowRes].Caption := 'x'+IntToStr(m1)
          else
            WaresProdCt_Common[RowRes].Caption := IntToStr(m1) + '-' + IntToStr(m2);

          case aHouse.HouseType of
            htHovel : If W = wtFeathers then WaresProdCt_Common[RowRes].Caption := '' else
                      If W = wtSausage then WaresProdCt_Common[RowRes].Caption := 'x' + IntToStr(ord(TKMHouseHovel(aHouse).GetsSausage)) else
                      If W = wtEgg then WaresProdCt_Common[RowRes].Caption := 'x' + IntToStr(Trunc(TKMHouseHovel(aHouse).EggsProgress));
          end;
          WaresProdCt_Common[RowRes].Top       := Base + 3 + Line * LINE_HEIGHT;
          WaresProdCt_Common[RowRes].Show;

          Inc(Line);
          Inc(RowRes);
        end;

      Label_DepletedMsg.Top := Base + Line * LINE_HEIGHT + 5;
      Label_DepletedMsg.Visible := aHouse.ResourceDepleted;
      if aHouse.ResourceDepleted then
        Label_DepletedMsg.Caption := gResTexts[aHouse.GetResourceDepletedMessageId];
    end;
end;


procedure TKMGUIGameHouse.ShowCommonOrders(aHouse: TKMHouse; Base: Integer; var Line, RowRes: Integer);
var
  I: Integer;
  W: TKMWareType;
begin
  //Show Orders
  if gRes.Houses[aHouse.HouseType].DoesOrders then
  begin
    Label_Common_Offer.Show;
    Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS] + ':';
    //Label_Common_Offer.Caption := gResTexts[TX_HOUSE_DELIVERS] + '(x' + IntToStr(gRes.Houses[aHouse.HouseType].ResProductionX) + '):';
    Label_Common_Offer.Top:=Base + Line * LINE_HEIGHT + 6;
    Inc(Line);
    for I := 1 to WARES_IN_OUT_COUNT do //Orders
    begin
      W := aHouse.WareOutput[I];
      if gRes.Wares[W].IsValid then
      begin
        WareOrderRow_Order[I].WareRow.TexID := gRes.Wares[W].GUIIcon;
        WareOrderRow_Order[I].WareRow.Caption := gRes.Wares[W].Title;
        WareOrderRow_Order[I].Hint := gRes.Wares[W].Title;
        WareOrderRow_Order[I].WareRow.Hint := gRes.Wares[W].Title;
        WareOrderRow_Order[I].WareRow.WareCount := aHouse.CheckWareOut(W);
        WareOrderRow_Order[I].WareRow.WareCntAsNumber := aHouse.GetMaxOutWare > 5;
        WareOrderRow_Order[I].WareRow.ShowName := aHouse.GetMaxOutWare <= 5;
        WareOrderRow_Order[I].OrderCount := aHouse.WareOrder[I];
        WareOrderRow_Order[I].WareRow.Hitable := true;
        WareOrderRow_Order[I].WareRow.Clickable := true;
        WareOrderRow_Order[I].WareRow.HideHighlight := false;
        WareOrderRow_Order[I].Show;
        WareOrderRow_Order[I].Top := Base + Line * LINE_HEIGHT;
        WareOrderRow_Order[I].Orderable := true;
        WareOrderRow_Order[I].ReplaceCaption := IfThen(fHouse.WareOrder[I] = 1000, #1, '');
        WareOrderRow_Order[I].AsCheckBox := fHouse.HouseType in [htProductionThatch, htMetallurgists];
        if W = wtSawDust then
          if not (aHouse.HouseType in [htStoneWorkshop, htProductionThatch]) then
            WareOrderRow_Order[I].Orderable := false;

        Inc(Line);
      end;
    end;
    ShowCommonCost(aHouse, Base, Line, RowRes)
  end;
end;

procedure TKMGUIGameHouse.ShowCommonCost(aHouse: TKMHouse; Base: Integer; var Line, RowRes: Integer);
var
  I, m1, m2, lastLine: Integer;
  W: TKMWareType;
  Warr : TKMWareTypeArray;
begin
  Label_Common_Costs.Hide;
  Label_Common_Costs.Top := Base + Line * LINE_HEIGHT + 2;
  Inc(Line);
  lastLine := Line;
  for I := 1 to WARES_IN_OUT_COUNT do //Costs
  begin
    W := aHouse.WareOutput[I];
    if gRes.Wares[W].IsValid then
    begin
      if W = wtSawDust then
        if not (aHouse.HouseType in [htStoneWorkshop, htProductionThatch]) then
        begin
          CostsRow_Costs[I].Visible := false;
          Continue;
        end
        else
          CostsRow_Costs[I].Visible := true;

      m1 := gRes.Wares[W].MinProduction(aHouse.HouseType);
      m2 := gRes.Wares[W].MaxProduction(aHouse.HouseType);

      CostsRow_Costs[I].Caption := gRes.Wares[W].Title;
      if m1 = m2 then
        CostsRow_Costs[I].Caption := CostsRow_Costs[I].Caption + ' x' + IntToStr(m1)
      else
        CostsRow_Costs[I].Caption := CostsRow_Costs[I].Caption + ' x' + IntToStr(m1) + '-' + IntToStr(m2);


      CostsRow_Costs[I].RX := rxGui;
      //Hide the icons when they are not used

      Warr := gRes.Wares[W].OrderCost;
      if Warr[0] in [wtAll, wtNone, wtWarfare] then
      begin
        CostsRow_Costs[I].Hide;
        Continue;
      end;
      IF (W = wtMace) and gHands[aHouse.Owner].EconomyDevUnlocked(20) then
        SetLength(Warr, high(Warr));
      IF (W = wtPlateArmor) and gHands[aHouse.Owner].EconomyDevUnlocked(21) then
        SetLength(Warr, high(Warr));
      Label_Common_Costs.Show;
      CostsRow_Costs[I].TexArr := gRes.Wares.WaresArrToIconArr(Warr);

      CostsRow_Costs[I].Show;
      CostsRow_Costs[I].Top := Base + lastLine * LINE_HEIGHT + (Line - LastLine) * 42{ + (I - 1) * 42}; //Pack them closer so they fit on 1024x576
      Inc(Line);
    end;

  end;

end;


procedure TKMGUIGameHouse.ShowTownHall(aHouse: TKMHouse);
begin
  Assert(aHouse is TKMHouseTownHall);

  House_TH_UnitChange(nil, []);
end;

function TKMGUIGameHouse.HasAnyUnit(unitArr: array of TKMUnitType): Boolean;
var UT : TKMUnitType;
begin
  Result := false;
  for UT in unitArr do
    if gMySpectator.Hand.Locks.GetUnitBlocked(UT, fHouse.HouseType) <> ulNotVisible then
      Exit(true);

end;

function TKMGUIGameHouse.HasAnyUnit(unitArr: array of TKMUnitType; OnlyVisible: Boolean): Boolean;
var UT : TKMUnitType;
begin
  Result := false;
  for UT in unitArr do
    if gMySpectator.Hand.Locks.GetUnitBlocked(UT, fHouse.HouseType) = ulUnlocked then
      Exit(true);

end;

function TKMGUIGameHouse.NextUnit(startFrom : Byte; unitArr : array of TKMUnitType; toLast : Boolean = false) : Integer;
var K : Integer;
begin
  Result := -1;

  if startFrom >= High(unitArr) then
    Exit;

  for K := startFrom + 1 to High(unitArr) do
    if gMySpectator.Hand.Locks.GetUnitBlocked(unitArr[K], fHouse.HouseType) = ulNotVisible then
      Continue
    else
    begin
      Result := K;
      if not toLast then
        Exit;
    end;

end;

function TKMGUIGameHouse.PreviousUnit(startFrom : Byte; unitArr : array of TKMUnitType; toLast : Boolean = false) : Integer;
var K : Integer;
begin
  Result := -1;
  if startFrom = low(unitArr) then
    Exit;
  for K :=  startFrom - 1 downto 0 do
    if gMySpectator.Hand.Locks.GetUnitBlocked(unitArr[K], fHouse.HouseType) = ulNotVisible then
      Continue
    else
    begin
      Result := K;
      if not toLast then
        Exit;
    end;

end;

procedure TKMGUIGameHouse.CheckLastSelected(var A : Byte; unitArr : array of TKMUnitType);
begin
  If gHands[fHouse.Owner].Locks.GetUnitBlocked(unitArr[A], fHouse.HouseType) = ulNotVisible then
    A := PreviousUnit(length(unitArr), unitArr, true);
end;

procedure TKMGUIGameHouse.UpdateHotkeys;
begin
  // School
  Button_School_Left.Hint := GetHintWHotkey(TX_HOUSE_SCHOOL_PREV_HINT, kfTrainGotoPrev);
  Button_School_Right.Hint := GetHintWHotkey(TX_HOUSE_SCHOOL_NEXT_HINT, kfTrainGotoNext);

  if gMySpectator.Hand.Locks.GetUnitBlocked(SCHOOL_GAME_ORDER[fLastSchoolUnit], htAny) = ulUnlocked then
    Button_School_Train.Hint := GetHintWHotkey(TX_HOUSE_SCHOOL_TRAIN_HINT, kfTrainEquipUnit)
  else
    Button_School_Train.Hint := gResTexts[TX_HOUSE_SCHOOL_TRAIN_DISABLED_HINT];

  // Barracks
  Button_Barracks_Left.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_PREV_HINT, kfTrainGotoPrev);
  Button_Barracks_Right.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_NEXT_HINT, kfTrainGotoNext);

  if gMySpectator.Hand.Locks.GetUnitBlocked(BARRACKS_GAME_ORDER[fLastBarracksUnit], htAny) = ulUnlocked then
    Button_Barracks_Train.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_TRAIN_HINT, kfTrainEquipUnit)
  else
    Button_Barracks_Train.Hint := gResTexts[TX_HOUSE_BARRACKS_TRAIN_DISABLED_HINT];

  // Townhall
  Button_TH_Left.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_PREV_HINT, kfTrainGotoPrev);
  Button_TH_Right.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_NEXT_HINT, kfTrainGotoNext);

  if gMySpectator.Hand.Locks.GetUnitBlocked(TH_GAME_ORDER[fLastTHUnit], htAny) = ulUnlocked then
    Button_TH_Train.Hint := GetHintWHotkey(TX_HOUSE_BARRACKS_TRAIN_HINT, kfTrainEquipUnit)
  else
    Button_TH_Train.Hint := gResTexts[TX_HOUSE_BARRACKS_TRAIN_DISABLED_HINT];
end;

function TKMGUIGameHouse.GetEquipAmount(Shift: TShiftState): Integer;
begin
  Result := Min(GetMultiplicator(Shift), MAX_UNITS_TO_EQUIP);
end;

procedure TKMGUIGameHouse.HouseSignClose(sender : TObject);
begin
  gMySpectator.Selected := nil;
  Panel_HouseSign.Hide;
end;

function TKMGUIGameHouse.Visible: Boolean;
begin
  Result := Panel_House.Visible;
end;


procedure TKMGUIGameHouse.Hide;
begin
  Panel_House.Hide;
end;


procedure TKMGUIGameHouse.House_Demolish(Sender: TObject; Shift: TShiftState);
begin
  if (gMySpectator.Selected = nil) or not (gMySpectator.Selected is TKMHouse) then
    Exit;

  if Sender = Button_House_DemolishYes then
  begin
    if TKMHouse(gMySpectator.Selected).HouseType = htWall2 then
      gGame.GameInputProcess.CmdBuild(gicBuildRemoveHouse, KMPoint(TKMHouse(gMySpectator.Selected).Position.X + 1, TKMHouse(gMySpectator.Selected).Position.Y))
    else
      gGame.GameInputProcess.CmdBuild(gicBuildRemoveHouse, TKMHouse(gMySpectator.Selected).Position);

    gMySpectator.Selected := nil; //fPlayers.Selected MUST be reset before calling ShowHouseInfo
    Panel_House.Hide; //Simpliest way to reset page and ShownHouse
  end;

  AskDemolish := False;
  OnHouseDemolish(Sender, Shift); //Return to build menu
end;


procedure TKMGUIGameHouse.House_RepairToggle(Sender: TObject);
begin
  if (gMySpectator.Selected = nil) or not (gMySpectator.Selected is TKMHouse) then Exit;

  gGame.GameInputProcess.CmdHouse(gicHouseRepairToggle, TKMHouse(gMySpectator.Selected));
  Button_HouseRepair.TexID := IfThen(TKMHouse(gMySpectator.Selected).BuildingRepair, 39, 40);
end;


procedure TKMGUIGameHouse.House_DeliveryModeToggle(Sender: TObject; Shift: TShiftState);
begin
  if (gMySpectator.Selected = nil)
    or not (gMySpectator.Selected is TKMHouse)
    or not gMySpectator.IsSelectedMyObj then
    Exit;

  if ssLeft in Shift then
    gGame.GameInputProcess.CmdHouse(gicHouseDeliveryModeNext, TKMHouse(gMySpectator.Selected))
  else if ssRight in Shift then
    gGame.GameInputProcess.CmdHouse(gicHouseDeliveryModePrev, TKMHouse(gMySpectator.Selected));
end;


procedure TKMGUIGameHouse.House_ClosedForWorkerToggle(Sender: TObject; Shift: TShiftState);
var
  house: TKMHouse;
  UT : TKMUnitType;
  I : Integer;
begin
  if (gMySpectator.Selected = nil) or not (gMySpectator.Selected is TKMHouse)
    or (gMySpectator.Selected is TKMHouseBarracks) then Exit;
  If ssShift in Shift then
  begin
    UT := gRes.Houses[fHouse.HouseType].WorkerType;
    if (UT in UNITS_VALID) and gMySpectator.Hand.Locks.UnitUnlocked(UT, htSchool) then
      If gHands[fHouse.Owner].Stats.GetHouseQty(htSchool) > 0 then
      begin
        house := gHands[fHouse.Owner].FindHouse(htSchool, 1);
        if not house.IsValid(htSchool, false, true) then
          Exit;
        for I := 0 to High(SCHOOL_GAME_ORDER) do
          If SCHOOL_GAME_ORDER[I] = UT then
          begin
            fLastSchoolUnit := I;
          end;

        gMySpectator.Selected := house;
        gMySpectator.UpdateNewSelected;
        If Assigned(fSetViewportEvent) then
          fSetViewportEvent(KMPointF(house.Entrance));
      end;

    Exit;
  end;
  house := TKMHouse(gMySpectator.Selected);
  
  gGame.GameInputProcess.CmdHouse(gicHouseClosedForWorkerTgl, house);

  HandleHouseClosedForWorker(house);
end;


procedure TKMGUIGameHouse.HandleHouseClosedForWorker(aHouse: TKMHouse);
begin
  if aHouse.IsClosedForWorker and (aHouse.HSpec.CanHasWorker or (aHouse.HouseType in [htSiegeTower])) then
  begin
    Button_House_Worker.ShowImageEnabled := False;
    Image_House_Worker_Closed.Show;
  end else begin
    Button_House_Worker.ShowImageEnabled := aHouse.HasWorker;
    Image_House_Worker_Closed.Hide;
  end;
end;


procedure TKMGUIGameHouse.HouseLogo_Click(Sender: TObject; Shift: TShiftState);
var
  H: TKMHouse;
begin
  if not (gMySpectator.Selected is TKMHouse) then Exit;

  H := TKMHouse(gMySpectator.Selected);
  if not H.IsDestroyed then
  begin
    If ssShift in Shift then
    begin
      gGame.GamePlayInterface.ShowGuide(H.HouseType);
      Exit;
    end;
    if Assigned(fSetViewportEvent) then
    begin
      gMySpectator.Highlight := H;
      fSetViewportEvent(KMPointF(H.Entrance));
    end;
  end;
end;


procedure TKMGUIGameHouse.House_OrderChange(Sender: TObject; aValue: Integer);
var
  I: Integer;
  H: TKMHouse;
begin
  if not (gMySpectator.Selected is TKMHouse) then Exit;

  H := TKMHouse(gMySpectator.Selected);

  for I := 1 to WARES_IN_OUT_COUNT do
    if (Sender = WareOrderRow_Order[I]) then
      gGame.GameInputProcess.CmdHouse(gicHouseOrderProduct, H, I, aValue);
end;


procedure TKMGUIGameHouse.House_WoodcutterClick(Sender: TObject; Shift: TShiftState);
begin
  if Sender <> Button_Woodcutter then Exit;

  if ssLeft in Shift then
    Radio_Woodcutter.ItemIndex := (Radio_Woodcutter.ItemIndex + 1) mod 3 //Cycle
  else
  if ssRight in Shift then
    Radio_Woodcutter.ItemIndex := (Radio_Woodcutter.ItemIndex + 3 - 1) mod 3; //Cycle reverse

  House_WoodcutterChange( Button_Woodcutter );
end;


procedure TKMGUIGameHouse.House_WoodcutterChange(Sender: TObject);
var
  W: TKMHouseWoodcutters;
  wMode: TKMWoodcutterMode;
begin
  W := TKMHouseWoodcutters(gMySpectator.Selected);

  if (Sender = Button_Woodcutter) or (Sender = Radio_Woodcutter) then
  begin
    if Radio_Woodcutter.ItemIndex = 0 then
      wMode := wmChopAndPlant
    else if Radio_Woodcutter.ItemIndex = 1 then
      wMode := wmChop
    else
      wMode := wmPlant;
    gGame.GameInputProcess.CmdHouse(gicHouseWoodcutterMode, W, wMode);
  end;

  case W.WoodcutterMode of
    wmChopAndPlant: begin
                      Button_Woodcutter.TexID := 310;
                      Button_Woodcutter.RX := rxGui;
                      Radio_Woodcutter.ItemIndex := 0;
                    end;
    wmChop:         begin
                      Button_Woodcutter.TexID := 51;
                      Button_Woodcutter.RX := rxGui;
                      Radio_Woodcutter.ItemIndex := 1;
                    end;
    wmPlant:        begin
                      Button_Woodcutter.TexID := 666;
                      Button_Woodcutter.RX := rxGui;
                      Radio_Woodcutter.ItemIndex := 2;
                    end;
  end;
end;


procedure TKMGUIGameHouse.House_FarmClick(Sender: TObject; Shift: TShiftState);
begin
  if Sender <> Button_Farm then Exit;

  if ssLeft in Shift then
    Radio_Farm.ItemIndex := (Radio_Farm.ItemIndex + 1) mod 3 //Cycle
  else
  if ssRight in Shift then
    Radio_Farm.ItemIndex := (Radio_Farm.ItemIndex + 3 - 1) mod 3; //Cycle reverse

  House_FarmChange( Button_Farm );
end;


procedure TKMGUIGameHouse.House_FarmChange(Sender: TObject);
var
  W: TKMHouseFarm;
  wMode: TKMWoodcutterMode;
begin
  W := TKMHouseFarm(gMySpectator.Selected);

  if (Sender = Button_Farm) or (Sender = Radio_Farm) then
  begin
    if Radio_Farm.ItemIndex = 0 then
      wMode := wmChopAndPlant
    else if Radio_Farm.ItemIndex = 1 then
      wMode := wmChop
    else
      wMode := wmPlant;
    gGame.GameInputProcess.CmdHouse(gicHouseFarmMode, W, wMode);
  end;

  case W.Mode of
    wmChopAndPlant: begin
                      Button_Farm.TexID := 956;
                      Button_Farm.RX := rxGui;
                      Radio_Farm.ItemIndex := 0;
                    end;
    wmChop:         begin
                      Button_Farm.TexID := 957;
                      Button_Farm.RX := rxGui;
                      Radio_Farm.ItemIndex := 1;
                    end;
    wmPlant:        begin
                      Button_Farm.TexID := 958;
                      Button_Farm.RX := rxGui;
                      Radio_Farm.ItemIndex := 2;
                    end;
  end;
end;


procedure TKMGUIGameHouse.KeyDown(Key: Word; aShift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;
  Exit;
  // Hotkey press is equal to click with LMB
  Include(aShift, ssLeft);

  //Prev unit
  if Key = gResKeys[kfTrainGotoPrev] then
  begin
    if Panel_House_School.Visible and Button_School_Left.Enabled then
    begin
      House_SchoolUnitChange(Button_School_Left, aShift);
      aHandled := True;
    end;

    if Panel_House_Siege.Visible and Button_Siege_Left.Enabled then
    begin
      House_SchoolUnitChange(Button_Siege_Left, aShift);
      aHandled := True;
    end;

    if Panel_HouseBarracks.Visible and Button_Barracks_Left.Enabled then
    begin
      House_BarracksUnitChange(Button_Barracks_Left, aShift);
      aHandled := True;
    end;

    if Panel_HouseTownHall.Visible and Button_TH_Left.Enabled then
    begin
      House_TH_UnitChange(Button_TH_Left, aShift);
      aHandled := True;
    end;
  end;

  //Next unit
  if Key = gResKeys[kfTrainGotoNext] then
  begin
    if Panel_House_School.Visible and Button_School_Right.Enabled then
    begin
      House_SchoolUnitChange(Button_School_Right, aShift);
      aHandled := True;
    end;
    if Panel_House_Siege.Visible and Button_Siege_Right.Enabled then
    begin
      House_SchoolUnitChange(Button_Siege_Right, aShift);
      aHandled := True;
    end;

    if Panel_HouseBarracks.Visible and Button_Barracks_Right.Enabled then
    begin
      House_BarracksUnitChange(Button_Barracks_Right, aShift);
      aHandled := True;
    end;

    if Panel_HouseTownHall.Visible and Button_TH_Right.Enabled then
    begin
      House_TH_UnitChange(Button_TH_Right, aShift);
      aHandled := True;
    end;
  end;

  //Hotkey for train / equip button
  if Key = gResKeys[kfTrainEquipUnit] then
  begin
    if Panel_House_School.Visible and Button_School_Train.Enabled then
    begin
      House_SchoolUnitChange(Button_School_Train, aShift);
      aHandled := True;
    end;

    if Panel_House_Siege.Visible and Button_Siege_Train.Enabled then
    begin
      House_SchoolUnitChange(Button_Siege_Train, aShift);
      aHandled := True;
    end;

    if Panel_HouseBarracks.Visible and Button_Barracks_Train.Enabled then
    begin
      House_BarracksUnitChange(Button_Barracks_Train, aShift);
      aHandled := True;
    end;

    if Panel_HouseTownHall.Visible and Button_TH_Train.Enabled then
    begin
      House_TH_UnitChange(Button_TH_Train, aShift);
      aHandled := True;
    end;
  end;
end;


procedure TKMGUIGameHouse.House_BarracksUnitChange(Sender: TObject; Shift: TShiftState);
var
  I, K, tmp: Integer;
  barracks: TKMHouseBarracks;
  UT : TKMUnitType;
  W : TKMWareType;
begin
  if gMySpectator.Selected = nil then
    Exit;
  if not (gMySpectator.Selected is TKMHouseBarracks) then
    Exit;

  barracks := TKMHouseBarracks(gMySpectator.Selected);

  //Update graphics owner color
  Button_BarracksRecruit.Show;
  Button_House_Worker.Hide; //In the barrack the recruit icon is always enabled
  Image_Barracks_Left.FlagColor := gHands[barracks.Owner].FlagColor;
  Image_Barracks_Right.FlagColor := gHands[barracks.Owner].FlagColor;
  Image_Barracks_Train.FlagColor := gHands[barracks.Owner].FlagColor;
  Button_BarracksRecruit.FlagColor := gHands[barracks.Owner].FlagColor;

  //Supply
  for I := 0 to high(Button_Barracks) do
    begin
      W := BarracksResOrder[Button_Barracks[I].Tag];
      tmp := barracks.CheckWareIn(W);

      Button_Barracks[I].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp, 1000));
      //Set highlights
      Button_Barracks[I].Down := False;
      UT := BARRACKS_GAME_ORDER[fLastBarracksUnit];
      for K := 0 to High(gRes.Units[UT].BarracksCost) do
        if W = gRes.Units[UT].BarracksCost[K].W then
        begin
          Button_Barracks[I].Down := True;
          if tmp < gRes.Units[UT].BarracksCost[K].C then
            Button_Barracks[I].DownColor := $FF0707FF
          else
            Button_Barracks[I].DownColor := $FFFFFFFF;

        end;

      if W = wtBitinArmor then
        if BARRACKS_GAME_ORDER[fLastBarracksUnit] in WARRIOR_BITIN_EQUIPABLE then
          if tmp > 0 then
            Button_Barracks[I].Down := true;

      if W = wtQuiver then
        if TKMUnitSpec.IsRanged(BARRACKS_GAME_ORDER[fLastBarracksUnit]) then
          if tmp > 0 then
            Button_Barracks[I].Down := true;

      if W = wtBoots then
        if tmp > 0 then
          Button_Barracks[I].Down := true;

      Image_Barracks_NotAccept[I].Visible := barracks.WareAccepted(W);
      Image_Barracks_NotAllowTakeOut[I].Visible := barracks.WareAllowedToTakeOut(W);
    end;

  tmp := barracks.RecruitsCount;
  Button_BarracksRecruit.Caption := IfThen(tmp = 0, '-', IntToKStr(tmp));
  Button_BarracksRecruit.Down := True; //Recruit is always enabled, all troops require one

  if barracks.RecruitsCount > 0 then
    Button_BarracksRecruit.DownColor := $FFFFFFFF
  else
    Button_BarracksRecruit.DownColor := $FF0707FF;


  Image_Barracks_NotAcceptRecruit.Visible := barracks.RecruitAccepted;

  if HasAnyUnit(BARRACKS_GAME_ORDER) then
  begin
    if (Sender = Button_Barracks_Left) then
      fLastBarracksUnit := PreviousUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER, IsRMBInShiftState(Shift) );
    if (Sender = Button_Barracks_Right) then
      fLastBarracksUnit := NextUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER, IsRMBInShiftState(Shift) );

    if Sender = Button_Barracks_Train then //Equip unit
      gGame.GameInputProcess.CmdHouse(gicHouseBarracksEquip, barracks, BARRACKS_GAME_ORDER[fLastBarracksUnit], GetEquipAmount(Shift));
    Button_Barracks_Train.Show;
    Button_Barracks_Train.Enabled := not gGame.IsPeaceTime and barracks.CanEquip(BARRACKS_GAME_ORDER[fLastBarracksUnit]);

    Button_Barracks_Left.Disable;
    Button_Barracks_Right.Disable;
    Image_Barracks_Left.Hide;
    Image_Barracks_Right.Hide;

    if PreviousUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER ) > -1 then
    begin
      Image_Barracks_Left.TexID := gRes.Units[BARRACKS_GAME_ORDER[PreviousUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER )]].GUIScroll;
      Button_Barracks_Left.Enable;
      Image_Barracks_Left.Show;
    end;
    Image_Barracks_Train.Show;
    Image_Barracks_Train.TexID := gRes.Units[BARRACKS_GAME_ORDER[fLastBarracksUnit]].GUIScroll;
    Label_Barracks_Unit.Caption := gRes.Units[BARRACKS_GAME_ORDER[fLastBarracksUnit]].GUIName;

    Image_Barracks_Train.Enabled := gMySpectator.Hand.Locks.GetUnitBlocked(BARRACKS_GAME_ORDER[fLastBarracksUnit], fHouse.HouseType) = ulUnlocked;

    if fLastBarracksUnit < High(BARRACKS_GAME_ORDER) then
      Image_Barracks_Right.TexID := gRes.Units[BARRACKS_GAME_ORDER[fLastBarracksUnit + 1]].GUIScroll;

    if NextUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER ) > -1 then
    begin
      Image_Barracks_Right.TexID := gRes.Units[BARRACKS_GAME_ORDER[NextUnit(fLastBarracksUnit, BARRACKS_GAME_ORDER )]].GUIScroll;
      Button_Barracks_Right.Enable;
      Image_Barracks_Right.Show;
    end;
  end else
  begin
    Button_Barracks_Left.Hide;
    Button_Barracks_Right.Hide;
    Button_Barracks_Train.Hide;
    Image_Barracks_Train.Hide;
    Image_Barracks_Left.Hide;
    Image_Barracks_Right.Hide;
  end;
end;

procedure TKMGUIGameHouse.House_TH_UnitChange(Sender: TObject; Shift: TShiftState);
var
  townHall: TKMHouseTownhall;
begin
  if gMySpectator.Selected = nil then Exit;
  if not (gMySpectator.Selected is TKMHouseTownhall) then Exit;

  townHall := TKMHouseTownHall(gMySpectator.Selected);

  //Update graphics owner color
  Button_House_Worker.Hide; //In the townhall the worker button is always hidden
  Image_TH_Left.FlagColor := gHands[townHall.Owner].FlagColor;
  Image_TH_Right.FlagColor := gHands[townHall.Owner].FlagColor;
  Image_TH_Train.FlagColor := gHands[townHall.Owner].FlagColor;

  if not HasAnyUnit(TH_GAME_ORDER) then
  begin
    Panel_HouseTownHall.Hide;
    Exit;
  end;
  if (Sender = Button_TH_Left) then
    fLastTHUnit := PreviousUnit(fLastTHUnit, TH_GAME_ORDER, IsRMBInShiftState(Shift) );
  if (Sender = Button_TH_Right) then
    fLastTHUnit := NextUnit(fLastTHUnit, TH_GAME_ORDER, IsRMBInShiftState(Shift) );

  if Sender = Button_TH_Train then //Equip unit
    gGame.GameInputProcess.CmdHouse(gicHouseTownHallEquip, townHall, TH_GAME_ORDER[fLastTHUnit], GetEquipAmount(Shift));

  Button_TH_Train.Enabled := not gGame.IsPeaceTime and townHall.CanEquip(TH_GAME_ORDER[fLastTHUnit]);
  Button_TH_Left.Disable;
  Button_TH_Right.Disable;
  Image_TH_Left.Hide;
  Image_TH_Right.Hide;

  Image_TH_Train.TexID := gRes.Units[TH_GAME_ORDER[fLastTHUnit]].GUIScroll;
  Label_TH_Unit.Caption := gRes.Units[TH_GAME_ORDER[fLastTHUnit]].GUIName;

  Image_TH_Train.Enabled := gMySpectator.Hand.Locks.GetUnitBlocked(TH_GAME_ORDER[fLastTHUnit], fHouse.HouseType) = ulUnlocked;

  if gMySpectator.Hand.Locks.GetUnitBlocked(TH_GAME_ORDER[fLastTHUnit], fHouse.HouseType) = ulUnlocked then
    Button_TH_Train.Hint := gResTexts[TX_HOUSE_BARRACKS_TRAIN_HINT]
  else
    Button_TH_Train.Hint := gResTexts[TX_HOUSE_BARRACKS_TRAIN_DISABLED_HINT];

  if PreviousUnit(fLastTHUnit, TH_GAME_ORDER ) > -1 then
  begin
    Image_TH_Left.TexID := gRes.Units[TH_GAME_ORDER[PreviousUnit(fLastTHUnit, TH_GAME_ORDER )]].GUIScroll;
    Image_TH_Left.Show;
    Button_TH_Left.Enable;
  end;

  if NextUnit(fLastTHUnit, TH_GAME_ORDER ) > -1 then
  begin
    Image_TH_Right.TexID := gRes.Units[TH_GAME_ORDER[NextUnit(fLastTHUnit, TH_GAME_ORDER )]].GUIScroll;
    Image_TH_Right.Show;
    Button_TH_Right.Enable;
  end;
  CostsRow_TH_Cost.Count := townHall.UnitCost(TH_GAME_ORDER[fLastTHUnit]);
  CostsRow_TH_Cost.Caption := CostsRow_TH_Cost.Count.ToString;

  Panel_HouseTownHall.Show;
end;


// Process click on Left-Train-Right buttons of School
procedure TKMGUIGameHouse.House_SchoolUnitChange(Sender: TObject; Shift: TShiftState);
var
  I: Integer;
  school: TKMHouseSchool;
  siege: TKMHouseSiegeWorkshop;
begin

  if gMySpectator.Selected = nil then
    Exit;

  if (gMySpectator.Selected is TKMHouseSchool) then
  begin
    school := TKMHouseSchool(gMySpectator.Selected);

    if not HasAnyUnit(SCHOOL_GAME_ORDER) then
    begin
     Panel_House_School.Hide;
     Exit;
    end;
    if (Sender = Button_School_Left) then
      fLastSchoolUnit := PreviousUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER, IsRMBInShiftState(Shift));

    if (Sender = Button_School_Right) then
      fLastSchoolUnit := NextUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER, IsRMBInShiftState(Shift));

    {if (Sender = Button_School_Left) and (fLastSchoolUnit > 0) then
      fLastSchoolUnit := PreviousUnit(fLastSchoolUnit, School_Order);
      //Dec(fLastSchoolUnit);
    if (Sender = Button_School_Right) and (fLastSchoolUnit < High(School_Order)) then
      fLastSchoolUnit := NextUnit(fLastSchoolUnit, School_Order);
      //Inc(fLastSchoolUnit); }

    if Sender = Button_School_Train then
    begin
      // Right click - fill queue with same units
      if IsRMBInShiftState(Shift) then
        gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrain, school, SCHOOL_GAME_ORDER[fLastSchoolUnit], 10)
      else
      if (ssLeft in Shift) then
      begin
        // Left click - add Unit to queue
        gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrain, school, SCHOOL_GAME_ORDER[fLastSchoolUnit], 1);
        // If Ctrl is also pressed, then change last unit order to 0
        if SCHOOL_CH_ORDER_TO_0_SHIFT in Shift then
          gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrainChLastUOrder, school, 0)
        // else If Alt is also pressed, then change last unit order to 1
        else if SCHOOL_CH_ORDER_TO_1_SHIFT in Shift then
          gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrainChLastUOrder, school, 1);
      end;
    end;

    if school.Queue[0] <> utNone then
      Button_School_UnitWIP.TexID := gRes.Units[school.Queue[0]].GUIIcon
    else
      Button_School_UnitWIP.TexID := 41; //Question mark

    Button_School_UnitWIPBar.Position := school.GetTrainingProgress;

    for I := 1 to 5 do
      if school.Queue[I] <> utNone then
      begin
        Button_School_UnitPlan[I].TexID := gRes.Units[school.Queue[I]].GUIIcon;
        Button_School_UnitPlan[I].Hint := gRes.Units[school.Queue[I]].GUIName;
      end
      else
      begin
        Button_School_UnitPlan[I].TexID:=0;
        Button_School_UnitPlan[I].Hint:='';
      end;

    Button_School_Train.Enabled := (not school.QueueIsFull)
                                    and (gMySpectator.Hand.Locks.GetUnitBlocked(SCHOOL_GAME_ORDER[fLastSchoolUnit], fHouse.HouseType) = ulUnlocked  );

    //if School_Order[fLastSchoolUnit] in [utSerf, utBuilder] then
    //   Button_School_Train.Enabled := Button_School_Train.Enabled and (gHands[School.Owner].CanTrainSerfs > 0);

    Button_School_Left.Disable;
    Button_School_Right.Disable;
    Image_School_Left.Hide;
    Image_School_Right.Hide;

    //if fLastSchoolUnit > 0 then
    if PreviousUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER) > -1 then
    begin
      Image_School_Left.TexID := gRes.Units[SCHOOL_GAME_ORDER[PreviousUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER)]].GUIScroll;
      Image_School_Left.Show;
      Button_School_Left.Enable
    end;

    Label_School_Unit.Caption := gRes.Units[SCHOOL_GAME_ORDER[fLastSchoolUnit]].GUIName;
    Image_School_Train.TexID := gRes.Units[SCHOOL_GAME_ORDER[fLastSchoolUnit]].GUIScroll;

    Image_School_Train.Enabled := gMySpectator.Hand.Locks.GetUnitBlocked(SCHOOL_GAME_ORDER[fLastSchoolUnit], fHouse.HouseType) = ulUnlocked;

    //if fLastSchoolUnit < High(School_Order) then
    if NextUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER) > -1 then
    begin
      Image_School_Right.TexID := gRes.Units[SCHOOL_GAME_ORDER[NextUnit(fLastSchoolUnit, SCHOOL_GAME_ORDER)]].GUIScroll;
      Image_School_Right.Show;
      Button_School_Right.Enable;
    end;
    Panel_House_School.Show;
  end else
  if (gMySpectator.Selected is TKMHouseSiegeWorkshop) then
  begin
    if not HasAnyUnit(SIEGE_GAME_ORDER) then
    begin
     Panel_House_Siege.Hide;
     Exit;
    end;
    siege := TKMHouseSiegeWorkshop(gMySpectator.Selected);


    if (Sender = Button_Siege_Left) then
      fLastSiegeUnit := PreviousUnit(fLastSiegeUnit, SIEGE_GAME_ORDER, IsRMBInShiftState(Shift));

    if (Sender = Button_Siege_Right) then
      fLastSiegeUnit := NextUnit(fLastSiegeUnit, SIEGE_GAME_ORDER, IsRMBInShiftState(Shift));

    if Sender = Button_Siege_Train then
    begin
      // Right click - fill queue with same units
      if IsRMBInShiftState(Shift) then
        gGame.GameInputProcess.CmdHouse(gicHouseSiegeTrain, siege, SIEGE_GAME_ORDER[fLastSiegeUnit], 10)
      else
      if (ssLeft in Shift) then
      begin
        // Left click - add Unit to queue
        gGame.GameInputProcess.CmdHouse(gicHouseSiegeTrain, siege, SIEGE_GAME_ORDER[fLastSiegeUnit], 1);
      end;
    end;

    if siege.Queue[0] <> utNone then
      Button_Siege_UnitWIP.TexID := gRes.Units[siege.Queue[0]].GUIIcon
    else
      Button_Siege_UnitWIP.TexID := 41; //Question mark

    Button_Siege_UnitWIP.Enabled := siege.Queue[0] <> utNone;

    Button_Siege_UnitWIPBar.Position := siege.GetTrainingProgress;

    Button_Siege_UnitWIPBar.LinesCount := Max(siege.PhasesCount - 1, 0);


    for I := 1 to 5 do
      if siege.Queue[I] <> utNone then
      begin
        Button_Siege_UnitPlan[I].TexID := gRes.Units[siege.Queue[I]].GUIIcon;
        Button_Siege_UnitPlan[I].Hint := gRes.Units[siege.Queue[I]].GUIName;
      end
      else
      begin
        Button_Siege_UnitPlan[I].TexID:=0;
        Button_Siege_UnitPlan[I].Hint:='';
      end;

    Button_Siege_Train.Enabled := (not siege.QueueIsFull)
                                    and (gMySpectator.Hand.Locks.GetUnitBlocked(SIEGE_GAME_ORDER[fLastSiegeUnit], fHouse.HouseType) = ulUnlocked);

    Button_Siege_Left.Disable;
    Button_Siege_Right.Disable;
    Image_Siege_Left.Hide;
    Image_Siege_Right.Hide;

    //if fLastSchoolUnit > 0 then
    if PreviousUnit(fLastSiegeUnit, SIEGE_GAME_ORDER) > -1 then
    begin
      Image_Siege_Left.TexID := gRes.Units[SIEGE_GAME_ORDER[PreviousUnit(fLastSiegeUnit, SIEGE_GAME_ORDER)]].GUIScroll;
      Image_Siege_Left.Show;
      Button_Siege_Left.Enable;
    end;

    Label_Siege_Unit.Caption := gRes.Units[SIEGE_GAME_ORDER[fLastSiegeUnit]].GUIName;
    Image_Siege_Train.TexID := gRes.Units[SIEGE_GAME_ORDER[fLastSiegeUnit]].GUIScroll;

    Image_Siege_Train.Enabled := gMySpectator.Hand.Locks.GetUnitBlocked(SIEGE_GAME_ORDER[fLastSiegeUnit], fHouse.HouseType) = ulUnlocked;


    //if fLastSchoolUnit < High(SiegeWorkshop_Order) then
    if NextUnit(fLastSiegeUnit, SIEGE_GAME_ORDER) > -1 then
    begin
      Image_Siege_Right.TexID := gRes.Units[SIEGE_GAME_ORDER[NextUnit(fLastSiegeUnit, SIEGE_GAME_ORDER)]].GUIScroll;
      Image_Siege_Right.Show;
      Button_Siege_Right.Enable;
    end;
    Panel_House_Siege.Show;

  end;

end;


// Process click on Units queue buttons of School
procedure TKMGUIGameHouse.House_SchoolUnitQueueClick(Sender: TObject; Shift: TShiftState);
var
  I, id: Integer;
  school: TKMHouseSchool;
  siege: TKMHouseSiegeWorkshop;
begin
  if gMySpectator.Selected is TKMHouseSchool then
  begin

    school := TKMHouseSchool(gMySpectator.Selected);
    id := TKMControl(Sender).Tag; //Item number that was clicked from the school queue

    //Right click clears entire queue after this item.
    //In that case we remove the same id repeatedly because they're automatically move along
    if IsRMBInShiftState(Shift) then
      for I := school.QueueLength - 1 downto id do
        gGame.GameInputProcess.CmdHouse(gicHouseRemoveTrain, school, I)
    else if SCHOOL_CH_ORDER_TO_0_SHIFT in Shift then
      // Left click + Shift - change Unit order in queue to 0
      gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrainChOrder, school, id, 0)
    else if SCHOOL_CH_ORDER_TO_1_SHIFT in Shift then
      // Left click + Ctrl - change Unit order in queue to 1
      gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrainChOrder, school, id, Min(id, 1))
    else
      //Left click removes 1 unit from queue
      gGame.GameInputProcess.CmdHouse(gicHouseRemoveTrain, school, id);

    House_SchoolUnitChange(nil, []);
  end else
  if gMySpectator.Selected is TKMHouseSiegeWorkshop then
  begin

    siege := TKMHouseSiegeWorkshop(gMySpectator.Selected);

    id := TKMControl(Sender).Tag; //Item number that was clicked from the school queue

    //Right click clears entire queue after this item.
    //In that case we remove the same id repeatedly because they're automatically move along
    if IsRMBInShiftState(Shift) then
      for I := siege.QueueLength - 1 downto id do
        gGame.GameInputProcess.CmdHouse(gicHouseRemoveTrain, siege, I)
    else
    //Left click removes 1 unit from queue
    gGame.GameInputProcess.CmdHouse(gicHouseRemoveTrain, siege, id);

    House_SchoolUnitChange(nil, []);

  end;
end;
{
procedure TKMGUIGameHouse.House_SchoolIconClicked(aValue: Integer; Shift : TShiftState);
var I : Integer;
  UT : TKMUnitType;
begin
  I := Icons_AllWorkers.GetTag(aValue);
  UT := TKMUnitType(I);
  if IsRMBInShiftState(Shift) then
    gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrain, fHouse, UT, 10)
  else
    gGame.GameInputProcess.CmdHouse(gicHouseSchoolTrain, fHouse, UT, 1)

end;
}
// That small red triangle blocking delivery of wares to Barracks
// Ware determined by Button.Tag property
procedure TKMGUIGameHouse.House_BarracksItemClickShift(Sender: TObject; Shift: TShiftState);
begin
  if gMySpectator.Selected = nil then
    Exit;
  if not (gMySpectator.Selected is TKMHouseBarracks) then
    Exit;
  //Red triangle - block delivery to barracks
  if ssLeft in Shift then
  begin
    if Sender <> Button_BarracksRecruit then
      gGame.GameInputProcess.CmdHouse(gicHouseBarracksAcceptFlag, TKMHouse(gMySpectator.Selected), BarracksResOrder[(Sender as TKMControl).Tag])
    else
      gGame.GameInputProcess.CmdHouse(gicHBarracksAcceptRecruitsTgl, TKMHouse(gMySpectator.Selected));
  end
  else
  //Orange triange - block take resources from
  if ssRight in Shift then
  begin
    if Sender <> Button_BarracksRecruit then
      gGame.GameInputProcess.CmdHouse(gicHBarracksNotAllowTakeOutFlag, TKMHouse(gMySpectator.Selected), BarracksResOrder[(Sender as TKMControl).Tag]);
  end;
end;


// That small red triangle blocking delivery of wares to Storehouse
// Ware determined by Button.Tag property
procedure TKMGUIGameHouse.House_StoreItemClickShift(Sender: TObject; X, Y : Integer; Shift: TShiftState);
var W : TKMWareType;
begin
  if gMySpectator.Selected = nil then
    Exit;

  if not (gMySpectator.Selected is TKMHouseStore) then
    Exit;
  W := TKMWareType(TKMButtonFlat(Sender).Tag);
  If (ssShift in Shift) then
  begin
    //Red triangle - block delivery to barracks
    if ssLeft in Shift then
      gGame.GameInputProcess.CmdHouse(gicStoreHouseBlockAll, TKMHouse(gMySpectator.Selected), 0)
    else
    //Orange triange - block take resources from
    if ssRight in Shift then
      gGame.GameInputProcess.CmdHouse(gicStoreHouseBlockAll, TKMHouse(gMySpectator.Selected), 1);
  end else
  If (ssCtrl in Shift) then
  begin
    //Red triangle - block delivery to barracks
    if ssLeft in Shift then
      gGame.GameInputProcess.CmdHouse(gicStoreHouseUnlockAll, TKMHouse(gMySpectator.Selected), 0)
    else
    //Orange triange - block take resources from
    if ssRight in Shift then
      gGame.GameInputProcess.CmdHouse(gicStoreHouseUnlockAll, TKMHouse(gMySpectator.Selected), 1);

  end;

  //Red triangle - block delivery to barracks
  if ssLeft in Shift then
    gGame.GameInputProcess.CmdHouse(gicHouseStoreNotAcceptFlag, TKMHouse(gMySpectator.Selected), W)
  else
  //Orange triange - block take resources from
  if ssRight in Shift then
    gGame.GameInputProcess.CmdHouse(gicHStoreNotAllowTakeOutFlag, TKMHouse(gMySpectator.Selected), W);
end;

procedure TKMGUIGameHouse.House_StoreItemMouseOver(Sender: TObject; Shift: TShiftState);
var ctrlDown : TKMControl;
    W, W2 : TKMWareType;
    notAccept : Boolean;
begin
  ctrlDown := TKMControl(Sender).MasterPanel.MasterControl.CtrlDown;

  if ctrlDown = nil then
    Exit;

  if not (sender is TKMButtonFlat) then
    Exit;

  if not (ctrlDown is TKMButtonFlat) then
    Exit;
  if (ctrlDown = Sender) then
    Exit;
  if TKMButtonFlat(ctrlDown).Tag2 = TKMButtonFlat(Sender).Tag2 then
  begin
    W := TKMWareType(TKMButtonFlat(ctrlDown).Tag);
    W2 := TKMWareType(TKMButtonFlat(Sender).Tag);

    if ssRight in Shift then
      notAccept := TKMHouseStore(fHouse).NotAllowTakeOutFlag[W]
    else
      notAccept := TKMHouseStore(fHouse).NotAcceptFlag[W];


    if W = W2 then
      Exit;
    if ssRight in Shift then
    begin
      If notAccept <> TKMHouseStore(fHouse).NotAllowTakeOutFlag[W2] then
        gGame.GameInputProcess.CmdHouse(gicHStoreSetNotAllowTakeOutFlag, fHouse, W2, byte(notAccept));

    end else
    begin
      If TKMHouseStore(fHouse).NotAcceptFlag[W] <> TKMHouseStore(fHouse).NotAcceptFlag[W2] then
        gGame.GameInputProcess.CmdHouse(gicHStoreSetNotAcceptFlag, fHouse, W2, byte(notAccept));


    end;

  end;


end;

procedure TKMGUIGameHouse.House_MarketFill(aMarket: TKMHouseMarket);
var
  I, tmp: Integer;
  W: TKMWareType;
  M : TKMHouseMarket;
begin

  M := aMarket;

  for I := 0 to high(Button_Market) do
    begin
      //First clean the down and color;

      Button_Market[I].DownColor := $FFFFFFFF;
      Button_Market[I].Down := false;
      Button_Market[I].HighLightColor := $40FFFFFF;


      W := TKMWareType(Button_Market[I].Tag);
      if M.AllowedToTrade(W) then
      begin

        Button_Market[I].TexID := gRes.Wares[W].GUIIcon;
        Button_Market[I].Hint := gRes.Wares[W].Title;
        tmp := M.GetResTotal(W);
        Button_Market[I].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp, 1000)); // Convert 1234 -> '1k'
      end else
      begin
        Button_Market[I].TexID := 41;
        Button_Market[I].Hint := gResTexts[TX_HOUSES_MARKET_HINT_BLOCKED];
        Button_Market[I].Caption := '-';
      end;

      //Disabling buttons will let player know that he cant select new trade without canceling current one
      Button_Market[I].Enabled := (W in [M.ResFrom, M.ResTo]) or not M.TradeInProgress;

      if M.ResFrom <> wtNone then
        if W = M.ResFrom then
        begin
          Button_Market[I].DownColor := $FF00C000;
          Button_Market[I].Down := true;
          Button_Market[I].HighLightColor := $4000C000;
        end;
      if M.ResTo <> wtNone then
        if W = M.ResTo then
        begin
          Button_Market[I].DownColor := $FF0707FF;
          Button_Market[I].HighLightColor := $400707FF;
          Button_Market[I].Down := true;
        end;


    end;

  //Position the shape that marks the FROM ware
  if M.ResFrom <> wtNone then
  begin
    Label_Market_In.Caption := Format(gResTexts[TX_HOUSES_MARKET_FROM], [M.RatioFrom]);
    Button_Market_In.TexID := gRes.Wares[M.ResFrom].GUIIcon;
    Button_Market_In.Caption := IntToStr(M.GetResTotal(M.ResFrom));
  end else
  begin
    Label_Market_In.Caption := Format(gResTexts[TX_HOUSES_MARKET_FROM],[0]);
    Button_Market_In.TexID := gRes.Wares[wtNone].GUIIcon;
    Button_Market_In.Caption := '-';
  end;

  //Position the shape that marks the TO ware
  if M.ResTo <> wtNone then
  begin
    Label_Market_Out.Caption := Format(gResTexts[TX_HOUSES_MARKET_TO], [M.RatioTo]);

    Button_Market_Out.Caption := IntToStr(M.GetResTotal(M.ResTo));
    Button_Market_Out.TexID := gRes.Wares[M.ResTo].GUIIcon;
  end else
  begin
    Label_Market_Out.Caption := Format(gResTexts[TX_HOUSES_MARKET_TO], [0]);
    Button_Market_Out.TexID := gRes.Wares[wtNone].GUIIcon;
    Button_Market_Out.Caption := '-';
  end;
  if aMarket is TKMHouseMarket then
  begin
    Button_Market_Remove.Enabled := (M.ResFrom <> wtNone) and (M.ResTo <> wtNone);
    Button_Market_Add.Enabled := Button_Market_Remove.Enabled;
    Label_Market_FromAmount.Caption := IntToStr(M.RatioFrom * M.WareOrder[1]);
    Label_Market_ToAmount.Caption := IntToStr(M.RatioTo * M.WareOrder[1]);
  end;
end;


procedure TKMGUIGameHouse.House_MarketOrderClick(Sender: TObject; Shift: TShiftState);
var
  M: TKMHouseMarket;
begin
  if gMySpectator.Selected is TKMHouseMarket then
  begin

    M := TKMHouseMarket(gMySpectator.Selected);

    if Sender = Button_Market_Remove then
      gGame.GameInputProcess.CmdHouse(gicHouseOrderProduct, M, 1, -GetMultiplicator(Shift));
    if Sender = Button_Market_Add then
      gGame.GameInputProcess.CmdHouse(gicHouseOrderProduct, M, 1, GetMultiplicator(Shift));
  end;

end;


procedure TKMGUIGameHouse.House_MarketSelect(Sender: TObject; Shift: TShiftState);
var
  M: TKMHouseMarket;
begin

  if (gMySpectator.Selected is TKMHouseMarket) then
  begin

    M := TKMHouseMarket(gMySpectator.Selected);

    if Shift = [ssLeft] then
      gGame.GameInputProcess.CmdHouse(gicHouseMarketFrom, M, TKMWareType(TKMButtonFlat(Sender).Tag));
    if Shift = [ssRight] then
      gGame.GameInputProcess.CmdHouse(gicHouseMarketTo, M, TKMWareType(TKMButtonFlat(Sender).Tag));

  end;

end;


procedure TKMGUIGameHouse.ToggleHouseAcceptWare(Sender: TObject; Shift: TShiftState);
var
  I, J: Integer;
  HS: TKMHouse;
  aCount : Integer;
begin
  HS := TKMHouse(gMySpectator.Selected);

  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if Sender <> nil then
      if Sender = WaresRow_Common[I] then
      begin
        J := WaresRow_Common[I].Tag;

        aCount := 1;
        if ssShift in Shift then
          aCount := aCount * 10;

        if ssCtrl in Shift then
          aCount := aCount * 10;

        if ssAlt in Shift then
          aCount := aCount * 10;

        if ssRight in Shift then
          aCount := -aCount;
        //HS.ToggleAcceptWaresIn(HS.WareInput[I], aCount)
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, HS, HS.WareInput[J], aCount);
      end else
      if (Sender = WaresRow_Max[I].ButtonInc) or (Sender = WaresRow_Max[I].ButtonDec) then
      begin
        J := WaresRow_Max[I].Tag;
        If Sender = WaresRow_Max[I].ButtonInc then
          aCount := -1
        else
          aCount := 1;

        if ssShift in Shift then
          aCount := aCount * 10;

        if ssCtrl in Shift then
          aCount := aCount * 10;

        if ssAlt in Shift then
          aCount := aCount * 10;

        if ssRight in Shift then
          aCount := aCount * 10;
        //HS.ToggleAcceptWaresIn(HS.WareInput[I], aCount)
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, HS, HS.WareInput[J], aCount);

      end;

    //Image_WareIn_Accept[I].Visible := (HS.GetAcceptWareIn(HS.WareInput[I]) > 0) and (HS.WareInput[I] in WARES_VALID);
    //Image_WareIn_Accept[I].Count := HS.GetAcceptWareIn(HS.WareInput[I]);
  end;
end;

procedure TKMGUIGameHouse.ToggleMerchantWareInput(Sender : TObject);
var
  HS: TKMHouse;
  I : Integer;
begin
  if not (Sender is TKMButton) then
    Exit;
  if not (gMySpectator.Selected is TKMHouse) then
    Exit;

  HS := TKMHouse(gMySpectator.Selected);
  if HS = nil then
    Exit;
  //don't set the same ware slot
  if HS.WareInputSlot = TKMButton(Sender).Tag then
    Exit;
  gGame.GameInputProcess.CmdHouse(gicHouseMerchantSetType, HS, TKMButton(Sender).Tag, byte(GetKeyState(VK_SHIFT) < 0));

  for I := 0 to high(Button_MerchantType) do
    Button_MerchantType[I].ShowImageEnabled := HS.WareInputSlot = I;

end;

procedure TKMGUIGameHouse.House_MerchantPlayer(Sender : TObject);
var HS : TKMHouseMerchant;
begin


  HS := TKMHouseMerchant(gMySpectator.Selected);

  //HS.ToggleSendToHand(TKMFlatButtonShape(Sender).Tag);
  gGame.GameInputProcess.CmdHouse(gicHouseMerchantSendTo, HS, TKMFlatButtonShape(Sender).Tag);
  //TKMFlatButtonShape(Sender).Down := HS.SendToHand[TKMFlatButtonShape(Sender).Tag];
  {doReset := false;

  if TKMFlatButtonShape(Sender).Down then
    doReset := true;


  for I := 0 to High(Button_PlayerSelect) do
    Button_PlayerSelect[I].Down := false;

  HS := TKMHouseMerchant(gMySpectator.Selected);

  if HS = nil then
    Exit;

  if doReset then
    HS.SetHandTo(HS.Owner)
  else
    HS.SetHandTo(TKMFlatButtonShape(Sender).Tag);

  Button_PlayerSelect[HS.HandTo].Down := true;}



end;
procedure TKMGUIGameHouse.House_StoreFill;
var
  I, J, K, L, top, tmp: Integer;
  H : TKMHouseStore;
  W : TKMWareType;
  hasAny : Boolean;
begin
  if gMySpectator.Selected = nil then Exit;

  if not (gMySpectator.Selected is TKMHouseStore) then Exit;

  H := TKMHouseStore(gMySpectator.Selected);

  Bar_TotalCount.Position := (H.TotalCount / H.MaxCount);
  Bar_TotalCount.Caption :=  IntToStr(Trunc(H.TotalCount / H.MaxCount * 100)) + '%';

  Bar_TotalCount.MainColor := MixColor(icGreen, icRed, Min(Bar_TotalCount.Position, 1));

  if Bar_TotalCount.Position >= 1 then
    Image_TotalCount.TexID := 718
  else
    Image_TotalCount.TexID := 717;

  //first hide everything
  for I := 0 to high(Button_Store) do
  begin
    Button_Store[I].Hide;
    Image_Store_NotAccept[I].Hide;
    Image_Store_NotAllowTakeOut[I].Hide;
  end;
  for I := 0 to High(Label_Wares) do
    Label_Wares[I].Hide;

  L := 0;
  J := 0;
  top := 20;
  for I := 0 to High(fStoreWareOrder) do
  begin
    hasAny := not CheckBox_Store.Checked;
    if not hasAny then
      for K := 0 to High(fStoreWareOrder[I]) do
      begin
        W := fStoreWareOrder[I, K];
        tmp := TKMHouseStore(gMySpectator.Selected).CheckWareIn(W);
        if (tmp > 0) then
        begin
          hasAny := true;
          Break;
        end;
      end;

    if not hasAny then
    begin
      Inc(J, length(fStoreWareOrder[I]));
      Continue;
    end;

    if L > 0 then
      top := top + 37 + (L - 1) div 5 * 37;
    L := 0;

    Label_Wares[I].Top := top;
    Label_Wares[I].Show;
    top := top + 16;
    for K := 0 to High(fStoreWareOrder[I]) do
    begin
      W := fStoreWareOrder[I, K];
      tmp := TKMHouseStore(gMySpectator.Selected).CheckWareIn(W);

      if (tmp > 0) or not CheckBox_Store.Checked then
      begin
        Button_Store[J].Show;
        Button_Store[J].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp));
        Button_Store[J].Left := L mod 5 * 37;
        Button_Store[J].Top := top + L div 5 * 37;

        Image_Store_NotAccept[J].Top := top + L div 5 * 37;
        Image_Store_NotAllowTakeOut[J].Top := top + L div 5 * 37;

        Image_Store_NotAccept[J].Left := Button_Store[J].Left + 20;
        Image_Store_NotAllowTakeOut[J].Left := Button_Store[J].Left;


        Image_Store_NotAccept[J].Visible := TKMHouseStore(gMySpectator.Selected).NotAcceptFlag[W];
        Image_Store_NotAllowTakeOut[J].Visible := TKMHouseStore(gMySpectator.Selected).NotAllowTakeOutFlag[W];
      end;
      if Button_Store[J].Visible then
        inc(L);

      Inc(J);
    end;
  end;

  {for I := 0 to high(Button_Store) do
    begin
      W := TKMWareType(Button_Store[I].Tag);
      tmp := TKMHouseStore(gMySpectator.Selected).CheckWareIn(W);
      Button_Store[I].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp));

      Image_Store_NotAccept[I].Visible := TKMHouseStore(gMySpectator.Selected).NotAcceptFlag[W];
      Image_Store_NotAllowTakeOut[I].Visible := TKMHouseStore(gMySpectator.Selected).NotAllowTakeOutFlag[W];
    end;}
end;

procedure TKMGUIGameHouse.House_WoodBurnerChange;
var I : Integer;
  H : TKMHouseWoodBurner;
begin
  if not (gMySpectator.Selected is TKMHouseWoodBurner) then Exit;
  H := TKMHouseWoodBurner(gMySpectator.Selected);

  Fuel_Bar.Position := H.FuelLevel;

  for I := Low(Wood_Bar) to High(Wood_Bar) do
    Wood_Bar[I].Position := H.BurningProgress(I);

end;

procedure TKMGuiGameHouse.House_Queue_ChangeWare(Sender: TObject);
var H : TKMHouseQueue;
  I : Integer;
  tmp : Integer;
begin

  if not (gMySpectator.Selected is TKMHouseQueue) then Exit;
  H := TKMHouseQueue(gMySpectator.Selected);

  {if Sender = Button_Queue_Left then
    tmp := (fLastWareOutput mod WARES_IN_OUT_COUNT) + 1
  else
    tmp := (fLastWareOutput mod WARES_IN_OUT_COUNT) + 1;}
  tmp := fLastWareOutput;
  if Sender = Button_Queue_Left then
  begin
    for I := 1 to 6 do
    begin
      IncLoop(tmp, 1, 6, -1);
      if H.WareOutput[tmp] <> wtNone then
        Break;
    end;

  end else
  if Sender = Button_Queue_Right then
  for I := 1 to 6 do
  begin
    IncLoop(tmp, 1, 6, 1);
    if H.WareOutput[tmp] <> wtNone then
      Break;
  end;


  if H.WareOutput[tmp] = wtNone then
    fLastWareOutput := 1
  else
    fLastWareOutput := tmp;

  for I := 0 to 3 do
    Button_Queue_Wares[I].TexID := gRes.Wares[H.WareOutput[fLastWareOutput]].GUIIcon;



end;

procedure TKMGUIGameHouse.House_Queue_Click(Sender: TObject; Shift: TShiftState);
var H : TKMHouseQueue;
  W : TKMWareType;
  Qt, I : Integer;
begin
  if not (gMySpectator.Selected is TKMHouseQueue) then Exit;

  H := TKMHouseQueue(gMySpectator.Selected);
  If sender = CheckBox_NotRem then
  begin
    CheckBox_NotRem.Checked := H.NotRemLastPos;
    gGame.GameInputProcess.CmdHouse(gicHouseQueueNotRem, fHouse);
  end else
  if TKMButtonFlat(sender).Tag >= 100 then
  begin
    W := H.WareOutput[fLastWareOutput];
    case TKMButtonFlat(sender).Tag of
      100 : Qt := 1;
      101 : Qt := 2;
      102 : Qt := 3;
      103 : Qt := 5;
      else QT := 0;
    end;
    {if (ssShift in Shift) or (ssRight in Shift) then
      H.AddWareToQueue(W, Qt, 5)
    else
      H.AddWareToQueue(W, Qt, 1);}
    if (ssShift in Shift) or (ssRight in Shift) then
      for I := 0 to 4 do
        gGame.GameInputProcess.CmdHouse(gicHouseQueueAdd, fHouse, ord(W), Qt)//right click so try to add ware 5 times
    else
      gGame.GameInputProcess.CmdHouse(gicHouseQueueAdd, fHouse, ord(W), Qt);

  end else
  begin
    {if (ssShift in Shift) or (ssRight in Shift) then
      for I := TKMButtonFlat(sender).Tag + 5 downto TKMButtonFlat(sender).Tag do
        H.RemWareFromQueue(I)
    else
      H.RemWareFromQueue(TKMButtonFlat(sender).Tag);}
    if (ssShift in Shift) or (ssRight in Shift) then
      for I := 0 to 4 do
        gGame.GameInputProcess.CmdHouse(gicHouseQueueRem, fHouse, TKMButtonFlat(sender).Tag)
    else
      gGame.GameInputProcess.CmdHouse(gicHouseQueueRem, fHouse, TKMButtonFlat(sender).Tag);
  end;
end;

procedure TKMGUIGameHouse.House_Palace_Click(Sender: TObject; Shift: TShiftState);
const MAX_ORDER_COUNT_PALACE = 20;
var H : TKMHousePalace;
  amt: Word;
begin
  H := TKMHousePalace(gMySpectator.Selected);
  //for I := 0 to High(Palace_Order) do
  if Sender = Image_CancelUnit then
  begin
    gGame.GameInputProcess.CmdHouse(gicHousePalaceCancelOrder, fHouse, fLastPalaceUnit);
  end else
  if Sender is TKMButton then
  begin
    If Sender = Button_PalaceLeft then
      fLastPalaceUnit := PreviousUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, ssRight in Shift);

    If Sender = Button_PalaceRight then
      fLastPalaceUnit := NextUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, ssRight in Shift);



  end;
  if (Sender = Button_Palace_UnitPlan) or (Sender = Button_PalaceTrain) then
  begin
    amt := 1;
    if ssShift in Shift then  amt := amt * 5;
    if ssCtrl in Shift then  amt := amt * 5;
    if ssAlt in Shift then  amt := amt * 5;

    if ssLeft in Shift then
      gGame.GameInputProcess.CmdHouse(gicHousePalaceOrder, fHouse, fLastPalaceUnit, Min(H.Orders[fLastPalaceUnit] + amt, MAX_ORDER_COUNT_PALACE));
      //H.Orders[fLastPalaceUnit] := 1;

    if ssRight in Shift then
      gGame.GameInputProcess.CmdHouse(gicHousePalaceOrder, fHouse, fLastPalaceUnit, Max(H.Orders[fLastPalaceUnit] - amt, 0));
      //H.Orders[fLastPalaceUnit] := 0;

  end;
  House_PalaceRefresh(H);
end;

procedure TKMGUIGameHouse.House_PalaceRefresh(aHouse : TKMHouse);

  procedure ShowWaresProdCt(aIndex, aCount : Integer);
  begin
    WaresProdCt_Common[aIndex].Caption := 'x' + aCount.ToString;
    WaresProdCt_Common[aIndex].Show;
  end;

var I, K, J, count, lastID, phase : Integer;
  Palace : TKMHousePalace;
  UT : TKMUnitType;
  trainingUnit : Byte;
  WP : TKMWarePlan;
begin
  Palace := TKMHousePalace(aHouse);

  if Palace.TrainingInProgress then
    fLastPalaceUnit := Palace.TrainedUnitID;

  if not HasAnyUnit(PALACE_UNITS_ORDER) then
  begin
    Panel_House_Palace.Hide;
    Exit;
  end;

  Button_PalaceRight.Enabled := not Palace.TrainingInProgress and (NextUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, false) <> -1);
  Button_PalaceLeft.Enabled := not Palace.TrainingInProgress and (PreviousUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, false) <> -1);

  Button_Palace_NextUnit.Enabled := not Palace.TrainingInProgress;
  Button_Palace_PreviousUnit.Enabled := not Palace.TrainingInProgress;
  I := NextUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, false);
  Button_Palace_NextUnit.Visible := I <> -1;
  if I > -1 then
    Button_Palace_NextUnit.TexID := gRes.Units[PALACE_UNITS_ORDER[I]].GUIIcon;

  I := PreviousUnit(fLastPalaceUnit, PALACE_UNITS_ORDER, false);
  Button_Palace_PreviousUnit.Visible := I <> -1;
  if I > -1 then
    Button_Palace_PreviousUnit.TexID := gRes.Units[PALACE_UNITS_ORDER[I]].GUIIcon;

  UT := PALACE_UNITS_ORDER[fLastPalaceUnit];
  LastID := 0;
  for I := 0 to High(Button_PalaceVWares) do
  begin
    //K := Palace.VWareIDs[fLastPalaceUnit, I];
    K := gRes.Wares.VirtualWares.PALACE_WARES[I];

    if gHands[Palace.Owner].VirtualWare[K] = 0 then
      Button_PalaceVWares[I].Caption := '--'
    else
      Button_PalaceVWares[I].Caption := IntToKStr(gHands[Palace.Owner].VirtualWare[K], 1000);

    Button_PalaceVWares[I].Show;
    count := 0;

    for J := 0 to high(gRes.Units[UT].PalaceCost.Wares) do
      if gRes.Units[UT].PalaceCost.Wares[J].Index = K then
      begin
        count := gRes.Units[UT].PalaceCost.Wares[J].C;
        Break;
      end;

    Button_PalaceVWares[I].Down := count > 0;

    if count <= gHands[Palace.Owner].VirtualWare[K] then
      Button_PalaceVWares[I].DownColor := $FFFFFFFF
    else
      Button_PalaceVWares[I].DownColor := $FF0000FF;

    LastID := I
  end;

  Bar_Palace_ProgressLeft.Top := Button_PalaceVWares[lastID].Bottom + 40;
  Bar_Palace_ProgressLeft.Position := Palace.FullProgress * 0.97;
  Bar_Palace_ProgressLeft.MainColor := TKMHousePalace(aHouse).BarColor;
  Bar_Palace_ProgressLeft.LinesCount := Max(TKMHousePalace(aHouse).UnitTrainPhases[UT] - 1, 0);

  Label_Palace_Unit.Top := Button_PalaceVWares[lastID].Bottom + 5;

  Bar_Palace_ProgressRight.Top := Button_PalaceVWares[lastID].Bottom + 40;
  Bar_Palace_ProgressRight.Position := Palace.PhaseProgress * 0.97;
  Bar_Palace_ProgressRight.MainColor := icGreen;
  Bar_Palace_ProgressRight.LinesCount := 0;
  Button_Palace_UnitPlan.Top := Button_PalaceVWares[lastID].Bottom + 40;
  Button_Palace_UnitPlan.TexID := gRes.Units[UT].GUIScroll;
  Button_Palace_UnitPlan.FlagColor := gHands[aHouse.Owner].FlagColor;

  Label_Palace_Unit.Caption := gRes.Units[UT].GUIName;

  Button_PalaceLeft.Top := Button_Palace_UnitPlan.Bottom + 15;
  Button_PalaceRight.Top := Button_Palace_UnitPlan.Bottom + 15;
  Button_PalaceTrain.Top := Button_Palace_UnitPlan.Bottom + 12;
  Button_Palace_PreviousUnit.Top := Button_Palace_UnitPlan.Bottom + 15;
  Button_Palace_NextUnit.Top := Button_Palace_UnitPlan.Bottom + 15;

  Button_Palace_PreviousUnit.FlagColor := gHands[aHouse.Owner].FlagColor;
  Button_Palace_NextUnit.FlagColor := gHands[aHouse.Owner].FlagColor;

  Button_Palace_UnitPlan.Caption := IfThen(Palace.TrainingInProgress, '--','v');
  Button_Palace_UnitPlan.CapColor := IfThen(Palace.TrainingInProgress, $FFFF0000, $FFFFFF00);

  if Palace.TrainingInProgress then
  begin
    case (gGameParams.Tick div 30) mod 5  of
      0: Button_Palace_UnitPlan.Caption := '<>';
      1: Button_Palace_UnitPlan.Caption := '<->';
      2: Button_Palace_UnitPlan.Caption := '<-->';
      3: Button_Palace_UnitPlan.Caption := '<--->';
      4: Button_Palace_UnitPlan.Caption := '<---->';

    end;
    //Button_Palace_UnitPlan.Caption := '<-->'
  end
  else
  if Palace.Orders[fLastPalaceUnit] > 0 then
    Button_Palace_UnitPlan.Caption := 'v'
  else
    Button_Palace_UnitPlan.Caption := '--';

  if Palace.Orders[fLastPalaceUnit] > 0 then
    Button_Palace_UnitPlan.Caption := Button_Palace_UnitPlan.Caption + '||' + IntToStr(Palace.Orders[fLastPalaceUnit]);

  if Palace.TrainingInProgress then
    Button_Palace_UnitPlan.CapColor := $FF05B5FA
  else
  if Palace.Orders[fLastPalaceUnit] > 0 then
    Button_Palace_UnitPlan.CapColor := $FFFFD900
  else
    Button_Palace_UnitPlan.CapColor := $FFFFFFFF;


  Image_Ornament.Top := Bar_Palace_ProgressLeft.Top - 17;
  Image_Ornament.Left := Bar_Palace_ProgressLeft.Left - 9;
  Image_Ornament.FlagColor := gHands[aHouse.Owner].GameFlagColor;

  for I := 0 to High(Button_PalaceVWaresCost) do
    Button_PalaceVWaresCost[I].Hide;

  for I := 0 to High(gRes.Units[UT].PalaceCost.Wares) do
  begin
    K := gRes.Units[UT].PalaceCost.Wares[I].Index;
    Button_PalaceVWaresCost[I].Show;
    count := gRes.units[UT].PalaceCost.Wares[I].C;
    Button_PalaceVWaresCost[I].Caption := IntToStr(gRes.units[UT].PalaceCost.Wares[I].C);
    Button_PalaceVWaresCost[I].Hint := gResTexts[gRes.Wares.VirtualWares.Ware[K].TextID];
    Button_PalaceVWaresCost[I].TexID := gRes.Wares.VirtualWares.Ware[K].GUIIcon;
    Button_PalaceVWaresCost[I].Left := Button_PalaceVWares[0].Left + 12 + (I mod 5) * 32;
    Button_PalaceVWaresCost[I].Top := Button_PalaceRight.Bottom + 20 + (I div 5) * 32;
    Button_PalaceVWaresCost[I].Down := false;

    if count > gHands[Palace.Owner].VirtualWare[K] then
    begin
      Button_PalaceVWaresCost[I].DownColor := $FF0000FF;
      Button_PalaceVWaresCost[I].Down := true;
    end;
  end;

  Image_CancelUnit.Top := Button_Palace_UnitPlan.Bottom - 20;
  Image_CancelUnit.Left := Button_Palace_UnitPlan.Left;
  Image_CancelUnit.Visible := Palace.TrainingInProgress;
  Image_CancelUnit.TexID := 32;
  Image_OrderCount.Top := Button_Palace_UnitPlan.Bottom - 20;
  Image_OrderCount.Left := Button_Palace_UnitPlan.Right - 20;
  Image_OrderCount.Caption := IntToStr(Palace.Orders[fLastPalaceUnit]);

  WaresProdCt_Common[1].Caption := '';
  WaresProdCt_Common[2].Caption := '';
  WaresProdCt_Common[3].Caption := '';
  WaresProdCt_Common[4].Caption := '';
  If Palace.TrainingInProgress  then
    WP := TKMHousePalace(aHouse).GetWarePlan
  else
    WP := TKMHousePalace(aHouse).GetWarePlanOf(fLastPalaceUnit);


  for I := 0 to high(WP) do
    if WP[I].C > 0 then
    begin
      case WP[I].W of
        wtPig: ShowWaresProdCt(1, WP[I].C);
        wtGold: ShowWaresProdCt(2, WP[I].C);
        wtApple: ShowWaresProdCt(3, WP[I].C);
        wtBoots: ShowWaresProdCt(4, WP[I].C);
      end;
    end;
  Panel_House_Palace.Show;
end;

procedure TKMGUIGameHouse.House_UpgradeClick(Sender: TObject);
var H : TKMHouse;
begin

  if gMySpectator.Selected = nil then
    Exit;

  H := TKMHouse(gMySpectator.Selected);
  if sender = Button_ForceWork then
  begin
    //H.ForceWorking := not H.ForceWorking;

    gGame.GameInputProcess.CmdHouse(gicHouseForceWork, H);
    Exit;
  end;
  if H.IsMaxLevel then
    Exit;

  if H.IsUpgrading then
    gGame.GameInputProcess.CmdHouse(gicHouseCancelUpgrade, H)
    //H.CancelUpgrading
  else
    gGame.GameInputProcess.CmdHouse(gicHouseMakeUpgrade, H)
    //H.MakeUpgrade;

end;

procedure TKMGUIGameHouse.House_SmallStoreTransfer(Sender: TObject);
var ware : TKMWareType;
var H : TKMHouse;
  I, J : integer;
begin
  ware := TKMWareType(TKMButton(Sender).Tag);
  if ware in [wtAll, wtNone, wtFood, wtValuable] then Exit;

  H := TKMHouse(gMySpectator.Selected);
  J := -1;
  for I := 1 to WARES_IN_OUT_COUNT do
    if H.WareOutput[I] = ware then
    begin
      J := I;
      Break;
    end;
  if J <> -1 then
    gGame.GameInputProcess.CmdHouse(gicHouseTransferWare, H, J);

end;

procedure TKMGUIGameHouse.House_StallClick(Sender: TObject; Shift: TShiftState);
var I : integer;
  H : TKMHouseStall;
begin
  H := TKMHouseStall(gMySpectator.Selected);
  for I := Low(Button_VWares) to High(Button_VWares) do
    if Sender = Button_VWares[I] then
    begin
      if (ssRight in Shift) or (ssShift in Shift) then
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyItem, fHouse, I, 10)
        //TKMHouseStall(gMySpectator.Selected).BuyItem(I, 10)
      else
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyItem, fHouse, I, 1)
        //TKMHouseStall(gMySpectator.Selected).BuyItem(I, 1);
    end;

  for I := Low(Button_Wares) to High(Button_Wares) do
    if Sender = Button_Wares[I] then
    begin
      if (ssRight in Shift) or (ssShift in Shift) then
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyCoin, fHouse, I, 10)
        //TKMHouseStall(gMySpectator.Selected).BuyCoin(I, 10)
      else
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyCoin, fHouse, I, 1)
        //TKMHouseStall(gMySpectator.Selected).BuyCoin(I, 1);
    end else
    if Sender = Button_NotAcceptWares[I] then
    begin
      if H.GetAcceptWareIn(H.WareInput[I]) > 0 then
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, H.WareInput[I], -1000)
      else
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, H.WareInput[I], 1000);
      {if H.GetAcceptWareIn(H.WareInput[I]) > 0 then

        //H.ToggleAcceptWaresIn(H.WareInput[I], -1000)
      else
        //H.ToggleAcceptWaresIn(H.WareInput[I], 1000);}
    end;

  if Sender = Button_Coin then
  begin
    for I := 1 to 4 do
      if (ssRight in Shift) or (ssShift in Shift) then
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyCoin, fHouse, I, 10)
        //TKMHouseStall(gMySpectator.Selected).BuyCoin(I, 10)
      else
        gGame.GameInputProcess.CmdHouse(gicHouseStallBuyCoin, fHouse, I, 1);
        //TKMHouseStall(gMySpectator.Selected).BuyCoin(I, 1);
  end;

end;

procedure TKMGUIGameHouse.House_NotAcceptWorkersClick(aValue: Integer);
begin
  if aValue = high(byte) then
    Exit;
  //H := TKMHouse(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicHouseDontAcceptWorker, fHouse, aValue);

  //H.DoNotAcceptWorker(aValue);
end;

procedure TKMGUIGameHouse.House_FarmToggleGrain(Sender: TObject; Shift: TShiftState);
begin
  if ssRight in Shift then
    gGame.GameInputProcess.CmdHouse(gicHouseFarmToggleGrain, fHouse, -1, TKMControl(Sender).Tag)
  else
    gGame.GameInputProcess.CmdHouse(gicHouseFarmToggleGrain, fHouse, 1, TKMControl(Sender).Tag);
end;

procedure TKMGUIGameHouse.House_FruitTreeToggle(Sender: TObject; Shift: TShiftState);
begin
  if ssRight in Shift then
    gGame.GameInputProcess.CmdHouse(gicHouseFruitTreeToggleType, fHouse, -1)
  else
    gGame.GameInputProcess.CmdHouse(gicHouseFruitTreeToggleType, fHouse, 1);
end;

procedure TKMGUIGameHouse.House_FlagClicked(Sender: TObject);
begin
  if Assigned(fSelectNextHouse) then
    fSelectNextHouse(gCursor.ShiftPressed);
end;

procedure TKMGUIGameHouse.Store_BellClick(Sender: TObject);
begin
  if not (fHouse.HouseType in [htStore, htTownhall]) then
    Exit;

  gGame.GameInputProcess.CmdHouse(gicHouseStoreBell, fHouse);

end;

procedure TKMGUIGameHouse.Ship_Clicked(Sender: TObject; Shift: TShiftState);
begin
  if Sender = Ship_ShipType then
  begin
    if ssRight in Shift then
      gGame.GameInputProcess.CmdHouse(gicHouseShipType, fHouse, -1)
    else
      gGame.GameInputProcess.CmdHouse(gicHouseShipType, fHouse, 1);
  end else
  begin
      gGame.GameInputProcess.CmdHouse(gicHouseShipDoWork, fHouse);

  end;

end;

procedure TKMGUIGameHouse.CollectorsClicked(Sender: TObject; Shift: TShiftState);
begin
  if fHouse is TKMHouseCollectors then
    gGame.GameInputProcess.CmdHouse(gicHouseCollectorsMode, fHouse);
end;

procedure TKMGUIGameHouse.SetHouseStyleClicked(Sender: TObject; Shift: TShiftState);
var I : Integer;
begin
  I := fHouse.Style;
  IncLoop(I, 0, length(fHouse.HSpec.Styles), IfThen(ssRight in Shift, -1, 1));

  gGame.GameInputProcess.CmdHouse(gicHouseStyleSet, fHouse, I);
end;

procedure TKMGUIGameHouse.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLastSchoolUnit);
  SaveStream.Write(fLastBarracksUnit);
  SaveStream.Write(fLastTHUnit);
  SaveStream.Write(fLastPalaceUnit);
  SaveStream.Write(fLastSiegeUnit);
  SaveStream.Write(fLastWareOutput);
end;


procedure TKMGUIGameHouse.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fLastSchoolUnit);
  LoadStream.Read(fLastBarracksUnit);
  LoadStream.Read(fLastTHUnit);
  LoadStream.Read(fLastPalaceUnit);
  LoadStream.Read(fLastSiegeUnit);
  LoadStream.Read(fLastWareOutput);
end;


end.
