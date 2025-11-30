unit KM_GUIMapEdHouse;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, StrUtils, SysUtils,
   Vcl.Controls,
   KM_Controls, KM_ControlsBase, KM_ControlsProgressBar, KM_ControlsWaresRow, KM_Points,
   KM_Defaults, KM_Pics, KM_Houses, KM_InterfaceGame, KM_ResHouses, KM_ControlsPopUp, KM_ControlsEdit, KM_ControlsScroll,
   KM_ControlsSwitch, KM_ResTypes;

type
  TKMMapEdHouse = class
  private
    fHouse: TKMHouse;
    fStorehouseItem: Byte; //Selected ware in storehouse
    fBarracksItem: ShortInt; //Selected ware in barracks, or -1 for recruit

    fStoreHouseWaresCopied : Boolean;
    fStoreHouseWares : array[WARE_MIN..WARE_MAX] of Word;
    procedure Create_Common(aParent: TKMPanel);
    procedure Create_Store;
    procedure Create_Barracks;
    procedure Create_TownHall;
    procedure Create_Woodcutters;
    procedure Create_Sign(aParent: TKMPanel);
    procedure Create_Pearl;
    procedure Create_Forest;
    procedure Create_Pasture;

    procedure HouseChange(Sender: TObject; aValue: Integer);
    procedure HouseHealthChange(Sender: TObject; Shift: TShiftState);
    procedure HouseHealthClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);

    procedure House_UpdateDeliveryMode(aMode: TKMDeliveryMode);
    procedure House_DeliveryModeToggle(Sender: TObject; Shift: TShiftState);
    procedure House_RepairToggle(Sender: TObject);
    procedure House_ClosedForWorkerToggle(Sender: TObject);
    procedure HandleHouseClosedForWorker(aHouse: TKMHouse);

    procedure House_RefreshRepair;
    procedure House_RefreshCommon;
    procedure BarracksRefresh;
    procedure WoodcuttersRefresh;
    procedure StoreRefresh;
    procedure SignRefresh;

    procedure BarracksSelectWare(Sender: TObject);
    procedure SetRallyPointClick(Sender: TObject);

    procedure BarracksChange(Sender: TObject; Shift: TShiftState);
    procedure BarracksChange2(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
    procedure StoreChange(Sender: TObject; Shift: TShiftState);

    procedure StoreSelectWare(Sender: TObject);
    procedure StoreSelectWare2(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);

    procedure ShowCommonResources;
    procedure HideAllCommonResources;
    procedure HouseSignClose(sender : TObject);
    Procedure HouseSignChange(sender : TObject);
    procedure HouseSetWareInput(Sender : TObject);
    procedure HouseCheckBoxClick(Sender : TObject);
    procedure ChangeStyle(Sender : TObject);
    Procedure RefreshStyle(aID : Byte = 0);
    Procedure HouseSetLevel(Sender : TObject; Shift: TShiftState);
    procedure HouseSetForceWorking(Sender : TObject);
    procedure HouseToggleAcceptWare(Sender : TObject);
    procedure MerchantChange(Sender : TObject);
    procedure SetWariant(Sender : TObject; Shift: TShiftState);

    procedure Panel_HouseContructionChange(Sender : TObject; Shift: TShiftState);
    procedure Panel_HouseBuildWaresChange(Sender : TObject; aCount : Integer);
    procedure ColorCodeChange(Sender : TObject);
    procedure Panel_FarmChange(Sender : TObject; Shift: TShiftState);

    procedure Pearl_Change(Sender : TObject);
    procedure Pearl_ChangeShift(Sender : TObject; Shift: TShiftState);
    procedure Pearl_ChangeWares(Sender : TObject; X: Integer);
    procedure Pearl_Refresh;

    procedure Forest_Refresh;
    procedure Forest_Clicked(Sender : TObject; Shift: TShiftState);

    procedure Pasture_Refresh;
    procedure Pasture_Clicked(Sender : TObject; Shift: TShiftState);
  protected
    Panel_HouseAdditional: TKMPanel;
      Button_HouseForceWork : TKMButton;
      Button_HouseNoRes,
      Button_HouseStyles, Button_HouseWariant: TKMButton;
      Button_WareInputSlot : array[0..9] of TKMButton;
      Button_Level : TKMButton;
      Button_Grain, Button_Grass, Button_Vege : TKMButton;

    Panel_Construction : TKMPanel;
      Label_HouseConstruction: TKMLabel;
      Image_HouseConstruction_Logo: TKMImage;
      ResRow_Ware_Build: array [0..2] of TKMWareOrderRow;
      HealthBar_BuildingProgress: TKMPercentBar;
      Button_BuildingProgressDec: TKMButton;
      Button_BuildingProgressInc: TKMButton;

    Panel_House: TKMScrollPanel;
      Label_House: TKMLabel;
      Image_House_Logo: TKMImage;
      Image_House_Worker: TKMImage;
      Button_HouseDeliveryMode: TKMButton;
      Button_HouseRepair: TKMButton;
      Image_House_Worker_Closed: TKMImage;
      Button_House_Worker: TKMButton;
      HealthBar_House: TKMPercentBar;
      Button_HouseHealthDec: TKMButton;
      Button_HouseHealthInc: TKMButton;
      Label_House_Input: TKMLabel;
      Label_House_Output: TKMLabel;
      Button_BlockWare: array[0..WARES_IN_OUT_COUNT - 1] of TKMButtonFlat;
      ResRow_Ware_Input: array [0..WARES_IN_OUT_COUNT - 1] of TKMWareOrderRow;
      ResRow_Ware_Output: array [0..WARES_IN_OUT_COUNT - 1] of TKMWareOrderRow;

      Edit_HouseFlagColor: TKMEdit;
      Shape_FlagColor: TKMFlatButtonShape;
      CheckBox_Indestructible : TKMCheckBox;


    Panel_HouseWoodcutters: TKMPanel;
      Button_Woodcutters_CuttingPoint: TKMButtonFlat;

    Panel_HouseStore: TKMScrollPanel;
      Button_Store: array of TKMButtonFlat;
      Label_Store_WareCount: TKMLabel;
      Image_Store_Selected : TKMImage;
      Button_StoreDec100, Button_StoreDec: TKMButton;
      Button_StoreInc100, Button_StoreInc: TKMButton;
      Image_TotalCount : TKMimage;
      Bar_TotalCount   : TKMPercentBar;
      Bevel_Store      : TKMBevel;
      Button_CopyWares,
      Button_PasteWares  : TKMButton;

    Panel_HouseBarracks: TKMPanel;
      Button_Barracks_RallyPoint: TKMButtonFlat;
      Button_Barracks: array of TKMButtonFlat;
      Button_Barracks_Recruit: TKMButtonFlat;
      Label_Barracks_WareCount: TKMLabel;
      Button_BarracksDec100, Button_BarracksDec: TKMButton;
      Button_BarracksInc100, Button_BarracksInc: TKMButton;

    Panel_HouseTownHall: TKMPanel;
      Button_TownHall_RallyPoint: TKMButtonFlat;
      WaresRow_TH_Gold_Input: TKMWareOrderRow;
    Label_SingCLickCTRL : TKMLabel;
    Panel_HouseSign : TKMPopUpMenu;
      Label_SignMessage,
      Label_HouseSignTop: TKMLabel;
      Edit_SignMessage : TKMEdit;
      Edit_HouseSign : TKMNumericEdit;
      Image_HouseSign : TKMImage;
      Bevel_HouseSign : TKMBevel;
      Bevel_HouseSignBehind : TKMBevel;
      Image_HouseSignClose: TKMImage;

    Panel_HousePearl : TKMPanel;
      Button_PearlType : TKMButtonFlat;
      Bar_Stage : TKMPercentBar;
      Button_Stage_Inc, Button_Stage_Dec : TKMButton;
      WaresRow_Stage_Delivered : TKMWareOrderRow;
      Bar_Stage_Progreess : TKMPercentBar;
      Button_Progress_Inc, Button_Progress_Dec : TKMButton;

    Panel_HouseForest : TKMPanel;
      Button_PlantTree : array of TKMButtonFlat;

    Panel_HousePasture : TKMPanel;
      Button_AnimalCount : array [0..ANIMALS_COUNT - 1] of TKMButtonFlat;

    Button_PlayerSelect: array [0..MAX_HANDS-1] of TKMFlatButtonShape;//select player in merchant's house
  public
    constructor Create(aParent: TKMPanel);

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);

    procedure Show(aHouse: TKMHouse);
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  KM_ControlsTypes,
  KM_GameSettings,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_ResTexts, KM_Resource, KM_RenderUI, KM_ResUnits,
  KM_HouseBarracks, KM_HouseTownHall, KM_HouseStore,
  KM_HousePearl, KM_HouseForest, KM_HousePasture,
  KM_ResFonts, KM_ResMapElements,{ KM_ResTypes,}
  KM_Cursor, KM_UtilsExt, KM_CommonUtils,
   KM_Game, KM_MainSettings;


{ TKMMapEdHouse }
constructor TKMMapEdHouse.Create(aParent: TKMPanel);
begin
  inherited Create;

  fBarracksItem   := 1; //First ware selected by default
  fStorehouseItem := 1; //First ware selected by default

  Create_Common(aParent);
  Create_Store;
  Create_Barracks;
  Create_TownHall;
  Create_Woodcutters;
  Create_Sign(aParent);
  Create_Pearl;
  Create_Forest;
  Create_Pasture;
end;


procedure TKMMapEdHouse.Create_Common(aParent: TKMPanel);
var
  I: Integer;
begin
  Panel_House := TKMScrollPanel.Create(aParent, TB_PAD, 45, TB_MAP_ED_WIDTH - TB_PAD + 10, aParent.Height - 50, [saVertical], bsGame, ssCommon);
  Panel_House.ScrollV.Left := Panel_House.ScrollV.Left + 20;
  //Panel_House.DoScrollUpdate := false;
  Panel_House.Padding.SetBottom(10);
  Panel_House.ScrollV_PadTop := 10;
  Panel_House.ScrollV_PadBottom := 10;
  Panel_House.ScrollV_PadLeft := 0;
  Panel_House.AnchorsStretch;

  Panel_HouseAdditional := TKMPanel.Create(aParent, TB_PAD, 45, TB_MAP_ED_WIDTH - TB_PAD, 400);
  Panel_HouseAdditional.Hitable := false;
    //Thats common things
    Label_House := TKMLabel.Create(Panel_HouseAdditional, 0, 0, Panel_House.Width, 0, '', fntOutline, taCenter);

    Button_HouseDeliveryMode := TKMButton.Create(Panel_HouseAdditional,0,42,30,30,37, rxGui, bsGame);
    Button_HouseDeliveryMode.Hint := gResTexts[TX_HOUSE_TOGGLE_DELIVERS_HINT];
    Button_HouseDeliveryMode.OnClickShift := House_DeliveryModeToggle;
    Button_HouseRepair := TKMButton.Create(Panel_HouseAdditional,30,42,30,30,40, rxGui, bsGame);
    Button_HouseRepair.Hint := gResTexts[TX_HOUSE_TOGGLE_REPAIR_HINT];
    Button_HouseRepair.OnClick := House_RepairToggle;

    Image_House_Worker := TKMImage.Create(Panel_HouseAdditional,60,41,32,32,0);
    Image_House_Worker.ImageCenter;
    Button_House_Worker := TKMButton.Create(Panel_HouseAdditional,60,42,30,30,141, rxGui, bsGame);
    Button_House_Worker.OnClick := House_ClosedForWorkerToggle; //Clicking the button cycles it
    Image_House_Worker_Closed := TKMImage.Create(Panel_HouseAdditional,78,42,12,12,49); //Red triangle for house closed for worker
    Image_House_Worker_Closed.Hitable := False;
    Image_House_Worker_Closed.Hide;

    Image_House_Logo := TKMImage.Create(Panel_HouseAdditional,90,41,32,32,338);
    Image_House_Logo.ImageCenter;
    HealthBar_House := TKMPercentBar.Create(Panel_HouseAdditional, 134, 49, 42, 20);
    Button_HouseHealthDec := TKMButton.Create(Panel_HouseAdditional, 120, 49, 14, 20, '-', bsGame);
    Button_HouseHealthInc := TKMButton.Create(Panel_HouseAdditional, 175, 49, 14, 20, '+', bsGame);
    Button_HouseHealthDec.OnClickShift := HouseHealthChange;
    Button_HouseHealthInc.OnClickShift := HouseHealthChange;
    Button_HouseHealthDec.OnClickHold  := HouseHealthClickHold;
    Button_HouseHealthInc.OnClickHold  := HouseHealthClickHold;

    Button_Level := TKMButton.Create(Panel_HouseAdditional, Panel_House.Width - 14, 41,28, 32, 746, rxGui, bsGame);
    Button_Level.OnClickShift := HouseSetLevel;


    Button_HouseStyles := TKMButton.Create(Panel_HouseAdditional, 30, 80, 30, 30, 0, rxGui, bsGame);
    Button_HouseStyles.OnClick := ChangeStyle;
    Button_HouseStyles.Tag := 1;
    Button_HouseStyles.Hint := gResTexts[1814];

    Button_HouseWariant := TKMButton.Create(Panel_HouseAdditional, 60, 80, 30, 30, 0, rxGui, bsGame);
    Button_HouseWariant.OnClickShift := SetWariant;
    Button_HouseWariant.Tag := 1;
    Button_HouseWariant.Hint := gResTexts[2183];

    Button_HouseForceWork := TKMButton.Create(Panel_HouseAdditional, 0, 72, 30, 30, 0, rxGui, bsGame);
    Button_HouseForceWork.Hint := gResTexts[1818];
    Button_HouseForceWork.OnClick := HouseSetForceWorking;

    Button_Grain := TKMButton.Create(Panel_HouseAdditional, 0, 102, 30, 30, 0, rxGui, bsGame);
    Button_Grain.Hint := gResTexts[1818];
    Button_Grain.OnClickShift := Panel_FarmChange;
    Button_Grass := TKMButton.Create(Panel_HouseAdditional, Button_Grain.Right, 102, 30, 30, 0, rxGui, bsGame);
    Button_Grass.Hint := gResTexts[1818];
    Button_Grass.OnClickShift := Panel_FarmChange;
    Button_Vege := TKMButton.Create(Panel_HouseAdditional, Button_Grass.Right, 102, 30, 30, 0, rxGui, bsGame);
    Button_Vege.Hint := gResTexts[1818];
    Button_Vege.OnClickShift := Panel_FarmChange;

    Button_HouseNoRes := TKMButton.Create(Panel_HouseAdditional, 30, 80, 30, 30, 0, rxGui, bsGame);
    Button_HouseNoRes.OnClick := HouseSetForceWorking;
    Button_HouseNoRes.Hint := gResTexts[1868];


    Edit_HouseFlagColor := TKMEdit.Create(Panel_HouseAdditional, Panel_HouseAdditional.Width - 80, 75, 75, 20, fntMetal);
    Edit_HouseFlagColor.AutoFocusable := False; // No need to make too much attention on that field
    Edit_HouseFlagColor.Anchors := [anLeft, anTop, anRight];
    Edit_HouseFlagColor.AllowedChars := acHex;
    Edit_HouseFlagColor.MaxLen := 6;
    Edit_HouseFlagColor.OnChange := ColorCodeChange;
    Edit_HouseFlagColor.Hint := gResTexts[1884];
    Shape_FlagColor := TKMFlatButtonShape.Create(Panel_HouseAdditional, Edit_HouseFlagColor.Left - 20, 76, 18, 18, '', fntGrey, 0);

    CheckBox_Indestructible := TKMCheckBox.Create(Panel_HouseAdditional, Shape_FlagColor.Left, 100,
                                                  Panel_HouseAdditional.Width - Shape_FlagColor.Left, 15,
                                                  gResTexts[2104],fntGrey);
    CheckBox_Indestructible.OnClick := HouseCheckBoxClick;
    CheckBox_Indestructible.Hint := CheckBox_Indestructible.Caption;

    for I := 0 to High(Button_WareInputSlot) do

    begin
      Button_WareInputSlot[I] := TKMButton.Create(Panel_HouseAdditional, 29 + 25 * (I mod 5), 100 + 25 * (I div 5), 22, 22, '', bsGame);
      Button_WareInputSlot[I].OnClick := HouseSetWareInput;
      Button_WareInputSlot[I].Tag := I;
      Button_WareInputSlot[I].Hint := gResTexts[1813];
      Button_WareInputSlot[I].RX := rxGui;
    end;

  Panel_Construction := TKMPanel.Create(aParent, TB_PAD, 45, TB_MAP_ED_WIDTH - TB_PAD, 400);

    for I := 0 to High(ResRow_Ware_Build) do
    begin
      Label_HouseConstruction := TKMLabel.Create(Panel_Construction, 0, 5, Panel_Construction.Width, 20, '', fntOutline, taCenter);
      Image_HouseConstruction_Logo := TKMImage.Create(Panel_Construction,Round(Panel_Construction.Width / 2 - 16),25,32,32,338);
      //Image_HouseConstruction_Logo.ImageCenter;
      Image_HouseConstruction_Logo.Hide;
      TKMLabel.Create(Panel_Construction, 0, 60, Panel_Construction.Width - 20, 20, gResTexts[2021], fntMetal, taCenter);
      Button_BuildingProgressDec := TKMButton.Create(Panel_Construction, 9, 85, 15, 15, '-', bsGame);
      HealthBar_BuildingProgress := TKMPercentBar.Create(Panel_Construction, 25, 85, TB_MAP_ED_WIDTH - 60, 15);
      Button_BuildingProgressInc := TKMButton.Create(Panel_Construction, HealthBar_BuildingProgress.Right + 1, 85, 15, 15, '+', bsGame);
      Button_BuildingProgressDec.OnClickShift := Panel_HouseContructionChange;
      Button_BuildingProgressInc.OnClickShift := Panel_HouseContructionChange;


      ResRow_Ware_Build[I] := TKMWareOrderRow.Create(Panel_Construction, 0, 120 + I * 25, TB_MAP_ED_WIDTH - 20);
      ResRow_Ware_Build[I].WareRow.RX := rxGui;
      ResRow_Ware_Build[I].OnChange := Panel_HouseBuildWaresChange;
      ResRow_Ware_Build[I].WareRow.WareCntAsNumber := true;
      case I of
        0: ResRow_Ware_Build[I].WareRow.TexID := gRes.Wares[wtTimber].GUIIcon;
        1: ResRow_Ware_Build[I].WareRow.TexID := gRes.Wares[wtStone].GUIIcon;
        2: ResRow_Ware_Build[I].WareRow.TexID := gRes.Wares[wtTile].GUIIcon;
      end;
      //ResRow_Ware_Build[I].OnChange := HouseChange;


    end;

    Label_House_Input := TKMLabel.Create(Panel_House, 0, 125, Panel_House.Width, 0, gResTexts[TX_HOUSE_NEEDS], fntGrey, taCenter);

    for I := 0 to high(ResRow_Ware_Input) do
    begin
      ResRow_Ware_Input[I] := TKMWareOrderRow.Create(Panel_House, 0, 145 + I * 25, Panel_House.Width - 20);
      ResRow_Ware_Input[I].WareRow.RX := rxGui;
      ResRow_Ware_Input[I].OnChange := HouseChange;
      ResRow_Ware_Input[I].WareRow.Clickable := true;
      ResRow_Ware_Input[I].WareRow.HideHighlight := false;
      ResRow_Ware_Input[I].WareRow.Hitable := true;
      ResRow_Ware_Input[I].WareRow.Tag := I;
      ResRow_Ware_Input[I].WareRow.OnClick := HouseToggleAcceptWare;
      ResRow_Ware_Input[I].WareRow.TextMarginX := -50;

      Button_BlockWare[I] := TKMButtonFlat.Create(Panel_House, ResRow_Ware_Input[I].Right, ResRow_Ware_Input[I].Top, 20, 20, 33, rxGuiMain);
      Button_BlockWare[I].Hint := gResTexts[1927];
      Button_BlockWare[I].OnClick := HouseToggleAcceptWare;
      Button_BlockWare[I].Tag := I;
    end;
    Label_House_Output := TKMLabel.Create(Panel_House, 0, 185+70, Panel_House.Width, 0, gResTexts[TX_HOUSE_DELIVERS]+':', fntGrey, taCenter);
    for I := 0 to high(ResRow_Ware_Output) do
    begin
      ResRow_Ware_Output[I] := TKMWareOrderRow.Create(Panel_House, 0, 30 + 255 + I * 25, Panel_House.Width);
      ResRow_Ware_Output[I].WareRow.RX := rxGui;
      ResRow_Ware_Output[I].OnChange := HouseChange;
    end;

    for I := 0 to High(Button_PlayerSelect) do
    begin
      Button_PlayerSelect[I] := TKMFlatButtonShape.Create(Panel_House, 0, 0, 25, 25, IntToStr(I + 1),fntGrey, $FF0000FF);
      Button_PlayerSelect[I].Tag := I;
      Button_PlayerSelect[I].LineWidth := 3;
      Button_PlayerSelect[I].OnClick := MerchantChange;
    end;

end;


procedure TKMMapEdHouse.Create_Sign(aParent: TKMPanel);
var  LABEL_DEC_SIZE : Integer;
var W, H : Integer;
begin
  if gMainSettings.Resolution.Width >= 1600 then
    LABEL_DEC_SIZE := 45
  else
  if gMainSettings.Resolution.Width > 1300 then
    LABEL_DEC_SIZE := 40
  else
    LABEL_DEC_SIZE := 30;

  Label_SingCLickCTRL :=  TKMLabel.Create(Panel_House, 9, 200, aParent.Width - 18, 100, gResTexts[1811], fntMetal, taCenter);
  Label_SingCLickCTRL.WordWrap := true;
  Label_SingCLickCTRL.Hitable := false;
  Label_SingCLickCTRL.Hide;
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

  Bevel_HouseSign := TKMBevel.Create(Panel_HouseSign, LABEL_DEC_SIZE,  LABEL_DEC_SIZE + 85 ,Panel_HouseSign.Width - (LABEL_DEC_SIZE * 2),Panel_HouseSign.Height -(LABEL_DEC_SIZE * 2) - 100);
  Bevel_HouseSign.BackAlpha := 0.7;
  Bevel_HouseSign.EdgeAlpha := 0.9;

  Label_SignMessage := TKMLabel.Create(Panel_HouseSign, LABEL_DEC_SIZE + 10,  LABEL_DEC_SIZE + 85 + 10 ,Panel_HouseSign.Width - (LABEL_DEC_SIZE * 2) - 20 ,Panel_HouseSign.Height -(LABEL_DEC_SIZE * 2) - 100 - 20,
                                        '', fntOutline, taCenter);
  Label_SignMessage.WordWrap := true;
  Label_SignMessage.TextVAlign := tvaMiddle;

  Label_HouseSignTop := TKMLabel.Create(Panel_HouseSign, LABEL_DEC_SIZE, LABEL_DEC_SIZE,Panel_HouseSign.Width - (LABEL_DEC_SIZE * 2),25 ,'',fntOutline,taCenter);
  Label_HouseSignTop.Caption := gResTexts[1671];

  TKMLabel.Create(Panel_HouseSign, LABEL_DEC_SIZE, LABEL_DEC_SIZE, 150, 20, gResTexts[1151], fntMetal, taLeft);

  Edit_HouseSign := TKMNumericEdit.Create(Panel_HouseSign, LABEL_DEC_SIZE, LABEL_DEC_SIZE + 25, -1, 10000,fntOutline, true);
  Edit_HouseSign.Value := -1;
  Edit_HouseSign.OnChange := HouseSignChange;

  Edit_SignMessage := TKMEdit.Create(Panel_HouseSign, LABEL_DEC_SIZE, LABEL_DEC_SIZE + 53, Label_HouseSignTop.Width, 25, fntOutline, true);
  Edit_SignMessage.OnChange := HouseSignChange;


  Image_HouseSignClose := TKMImage.Create(Panel_HouseSign, Panel_HouseSign.Width - 50, 7, 32, 32, 52);
  Image_HouseSignClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
  Image_HouseSignClose.HighlightOnMouseOver := True;
  Image_HouseSignClose.OnClick := HouseSignClose;

  Panel_HouseSign.Hide;
end;



{Store page}
procedure TKMMapEdHouse.Create_Store;
const BAR_WIDTH = 90;
var
  I, K, J, top: Integer;
  C: Integer;

begin

  top := 110;
  Bevel_Store := TKMBevel.Create(Panel_HouseAdditional, 0, 16 + top, 112, 62);
  Bevel_Store.BackAlpha := 0.8;
  Bevel_Store.Color.SetColor(0.1, 0.2, 0.1);
  Image_TotalCount := TKMImage.Create(Panel_HouseAdditional, 3, 18 + top, 20, 20, 717);
  Bar_TotalCount     := TKMPercentBar.Create(Panel_HouseAdditional, 20 , 20 + top, BAR_WIDTH,15);

  Button_StoreDec100     := TKMButton.Create(Panel_HouseAdditional, 0, 37 + top, 20, 20, '<', bsGame);
  Button_StoreDec100.Tag := 100;
  Button_StoreDec        := TKMButton.Create(Panel_HouseAdditional, 0, 57 + top, 20, 20, '-', bsGame);
  Button_StoreDec.Tag    := 1;
  Image_Store_Selected   := TKMImage.Create (Panel_HouseAdditional, 45, 37 + top, 20, 20, 0);
  Label_Store_WareCount  := TKMLabel.Create (Panel_HouseAdditional, 30, 59 + top, 50, 20, '',  fntMetal, taCenter);
  Button_StoreInc100     := TKMButton.Create(Panel_HouseAdditional, Bar_TotalCount.Right - 20, 37 + top, 20, 20, '>', bsGame);
  Button_StoreInc100.Tag := 100;
  Button_StoreInc        := TKMButton.Create(Panel_HouseAdditional, Bar_TotalCount.Right - 20, 57 + top, 20, 20, '+', bsGame);
  Button_StoreInc.Tag    := 1;
  Button_StoreDec100.OnClickShift := StoreChange;
  Button_StoreDec.OnClickShift    := StoreChange;
  Button_StoreInc100.OnClickShift := StoreChange;
  Button_StoreInc.OnClickShift    := StoreChange;

  Button_CopyWares := TKMButton.Create(Panel_HouseAdditional, Panel_HouseAdditional.Width - 55, 37 + top, 25, 25, '\/', bsGame);
  Button_CopyWares.Hint := 'Copy wares';
  Button_CopyWares.OnClickShift := StoreChange;
  Button_PasteWares := TKMButton.Create(Panel_HouseAdditional, Panel_HouseAdditional.Width - 30, 37 + top, 25, 25, '/\', bsGame);
  Button_PasteWares.Hint := 'Paste wares';
  Button_PasteWares.OnClickShift := StoreChange;

  Panel_HouseStore := TKMScrollPanel.Create(Panel_House,5,Bevel_Store.Bottom + 3,Panel_House.Width - 5,Panel_House.Height - Bevel_Store.Bottom - 3 - 40, [saVertical], bsGame, ssCommon);
  top := 0;
  J := 0;
  C := 0;
  K := 0;
  SetLength(Button_Store, 0);
  for I := 1 to STORE_RES_COUNT do
  begin
    if StoreResType[I] in [wtNone] then
    begin
      C := 0;
      if J > 0 then
        top := Button_Store[J - 1].Bottom + 1;

      with TKMLabel.Create(Panel_HouseStore, 0, top, TB_WIDTH, 15, gResTexts[1657 + K], fntOutline, taCenter) do
        Hitable := false;

      Inc(top, 17);
      Inc(K);
      Continue;
    end;
    SetLength(Button_Store, J + 1);

    Button_Store[J] := TKMButtonFlat.Create(Panel_HouseStore, C mod 5 * 39, top + C div 5 * 37, 32, 36, 0);
    Button_Store[J].TexID := gRes.Wares[StoreResType[I]].GUIIcon;
    Button_Store[J].Tag := byte(StoreResType[I]);
    Button_Store[J].Tag2 := J;
    Button_Store[J].Hint := gRes.Wares[StoreResType[I]].Title;
    Button_Store[J].OnClick := StoreSelectWare;
    Button_Store[J].OnMouseWheel := StoreSelectWare2;

    Inc(C);
    Inc(J);
  end;
end;

procedure TKMMapEdHouse.StoreSelectWare2(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
var I : Integer;
  ware: TKMWareType;
  store: TKMHouseStore;
  newCount: Word;
begin
  if not Panel_HouseStore.Visible then Exit;
  if not (Sender is TKMButtonFlat) then Exit; //Only FlatButtons
  if not (fHouse is TKMHouseStore) then Exit;
  if aHandled then Exit;

  aHandled :=  WheelSteps <> 0;
  TKMButtonFlat(Sender).Focus;

  for I := 0 to high(Button_Store) do
    Button_Store[I].Down := False;

  TKMButtonFlat(Sender).Down := True;
  fStorehouseItem := TKMButtonFlat(Sender).Tag2;

  store := TKMHouseStore(fHouse);
  ware := TKMWareType(TKMButtonFlat(Sender).Tag);

  if not (ware in WARES_VALID) then  Exit;
  begin
    newCount := 1;
    if ssCtrl in gCursor.SState then
      newCount := newCount * 5;

    if (ssShift in gCursor.SState) then
      newCount := newCount * 10;

    if (ssAlt in gCursor.SState) then
      newCount := newCount * 20;

    if WheelSteps > 0 then
      store.WareAddToIn(ware, newCount)
    else
    if WheelSteps < 0 then
    begin
      newCount := Math.Min(store.CheckWareIn(ware), newCount);
      store.WareTakeFromOut(ware, newCount);
    end;

  end;
  Label_Store_WareCount.Caption := inttostr(store.CheckWareIn(ware));
  Image_Store_Selected.TexID := gRes.Wares[ware].GUIIcon;
  StoreRefresh;
end;

procedure TKMMapEdHouse.Create_Woodcutters;
begin
  Panel_HouseWoodcutters := TKMPanel.Create(Panel_House,0,85 + 40,Panel_House.Width,40);
    Button_Woodcutters_CuttingPoint := TKMButtonFlat.Create(Panel_HouseWoodcutters, 0, 0, Panel_HouseWoodcutters.Width, 22, 0);
    Button_Woodcutters_CuttingPoint.CapOffsetY := -11;
    Button_Woodcutters_CuttingPoint.Caption := gResTexts[TX_HOUSES_WOODCUTTER_CUTTING_POINT];
    Button_Woodcutters_CuttingPoint.Hint := gResTexts[TX_MAPED_WOODCUTTER_CUTTING_POINT_HINT];
    Button_Woodcutters_CuttingPoint.OnClick := SetRallyPointClick;
end;

procedure TKMMapEdHouse.Create_Pearl;
var top : Integer;
begin
  Panel_HousePearl := TKMPanel.Create(Panel_House,0,85 + 40,Panel_House.Width - 10,400);
    Button_PearlType := TKMButtonFlat.Create(Panel_HousePearl, 0, 0, Panel_HousePearl.Width, 40, 0, rxGui);
    Button_PearlType.OnClick :=  Pearl_Change;
    top := Button_PearlType.Bottom + 5;
    Button_Stage_Dec := TKMButton.Create(Panel_HousePearl, 0, top, 20, 20, '-', bsGame);
    Bar_Stage := TKMPercentBar.Create(Panel_HousePearl, 20, top, Panel_HousePearl.Width - 40, 20);
    Button_Stage_Inc := TKMButton.Create(Panel_HousePearl, Panel_HousePearl.Width - 20, top, 20, 20, '+', bsGame);
    Button_Stage_Dec.OnClickShift := Pearl_ChangeShift;
    Button_Stage_Inc.OnClickShift := Pearl_ChangeShift;
    inc(top, 25);
    WaresRow_Stage_Delivered := TKMWareOrderRow.Create(Panel_HousePearl, 0, top, Panel_HousePearl.Width);
    WaresRow_Stage_Delivered.OnChange := Pearl_ChangeWares;

    inc(top, 25);
    Button_Progress_Dec := TKMButton.Create(Panel_HousePearl, 0, top, 20, 20, '-', bsGame);
    Bar_Stage_Progreess := TKMPercentBar.Create(Panel_HousePearl, 20, top, Panel_HousePearl.Width - 40, 20);
    Button_Progress_Inc := TKMButton.Create(Panel_HousePearl, Panel_HousePearl.Width - 20, top, 20, 20, '+', bsGame);
    Button_Progress_Dec.OnClickShift := Pearl_ChangeShift;
    Button_Progress_Inc.OnClickShift := Pearl_ChangeShift;
end;


procedure TKMMapEdHouse.Create_Forest;
var I : integer;
begin

  Panel_HouseForest := TKMPanel.Create(Panel_House,0,85 + 120,Panel_House.Width - 10,400);

  SetLength(Button_PlantTree, length(gGrowingTrees));

  for I := 0 to High(Button_PlantTree) do
  begin
    Button_PlantTree[I] := TKMButtonFlat.Create(Panel_HouseForest, I mod 5 * 35, I div 5 * 37, 33, 35, gGrowingTrees[I].GuiIcon);
    Button_PlantTree[I].OnClickShift := Forest_Clicked;
    Button_PlantTree[I].Tag := I;
  end;
end;

procedure TKMMapEdHouse.Create_Pasture;
var I : integer;
begin

  Panel_HousePasture := TKMPanel.Create(Panel_House,0,85 + 285,Panel_House.Width - 10,400);

  for I := 0 to High(Button_AnimalCount) do
    begin
      Button_AnimalCount[I] := TKMButtonFlat.Create(Panel_HousePasture, I mod 5 * 40, I div 5 * 37, 30, 34, 0);
      Button_AnimalCount[I].Tag := I;
      Button_AnimalCount[I].OnClickShift := Pasture_Clicked;
      Button_AnimalCount[I].TexID := PASTURE_ANIMALS_ORDER[I].Spec.GuiIcon;
    end;
end;


{Barracks page}
procedure TKMMapEdHouse.Create_Barracks;
var
  I, top, left: Integer;
  dX, dY, lastID, J, rightIconStart : Integer;
begin
  Panel_HouseBarracks := TKMPanel.Create(Panel_House,0,76 + 40,Panel_House.Width,400);

    Button_Barracks_RallyPoint := TKMButtonFlat.Create(Panel_HouseBarracks, 0, 8, Panel_House.Width, 22, 0);
    Button_Barracks_RallyPoint.CapOffsetY := -11;
    Button_Barracks_RallyPoint.Caption := gResTexts[TX_HOUSES_RALLY_POINT];
    Button_Barracks_RallyPoint.Hint := Format(gResTexts[TX_MAPED_RALLY_POINT_HINT], [gRes.Houses[htBarracks].HouseName]);;
    Button_Barracks_RallyPoint.OnClick := SetRallyPointClick;



  top := 0;
  J := 0;
  LastID := 0;
  dY := 0;
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
      dX := ((LastID) mod 5) * 31;
      dY := top + ((LastID) div 5) * 42;
      Button_Barracks[J] := TKMButtonFlat.Create(Panel_HouseBarracks, dX, dY, 28, 38, 0);
      Button_Barracks[J].TexOffsetX := 1;
      Button_Barracks[J].TexOffsetY := 1;
      Button_Barracks[J].CapOffsetY := 2;
      Button_Barracks[J].Tag := I;
      Button_Barracks[J].TexID := gRes.Wares[BarracksResOrder[I]].GUIIcon;
      Button_Barracks[J].Hint := gRes.Wares[BarracksResOrder[I]].Title;
      Button_Barracks[J].OnClick := BarracksSelectWare;
      Button_Barracks[J].OnMouseWheel := BarracksChange2;
      Inc(J);
      Inc(LastID);
    end;

    LastID := 0;
    for I := rightIconStart to High(Button_Barracks) do
    begin
      dX := Panel_HouseBarracks.Width - 35;
      dY := 42 + LastID * 42;
      Button_Barracks[I].Left := dX;
      Button_Barracks[I].Top := dY;
      Inc(LastID);
    end;


    //Button_Barracks_Recruit := TKMButtonFlat.Create(Panel_HouseBarracks, (BARRACKS_RES_COUNT mod 6)*31,26+8+(BARRACKS_RES_COUNT div 6)*42,28,38,0);
    Button_Barracks_Recruit := TKMButtonFlat.Create(Panel_House, Button_HouseRepair.Right + 2, Button_HouseRepair.Top - 4 ,28,38,0);
    Button_Barracks_Recruit.Tag := -1;
    Button_Barracks_Recruit.TexOffsetX := 1;
    Button_Barracks_Recruit.TexOffsetY := 1;
    Button_Barracks_Recruit.CapOffsetY := 2;
    Button_Barracks_Recruit.TexID := gRes.Units[utRecruit].GUIIcon;
    Button_Barracks_Recruit.Hint := gRes.Units[utRecruit].GUIName;
    Button_Barracks_Recruit.OnClick := BarracksSelectWare;
    Button_Barracks_Recruit.OnMouseWheel := BarracksChange2;

    top := Button_Barracks[rightIconStart - 1].Bottom + 20;
    left := 9;

    Button_BarracksDec100     := TKMButton.Create(Panel_HouseBarracks,left,      top,      20, 20, '<', bsGame);
    Button_BarracksDec100.Tag := 100;
    Button_BarracksDec        := TKMButton.Create(Panel_HouseBarracks,left,      top + 20, 20, 20, '-', bsGame);
    Button_BarracksDec.Tag    := 1;
    Label_Barracks_WareCount  := TKMLabel.Create (Panel_HouseBarracks,left + 20, top + 12, 50, 20, '', fntMetal, taCenter);
    Button_BarracksInc100     := TKMButton.Create(Panel_HouseBarracks,left + 70, top,      20, 20, '>', bsGame);
    Button_BarracksInc100.Tag := 100;
    Button_BarracksInc        := TKMButton.Create(Panel_HouseBarracks,left + 70, top + 20, 20, 20, '+', bsGame);
    Button_BarracksInc.Tag    := 1;
    Button_BarracksDec100.OnClickShift := BarracksChange;
    Button_BarracksDec.OnClickShift    := BarracksChange;
    Button_BarracksInc100.OnClickShift := BarracksChange;
    Button_BarracksInc.OnClickShift    := BarracksChange;
end;


procedure TKMMapEdHouse.BarracksChange2(Sender: TObject; WheelSteps: Integer; var aHandled: Boolean);
var I : Integer;
  ware: TKMWareType;
  barracks: TKMHouseBarracks;
  newCount: Word;
begin
  if not Panel_HouseBarracks.Visible then Exit;
  if not (Sender is TKMButtonFlat) then Exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then Exit; //with set Tag
  if not (fHouse is TKMHouseBarracks) then Exit;
  if aHandled then Exit;

  aHandled :=  WheelSteps <> 0;
  TKMButtonFlat(Sender).Focus;
  fBarracksItem := -1;
  for I := 0 to high(Button_Barracks) do
  begin
    Button_Barracks[I].Down := False;
    if Sender = Button_Barracks[I] then
      fBarracksItem := I;

  end;

  TKMButtonFlat(Sender).Down := True;

  barracks := TKMHouseBarracks(fHouse);

  newCount := 1;

  if ssCtrl in gCursor.SState then
    newCount := newCount * 5;

  if (ssShift in gCursor.SState) then
    newCount := newCount * 10;

  if (ssAlt in gCursor.SState) then
    newCount := newCount * 20;

  if fBarracksItem = -1 then
  begin
    // Recruits
    if WheelSteps < 0 then
      barracks.MapEdRecruitCount := Math.Max(0,  barracks.MapEdRecruitCount - newCount)
    else
    if WheelSteps > 0 then
      barracks.MapEdRecruitCount := Math.Min(High(Word),  barracks.MapEdRecruitCount + newCount);

    Label_Barracks_WareCount.Caption := IntToStr(barracks.MapEdRecruitCount);
  end else
  if (BarracksResOrder[Button_Barracks[fBarracksItem].Tag] in WARES_WARFARE) then
  begin
    ware := BarracksResOrder[Button_Barracks[fBarracksItem].Tag];

    if WheelSteps > 0 then
      barracks.WareAddToIn(ware, newCount)
    else
    if WheelSteps < 0 then
    begin
      newCount := Math.Min(barracks.CheckWareIn(ware), newCount);
      barracks.WareTakeFromOut(ware, newCount);
    end;
    Label_Barracks_WareCount.Caption := IntToStr(barracks.CheckWareIn(ware));

  end;


  BarracksRefresh;
end;


procedure TKMMapEdHouse.Create_TownHall;
begin
  Panel_HouseTownHall := TKMPanel.Create(Panel_House,0,76 + 40,Panel_House.Width,400);

    Button_TownHall_RallyPoint := TKMButtonFlat.Create(Panel_HouseTownHall, 0, 8, Panel_House.Width, 22, 0);
    Button_TownHall_RallyPoint.CapOffsetY := -11;
    Button_TownHall_RallyPoint.Caption := gResTexts[TX_HOUSES_RALLY_POINT];
    Button_TownHall_RallyPoint.Hint := Format(gResTexts[TX_MAPED_RALLY_POINT_HINT], [gRes.Houses[htTownhall].HouseName]);
    Button_TownHall_RallyPoint.OnClick := SetRallyPointClick;
end;



procedure TKMMapEdHouse.Hide;
begin
  Panel_House.Hide;
  Panel_HouseAdditional.Hide;

end;


function TKMMapEdHouse.Visible: Boolean;
begin
  Result := Panel_House.Visible;
end;


procedure TKMMapEdHouse.UpdateState;
begin
  if Visible then
  begin
    case fHouse.HouseType of
      htBarracks:    Button_Barracks_RallyPoint.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_RALLY_POINT);
      htTownHall:    Button_TownHall_RallyPoint.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_RALLY_POINT);
      htWoodcutters: Button_Woodcutters_CuttingPoint.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_RALLY_POINT);
    end;
    Panel_House.UpdateScrolls;
  end;

end;



procedure TKMMapEdHouse.HouseSignClose(sender : TObject);
begin
  gMySpectator.Selected := nil;
  Panel_HouseSign.Hide;
end;


procedure TKMMapEdHouse.HouseSignChange(Sender : TObject);
begin

  Edit_SignMessage.Visible := Edit_HouseSign.Value = -1;

  if Edit_HouseSign.Value > -1 then
    fHouse.Text := '<$' + IntToStr(Edit_HouseSign.Value) + '>'
  else
    fHouse.Text := Edit_SignMessage.Text;

  if Edit_SignMessage.Visible then
    Label_SignMessage.Caption := fHouse.Text
  else
    Label_SignMessage.Caption := gGame.TextMission.ParseTextMarkup(UnicodeString(fHouse.Text));
end;

procedure TKMMapEdHouse.HouseCheckBoxClick(Sender: TObject);
begin
  fHouse.Indestructible := CheckBox_Indestructible.Checked;
end;


procedure TKMMapEdHouse.HideAllCommonResources;
var
  I: Integer;
begin
  Label_House_Input.Hide;
  Label_House_Output.Hide;
  for I := 0 to high(ResRow_Ware_Input) do
  begin
    ResRow_Ware_Input[I].Hide;
    Button_BlockWare[I].Hide;
    ResRow_Ware_Output[I].Hide;
  end;
end;


procedure TKMMapEdHouse.ShowCommonResources;
var
  I: Integer;
  lastRow, line : Byte;
  ware: TKMWareType; //todo: Change to wareSpec
begin

  Label_House_Input.Hide;

  for I := 0 to high(ResRow_Ware_Input) do
  begin
    ResRow_Ware_Input[I].Hide;
    ResRow_Ware_Output[I].Hide;
    Button_BlockWare[I].Hide;
  end;

  lastRow := 0;
  line := 0;
  for I := 0 to high(ResRow_Ware_Input) do
  begin
    //ware := houseSpec.WareInput[I+1];
    ware := fHouse.WareInput[I + 1];
    if gRes.Wares[ware].IsValid then
    begin
      ResRow_Ware_Input[lastRow].Top := 145 + line * 25;
      ResRow_Ware_Input[lastRow].WareRow.TexID := gRes.Wares[ware].GUIIcon;
      ResRow_Ware_Input[lastRow].WareRow.Caption := gRes.Wares[ware].Title;
      ResRow_Ware_Input[lastRow].Hint := gRes.Wares[ware].Title;
      ResRow_Ware_Input[lastRow].WareRow.WareCount := fHouse.CheckWareIn(ware);
      ResRow_Ware_Input[lastRow].OrderCntMax := fHouse.GetMaxInWare;
      ResRow_Ware_Input[lastRow].OrderCount := fHouse.CheckWareIn(ware);
      ResRow_Ware_Input[lastRow].Show;
      Button_BlockWare[lastRow].Top := ResRow_Ware_Input[lastRow].Top;
      Button_BlockWare[lastRow].TexID := ifThen(fHouse.GetAcceptWareIn(ware) > 0, 32, 33);
      Button_BlockWare[lastRow].Show;
      Label_House_Input.Show;
      Inc(lastRow);
      Inc(line);
    end;

      //ResRow_Ware_Input[I].Hide;
  end;

  Label_House_Output.Hide;
  lastRow := 0;
  Label_House_Output.Top := 145 + line * 25;
  for I := 0 to high(ResRow_Ware_Output) do
  begin
    ware := fHouse.WareOutput[I+1];
    if gRes.Wares[ware].IsValid then
    begin
      ResRow_Ware_Output[lastRow].Top := 170 + line * 25;
      ResRow_Ware_Output[lastRow].WareRow.TexID := gRes.Wares[ware].GUIIcon;
      ResRow_Ware_Output[lastRow].WareRow.Caption := gRes.Wares[ware].Title;
      ResRow_Ware_Output[lastRow].Hint := gRes.Wares[ware].Title;
      ResRow_Ware_Output[lastRow].OrderCntMax := fHouse.GetMaxOutWare;
      ResRow_Ware_Output[lastRow].WareRow.WareCount := fHouse.CheckWareOut(ware);
      ResRow_Ware_Output[lastRow].OrderCount := fHouse.CheckWareOut(ware);
      ResRow_Ware_Output[lastRow].Show;
      Label_House_Output.Show;
      Inc(lastRow);
      Inc(line);
    end;

  end;
end;


procedure TKMMapEdHouse.HandleHouseClosedForWorker(aHouse: TKMHouse);
begin
  if aHouse.IsClosedForWorker then
  begin
    Button_House_Worker.ShowImageEnabled := False;
    Image_House_Worker_Closed.Show;
  end else begin
    Button_House_Worker.ShowImageEnabled := aHouse.HasWorker;
    Image_House_Worker_Closed.Hide;
  end;
end;


procedure TKMMapEdHouse.Show(aHouse: TKMHouse);
var
  houseSpec: TKMHouseSpec;
  I : Integer;
  C: Cardinal;
begin
  fHouse := aHouse;
  if fHouse = nil then Exit;

  houseSpec := gRes.Houses[fHouse.HouseType];
  if fHouse.BuildingState <> hbsDone then
  begin
    Panel_House.Hide;
    Panel_HouseAdditional.Hide;
    Panel_Construction.Show;

    ResRow_Ware_Build[0].OrderCntMax := fHouse.HSpec.WoodCost;
    ResRow_Ware_Build[1].OrderCntMax := fHouse.HSpec.StoneCost;
    ResRow_Ware_Build[2].OrderCntMax := fHouse.HSpec.TileCost;


    Label_HouseConstruction.Caption := houseSpec.HouseName;
    Image_HouseConstruction_Logo.TexID := houseSpec.GUIIcon;
    Panel_HouseContructionChange(nil, []);
    Panel_House.Hide;
    Exit;
  end;
  Panel_Construction.Hide;
  Panel_House.Show;
  Panel_HouseAdditional.Show;
  {Common data}
  Label_House.Caption := houseSpec.HouseName;
  Image_House_Logo.TexID := houseSpec.GUIIcon;

  HealthBar_House.Caption := IntToStr(Round(fHouse.GetHealth)) + '/' + IntToStr(fHouse.MaxHealth);
  HealthBar_House.Position := fHouse.GetHealth / fHouse.MaxHealth;

  if not (fHouse.HouseType in [htSign]) then
    ShowCommonResources
  else
    HideAllCommonResources;

  Edit_HouseFlagColor.Visible := fHouse.IsValid;
  Shape_FlagColor.Visible := Edit_HouseFlagColor.Visible;
  if Edit_HouseFlagColor.Visible then
  begin
    C := fHouse.FlagColor;
    if C = 0 then
      C := gHands[fHouse.Owner].FlagColor;
    C := C or $FF000000;

    Edit_HouseFlagColor.SetTextSilently( Format('%.6x', [C and $FFFFFF]) );
    Shape_FlagColor.ShapeColor := C;
  end;

  CheckBox_Indestructible.Checked := fHouse.Indestructible;

  Label_SingCLickCTRL.Hide;
  House_RefreshCommon;
  RefreshStyle(fHouse.Style);
  Button_Level.Hide;
  if length(houseSpec.Levels) > 0 then
  begin
    Button_Level.Tag := fHouse.CurrentLevel;
    Button_Level.Show;
    if fHouse.IsMaxLevel then
      Button_Level.TexID := 746
    else
      Button_Level.TexID := 748

  end;

  Button_HouseForceWork.Hide;
  Button_HouseNoRes.Hide;
  if houseSpec.AcceptsWares and not (aHouse.HouseType in [htStore, htPalace, htInn]) then
  begin
    Button_HouseNoRes.Show;
    Button_HouseNoRes.TexID :=  IfThen(fHouse.DontNeedRes, 718, 717);
  end;
  if houseSpec.CanForceWork then
  begin
    Button_HouseForceWork.Show;
    Button_HouseForceWork.TexID :=  IfThen(fHouse.ForceWorking, 770, 769);
  end;

  SortVisibleControls(0, 72, Panel_HouseAdditional.Width, 0, [Button_HouseStyles, Button_HouseWariant, Button_Level, Button_HouseForceWork, Button_HouseNoRes]);

  for I := 0 to High(Button_PlayerSelect) do
    Button_PlayerSelect[I].Hide;
  for I := 0 to Panel_House.ChildPanel.ChildCount - 1 do
    if Panel_House.ChildPanel.Childs[I] is TKMPanel then
      Panel_House.ChildPanel.Childs[I].Hide;


  Button_StoreDec100.Visible := false;
  Button_StoreDec.Visible := false;
  Image_Store_Selected.Visible := false;
  Label_Store_WareCount.Visible := false;
  Button_StoreInc100.Visible := false;
  Button_StoreInc.Visible := false;
  Image_TotalCount.Visible := false;
  Bar_TotalCount.Visible := false;
  Bevel_Store.Visible := false;
  Button_PasteWares.Visible := false;
  Button_CopyWares.Visible := false;
  Button_Barracks_Recruit.Hide;

  Button_Grain.Visible := fHouse.HouseType in [htFarm, htProductionThatch];
  Button_Grass.Visible := fHouse.HouseType in [htFarm, htProductionThatch];
  Button_Vege.Visible := fHouse.HouseType in [htFarm, htProductionThatch];
  if Button_Vege.Visible then
  begin
    if fHouse.HouseType = htFarm then
    begin
      Button_Grass.TexID := GRAIN_GUI_PIC[TFarm(aHouse).GrassType];
      Button_Grass.Hint := gResTexts[GRAIN_GUI_HINT[TFarm(aHouse).GrassType]];

      Button_Grain.TexID := GRAIN_GUI_PIC[TFarm(aHouse).GrainType] ;
      Button_Grain.Hint := gResTexts[GRAIN_GUI_HINT[TFarm(aHouse).GrainType]];

      Button_Vege.TexID := GRAIN_GUI_PIC[TFarm(aHouse).VegeType] ;
      Button_Vege.Hint := gResTexts[GRAIN_GUI_HINT[TFarm(aHouse).VegeType]];
    end else
    if fHouse.HouseType = htProductionThatch then
    begin
      Button_Grass.TexID := GRAIN_GUI_PIC[TThatch(aHouse).GrassType];
      Button_Grass.Hint := gResTexts[GRAIN_GUI_HINT[TThatch(aHouse).GrassType]];

      Button_Grain.TexID := GRAIN_GUI_PIC[TThatch(aHouse).GrainType] ;
      Button_Grain.Hint := gResTexts[GRAIN_GUI_HINT[TThatch(aHouse).GrainType]];

      Button_Vege.TexID := GRAIN_GUI_PIC[TThatch(aHouse).VegeType] ;
      Button_Vege.Hint := gResTexts[GRAIN_GUI_HINT[TThatch(aHouse).VegeType]];
    end;

  end;

  case fHouse.HouseType of
    htStore:       begin
                      Bevel_Store.Show;
                      Button_StoreDec100.Visible := true;
                      Button_StoreDec.Visible := true;
                      Image_Store_Selected.Visible := true;
                      Label_Store_WareCount.Visible := true;
                      Button_StoreInc100.Visible := true;
                      Button_StoreInc.Visible := true;
                      Image_TotalCount.Visible := true;
                      Bar_TotalCount.Visible := true;
                      Button_PasteWares.Visible := true;
                      Button_CopyWares.Visible := true;
                      Button_PasteWares.Enabled := fStoreHOuseWaresCopied;
                      Panel_HouseStore.Show;
                      StoreRefresh;
                      //Reselect the ware so the display is updated
                      StoreSelectWare(Button_Store[fStorehouseItem]);
                    end;
    htBarracks:   begin
                      Button_Barracks_Recruit.Show;
                      Panel_HouseBarracks.Show;
                      BarracksRefresh;
                      //In the barrack the recruit icon is always enabled
                      Image_House_Worker.Hide;
                      //Image_House_Worker.Enable;
                      Button_House_Worker.Visible := False;
                      Button_Barracks_Recruit.FlagColor := gHands[fHouse.Owner].FlagColor;
                      //Reselect the ware so the display is updated
                      if fBarracksItem = -1 then
                        BarracksSelectWare(Button_Barracks_Recruit)
                      else
                        BarracksSelectWare(Button_Barracks[fBarracksItem]);
                    end;
    htWoodcutters: begin
                      Panel_HouseWoodcutters.Show;
                      WoodcuttersRefresh;
                    end;
    htSign:         begin
                      Label_SingCLickCTRL.Show;

                      if ssCtrl in gCursor.SState then
                      begin
                        Panel_HouseSign.Show;
                        SignRefresh;
                      end;
                    end;
    htMerchant:     begin
                      MerchantChange(nil);
                    end;
    htPearl:        begin
                      Panel_HousePearl.Show;
                      Pearl_Refresh;
                    end;
    htForest:       begin
                      Panel_HouseForest.Show;
                      Forest_Refresh;
                    end;
    htPasture:      begin
                      Panel_HousePasture.Show;
                      Pasture_Refresh;
                    end;
  else
    Panel_HouseWoodcutters.Hide;
    Panel_House.Show;
  end;
  Panel_HouseAdditional.Show;
  Panel_House.HideScrolls := Panel_HouseStore.Visible;
end;


procedure TKMMapEdHouse.StoreRefresh;
var
  I, tmp: Integer;
  H : TKMHouseStore;
  W : TKMWareType;
begin
  H := TKMHouseStore(fHouse);
  for I := 0 to high(Button_Store) do
  begin
    W := TKMWareType(Button_Store[I].Tag);
    tmp := H.CheckWareIn(W);
    Button_Store[I].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp));
  end;

  Bar_TotalCount.Position := (H.TotalCount / H.MaxCount);
  Bar_TotalCount.Caption :=  IntToStr(Round(H.TotalCount / H.MaxCount * 100)) + '%';

  if Bar_TotalCount.Position >= 1 then
    Bar_TotalCount.MainColor := icRed
  else
  if Bar_TotalCount.Position >= 0.90 then
    Bar_TotalCount.MainColor := icDarkOrange
  else
  if Bar_TotalCount.Position >= 0.70 then
    Bar_TotalCount.MainColor := icOrange
  else
    Bar_TotalCount.MainColor := icGreen;

  if Bar_TotalCount.Position >= 1 then
    Image_TotalCount.TexID := 718
  else
    Image_TotalCount.TexID := 717;

end;


procedure TKMMapEdHouse.House_RefreshRepair;
begin
  Button_HouseRepair.TexID := IfThen(fHouse.BuildingRepair, 39, 40);
  Button_HouseRepair.Show;
end;


procedure TKMMapEdHouse.House_RefreshCommon;
var
  houseSpec: TKMHouseSpec;
  I : Integer;
  addToTop : Integer;
begin
  houseSpec := gRes.Houses[fHouse.HouseType];
  addToTop := 0;
  House_UpdateDeliveryMode(fHouse.DeliveryMode);
  Button_HouseDeliveryMode.Enabled := fHouse.AllowDeliveryModeChange;
  Button_HouseDeliveryMode.Show;

  House_RefreshRepair;

  Button_House_Worker.TexID  := gRes.Units[houseSpec.WorkerType].GUIIcon;
  HandleHouseClosedForWorker(fHouse);
  Button_House_Worker.Hint := Format(gResTexts[TX_HOUSES_CLOSED_FOR_WORKER_HINT], [gRes.Units[houseSpec.WorkerType].GUIName]);
  Button_House_Worker.FlagColor := gHands[fHouse.Owner].FlagColor;
  Button_House_Worker.Visible := gRes.Houses[fHouse.HouseType].CanHasWorker;
  Image_House_Worker.TexID := gRes.Units[houseSpec.WorkerType].GUIIcon;
  Image_House_Worker.FlagColor := gHands[fHouse.Owner].FlagColor;
  Image_House_Worker.Hint := gRes.Units[houseSpec.WorkerType].GUIName;
  Image_House_Worker.Hide; // show it on special pages (like Barracks, f.e.)

  for I := 0 to high(ResRow_Ware_Input) do
  begin
    ResRow_Ware_Input[I].WareRow.WareCntAsNumber := houseSpec.MaxWareCount >= 7;
    ResRow_Ware_Input[I].OrderCntMax := houseSpec.MaxWareCount;
  end;
  Button_HouseStyles.Hide;

  if length(houseSpec.Styles) > 0 then
  begin
    Inc(addToTop, 15);
    Button_HouseStyles.Show;
    Button_HouseStyles.TexID := 399;
  end;
  Button_HouseWariant.Visible := fHouse.HSpec.HasStoneWariants > 0;
  Button_HouseWariant.Caption := IntToStr(fHouse.PicWariant + 1);

  for I := 0 to High(Button_WareInputSlot) do
    Button_WareInputSlot[I].Hide;

  if Length(houseSpec.WareInputSlots) > 0 then
  begin
    Inc(addToTop, ( length(houseSpec.WareInputSlots) div 5) * 15 + 15);
    for I := 0 to high(Button_WareInputSlot) do
      if I < Length(houseSpec.WareInputSlots) then
      begin
        Button_WareInputSlot[I].Top := 100 + (I div 5 * 25) + ((length(houseSpec.Styles) + 5) div 5) * 33;
        Button_WareInputSlot[I].Show;
        Button_WareInputSlot[I].Enabled := fHouse.CanChangeWareInput;
        Button_WareInputSlot[I].TexID := houseSpec.WareInputSlots[I].Icon;
        Button_WareInputSlot[I].ShowImageEnabled := (fHouse.WareInputSlot = I);
      end;
  end;

  Panel_House.Top := 60 + addToTop;

end;


procedure TKMMapEdHouse.BarracksRefresh;
var
  I, tmp: Integer;
  W : TKMWareType;
begin
  for I := 0 to high(Button_Barracks) do
  begin
    W := BarracksResOrder[Button_Barracks[I].Tag];
    tmp := TKMHouseBarracks(fHouse).CheckWareIn(W);
    Button_Barracks[I].Caption := IfThen(tmp = 0, '-', IntToKStr(tmp, 1000));
  end;
  tmp := TKMHouseBarracks(fHouse).MapEdRecruitCount;
  Button_Barracks_Recruit.Caption := IfThen(tmp = 0, '-', IntToKStr(tmp, 1000));
  Button_Barracks_RallyPoint.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_RALLY_POINT);
end;

procedure TKMMapEdHouse.WoodcuttersRefresh;
begin
  Button_Woodcutters_CuttingPoint.Down := (gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_RALLY_POINT);
end;

procedure TKMMapEdHouse.SignRefresh;

  function GetNumber(const aText : String) : Integer;
  var I : integer;
      srcText : String;
  begin
    Result := -1;
    if (length(aText) < 2) or (length(aText) > 8) then
      Exit;
    I := 1;
    srcText := '';
    
    if (aText[1] <> '<') or (aText[2] <> '$') then Exit;

    while I < length(aText) do
    begin
      if (aText[I] <> '<') and (aText[I] <> '$') then
        srcText := srcText + aText[I];
      Inc(I);
      if (aText[I] = '>') then
        If TryStrToInt(srcText, Result) then
          Break
        else
        begin
          Result := -1;
          Exit;
        end;
    end;
  end;

begin
  Edit_HouseSign.Value := GetNumber(fHouse.Text);
  if Edit_HouseSign.Value = -1 then
    Edit_SignMessage.Text := fHouse.Text;
  HouseSignChange(nil);
end;

procedure TKMMapEdHouse.HouseHealthChange(Sender: TObject; Shift: TShiftState);
begin
  if Sender = Button_HouseHealthDec then fHouse.AddDamage(GetMultiplicator(Shift), nil, True);
  if Sender = Button_HouseHealthInc then fHouse.AddRepair(GetMultiplicator(Shift));
  HealthBar_House.Caption := IntToStr(Round(fHouse.GetHealth)) + '/' + IntToStr(fHouse.MaxHealth);
  HealthBar_House.Position := fHouse.GetHealth / fHouse.MaxHealth;
end;

procedure TKMMapEdHouse.HouseChange(Sender: TObject; aValue: Integer);
var
  I: Integer;
  ware: TKMWareType;
  newCountAdd: Integer;
  lastRow : Byte;
begin
  House_RefreshCommon;

  lastRow := 0;
  for I := 0 to high(ResRow_Ware_Input) do
  begin
    //ware := houseSpec.WareInput[I+1];
    Ware := fHouse.WareInput[I + 1];
    if not (ware in [WARE_MIN..WARE_MAX]) then Continue;

    if (Sender = ResRow_Ware_Input[lastRow]) and (aValue > 0) then
    begin
      newCountAdd := Math.Min(aValue, fHouse.GetMaxInWare - fHouse.CheckWareIn(ware));
      fHouse.WareAddToIn(ware, newCountAdd);
    end;

    if (Sender = ResRow_Ware_Input[lastRow]) and (aValue < 0) then
    begin
      newCountAdd := Math.Min(Abs(aValue), fHouse.CheckWareIn(ware));
      fHouse.WareTakeFromIn(ware, newCountAdd);
    end;

    ResRow_Ware_Input[lastRow].OrderCntMax := fHouse.GetMaxInWare;
    ResRow_Ware_Input[lastRow].OrderCount := fHouse.CheckWareIn(ware);
    ResRow_Ware_Input[lastRow].WareRow.WareCount := ResRow_Ware_Input[lastRow].OrderCount;
    Inc(lastRow);
  end;

  lastRow := 0;
  for I := 0 to high(ResRow_Ware_Output) do
  begin
    ware := fHouse.WareOutput[I+1];
    if not (ware in WARES_VALID) then Continue;

    if (Sender = ResRow_Ware_Output[lastRow]) and (aValue > 0) then
    begin
      newCountAdd := Math.Min(aValue, fHouse.GetMaxOutWare - fHouse.CheckWareOut(ware));
      if gRes.Houses[fHouse.HouseType].IsWorkshop then
        newCountAdd := Math.Min(newCountAdd, MAX_WARES_OUT_WORKSHOP - fHouse.CheckWareOut(wtAll));
      fHouse.WareAddToOut(ware, newCountAdd);
    end;

    if (Sender = ResRow_Ware_Output[lastRow]) and (aValue < 0) then
    begin
      newCountAdd := Math.Min(Abs(aValue), fHouse.CheckWareOut(ware));
      fHouse.WareTakeFromOut(ware, newCountAdd);
    end;

    ResRow_Ware_Output[lastRow].OrderCntMax := fHouse.GetMaxOutWare;
    ResRow_Ware_Output[lastRow].OrderCount := fHouse.CheckWareOut(ware);
    ResRow_Ware_Output[lastRow].WareRow.WareCount := ResRow_Ware_Output[lastRow].OrderCount;
    Inc(lastRow);
  end;
end;


procedure TKMMapEdHouse.HouseHealthClickHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
var
  I: Integer;
begin
  for I := 0 to 3 do
    if (Sender = Button_HouseHealthDec) or (Sender = Button_HouseHealthInc) then
      HouseHealthChange(Sender, GetShiftState(AButton));
end;


procedure TKMMapEdHouse.SetRallyPointClick(Sender: TObject);
var
  btn: TKMButtonFlat;
begin
  if (Sender <> Button_Barracks_RallyPoint)
  and (Sender <> Button_TownHall_RallyPoint)
  and (Sender <> Button_Woodcutters_CuttingPoint) then
    Exit;

  btn := TKMButtonFlat(Sender);

  btn.Down := not btn.Down;
  if btn.Down then
  begin
    gCursor.Mode := cmMarkers;
    gCursor.Tag1 := MARKER_RALLY_POINT;
  end else
    gCursor.Mode := cmNone;
end;


procedure TKMMapEdHouse.House_UpdateDeliveryMode(aMode: TKMDeliveryMode);
begin
  Button_HouseDeliveryMode.TexID := DELIVERY_MODE_SPRITE[aMode];
end;


procedure TKMMapEdHouse.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  if (Key = VK_ESCAPE)
  and Visible
  and (gMySpectator.Selected <> nil) then
  begin
    gMySpectator.Selected := nil;
    Hide;
    aHandled := True;
  end;
end;

procedure TKMMapEdHouse.House_DeliveryModeToggle(Sender: TObject; Shift: TShiftState);
begin
  if ssLeft in Shift then
    fHouse.SetNextDeliveryMode
  else if ssRight in Shift then
    fHouse.SetPrevDeliveryMode;

  // Apply changes immediately
  fHouse.SetDeliveryModeInstantly(fHouse.NewDeliveryMode);

  House_UpdateDeliveryMode(fHouse.DeliveryMode);
end;


procedure TKMMapEdHouse.House_RepairToggle(Sender: TObject);
begin
  fHouse.BuildingRepair := not fHouse.BuildingRepair;
  House_RefreshRepair;
end;


procedure TKMMapEdHouse.House_ClosedForWorkerToggle(Sender: TObject);
begin
  fHouse.IsClosedForWorker := not fHouse.IsClosedForWorker;
  House_RefreshCommon;
end;


procedure TKMMapEdHouse.BarracksSelectWare(Sender: TObject);
var
  I: Integer;
begin
  if not Panel_HouseBarracks.Visible then Exit;
  if not (Sender is TKMButtonFlat) then Exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then Exit; //with set Tag

  Button_Barracks_Recruit.Down := False;
  for I := 0 to high(Button_Barracks) do
  begin
    Button_Barracks[I].Down := False;
    if Sender = Button_Barracks[I] then
      fBarracksItem := I;

  end;
  TKMButtonFlat(Sender).Down := True;
  BarracksChange(Sender, []);
end;


procedure TKMMapEdHouse.StoreSelectWare(Sender: TObject);
var
  I: Integer;
begin
  if not Panel_HouseStore.Visible then Exit;
  if not (Sender is TKMButtonFlat) then Exit; //Only FlatButtons

  for I := 0 to high(Button_Store) do
    Button_Store[I].Down := False;

  TKMButtonFlat(Sender).Down := True;
  fStorehouseItem := TKMButtonFlat(Sender).Tag2;
  StoreChange(Sender, []);
end;


procedure TKMMapEdHouse.BarracksChange(Sender: TObject; Shift: TShiftState);
var
  ware: TKMWareType;
  barracks: TKMHouseBarracks;
  newCount: Word;
begin
  barracks := TKMHouseBarracks(fHouse);
  if fBarracksItem = -1 then
  begin
    // Recruits
    if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then
      barracks.MapEdRecruitCount := Math.Max(0, barracks.MapEdRecruitCount - GetMultiplicator(Shift) * TKMButton(Sender).Tag);

    if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then
      barracks.MapEdRecruitCount := Math.Min(High(Word), barracks.MapEdRecruitCount + GetMultiplicator(Shift) * TKMButton(Sender).Tag);

    Label_Barracks_WareCount.Caption := IntToStr(barracks.MapEdRecruitCount);
  end else
  begin
    // Wares
    ware := BarracksResOrder[Button_Barracks[fBarracksItem].Tag];

    if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then
    begin
      newCount := Math.Min(barracks.CheckWareIn(ware), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
      barracks.WareTakeFromOut(ware, newCount);
    end;

    if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then
    begin
      newCount := Math.Min(High(Word) - barracks.CheckWareIn(ware), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
      barracks.WareAddToIn(ware, newCount);
    end;

    Label_Barracks_WareCount.Caption := IntToStr(barracks.CheckWareIn(ware));
  end;

  BarracksRefresh;
end;


procedure TKMMapEdHouse.StoreChange(Sender: TObject; Shift: TShiftState);
var
  ware, WT: TKMWareType;
  store: TKMHouseStore;
  newCount: Word;
begin
  store := TKMHouseStore(fHouse);
  ware := TKMWareType(Button_Store[fStorehouseItem].Tag);

  If sender = Button_CopyWares then
  begin
    for WT := WARE_MIN to WARE_MAX do
        fStoreHouseWares[WT] := store.CheckWareIn(WT);
    fStoreHOuseWaresCopied := true;
    Button_PasteWares.Enabled := fStoreHOuseWaresCopied;
    Exit;
  end else
  If sender = Button_PasteWares then
  begin
    If fStoreHOuseWaresCopied then
      for WT := WARE_MIN to WARE_MAX do
        store.SetWareInCnt(WT, fStoreHouseWares[WT]);
  end;
  //We need to take no more than it is there, thats part of bugtracking idea
  if (Sender = Button_StoreDec100) or (Sender = Button_StoreDec) then begin
    newCount := Math.Min(store.CheckWareIn(ware), GetMultiplicator(Shift) * TKMButton(Sender).Tag);
    store.WareTakeFromOut(ware, newCount);
  end;

  //We can always add any amount of resource, it will be capped by Store
  if (Sender = Button_StoreInc100) or (Sender = Button_StoreInc) then
    store.WareAddToIn(ware, GetMultiplicator(Shift) * TKMButton(Sender).Tag);

  Label_Store_WareCount.Caption := inttostr(store.CheckWareIn(ware));
  Image_Store_Selected.TexID := gRes.Wares[ware].GUIIcon;
  StoreRefresh;
end;

procedure TKMMapEdHouse.ChangeStyle(Sender: TObject);
begin
  RefreshStyle(TKMButtonFlat(Sender).Tag);
end;

procedure TKMMapEdHouse.HouseSetLevel(Sender : TObject; Shift: TShiftState);
var aCount : Byte;
  I, J : Integer;
begin
  aCount := 1;
  if ssShift in Shift then
    aCount := 5;

  J := fHouse.CurrentLevel;
  if ssLeft in Shift then
  begin
    IncLoop(J, 0, length(gRes.Houses[fHouse.HouseType].Levels), aCount);
  end
  else
  if ssRight in Shift then
    IncLoop(J, 0, length(gRes.Houses[fHouse.HouseType].Levels), -aCount);

  fHouse.CurrentLevel := J;

  if fHouse.IsMaxLevel then
    Button_Level.TexID := 746
  else
    Button_Level.TexID := 748;

  for I := Low(ResRow_Ware_Input) to High(ResRow_Ware_Input) do
    if ResRow_Ware_Input[I].OrderCount > fHouse.GetMaxInWare then
    begin
      ResRow_Ware_Input[I].WareRow.WareCount := fHouse.GetMaxInWare;
      ResRow_Ware_Input[I].OrderCount := fHouse.GetMaxInWare;
      fHouse.SetWareInCnt(fHouse.WareInput[I + 1], ResRow_Ware_Input[I].OrderCount)
    end;

  HouseChange(nil, 0);

  HealthBar_House.Caption := IntToStr(Round(fHouse.GetHealth)) + '/' + IntToStr(fHouse.MaxHealth);
  HealthBar_House.Position := fHouse.GetHealth / fHouse.MaxHealth;

end;

Procedure TKMMapEdHouse.RefreshStyle(aID : Byte = 0);
var I : Integer;
begin
  fHouse.Style := aID;

  I := aID + 1;

  if I > length(gRes.Houses[fHouse.HouseType].Styles) then
    I := 0;

  Button_HouseStyles.Tag := I;
  if aID = 0 then
    Button_HouseStyles.TexID := 399
  else
    Button_HouseStyles.TexID := gRes.Houses[fHouse.HouseType].Styles[aID - 1].Icon;


end;

procedure TKMMapEdHouse.HouseSetWareInput(Sender : TObject);
var
  I : Integer;
begin
  if not (Sender is TKMButton) then
    Exit;


  if fHouse = nil then
    Exit;

  fHouse.SetWareSlot(TKMButton(Sender).Tag, true);

  for I := 0 to high(Button_WareInputSlot) do
    Button_WareInputSlot[I].ShowImageEnabled := fHouse.WareInputSlot = I;

  ShowCommonResources;
end;

procedure TKMMapEdHouse.HouseToggleAcceptWare(Sender : TObject);
var I : Integer;
  WT : TKMWareType;
begin
  WT := fHouse.WareInput[TKMControl(Sender).Tag + 1];
  if fHouse.GetAcceptWareIn(WT) > 0 then
    fHouse.ToggleAcceptWaresIn(WT, -100)
  else
    fHouse.ToggleAcceptWaresIn(WT, 100);

  for I := Low(Button_BlockWare) to High(Button_BlockWare) do
    if Button_BlockWare[I].Visible then
    begin
      WT := fHouse.WareInput[I + 1];
      Button_BlockWare[I].TexID := ifThen(fHouse.GetAcceptWareIn(WT) > 0, 32, 33);
    end;

end;

procedure TKMMapEdHouse.MerchantChange(Sender: TObject);
var I, lastRow, top : Integer;
begin
  if Sender <> nil then
    TKMHouseMerchant(fHouse).ToggleSendToHand(TKMControl(Sender).Tag);


  top := ResRow_Ware_Input[high(ResRow_Ware_Input)].Bottom + 20;
  lastRow := 0;

  for I := 0 to High(Button_PlayerSelect) do
    if (I < gHands.Count)
    and (gHands[I].Houses.Stores.Count > 0)
    and (gHands[fHouse.Owner].Alliances[I] = atAlly)
    and (I <> fHouse.Owner) then
    begin
      Button_PlayerSelect[I].Down := TKMHouseMerchant(fHouse).SendToHand[I];
      Button_PlayerSelect[I].Top := top + (lastRow div 6) * 30;
      Button_PlayerSelect[I].Left := (lastRow mod 6) * 30;
      Button_PlayerSelect[I].Show;
      Button_PlayerSelect[I].Down := TKMHouseMerchant(fHouse).SendToHand[I];
      Button_PlayerSelect[I].Hint := gResTexts[1718] + gHands[I].OwnerName;
      Button_PlayerSelect[I].ShapeColor := gHands[I].FlagColor;
      Inc(lastRow);
    end else
      Button_PlayerSelect[I].Hide;
end;

procedure TKMMapEdHouse.HouseSetForceWorking(Sender : TObject);
begin
  if Sender = Button_HouseNoRes then
  begin
    fHouse.DontNeedRes := not fHouse.DontNeedRes;
    Button_HouseNoRes.TexID :=  IfThen(fHouse.DontNeedRes, 718, 717);
  end else
  begin
    fHouse.ForceWorking := not fHouse.ForceWorking;
    Button_HouseForceWork.TexID :=  IfThen(fHouse.ForceWorking, 770, 769);
  end;
end;

procedure TKMMapEdHouse.Panel_FarmChange(Sender: TObject; Shift: TShiftState);
var I, J : Integer;
begin
  J := 1 - (2 * byte(ssRight in Shift));
  I := -1;
  if Sender = Button_Grain then I := 0 else
  if Sender = Button_Grass then I := 1 else
  if Sender = Button_Vege then I := 2;

  Assert(I <> -1, 'TKMMapEdHouse.Panel_FarmChange');

  if fHouse is TFarm then TFarm(fHouse).SetNextGrainType(J, I) else
  if fHouse is TThatch then TThatch(fHouse).SetNextGrainType(J, I);

  Show(fHouse);
end;

procedure TKMMapEdHouse.Panel_HouseBuildWaresChange(Sender: TObject; aCount: Integer);
var newCount : Word;
begin
  if Sender = ResRow_Ware_Build[0] then
  begin
    newCount := EnsureRange(fHouse.BuildSupplyWood + aCount, 0, fHouse.HSpec.WoodCost);
    ResRow_Ware_Build[0].OrderCount := newCOunt;
  end;
  if Sender = ResRow_Ware_Build[1] then
  begin
    newCount := EnsureRange(fHouse.BuildSupplyStone + aCount, 0, fHouse.HSpec.StoneCost);
    ResRow_Ware_Build[1].OrderCount := newCOunt;
  end;
  if Sender = ResRow_Ware_Build[2] then
  begin
    newCount := EnsureRange(fHouse.BuildSupplyTile + aCount, 0, fHouse.HSpec.TileCost);
    ResRow_Ware_Build[2].OrderCount := newCOunt;
  end;


  fHouse.SetBuildingProgress(fHouse.BuildingProgress, ResRow_Ware_Build[0].OrderCount,
                                                      ResRow_Ware_Build[1].OrderCount,
                                                      ResRow_Ware_Build[2].OrderCount);
  Panel_HouseContructionChange(Sender, []);
end;
procedure TKMMapEdHouse.Panel_HouseContructionChange(Sender: TObject; Shift: TShiftState);
begin

  if Sender = Button_BuildingProgressDec then fHouse.IncBuildingProgress(5 * GetMultiplicator(Shift));
  if Sender = Button_BuildingProgressInc then fHouse.IncBuildingProgress(5 * -GetMultiplicator(Shift));

  if (Sender = Button_BuildingProgressDec) or (Sender = Button_BuildingProgressInc) then
    fHouse.SetBuildingProgress(fHouse.BuildingProgress, ResRow_Ware_Build[0].OrderCount,
                                                        ResRow_Ware_Build[1].OrderCount,
                                                        ResRow_Ware_Build[2].OrderCount);



  ResRow_Ware_Build[0].OrderCount := fHouse.BuildSupplyWood;
  ResRow_Ware_Build[1].OrderCount := fHouse.BuildSupplyStone;
  ResRow_Ware_Build[2].OrderCount := fHouse.BuildSupplyTile;
  ResRow_Ware_Build[0].WareRow.WareCount := fHouse.BuildSupplyWood;
  ResRow_Ware_Build[1].WareRow.WareCount := fHouse.BuildSupplyStone;
  ResRow_Ware_Build[2].WareRow.WareCount := fHouse.BuildSupplyTile;
  HealthBar_BuildingProgress.Caption := IntToStr(fHouse.BuildingProgress) + ' / ' + IntToStr(fHouse.MaxHealth);
  HealthBar_BuildingProgress.Position := fHouse.BuildingProgress / fHouse.MaxHealth;

end;


procedure TKMMapEdHouse.ColorCodeChange(Sender: TObject);
var C : Cardinal;
begin
  if Sender <> Edit_HouseFlagColor then
    Exit;
  Edit_HouseFlagColor.SetTextSilently(UpperCase(Edit_HouseFlagColor.Text));
  if length(Edit_HouseFlagColor.Text) > 0 then
  begin
    C := StrToInt('$' + Edit_HouseFlagColor.Text);
    C := C or $FF000000;

    Shape_FlagColor.ShapeColor := C;
    if fHouse <> nil then
      fHouse.FlagColor := C;
    //Edit_HouseFlagColor.SetTextSilently( Format('%.6x', [C and $FFFFFF]) );
    //Shape_FlagColor.FillColor := C;
  end;
end;

procedure TKMMapEdHouse.SetWariant(Sender: TObject; Shift: TShiftState);
var maxW, C : Integer;
begin
  maxW := fHouse.HSpec.HasStoneWariants - 1;
  C := fHouse.PicWariant;
  If ssRight in Shift then
    IncLoop(C, -1, maxW, -1)
  else
    IncLoop(C, -1, maxW);
  fHouse.PicWariant := C;
  Button_HouseWariant.Caption := IntToStr(fHouse.PicWariant + 1);
end;

procedure TKMMapEdHouse.Pearl_Change(Sender : TObject);
var PL : TKMHousePearl;
  pt, stage, progress : Word;
begin
  PL := TKMHousePearl(fHouse);
  stage := PL.BuildStage;
  progress := PL.StageProgress;
  If Sender = Button_PearlType then
  begin
    pt := byte(PL.PearlType);

    IncLoop(pt, 0, byte(high(TKMPearlType)) );
    PL.SelectType(TKMPearlType(pt));
  end else
  If PL.PearlType <> ptNone then
  begin

    If Sender = Button_Stage_Inc then
    begin
      stage := PL.BuildStage + 1;
    end else
    If Sender = Button_Stage_Dec then
    begin
      stage := Max(PL.BuildStage - 1, 0);
    end else
    If Sender = Button_Progress_Inc then
    begin
      progress := progress + gRes.Houses.Pearls[PL.PearlType].ProgressPerStage;
    end else
    If Sender = Button_Progress_Dec then
    begin
      progress := Max(progress - gRes.Houses.Pearls[PL.PearlType].ProgressPerStage, 0);
    end;

  end;
  PL.SetStage(stage);
  PL.SetStageProgress(progress);

  Pearl_Refresh;
end;

procedure TKMMapEdHouse.Pearl_ChangeShift(Sender: TObject; Shift: TShiftState);
var PL : TKMHousePearl;
  stage, progress, I : Word;
begin
  PL := TKMHousePearl(fHouse);
  stage := PL.BuildStage;
  progress := PL.StageProgress;
  I := 1;
  If ssRight in Shift then I := I * 2;
  If ssShift in Shift  then I := I * 2;
  If ssCtrl in Shift  then I := I * 2;
  If ssAlt in Shift  then I := I * 2;

  If PL.PearlType <> ptNone then
  begin

    If Sender = Button_Stage_Inc then
    begin
      stage := PL.BuildStage + I;
    end else
    If Sender = Button_Stage_Dec then
    begin
      stage := Max(PL.BuildStage - I, 0);
    end else
    If Sender = Button_Progress_Inc then
    begin
      progress := progress + gRes.Houses.Pearls[PL.PearlType].ProgressPerStage * I;
    end else
    If Sender = Button_Progress_Dec then
    begin
      progress := Max(progress - gRes.Houses.Pearls[PL.PearlType].ProgressPerStage * I, 0);
    end;
  end;

  PL.SetStage(stage);
  PL.SetStageProgress(progress);

  Pearl_Refresh;
end;

procedure TKMMapEdHouse.Pearl_ChangeWares(Sender : TObject; X: Integer);
var PL : TKMHousePearl;
  I, S : Integer;
begin
  PL := TKMHousePearl(fHouse);
  S := PL.BuildStage;
  If S < gRes.Houses.Pearls[PL.PearlType].StageCount then
  begin
    I := PL.Delivered[S].C + X;
    PL.Delivered[S].C := EnsureRange(I, 0, PL.BuildCost[S].C) ;
  end;
  Pearl_Refresh;
end;

procedure TKMMapEdHouse.Pearl_Refresh;
var PL : TKMHousePearl;
  maxStage : Byte;
begin
  PL := TKMHousePearl(fHouse);

  Button_PearlType.TexID := PEARL_ICONS[PL.PearlType];

  case PL.PearlType of
    ptValtaria : Button_PearlType.BackBevelColor := $FF00CC30 and $55FFFFFF;
    ptArium : Button_PearlType.BackBevelColor := $FF9999FF and $55FFFFFF;
    ptAgros : Button_PearlType.BackBevelColor := $FF99FFB1 and $55FFFFFF;
    ptRalender : Button_PearlType.BackBevelColor:= $FFFF33F3 and $55FFFFFF;
    else Button_PearlType.BackBevelColor := $FFFFFFFF and $55FFFFFF;
  end;
  maxStage := gRes.Houses.Pearls[PL.PearlType].StageCount;
  Button_Stage_Inc.Enabled := PL.BuildStage < maxStage;
  Button_Stage_Dec.Enabled := PL.BuildStage > 0;
  Button_Progress_Inc.Enabled := (PL.BuildStage < maxStage) and (PL.StageProgress < PL.MaxProgress);
  Button_Progress_Dec.Enabled := (PL.BuildStage < maxStage) and (PL.StageProgress > 0);

  Bar_Stage.LinesCount := Max(maxStage - 1, 0);
  Bar_Stage.Position := 0;
  Bar_Stage_Progreess.Position := 0;
  Bar_Stage_Progreess.Caption := '';
  WaresRow_Stage_Delivered.Disable;
  If maxStage > 0 then
  begin
    Bar_Stage.Position := PL.BuildStage / maxStage;
    If PL.BuildStage < maxStage then
    begin
      WaresRow_Stage_Delivered.Enable;
      WaresRow_Stage_Delivered.WareRow.TexID := gRes.Wares[PL.BuildCost[PL.BuildStage].W].GuiIcon;
      WaresRow_Stage_Delivered.OrderCntMax := PL.BuildCost[PL.BuildStage].C;
      WaresRow_Stage_Delivered.OrderCount := PL.Delivered[PL.BuildStage].C;
      WaresRow_Stage_Delivered.WareRow.WareCntAsNumber := true;
      WaresRow_Stage_Delivered.WareRow.WareCount := PL.Delivered[PL.BuildStage].C;
      Bar_Stage_Progreess.Position := PL.Progress;
      Bar_Stage_Progreess.Caption := PL.StageProgress.ToString + ' / ' + PL.MaxProgress.ToString;
    end;
  end;
end;


procedure TKMMapEdHouse.Forest_Refresh;
var HF : TKMHouseForest;
  I : Integer;
begin
  HF := TKMHouseForest(fHouse);
  for I := 0 to High(Button_PlantTree) do
    Button_PlantTree[I].Caption := HF.TreesCount(I).ToString;
end;

procedure TKMMapEdHouse.Forest_Clicked(Sender : TObject; Shift: TShiftState);
var HF : TKMHouseForest;
begin
  HF := TKMHouseForest(fHouse);

  If ssRight in Shift then
    HF.RemoveMapEdTree(TKMControl(Sender).Tag, byte(ssShift in Shift) * 4 + 1)
  else
    HF.AddMapEdTree(TKMControl(Sender).Tag, byte(ssShift in Shift) * 4 + 1);
  Forest_Refresh;
end;

procedure TKMMapEdHouse.Pasture_Refresh;
var HP : TKMHousePasture;
  I : Integer;
  an : TKMPastureAnimalType;
begin
  HP := TKMHousePasture(fHouse);
  for I := 0 to High(Button_AnimalCount) do
  begin
    an := PASTURE_ANIMALS_ORDER[I];
    Button_AnimalCount[I].Caption := HP.AnimalsCount(an).ToString;
  end;


end;

procedure TKMMapEdHouse.Pasture_Clicked(Sender : TObject; Shift: TShiftState);
var HP : TKMHousePasture;
  an : TKMPastureAnimalType;
begin
  HP := TKMHousePasture(fHouse);

  an := PASTURE_ANIMALS_ORDER[TKMControl(Sender).Tag];
  If ssRight in Shift then
    HP.SellAnimal(an)
  else
    HP.BuyAnimalScript(an, 1);
  Pasture_Refresh;
end;

end.
