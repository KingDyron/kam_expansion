unit KM_GUIGameGuide;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_Points,
  KM_Controls, KM_ControlsBase, KM_CommonClasses, KM_CommonTypes,
  KM_ControlsWaresRow, KM_ControlsMemo, KM_ControlsScroll,
  KM_Pics, KM_ResTypes, KM_TerrainTypes,
  KM_InterfaceGame, KM_ScriptingTypes,
  Classes;


type
  TKMGuidePageType = (gtUnits, gtHouses, gtGrain, gtFruitTree, gtAnimals);

  TKMGrainViewer = class(TKMControl)
    private
      fGrainType : TKMGrainType;
      fStage : Word;
    public
      AnimStep : Cardinal;
      Caption : String;
      constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
      procedure SetGrainType(aType : TKMGrainType);
      procedure UpdateState(aTickCount: Cardinal);override;
      procedure Paint; override;
  end;
  TKMFruitViewer = class(TKMControl)
    private
      fFruitType : Integer;
      fStage : Word;
    public
      AnimStep : Cardinal;
      Caption : String;
      constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
      procedure SetFruitType(aType : Integer);
      procedure UpdateState(aTickCount: Cardinal);override;
      procedure Paint; override;
  end;

  TKMClimateViewer = class(TKMControl)
    private
      fCount : Byte;
      fClimateOrder : TKMTerrainClimatArray;
    public
      ClimateColor : array[TKMTerrainClimat] of TKMColor3f;
      Caption : array[TKMTerrainClimat] of String;

      constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
      procedure SetOrder(aOrder : TKMTerrainClimatArray);
      procedure Paint; override;
  end;

  TKMAnimalViewer = class(TKMControl)
    private
      fAnimalType : TKMUnitType;
      fDir : TKMDirection;
    public
      AnimStep : Cardinal;
      Caption : String;
      constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
      procedure SetAnimalType(aType : TKMUnitType);
      procedure UpdateState(aTickCount: Cardinal);override;
      procedure Paint; override;
  end;

  TKMGUIGameGuide = class
  private
    fOnShow : TNotifyEvent;
    fType : TKMGuidePageType;
    fSelected : Byte;
    fSelectedFruit : Integer;
    fSelectedGrain : TKMGrainType;
    fSelectedAnimal: TKMUnitType;
    procedure Guide_SelectType(Sender : TObject);
    procedure ClosePanel(Sender : TObject);
    procedure RefreshPanel;
    procedure Guide_SelectIcon(Sender : TObject);
    procedure ReleasedByClick(Sender : TOBject);
    procedure WaresRowClick(Sender : TOBject);

    procedure GrainClick(Sender : TOBject);
    procedure FruitClick(Sender : TOBject);
    procedure AnimalClick(Sender : TOBject);

    procedure RefreshUnit;
    procedure RefreshHouse;
    procedure RefreshGrain;
    procedure RefreshFruit;
    procedure RefreshAnimal;
    procedure CreateGrains;
    procedure CreateFruits;
    procedure CreateAnimals;
  protected
    Pin_Open : TKMButtonFlatPin;
    Panel_Guide : TKMPanel;
      Image_Background: TKMImage;
      Image_ClosePanel: TKMImage;

      Button_Page: array[TKMGuidePageType] of TKMButton;
      Panel_Scroll : TKMScrollPanel;

        Button_Select: array[0..high(GUIHouseOrderFull) - 1] of TKMButtonFlat;
        Image_Selected: TKMButtonFlat;
        Panel_House : TKMPanel;
          Icons_Workers: TKMUnitsButtonsMulti;
          WaresOut,
          WaresIn : array[1..6] of TKMWaresRow;

          WareRow_WoodCost,
          WareRow_StoneCost,
          WareRow_TileCost : TKMWaresRow;
          Button_VWares,
          Button_ReleasedBy : array[0..5] of TKMButtonFlat;
          Label_Description : TKMMemo;

        Panel_Unit : TKMPanel;
          Label_Attack,
          Label_AttackHorse,
          Label_Speed,
          Label_Sight,
          Label_AttackHouse,
          Label_Defense,
          Label_HP : TKMLabel;
          Label_UnitDescription : TKMMemo;
          VWareCost: TKMControlArray;
          WareCost : array[1..4] of TKMWaresRow;

        Panel_Grain : TKMPanel;
          Button_Grains : array[GRAIN_MIN..GRAIN_MAX] of TKMButtonFlat;
          GrainViewer : TKMGrainViewer;
          Grain_Wares : array[0..4] of TKMWaresRow;
          Grain_GrowingTime, Grain_RegrowingTime : TKMWaresRow;

        Panel_Fruit : TKMPanel;
          Button_Fruits : array of TKMButtonFlat;
          Fruit_Viewer : TKMFruitViewer;
          Fruit_Crops : TKMWaresRow;
          Fruit_GrowingTime, Fruit_RegrowingTime : TKMWaresRow;
          //Fruit_GrowingTime,
          Terrain_Best,
          Terrain_Worst : TKMLabel;
          Fruit_ClimateViewer : TKMClimateViewer;

        Panel_Animals : TKMPanel;
          Button_Animals : array of TKMButtonFlat;
          Animal_Viewer : TKMAnimalViewer;
          Animal_Crops : array[0..3] of TKMWaresRow;
          Animal_CropsLabel : TKMLabel;

  public
    constructor Create(aParent: TKMPanel; aOnShow : TNotifyEvent);

    procedure Show; overload;
    procedure Show(aUnitType : TKMUnitType); overload;
    procedure Show(aHouseType : TKMHouseType); overload;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses  KM_RenderUI, KM_Resource, KM_HandsCollection,
      KM_UtilsExt,
      KM_HouseSiegeWorkshop,
      KM_ResUnits, KM_ResHouses, KM_ResTexts, KM_ResFonts, KM_ResMapElements,
      KM_ResTileSet, KM_ResTileSetTypes,
      KM_TerrainPainter,
      KM_CommonUtils,
      SysUtils, Math;

constructor TKMGUIGameGuide.Create(aParent: TKMPanel; aOnShow : TNotifyEvent);
var I, top : Integer;
  PT : TKMGuidePageType;
begin
  inherited Create;
  fSelectedGrain := gftNone;
  fSelectedFruit := -1;
  fSelected := 255;
  fOnShow := aOnShow;
  Pin_Open := TKMButtonFlatPin.Create(aParent, 198, 20, 25, 32, 314);
  Pin_Open.OnClick := ClosePanel;
  Pin_Open.Hint := gResTexts[1947];
  Panel_Guide := TKMPanel.Create(aParent, 240, 0, 500, aParent.Height - 200);
  Panel_Guide.Anchors := [anLeft, anTop, anBottom];

  Image_Background := TKMImage.Create(Panel_Guide, 0, 0, Panel_Guide.Width, Panel_Guide.Height, 18, rxGuiMain);
  Image_Background.ImageStretch;
  Image_Background.AnchorsStretch;
  TKMBevel.Create(Panel_Guide, 20, 40, Panel_Guide.Width - 40, 40);

  Image_ClosePanel := TKMImage.Create(Panel_Guide, Panel_Guide.Width - 60, 10, 30, 30, 52);

  Image_ClosePanel.OnClick := ClosePanel;

  for PT := low(TKMGuidePageType) to high(TKMGuidePageType) do
  begin
    Button_Page[PT] := TKMButton.Create(Panel_Guide, 25 + 35 * byte(PT), 45, 30, 30, 665, rxGui, bsGame);
    case PT of
      gtUnits: Button_Page[PT].TexID := 665;
      gtHouses: Button_Page[PT].TexID := 391;
      gtGrain: Button_Page[PT].TexID := gRes.Wares[wtVegetables].GUIIcon;
      gtFruitTree: Button_Page[PT].TexID := gRes.Wares[wtApple].GUIIcon;
      gtAnimals: Button_Page[PT].TexID := 915;
    end;
    Button_Page[PT].OnClick := Guide_SelectType;
    Button_Page[PT].Tag := byte(PT);
  end;

  Panel_Scroll := TKMScrollPanel.Create(Panel_Guide, 0, 85, Panel_Guide.Width - 30, Panel_Guide.Height - 85 - 40, [saVertical], bsGame, ssGame);
  Panel_Scroll.Anchors := [anLeft, anTop, anBottom];

  for I := 0 to High(Button_Select) do
  begin
    Button_Select[I] := TKMButtonFlat.Create(Panel_Scroll, 0, 0, 30, 30, 0);
    Button_Select[I].OnClick := Guide_SelectIcon;
    Button_Select[I].Tag := I;
  end;

  Image_Selected := TKMButtonFlat.Create(Panel_Scroll, 250, 10, 200, 200, 0);
  Image_Selected.Hitable := false;

  Icons_Workers := TKMUnitsButtonsMulti.Create(Panel_Scroll, 260, 30, 200, 200);
  ////////////////////////Houses
  Panel_House := TKMPanel.Create(Panel_Scroll, -5, 340, 490, 400);
  Panel_House.Hitable := false;

    TKMBevel.Create(Panel_House, 20, 0, 450, Panel_House.Height - 10);

    TKMLabel.Create(Panel_House, 20, 5, 70, 20, gResTexts[1907], fntMetal, taCenter);

    WareRow_WoodCost := TKMWaresRow.Create(Panel_House, 30, 25, 50);
    WareRow_StoneCost := TKMWaresRow.Create(Panel_House, 30, 50, 50);
    WareRow_TileCost := TKMWaresRow.Create(Panel_House, 30, 75, 50);
    WareRow_WoodCost.TexID := gRes.Wares[wtTimber].GUIIcon;
    WareRow_StoneCost.TexID := gRes.Wares[wtStone].GUIIcon;
    WareRow_TileCost.TexID := gRes.Wares[wtTile].GUIIcon;

    WareRow_WoodCost.TxtOffset := -25;
    WareRow_WoodCost.TextOffset := 15;

    WareRow_StoneCost.TxtOffset := -25;
    WareRow_StoneCost.TextOffset := 15;

    WareRow_TileCost.TxtOffset := -25;
    WareRow_TileCost.TextOffset := 15;

    WareRow_WoodCost.WareCntAsNumber := true;
    WareRow_StoneCost.WareCntAsNumber := true;
    WareRow_TileCost.WareCntAsNumber := true;
    WareRow_WoodCost.Clickable := true;
    WareRow_StoneCost.Clickable := true;
    WareRow_TileCost.Clickable := true;
    WareRow_WoodCost.Hitable := true;
    WareRow_StoneCost.Hitable := true;
    WareRow_TileCost.Hitable := true;
    WareRow_WoodCost.HideHighlight := false;
    WareRow_StoneCost.HideHighlight := false;
    WareRow_TileCost.HideHighlight := false;
    WareRow_WoodCost.Tag :=  ord(wtTimber);
    WareRow_StoneCost.Tag :=  ord(wtStone);
    WareRow_TileCost.Tag :=  ord(wtTile);
    WareRow_WoodCost.OnClick := WaresRowClick;
    WareRow_StoneCost.OnClick := WaresRowClick;
    WareRow_TileCost.OnClick := WaresRowClick;
    WareRow_WoodCost.Hint := gResTexts[1917];
    WareRow_StoneCost.Hint := gResTexts[1917];
    WareRow_TileCost.Hint := gResTexts[1917];


    TKMLabel.Create(Panel_House, Panel_House.Width - 370, 5, 160, 20, gResTexts[1908], fntMetal, taCenter);
    TKMLabel.Create(Panel_House, Panel_House.Width - 190, 5, 160, 20, gResTexts[1909], fntMetal, taCenter);
    for I := 1 to 6 do
    begin
      WaresIn[I] := TKMWaresRow.Create(Panel_House, Panel_House.Width - 370, 5 + I * 25, 160);
      WaresIn[I].Hitable := true;
      WaresIn[I].HideHighlight := false;
      WaresIn[I].OnClick := WaresRowClick;
      WaresIn[I].Clickable := true;
      WaresIn[I].Hint := gResTexts[1917];
      WaresIn[I].Tag2 := 0;

      WaresOut[I] := TKMWaresRow.Create(Panel_House, Panel_House.Width - 190, 5 + I * 25, 160);
      WaresOut[I].Hitable := true;
      WaresOut[I].HideHighlight := false;
      WaresOut[I].OnClick := WaresRowClick;
      WaresOut[I].Clickable := true;
      WaresOut[I].Hint := gResTexts[1917];
      WaresOut[I].Tag2 := 0;
    end;

    TKMLabel.Create(Panel_House, 30, 190, 300, 20, gResTexts[1910], fntMetal, taLeft);
    for I := 0 to High(Button_ReleasedBy) do
    begin
      Button_ReleasedBy[I] := TKMButtonFlat.Create(Panel_House, 40 + I * 34, 205, 30, 30, 0);
      Button_ReleasedBy[I].OnClick := ReleasedByClick;
    end;

    TKMLabel.Create(Panel_House, 30, 255, 300, 20, gResTexts[1911], fntMetal, taLeft);
    for I := 0 to High(Button_VWares) do
    begin
      Button_VWares[I] := TKMButtonFlat.Create(Panel_House, 40 + I * 34, 270, 27, 27, 0);
      Button_VWares[I].Tag2 := 1;
      Button_VWares[I].OnClick := WaresRowClick;
    end;

    Label_Description := TKMMemo.Create(Panel_House, 25, 300, Panel_House.Width - 50, Panel_House.Height - 320, fntGrey, bsGame, false);
    Label_Description.WordWrap := true;

  Panel_House.Hide;


  ////////////////////////Units
  Panel_Unit := TKMPanel.Create(Panel_Scroll, -5, 300, 490, 400);
  Panel_Unit.Hitable := false;

    TKMBevel.Create(Panel_Unit, 20, -5, 450,  Panel_Unit.Height - 20);
    TKMBevel.Create(Panel_Unit, 30, 0, 190,  185);

    Label_Attack := TKMLabel.Create(Panel_Unit, 40, 5, 200, 20, gResTexts[1600], fntMetal, taLeft);
    Label_Defense := TKMLabel.Create(Panel_Unit, 40, 30, 200, 20, gResTexts[1601], fntMetal, taLeft);
    Label_HP := TKMLabel.Create(Panel_Unit, 40, 55, 200, 20, gResTexts[1603], fntMetal, taLeft);
    Label_AttackHorse := TKMLabel.Create(Panel_Unit, 40, 80, 200, 20, gResTexts[1913], fntMetal, taLeft);
    Label_Speed := TKMLabel.Create(Panel_Unit, 40, 105, 200, 20, gResTexts[1914], fntMetal, taLeft);
    Label_Sight := TKMLabel.Create(Panel_Unit, 40, 130, 200, 20, gResTexts[1915], fntMetal, taLeft);
    Label_AttackHouse := TKMLabel.Create(Panel_Unit, 40, 155, 200, 20, gResTexts[1916], fntMetal, taLeft);

    TKMBevel.Create(Panel_Unit, 460 - 195, 0, 190,  185);

    TKMLabel.Create(Panel_Unit, 275, 5, 200, 20, gResTexts[1912], fntMetal, taLeft);

    for I := 1 to High(WareCost) do
    begin
      WareCost[I] := TKMWaresRow.Create(Panel_Unit, 275, I * 22, 170);
      WareCost[I].Hitable := true;
      WareCost[I].Clickable := True;
      WareCost[I].OnClick := WaresRowClick;
      WareCost[I].HideHighlight := false;
      WareCost[I].Hint := gResTexts[1917];
      WareCost[I].WareCntAsNumber := true;
      WareCost[I].TextOffset := 0;
      WareCost[I].Tag2 := 0;
    end;

    top := WareCost[high(WareCost)].Bottom + 5;
    SetLength(VWareCost, 12);
    for I := 0 to High(VWareCost) do
    begin
      VWareCost[I] := TKMButtonFlat.Create(Panel_Unit, 272 + (I mod 6) * 30, top + (I div 6) * 32, 27, 30, 0, rxGui);
      VWareCost[I].Tag2 := 1;
      VWareCost[I].OnClick := WaresRowClick;
    end;


    Label_UnitDescription := TKMMemo.Create(Panel_Unit, 30, 195, 430, 170, fntGrey, bsGame, false);
    Label_UnitDescription.WordWrap := true;

  Panel_Unit.Hide;

  CreateGrains;
  CreateFruits;
  CreateAnimals;
  Hide;
end;

procedure TKMGUIGameGuide.CreateGrains;
var I, J : Integer;
  GT : TKMGrainType;
begin
  Panel_Grain := TKMPanel.Create(Panel_Scroll, 10, 10, 470, 600);
  Panel_Grain.Hide;
  I := 0;
  J := 0;
  for GT := Low(Button_Grains) to High(Button_Grains) do
  begin
    //make new row for every type of grain
    If GT in [gftGrass, gftPumpkin, gftWinePurple, gftWildRose] then
    begin
      I := 0;
      inc(J);
    end;

    Button_Grains[GT] := TKMButtonFlat.Create(Panel_Grain, 20 + I * 34, J * 34, 30, 30, GRAIN_GUI_PIC[GT], rxGui);
    Button_Grains[GT].OnClick := GrainClick;
    case J of
      0: Button_Grains[GT].BackBevelColor := $2203D3FC;
      1: Button_Grains[GT].BackBevelColor := $2200FF00;
      2: Button_Grains[GT].BackBevelColor := $220000FF;
      else Button_Grains[GT].BackBevelColor := $22FF0000;
    end;
    Button_Grains[GT].Tag := byte(GT);
    Inc(I);
  end;
  GrainViewer := TKMGrainViewer.Create(Panel_Grain, 300, 10, 150, 150);

  TKMBevel.Create(Panel_Grain, 5, 180, 460,  400);

  TKMLabel.Create(Panel_Grain, 20, 184, 160, 20, gResTexts[2187], fntMetal, taLeft);
  for I := Low(Grain_Wares) to High(Grain_Wares) do
  begin
    Grain_Wares[I] := TKMWaresRow.Create(Panel_Grain, 20 + I * 85, 204, 80, false);
    Grain_Wares[I].TxtOffset := -55;
    Grain_Wares[I].TextOffset := 25;
    Grain_Wares[I].WareCount := 1;
    case I of
      0: Grain_Wares[I].TexID := gRes.Wares[wtCorn].GuiIcon;
      1: Grain_Wares[I].TexID := gRes.Wares[wtSeed].GuiIcon;
      2: Grain_Wares[I].TexID := gRes.Wares[wtHay].GuiIcon;
      3: Grain_Wares[I].TexID := gRes.Wares[wtVegetables].GuiIcon;
      4: Grain_Wares[I].TexID := gRes.Wares[wtWine].GuiIcon;
    end;
  end;

  Grain_GrowingTime := TKMWaresRow.Create(Panel_Grain, 20, 230, 140, false);
  Grain_GrowingTime.TxtOffset := -115;
  Grain_GrowingTime.TextOffset := 45;
  Grain_GrowingTime.WareCount := 1;
  Grain_GrowingTime.TexID := 1028;
  Grain_GrowingTime.Hint := gResTexts[2185];

  Grain_ReGrowingTime := TKMWaresRow.Create(Panel_Grain, Grain_GrowingTime.Right + 20, 230, 140, false);
  Grain_ReGrowingTime.TxtOffset := -115;
  Grain_ReGrowingTime.TextOffset := 45;
  Grain_ReGrowingTime.WareCount := 1;
  Grain_ReGrowingTime.TexID := 1029;
  Grain_ReGrowingTime.Hint := gResTexts[2186];
end;

procedure TKMGUIGameGuide.CreateFruits;
var I: Integer;
begin
  Panel_Fruit := TKMPanel.Create(Panel_Scroll, 10, 10, 470, 600);
  Panel_Fruit.Hide;
  Setlength(Button_Fruits, length(gFruitTrees));

  for I := 0 to High(Button_Fruits) do
  begin
    Button_Fruits[I] := TKMButtonFlat.Create(Panel_Fruit, 20 + (I mod 8) * 34, (I div 8) * 34, 30, 30, gFruitTrees[I].GuiIcon, rxGui);
    Button_Fruits[I].Hint := gResTexts[gFruitTrees[I].HintID];
    Button_Fruits[I].Tag := I;
    Button_Fruits[I].OnClick := FruitClick;
  end;

  Fruit_Viewer := TKMFruitViewer.Create(Panel_Fruit, 300, 10, 150, 150);
  TKMBevel.Create(Panel_Fruit, 5, 180, 460,  400);


  TKMLabel.Create(Panel_Fruit, 20, 184, 160, 20, gResTexts[2187], fntMetal, taLeft);
  Fruit_Crops := TKMWaresRow.Create(Panel_Fruit, 20, 204, 80, false);
  Fruit_Crops.TxtOffset := -55;
  Fruit_Crops.TextOffset := 25;
  Fruit_Crops.WareCount := 1;
  Fruit_Crops.TexID := gRes.Wares[wtApple].GUIIcon;
  //Fruit_GrowingTime := TKMLabel.Create(Panel_Fruit, 20, 230, 400, 20, '', fntMetal, taLeft);

  Terrain_Best := TKMLabel.Create(Panel_Fruit, 20, 310, 430, 20, '', fntMetal, taLeft);
  Terrain_Best.FontColor := $FF22D000;
  Terrain_Worst := TKMLabel.Create(Panel_Fruit, 20, 310, 430, 20, '', fntMetal, taRight);
  Terrain_Worst.FontColor := $FF2200D0;

  Fruit_ClimateViewer := TKMClimateViewer.Create(Panel_Fruit, 20, 370, 430, 100);


  Fruit_GrowingTime := TKMWaresRow.Create(Panel_Fruit, 20, 230, 140, false);
  Fruit_GrowingTime.TxtOffset := -115;
  Fruit_GrowingTime.TextOffset := 45;
  Fruit_GrowingTime.WareCount := 1;
  Fruit_GrowingTime.TexID := 1028;
  Fruit_GrowingTime.Hint := gResTexts[2185];

  Fruit_ReGrowingTime := TKMWaresRow.Create(Panel_Fruit, Fruit_GrowingTime.Right + 20, 230, 140, false);
  Fruit_ReGrowingTime.TxtOffset := -115;
  Fruit_ReGrowingTime.TextOffset := 45;
  Fruit_ReGrowingTime.WareCount := 1;
  Fruit_ReGrowingTime.TexID := 1029;
  Fruit_ReGrowingTime.Hint := gResTexts[2186];

end;

procedure TKMGuiGameGuide.CreateAnimals;
var I : Integer;
  UT : TKMUnitType;
begin
  Panel_Animals := TKMPanel.Create(Panel_Scroll, 10, 10, 470, 600);
  Panel_Animals.Hide;

  SetLength(Button_Animals, length(Animal_Order));

  for I := 0 to High(Button_Animals) do
  begin
    UT := Animal_Order[I];
    Button_Animals[I] := TKMButtonFlat.Create(Panel_Animals, 20 + (I mod 6) * 40, (I div 6) * 40, 37, 37, gRes.Units[UT].GUIIcon, rxGui);
    Button_Animals[I].Tag := byte(UT);
    Button_Animals[I].OnClick := AnimalClick;
  end;

  Animal_Viewer := TKMAnimalViewer.Create(Panel_Animals, 300, 10, 150, 150);
  TKMBevel.Create(Panel_Animals, 5, 180, 460,  400);

  Animal_CropsLabel := TKMLabel.Create(Panel_Animals, 20, 184, 400, 20, gResTexts[2187], fntMetal, taLeft);

  for I := Low(Animal_Crops) to High(Animal_Crops) do
  begin
    Animal_Crops[I] := TKMWaresRow.Create(Panel_Animals, 20 + I * 85, 204, 80, false);
    case I of
      0: Animal_Crops[I].TexID := gRes.Wares[wtPig].GUIIcon;
      1: Animal_Crops[I].TexID := gRes.Wares[wtLeather].GUIIcon;
      2: Animal_Crops[I].TexID := gRes.Wares[wtFeathers].GUIIcon;
      3: Animal_Crops[I].TexID := gRes.Wares[wtFish].GUIIcon;
    end;
    Animal_Crops[I].TxtOffset := -55;
    Animal_Crops[I].TextOffset := 25;
    Animal_Crops[I].WareCount := 1;
  end;

end;

procedure TKMGUIGameGuide.Show;
var
  PT : TKMGuidePageType;
begin
  Panel_Guide.Show;
  for PT := low(TKMGuidePageType) to high(TKMGuidePageType) do
    Button_Page[PT].FlagColor := gMySpectator.Hand.FlagColor;

  if Assigned(fOnShow) then
    fOnShow(Self);

  RefreshPanel;
end;

procedure TKMGuiGameGuide.Show(aUnitType : TKMUnitType);
var I : Integer;
begin
  for I := 0 to High(MapEd_Order) do
    If aUnitType = MapEd_Order[I] then
    begin
      fselected := I;
      fType := gtUnits;
      Show;
      Exit;
    end;

end;

procedure TKMGuiGameGuide.Show(aHouseType : TKMHouseType);
var I : Integer;
begin
  for I := 0 to High(GUIHouseOrderFull) - 1 do
    If aHouseType = GUIHouseOrderFull[I + 1] then
    begin
      fselected := I;
      fType := gtHouses;
      Show;
      Exit;
    end;

end;


procedure TKMGUIGameGuide.Hide;
begin
  Panel_Guide.Hide;
end;


function TKMGUIGameGuide.Visible: Boolean;
begin
  Result := Panel_Guide.Visible;
end;

procedure TKMGUIGameGuide.Guide_SelectType(Sender: TObject);
var PT : TKMGuidePageType;
begin
  PT := TKMGuidePageType(TKMButton(Sender).Tag);
  If PT = fType then
    Exit;
  fType := PT;
  fSelected := 255;
  fSelectedGrain := gftNone;
  RefreshPanel;

end;

procedure TKMGUIGameGuide.ClosePanel(Sender: TObject);
begin
  if Sender = Pin_Open then
  begin
    if Visible then
      Hide
    else
      Show;

  end else
    Hide;
end;

procedure TKMGUIGameGuide.RefreshUnit;
  procedure ShowWareCost(aIndex, aCount: Integer; aWareType : TKMWareType);
  begin
    WareCost[aIndex].Tag := ord(aWareType);
    WareCost[aIndex].TexID := gRes.Wares[aWareType].GUIIcon;
    WareCost[aIndex].WareCount := aCount;
    WareCost[aIndex].MaxWares := aCount;
    WareCost[aIndex].Caption := gRes.Wares[aWareType].Title;

    WareCost[aIndex].Show;
  end;
var I, J, fullCount : Integer;
  UT : TKMUnitType;

  warePlan : TKMWarePlan;
begin
  if fSelected = 255 then
    Exit;

  UT := MapEd_Order[fSelected];
  Panel_Unit.Show;
  Image_Selected.Show;
  Image_Selected.RX := rxGui;
  Image_Selected.TexID := gRes.Units[UT].GUIScroll;
  Image_Selected.FlagColor := gMySpectator.Hand.FlagColor;
  Image_Selected.Height := 250;
  Image_Selected.CapOffsetY := 100;
  Image_Selected.Caption := gRes.Units[UT].GUIName;
  Icons_Workers.Hide;
  with gRes.Units[UT] do
  begin
    Label_Attack.Caption := gResTexts[1600] + IntToStr(Attack);
    Label_Defense.Caption := gResTexts[1601] + IntToStr(Defence);
    Label_HP.Caption := gResTexts[1603] + IntToStr(HitPoints);
    Label_AttackHorse.Caption := gResTexts[1913] + IntToStr(AttackHorse);
    Label_Speed.Caption := gResTexts[1914] + IntToStr(AbsSpeed);
    Label_Sight.Caption := gResTexts[1915] + IntToStr(Sight);
    Label_AttackHouse.Caption := gResTexts[1916] + IntToStr(HouseDamage);
    Label_UnitDescription.Text := Description;
  end;
  for I := 1 to high(WareCost) do
    WareCost[I].Hide;

  for I := 0 to High(VWareCost) do
      VWareCost[I].Hide;

  case gRes.Units[UT].GetTrainingHouse of
    htPalace: begin
                warePlan := gRes.Units[UT].PalaceCost.Plan;
                for I := 1 to 4 do
                  if warePlan[I - 1].W <> wtNone then
                  begin
                    fullCount := warePlan[I - 1].C + (gRes.Units[UT].PalaceCost.PhaseCount - 1);

                    ShowWareCost(I, fullCount, warePlan[I - 1].W);
                  end;
                for I := 0 to High(VWareCost) do
                  if I < length(gRes.Units[UT].PalaceCost.Wares) then
                    with gRes.Units[UT].PalaceCost do
                    begin
                      J := Wares[I].Index;
                      //VWareCost[I].Caption := IntToStr(Wares[I].C);
                      TKMButtonFlat(VWareCost[I]).Caption := IntToStr(Wares[I].C);
                      VWareCost[I].Show;
                      VWareCost[I].Hint := gResTexts[gRes.Wares.VirtualWares.WareS[Wares[I].W].TextID];
                      TKMButtonFlat(VWareCost[I]).TexID := gRes.Wares.VirtualWares.WareS[Wares[I].W].GUIIcon;
                      //VWareCost[I].TexID := gRes.Wares.VirtualWares.Ware[J].GUIIcon;
                      VWareCost[I].Tag := J;
                    end;
                SortVisibleControls(265, WareCost[high(WareCost)].Bottom + 3, 190, 3, VWareCost, false, true);
              end;
    htBarracks: for I := 0 to high(gRes.Units[UT].BarracksCost) do
                  if gRes.Units[UT].BarracksCost[I].W <> wtNone then
                    ShowWareCost(I + 1,
                                gRes.Units[UT].BarracksCost[I].C,
                                gRes.Units[UT].BarracksCost[I].W);

    htTownhall: ShowWareCost(1, gRes.Units[UT].TownhallCost, wtGold);


    htSiegeWorkshop:begin
                      warePlan := TKMHouseSiegeWorkshop.GetTotalCost(UT);

                      for I := 1 to 4 do
                        if warePlan[I - 1].W <> wtNone then
                          ShowWareCost(I, warePlan[I - 1].C, warePlan[I - 1].W);

                    end;
  end;


end;

procedure TKMGUIGameGuide.RefreshHouse;
var I, J : Integer;
  HT, HT2 : TKMHouseType;
  WT : TKMWareType;
begin
  if fSelected = 255 then
    Exit;
  Panel_House.Show;
  HT := GUIHouseOrderFull[fSelected + 1];
  Image_Selected.Show;
  Image_Selected.RX := rxHouses;
  Image_Selected.TexID := gRes.Houses[HT].GetRandomStonePic + 1;
  Image_Selected.FlagColor := high(Cardinal);
  Image_Selected.Height := 325;
  Image_Selected.CapOffsetY := 138;
  Image_Selected.Caption := gRes.Houses[HT].HouseName;

  Icons_Workers.Show;

  Icons_Workers.UnitPlan := gRes.Houses[HT].Workers;
  If gRes.Houses[HT].WorkerType <> utNone then
    Icons_Workers.Caption := 'Max : ' + gRes.Houses[HT].MaxWorkersCount.ToString
  else
    Icons_Workers.Caption := '';
  WareRow_WoodCost.WareCount := gRes.Houses[HT].WoodCost;
  WareRow_StoneCost.WareCount := gRes.Houses[HT].StoneCost;
  WareRow_TileCost.WareCount := gRes.Houses[HT].TileCost;

  for I := 1 to 6 do
  begin
    WT := gRes.Houses[HT].WareInput[I];
      WaresIn[I].Visible := WT <> wtNone;
    if WaresIn[I].Visible then
    begin
      WaresIn[I].TexID := gRes.Wares[WT].GuiIcon;
      WaresIn[I].WareCount := 1;
      WaresIn[I].Caption := gRes.Wares[WT].Title;
      WaresIn[I].Tag := Ord(WT);

      if not gRes.Wares[WT].IsValid then
        WaresIn[I].Clickable := false
      else
        WaresIn[I].Clickable := true;
    end;

    WT := gRes.Houses[HT].WareOutput[I];
    WaresOut[I].Visible := WT <> wtNone;

    if WaresOut[I].Visible then
    begin
      WaresOut[I].TexID := gRes.Wares[WT].GuiIcon;
      WaresOut[I].WareCount := gRes.Houses[HT].GetWareProdCt(WT);
      WaresOut[I].Caption := gRes.Wares[WT].Title;
      WaresOut[I].Tag := Ord(WT);

      if not gRes.Wares[WT].IsValid then
        WaresOut[I].Clickable := false
      else
        WaresOut[I].Clickable := true;

    end;
  end;

  I := 0;

  for HT2 in gRes.Houses[HT].ReleasedBy do
  begin
    if HT2 = htNone then
      Continue;

    Button_ReleasedBy[I].TexID := gRes.Houses[HT2].GUIIcon;
    Button_ReleasedBy[I].Tag := ord(HT2);
    Button_ReleasedBy[I].Show;
    Button_ReleasedBy[I].Hint := gRes.Houses[HT2].HouseName;
    Inc(I);
  end;

  for J := I to High(Button_ReleasedBy) do
    Button_ReleasedBy[J].Hide;

  Label_Description.Text := gRes.Houses[HT].HouseDescription;
  J := 0;
  for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
    if HT in gRes.Wares.VirtualWares.Ware[I].ProduceInHouses then
      if J <= high(Button_VWares) then
      begin
        Button_VWares[J].TexID := gRes.Wares.VirtualWares.Ware[I].GUIIcon;
        Button_VWares[J].Tag := I;
        Button_VWares[J].Show;
        Button_VWares[J].Hint := gResTexts[gRes.Wares.VirtualWares.Ware[I].TextID];
        inc(J);
      end;
  for I := J to high(Button_VWares) do
    Button_VWares[I].Hide;

end;

procedure TKMGUIGameGuide.RefreshGrain;
var GT : TKMGrainDat;
  time : Cardinal;
  h,m,s : Byte;
  cap : String;
  lastStage, regStage : Byte;
  I : Integer;
begin
  Panel_Grain.Show;

  If fSelectedGrain = gftNone then
    Exit;
  GT := gFieldGrains[fSelectedGrain];
  GrainViewer.Caption := gResTexts[GRAIN_GUI_HINT[fSelectedGrain]];

  Grain_Wares[0].Caption := GT.Straw.ToString(ffNumber, 3, 3);
  Grain_Wares[1].Caption := GT.Seeds.ToString(ffNumber, 3, 3);
  Grain_Wares[2].Caption := GT.Hay.ToString(ffNumber, 3, 3);
  Grain_Wares[3].Caption := GT.Vege.ToString(ffNumber, 3, 3);
  Grain_Wares[4].Caption := GT.Wine.ToString(ffNumber, 3, 3);
  for I := Low(Grain_Wares) to High(Grain_Wares) do
    If Grain_Wares[I].Caption <> '0,000' then
      Grain_Wares[I].CapColor := icWhite
    else
      Grain_Wares[I].CapColor := icGray;

  If fSelectedGrain in GRAIN_WINE then
    lastStage := GT.StagesCount - 1
  else
    lastStage := GT.StagesCount - 2;
  time := (GT.Stage[lastStage].Age * TERRAIN_PACE);


  h := time div (60 * 60 * 10) mod 24;
  m := time div (60 * 10) mod 60;
  s := time div 10 mod 60;
  cap := '';//gResTexts[2185] + '|    ';
  If h > 0 then
    cap := cap + h.ToString + 'h ';
  If m > 0 then
    cap := cap + m.ToString + 'm ';
  If s > 0 then
    cap := cap + s.ToString + 's ';
  Grain_GrowingTime.Caption := cap;
  Grain_ReGrowingTime.Hide;
  If fSelectedGrain in GRAIN_GRASS then
  begin
    regStage := GT.Stage[lastStage].NextStage;
    time := (GT.Stage[lastStage].Age - GT.Stage[regStage].Age) * TERRAIN_PACE;
    h := time div (60 * 60 * 10) mod 24;
    m := time div (60 * 10) mod 60;
    s := time div 10 mod 60;
    cap := '';
    If h > 0 then
      cap := cap + h.ToString + 'h ';
    If m > 0 then
      cap := cap + m.ToString + 'm ';
    If s > 0 then
      cap := cap + s.ToString + 's ';
    Grain_ReGrowingTime.Caption := cap;
    Grain_ReGrowingTime.Show;
  end;


end;

procedure TKMGUIGameGuide.RefreshFruit;
var F : TKMFruitTree;
  cap : String;
  time, I, J : Integer;
  h,m,s : Byte;
  climOrder : TKMTerrainClimatArray;
  clim : TKMTerrainClimat;
  best, worst : Single;
  procedure CompareClimates(var A : TKMTerrainClimat; var B: TKMTerrainClimat);
  var tmp : TKMTerrainClimat;
  begin
    If F.ClimateMulti[B] > F.ClimateMulti[A] then
    begin
      tmp := A;
      A := B;
      B := tmp;
    end;
  end;

begin
  Panel_Fruit.Show;
  If fSelectedFruit = -1 then
    Exit;
  F := gFruitTrees[fSelectedFruit];
  Fruit_Viewer.Caption := gResTexts[F.HintID];

  time := F.StagesCount * F.ProgressPerStage;
  h := time div (60 * 60 * 10) mod 24;
  m := time div (60 * 10) mod 60;
  s := time div 10 mod 60;
  cap := '';
  If h > 0 then
    cap := cap + h.ToString + 'h ';
  If m > 0 then
    cap := cap + m.ToString + 'm ';
  If s > 0 then
    cap := cap + s.ToString + 's ';

  Fruit_GrowingTime.Caption := cap;
  time := (F.StagesCount - F.MatureTreeStage) * F.ProgressPerStage;
  h := time div (60 * 60 * 10) mod 24;
  m := time div (60 * 10) mod 60;
  s := time div 10 mod 60;
  cap := '';
  If h > 0 then
    cap := cap + h.ToString + 'h ';
  If m > 0 then
    cap := cap + m.ToString + 'm ';
  If s > 0 then
    cap := cap + s.ToString + 's ';
  Fruit_ReGrowingTime.Caption := cap;


  Fruit_Crops.Caption := F.Fruits.ToString(ffNumber, 3, 3);
  climOrder := [tcDry1, tcDry2, tcWarm1, tcWarm2, tcWet1, tcWet2, tcNeutral, tcCold1, tcCold2];
  for I := 0 to High(climOrder) do
    for J := I to High(climOrder) do
      If I <> J then
      CompareClimates(climOrder[I], climOrder[J]);


  Terrain_Best.Caption := gResTexts[2188] + '|' + gResTexts[2187] + Round(F.ClimateMulti[climOrder[0]] * 100).ToString + '%|' +
                           gResTexts[293] +': '  + Round(1 / F.ClimateMulti[climOrder[0]] * 100).ToString + '%';
  Terrain_Worst.Caption := gResTexts[2189] + '|' + gResTexts[2187] + Round(F.ClimateMulti[climOrder[high(climOrder)]] * 100).ToString + '%|' +
                            gResTexts[293] +': ' + Round(1 / F.ClimateMulti[climOrder[high(climOrder)]] * 100).ToString + '%';


  Fruit_ClimateViewer.SetOrder(climOrder);

  I := (F.StagesCount - F.MatureTreeStage) * F.ProgressPerStage;

  best := 0;
  worst := 100;

  for clim in climOrder do
  begin
    If F.ClimateMulti[clim] > best then
      best := F.ClimateMulti[clim];

    If F.ClimateMulti[clim] < worst then
      worst := F.ClimateMulti[clim];
  end;
  best := best - 1;
  for clim in climOrder do
  begin
    //how many fruits per 2 hours
    time := Round(72000 / (I / F.ClimateMulti[clim]));
    J := Round(F.Fruits * F.ClimateMulti[clim] * time);
    Fruit_ClimateViewer.Caption[clim] := J.ToString;
    If InRange(F.ClimateMulti[clim], 0.9, 1.20) then
      Fruit_ClimateViewer.ClimateColor[clim].SetColor(0, 0, 1)
    else
    If F.ClimateMulti[clim] > 1 then
      Fruit_ClimateViewer.ClimateColor[clim].SetColor(0, (F.ClimateMulti[clim] - 1) / best, 1 - ((F.ClimateMulti[clim] - 1) / best))
    else
      Fruit_ClimateViewer.ClimateColor[clim].SetColor(1 - (F.ClimateMulti[clim] - worst), 0, (F.ClimateMulti[clim] - worst));

  end;
end;


procedure TKMGUIGameGuide.RefreshAnimal;
var fill : Single;
   I : Integer;
begin
  Panel_Animals.Show;
  If fSelectedAnimal = utNone then
    Exit;
  fill := 0;
  case fSelectedAnimal of
    utWolf: Inc(fill, 0.4);//wolf
    utDeerMale: Inc(fill, 1.2);//deer male
    utDeerFemale: Inc(fill, 1);//deer female
    utFox: Inc(fill, 0.30);//fox
    utBoar: Inc(fill, 1.4);//boar
    utBear: Inc(fill, 2);//bear
    utLandDuck: Inc(fill, 0.2);//duck
    utRabbit: Inc(fill, 0.2);//rabbit
    utWhiteBear: Inc(fill, 2);//polar bear
    utFish: Inc(fill, 1);//polar bear
  end;
  for I := Low(Animal_Crops) to High(Animal_Crops) do
    Animal_Crops[I].Caption := '0,00';


  If fSelectedAnimal = utFish then
  begin
      Animal_Crops[3].Caption := fill.ToString(ffNumber, 3, 2); //fish
  end else
  begin
    Animal_Crops[0].Caption := fill.ToString(ffNumber, 3, 2); //meat

    If fSelectedAnimal = utLandDuck then
      Animal_Crops[2].Caption := Single(fill * 2).ToString(ffNumber, 3, 2) //feathers
    else
      Animal_Crops[1].Caption := fill.ToString(ffNumber, 3, 2); //skin
  end;

  for I := Low(Animal_Crops) to High(Animal_Crops) do
    If Animal_Crops[I].Caption <> '0,00' then
      Animal_Crops[I].CapColor := icWhite
    else
      Animal_Crops[I].CapColor := icGray;


  If fSelectedAnimal = utFish then
    Animal_CropsLabel.Caption := gResTexts[2187] + '  (' + gRes.Houses[htFishermans].HouseName + ')'
  else
  if fill > 0 then
    Animal_CropsLabel.Caption := gResTexts[2187] + '  (' + gRes.Houses[htCollectors].HouseName + ')'
  else
    Animal_CropsLabel.Caption := gResTexts[2187];



end;

procedure TKMGUIGameGuide.RefreshPanel;
var I : Integer;
  UT : TKMUnitType;
begin
  If fType = gtUnits then
  begin
    Image_Selected.Height := 250;
    for I := 0 to High(Button_Select) do
    begin
      Button_Select[I].FlagColor := gMySpectator.Hand.FlagColor;
      if I > high(MapEd_Order) then
        Button_Select[I].Hide
      else
      begin
        UT := MapEd_Order[I];
        Button_Select[I].Top := 10 + (I div 6) * 40;
        Button_Select[I].Left := 30 + (I mod 6) * 34;
        Button_Select[I].Height := 38;

        Button_Select[I].TexID := gRes.Units[UT].GUIIcon;
        Button_Select[I].Hint := gRes.Units[UT].GUIName;

        Button_Select[I].Show;
        Button_Select[I].Down := fSelected = I;

        case gRes.Units[UT].GetTrainingHouse of
          htSchool : Button_Select[I].BackBevelColor := $2276403E;
          htBarracks : Button_Select[I].BackBevelColor := $22FF0000;
          htTownhall : Button_Select[I].BackBevelColor := $2200BDFF;
          htPalace : Button_Select[I].BackBevelColor := $220000FF;
          htSiegeWorkshop : Button_Select[I].BackBevelColor := $22FF00C6;
          htShipyard : Button_Select[I].BackBevelColor := $22FFB400;
          else
            Button_Select[I].BackBevelColor := $22FFFFFF;
        end;
      end;
    end;

  end else
  If fType = gtHouses then
  begin
    for I := 0 to High(Button_Select) do
    begin
      Button_Select[I].BackBevelColor := 0;
      Button_Select[I].FlagColor := high(Cardinal);
      Button_Select[I].Top := 10 + (I div 6) * 34;
      Button_Select[I].Left := 30 + (I mod 6) * 34;
      Button_Select[I].Height := 32;

      Button_Select[I].TexID := gRes.Houses[GUIHouseOrderFull[I + 1]].GUIIcon;
      Button_Select[I].Hint := gRes.Houses[GUIHouseOrderFull[I + 1]].HouseName;
      Button_Select[I].Show;
      Button_Select[I].Down := fSelected = I;
    end;

  end else
  for I := 0 to High(Button_Select) do
    Button_Select[I].Hide;

  Panel_House.Hide;
  Panel_Unit.Hide;
  Panel_Grain.Hide;
  Panel_Fruit.Hide;
  Panel_Animals.Hide;
  Image_Selected.Hide;
  Icons_Workers.hide;
  case fType of
    gtUnits: RefreshUnit;
    gtHouses: RefreshHouse;
    gtGrain: RefreshGrain;
    gtFruitTree: RefreshFruit;
    gtAnimals: RefreshAnimal;
  end;
end;
procedure TKMGUIGameGuide.Guide_SelectIcon(Sender: TObject);
begin
  fSelected := TKMButton(Sender).Tag;
  RefreshPanel;
end;

procedure TKMGUIGameGuide.ReleasedByClick(Sender: TObject);
var I : integer;
  HT : TKMHouseType;
begin
  HT := TKMHouseType(TKMButtonFlat(Sender).Tag);
  for I := 0 to High(Button_Select) do
    if GUIHouseOrderFull[I + 1] = HT then
    begin
      fSelected := I;
      Break;
    end;
  RefreshPanel;
end;

procedure TKMGUIGameGuide.WaresRowClick(Sender: TObject);
  function FindHouseWhichProduceWare(aWare: TKMWareType; aAvoid : TKMHouseType) : TKMHouseType;
  var J : Integer;
    HT, HT2 : TKMHouseType;
  begin
    Result := htNone;

    HT2 := TKMHouseType(Byte(aAvoid) + 1);
    for HT := HOUSE_MIN to HOUSE_MAX do
    begin
      if HT2 > HOUSE_MAX then
        HT2 := HOUSE_MIN;
      for J := 1 to 4 do
        If gRes.Houses[HT2].WareOutput[J] = aWare then
          Exit(HT2);
      HT2 := TKMHouseType(Byte(HT2) + 1);
    end;

  end;

  function FindHouseWhichProduceVWare(aWareID: Integer; aAvoid : TKMHouseType) : TKMHouseType;
  var HT, HT2 : TKMHouseType;
  begin
    Result := htNone;

    HT2 := TKMHouseType(Byte(aAvoid) + 1);
    for HT := HOUSE_MIN to HOUSE_MAX do
    begin
      if HT2 > HOUSE_MAX then
        HT2 := HOUSE_MIN;

      if HT2 in gRes.Wares.VirtualWares.Ware[aWareID].ProduceInHouses then
          Exit(HT2);

      HT2 := TKMHouseType(Byte(HT2) + 1);
    end;

  end;

var I : Integer;
    H : TKMHouseType;
    W : TKMWareType;
begin
  if TKMControl(Sender).Tag2 = 1 then
  begin
    H := FindHouseWhichProduceVWare(TKMControl(Sender).Tag, GUIHouseOrderFull[fSelected + 1] );
  end
  else
  begin
    W := TKMWareType( TKMControl(Sender).Tag );

    if not gRes.Wares[W].IsValid then Exit;
    H := FindHouseWhichProduceWare(W, GUIHouseOrderFull[fSelected + 1] );

  end;


  if H = htNone then
    Exit;
  fType := gtHouses;

  for I := 0 to High(Button_Select) do
    if GUIHouseOrderFull[I + 1] = H then
    begin
      fSelected := I;
      Break;
    end;

  RefreshPanel;



end;
procedure TKMGUIGameGuide.GrainClick(Sender : TOBject);
begin
  fSelectedGrain := TKMGrainType(TKMButtonFlat(Sender).tag);
  GrainViewer.SetGrainType(fSelectedGrain);
  RefreshGrain;
end;

procedure TKMGUIGameGuide.FruitClick(Sender : TOBject);
begin
  fSelectedFruit := TKMButtonFlat(Sender).Tag;
  Fruit_Viewer.SetFruitType(fSelectedFruit);
  RefreshFruit;
end;

procedure TKMGUIGameGuide.AnimalClick(Sender : TOBject);
begin
  fSelectedAnimal := TKMUnitType(TKMButtonFlat(Sender).Tag);
  Animal_Viewer.SetAnimalType(fSelectedAnimal);
  RefreshAnimal;
end;



constructor TKMGrainViewer.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fGrainType := gftNone;
  AnimStep := 0;
end;

procedure TKMGrainViewer.SetGrainType(aType : TKMGrainType);
begin
  fGrainType := aType;
  AnimStep := 0;
  fStage := 0;
end;

procedure TKMGrainViewer.UpdateState(aTickCount: Cardinal);
begin
  Inherited;
  Inc(AnimStep);

  fStage := (AnimStep div 15) mod (gFieldGrains[fGrainType].StagesCount);
end;

procedure TKMGrainViewer.Paint;
var A : TKMAnimLoop;
  obj, id1 : Word;
  cnX, cnY : Integer;
begin
  Inherited;
  TKMRenderUI.WriteBevel(ABSLeft, ABSTop, Width, Height);

  If fGrainType = gftNone then
    Exit;
  //center
  cnX := AbsLeft + Width div 2 - 20;//corner of the tile
  cnY := AbsTop + Height div 2 - 20;//corner of the tile


  TKMRenderUI.WritePicture(cnX, cnY, 40, 40, [anLeft, anTop, anRight, anBottom], rxTiles, gFieldGrains[fGrainType].Stage[fStage].Terr + 1);
  If gFieldGrains[fGrainType].Stage[fStage].Obj <> 255 then
  begin
    obj := gFieldGrains[fGrainType].Stage[fStage].Obj;
    A := gMapElements[obj].Anim;
    id1 := A.Step[(AnimStep mod A.Count) + 1];

    If gMapElements[obj].WineOrCorn then
    begin
      cnX := cnX + 20;
      cnY := cnY + 20;
      If gMapElements[obj].RenderAsTwo then
      begin
        TKMRenderUI.WritePictureWithPivot(cnX + 10, cnY,rxTrees, id1 + 1);
        id1 := A.Step[((AnimStep + 1) mod A.Count) + 1];

        TKMRenderUI.WritePictureWithPivot(cnX - 10, cnY, rxTrees, id1 + 1);
      end else
      begin
        TKMRenderUI.WritePictureWithPivot(cnX - 10, cnY - 10, rxTrees, id1 + 1);
        id1 := A.Step[((AnimStep + 1) mod A.Count) + 1];
        TKMRenderUI.WritePictureWithPivot(cnX + 10, cnY - 10, rxTrees, id1 + 1);

        id1 := A.Step[((AnimStep + 2) mod A.Count) + 1];
        TKMRenderUI.WritePictureWithPivot(cnX - 10, cnY + 10, rxTrees, id1 + 1);
        id1 := A.Step[((AnimStep + 3) mod A.Count) + 1];
        TKMRenderUI.WritePictureWithPivot(cnX + 10, cnY + 10, rxTrees, id1 + 1);

      end;
    end else
    begin
      TKMRenderUI.WritePictureWithPivot(cnX, cnY, rxTrees, id1 + 1);
    end;

  end;
  TKMRenderUI.WriteText(AbsLeft, AbsBottom - 20, Width, Caption, fntGrey, taCenter);
end;



constructor TKMFruitViewer.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fFruitType := -1;
  AnimStep := 0;
end;

procedure TKMFruitViewer.SetFruitType(aType: Integer);
begin
  fFruitType := aType;
  AnimStep := 0;
  fStage := 0;
end;

procedure TKMFruitViewer.UpdateState(aTickCount: Cardinal);
begin
  Inherited;
  If fFruitType = -1 then
    Exit;
  Inc(AnimStep);

  fStage := (AnimStep div 15) mod (gFruitTrees[fFruitType].StagesCount);
end;


procedure TKMFruitViewer.Paint;
var A : TKMAnimLoop;
  obj, id1 : Word;
  cnX, cnY : Integer;
begin
  Inherited;
  TKMRenderUI.WriteBevel(ABSLeft, ABSTop, Width, Height);

  If fFruitType = -1 then
    Exit;
  //center
  cnX := AbsLeft + Width div 2;//center
  cnY := AbsBottom - 40;//almost bottom. Tree starts from the ground

  obj := gFruitTrees[fFruitType].Stage[fStage];
  A := gMapElements[obj].Anim;
  id1 := A.Step[(AnimStep mod A.Count) + 1];

  TKMRenderUI.WritePictureWithPivot(cnX, cnY, rxTrees, id1 + 1);

  TKMRenderUI.WriteText(AbsLeft, AbsBottom - 20, Width, Caption, fntGrey, taCenter);
end;

constructor TKMClimateViewer.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  SetOrder([]);
end;

procedure TKMClimateViewer.SetOrder(aOrder: TKMTerrainClimatArray);
begin
  fCount := length(aOrder);
  fClimateOrder := aOrder;
end;

procedure TKMClimateViewer.Paint;
var I, J, gap, L, T : Integer;
  terr : array of TKMTerrainKind;
begin
  Inherited;
  If fCount = 0 then
    Exit;
  L := AbsLeft;
  T := AbsTop;
  gap := 18 + 32;
  for I := 0 to fCount - 1 do
  begin

    case fClimateOrder[I] of
      tcDry1 : terr := [tkGrassSand2, tkGrassSand3];
      tcDry2 : terr := [tkCoastSand, tkSand];
      tcWarm1 : terr := [tkGrass, tkMoss];
      tcWarm2 : terr := [tkGrassSand1];
      tcWet1 : terr := [tkPaleGrass{, tkSwamp}];
      tcWet2 : terr := [tkGrassDirt{, tkGrassyWater, tkWater, tkFastWater}];
      tcNeutral : terr := [tkDirt, tkCobbleStone, tkGravel];
      tcCold1 : terr := [tkSnowOnGrass, tkSnowOnDirt];
      tcCold2 : terr := [tkSnow, tkDeepSnow{, tkIce}];
      else
        Continue;
    end;
    for J := 0 to High(terr) do
    begin
      TKMRenderUI.WriteBevel(l + gap * I - 4, T + J * 43 - 4, 40, 40, ClimateColor[fClimateOrder[I]]);
      TKMRenderUI.WritePictureWithPivot(l + gap * I, T + J * 43, rxTiles, Combo[terr[J], terr[J], 1] + 1);
    end;
    TKMRenderUI.WriteText(l + gap * I, T + J * 43, 40, Caption[fClimateOrder[I]], fntGrey, taLeft);
  end;
end;


constructor TKMAnimalViewer.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aHeight: Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fAnimalType := utNone;
  fDir := dirN;
end;

procedure TKMAnimalViewer.SetAnimalType(aType: TKMUnitType);
begin
  fAnimalType := aType;
  AnimStep := 0;
  fDir := dirN;
end;

procedure TKMAnimalViewer.UpdateState(aTickCount: Cardinal);
begin
  Inherited;
  If fAnimalType = utNone then
    Exit;
  Inc(AnimStep);
  fDir := TKMDirection(((AnimStep div 10) mod 8) + 1);
end;

procedure TKMAnimalViewer.Paint;
var cX, cY : Integer;
  A : TKMAnimation;
begin
  Inherited;
  If fAnimalType = utNone then
    Exit;

  cX := AbsLeft + Width div 2;//center
  cY := AbsTop + Height div 2;//center
  TKMRenderUI.WriteBevel(ABSLeft, ABSTop, Width, Height);

  If fAnimalType = utFish then
    A := gRes.Units[fAnimalType].UnitAnim[uaDie, fDir]
  else
    A := gRes.Units[fAnimalType].UnitAnim[uaWalk, fDir];

  TKMRenderUI.WritePictureWithPivot(cX, cY, rxUnits, A.Animation[AnimStep] + 1);
end;

end.


