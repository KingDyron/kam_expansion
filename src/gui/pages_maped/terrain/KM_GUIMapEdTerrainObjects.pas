unit KM_GUIMapEdTerrainObjects;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Math, SysUtils,
  KM_InterfaceDefaults,
  KM_Controls, KM_ControlsList, KM_ControlsBase, KM_ControlsPopUp, KM_ControlsScroll, KM_ControlsSwitch, KM_ControlsTrackBar,
  KM_ControlsEdit, KM_ControlsDrop,
  KM_TerrainTypes, KM_ResMapElements,
  KM_Defaults, KM_Pics, KM_Cursor, KM_Points, KM_CommonTypes,
  Vcl.Controls;

type
  TKMTerrainObjectAttribute = (toaBlockDiagonal, toaBlockAllExceptBuild, toaBlockBuild, toaChoppableTree);

  TKMTerrainObjectAttributeSet = set of TKMTerrainObjectAttribute;

  TKMMapEdTerrainObjects = class(TKMMapEdSubMenuPage)
  private
    fHideAllPages: TEvent;
    //Objects in MapElem are placed sparsely, so we need to compact them
    //to use in MapEd palette
    fLastObjectIndex: Integer;
    fCountCompact: Integer;
    fCompactToMapElem: array of Word; //Pointers to valid MapElem's
    fMapElemToCompact: array of Word; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.

    fObjPaletteTableSize: TKMPoint;
    fObjPaletteAttr: array of TKMTerrainObjectAttributeSet;

    procedure SwitchPage(aPage : Byte);
    function GetObjPaletteTableHeight: Integer;
    function GetObjPaletteTableWidth: Integer;

    procedure UpdatePaletteButton(aBtnID: Integer);
    function UpdateObjAttributesAndDesc(aBtn: TKMButtonFlat; aObjID: Integer): TKMTerrainObjectAttributeSet;
    procedure CompactMapElements;
    procedure ObjectsUpdate(aObjIndex: Integer);
    procedure UpdateObjectsScrollPosToIndex(aObjIndex: Integer);
    procedure ObjectsOverride(Sender: TObject);
    procedure ObjectsChange(Sender: TObject);
    procedure ObjectsBrushChange(Sender: TObject);
    procedure ObjectsRefresh(Sender: TObject);

    procedure ObjectsPalette_Refresh(Sender: TObject);
    procedure ObjPalette_UpdateControlsPosition;
    procedure ObjectsPalette_OnShow(Sender: TObject; aVisible: Boolean);
    procedure ObjPalette_ClickShift(Sender: TObject; Shift: TShiftState);

    procedure ObjectsPaletteButton_Click(Sender: TObject);
    procedure ObjectsPaletteClose_Click(Sender: TObject);


    procedure CreateWaresPanel(aParent : TKMPanel);

    procedure Wares_Change(Sender : TObject);
    procedure Button_Swith_Click(Sender : TObject);
    procedure WaresOverride(Sender: TObject);

    procedure SortType_Change(Sender : TObject);

  protected
    Buttons_SwitchPage : array[0..1] of TKMButton;

    Panel_Objects: TKMScrollPanel;
      ObjectErase: TKMButtonFlat;
      ObjectBlock: TKMButtonFlat;
      ObjectsPalette_Button: TKMButtonFlat;
      ObjectsTable: array [0..8] of TKMButtonFlat;
      ObjectsScroll: TKMScrollBar;
      Objects_Override: TKMCheckBox;

    Panel_WaresOnGround : TKMScrollPanel;
      Wares_Override: TKMCheckBox;
      Button_Brush : TKMButtonFlat;
      Button_Wares : array of TKMButtonFlat;
      Scroll_Count : TKMTrackBar;


    PopUp_ObjectsPalette: TKMPopUpMenu;
      Bevel_ObjectsPalette: TKMBevel;
      Image_ObjectsPalette: TKMImage;
      Label_ObjectsPalette: TKMLabel;
      Button_ClosePalette: TKMButton;
      Button_ObjPaletteErase: TKMButtonFlat;
      Button_ObjPaletteBlock: TKMButtonFlat;
      Panel_ObjectsTable: TKMScrollPanel;
        ObjectsPaletteTable: array of TKMButtonFlat;
      Image_ObjectAttributes: array[0..2] of array of TKMImage;
      DropBox_SortType: TKMDropList;

      //Objects brush
      BrushSize, ForestDensity, ForestAge: TKMTrackBar;
      ObjectTypeSet: array [TKMTerrainObjectType] of TKMButtonFlat;
      BrushCircle, BrushSquare: TKMButtonFlat;
      CleanBrush: TKMButtonFlat;
      Label_ForestAge: TKMLabel;
      OverrideObjects: TKMCheckBox;
      Scroll_ObjectsPalette: TKMScrollBar;

  public
    constructor Create(aParent: TKMPanel; aHideAllPages: TEvent);

    procedure KeyDown(Key: Word; Shift: TShiftState; aIsFirst: Boolean; var aHandled: Boolean);
    procedure MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer; var aHandled: Boolean);

    procedure Show;
    function Visible: Boolean; override;
    function IsPaletteVisible: Boolean;
    procedure PaletteHide;

    procedure Hide;
    procedure Resize;
    procedure Cancel_Clicked(var aHandled: Boolean);
    procedure UpdateHotkeys;
    procedure UpdateState;
  end;

  TKMMapEdTerrainPatterns = class(TKMMapEdSubMenuPage)
  private
    procedure ObjectsSelectCollection(Sender: TObject);
    procedure ObjectsPatternClick(Sender: TObject);
    procedure ObjectsPatternClickDouble(Sender: TObject);
    procedure DeletePatternClick(Sender: TObject);
    procedure RefreshPatternsList;
    procedure RefreshCollectionList;
    procedure ShowRename;
    procedure ConfirmRename(Sender: TObject);
    procedure MovePattern(Sender: TObject);
    procedure MovePatternHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
  protected
    Panel_PatternSelection : TKMPanel;
      BrushSelectSize : TKMTrackBar;

      Brush_Hide,
      Brush_Show,
      Brush_Clean,
      Brush_Select,
      BrushSelectSquare,
      BrushSelectCircle : TKMButtonFlat;
      Button_SetFromPattern,
      Button_OpenPatternSettings : TKMButton;

    PopUp_Patterns: TKMPopUpMenu;
      ColumnBox_CollectionList,
      ColumnBox_PatternsList: TKMColumnBox;
      Button_SavePatterns: TKMButton;
      Button_RefreshPatterns, Button_ReName, Button_Delete: TKMButton;
      Button_MoveUp, Button_MoveDown: TKMButton;
      Check_Override,
      Check_DeSelect: TKMCheckBox;
      Scroll_AdditionalHeight : TKMTrackBar;

    PopUp_Rename: TKMPopUpMenu;
      Image_Rename: TKMImage;
      Label_RenameTitle, Label_RenameName: TKMLabel;
      FilenameEdit_Rename: TKMFilenameEdit;
      Button_MapRenameConfirm, Button_MapRenameCancel: TKMButton;

    PopUp_Delete : TKMPopUpConfirm;
  public
    constructor Create(aParent: TKMPanel);
    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdateState;

  end;

implementation
uses
  KM_Main, KM_Resource, KM_ResFonts, KM_ResTexts, KM_ResKeys, KM_Terrain,
  KM_HandsCollection, KM_RenderUI, KM_InterfaceGame, KM_Utils, KM_Game,
  KM_ResTypes;

type
  TKMObjBrushForestAge = (faAll, faAllButStomps, faYoung, faMedium, faBig, faChop, faStomp);

const
  OBJECTS_PALETTE_MAX_COLS_CNT = 17;
  OBJ_CELL_W = 68;
  OBJ_CELL_H = 84;

  OBJ_CELL_PALETTE_W = 68;
  OBJ_CELL_PALETTE_H = 84;

  OBJ_NONE_TAG = -100;
  OBJ_BLOCK_TAG = -200;
  //Objects brush bins
  BTN_BRUSH_TYPE_S = 30;
  BTN_BRUSH_SIZE = 36;

  OBJECT_MAX_DENSITY = 30;


  FOREST_AGE_THUMB_TX: array[TKMObjBrushForestAge] of Integer = (TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL_BUT_STOMP,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_YOUNG,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_MEDIUM,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_BIG,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_RDY_2CHOP,
                                                                 TX_MAPED_OBJECTS_BRUSH_TREES_AGE_STOMP);

  FOREST_AGE_HINT_TX: array[TKMObjBrushForestAge] of Integer = (TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL_BUT_STOMP_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_YOUNG_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_MEDIUM_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_BIG_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_RDY_2CHOP_HINT,
                                                                TX_MAPED_OBJECTS_BRUSH_TREES_AGE_STOMP_HINT);

  OBJECT_TYPE_BTN: array [TKMTerrainObjectType] of record
    TexID: Integer;
    HintTX: Integer;
  end = (
    (TexID: 226;  HintTX: TX_MAPED_OBJECTS_BRUSH_TREES;),
    (TexID: 34;   HintTX: TX_MAPED_OBJECTS_BRUSH_ALL;),
    (TexID: 14;   HintTX: TX_MAPED_OBJECTS_BRUSH_FLOWERS;),
    (TexID: 4;    HintTX: TX_MAPED_OBJECTS_BRUSH_MUSHROOMS;),
    (TexID: 26;   HintTX: TX_MAPED_OBJECTS_BRUSH_STUMPS;),
    (TexID: 39;   HintTX: TX_MAPED_OBJECTS_BRUSH_DEAD;),
    (TexID: 21;   HintTX: TX_MAPED_OBJECTS_BRUSH_STONES;),
    (TexID: 143;  HintTX: TX_MAPED_OBJECTS_BRUSH_BUSH;),
    (TexID: 173;  HintTX: TX_MAPED_OBJECTS_BRUSH_CACTUS;),
    (TexID: 245;  HintTX: TX_MAPED_OBJECTS_BRUSH_RUINS;)
  );


{ TKMMapEdTerrainObjects }
constructor TKMMapEdTerrainObjects.Create(aParent: TKMPanel; aHideAllPages: TEvent);

  function GetForestAgeThumbWidth(aFont: TKMFont): Integer;
  var
    BFA: TKMObjBrushForestAge;
  begin
    Result := 0;
    for BFA := Low(FOREST_AGE_THUMB_TX) to High(FOREST_AGE_THUMB_TX) do
      Result := Max(Result, gRes.Fonts[aFont].GetTextSize(gResTexts[FOREST_AGE_THUMB_TX[BFA]]).X);

    Inc(Result, TKMTrackBar.THUMB_WIDTH_ADD);
  end;

var
  top: Integer;

  function NextTop(aInc: Integer): Integer;
  begin
    Result := top;
    top := top + aInc;
  end;

var
  I, J: Integer;
//  TOA: TKMTerrainObjectAttribute;
  //For brushes
   K: Integer;
  OT: TKMTerrainObjectType;

begin
  inherited Create;

  fHideAllPages := aHideAllPages;
  fLastObjectIndex := -1;
  SetLength(fCompactToMapElem, OBJECTS_CNT + 1);
  SetLength(fMapElemToCompact, OBJECTS_CNT + 1);

  CompactMapElements;
  K := length(Buttons_SwitchPage);

  J := aParent.Width div 2 - (K * 30) div 2;
  Inc(J, 9);
  for I := 0 to High(Buttons_SwitchPage) do
  begin
    Buttons_SwitchPage[I] := TKMButton.Create(aParent, J + I * 30, 32, 30, 28, 0, rxGui, bsGame);
    Buttons_SwitchPage[I].OnClick := Button_Swith_Click;
    Buttons_SwitchPage[I].Tag := I;
    case I of
      0: Buttons_SwitchPage[I].TexID := 712;
      1: Buttons_SwitchPage[I].TexID := 717;
    end;
    case I of
      0: Buttons_SwitchPage[I].Hint := gResTexts[372];
      1: Buttons_SwitchPage[I].Hint := gResTexts[2089];
    end;
  end;
    
  Panel_Objects := TKMScrollPanel.Create(aParent, 0, 28 + 30, aParent.Width, aParent.Height - 28 - 30, [saVertical], bsMenu, ssCommon);
  Panel_Objects.Padding.SetBottom(10);
  Panel_Objects.ScrollV_PadTop := 10;
  Panel_Objects.ScrollV_PadBottom := 10;
  Panel_Objects.ScrollV_PadLeft := 0;
  Panel_Objects.ScrollV.Left := Panel_Objects.ScrollV.Left + 20;
  Panel_Objects.AnchorsStretch;

  with TKMLabel.Create(Panel_Objects, 0, TERRAIN_PAGE_TITLE_Y, Panel_Objects.Width, 0, gResTexts[TX_MAPED_OBJECTS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 9, 295, Panel_Objects.Width - 9, 20, saHorizontal, bsGame);
  ObjectsScroll.Anchors := [anLeft, anTop, anRight];
  ObjectsScroll.MinValue := 0;
  ObjectsScroll.MaxValue := (fCountCompact - 1) div 3 - 2;
  ObjectsScroll.Position := 0;
  ObjectsScroll.OnChange := ObjectsRefresh;
  for I := 0 to 2 do
    for J := 0 to 2 do
    begin
      ObjectsTable[I*3+J] := TKMButtonFlat.Create(Panel_Objects, 9 + I*(OBJ_CELL_W + 1), 40 + J*(OBJ_CELL_H + 1),
                                                  OBJ_CELL_W, OBJ_CELL_H, 1, rxTrees); //RXid=1  // 1 2
      ObjectsTable[I*3+J].CapOffsetY := 15;
      ObjectsTable[I*3+J].Tag := I*3+J; //Store ID
      ObjectsTable[I*3+J].OnClick := ObjectsChange;
      ObjectsTable[I*3+J].OnMouseWheel := ObjectsScroll.MouseWheel;
    end;
  ObjectErase := TKMButtonFlat.Create(Panel_Objects, 9, 4, 32, 32, 340);
  ObjectErase.Tag := OBJ_NONE_TAG; //no object
  ObjectErase.OnClick := ObjectsChange;

  ObjectBlock := TKMButtonFlat.Create(Panel_Objects, Panel_Objects.Width - 32, 4, 32, 32, 254,rxTrees);
  ObjectBlock.Anchors := [anTop, anRight];
  ObjectBlock.Tag := OBJ_BLOCK_TAG; //block object
  ObjectBlock.OnClick := ObjectsChange;

  ObjectsPalette_Button := TKMButtonFlat.Create(Panel_Objects, 9, 320, Panel_Objects.Width - 9, 21, 0);
  ObjectsPalette_Button.Anchors := [anLeft, anTop, anRight];
  ObjectsPalette_Button.Caption := gResTexts[TX_MAPED_TERRAIN_OBJECTS_PALETTE];
  ObjectsPalette_Button.CapOffsetY := -11;
  ObjectsPalette_Button.OnClick := ObjectsPaletteButton_Click;
  PopUp_ObjectsPalette := TKMPopUpMenu.Create(aParent.MasterParent, aParent.MasterParent.Width - 50);
  PopUp_ObjectsPalette.Height := aParent.MasterParent.Height - 50;
  PopUp_ObjectsPalette.OnChangeVisibility := ObjectsPalette_OnShow;
  // Keep the pop-up centered
  PopUp_ObjectsPalette.AnchorsCenter;
  PopUp_ObjectsPalette.Left := 25;
  PopUp_ObjectsPalette.Top := 25;

    Bevel_ObjectsPalette := TKMBevel.Create(PopUp_ObjectsPalette, -2000,  -2000, 5000, 5000);
    Bevel_ObjectsPalette.BackAlpha := 0.7;
    Bevel_ObjectsPalette.EdgeAlpha := 0.9;
    Bevel_ObjectsPalette.OnClickShift := ObjPalette_ClickShift;

    Image_ObjectsPalette := TKMImage.Create(PopUp_ObjectsPalette, 0, 0, PopUp_ObjectsPalette.Width, PopUp_ObjectsPalette.Height, 18, rxGuiMain);
    Image_ObjectsPalette.ImageStretch;
    Image_ObjectsPalette.OnClickShift := ObjPalette_ClickShift;

    Scroll_ObjectsPalette := TKMScrollBar.Create(PopUp_ObjectsPalette, PopUp_ObjectsPalette.Width - 20, 25, 20, PopUp_ObjectsPalette.Height - 75, saVertical, bsGame);
    Scroll_ObjectsPalette.MinValue := 0;
    Scroll_ObjectsPalette.Position := 0;
    Scroll_ObjectsPalette.OnChange := ObjectsPalette_Refresh;
    Scroll_ObjectsPalette.Hide;
    // Add event handlers after Scroll is created
    Image_ObjectsPalette.OnMouseWheel := Scroll_ObjectsPalette.MouseWheel;
    Bevel_ObjectsPalette.OnMouseWheel := Scroll_ObjectsPalette.MouseWheel;

    Panel_ObjectsTable := TKMScrollPanel.Create(PopUp_ObjectsPalette, PopUp_ObjectsPalette.Width div 2 - 600, 0, 1200, PopUp_ObjectsPalette.Height - 125, [saVertical], bsMenu, ssCommon);
    Panel_ObjectsTable.AnchorsStretch;
    Panel_ObjectsTable.ScrollV.WheelStep := 100;
    Panel_ObjectsTable.MouseWheelStep := 100;

    SetLength(ObjectsPaletteTable, fCountCompact);
    for I := 0 to fCountCompact - 1 do
    begin
      ObjectsPaletteTable[I] := TKMButtonFlat.Create(Panel_ObjectsTable, 0, 0, OBJ_CELL_PALETTE_W, OBJ_CELL_PALETTE_H, 1, rxTrees); // Left and Top will update later
      ObjectsPaletteTable[I].Tag := I; //Store ID
      ObjectsPaletteTable[I].CapOffsetY := 15;
//      ObjectsPaletteTable[I].TexOffsetY := 0;
      ObjectsPaletteTable[I].Enable;
      ObjectsPaletteTable[I].Hide;
      //ObjectsPaletteTable[I].OnMouseWheel := Scroll_ObjectsPalette.MouseWheel;
      ObjectsPaletteTable[I].OnClickShift := ObjPalette_ClickShift;

    end;

    SetLength(fObjPaletteAttr, fCountCompact);
    for J := Low(Image_ObjectAttributes) to High(Image_ObjectAttributes) do
    begin
      SetLength(Image_ObjectAttributes[J], fCountCompact);
      for I := 0 to fCountCompact - 1 do
      begin
        Image_ObjectAttributes[J, I] := TKMImage.Create(Panel_ObjectsTable, 0, 0, 0, 0, 0);
        Image_ObjectAttributes[J, I].Hide;
        Image_ObjectAttributes[J, I].Hitable := False;
      end;
    end;

    Label_ObjectsPalette := TKMLabel.Create(PopUp_ObjectsPalette, PopUp_ObjectsPalette.Center.X, 0, gResTexts[TX_MAPED_TERRAIN_OBJECTS_PALETTE], fntOutline, taCenter);

    Button_ObjPaletteErase := TKMButtonFlat.Create(PopUp_ObjectsPalette, 0, 0, OBJ_CELL_W, 32, 340);
    Button_ObjPaletteErase.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_REMOVE];
    Button_ObjPaletteErase.Tag := OBJ_NONE_TAG; //no object
    Button_ObjPaletteErase.OnClickShift := ObjPalette_ClickShift;

    Button_ObjPaletteBlock := TKMButtonFlat.Create(PopUp_ObjectsPalette, 0, 0, OBJ_CELL_W, 32, 254, rxTrees);
    Button_ObjPaletteBlock.Hint := gResTexts[TX_MAPED_TERRAIN_OBJECTS_BLOCK];
    Button_ObjPaletteBlock.Tag := OBJ_BLOCK_TAG; //block object
    Button_ObjPaletteBlock.OnClickShift := ObjPalette_ClickShift;

    DropBox_SortType := TKMDropList.Create(PopUp_ObjectsPalette, 0, 0, 180, 20, fntGrey, gResTexts[2090], bsMenu);
    DropBox_SortType.Add(gResTexts[2091]);
    DropBox_SortType.Add(gResTexts[2092]);
    DropBox_SortType.Add(gResTexts[2093]);
    DropBox_SortType.Add(gResTexts[2094]);
    DropBox_SortType.Add(gResTexts[2095]);
    DropBox_SortType.Add(gResTexts[2096]);
    DropBox_SortType.OnChange := SortType_Change;
    DropBox_SortType.DropUp := true;
    DropBox_SortType.ItemIndex := 0;
    DropBox_SortType.Hint := gResTexts[2097];
    Button_ClosePalette  := TKMButton.Create(PopUp_ObjectsPalette, PopUp_ObjectsPalette.Center.X - 100, PopUp_ObjectsPalette.Bottom - 50,
                                             200, 30, gResTexts[TX_MAPED_TERRAIN_CLOSE_PALETTE], bsGame);
    Button_ClosePalette.Anchors := [anLeft,anBottom];
    Button_ClosePalette.OnClick := ObjectsPaletteClose_Click;

    ObjPalette_UpdateControlsPosition;


  // Objects brushes
  top := 350;
  Objects_Override := TKMCheckBox.Create(Panel_Objects, 9, NextTop(25), Panel_Objects.Width, 20, gResTexts[1503], fntMetal);
  Objects_Override.Checked := true;
  Objects_Override.OnClick := ObjectsOverride;

  with TKMLabel.Create(Panel_Objects, 9, NextTop(25), Panel_Objects.Width, 0, gResTexts[TX_MAPED_OBJECTS_BRUSH], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  BrushSize := TKMTrackBar.Create(Panel_Objects, 9, top + 3, (Panel_Objects.Width - (BTN_BRUSH_TYPE_S * 2) - 18) - 18, 4, MAPED_BRUSH_MAX_SIZE);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.Position := 1;
  BrushSize.OnChange := ObjectsBrushChange;
  BrushSize.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT, gResTexts[TX_KEY_CTRL_MOUSEWHEEL]);

  BrushCircle := TKMButtonFlat.Create(Panel_Objects, Panel_Objects.Width - (BTN_BRUSH_TYPE_S * 2) - 18, top, BTN_BRUSH_TYPE_S, BTN_BRUSH_TYPE_S, 592);
  BrushCircle.Anchors := [anTop, anRight];
  BrushCircle.OnClick := ObjectsBrushChange;
  BrushCircle.TexOffsetX := 1;
  BrushCircle.TexOffsetY := 1;
  BrushCircle.Down := True;

  BrushSquare := TKMButtonFlat.Create(Panel_Objects, Panel_Objects.Width - BTN_BRUSH_TYPE_S - 9, top, BTN_BRUSH_TYPE_S, BTN_BRUSH_TYPE_S, 593);
  BrushSquare.Anchors := [anTop, anRight];
  BrushSquare.OnClick := ObjectsBrushChange;
  BrushSquare.TexOffsetX := 1;
  BrushSquare.TexOffsetY := 1;

  NextTop(35);

  CleanBrush := TKMButtonFlat.Create(Panel_Objects, 9, NextTop(45), 34, 34, 673, rxGui);
  CleanBrush.Anchors := [anLeft, anTop];
  CleanBrush.OnClick := ObjectsBrushChange;

  OverrideObjects := TKMCheckBox.Create(Panel_Objects, 9, NextTop(40), Panel_Objects.Width - 9, 40, gResTexts[TX_MAPED_OBJECTS_BRUSH_OVERRIDE_OBJECTS], fntMetal);
  OverrideObjects.OnClick := ObjectsBrushChange;
  OverrideObjects.Hint := gResTexts[TX_MAPED_OBJECTS_BRUSH_OVERRIDE_OBJECTS_HINT];

  for OT := Low(TKMTerrainObjectType) to High(TKMTerrainObjectType) do
  begin
    J := Ord(OT) mod 5;
    K := Ord(OT) div 5;

    ObjectTypeSet[OT] := TKMButtonFlat.Create(Panel_Objects, 9+BTN_BRUSH_SIZE*J, top + BTN_BRUSH_SIZE*K, 34, 34, OBJECT_TYPE_BTN[OT].TexID, rxTrees);

    ObjectTypeSet[OT].OnClick := ObjectsBrushChange;
    ObjectTypeSet[OT].Hint := gResTexts[OBJECT_TYPE_BTN[OT].HintTX];
  end;

  NextTop(80);

  ForestDensity   := TKMTrackBar.Create(Panel_Objects, 9, NextTop(50), (Panel_Objects.Width) - 18, 1, OBJECT_MAX_DENSITY);
  ForestDensity.Anchors := [anLeft, anTop, anRight];
  ForestDensity.Caption := gResTexts[TX_MAPED_OBJECTS_BRUSH_DENSITY];
  ForestDensity.Position := 10;
  ForestDensity.OnChange := ObjectsBrushChange;
  ForestDensity.Hint := GetHintWHotkey(TX_MAPED_OBJECTS_BRUSH_DENSITY_HINT, gResTexts[TX_KEY_ALT_MOUSEWHEEL]);

  ForestAge := TKMTrackBar.Create(Panel_Objects, 9, NextTop(50), (Panel_Objects.Width) - 18,
                                  Ord(Low(TKMObjBrushForestAge)), Ord(High(TKMObjBrushForestAge)));
  ForestAge.Anchors := [anLeft, anTop, anRight];
  ForestAge.Caption := gResTexts[TX_MAPED_OBJECTS_BRUSH_AGE];
  ForestAge.Position := Ord(faAll); // All ages by default
  ForestAge.OnChange := ObjectsBrushChange;
  ForestAge.ThumbText := gResTexts[TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL];
  ForestAge.Hint := GetHintWHotkey(TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL_HINT, gResTexts[TX_KEY_SHIFT_MOUSEWHEEL]);

  ForestAge.FixedThumbWidth := True;
  ForestAge.ThumbWidth := GetForestAgeThumbWidth(ForestAge.Font);

  Label_ForestAge := TKMLabel.Create(Panel_Objects, 9, NextTop(20) - 8, Panel_Objects.Width - 18, 20,
                                     gResTexts[TX_MAPED_OBJECTS_BRUSH_TREES_AGE_ALL_HINT], fntGrey, taRight);
  Label_ForestAge.Anchors := [anLeft, anTop, anRight];



  

  gCursor.MapEdObjectsType[otTrees] := True;
  gCursor.MapEdObjectsType[otAllButTrees] := True;

  gCursor.MapEdForestAge := 1;
  gCursor.MapEdCleanBrush := False;
  CreateWaresPanel(aParent);

  // hotkeys for object functions
  fSubMenuActionsEvents[0] := ObjectsChange;
  fSubMenuActionsEvents[1] := ObjectsChange;
  fSubMenuActionsEvents[2] := ObjectsBrushChange;
  fSubMenuActionsEvents[3] := ObjectsBrushChange;
  fSubMenuActionsEvents[4] := ObjectsBrushChange;
  fSubMenuActionsEvents[5] := ObjectsBrushChange;
  fSubMenuActionsEvents[6] := ObjectsBrushChange;

  fSubMenuActionsCtrls[0,0] := ObjectErase;
  fSubMenuActionsCtrls[1,0] := ObjectBlock;
  fSubMenuActionsCtrls[2,0] := BrushCircle;
  fSubMenuActionsCtrls[3,0] := BrushSquare;
  fSubMenuActionsCtrls[4,0] := CleanBrush;
  fSubMenuActionsCtrls[5,0] := ObjectTypeSet[otTrees];
  fSubMenuActionsCtrls[6,0] := ObjectTypeSet[otAllButTrees];
end;

procedure TKMMapEdTerrainObjects.CreateWaresPanel(aParent: TKMPanel);
var I : Integer;
  WT : TKMWareType;
begin
  Panel_WaresOnGround := TKMScrollPanel.Create(aParent, 0, 28 + 30, aParent.Width, aParent.Height - 28 - 30, [saVertical], bsMenu, ssCommon);
  Panel_WaresOnGround.Padding.SetBottom(10);
  Panel_WaresOnGround.ScrollV_PadTop := 10;
  Panel_WaresOnGround.ScrollV_PadBottom := 10;
  Panel_WaresOnGround.ScrollV_PadLeft := -20;
  Panel_WaresOnGround.ScrollV.Left := Panel_Objects.ScrollV.Left + 20;
  Panel_WaresOnGround.AnchorsStretch;

  with TKMLabel.Create(Panel_WaresOnGround, 0, TERRAIN_PAGE_TITLE_Y, Panel_WaresOnGround.Width, 0, gResTexts[2089], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  //Button_Brush := TKMButtonFlat.Create(Panel_WaresOnGround, 9, 27, 33, 33, 673);
  Wares_Override := TKMCheckBox.Create(Panel_WaresOnGround, 9, 25, Panel_Objects.Width, 20, gResTexts[1503], fntMetal);
  Wares_Override.Checked := true;
  Wares_Override.OnClick := WaresOverride;


  Scroll_Count := TKMTrackBar.Create(Panel_WaresOnGround, 9, 52, Panel_WaresOnGround.Width - 18, 0, 15);
  Scroll_Count.Caption := gResTexts[1040];
  Scroll_Count.OnChange := Wares_Change;
  Scroll_Count.ThumbWidth := 100;

  with TKMLabel.Create(Panel_WaresOnGround, 0, 92, Panel_WaresOnGround.Width, 0, gResTexts[2074], fntGrey, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  SetLength(Button_Wares, 0);
  for WT := WARE_MIN to WARE_MAX do
    if length(gRes.Wares[WT].AtTerrainPic) > 0 then
    begin
      I := length(Button_Wares);
      SetLength(Button_Wares, I + 1);
      Button_Wares[I] := TKMButtonFlat.Create(Panel_WaresOnGround, 9 + I mod 5 * 34, 110 + I div 5 * 34, 30, 30, gRes.Wares[WT].GUIIcon);
      Button_Wares[I].Tag := byte(WT);
      Button_Wares[I].Hint := gRes.Wares[WT].Title;
      Button_Wares[I].OnClick := Wares_Change;
    end;
end;

procedure TKMMapEdTerrainObjects.SwitchPage(aPage: Byte);
begin
  Panel_Objects.Hide;
  Panel_WaresOnGround.Hide;
  if aPage = 0 then
  begin
    ObjectsRefresh(nil);
    Panel_Objects.Show;
  end else
  if aPage = 1 then
  begin
    Scroll_Count.Position := 1;
    gCursor.MapEdOverrideObjects := Wares_Override.Checked;
    Wares_Change(Button_Wares[0]);
    Panel_WaresOnGround.Show;
  end;

end;

procedure TKMMapEdTerrainObjects.ObjectsPalette_OnShow(Sender: TObject; aVisible: Boolean);
begin
  if aVisible then
    ObjPalette_UpdateControlsPosition;
end;


function TKMMapEdTerrainObjects.GetObjPaletteTableHeight: Integer;
begin
  Result := (OBJ_CELL_PALETTE_H + 1)*Min(fObjPaletteTableSize.Y, ((fCountCompact - 1) div fObjPaletteTableSize.X) + 1);
end;


function TKMMapEdTerrainObjects.GetObjPaletteTableWidth: Integer;
begin
  Result := (OBJ_CELL_PALETTE_W + 1)*fObjPaletteTableSize.X;
end;


procedure TKMMapEdTerrainObjects.UpdatePaletteButton(aBtnID: Integer);
const
  ATTR_IMG_SIZE = 25;
  IMG_ATTR_TEXID: array[TKMTerrainObjectAttribute] of Integer = (677, 340, 40, 371 {395});
  IMG_ATTR_W: array[TKMTerrainObjectAttribute] of Integer = (14, 19, 23, 18);
  IMG_ATTR_H: array[TKMTerrainObjectAttribute] of Integer = (14, 19, 22, 14);
var
  I, index: Integer;
  TOA: TKMTerrainObjectAttribute;
  attributes: TKMTerrainObjectAttributeSet;
begin
  attributes := UpdateObjAttributesAndDesc(ObjectsPaletteTable[aBtnID], fCompactToMapElem[aBtnID]);
  index := 0;
//  for TOA := Low(TKMTerrainObjectAttribute) to High(TKMTerrainObjectAttribute) do
  for TOA := toaChoppableTree to toaChoppableTree do
  begin
    if TOA in attributes then
    begin
      Image_ObjectAttributes[index, aBtnID].Visible := True;
      Image_ObjectAttributes[index, aBtnID].Left := ObjectsPaletteTable[aBtnID].Left + 2 + ATTR_IMG_SIZE*index;
      Image_ObjectAttributes[index, aBtnID].Top := ObjectsPaletteTable[aBtnID].Bottom - IMG_ATTR_H[TOA] - 10;
      Image_ObjectAttributes[index, aBtnID].Width := IMG_ATTR_W[TOA];
      Image_ObjectAttributes[index, aBtnID].Height := IMG_ATTR_H[TOA];
      Image_ObjectAttributes[index, aBtnID].TexID := IMG_ATTR_TEXID[TOA];
      Inc(index);
    end;
  end;
  for I := index to High(Image_ObjectAttributes) do
    Image_ObjectAttributes[I, aBtnID].Hide;
end;


procedure TKMMapEdTerrainObjects.UpdateHotkeys;
begin
  ObjectsPalette_Button.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_OBJECTS_PALETTE, kfMapedObjPalette);

  ObjectErase.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_OBJECTS_REMOVE, kfMapedSubMenuAction1);
  ObjectBlock.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_OBJECTS_BLOCK,  kfMapedSubMenuAction2);
  BrushCircle.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_HEIGHTS_CIRCLE, kfMapedSubMenuAction3);
  BrushSquare.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_HEIGHTS_SQUARE, kfMapedSubMenuAction4);
  CleanBrush.Hint  := GetHintWHotkey(TX_MAPED_OBJECTS_BRUSH_CLEAN,    kfMapedSubMenuAction5);
end;


function TKMMapEdTerrainObjects.UpdateObjAttributesAndDesc(aBtn: TKMButtonFlat; aObjID: Integer): TKMTerrainObjectAttributeSet;
begin
  Result := [];

  aBtn.CapColor := icWhite;
  aBtn.Hint := aBtn.Caption;

  if ObjectIsChoppableTree(aObjID, [caAge1, caAge2, caAge3, caAgeFull]) then
  begin
    Include(Result, toaChoppableTree);
    aBtn.CapColor := icRoyalYellow;
    aBtn.Hint := aBtn.Hint + ' / ' + gResTexts[TX_MAPED_OBJ_CHOPPABLE_HINT];
  end;

  if gMapElements[aObjID].DiagonalBlocked then
  begin
    Include(Result, toaBlockDiagonal);
    aBtn.Hint := aBtn.Hint + ' / ' + gResTexts[TX_MAPED_OBJ_BLOCK_WALK_DIAG_HINT];
    aBtn.CapColor := icLightRed;
  end;

//This section is not needed, since every AllBlocked object is also blocks building
//  if gMapElements[aObjID].AllBlocked then
//  begin
//    Include(Result, toaBlockAllExceptBuild);
//    HintStr := ' / ' + gResTexts[TX_MAPED_OBJ_BLOCK_ALL_HINT];
//    aBtn.CapColor := icOrange;
//  end;

  if not gMapElements[aObjID].CanBeRemoved then
  begin
    Include(Result, toaBlockBuild);
    if gMapElements[aObjID].AllBlocked then
    begin
      Include(Result, toaBlockAllExceptBuild);
      aBtn.Hint := aBtn.Hint + ' / ' + gResTexts[TX_MAPED_OBJ_BLOCK_WALK_BUILD_HINT];
      aBtn.CapColor := icRed;
    end
    else
    begin
      aBtn.Hint := aBtn.Hint + ' / ' + gResTexts[TX_MAPED_OBJ_BLOCK_BUILD_HINT];
      if toaBlockDiagonal in Result then
        aBtn.CapColor := icOrange
      else
        aBtn.CapColor := icRoyalYellow;
    end;
  end;
end;


procedure TKMMapEdTerrainObjects.ObjectsPalette_Refresh(Sender: TObject);
var
  I, J, K, leftAdj, {topAdj,} L, obj: Integer;
begin
  leftAdj := (PopUp_ObjectsPalette.Width - fObjPaletteTableSize.X*(OBJ_CELL_W + 1) - 25*Byte(Scroll_ObjectsPalette.Visible)) div 2;
  //topAdj := Image_ObjectsPalette.Top + 60;

  // Make invisible all palette buttons at the end of the list, after shown buttons 'page'
  for I := 0 to fCountCompact - 1 do
  begin
    //ObjectsPaletteTable[I].Visible := False;
    for J := Low(Image_ObjectAttributes) to High(Image_ObjectAttributes) do
      Image_ObjectAttributes[J, I].Hide;
  end;

  Button_ObjPaletteErase.Left := leftAdj;
  Button_ObjPaletteBlock.Left := leftAdj + OBJ_CELL_W + 1;
  DropBox_SortType.Top := Button_ObjPaletteBlock.Top;
  DropBox_SortType.Left := Button_ObjPaletteBlock.Right + 3;

  L := 0;
  K := -1;
  for I := 0 to fCountCompact - 1 do
    begin
      //K := (I + Scroll_ObjectsPalette.Position)*fObjPaletteTableSize.X + J;
      //K := I*fObjPaletteTableSize.X + J;
      Inc(K);
      obj := fCompactToMapElem[K];
      ObjectsPaletteTable[K].Hide;

      case DropBox_SortType.ItemIndex of
        -1,
        0 : ;//everything must be shown
        //only trees
        1: If not KM_ResMapElements.ObjectIsChoppableTree(obj, [caAge1, caAge2, caAge3, caAgeFull]) then
            Continue;
        //only Fruit trees
        2: if gMapElements[obj].IsFruit = 0 then
            Continue;
        //only corn and grapes
        3: if not ( (gMapElements[obj].IsCorn > 0)
                or (gMapElements[obj].IsGrass > 0)
                or (gMapElements[obj].IsVege > 0)
                or (gMapElements[obj].IsWine > 0)
                )
            then
            Continue;
        //other objects
        4: if not ((length(gMapElements[obj].VWares) > 0) or KM_ResMapElements.ObjectIsWare(obj) ) then
          Continue;
        5: if KM_ResMapElements.ObjectIsChoppableTree(obj, [caAge1, caAge2, caAge3, caAgeFull])
              or (gMapElements[obj].IsFruit > 0)
              or (gMapElements[obj].IsCorn > 0)
              or (gMapElements[obj].IsGrass > 0)
              or (gMapElements[obj].IsVege > 0)
              or (gMapElements[obj].IsWine > 0)
              or ((length(gMapElements[obj].VWares) > 0) or KM_ResMapElements.ObjectIsWare(obj) )
            then
              Continue;

      end;

      if K < fCountCompact then
      begin
        ObjectsPaletteTable[K].Left := (L mod fObjPaletteTableSize.X) * (OBJ_CELL_PALETTE_W + 1){ + leftAdj};
        ObjectsPaletteTable[K].Top := 25 + (L div fObjPaletteTableSize.X) *(OBJ_CELL_PALETTE_H + 1){  + topAdj};
        ObjectsPaletteTable[K].TexID := gMapElements[obj].Anim.Step[1] + 1;
        ObjectsPaletteTable[K].Caption := IntToStr(obj);

        UpdatePaletteButton(K);

        ObjectsPaletteTable[K].Visible := True;
      end;
      Inc(L);
    end;
  {for I := 0 to fObjPaletteTableSize.Y - 1 do
    for J := 0 to fObjPaletteTableSize.X - 1 do
    begin
      //K := (I + Scroll_ObjectsPalette.Position)*fObjPaletteTableSize.X + J;
      //K := I*fObjPaletteTableSize.X + J;
      Inc(K);
      obj := fCompactToMapElem[K];
      case DropBox_SortType.ItemIndex of
        -1,
        0 : ;//everything must be shown
        //only trees
        1: If not KM_ResMapElements.ObjectIsChoppableTree(obj) then
            Continue;
        //only Fruit trees
        2: if not gMapElements[obj].IsFruit > 0 then
            Continue;
        //only corn and grapes
        3: if not ( (gMapElements[obj].IsCorn > 0)
                or (gMapElements[obj].IsGrass > 0)
                or (gMapElements[obj].IsVege > 0)
                or (gMapElements[obj].IsWine > 0)
                )
            then
            Continue;
        //other objects
        4: if KM_ResMapElements.ObjectIsChoppableTree(obj)
              or (gMapElements[obj].IsFruit > 0)
              or (gMapElements[obj].IsCorn > 0)
              or (gMapElements[obj].IsGrass > 0)
              or (gMapElements[obj].IsVege > 0)
              or (gMapElements[obj].IsWine > 0)
            then
              Continue;

      end;

      if K < fCountCompact then
      begin
        ObjectsPaletteTable[K].Left := (L mod fObjPaletteTableSize.X) * (OBJ_CELL_PALETTE_W + 1) + leftAdj;
        ObjectsPaletteTable[K].Top := 25 + (L div fObjPaletteTableSize.X) *(OBJ_CELL_PALETTE_H + 1)  + topAdj;
        ObjectsPaletteTable[K].TexID := gMapElements[obj].Anim.Step[1] + 1;
        ObjectsPaletteTable[K].Caption := IntToStr(obj);

        UpdatePaletteButton(K);

        ObjectsPaletteTable[K].Visible := True;
      end;
      Inc(L);
    end;}

  {// Make invisible all palette buttons at the end of the list, after shown buttons 'page'
  for I := K + 1 to fCountCompact - 1 do
  begin
    ObjectsPaletteTable[I].Visible := False;
    for J := Low(Image_ObjectAttributes) to High(Image_ObjectAttributes) do
      Image_ObjectAttributes[J, I].Hide;
  end;
  }
  // Make invisible all palette buttons at the start of the list, before shown buttons 'page'
  {for I := 0 to Scroll_ObjectsPalette.Position - 1 do
    for J := 0 to fObjPaletteTableSize.X - 1 do
    begin
      K := I*fObjPaletteTableSize.X + J;
      if K < fCountCompact then
        ObjectsPaletteTable[K].Visible := False;
    end;}

  // Update palette buttons Down state
  for I := 0 to fCountCompact - 1 do
    ObjectsPaletteTable[I].Down := (gCursor.Mode = cmObjects)
                                and (gCursor.Tag1 <> OBJ_NONE)
                                and (gCursor.Tag1 <> OBJ_BLOCK)
                                and (ObjectsPaletteTable[I].Tag = fMapElemToCompact[gCursor.Tag1]);
end;


procedure TKMMapEdTerrainObjects.ObjPalette_UpdateControlsPosition;
var
  RowsCnt, ColsCnt: Integer;
begin
  PopUp_ObjectsPalette.Top := 25;
  PopUp_ObjectsPalette.Left := 25;
  PopUp_ObjectsPalette.Width := PopUp_ObjectsPalette.MasterParent.Width - 50;
  PopUp_ObjectsPalette.Height := PopUp_ObjectsPalette.MasterParent.Height - 50;

  RowsCnt := Max(1, (PopUp_ObjectsPalette.Height - 80) div (OBJ_CELL_PALETTE_H + 1));
  //Calc cols count without Scroll first
  //ColsCnt := EnsureRange(PopUp_ObjectsPalette.Width div (OBJ_CELL_PALETTE_W + 1), 1, OBJECTS_PALETTE_MAX_COLS_CNT);
  //Scroll_ObjectsPalette.Visible := RowsCnt*ColsCnt < fCountCompact;
  //Recalc ColsCount considering possible scroll width
  ColsCnt := EnsureRange((PopUp_ObjectsPalette.Width - 25*Byte(Scroll_ObjectsPalette.Visible)) div (OBJ_CELL_W + 1), 1, OBJECTS_PALETTE_MAX_COLS_CNT);

  fObjPaletteTableSize := KMPoint(ColsCnt, RowsCnt);

  Image_ObjectsPalette.Width := GetObjPaletteTableWidth + 150;
  Image_ObjectsPalette.Height := GetObjPaletteTableHeight + 220;
  Image_ObjectsPalette.Left := (PopUp_ObjectsPalette.Width - Image_ObjectsPalette.Width) div 2;
  Image_ObjectsPalette.Top := ((PopUp_ObjectsPalette.Height - Image_ObjectsPalette.Height) div 2) - 25;

  Label_ObjectsPalette.Left := PopUp_ObjectsPalette.Center.X - 30;
  Label_ObjectsPalette.Top := Image_ObjectsPalette.Top + 60;

  Button_ClosePalette.Left := PopUp_ObjectsPalette.Center.X - 125;
  Button_ClosePalette.Top := Image_ObjectsPalette.Bottom - 110;

  Button_ObjPaletteErase.Top := Image_ObjectsPalette.Bottom - 110;
  Button_ObjPaletteBlock.Top := Image_ObjectsPalette.Bottom - 110;

  {Scroll_ObjectsPalette.Left := Image_ObjectsPalette.Right - 50;
  Scroll_ObjectsPalette.Top := Image_ObjectsPalette.Top + 80;
  Scroll_ObjectsPalette.Height := Image_ObjectsPalette.Height - 150;

  Scroll_ObjectsPalette.MaxValue := ((fCountCompact - 1) div ColsCnt) + 1 - RowsCnt;}

  ObjectsPalette_Refresh(nil);
end;


//Map sparse objects into a tight lookup array
procedure TKMMapEdTerrainObjects.CompactMapElements;
var
  I: Integer;
begin
  fCountCompact := 0;
  for I := 0 to gRes.MapElements.Count - 1 do
    if (I <> OBJ_BLOCK_TAG) and (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0)
      and (gMapElements[I].Stump = -1) then //Hide falling trees and invisible wall (61)
    begin
      fCompactToMapElem[fCountCompact] := I; //pointer
      fMapElemToCompact[I] := fCountCompact; //Reverse lookup
      Inc(fCountCompact);
    end;
end;


//aObjIndex - Object index which should be visible after update
procedure TKMMapEdTerrainObjects.UpdateObjectsScrollPosToIndex(aObjIndex: Integer);
begin
  // Update Scroll position for objects panel in the menu
  if (aObjIndex >= 0) and not InRange(aObjIndex, ObjectsScroll.Position * 3, ObjectsScroll.Position * 3 + 8) then
    ObjectsScroll.Position := (aObjIndex div 3) - 1; // set scroll so object is in the mid of table
end;


procedure TKMMapEdTerrainObjects.ObjPalette_ClickShift(Sender: TObject; Shift: TShiftState);
var
  objIndex: Integer;
begin
  if ssRight in Shift then
    PopUp_ObjectsPalette.Hide
  else if (ssLeft in Shift) and (Sender is TKMButtonFlat) then
  begin
    PopUp_ObjectsPalette.Hide;
    objIndex := TKMButtonFlat(Sender).Tag;
    ObjectsUpdate(objIndex);

    if (Sender <> Button_ObjPaletteErase)
      and (Sender <> Button_ObjPaletteBlock) then
      UpdateObjectsScrollPosToIndex(objIndex);
  end;
end;


procedure TKMMapEdTerrainObjects.ObjectsBrushChange(Sender: TObject);
var
  treeAgeHintTX: Integer;
  OT: TKMTerrainObjectType;

begin
  gCursor.Mode := cmObjectsBrush;

  ForestAge.ThumbText := gResTexts[FOREST_AGE_THUMB_TX[TKMObjBrushForestAge(ForestAge.Position)]];
  treeAgeHintTX := FOREST_AGE_HINT_TX[TKMObjBrushForestAge(ForestAge.Position)];

  Label_ForestAge.Caption := gResTexts[treeAgeHintTX];

  ForestAge.Hint := GetHintWHotkey(treeAgeHintTX, gResTexts[TX_KEY_SHIFT_MOUSEWHEEL]);

  for OT := Low(TKMTerrainObjectType) to High(TKMTerrainObjectType) do
    if Sender = ObjectTypeSet[OT] then
      gCursor.MapEdObjectsType[OT] := not gCursor.MapEdObjectsType[OT];

  if Sender = CleanBrush then
  begin
    if CleanBrush.Down = False then
    begin
      gCursor.MapEdCleanBrush := True;
      CleanBrush.Down := True;
    end
    else
    begin
      gCursor.MapEdCleanBrush := False;
      CleanBrush.Down := False;
    end;
  end;
  if Sender = BrushCircle then
  begin
    gCursor.MapEdShape := hsCircle;
    BrushCircle.Down := True;
    BrushSquare.Down := False;
  end
  else
  if Sender = BrushSquare then
  begin
    gCursor.MapEdShape := hsSquare;
  end;
  if gCursor.MapEdShape = hsSquare then
  begin
    BrushCircle.Down := False;
    BrushSquare.Down := True;
  end;

  gCursor.MapEdOverrideObjects := OverrideObjects.Checked;
  gCursor.MapEdSize := BrushSize.Position;
  gCursor.MapEdForestAge := ForestAge.Position;
  gCursor.MapEdObjectsDensity := ForestDensity.Position;
end;


procedure TKMMapEdTerrainObjects.ObjectsChange(Sender: TObject);
var
  objIndex: Integer;
begin
  case TKMButtonFlat(Sender).Tag of
    OBJ_BLOCK_TAG,
    OBJ_NONE_TAG:  objIndex := TKMButtonFlat(Sender).Tag; // Block or Erase
    else           objIndex := ObjectsScroll.Position * 3 + TKMButtonFlat(Sender).Tag; //0..n-1
  end;

  ObjectsUpdate(objIndex);

  // Update Objects Palette scroll position
  {if (objIndex <> OBJ_BLOCK_TAG) and (objIndex <> OBJ_NONE_TAG)
    and not InRange(objIndex,
                    Scroll_ObjectsPalette.Position*fObjPaletteTableSize.X,
                    Scroll_ObjectsPalette.Position*fObjPaletteTableSize.X + fObjPaletteTableSize.X*fObjPaletteTableSize.Y - 1) then
    Scroll_ObjectsPalette.Position := ((objIndex - 1) div fObjPaletteTableSize.X);}
end;


procedure TKMMapEdTerrainObjects.ObjectsOverride(Sender: TObject);
begin
  if gCursor.Tag1 = OBJ_BLOCK then
    ObjectsUpdate(OBJ_BLOCK_TAG)
  else
  if gCursor.Tag1 = OBJ_NONE then
    ObjectsUpdate(OBJ_NONE_TAG)
  else
    ObjectsUpdate(fMapElemToCompact[gCursor.Tag1]);
end;

procedure TKMMapEdTerrainObjects.ObjectsUpdate(aObjIndex: Integer);
begin
  //Skip indexes out of range
  if not InRange(aObjIndex, 0, fCountCompact - 1)
    and (aObjIndex <> OBJ_BLOCK_TAG)
    and (aObjIndex <> OBJ_NONE_TAG) then
    Exit;

  gCursor.Mode := cmObjects;
  case aObjIndex of
    OBJ_BLOCK_TAG:  gCursor.Tag1 := OBJ_BLOCK; //Block
    OBJ_NONE_TAG:   gCursor.Tag1 := OBJ_NONE; //Erase
    else gCursor.Tag1 := fCompactToMapElem[aObjIndex];
  end;
  gCursor.MapEdOverrideObjectsSingle := Objects_Override.Checked;

  //Remember last selected object
  fLastObjectIndex := aObjIndex;

  ObjectsRefresh(nil);
end;



procedure TKMMapEdTerrainObjects.ObjectsPaletteButton_Click(Sender: TObject);
begin
  PopUp_ObjectsPalette.Show;
end;


procedure TKMMapEdTerrainObjects.ObjectsPaletteClose_Click(Sender: TObject);
begin
  PopUp_ObjectsPalette.Hide;
end;


procedure TKMMapEdTerrainObjects.ObjectsRefresh(Sender: TObject);
var
  I: Integer;
  objIndex: Integer;
  OT: TKMTerrainObjectType;
begin
  for I := 0 to 8 do
  begin
    objIndex := ObjectsScroll.Position * 3 + I;
    if objIndex < fCountCompact then
    begin
      ObjectsTable[I].TexID := gMapElements[fCompactToMapElem[objIndex]].Anim.Step[1] + 1;
      ObjectsTable[I].Caption := IntToStr(fCompactToMapElem[objIndex]);

      UpdateObjAttributesAndDesc(ObjectsTable[I], fCompactToMapElem[objIndex]);

      ObjectsTable[I].Enable;
    end
    else
    begin
      ObjectsTable[I].TexID := 0;
      ObjectsTable[I].Caption := '';
      ObjectsTable[I].Disable;
    end;
    //Mark the selected one using reverse lookup
    ObjectsTable[I].Down := (gCursor.Mode = cmObjects)
                             and (gCursor.Tag1 <> OBJ_NONE)
                             and (gCursor.Tag1 <> OBJ_BLOCK)
                             and (objIndex = fMapElemToCompact[gCursor.Tag1]);
  end;

  for OT := Low(TKMTerrainObjectType) to High(TKMTerrainObjectType) do
    ObjectTypeSet[OT].Down := gCursor.MapEdObjectsType[OT];

  ObjectErase.Down := (gCursor.Mode = cmObjects) and (gCursor.Tag1 = OBJ_NONE);  //or delete button
  ObjectBlock.Down := (gCursor.Mode = cmObjects) and (gCursor.Tag1 = OBJ_BLOCK); //or block button

end;


procedure TKMMapEdTerrainObjects.Show;
var I : integer;
begin

  for I := 0 to High(Buttons_SwitchPage) do
    Buttons_SwitchPage[I].Show;

  gMain.FormMain.SuppressAltForMenu := True;
  
  case fLastObjectIndex of
    -1:   ; // Do not update Objects if no last object was selected
    OBJ_BLOCK_TAG: ObjectsChange(ObjectBlock);
    OBJ_NONE_TAG:  ObjectsChange(ObjectErase);
    else  begin
            UpdateObjectsScrollPosToIndex(fLastObjectIndex);
            ObjectsChange(ObjectsTable[fLastObjectIndex - ObjectsScroll.Position*3]);
          end;
  end;

  SwitchPage(0);
end;


function TKMMapEdTerrainObjects.Visible: Boolean;
begin
  Result := Panel_Objects.Visible or Panel_WaresOnGround.Visible;
end;


function TKMMapEdTerrainObjects.IsPaletteVisible: Boolean;
begin
  Result := PopUp_ObjectsPalette.Visible;
end;


procedure TKMMapEdTerrainObjects.PaletteHide;
begin
  PopUp_ObjectsPalette.Hide;
end;


procedure TKMMapEdTerrainObjects.Hide;
var I : Integer;
begin
  Panel_Objects.Hide;
  PopUp_ObjectsPalette.Hide;
  Panel_WaresOnGround.Hide;
  gMain.FormMain.SuppressAltForMenu := False;

  for I := 0 to High(Buttons_SwitchPage) do
    Buttons_SwitchPage[I].Hide;
end;


procedure TKMMapEdTerrainObjects.MouseWheel(Shift: TShiftState; WheelSteps, X, Y: Integer; var aHandled: Boolean);
begin
  // Do not use ssCtrl in Shift here, as it can sometimes be wrong values inside Shift (ssShift instead of ssCtrl)
  if aHandled or not Visible then
    Exit;

  if GetKeyState(VK_CONTROL) < 0 then
  begin
    BrushSize.Position := Max(0, BrushSize.Position - WheelSteps); //can't set negative number
    aHandled := True;
  end;

  if GetKeyState(VK_MENU) < 0 then
  begin
    ForestDensity.Position := Max(0, ForestDensity.Position - WheelSteps); //can't set negative number
    aHandled := True;
  end;

  if GetKeyState(VK_SHIFT) < 0 then
  begin
    ForestAge.Position := Max(0, ForestAge.Position - WheelSteps); //can't set negative number
    aHandled := True;
  end;

  if aHandled then
    ObjectsBrushChange(nil);   
end;


procedure TKMMapEdTerrainObjects.KeyDown(Key: Word; Shift: TShiftState; aIsFirst: Boolean; var aHandled: Boolean);
begin
  if aHandled or not aIsFirst then Exit;

  aHandled := Key = gResKeys[kfMapedObjPalette];

  if Key = VK_ESCAPE then
  begin
    if PopUp_ObjectsPalette.Visible then
     begin
       PopUp_ObjectsPalette.Hide;
        aHandled := True;
     end;
  end
  else
  if Key = gResKeys[kfMapedObjPalette] then
  begin
    //Reset selected and hide all pages
    if Assigned(fHideAllPages) and (gMySpectator.Selected <> nil) then
    begin
      gMySpectator.Selected := nil;
      fHideAllPages;
    end;

    PopUp_ObjectsPalette.ToggleVisibility;
  end;
end;


procedure TKMMapEdTerrainObjects.Resize;
begin
  ObjPalette_UpdateControlsPosition;
end;


procedure TKMMapEdTerrainObjects.Cancel_Clicked(var aHandled: Boolean);
begin
  if aHandled then Exit;

  // Reset last object on RMB click
  if gCursor.Mode = cmObjects then
  begin
    fLastObjectIndex := -1;
    aHandled := True;
  end;
end;



procedure TKMMapEdTerrainObjects.UpdateState;
begin
  ObjectsRefresh(nil);
end;

procedure TKMMapEdTerrainObjects.Button_Swith_Click(Sender : TObject);
begin
  SwitchPage(TKMControl(Sender).Tag);
end;

procedure TKMMapEdTerrainObjects.WaresOverride(Sender: TObject);
begin
  gCursor.MapEdOverrideObjects := Wares_Override.Checked;
end;

procedure TKMMapEdTerrainObjects.Wares_Change(Sender: TObject);
var oldTag : Integer;
  I : Integer;
  WT : TKMWareType;
  wasRandom : Boolean;
begin
  oldTag := gCursor.Tag1;
  wasRandom := Scroll_Count.Position = Scroll_Count.MaxValue;

  for I := 0 to High(Button_Wares) do
    if Sender = Button_Wares[I] then
    begin
      gCursor.Tag1 := Button_Wares[I].Tag;
      Break;
    end;

  if oldTag = 0 then
    gCursor.Tag1 := Button_Wares[0].Tag;

  for I := 0 to High(Button_Wares) do
    Button_Wares[I].Down := gCursor.Tag1 = Button_Wares[I].Tag;

  WT := TKMWareType(gCursor.Tag1);

  if oldTag <> gCursor.Tag1 then
    Scroll_Count.MaxValue := length(gRes.Wares[WT].AtTerrainPic) + 1; //max + 1 = random value

  if wasRandom then
    Scroll_Count.Position := Scroll_Count.MaxValue;

  if Scroll_Count.Position = Scroll_Count.MaxValue then
    Scroll_Count.ThumbText := 'Random'
  else
    Scroll_Count.ThumbText := IntToStr(Scroll_Count.Position);
  gCursor.Mode := cmWaresOnGround;
  gCursor.MapEd_WaresCount := Scroll_Count.Position;
  gCursor.MapEdOverrideObjects := Wares_Override.Checked;

end;

procedure TKMMapEdTerrainObjects.SortType_Change(Sender: TObject);
begin
  ObjectsPalette_Refresh(Sender);
end;



constructor TKMMapEdTerrainPatterns.Create(aParent: TKMPanel);
begin
  Inherited Create;

  Panel_PatternSelection := TKMPanel.Create(aParent, 0, 20, aParent.Width, aParent.Height - 28);
  Panel_PatternSelection.Hitable := false;

  TKMLabel.Create(Panel_PatternSelection, 0, 15, aParent.Width, 20, gResTexts[1793], fntOutline, taCenter);

  PopUp_Patterns := TKMPopUpMenu.Create(aParent.MasterPanel, 900);
  PopUp_Patterns.Height := aParent.MasterPanel.Height div 2;
  PopUp_Patterns.Left := (aParent.MasterPanel.Width - PopUp_Patterns.Width) div 2;
  PopUp_Patterns.Top := (aParent.MasterPanel.Height - PopUp_Patterns.Height) div 2;
  PopUp_Patterns.AnchorsCenter;
  TKMBevel.Create(PopUp_Patterns, -2000, -2000, 5000, 5000).OnClick := ObjectsPatternClick;
  TKMImage.Create(PopUp_Patterns, 0, 0, PopUp_Patterns.Width, PopUp_Patterns.Height, 15, rxGuiMain).ImageStretch;

    ColumnBox_CollectionList:= TKMColumnBox.Create(PopUp_Patterns, PopUp_Patterns.Width div 2 - 5, 80, PopUp_Patterns.Width div 4, PopUp_Patterns.Height - 80 - 50, fntMetal, bsGame);
    ColumnBox_CollectionList.SetColumns(fntOutline, ['Collections'], [0]);
    ColumnBox_CollectionList.Anchors := [anLeft,anTop,anBottom];
    ColumnBox_CollectionList.ShowLines := True;
    ColumnBox_CollectionList.ShowHintWhenShort := True;
    ColumnBox_CollectionList.HintBackColor := TKMColor4f.New(57, 48, 50); // Dark grey
    ColumnBox_CollectionList.PassAllKeys := True;
    ColumnBox_CollectionList.OnChange := ObjectsSelectCollection;

    ColumnBox_PatternsList:= TKMColumnBox.Create(PopUp_Patterns, PopUp_Patterns.Width div 2 + PopUp_Patterns.Width div 4, 80, PopUp_Patterns.Width div 4, PopUp_Patterns.Height - 80 - 50, fntMetal, bsGame);
    ColumnBox_PatternsList.SetColumns(fntOutline, [gResTexts[1834]], [0]);
    ColumnBox_PatternsList.Anchors := [anLeft,anTop,anBottom];
    ColumnBox_PatternsList.ShowLines := True;
    ColumnBox_PatternsList.ShowHintWhenShort := True;
    ColumnBox_PatternsList.HintBackColor := TKMColor4f.New(57, 48, 50); // Dark grey
    ColumnBox_PatternsList.PassAllKeys := True;
    ColumnBox_PatternsList.OnChange := ObjectsPatternClick;
    ColumnBox_PatternsList.OnDoubleClick := ObjectsPatternClickDouble;


    Button_SavePatterns := TKMButton.Create(PopUp_Patterns, 9, PopUp_Patterns.Height - 50, 150, 25, gResTexts[1829], bsGame);
    Button_SavePatterns.OnClick := ObjectsPatternClick;
    //Button_SavePatterns.Hide;

    Button_RefreshPatterns:= TKMButton.Create(PopUp_Patterns, ColumnBox_PatternsList.Right - 150, ColumnBox_PatternsList.Bottom + 5, 125, 25, gResTexts[1830], bsGame);
    Button_RefreshPatterns.OnClick := ObjectsPatternClick;

    Button_ReName:= TKMButton.Create(PopUp_Patterns, ColumnBox_CollectionList.Left, ColumnBox_PatternsList.Bottom + 5, 125, 25, gResTexts[1831], bsGame);
    Button_ReName.OnClick := ObjectsPatternClick;

    Button_Delete := TKMButton.Create(PopUp_Patterns, Button_ReName.Right + 1, ColumnBox_PatternsList.Bottom + 5, 125, 25, gResTexts[1832], bsGame);
    Button_Delete.OnClick := ObjectsPatternClick;

    Button_MoveUp := TKMButton.Create(PopUp_Patterns, Button_Delete.Right + 1, Button_Delete.Top, 30, 25, 4, rxGui, bsGame);
    Button_MoveUp.OnClick := MovePattern;
    Button_MoveUp.OnClickHold := MovePatternHold;

    Button_MoveDown := TKMButton.Create(PopUp_Patterns, Button_MoveUp.Right + 1, Button_Delete.Top, 30, 25, 5, rxGui, bsGame);
    Button_MoveDown.OnClick := MovePattern;
    Button_MoveDown.OnClickHold := MovePatternHold;

    Check_Override := TKMCheckBox.Create(PopUp_Patterns, 9, 80, PopUp_Patterns.Width div 2, 25, gResTexts[1833], fntMetal);
    Check_Deselect := TKMCheckBox.Create(PopUp_Patterns, 9, 105, PopUp_Patterns.Width div 2, 25, gResTexts[2030], fntMetal);
    Check_Deselect.Checked := true;

    Scroll_AdditionalHeight := TKMTrackBar.Create(PopUp_Patterns, 9, PopUp_Patterns.Height div 2, 100, 0, 100);
    Scroll_AdditionalHeight.OnChange := ObjectsPatternClick;
    Scroll_AdditionalHeight.Position := 50;
    Scroll_AdditionalHeight.Hide;

  BrushSelectSize := TKMTrackBar.Create(Panel_PatternSelection, 9, 50, (Panel_PatternSelection.Width - (BTN_BRUSH_TYPE_S * 2) - 18) - 18, 1, MAPED_BRUSH_MAX_SIZE);
  BrushSelectSize.Anchors := [anLeft, anTop, anRight];
  BrushSelectSize.Position := 1;
  BrushSelectSize.OnChange := ObjectsPatternClick;
  BrushSelectSize.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT, gResTexts[TX_KEY_CTRL_MOUSEWHEEL]);

  BrushSelectCircle := TKMButtonFlat.Create(Panel_PatternSelection, Panel_PatternSelection.Width - (BTN_BRUSH_TYPE_S * 2) - 18, 50, BTN_BRUSH_TYPE_S, BTN_BRUSH_TYPE_S, 592);
  BrushSelectCircle.Anchors := [anTop, anRight];
  BrushSelectCircle.OnClick := ObjectsPatternClick;
  BrushSelectCircle.TexOffsetX := 1;
  BrushSelectCircle.TexOffsetY := 1;
  BrushSelectCircle.Down := True;

  BrushSelectSquare := TKMButtonFlat.Create(Panel_PatternSelection, Panel_PatternSelection.Width - BTN_BRUSH_TYPE_S - 9, 50, BTN_BRUSH_TYPE_S, BTN_BRUSH_TYPE_S, 593);
  BrushSelectSquare.Anchors := [anTop, anRight];
  BrushSelectSquare.OnClick := ObjectsPatternClick;
  BrushSelectSquare.TexOffsetX := 1;
  BrushSelectSquare.TexOffsetY := 1;

  Brush_Select := TKMButtonFlat.Create(Panel_PatternSelection, 9, 75, 30, 30, 673, rxGui);
  Brush_Select.OnClick := ObjectsPatternClick;
  Brush_Select.HighLightColor := icBlue;
  Brush_Select.Hint := gResTexts[1942];
  Brush_Clean := TKMButtonFlat.Create(Panel_PatternSelection, 42, 75, 30, 30, 673, rxGui);
  Brush_Clean.OnClick := ObjectsPatternClick;
  Brush_Clean.HighLightColor := icRed;
  Brush_Clean.Hint := gResTexts[1943];

  Button_OpenPatternSettings := TKMButton.Create(Panel_PatternSelection, 9, 110, Panel_PatternSelection.Width - 18, 25, gResTexts[1826], bsGame);
  Button_OpenPatternSettings.OnClick := ObjectsPatternClick;

  Button_SetFromPattern := TKMButton.Create(Panel_PatternSelection, 9, 135, Panel_PatternSelection.Width - 18, 25, gResTexts[1827], bsGame);
  Button_SetFromPattern.OnClick := ObjectsPatternClick;

  PopUp_Patterns.Hide;


  TKMLabel.Create(Panel_PatternSelection, 0, Button_SetFromPattern.Bottom + 10, aParent.Width, 20, gResTexts[1941], fntOutline, taCenter);

  Brush_Hide := TKMButtonFlat.Create(Panel_PatternSelection, 9, Button_SetFromPattern.Bottom + 40, 30, 30, 673, rxGui);
  Brush_Hide.OnClick := ObjectsPatternClick;
  Brush_Hide.HighLightColor := icRoyalYellow;
  Brush_Hide.Hint := gResTexts[1944];

  Brush_Show := TKMButtonFlat.Create(Panel_PatternSelection, 42, Button_SetFromPattern.Bottom + 40, 30, 30, 673, rxGui);
  Brush_Show.OnClick := ObjectsPatternClick;
  Brush_Show.HighLightColor := icRed;
  Brush_Show.Hint := gResTexts[1945];


  PopUp_Rename := TKMPopUpMenu.Create(PopUp_Patterns, 400);
  PopUp_Rename.Height := 200;
  // Keep the pop-up centered
  PopUp_Rename.AnchorsCenter;
  PopUp_Rename.Left := ColumnBox_PatternsList.Left - 200;
  PopUp_Rename.Top := ColumnBox_PatternsList.Bottom - 300;
  PopUp_Rename.Hide;

    TKMBevel.Create(PopUp_Rename, -2000, -2000, 5000, 5000);

    Image_Rename := TKMImage.Create(PopUp_Rename, 0, 0, PopUp_Rename.Width, PopUp_Rename.Height, 15, rxGuiMain);
    Image_Rename.ImageStretch;

    Label_RenameTitle := TKMLabel.Create(PopUp_Rename, 20, 50, 360, 30, gResTexts[TX_MENU_MAP_RENAME], fntOutline, taCenter);
    Label_RenameTitle.Anchors := [anLeft,anBottom];

    Label_RenameName := TKMLabel.Create(PopUp_Rename, 25, 100, 60, 20, gResTexts[TX_MENU_REPLAY_RENAME_NAME], fntMetal, taLeft);
    Label_RenameName.Anchors := [anLeft,anBottom];

    FilenameEdit_Rename := TKMFilenameEdit.Create(PopUp_Rename, 105, 97, 275, 20, fntMetal);
    FilenameEdit_Rename.Anchors := [anLeft,anBottom];

    Button_MapRenameConfirm := TKMButton.Create(PopUp_Rename, 20, 155, 170, 30, gResTexts[TX_MENU_REPLAY_RENAME_CONFIRM], bsMenu);
    Button_MapRenameConfirm.Anchors := [anLeft,anBottom];
    Button_MapRenameConfirm.OnClick := ConfirmRename;

    Button_MapRenameCancel := TKMButton.Create(PopUp_Rename, 210, 155, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
    Button_MapRenameCancel.Anchors := [anLeft,anBottom];
    Button_MapRenameCancel.OnClick := ObjectsPatternClick;

  PopUp_Delete := TKMPopUpConfirm.Create(PopUp_Patterns, ColumnBox_PatternsList.Center.X - 200, ColumnBox_PatternsList.Bottom - 300, 400);
  PopUp_Delete.LabelTop.Caption := gResTexts[1835];
  PopUp_Delete.LabelCenter.WordWrap := true;
  PopUp_Delete.LabelCenter.Caption := gResTexts[1836];

  PopUp_Delete.OnConfirm := DeletePatternClick;
  PopUp_Delete.OnCancel := DeletePatternClick;
end;

procedure TKMMapEdTerrainPatterns.ObjectsPatternClick(Sender: TObject);
begin
  if Sender = Button_Delete then
    PopUp_Delete.Show
  else
  if Sender = Button_MapRenameCancel then
  begin
    PopUp_Rename.Hide;
  end else
  if Sender = Button_ReName then
  begin
    ShowRename;
  end else
  if Sender = Button_SavePatterns then
  begin
    gRes.Patterns.Save;
    Exit;
  end else
  if Sender = Button_RefreshPatterns then
  begin
    RefreshCollectionList;
    Exit;
  end;
  if Sender = Button_OpenPatternSettings then
    PopUp_Patterns.Show
  else
  if Sender is TKMBevel then
  begin
    PopUp_Patterns.Hide;
    //Exit;
  end else
  if Sender = Button_SetFromPattern then
    gGame.MapEditor.Selection.SetFromPattern(ColumnBox_CollectionList.ItemIndex,
                                             ColumnBox_PatternsList.ItemIndex,
                                             Check_Override.Checked,
                                             Check_Deselect.Checked)
  else
  if Sender = BrushSelectCircle then
  begin
    gCursor.MapEdShape := hsCircle;
    BrushSelectCircle.Down := True;
    BrushSelectSquare.Down := False;
  end else
  if Sender = BrushSelectSquare then
  begin
    gCursor.MapEdShape := hsSquare;
  end else
  if Sender = Brush_Select then
  begin
    gCursor.Tag1 := 0;
  end else
  if Sender = Brush_Clean then
  begin
    gCursor.Tag1 := 1;
  end else
  if Sender = Brush_Hide then
  begin
    gCursor.Tag1 := 2;
  end else
  if Sender = Brush_Show then
  begin
    gCursor.Tag1 := 3;
  end;


  if gCursor.MapEdShape = hsSquare then
  begin
    BrushSelectCircle.Down := False;
    BrushSelectSquare.Down := True;
  end;

  gCursor.Mode := cmTileSelection;


  Brush_Select.Down := (gCursor.Mode = cmTileSelection) and (gCursor.Tag1 = 0);
  Brush_Clean.Down := (gCursor.Mode = cmTileSelection) and (gCursor.Tag1 = 1);
  Brush_Hide.Down := (gCursor.Mode = cmTileSelection) and (gCursor.Tag1 = 2);
  Brush_Show.Down := (gCursor.Mode = cmTileSelection) and (gCursor.Tag1 = 3);

  gCursor.MapEdPatterns.AddHeight := Scroll_AdditionalHeight.Position;
  //gCursor.MapEdOverrideObjects := OverrideObjects.Checked;
  gCursor.MapEdSize := BrushSelectSize.Position;
  Scroll_AdditionalHeight.ThumbText := IntToStr(Scroll_AdditionalHeight.Position - 50);


  gCursor.MapEdPatterns.AddHeight := Scroll_AdditionalHeight.Position;
end;

procedure TKMMapEdTerrainPatterns.ObjectsPatternClickDouble(Sender: TObject);
begin
  if not ColumnBox_PatternsList.IsSelected then
    Exit;

  gGame.MapEditor.Selection.SetFromPattern(ColumnBox_CollectionList.ItemIndex,
                                            ColumnBox_PatternsList.ItemIndex,
                                            Check_Override.Checked,
                                            Check_Deselect.Checked)
end;

// Check if new name is allowed
procedure TKMMapEdTerrainPatterns.DeletePatternClick(Sender: TObject);
begin
  if Sender = PopUp_Delete.Button_Confirm then
  begin
    If ColumnBox_CollectionList.ItemIndex = 0 then
    begin
      gRes.Patterns.DeletePattern(ColumnBox_PatternsList.ItemIndex);
      ObjectsPatternClick(Button_RefreshPatterns);
    end;
    PopUp_Delete.Hide;

  end else
  if Sender = PopUp_Delete.Button_Cancel then
  begin
    PopUp_Delete.Hide;
  end;

end;

procedure TKMMapEdTerrainPatterns.RefreshCollectionList;
var I, topID, itemID : Integer;
begin
  topID := ColumnBox_CollectionList.TopIndex;
  itemID := ColumnBox_CollectionList.ItemIndex;

  ColumnBox_CollectionList.Clear;

  ColumnBox_CollectionList.AddItem(MakeListRow([gRes.Patterns.Local.GetName], [$FFFFFF00]));
  for I := 0 to gRes.Patterns.Count - 1 do
    ColumnBox_CollectionList.AddItem(MakeListRow([gRes.Patterns[I].GetName]));

  ColumnBox_CollectionList.ItemIndex := itemID;
  ColumnBox_CollectionList.TopIndex := topID;
  RefreshPatternsList;
end;

procedure TKMMapEdTerrainPatterns.RefreshPatternsList;
var I, id, topID, itemID : Integer;
begin
  topID := ColumnBox_PatternsList.TopIndex;
  itemID := ColumnBox_PatternsList.ItemIndex;
  ColumnBox_PatternsList.Clear;
  If not ColumnBox_CollectionList.IsSelected then
    Exit;
  id := ColumnBox_CollectionList.ItemIndex;

  If id = 0 then
  begin
    for I := 0 to gRes.Patterns.Local.Count - 1 do
      ColumnBox_PatternsList.AddItem(MakeListRow([String(gRes.Patterns.Local[I].GetName)]));
  end else
    for I := 0 to gRes.Patterns[id - 1].Count - 1 do
      ColumnBox_PatternsList.AddItem(MakeListRow([String(gRes.Patterns[id - 1].Pattern[I].GetName)]));

  ColumnBox_PatternsList.ItemIndex := itemID;
  ColumnBox_PatternsList.TopIndex := topID;
end;

procedure TKMMapEdTerrainPatterns.ObjectsSelectCollection(Sender: TObject);
begin
  RefreshPatternsList;
end;

procedure TKMMapEdTerrainPatterns.ShowRename;
var K : integer;
begin
  If ColumnBox_CollectionList.ItemIndex <> 0 then
    Exit;
  If not ColumnBox_PatternsList.IsSelected then
    Exit;

  K := ColumnBox_PatternsList.ItemIndex;
  FilenameEdit_Rename.SetTextSilently(String(gRes.Patterns.Local.Pattern[K].GetName));
  PopUp_Rename.Show;

end;

procedure TKMMapEdTerrainPatterns.ConfirmRename(Sender: TObject);
var K : integer;
begin
  If ColumnBox_CollectionList.ItemIndex <> 0 then
    Exit;
  If not ColumnBox_PatternsList.IsSelected then
    Exit;

  K := ColumnBox_PatternsList.ItemIndex;
  gRes.Patterns.Local.Pattern[K].SetName(AnsiString(FilenameEdit_Rename.Text));

  RefreshPatternsList;
  PopUp_Rename.Hide;

end;

procedure TKMMapEdTerrainPatterns.MovePattern(Sender: TObject);
var I, newID: Integer;
begin
  If ColumnBox_CollectionList.ItemIndex <> 0 then
    Exit;
  If not ColumnBox_PatternsList.IsSelected then
    Exit;
  I := ColumnBox_PatternsList.ItemIndex;
  If Sender = Button_MoveUp then
    newID := gRes.Patterns.PatternMoveUp(I)
  else
  If Sender = Button_MoveDown then
    newID := gRes.Patterns.PatternMoveDown(I)
  else
    Exit;

  RefreshPatternsList;
  ColumnBox_PatternsList.ItemIndex := newID;
  ColumnBox_PatternsList.TopIndex := EnsureRange(newID - 8, 0, ColumnBox_PatternsList.RowCount);
end;

procedure TKMMapEdTerrainPatterns.MovePatternHold(Sender: TObject; AButton: TMouseButton; var aHandled: Boolean);
begin
  aHandled := true;
  If AButton = mbLeft then
    MovePattern(Sender);
end;


procedure TKMMapEdTerrainPatterns.Show;
begin
  Panel_PatternSelection.Show;
  ObjectsPatternClick(Button_RefreshPatterns);
end;

procedure TKMMapEdTerrainPatterns.Hide;
begin
  Panel_PatternSelection.Hide;
end;

function TKMMapEdTerrainPatterns.Visible: Boolean;
begin
  Result := Panel_PatternSelection.Visible;
end;

procedure TKMMapEdTerrainPatterns.UpdateState;
begin
  Button_Delete.Enabled := (ColumnBox_CollectionList.ItemIndex = 0) and ColumnBox_PatternsList.IsSelected;
  Button_ReName.Enabled := Button_Delete.Enabled;
  Button_MoveUp.Enabled := Button_Delete.Enabled;
  Button_MoveDown.Enabled := Button_Delete.Enabled;

end;

end.
