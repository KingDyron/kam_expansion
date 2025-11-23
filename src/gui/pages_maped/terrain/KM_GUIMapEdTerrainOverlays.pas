unit KM_GUIMapEdTerrainOverlays;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_InterfaceDefaults,
   KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsTrackBar,
   KM_Defaults, KM_Pics, KM_TerrainTypes;

type
  TKMMapEdTerrainOverlays = class(TKMMapEdSubMenuPage)
  private
    fLastOverlay: Word;

    procedure OverlayChange(Sender: TObject);
    procedure OverlaySet(aIndex: Integer);
    procedure OverlayRefresh(Sender: TObject);
  protected
    Panel_Overlays: TKMPanel;
    CheckBox_Override : TKMCheckBox;
    CheckBox_ApplyOnRoad : TKMCheckBox;
    OverlaysTable: array of TKMButtonFlat;

    BrushSize: TKMTrackBar;
    BrushCircle: TKMButtonFlat;
    BrushSquare: TKMButtonFlat;

  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    procedure UpdateState;
    function Visible: Boolean; override;
  end;


implementation
uses
  TypInfo,
  KM_ResFonts, KM_ResTexts, KM_ResTypes, KM_ResTileset, KM_Resource,
  KM_Cursor, KM_RenderUI, KM_InterfaceGame,
  KM_Utils;


constructor TKMMapEdTerrainOverlays.Create(aParent: TKMPanel);
const
  BTN_SIZE = 34;
  BTNS_PER_ROW = 6;

  {OVERLAY_HINTS_TX: array [TKMTileOverlay] of Integer =
                            (TX_MAPED_TERRAIN_OVERLAY_TO_NONE, TX_MAPED_TERRAIN_OVERLAY_TO_DIG1,
                             TX_MAPED_TERRAIN_OVERLAY_TO_DIG2, TX_MAPED_TERRAIN_OVERLAY_TO_DIG3,
                             TX_MAPED_TERRAIN_OVERLAY_TO_DIG4, TX_MAPED_TERRAIN_OVERLAY_TO_ROAD,

                             1694, 1695,
                             1696, 1697,
                             1698,//coal
                             1699,
                             1700, 1701,
                             1702, 1703,//clay

                             1704, 1705,//decorations
                             1706, 1707,//decorations
                             1708, 1709,//decorations
                             1710, 1711,//infinity
                             1712, //infinity
                             1395, 1394, //block
                             0, 0, 0, 0, 0, 0,
                             2110, 2121, 2122, 2123, 2132);
  {TKMTileOverlay = (toNone, toDig1, toDig2, toDig3, toDig4, toRoad, toCoal1, toCoal2, toCoal3, toCoal4, toCoal5,
                    toClay1, toClay2, toClay3, toClay4, toClay5,
                    toFence1, toFence2, toFence3, toFence4, toFence5, toFence6, toInfinity);}
var
  TTO: Word;
  top, I, J, C: Integer;
begin
  inherited Create;

  Panel_Overlays := TKMPanel.Create(aParent, 0, 28, aParent.Width, 400);
  with TKMLabel.Create(Panel_Overlays, 0, TERRAIN_PAGE_TITLE_Y, Panel_Overlays.Width, 0, gResTexts[TX_MAPED_TERRAIN_OVERLAYS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];

  CheckBox_Override := TKMCheckBox.Create(Panel_Overlays, 9, 30, Panel_Overlays.Width, 15, gResTexts[2138], fntMetal);
  CheckBox_Override.OnClick := OverlayChange;
  CheckBox_Override.Check;

  CheckBox_ApplyOnRoad := TKMCheckBox.Create(Panel_Overlays, 9, 50, Panel_Overlays.Width, 15, gResTexts[2295], fntMetal);
  CheckBox_ApplyOnRoad.OnClick := OverlayChange;
  CheckBox_ApplyOnRoad.UnCheck;
  top := CheckBox_ApplyOnRoad.Bottom;
  BrushSize   := TKMTrackBar.Create(Panel_Overlays, 9, top, (Panel_Overlays.Width - (24 * 2) - 18) - 18, 1, 7);
  BrushSize.Anchors := [anLeft, anTop, anRight];
  BrushSize.Hint := GetHintWHotkey(TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT, gResTexts[TX_KEY_CTRL_MOUSEWHEEL]);
  BrushSize.Position := 1;
  BrushSize.OnChange := OverlayChange;

  BrushCircle := TKMButtonFlat.Create(Panel_Overlays, Panel_Overlays.Width - (24 * 2) - 18,
                                                     top, 24, 24, 592);
  BrushCircle.Anchors := [anTop, anRight];
  BrushCircle.OnClick := OverlayChange;
  BrushCircle.TexOffsetX := 1;
  BrushCircle.TexOffsetY := 1;

  BrushSquare := TKMButtonFlat.Create(Panel_Overlays, Panel_Overlays.Width - 24 - 9, top, 24, 24, 593);
  BrushSquare.Anchors := [anTop, anRight];
  BrushSquare.OnClick := OverlayChange;
  BrushSquare.TexOffsetX := 1;
  BrushSquare.TexOffsetY := 1;

  top := BrushSize.Bottom + 5;

  {for TTO := Low(OverlaysTable) to High(OverlaysTable) do
  begin
    OverlaysTable[TTO] := TKMButtonFlat.Create(Panel_Overlays, 9 + (Byte(TTO) mod BTNS_PER_ROW) * BTN_SIZE,
                                                             BTN_SIZE + (Byte(TTO) div BTNS_PER_ROW) * BTN_SIZE,
                                                             BTN_SIZE,
                                                             BTN_SIZE,
                                                             IfThen(TILE_OVERLAY_IDS[TTO] > 0, TILE_OVERLAY_IDS[TTO] + 1, 0),
                                                             rxTiles);
    OverlaysTable[TTO].Tag := TILE_OVERLAY_IDS[TTO];
//    OverlaysTable[J].Caption := IntToStr(OverlaysTable[J].Tag);
//    OverlaysTable[J].CapOffsetY := -8;
//    OverlaysTable[J].TexOffsetY := 6;
//    OverlaysTable[J].CapColor := icYellow;
    OverlaysTable[TTO].Hint := gResTexts[OVERLAY_HINTS_TX[TTO]];
    OverlaysTable[TTO].OnClick := OverlayChange;
  end;}
  //top := BTN_SIZE + 40;
  J := 0;
  C := 0;
  for I := 0 to High(GuiOverlayOrder) do
  begin
    TTO := GuiOverlayOrder[I];
    if (I > 0) and (TTO = 0) then
    begin
      C := 0;
      top := OverlaysTable[J - 1].Bottom + 2;
      Continue;
    end;
    SetLength(OverlaysTable, J + 1);
    OverlaysTable[J] := TKMButtonFlat.Create(Panel_Overlays, 9 + (C mod BTNS_PER_ROW) * BTN_SIZE,
                                                             top + (C div BTNS_PER_ROW) * BTN_SIZE,
                                                             BTN_SIZE,
                                                             BTN_SIZE,
                                                             IfThen(gRes.Tileset.Overlay[TTO].TileID > 0, gRes.Tileset.Overlay[TTO].TileID + 1, 0),
                                                             rxTiles);
    OverlaysTable[J].Tag := TTO;
    if gRes.Tileset.Overlay[TTO].Hint > 0 then
      OverlaysTable[J].Hint := gResTexts[gRes.Tileset.Overlay[TTO].Hint];
    OverlaysTable[J].OnClick := OverlayChange;

    if gRes.Tileset.Overlay[TTO].HasRoadConnection then
      OverlaysTable[J].BackBevelColor := $55FF0000;

    inc(C);
    inc(J);
  end;

  //OverlaysTable[toInfinityClay].Tag := -1;
  //OverlaysTable[toInfinityClay].TexID := 41;
  //OverlaysTable[toInfinityClay].RX := rxGui;
  //OverlaysTable[toInfinityClay].Disable;
end;


procedure TKMMapEdTerrainOverlays.OverlayChange(Sender: TObject);
var
  I : Integer;
begin
  gCursor.MapEdOverrideCustomTiles := CheckBox_Override.Checked;
  gCursor.MapEdApplyOverlayOnRoad := CheckBox_ApplyOnRoad.Checked;

  gCursor.MapEdSize := BrushSize.Position;
  If BrushSquare.Down then
    gCursor.MapEdShape := hsSquare
  else
    gCursor.MapEdShape := hsCircle;
  BrushCircle.Down := gCursor.MapEdShape = hsCircle;
  BrushSquare.Down := gCursor.MapEdShape = hsSquare;

  for I := Low(OverlaysTable) to High(OverlaysTable) do
    if Sender = OverlaysTable[I] then
    begin
      gCursor.Mode := cmOverlays;
      gCursor.Tag1 := OverlaysTable[I].Tag;
      gCursor.MapEdOverlayOnRoad := OverlaysTable[I].Tag;
    end;

end;


procedure TKMMapEdTerrainOverlays.OverlaySet(aIndex: Integer);
begin
  if aIndex > 0 then
  begin
    gCursor.Mode := cmOverlays;
    gCursor.Tag1 := aIndex;
    fLastOverlay := aIndex;
    gCursor.MapEdOverrideCustomTiles := CheckBox_Override.Checked;
  end;

  OverlayRefresh(nil);
end;


procedure TKMMapEdTerrainOverlays.OverlayRefresh(Sender: TObject);
var
  I : Integer;
begin
  for I := Low(OverlaysTable) to High(OverlaysTable) do
    OverlaysTable[I].Down := (gCursor.Mode = cmOverlays) and (gCursor.Tag1 = OverlaysTable[I].Tag);
end;


procedure TKMMapEdTerrainOverlays.Show;
begin
  OverlaySet(fLastOverlay);
  gCursor.MapEdDir := 0;
  gCursor.MapEdSize :=  BrushSize.Position;
  Panel_Overlays.Show;
end;


function TKMMapEdTerrainOverlays.Visible: Boolean;
begin
  Result := Panel_Overlays.Visible;
end;


procedure TKMMapEdTerrainOverlays.Hide;
begin
  Panel_Overlays.Hide;
end;


procedure TKMMapEdTerrainOverlays.UpdateState;
begin
  OverlayRefresh(nil);
end;


end.
