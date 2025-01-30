unit KM_TerrainTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults,
  KM_CommonTypes,
  KM_CommonClasses;

type
  //* Tile overlay type
  TKMTileOverlay = (toNone, toDig1, toDig2, toDig3, toDig4, toRoad,
                    toCoal1, toCoal2, toCoal3, toCoal4, toCoal5,
                    toClay1, toClay2, toClay3, toClay4, toClay5,
                    toFence1, toFence2, toFence3, toFence4, toFence5, toFence6, toInfinity, toInfinityClay,
                    toInfinityCoal, toBlock, toBlockBuilding,
                    toDig3Wooden, toDig4Wooden, toDig3Clay, toDig4Clay, toDig3Exclusive, toDig4Exclusive,
                    toStopGrowing, toGold, toIron, toBitin, toCoal);


  // Tile corners
  //  ____
  // |0 1|
  // |3 2|
  //  ---
  TKMTileCorners = array [0..3] of Boolean;

  TKMVertexUsage = (vuNone=0,  //Nobody is on this vertex
                    vuNWSE,    //Vertex is used NW-SE like this: \
                    vuNESW);   //Vertex is used NE-SW like this: /

  TKMFenceKind = (fncNone, fncCorn, fncWine, fncHousePlan, fncHouseFence, fncWoodenBridge, fncStoneBridge, fncGrassLand, fncVegeField,
                    fncAppleTree);

  //Farmers/Woodcutters preferred activity
  TKMPlantAct = (taCut, taPlant, taAny);

  TKMTerrainClimat = (tcNone, tcDry1, tcDry2, tcWarm1, tcWarm2, tcWet1, tcWet2, tcNeutral, tcCold1, tcCold2);
type
  TKMTileChangeType = (tctTerrain, tctRotation, tctHeight, tctObject);

  TKMTileChangeTypeSet = set of TKMTileChangeType;

  TKMTerrainTileChangeError = packed record
    X, Y: Byte;
    ErrorsIn: TKMTileChangeTypeSet;
  end;

  TKMTerrainTileChangeErrorArray = array of TKMTerrainTileChangeError;


  TKMTerrainTileFence = record
    Kind: TKMFenceKind; //Fences (ropes, planks, stones)
    Side: Byte; //Bitfield whether the fences are enabled
  end;

  TKMTerrainWare = record
    W : Byte; //later it's gonna be converted to TKMWareType
    C, C2 : Byte;
  end;

  TKMLandFences = array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of TKMTerrainTileFence;

  TKMTerrainLayer = record
  private
    function GetCorner(aCorner: Byte): Boolean;
    procedure SetCorner(aCorner: Byte; const aValue: Boolean);
  public
    Terrain: Word;
    Rotation: Byte;
    Corners: Byte; //Corners, that this layer 'owns' (corners are distributed between all layers, so any layer can own 1-4 corners)
    property Corner[aCorner: Byte]: Boolean read GetCorner write SetCorner;
    function GetCorners: TKMTileCorners;
    procedure SetCorners(const aCorners: TKMTileCorners); overload;
    procedure SetCorners(const aCorners: TKMByteSet); overload;
    procedure ClearCorners;
    procedure CopyCorners(aLayer: TKMTerrainLayer);
    procedure SwapCorners(var aLayer: TKMTerrainLayer);
    procedure SetAllCOrners;

    procedure Save(aSaveStream: TKMemoryStream);
  end;

  TKMTerrainTileBasic = record
    BaseLayer: TKMTerrainLayer;
    Layer: array [0..2] of TKMTerrainLayer;
    LayersCnt: Byte;
    Height: Byte;
    Obj: Word;
    IsCustom: Boolean;
    BlendingLvl: Byte;
    TileOverlay: TKMTileOverlay;
    TileOverlay2: TKMTileOverlay;
    IsHidden: Boolean;
    RoadType: TKMRoadType;
    GrainType: TKMGrainType;
    Ware : TKMTerrainWare;
  end;
  // Notice fields order, because of record 4-bytes alignment
  TKMTerrainTile = record
  private
    fHeight: Byte;
    procedure SetHeight(aValue: Byte); inline;
  public
    BaseLayer: TKMTerrainLayer;
    Layer: array [0..2] of TKMTerrainLayer;
    LayersCnt: Byte;
    DefRotation: Byte;
    DefTile: Word;
    Obj: Word;
    IsCustom, IsHidden: Boolean; // Custom tile (rotated tile, atm)
    BlendingLvl: Byte; // Use blending for layers transitions

    //Age of tree, another independent variable since trees can grow on fields
    TreeAge: Byte; //Not init=0 .. Full=TreeAgeFull Depending on this tree gets older and thus could be chopped

    //Age of field/wine, another independent variable
    FieldAge: Byte; //Empty=0, 1, 2, 3, 4, Full=CORN_AGE_MAX  Depending on this special object maybe rendered (straw, grapes)

    //Tells us the stage of house construction or workers making a road
    TileLock: TKMTileLock;

    JamMeter: Byte; //How much this tile is jammed with units, pushing each other

    //Used to display half-dug road
    TileOverlay: TKMTileOverlay; //toNone toDig1, toDig2, toDig3, toDig4 + toRoad
    TileOverlay2: TKMTileOverlay; //toNone, toCoal1, toCoal2, toCoal3,toCoal4, toCoal5
    TileSelected : Boolean;//tile was selected for editing

    TileOwner: TKMHandID; //Who owns the tile by having a house/road/field on it
    IsHouse,
    IsUnit: Pointer; //Whenever there's a unit on that tile mark the tile as occupied and count the number
    IsVertexUnit: TKMVertexUsage; //Whether there are units blocking the vertex. (walking diagonally or fighting)

    // Used from Land in runtime for better performance (not proved yet, but anyway),
    // since its loaded to CPU cache at the same time as Height and other terrain properties
    // But no actually need to save it.
    // But we will save it to the stream anyway, since its much faster to save all Land by rows, instead of by separate fields
    Light: Byte; //KaM stores node lighting in 0..32 range (-16..16), but we can use 0..255

    Passability: TKMTerrainPassabilitySet; //Meant to be set of allowed actions on the tile
    WalkConnect: array [TKMWalkConnect] of Byte; //Whole map is painted into interconnected areas
    BridgeType : Byte;
    RoadType: TKMRoadType;
    GrainType: TKMGrainType;
    NightAffection: Single;
    Ware : TKMTerrainWare;
    IsAiReserved : Boolean;

    property Height: Byte read fHeight write SetHeight;

    function HasLayers: Boolean;
    function HasNoLayers: Boolean;
    function GetRenderHeight: Byte; inline;
    procedure SetHeightExact(aValue: Byte);
    procedure IncJamMeter(aValue: Integer);
    function GetRenderLight: Single;
    function GetBasic: TKMTerrainTileBasic;
  end;

  TKMTerrainTileArray = array of TKMTerrainTile;

  //* Brief terrain tile info
  TKMTerrainTileBrief = packed record
    X,Y: Word;
    Terrain: Word;
    Rotation: Byte;
    Height: Byte;
    Obj: Word;
    UpdateTerrain, UpdateRotation, UpdateHeight, UpdateObject: Boolean;
  end;

  TKMTerrainTileBriefArray = array of TKMTerrainTileBrief;

  TKMTerrainTileExt = record
    RenderLight: Single;
    RenderHeight: Byte;
  end;

  TKMLand = array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of TKMTerrainTile;

  PKMLand = ^TKMLand;

  TKMLandExt = array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of TKMTerrainTileExt;
  PKMLandExt = ^TKMLandExt;

  TKMTerrainSelectionPasteType = (ptTerrain, ptHeight, ptObject, ptOverlay);

  TKMTerrainSelectionPasteTypeSet = set of TKMTerrainSelectionPasteType;

  TKMTerrainObjectType = (
    otTrees, otAllButTrees, otFlowers, otMushrooms, otStumps,
    otDeadTrees, otStones, otBushes, otCactus, otRuins);

  function TileOverlayVisibleInGame(aOverlay : TKMTileOverlay) : Boolean;
const
  OBJ_BLOCK = 61;
  OBJ_NONE = 255;
  OBJ_INVISIBLE = 254; //Special object without any attributes set
  HEIGHT_RAND_VALUE = 8;
  //overlays, that considered as road: basically road and dig4, which looks almost like a finished road
  ROAD_LIKE_OVERLAYS: set of TKMTileOverlay = [toDig4, toRoad, toDig3Clay, toDig4Wooden, toDig4Exclusive];
  COAL_LIKE_OVERLAYS: set of TKMTileOverlay = [toCoal1..toCoal5, toInfinityCoal];
  TILE_OVERLAY_2 : set of TKMTileOverlay = [toCoal1..toBlockBuilding, toStopGrowing..toCoal];
  TILE_OVERLAY_ALLOW_TREES: set of TKMTileOverlay = [toNone, toGold, toIron, toBitin, toCoal, toStopGrowing,
                                                    toFence1, toFence2, toFence3, toFence4, toFence5, toFence6, toInfinity, toInfinityClay,
                                                    toInfinityCoal, toBlock, toBlockBuilding];
  TILE_OVERLAY_IDS: array[TKMTileOverlay] of Integer = (0, 249, 251, 253, 255, 254,
                                                          598, 599, 600, 601, 602,//toNone, toDig1, toDig2, toDig3, toDig4, toRoad
                                                          619, 620, 621, 622, 623,
                                                          603, 604, 605, 606, 607, 608, 617, 626, 627,
                                                          679, 680, 681, 682, 683, 684, 685, 686, 689,
                                                          694, 695, 696, 697);

  WINE_TERRAIN_ID = 55;
  CORN_STAGE5_OBJ_ID = 58;
  CORN_STAGE6_OBJ_ID = 59;
  HEIGHT_DEFAULT = 30;
  HEIGHT_MAX = 150;

implementation
uses
  SysUtils, Math,
  KM_CommonUtils, KromUtils, KM_GameParams, KM_GameSettings;


{ TKMTerrainLayer }
procedure TKMTerrainLayer.CopyCorners(aLayer: TKMTerrainLayer);
begin
  Corners := aLayer.Corners;
end;


function TKMTerrainLayer.GetCorner(aCorner: Byte): Boolean;
begin
  case aCorner of
    0:  Result := ToBoolean(Corners and $1); // and 0001
    1:  Result := ToBoolean(Corners and $2); // and 0010
    2:  Result := ToBoolean(Corners and $4); // and 0100
    3:  Result := ToBoolean(Corners and $8); // and 1000
    else raise Exception.Create('Wrong corner id');
  end;
end;


function TKMTerrainLayer.GetCorners: TKMTileCorners;
var
  I: Integer;
begin
  for I := 0 to 3 do
    Result[I] := Corner[I];
end;


procedure TKMTerrainLayer.Save(aSaveStream: TKMemoryStream);
begin
  aSaveStream.Write(Terrain);
  aSaveStream.Write(Rotation);
  aSaveStream.Write(Corners);
end;


procedure TKMTerrainLayer.SetAllCOrners;
begin
  Corners := $F;
end;


procedure TKMTerrainLayer.SetCorner(aCorner: Byte; const aValue: Boolean);
begin
  case aCorner of
    0:  Corners := (Corners and $E) or  Byte(aValue);         // 1110 or aValue
    1:  Corners := (Corners and $D) or (Byte(aValue) shl 1);  // 1101 or aValue
    2:  Corners := (Corners and $B) or (Byte(aValue) shl 2);  // 1011 or aValue
    3:  Corners := (Corners and $7) or (Byte(aValue) shl 3);  // 0111 or aValue
    else raise Exception.Create('Wrong conner id');
  end;
end;


procedure TKMTerrainLayer.SetCorners(const aCorners: TKMTileCorners);
var
  I: Integer;
begin
  for I := 0 to 3 do
    Corner[I] := aCorners[I];
end;


procedure TKMTerrainLayer.SetCorners(const aCorners: TKMByteSet);
var
  I: Integer;
begin
  for I := 0 to 3 do
    Corner[I] := I in aCorners;
end;


procedure TKMTerrainLayer.SwapCorners(var aLayer: TKMTerrainLayer);
begin
  SwapInt(Corners, aLayer.Corners);
end;


procedure TKMTerrainLayer.ClearCorners;
begin
  Corners := 0;
end;


{ TKMTerrainTile }
function TKMTerrainTile.GetBasic: TKMTerrainTileBasic;
var
  L: Integer;
begin
  Result.BaseLayer    := BaseLayer;
  Result.LayersCnt    := LayersCnt;
  Result.Height       := Height;
  Result.Obj          := Obj;
  Result.IsCustom     := IsCustom;
  Result.IsHidden     := IsHidden;
  Result.BlendingLvl  := BlendingLvl;
  Result.TileOverlay  := TileOverlay;
  Result.TileOverlay2  := TileOverlay2;
  Result.Ware         := Ware;
  for L := 0 to 2 do
    Result.Layer[L] := Layer[L];
end;


function TKMTerrainTile.HasLayers: Boolean;
begin
  Result := LayersCnt > 0;
end;


function TKMTerrainTile.HasNoLayers: Boolean;
begin
  Result := LayersCnt = 0;
end;


procedure TKMTerrainTile.IncJamMeter(aValue: Integer);
begin
  JamMeter := EnsureRange(JamMeter + aValue, 0, 255);
end;


function TKMTerrainTile.GetRenderHeight: Byte;
begin
  if mlFlatTerrain in gGameParams.VisibleLayers then
    Result := HEIGHT_DEFAULT
  else
    Result := Height;
end;


procedure TKMTerrainTile.SetHeight(aValue: Byte);
begin
  // Limit height value only in the MapEd
  if gGameParams.IsMapEditor then
    fHeight := EnsureRange(aValue, 0, gGameSettings.MapEdMaxTerrainHeight)
  else
    fHeight := aValue;
end;


// Set height without any limitations
procedure TKMTerrainTile.SetHeightExact(aValue: Byte);
begin
  fHeight := aValue;
end;


// Returns Light in -1..1 range
function TKMTerrainTile.GetRenderLight: Single;
begin
  Result := Light / 127.5 - 1;
end;


function TileOverlayVisibleInGame(aOverlay : TKMTileOverlay) : Boolean;
begin
  case aOverlay of
    toInfinity,
    toBlock,
    toStopGrowing,
    toGold, toIron, toBitin, toCoal,
    toBlockBuilding : Result := false;
    else Result := true;

  end;
end;

end.
