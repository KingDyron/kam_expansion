unit KM_ResTileset;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KromUtils,
//  KM_IoXML,
  KM_Defaults, KM_CommonTypes, KM_CommonClasses, KM_ResTypes, KM_ResTilesetTypes, KM_TerrainTypes;


var
  // Mirror tiles arrays, according to the tiles corners terrain kinds
  // If no mirror tile is found, then self tile is set by default
  // for tiles below 256 we set default tiles (themselfs)
  ResTileset_MirrorTilesH: array{ [0..TILES_CNT-1]} of Integer; // mirror horizontally
  ResTileset_MirrorTilesV: array{ [0..TILES_CNT-1]} of Integer; // mirror vertically

type
  TKMTilesParamsArray = array{ [0..TILES_CNT-1]} of TKMTileParams;


  TKMTerrainOverlayFunction = (tofNone, tofRoadDig, tofRoad, tofInfinity, tofBlock, tofBlockBuilding, tofStopGrowing, tofAllowBuild,
                              tofSnow, tofRoadWalk, tofSand);
  //tofNone = decorative tile overlay

  {TKMTileOverlay = byte;(toNone, toDig1, toDig2, toDig3, toDig4, toRoad,
                    toCoal1, toCoal2, toCoal3, toCoal4, toCoal5,
                    toClay1, toClay2, toClay3, toClay4, toClay5,
                    toFence1, toFence2, toFence3, toFence4, toFence5, toFence6,
                    toDig3Wooden, toDig4Wooden, toDig3Clay, toDig4Clay, toDig3Exclusive, toDig4Exclusive,
                    toGold, toIron, toBitin, toCoal);}
  TKMTerrainOverlayConnection = record
    Tile : Word;
    Rot : Byte;
  end;
  TKMTerrainOverlayParams = record
    TileID, ViewAs : Word;
    FirstLayer, RenderFirst : Boolean;
    CanTree : Boolean;
    Rotate : Boolean;
    Visible, ViewInGame : Boolean;
    W : TKMWareType;
    resCount : Byte;
    funct : TKMTerrainOverlayFunction;
    Hint : Word;
    HasRoadConnection : Boolean;
    RoadConnection : array of TKMTerrainOverlayConnection;
  end;

  TKMTerrainOverlayParamsArray = array of TKMTerrainOverlayParams;
  TKMResTileset = class
  private
    fTiles: TKMTilesParamsArray;
    fOverlays : TKMTerrainOverlayParamsArray;

//    fXML: TKMXmlDocument;
    fCRC: Cardinal;

//    TileTable: array [1 .. 30, 1 .. 30] of packed record
//      Tile1, Tile2, Tile3: Byte;
//      b1, b2, b3, b4, b5, b6, b7: Boolean;
//    end;

    function GetTerKindsCnt(aTile: Word): Integer;

//    function LoadPatternDAT(const FileName: string): Boolean;
//    procedure InitRemakeTiles;

    procedure InitMirrorTiles;

//    function GetTilesXMLPath: string;
    function GetTilesJsonPath: string;

    function GetTileParams(aIndex: Word): TKMTileParams;
    function GetOverlayParams(aIndex: Word): TKMTerrainOverlayParams;
  protected
    // For Batcher
    property Tiles: TKMTilesParamsArray read fTiles write fTiles;
  public
//    PatternDAT: array [1..TILES_CNT] of packed record
//      MinimapColor: Byte;
//      Walkable: Byte;  //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
//      Buildable: Byte; //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
//      u1: Byte; // 1/2/4/8/16 bitfield, seems to have no logical explanation
//      u2: Byte; // 0/1 Boolean? seems to have no logical explanation
//      u3: Byte; // 1/2/4/8 bitfield, seems to have no logical explanation
//    end;

    constructor Create;
    destructor Destroy; override;

    property CRC: Cardinal read fCRC;

//    procedure ExportPatternDat(const aFilename: string);

    property Tile[aIndex: Word]: TKMTileParams read GetTileParams; default;
    property Overlay[aIndex: Word]: TKMTerrainOverlayParams read GetOverlayParams;
    function OverlayCount : Word;
    procedure SetTileColors(var aTileColors: TKMColor3bArray);

    function TileIsWater(aTile: Word): Boolean;
    function TileHasWater(aTile: Word): Boolean;
    function TileIsIce(aTile: Word): Boolean;
    function TileIsSand(aTile: Word): Boolean;
    function TileIsStone(aTile: Word): Word;
    function TileIsSnow(aTile: Word): Boolean;
    function TileIsCoal(aTile: Word): Word;
    function TileIsIron(aTile: Word): Word;
    function TileIsGold(aTile: Word): Word;
    function TileIsSoil(aTile: Word): Boolean;
    function TileIsWalkable(aTile: Word): Boolean;
    function TileIsRoadable(aTile: Word): Boolean;
    function TileIsCornField(aTile: Word): Boolean;
    function TileIsWineField(aTile: Word): Boolean;

    function TileIsGoodForIronMine(aTile: Word): Boolean;
    function TileIsGoodForGoldMine(aTile: Word): Boolean;

    function TileIsCorner(aTile: Word): Boolean;
    function TileIsEdge(aTile: Word): Boolean;

//    procedure SaveToXML;

    procedure SaveToJson(aCompact: Boolean = False; aSaveStream: TKMemoryStream = nil);
    function LoadFromJson : Cardinal;
    Procedure ReloadJSONData(UpdateCRC: Boolean);

    class function TileIsAllowedToSet(aTile: Word): Boolean;
  end;

  TKMTerrainOverlayHelper = record helper for TKMTileOverlay
    function Params : TKMTerrainOverlayParams;
    function IsRoadDir: Boolean;
    function IsRoad: Boolean;
    function IsRoadOrDig: Boolean;

    function IsWare(aWare: TKMWareType) : Boolean;
    function ResCount : byte;
    function AllowTree : Boolean;
    function IsInfinite : Boolean;
    function IsFirstLayer : Boolean;
    function IsSecondLayer : Boolean;
    function StopsGrowing : Boolean;
    function Visible : Boolean;
    function ViewInGame : Boolean;
    function TileID : Word;
    function Rotate : Boolean;
    function BlocksWalking : Boolean;
    function BlocksBuilding : Boolean;
    function AllowsBuilding : Boolean;
    function IsSnow : Boolean;
    function IsRoadWalkable: Boolean;
    function IsSand : Boolean;
  end;

const
  OVERLAY_NONE = 0;
  OVERLAY_DIG_1 = 1;
  OVERLAY_DIG_2 = 2;
  OVERLAY_DIG_3 = 3;
  OVERLAY_DIG_4 = 4;
  OVERLAY_ROAD = 5;
  OVERLAY_DIG_WOOD_3 = 27;
  OVERLAY_DIG_WOOD_4 = 28;
  OVERLAY_DIG_CLAY_3 = 29;
  OVERLAY_DIG_CLAY_4 = 30;
  OVERLAY_DIG_EXCLUSIVE_3 = 31;
  OVERLAY_DIG_EXCLUSIVE_4 = 32;
  UNDERGROUND_GOLD_ID = 34;
  UNDERGROUND_IRON_ID = 35;
  UNDERGROUND_BITIN_ID = 36;
  UNDERGROUND_COAL_ID = 37;
  COAL_OVERLAY_MIN = 6;
  COAL_OVERLAY_MAX = 10;

var
  GuiOverlayOrder : array of Word;

implementation
uses
  TypInfo, Math,
  KM_JsonHelpers, JsonDataObjects,
  KM_FileIO,
  KM_CommonUtils, KM_CommonClassesExt, KM_JsonUtils,
  KM_Resource;

const
//  TILES_XML_PATH = 'data' + PathDelim + 'defines' + PathDelim + 'tiles.xml';
  TILES_JSON_PATH = 'data' + PathDelim + 'defines' + PathDelim + 'tiles.json';

  TILES_NOT_ALLOWED_TO_SET: array [0..13] of Word = (
    55,59,60,61,62,63,              // wine and corn
    //189,169,185,                  // duplicates of 108,109,110
    248,249,250,251,252,253,254,255 // roads and overlays
  );


{ TKMResTileset }
constructor TKMResTileset.Create;
//var
//  I: Integer;
//  crcStream: TKMemoryStream;
begin
  inherited;

  fCRC := LoadFromJson;

  {crcStream := TKMemoryStreamBinary.Create;
  try
    SaveToJson(True, crcStream);
    fCRC := Adler32CRC(crcStream);
  finally
    crcStream.Free;
  end;}

  InitMirrorTiles;

//  LoadPatternDAT(aPatternPath);
//  InitRemakeTiles;

//  for I := 0 to TILES_CNT - 1 do
//  begin
//    fTiles[I] := TKMTileParams.Create;
//    fTiles[I].ID          := I;
//    fTiles[I].Walkable    := TileIsWalkable(I);
//    fTiles[I].Roadable    := TileIsRoadable(I);
//    fTiles[I].Water       := TileIsWater(I);
//    fTiles[I].HasWater    := TileHasWater(I);
//    fTiles[I].Ice         := TileIsIce(I);
//    fTiles[I].Sand        := TileIsSand(I);
//    fTiles[I].Stone       := TileIsStone(I);
//    fTiles[I].Snow        := TileIsSnow(I);
//    fTiles[I].Coal        := TileIsCoal(I);
//    fTiles[I].Gold        := TileIsGold(I);
//    fTiles[I].Iron        := TileIsIron(I);
//    fTiles[I].Soil        := TileIsSoil(I);
//    fTiles[I].Corn        := TileIsCornField(I);
//    fTiles[I].Wine        := TileIsWineField(I);
//    fTiles[I].IronMinable := TileIsGoodForIronMine(I);
//    fTiles[I].GoldMinable := TileIsGoodForGoldMine(I);
//    fTiles[I].TerKinds[0] := TILE_CORNERS_TERRAIN_KINDS[I, 0];
//    fTiles[I].TerKinds[1] := TILE_CORNERS_TERRAIN_KINDS[I, 1];
//    fTiles[I].TerKinds[2] := TILE_CORNERS_TERRAIN_KINDS[I, 2];
//    fTiles[I].TerKinds[3] := TILE_CORNERS_TERRAIN_KINDS[I, 3];
//  end;


//  fXML := TKMXmlDocument.Create;
end;


destructor TKMResTileset.Destroy;
var
  I: Integer;
begin
//  fXML.Free;

  for I := TILES_CNT - 1 downto 0 do
    fTiles[I].Free;

  inherited;
end;


//procedure TKMResTileset.InitRemakeTiles;
//const
//  //ID in png_name
//  WALK_BUILD:     array[0..185] of Integer = (257,262,264,274,275,283,291,299,302,303,304,305,306,313,314,315,316,317,318,319,//20
//                                              320,321,322,338,352,355,362,363,364,365,366,367,368,369,374,375,376,377,378,379,//40
//                                              380,381,384,385,386,387,390,391,392,393,398,399,400,401,402,403,404,405,410,411,//60
//                                              412,413,414,415,416,417,420,421,422,423,426,427,428,429,434,435,436,437,438,439,//80
//                                              440,441,446,447,448,449,450,451,452,453,458,459,460,461,462,463,464,465,470,471,//100
//                                              472,473,474,475,476,477,480,481,482,483,486,487,488,489,494,495,496,497,498,499,//120
//                                              500,501,506,507,508,509,510,511,512,513,518,519,520,521,522,523,524,525,530,531,//140
//                                              532,533,534,535,536,537,540,541,542,543,546,547,548,549,554,555,556,557,558,559,//160
//                                              560,561,566,567,568,569,570,571,572,573,578,579,580,581,582,583,584,585,590,591,//180
//                                              592,593,594,595,596,597);
//  WALK_NO_BUILD:   array[0..106] of Integer = (258,263,269,270,271,272,278,279,280,281,286,287,288,289,294,295,296,297,309,310,//20
//                                              311,312,339,350,351,353,356,358,359,360,361,370,371,372,373,382,383,388,389,394,//40
//                                              395,396,397,406,407,408,409,418,419,424,425,430,431,432,433,442,443,444,445,454,//60
//                                              455,456,457,466,467,468,469,478,479,484,485,490,491,492,493,502,503,504,505,514,//80
//                                              515,516,517,526,527,528,529,538,539,544,545,550,551,552,553,562,563,564,565,574,//100
//                                              575,576,577,586,587,588,589);
//  NO_WALK_NO_BUILD: array[0..44] of Integer = (259,260,261,265,266,267,268,273,276,277,282,284,285,290,292,293,298,300,301,307,//20
//                                              308,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,340,341,342,343,//40
//                                              344,345,346,354,357);
//var
//  I: Integer;
//begin
//  for I := Low(WALK_BUILD) to High(WALK_BUILD) do
//  begin
//    Assert(WALK_BUILD[I] > 255); //We init only new tiles with ID > 255
//    PatternDAT[WALK_BUILD[I]].Walkable := 1;
//    PatternDAT[WALK_BUILD[I]].Buildable := 1;
//  end;
//
//  for I := Low(WALK_NO_BUILD) to High(WALK_NO_BUILD) do
//  begin
//    Assert(WALK_NO_BUILD[I] > 255); //We init only new tiles with ID > 255
//    PatternDAT[WALK_NO_BUILD[I]].Walkable := 1;
//    PatternDAT[WALK_NO_BUILD[I]].Buildable := 0;
//  end;
//
//  for I := Low(NO_WALK_NO_BUILD) to High(NO_WALK_NO_BUILD) do
//  begin
//    Assert(NO_WALK_NO_BUILD[I] > 255); //We init only new tiles with ID > 255
//    PatternDAT[NO_WALK_NO_BUILD[I]].Walkable := 0;
//    PatternDAT[NO_WALK_NO_BUILD[I]].Buildable := 0;
//  end;
//
//  InitMirrorTiles;
//end;


procedure TKMResTileset.InitMirrorTiles;
const
  MIRROR_TILES_INIT_FROM = 256;
var
  I, J, K: Integer;
  skipH, skipV: Boolean;
begin
  // Init MirrorTilesH and MirrorTilesV
  // We can put into const arrays though, if needed for speedup the process
  for I := Low(fTiles) to High(fTiles) do
  begin
    skipH := False;
    skipV := False;

    // 'Self' mirror by default
    ResTileset_MirrorTilesH[I] := I;
    ResTileset_MirrorTilesV[I] := I;

    if I < MIRROR_TILES_INIT_FROM then
      Continue;

    // Skip if we have custom terrain here (maybe we can use it too?)
    for J := 0 to 3 do
      if fTiles[I].TerKinds[J] = tkCustom then
      begin
        skipH := True;
        skipV := True;
        Break;
      end;

    // Check if mirror tile is needed
    if (fTiles[I].TerKinds[0] = fTiles[I].TerKinds[1])
      and (fTiles[I].TerKinds[2] = fTiles[I].TerKinds[3]) then
      skipH := True;

    if (fTiles[I].TerKinds[0] = fTiles[I].TerKinds[3])
      and (fTiles[I].TerKinds[1] = fTiles[I].TerKinds[2]) then
      skipV := True;

    // try to find mirror tiles based on corners terrain kinds
    for K := MIRROR_TILES_INIT_FROM to High(fTiles) do
    begin
      if skipH and skipV then
        Break;

      if not skipH
        and (fTiles[I].TerKinds[0] = fTiles[K].TerKinds[1])
        and (fTiles[I].TerKinds[1] = fTiles[K].TerKinds[0])
        and (fTiles[I].TerKinds[2] = fTiles[K].TerKinds[3])
        and (fTiles[I].TerKinds[3] = fTiles[K].TerKinds[2]) then
      begin
        ResTileset_MirrorTilesH[I] := K;
        skipH := True;
      end;

      if not skipV
        and (fTiles[I].TerKinds[0] = fTiles[K].TerKinds[3])
        and (fTiles[I].TerKinds[3] = fTiles[K].TerKinds[0])
        and (fTiles[I].TerKinds[1] = fTiles[K].TerKinds[2])
        and (fTiles[I].TerKinds[2] = fTiles[K].TerKinds[1]) then
      begin
        ResTileset_MirrorTilesV[I] := K;
        skipV := True;
      end;
    end;
  end;
end;


//Reading pattern data (tile info)
//function TKMResTileset.LoadPatternDAT(const FileName: string): Boolean;
//var
//  I: Integer;
//  f: file;
//  s: Word;
//begin
//  Result := False;
//  if not FileExists(FileName) then
//    Exit;
//  AssignFile(f, FileName);
//  FileMode := fmOpenRead;
//  Reset(f, 1);
//  BlockRead(f, PatternDAT[1], 6 * 256);
//  for I := 1 to 30 do
//  begin
//    BlockRead(f, TileTable[I, 1], 30 * 10);
//    BlockRead(f, s, 1);
//  end;
//
//  CloseFile(f);
//  fCRC := Adler32CRC(FileName);
//
//  if WriteResourceInfoToTXT then
//    ExportPatternDat(ExeDir + 'Export'+PathDelim+'Pattern.csv');
//
//  Result := True;
//end;


procedure TKMResTileset.SetTileColors(var aTileColors: TKMColor3bArray);
var
  I: Integer;
begin
  Assert(Length(aTileColors) = Length(fTiles));

  for I := 0 to High(aTileColors) do
    fTiles[I].MainColor := aTileColors[I];
end;


//procedure TKMResTileset.ExportPatternDat(const aFileName: string);
//var
//  I, K: Integer;
//  ft: TextFile;
//begin
//  AssignFile(ft, ExeDir + 'Pattern.csv');
//  Rewrite(ft);
//  Writeln(ft, 'PatternDAT');
//  for I := 0 to 15 do
//  begin
//    for K := 1 to 16 do
//      write(ft, inttostr(I * 16 + K), ' ', PatternDAT[I * 16 + K].u1, ';');
//    writeln(ft);
//  end;
//  writeln(ft, 'TileTable');
//  for I := 1 to 30 do
//  begin
//    for K := 1 to 30 do
//    begin
//      write(ft, inttostr(TileTable[I, K].Tile1) + '_' + inttostr(TileTable[I, K].Tile2) + '_' +
//        inttostr(TileTable[I, K].Tile3) + ' ');
//      write(ft, inttostr(Word(TileTable[I, K].b1)));
//      write(ft, inttostr(Word(TileTable[I, K].b2)));
//      write(ft, inttostr(Word(TileTable[I, K].b3)));
//      write(ft, inttostr(Word(TileTable[I, K].b4)));
//      write(ft, inttostr(Word(TileTable[I, K].b5)));
//      write(ft, inttostr(Word(TileTable[I, K].b6)));
//      write(ft, inttostr(Word(TileTable[I, K].b7)));
//      write(ft, ';');
//    end;
//
//    writeln(ft);
//  end;
//  closefile(ft);
//end;


function TKMResTileset.GetTerKindsCnt(aTile: Word): Integer;
var
  I: Integer;
  terKinds: TKMTerrainKindSet;
begin
  terKinds := [];

  for I := 0 to 3 do
    Include(terKinds, fTiles[aTile].TerKinds[I]);

  Result := TSet<TKMTerrainKindSet>.Cardinality(terKinds);
end;


function TKMResTileset.TileIsCorner(aTile: Word): Boolean;
const
  CORNERS = [10,15,18,21..23,25,38,49,51..54,56,58,65,66,68..69,71,72,74,78,80,81,83,84,86..87,89,90,92,93,95,96,98,99,
             101,102,104,105,107..108,110..111,113,114,116,118,119,120,122,123,126..127,138,142,143,165,176..193,196,
             202,203,205,213,220,234..241,243,247];
var
  I: Integer;
begin
  if aTile < 256 then
    Result := aTile in CORNERS
  else
  begin
    // Do not consider tile as 'with corner' if he has some custom parts (bridge / castle parts etc)
    for I := 0 to 3 do
      if fTiles[aTile].TerKinds[I] = tkCustom then
        Exit(False);

    Result := GetTerKindsCnt(aTile) = 2;
  end;
end;


function TKMResTileset.TileIsEdge(aTile: Word): Boolean;
const
  EDGES = [4,12,19,39,50,57,64,67,70,73,76,79,82,85,88,91,94,97,
           100,103,106,109,112,115,117,121,124..125,139,141,166..175,194,198..200,
           204,206..212,216..219,223,224..233,242,244];
var
  a,b,c,d: TKMTerrainKind;
begin
  if aTile < 256 then
    Result := aTile in EDGES
  else
  begin
    //  a | b
    //  -----
    //  d | c

    a := fTiles[aTile].TerKinds[0];
    b := fTiles[aTile].TerKinds[1];
    c := fTiles[aTile].TerKinds[2];
    d := fTiles[aTile].TerKinds[3];

    // Do not consider tile as 'with edge' if he has some custom parts (bridge / castle parts etc)
    if   (a = tkCustom) or (b = tkCustom)
      or (c = tkCustom) or (d = tkCustom) then
      Exit(False);

    Result :=  ((a = b) and (c = d))
            or ((a = d) and (b = c));
  end;
end;


// Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed
function TKMResTileset.TileIsWater(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Water;
end;


// Check if requested tile has ice
function TKMResTileset.TileIsIce(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Ice;
end;


// Check if requested tile has any water, including ground-water transitions
function TKMResTileset.TileHasWater(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].HasWater;
end;


// Check if requested tile is sand suitable for crabs
function TKMResTileset.TileIsSand(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Sand;
end;


// Check if requested tile is Stone and returns Stone deposit
function TKMResTileset.TileIsStone(aTile: Word): Word;
begin
  Result := fTiles[aTile].Stone;
end;


// Check if requested tile is snow
function TKMResTileset.TileIsSnow(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Snow;
end;


function TKMResTileset.TileIsCoal(aTile: Word): Word;
begin
  Result := fTiles[aTile].Coal;
end;


function TKMResTileset.TileIsGoodForIronMine(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].IronMinable;
end;


function TKMResTileset.TileIsGoodForGoldMine(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].GoldMinable;
end;


function TKMResTileset.TileIsIron(aTile: Word): Word;
begin
  Result := fTiles[aTile].Iron;
end;


function TKMResTileset.TileIsGold(aTile: Word): Word;
begin
  Result := fTiles[aTile].Gold;
end;


// Check if requested tile is soil suitable for fields and trees
function TKMResTileset.TileIsSoil(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Soil;
end;


// Check if requested tile is generally walkable
function TKMResTileset.TileIsWalkable(aTile: Word): Boolean;
begin
  //Includes 1/2 and 3/4 walkable as walkable
  Result := fTiles[aTile].Walkable;
end;


// Check if requested tile is generally suitable for road building
function TKMResTileset.TileIsRoadable(aTile: Word): Boolean;
begin
  //Do not include 1/2 and 1/4 walkable as roadable
  Result := fTiles[aTile].Roadable;
end;


function TKMResTileset.TileIsCornField(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Corn;
end;


function TKMResTileset.TileIsWineField(aTile: Word): Boolean;
begin
  Result := fTiles[aTile].Wine;
end;


//function TKMResTileset.GetTilesXMLPath: string;
//begin
//  Result := ExeDir + TILES_XML_PATH;
//end;


function TKMResTileset.GetTilesJsonPath: string;
begin
  Result := ExeDir + TILES_JSON_PATH;
end;


function TKMResTileset.GetTileParams(aIndex: Word): TKMTileParams;
begin
  Assert(InRange(aIndex, 0, High(fTiles)));
  Result := fTiles[aIndex];
end;

function TKMResTileset.GetOverlayParams(aIndex: Word): TKMTerrainOverlayParams;
begin
  Result := fOverlays[aIndex];
end;

function TKMResTileset.OverlayCount: Word;
begin
  Result := length(fOverlays);
end;


//procedure TKMResTileset.SaveToXML;
//var
//  nTile: TKMXmlNode;
//
//  procedure AddTileBAttr(aName: string; aValue: Boolean);
//  begin
//    if aValue then
//      nTile.Attributes[aName] := aValue;
//  end;
//
//  procedure AddTileIAttr(aName: string; aValue: Integer);
//  begin
//    if aValue > 0 then
//      nTile.Attributes[aName] := aValue;
//  end;
//
//var
//  I, J, K: Integer;
//  animLayerStr: string;
//  nRoot, nTiles, nAnimLayer: TKMXmlNode;
//begin
//  nRoot := fXML.Root;
//
//  nRoot.Clear;
//
//  nTiles := nRoot.AddChild('Tiles');
//
//  for I := 0 to TILES_CNT - 1 do
//  begin
//    nTile := nTiles.AddChild('Tile');
//    nTile.Attributes['ID']          := I;
//    nTile.Attributes['Walkable']    := fTiles[I].Walkable;
//    nTile.Attributes['Roadable']    := fTiles[I].Roadable;
//
//    AddTileIAttr('Stone', fTiles[I].Stone);
//    AddTileIAttr('Coal',  fTiles[I].Coal);
//    AddTileIAttr('Iron',  fTiles[I].Iron);
//    AddTileIAttr('Gold',  fTiles[I].Gold);
//
//    AddTileBAttr('IronMinable', fTiles[I].IronMinable);
//    AddTileBAttr('GoldMinable', fTiles[I].GoldMinable);
//
//    AddTileBAttr('Water',      fTiles[I].Water);
//    AddTileBAttr('HasWater',   fTiles[I].HasWater);
//    AddTileBAttr('Ice',        fTiles[I].Ice);
//    AddTileBAttr('Sand',       fTiles[I].Sand);
//    AddTileBAttr('Snow',       fTiles[I].Snow);
//    AddTileBAttr('Soil',       fTiles[I].Soil);
//    AddTileBAttr('Corn',       fTiles[I].Corn);
//    AddTileBAttr('Wine',       fTiles[I].Wine);
//
//    nTile.Attributes['CornersTerKinds'] := GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[0])) + ',' +
//                                           GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[1])) + ',' +
//                                           GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[2])) + ',' +
//                                           GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[3]));
//
//    if not fTiles[I].Animation.HasAnim then Continue;
//
//    for J := Low(fTiles[I].Animation.Layers) to High(fTiles[I].Animation.Layers) do
//    begin
//      nAnimLayer := nTile.AddChild('AnimLayer');
//      nAnimLayer.Attributes['Frames'] := fTiles[I].Animation.Layers[J].Frames;
//
//      animLayerStr := '';
//      for K := Low(fTiles[I].Animation.Layers[J].Anims) to High(fTiles[I].Animation.Layers[J].Anims) do
//      begin
//        if animLayerStr <> '' then
//          animLayerStr := animLayerStr + ',';
//
//        animLayerStr := animLayerStr + IntToStr(fTiles[I].Animation.Layers[J].Anims[K]);
//      end;
//
//      nAnimLayer.Attributes['AnimIDs'] := animLayerStr;
//    end;
//  end;
//
//  fXML.SaveToFile(GetTilesXMLPath);
//end;


procedure TKMResTileset.SaveToJson(aCompact: Boolean = False; aSaveStream: TKMemoryStream = nil);
var
  nTile: TKMJson;

  procedure AddTileBAttr(aName: string; aValue: Boolean);
  begin
    if aValue or aCompact then
      nTile.B[aName] := aValue;
  end;

  procedure AddTileIAttr(aName: string; aValue: Integer);
  begin
    if (aValue <> 0) or aCompact then
      nTile.I[aName] := aValue;
  end;

var
  I, J, K: Integer;
  nRoot, nAnimLayer: TKMJson;
  nTiles, nTerKinds, nAnimLayers, nAnims: TKMJsonArray;
begin
  JsonSerializationConfig.InlinedByDefault := True;
  JsonSerializationConfig.IndentChar := '  '; // 2 spaces
  JsonSerializationConfig.LineBreak := #13#10; // CRLF

  nRoot := TJsonObject.Create;
  nRoot.Inlined := False;
  try
    nTiles := nRoot.A['Tiles'];
    nTiles.Inlined := False;
    for I := 0 to TILES_CNT - 1 do
    begin
      nTile := nTiles.AddObject;

//      nTile.FromSimpleObject(fTiles[I], False, True); // Do not use serialization for now, RTTI is heavy and slow

      nTile.I['ID'] := I;
      nTile.B['Walkable'] := fTiles[I].Walkable;
      nTile.B['Roadable'] := fTiles[I].Roadable;
      nTile.B['NotBuildable'] := fTiles[I].NotBuildable;
      AddTileIAttr('Stone', fTiles[I].Stone);
      AddTileIAttr('Coal',  fTiles[I].Coal);
      AddTileIAttr('Iron',  fTiles[I].Iron);
      AddTileIAttr('Gold',  fTiles[I].Gold);
      AddTileIAttr('Bitin',  fTiles[I].Bitin);
      AddTileIAttr('Clay',  fTiles[I].Clay);

      AddTileBAttr('IronMinable', fTiles[I].IronMinable);
      AddTileBAttr('GoldMinable', fTiles[I].GoldMinable);

      AddTileBAttr('Water',    fTiles[I].Water);
      AddTileBAttr('HasWater', fTiles[I].HasWater);
      AddTileBAttr('Ice',      fTiles[I].Ice);
      AddTileBAttr('Snow',     fTiles[I].Snow);
      AddTileBAttr('Sand',     fTiles[I].Sand);
      AddTileBAttr('Soil',     fTiles[I].Soil);
      AddTileBAttr('Corn',     fTiles[I].Corn);
      AddTileBAttr('Wine',     fTiles[I].Wine);
      AddTileBAttr('Grass',     fTiles[I].Grass);
      AddTileBAttr('Vegetables',     fTiles[I].Vegetables);

      nTerKinds := nTile.A['CornersTerKinds'];
      nTerKinds.Add(GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[0])));
      nTerKinds.Add(GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[1])));
      nTerKinds.Add(GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[2])));
      nTerKinds.Add(GetEnumName(TypeInfo(TKMTerrainKind), Integer(fTiles[I].TerKinds[3])));

      if not fTiles[I].Animation.HasAnim then Continue;

      for J := Low(fTiles[I].Animation.Layers) to High(fTiles[I].Animation.Layers) do
      begin
        nAnimLayers := nTile.A['AnimLayers'];
        nAnimLayer := nAnimLayers.AddObject;
        nAnimLayer.I['Frames'] := fTiles[I].Animation.Layers[J].Frames;

        for K := Low(fTiles[I].Animation.Layers[J].Anims) to High(fTiles[I].Animation.Layers[J].Anims) do
        begin
          nAnims := nAnimLayer.A['Anims'];
          nAnims.Add(fTiles[I].Animation.Layers[J].Anims[K]);
        end;
      end;
    end;

    // Save to stream if specified (used for CRC calculations)
    if aSaveStream = nil then
      nRoot.SaveToFile(GetTilesJsonPath, aCompact, TEncoding.UTF8)
    else
      nRoot.SaveToStream(aSaveStream, aCompact, TEncoding.UTF8);
  finally
    nRoot.Free;
  end;
end;


function TKMResTileset.LoadFromJson : Cardinal;
var
  I, J, K, S: Integer;
  jsonPath: string;
  nTile, nRoot, nAnimLayer: TJsonObject;
  nTiles, nTerKinds, nAnimLayers, nAnims: TJsonArray;
  terKind: TKMTerrainKind;
begin
  jsonPath := GetTilesJsonPath;
  nRoot := TJsonObject.ParseFromFile(jsonPath) as TJsonObject;
  Result := GetJSONCRC(nRoot);
  try
    nTiles := nRoot.A['Tiles'];


    TILES_CNT := nTiles.Count;
    MAX_TILE_TO_SHOW := TILES_CNT;
    SetLength(ResTileset_MirrorTilesH, TILES_CNT);
    SetLength(ResTileset_MirrorTilesV, TILES_CNT);
    SetLength(fTiles, TILES_CNT);
    //Assert(nTiles.Count = TILES_CNT);

    for I := 0 to nTiles.Count - 1 do
    begin
      nTile := nTiles.O[I];

      FreeAndNil(fTiles[I]);
      fTiles[I] := TKMTileParams.Create;

  //    nTile.ToSimpleObject(tile, False); // Do not use serialization for now, RTTI is heavy and slow

      fTiles[I].ID := nTile.I['ID'];
      fTiles[I].Walkable := nTile.B['Walkable'];
      fTiles[I].Roadable := nTile.B['Roadable'];
      fTiles[I].NotBuildable := nTile.B['NotBuildable'];

      fTiles[I].Stone := nTile.I['Stone'];
      fTiles[I].Coal  := nTile.I['Coal'];
      fTiles[I].Iron  := nTile.I['Iron'];
      fTiles[I].Gold  := nTile.I['Gold'];
      fTiles[I].Bitin  := nTile.I['Bitin'];
      fTiles[I].Clay  := nTile.I['Clay'];

      fTiles[I].IronMinable := nTile.B['IronMinable'];
      fTiles[I].GoldMinable := nTile.B['GoldMinable'];
      fTiles[I].CoalMinable := nTile.B['CoalMinable'];
      fTiles[I].MineShaft := nTile.B['MineShaft'];

      fTiles[I].Water    := nTile.B['Water'];
      fTiles[I].HasWater := nTile.B['HasWater'];
      fTiles[I].Ice      := nTile.B['Ice'];
      fTiles[I].Snow     := nTile.B['Snow'];
      fTiles[I].Sand     := nTile.B['Sand'];
      fTiles[I].Soil     := nTile.B['Soil'];
      fTiles[I].Corn     := nTile.B['Corn'];
      fTiles[I].Wine     := nTile.B['Wine'];
      fTiles[I].Grass    := nTile.B['Grass'];
      fTiles[I].Vegetables:= nTile.B['Vegetables'];

      nTerKinds := nTile.A['CornersTerKinds'];
      Assert(nTerKinds.Count = 4);
      for K := 0 to 3 do
        if TKMEnumUtils.TryGetAs<TKMTerrainKind>(nTerKinds.S[K], terKind) then
          fTiles[I].TerKinds[K] := terKind
        else
          raise Exception.Create('Error loading ' + jsonPath + ': wrong CornersTerKind: ' + nTerKinds.S[K]);

      if not nTile.Contains('AnimLayers') then Continue;

      nAnimLayers := nTile.A['AnimLayers'];

      SetLength(fTiles[I].Animation.Layers, nAnimLayers.Count);

      for J := 0 to nAnimLayers.Count - 1 do
      begin
        nAnimLayer := nAnimLayers.O[J];

        fTiles[I].Animation.Layers[J].Frames := nAnimLayer.I['Frames'];

        If nAnimLayer.Contains('Start') and nAnimLayer.Contains('Count') then
        begin
          SetLength(fTiles[I].Animation.Layers[J].Anims, nAnimLayer.I['Count']);
          S := nAnimLayer.I['Start'];
          for K := 0 to nAnimLayer.I['Count'] - 1 do
            fTiles[I].Animation.Layers[J].Anims[K] := S + K;
        end else
        begin
          nAnims := nAnimLayer.A['Anims'];
          SetLength(fTiles[I].Animation.Layers[J].Anims, nAnims.Count);
          for K := 0 to nAnims.Count - 1 do
          begin
            fTiles[I].Animation.Layers[J].Anims[K] := nAnims[K];
          end;
        end;
      end;
    end;

    nTiles := nRoot.A['Overlays'];
    SetLength(fOverlays, Min(nTiles.Count, 255));
    for I := 0 to high(fOverlays) do
      with fOverlays[I] do
      begin
        nTile := nTiles.O[I];
        TileID := nTile.I['TileID'];
        ViewAs := nTile.I['ViewAs'];
        FirstLayer := nTile.B['IsFirstLayer'];
        RenderFirst := nTile.B['RenderFirst'];
        Visible := nTile.B['Visible'];
        ViewInGame := nTile.B['ViewInGame'];
        CanTree := nTile.B['CanTree'];
        Rotate := nTile.B['Rotate'];
        Hint := nTile.I['Hint'];
        W := TKMWareType(nTile.I['Ware']);
        resCount := nTile.I['WareCount'];
        funct := TKMTerrainOverlayFunction(nTile.I['Function']);


        nTerKinds := nTile.A['Connection'];//road connection ID
        nAnimLayers := nTile.A['ConnectionRot'];//road connection rotation
        If (nTerKinds.Count = 16) and (nAnimLayers.Count = 16) then
        begin
          SetLength(RoadConnection, 16);
          for K := 0 to 15 do
          begin
            RoadConnection[K].Tile := nTerKinds.I[K];
            RoadConnection[K].Rot := nAnimLayers.I[K];
          end;

          HasRoadConnection := true;
        end else
          HasRoadConnection := false;
      end;
    nTiles := nRoot.A['OverlayGuiOrder'];
    SetLength(GuiOverlayOrder, nTiles.Count);
    for I := 0 to nTiles.Count - 1 do
      GuiOverlayOrder[I] := nTiles.I[I];

  finally
    nRoot.Free;
  end;
end;

procedure TKMResTileset.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin
  oldCRC := fCRC;
  fCRC := fCRC xor LoadFromJSON;
  if not UpdateCRC then
    fCRC := oldCRC;
  InitMirrorTiles;
end;


class function TKMResTileset.TileIsAllowedToSet(aTile: Word): Boolean;
begin
  Result := not ArrayContains(aTile, TILES_NOT_ALLOWED_TO_SET);
end;

function TKMTerrainOverlayHelper.Params: TKMTerrainOverlayParams;
begin
  Result := gRes.Tileset.Overlay[Word(self)];
end;

function TKMTerrainOverlayHelper.IsRoadDir: Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofRoadDig;
end;

function TKMTerrainOverlayHelper.IsRoad: Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofRoad;
end;
function TKMTerrainOverlayHelper.IsRoadOrDig: Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct in [tofRoad, tofRoadDig];
end;

function TKMTerrainOverlayHelper.IsWare(aWare : TKMWareType) : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].W = aWare;
end;
function TKMTerrainOverlayHelper.ResCount : Byte;
begin
  Result := gRes.Tileset.Overlay[Word(self)].resCount;
end;
function TKMTerrainOverlayHelper.AllowTree : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].CanTree;
end;
function TKMTerrainOverlayHelper.IsInfinite : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofInfinity;
end;
function TKMTerrainOverlayHelper.IsFirstLayer : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].FirstLayer;
end;
function TKMTerrainOverlayHelper.IsSecondLayer : Boolean;
begin
  Result := not gRes.Tileset.Overlay[Word(self)].FirstLayer;
  //Result := not (gRes.Tileset.Overlay[Word(self)].funct in [tofRoadDig, tofRoad])
end;
function TKMTerrainOverlayHelper.StopsGrowing : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofStopGrowing;
end;

function TKMTerrainOverlayHelper.ViewInGame : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].ViewInGame;
end;
function TKMTerrainOverlayHelper.Visible : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].Visible;
end;
function TKMTerrainOverlayHelper.TileID : Word;
begin
  Result := gRes.Tileset.Overlay[Word(self)].TileID;
end;
function TKMTerrainOverlayHelper.Rotate : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].Rotate;
end;
function TKMTerrainOverlayHelper.BlocksWalking : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofBlock;
end;
function TKMTerrainOverlayHelper.BlocksBuilding : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofBlockBuilding;
end;
function TKMTerrainOverlayHelper.AllowsBuilding : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofAllowBuild;
end;
function TKMTerrainOverlayHelper.IsSnow : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofSnow;
end;
function TKMTerrainOverlayHelper.IsRoadWalkable : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofRoadWalk;
end;
function TKMTerrainOverlayHelper.IsSand : Boolean;
begin
  Result := gRes.Tileset.Overlay[Word(self)].funct = tofSand;
end;

end.
