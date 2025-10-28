

unit KM_TerrainPainter;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_TerrainTypes, KM_ResTilesetTypes, KM_CommonTypes, KM_MapEdTypes;

type
  //Terrain helper that is used to paint terrain types in Map Editor
  TKMTerrainPainter = class
  private
    fLandTerKind: TKMLandTerKind;
    fTempTerKind : array[1..MAX_MAP_SIZE] of array[1..MAX_MAP_SIZE] of TKMTerrainKind;
    // Temp data, do not saved
    fUseTempLand: Boolean;
    fReplaceLayers: Boolean;
    fBrushAreaTerKindCnt: Integer;
    fBrushAreaTerKind: array of TKMPoint;
    fTempLand: array of array of TKMTerrainTileBasic;
    fLastPosition: TKMPoint;

    // parameters to be used by painter
    // common
    fSize: Integer;
    fMapXn, fMapYn: Integer; //Cursor position node
    fMapXc, fMapYc: Integer; //Cursor position cell
    fShape: TKMMapEdShape;

    // while applying brushes
    fTerKind: TKMTerrainKind;
    fBrushMask: TKMTileMaskKind;
    fUseMagicBrush: Boolean;
    fRandomizeTiling: Boolean;
    fOverrideCustomTiles: Boolean;
    fBlendingLvl: Byte;

    // while applying elevation
    fIsEqualize: Boolean;
    fRaise: Boolean;
    fSlope: Byte;
    fSpeed: Byte;

    function BrushAreaTerKindContains(aCell: TKMPoint): Boolean;
    function GetVertexCornerTerKinds(X,Y: Word; var aCornersTerKinds: TKMTerrainKindCorners): Integer;
    procedure GetTileOwnCornersTKinds(const aCell: TKMPoint; var aCornersTerKinds: TKMTerrainKindCorners);
    procedure GetTileLandNodeTKinds(const aCell: TKMPoint; var aCornersTerKinds: TKMTerrainKindCorners);
    procedure GetTileCornersTKinds(const aCell: TKMPoint; out aCornersTerKinds: TKMTerrainKindCorners;
                                   aGetOnlyTileCornersTK: Boolean = False; aGetOnlyLandNodeTK: Boolean = False);
    procedure BrushTile(const X, Y: Integer);
    procedure BrushTerrainTile(const X, Y: Integer; aTerKind: TKMTerrainKind);
    procedure BrushObjects(const X, Y: Integer; aUseLandTKind: Boolean = True; aTerKind: TKMTerrainKind = tkCustom);
    procedure ApplyBrushSelection(const X, Y: Integer);
    procedure ApplyBrushObjects(const X, Y: Integer);
    procedure ApplyBrushObjectsWTerKind(const X, Y: Integer);
    procedure MagicBrush(const X,Y: Integer); overload;
    procedure MagicBrush(const X,Y: Integer; aMaskKind: TKMTileMaskKind); overload;
    procedure UseMagicBrush(X,Y,aSize: Integer; aSquare: Boolean; aAroundTiles: Boolean = False);
    procedure UpdateTempLand;
    procedure SetMapEdParams;
    procedure SetCommonParams(X, Y: Single; aShape: TKMMapEdShape);
    procedure SetHeightSpecialParams(aIsEqualize, aRaise: Boolean; aSize, aSlope, aSpeed: Integer);
    procedure SetBrushSpecialParams(aSize: Integer; aTerKind: TKMTerrainKind; aRandomTiles, aOverrideCustomTiles: Boolean;
                                    aBrushMask: TKMTileMaskKind; aBlendingLvl: Integer; aUseMagicBrush: Boolean);

    procedure DoApplyBrush;
    procedure DoApplyTileBrush(const X, Y: Integer);
    procedure EditTile(const aLoc: TKMPoint; aTile: Word; aRotation: Byte; aIsCustom: Boolean = True);
    procedure GenerateAddnData;
    procedure InitSize(X,Y: Word);
    function GetTerKind(aTKValue: Integer; aUseKamFormat: Boolean): TKMTerrainKind;

    function IsTerrainRepresentTerKind(aTerId: Word; aTerKind: TKMTerrainKind): Boolean;

    procedure RebuildTile(const X,Y: Integer); overload;
    procedure RebuildTile(const X,Y: Integer; aRandomTiles: Boolean); overload;
    function TryGetVertexEffectiveTerKind(X, Y: Word; var aEffectiveTKind: TKMTerrainKind): Boolean;
  public
    LandTerKind: TKMLandTerKind;

    property MainLandTerKind: TKMLandTerKind read fLandTerKind; // readonly

    procedure SetMainLandTerKind;

    procedure InitEmpty;

    procedure LoadFromFile(const aFileName: UnicodeString);
    procedure SaveToFile(const aFileName: UnicodeString); overload;
    procedure SaveToFile(const aFileName: UnicodeString; const aInsetRect: TKMRect); overload;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure Eyedropper(const aLoc: TKMPoint);
    procedure RotateTile(const aLoc: TKMPoint);
    procedure MagicWater(const aLoc: TKMPoint);
    function CanEditTile(const X,Y: Integer) : Boolean;

    procedure RMG2MapEditor(X,Y: Integer; aTile: Word);

    procedure SetBrushParams(X, Y: Single; aShape: TKMMapEdShape; aSize: Integer;
                             aTerKind: TKMTerrainKind; aRandomTiles, aOverrideCustomTiles: Boolean;
                             aBrushMask: TKMTileMaskKind = mkNone; aBlendingLvl: Integer = TERRAIN_DEF_BLENDING_LVL;
                             aUseMagicBrush: Boolean = False);
    procedure SetHeightParams(X, Y: Single; aShape: TKMMapEdShape; aSize: Integer;
                              aIsEqualize, aRaise: Boolean; aSlope, aSpeed: Byte);

    procedure ApplyBrush;
    procedure ApplyHeight;
    procedure ApplyObjectsBrush;
    procedure ApplySelectionBrush;
    procedure ApplyConstHeight;
    procedure ApplyElevateKind(aTerKind: TKMTerrainKind);
    procedure ApplyTileBrush;

    procedure RebuildMap; overload;
    procedure RebuildMap(const aRect: TKMRect; aRandomTiles: Boolean = False); overload;
    procedure RebuildMap(X,Y,aSize: Integer; aSquare: Boolean; aAroundTiles: Boolean = False); overload;

    procedure MagicBrush(const aRect: TKMRect; aMaskKind: TKMTileMaskKind); overload;

    function PickRandomTile(aTerrainKind: TKMTerrainKind): Word; overload;
    function PickRandomTile(aTerrainKind: TKMTerrainKind; aRandom: Boolean): Word; overload;

    function PickRandomObject(aTerrainKind: TKMTerrainKind; aObjType: TKMTerrainObjectType; aX, aY: Integer): Integer;

    procedure SetTempTerKind;

    procedure FixTerrainKindInfoAtBorders(aMakeCheckpoint: Boolean = True);
    procedure FixTerrainKindInfo(aMakeCheckpoint: Boolean = True); overload;
    procedure FixTerrainKindInfo(const aRect: TKMRect; aMakeCheckpoint: Boolean = True); overload;

    class function GetRandomTile(aTerrainKind: TKMTerrainKind; aSkipRandom: Boolean = False): Word;

    procedure UpdateStateIdle;
    procedure UpdateState;
  end;


const
  //Table of combinations between terrain types (0-based)
  //1 - no transition
  //2 - in half
  //3 - one corner
  //"-" means flip before use
  Combo: array [TKMTerrainKind, TKMTerrainKind, 1..3] of SmallInt = (
  //             Custom  Grass          Moss    PaleGrass     CoastSand         GrassSand1 GrassSand2       GrassSand3    Sand          GrassDirt     Dirt            Barren Land       Cobblest         GrassyWater   Swamp      Ice          SnowOnGrass     SnowOnDirt     Snow          DeepSnow   StoneMount       GoldMount        IronMount        Abyss            Gravel        Coal              Gold             Iron             Water            FastWater    Lava
  {Custom}     ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Custom
  {Grass}      ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Grass
  {Moss}       ((0,0,0),(-19, -18,  9),(8,8,8),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Moss
  {PaleGrass}  ((0,0,0),( 66,  67, 68),(0,0,0),( 17, 17, 17),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //PaleGrass
  {CoastSand}  ((0,0,0),( 69,  70, 71),(0,0,0),(  0,  0,  0),(   32,  32,  32),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //CoastSand
  {GrassSand1} ((0,0,0),( 72,  73, 74),(0,0,0),(  0,  0,  0),(    0,   0,   0),(26,26,26),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //GrassSand1
  {GrassSand2} ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(  102, 103, 104),(75,76,77),(  27,  27,  27),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //GrassSand2
  {GrassSand3} ((0,0,0),( 93,  94, 95),(0,0,0),(  0,  0,  0),(  319, 320, 321),( 0, 0, 0),(  78,  79,  80),( 28, 28, 28),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //GrassSand3
  {Sand}       ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(   99, 100, 101),( 0, 0, 0),(  81,  82,  83),( 81, 82, 83),( 29, 29, 29),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Sand
  {GrassDirt}  ((0,0,0),( 84,  85, 86),(0,0,0),(-98,-97,-96),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),( 34, 34, 34),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //GrassDirt
  {Dirt}       ((0,0,0),( 56,  57, 58),(0,0,0),(  0,  0,  0),( -113,-112,-111),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),( 87, 88, 89),(  35,  35,  35),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Dirt
  {BarrenLand} ((0,0,0),(711, 712,713), (0,0,0),(  0,  0,  0),(  714, 715, 716),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),( 717, 718, 719),( 698, 698, 698),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),( 0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //BarrenLand
  {Cobblest}   ((0,0,0),(802, 803,804),(0,0,0),(  0,  0,  0),(  805, 806, 807),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(  38,  39, 215),(   0,   0,   0),( 215, 215, 215),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(814,815,816),(808,809,810),(811,812,813),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Cobblestone
  {GrassyWater}((0,0,0),(120, 121,122),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),( 48, 48, 48),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //GrassyWater
  {Swamp}      ((0,0,0),( 90,  91, 92),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(-722,-721,-720),(   0,   0,   0),(  0,  0,  0),(40,40,40),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Swamp
  {Ice}        ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),( 44, 44, 44),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Ice
  {SnowOnGrass}((0,0,0),(316, 317,318),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),( 247,  64,  65),(-731,-730,-729),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(315,315, 315),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //SnowOnGrass
  {SnowOnDirt} ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),( 247,  64,  65),(-725,-724,-723),( 816, 815, 814),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(312,313, 314),( 47, 47,  47),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //SnowOnDirt
  {Snow}       ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(-728,-727,-726),( 810, 809, 808),(  0,  0,  0),( 0, 0, 0),( 44, -4,-10),(  0,  0,   0),(220,212, 213),( 46, 46, 46),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Snow
  {DeepSnow}   ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),( 813, 812, 811),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(203,204,205),(45,45,45),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //DeepSnow
  {StoneMount} ((0,0,0),(274, 139,138),(0,0,0),(  0,  0,  0),(  273, 269, 268),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(273,269,268),(  0,  0,  0),( 282, 278, 277),( 745, 742, 741),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(298,294, 293),(290,286,285),( 0, 0, 0),( 132, 132, 132),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Stone Mountain
  {GoldMount}  ((0,0,0),(180, 172,176),(0,0,0),(  0,  0,  0),(  181, 173, 177),( 0, 0, 0),( 182, 174, 178),(  0,  0,  0),(  0,  0,  0),(351,352,353),( 183, 175, 179),( 735, 736, 737),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(800, 799, 798),(  0,  0,   0),( 49,171,  51),(261,262,306),( 0, 0, 0),( 340, 341, 342),( 159, 159, 159),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Gold Mountains
  {IronMount}  ((0,0,0),(188, 168,184),(0,0,0),(  0,  0,  0),(  189, 169, 185),( 0, 0, 0),( 190, 170, 186),(  0,  0,  0),(  0,  0,  0),(354,355,356),( 191, 167, 187),( 732, 733, 734),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(256,257, 258),( 52,166, 54),( 0, 0, 0),( 331, 332, 333),( 322, 323, 324),( 164, 164, 164),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Iron Mountains
  {Abyss}      ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),( 346, 347, 348),( -53, -50,-165),( 245,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Abyss
  {Gravel}     ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),( -113,-112,-111),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(  21,  21,  20),(   0,   0,   0),( -38, -39, -38),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(-65,-64,-247),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(-179,-175,-183),(-187,-167,-191),(   0,   0,   0),( 20, 20, 20),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Gravel
  {Coal}       ((0,0,0),( 56,  57, 58),(0,0,0),(  0,  0,  0),( -113,-112,-111),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),( 87, 88, 89),( 152, 153, 154),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(-65,-64,-247),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(-179,-175,-183),(-187,-167,-191),(   0,   0,   0),(  0,  0,  0),(155,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Coal
  {Gold}       ((0,0,0),(180, 172,176),(0,0,0),(  0,  0,  0),(  181, 173, 177),( 0, 0, 0),( 182, 174, 178),(  0,  0,  0),(  0,  0,  0),(351,352,353),( 183, 175, 179),( 735, 736, 737),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),( 49,171,  51),(261,262,306),( 0, 0, 0),( 343, 344, 345),( 144, 145, 146),(   0,   0,   0),(-348,-347,-346),(183,175,179),(183,  175,  179),( 147,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Gold
  {Iron}       ((0,0,0),(188, 168,184),(0,0,0),(  0,  0,  0),(  189, 169, 185),( 0, 0, 0),( 190, 170, 186),(  0,  0,  0),(  0,  0,  0),(354,355,356),( 191, 167, 187),( 732, 733, 734),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(256,257, 258),( 52,166, 54),( 0, 0, 0),( 334, 335, 336),(   0,   0,   0),( 328, 329, 330),( 165,  50,  53),(191,167,187),(191,	167,	 187),( 325, 326, 327),( 151,   0,   0),(   0,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Iron
  {Water}      ((0,0,0),(123,-125,127),(0,0,0),(  0,  0,  0),(  116,-117, 118),( 0, 0, 0),(-243,-242,-241),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(-107,-106,-105),( 747,-748, 749),(-107,-106,-105),(114,115,119),( 0, 0, 0),(-22,-12,-23),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(-143,-200,-236),(-237,-200,-236),(-239,-200,-236),( 245,   0,   0),(  0,  0,  0),(-107,-106, -105),(-237,-200,-236),(-239,-200,-236),( 192,   0,   0),(   0, 0, 0),(   0, 0, 0)), //Water
  {FastWater}  ((0,0,0),(123,-125,127),(0,0,0),(  0,  0,  0),(  116,-117, 118),( 0, 0, 0),(-243,-242,-241),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(-107,-106,-105),(   0,   0,   0),(-107,-106,-105),(114,115,119),( 0, 0, 0),(-22,-12,-23),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(-143,-200,-236),(-237,-200,-236),(-239,-200,-236),( 245,   0,   0),(  0,  0,  0),(-107,-106, -105),(-237,-200,-236),(-239,-200,-236),( 192, 192, 209),( 209, 0, 0),(   0, 0, 0)), //FastWater
  {Lava}       ((0,0,0),(  0,   0,  0),(0,0,0),(  0,  0,  0),(    0,   0,   0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  0,  0,  0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),( -15,   7,   7),(-300,   7,   7),(   0,   0,   0),(  0,  0,  0),(  0,    0,    0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(   0, 0, 0),(   7, 7, 7))  //Lava
               );

  //0     number of variants (1..X)
  //1..X  tile variants
  //
  RandomTiling: array [tkCustom..tkLava, 0..15] of Word = (
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (15,1,1,1,2,2,2,3,3,3,5,5,5,11,13,14), // Grass - reduced chance for "eye-catching" tiles
    (1,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // Moss
    (1,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0),    // PaleGrass
    (2,31,33,0,0,0,0,0,0,0,0,0,0,0,0,0),   // CoastSand
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // GrassSand1
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // GrassSand2
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // GrassSand3
    (1,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0),    // Sand
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // GrassDirt
    (2,36,37,0,0,0,0,0,0,0,0,0,0,0,0,0),   // Dirt
    (12,699,700,701,702,703,704,705,706,707,708,709,710,0,0,0),   // Barren Land
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // Cobblestone
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // GrassyWater
    (3,41,42,43,0,0,0,0,0,0,0,0,0,0,0,0),  // Swamp
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // Ice
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // SnowOnGrass
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // SnowOnDirt
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // Snow
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),     // DeepSnow
    (8,129,130,131,132,134,135,136,137,0,0,0,0,0,0,0),    // StoneMount
    (5,156,157,158,159,201{?},0,0,0,0,0,0,0,0,0,0),       // GoldMount
    (5,160,161,162,163,164,0,0,0,0,0,0,0,0,0,0),          // IronMount
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),                    // Abyss
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),                    // Gravel
    (11,152,153,154,154,154,155,155,155,155,263,263,0,0,0,0),  // Coal (enriched pattern)
    (12,144,145,146,146,146,147,147,147,147,307,307,307,0,0,0),  // Gold
    (13,148,149,150,150,151,151,151,151,259,259,260,260,260,0,0),//259,260,260,260,0,0),  // Iron
    (2,193,193,0,0,0,0,0,0,0,0,0,0,0,0,0),  // Water
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),      // FastWater
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)       // Lava
  );
    RandomObjects: array[tkCustom..tkLava,0..38] of Byte = (
    //objects start
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// tkcustom
    (0,1,2,3,4,5,6,7,10,11,12,13,14,15,16,20,45,47,41,42,190,191,192,193,194,8,9,0,1,5,6,7,0,1,2,3,4,5,6), // Grass
    (17,18,19,21,20,10,11,12,13,14,15,16,5,6,7,5,6,7,17,18,19,21,20,10,11,12,13,14,15,16,5,6,7,5,6,7,17,18,19),     // Moss
    (1,2,3,4,5,6,7,10,11,12,13,14,16,22,23,24,39,41,42,43,45,46,47,25,26,27,29,30,31,33,34,35,190,191,192,193,194,8,9),    // PaleGrass
    (0,0,0,1,2,3,1,2,3,0,1,2,3,4,8,9,68,190,191,192,193,194,0,0,0,1,2,3,1,2,3,0,1,2,3,4,8,9,68),   // CoastSand
    (0,1,2,3,4,31,34,38,45,68,214,211,210,212,190,191,192,8,0,1,2,3,4,31,34,38,45,68,214,211,210,212,190,191,192,8,0,1,2),     // GrassSand1
    (0,1,2,3,4,31,34,38,45,68,214,211,210,212,195,196,190,191,192,9,0,1,2,3,4,31,34,38,45,68,214,211,210,212,195,196,190,191,192),     // GrassSand2
    (0,1,2,3,4,31,34,38,45,68,214,211,210,212,195,196,216,217,190,191,192,8,0,1,2,3,4,31,34,38,45,68,214,211,210,212,195,196,216),     // GrassSand3
    (210,211,212,213,214,215,216,217,218,219,220,195,196,210,211,212,213,214,215,216,217,218,219,220,195,196,210,211,212,213,214,215,216,217,218,219,220,195,196),    // Sand
    (1,2,3,4,10,11,12,13,14,15,20,17,18,19,25,26,27,29,30,31,33,34,35,37,38,39,41,42,43,45,46,47,190,191,192,1,2,3,4),     // GrassDirt
    (0,1,2,3,4,15,20,25,26,27,29,30,31,33,34,35,64,65,66,190,191,192,8,9,0,1,2,3,4,15,20,25,26,27,29,30,31,33,34),   // Dirt
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),       // BarrenLand
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),     // Cobblestone
    (60,62,63,59,57,60,62,63,60,62,63,60,62,63,59,57,60,62,63,60,62,63,60,62,63,59,57,60,62,63,60,62,63,60,62,63,60,62,63),     // GrassyWater
    (60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63,60,62,63),  // Swamp
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),     // Ice
    (13,14,10,11,15,16,37,38,39,46,30,51,33,34,35,0,1,2,3,4,190,191,192,8,9,13,14,10,11,15,16,37,38,39,46,30,51,33,34),     // SnowOnGrass
    (10,11,12,0,1,2,3,4,26,20,25,26,27,29,30,31,33,34,35,64,65,66,190,191,192,8,9,10,11,12,0,1,2,3,4,26,20,25,26),     // SnowOnDirt
    (10,11,15,33,34,35,30,29,27,25,26,64,65,66,49,50,51,190,191,192,10,11,15,33,34,35,30,29,27,25,26,64,65,66,49,50,51,190,191),// Snow
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// DeepSnow
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// StoneMount
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// GoldMount
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// IronMount
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),// Abyss
    (0,1,2,3,4,68,0,0,0,0,1,2,3,1,2,3,0,1,2,3,4,68,0,0,0,0,1,2,3,1,2,3,8,0,1,2,3,4,9), // Gravel
    (0,1,2,3,4,68,0,0,0,0,1,2,3,1,2,3,0,1,2,3,4,68,0,0,0,0,1,2,3,1,2,3,8,9,0,1,2,3,4),  // Coal (enriched pattern)
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),  // Gold
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),  // Iron
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),  // Water
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255),      // FastWater
    (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255)       // Lava
    );
    RandomMushrooms: array[0..3]  of Byte = (17,18,19,21);
    RandomStones:    array[0..17] of Word = (0,1,2,3,4,8,9,0,1,2,3,4, 2, 3, 4, 278, 279, 279);
    RandomFlowers:   array[0..2]  of Byte = (22,23,24);
    RandomBush:      array[0..13] of Byte = (210,211,214,210,211,210,211,214,212,213,215,210,211,214);
    RandomCactus:    array[0..4]  of Byte = (216,217,218,219,220);
    RandomRuins:     array[0..5]  of Byte = (68,69,70,71,72,73);
    RandomDeadTrees: array[0..4]  of Byte = (190,191,192,193,194);
    RandomStumps:    array[0..30] of Byte = (10,11,12,13,14,15,16,25,26,27,29,30,31,33,34,35,37,38,39,41,42,43,45,46,47,49,50,51,64,65,66);

//  RMG2Painter: array [0..255] of TKMTerrainKind = (
//    tkGrass, tkGrass, tkGrass, tkGrass, tkSnow, tkGrass, tkGrass, tkCustom, tkMoss, tkMoss, tkSnow, tkGrass, tkWater,
//    tkGrass, tkGrass, tkCustom, tkPaleGrass, tkPaleGrass, tkGrass, tkGrass, tkDirt, tkDirt, tkWater, tkWater,
//    tkCustom, tkCustom, tkGrassSand1, tkGrassSand2, tkGrassSand1, tkRichSand, tkRichSand, tkSand, tkSand, tkSand,
//    tkDirtGrass, tkDirt, tkDirt, tkDirt, tkDirt, tkDirt, tkSwamp, tkSwamp, tkSwamp, tkSwamp, tkIce, tkDeepSnow, tkSnow,
//    tkShallowSnow, tkGrassyWater, tkShallowSnow, tkIronMount, tkShallowSnow, tkDeepSnow, tkIronMount, tkDeepSnow,
//    tkCustom, tkGrass, tkGrass, tkGrass, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkDirt, tkDirt, tkGrass,
//    tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkRustyGrass2, tkRustyGrass2, tkRustyGrass2,
//    tkGrassSand2, tkGrassSand2, tkGrassSand2, tkGrassSand1, tkGrassSand1, tkGrassSand1, tkGrass, tkGrass, tkGrass,
//    tkDirtGrass, tkDirtGrass, tkDirtGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkDirtGrass, tkDirtGrass,
//    tkDirtGrass, tkRichSand, tkRichSand, tkRichSand, tkGrassSand2, tkGrassSand2, tkGrassSand2, tkWater, tkWater, tkWater,
//    tkSand, tkSand, tkSand, tkDirt, tkDirt, tkDirt, tkGrassyWater, tkGrassyWater, tkWater, tkWater, tkWater, tkGrassyWater,
//    tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkGrass, tkStoneMount, tkStoneMount, tkStoneMount,
//    tkStoneMount, tkStoneMount, tkStoneMount, tkStoneMount, tkStoneMount, tkStoneMount, tkStoneMount, tkStoneMount,
//    tkStoneMount, tkStoneMount, tkStoneMount, tkWater, tkWater, tkGold, tkGold, tkGold, tkGold, tkIron, tkIron, tkIron,
//    tkIron, tkCoal, tkCoal, tkCoal, tkCoal, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkIronMount, tkIronMount,
//    tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount,
//    tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount,
//    tkGoldMount, tkGoldMount, tkGoldMount, tkGoldMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount, tkIronMount,
//    tkIronMount, tkIronMount, tkIronMount, tkWater, tkWater, tkWater, tkStoneMount, tkWater, tkDirt, tkCustom, tkCustom,
//    tkWater, tkGoldMount, tkCustom, tkSnow, tkSnow, tkSnow, tkCustom, tkCustom, tkWater, tkWater, tkWater, tkWater, tkSnow,
//    tkSnow, tkCustom, tkDirt, tkCustom, tkCustom, tkCustom, tkCustom, tkSnow, tkCustom, tkCustom, tkCustom, tkCustom,
//    tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkWater, tkWater, tkWater,
//    tkWater, tkWater, tkWater, tkWater, tkWater, tkWater, tkWater, tkWater, tkAbyss, tkCustom, tkDirt, tkCustom, tkCustom,
//    tkCustom, tkCustom, tkCustom, tkCustom, tkCustom, tkCustom
//  );


implementation
uses
  KM_Game, KM_Cursor, KM_Resource, KM_Log, KM_CommonUtils, KM_Utils,
  KM_GameParams, KM_GameSettings,
  KM_ResSprites, KM_MapEditorHistory, KM_ResTexts, KM_Terrain, KM_ResTypes;


type
  TKMTileMaskInfo = record
    TerKind: TKMTerrainKind;
    Rotation: Integer;
    SubType: TKMTileMaskSubType;
    Corners: TKMTileCorners;
    procedure SetCorners(aCorners: TKMByteSet);
  end;


function GetCombo(aTerKindFrom, aTerKindTo: TKMTerrainKind; aTransition: Byte; var aFound: Boolean): SmallInt;
begin
  aFound := True;
  Result := Combo[aTerKindFrom, aTerKindTo, aTransition];
  if (Result = 0)
    and not ((aTerKindFrom = tkGrass) and (aTerKindTo = tkGrass))
    and (aTerKindTo <> tkCustom)
    and (aTerKindFrom <> tkCustom) then
    //We have no direct transition
    aFound := False;
end;


{ TKMTileMaskInfo }
procedure TKMTileMaskInfo.SetCorners(aCorners: TKMByteSet);
var
  I: Integer;
begin
  for I := 0 to 3 do
    Corners[I] := I in aCorners;
end;


//Convert terrain kind from old format (before r9371, new automatic tile transitions) to new terrain kind
function TKMTerrainPainter.GetTerKind(aTKValue: Integer; aUseKamFormat: Boolean): TKMTerrainKind;
begin
  Result := tkCustom;
  if InRange(aTKValue, ShortInt(Low(TKMTerrainKind)), ShortInt(High(TKMTerrainKind))) then
  begin
    if not aUseKamFormat then
      Result := TKMTerrainKind(aTKValue)
    else begin
      case aTKValue of                //In old format it was:
        0:  Result := tkCustom;       //tkCustom
        1:  Result := tkGrass;        //tkGrass
        2:  Result := tkMoss;         //tkMoss
        3:  Result := tkPaleGrass;    //tkRustyGrass1
        4:  Result := tkGrassSand1;   //tkRustyGrass2
        5:  Result := tkGrassDirt;    //tkDirtGrass
        6:  Result := tkCoastSand;    //tkSand
        7:  Result := tkSand;         //tkRichSand
        8:  Result := tkDirt;         //tkDirt
        9:  Result := tkCobbleStone;  //tkCobbleStone
        10: Result := tkGrassSand3;   //tkGrassSand1
        11: Result := tkGrassSand2;   //tkGrassSand2
        12: Result := tkGrassyWater;  //tkGrassyWater
        13: Result := tkSwamp;        //tkSwamp
        14: Result := tkIce;          //tkIce
        15: Result := tkSnowOnDirt;   //tkShallowSnow
        16: Result := tkSnow;         //tkSnow
        17: Result := tkDeepSnow;     //tkDeepSnow
        18: Result := tkStone;        //tkStoneMount
        19: Result := tkGoldMount;    //tkGoldMount
        20: Result := tkIronMount;    //tkIronMount,
        21: Result := tkAbyss;        //tkAbyss
        22: Result := tkGravel;       //tkGravel
        23: Result := tkWater;        //tkCoal
        24: Result := tkCoal;         //tkGold
        25: Result := tkGold;         //tkIron
        26: Result := tkIron;         //tkWater
        27: Result := tkFastWater;    //tkFastWater
        28: Result := tkLava;         //tkLava
      end;
    end;
  end;
end;


{ TKMTerrainPainter }
function TKMTerrainPainter.BrushAreaTerKindContains(aCell: TKMPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to fBrushAreaTerKindCnt - 1 do
    if aCell = fBrushAreaTerKind[I] then
    begin
      Result := True;
      Exit;
    end;
end;

function TKMTerrainPainter.CanEditTile(const X: Integer; const Y: Integer): Boolean;
begin
  //Result := false;
  Result := gTerrain.MaskContains(fTempTerKind[Y, X])
            and ((Y + 1 >= gTerrain.MapY) or gTerrain.MaskContains(fTempTerKind[Y + 1, X]))
            and ((X + 1 >= gTerrain.MapX) or gTerrain.MaskContains(fTempTerKind[Y, X + 1]))
            and (((Y + 1 >= gTerrain.MapY) or (X + 1 >= gTerrain.MapX)) or gTerrain.MaskContains(fTempTerKind[Y + 1, X + 1]));
end;

procedure TKMTerrainPainter.SetTempTerKind;
var I, K : Integer;
begin
  for I := 1 to gTerrain.MapY do
    for K := 1 to gTerrain.MapX do
      fTempTerKind[I, K] := LandTerKind[I, K].TerKind;

end;

procedure TKMTerrainPainter.BrushTile(const X, Y: Integer);
begin
  BrushTerrainTile(X,Y, fTerKind);
end;


procedure TKMTerrainPainter.ApplyBrushObjects(const X, Y: Integer);
begin
    BrushObjects(X, Y);
end;


procedure TKMTerrainPainter.ApplyBrushObjectsWTerKind(const X, Y: Integer);
begin
  BrushObjects(X, Y, False, fTerKind);
end;


procedure TKMTerrainPainter.BrushTerrainTile(const X, Y: Integer; aTerKind: TKMTerrainKind);

  procedure AddBrushAreaTerKind(aX,aY: Integer);
  var
    P: TKMPoint;
  begin
    P := KMPoint(aX,aY);
    if not BrushAreaTerKindContains(P) then
    begin
      fBrushAreaTerKind[fBrushAreaTerKindCnt] := P;
      Inc(fBrushAreaTerKindCnt);
    end;
  end;

begin
  if fSize = 0 then
  begin
    if not gTerrain.VerticeInMapCoords(X, Y) then
      Exit;

    LandTerKind[fMapYn, fMapXn].TerKind := aTerKind;
    AddBrushAreaTerKind(fMapXn, fMapYn);
    Exit;
  end;

  if not gTerrain.TileInMapCoords(X, Y) then
    Exit;

  if not CanEditTile(X, Y) then
    Exit;

  LandTerKind[Y,   X].TerKind   := aTerKind;
  LandTerKind[Y,   X+1].TerKind := aTerKind;
  LandTerKind[Y+1, X+1].TerKind := aTerKind;
  LandTerKind[Y+1, X].TerKind   := aTerKind;

  gTerrain.Land^[Y, X].BaseLayer.Terrain := PickRandomTile(aTerKind);
  gTerrain.Land^[Y, X].BaseLayer.Rotation := KaMRandom(4, 'TKMTerrainPainter.BrushTerrainTile'); //Random direction for all plain tiles
  gTerrain.Land^[Y, X].IsCustom := False;

  AddBrushAreaTerKind(X,  Y);
  AddBrushAreaTerKind(X+1,Y);
  AddBrushAreaTerKind(X+1,Y+1);
  AddBrushAreaTerKind(X,  Y+1);

  if gCursor.MapEdUseTerrainObjects
    and (fLastPosition <> KMPoint(fMapXc,fMapYc)) then
    begin
      fLastPosition := KMPoint(fMapXc,fMapYc);
      IterateOverArea(KMPoint(fMapXc,fMapYc), fSize, fShape = hsSquare, ApplyBrushObjectsWTerKind);
    end;
end;


function TKMTerrainPainter.IsTerrainRepresentTerKind(aTerId: Word; aTerKind: TKMTerrainKind): Boolean;
var
  I: Integer;
begin
  Result := (aTerId = BASE_TERRAIN[aTerKind]);
  for I := 1 to RandomTiling[aTerKind,0] do
  begin
    if Result then Exit;
    Result := Result or (aTerId = RandomTiling[aTerKind,I]);
  end;
end;


function TKMTerrainPainter.PickRandomTile(aTerrainKind: TKMTerrainKind): Word;
begin
  Result := PickRandomTile(aTerrainKind, fRandomizeTiling);
end;


function TKMTerrainPainter.PickRandomTile(aTerrainKind: TKMTerrainKind; aRandom: Boolean): Word;
begin
  Result := GetRandomTile(aTerrainKind, not aRandom);
end;


function TKMTerrainPainter.PickRandomObject(aTerrainKind: TKMTerrainKind; aObjType: TKMTerrainObjectType; aX, aY: Integer): Integer;
const
  TREE_AGE: array[0..4] of TKMChopableAge = (caAge1, caAge2, caAge3, caAgeFull, caAgeStump);
var
  objRandom: Integer;
  treeAge: TKMChopableAge;
begin
  Result := OBJ_NONE;

  if ( LandTerKind[aY+1, aX+1].TerKind = aTerrainKind ) then
    case aObjType of
      otTrees:        begin
                        case gCursor.MapEdForestAge of
                          0:  treeAge := TREE_AGE[KaMRandom(Length(TREE_AGE), 'TKMTerrainPainter.PickRandomObject')];
                          1:  treeAge := TREE_AGE[KaMRandom(Length(TREE_AGE) - 1, 'TKMTerrainPainter.PickRandomObject')];
                          else
                            treeAge := TREE_AGE[gCursor.MapEdForestAge - 2];
                        end;

                        Result := gTerrain.ChooseTreeToPlace(KMPoint(aX, aY), treeAge, False);
                      end;
      otAllButTrees:  begin
                        objRandom := KaMRandom(High(RandomObjects[aTerrainKind]), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomObjects[aTerrainKind, objRandom];
                      end;
      otFlowers:      if (tpWolf in gTerrain.Land^[aY, aX].Passability) then begin
                        objRandom := KaMRandom(High(RandomFlowers), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomFlowers[objRandom];
                      end;
      otMushrooms:    if   (aTerrainKind in [tkGrass..tkPaleGrass])
                        or (aTerrainKind in [tkGrassSand1..tkGrassSand2])
                        or (aTerrainKind in [tkGrassDirt..tkDirt]) then
                      begin
                        objRandom := KaMRandom(High(RandomMushrooms), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomMushrooms[objRandom];
                      end;
      otStumps:       if   (aTerrainKind in [tkGrass..tkGrassSand3])
                        or (aTerrainKind in [tkGrassDirt..tkDirt]) then
                      begin
                        objRandom := KaMRandom(High(RandomStumps), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomStumps[objRandom];
                      end;
      otDeadTrees:    if   (aTerrainKind in [tkGrass..tkGrassSand3])
                        or (aTerrainKind in [tkGrassDirt..tkDirt]) then
                      begin
                        objRandom := KaMRandom(High(RandomDeadTrees), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomDeadTrees[objRandom];
                      end;
      otStones:       if (tpMakeRoads in gTerrain.Land^[aY, aX].Passability) then
                      begin
                        objRandom := KaMRandom(High(RandomStones), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomStones[objRandom];
                      end;
      otBushes:       if aTerrainKind in [tkGrassSand3..tkSand] then
                      begin
                        objRandom := KaMRandom(High(RandomBush), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomBush[objRandom];
                      end;
      otCactus:       if aTerrainKind in [tkGrassSand3..tkSand] then
                      begin
                        objRandom := KaMRandom(High(RandomCactus), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomCactus[objRandom];
                      end;
      otRuins:        if (tpMakeRoads in gTerrain.Land^[aY, aX].Passability) then
                      begin
                        objRandom := KaMRandom(High(RandomRuins), 'TKMTerrainPainter.PickRandomObject');
                        Result := RandomRuins[objRandom];
                      end;
    end;
end;

function TKMTerrainPainter.TryGetVertexEffectiveTerKind(X, Y: Word; var aEffectiveTKind: TKMTerrainKind): Boolean;

  function EqualTKinds(aTK1, aTK2: TKMTerrainKind): Boolean;
  var
    I: Integer;
  begin
    Result := aTK1 = aTK2;
    if not Result then
      for I := Low(TERRAIN_EQUALITY_PAIRS) to High(TERRAIN_EQUALITY_PAIRS) do
        if (TERRAIN_EQUALITY_PAIRS[I].TK1 in [aTK1, aTK2])
          and (TERRAIN_EQUALITY_PAIRS[I].TK2 in [aTK1, aTK2]) then
          Result := True;
  end;

var
  I, K, M: Integer;
  vertexTKinds: TKMTerrainKindCorners;
  sameTKind, noCustomTK: Boolean;
  terKindCnts, mostTKIs: array[0..3] of Integer;
  mostTKCnt, mostTKI, vrxCnt: Integer;
begin
  Result := False;
  if not gTerrain.TileInMapCoords(X, Y) then Exit;

  aEffectiveTKind := tkCustom;

  vrxCnt := GetVertexCornerTerKinds(X, Y, vertexTKinds);
  sameTKind := True;

  Assert(vrxCnt > 0, 'Can''t find vertex corners');

  for K := 0 to vrxCnt - 1 do
  begin
    terKindCnts[K] := 0;
    mostTKIs[K] := 0;
  end;

  //Find most popular TerKind
  //Count all TerKinds occurrences first
  for K := 0 to vrxCnt - 1 do
    for M := K + 1 to vrxCnt - 1 do
      if vertexTKinds[K] = vertexTKinds[M] then
        Inc(terKindCnts[K]);

  //Get Most popular one index
  mostTKCnt := -1;
  I := 0;
  for K := 0 to vrxCnt - 1 do
    if terKindCnts[K] > mostTKCnt then
    begin
      I := 0;
      mostTKIs[I] := K;
      mostTKCnt := terKindCnts[K];
      Inc(I);
    end
    else
    if terKindCnts[K] = mostTKCnt then
    begin
      mostTKIs[I] := K;
      Inc(I);
    end;

  mostTKI := mostTKIs[KaMRandom(I, 'TKMTerrainPainter.TryGetVertexEffectiveTerKind')];

  noCustomTK := vertexTKinds[0] <> tkCustom;
  for K := 1 to vrxCnt - 1 do
  begin
    sameTKind := sameTKind and EqualTKinds(vertexTKinds[K], vertexTKinds[K-1]);
    noCustomTK := noCustomTK and (vertexTKinds[K] <> tkCustom);
  end;

  //Replace TerKind with most popular one if there all TerKinds are equal or if there is no custom TerKinds
  if sameTKind or noCustomTK then
  begin
    aEffectiveTKind := vertexTKinds[mostTKI];
    Result := True;
  end;
end;


procedure TKMTerrainPainter.FixTerrainKindInfoAtBorders(aMakeCheckpoint: Boolean = True);
begin
  FixTerrainKindInfo(KMRect(1,            2,            1,            gTerrain.MapY - 1), aMakeCheckpoint); // Left
  FixTerrainKindInfo(KMRect(1,            1,            gTerrain.MapX,1),                 aMakeCheckpoint); // Top
  FixTerrainKindInfo(KMRect(gTerrain.MapX,2,            gTerrain.MapX,gTerrain.MapY - 1), aMakeCheckpoint); //Right
  FixTerrainKindInfo(KMRect(1,            gTerrain.MapY,gTerrain.MapX,gTerrain.MapY),     aMakeCheckpoint); //Bottom
end;


procedure TKMTerrainPainter.FixTerrainKindInfo(aMakeCheckpoint: Boolean = True);
begin
  FixTerrainKindInfo(KMRect(1, 1, gTerrain.MapX, gTerrain.MapY), aMakeCheckpoint);
end;


procedure TKMTerrainPainter.FixTerrainKindInfo(const aRect: TKMRect; aMakeCheckpoint: Boolean = True);
var
  I, J: Integer;
  terKind: TKMTerrainKind;
begin
  for I := aRect.Top to aRect.Bottom do
    for J := aRect.Left to aRect.Right do
    begin
      if TryGetVertexEffectiveTerKind(J, I, terKind) then
      begin
        LandTerKind[I,J].TerKind := terKind;
        LandTerKind[I,J].Tiles := High(SmallInt);
      end;
    end;

  if aMakeCheckpoint then
    gGame.MapEditor.History.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_BRUSH_FIX_TERRAIN_SHORT]);
end;


class function TKMTerrainPainter.GetRandomTile(aTerrainKind: TKMTerrainKind; aSkipRandom: Boolean = False): Word;
begin
  Result := Abs(Combo[aTerrainKind, aTerrainKind, 1]);

  if aSkipRandom or (RandomTiling[aTerrainKind, 0] = 0) then Exit;


  if aTerrainKind in [tkStone..tkIronMount, tkCoal..tkIron] then
    //Equal chance
    Result := RandomTiling[aTerrainKind, KaMRandom(RandomTiling[aTerrainKind, 0], 'TKMTerrainPainter.GetRandomTile') + 1]
  else
  if KaMRandom(6, 'TKMTerrainPainter.GetRandomTile 2') = 1 then
    //Chance reduced to 1/6
    Result := RandomTiling[aTerrainKind, KaMRandom(RandomTiling[aTerrainKind, 0], 'TKMTerrainPainter.GetRandomTile 3') + 1];
end;


procedure TKMTerrainPainter.RebuildTile(const X,Y: Integer);
begin
  RebuildTile(X, Y, fRandomizeTiling);
end;


procedure TKMTerrainPainter.RebuildTile(const X,Y: Integer; aRandomTiles: Boolean);
var
  pY, pX, nodes, rot, T: Integer;
  tmp, ter1, ter2, A, B, C, D: TKMTerrainKind;
  found: Boolean;
begin
  if not gTerrain.TileInMapCoords(X, Y) then Exit;

  pX := EnsureRange(X, 1, gTerrain.MapX - 1);
  pY := EnsureRange(Y, 1, gTerrain.MapY - 1);

  //don't touch custom placed tiles (tkCustom type)
//  if (LandTerKind[pY  ,pX].TerKind <> tkCustom)
//    or (LandTerKind[pY  ,pX+1].TerKind <> tkCustom)
//    or (LandTerKind[pY+1,pX].TerKind <> tkCustom)
//    or (LandTerKind[pY+1,pX+1].TerKind <> tkCustom) then
  if not fOverrideCustomTiles and gTerrain.Land^[pY,pX].IsCustom then Exit;

  A := (LandTerKind[pY    , pX    ].TerKind);
  B := (LandTerKind[pY    , pX + 1].TerKind);
  C := (LandTerKind[pY + 1, pX    ].TerKind);
  D := (LandTerKind[pY + 1, pX + 1].TerKind);
  rot := 0;
  nodes := 1;

  //A-B
  //C-D
  ter1 := tkCustom;
  ter2 := tkCustom;

  if (A=B)or(C=D)  then begin ter1:=A; ter2:=C; nodes:=2; if A<C then rot:=2 else rot:=0; end;
  if (A=C)or(B=D)  then begin ter1:=A; ter2:=B; nodes:=2; if A<B then rot:=1 else rot:=3; end;

  //special case \ and /
  if A=D then begin ter1:=A; ter2:=B; nodes:=4+1; rot:=1; end;
  if B=C then begin ter1:=A; ter2:=B; nodes:=4+2; rot:=0; end;

  if (A=B)and(C=D) then begin ter1:=A; ter2:=C; nodes:=2; if A<C then rot:=2 else rot:=0; end;
  if (A=C)and(B=D) then begin ter1:=A; ter2:=B; nodes:=2; if A<B then rot:=1 else rot:=3; end;

  if (B=C)and(C=D) then begin ter1:=C; ter2:=A; nodes:=3; if C<A then rot:=3 else rot:=1; end;
  if (A=C)and(C=D) then begin ter1:=A; ter2:=B; nodes:=3; if A<B then rot:=0 else rot:=2; end;
  if (A=B)and(B=D) then begin ter1:=A; ter2:=C; nodes:=3; if A<C then rot:=2 else rot:=0; end;
  if (A=B)and(B=C) then begin ter1:=A; ter2:=D; nodes:=3; if A<D then rot:=1 else rot:=3; end;

  if (A=B)and(B=C)and(C=D) then begin ter1:=A; ter2:=A; nodes:=4; rot:=0; end;

  //Terrain table has only half filled, so make sure first comes bigger ID
  if ter1 < ter2 then
  begin
   tmp := ter1;
   ter1 := ter2;
   ter2 := tmp;
   case nodes of
      1..3: nodes := 4 - nodes;  //invert nodes count
      5..6: rot := 1;
    end;
  end;

  //Some tiles placed upside down or need other special treatment
  if nodes < 4 then
  begin
    //Flip direction
    if Combo[ter1, ter2, nodes] < 0 then
      rot := (rot + 2) mod 4;
    //For some weird reason lava needs to be rotated 90`
    if ter1 = tkLava then
      rot := (rot + 1) mod 4;
  end;

  T := 0;
  if nodes < 4 then T := Abs(GetCombo(ter1, ter2, nodes, found));     //transition tiles
  if nodes = 4 then T := Abs(GetCombo(ter1, ter2, 1, found));         //no transition
  if nodes > 4 then T := Abs(GetCombo(ter1, ter2, 3, found));         //transition use 1 or 3

  //for plain tiles only
  if ter1 = ter2 then
  begin
    T := PickRandomTile(ter1, aRandomTiles);
    rot := KaMRandom(4, 'TKMTerrainPainter.RebuildTile'); //random direction for all plain tiles
  end;

  //Need to check if this tile was already smart-painted, "4-Nodes" hence default value is 0
  if (LandTerKind[pY,pX].Tiles <> Byte(ter1)*Byte(ter2)*(4-nodes))
    or ((nodes = 4) and not IsTerrainRepresentTerKind(gTerrain.Land^[pY,pX].BaseLayer.Terrain, ter1)) //All nodes, but terrain is different from needed TerKind
    or gTerrain.Land^[pY,pX].HasLayers then
  begin
    LandTerKind[pY,pX].Tiles := Byte(ter1)*Byte(ter2)*(4-nodes);//store not only nodes info, but also terrain type used
    if found and ((nodes = 4) or (fBrushMask = mkNone)) then
    begin
      gTerrain.Land^[pY,pX].BaseLayer.Terrain := T;
      gTerrain.Land^[pY,pX].BaseLayer.SetAllCorners;
      gTerrain.Land^[pY,pX].LayersCnt := 0;
      gTerrain.Land^[pY,pX].BaseLayer.Rotation := rot mod 4;
    end
  end;
end;


procedure TKMTerrainPainter.RebuildMap;
begin
  RebuildMap(KMRect(1, 1, gTerrain.MapX, gTerrain.MapY));
end;


procedure TKMTerrainPainter.RebuildMap(const aRect: TKMRect; aRandomTiles: Boolean = False);
var
  I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      RebuildTile(K, I, aRandomTiles);
end;


procedure TKMTerrainPainter.RebuildMap(X,Y,aSize: Integer; aSquare: Boolean; aAroundTiles: Boolean = False);
begin
  IterateOverArea(KMPoint(X,Y), aSize, aSquare, RebuildTile, aAroundTiles);
end;


//Get tile corners terkinds (TKinds, based on CornersTerKinds or generated mask)
procedure TKMTerrainPainter.GetTileOwnCornersTKinds(const aCell: TKMPoint; var aCornersTerKinds: TKMTerrainKindCorners);
begin
  GetTileCornersTKinds(aCell, aCornersTerKinds, True);
end;


procedure TKMTerrainPainter.GetTileLandNodeTKinds(const aCell: TKMPoint; var aCornersTerKinds: TKMTerrainKindCorners);
begin
  GetTileCornersTKinds(aCell, aCornersTerKinds, False, True);
end;


function TKMTerrainPainter.GetVertexCornerTerKinds(X,Y: Word; var aCornersTerKinds: TKMTerrainKindCorners): Integer;
var
  cornersTerKinds: TKMTerrainKindCorners;

  procedure CheckTile(aX, aY: Integer; aCorner: Byte);
  begin
    if not gTerrain.TileInMapCoords(aX, aY) then Exit;

    GetTileOwnCornersTKinds(KMPoint(aX, aY), cornersTerKinds);

    aCornersTerKinds[Result] := cornersTerKinds[aCorner];
    Inc(Result);
  end;

begin
  Result := 0;

  CheckTile(X-1, Y-1, 2);
  CheckTile(X  , Y-1, 3);
  CheckTile(X  , Y  , 0);
  CheckTile(X-1, Y  , 1);
end;


procedure TKMTerrainPainter.GetTileCornersTKinds(const aCell: TKMPoint;
                                                out aCornersTerKinds: TKMTerrainKindCorners;
                                                aGetOnlyTileCornersTK: Boolean = False;
                                                aGetOnlyLandNodeTK: Boolean = False);
var
  terKindFound: array [0..3] of Boolean;

  procedure CheckTerKind(aX,aY,aI: Integer);
  var
    TerKind: TKMTerrainKind;
  begin
    if not gTerrain.VerticeInMapCoords(aX, aY)
      or (aGetOnlyTileCornersTK and not BrushAreaTerKindContains(KMPoint(aX, aY))) then
      Exit;

    TerKind := LandTerKind[aY,aX].TerKind;
    if TerKind <> tkCustom then
    begin
      terKindFound[aI] := True;
      aCornersTerKinds[aI] := TerKind;
    end;
  end;

var
  I,L: Integer;
  tile: TKMTerrainTileBasic;

begin
  Assert(not (aGetOnlyTileCornersTK and aGetOnlyLandNodeTK)); //At least 1 of those parameters should be False

  //Init with tkCustom
  for I := 0 to 3 do
  begin
    terKindFound[I] := False;
    aCornersTerKinds[I] := tkCustom;
  end;

  //Get tile corners TKinds based on LandTerKind
  if not aGetOnlyTileCornersTK then
  begin
    CheckTerKind(aCell.X,   aCell.Y,   0);
    CheckTerKind(aCell.X+1, aCell.Y,   1);
    CheckTerKind(aCell.X+1, aCell.Y+1, 2);
    CheckTerKind(aCell.X,   aCell.Y+1, 3);
  end;

  //Get tile corners terkinds (TKinds, based on CornersTerKinds or generated mask)
  if not aGetOnlyLandNodeTK and gTerrain.TileInMapCoords(aCell) then
  begin
    if fUseTempLand then
      tile := fTempLand[aCell.Y, aCell.X]
    else
      tile := GetTerrainTileBasic(gTerrain.Land^[aCell.Y, aCell.X]);

    for I := 0 to 3 do
      if not terKindFound[I] then
      begin
        if tile.BaseLayer.Corner[I] then
          aCornersTerKinds[I] := gRes.Tileset[tile.BaseLayer.Terrain].TerKinds[(I + 4 - tile.BaseLayer.Rotation) mod 4]
        else
          for L := 0 to tile.LayersCnt - 1 do
            if tile.Layer[L].Corner[I] then
              aCornersTerKinds[I] := gRes.Sprites.GetGenTerrainInfo(tile.Layer[L].Terrain).TerKind;
      end;
  end;
end;


function GetMaskType(aCornerTerKinds: TKMTerrainKindCorners; var aLayerOrder: array of TKMTileMaskInfo): TKMTileMaskType;
var
  A,B,C,D: TKMTerrainKind;
  I, J, tmp: Integer;
  cornerI: array[0..3] of Integer;
begin
  Result := tmtNone; // makes compiler happy
  // A B
  // D C
  A := aCornerTerKinds[0];
  B := aCornerTerKinds[1];
  C := aCornerTerKinds[2];
  D := aCornerTerKinds[3];

  // A A
  // A A
  if (A = B) and (A = C) and (A = D) then
  begin
    Result := tmtNone;
    aLayerOrder[0].TerKind := A;
    aLayerOrder[0].SetCorners([0,1,2,3]);
    aLayerOrder[0].Rotation := 0;
    Exit;
  end;

  // A A
  // D A
  if ((A = B) and (A = C)) then
  begin
    if TER_KIND_ORDER[B] < TER_KIND_ORDER[D] then
    begin
      aLayerOrder[0].TerKind := B;
      aLayerOrder[0].SetCorners([0,1,2]);
      aLayerOrder[1].TerKind := D;
      aLayerOrder[1].Rotation := 3;
      aLayerOrder[1].SetCorners([3]);
      Result := tmt2Corner;
    end else
    begin
      aLayerOrder[0].TerKind := D;
      aLayerOrder[0].SetCorners([3]);
      aLayerOrder[1].TerKind := B;
      aLayerOrder[1].Rotation := 1;
      aLayerOrder[1].SetCorners([0,1,2]);
      Result := tmt2Diagonal;
    end;
    Exit;
  end;

  // A A
  // A C
  if ((A = B) and (A = D)) then
  begin
    if TER_KIND_ORDER[A] < TER_KIND_ORDER[C] then
    begin
      aLayerOrder[0].TerKind := A;
      aLayerOrder[0].SetCorners([0,1,3]);
      aLayerOrder[1].TerKind := C;
      aLayerOrder[1].Rotation := 2;
      aLayerOrder[1].SetCorners([2]);
      Result := tmt2Corner;
    end else
    begin
      aLayerOrder[0].TerKind := C;
      aLayerOrder[0].SetCorners([2]);
      aLayerOrder[1].TerKind := A;
      aLayerOrder[1].Rotation := 0;
      aLayerOrder[1].SetCorners([0,1,3]);
      Result := tmt2Diagonal;
    end;
    Exit;
  end;

  // A B
  // B B
  if ((B = C) and (B = D)) then
  begin
    if TER_KIND_ORDER[A] < TER_KIND_ORDER[C] then
    begin
      aLayerOrder[0].TerKind := A;
      aLayerOrder[0].SetCorners([0]);
      aLayerOrder[1].TerKind := C;
      aLayerOrder[1].Rotation := 2;
      aLayerOrder[1].SetCorners([1,2,3]);
      Result := tmt2Diagonal;
    end else
    begin
      aLayerOrder[0].TerKind := C;
      aLayerOrder[0].SetCorners([1,2,3]);
      aLayerOrder[1].TerKind := A;
      aLayerOrder[1].Rotation := 0;
      aLayerOrder[1].SetCorners([0]);
      Result := tmt2Corner;
    end;
    Exit;
  end;

  // A B
  // A A
  if ((A = C) and (A = D)) then
  begin
    if TER_KIND_ORDER[D] < TER_KIND_ORDER[B] then
    begin
      aLayerOrder[0].TerKind := D;
      aLayerOrder[0].SetCorners([0,2,3]);
      aLayerOrder[1].TerKind := B;
      aLayerOrder[1].Rotation := 1;
      aLayerOrder[1].SetCorners([1]);
      Result := tmt2Corner;
    end else
    begin
      aLayerOrder[0].TerKind := B;
      aLayerOrder[0].SetCorners([1]);
      aLayerOrder[1].TerKind := D;
      aLayerOrder[1].Rotation := 3;
      aLayerOrder[1].SetCorners([0,2,3]);
      Result := tmt2Diagonal;
    end;
    Exit;
  end;

  // A A
  // C C
  if (A = B) and (C = D) then
  begin
    Result := tmt2Straight;
    if TER_KIND_ORDER[A] < TER_KIND_ORDER[C] then
    begin
      aLayerOrder[0].TerKind := A;
      aLayerOrder[0].SetCorners([0,1]);
      aLayerOrder[1].TerKind := C;
      aLayerOrder[1].Rotation := 2;
      aLayerOrder[1].SetCorners([2,3]);
    end else
    begin
      aLayerOrder[0].TerKind := C;
      aLayerOrder[0].SetCorners([2,3]);
      aLayerOrder[1].TerKind := A;
      aLayerOrder[1].Rotation := 0;
      aLayerOrder[1].SetCorners([0,1]);
    end;
    Exit;
  end;

  // A B
  // A B
  if (A = D) and (B = C) then
  begin
    Result := tmt2Straight;
    if TER_KIND_ORDER[A] < TER_KIND_ORDER[B] then
    begin
      aLayerOrder[0].TerKind := A;
      aLayerOrder[0].SetCorners([0,3]);
      aLayerOrder[1].TerKind := B;
      aLayerOrder[1].Rotation := 1;
      aLayerOrder[1].SetCorners([1,2]);
    end else
    begin
      aLayerOrder[0].TerKind := B;
      aLayerOrder[0].SetCorners([1,2]);
      aLayerOrder[1].TerKind := A;
      aLayerOrder[1].Rotation := 3;
      aLayerOrder[1].SetCorners([0,3]);
    end;
    Exit;
  end;


  // A B
  // B A
  if (A = C) and (B = D) then
  begin
    Result := tmt2Opposite;
    if TER_KIND_ORDER[A] < TER_KIND_ORDER[B] then
    begin
      aLayerOrder[0].TerKind := A;
      aLayerOrder[0].SetCorners([0,2]);
      aLayerOrder[1].TerKind := B;
      aLayerOrder[1].Rotation := 1;
      aLayerOrder[1].SetCorners([1,3]);
    end else
    begin
      aLayerOrder[0].TerKind := B;
      aLayerOrder[0].SetCorners([1,3]);
      aLayerOrder[1].TerKind := A;
      aLayerOrder[1].Rotation := 0;
      aLayerOrder[1].SetCorners([0,2]);
    end;
    Exit;
  end;

  for I := 0 to 3 do
   cornerI[I] := I;

  for I := 0 to 3 do
    for J := I to 3 do
      if TER_KIND_ORDER[aCornerTerKinds[cornerI[I]]] > TER_KIND_ORDER[aCornerTerKinds[cornerI[J]]] then
      begin
        tmp := cornerI[I];
        cornerI[I] := cornerI[J];
        cornerI[J] := tmp;
      end;

  // A A
  // C D
  J := 0;
  for I := 0 to 3 do // go up to '4' corner, need to cycle all around to find A = D situation
  begin
    if (I = 0)
      or ((I < 4) and (aCornerTerKinds[cornerI[I]] <> aCornerTerKinds[cornerI[I-1]])) then
    begin
      aLayerOrder[J].TerKind := aCornerTerKinds[cornerI[I]];
      aLayerOrder[J].Rotation := cornerI[I];
      aLayerOrder[J].SetCorners([cornerI[I]]);
      Inc(J);
    end else
    if (aCornerTerKinds[cornerI[I]] = aCornerTerKinds[cornerI[I-1]]) then
    begin
      // CornerI was not sorted with stable sort, so it could be possible we will find first not the minimum rotation
      if ((cornerI[I] = 0) and (cornerI[I-1] = 3))
        or ((cornerI[I] = 3) and (cornerI[I-1] = 0)) then // use 3rd rotation for A-D situation (choose 3 between 0 and 3)
        aLayerOrder[J-1].Rotation := 3
      else
        aLayerOrder[J-1].Rotation := Min(cornerI[I], cornerI[I-1]);
      aLayerOrder[J-1].SubType := mstExtra;
      if Abs(cornerI[I] - cornerI[I-1]) = 2 then
        Result := tmt3Opposite
      else
        Result := tmt3Straight;
      aLayerOrder[J-1].Corners[cornerI[I]] := True;
    end;
  end;

  case J of
    3:      Exit;
    4:      Exit(tmt4Square);
    else    raise Exception.Create('Wrong number of corners with different TerKind: ' + IntToStr(J));
  end;
end;


procedure TKMTerrainPainter.MagicBrush(const aRect: TKMRect; aMaskKind: TKMTileMaskKind);
var
  I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      MagicBrush(K, I, aMaskKind);
end;


procedure TKMTerrainPainter.MagicBrush(const X,Y: Integer);
begin
  MagicBrush(X, Y, fBrushMask);
end;


procedure TKMTerrainPainter.MagicBrush(const X,Y: Integer; aMaskKind: TKMTileMaskKind);

  //This method tries to find the best appropriate TerKind for target cell (aCell) and for specified corner (aCorner)
  //1. We get cells next to aCell aCorner, and within map sizes
  //2. get TerKind from that cells, from corners, which are 'connected' to aCell aCorner, which are jointed
  //3. there could be several possibilities:
  // - no cells were found around aCell (map corner) - return aFound = False
  // - Found several Cells, Get several TerKinds, then
  //   - choose most popular one first
  //   - if all are different (1 occurences for each) then choose from diagonal cell
  //   - else take first one clockwise
  function GetCornerTerKind(aCorner: Byte; aCell: TKMPoint; var aFound: Boolean): TKMTerrainKind;
  var
    I, J, K: Integer;
    Rect: TKMRect;
    Dir: TKMDirection;
    RectCorners: TKMPointArray;
    DiagTerKind: TKMTerrainKind;
    HasCornerTiles: Boolean;
    TerKinds: array [0..2] of TKMTerrainKind;
    cornersTerKinds: TKMTerrainKindCorners;
  begin
    Result := tkCustom;
    case aCorner of
      0: Dir := dirNW;
      1: Dir := dirNE;
      2: Dir := dirSE;
      3: Dir := dirSW;
      else raise Exception.Create('Unknown direction'); // Makes compiler happy
    end;
    //get 4 tiles around corner within map borders
    Rect := KMRect(aCell);                                            // x x  - we will get these x Tiles for corner 0, f.e.
    Rect := KMRectGrow(Rect, Dir);                                    // x o
    Rect := KMClipRect(Rect, 0, 0, gTerrain.MapX, gTerrain.MapY);     // Clip rect with possible nodes boundaries.
    RectCorners := KMRectCorners(Rect);                               //
    //after this preparation we will get 4 points (rect corners)

    K := 0;
    //DiagTerKind := tkNone;
    DiagTerKind := tkCustom;
    HasCornerTiles := False;
    for I := 0 to Length(RectCorners) - 1 do
    begin
      J := (I + aCorner) mod 4;
      if not KMSamePoint(aCell, RectCorners[J]) then //if rect was clipped due to map sizes restriction, then its corner will be same as our target cell (aCell)
      begin
        HasCornerTiles := True;
        //From all tile terkinds get corner we need.
        //we used that fact, that corner formula can be obtained as (I + aCorner + 2).
        //so f.e. for 0 corner, and tile 'above' (tile to the top from tasrget cell(aCell)) we have to get terrainKind from corner 3
        //3 = 1 (I, start from top left alwaysm top tile is the 2nd tile) + 0 (aCorner) + 2
        //and so on
        GetTileCornersTKinds(RectCorners[J], cornersTerKinds);
        TerKinds[K] := cornersTerKinds[(I+aCorner+2) mod 4];
        // Find diagTerKind, as its preferrable over other cells
        if (aCell.X <> RectCorners[J].X) and (aCell.Y <> RectCorners[J].Y) then
          DiagTerKind := TerKinds[K];
        Inc(K);
      end;
    end;

    aFound := False;

    if not HasCornerTiles then Exit;

    //Find most popular TerKind
    //choose most popular one TerKind first
    if (TerKinds[0] = TerKinds[1])
      or (TerKinds[0] = TerKinds[2]) then
    begin
      Result := TerKinds[0];
      aFound := True;
    end else if (TerKinds[1] = TerKinds[2]) then
    begin
      Result := TerKinds[1];
      aFound := True;
    end else if DiagTerKind <> tkCustom then
    begin
      Result := DiagTerKind; //if all are different (1 occurences for each) then choose from diagonal cell
      aFound := True;
    end;
  end;

  procedure GetTerKindsAround(aCell: TKMPoint; out aCornersTerKinds: TKMTerrainKindCorners);
  var
    I: Integer;
    TerKind: TKMTerrainKind;
    TerKindFound: array[0..3] of Boolean;
  begin
    //get all 4 terkind for corners
    for I := 0 to 3 do
    begin
      TerKind := GetCornerTerKind(I, aCell, TerKindFound[I]);
      if TerKindFound[I] then
        aCornersTerKinds[I] := TerKind;
    end;

    //For corner, where no terkind from around tiles were found - replace it with neighbour corner terkind (there can't be 2 missed terkinds in a row)
    for I := 0 to 3 do
      if not TerKindFound[I] then
        aCornersTerKinds[I] := aCornersTerKinds[(I+1) mod 4];
  end;

  function HasCollision(aTerKind1, aTerKind2: TKMTerrainKind): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if aTerKind1 = aTerKind2 then Exit;

    Result := True;
    for I := Low(TERRAIN_EQUALITY_PAIRS) to High(TERRAIN_EQUALITY_PAIRS) do
    begin
      if (TERRAIN_EQUALITY_PAIRS[I].TK1 in [aTerKind1, aTerKind2])
        and (TERRAIN_EQUALITY_PAIRS[I].TK2 in [aTerKind1, aTerKind2]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  procedure ApplyMagicBrush(MaskKind: TKMTileMaskKind);
  var
    I: Integer;
    tileOwnTerKinds: TKMTerrainKindCorners;
    tileNodeTerKinds: TKMTerrainKindCorners;
    aroundTerKinds: TKMTerrainKindCorners;
    CollisionFound: Boolean;
    LayerOrder: array of TKMTileMaskInfo;
    MaskType: TKMTileMaskType;
  begin
    //Do not check tile node corners when using 'force paint' mode
    if not fOverrideCustomTiles then
    begin
      GetTileLandNodeTKinds(KMPoint(X,Y), tileNodeTerKinds);

      for I := 0 to 3 do
        if tileNodeTerKinds[I] = tkCustom then  // Do not set masks for tiles with at least 1 custom real corner
          Exit;
    end;

    GetTileOwnCornersTKinds(KMPoint(X,Y), tileOwnTerKinds);
    GetTerKindsAround(KMPoint(X,Y), aroundTerKinds);

    for I := 0 to 3 do
      if aroundTerKinds[I] = tkCustom then  // Do not set masks if at least 1 around corner is custom
        Exit;

    CollisionFound := False;
    for I := 0 to 3 do
      if HasCollision(tileOwnTerKinds[I], aroundTerKinds[I]) then
      begin
        CollisionFound := True;
        Break;
      end;

    if CollisionFound then  //Need to apply MagicBrush
      with gTerrain.Land^[Y,X] do
      begin
        SetLength(LayerOrder, 4);
        for I := 0 to 3 do
          LayerOrder[I].SubType := mstMain;

        MaskType := GetMaskType(aroundTerKinds, LayerOrder);

        BaseLayer.Terrain := BASE_TERRAIN[LayerOrder[0].TerKind];
        BaseLayer.Rotation := 0;
        BaseLayer.SetCorners(LayerOrder[0].Corners);
        LayersCnt := TILE_MASKS_LAYERS_CNT[MaskType] - 1;

        if MaskType = tmtNone then Exit;

        for I := 1 to LayersCnt do // start from 1, just for convinience
        begin
          Layer[I-1].Terrain := gRes.Sprites.GenTerrainTransitions[LayerOrder[I].TerKind, MaskKind, MaskType, LayerOrder[I].SubType];
          Layer[I-1].Rotation := LayerOrder[I].Rotation;
          Layer[I-1].SetCorners(LayerOrder[I].Corners);
        end;
      end
  end;

var
  L: Integer;
  genInfo: TKMGenTerrainInfo;
begin
  if not gTerrain.TileInMapCoords(X, Y) or (not fOverrideCustomTiles and gTerrain.Land^[Y,X].IsCustom) then Exit;

  if (aMaskKind = mkNone) and not fReplaceLayers then Exit;

  if aMaskKind <> mkNone then
    ApplyMagicBrush(aMaskKind);

  //No need to update BlendingLvl for basic tiles (without auto transitions)
  if gTerrain.Land^[Y,X].HasLayers then
    gTerrain.Land^[Y,X].BlendingLvl := fBlendingLvl;

  if fReplaceLayers then
  begin
    case aMaskKind of
      mkNone:  begin
                  gTerrain.Land^[Y,X].LayersCnt := 0; // Simple way to clear all layers
                  gTerrain.Land^[Y,X].BaseLayer.SetAllCorners;
                end;
      else      for L := 0 to gTerrain.Land^[Y,X].LayersCnt - 1 do
                begin
                  genInfo := gRes.Sprites.GetGenTerrainInfo(gTerrain.Land^[Y,X].Layer[L].Terrain);
                  if genInfo.Mask.Kind <> aMaskKind then
                    gTerrain.Land^[Y,X].Layer[L].Terrain :=
                      gRes.Sprites.GenTerrainTransitions[genInfo.TerKind, aMaskKind, genInfo.Mask.MType, genInfo.Mask.SubType];
                end;
    end;
  end;


end;


procedure TKMTerrainPainter.UpdateTempLand;
var
  I,J,L: Integer;
begin
  for I := 1 to gTerrain.MapY do
    for J := 1 to gTerrain.MapX do
    begin
      fTempLand[I,J].BaseLayer := gTerrain.Land^[I,J].BaseLayer;
      fTempLand[I,J].LayersCnt := gTerrain.Land^[I,J].LayersCnt;
      fTempLand[I,J].Height := gTerrain.Land^[I,J].Height;
      fTempLand[I,J].Obj := gTerrain.Land^[I,J].Obj;
      for L := 0 to 2 do
        fTempLand[I,J].Layer[L] := gTerrain.Land^[I,J].Layer[L];
    end;
end;


procedure TKMTerrainPainter.UseMagicBrush(X,Y,aSize: Integer; aSquare: Boolean; aAroundTiles: Boolean = False);
begin
  // update TempLand with data from actual Land array
  UpdateTempLand;

  fUseTempLand := aSize > 0; //Do not use temp land when size = 0
  fReplaceLayers := False;
  IterateOverArea(KMPoint(X,Y), aSize, aSquare, MagicBrush, aAroundTiles);
end;


// Set brush map ed params based on cursor (basically parames were set in mapEd GUI)
procedure TKMTerrainPainter.SetMapEdParams;
begin
  SetCommonParams(gCursor.Float.X, gCursor.Float.Y, gCursor.MapEdShape);
  SetBrushSpecialParams(gCursor.MapEdSize, TKMTerrainKind(gCursor.Tag1),
                        gCursor.MapEdRandomizeTiling, gCursor.MapEdOverrideCustomTiles,
                        gCursor.MapEdBrushMask, gCursor.MapEdBlendingLvl, gCursor.MapEdUseMagicBrush);

  SetHeightSpecialParams(gCursor.Mode = cmEqualize, ssLeft in gCursor.SState,
                            gCursor.MapEdSize, gCursor.MapEdSlope, gCursor.MapEdSpeed);
end;


procedure TKMTerrainPainter.SetCommonParams(X, Y: Single; aShape: TKMMapEdShape);
begin
  //Cell below cursor
  fMapXc := EnsureRange(Round(X + 0.5), 1, gTerrain.MapX);
  fMapYc := EnsureRange(Round(Y + 0.5), 1, gTerrain.MapY);

  //Node below cursor
  fMapXn := EnsureRange(Round(X + 1), 1, gTerrain.MapX);
  fMapYn := EnsureRange(Round(Y + 1), 1, gTerrain.MapY);

  fShape := aShape;
end;


procedure TKMTerrainPainter.SetHeightSpecialParams(aIsEqualize, aRaise: Boolean; aSize, aSlope, aSpeed: Integer);
begin
  fIsEqualize := aIsEqualize;
  fRaise := aRaise;
  fSize := EnsureRange(aSize, 0, 100);
  fSlope := EnsureRange(aSlope, 0, 255);
  fSpeed := EnsureRange(aSpeed, 0, 255);
end;


procedure TKMTerrainPainter.SetBrushSpecialParams(aSize: Integer; aTerKind: TKMTerrainKind; aRandomTiles, aOverrideCustomTiles: Boolean;
                                                  aBrushMask: TKMTileMaskKind; aBlendingLvl: Integer; aUseMagicBrush: Boolean);
begin
  fSize := EnsureRange(aSize, 0, MAPED_BRUSH_MAX_SIZE);
  fTerKind := aTerKind;
  fRandomizeTiling := aRandomTiles;
  fOverrideCustomTiles := aOverrideCustomTiles;
  fBrushMask := aBrushMask;
  fBlendingLvl := EnsureRange(aBlendingLvl, 0, TERRAIN_MAX_BLendING_LEVEL);
  fUseMagicBrush := aUseMagicBrush;
end;


procedure TKMTerrainPainter.SetBrushParams(X, Y: Single; aShape: TKMMapEdShape; aSize: Integer;
                                           aTerKind: TKMTerrainKind; aRandomTiles, aOverrideCustomTiles: Boolean;
                                           aBrushMask: TKMTileMaskKind = mkNone; aBlendingLvl: Integer = TERRAIN_DEF_BLendING_LVL;
                                           aUseMagicBrush: Boolean = False);
begin
  SetCommonParams(X, Y, aShape);
  SetBrushSpecialParams(aSize, aTerKind, aRandomTiles, aOverrideCustomTiles, aBrushMask, aBlendingLvl, aUseMagicBrush);
end;


procedure TKMTerrainPainter.SetHeightParams(X, Y: Single; aShape: TKMMapEdShape; aSize: Integer;
                                            aIsEqualize, aRaise: Boolean; aSlope, aSpeed: Byte);
begin
  SetCommonParams(X, Y, aShape);
  SetHeightSpecialParams(aIsEqualize, aRaise, aSize, aSlope, aSpeed);
end;


procedure TKMTerrainPainter.ApplyBrush;
begin
  if fUseMagicBrush then
  begin
    fUseTempLand := False;
    fReplaceLayers := True;
    IterateOverArea(KMPoint(fMapXc, fMapYc), fSize, fShape = hsSquare, MagicBrush);
  end else
    DoApplyBrush;
end;



procedure TKMTerrainPainter.DoApplyBrush;
var
  X, Y: Integer;
  rect: TKMRect;
begin
  // Clear fBrushAreaTerKind array. It will be refilled in BrushTerrainTile
  fBrushAreaTerKindCnt := 0;

  IterateOverArea(KMPoint(fMapXc,fMapYc), fSize, fShape = hsSquare, BrushTile);

  if fSize = 0 then
  begin
    X := fMapXn;
    Y := fMapYn;
  end else begin
    X := fMapXc;
    Y := fMapYc;
  end;
  RebuildMap(X, Y, fSize, fShape = hsSquare, True);

  if fBrushMask <> mkNone then
    UseMagicBrush(X, Y, fSize, (fShape = hsSquare), True);

  rect := KMRectGrow(KMRect(KMPoint(fMapXc, fMapYc)), (fSize div 2) + 1);
  gTerrain.UpdatePassability(rect);
  gTerrain.UpdateLighting(rect); //Also update lighting because of water
end;

procedure TKMTerrainPainter.ApplyTileBrush;
var
  X, Y: Integer;
  rect: TKMRect;
begin
  X := fMapXc;
  Y := fMapYc;

  IterateOverArea(KMPoint(X, Y), fSize, fShape = hsSquare, DoApplyTileBrush);

  rect := KMRectGrow(KMRect(KMPoint(X, Y)), (fSize div 2) + 1);
  gTerrain.UpdatePassability(rect);
  gTerrain.UpdateLighting(rect); //Also update lighting because of water
end;

procedure TKMTerrainPainter.DoApplyTileBrush(const X, Y: Integer);
begin
  if gCursor.MapEdDir in [0..3] then //Defined direction
    EditTile(KMPoint(X, Y), gCursor.Tag1, gCursor.MapEdDir)
  else //Random direction
    EditTile(KMPoint(X, Y), gCursor.Tag1, KaMRandom(4, 'TKMTerrainPainter.UpdateStateIdle'));
end;


procedure TKMTerrainPainter.ApplyObjectsBrush;
var
  rect: TKMRect;
begin
  if (fLastPosition <> KMPoint(fMapXc,fMapYc)) then
  begin
    fLastPosition := KMPoint(fMapXc,fMapYc);
    IterateOverArea(KMPoint(fMapXc,fMapYc), fSize, fShape = hsSquare, ApplyBrushObjects);
    rect := KMRectGrow(KMRect(fLastPosition), (fSize div 2) + 1);
    gTerrain.UpdatePassability(rect);
  end;
end;

procedure TKMTerrainPainter.ApplySelectionBrush;
begin
  IterateOverArea(KMPoint(fMapXc,fMapYc), fSize, fShape = hsSquare, ApplyBrushSelection);
end;

procedure TKMTerrainPainter.ApplyBrushSelection(const X: Integer; const Y: Integer);
begin
  if not gTerrain.TileInMapCoords(X, Y) then
    Exit;
  if (gCursor.Tag1 >= 2)and CanEditTile(X,Y) then
  begin
    gTerrain.Land^[Y,X].IsHidden := gCursor.Tag1 = 2;
  end else
  begin
    if (gCursor.Tag1 = 0) and not CanEditTile(X,Y) then Exit;

    gTerrain.Land^[Y,X].TileSelected := gCursor.Tag1 = 0

  end;


end;

procedure TKMTerrainPainter.BrushObjects(const X, Y: Integer; aUseLandTKind: Boolean = True; aTerKind: TKMTerrainKind = tkCustom);

  Function HasTreeNearby : Boolean;
  var I, K : Integer;
  begin
    Result := false;
    for I := -1 to 1 do
      for K := -1 to 1 do
        if gTerrain.TileInMapCoords(X + K, Y + I) then
          if InRange(gTerrain.Land^[Y + I, X + K].Obj, 88, 206) then
            Exit(true);


  end;

var
  key, randomObject: Integer;
  OT: TKMTerrainObjectType;
begin
  if not gTerrain.TileInMapCoords(X, Y) then
    Exit;

  if not CanEditTile(X, Y) then
    Exit;

  if gCursor.MapEdCleanBrush then
     gTerrain.Land^[Y, X].Obj := OBJ_NONE
  else
  begin
    if gCursor.MapEdOverrideObjects then
      gTerrain.Land^[Y, X].Obj := OBJ_NONE;

    for OT := Low(TKMTerrainObjectType) to High(TKMTerrainObjectType) do
      if gCursor.MapEdObjectsType[OT] then
      begin
        if OT = otTrees then
          if HasTreeNearby then
            Continue;

        key := KaMRandom(400 div gCursor.MapEdObjectsDensity, 'TKMTerrainPainter.BrushObjects');
        if key < 2 then
        begin
          if aUseLandTKind then
            aTerKind := LandTerKind[Y, X].TerKind;

          randomObject := PickRandomObject(aTerKind, OT, X, Y);
          if gCursor.MapEdOverrideObjects then
            gTerrain.Land^[Y, X].Obj := randomObject
          else
            if gTerrain.Land^[Y, X].Obj = OBJ_NONE then
              gTerrain.Land^[Y, X].Obj := randomObject;
        end;
      end;
  end;
end;


procedure TKMTerrainPainter.ApplyConstHeight;
var
  I, K, size: Integer;
  R: TKMRect;
begin
  if fLastPosition <> KMPoint(Max(fMapXn, 1), Max(fMapYn, 1)) then
  begin
    size := fSize - 1;
    fLastPosition := KMPoint(Max(fMapXn, 1), Max(fMapYn, 1));
    for I := Max(fMapYn - size, 1) to Min(fMapYn + size, gTerrain.MapY) do
      for K := Max(fMapXn - size, 1) to Min(fMapXn + size, gTerrain.MapX) do
        begin
          case fShape of
            hsCircle: if Sqr(I - fMapYn) + Sqr(K - fMapXn) <= Sqr(size) then
                      begin
                        gTerrain.Land[I,K].Height := gCursor.MapEdConstHeight;   // Negative number means that point is outside circle
                        gTerrain.UpdateRenderHeight(K, I);
                      end;
            hsSquare: begin
                        gTerrain.Land[I,K].Height := gCursor.MapEdConstHeight;
                        gTerrain.UpdateRenderHeight(K, I);
                      end;
          end;
      end;

    R := KMRectGrow(KMRect(KMPointF(fMapXn, fMapYn)), fSize);
    gTerrain.UpdateLighting(R);
    gTerrain.UpdatePassability(R);
  end;
end;

procedure TKMTerrainPainter.ApplyElevateKind(aTerKind: TKMTerrainKind);
var
  aX, aY: Integer;
begin
    if aTerKind <> tkCustom then
    begin
      for aY := 0 to gTerrain.MapY do
        for aX := 0 to gTerrain.MapX do
          if LandTerKind[aY, aX].TerKind = aTerKind then
          begin
            // Inc or Dec height by 1
            gTerrain.Land[aY,aX].Height := EnsureRange(gTerrain.Land[aY,aX].Height + (Byte(fRaise) * 2 - 1), 0, HEIGHT_MAX);
            gTerrain.LandExt[aY,aX].RenderHeight := gTerrain.Land[aY,aX].GetRenderHeight;
          end;

      gTerrain.UpdateLighting;
      gTerrain.UpdatePassability;
    end;
end;

procedure TKMTerrainPainter.ApplyHeight;
var
  I, K: Integer;
  tmp, base: Single;
  R: TKMRect;
begin
  for I := Max(fMapYn - fSize, 1) to Min(fMapYn + fSize, gTerrain.MapY) do
  for K := Max(fMapXn - fSize, 1) to Min(fMapXn + fSize, gTerrain.MapX) do
  begin
    base := -1;
    // We have square area basing on mouse point +/- radius
    // Now we need to check whether point is inside brush type area(circle etc.)
    // Every MapEdShape case has it's own check routine
    case fShape of
      hsCircle: tmp := Max((1 - GetLength(I - fMapYn, K - fMapXn) / fSize), 0);   // Negative number means that point is outside circle
      hsSquare: tmp := 1 - Max(Abs(I - fMapYn), Abs(K - fMapXn)) / fSize;
      else      tmp := 0;
    end;

    // Default cursor mode is elevate/decrease
    if fIsEqualize then
    begin // START Unequalize
      if fRaise then
      begin
        if (I > 1) and (K >= 1) and (I < gTerrain.MapY) and (K < gTerrain.MapX) then
        begin
          // Unequalize compares heights of adjacent tiles and increases differences
          if (gTerrain.Land^[I,K].Height < gTerrain.Land^[I-1,K+1].Height) then
            tmp := -Min(gTerrain.Land^[I-1,K+1].Height - gTerrain.Land^[I,K].Height, tmp)
          else
          if (gTerrain.Land^[I,K].Height > gTerrain.Land^[I-1,K+1].Height) then
            tmp := Min(gTerrain.Land^[I,K].Height - gTerrain.Land^[I-1,K+1].Height, tmp)
          else
          if tmp <> 0 then // Tmp = 0 outside of hsCircle area
            //Add random value (-1/0/1) so absolutely flat surface will be unequlized too
            tmp := KaMRandom(2, 'TKMTerrainPainter.ApplyHeight')*3 - 2;
        end
        else
          tmp := 0;
       //end Unequalize
      end else
      // START Flatten
      begin
        //Base value for flatten terrain
        base := gTerrain.Land^[fMapYn, fMapXn].Height;

        //Flatten compares heights of mouse click and active tile then it increases/decreases height of active tile
        if (gTerrain.Land^[I,K].Height < base) then
          tmp := - Min(base - gTerrain.Land^[I,K].Height, tmp)
        else
        if (gTerrain.Land^[I,K].Height > base) then
          tmp := Min(gTerrain.Land^[I,K].Height - base, tmp)
        else
          tmp := 0;
      end;
      //end Flatten
    end;
    //COMMON PART FOR Elevate/Lower and Unequalize/Flatten
    //Compute resulting floating-point height
    tmp := Power(Abs(tmp),(fSlope+1)/6)*Sign(tmp); //Modify slopes curve
    tmp := tmp * (4.75/14*(fSpeed - 1) + 0.25);
    tmp := EnsureRange(gTerrain.Land^[I,K].Height + LandTerKind[I,K].HeightAdd/255 + tmp * (Byte(fRaise)*2 - 1), 0, HEIGHT_MAX); // (Byte(aRaise)*2 - 1) - LeftButton pressed it equals 1, otherwise equals -1

    //For flatten only
    if (base <> -1) then
    begin
      //We do not want to height jump over base value around
      //Problem is if one time H > base, next time H < base and so on in infinite loop
      //then those dots will be seen as flickering (shaking / trembling) which is not looking good
      //So in case we come so close to base value, so we overhead it, then just cut it to base value
      if ((gTerrain.Land^[I,K].Height >= base) and (tmp < base))
        or ((gTerrain.Land^[I,K].Height <= base) and (tmp > base)) then
        tmp := base;
    end;

    gTerrain.Land^[I,K].Height := Trunc(tmp);
    gTerrain.UpdateRenderHeight(K, I);
    LandTerKind[I,K].HeightAdd := Round(Frac(tmp)*255); //write Fractional part in 0..255 range (1Byte) to save us mem
  end;

  R := KMRectGrow(KMRect(KMPointF(fMapXn, fMapYn)), fSize);
  gTerrain.UpdateLighting(R);
  gTerrain.UpdatePassability(R);
end;


procedure TKMTerrainPainter.EditTile(const aLoc: TKMPoint; aTile: Word; aRotation: Byte; aIsCustom: Boolean = True);
begin
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  if not CanEditTile(aLoc.X, aLoc.Y) then Exit;
  
  gTerrain.Land^[aLoc.Y, aLoc.X].IsCustom := aIsCustom;
  gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain := aTile;
  gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.SetAllCorners;
  gTerrain.Land^[aLoc.Y, aLoc.X].LayersCnt := 0;
  gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation := aRotation;

  gTerrain.UpdatePassability(aLoc);
  gTerrain.UpdateLighting(aLoc.X, aLoc.Y); //Also update lighting because of water
end;


procedure TKMTerrainPainter.RMG2MapEditor(X,Y: Integer; aTile: Word);
begin
  LandTerKind[Y,X].TerKind := gRes.Tileset[aTile].TerKinds[0];
end;


procedure TKMTerrainPainter.MagicWater(const aLoc: TKMPoint);
type
  TMagicType = (mtNone, mtWater, mtShore, mtIce);
var
  filledTiles: array of array of TMagicType;

  function CanRotate(aTileID: Word): Boolean;
  begin
    Result := (gRes.Tileset[aTileID].Water
              and not (aTileID in [114, 115, 119, 194, 200, 210, 211, 235, 236]))
              or (gRes.Tileset[aTileID].Ice
              and not (aTileID in [4, 10, 12, 22, 23]));
  end;

  procedure MagicFillArea(X, Y: Word);
  begin
    if filledTiles[Y, X] <> mtNone then
      Exit;

    //Detect rotateable shores
    if (gTerrain.Land^[y,x].BaseLayer.Terrain in [126, 127]) then
      filledTiles[y,x] := mtShore;

    //Detect full ice tiles
    if (gTerrain.Land^[y, x].BaseLayer.Terrain = 44) then
      filledTiles[y, x] := mtIce;

    //Detect water
    if CanRotate(gTerrain.Land^[y,x].BaseLayer.Terrain) then
    begin
      filledTiles[y,x] := mtWater;

      if x - 1 >= 1 then
      begin
        if y - 1 >= 1 then              MagicFillArea(x - 1, y - 1);
                                        MagicFillArea(x - 1, y  );
        if y + 1 <= gTerrain.MapY then  MagicFillArea(x - 1, y + 1);
      end;

      if y - 1 >= 1 then                MagicFillArea(x, y - 1);
      if y + 1 <= gTerrain.MapY then    MagicFillArea(x, y + 1);

      if x + 1 <= gTerrain.MapX then
      begin
        if y - 1 >= 1 then              MagicFillArea(x + 1, y - 1);
                                        MagicFillArea(x + 1, y  );
        if y + 1 <= gTerrain.MapY then  MagicFillArea(x + 1, y + 1);
      end;
    end;
  end;

var
  I, K: Integer;
  newRot: Byte;
begin
  if not CanRotate(gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain) then
    Exit;

  SetLength(filledTiles, gTerrain.MapY+1, gTerrain.MapX+1);

  MagicFillArea(aLoc.X,aLoc.Y);

  newRot := (gTerrain.Land^[aLoc.Y,aLoc.X].BaseLayer.Rotation + 1) mod 4;
  for I := 1 to gTerrain.MapY do
    for K := 1 to gTerrain.MapX do
      case filledTiles[I,K] of
        mtWater,
        mtIce:    begin
                    gTerrain.Land^[I,K].BaseLayer.Rotation := newRot;
                  end;
        mtShore:  begin
                    //These shores can be flipped
                    if (gTerrain.Land^[I,K].BaseLayer.Terrain in [126, 127]) then
                      case gTerrain.Land^[I,K].BaseLayer.Rotation of
                        0: if newRot = 3 then gTerrain.Land^[I,K].BaseLayer.Terrain := 126 else
                           if newRot = 1 then gTerrain.Land^[I,K].BaseLayer.Terrain := 127;

                        1: if newRot = 0 then gTerrain.Land^[I,K].BaseLayer.Terrain := 126 else
                           if newRot = 2 then gTerrain.Land^[I,K].BaseLayer.Terrain := 127;

                        2: if newRot = 1 then gTerrain.Land^[I,K].BaseLayer.Terrain := 126 else
                           if newRot = 3 then gTerrain.Land^[I,K].BaseLayer.Terrain := 127;

                        3: if newRot = 2 then gTerrain.Land^[I,K].BaseLayer.Terrain := 126 else
                           if newRot = 0 then gTerrain.Land^[I,K].BaseLayer.Terrain := 127;
                      end;
                  end;
      end;
  gGame.MapEditor.History.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_MAGIC_WATER] + ' ' + aLoc.ToString);
end;


procedure TKMTerrainPainter.GenerateAddnData;
const
  SPECIAL_TILES: array[0..28] of Word = ( 24, 25,194,198,199,202,206,207,214,216,217,218,219,221,222,223,224,225,226,227,228,
                                         229,230,231,232,233,246,264,265); //Waterfalls and bridges
  OTHER_WATER_TILES = [193,208,209,240,244]; //Water tiles not used in painting (fast, straight, etc.)
  //Accuracies
  ACC_MAX = 5;  //Special tiles
  ACC_HIGH = 4; //Primary tiles
  ACC_MED = 3; //Random tiling
  ACC_LOW = 2; //Edges
  ACC_MIN = 1; //Coal random tiling (edges are better in this case)
  ACC_NONE = 0;
var
  accuracy: array of array of Byte;

  procedure SetTerrainKindVertex(X,Y: Integer; T: TKMTerrainKind; aAccuracy: Byte);
  begin
    if not gTerrain.TileInMapCoords(X,Y) then Exit;

    //Special rules to fix stone hill corners:
    // - Never overwrite tkStone with tkGrass
    // - Always allow tkStone to overwrite tkGrass
    if (fLandTerKind[Y,X].TerKind = tkStone) and (T = tkGrass) then Exit;
    if (fLandTerKind[Y,X].TerKind = tkGrass) and (T = tkStone) then aAccuracy := ACC_MAX;

    //Skip if already set more accurately
    if aAccuracy < accuracy[Y,X] then Exit;

    fLandTerKind[Y,X].TerKind := T;
    accuracy[Y,X] := aAccuracy;
  end;

  procedure SetTerrainKindTile(X,Y: Integer; T: TKMTerrainKind; aAccuracy: Byte);
  begin
    SetTerrainKindVertex(X  , Y  , T, aAccuracy);
    SetTerrainKindVertex(X+1, Y  , T, aAccuracy);
    SetTerrainKindVertex(X  , Y+1, T, aAccuracy);
    SetTerrainKindVertex(X+1, Y+1, T, aAccuracy);
  end;

var
  I, K, J, rot: Integer;
  A: Byte;
  T, T2: TKMTerrainKind;
begin
  SetLength(accuracy, gTerrain.MapY+1, gTerrain.MapX+1);

  for I := 1 to gTerrain.MapY do
  for K := 1 to gTerrain.MapX do
  begin
    fLandTerKind[I,K].TerKind := tkCustom; //Everything custom by default
    accuracy[I,K] := ACC_NONE;
  end;

  for I := 1 to gTerrain.MapY do
  for K := 1 to gTerrain.MapX do
    //Special tiles such as bridges should remain as tkCustom
    if ArrayContains(gTerrain.Land^[I,K].BaseLayer.Terrain, SPECIAL_TILES) then
      SetTerrainKindTile(K, I, tkCustom, ACC_MAX) //Maximum accuracy
    else
      //Water tiles not used in painting (fast, straight, etc.)
      if gTerrain.Land^[I,K].BaseLayer.Terrain in OTHER_WATER_TILES then
        SetTerrainKindTile(K, I, tkWater, ACC_MED) //Same accuracy as random tiling (see below)
      else
        for T := Low(TKMTerrainKind) to High(TKMTerrainKind) do
          if T <> tkCustom then
          begin
            //METHOD 1: Terrain type is the primary tile for this terrain
            if gTerrain.Land^[I,K].BaseLayer.Terrain = Abs(Combo[T,T,1]) then
            begin
              SetTerrainKindTile(K, I, T, ACC_HIGH);
              Break; //Neither of the methods below can beat this one, so save time and don't check more TerrainKinds
            end;

            //METHOD 2: Terrain type is in RandomTiling
            for J := 1 to RandomTiling[T,0] do
              if gTerrain.Land^[I,K].BaseLayer.Terrain = RandomTiling[T,J] then
              begin
                A := ACC_MED; //Random tiling is fairly accurate
                if T = tkCoal then A := ACC_MIN; //Random coal tiles are also used for edges, so edges are more accurate
                SetTerrainKindTile(K, I, T, A);
              end;

            //METHOD 3: Edging data
            A := ACC_LOW; //Edging data is not as accurate as other methods (some edges reuse the same tiles)
            for T2 := Low(TKMTerrainKind) to High(TKMTerrainKind) do
            begin
              //1 vertex is T, 3 vertexes are T2
              if gTerrain.Land^[I,K].BaseLayer.Terrain = Abs(Combo[T,T2,1]) then
              begin
                rot := gTerrain.Land^[I,K].BaseLayer.Rotation mod 4;
                if Combo[T,T2,1] < 0 then rot := (rot+2) mod 4; //Flip
                case rot of
                  0: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                  1: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  2: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                  3: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                end;
              end;
              //Half T, half T2
              if gTerrain.Land^[I,K].BaseLayer.Terrain = Abs(Combo[T,T2,2]) then
              begin
                rot := gTerrain.Land^[I,K].BaseLayer.Rotation mod 4;
                if Combo[T,T2,2] < 0 then rot := (rot+2) mod 4; //Flip
                case rot of
                  0: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                  1: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  2: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  3: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                end;
              end;
              //3 vertex are T, 1 vertexes is T2
              if gTerrain.Land^[I,K].BaseLayer.Terrain = Abs(Combo[T,T2,3]) then
              begin
                rot := gTerrain.Land^[I,K].BaseLayer.Rotation mod 4;
                if Combo[T,T2,3] < 0 then rot := (rot+2) mod 4; //Flip
                case rot of
                  0: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T2, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  1: begin
                       SetTerrainKindVertex(K,   I,   T2, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  2: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T2, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T, A);
                     end;
                  3: begin
                       SetTerrainKindVertex(K,   I,   T, A);
                       SetTerrainKindVertex(K+1, I,   T, A);
                       SetTerrainKindVertex(K,   I+1, T, A);
                       SetTerrainKindVertex(K+1, I+1, T2, A);
                     end;
                end;
              end;
            end;
          end;
end;


procedure TKMTerrainPainter.InitSize(X, Y: Word);
begin
  fBrushAreaTerKindCnt := 0;

  SetLength(fLandTerKind, Y+1, X+1);
  LandTerKind := fLandTerKind;
  SetLength(fTempLand, Y+1, X+1);
  SetLength(fBrushAreaTerKind, Sqr(MAPED_BRUSH_MAX_SIZE+1));
end;


procedure TKMTerrainPainter.InitEmpty;
var
  I, K: Integer;
begin
  InitSize(gTerrain.MapX, gTerrain.MapY);

  //Fill in default terain type - Grass
  for I := 1 to gTerrain.MapY do
    for K := 1 to gTerrain.MapX do
      fLandTerKind[I,K].TerKind := tkGrass;
end;


procedure TKMTerrainPainter.SetMainLandTerKind;
begin
  if Self = nil then Exit;

  LandTerKind := fLandTerKind;
end;


//Skip the KaM data and load MapEd vertice info
procedure TKMTerrainPainter.LoadFromFile(const aFileName: UnicodeString);
var
  I, K: Integer;
  terType: ShortInt; //Krom's editor saves terrain kind as ShortInt
  S: TKMemoryStream;
  newX, newY: Integer;
  resHead: packed record
    x1: Word;
    Allocated, Qty1, Qty2, x5, Len17: Integer;
  end;
  chunk: AnsiString;
  mapEdChunkFound: Boolean;
  useKaMFormat: Boolean;
  gameRev: Integer;
  mapDataSize: Cardinal;
begin
  if not FileExists(aFileName) then Exit;

  InitSize(gTerrain.MapX, gTerrain.MapY);

  S := TKMemoryStreamBinary.Create;
  try
    S.LoadFromFile(aFileName);

    LoadMapHeader(S, newX, newY, gameRev, mapDataSize);
    useKaMFormat := ( gameRev = 0 );

    //Skip terrain data
    if useKaMFormat then
      S.Position := S.Position + 23 * newX * newY
    else
      S.Position := S.Position + mapDataSize;

    //For now we just throw away the resource footer because we don't understand it (and save a blank one)
    if useKaMFormat then
    begin
      S.Read(resHead, 22);
      S.Position := S.Position + 17 * resHead.Allocated;
    end;

    //ADDN
    mapEdChunkFound := False;
    if S.Position < S.Size then
    begin
      chunk := '    ';
      S.Read(chunk[1], 4);
      if chunk = 'ADDN' then
      begin
        S.Read(chunk[1], 4);
        if chunk = 'TILE' then
        begin
          S.Read(I, 4); //Chunk size
          S.Read(I, 4); //Cypher - ommited
          for I := 1 to newY do
          for K := 1 to newX do
          begin
            //Krom's editor saves negative numbers for tiles placed manually
            S.Read(terType, 1);
            fLandTerKind[I,K].TerKind := GetTerKind(terType, useKaMFormat);
          end;
          mapEdChunkFound := True; //Only set it once it's all loaded successfully
        end
        else
          gLog.AddNoTime(aFileName + ' has no MapEd.TILE chunk');
      end
      else
        gLog.AddNoTime(aFileName + ' has no MapEd.ADDN chunk');
    end
    else
      gLog.AddNoTime(aFileName + ' has no MapEd chunk');
  finally
    S.Free;
  end;

  //We can regenerate the MapEd data if it's missing (won't be as good as the original)
  if not mapEdChunkFound then
  begin
    gLog.AddNoTime('Regenerating missing MapEd data as best as we can');
    GenerateAddnData;
  end;
end;


procedure TKMTerrainPainter.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  for I := 1 to gTerrain.MapY do
    SaveStream.Write(fLandTerKind[I,1], SizeOf(fLandTerKind[I,1]) * gTerrain.MapX);
end;


procedure TKMTerrainPainter.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  InitSize(gTerrain.MapX, gTerrain.MapY);
  for I := 1 to gTerrain.MapY do
    LoadStream.Read(fLandTerKind[I,1], SizeOf(fLandTerKind[I,1]) * gTerrain.MapX);
end;


procedure TKMTerrainPainter.SaveToFile(const aFileName: UnicodeString);
begin
  SaveToFile(aFileName, KMRECT_ZERO);
end;


procedure TKMTerrainPainter.SaveToFile(const aFileName: UnicodeString; const aInsetRect: TKMRect);
var
  I, K, ifrom, KFrom: Integer;
  S: TKMemoryStream;
  newX, newY: Integer;
  resHead: packed record
    x1: Word;
    Allocated, Qty1, Qty2, x5, Len17: Integer;
  end;
  useKaMFormat: Boolean;
  gameRev: Integer;
  mapDataSize: Cardinal;
begin
  if not FileExists(aFileName) then Exit;

  S := TKMemoryStreamBinary.Create;
  try
    S.LoadFromFile(aFileName);

    LoadMapHeader(S, newX, newY, gameRev, mapDataSize);

    useKaMFormat := ( gameRev = 0 );

    //Skip terrain data
    if useKaMFormat then
      S.Position := S.Position + 23 * newX * newY
    else
      S.Position := S.Position + mapDataSize;

    //For now we just throw away the resource footer because we don't understand it (and save a blank one)
    if useKaMFormat then
    begin
      S.Read(resHead, 22);
      S.Position := S.Position + 17 * resHead.Allocated;
    end;

    S.Write(AnsiString('ADDN')[1], 4);
    S.Write(AnsiString('TILE')[1], 4);

    S.Write(Integer(newX * newY)); //Chunk size
    S.Write(Integer(0)); //Cypher - ommited
    for I := 1 to newY do
    begin
      ifrom := EnsureRange(I - aInsetRect.Top, 1, gTerrain.MapY - 1);
//      ifrom := EnsureRange(I - aInsetRect.Top, 1, aInsetRect.Top + gTerrain.MapY - 1);
      for K := 1 to newX do
      begin
        KFrom := EnsureRange(K - aInsetRect.Left, 1, gTerrain.MapX - 1);
//        KFrom := EnsureRange(K - aInsetRect.Left, 1, aInsetRect.Left + gTerrain.MapX - 1);
        if (ifrom <> I - aInsetRect.Top)
          or (KFrom <> K - aInsetRect.Left) then
            S.Write(Byte(tkGrass)) // Its ok if we fill all with grass
        else
          S.Write(fLandTerKind[ifrom,KFrom].TerKind, 1);
      end;
    end;

    S.SaveToFile(aFileName);
  finally
    S.Free;
  end;
end;


procedure TKMTerrainPainter.Eyedropper(const aLoc: TKMPoint);
begin
  //Save specified loc's terrain info
  gCursor.Tag1 := gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Terrain;
  gCursor.MapEdDir := gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation;
end;


procedure TKMTerrainPainter.RotateTile(const aLoc: TKMPoint);

  procedure RotateCorners(var aLayer: TKMTerrainLayer);
  var
    tmp: Byte;
  begin
    tmp := aLayer.Corners;
    aLayer.Corners := ((tmp shl 1) and $F) or (tmp shr 3);
  end;

var
  L: Integer;
begin
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;

  gTerrain.Land^[aLoc.Y, aLoc.X].IsCustom := True;
  gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation := (gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer.Rotation + 1) mod 4;
  RotateCorners(gTerrain.Land^[aLoc.Y, aLoc.X].BaseLayer);

  for L := 0 to gTerrain.Land^[aLoc.Y, aLoc.X].LayersCnt - 1 do
  begin
    gTerrain.Land^[aLoc.Y, aLoc.X].Layer[L].Rotation := (gTerrain.Land^[aLoc.Y, aLoc.X].Layer[L].Rotation + 1) mod 4;
    RotateCorners(gTerrain.Land^[aLoc.Y, aLoc.X].Layer[L]);
  end;

  gTerrain.UpdatePassability(aLoc);
  gGame.MapEditor.History.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_ROTATE_TILE] + ' ' + aLoc.ToString);
end;


procedure TKMTerrainPainter.UpdateStateIdle;
begin
  case gCursor.Mode of
    cmElevate,
    cmEqualize:     if (ssLeft in gCursor.SState) or (ssRight in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyHeight;
                    end;
    cmConstHeight:     if (ssLeft in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyConstHeight;
                    end;
    cmElevateAll:     if (ssLeft in gCursor.SState) or (ssRight in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyElevateKind(LandTerKind[fMapYc, fMapXc].TerKind);
                    end;
    cmBrush:        if (ssLeft in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyBrush;
                    end;
    cmTiles:        if (ssLeft in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyTileBrush;
                      {if gCursor.MapEdDir in [0..3] then //Defined direction
                        EditTile(gCursor.Cell, gCursor.Tag1, gCursor.MapEdDir)
                      else //Random direction
                        EditTile(gCursor.Cell, gCursor.Tag1, KaMRandom(4, 'TKMTerrainPainter.UpdateStateIdle'));}
                    end;
    cmObjects:      if ssLeft in gCursor.SState then
                      if CanEditTile(gCursor.Cell.X, gCursor.Cell.Y) then
                      if gCursor.MapEdOverrideObjectsSingle or (gTerrain.Land^[gCursor.Cell.Y, gCursor.Cell.X].Obj = OBJ_NONE) then
                       gTerrain.SetObject(gCursor.Cell, gCursor.Tag1);
    cmWaresOnGround:  begin
                        if ssLeft in gCursor.SState then
                          if CanEditTile(gCursor.Cell.X, gCursor.Cell.Y) then
                          gTerrain.SetWareOnGround(gCursor.Cell, TKMWareType(gCursor.Tag1), gCursor.MapEd_WaresCount);

                      end;

    cmObjectsBrush: if (ssLeft in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplyObjectsBrush;
                    end;

    cmTileSelection: if (ssLeft in gCursor.SState) then
                    begin
                      SetMapEdParams; //Set mapEd params from gGameCursor
                      ApplySelectionBrush;
                    end;

    cmOverlays:     if (ssLeft in gCursor.SState) then
                    begin
                      If gCursor.MapEdApplyOverlayOnRoad then
                        If gTerrain.TileHasRoad(gCursor.Cell) then
                          gTerrain.SetOverlay(gCursor.Cell, TKMTileOverlay(gCursor.Tag1), false) //Holding shift allows overwrite roads
                        else
                      else
                        gTerrain.SetOverlay(gCursor.Cell, TKMTileOverlay(gCursor.Tag1), ssShift in gCursor.SState); //Holding shift allows overwrite roads
                    end;
  end;
end;


procedure TKMTerrainPainter.UpdateState;
begin
//  case gGameCursor.Mode of
//    cmBrush:      if (ssLeft in gGameCursor.SState) then
//                  begin
//                    if gGameCursor.MapEdMagicBrush then
//                    begin
//                      UpdateTempLand;
//                      MagicBrush(gGameCursor.Cell, False);
//                    end else
//                      EditBrush(gGameCursor.Cell);
//                  end;
//  end;
end;


end.


