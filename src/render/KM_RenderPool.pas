unit KM_RenderPool;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  KM_Defaults, KM_CommonTypes, KM_CommonClasses, KM_Pics, KM_Points, KM_Render, KM_Viewport,
  KM_RenderTerrain, KM_ResHouses, KM_ResSprites, KM_Units, KM_HandEntity,
  KM_Houses, KM_Terrain, KM_CommonGameTypes, KM_RenderDebug,
  KM_ResTypes, KM_ResDevelopment;

type
  TKMPaintLayer = (plTerrain, plObjects, plCursors);

  TKMRenderSprite = record
    Loc: TKMPointF; // Where sprite lower-left corner is located
    Feet: TKMPointF; // Feet of the sprite for FOW calculation (X;Y) and Z ordering (Y only)
    RX: TRXType;
    ID: Integer;
    UID: Integer;
    NewInst: Boolean;
    TeamColor, HighlightColor: Cardinal;
    AlphaStep: Single; // Only apply-able to HouseBuild
    SelectionRect: TKMRectF; // Used for selecting units by sprite
    NightLoc : TKMPoint;
  end;

  // List of sprites prepared to be rendered
  TKMRenderList = class
  private
    fUnitsRXData: TRXData; //shortcut
    fCount: Word;
    fRenderOrder: array of Word; // Order in which sprites will be drawn ()
    fFrontRenderList: array of TKMRenderSprite;
    fRenderList: array of TKMRenderSprite;

    fDbgSpritesQueued: Word;
    fDbgSpritesDrawn: Word;

    procedure ClipRenderList;
    procedure SendToRender(aId: Integer; aFrontList : Boolean = false);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSprite(aRX: TRXType; aID: Integer; pX,pY: Single; nX, nY : Integer; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteFront(aRX: TRXType; aID: Integer; pX,pY: Single; nX, nY : Integer; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteG(aRX: TRXType; aID: Integer; aUID: Integer; pX,pY,gX,gY: Single; nX, nY : Integer; aTeam: Cardinal = $0; aAlphaStep: Single = -1; aHighlight: Cardinal = $0);

    property DbgSpritesQueued: Word read fDbgSpritesQueued;
    property DbgSpritesDrawn: Word read fDbgSpritesDrawn;

    function GetSelectionUID(const aCurPos: TKMPointF): Integer;
    procedure Clear;
    procedure SortRenderList;
    procedure Render;
  end;

  // Collect everything that need to be rendered and put it in a list
  TKMRenderPool = class
  private
    fCounter: Cardinal;
    fRXData: array [TRXType] of TRXData; // Shortcuts
    fViewport: TKMViewport;
    fRender: TKMRender;
    // fSampleHouse: TOBJModel;
    rPitch,rHeading,rBank: Integer;
    fRenderList: TKMRenderList;
    fRenderTerrain: TKMRenderTerrain;
    fRenderDebug: TKMRenderDebug;

    fFieldsList: TKMPointTagList;
    fBridgePlansList: TKMPointDirTagList;
    fHousePlansList: TKMPointDirList;
    fTabletsList: TKMPointTagList;
    fMarksList: TKMPointTagList;
    fHouseOutline: TKMPointList;
    procedure UnitsHitTest(const X, Y : Integer);
    procedure ApplyTransform;
    procedure SetDefaultRenderParams;
    procedure RenderBackgroundUI(const aRect: TKMRect);
    // Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderForegroundUI;
    procedure RenderForegroundUI_Tiles;
    procedure RenderForegroundUI_Brush;
    procedure RenderForegroundUI_BigErase;
    procedure RenderForegroundUI_ElevateEqualize;
    procedure RenderForegroundUI_ObjectsBrush;
    procedure RenderForegroundUI_DefenceGroup;
    procedure RenderForegroundUI_Markers;
    procedure RenderForegroundUI_Units;
    procedure RenderForegroundUI_UnitsCustom;
    procedure RenderForegroundUI_PaintBucket(aHighlightAll: Boolean);
    procedure RenderForegroundUI_UniversalEraser(aHighlightAll: Boolean);
    procedure RenderForegroundUI_ChangeResCount;
    procedure RenderForegroundUI_SetHouseRepair;
    procedure RenderForegroundUI_Bridge;
    procedure RenderForegroundUI_Decoration;
    procedure RenderForegroundUI_WareOnGround;
    procedure DoRenderGroup(aUnitType: TKMUnitType; aLoc: TKMPointDir; aMembersCnt, aUnitsPerRow: Integer;  aHandColor: Cardinal);
    function TryRenderUnitOrGroup(aEntity: TKMHandEntity; aUnitFilterFunc, aGroupFilterFunc: TBooleanFunc; aUseGroupFlagColor, aDoHighlight: Boolean; aHandColor, aFlagColor: Cardinal; aHighlightColor: Cardinal = 0): Boolean;
    procedure RenderUnit(U: TKMUnit; const P: TKMPoint; aFlagColor: Cardinal; aDoHighlight: Boolean; aHighlightColor: Cardinal); overload;
    procedure RenderUnit(aUnitType: TKMUnitType; const P: TKMPointDir; aAnimStep: Integer; aFlagColor: Cardinal; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0); overload;
    function PaintBucket_UnitToRender(aUnit: TObject): Boolean;
    function PaintBucket_GroupToRender(aGroup: TObject): Boolean;


    procedure RenderHouseOutline(aHouseSketch: TKMHouseSketch; aCol: Cardinal = icCyan);

    // Terrain rendering sub-class
    procedure CollectPlans(const aRect: TKMRect);
    procedure CollectTerrainObjects(const aRect: TKMRect; aAnimStep: Cardinal);
    procedure PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Integer; aFirstPass: Boolean;
                             aDoImmediateRender: Boolean = False);
    procedure PaintFlagPoints(aFirstPass: Boolean);

    procedure RenderWireHousePlan(const P: TKMPoint; aHouseType: TKMHouseType; aIgnoreObjects: Boolean = false);
    procedure RenderTileOwnerLayer(const aRect: TKMRect);
    procedure RenderTilesGrid(const aRect: TKMRect);

    procedure RenderWireTileInt(const X,Y: Integer);
    procedure RenderWireTileIntObjBr(const X,Y: Integer);
    procedure RenderTileInt(const X, Y: Integer);
    procedure RenderTileBrushInt(const X, Y: Integer);

    procedure RenderBigEraseTileInt(const X,Y: Integer);
    procedure RenderAssignToShip;
  protected

    procedure RenderSprite(aRX: TRXType; aId: Integer; aX, aY: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);overload;
    procedure RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); overload; virtual;
    procedure RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); virtual;
    procedure RenderSpriteFront(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); virtual;
    procedure RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
                                    aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0); virtual;
  public
    IntScale : Single;
    constructor Create(aViewport: TKMViewport; aRender: TKMRender);
    destructor Destroy; override;

    procedure ReInit;

    procedure AddAlert(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4);
    procedure AddSpecAnim(const aLoc: TKMPointF; aAnim: TKMAnimation; aSTep: Byte;
                          aRX : TRXType = rxTrees; aFront : Boolean = true; aAlphaStep : Single = -1); overload;
    procedure AddSpecAnim(const aLoc, aGround: TKMPointF; aAnim: TKMAnimation; aSTep: Byte;
                          aRX : TRXType = rxTrees; aFront : Boolean = true; aAlphaStep : Single = -1);overload;
    procedure AddProjectile(aProj: TKMProjectileType; const aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
    procedure AddHouse(aHouse: TKMHouseType; const aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single; aWoodPic : Integer = -1; aStonePic : Integer = -1; aSnowPic : Integer = -1;
                               aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddWholeHouse(H: TKMHouse; aFlagColor: Cardinal; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);

    procedure AddHouseTablet(aHouse: TKMHouseType; const aLoc: TKMPoint);
    procedure AddHouseBuildSupply(aHouse: TKMHouseType; const Loc: TKMPoint; Wood, Stone, tile: Byte);
    procedure AddHouseWork(aHouse: TKMHouseType; const aLoc: TKMPoint; aActSet: TKMHouseActionSet; aAnimStep, aAnimStepPrev: Cardinal; aFlagColor: TColor4; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddHouseAnimation(const aLoc: TKMPoint; aAnimation : TKMAnimation; aAnimStep: Cardinal; aFlagColor: TColor4; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);

    procedure AddHousePearl(aPearlType : TKMPearlType; const aLoc: TKMPoint; const aStage : Byte; aWoodStep, aStoneStep, aSnowStep: Single; aSnowID : Word;
                                aFlagColor : Cardinal = 0;
                               aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddHousePasture(aLoc : TKMPoint; aFlagColor : Cardinal = 0;
                               aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddHouseArena(aType: TKMDevelopmentTreeType; const aLoc: TKMPoint; aAnimStep: Cardinal; Colors : TKMCardinalArray;
                            aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);

    procedure AddHouseSchoolClock(const aLoc: TKMPoint; aAnimStep: Cardinal; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);

    procedure AddHouseTowerRecruits(const aLoc: TKMPoint; aRight, aLeft : Boolean;
                                  aAnimStep: Cardinal; aDoImmediateRender: Boolean = False);

    procedure AddHousePastureAnimal(const aLoc: TKMPointF; aAnimal : TKMPastureAnimalType; Action : TKMPastureAnimalAction; Dir : TKMDirection;
                                  aAnimStep: Cardinal; C1, C2: TColor4;
                                  aDoImmediateRender: Boolean = False);
    procedure AddAnimation(const aLoc: TKMPoint; aAnim : TKMAnimLoop; aAnimStep: Cardinal; aFlagColor: TColor4; aRX : TRXType;
                          aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0; aFront : Boolean = false; aAlphaStep : Single = -1);Overload;
    procedure AddAnimation(const aLoc: TKMPointF; aAnim : TKMAnimLoop; aAnimStep: Cardinal; aFlagColor: TColor4; aRX : TRXType;
                          aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0; aFront : Boolean = false; aAlphaStep : Single = -1);Overload;

    procedure AddAnimation(const aLoc: TKMPoint; aAnim : TKMAnimation; aAnimStep: Cardinal; aFlagColor: TColor4; aRX : TRXType;
                          aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0; aFront : Boolean = false; aAlphaStep : Single = -1);Overload;
    procedure AddAnimation(const aLoc: TKMPointF; aAnim : TKMAnimation; aAnimStep: Cardinal; aFlagColor: TColor4; aRX : TRXType;
                          aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0; aFront : Boolean = false; aAlphaStep : Single = -1);Overload;
    procedure AddAnimationG(const aLoc: TKMPointF; aAnim : TKMAnimation; aAnimStep: Cardinal; aFlagColor: TColor4; aRX : TRXType;
                          aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0; aFront : Boolean = false; aAlphaStep : Single = -1);Overload;


    procedure AddSprite(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                        aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0; aFront : Boolean = false; aAlphaStep : Single = -1);overload;

    procedure AddSprite(const aLoc: TKMPointF; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                        aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0; aFront : Boolean = false; aAlphaStep : Single = -1);overload;
    procedure AddSpriteWH(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                        aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0; aFront : Boolean = false; aAlphaStep : Single = -1);
    procedure AddSpriteG(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                        aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0; aFront : Boolean = false; aAlphaStep : Single = -1);

    procedure AddHouseSupply(aHouse: TKMHouseType; const aLoc: TKMPoint; const R1, R2, R3: array of Byte; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddHouseMarketSupply(const aLoc: TKMPoint; aResType: TKMWareType; aResCount: Word; aAnimStep: Integer);
    procedure AddHouseStableBeasts(aHouse: TKMHouseType; const aLoc: TKMPoint; aBeastId,aBeastAge,aAnimStep: Integer; aRX: TRXType = rxHouses);
    procedure AddHousePalaceFlags(aHouse: TKMHouseType; const aLoc: TKMPoint; aFlagID,aAnimStep: Integer; FlagColor : Cardinal);
    procedure AddHousePotteryTiles(const aLoc: TKMPoint; aTileID: Byte);
    procedure AddHouseMerchantChests(const aLoc: TKMPoint; aOffsetX, aOffsetY : Integer);
    procedure AddHouseEater(const Loc: TKMPoint; aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
    procedure AddForestLogs(const aLoc: TKMPoint; aCount : Byte; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
    procedure AddPastureSilos(const aLoc: TKMPoint; aID : Word; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);


    procedure AddUnit(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0; aAlphaStep : Single = -1);
    procedure AddUnitCarry(aCarry: TKMWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single; FlagColor: TColor4);
    procedure AddUnitThought(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; aStep : Cardinal; Thought: TKMUnitThought; pX,pY: Single);
    procedure AddUnitFlag(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; FlagAnim: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False);
    procedure AddUnitWithDefaultArm(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHignlight: Boolean = False; HighlightColor: TColor4 = 0);
    procedure AddUnitBitin(pX, pY : Single);
    procedure RenderMapElement1(aIndex: Word; aAnimStep: Cardinal; LocX,LocY: Integer; aLoopAnim: Boolean; aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
    procedure RenderMapElement4(aIndex: Word; aAnimStep: Cardinal; pX,pY: Integer; aIsDouble: Boolean; aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
    procedure RenderMapElement(aIndex: Word; aAnimStep,pX,pY: Integer; aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
    procedure RenderTree(aIndex: Word; aAnimStep: Cardinal; LocX,LocY: Single; nightLoc : TKMPoint;
                        aDoImmediateRender: Boolean = False; aDeleting: Boolean = False);
    procedure RenderSpriteOnTile(const aLoc: TKMPoint; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aRX : TRXType = rxGui);
    procedure RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False; aRX : TRXType = rxGui);
    procedure RenderTile(aTerrainId: Word; pX,pY,Rot: Integer);
    procedure RenderWireTile(const P: TKMPoint; aCol: TColor4; aInset: Single = 0.0; aLineWidth: Single = -1);

    property RenderDebug: TKMRenderDebug read fRenderDebug;
    property ViewPort : TKMViewport read fViewport;

    property RenderList: TKMRenderList read fRenderList;
    property RenderTerrain: TKMRenderTerrain read fRenderTerrain;
    procedure SetRotation(aH,aP,aB: Integer);

    procedure Render(aTickLag: Single);

    property BaseRender : TKMRender read fRender;
  end;

  TKMRenderPoolNoNight = class(TKMRenderPool)
  protected
    procedure RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); overload; override;
    procedure RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); override;
    procedure RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
                                    aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0); override;
  end;

  TKMRenderPoolNoShadows = class(TKMRenderPool)
  protected
    procedure RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); overload; override;
    procedure RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1); override;
    procedure RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
                                    aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0); override;
  end;

var
  gRenderPool: TKMRenderPool;


implementation
uses
  KM_Entity,
  KM_RenderAux, KM_RenderGameAux, KM_HandsCollection, KM_Game, KM_GameSettings, KM_Sound, KM_Resource, KM_ResUnits,
  KM_ResMapElements, KM_AIFields, KM_TerrainPainter, KM_Cursor,  KM_MapEdTypes,
  KM_Hand, KM_UnitGroup, KM_CommonUtils,
  KM_GameParams, KM_Utils, KM_ResTilesetTypes, KM_DevPerfLog, KM_DevPerfLogTypes,
  KM_HandTypes,
  KM_Projectiles, KM_SpecialAnim, KM_Particles,
  KM_TerrainTypes, KM_ResTileset,
  KM_AITypes;


const
  DELETE_COLOR = $1616FF;
  INTERP_LEVEL = 8;


constructor TKMRenderPool.Create(aViewport: TKMViewport; aRender: TKMRender);
var
  RT: TRXType;
begin
  inherited Create;

  fCounter := 0;

  for RT := Low(TRXType) to High(TRXType) do
    fRXData[RT] := gRes.Sprites[RT].RXData;

  fRender := aRender;
  fViewport := aViewport;

  fRenderList     := TKMRenderList.Create;
  fRenderTerrain  := TKMRenderTerrain.Create;
  fRenderDebug    := TKMRenderDebug.Create;
  gRenderAux      := TKMRenderAux.Create;
  gRenderGameAux  := TKMRenderGameAux.Create;

  fFieldsList     := TKMPointTagList.Create;
  fHousePlansList := TKMPointDirList.Create;
  fBridgePlansList := TKMPointDirTagList.Create;
  fTabletsList    := TKMPointTagList.Create;
  fMarksList      := TKMPointTagList.Create;
  fHouseOutline   := TKMPointList.Create;
  // fSampleHouse := TOBJModel.Create;
  // fSampleHouse.LoadFromFile(ExeDir + 'Store.obj');
end;


destructor TKMRenderPool.Destroy;
begin
  fFieldsList.Free;
  fHousePlansList.Free;
  fBridgePlansList.Free;
  fTabletsList.Free;
  fMarksList.Free;
  fHouseOutline.Free;
  // fSampleHouse.Free;
  fRenderList.Free;
  fRenderDebug.Free;
  fRenderTerrain.Free;
  gRenderGameAux.Free;
  gRenderAux.Free;

  inherited;
end;


procedure TKMRenderPool.ReInit;
begin
  if Self = nil then Exit;

  fRenderDebug.ReInit;
end;


procedure TKMRenderPool.SetRotation(aH, aP, aB: Integer);
begin
  rHeading := aH;
  rPitch   := aP;
  rBank    := aB;
end;


procedure TKMRenderPool.ApplyTransform;
var
  viewportPosRound: TKMPointF;
begin
  //Need to round the viewport position so we only translate by whole pixels
  viewportPosRound := RoundToTilePixel(fViewport.Position);

  glLoadIdentity; // Reset The View
  //Use integer division so we don't translate by half a pixel if clip is odd
  glTranslatef(fViewport.ViewportClip.X div 2, fViewport.ViewportClip.Y div 2, 0);

  glScalef(fViewport.Zoom*CELL_SIZE_PX, fViewport.Zoom*CELL_SIZE_PX, 1 / 256);

  glTranslatef(-viewportPosRound.X + gGame.ActiveInterface.ToolbarWidth/CELL_SIZE_PX/fViewport.Zoom, -viewportPosRound.Y, 0);

  if RENDER_3D then
  begin
    fRender.SetRenderMode(rm3D);

    glkScale(-CELL_SIZE_PX/14);
    glRotatef(rHeading,1,0,0);
    glRotatef(rPitch  ,0,1,0);
    glRotatef(rBank   ,0,0,1);
    glTranslatef(-viewportPosRound.X + gGame.ActiveInterface.ToolBarWidth/CELL_SIZE_PX/fViewport.Zoom, -viewportPosRound.Y - 8, 10);
    glScalef(fViewport.Zoom, fViewport.Zoom, 1);
  end;

  glRotatef(rHeading,1,0,0);
  glRotatef(rPitch  ,0,1,0);
  glRotatef(rBank   ,0,0,1);
  glTranslatef(0, 0, -viewportPosRound.Y);
end;


procedure TKMRenderPool.SetDefaultRenderParams;
begin
  glLineWidth(fViewport.Zoom * 2);
  glPointSize(fViewport.Zoom * 5);
  glEnable(GL_LINE_SMOOTH);
end;

// Render:
// 1. Sets viewport
// 2. Renders terrain
// 3. Polls Game objects to add themselves to RenderList through Add** methods
// 4. Renders cursor highlights
procedure TKMRenderPool.Render(aTickLag: Single);
var
  clipRect: TKMRect;
begin
  if fRender.Blind then Exit;

  Inc(fCounter);

  ApplyTransform;

  glPushAttrib(GL_LINE_BIT or GL_POINT_BIT);
    SetDefaultRenderParams;

    // Render only within visible area
    clipRect := fViewport.GetClip;

    fRenderDebug.ClipRect := clipRect;

    // Collect players plans for terrain layer
    CollectPlans(clipRect);

    // With depth test we can render all terrain tiles and then apply light/shadow without worrying about
    // foothills shadows going over mountain tops. Each tile strip is rendered an next Z plane.
    // Means that Z-test on gpu will take care of clipping the foothill shadows
    glEnable(GL_DEPTH_TEST);

    // Everything flat of terrain
    {$IFDEF PERFLOG}
    gPerfLogs.SectionEnter(psFrameTerrain);
    {$ENDIF}
    fRenderTerrain.ClipRect := clipRect;
    fRenderTerrain.RenderBase(gTerrain.AnimStep, gMySpectator.FogOfWar);

    // Disable depth test //and write to depth buffer,
    // so that terrain shadows could be applied seamlessly ontop
    glDisable(GL_DEPTH_TEST);

    if mlOverlays in gGameParams.VisibleLayers then
    begin
      fRenderTerrain.RenderFences(gMySpectator.FogOfWar);
      fRenderTerrain.RenderPlayerPlans(fFieldsList, fHousePlansList, fBridgePlansList);
    end;

    if mlMiningRadius in gGameParams.VisibleLayers then
      fRenderDebug.PaintMiningRadius;

    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psFrameTerrain);
    {$ENDIF}

    // House highlight, debug display
    RenderBackgroundUI(clipRect);

    // Sprites are added by Terrain/Players/Projectiles, then sorted by position
    fRenderList.Clear;
    CollectTerrainObjects(clipRect, gTerrain.AnimStep);

    PaintFlagPoints(True);
    gHands.Paint(clipRect, aTickLag); // Units and houses
    gProjectiles.Paint(aTickLag);

    if not gGameParams.IsMapEditor then
    begin
      gSpecAnim.Paint(0);
      gParticles.Paint(aTickLag, clipRect);
      gGame.Weather.Paint(aTickLag, clipRect);
    end;


    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(0);

    fRenderList.SortRenderList;
    fRenderList.Render;

    if mlDefencesAll in gGameParams.VisibleLayers then
      fRenderDebug.PaintDefences;

    fRenderTerrain.RenderFOW(gMySpectator.FogOfWar);

    // Alerts/rally second pass is rendered after FOW
    PaintFlagPoints(False);
    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(1);
    gHands.PaintUnitMessages;
    // Cursor overlays (including blue-wire plans), go on top of everything
    RenderForegroundUI;

  glPopAttrib;
end;


procedure TKMRenderPool.RenderBackgroundUI(const aRect: TKMRect);

  procedure HighlightUnit(U: TKMUnit; aCol: Cardinal); inline;
  begin
    gRenderAux.CircleOnTerrain(U.PositionF.X - 0.5 + U.GetSlide(axX),
                               U.PositionF.Y - 0.5 + U.GetSlide(axY),
                               0.4, aCol, icCyan);
  end;

  procedure HighlightEntity(aEntityH: TKMHighlightEntity);
  var
    I: Integer;
    G: TKMUnitGroup;
    col: Cardinal;
  begin
    if aEntityH.Entity = nil then Exit;
    
    case aEntityH.Entity.EntityType of
      etHouse:  RenderHouseOutline(TKMHouseSketch(aEntityH.Entity), aEntityH.Color); //fPositionF.X - 0.5 + GetSlide(axX), fPositionF.Y - 0.5 + GetSlide(axY), 0.35
      etUnit:   HighlightUnit(TKMUnit(aEntityH.Entity), GetRandomColorWSeed(aEntityH.Entity.UID));
      etGroup:  begin
                  G := TKMUnitGroup(aEntityH.Entity);
                  col := GetRandomColorWSeed(G.UID);
                  for I := 0 to G.Count - 1 do
                    HighlightUnit(G.Members[I], col);
                end;
    end;
  end;


var
  I, K: Integer;
begin
  //Reset Texture, just in case we forgot to do it inside some method
  TKMRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  if gMySpectator.Selected is TKMHouse then
    HighlightEntity(TKMHighlightEntity.New(gMySpectator.Selected, icBlue) );
  HighlightEntity(gMySpectator.HighlightEntity);
  HighlightEntity(gMySpectator.HighlightDebug);
  HighlightEntity(gMySpectator.HighlightDebug2);
  HighlightEntity(gMySpectator.HighlightDebug3);


  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plTerrain, aRect);

  if gAIFields <> nil then
    gAIFields.Paint(aRect);

  if SHOW_WALK_CONNECT then
  begin
    glPushAttrib(GL_DEPTH_BUFFER_BIT);
      glDisable(GL_DEPTH_TEST);

      for I := aRect.Top to aRect.Bottom do
      for K := aRect.Left to aRect.Right do
        gRenderAux.Text(K, I, IntToStr(gTerrain.Land^[I,K].WalkConnect[wcWalk]), $FFFFFFFF);

    glPopAttrib;
  end;

  if SHOW_TERRAIN_WIRES then
    gRenderAux.Wires(aRect);

  if SHOW_TERRAIN_PASS <> 0 then
    gRenderGameAux.Passability(aRect, SHOW_TERRAIN_PASS);

  if SHOW_TERRAIN_IDS then
    gRenderGameAux.TileTerrainIDs(aRect);

  if SHOW_TERRAIN_KINDS then
    gRenderGameAux.TileTerrainKinds(aRect);

  if SHOW_TERRAIN_OVERLAYS then
    gRenderGameAux.TileTerrainOverlays(aRect);

  if SHOW_TERRAIN_HEIGHT then
    gRenderGameAux.TileTerrainHeight(aRect);

  if SHOW_JAM_METER then
    gRenderGameAux.TileTerrainJamMeter(aRect);

  if SHOW_TILE_OBJECT_ID then
    gRenderGameAux.TileTerrainTileObjectID(aRect);

  if SHOW_TILES_OWNER then
    RenderTileOwnerLayer(aRect);

  if SHOW_TREE_AGE then
    gRenderGameAux.TileTerrainTreeAge(aRect);

  if SHOW_FIELD_AGE then
    gRenderGameAux.TileTerrainFieldAge(aRect);

  if SHOW_TILE_LOCK then
    gRenderGameAux.TileTerrainTileLock(aRect);

  if SHOW_TILE_UNIT then
    gRenderGameAux.TileTerrainTileUnit(aRect);

  if SHOW_VERTEX_UNIT then
    gRenderGameAux.TileTerrainVertexUnit(aRect);

  if SHOW_TERRAIN_TILES_GRID then
    RenderTilesGrid(aRect);

  if SHOW_UNIT_MOVEMENT then
    gRenderAux.UnitMoves(aRect);

  if SHOW_VIEWPORT_POS and (gGame.ActiveInterface <> nil) then
    gGame.ActiveInterface.Viewport.Paint;
end;


procedure TKMRenderPool.CollectTerrainObjects(const aRect: TKMRect; aAnimStep: Cardinal);
var
  I, K: Integer;
  WT : TKMWareType;
  J : Integer;
begin
  if not (mlObjects in gGameParams.VisibleLayers) then Exit;

  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plObjects, aRect);


  with gTerrain do
    for I := aRect.Top to aRect.Bottom do
      for K := aRect.Left to aRect.Right do
      begin
        if (Land^[I, K].Obj <> OBJ_NONE) then
          if (House(K, I) = nil) or not ((House(K, I) is TKMHouseAppleTree) and TKMHouse(House(K, I)).IsComplete) then
          RenderMapElement(Land^[I, K].Obj, AnimStep, K, I);

        if (Land^[I, K].Ware.C > 0) then
          if (gMySpectator.FogOfWar.CheckVerticeRenderRev(K,I) > FOG_OF_WAR_MIN) then// Do not render tiles fully covered by FOW
          begin
            WT := TKMWareType(Land^[I, K].Ware.W);
            if WT = wtNone then
              Continue;
            J := gRes.Wares[WT].GetTerrainPic(Land^[I, K].Ware.C);
            if J = 0 then
              Continue;
            AddSpriteG(KMPoint(K, I), KMPOINT_ZERO, J, rxTrees, 0);

          end;
      end;

  // Falling trees are in a separate list
  with gTerrain do
    for I := 0 to FallingTrees.Count - 1 do
    begin
      RenderTree(FallingTrees.Tag[I], aAnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y, KMPoint(FallingTrees[I]));
      //RenderMapElement1(FallingTrees.Tag[I], aAnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y, False);
      Assert(AnimStep - FallingTrees.Tag2[I] <= 100, 'Falling tree overrun?');
    end;

  // Tablets on house plans, for self and allies
  fTabletsList.Clear;
  if gGameParams.IsReplayOrSpectate then
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        gHands[I].GetPlansTablets(fTabletsList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetPlansTablets(fTabletsList, aRect)
  else
    gMySpectator.Hand.GetPlansTablets(fTabletsList, aRect);

  for I := 0 to fTabletsList.Count - 1 do
    AddHouseTablet(TKMHouseType(fTabletsList.Tag[I]), fTabletsList[I]);
end;


procedure TKMRenderPool.PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Integer; aFirstPass: Boolean;
                                     aDoImmediateRender: Boolean = False);

  procedure RenderLineToPoint(const aP: TKMPointF);
  begin
    gRenderAux.LineOnTerrain(aHouseEntrance.X - 0.5, aHouseEntrance.Y - 0.5, aP.X, aP.Y, aColor, $F0F0, False);
  end;

var
  P: TKMPointF;
begin
  P := KMPointF(aFlagPoint.X - 0.5, aFlagPoint.Y - 0.5);
  if not aDoImmediateRender then
  begin
    if aFirstPass then
    begin
      AddAlert(P, aTexId, aColor);
      RenderLineToPoint(P);
    end
    else
    if gMySpectator.FogOfWar.CheckRevelation(P) < FOG_OF_WAR_MAX then
      RenderSpriteOnTerrain(P, aTexId, aColor, True); //Force to paint, even under FOW
  end
  else begin
    RenderSpriteOnTile(aFlagPoint, aTexId, aColor);
    RenderLineToPoint(P);
  end;
end;


procedure TKMRenderPool.PaintFlagPoints(aFirstPass: Boolean);
var
  house: TKMHouseWFlagPoint;
begin
  // Skip render if no house with flagpoint is chosen
  if not (gMySpectator.Selected is TKMHouseWFlagPoint) then
    Exit;

  house := TKMHouseWFlagPoint(gMySpectator.Selected);
  if house.IsFlagPointSet then
    PaintFlagPoint(house.Entrance, house.FlagPoint, gHands[house.Owner].GameFlagColor, gRes.Houses[house.HouseType].FlagPointTexId, aFirstPass);
end;


procedure TKMRenderPool.RenderTile(aTerrainId: Word; pX, pY, Rot: Integer);
begin
  fRenderTerrain.RenderTile(aTerrainId, pX, pY, Rot);
end;


procedure TKMRenderPool.RenderMapElement(aIndex: Word; aAnimStep,pX,pY: Integer; aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
begin
  if (gMySpectator.FogOfWar.CheckVerticeRenderRev(pX,pY) <= FOG_OF_WAR_MIN) then Exit;// Do not render tiles fully covered by FOW
  // Render either normal object or quad depending on what it is

  if gMapElements[aIndex].WineOrCorn then
    RenderMapElement4(aIndex,aAnimStep,pX,pY,gMapElements[aIndex].RenderAsTwo,aDoImmediateRender,aDeleting, aOnTop) // 54..57 are grapes, all others are doubles
  else
    RenderMapElement1(aIndex,aAnimStep + pX+pY,pX,pY,True,aDoImmediateRender,aDeleting, aOnTop);
end;


procedure TKMRenderPool.RenderMapElement1(aIndex: Word; aAnimStep: Cardinal; LocX,LocY: Integer; aLoopAnim: Boolean;
                                        aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
var
  R: TRXData;
  pX, pY, Step: Integer;
  cornerX, cornerY: Single;
  gX, gY: Single;
  Id, Id0: Integer;
  FOW: Byte;
  A : TKMAnimLoop;
begin
  if (gMySpectator.FogOfWar.CheckVerticeRenderRev(LocX,LocY) <= FOG_OF_WAR_MIN) then Exit;

  if aIndex = OBJ_BLOCK then
  begin
    // Invisible wall
    // Render as a red outline in map editor mode
    if gGameParams.IsMapEditor then
    begin
      gRenderAux.Quad(LocX, LocY, $600000FF);
      RenderWireTile(KMPoint(LocX, LocY), $800000FF);
    end;
  end
  else
  if (aIndex = 80) or (aIndex = 81) then
  begin
    // Invisible wall
    // Render as a red outline in map editor mode
    if gGameParams.IsMapEditor then
    begin
      gRenderAux.Quad(LocX, LocY, $60808000);
      RenderWireTile(KMPoint(LocX, LocY), $80B0B000);
    end;
  end
  else
  begin
    if gMapElements[aIndex].Anim.Count = 0 then Exit;


    if gGameParams.DynamicFOW then
    begin
      FOW := gMySpectator.FogOfWar.CheckTileRevelation(LocX,LocY);
      if FOW <= 128 then aAnimStep := 0; // Stop animation
    end;
    A := gMapElements[aIndex].Anim;
    step := aAnimStep mod A.Count + 1;
    Id := gMapElements[aIndex].Anim.Step[step] + 1;
    Id0 := gMapElements[aIndex].Anim.Step[1] + 1;


    if Id <= 0 then exit;

    R := fRXData[rxTrees];
    pX := LocX - 1;
    pY := LocY - 1;

    if gGameSettings.GFX.AllowSnowObjects then
      if (gMapElements[aIndex].SnowPic > 255) and (gTerrain.TileIsSnow(pX, pY)) then
        Id := gMapElements[aIndex].SnowPic;

    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;

    cornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    cornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

    if gMapElements[aIndex].RandomPos then
    begin
      cornerX := cornerX + Sin(LocY * gTerrain.Land[LocY, LocX].BaseLayer.Rotation) / 3;
      cornerY := cornerY + Cos(LocX * gTerrain.Land[LocY, LocX].Height) / 3;

      gX := gX + Sin(LocY * gTerrain.Land[LocY, LocX].BaseLayer.Rotation) / 3;
      gY := gY + Cos(LocX * gTerrain.Land[LocY, LocX].Height) / 3;
    end;

    if aDoImmediateRender then
    begin
      if gMapElements[aIndex].RenderAsTileOverlay then
        fRenderTerrain.RenderSpriteOnTile(Id, LocX, LocY, gTerrain.Land[LocY, LocX].BaseLayer.Rotation * byte(gMapElements[aIndex].RotateToTile), rxTrees)
      else
        RenderSprite(rxTrees, Id, cornerX, cornerY, $FFFFFFFF, aDeleting, DELETE_COLOR)
    end
    else
    if gMapElements[aIndex].RenderAsTileOverlay then
      fRenderTerrain.RenderSpriteOnTile(Id, LocX, LocY, gTerrain.Land[LocY, LocX].BaseLayer.Rotation * byte(gMapElements[aIndex].RotateToTile), rxTrees)
    else
    if aOnTop then
      fRenderList.AddSprite(rxTrees, Id, cornerX, cornerY, LocX,LocY)
    else
      fRenderList.AddSpriteG(rxTrees, Id, 0, cornerX, cornerY, gX, gY, LocX,LocY);
  end;
end;

procedure TKMRenderPool.RenderTree(aIndex: Word; aAnimStep: Cardinal; LocX: Single; LocY: Single; nightLoc : TKMPoint;
                                  aDoImmediateRender: Boolean = False; aDeleting: Boolean = False);
var
  R: TRXData;
  pX, pY : Single;
  Step: Integer;
  cornerX, cornerY: Single;
  gX, gY: Single;
  Id, Id0: Integer;
  A : TKMAnimLoop;
begin
  A := gMapElements[aIndex].Anim;
  if A.Count = 0 then Exit;

  step := aAnimStep mod A.Count + 1;
  Id := gMapElements[aIndex].Anim.Step[step] + 1;
  Id0 := gMapElements[aIndex].Anim.Step[1] + 1;

  if Id <= 0 then exit;

  R := fRXData[rxTrees];
  pX := LocX - 1;
  pY := LocY - 1;

  if gGameSettings.GFX.AllowSnowObjects then
    if (gMapElements[aIndex].SnowPic > 255) and (gTerrain.TileIsSnow(Round(pX), round(pY))) then
      Id := gMapElements[aIndex].SnowPic;

  gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
  gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;

  cornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
  cornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

  if aDoImmediateRender then
      RenderSprite(rxTrees, Id, cornerX, cornerY, $FFFFFFFF, aDeleting, DELETE_COLOR)
  else
    fRenderList.AddSpriteG(rxTrees, Id, 0, cornerX, cornerY, gX, gY, nightLoc.X,nightLoc.Y);
end;


// 4 objects packed on 1 tile for Corn and Grapes
procedure TKMRenderPool.RenderMapElement4(aIndex: Word; aAnimStep: Cardinal; pX: Integer; pY: Integer;
                                          aIsDouble: Boolean; aDoImmediateRender: Boolean = False; aDeleting: Boolean = False; aOnTop: Boolean = False);
var
  R: TRXData;

  procedure AddSpriteBy(aAnimStep: Integer; pX,pY: Single; nX, nY : Integer);
  var
    Id, Id0: Integer;
    CornerX, CornerY, gX, gY: Single;
    A : TKMAnimLoop;
  begin
    A := gMapElements[aIndex].Anim;
    Id := A.Step[aAnimStep mod A.Count + 1]+1;
    Id0 := A.Step[1] + 1;

    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    CornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

    if aDoImmediateRender then
      RenderSprite(rxTrees, Id, CornerX, CornerY, $FFFFFFFF, aDeleting, DELETE_COLOR)
    else
    if aOnTop then
      fRenderList.AddSprite(rxTrees, Id, CornerX, CornerY, nX, nY)
    else
      fRenderList.AddSpriteG(rxTrees, Id, 0, CornerX, CornerY, gX, gY, nX, nY);
  end;

var
  FOW: Byte;
begin
  if gGameParams.DynamicFOW then
  begin
    FOW := gMySpectator.FogOfWar.CheckTileRevelation(pX, pY);
    if FOW <= 128 then aAnimStep := 0; // Stop animation
  end;

  R := fRXData[rxTrees];
  if aIsDouble then
  begin
    AddSpriteBy(aAnimStep  , pX - 0.75, pY - 0.6, pX, pY);
    AddSpriteBy(aAnimStep+1, pX - 0.25, pY - 0.6, pX, pY);
  end
  else
  begin
    AddSpriteBy(aAnimStep  , pX - 0.75, pY - 0.75, pX, pY);
    AddSpriteBy(aAnimStep+1, pX - 0.25, pY - 0.75, pX, pY);
    AddSpriteBy(aAnimStep+1, pX - 0.75, pY - 0.25, pX, pY);
    AddSpriteBy(aAnimStep  , pX - 0.25, pY - 0.25, pX, pY);
  end;
end;


// Render alert
procedure TKMRenderPool.AddAlert(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4);
var
  cornerX, cornerY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];

  cornerX := aLoc.X + R.Pivot[aId].X / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(aLoc).Y + R.Pivot[aId].Y / CELL_SIZE_PX;

  fRenderList.AddSpriteG(rxGui, aId, 0, cornerX, cornerY, aLoc.X, aLoc.Y, aLoc.RX, aLoc.RY, aFlagColor);
end;


// Render house WIP tablet
procedure TKMRenderPool.AddHouseTablet(aHouse: TKMHouseType; const aLoc: TKMPoint);
var
  Id: Integer;
  cornerX, cornerY, gX, gY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];
  Id := gRes.Houses[aHouse].TabletIcon;

  gX := aLoc.X + (R.Pivot[Id].X + R.Size[Id].X / 2) / CELL_SIZE_PX - 0.5;
  gY := aLoc.Y + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.45;
  cornerX := aLoc.X + R.Pivot[Id].X / CELL_SIZE_PX - 0.25;
  cornerY := aLoc.Y - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.55;
  fRenderList.AddSpriteG(rxGui, Id, 0, cornerX, cornerY, gX, gY, aLoc.X, aLoc.Y);
end;


// Render house build supply
procedure TKMRenderPool.AddHouseBuildSupply(aHouse: TKMHouseType; const Loc: TKMPoint; Wood, Stone, tile: Byte);
var
  rx: TRXData;
  id: Integer;
  supply: THouseBuildSupply;
  cornerX, cornerY: Single;
  rStone, rWood, rTile : Byte;
begin
  rx := fRXData[rxHouses];
  supply := gRes.Houses[aHouse].BuildSupply;

  rStone := Stone;
  rWood := Wood;
  rTile := tile;
  SetLimit(rWood, 0, 6);
  SetLimit(rStone, 0, 6);
  SetLimit(rTile, 0, 5);

  if rWood <> 0 then
  begin
    id := 260 + rWood - 1;
    cornerX := Loc.X + (supply[1].MoveX + rx.Pivot[id].X - 4) / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[1].MoveY+ rx.Pivot[id].Y + rx.Size[id].Y - 5) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);
  end;

  //if there is more than 6 wood then add another pile
  if Wood > 6 then
  begin
    rWood := Wood - 6;
    SetLimit(rWood, 0, 6);

    if rWood <> 0 then
    begin
      id := 260 + rWood - 1;
      cornerX := Loc.X + (supply[1].MoveX + rx.Pivot[id].X + 9)/ CELL_SIZE_PX - 1;
      cornerY := Loc.Y + (supply[1].MoveY + rx.Pivot[id].Y + rx.Size[id].Y + 8) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

      fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);

    end;


  end;



  if rStone <> 0 then
  begin
    id := 267 + rStone - 1;

    cornerX := Loc.X + (supply[2].MoveX + rx.Pivot[id].X) / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[2].MoveY + rx.Pivot[id].Y + rx.Size[id].Y - 6) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);
  end;

  //if there is more than 6 stone then add another pile
  if Stone > 6 then
  begin
    rStone := Stone - 6;
    SetLimit(rStone, 0, 6);

    if rStone <> 0 then
    begin
      id := 267 + rStone - 1;
      cornerX := Loc.X + (supply[2].MoveX + rx.Pivot[id].X + 9)/ CELL_SIZE_PX - 1;
      cornerY := Loc.Y + (supply[2].MoveY + rx.Pivot[id].Y + rx.Size[id].Y + 8) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

      fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);

    end;


  end;

  if rTile <> 0 then
  begin
    id := 2288 + rTile - 1;
    cornerX := Loc.X + (supply[3].MoveX + rx.Pivot[id].X) / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[3].MoveY + rx.Pivot[id].Y + rx.Size[id].Y - 6) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);
  end;

  //if there is more than 6 tiles then add another pile
  if Tile > 5 then
  begin
    rTile := Tile - 5;
    SetLimit(rTile, 0, 5);

    if rTile <> 0 then
    begin
      id := 2288 + rTile - 1;
      cornerX := Loc.X + (supply[3].MoveX + rx.Pivot[id].X + 9)/ CELL_SIZE_PX - 1;
      cornerY := Loc.Y + (supply[3].MoveY + rx.Pivot[id].Y + rx.Size[id].Y + 8) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

      fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, Loc.X, Loc.Y);

    end;


  end;
end;


procedure TKMRenderPool.AddWholeHouse(H: TKMHouse; aFlagColor: Cardinal; aDoImmediateRender: Boolean = False;
  aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
begin
  if H <> nil then
  begin
    AddHouse(H.HouseType, H.Position, 1, 1, 0, -1, H.GetStonePic, H.GetSnowPic, aDoImmediateRender, aDoHighlight, aHighlightColor);

    AddHouseSupply(H.HouseType, H.Position, H.WareInArray, H.WareOutArray, H.WareOutPoolArray, aDoImmediateRender, aDoHighlight, aHighlightColor);

    if H.CurrentAction <> nil then
      gRenderPool.AddHouseWork(H.HouseType, H.Position, H.CurrentAction.SubAction, H.WorkAnimStep, H.WorkAnimStepPrev, aFlagColor, aDoImmediateRender, aDoHighlight, aHighlightColor);
  end;
end;

procedure TKMRenderPool.AddHousePearl(aPearlType: TKMPearlType; const aLoc: TKMPoint; const aStage : Byte;
                                      aWoodStep: Single; aStoneStep: Single; aSnowStep: Single;  aSnowID : Word; aFlagColor : Cardinal = 0;
                                      aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  rxData: TRXData;
  picWood, picStone, picSnow, id: Integer;
  P : TKMPearlData;
  I : Integer;
//const
    //WallTowerWariants : array of Integer = [2119, 2121, 2121, 2121, 2121, 2121, 2121];
  function CornerX(aPic: Integer): Single;
  begin
    Result := aLoc.X + rxData.Pivot[aPic].X / CELL_SIZE_PX - 1;
  end;

  function CornerY(aPic: Integer): Single;
  begin
    Result := aLoc.Y + (rxData.Pivot[aPic].Y + rxData.Size[aPic].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
  end;

begin
  // We cannot skip when WoodStep = 0 because building supply is rendered as a child.
  // Instead RenderSpriteAlphaTest will skip rendering when WoodStep = 0

  rxData := fRXData[rxHouses];
  P := gRes.Houses.Pearls[aPearlType];

  picWood := P.GetStagePic(aStage - 1) + 1;
  picStone := P.GetStagePic(aStage) + 1;
  picSnow := aSnowID{P.SnowPic} + 1;

  // If it's fully built we can render without alpha
  if (aWoodStep = 1) and (aStoneStep = 1) then
  begin
    picStone := picWood;
    // Snow only happens on fully built houses
    if gGameSettings.GFX.AllowSnowHouses
      and (aSnowStep > 0)
      and (picSnow <> 0)and (picSnow > 1000) then
    begin
      // If snow is 100% we only need to render snow sprite
      if (aSnowStep = 1)then
        fRenderList.AddSprite(rxHouses, picSnow, CornerX(picSnow), CornerY(picSnow){, gX, gY}, aLoc.X, aLoc.Y, $0)
      else
      begin
        // Render stone with snow blended on top using AlphaTest
        fRenderList.AddSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone){, gX, gY}, aLoc.X, aLoc.Y, $0);
        fRenderList.AddSprite(rxHouses, picSnow, CornerX(picSnow), CornerY(picSnow){, gX, gY}, aLoc.X, aLoc.Y, $0, aSnowStep);
      end;
    end
    else if aDoImmediateRender then
      RenderSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), $0, aDoHighlight, aHighlightColor)
    else
      fRenderList.AddSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone){, gX, gY}, aLoc.X, aLoc.Y, $0);

    for I := IfThen(aSnowStep > 0, P.SnowAnimations, 0) to High(P.A) do
    begin
      id := P.A[I].Animation[gTerrain.AnimStep] + 1;
      fRenderList.AddSprite(rxHouses, id, CornerX(id) + P.A[I].X / CELL_SIZE_PX, CornerY(id) + P.A[I].Y / CELL_SIZE_PX
                            {, gX, gY}, aLoc.X, aLoc.Y, aFlagColor);
    end;

  end
  else
  begin
    // Wood part of the house (may be seen below Stone part before construction is complete, e.g. Sawmill)
    fRenderList.AddSprite(rxHouses, picWood, CornerX(picWood), CornerY(picWood){, gX, gY}, aLoc.X, aLoc.Y, $0, aWoodStep);
    if aStoneStep > 0 then
      fRenderList.AddSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), aLoc.X, aLoc.Y, $0, aStoneStep);
  end;

end;

procedure TKMRenderPool.AddHousePasture(aLoc: TKMPoint; aFlagColor: Cardinal = 0;
                                       aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  rxData: TRXData;

  function CornerX(aX, aY, aPic: Integer): Single;
  begin
    Result := aX + rxData.Pivot[aPic].X / CELL_SIZE_PX - 1;
  end;
  function CornerY(aX, aY, aPic: Integer): Single;
  begin
    Result := aY + (rxData.Pivot[aPic].Y + rxData.Size[aPic].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aY, aX].RenderHeight / CELL_HEIGHT_DIV;
  end;

  procedure AddToRender(aX, aY, aOffX, aOffY, aID : Integer);
  var ground : Single; 
    gX, gY, cX, cY : Single;
  begin       
    ground := rxData.Pivot[aID].Y + rxData.Size[aID].Y;                 
    gX := aX + (rxData.Pivot[aID].X + rxData.Size[aID].X / 2) / CELL_SIZE_PX - 1;
    gY := aY + ground / CELL_SIZE_PX;
    cX := CornerX(aX, aY, aID) + aOffX / CELL_SIZE_PX;   
    cY := CornerY(aX, aY, aID) + aOffY / CELL_SIZE_PX;
    
     if aDoImmediateRender then
      RenderSprite(rxHouses, aID, cX, cY, $0, aDoHighlight, aHighlightColor)
    else
      fRenderList.AddSpriteG(rxHouses, aID, 0, cX, cY, gX, gY, aX, aY, $0);
  end;
begin
  rxData := fRXData[rxHouses];
  AddToRender(aLoc.X - 2, aLoc.Y, 0, 0, 2767);
  AddToRender(aLoc.X - 2, aLoc.Y - 3, 0, 0, 2768);   
  AddToRender(aLoc.X, aLoc.Y - 3, 0, 0, 2769);
  AddToRender(aLoc.X + 1, aLoc.Y - 3, 20, 0, 2770);
end;
procedure TKMRenderPool.AddHouseArena(aType: TKMDevelopmentTreeType; const aLoc: TKMPoint; aAnimStep: Cardinal; Colors : TKMCardinalArray;
                        aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  I : Integer;
  id: Cardinal;
  rxData: TRXData;
  A : TKMAnimation;
  C : Cardinal;
  count : Byte;

  function CornerX: Single;
  begin
    Result := aLoc.X + (rxData.Pivot[id].X + A.X) / CELL_SIZE_PX - 1;
  end;
  function CornerY: Single;
  begin
    Result := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y + A.Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
  end;

begin
  count := Length(Colors);
  for I := 0 to gRes.Houses.ArenaAnim[aType].Count - 1 do
  begin
    A := gRes.Houses.ArenaAnim[aType].Item[I];
    if A.Count = 0 then
      Continue;

    rxData := fRXData[rxHouses];

    id := A.Animation[aAnimStep] + 1;
    C := Colors[I mod count];
    if aDoImmediateRender then
      RenderSprite(rxHouses, id, cornerX, cornerY, 0, true)
    else
      fRenderList.AddSprite(rxHouses, id, CornerX, CornerY, aLoc.X, aLoc.Y, C, -1);
  end;
end;

procedure TKMRenderPool.AddHousePastureAnimal(const aLoc: TKMPointF; aAnimal : TKMPastureAnimalType; Action : TKMPastureAnimalAction; Dir : TKMDirection;
                                  aAnimStep: Cardinal; C1, C2: TColor4;
                                  aDoImmediateRender: Boolean = False);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY, gX, gY: Single;
  A : TKMAnimation;
begin
  A := aAnimal.Spec.Anim[Action, Dir];
  if A.Count = 0 then
    Exit;

  rxData := fRXData[rxUnits];

  id := A.Animation[aAnimStep];

  {gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
  gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;

  cornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
  cornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;
  }


  gX := aLoc.X + (rxData.Pivot[id].X + rxData.Size[id].X / 2) / CELL_SIZE_PX{ - 1};
  gY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX{ - 1.5};

  cornerX := aLoc.X + (rxData.Pivot[id].X) / CELL_SIZE_PX{ - 1};
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX{ - 1}
                   - gTerrain.RenderHeightAt(gX, gY);

  if aDoImmediateRender then
    RenderSprite(rxUnits, id, cornerX, cornerY, C1, true, C2)
  else
    fRenderList.AddSpriteG(rxUnits, id, 0, cornerX, cornerY, gX, gY, aLoc.RX, aLoc.RY, C1, -1, C2);
end;


procedure TKMRenderPool.AddAnimation(const aLoc: TKMPoint; aAnim: TKMAnimLoop; aAnimStep: Cardinal;
                                      aFlagColor: Cardinal; aRX : TRXType; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                      aFront : Boolean = false; aAlphaStep : Single = -1);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  if aAnim.Count = 0 then
    Exit;

  rxData := fRXData[aRX];

  id := aAnim.Step[aAnimStep mod Byte(aAnim.Count) + 1] + 1;

  cornerX := aLoc.X + (rxData.Pivot[id].X + aAnim.MoveX) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.MoveY + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(aRX, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(aRX, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;

procedure TKMRenderPool.AddAnimation(const aLoc: TKMPointF; aAnim: TKMAnimLoop; aAnimStep: Cardinal;
                                      aFlagColor: Cardinal; aRX : TRXType; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                      aFront : Boolean = false; aAlphaStep : Single = -1);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  if aAnim.Count = 0 then
    Exit;

  rxData := fRXData[aRX];

  id := aAnim.Step[aAnimStep mod Byte(aAnim.Count) + 1] + 1;

  cornerX := aLoc.X + (rxData.Pivot[id].X + aAnim.MoveX) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.MoveY + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[Round(aLoc.Y) + 1, Round(aLoc.X)].RenderHeight / CELL_HEIGHT_DIV;

  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(aRX, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, aFlagColor);
end;

procedure TKMRenderPool.AddAnimation(const aLoc: TKMPoint; aAnim: TKMAnimation; aAnimStep: Cardinal;
                                      aFlagColor: Cardinal; aRX : TRXType; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                      aFront : Boolean = false; aAlphaStep : Single = -1);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY, gX, gY: Single;
begin
  if aAnim.Count = 0 then
    Exit;

  rxData := fRXData[aRX];

  id := aAnim.Animation[aAnimStep];

  gX := aLoc.X + (rxData.Pivot[id].X + rxData.Size[id].X / 2) / CELL_SIZE_PX{ - 1};
  gY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX{ - 1.5};

  cornerX := aLoc.X + (rxData.Pivot[id].X + aAnim.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                    - gTerrain.RenderHeightAt(gX, gY);
  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(aRX, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(aRX, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;

procedure TKMRenderPool.AddAnimation(const aLoc: TKMPointF; aAnim: TKMAnimation; aAnimStep: Cardinal;
                                      aFlagColor: Cardinal; aRX : TRXType; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                      aFront : Boolean = false; aAlphaStep : Single = -1);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY, gX, gY: Single;
begin
  if aAnim.Count = 0 then
    Exit;

  rxData := fRXData[aRX];

  id := aAnim.Animation[aAnimStep];
  gX := aLoc.X + (rxData.Pivot[id].X + rxData.Size[id].X / 2) / CELL_SIZE_PX{ - 1};
  gY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX{ - 1.5};

  cornerX := aLoc.X + (rxData.Pivot[id].X + aAnim.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                    - gTerrain.RenderHeightAt(gX, gY);

  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(aRX, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, aFlagColor);
end;


procedure TKMRenderPool.AddAnimationG(const aLoc: TKMPointF; aAnim: TKMAnimation; aAnimStep: Cardinal;
                                      aFlagColor: Cardinal; aRX : TRXType; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                      aFront : Boolean = false; aAlphaStep : Single = -1);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY, gX, gY: Single;
begin
  if aAnim.Count = 0 then
    Exit;

  rxData := fRXData[aRX];

  id := aAnim.Animation[aAnimStep];

  gX := aLoc.X + (rxData.Pivot[id].X + rxData.Size[id].X / 2) / CELL_SIZE_PX - 1;
  gY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1.5;

  cornerX := aLoc.X + (rxData.Pivot[id].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                    - gTerrain.RenderHeightAt(gX, gY);

  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(aRX, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSpriteG(aRX, id, 0, cornerX, cornerY, gX, gY, aLoc.RX, aLoc.RY, aFlagColor);
end;

procedure TKMRenderPool.AddSprite(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                                  aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                  aFront : Boolean = false; aAlphaStep : Single = -1);
var
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  rxData := fRXData[RX];

  cornerX := aLoc.X + (rxData.Pivot[aID].X + aOffset.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[aID].Y + rxData.Size[aID].Y + aOffset.Y) / CELL_SIZE_PX - 1;

  if aFront then
    fRenderList.AddSpriteFront(RX, aID, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(RX, aID, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(RX, aID, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;

procedure TKMRenderPool.AddSprite(const aLoc: TKMPointF; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                                  aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                  aFront : Boolean = false; aAlphaStep : Single = -1);
var
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  rxData := fRXData[RX];

  cornerX := aLoc.X + (rxData.Pivot[aID].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[aID].Y + rxData.Size[aID].Y) / CELL_SIZE_PX - 1;

  if aFront then
    fRenderList.AddSpriteFront(RX, aID, cornerX, cornerY, 0, 0, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(RX, aID, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(RX, aID, cornerX, cornerY, Trunc(aLoc.X), Trunc(aLoc.Y), aFlagColor);
end;

procedure TKMRenderPool.AddSpriteWH(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                                  aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                  aFront : Boolean = false; aAlphaStep : Single = -1);
var
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  rxData := fRXData[RX];

  cornerX := aLoc.X + (rxData.Pivot[aID].X + aOffset.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[aID].Y + rxData.Size[aID].Y + aOffset.Y) / CELL_SIZE_PX - 1
              - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  if aFront then
    fRenderList.AddSpriteFront(RX, aID, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(RX, aID, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(RX, aID, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;

procedure TKMRenderPool.AddSpriteG(const aLoc, aOffset: TKMPoint; aID : Word; RX : TRXType; aFlagColor : Cardinal;
                                  aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0;
                                  aFront : Boolean = false; aAlphaStep : Single = -1);
var
  rxData: TRXData;
  cornerX, cornerY, gX, gY: Single;
begin
  rxData := fRXData[RX];

  cornerX := aLoc.X + (rxData.Pivot[aID].X + aOffset.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[aID].Y + rxData.Size[aID].Y + aOffset.Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  gX := aLoc.X + (rxData.Pivot[aID].X + rxData.Size[aID].X / 2) / CELL_SIZE_PX - 1;
  gY := aLoc.Y + (rxData.Pivot[aID].Y + rxData.Size[aID].Y) / CELL_SIZE_PX - 1.5;
  if aFront then
    fRenderList.AddSpriteFront(RX, aID, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor, aAlphaStep)
  else
  if aDoImmediateRender then
    RenderSprite(RX, aID, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSpriteG(RX, aID, 0, cornerX, cornerY, gX, gY, aLoc.X, aLoc.Y, aFlagColor);
end;

// Render house in wood
procedure TKMRenderPool.AddHouse(aHouse: TKMHouseType; const aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single; aWoodPic : Integer = -1; aStonePic : Integer = -1; aSnowPic : Integer = -1;
                               aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  rxData: TRXData;
  picWood, picStone, picSnow: Integer;
  groundWood, groundStone, gX, gY: Single;
//const
    //WallTowerWariants : array of Integer = [2119, 2121, 2121, 2121, 2121, 2121, 2121];
  function CornerX(aPic: Integer): Single;
  begin
    Result := aLoc.X + rxData.Pivot[aPic].X / CELL_SIZE_PX - 1;
  end;

  function CornerY(aPic: Integer): Single;
  begin
    Result := aLoc.Y + (rxData.Pivot[aPic].Y + rxData.Size[aPic].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
  end;

begin
  // We cannot skip when WoodStep = 0 because building supply is rendered as a child.
  // Instead RenderSpriteAlphaTest will skip rendering when WoodStep = 0

  rxData := fRXData[rxHouses];

  picWood := aWoodPic + 1;
  picSnow := aSnowPic + 1;
  picStone := aStonePic + 1;

  groundWood := rxData.Pivot[picWood].Y + rxData.Size[picWood].Y;
  groundStone := rxData.Pivot[picStone].Y + rxData.Size[picStone].Y;

  gX := aLoc.X + (rxData.Pivot[picWood].X + rxData.Size[picWood].X / 2) / CELL_SIZE_PX - 1;
  gY := aLoc.Y + Max(groundWood, groundStone) / CELL_SIZE_PX - 1.5;

  // If it's fully built we can render without alpha
  if (aWoodStep = 1) and (aStoneStep = 1) then
  begin
    // Snow only happens on fully built houses
    if gGameSettings.GFX.AllowSnowHouses
      and (aSnowStep > 0)
      and (picSnow <> 0)and (picSnow <> 6) then
    begin
      // If snow is 100% we only need to render snow sprite
      if (aSnowStep = 1)then
        fRenderList.AddSpriteG(rxHouses, picSnow, 0, CornerX(picSnow), CornerY(picSnow), gX, gY, aLoc.X, aLoc.Y, $0)
      else
      begin
        // Render stone with snow blended on top using AlphaTest
        fRenderList.AddSpriteG(rxHouses, picStone, 0, CornerX(picStone), CornerY(picStone), gX, gY, aLoc.X, aLoc.Y, $0);
        fRenderList.AddSpriteG(rxHouses, picSnow, 0, CornerX(picSnow), CornerY(picSnow), gX, gY, aLoc.X, aLoc.Y, $0, aSnowStep);
      end;
    end
    else if aDoImmediateRender then
      RenderSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), $0, aDoHighlight, aHighlightColor)
    else
      fRenderList.AddSpriteG(rxHouses, picStone, 0, CornerX(picStone), CornerY(picStone), gX, gY, aLoc.X, aLoc.Y, $0);
  end
  else
  begin
    // Wood part of the house (may be seen below Stone part before construction is complete, e.g. Sawmill)
    fRenderList.AddSpriteG(rxHouses, picWood, 0, CornerX(picWood), CornerY(picWood), gX, gY, aLoc.X, aLoc.Y, $0, aWoodStep);
    if aStoneStep > 0 then
      fRenderList.AddSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), aLoc.X, aLoc.Y, $0, aStoneStep);
  end;
end;


procedure TKMRenderPool.AddHouseWork(aHouse: TKMHouseType; const aLoc: TKMPoint; aActSet: TKMHouseActionSet; aAnimStep, aAnimStepPrev: Cardinal;
                                   aFlagColor: TColor4; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  id: Cardinal;
  AT: TKMHouseActionType;
  A: TKMAnimation;
  rxData: TRXData;
  cornerX, cornerY: Single;
const
  //These house actions should animate smoothly and continuously, regardless of AnimStep
  CONTINUOUS_ANIMS: TKMHouseActionSet = [
    haSmoke, haFlagpole,
    haFlag1, haFlag2, haFlag3,
    haFire1, haFire2, haFire3, haFire4, haFire5, haFire6, haFire7, haFire8
  ];
begin

  if aActSet = [] then Exit;

  rxData := fRXData[rxHouses];

  //See if action is in set and render it
  for AT := Low(TKMHouseActionType) to high(TKMHouseActionType)  do
  if AT in aActSet then
  begin
    A := gRes.Houses[aHouse].Anim[AT];
    if A.Count > 0 then
    begin

      id := A.Animation[aAnimStep] + 1;

      cornerX := aLoc.X + (rxData.Pivot[id].X + A.X) / CELL_SIZE_PX - 1;
      cornerY := aLoc.Y + (rxData.Pivot[id].Y + A.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

      if aDoImmediateRender then
        RenderSprite(rxHouses, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
      else
        fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
    end;
  end;
end;

procedure TKMRenderPool.AddHouseAnimation(const aLoc: TKMPoint; aAnimation : TKMAnimation; aAnimStep: Cardinal; aFlagColor: TColor4; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  id: Cardinal;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin
  If aAnimation.Count = 0 then
    Exit;

  rxData := fRXData[rxHouses];

  id := aAnimation.Animation[aAnimStep];

  cornerX := aLoc.X + (rxData.Pivot[id].X + aAnimation.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnimation.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  if aDoImmediateRender then
    RenderSprite(rxHouses, id, cornerX, cornerY, aFlagColor, aDoHighlight, aHighlightColor)
  else
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;


procedure TKMRenderPool.AddHouseSchoolClock(const aLoc: TKMPoint; aAnimStep: Cardinal;
                                   aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  id: Cardinal;
  A: TKMAnimation;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin

  rxData := fRXData[rxHouses];

  A := gRes.Houses.School_Clock;
  if A.Count > 0 then
  begin

    id := A.Animation[aAnimStep];

    cornerX := aLoc.X + (rxData.Pivot[id].X + A.X) / CELL_SIZE_PX - 1;
    cornerY := aLoc.Y + (rxData.Pivot[id].Y + A.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

    if aDoImmediateRender then
      RenderSprite(rxHouses, id, cornerX, cornerY, 0, aDoHighlight, aHighlightColor)
    else
      fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, 0);
  end;
end;

procedure TKMRenderPool.AddHouseTowerRecruits(const aLoc: TKMPoint; aRight, aLeft : Boolean;
                              aAnimStep: Cardinal; aDoImmediateRender: Boolean = False);
var
  id: Cardinal;
  A: TKMAnimation;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin

  rxData := fRXData[rxHouses];

  If aRight then
  begin
    A := gRes.Houses.WallTower_RecruitRight;
    if A.Count > 0 then
    begin

      id := A.Animation[aAnimStep];

      cornerX := aLoc.X + (rxData.Pivot[id].X + A.X) / CELL_SIZE_PX - 1;
      cornerY := aLoc.Y + (rxData.Pivot[id].Y + A.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

      if aDoImmediateRender then
        RenderSprite(rxHouses, id, cornerX, cornerY, 0, false, 0)
      else
        fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, 0);
    end;
  end;
  If aLeft then
  begin
    A := gRes.Houses.WallTower_RecruitLeft;
    if A.Count > 0 then
    begin

      id := A.Animation[aAnimStep];

      cornerX := aLoc.X + (rxData.Pivot[id].X + A.X) / CELL_SIZE_PX - 1;
      cornerY := aLoc.Y + (rxData.Pivot[id].Y + A.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                       - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

      if aDoImmediateRender then
        RenderSprite(rxHouses, id, cornerX, cornerY, 0, false, 0)
      else
        fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, 0);
    end;
  end;
end;

procedure TKMRenderPool.AddHouseSupply(aHouse: TKMHouseType; const aLoc: TKMPoint; const R1, R2, R3: array of Byte;
                                     aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  id, I, K, I2, count: Integer;
  rxData: TRXData;

  procedure AddHouseSupplySprite(aId: Integer);
  var
    CornerX, CornerY: Single;
  begin
    if aId = 0 then Exit;

    CornerX := aLoc.X + rxData.Pivot[aId].X / CELL_SIZE_PX - 1;
    CornerY := aLoc.Y + (rxData.Pivot[aId].Y + rxData.Size[aId].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

    if aDoImmediateRender then
    begin
      RenderSprite(rxHouses, aId, CornerX, CornerY, $0, aDoHighlight, aHighlightColor)
    end else
      fRenderList.AddSprite(rxHouses, aId, CornerX, CornerY, aLoc.X, aLoc.Y);
  end;

begin
  if  (aHouse in WALL_HOUSES) then Exit;

  rxData := fRXData[rxHouses];
  for I := 1 to length(R1) do
    if (R1[I - 1]) > 0 then
    begin
      count := Min(R1[I - 1], MAX_WARES_IN_HOUSE);
      I2 := I;

      // Need to swap Coal and Steel for the ArmorSmithy
      // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
      if (aHouse = htArmorSmithy) and (I in [1,2]) then
        I2 := 3-I;

      // Need to swap Timber and Leather for the ArmorWorkshop
      // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
      if (aHouse = htArmorWorkshop) and (I in [1,2]) then
        I2 := 3-I;

      id := gRes.Houses[aHouse].SupplyIn[I2, count] + 1;
      AddHouseSupplySprite(id);
    end;

  if length(R3) > 0 then
    if gRes.Houses[aHouse].IsWorkshop then
    begin
      {for I := 1 to length(R2) do
        if R2[I - 1] > 0 then
        begin
          count := Min(R2[I - 1], MAX_WARES_IN_HOUSE);
          for K := 1 to count do
          begin
            id := gRes.Houses[aHouse].SupplyOut[I, K] + 1;
            AddHouseSupplySprite(id);
          end;
        end;}

      for K := 0 to 19 do
        if R3[K] > 0 then
        begin
          I2 := R3[K];
          id := gRes.Houses[aHouse].SupplyOut[I2, K mod MAX_WARES_IN_HOUSE + 1] + 1;
          AddHouseSupplySprite(id);
        end;

    end
    else
    begin
      for I := 1 to length(R2) do
        if R2[I - 1] > 0 then
        begin
          count := Min(R2[I - 1], MAX_WARES_IN_HOUSE);
          id := gRes.Houses[aHouse].SupplyOut[I, count] + 1;

            AddHouseSupplySprite(id);
        end;
    end;
end;

procedure TKMRenderPool.AddForestLogs(const aLoc: TKMPoint; aCount: Byte; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  id: Integer;
  rxData: TRXData;

  procedure AddHouseSupplySprite(aId: Integer);
  var
    CornerX, CornerY: Single;
  begin
    if aId = 0 then Exit;

    CornerX := aLoc.X + rxData.Pivot[aId].X / CELL_SIZE_PX - 1;
    CornerY := aLoc.Y + (rxData.Pivot[aId].Y + rxData.Size[aId].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

    if aDoImmediateRender then
    begin
      RenderSprite(rxHouses, aId, CornerX, CornerY, $0, aDoHighlight, aHighlightColor)
    end else
      fRenderList.AddSprite(rxHouses, aId, CornerX, CornerY, aLoc.X, aLoc.Y);
  end;
begin
  rxData := fRXData[rxHouses];
  aCount := Min(aCount, 41);
  id := 2858 + aCount - 1;
  AddHouseSupplySprite(id);
end;

procedure TKMRenderPool.AddPastureSilos(const aLoc: TKMPoint; aID: Word; aDoImmediateRender: Boolean = False; aDoHighlight: Boolean = False; aHighlightColor: TColor4 = 0);
var
  rxData: TRXData;

  procedure AddHouseSupplySprite;
  var
    CornerX, CornerY: Single;
  begin
    if aId = 0 then Exit;

    CornerX := aLoc.X + rxData.Pivot[aId].X / CELL_SIZE_PX - 1;
    CornerY := aLoc.Y + (rxData.Pivot[aId].Y + rxData.Size[aId].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

    if aDoImmediateRender then
    begin
      RenderSprite(rxHouses, aId, CornerX, CornerY, $0, aDoHighlight, aHighlightColor)
    end else
      fRenderList.AddSprite(rxHouses, aId, CornerX, CornerY, aLoc.X, aLoc.Y);
  end;
begin
  rxData := fRXData[rxHouses];
  AddHouseSupplySprite;
end;


procedure TKMRenderPool.AddHouseMarketSupply(const aLoc: TKMPoint; aResType: TKMWareType; aResCount: Word; aAnimStep: Integer);
var
  I, id: Integer;
  cornerX, cornerY: Single;
  rxData: TRXData;
begin
  if aResType = wtHorse then // Horses are a beast, BeastId is the count, age is 1
    for I := 1 to Min(aResCount, MARKET_WARES[aResType].Count) do // Render each beast
      AddHouseStableBeasts(htMarket, aLoc, I, 1, aAnimStep, rxHouses)
  else
  begin
    if MARKET_WARES[aResType].Count = 0 then Exit;
    id := (MARKET_WARES[aResType].TexStart-1) + Min(aResCount, MARKET_WARES[aResType].Count);
    if id = 0 then Exit;

    rxData := fRXData[rxHouses];
    cornerX := aLoc.X + (rxData.Pivot[id].X + MARKET_WARES_OFF_X) / CELL_SIZE_PX - 1;
    cornerY := aLoc.Y + (rxData.Pivot[id].Y + MARKET_WARES_OFF_Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.LandExt^[aLoc.Y+1,aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y);
  end;
end;


procedure TKMRenderPool.AddHouseStableBeasts(aHouse: TKMHouseType; const aLoc: TKMPoint; aBeastId, aBeastAge, aAnimStep: Integer; aRX: TRXType = rxHouses);
var
  cornerX, cornerY: Single;
  id: Integer;
  rxData: TRXData;
  A: TKMAnimLoop;
begin
  rxData := fRXData[aRX];

  A := gRes.Houses.BeastAnim[aHouse,aBeastId,aBeastAge];

  id := A.Step[aAnimStep mod A.Count + 1] + 1;

  cornerX := aLoc.X + (A.MoveX + rxData.Pivot[id].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (A.MoveY + rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(aRX, id, cornerX, cornerY, aLoc.X, aLoc.Y);
end;

procedure TKMRenderPool.AddHousePalaceFlags(aHouse: TKMHouseType; const aLoc: TKMPoint; aFlagID: Integer; aAnimStep: Integer; FlagColor : Cardinal);
var
  cornerX, cornerY: Single;
  id: Integer;
  rxData: TRXData;
  A: TKMAnimation;
begin
  rxData := fRXData[rxHouses];

  A := gRes.Houses.Palace_Flags[aFlagID];

  id := A.Animation[aAnimStep] + 1;

  cornerX := aLoc.X + (A.X + rxData.Pivot[id].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (A.Y + rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y, FlagColor);
end;

procedure TKMRenderPool.AddHousePotteryTiles(const aLoc: TKMPoint; aTileID: Byte);
var
  id: Integer;
  cornerX, cornerY: Single;
  rxData: TRXData;
begin
  rxData := fRXData[rxHouses];
  case aTileID of
    1: id := 2218;
    2: id := 2219;
    3: id := 2220;
    4: id := 2221;
    else id := 5;
  end;

  cornerX := aLoc.X + (rxData.Pivot[id].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y);


end;

procedure TKMRenderPool.AddHouseMerchantChests(const aLoc: TKMPoint; aOffsetX: Integer; aOffsetY: Integer);
var
  id: Integer;
  cornerX, cornerY: Single;
  rxData: TRXData;
begin
  rxData := fRXData[rxHouses];

  id := 2237;

  cornerX := aLoc.X + (rxData.Pivot[id].X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;

  cornerX := cornerX + aOffsetX / CELL_SIZE_PX;
  cornerY := cornerY + aOffsetY / CELL_SIZE_PX;

  fRenderList.AddSprite(rxHouses, id, cornerX, cornerY, aLoc.X, aLoc.Y);



end;

procedure TKMRenderPool.AddSpecAnim(const aLoc: TKMPointF; aAnim: TKMAnimation; aSTep: Byte; aRX : TRXType = rxTrees; aFront : Boolean = true; aAlphaStep : Single = -1);
var
  id: Integer;
  rxData: TRXData;
  cornerX, cornerY: Single;
begin

  // We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not gTerrain.TileInMapCoords(Round(aLoc.X), Round(aLoc.Y)) then Exit;

  if gGameParams.DynamicFOW then
  begin
    if gMySpectator.FogOfWar.CheckRevelation(aLoc) <= 128 then Exit; // Don't render objects which are behind FOW
  end;
  if aAnim.Count = 0 then
    Exit;
  id := aAnim.Animation[aStep];
  rxData := fRXData[aRX];

  cornerX := aLoc.X +  (rxData.Pivot[id].X + aAnim.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1 - gTerrain.LandExt^[Round(aLoc.Y) + 1, Round(aLoc.X)].RenderHeight / CELL_HEIGHT_DIV ;
  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, 0, aAlphaStep)
  else
    fRenderList.AddSpriteG(aRX, id, 0, cornerX, cornerY, cornerX, cornerY, aLoc.RX, aLoc.RY );
  //RenderSprite(aRX, id, cornerX, cornerY, $0)
end;

procedure TKMRenderPool.AddSpecAnim(const aLoc, aGround: TKMPointF; aAnim: TKMAnimation; aSTep: Byte; aRX : TRXType = rxTrees; aFront : Boolean = true; aAlphaStep : Single = -1);
var
  id: Integer;
  rxData: TRXData;
  cornerX, cornerY: Single;
  gY : Single;
begin

  // We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not gTerrain.TileInMapCoords(Trunc(aLoc.X), Trunc(aLoc.Y)) then Exit;

  if gGameParams.DynamicFOW then
  begin
    if gMySpectator.FogOfWar.CheckRevelation(aLoc) <= 128 then Exit; // Don't render objects which are behind FOW
  end;
  if aAnim.Count = 0 then
    Exit;
  id := aAnim.Animation[aStep];
  rxData := fRXData[aRX];

  cornerX := aLoc.X +  (rxData.Pivot[id].X + aAnim.X) / CELL_SIZE_PX - 1;
  cornerY := aLoc.Y + (rxData.Pivot[id].Y + aAnim.Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1
              - gTerrain.LandExt^[Trunc(aLoc.Y) + 1, Trunc(aLoc.X)].RenderHeight / CELL_HEIGHT_DIV ;

  gY := aGround.Y + (rxData.Pivot[id].Y + aAnim.Y + rxData.Size[id].Y) / CELL_SIZE_PX;

  if aFront then
    fRenderList.AddSpriteFront(aRX, id, cornerX, cornerY, aLoc.RX, aLoc.RY, 0, aAlphaStep)
  else
    fRenderList.AddSpriteG(aRX, id, 0, cornerX, cornerY, aGround.X, gY, aLoc.RX, aLoc.RY );
  //RenderSprite(aRX, id, cornerX, cornerY, $0)
end;

// aRenderPos has gTerrain.HeightAt factored in already, aTilePos is on tile coordinates for Z ordering
procedure TKMRenderPool.AddProjectile(aProj: TKMProjectileType; const aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
var
  FOW: Byte;
  id: Integer;
  rxData: TRXData;
  cornerX, cornerY: Single;
  ground, fracStep: Single;
  A : TKMAnimation;
  step : Integer;
begin
  // We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not gTerrain.TileInMapCoords(Round(aRenderPos.X), Round(aRenderPos.Y)) then Exit;

  if gGameParams.DynamicFOW then
  begin
    FOW := gMySpectator.FogOfWar.CheckRevelation(aRenderPos);
    if FOW <= 128 then Exit; // Don't render objects which are behind FOW
  end;

  case aProj of
    ptTowerBolt:      A := gRes.Units[utCrossbowman].UnitAnim[uaSpec, aDir];
    ptWallBolt,
    ptArrow:     A := gRes.Units[utBowman].UnitAnim[uaSpec, aDir];
    ptBallistaBolt:      A := gRes.Units[utBallista].UnitAnim[uaSpec, aDir];
    ptBolt:      A := gRes.Units[utCrossbowman].UnitAnim[uaSpec, aDir];
    ptSlingRock: A :=gRes.Units[utRogue].UnitAnim[uaSpec, aDir];
    ptTowerRock,
    ptCatapultRock:      A :=gRes.Units[utCatapult].UnitAnim[uaSpec, aDir];
    //ptTowerRock: A :=gRes.Units[utRecruit].UnitAnim[uaSpec, aDir];
    else  Exit;
  end;

  if A.Count = 0 then
    Exit;

  fracStep := Min(aFlight, 1) * (A.count-1);
  step := Trunc(fracStep);
  step := step mod A.Count;

  id := A.Animation[step] + 1;
  rxData := fRXData[rxUnits];

  cornerX := rxData.Pivot[id].X / CELL_SIZE_PX - 1;
  cornerY := (rxData.Pivot[id].Y + rxData.Size[id].Y) / CELL_SIZE_PX - 1;

  case aProj of
    ptTowerBolt,
    ptBallistaBolt,
    ptArrow, ptBolt, ptSlingRock:  ground := aTilePos.Y + (0.5 - Abs(Min(aFlight, 1) - 0.5)) - 0.5;
    ptCatapultRock,
    ptTowerRock:                     ground := aTilePos.Y + Min(aFlight, 1)/5 - 0.4;
    else                              ground := aTilePos.Y - 1; // Nothing?
  end;

  fRenderList.AddSpriteG(rxUnits, id, 0, aRenderPos.X + cornerX, aRenderPos.Y + cornerY, aTilePos.X - 1, ground, aRenderPos.RX, aRenderPos.RY);
end;


procedure TKMRenderPool.AddUnit(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; StepFrac: Single;
                              pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False;
                              DoHighlight: Boolean = False; HighlightColor: TColor4 = 0; aAlphaStep : Single = -1);
var
  cornerX, cornerY, ground: Single;
  id: Integer;
  R: TRXData;
  A : TKMAnimation;
begin
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];

  if A.Count = 0 then Exit;

  //id := A.Step[StepId mod A.Count + 1] + 1;
  id := A.Animation[StepId] + 1;

  if id <= 0 then exit;
  R := fRXData[rxUnits];

  cornerX := pX + (R.Pivot[id].X + A.X) / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[id].Y + R.Size[id].Y + A.Y) / CELL_SIZE_PX;
  ground := pY + (R.Pivot[id].Y + R.Size[id].Y + A.Y) / CELL_SIZE_PX;

  if DoImmediateRender then
    RenderSprite(rxUnits, id, cornerX, cornerY, FlagColor, DoHighlight, HighlightColor)
  else
    if NewInst then
      fRenderList.AddSpriteG(rxUnits, id, aUID, cornerX, cornerY, pX, ground, Max(Round(pX), 1), Max(Round(pY), 1), FlagColor, aAlphaStep)
    else
      fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, Round(pX), Round(pY), FlagColor, aAlphaStep);

  if SHOW_UNIT_MOVEMENT then
    if NewInst then
    begin
      gRenderAux.DotOnTerrain(pX, pY, FlagColor);
      gRenderAux.Dot(cornerX, cornerY, $FF000080);
    end;
end;


procedure TKMRenderPool.AddHouseEater(const Loc: TKMPoint; aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
var
  cornerX, cornerY: Single;
  id: Integer;
  R: TRXData;
  A : TKMAnimation;
  step : Integer;
begin
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
  step := StepID mod A.Count;

  id := A.Animation[step] + 1;

  if id <= 0 then exit;
  R := fRXData[rxUnits];

  // Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  cornerX := Loc.X + OffX + R.Pivot[id].X / CELL_SIZE_PX - 1;
  cornerY := Loc.Y + OffY + (R.Pivot[id].Y + R.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.LandExt^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, Loc.X, Loc.Y, FlagColor);
end;


procedure TKMRenderPool.AddUnitCarry(aCarry: TKMWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single; FlagColor: TColor4);
var
  cornerX, cornerY: Single;
  id: Integer;
  A: TKMAnimation;
  R: TRXData;
begin
  //if aCarry >= wtSteel then
   // aCarry := wtIron;

  A := gRes.Units.SerfCarry[aCarry, aDir];
  id := A.Animation[StepID] + 1;

  if id <= 0 then Exit;
  R := fRXData[rxUnits];

  cornerX := pX + (R.Pivot[id].X + A.X) / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[id].Y + R.Size[id].Y + A.Y) / CELL_SIZE_PX;
  fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, Round(pX), Round(pY), FlagColor);
end;


procedure TKMRenderPool.AddUnitThought(aUnit: TKMUnitType; aAct: TKMUnitActionType;
                                     aDir: TKMDirection;  aStep : Cardinal;
                                     Thought: TKMUnitThought; pX,pY: Single);
const
  ICON_OFF_X = 19;
  ICON_OFF_Y = -81;
var
  cornerX, cornerY{, ground}: Single;
  R: TRXData;
  A: TKMAnimation;
  id, {id0,} step: Integer;
  height : Single;
begin
  if Thought = thNone then Exit;
  R := fRXData[rxUnits];

  // Unit position
  step := gTerrain.AnimStep;
  //animCount := THOUGHT_BOUNDS[Thought, 2] - THOUGHT_BOUNDS[Thought, 1];
  A := gRes.Units.Thought;//gRes.Units[aUnit].UnitAnim[aAct, aDir];
  //id0 := gRes.Units[aUnit].UnitAnim[aAct, aDir].Animation[aStep];
  // Units feet
  // := pY + (R.Pivot[id0].Y + R.Size[id0].Y) / CELL_SIZE_PX;
  // The thought should be slightly lower than the unit so it goes OVER warrior flags
  //ground := ground + THOUGHT_X_OFFSET;

  id := A.Animation[step] + 1;
  height := gTerrain.RenderFlatToHeight(pX, pY);
  cornerX := pX + (R.Pivot[id].X + A.X) / CELL_SIZE_PX;
  cornerY := height + (R.Pivot[id].Y + R.Size[id].Y + A.Y) / CELL_SIZE_PX - 1.5;
  fRenderList.AddSpriteFront(rxUnits, id, cornerX, cornerY, Round(pX), Round(pY));
  //fRenderList.AddSpriteG(rxUnits, id, 0, cornerX, cornerY, pX, ground, Round(pX), Round(pY));

  id := THOUGHT_ICON[Thought];
  cornerX := pX + (ICON_OFF_X + R.Pivot[id].X + A.X) / CELL_SIZE_PX;
  cornerY := height + (ICON_OFF_Y + R.Pivot[id].Y + R.Size[id].Y + A.Y) / CELL_SIZE_PX;
  fRenderList.AddSpriteFront(rxUnits, id, cornerX, cornerY, Round(pX), Round(pY));
  //fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, Round(pX), Round(pY));
end;


procedure TKMRenderPool.AddUnitFlag(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection;
                                  FlagAnim: Integer; pX, pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False);
var
  R: TRXData;
  A: TKMAnimation;
  id0, idFlag: Integer;
  flagX, flagY, ground: Single;
begin
  R := fRXData[rxUnits];

  // Unit position
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
  if A.Count = 0 then Exit;


  // Units feet

  id0 := A.Animation[UNIT_STILL_FRAMES[aDir]] + 1;
  ground := pY + (R.Pivot[id0].Y + R.Size[id0].Y) / CELL_SIZE_PX;
  if id0 <= 0 then
    Exit;
  A := gRes.Units[aUnit].UnitAnim[uaWalkArm, aDir];
  if A.Count <= 0 then Exit;
  id0 := A.Animation[FlagAnim] + 1;
  // Flag position
  idFlag := id0;
  if idFlag <= 0 then Exit;

  flagX := pX + (R.Pivot[idFlag].X + A.X)/ CELL_SIZE_PX - 0.5;
  flagY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[idFlag].Y + R.Size[idFlag].Y + A.Y) / CELL_SIZE_PX - 0.5;

  if DoImmediateRender then
    RenderSprite(rxUnits, idFlag, flagX, flagY, FlagColor)
  else
    fRenderList.AddSpriteG(rxUnits, idFlag, 0, flagX, flagY, pX, ground, Round(pX), Round(pY), FlagColor);
end;


procedure TKMRenderPool.AddUnitWithDefaultArm(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHignlight: Boolean = False; HighlightColor: TColor4 = 0);
begin
  if aUnit = utFish then
    aAct := TKMUnitFish.GetFishActionType(FISH_CNT_DEFAULT); // In map editor always render default fish

  AddUnit(aUnit, aUID, aAct, aDir, StepId, 0.0, pX, pY, FlagColor, True, DoImmediateRender, DoHignlight, HighlightColor);
  if gRes.Units[aUnit].SupportsAction(uaWalkArm) then
    AddUnit(aUnit, aUID, uaWalkArm, aDir, StepId, 0.0, pX, pY, FlagColor, True, DoImmediateRender, DoHignlight, HighlightColor);
end;

procedure TKMRenderPool.AddUnitBitin(pX: Single; pY: Single);
var id : Integer;
  cornerX, cornerY: Single;
  R: TRXData;

begin
  id := 9602;

  R := fRXData[rxUnits];
  cornerX := pX + R.Pivot[id].X / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[id].Y + R.Size[id].Y) / CELL_SIZE_PX - 1;

  fRenderList.AddSpriteG(rxUnits, id, 0, cornerX, cornerY, cornerX, cornerY, Round(pX), Round(pY), $0);

end;

{procedure TRenderPool.RenderObject(aRX: TRXType; aId: Word; pX,pY: Single);
type
    TVector4f = record X,Y,Z,W: Single; end;
    TColor4f = record R,G,B,A: Single; end;
const
    LightPos: TVector4f = (X:-1; Y:0; Z:-2; W:0);
    LightAmb: TColor4f = (R:0.1; G:0.1; B:0.1; A:0);
    LightDiff: TColor4f = (R:0.9; G:0.9; B:0.9; A:0);
    LightSpec: TColor4f = (R:1.0; G:1.0; B:1.0; A:0);
begin
  glPushMatrix;
  glPushAttrib(GL_LIGHTING_BIT or GL_DEPTH_BUFFER_BIT);
    glScalef(1, 1, CELL_SIZE_PX);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmb);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
    glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpec);
    glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);

    glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 0.0); //Directional lighting
    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR); //Specular does not depend on material color

    glTranslatef(pX, pY, -1);
    glRotatef(32.5, 1, 0, 0);
    glColor4f(0.8, 0.8, 0.8, 1);

    fSampleHouse.DrawModel;
  glPopAttrib;
  glPopMatrix;
end;}

procedure TKMRenderPool.RenderSprite(aRX: TRXType; aId: Integer; aX: Single; aY: Single; Col: Cardinal; DoHighlight: Boolean = False; HighlightColor: Cardinal = 0; aForced: Boolean = False; aAlphaStep: Single = -1);
begin
  RenderSprite(aRX, aId, aX, aY, 1, col, DoHighlight, HighlightColor, aForced, aAlphaStep);
end;

procedure TKMRenderPool.RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
  night : Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  night := aNight;//gTerrain.GetNightAtTile(tX, tY);

  ///no shadow first
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // disable shadows
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    glAlphaFunc(GL_GEQUAL, 0.8);
    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      //glStencilOp(GL_DECR, GL_DECR, GL_DECR);
      TKMRender.BindTexture(Tex.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);


  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255);
    glColor4f(1 * night,1 * night,1 * night, 0 - aAlphaStep);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;
  glDisable(GL_STENCIL_TEST);

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      //glColor4ubv(@Col);
      glColor4ub( Round(Col AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 8 AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 16 AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 24 and $FF * (0.7 + night / 4) * Abs(aAlphaStep) )
                  );
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

  ///render shadow now
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // disable shadows
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    glAlphaFunc(GL_LESS, 0.8);
    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      //glStencilOp(GL_DECR, GL_DECR, GL_DECR);
      TKMRender.BindTexture(Tex.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);


  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255);
    if aAlphaStep <> -1 then
      glColor4f(1,1,1,  (0 - aAlphaStep) * night)
    else
      glColor4f(1,1,1,  1 * night);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;
  glDisable(GL_STENCIL_TEST);
end;


procedure TKMRenderPool.RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
  night : Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  night := aNight;//gTerrain.GetNightAtTile(tX, tY);

  ///no shadow first
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // disable shadows
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    glAlphaFunc(GL_GEQUAL, 0.8);
    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      //glStencilOp(GL_DECR, GL_DECR, GL_DECR);
      TKMRender.BindTexture(Tex.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);


  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255);
    glColor4f(1 * night,1 * night,1 * night, 0 - aAlphaStep);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor4f((HighlightColor AND $FF / 255) * night,
                (HighlightColor SHR 8 AND $FF / 255) * night,
                (HighlightColor SHR 16 AND $FF / 255) * night,
                 0 - aAlphaStep);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;
  glDisable(GL_STENCIL_TEST);

  if (gGFXData[aRX, aId].Alt.TexID <> 0) then
    with gGFXData[aRX, aId] do
    begin
      //glColor4ubv(@Col);
      glColor4ub( Round(Col AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 8 AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 16 AND $FF * (0.7 + night / 4)),
                  Round(Col SHR 24 and $FF * (0.7 + night / 4) * Abs(aAlphaStep) )
                  );
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

  ///render shadow now
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // disable shadows
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    glAlphaFunc(GL_LESS, 0.8);
    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      //glStencilOp(GL_DECR, GL_DECR, GL_DECR);
      TKMRender.BindTexture(Tex.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);


  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255 * TERRAIN_DARK, 255);
    if aAlphaStep <> -1 then
      glColor4f(1,1,1,  (0 - aAlphaStep) * night)
    else
      glColor4f(1,1,1,  1 * night);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;
  glDisable(GL_STENCIL_TEST);
end;


procedure TKMRenderPool.RenderSpriteFront(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;

// Param - defines at which level alpha-test will be set (acts like a threshhold)
// Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
// white there will have sprite rendered
// If there are two masks then we need to render sprite only there
// where its mask is white AND where second mask is black
procedure TKMRenderPool.RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
  aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
var
  tX, tY: Integer;
  rX, rY: Single;
  night : Single;
begin
  // Skip rendering if alphas are zero (occurs so non-started houses can still have child sprites)
  if (aWoodProgress = 0) and (aStoneProgress = 0) then Exit;

  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  if gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN then Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  X2 := RoundToTilePixel(X2);
  Y2 := RoundToTilePixel(Y2);
  night := aNight;//gTerrain.GetNightAtTile(tX, tY);
  ////no shadow first
  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // Prepare stencil mask. Sprite will be rendered only where are white pixels
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    // Wood progress
    glAlphaFunc(GL_GEQUAL, 1 - aWoodProgress);
    with gGFXData[aRX,aId] do
    begin
      //glColor3f(1, 1, 1);
      glColor3f(1 * night, 1 * night, 1 * night);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    // Disable Shadow
    if aShadow then
      glAlphaFunc(GL_GEQUAL, 0.8)
    else
      glAlphaFunc(GL_LEQUAL, 0.8);

    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);
      TKMRender.BindTexture(Tex.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;
    // Stone progress
    if aId2 <> 0 then
    begin
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);

      glAlphaFunc(GL_GREATER, 1 - aStoneProgress);
        with gGFXData[aRX,aId2] do
        begin
          glColor3f(1 * night, 1 * night, 1 * night);
          TKMRender.BindTexture(Alt.TexID);
          glBegin(GL_QUADS);
            glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2                     ,Y2         );
            glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2         );
            glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2-pxHeight/CELL_SIZE_PX);
            glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2                     ,Y2-pxHeight/CELL_SIZE_PX);
          glEnd;
          TKMRender.BindTexture(0);
        end;
    end;


    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  // Render sprite
  with gGFXData[aRX,aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255, 255, 255, 255);

    if aShadow then
      glColor4f(1, 1, 1, 1 * (night{ - MIN_NIGHT_DARKNESS}))
    else
      glColor4f(1 * night, 1 * night, 1 * night, 1);

    TKMRender.BindTexture(Tex.TexID);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


procedure TKMRenderPool.CollectPlans(const aRect: TKMRect);
var
  I: Integer;
begin
  fFieldsList.Clear;
  fHousePlansList.Clear;
  fBridgePlansList.Clear;
  // Collect field plans (road, corn, wine)
  if gGameParams.IsReplayOrSpectate then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetFieldPlans as it will give us plans multiple times for allies
        gHands[I].Constructions.FieldworksList.GetFields(fFieldsList, aRect, False)
    else
      gHands[gMySpectator.FOWIndex].GetFieldPlans(fFieldsList, aRect, False)
  end
  else
  begin
    // Field plans for self and allies
    // Include fake field plans for painting
    gMySpectator.Hand.GetFieldPlans(fFieldsList, aRect, True);
  end;

  // House plans for self and allies
  if gGameParams.IsReplayOrSpectate then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetHousePlans as it will give us plans multiple times for allies
        gHands[I].Constructions.HousePlanList.GetOutlines(fHousePlansList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetHousePlans(fHousePlansList, aRect)
  end
  else
    gMySpectator.Hand.GetHousePlans(fHousePlansList, aRect);

end;




procedure TKMRenderPoolNoNight.RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;


procedure TKMRenderPoolNoNight.RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;
// Param - defines at which level alpha-test will be set (acts like a threshhold)
// Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
// white there will have sprite rendered
// If there are two masks then we need to render sprite only there
// where its mask is white AND where second mask is black
procedure TKMRenderPoolNoNight.RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
  aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  // Skip rendering if alphas are zero (occurs so non-started houses can still have child sprites)
  if ((aWoodProgress = 0) and (aStoneProgress = 0)) or aShadow then Exit;

  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  if gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN then Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  X2 := RoundToTilePixel(X2);
  Y2 := RoundToTilePixel(Y2);

  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // Prepare stencil mask. Sprite will be rendered only where are white pixels
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    // Wood progress
    glAlphaFunc(GL_GEQUAL, 1 - aWoodProgress);
    with gGFXData[aRX,aId] do
    begin
      glColor4f(1, 1, 1, 1);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    // Stone progress
    if aId2 <> 0 then
    begin
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);

      glAlphaFunc(GL_GEQUAL, 1 - aStoneProgress);
        with gGFXData[aRX,aId2] do
        begin
          glColor4f(1, 1, 1, 1);
          TKMRender.BindTexture(Alt.TexID);
          glBegin(GL_QUADS);
            glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2                     ,Y2         );
            glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2         );
            glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2-pxHeight/CELL_SIZE_PX);
            glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2                     ,Y2-pxHeight/CELL_SIZE_PX);
          glEnd;
          TKMRender.BindTexture(0);
        end;
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  // Render sprite
  with gGFXData[aRX,aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TKMRender.BindTexture(Tex.TexID);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


procedure TKMRenderPoolNoShadows.RenderSprite(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255, 255, 255, 255);
    glColor4f(1 * aNight, 1 * aNight, 1 * aNight, 1);

    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;


procedure TKMRenderPoolNoShadows.RenderSpritePool(aRX: TRXType; aId: Integer; aX, aY, aNight: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False; aAlphaStep : Single = -1);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN) then
    Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    //glColor4ub(255, 255, 255, 255);
    glColor4f(1 * aNight, 1 * aNight, 1 * aNight, 1);
    TKMRender.BindTexture(Tex.TexID);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(rX                     , rY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.TexID <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(rX                     , rY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;
// Param - defines at which level alpha-test will be set (acts like a threshhold)
// Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
// white there will have sprite rendered
// If there are two masks then we need to render sprite only there
// where its mask is white AND where second mask is black
procedure TKMRenderPoolNoShadows.RenderSpriteAlphaTest(aShadow : Boolean; aRX: TRXType; aId: Integer; aWoodProgress: Single; aX, aY, aNight: Single;
  aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
var
  tX, tY: Integer;
  rX, rY: Single;
begin
  // Skip rendering if alphas are zero (occurs so non-started houses can still have child sprites)
  if ((aWoodProgress = 0) and (aStoneProgress = 0)) or aShadow then Exit;

  tX := EnsureRange(Round(aX), 1, gTerrain.MapX) - 1;
  tY := EnsureRange(Round(aY), 1, gTerrain.MapY) - 1;
  if gMySpectator.FogOfWar.CheckVerticeRenderRev(tX, tY) <= FOG_OF_WAR_MIN then Exit;

  rX := RoundToTilePixel(aX);
  rY := RoundToTilePixel(aY);

  X2 := RoundToTilePixel(X2);
  Y2 := RoundToTilePixel(Y2);

  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // Prepare stencil mask. Sprite will be rendered only where are white pixels
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    // Wood progress
    glAlphaFunc(GL_GEQUAL, 1 - aWoodProgress);
    with gGFXData[aRX,aId] do
    begin
          glColor3f(1, 1, 1);
      TKMRender.BindTexture(Alt.TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(rX                     , rY         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TKMRender.BindTexture(0);
    end;

    // Stone progress
    if aId2 <> 0 then
    begin
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);

      glAlphaFunc(GL_GEQUAL, 1 - aStoneProgress);
        with gGFXData[aRX,aId2] do
        begin
          glColor3f(1, 1, 1);
          TKMRender.BindTexture(Alt.TexID);
          glBegin(GL_QUADS);
            glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2                     ,Y2         );
            glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2         );
            glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2-pxHeight/CELL_SIZE_PX);
            glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2                     ,Y2-pxHeight/CELL_SIZE_PX);
          glEnd;
          TKMRender.BindTexture(0);
        end;
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  // Render sprite
  with gGFXData[aRX,aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor3f(1 * aNight, 1 * aNight, 1 * aNight);

    TKMRender.BindTexture(Tex.TexID);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(rX                     , rY         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(rX+pxWidth/CELL_SIZE_PX, rY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(rX                     , rY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TKMRender.BindTexture(0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


//Render wire on tile
//P - tile coords
//Col - Color
//aInset - Internal adjustment, to render wire "inside" tile
procedure TKMRenderPool.RenderWireTile(const P: TKMPoint; aCol: TColor4; aInset: Single = 0.0; aLineWidth: Single = -1);
begin
  if not gTerrain.TileInMapCoords(P.X, P.Y) then Exit;

  TKMRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  //Change LineWidth
  if aLineWidth > 0 then
    glLineWidth(aLineWidth);

  gRenderAux.RenderWireTile(P, aCol, aInset);

  if aLineWidth > 0 then
    SetDefaultRenderParams;
end;


// Until profiling we use straightforward approach of recreating outline each frame
// Optimize later if needed
procedure TKMRenderPool.RenderHouseOutline(aHouseSketch: TKMHouseSketch; aCol: TColor4 = icCyan);
var
  I: Integer;
  loc: TKMPoint;
  X, Y: Word;
begin
  if (aHouseSketch = nil) or aHouseSketch.IsEmpty then
    Exit;

  // Get an outline of build area
  fHouseOutline.Clear;

  loc := aHouseSketch.Position;
  gRes.Houses[aHouseSketch.HouseType].Outline(fHouseOutline);

  TKMRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)
//  glColor3f(0, 1, 1);
  glColor4ubv(@aCol);
  glBegin(GL_LINE_LOOP);
    with gTerrain do
    for I := 0 to fHouseOutline.Count - 1 do
    begin
      if (fHouseOutline[I].X = -1) and (fHouseOutline[I].Y = -1) then
      begin
        glEnd;
        glColor4ubv(@aCol);
        glBegin(GL_LINE_LOOP);
      end else
      begin
        X := loc.X + fHouseOutline[I].X - 3;
        Y := loc.Y + fHouseOutline[I].Y - 4;
        glVertex2f(X, Y - LandExt^[Y+1, X+1].RenderHeight / CELL_HEIGHT_DIV);
      end;
    end;
  glEnd;
end;


procedure TKMRenderPool.RenderSpriteOnTile(const aLoc: TKMPoint; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aRX : TRXType = rxGui);
var
  pX, pY: Single;
begin
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y)
    or (gMySpectator.FogOfWar.CheckVerticeRenderRev(aLoc.X,aLoc.Y) <= FOG_OF_WAR_MIN) then Exit;

  pX := aLoc.X - 0.5 + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.RenderFlatToHeight(aLoc.X - 0.5, aLoc.Y - 0.5) -
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(aRX, aId, pX, pY, gTerrain.GetNightAtTile(aLoc), aFlagColor);
end;


procedure TKMRenderPool.RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False; aRX : TRXType = rxGui);
var
  pX, pY: Single;
begin
  // if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;
  pX := aLoc.X + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.RenderFlatToHeight(aLoc.X, aLoc.Y) +
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(aRX, aId, pX, pY, gTerrain.GetNightAtTile(KMPoint(aLoc)), aFlagColor, False, 0, aForced);
end;


procedure TKMRenderPool.RenderWireHousePlan(const P: TKMPoint; aHouseType: TKMHouseType; aIgnoreObjects: Boolean = false);
var
  I: Integer;
  showHMarksIgnoreFOW: Boolean;
begin
  fMarksList.Clear;
  //Show house marks ignoring player FOW if we can see all map in replay/spec
  showHMarksIgnoreFOW := gGameParams.IsReplayOrSpectate and (gMySpectator.FOWIndex = -1);
  gMySpectator.Hand.GetHouseMarks(P, aHouseType, fMarksList, showHMarksIgnoreFOW, aIgnoreObjects);

  for I := 0 to fMarksList.Count - 1 do
  if fMarksList.Tag[I] = TC_OUTLINE then
    RenderWireTile(fMarksList[I], icCyan) // Cyan rect
  else
    RenderSpriteOnTile(fMarksList[I], fMarksList.Tag[I]); // Icon
end;


procedure TKMRenderPool.RenderForegroundUI_Markers;
var
  P: TKMPoint;
  house: TKMHouseWFlagPoint;
begin
  P := gCursor.Cell;
  case gCursor.Tag1 of
    MARKER_REVEAL:        begin
                            RenderSpriteOnTile(P, 394, gMySpectator.Hand.FlagColor);
                            gRenderAux.CircleOnTerrain(P.X-0.5, P.Y-0.5,
                             gCursor.MapEdSize,
                             gMySpectator.Hand.FlagColor AND $10FFFFFF,
                             gMySpectator.Hand.FlagColor);
                          end;
    MARKER_DEFEND:        begin
                            RenderSpriteOnTile(P, 955, gMySpectator.Hand.FlagColor);
                            gRenderAux.CircleOnTerrain(P.X-0.5, P.Y-0.5,
                             gCursor.MapEdSize,
                             gMySpectator.Hand.FlagColor AND $10FFFFFF,
                             gMySpectator.Hand.FlagColor);
                          end;
    MARKER_DEFENCE:       begin
                            if gCursor.MapEdDefPosSetGroup then
                              RenderForegroundUI_DefenceGroup;
                            RenderSpriteOnTile(P, Ord(gCursor.MapEdDirection) + 510, gMySpectator.Hand.FlagColor);
                            {case gCursor.MapEdDefPosGroupType of
                              gtMelee:            RenderSpriteOnTile(P, 371, gMySpectator.Hand.FlagColor);
                              gtAntiHorse:        RenderSpriteOnTile(P, 374, gMySpectator.Hand.FlagColor);
                              gtRanged:           RenderSpriteOnTile(P, 376, gMySpectator.Hand.FlagColor);
                              gtMounted:          RenderSpriteOnTile(P, 377, gMySpectator.Hand.FlagColor);
                              gtMachines:         RenderSpriteOnTile(P, 705, gMySpectator.Hand.FlagColor);
                              gtMachinesMelee:    RenderSpriteOnTile(P, 734, gMySpectator.Hand.FlagColor);
                              gtWreckers:         RenderSpriteOnTile(P, 891, gMySpectator.Hand.FlagColor);
                              gtShips:         RenderSpriteOnTile(P, 891, gMySpectator.Hand.FlagColor);
                            end;}
                            RenderSpriteOnTile(P, GROUP_TYPE_GUI_ICON[gCursor.MapEdDefPosGroupType], gMySpectator.Hand.FlagColor);
                            if gCursor.MapEdDefPosType = dtBackLine then
                              RenderWireTile(P, icBlue, 0.1);
                            if gCursor.MapEdDefPosType = dtGuardLine then
                              RenderWireTile(P, icOrange, 0.1);

                          end;
    MARKER_CENTERSCREEN:  RenderSpriteOnTile(P, 391, gMySpectator.Hand.FlagColor);
    MARKER_AISTART:       RenderSpriteOnTile(P, 390, gMySpectator.Hand.FlagColor);
    MARKER_RALLY_POINT:   if gMySpectator.Selected is TKMHouseWFlagPoint then
                          begin
                            house := TKMHouseWFlagPoint(gMySpectator.Selected);
                            PaintFlagPoint(house.Entrance, P, gMySpectator.Hand.FlagColor, gRes.Houses[house.HouseType].FlagPointTexId, True, True);
                          end;
    MARKER_ANIMALS:       begin
                            RenderSpriteOnTile(P, 914, gMySpectator.Hand.FlagColor);
                            gRenderAux.CircleOnTerrain(P.X-0.5, P.Y-0.5,
                             1,
                             icDeepGreen AND $55FFFFFF,
                             icDeepGreen);
                            gRenderAux.CircleOnTerrain(P.X-0.5, P.Y-0.5,
                             gCursor.MapEdSize,
                             icGreen AND $10FFFFFF,
                             icGreen);

                          end;
  end;
end;


procedure TKMRenderPool.RenderForegroundUI_ElevateEqualize;
var
  I, K: Integer;
  tmp: Single;
  rad, slope: Byte;
  F: TKMPointF;
begin
  F := gCursor.Float;
  rad := gCursor.MapEdSize;
  slope := gCursor.MapEdSlope;
  for I := Max((Round(F.Y) - rad), 1) to Min((Round(F.Y) + rad), gTerrain.MapY -1) do
    for K := Max((Round(F.X) - rad), 1) to Min((Round(F.X) + rad), gTerrain.MapX - 1) do
    begin
      case gCursor.MapEdShape of
        hsCircle: tmp := 1 - GetLength(I-Round(F.Y), K-Round(F.X)) / rad;
        hsSquare: tmp := 1 - Math.max(abs(I-Round(F.Y)), abs(K-Round(F.X))) / rad;
        else                 tmp := 0;
      end;
      tmp := Power(Abs(tmp), (slope + 1) / 6) * Sign(tmp); // Modify slopes curve
      tmp := EnsureRange(tmp * 2.5, 0, 1); // *2.5 makes dots more visible
      gRenderAux.DotOnTerrain(K, I, $FF or (Round(tmp*255) shl 24));
    end;
    case gCursor.MapEdShape of
      hsCircle: gRenderAux.CircleOnTerrain(round(F.X), round(F.Y), rad, $00000000,  $FFFFFFFF);
      hsSquare: gRenderAux.SquareOnTerrain(round(F.X) - rad, round(F.Y) - rad, round(F.X + rad), round(F.Y) + rad, $FFFFFFFF);
    end;
end;


procedure TKMRenderPool.RenderForegroundUI_ObjectsBrush;
begin
  IterateOverArea(gCursor.Cell, gCursor.MapEdSize, gCursor.MapEdShape = hsSquare, RenderWireTileIntObjBr);
end;

procedure TKMRenderPool.RenderWireTileInt(const X,Y: Integer);
begin
  RenderWireTile(KMPoint(X, Y), icLightCyan, 0, 0.3);
end;

procedure TKMRenderPool.RenderWireTileIntObjBr(const X,Y: Integer);
begin
  RenderWireTile(KMPoint(X, Y), icLightCyan, 0, -1);
  //RenderWireTile(KMPoint(X, Y), icLightCyan) // Cyan quad
end;

procedure TKMRenderPool.RenderTileInt(const X, Y: Integer);
begin
 if gCursor.MapEdSize = 0 then
    // Brush size smaller than one cell
    gRenderAux.DotOnTerrain(Round(gCursor.Float.X), Round(gCursor.Float.Y), $FF80FF80)
  else
    RenderTile(Combo[TKMTerrainKind(gCursor.Tag1), TKMTerrainKind(gCursor.Tag1),1],X,Y,0);
end;

procedure TKMRenderPool.RenderTileBrushInt(const X, Y: Integer);
begin
  If gCursor.MapEdDir in [0..3] then
    RenderTile(gCursor.Tag1,X,Y,gCursor.MapEdDir)
  else
    RenderTile(gCursor.Tag1,X,Y, (gTerrain.AnimStep div 5) mod 4);
end;


procedure TKMRenderPool.RenderForegroundUI_BigErase;
begin
  if gCursor.MapEdSize <= 1 then
    RenderBigEraseTileInt(gCursor.Cell.X, gCursor.Cell.Y)
  else
    IterateOverArea(gCursor.Cell, gCursor.MapEdSize, false, RenderBigEraseTileInt);
end;


procedure TKMRenderPool.RenderBigEraseTileInt(const X: Integer; const Y: Integer);
var P : TKMPoint;
begin
  P := KMPoint(X,Y);

  if not gTerrain.TileInMapCoords(X, Y) then Exit;

  if (gTerrain.Land^[Y,X].TileOverlay.IsRoad)
  or (gMySpectator.Hand.HousesHitTest(X, Y) <> nil)
  or (gTerrain.TileIsWineField(P))
  or (gTerrain.TileHasPalisade(X, Y))
  or (gTerrain.TileIsCornField(P))
  or (gTerrain.TileIsGrassField(P))
  then
    RenderWireTile(P, icCyan) // Cyan quad
  else
    RenderSpriteOnTile(P, TC_BLOCK); // Red X

end;

procedure TKMRenderPool.RenderForegroundUI_Brush;
var
  P, RP: TKMPoint;
  size: Integer;
  isSquare: Boolean;
begin
  P := gCursor.Cell;
  size := gCursor.MapEdSize;
  isSquare := gCursor.MapEdShape = hsSquare;
  if gCursor.MapEdUseMagicBrush then
    IterateOverArea(P, size, isSquare, RenderWireTileInt)
  else
  if gCursor.Tag1 <> 0 then
  begin
    if SHOW_BRUSH_APPLY_AREA then
    begin
      RP := P;
      if size = 0 then
        RP := KMPoint(Round(gCursor.Float.X+1), Round(gCursor.Float.Y+1));
      IterateOverArea(RP, size, isSquare, RenderWireTileInt, True); // Render surrounding tiles, that will be fixed with transitions
    end;
    IterateOverArea(P, size, isSquare, RenderTileInt);
  end;
end;

procedure TKMRenderPool.RenderForegroundUI_Tiles;
var
  P: TKMPoint;
  size: Integer;
  isSquare: Boolean;
begin
  P := gCursor.Cell;
  size := gCursor.MapEdSize;
  isSquare := gCursor.MapEdShape = hsSquare;
  IterateOverArea(P, size, isSquare, RenderTileBrushInt);
end;

//Render tile owner layer
procedure TKMRenderPool.RenderTileOwnerLayer(const aRect: TKMRect);
var
  I, K: Integer;
  P: TKMPoint;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      P := KMPoint(K, I);
      if    (gTerrain.Land^[I, K].TileOwner <> HAND_NONE) //owner is set for tile
        and (gTerrain.TileIsCornField(P)                   // show only for corn + wine + roads
          or gTerrain.TileIsWineField(P)
          or gTerrain.TileHasPalisade(P.X, P.Y)
          or (gTerrain.Land^[I, K].TileOverlay.IsRoad)) then
        RenderWireTile(P, gHands[gTerrain.Land^[I, K].TileOwner].FlagColor, 0.05);
    end;
end;


//Render tiles grid layer
procedure TKMRenderPool.RenderTilesGrid(const aRect: TKMRect);
var
  I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      RenderWireTile(KMPoint(K, I), icDarkCyan, 0, 1);
end;


procedure TKMRenderPool.RenderForegroundUI;
var
  P: TKMPoint;
  F: TKMPointF;
begin
  if gCursor.Cell.Y * gCursor.Cell.X = 0 then Exit; // Caused a rare crash

  TKMRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plCursors, KMRect(0,0,0,0));

  P := gCursor.Cell;
  F := gCursor.Float;

  if (gCursor.Mode <> cmNone) and (gCursor.Mode <> cmHouses) and
     (gMySpectator.FogOfWar.CheckTileRevelation(P.X, P.Y) = 0) then
    RenderSpriteOnTile(P, TC_BLOCK)       // Red X
  else

  with gTerrain do
  case gCursor.Mode of
    cmNone:       ;

    cmErase:      if gGameParams.IsMapEditor then
                  begin
                      RenderForegroundUI_BigErase;
                  end
                  else
                  begin
                    if ((gMySpectator.Hand.Constructions.FieldworksList.HasFakeField(P) <> ftNone)
                        or gMySpectator.Hand.Constructions.HousePlanList.HasPlan(P)
                        //or gMySpectator.Hand.Constructions.BridgePlanList.HasPlan(P)
                        or (gMySpectator.Hand.HousesHitTest(P.X, P.Y) <> nil))
                        or CanAddField(P.X, P.Y, ftRemove,gMySpectator.HandID)
                    then
                      RenderWireTile(P, icCyan) // Cyan quad
                    else
                      RenderSpriteOnTile(P, TC_BLOCK); // Red X
                  end;
    cmBridges:    RenderForegroundUI_Bridge;
    cmDecorations:RenderForegroundUI_Decoration;
    cmPalisade:   if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftPalisade)) and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X

    cmRoad:       If gGame.Params.IsMapEditor then
                  begin
                    If gCursor.MapEdSize > 1 then
                    begin
                      RenderForegroundUI_ObjectsBrush;
                    end else
                    if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftRoad)) and (gCursor.Tag1 <> Ord(cfmErase)) then
                      RenderWireTile(P, icCyan) // Cyan quad
                    else
                      RenderSpriteOnTile(P, TC_BLOCK);
                  end else
                  if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftRoad)) and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X

    cmField:      if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftCorn) or (gGameParams.IsMapEditor and gTerrain.TileIsCornField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmGrassLand:  if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftGrassLand) or (gGameParams.IsMapEditor and gTerrain.TileIsGrassField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmVegeField:  if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftVegeField) or (gGameParams.IsMapEditor and gTerrain.TileIsVegeField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X

    cmWine:       if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftWine) or (gGameParams.IsMapEditor and gTerrain.TileIsWineField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmHouses:     RenderWireHousePlan(KMPointAdd(P, gCursor.DragOffset), TKMHouseType(gCursor.Tag1), false); // Cyan quads and red Xs
    cmBrush:      RenderForegroundUI_Brush;
    cmTiles:      RenderForegroundUI_Tiles;{if gCursor.MapEdDir in [0..3] then
                    RenderTile(gCursor.Tag1, P.X, P.Y, gCursor.MapEdDir)
                  else
                    RenderTile(gCursor.Tag1, P.X, P.Y, (gTerrain.AnimStep div 5) mod 4);} // Spin it slowly so player remembers it is on randomized
    cmOverlays:   begin
                    RenderWireTile(P, icCyan);
                    if gCursor.Tag1 > 0 then
                      RenderTile(gRes.Tileset.Overlay[gCursor.Tag1].TileID, P.X, P.Y, 0);
                    end;
    cmObjects:    begin
                    // If there's object below - paint it in Red
                    RenderMapElement(gTerrain.Land^[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, True, True);
                    RenderMapElement(gCursor.Tag1, gTerrain.AnimStep, P.X, P.Y, True);
                  end;
    cmWaresOnGround:  begin
                        RenderForegroundUI_WareOnGround;
                      end;
            //AddSpriteG(KMPoint(K, I), KMPOINT_ZERO, J, rxTrees, 0);
    cmObjectsBrush: RenderForegroundUI_ObjectsBrush;
    cmTileSelection: RenderForegroundUI_ObjectsBrush;

    cmMagicWater: begin
                    If gTerrain.Land[P.Y, P.X].BaseLayer.Rotation+1 <=3 then
                      RenderTile(192, P.X, P.Y, gTerrain.Land[P.Y, P.X].BaseLayer.Rotation+1)
                    else
                      RenderTile(192, P.X, P.Y, 0);
                    RenderWireTile(P, icCyan);
                  end;
    cmEyeDropper: RenderWireTile(P, icCyan); // Cyan quad
    cmRotateTile: RenderWireTile(P, icCyan); // Cyan quad
    cmElevate,
    cmEqualize:         RenderForegroundUI_ElevateEqualize;
    cmConstHeight:      RenderForegroundUI_ElevateEqualize;
    cmUnits:            RenderForegroundUI_Units;
    cmMarkers:          RenderForegroundUI_Markers;
    cmPaintBucket:      RenderForegroundUI_PaintBucket(ssShift in gCursor.SState);
    cmUniversalEraser:  RenderForegroundUI_UniversalEraser(ssShift in gCursor.SState);
    cmChangeResCount:   RenderForegroundUI_ChangeResCount;
    cmPearlRepair:      RenderForegroundUI_SetHouseRepair;
    cmAssignToShip:     RenderAssignToShip;
    cmCustom:           case gCursor.Custom.RenderType of
                          crtWireTile : RenderWireTile(P, icCyan);
                          crtTile : RenderTile(gCursor.Custom.Tag1, P.X, P.Y, gCursor.Custom.Tag2);
                          crtObject : begin
                                        // If there's object below - paint it in Red
                                        RenderMapElement(gTerrain.Land^[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, True, True);
                                        RenderMapElement(gCursor.Custom.Tag1, gTerrain.AnimStep, P.X, P.Y, True);
                                      end;
                          crtUnit : RenderForegroundUI_UnitsCustom;
                          crtHouse : RenderWireHousePlan(P, HOUSE_ID_TO_TYPE[gCursor.Custom.Tag1], false);
                          crtHouseSite : RenderWireHousePlan(P, HOUSE_ID_TO_TYPE[gCursor.Custom.Tag1], true);
                          crtX : RenderSpriteOnTile(P, TC_BLOCK);
                          crtDelete : if ((gMySpectator.Hand.Constructions.FieldworksList.HasFakeField(P) <> ftNone)
                                          or gMySpectator.Hand.Constructions.HousePlanList.HasPlan(P)
                                          //or gMySpectator.Hand.Constructions.BridgePlanList.HasPlan(P)
                                          or (gMySpectator.Hand.HousesHitTest(P.X, P.Y) <> nil))
                                          or CanAddField(P.X, P.Y, ftRemove,gMySpectator.HandID)
                                      then
                                        RenderWireTile(P, icCyan) // Cyan quad
                                      else
                                        RenderSpriteOnTile(P, TC_BLOCK); // Red X
                        end;

  end;

  if DISPLAY_SOUNDS then gSoundPlayer.Paint;
end;


procedure TKMRenderPool.RenderUnit(U: TKMUnit; const P: TKMPoint; aFlagColor: Cardinal; aDoHighlight: Boolean; aHighlightColor: Cardinal);
begin
  RenderUnit(U.UnitType, KMPointDir(P, U.Direction), U.AnimStep, aFlagColor, aDoHighlight, aHighlightColor);
end;


procedure TKMRenderPool.RenderUnit(aUnitType: TKMUnitType; const P: TKMPointDir; aAnimStep: Integer; aFlagColor: Cardinal; aDoHighlight: Boolean = False; aHighlightColor: Cardinal = 0);
begin
  AddUnitWithDefaultArm(aUnitType, 0, uaWalk, P.Dir, aAnimStep, P.Loc.X + UNIT_OFF_X, P.Loc.Y + UNIT_OFF_Y,
                        aFlagColor, True, aDoHighlight, aHighlightColor);
end;


procedure TKMRenderPool.DoRenderGroup(aUnitType: TKMUnitType; aLoc: TKMPointDir; aMembersCnt, aUnitsPerRow: Integer; aHandColor: Cardinal);

  procedure PaintGroup;
  var
    I: Integer;
    unitPos: TKMPointF;
    newPos: TKMPoint;
    doesFit: Boolean;
  begin
    //Paint virtual members in MapEd mode
    for I := 1 to aMembersCnt - 1 do
    begin
      newPos := GetPositionInGroup2(aLoc.Loc.X, aLoc.Loc.Y, aLoc.Dir, I, aUnitsPerRow, gTerrain.MapX, gTerrain.MapY, doesFit);
      if not doesFit then Continue; //Don't render units that are off the map in the map editor
      unitPos.X := newPos.X + UNIT_OFF_X; //MapEd units don't have sliding
      unitPos.Y := newPos.Y + UNIT_OFF_Y;
      gRenderPool.AddUnit(aUnitType, 0, uaWalk, aLoc.Dir, UNIT_STILL_FRAMES[aLoc.Dir], 0.0, unitPos.X, unitPos.Y, aHandColor, True, True);
    end;
  end;

begin
  if TKMUnitGroup.IsFlagRenderBeforeUnit(aLoc.Dir) then
  begin
    PaintGroup;
    RenderUnit(aUnitType, aLoc, UNIT_STILL_FRAMES[aLoc.Dir], aHandColor);
  end else begin
    RenderUnit(aUnitType, aLoc, UNIT_STILL_FRAMES[aLoc.Dir], aHandColor);
    PaintGroup;
  end;
end;


//Try to render Unit or Unit group.
//Return True, if succeeded
function TKMRenderPool.TryRenderUnitOrGroup(aEntity: TKMHandEntity; aUnitFilterFunc, aGroupFilterFunc: TBooleanFunc;
                                          aUseGroupFlagColor, aDoHighlight: Boolean;
                                          aHandColor, aFlagColor: Cardinal; aHighlightColor: Cardinal = 0): Boolean;
var
  U: TKMUnit;
  G: TKMUnitGroup;
  groupFlagColor: Cardinal;
begin
  Result := False;
  if aEntity.IsUnit then
  begin
    U := TKMUnit(aEntity);
    if not Assigned(aUnitFilterFunc) or aUnitFilterFunc(aEntity) then
    begin
      RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
      Result := True;
    end;
  end else 
  if aEntity.IsGroup then
  begin
    G := TKMUnitGroup(aEntity);
    if not Assigned(aGroupFilterFunc) or aGroupFilterFunc(aEntity) then
    begin
      U := G.FlagBearer;
      if aUseGroupFlagColor then
        groupFlagColor := G.FlagColor
      else
        groupFlagColor := aFlagColor;

      if G.IsFlagRenderBeforeUnit(U.Direction) then
      begin
        G.PaintHighlighted(0.0, aHandColor, groupFlagColor, True, aDoHighlight, aHighlightColor);
        RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
      end else begin
        RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
        G.PaintHighlighted(0.0, aHandColor, groupFlagColor, True, aDoHighlight, aHighlightColor);
      end;
      Result := True;
    end;
  end;
end;

procedure TKMRenderPool.UnitsHitTest(const X, Y : Integer);
var entity : TKMHandEntity;
begin
  if not gTerrain.TileInMapCoords(X, Y) then Exit;

  entity := gHands.PlayerAnimals.UnitsHitTest(X, Y);
  if entity = nil then
    entity := gHands.UnitsHitTest(X, Y);

  TryRenderUnitOrGroup(entity, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);

end;

procedure TKMRenderPool.RenderForegroundUI_Units;
var
  P: TKMPoint;
  dir : TKMDirection;
  UT: TKMUnitType;
  formation: TKMFormation;
begin
  Assert(gGameParams.IsMapEditor);
  if gCursor.Tag1 = 255 then
  begin
    RenderForegroundUI_ObjectsBrush;
    IterateOverArea(gCursor.Cell, gCursor.MapEdSize, gCursor.MapEdShape = hsSquare, UnitsHitTest);
    //entity := gMySpectator.HitTestCursorWGroup(True);
    //TryRenderUnitOrGroup(entity, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);
  end
  else
  begin
    UT := TKMUnitType(gCursor.Tag1);
    dir := dirS;

    P := gCursor.Cell;
    if gTerrain.CanPlaceUnit(P, UT) then
    begin
      if UT in UNITS_WARRIORS then
      begin
        gGame.MapEditor.DetermineGroupFormationAndDir(P, UNIT_TO_GROUP_TYPE[TKMUnitType(gCursor.Tag1)], formation, dir);
        DoRenderGroup(UT, KMPointDir(P, dir), formation.NumUnits, formation.UnitsPerRow, gMySpectator.Hand.FlagColor)
      end
      else
        AddUnitWithDefaultArm(UT, 0, uaWalk, dir, UNIT_STILL_FRAMES[dirS], P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, gMySpectator.Hand.FlagColor, True);
    end
    else
      RenderSpriteOnTile(P, TC_BLOCK); // Red X
  end;
end;

procedure TKMRenderPool.RenderForegroundUI_DefenceGroup;
var
  P: TKMPoint;
  dir : TKMDirection;
  UT: TKMUnitType;
  formation: TKMFormation;
  groupType : TKMGroupType;
begin
  Assert(gGameParams.IsMapEditor);

  groupType := gCursor.MapEdDefPosGroupType;
  dir := gCursor.MapEdDirection;
  UT := UNIT_TYPES_BY_GT_LVL[groupType, gCursor.MapEdDefPosGroupLevel];

  P := gCursor.Cell;
  if gTerrain.CanPlaceUnit(P, UT) then
  begin
    formation := gMySpectator.Hand.AI.General.DefencePositions.TroopFormations[groupType];
    DoRenderGroup(UT, KMPointDir(P, dir), formation.NumUnits, formation.UnitsPerRow, gMySpectator.Hand.FlagColor)
  end
  else
    RenderSpriteOnTile(P, TC_BLOCK);

end;
procedure TKMRenderPool.RenderForegroundUI_UnitsCustom;
var
  P: TKMPoint;
  dir : TKMDirection;
  UT: TKMUnitType;
begin
  UT := UNIT_ID_TO_TYPE[gCursor.Custom.Tag1];
  dir := dirS;

  P := gCursor.Cell;
  if gTerrain.CanPlaceUnit(P, UT) then
  begin
    if UT in UNITS_WARRIORS then
    begin
      DoRenderGroup(UT, KMPointDir(P, dirS), 1, 1, gMySpectator.Hand.FlagColor)
    end
    else
      AddUnitWithDefaultArm(UT, 0, uaWalk, dir, UNIT_STILL_FRAMES[dirS], P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, gMySpectator.Hand.FlagColor, True);
  end
  else
    RenderSpriteOnTile(P, TC_BLOCK); // Red X
end;
procedure TKMRenderPool.RenderAssignToShip;
var U : TKMUnit;
  P : TKMPoint;
begin
  Assert(not gGameParams.IsMapEditor);
  P := gCursor.Cell;
  if gCursor.Tag1 = 1 then
  begin
    U := gMySpectator.Hand.UnitsHitTest(P);
    if (U = nil)
    or (U.UnitType in [utShip])
    or (gRes.Units[U.UnitType].ShipWeight <= 0)
    or not U.Visible
    or not U.IsIdle then
      RenderSpriteOnTile(P, TC_BLOCK) // Red X
    else
      TryRenderUnitOrGroup(U, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);



    Exit;
  end;

  U := gMySpectator.Hand.UnitsHitTest(P, utShip);
  if (U = nil) or not U.Visible then
    RenderSpriteOnTile(P, TC_BLOCK) // Red X
  else
  begin
    TryRenderUnitOrGroup(U, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);
  end;

end;

procedure TKMRenderPool.RenderForegroundUI_UniversalEraser(aHighlightAll: Boolean);
var
  entity: TKMHandEntity;
  P: TKMPoint;
  isRendered: Boolean;
begin
  P := gCursor.Cell;
  entity := gMySpectator.HitTestCursorWGroup(True);

  isRendered := TryRenderUnitOrGroup(entity, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);

  if (entity is TKMHouse) then
  begin
    AddWholeHouse(TKMHouse(entity), gHands[entity.Owner].FlagColor, True, True, DELETE_COLOR);
    isRendered := True;
  end;

  // Terrain object found on the cell
  if (aHighlightAll or not isRendered) and (gTerrain.Land^[P.Y,P.X].Obj <> OBJ_NONE) then
  begin
    RenderMapElement(gTerrain.Land^[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, True, True);
    isRendered := True;
  end;

  if (aHighlightAll or not isRendered) and
    (((gTerrain.Land^[P.Y, P.X].TileOverlay <> OVERLAY_NONE)
        and (gTerrain.Land^[P.Y, P.X].TileLock = tlNone)) //Sometimes we can point road tile under the house, do not show Cyan quad then
      or (gGame.MapEditor.LandMapEd^[P.Y, P.X].CornOrWine <> 0)) then
    RenderWireTile(P, icCyan); // Cyan quad
end;

procedure TKMRenderPool.RenderForegroundUI_ChangeResCount;
var
  entity: TKMHandEntity;
  P: TKMPoint;
begin
  P := gCursor.Cell;
  entity := gMySpectator.HitTestCursorWGroup(True);

  if (entity is TKMHouse) then
    AddWholeHouse(TKMHouse(entity), gHands[entity.Owner].FlagColor, True, True, icCyan);
end;

procedure TKMRenderPool.RenderForegroundUI_SetHouseRepair;
var
  H: TKMHouse;
  P: TKMPoint;
begin
  P := gCursor.Cell;
  H := gMySpectator.Hand.HousesHitTest(P.X, P.Y);

  if H <> nil then
  begin
    RenderWireTile(P, icCyan); // Cyan rect
    If H.BuildingRepair then
      RenderSpriteOnTile(P, 40)
    else
      RenderSpriteOnTile(P, 39);
  end
  else
    RenderSpriteOnTile(P, 340); // X

end;

procedure TKMRenderPool.RenderForegroundUI_Bridge;
var I : integer;
  P: TKMPoint;
  list : TKMPointTagList;
begin
  list := TKMPointTagList.Create;
  P := gCursor.Cell;
  //gRes.Bridges[gCursor.Tag1].SetRotation(gCursor.MapEdDir);
  gMySpectator.Hand.GetStructureMarks(P, gCursor.Tag1, gCursor.MapEdDir, list, gGameParams.IsReplayOrSpectate and (gMySpectator.FOWIndex = -1));
  {with gRes.Bridges[0] do
    for I := 0 to High(Points) do
      if Points[I] <> 0 then
      begin
        P.X := gCursor.Cell.X + (I mod Size.X) + Offset.X;
        P.Y := gCursor.Cell.Y + (I div Size.X) + Offset.Y;
        RenderWireTile(P, icCyan); // Cyan quad
      end;}
  for I := 0 to list.Count - 1 do
    if list.Tag[I] = TC_OUTLINE then
      RenderWireTile(list[I], icCyan) // Cyan rect
    else
      RenderSpriteOnTile(list[I], list.Tag[I]); // Icon
  list.Free;
end;

procedure TKMRenderPool.RenderForegroundUI_Decoration;
var P : TKMPoint;
begin
  P := gCursor.Cell;
  with gDecorations[gCursor.Tag1] do
  case DType of
    dtObject: RenderMapElement(ID, gTerrain.AnimStep, P.X, P.Y, true);
    dtTile,
    dtTileOverlay: RenderTile(ID, P.X, P.Y, 0);
  end;
end;

procedure TKMRenderPool.RenderForegroundUI_WareOnGround;
var P : TKMPoint;
  id, C : Integer;
  WT : TKMWareType;
  arr : TKMWordArray;
  step : Integer;
  cornerX, cornerY : Single;
  gX, gY: Single;
  R: TRXData;
begin
  P := gCursor.Cell;

  WT := TKMWareType(gCursor.Tag1);
  arr := gRes.Wares[WT].AtTerrainPic;
  C := gCursor.MapEd_WaresCount;
  step := (gTerrain.AnimStep div 4) mod length(arr);

  if C = 0 then //remove wares on ground
  begin
    gRenderAux.Quad(P.X, P.Y, $600000FF);
    RenderWireTile(P, icRed, 0, 0.8);
    Exit;
  end else
  if InRange(C - 1, 0, high(arr)) then //place specific ware count
  begin
    id := arr[C - 1];
  end else  //place random ware count
  begin
    id := arr[step];
  end;

  R := fRXData[rxTrees];
  gX := P.X - 1 + (R.Pivot[arr[0]].X + R.Size[arr[0]].X/2) / CELL_SIZE_PX;
  gY := P.Y - 1 - (R.Pivot[arr[0]].Y + R.Size[arr[0]].Y) / CELL_SIZE_PX;

  cornerX := P.X - 1 + R.Pivot[Id].X / CELL_SIZE_PX;
  cornerY := P.Y - 1 - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

  RenderWireTile(P, icLightCyan, 0, 1);
  RenderSprite(rxTrees, Id, cornerX, cornerY, $FFFFFFFF, false);
end;

function TKMRenderPool.PaintBucket_GroupToRender(aGroup: TObject): Boolean;
begin
   Result := (aGroup is TKMUnitGroup) and (TKMUnitGroup(aGroup).Owner <> gMySpectator.HandID);
end;


function TKMRenderPool.PaintBucket_UnitToRender(aUnit: TObject): Boolean;
begin
   Result := (aUnit is TKMUnit) and not (aUnit is TKMUnitAnimal) and
    (TKMUnit(aUnit).Owner <> gMySpectator.HandID);
end;


procedure TKMRenderPool.RenderForegroundUI_PaintBucket(aHighlightAll: Boolean);
var
  entity: TKMHandEntity;
  highlightColor: Cardinal;
  P: TKMPoint;
  isRendered: Boolean;
begin
  P := gCursor.Cell;
  highlightColor := MultiplyBrightnessByFactor(gMySpectator.Hand.FlagColor, 2, 0.3, 0.9);
  entity := gMySpectator.HitTestCursorWGroup;

  isRendered := TryRenderUnitOrGroup(entity, PaintBucket_UnitToRender, PaintBucket_GroupToRender,
                                     False, True,
                                     gMySpectator.Hand.FlagColor, gMySpectator.Hand.FlagColor, highlightColor);

  if entity.IsHouse and (entity.Owner <> gMySpectator.HandID) then
  begin
    AddWholeHouse(TKMHouse(entity), gMySpectator.Hand.FlagColor, True, True, highlightColor);
    isRendered := True;
  end;

  if (aHighlightAll or not isRendered) and
    (((gTerrain.Land^[P.Y, P.X].TileOverlay.IsRoad)
        and (gTerrain.Land^[P.Y, P.X].TileLock = tlNone)) //Sometimes we can point road tile under the house, do not show Cyan quad then
      or (gGame.MapEditor.LandMapEd^[P.Y, P.X].CornOrWine <> 0))
    and (gTerrain.Land^[P.Y, P.X].TileOwner <> gMySpectator.HandID) then //Only if tile has other owner
    RenderWireTile(P, icCyan); // Cyan quad
end;


{ TRenderList }
constructor TKMRenderList.Create;
begin
  inherited;

  // Pre-allocate some space
  SetLength(fRenderList, 512);

  fUnitsRXData := gRes.Sprites[rxUnits].RXData;
end;


destructor TKMRenderList.Destroy;
begin
  SetLength(fRenderList, 0);

  inherited;
end;


function TKMRenderList.GetSelectionUID(const aCurPos: TKMPointF): Integer;
var
  I, K: Integer;
begin
  Result := -1; // Didn't hit anything
  // Skip if cursor is over FOW
  if gMySpectator.FogOfWar.CheckRevelation(aCurPos) <= FOG_OF_WAR_MIN then Exit;
  // Select closest (higher Z) units first (list is in low..high Z-order)
  for I := Length(fRenderOrder) - 1 downto 0 do
  begin
    K := fRenderOrder[I];
    // Don't check child sprites, we don't want to select serfs by the long pike they are carrying
    if (fRenderList[K].UID > 0) and KMInRect(aCurPos, fRenderList[K].SelectionRect) then
    begin
      Result := fRenderList[K].UID;
      Exit;
    end;
  end;
end;


procedure TKMRenderList.Clear;
begin
  fCount := 0;
  SetLength(fFrontRenderList, 0);
end;


procedure TKMRenderList.ClipRenderList;
var
  I, J: Integer;
begin
  SetLength(fRenderOrder, fCount);
  J := 0;
  for I := 0 to fCount - 1 do
    if fRenderList[I].NewInst then
    begin
      fRenderOrder[J] := I;
      Inc(J);
    end;
  SetLength(fRenderOrder, J);
end;


// Sort all items in list from top-right to bottom-left
procedure TKMRenderList.SortRenderList;
var
  renderOrderAux: array of Word;

  procedure DoQuickSort(aLo, aHi: Integer);
  var
    lo, hi: Integer;
    mid: Single;
  begin
    lo := aLo;
    hi := aHi;
    mid := fRenderList[fRenderOrder[(lo + hi) div 2]].Feet.Y;
    repeat
      while fRenderList[fRenderOrder[lo]].Feet.Y < mid do Inc(lo);
      while fRenderList[fRenderOrder[hi]].Feet.Y > mid do Dec(hi);
      if lo <= hi then
      begin
        SwapInt(fRenderOrder[lo], fRenderOrder[hi]);
        Inc(lo);
        Dec(hi);
      end;
    until lo > hi;
    if hi > aLo then DoQuickSort(aLo, hi);
    if lo < aHi then DoQuickSort(lo, aHi);
  end;

  procedure Merge(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd - 1 do
      if (A < aMid) and ((B >= aEnd)
      or (fRenderList[renderOrderAux[A]].Feet.Y <= fRenderList[renderOrderAux[B]].Feet.Y)) then
      begin
        fRenderOrder[I] := renderOrderAux[A];
        Inc(A);
      end else begin
        fRenderOrder[I] := renderOrderAux[B];
        Inc(B);
      end;
  end;

  // The same as Merge, but RenderOrder and RenderOrderAux are switched
  procedure MergeAux(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd-1 do
      if (A < aMid) and ((B >= aEnd)
      or (fRenderList[fRenderOrder[A]].Feet.Y <= fRenderList[fRenderOrder[B]].Feet.Y)) then
      begin
        renderOrderAux[I] := fRenderOrder[A];
        Inc(A);
      end else begin
        renderOrderAux[I] := fRenderOrder[B];
        Inc(B);
      end;
  end;

  // aUseAux tells us which array to store results in, it should flip each recurse
  procedure DoMergeSort(aStart, aEnd: Integer; aUseAux: Boolean);
  var
    mid: Integer;
  begin
    if aEnd - aStart < 2 then Exit;
    mid := (aStart + aEnd) div 2;
    DoMergeSort(aStart, mid, not aUseAux);
    DoMergeSort(mid, aEnd, not aUseAux);
    if aUseAux then
      MergeAux(aStart, mid, aEnd)
    else
      Merge(aStart, mid, aEnd);
  end;

begin
  ClipRenderList;
  if fCount > 0 then
  begin
    SetLength(renderOrderAux, Length(fRenderOrder));
    Move(fRenderOrder[0], renderOrderAux[0], Length(fRenderOrder)*SizeOf(fRenderOrder[0]));
    // Quicksort is unstable which causes Z fighting, so we use mergesort
    DoMergeSort(0, Length(fRenderOrder), False);
    // DoQuickSort(0, Length(RenderOrder) - 1);
  end;
end;


// New items must provide their ground level
procedure TKMRenderList.AddSpriteG(aRX: TRXType; aID: Integer; aUID: Integer; pX: Single; pY: Single; gX: Single; gY: Single; nX: Integer; nY: Integer; aTeam: Cardinal = 0; aAlphaStep: Single = -1; aHighlight: Cardinal = $0);
const
  MAX_SEL_RECT_HEIGHT = 60; //Restrict too long images selection rect
var
  hAdd, imH, hTop: Single;
  snsTop, snsBottom: Integer;
begin
  if fCount >= Length(fRenderList) then
    SetLength(fRenderList, fCount + 256); // Book some space

  fRenderList[fCount].Loc        := KMPointF(pX, pY); // Position of sprite, floating-point
  fRenderList[fCount].Feet       := KMPointF(gX, gY); // Ground position of sprite for Z-sorting
  fRenderList[fCount].RX         := aRX;             // RX library
  fRenderList[fCount].Id         := aId;             // Texture Id
  fRenderList[fCount].UID        := aUID;            // Object Id
  fRenderList[fCount].NewInst    := True;            // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  fRenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  fRenderList[fCount].HighlightColor  :=aHighlight;           // Team Id (determines color)
  fRenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings
  fRenderList[fCount].NightLoc  := KMPoint(nX, nY);      // Alpha step for wip buildings

  if aUID > 0 then
    with fRenderList[fCount].SelectionRect do
    begin
      snsTop    := fUnitsRXData.SizeNoShadow[aId].Top;
      snsBottom := fUnitsRXData.SizeNoShadow[aId].Bottom;

      imH := snsBottom - snsTop + 1;
      hTop := EnsureRange(imH, CELL_SIZE_PX, MAX_SEL_RECT_HEIGHT);

      //Enlarge rect from image size to the top, to be at least CELL_SIZE_PX height
      hAdd := Max(0, CELL_SIZE_PX - imH); // height to add to image pos. half to the top, half to the bottom

      Left := pX - 0.5 - fUnitsRXData.Pivot[aId].X / CELL_SIZE_PX;
      Right := Left + 1; // Exactly +1 tile
      Bottom := gY + ((hAdd / 2) - (fUnitsRXData.Size[aId].Y - snsBottom - 1))/ CELL_SIZE_PX; // Consider shadow at the image bottom
      Top := Bottom - hTop / CELL_SIZE_PX; // -1 ~ -1.5 tiles
    end;

  Inc(fCount); // New item added
end;


// Child items don't need ground level
procedure TKMRenderList.AddSprite(aRX: TRXType; aID: Integer; pX: Single; pY: Single; nX: Integer; nY: Integer; aTeam: Cardinal = 0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(fRenderList) then
    SetLength(fRenderList, fCount + 256); // Book some space

  fRenderList[fCount].Loc        := KMPointF(pX,pY); // Position of sprite, floating-point
  fRenderList[fCount].Feet       := fRenderList[fCount-1].Feet;  // Ground position of sprite for Z-sorting
  fRenderList[fCount].RX         := aRX;             // RX library
  fRenderList[fCount].Id         := aId;             // Texture Id
  fRenderList[fCount].UID        := 0;               // Child sprites aren't used for selecting units
  fRenderList[fCount].NewInst    := False;           // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  fRenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  fRenderList[fCount].HighlightColor  := 0;           // Team Id (determines color)
  fRenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings
  fRenderList[fCount].NightLoc  := KMPoint(nX, nY);      // Alpha step for wip buildings

  Inc(fCount); // New item added
end;

procedure TKMRenderList.AddSpriteFront(aRX: TRXType; aID: Integer; pX: Single; pY: Single; nX: Integer; nY: Integer; aTeam: Cardinal = 0; aAlphaStep: Single = -1);
var J : Integer;
begin
  J := length(fFrontRenderList);
  SetLength(fFrontRenderList, J + 1); // Book some space

  fFrontRenderList[J].Loc        := KMPointF(pX,pY); // Position of sprite, floating-point
  fFrontRenderList[J].Feet       := KMPointF(pX,pY);  // it doesnt' matter here
  fFrontRenderList[J].RX         := aRX;             // RX library
  fFrontRenderList[J].Id         := aId;             // Texture Id
  fFrontRenderList[J].UID        := 0;
  fFrontRenderList[J].NewInst    := False;           // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  fFrontRenderList[J].TeamColor  := aTeam;           // Team Id (determines color)
  fFrontRenderList[J].HighlightColor  := 0;           // Team Id (determines color)
  fFrontRenderList[J].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings
  fFrontRenderList[J].NightLoc  := KMPoint(nX, nY);      // Alpha step for wip buildings

end;


procedure TKMRenderList.SendToRender(aId: Integer; aFrontList : Boolean = false);
var
  sp1, sp2: TKMRenderSprite;
  sp2Exists: Boolean;
begin
  If aFrontList then
  begin
    sp1 := fFrontRenderList[aId];
    gRenderPool.RenderSpriteFront(sp1.RX, sp1.Id, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc), sp1.TeamColor, sp1.HighlightColor <> 0, sp1.HighlightColor, false, sp1.AlphaStep);
    Exit;
  end;
  // Shortcuts to Sprites info
  sp1 := fRenderList[aId];

  sp2Exists := (aId + 1 < fCount);
  if sp2Exists then
    sp2 := fRenderList[aId + 1];

  if (sp1.AlphaStep = -1) or (sp1.RX = rxUnits) then
    gRenderPool.RenderSpritePool(sp1.RX, sp1.Id, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc), sp1.TeamColor, sp1.HighlightColor <> 0, sp1.HighlightColor, false, sp1.AlphaStep)
  else
  begin
    // Houses are rendered as Wood+Stone part. For Stone we want to skip
    // Wooden part where it is occluded (so that smooth shadows dont overlay)

    // Check if next comes our child, Stone layer
    if sp2Exists and not sp2.NewInst and (sp2.AlphaStep >= 0) then
    begin
      gRenderPool.RenderSpriteAlphaTest(false, sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc),
                                                sp2.Id, sp2.AlphaStep, sp2.Loc.X, sp2.Loc.Y);
      gRenderPool.RenderSpriteAlphaTest(true, sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc),
                                                sp2.Id, sp2.AlphaStep, sp2.Loc.X, sp2.Loc.Y);
    end
    else
    begin
      gRenderPool.RenderSpriteAlphaTest(false, sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc));
      gRenderPool.RenderSpriteAlphaTest(true, sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y, gTerrain.GetNightAtTile(sp1.NightLoc));
    end;
  end;

  if SHOW_GROUND_LINES and sp1.NewInst then
  begin
    // Child ground lines are useless
    glBegin(GL_LINES);
      glColor3f(1,1,0.5);
      glVertex2f(sp1.Feet.X + 0.15, gTerrain.RenderFlatToHeight(sp1.Feet).Y);
      glVertex2f(sp1.Feet.X - 0.15, gTerrain.RenderFlatToHeight(sp1.Feet).Y);
    glEnd;
  end;
end;


// Now render all these items from list
procedure TKMRenderList.Render;
var
  I, K, objectsCount: Integer;
begin
  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psFrameRenderList);
  {$ENDIF}

  fDbgSpritesQueued := fCount;
  fDbgSpritesDrawn := 0;
  objectsCount := Length(fRenderOrder);

  for I := 0 to objectsCount - 1 do
  begin
    K := fRenderOrder[I];
    glPushMatrix;

      if RENDER_3D then
      begin
        glTranslatef(fRenderList[K].Loc.X, fRenderList[K].Loc.Y, 0);
        glRotatef(gRenderPool.rHeading, -1, 0, 0);
        glTranslatef(-fRenderList[K].Loc.X, -fRenderList[K].Loc.Y, 0);
      end;

      repeat // Render child sprites after their parent
        SendToRender(K);
        if SHOW_SEL_BUFFER and fRenderList[K].NewInst and (fRenderList[K].UID > 0) then
          gRenderAux.SquareOnTerrain(fRenderList[K].SelectionRect.Left , fRenderList[K].SelectionRect.Top,
                                     fRenderList[K].SelectionRect.Right, fRenderList[K].SelectionRect.Bottom, fRenderList[K].UID or $FF000000, 1);
        Inc(K);
        Inc(fDbgSpritesDrawn);
      until ((K = fCount) or fRenderList[K].NewInst);
    glPopMatrix;
  end;
  //front render list is mainly for animations and particles
  for I := 0 to High(fFrontRenderList) do
  begin

    glPushMatrix;

      if RENDER_3D then
      begin
        glTranslatef(fFrontRenderList[I].Loc.X, fFrontRenderList[I].Loc.Y, 0);
        glRotatef(gRenderPool.rHeading, -1, 0, 0);
        glTranslatef(-fFrontRenderList[I].Loc.X, -fFrontRenderList[I].Loc.Y, 0);
      end;

    SendToRender(I, true);//no childs here

    glPopMatrix;


  end;

  {$IFDEF PERFLOG}
  gPerfLogs.SectionLeave(psFrameRenderList);
  {$ENDIF}
end;


end.
