unit KM_ResTilesetTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes;
var
  TILES_CNT : Word;//= 664;
  MAX_TILE_TO_SHOW : Word;
const
  MAX_STATIC_TERRAIN_ID = 9997;

type
  //TKMTileProperty = set of (tpWalkable, tpRoadable);

  TKMTileMaskType = (tmtNone,
    tmt2Straight, // A A
                  // B B

    tmt2Diagonal, // A A
                  // B A

    tmt2Corner,   // A B
                  // B B

    tmt2Opposite, // A B
                  // B A

    tmt3Straight, // A A
                  // C D

    tmt3Opposite, // A B
                  // D A

    tmt4Square);  // A B
                  // D C

  TKMTileMaskSubType = (mstMain, mstExtra);

  //* Terrain mask kind
  TKMTileMaskKind = (mkNone, mkSoft1, mkSoft2, mkSoft3, mkStraight, mkGradient);

  // Mask usage: as a pixel mask, or as a gradient mask
  TKMTileMaskKindUse = (mkuPixel, mkuAlpha);

  TKMMaskFullType = record
    Kind: TKMTileMaskKind;
    MType: TKMTileMaskType;
    SubType: TKMTileMaskSubType;
  end;

  PKMMaskFullType = ^TKMMaskFullType;

  //* Terrain kind
  TKMTerrainKind = (
//    tkNone,
    tkCustom,     //0
    tkGrass,      //1
    tkMoss,       //2
    tkPaleGrass,  //3
    tkCoastSand,  //4
    tkGrassSand1, //5
    tkGrassSand2, //6
    tkGrassSand3, //7
    tkSand,       //8
    tkGrassDirt,  //9
    tkDirt,       //10
    tkBarrenLand, //11
    tkCobbleStone,//12
    tkGrassyWater,//13
    tkSwamp,      //14
    tkIce,        //15
    tkSnowOnGrass,//16
    tkSnowOnDirt, //17
    tkSnow,       //18
    tkDeepSnow,   //19
    tkStone,      //20
    tkGoldMount,  //21
    tkIronMount,  //22
    tkAbyss,      //23
    tkGravel,     //24
    tkCoal,       //25
    tkGold,       //26
    tkIron,       //27
    tkWater,      //28
    tkFastWater,  //29
    tkLava,
    tkShallowWater1,
    tkShallowWater2,
    tkDeepWater);      //30


  TKMTerrainKindsArray = array of TKMTerrainKind;

  TKMTerrainKindSet = set of TKMTerrainKind;

  TKMTerrainKindCorners = array[0..3] of TKMTerrainKind;
const
  NO_BRIDGE_TER_KINDS : TKMTerrainKindSet =  [tkLava, tkIron, tkGold, tkCoal, tkAbyss, tkIronMount, tkGoldMount, tkStone,
                        tkIce, tkCobbleStone, tkCustom];
  GRASS_TERR_KINDS = [tkGrass, tkMoss, tkPaleGrass, tkGrassSand1, tkGrassSand2,
                      tkGrassDirt, tkDirt];

  CORN_TERR_KINDS = [tkGrass, tkMoss, tkPaleGrass, tkGrassSand1, tkGrassSand2, tkGrassSand3,
                      tkGrassDirt, tkDirt, tkGravel, tkSnowOnGrass, tkSnowOnDirt];

  WINE_TERR_KINDS = [tkGrass, tkMoss, tkPaleGrass, tkGrassSand1, tkGrassSand2,tkGrassSand3, tkCoastSand,
                      tkGrassDirt, tkDirt, tkGravel, tkSnowOnGrass, tkSnowOnDirt, tkSnow, tkCoal,
                      tkDeepSnow, tkSand];
type
  TKMTileAnimLayer = record
    Frames: Byte;
    Anims: array of Word;
    function AnimsCnt: Byte;
    function GetAnim(aAnimStep: Integer): Word;
    function HasAnim: Boolean;
  end;

  TKMTileAnim = record
    Layers: array of TKMTileAnimLayer;
    function LayersCnt: Byte;
    function HasAnim: Boolean;
  end;

const
  TER_KIND_ORDER: array[tkCustom..tkLava] of Integer =
    (0,1,2,3,4,5,6,7,8,9,10,11, 12,
      -1,    // To make Water/FastWater-GrassyWater transition possible with layers we need GrassyWater to be above Water because of animation (water above grassy anim looks ugly)
      14,
      -2,
      16,17,18,19,20,21,22,23,24,25,26, 27,
      -4,-3, // Put GrassyWater/Water/FastWater always to the base layer, because of animation
      30);

  BASE_TERRAIN: array[TKMTerrainKind] of Word = //tkCustom..tkLava] of Word =
    (0, 0, 8, 17, 32, 26, 27, 28, 29, 34, 35, 698, 215, 48, 40, 44, 315, 47, 46, 45, 132, 159, 164, 245, 20, 155, 147, 151, 192, 209, 7,
    816, 817, 818);

//  TILE_MASKS: array[mt_2Straight..mt_4Square] of Word =
//      (279, 278, 280, 281, 282, 277);

  TILE_MASKS_LAYERS_CNT: array[TKMTileMaskType] of Byte =
    (1, 2, 2, 2, 2, 3, 3, 4);

  TILE_MASK_KINDS_PREVIEW: array[TKMTileMaskKind] of Integer =
    (-1, 4951, 4961, 4971, 4981, 4991); //+1 here, so -1 is no image, and not grass

  TILE_MASK_KIND_USAGE: array [TKMTileMaskKind] of TKMTileMaskKindUse =
    (mkuPixel, mkuPixel, mkuPixel, mkuPixel, mkuPixel, mkuAlpha);


  TILE_MASKS_FOR_LAYERS:  array[Succ(Low(TKMTileMaskKind))..High(TKMTileMaskKind)]
                            of array[Succ(Low(TKMTileMaskType))..High(TKMTileMaskType)]
                              of array[TKMTileMaskSubType] of Integer =
     //Softest
    (((4949, -1),
      (4950, -1),
      (4951, -1),
      (4952, -1),
      (4951, 4949),
      (4951, 4952),
      (4951, -1)),
     //Soft
     ((4959, -1),
      (4960, -1),
      (4961, -1),
      (4962, -1),
      (4961, 4959),
      (4961, 4962),
      (4961, -1)),
     //Soft2
     ((4969, -1),
      (4970, -1),
      (4971, -1),
      (4972, -1),
      (4971, 4969),
      (4971, 4972),
      (4971, -1)),
     //Hard
     ((4979, -1),
      (4980, -1),
      (4981, -1),
      (4982, -1),
      (4981, 4979),
      (4981, 4982),
      (4981, -1)),
     //Gradient
     ((4989, -1),
      (4990, -1),
      (4991, -1),
      (4992, -1),
      (4991, 4989),
      (4991, 4992),
      (4991, -1))
      //Hard2
     {((569, -1),
      (570, -1),
      (571, -1),
      (572, -1),
      (573, 574),
      (575, 576),
      (577, -1)),}
      //Hard3
     {((569, -1),
      (570, -1),
      (571, -1),
      (572, -1),
      (571, 569),
      (571, 572),
      (571, -1))}
      );

  // Does masks apply Walkable/Buildable restrictions on tile.
  // F.e. mt_2Corner mask does not add any restrictions
//  TILE_MASKS_PASS_RESTRICTIONS: array[mt_2Straight..mt_4Square] of array[TKMTileMaskSubType]
//                            of array[0..1] of Byte =  // (Walkable, Buildable) (0,1): 0 = False/1 = True
//     (((0,1), (0,0)),  // mt_2Straight
//      ((1,1), (0,0)),  // mt_2Diagonal
//      ((0,0), (0,0)),  // mt_2Corner
//      ((0,1), (0,0)),  // mt_2Opposite
//      ((0,0), (0,1)),  // mt_3Straight
//      ((0,0), (0,1)),  // mt_3Opposite
//      ((0,0), (0,0))); // mt_4Square


  TERRAIN_EQUALITY_PAIRS: array[0..1] of record
      TK1, TK2: TKMTerrainKind;
    end =
      (
//        (TK1: tkGold; TK2: tkGoldMount),
//        (TK1: tkIron; TK2: tkIronMount),
        (TK1: tkWater; TK2: tkFastWater),
        (TK1: tkSnowOnGrass; TK2: tkSnowOnDirt)
      );

type
  TKMTileParams = class
  private
    fID: Integer;
    fWalkable: Boolean;
    fRoadable: Boolean;
    fNotBuildable : Boolean;
    fStone: Byte;
    fCoal: Byte;
    fIron: Byte;
    fBitin: Byte;
    fClay: Byte;
    fGold: Byte;
    fWater: Boolean;
    fHasWater: Boolean;
    fIce: Boolean;
    fSand: Boolean;
    fSnow: Boolean;
    fSoil: Boolean;
    fCorn: Boolean;
    fWine: Boolean;
    fGrass: Boolean;
    fVegetables: Boolean;
    fIronMinable: Boolean;
    fGoldMinable: Boolean;
    fCoalMinable: Boolean;
    fMineShaft: Boolean;
    // Not Saved
    fMainColor: TKMColor3b;
    function GetHasAnim: Boolean;
  public
    Animation: TKMTileAnim;
    TerKinds: TKMTerrainKindCorners; //Corners: LeftTop - RightTop - RightBottom - LeftBottom

    property HasAnim: Boolean read GetHasAnim;
    property MainColor: TKMColor3b read fMainColor write fMainColor;
//  published // for serialization / deserialization. Will automatically add {M+} to use RTTI
    property ID: Integer read fID write fID;
    property Walkable: Boolean read fWalkable write fWalkable;
    property Roadable: Boolean read fRoadable write fRoadable;
    property NotBuildable: Boolean read fNotBuildable write fNotBuildable;
    property Stone: Byte read fStone write fStone;
    property Coal: Byte read fCoal write fCoal;
    property Iron: Byte read fIron write fIron;
    property Bitin: Byte read fBitin write fBitin;
    property Clay: Byte read fClay write fClay;
    property Gold: Byte read fGold write fGold;
    property Water: Boolean read fWater write fWater;
    property HasWater: Boolean read fHasWater write fHasWater;
    property Ice: Boolean read fIce write fIce;
    property Sand: Boolean read fSand write fSand;
    property Snow: Boolean read fSnow write fSnow;
    property Soil: Boolean read fSoil write fSoil;
    property Corn: Boolean read fCorn write fCorn;
    property Wine: Boolean read fWine write fWine;
    property Grass: Boolean read fGrass write fGrass;
    property Vegetables: Boolean read fVegetables write fVegetables;
    property IronMinable: Boolean read fIronMinable write fIronMinable;
    property GoldMinable: Boolean read fGoldMinable write fGoldMinable;
    property CoalMinable: Boolean read fCoalMinable write fCoalMinable;
    property MineShaft: Boolean read fMineShaft write fMineShaft;

//    Corner: Boolean;
//    Edge: Boolean;
  end;

implementation


{ TKMTileAnim }
function TKMTileAnim.LayersCnt: Byte;
begin
  Result := Length(Layers);
end;


function TKMTileAnim.HasAnim: Boolean;
var
  I: Integer;
begin
  if Length(Layers) = 0 then Exit(False);

  Result := False;

  for I := 0 to LayersCnt - 1 do
    Result := Result or Layers[I].HasAnim;
end;


{ TKMTileAnimLayer }
function TKMTileAnimLayer.HasAnim: Boolean;
begin
  Result := Length(Anims) > 0;
end;


function TKMTileAnimLayer.AnimsCnt: Byte;
begin
  Result := Length(Anims);
end;


function TKMTileAnimLayer.GetAnim(aAnimStep: Integer): Word;
begin
  if AnimsCnt = 0 then Exit(0);
  Assert(Frames > 0, 'Frames = 0!');

  Result := Anims[(aAnimStep div Frames) mod AnimsCnt];

  if Result = 0 then Exit;

  // -1 because of difference in 0-based and 1-based in tiles numbering
  // todo: refactor it to 0-based
  Dec(Result);
end;


{ TKMTileParams }
function TKMTileParams.GetHasAnim: Boolean;
begin
  Result := Animation.HasAnim;
end;


end.
