unit KM_RenderTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonGameTypes, KM_Points;

type
  TKMTexFormat = (
    tfRGB5A1,
    tfRGBA8,
    tfAlpha8 //Mask used for team colors and house construction steps (GL_ALPHA)
    );

  TKMFilterType = (
    ftNearest,
    ftLinear
  );

  TKMRenderPoolAddProjectileEvent = procedure (aProj: TKMProjectileType; const aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single) of object;

const
  BEVEL_EDGE_ALPHA_DEF = 1;
  BEVEL_BACK_ALPHA_DEF = 0.5;

implementation


end.
