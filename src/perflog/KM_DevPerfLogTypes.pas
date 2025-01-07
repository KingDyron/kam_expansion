unit KM_DevPerfLogTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes;

type
  TKMPerfLogKind = (plkNone, plkCPU, plkGFX);

  TPerfSectionDev = (
    psNone,
    psGameTick,
      psGameSave,
      psGameSaveWait,
      psGameSavePoint,
      psHands,
        psUnits,
        psDelivery,
        psWalkConnect,
      psGameFOW,
      psPathfinding,
      psHungarian,
      psAIFields,
      psAI,
        psAICityAdv,
        psAIArmyAdv,
        psAICityCls,
        psAIArmyCls,
      psTerrain,
      psTerrainFinder,
      psScripting,
      psMinimap,
      psFrameUpdateVBO,
      psRenderDebug,
        psMarchingSquares,
    psFrameFullC,                 // Full render frame as seen by gMain
    psMemUsed,
    psStackUsed,
    psFrameFullG,                 // Full render frame on GPU (doublecheck TKMPerfLogGFXStack)
        psFrameTerrain,
          psFrameTerrainBase,
            psFrameTiles,
            psFrameWater,
            psFrameTilesLayers,
            psFrameOverlays,
            psFrameLighting,
            psFrameShadows,
        psFrameRenderList,
        psFrameFOW,
      psFrameGui                  // Frame of the Gameplay GUI
  );
  TPerfSectionSet = set of TPerfSectionDev;

const
  FIRST_GFX_SECTION = psFrameFullG;
  // Tabs are for GUI structure
  // Can not use typed constants within another constants declaration :(
  // http://stackoverflow.com/questions/28699518/
  // Avoid Green, it blends with mostly green terrain
  SECTION_INFO: array [TPerfSectionDev] of record
    Name: string;
//    ClassName: TKMPerfLogClass;
    Kind: TKMPerfLogKind;
    Timer: Boolean;
    Color: TKMColor3f;
  end = (
    (Name: 'All';                     Kind: plkNone;Timer: False;Color: (R:0;    G:0;    B:0);),
    (Name: 'GameTick';                Kind: plkCPU; Timer: True; Color: (R:1.0;  G:1;    B:0);),
    (Name: '   GameSave';             Kind: plkCPU; Timer: True; Color: (R:0.3;  G:1;    B:0.7);),
    (Name: '   GameSaveWait';         Kind: plkCPU; Timer: True; Color: (R:0.6;  G:0.9;  B:0.6);),
    (Name: '   GameSavePoint';        Kind: plkCPU; Timer: True; Color: (R:0;    G:0.3;  B:0.7);),
    (Name: '   Hands';                Kind: plkCPU; Timer: True; Color: (R:1;    G:0.25; B:0);),
    (Name: '     Units';              Kind: plkCPU; Timer: True; Color: (R:0;    G:0.75; B:0.75);),
    (Name: '     Deliveries';         Kind: plkCPU; Timer: True; Color: (R:0.75; G:0.25; B:0.25);),
    (Name: '     WalkConnect';        Kind: plkCPU; Timer: True; Color: (R:1;    G:0.75; B:0.75);),
    (Name: '   FOW';                  Kind: plkCPU; Timer: True; Color: (R:0;    G:0.5;  B:0);),
    (Name: '   Pathfinding';          Kind: plkCPU; Timer: True; Color: (R:0.0;  G:1;    B:1);),
    (Name: '   HungarianReorder';     Kind: plkCPU; Timer: True; Color: (R:1.0;  G:0;    B:1);),
    (Name: '   AIFields';             Kind: plkCPU; Timer: True; Color: (R:0;    G:0.5;  B:1);),
    (Name: '   AI';                   Kind: plkCPU; Timer: True; Color: (R:1;    G:1;    B:1);),
    (Name: '     AI City Advanced';   Kind: plkCPU; Timer: True; Color: (R:0;    G:1;    B:0.25);),
    (Name: '     AI Army Advanced';   Kind: plkCPU; Timer: True; Color: (R:0.25; G:0.75; B:1);),
    (Name: '     AI City Classic';    Kind: plkCPU; Timer: True; Color: (R:0;    G:0.3;  B:0.3);),
    (Name: '     AI Army Classic';    Kind: plkCPU; Timer: True; Color: (R:0.5;  G:0.5;  B:0.25);),
    (Name: '   Terrain';              Kind: plkCPU; Timer: True; Color: (R:0.5;  G:0.5;  B:0.5);),
    (Name: '   TerrainFinder';        Kind: plkCPU; Timer: True; Color: (R:0;    G:0.6;  B:0.6);),
    (Name: '   Scripting';            Kind: plkCPU; Timer: True; Color: (R:1;    G:0.5;  B:0.75);),
    (Name: '   Minimap';              Kind: plkCPU; Timer: True; Color: (R:0.7;  G:0;    B:0.9);),
    (Name: '   UpdateVBO';            Kind: plkCPU; Timer: True; Color: (R:0.3;  G:0;    B:0.6);),
    (Name: '   RenderDebug';          Kind: plkCPU; Timer: True; Color: (R:0;    G:0.2;  B:0.7);),
    (Name: '     Marching squares';   Kind: plkCPU; Timer: True; Color: (R:0.9;  G:0.2;  B:0.8);),
    (Name: 'Render.CPU';              Kind: plkCPU; Timer: True; Color: (R:1.0;  G:0;    B:0.5);),
    (Name: 'MemUsed';                 Kind: plkCPU; Timer: False;Color: (R:1.0;  G:0;    B:0);),
    (Name: 'StackUsed';               Kind: plkCPU; Timer: False;Color: (R:0;    G:0;    B:1);),
    (Name: 'Render.GFX';              Kind: plkGFX; Timer: True; Color: (R:0;    G:1;    B:1);),
    (Name: '     Terrain';            Kind: plkGFX; Timer: True; Color: (R:0;    G:0.25; B:0.25);),
    (Name: '       TerBase';          Kind: plkGFX; Timer: True; Color: (R:0;    G:0.5;  B:0.25);),
    (Name: '         Tiles';          Kind: plkGFX; Timer: True; Color: (R:0.25; G:0;    B:0.25);),
    (Name: '         Water';          Kind: plkGFX; Timer: True; Color: (R:0.25; G:0;    B:0.5);),
    (Name: '         Layers';         Kind: plkGFX; Timer: True; Color: (R:0.5;  G:0;    B:0.25);),
    (Name: '         Overlays';       Kind: plkGFX; Timer: True; Color: (R:0.5;  G:0.5;  B:0.25);),
    (Name: '         Light';          Kind: plkGFX; Timer: True; Color: (R:0.5;  G:0.25; B:0);),
    (Name: '         Shadows';        Kind: plkGFX; Timer: True; Color: (R:0.75; G:0.5;  B:0);),
    (Name: '     RenderList';         Kind: plkGFX; Timer: True; Color: (R:0.5;  G:1;    B:0.75);),
    (Name: '     FOWRender';          Kind: plkGFX; Timer: True; Color: (R:0.75; G:1;    B:0.75);),
    (Name: '   GUI';                  Kind: plkGFX; Timer: True; Color: (R:1.0;  G:0.25; B:0);)
  );

  function GetSectionName(aSection: TPerfSectionDev): string;
  function IsCPUSection(aSection: TPerfSectionDev): Boolean;
  function IsGFXSection(aSection: TPerfSectionDev): Boolean;
  function IsTimerSection(aSection: TPerfSectionDev): Boolean;

const
  LOW_PERF_SECTION = TPerfSectionDev(1);

implementation
uses
  TypInfo;


function GetSectionName(aSection: TPerfSectionDev): string;
begin
  Result := GetEnumName(TypeInfo(TPerfSectionDev), Integer(aSection));
  Result := Copy(Result, 3, Length(Result) - 2);
end;


function IsCPUSection(aSection: TPerfSectionDev): Boolean;
begin
  Result := SECTION_INFO[aSection].Kind = plkCPU;
end;


function IsGFXSection(aSection: TPerfSectionDev): Boolean;
begin
  Result := SECTION_INFO[aSection].Kind = plkGFX;
end;


function IsTimerSection(aSection: TPerfSectionDev): Boolean;
begin
  Result := SECTION_INFO[aSection].Timer;
end;


end.
