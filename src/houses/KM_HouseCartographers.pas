unit KM_HouseCartographers;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes;

type
  TKMCartographesMode = (cmChartman, cmSpy);
  TKMCartographersPaintLayer = (cplUnderground, cplMiningRadius, cplTowerRange, cplSpawners, cplObjects);

  TKMHouseCartographers = class(TKMHouseWFlagPoint)
  private
    //fMinimap : //not used right now
    fMode : TKMCartographesMode;
    fLayer: array[TKMCartographersPaintLayer] of Boolean;
    fHouseList : TKMHouseArray;//this value is not saved !!!
    procedure RefreshHousesList;
  protected
    procedure SetFlagPoint(aFlagPoint: TKMPoint); override;
    function GetMaxDistanceToPoint: Integer; override;
    procedure Activate(aWasBuilt: Boolean); virtual;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    procedure PostLoadMission; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;

    function CanWork : Boolean;
    procedure ToggleLayer(aLayer : TKMCartographersPaintLayer); overload;
    procedure ToggleLayer(aLayer : Byte); overload;

  end;

implementation
uses
  KM_Game,
  KM_CommonUtils, KromUtils,
  KM_HandsCollection,
  KM_HouseHelpers,
  KM_HouseCollection,
  KM_RenderAux, KM_RenderDebug, KM_RenderPool,
  KM_Terrain;

constructor TKMHouseCartographers.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var CP : TKMCartographersPaintLayer;
begin
  Inherited;
  fMode := cmChartman;
  for CP := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
    fLayer[CP] := false;
  fLayer[cplSpawners] := true;
  //FillChar(fLayer, SizeOf(fLayer), #0);
end;

constructor TKMHouseCartographers.Load(LoadStream: TKMemoryStream);
begin
  Inherited;

end;

procedure TKMHouseCartographers.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
end;

procedure TKMHouseCartographers.Activate(aWasBuilt: Boolean);
begin
  Inherited;
end;

procedure TKMHouseCartographers.SetFlagPoint(aFlagPoint: TKMPoint);
begin
  Inherited;
end;

function TKMHouseCartographers.GetMaxDistanceToPoint: Integer;
begin
  Result := 80;
end;

procedure TKMHouseCartographers.SyncLoad;
begin
  Inherited;
  If IsComplete then
    If not gGame.Params.IsMapEditor then
      RefreshHousesList;
end;

procedure TKMHouseCartographers.PostLoadMission;
begin
  Inherited;
  If IsComplete then
    If not gGame.Params.IsMapEditor then
      RefreshHousesList;
end;

procedure TKMHouseCartographers.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If not IsComplete or not HasWorkerInside then
    Exit;
  If aTick mod 3000 = 0 then
    RefreshHousesList;
end;

procedure TKMHouseCartographers.Paint;
  procedure PaintUndergroundDeposits;
  var list : TKMPointTagList;
    I : Integer;
    C, factor : Cardinal;
  begin
    list := TKMPointTagList.Create;
    gTerrain.FindDeposits(Entrance, 30, list, true);

    for I := 0 to list.Count - 1 do
    begin
      case list.Tag[I] of
        1: C := icYellow;
        2: C := icSteelBlue;
        3: C := icLightRed;
        4: C := icBlack;
        else
          C := $FFFFFFFF;
      end;
      factor := list.Tag2[I] * 15 + 40;
      factor := factor shl 24;
      factor := factor or $00FFFFFF;
      C := C and factor;
      gRenderAux.Quad(list[I].X, list[I].Y, C);
    end;
    list.Free;
  end;
  procedure PaintMiningRange(H : TKMHouse);
  begin
    If H.HouseType in [htQuarry, htIronMine, htBitinMine, htGoldMine, htCoalMine,
                        htFishermans, htFarm, htVineyard, htWoodcutters, htPottery] then
      gRenderPool.RenderDebug.PaintMiningRadius(H);
  end;

  procedure PaintTowerRange(H : TKMHouse);
  begin
    If H.HouseType in [htWatchTower, htWallTower] then
      gRenderPool.RenderDebug.RenderTiledArea(TKMHouseTower(H).Entrance, TKMHouseTower(H).RangeMin, TKMHouseTower(H).RangeMax,
                                              GetLength, $40FFFFFF, icWhite);
  end;
var I : Integer;
begin
  Inherited;
  If not IsComplete or not HasWorkerInside then
    Exit;

  if gMySpectator.Selected <> self then
    Exit;

  If fMode = cmSpy then
    Exit;
  If fLayer[cplUnderground] then
    PaintUndergroundDeposits;
  If fLayer[cplMiningRadius] then
    for I := 0 to High(fHouseList) do
      If fHouseList[I].IsValid then
        PaintMiningRange(fHouseList[I]);

  If fLayer[cplTowerRange] then
    for I := 0 to High(fHouseList) do
      If fHouseList[I].IsValid then
        PaintTowerRange(fHouseList[I]);
  If fLayer[cplSpawners] then
    gHands.PlayerAnimals.PaintSpawners(KMRectGrow(KMRect(Entrance), 60));

end;

procedure TKMHouseCartographers.RefreshHousesList;
begin
  If fMode = cmSpy then
    Exit;
  fHouseList := gHands.GetHousesInRadius(Entrance, 30 * 30);
end;

function TKMHouseCartographers.CanWork: Boolean;
begin
  Result := true;
end;

procedure TKMHouseCartographers.ToggleLayer(aLayer : TKMCartographersPaintLayer);
begin
  fLayer[aLayer] := not fLayer[aLayer];
end;

procedure TKMHouseCartographers.ToggleLayer(aLayer : Byte);
begin
  ToggleLayer(TKMCartographersPaintLayer(aLayer) );
end;

end.
