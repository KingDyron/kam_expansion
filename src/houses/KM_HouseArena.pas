unit KM_HouseArena;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes, KM_ResDevelopment;

type
  TKMHouseArena = class(TKMHouse)
  private
    fDevType : TKMDevelopmentTreeType;
    fArenaAnimStep : Cardinal;
  protected
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;

    Constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;

implementation
uses
  Classes,
  KM_HandsCollection,
  KM_RenderPool, KM_RenderAux,
  KM_Resource,
  KM_Terrain,
  KM_CommonUtils;

function TKMHouseArena.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHouseArena.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirE),
              KMPointDir(Entrance.X - 4, Entrance.Y, dirW)
            ];
end;

function TKMHouseArena.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
const  ENTRANCE_POS : array[1..2] of TKMPoint = ( (X : 0; Y : 0),
                                                (X : -4; Y : 0));
const  ENTRANCE_DIR : array[1..2] of TKMDirection = (dirE, dirW);

var I : Integer;
  lastDist, tmp : Single;
begin
  Result := Inherited;

  lastDist := 99999;
  for I := low(ENTRANCE_POS) to High(ENTRANCE_POS) do
  begin
    tmp := KMLength(aLoc, Entrance + ENTRANCE_POS[I]);
    If tmp < lastDist then
    begin
      lastDist := tmp;
      Result.Loc := Entrance + ENTRANCE_POS[I];
      Result.Dir := ENTRANCE_DIR[I];
    end;
  end;
end;


constructor TKMHouseArena.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fArenaAnimStep := 1;
  fDevType := dttEconomy;
end;

Constructor TKMHouseArena.Load(LoadStream : TKMemoryStream);
begin
  Inherited;
end;

procedure TKMHouseArena.Save(SaveStream : TKMemoryStream);
begin
  Inherited;
end;

procedure TKMHouseArena.UpdateState(aTick: Cardinal);
  procedure UpdatePointBelow;
  var entrs: TKMPointDirArray;
      I : Integer;
      P : TKMPoint;
  begin
    entrs := Entrances;
    for I := 0 to High(entrs) do
    begin
      P := entrs[I].DirFaceLoc;
      If gTerrain.CheckPassability(P, tpWalk) then
      begin
        PointBelowEntrance := P;
        Exit;
      end;
    end;
  end;

begin
  Inherited;
  If not IsComplete then
    If aTick mod 10 = 0 then
      UpdatePointBelow
    else
  else
  begin
    If aTick mod 600 = 0 then
      UpdatePointBelow;
    If fArenaAnimStep > 0 then
      Inc(fArenaAnimStep);
  end;
end;


procedure TKMHouseArena.Paint;
begin
  Inherited;
  If not IsComplete then
    Exit;
  If fArenaAnimStep > 0 then
    gRenderPool.AddHouseArena(fDevType, Position, fArenaAnimStep);
end;

end.
