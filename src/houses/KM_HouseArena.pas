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
    fArenaWaitTillNext : Byte;
    fArenaAnimStep : Cardinal;
    fAnimColors : TKMCardinalArray;
    procedure UpdatePointBelowEntrance;
  protected
    procedure UpdateEntrancePos; override;
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    function GetLVLExp(aLevel : Word) : Cardinal;
    function LVLExp : Cardinal;
    function NextLVLExp : Cardinal;
    function GetLVLProgress : Single;
    function EXP : Cardinal;
    function LVL : Word;


    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
    constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;

implementation
uses
  Classes,
  KM_Game,
  KM_HandsCollection, KM_HandLogistics, KM_HandTypes, KM_Hand, KM_Entity, KM_HandEntity,
  KM_TerrainTypes,
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
  fArenaAnimStep := 0;
  fArenaWaitTillNext := 120;
end;

Constructor TKMHouseArena.Load(LoadStream : TKMemoryStream);
var I, N : Integer;
begin
  Inherited;
  LoadStream.ReadData(fArenaWaitTillNext);
  LoadStream.ReadData(fArenaAnimStep);

  LoadStream.ReadData(N);
  SetLength(fAnimColors, N);
  for I := 0 to N - 1 do
    LoadStream.ReadData(fAnimColors[I]);

end;

procedure TKMHouseArena.Save(SaveStream : TKMemoryStream);
var I, N : Integer;
begin
  Inherited;
  SaveStream.WriteData(fArenaWaitTillNext);
  SaveStream.WriteData(fArenaAnimStep);

  N := length(fAnimColors);
  SaveStream.WriteData(N);
  for I := 0 to N - 1 do
    SaveStream.WriteData(fAnimColors[I]);
end;

procedure TKMHouseArena.UpdateEntrancePos;
begin
  Inherited;
  UpdatePointBelowEntrance;
end;

procedure TKMHouseArena.UpdatePointBelowEntrance;
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

function TKMHouseArena.GetLVLExp(aLevel : Word) : Cardinal;
begin
  Result := gHands[Owner].GetLVLExp(aLevel);
end;

function TKMHouseArena.LVLExp: Cardinal;
begin
  Result := GetLVLExp(LVL);
end;
function TKMHouseArena.NextLVLExp: Cardinal;
begin
  Result := GetLVLExp(LVL + 1);
end;

function TKMHouseArena.GetLVLProgress: Single;
var maxEXP, minEXP : Cardinal;
begin
  minEXP := LVLExp;
  maxEXP := GetLVLExp(LVL + 1) - minEXP;

  Result := (EXP - minEXP) / maxEXP;
end;

function TKMHouseArena.EXP : Cardinal;
begin
  Result := gHands[Owner].Exp;
end;

function TKMHouseArena.LVL : Word;
begin
  Result := gHands[Owner].Level;
end;



procedure TKMHouseArena.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If not IsComplete then
    If aTick mod 10 = 0 then
      UpdatePointBelowEntrance;
end;


procedure TKMHouseArena.Paint;
begin
  Inherited;
  If not IsComplete then
    Exit;
end;

end.
