unit KM_HouseForest;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes;

type
  TKMForestTree = record
    Age, ID : Byte;
    Obj : Word;
    Pos : TKMPointF;

  end;

  TKMHouseForest = class(TKMHouse)
  private
    fCount : Byte;
    fTrees : array of TKMForestTree;
    function TreeWillCollide(aTreeID : Byte; aLoc : TKMPointF) : Boolean;
    function GetRandomPosForTree(aTreeID : Byte) : TKMPointF;
  protected
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray; override;

    function AddTree(aTreeID : Byte) : Boolean;
    procedure UpdateState(aTick: Cardinal); Override;
    procedure Paint; override;
  end;


implementation
uses
  Classes,
  KM_CommonUtils,
  KM_RenderPool,
  KM_ResMapElements;

constructor TKMHouseForest.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var I : Integer;
begin
  Inherited;
  while AddTree(KaMRandom(length(gGrowingTrees), '')) do
    Inc(I);
end;

function TKMHouseForest.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHouseForest.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirS),
              KMPointDir(Entrance.X - 2, Entrance.Y - 2, dirW),
              KMPointDir(Entrance.X, Entrance.Y - 4, dirN),
              KMPointDir(Entrance.X + 2, Entrance.Y - 2, dirE)
            ];
end;

function TKMHouseForest.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
const  ENTRANCE_POS : array[1..4] of TKMPoint = ( (X : 0; Y : 0),
                                                (X : -2; Y : -2),
                                                (X : 0; Y : -4),
                                                (X : 2; Y : -2));
const  ENTRANCE_DIR : array[1..4] of TKMDirection = (dirS, dirW, dirN, dirE);

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

function TKMHouseForest.TreeWillCollide(aTreeID : Byte; aLoc : TKMPointF) : Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fCount - 1 do
    If KMLengthDiag(aLoc, fTrees[I].Pos) <= gGrowingTrees[fTrees[I].ID].Size + gGrowingTrees[aTreeID].Size then
      Exit(true);
end;



function TKMHouseForest.GetRandomPosForTree(aTreeID : Byte): TKMPointF;
var I : Byte;
begin
  I := 0;
  repeat
    Result.X := KaMRandomS1(4.75, 'TKMHousePasture.GetPositionForAnimal 1');
    Result.Y := KaMRandomS1(4, 'TKMHousePasture.GetPositionForAnimal 2');
    Inc(I);
  until (I >= 10) or not TreeWillCollide(aTreeID, Result);//max 10 tries

  //no place for new tree
  If I >= 10 then
    Result := KMPOINTF_INVALID_TILE;
end;

function TKMHouseForest.AddTree(aTreeID : Byte) : Boolean;
var newLoc : TKMPointF;
begin
  newLoc := GetRandomPosForTree(aTreeID);
  Result := true;
  If newLoc.X = -1 then//there is no place for new tree
    Exit(false);

  inc(fCount);
  If length(fTrees) < fCount then
    SetLength(fTrees, fCount + 5);

  fTrees[fCount - 1].Pos := newLoc;
  fTrees[fCount - 1].Age := 1;
  fTrees[fCount - 1].Obj := 390{gGrowingTrees[aTreeID].ObjID};  //tree sapling
  fTrees[fCount - 1].ID := aTreeID;


end;

procedure TKMHouseForest.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  Inherited;
  If not IsComplete then
    Exit;
  If aTick mod TERRAIN_PACE <> 0 then
    Exit;

  for I := 0 to fCount - 1 do
  with fTrees[I] do
  begin
    If Age = 255 then
      Continue;
    Inc(Age);
    If Age = 8 then
      Obj := gGrowingTrees[ID].ObjID
    else
    If Age = gMapElements[Obj].TreeGrowAge + 8 then
    begin
      Obj := gMapElements[Obj].NextTreeAgeObj;
      If ObjectIsChoppableTree(Obj, caAgeFull) then
        Age := 255;
    end;
  end;
end;

procedure TKMHouseForest.Paint;
var I : Integer;
  entr : TKMPoint;
begin
  Inherited;
  If not IsComplete then
    Exit;
  entr := Entrance;


  I := ResOut[1];
  If I > 0 then
    gRenderPool.AddForestLogs(Position, I);
  for I := 0 to fCount - 1 do
  begin
    gRenderPool.RenderTree(fTrees[I].Obj, FlagAnimStep + I,
                                  fTrees[I].Pos.X + entr.X - 2, fTrees[I].Pos.Y + entr.Y - 4,
                                  entr);
  end;

end;

end.
