unit KM_HouseForest;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes;

type

  TKMHouseForest = class(TKMHouse)
  private
  protected
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray; override;
  end;


implementation
uses
  Classes;

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
  //return random for testing;
  {I := KaMRandom(4, 'TKMHousePearl.GetClosestEntrance') + 1;
  Result.Loc := Entrance + ENTRANCE_POS[I];
  Result.Dir := ENTRANCE_DIR[I];}

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
end.
