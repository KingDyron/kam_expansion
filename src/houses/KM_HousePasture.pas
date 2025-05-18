unit KM_HousePasture;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes;

type

  TKMHousePasture = class(TKMHouse)
  private
  protected
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;
  end;

implementation
uses
  Classes;

function TKMHousePasture.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHousePasture.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirS),
              KMPointDir(Entrance.X - 1, Entrance.Y - 3, dirN)
            ];
end;

function TKMHousePasture.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
const  ENTRANCE_POS : array[1..2] of TKMPoint = ( (X : 0; Y : 0),
                                                (X : -1; Y : -3));
const  ENTRANCE_DIR : array[1..2] of TKMDirection = (dirS, dirN);

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
