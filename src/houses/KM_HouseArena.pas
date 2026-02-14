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
    fCurrentDevType,
    fDevType : TKMDevelopmentTreeType;
    fArenaWaitTillNext : Byte;
    fArenaAnimStep : Cardinal;
    fAnimColors : TKMCardinalArray;
    procedure UpdatePointBelowEntrance;
    function FestivalDuration : Word;
    procedure SetDevType(aValue : TKMDevelopmentTreeType);
  protected
    procedure UpdateEntrancePos; override;
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    property FestivalType : TKMDevelopmentTreeType read fDevType write SetDevType;
    procedure StartFestival;
    function BuildingCost : Byte;
    function WarfareCost : Byte;
    function ValuableCost : Byte;
    function FestivalStarted : Boolean;
    function CanStartFestival : Boolean;
    function PointsCount : Byte; overload;
    function PointsCount(aType : TKMDevelopmentTreeType) : Byte; overload;

    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
    Constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;

const
  FESTIVAL_DURATION = 2000;
  FESTIVAL_DURATION_ALL = FESTIVAL_DURATION + FESTIVAL_DURATION div 2;

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
  fDevType := dttNone;
end;

Constructor TKMHouseArena.Load(LoadStream : TKMemoryStream);
var I, N : Integer;
begin
  Inherited;
  LoadStream.ReadData(fArenaWaitTillNext);
  LoadStream.ReadData(fArenaAnimStep);
  LoadStream.ReadData(fCurrentDevType);
  LoadStream.ReadData(fDevType);
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
  SaveStream.WriteData(fCurrentDevType);
  SaveStream.WriteData(fDevType);

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

procedure TKMHouseArena.SetDevType(aValue: TKMDevelopmentTreeType);
begin
  If fDevType = aValue then
    fDevType := dttNone
  else
    fDevType := aValue;

  If not FestivalStarted then
    fArenaWaitTillNext := 100;
end;

procedure TKMHouseArena.StartFestival;
var I, R : Integer;
  tmp : Cardinal;
begin
  If not CanStartFestival then
    Exit;
  fArenaAnimStep := 1;
  gHands[Owner].TakeFestivalPoints(fptBuilding, BuildingCost);
  gHands[Owner].TakeFestivalPoints(fptEconomy, ValuableCost);
  gHands[Owner].TakeFestivalPoints(fptWarfare, WarfareCost);
  fCurrentDevType := fDevType;

  //set flag colors of the animation
  SetLength(fAnimColors, 1);
  fAnimColors[0] := gHands[Owner].FlagColor; //always add owner's color
  //add allies
  for I := 1 to gHands.Count - 1 do
    If gHands[I].Enabled and (gHands[I].Alliances[Owner] = atAlly) then
    begin
      SetLength(fAnimColors, Length(fAnimColors) + 1);
      fAnimColors[high(fAnimColors)] := gHands[I].FlagColor;
    end;
  //shuffle
  for I := High(fAnimColors) downto 0 do
  begin
    R := KaMRandom(I, 'TKMHouseArena.StartFestival');
    tmp := fAnimColors[I];
    fAnimColors[I] := fAnimColors[R];
    fAnimColors[R] := tmp;
  end;

end;

function TKMHouseArena.FestivalDuration : Word;
begin
  If fDevType = dttAll then
  begin
    Result := IfThen(gHands[Owner].EconomyDevUnlocked(29), FESTIVAL_DURATION_ALL, FESTIVAL_DURATION);
  end
  else
    Result := FESTIVAL_DURATION;

  if gHands[Owner].EconomyDevUnlocked(4) then
    Result := Result - 300;
end;

function TKMHouseArena.FestivalStarted: Boolean;
begin
  Result := (fArenaAnimStep > 0);
end;


function TKMHouseArena.CanStartFestival: Boolean;
begin
  Result := (fDevType <> dttNone)
            and (gHands[Owner].FestivalPoints[fptBuilding] >= BuildingCost)
            and (gHands[Owner].FestivalPoints[fptEconomy] >= ValuableCost)
            and (gHands[Owner].FestivalPoints[fptWarfare] >= WarfareCost);
end;

function TKMHouseArena.PointsCount: Byte;
begin
  case fDevType of
    dttNone : Result := 0;
    dttAll : Result := IfThen(gHands[Owner].EconomyDevUnlocked(29), 2, 1);
    else Result := 3;

  end;
end;

function TKMHouseArena.PointsCount(aType: TKMDevelopmentTreeType): Byte;
begin
  case aType of
    dttNone : Result := 0;
    dttAll : Result := IfThen(gHands[Owner].EconomyDevUnlocked(29), 2, 1);
    else Result := 3;

  end;
end;

function TKMHouseArena.BuildingCost : Byte;
begin
  Result := 0;
  case fDevType of
    dttBuilder : Result := 12;
    dttEconomy : Result := 6;
    dttArmy : Result := 3;
    dttAll : Result := 5;
  end;
  If gHands[Owner].EconomyDevUnlocked(32) then
    Result := Max(Result * 4 div 5, 1);
end;

function TKMHouseArena.WarfareCost : Byte;
begin
  Result := 0;
  case fDevType of
    dttBuilder : Result := 0;
    dttEconomy : Result := 0;
    dttArmy : Result := 16;
    dttAll : Result := 12;
  end;
  If gHands[Owner].EconomyDevUnlocked(32) then
    Result := Result * 4 div 5;
end;

function TKMHouseArena.ValuableCost : Byte;
begin
  Result := 0;
  case fDevType of
    dttBuilder : Result := 60;
    dttEconomy : Result := 90;
    dttArmy : Result := 70;
    dttAll : Result := 60;
  end;
  If gHands[Owner].EconomyDevUnlocked(32) then
    Result := Result * 4 div 5;
end;


procedure TKMHouseArena.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If not IsComplete then
    If aTick mod 10 = 0 then
      UpdatePointBelowEntrance
    else
  else
  begin
    If fArenaWaitTillNext > 0 then
      Dec(fArenaWaitTillNext)
    else
    begin
      If (fCurrentDevType <> dttNone) and (fArenaAnimStep > 0) then
      begin
        Inc(fArenaAnimStep);
        IF fArenaAnimStep >= FestivalDuration then
        begin
          gHands[Owner].AddDevPoint( fCurrentDevType, PointsCount(fCurrentDevType) );
          gGame.RefreshDevelopmentTree;
          fArenaAnimStep := 0;
          fArenaWaitTillNext := 150;
        end;
      end else
      If (fArenaAnimStep = 0) then
        StartFestival;
    end;


  end;
end;


procedure TKMHouseArena.Paint;
begin
  Inherited;
  If not IsComplete then
    Exit;
  If fArenaAnimStep > 0 then
    gRenderPool.AddHouseArena(fCurrentDevType, Position, fArenaAnimStep, fAnimColors);
end;

end.
