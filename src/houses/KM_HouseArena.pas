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
    //procedure AddDemandsOnActivate(aWasBuilt: Boolean); override;
    //function GetWareDistribution(aID: Byte): Byte;override; //Will use GetRatio from mission settings to find distribution amount
    procedure UpdateEntrancePos; override;
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;
    //function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; override;
    //procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); override;
    //function CheckWareIn(aWare: TKMWareType): Word; override;

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

    //procedure UpdateDemands; override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
    Constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;

const
  FESTIVAL_DURATION = 2400;
  FESTIVAL_DURATION_ALL = 3600;

implementation
uses
  Classes,
  KM_Game,
  KM_HandsCollection, KM_HandLogistics,
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

{
function TKMHouseArena.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  Result := true;
end;

procedure TKMHouseArena.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False);
var
  I, ordersRemoved: Integer;
begin
  Assert(aWare <> wtNone);
  If aWare in WARES_WARFARE then
  begin
    case aWare of
      wtBow,
      wtLance,
      wtAxe,
      wtPlateArmor,
      wtWoodenShield: Inc(fWarfareDelivered, 1);

      wtMace,
      wtQuiver,
      wtLeatherArmor: Inc(fWarfareDelivered, 2);

      wtIronShield,
      wtIronArmor,
      wtSword,
      wtPike,
      wtCrossbow,
      wtFlail: Inc(fWarfareDelivered, 4);

      wtHorse: Inc(fWarfareDelivered, 12);
      wtBitinArmor: Inc(fWarfareDelivered, 80);
    end;
    for I := 1 to WARES_IN_OUT_COUNT do
      if wtWarfare = WareInput[I] then
      begin
        //Don't allow the static script to overfill houses
        if aFromStaticScript then
          aCount := EnsureRange(aCount, 0, GetMaxInWare);
        //ResIn[I] := ResIn[I] + aCount;
        if aFromStaticScript then
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] + aCount;
          ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - ordersRemoved;
        end else
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - aCount;
        end;
        Break;
      end;

  end else
  If aWare in WARES_HOUSE_FOOD then
  begin
    case aWare of
      wtApple,
      wtVegetables: Inc(fFoodDelivered, 1);
      wtSausage,
      wtWine: Inc(fFoodDelivered, 2);
      wtFish,
      wtBread: Inc(fFoodDelivered, 3);
    end;
    for I := 1 to WARES_IN_OUT_COUNT do
      if wtFood = WareInput[I] then
      begin
        //Don't allow the static script to overfill houses
        if aFromStaticScript then
          aCount := EnsureRange(aCount, 0, GetMaxInWare);
        //ResIn[I] := ResIn[I] + aCount;
        if aFromStaticScript then
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] + aCount;
          ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - ordersRemoved;
        end else
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - aCount;
        end;
        Break;
      end;
  end else
  If aWare in WARES_VALUABLE then
  begin
    case aWare of
      wtEgg: Inc(fValuableDelivered, 9);
      wtFeathers: Inc(fValuableDelivered, 3);
      wtBitin: Inc(fValuableDelivered, 15);
      wtIron: Inc(fValuableDelivered, 9);
      wtGold: Inc(fValuableDelivered, 5);
      wtJewerly: Inc(fValuableDelivered, 250);
    end;
    for I := 1 to WARES_IN_OUT_COUNT do
      if wtValuable = WareInput[I] then
      begin
        //Don't allow the static script to overfill houses
        if aFromStaticScript then
          aCount := EnsureRange(aCount, 0, GetMaxInWare);
        //ResIn[I] := ResIn[I] + aCount;
        if aFromStaticScript then
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] + aCount;
          ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - ordersRemoved;
        end else
        begin
          WareDeliveryCnt[I] := WareDeliveryCnt[I] - aCount;
        end;
        Break;
      end;
  end else
  for I := 1 to WARES_IN_OUT_COUNT do
    if aWare = WareInput[I] then
    begin
      //Don't allow the static script to overfill houses
      if aFromStaticScript then
        aCount := EnsureRange(aCount, 0, GetMaxInWare - ResIn[I]);
      //WareDeliveryCnt stay same, because corresponding demand will be closed
      ResIn[I] := ResIn[I] + aCount;
      if aFromStaticScript then
      begin
        WareDeliveryCnt[I] := WareDeliveryCnt[I] + aCount;
        ordersRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, aWare, aCount);
        WareDeliveryCnt[I] := WareDeliveryCnt[I] - ordersRemoved;
      end;
    end;
  UpdateDemands;
end;

function TKMHouseArena.CheckWareIn(aWare: TKMWareType): Word;
begin
  case aWare of
    wtWarfare : Result := fWarfareDelivered;
    wtFood : Result := fFoodDelivered;
    wtValuable : Result := fValuableDelivered;
    else Result :=  0;
  end;
end;

procedure TKMHouseArena.AddDemandsOnActivate(aWasBuilt: Boolean);
begin
  UpdateDemands;
  UpdatePointBelowEntrance;
end;

function TKMHouseArena.GetWareDistribution(aID: Byte): Byte;
begin
  If WareInput[aID] = wtFood then
    Result := 3
  else
  If WareInput[aID] = wtValuable then
    Result := 3
  else
  If WareInput[aID] = wtWarfare then
    Result := 3
  else
    Result := gHands[Owner].Stats.WareDistribution[WareInput[aID],HouseType];

  if CurrentLevel > 0 then
    if HSpec.Levels[CurrentLevel - 1].MaxInWares > 0 then
      Result := HSpec.Levels[CurrentLevel - 1].MaxInWares;

end;}

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
{
procedure TKMHouseArena.UpdateDemands;
Const MAX_ORDERS = 10;
  function MaxOrders : Word;
  begin
    Result := MAX_ORDERS;
    case HouseType of
      htTownhall : Result := 20;
    end;
  end;
var
  I: Integer;
  demandsRemoved, plannedToRemove, demandsToChange: Integer;
  maxDistribution: Byte;
  resDelivering : Integer;
begin
  If not IsComplete then
    Exit;
  for I := 1 to WARES_IN_OUT_COUNT do
  begin
    if (WareInput[I] in [wtAll, wtNone]) then Continue;

    resDelivering := WareDeliveryCnt[I] - WareDemandsClosing[I];

    maxDistribution := Max(0, Min(GetWareDistribution(I), GetMaxInWare - GetAcceptWareIn(WareInput[I]) ));
    if ResetDemands then
      maxDistribution := 0;

    //demandsToChange := resDistribution - (WareDeliveryCnt[I] - WareDemandsClosing[I]);

    //demandsToChange := Min( 5 - (demandsToChange - fWareIn[I]), GetMaxInWare -  demandsToChange);

    demandsToChange := Min( MaxOrders - (resDelivering - ResIn[I]), maxDistribution -  resDelivering);

    //Not enough resources ordered, add new demand
    if demandsToChange > 0 then
    begin
      gHands[Owner].Deliveries.Queue.AddDemand(Self, nil, WareInput[I], demandsToChange, dtOnce, diNorm);

      WareDeliveryCnt[I] := WareDeliveryCnt[I] + demandsToChange;
    end else
    //Too many resources ordered, attempt to remove demand if nobody has taken it yet
    if demandsToChange < 0 then
    begin
      demandsRemoved := gHands[Owner].Deliveries.Queue.TryRemoveDemand(Self, WareInput[I], -demandsToChange, plannedToRemove);

      WareDeliveryCnt[I] := WareDeliveryCnt[I] - demandsRemoved; //Only reduce it by the number that were actually removed
      WareDemandsClosing[I] := WareDemandsClosing[I] + plannedToRemove;
    end;
  end;
end;
}

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
      //If aTick mod 600 = 0 then
      //  UpdatePointBelowEntrance;
      If (fCurrentDevType <> dttNone) and (fArenaAnimStep > 0) then
      begin
        Inc(fArenaAnimStep);
        IF fArenaAnimStep >= FestivalDuration then
        begin
          gHands[Owner].AddDevPoint( fCurrentDevType, PointsCount(fCurrentDevType) );
          gGame.RefreshDevelopmentTree;
          fArenaAnimStep := 0;
          fArenaWaitTillNext := 150;
          //fDevType := dttNone;
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
  //gRenderAux.Quad(PointBelowEntrance.X, PointBelowEntrance.Y);
  If fArenaAnimStep > 0 then
    gRenderPool.AddHouseArena(fCurrentDevType, Position, fArenaAnimStep, fAnimColors);
end;

end.
