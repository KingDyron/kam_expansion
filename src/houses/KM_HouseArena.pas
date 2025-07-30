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
    fWarfareDelivered, fFoodDelivered : Byte;
    procedure UpdatePointBelowEntrance;
  protected
    procedure AddDemandsOnActivate(aWasBuilt: Boolean); override;
    function GetWareDistribution(aID: Byte): Byte;override; //Will use GetRatio from mission settings to find distribution amount
  public
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;
    function ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean; override;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); override;

    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    procedure UpdateDemands; override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
    Constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;

implementation
uses
  Classes,
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
  fArenaAnimStep := 1;
  fDevType := dttNone;
  fWarfareDelivered := 0;
  fFoodDelivered := 0;
end;

Constructor TKMHouseArena.Load(LoadStream : TKMemoryStream);
begin
  Inherited;
end;

procedure TKMHouseArena.Save(SaveStream : TKMemoryStream);
begin
  Inherited;
end;

function TKMHouseArena.ShouldAbandonDeliveryTo(aWareType: TKMWareType): Boolean;
begin
  Result := Inherited
             or ((fWarfareDelivered > 10) and (aWareType in WARES_WARFARE))
             or ((fFoodDelivered > 10) and (aWareType in WARES_HOUSE_FOOD));
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

procedure TKMHouseArena.AddDemandsOnActivate(aWasBuilt: Boolean);
begin
  UpdateDemands;
  UpdatePointBelowEntrance;
end;

function TKMHouseArena.GetWareDistribution(aID: Byte): Byte;
begin
  If WareInput[aID] = wtFood then
    Result := 2
  else
  If WareInput[aID] = wtWarfare then
    Result := 2
  else
    Result := gHands[Owner].Stats.WareDistribution[WareInput[aID],HouseType];

  if CurrentLevel > 0 then
    if HSpec.Levels[CurrentLevel - 1].MaxInWares > 0 then
      Result := HSpec.Levels[CurrentLevel - 1].MaxInWares;

end;

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

    maxDistribution := Max(0, Min(GetWareDistribution(I), GetMaxInWare{ - GetAcceptWareIn(WareInput[I])}));
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

procedure TKMHouseArena.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If not IsComplete then
    If aTick mod 10 = 0 then
      UpdatePointBelowEntrance
    else
  else
  begin
    //If aTick mod 600 = 0 then
    //  UpdatePointBelowEntrance;
    If fArenaAnimStep > 0 then
      Inc(fArenaAnimStep);
  end;
end;


procedure TKMHouseArena.Paint;
begin
  Inherited;
  If not IsComplete then
    Exit;
  //gRenderAux.Quad(PointBelowEntrance.X, PointBelowEntrance.Y);
  If fArenaAnimStep > 0 then
    gRenderPool.AddHouseArena(fDevType, Position, fArenaAnimStep, [gHands[Owner].FlagColor, $FF00FF55]);
end;

end.
