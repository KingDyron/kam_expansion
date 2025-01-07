unit KM_HouseHelpers;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses,
  KM_Hand, KM_HandsCollection, KM_HouseWoodcutters, KM_HouseArmorWorkshop, KM_HouseBarracks, KM_HouseCottage, KM_HouseInn, KM_HouseTownHall,
  KM_HouseMarket, KM_HouseSchool, KM_HouseQueue, KM_HouseSiegeWorkshop, KM_HouseStore, KM_HouseSwineStable, KM_HouseWoodBurner,
  KM_ResTypes;
  
type
  TKMHouseHelpers = class helper for TKMHouse
    function Hand : TKMHand;
    function Shipyard : TKMHouseShipYard;
    function Farm : TKMHouseFarm;
    function ProductionThatch : TKMHouseProdThatch;
    function Woodcutters : TKMHouseWoodcutters;
    function WFlagPoint : TKMHouseWFlagPoint;
    function Tower : TKMHouseTower;
    function WallTower : TKMHouseWallTower;
    function Well : TKMHouseWell;
    function Merchant : TKMHouseMerchant;
    function AppleTree : TKMHouseAppleTree;
    function Pottery : TKMHousePottery;
    function Collectors : TKMHouseCollectors;
    function Stall : TKMHouseStall;
    function VineYard : TKMHouseVineYard;
    function Palace : TKMHousePalace;
    function WoodBurner : TKMHouseWoodBurner;
    function SwineStable : TKMHouseSwineStable;
    function Hovel : TKMHouseHovel;
    function Queue : TKMHouseQueue;
    function ArmorWorkshop : TKMHouseArmorWorkshop;
    function Barracks : TKMHouseBarracks;
    function Cottage : TKMHouseCottage;
    function Inn : TKMHouseInn;
    function TownHall : TKMHouseTownHall;
    function Market : TKMHouseMarket;
    function School : TKMHouseSchool;
    function SiegeWorkshop : TKMHouseSiegeWorkshop;
    function Store : TKMHouseStore;
    function NotNil : Boolean;
  end;

implementation

function TKMHouseHelpers.Hand: TKMHand;
begin
  Result := gHands[Owner];
end;
function TKMHouseHelpers.Shipyard : TKMHouseShipYard;
begin
  If self is TKMHouseShipYard then Result := TKMHouseShipYard(self) else Exit(nil);
end;
function TKMHouseHelpers.Farm : TKMHouseFarm;
begin
  If self is TKMHouseFarm then Result := TKMHouseFarm(self) else Exit(nil);
end;
function TKMHouseHelpers.ProductionThatch : TKMHouseProdThatch;
begin
  If self is TKMHouseProdThatch then Result := TKMHouseProdThatch(self) else Exit(nil);
end;
function TKMHouseHelpers.Woodcutters : TKMHouseWoodcutters;
begin
  If self is TKMHouseWoodcutters then Result := TKMHouseWoodcutters(self) else Exit(nil);
end;
function TKMHouseHelpers.WFlagPoint : TKMHouseWFlagPoint;
begin
  If self is TKMHouseWFlagPoint then Result := TKMHouseWFlagPoint(self) else Exit(nil);
end;
function TKMHouseHelpers.Tower : TKMHouseTower;
begin
  If self is TKMHouseTower then Result := TKMHouseTower(self) else Exit(nil);
end;
function TKMHouseHelpers.WallTower : TKMHouseWallTower;
begin
  If self is TKMHouseWallTower then Result := TKMHouseWallTower(self) else Exit(nil);
end;
function TKMHouseHelpers.Well : TKMHouseWell;
begin
  If self is TKMHouseWell then Result := TKMHouseWell(self) else Exit(nil);
end;
function TKMHouseHelpers.Merchant : TKMHouseMerchant;
begin
  If self is TKMHouseMerchant then Result := TKMHouseMerchant(self) else Exit(nil);
end;
function TKMHouseHelpers.AppleTree : TKMHouseAppleTree;
begin
  If self is TKMHouseAppleTree then Result := TKMHouseAppleTree(self) else Exit(nil);
end;
function TKMHouseHelpers.Pottery : TKMHousePottery;
begin
  If self is TKMHousePottery then Result := TKMHousePottery(self) else Exit(nil);
end;
function TKMHouseHelpers.Collectors : TKMHouseCollectors;
begin
  If self is TKMHouseCollectors then Result := TKMHouseCollectors(self) else Exit(nil);
end;
function TKMHouseHelpers.Stall : TKMHouseStall;
begin
  If self is TKMHouseStall then Result := TKMHouseStall(self) else Exit(nil);
end;
function TKMHouseHelpers.VineYard : TKMHouseVineYard;
begin
  If self is TKMHouseVineYard then Result := TKMHouseVineYard(self) else Exit(nil);
end;
function TKMHouseHelpers.Palace : TKMHousePalace;
begin
  If self is TKMHousePalace then Result := TKMHousePalace(self) else Exit(nil);
end;
function TKMHouseHelpers.WoodBurner : TKMHouseWoodBurner;
begin
  If self is TKMHouseWoodBurner then Result := TKMHouseWoodBurner(self) else Exit(nil);
end;
function TKMHouseHelpers.SwineStable : TKMHouseSwineStable;
begin
  If self is TKMHouseSwineStable then Result := TKMHouseSwineStable(self) else Exit(nil);
end;
function TKMHouseHelpers.Hovel : TKMHouseHovel;
begin
  If self is TKMHouseHovel then Result := TKMHouseHovel(self) else Exit(nil);
end;
function TKMHouseHelpers.Queue : TKMHouseQueue;
begin
  If self is TKMHouseQueue then Result := TKMHouseQueue(self) else Exit(nil);
end;
function TKMHouseHelpers.ArmorWorkshop : TKMHouseArmorWorkshop;
begin
  If self is TKMHouseArmorWorkshop then Result := TKMHouseArmorWorkshop(self) else Exit(nil);
end;
function TKMHouseHelpers.Barracks : TKMHouseBarracks;
begin
  If self is TKMHouseBarracks then Result := TKMHouseBarracks(self) else Exit(nil);
end;
function TKMHouseHelpers.Cottage : TKMHouseCottage;
begin
  If self is TKMHouseCottage then Result := TKMHouseCottage(self) else Exit(nil);
end;
function TKMHouseHelpers.Inn : TKMHouseInn;
begin
  If self is TKMHouseInn then Result := TKMHouseInn(self) else Exit(nil);
end;
function TKMHouseHelpers.TownHall : TKMHouseTownHall;
begin
  If self is TKMHouseTownHall then Result := TKMHouseTownHall(self) else Exit(nil);
end;
function TKMHouseHelpers.Market : TKMHouseMarket;
begin
  If self is TKMHouseMarket then Result := TKMHouseMarket(self) else Exit(nil);
end;
function TKMHouseHelpers.School : TKMHouseSchool;
begin
  If self is TKMHouseSchool then Result := TKMHouseSchool(self) else Exit(nil);
end;
function TKMHouseHelpers.SiegeWorkshop : TKMHouseSiegeWorkshop;
begin
  If self is TKMHouseSiegeWorkshop then Result := TKMHouseSiegeWorkshop(self) else Exit(nil);
end;
function TKMHouseHelpers.Store : TKMHouseStore;
begin
  If self is TKMHouseStore then Result := TKMHouseStore(self) else Exit(nil);
end;
function TKMHouseHelpers.NotNil: Boolean;
begin
  Result := self <> nil;
end;

end.
