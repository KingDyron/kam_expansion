unit KM_HousePearl;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_AITypes,
  KM_Houses,
  KM_ResTypes;

type

  TKMHousePearl = class(TKMHouseWFlagPoint)
  private
    //basic
    fPearlType : TKMPearlType;
    fProgress, fMaxProgress : Word;
    fConfirmed : Boolean;
    fBuildCost, fDelivered : TKMWarePlan;
    fBuildStage : Byte;
    fIsCompleted : Boolean;
    //special abilities
    fReloadTime, fMaxReloadTime : Word;
    fWorkingTime : Word;

    fDoRally : Boolean;
    fResFrom, fResTo, fRResTo : TKMWareType;
    fVResTo : Byte;
    fSnowStepPearl : Single;
    function PearlCenter : TKMPoint;
    Procedure SetBuildCost;
    procedure MakeRally;
    procedure HitEnemyWithStone;
    procedure RageAllUnits;
    procedure EquipAllUnitsWithBitinArmor;

    procedure MakeAuraEffect;
    procedure SetResFrom(aType : TKMWareType);
    procedure SetResTo(aType : TKMWareType);

    procedure RSetResTo(aType : TKMWareType);
    procedure UseSpecial;
  protected
    procedure ActivatePearl;
    procedure SetFlagPoint(aFlagPoint: TKMPoint); override;
  public
    //main
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
    //overriden
    procedure IncAnimStep;override;
    procedure IncSnowStepPearl;
    procedure WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False); override;
    function CanHasWorker(aType : TKMUnitType) : Boolean; override;
    procedure Demolish(aFrom: TKMHandID; IsSilent: Boolean = False); override;
    //new
    function CanBuild : Boolean;
    function BuildCost : TKMWarePlan;
    function Delivered : TKMWarePlan;
    function Completed : Boolean;
    function Progress : Single;

    procedure SelectType(aType : TKMPearlType);
    procedure ConfirmBuild;
    procedure IncBuildingPearlProgress;

    function ValtariaRatioTo : Word;
    function ValtariaRatioFrom : Word;
    procedure ValtariaDoExchange(aCount : Byte = 1);

    function AriumRatioTo : Word;
    function AriumRatioFrom : Word;
    property VResTo : Byte read fVResTo write fVResTo;
    procedure AriumDoExchange(aCount : Byte = 1);
    procedure RepairAll;

    procedure AgrosDoExchange;

    function RalenderRatioTo : Word;
    function RalenderRatioFrom : Word;
    procedure RalenderDoExchange(aCount : Byte = 1);
    function RalenderGetClosestTower(aLoc : TKMPoint) : TKMPointF; overload;
    function RalenderGetClosestTower(aLoc : TKMPointF) : TKMPointF; overload;

    property RResTo : TKMWareType read fRResTo write RSetResTo;

    property Confirmed : Boolean read fConfirmed;
    property PearlType : TKMPearlType read fPearlType;
    property DoRally : Boolean read fDoRally;
    property ResFrom : TKMWareType read fResFrom write SetResFrom;
    property ResTo : TKMWareType read fResTo write SetResTo;
    function ReloadProgress : Single;
    function PearlName : UnicodeString;
    function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc : TKMPoint) : TKMPointDir; override;
    function Entrances : TKMPointDirArray; override;

    procedure DoUseSpecial;
    procedure DoExchange(aCount : Byte);

    //map editor
    procedure SetStage(aStage : Integer);
    procedure SetStageProgress(aProgress : Word);
    procedure MapEdConfirm;
    procedure SetWaresCount(aCount : Integer);
    function BuildStage : Byte;
    property StageProgress : Word read fProgress;
    property MaxProgress : Word read fMaxProgress;
    function WaresDeliveredCount : Word;

  end;

implementation
uses
  Classes,
  KM_Game, KM_GameParams,
  KM_CommonUtils,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_HouseHelpers,
  KM_HouseCollection,
  KM_Units, KM_UnitWarrior, KM_UnitsCollection,
  KM_Projectiles, KM_CommonGameTypes,
  KM_Resource, KM_ResUnits, KM_ResHouses, KM_ResTexts,
  KM_RenderPool,
  KM_Terrain;

//main
constructor TKMHousePearl.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fProgress := 0;
  fPearlType := ptNone;
  fBuildCost.Reset;
  fDelivered.Reset;
  fConfirmed := false;
  fDoRally := false;

  fReloadTime := 0;
  fMaxReloadTime := 1;

  fResFrom := wtNone;
  fResTo := wtNone;
  fRResTo := wtNone;
  fVResTo := 0;
  fIsCompleted := false;
end;

constructor TKMHousePearl.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.ReadData(fPearlType);
  LoadStream.ReadData(fProgress);
  LoadStream.ReadData(fMaxProgress);
  LoadStream.ReadData(fConfirmed);
  fBuildCost.Load(LoadStream);
  fDelivered.Load(LoadStream);
  LoadStream.Read(fBuildStage);
  LoadStream.Read(fIsCompleted);

  LoadStream.Read(fDoRally);
  LoadStream.ReadData(fResFrom);
  LoadStream.ReadData(fResTo);
  LoadStream.Read(fVResTo);
  LoadStream.ReadData(fReloadTime);
  LoadStream.ReadData(fMaxReloadTime);
  LoadStream.ReadData(fWorkingTime);
  LoadStream.ReadData(fSnowStepPearl);
end;

procedure TKMHousePearl.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.WriteData(fPearlType);
  SaveStream.WriteData(fProgress);
  SaveStream.WriteData(fMaxProgress);
  SaveStream.WriteData(fConfirmed);
  fBuildCost.Save(SaveStream);
  fDelivered.Save(SaveStream);
  SaveStream.Write(fBuildStage);
  SaveStream.Write(fIsCompleted);

  SaveStream.Write(fDoRally);
  SaveStream.WriteData(fResFrom);
  SaveStream.WriteData(fResTo);
  SaveStream.Write(fVResTo);
  SaveStream.Write(fReloadTime);
  SaveStream.Write(fMaxReloadTime);
  SaveStream.Write(fWorkingTime);
  SaveStream.Write(fSnowStepPearl);
end;

destructor TKMHousePearl.Destroy;
begin
  Inherited;
end;

procedure TKMHousePearl.UpdateState(aTick: Cardinal);
begin
  Inherited;
  If Completed then
    IF fPearlType <> ptNone then
    begin
      IncSnowStepPearl;
      If aTick mod 100 = 0 then
        MakeAuraEffect;
      If fPearlType = ptRalender then
      begin
        If fDoRally then
          If aTick mod 20 = 0 then
            MakeRally;
        If aTick mod 50 = 0 then
          HitEnemyWithStone;
      end;
      If (fReloadTime < fMaxReloadTime) and (fWorkingTime = 0) then
        Inc(fReloadTime);
      If fWorkingTime > 0 then
      begin
        Dec(fWorkingTime);
        If fWorkingTime = 0 then
          fDoRally := false;
      end;
    end;
end;

procedure TKMHousePearl.Paint;
var progress : Single;
begin
  Inherited;

  IF fPearlType <> ptNone then
  begin
    If Completed then
      gRenderPool.AddHousePearl(fPearlType, fPosition, fBuildStage, 1, 1, fSnowStepPearl, gHands[Owner].FlagColor, false)
    else
    If Confirmed then
    begin
      progress := fProgress / fMaxProgress;
      gRenderPool.AddHousePearl(fPearlType, fPosition, fBuildStage, 1, progress, 0, gHands[Owner].FlagColor, false);
    end;
        //gRenderPool.AddSpriteWH(fPosition, KMPOINT_ZERO, 2715, rxHouses, gHands[Owner].FlagColor);
    gRenderPool.AddHouseWork(HouseType, fPosition, CurrentAction.SubAction * [haFire1..haFire8], WorkAnimStep, WorkAnimStepPrev, GetFlagColor);
  end;

end;



//overriden

procedure TKMHousePearl.IncAnimStep;
begin
  Inherited;
  If Completed then
    IF fPearlType <> ptNone then
      IncSnowStepPearl;
end;
procedure TKMHousePearl.IncSnowStepPearl;
const
  //How much ticks it takes for a house to become completely covered in snow
  SNOW_TIME = 1200;
begin
  if OnTerrain = tptSnow then
  begin
    if (fSnowStepPearl < 1) then
      fSnowStepPearl := Min(fSnowStepPearl + (1 + Byte(gGameParams.IsMapEditor) * 10) / SNOW_TIME, 1)
  end else
    fSnowStepPearl := 0;
end;

procedure TKMHousePearl.WareAddToIn(aWare: TKMWareType; aCount: Integer = 1; aFromStaticScript: Boolean = False);
var I : Integer;
begin
  //first check if ware is building resource
  for I := 0 to High(fDelivered) do
    If (fDelivered[I].W = aWare) and (Delivered[I].C < fBuildCost[I].C) then
    begin
      Inc(fDelivered[I].C, aCount);
      Exit;
    end;

  inherited;
end;

function TKMHousePearl.CanHasWorker(aType : TKMUnitType) : Boolean;
begin
    Result := (fPearlType = ptAgros)
              and Completed
              and (aType = utRecruit)
              and (WorkersCount= 0)
              and not IsDestroyed
              and IsComplete
              and not IsClosedForWorker;
end;

procedure TKMHousePearl.Demolish(aFrom: TKMHandID; IsSilent: Boolean = False);
begin
  gHands[Owner].PearlDestroyed(fPearlType);
  Inherited;
end;

//new
function TKMHousePearl.CanBuild: Boolean;
begin
  Result := (fProgress < fMaxProgress) and (fDelivered[fBuildStage].C = fBuildCost[fBuildStage].C);
end;

function TKMHousePearl.PearlCenter : TKMPoint;
begin
  Result.X := Entrance.X;
  Result.Y := Entrance.Y - 2;
end;

procedure TKMHousePearl.SetBuildCost;
var I : integer;
begin
  fBuildCost.Reset;
  {case fPearlType of
    ptNone,
    ptValtaria: begin
                  fBuildCost.AddWare(wtTimber, 1);
                  fBuildCost.AddWare(wtStone, 1);
                  fBuildCost.AddWare(wtTrunk, 1);
                  fBuildCost.AddWare(wtTile, 1);
                  fBuildCost.AddWare(wtIron, 1);
                  fBuildCost.AddWare(wtGold, 1);
                end;
    ptArium: begin
                  fBuildCost.AddWare(wtTimber, 1);
                  fBuildCost.AddWare(wtStone, 1);
                  fBuildCost.AddWare(wtTrunk, 1);
                  fBuildCost.AddWare(wtTile, 1);
                  fBuildCost.AddWare(wtIron, 1);
                  fBuildCost.AddWare(wtSteelE, 1);
                  fBuildCost.AddWare(wtBitinE, 1);
                end;
    ptAgros: begin
                  fBuildCost.AddWare(wtTimber, 1);
                  fBuildCost.AddWare(wtStone, 1);
                  fBuildCost.AddWare(wtTrunk, 1);
                  fBuildCost.AddWare(wtTile, 1);
                end;
    ptRalender: begin
                  fBuildCost.AddWare(wtTimber, 1);
                  fBuildCost.AddWare(wtStone, 1);
                  fBuildCost.AddWare(wtTrunk, 1);
                  fBuildCost.AddWare(wtTile, 1);
                  fBuildCost.AddWare(wtIron, 1);
                end;
  end;}
  fBuildCost.SetCount(gRes.Houses.Pearls[fPearlType].StageCount);
  fDelivered.SetCount(fBuildCost.Count);
  for I := 0 to High(fBuildCost) do
  begin
    fBuildCost[I].W := gRes.Houses.Pearls[fPearlType].Cost[I].W;
    fBuildCost[I].C := gRes.Houses.Pearls[fPearlType].Cost[I].C;
    fDelivered[I].W := fBuildCost[I].W;
    fDelivered[I].C := 0;
  end;

end;

procedure TKMHousePearl.SelectType(aType: TKMPearlType);
begin
  If fConfirmed and not gGame.Params.IsMapEditor then
    Exit;
  fPearlType := aType;
  SetBuildCost;
end;

procedure TKMHousePearl.ConfirmBuild;
begin
  If fConfirmed or (fPearlType = ptNone) then
    Exit;

  fConfirmed := true;
  fBuildStage := 0;
  gHands[Owner].Constructions.PearlList.AddHouse(self);

  fMaxProgress := gRes.Houses.Pearls[fPearlType].ProgressPerStage * fBuildCost[fBuildStage].C;

  gHands[Owner].Deliveries.Queue.AddDemand(self, nil, fBuildCost[fBuildStage].W, fBuildCost[fBuildStage].C);
end;

procedure TKMHousePearl.SetFlagPoint(aFlagPoint: TKMPoint);
begin
  If not Completed or not (fPearlType = ptAgros) then
  begin
    aFlagPoint := PointBelowEntrance;
  end;
  Inherited;
end;

procedure TKMHousePearl.ActivatePearl;
var I : integer;
begin
  fIsCompleted := true;
  gHands[Owner].PearlBuilt(fPearlType);

  //bonus on activate
  case fPearlType of
    ptValtaria : gHands[Owner].FogOfWar.RevealCircle(Entrance, 50, FOG_OF_WAR_MAX, frtHouse) ;
    ptArium:  begin
                for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
                  gHands[Owner].VirtualWareTake(I, -100);
                gHands[Owner].Workless := gHands[Owner].Workless + 100;
              end;

    ptAgros : EquipAllUnitsWithBitinArmor;
    //ralender has watch tower inside it, look at UpdateState
    ptRalender : ;
  end;

  case fPearlType of
    ptValtaria : MakeWareSlot([wtSword, wtBow, wtCrossBow, wtMace, wtFlail, wtPike], [wtBread, wtSausage, wtFish, wtWine, wtApple, wtVegetables]);//exchange weapons to food
    ptRalender : MakeWareSlot([wtEgg], [wtWoodenShield, wtLeatherArmor, wtBoots, wtIronShield, wtIronArmor, wtQuiver]);//exchange eggs to weapons
  end;

  UseSpecial;
end;


procedure TKMHousePearl.IncBuildingPearlProgress;
begin
  If Completed then
    Exit;
  Inc(fProgress);

  If gHands[Owner].BuildDevUnlocked(19) then
    Inc(fProgress);

  If fProgress >= fMaxProgress then
  begin
    Inc(fBuildStage);
    If fBuildStage = gRes.Houses.Pearls[fPearlType].StageCount then
      ActivatePearl
    else
    begin
      fProgress := 0;
      gHands[Owner].Deliveries.Queue.AddDemand(self, nil, fBuildCost[fBuildStage].W, fBuildCost[fBuildStage].C);
      fMaxProgress := gRes.Houses.Pearls[fPearlType].ProgressPerStage * fBuildCost[fBuildStage].C;
    end;

  end;
end;

function TKMHousePearl.BuildCost : TKMWarePlan;
begin
  If fPearlType <> ptNone then
    Result := fBuildCost
  else
    Result.SetCount(0);
end;

function TKMHousePearl.Delivered : TKMWarePlan;
begin
  If fPearlType <> ptNone then
    Result := fDelivered
  else
    Result.SetCount(0);
end;

function TKMHousePearl.Completed: Boolean;
begin
  Result := fIsCompleted{(fProgress >= fMaxProgress) and (fPearlType <> ptNone) and fConfirmed};
end;

function TKMHousePearl.Progress: Single;
begin
  Result := fProgress / fmaxProgress;
end;


procedure TKMHousePearl.MakeRally;
var I : Integer;
begin
  for I := 0 to gHands[Owner].Units.Count - 1 do
    If (gHands[Owner].Units[I].UnitType in UNITS_CITIZEN)
      and gHands[Owner].Units[I].IsIdle
      and (KMLengthDiag(gHands[Owner].Units[I].Position, PearlCenter) < 30)  then
      gHands[Owner].Units[I].GoMakePearlRally(self);
end;

procedure TKMHousePearl.HitEnemyWithStone;
var U : TKMUnit;
begin
  U := gTerrain.UnitsHitTestWithinRad(Entrance + KMPoint(0, -2),//center of the building
                                      2,
                                      13,
                                      Owner, atEnemy, dirNA, not RANDOM_TARGETS, False);
  If (U <> nil) and not (U.IsDeadOrDying) then
    gProjectiles.AimTarget(RalenderGetClosestTower(U.PositionF), U.PositionF, 0.2, ptTowerRock, nil, 0, 15.99);
end;

procedure TKMHousePearl.RageAllUnits;
var I : Integer;
begin
  for I := 0 to gHands[Owner].Units.Count - 1 do
    If (gHands[Owner].Units[I] is TKMUnitWarrior)
      and (KMLengthDiag(gHands[Owner].Units[I].Position, PearlCenter) < 30)  then
      TKMUnitWarrior(gHands[Owner].Units[I]).SetRageTime(200);
end;

procedure TKMHousePearl.EquipAllUnitsWithBitinArmor;
var I : Integer;
begin
  for I := 0 to gHands[Owner].Units.Count - 1 do
    If (gHands[Owner].Units[I].UnitType in WARRIORS_IRON)
      and (KMLengthDiag(gHands[Owner].Units[I].Position, PearlCenter) < 30)  then
      TKMUnitWarrior(gHands[Owner].Units[I]).AddBitin;
end;

procedure TKMHousePearl.MakeAuraEffect;
var I : Integer;
  effectType : TKMUnitEffectType;
  maxDist : Word;
begin
  effectType := uetNone;

  case fPearlType of
    ptValtaria : effectType := uetHealing;
    ptArium : effectType := uetSpeedUp;

    ptAgros : effectType := uetAttack;
    ptRalender : effectType := uetDefence;
  end;
  maxDist := 10;

  If gHands[Owner].BuildDevUnlocked(18) then
    maxDist := 14;

  for I := 0 to gHands[Owner].Units.Count - 1 do
    If (KMLengthDiag(gHands[Owner].Units[I].Position, PearlCenter) <= maxDist)
      and not gHands[Owner].Units[I].IsDeadOrDying then
      gHands[Owner].Units[I].SetEffect(effectType, 100);
end;


procedure TKMHousePearl.SetResFrom(aType : TKMWareType);
begin
  If GetWareInIndex(aType) > 0 then
    fResFrom := aType;
end;

procedure TKMHousePearl.SetResTo(aType : TKMWareType);
begin
  If GetWareOutIndex(aType) > 0 then
    fResTo := aType;
end;

function TKMHousePearl.ValtariaRatioFrom: Word;
begin
  If (fResFrom = wtNone) or (fResTo = wtNone) then
    Exit(0);
  Result := 1;
end;

function TKMHousePearl.ValtariaRatioTo: Word;
var costFrom, costTo : Single;
begin
  If (fResFrom = wtNone) or (fResTo = wtNone) then
    Exit(0);
  //When trading target ware is priced higher
  costFrom := gRes.Wares[fResFrom].MarketPrice;
  costTo := gRes.Wares[fResTo].MarketPrice * 2.2 * 0.25;
  Result := Min(Round(costFrom / Min(costFrom, costTo)), High(Word));
end;

procedure TKMHousePearl.ValtariaDoExchange(aCount : Byte = 1);
var I : Integer;
begin
  If (fResFrom = wtNone) or (fResTo = wtNone) then
    Exit;
  for I := 1 to aCount do
  begin
    If (CheckWareIn(fResFrom) > 0) and (CheckWareOut(fResTo) < GetMaxOutWare) then
    begin
      WareTakeFromIn(fResFrom);
      WareAddToOut(fResTo, ValtariaRatioTo);
    end else
      Exit;
  end;

end;

function TKMHousePearl.AriumRatioFrom: Word;
var costFrom, costTo : Single;
begin
  If fVResTo = 0 then
    Exit(0);

  costFrom := 1;
  costTo := gRes.Wares.VirtualWares[fVResTo].CoinPrice * 2;
  Result := Min(Round(costTo / Min(costFrom, costTo)), High(Word));
end;

function TKMHousePearl.AriumRatioTo: Word;
var costFrom, costTo : Single;
begin
  If fVResTo = 0 then
    Exit(0);

  //When trading target ware is priced higher
  costFrom := 1;
  costTo := gRes.Wares.VirtualWares[fVResTo].CoinPrice * 2;
  Result := Min(Round(costFrom / Min(costFrom, costTo)), High(Word));
end;

procedure TKMHousePearl.AriumDoExchange(aCount: Byte = 1);
var I, C : Integer;
begin
  If fVResTo = 0 then
    Exit;

  for I := 1 to aCount do
  begin
    C := AriumRatioFrom;
    If gHands[Owner].VirtualWareTake('vtCoin', C) then
      gHands[Owner].VirtualWareTake(fVResTo, -1)
    else
      Exit;
  end;
end;

procedure TKMHOusePearl.RepairAll;
var houses : TKMHousesCollection;
  I : Integer;
begin
  houses := gHands[Owner].Houses;
  for I := 0 to houses.Count - 1 do
    If (houses[I] <> nil)
    and not houses[I].IsDestroyed
    and houses[I].IsDamaged then
      houses[I].BuildingRepair := true;
end;

procedure TKMHousePearl.AgrosDoExchange;
  function IsBarracksUnit(aType : TKMUnitType) : Boolean;
  var UT : TKMUnitType;
  begin
    Result := false;
    for UT in BARRACKS_GAME_ORDER do
      If aType = UT then
        Exit(true);
  end;

var I, C : Integer;
  U : TKMUnitRecruit;
  W : TKMUnitWarrior;
  UT : TKMUnitType;

begin
  If WorkersCount(utRecruit) > 0 then
  for I := 0 to High(fWorkers) do
    If (TKMUnit(fWorkers[I]).UnitType = utRecruit) and (TKMUnit(fWorkers[I]).InHouse = self) then
    begin
      C := KamRandom(318 + 200 + 1, 'TKMHousePearl.AgrosDoExchange') + 1;
      case C of
        //1..100    : UT := utFighter;
        101..125  : UT := utPyro;
        126..150  : UT := utTorchMan;
        151..175  : UT := utLekter;
        176..200  : UT := utMedic;
        201..220  : UT := utArcher;
        221..225  : UT := utPaladin;
        226..235  : UT := utSpy;
        236..265  : UT := utTrainedWolf;
        266..285  : UT := utAmmoCart;
        286..295  : UT := utPikeMachine;
        296..315  : UT := utSpikedTrap;
        316..318  : UT := utPaladin;
        else UT := utFighter;
      end;
      If UT = utNone then
        Exit;

      U := TKMUnitRecruit(fWorkers[I]);
      self.SetState(hstEmpty);
      U.KillInHouse;
      If not IsBarracksUnit(UT) then
        gHands[Owner].Stats.RecruitKilledInPearl;

      W := TKMUnitWarrior(gHands[Owner].TrainUnit(UT, Self));
      W.Visible := False; //Make him invisible as he is inside the barracks
      W.Condition := UNIT_MAX_CONDITION;

      W.SetActionGoIn(uaWalk, gdGoOutside, Self, true);
      if Assigned(W.OnUnitTrained) then
        W.OnUnitTrained(W);

      gHands[Owner].SetVirtualWareCnt('vtCertificate', 3);
      Exit;
    end;
end;


procedure TKMHousePearl.RSetResTo(aType : TKMWareType);
begin
  If GetWareOutIndex(aType) > 0 then
    fRResTo := aType;
end;

function TKMHousePearl.RalenderRatioFrom: Word;
begin
  If (fRResTo = wtNone) then
    Exit(0);
  Result := 1;
end;

function TKMHousePearl.RalenderRatioTo: Word;
var costFrom, costTo : Single;
begin
  If (fRResTo = wtNone) then
    Exit(0);
  //When trading target ware is priced higher
  costFrom := gRes.Wares[wtEgg].MarketPrice;
  costTo := gRes.Wares[fRResTo].MarketPrice * 2.2 * 0.22;
  Result := Min(Round(costFrom / Min(costFrom, costTo)), High(Word));
end;

procedure TKMHousePearl.RalenderDoExchange(aCount : Byte = 1);
var I : Integer;
begin
  If (fRResTo = wtNone) then
    Exit;
  for I := 1 to aCount do
  begin
    If (CheckWareIn(wtEgg) > 0) and (CheckWareOut(fRResTo) < GetMaxOutWare) then
    begin
      WareTakeFromIn(wtEgg);
      WareAddToOut(fRResTo, RalenderRatioTo);
    end else
      Exit;
  end;

end;

function TKMHousePearl.RalenderGetClosestTower(aLoc : TKMPoint) : TKMPointF;
begin
  RalenderGetClosestTower(aLoc.ToFloat);
end;

function TKMHousePearl.RalenderGetClosestTower(aLoc : TKMPointF) : TKMPointF;
const  TOWER_POS : array[1..4] of TKMPointF = ( (X : -1.52; Y : 1.92),
                                                (X : -1.52; Y : -2.05),
                                                (X : 2.55; Y : -1.92),
                                                (X : 2.6; Y : 1.92) );
var I : Integer;
  lastDist, tmp : Single;
begin
  Result := PearlCenter.ToFloat;
  lastDist := 99999;
  for I := low(TOWER_POS) to High(TOWER_POS) do
  begin
    tmp := KMLength(aLoc, PearlCenter.ToFloat + TOWER_POS[I]);
    If tmp < lastDist then
    begin
      lastDist := tmp;
      Result := PearlCenter.ToFloat + TOWER_POS[I];
    end;
  end;

end;


function TKMHousePearl.ReloadProgress: Single;
begin
  Result := fReloadTime / fMaxReloadTime;
end;

procedure TKMHousePearl.UseSpecial;
begin
  fReloadTime := 0;

  case fPearlType of
    ptValtaria: fMaxReloadTime := 1800;
    ptArium: fMaxReloadTime := 1200;
    ptAgros: fMaxReloadTime := 6000;
    ptRalender: fMaxReloadTime := 3000;
    else fMaxReloadTime := 1;
  end;

  If gHands[Owner].EconomyDevUnlocked(19) then
    fMaxReloadTime := Round(fMaxReloadTime * 0.80);

  case fPearlType of
    ptRalender: fWorkingTime := 1200;
    else fWorkingTime := 0;
  end;

end;

function TKMHousePearl.PearlName: UnicodeString;
begin
  case fPearlType of
    ptValtaria..ptRalender: Result := gResTexts[byte(fPearlType) + 2208];
    else Result := '';
  end;
end;

function TKMHousePearl.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHousePearl.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirS),
              KMPointDir(Entrance.X - 2, Entrance.Y - 2, dirW),
              KMPointDir(Entrance.X, Entrance.Y - 4, dirN),
              KMPointDir(Entrance.X + 2, Entrance.Y - 2, dirE)
            ];
end;


function TKMHousePearl.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
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

procedure TKMHousePearl.DoUseSpecial;
begin
  UseSpecial;

  case fPearlType of
    ptNone: ;
    ptValtaria: gHands[Owner].ProceedPearlBell(PearlCenter);
    ptArium: RepairAll;
    ptAgros: RageAllUnits;
    ptRalender: fDoRally := true;
  end;
end;

procedure TKMHousePearl.DoExchange(aCount : Byte);
begin
  case fPearlType of
    ptNone: ;
    ptValtaria: ValtariaDoExchange(aCount);
    ptArium: AriumDoExchange(aCount);
    ptAgros: AgrosDoExchange;
    ptRalender: RalenderDoExchange(aCount);
  end;
end;

//map editor
procedure TKMHousePearl.SetStage(aStage : Integer);
var maxStage : Byte;
begin
  maxStage := gRes.Houses.Pearls[fPearlType].StageCount;
  fBuildStage := EnsureRange(aStage, 0, maxStage);
  fProgress := 0;
  fConfirmed := false;
  If maxStage > 0 then
  begin
    fConfirmed := true;
    fIsCompleted := false;
    if fBuildStage = maxStage then
    begin
      fIsCompleted := true;
    end else
    If fBuildStage < maxStage then
      fMaxProgress := gRes.Houses.Pearls[fPearlType].ProgressPerStage * fBuildCost[fBuildStage].C;
  end;
end;

procedure TKMHousePearl.SetStageProgress(aProgress : Word);
var maxProg : Integer;
begin
  If fBuildStage = gRes.Houses.Pearls[fPearlType].StageCount then
  begin
    fProgress:= 0;
    fIsCompleted := true;
    Exit;
  end;
  fIsCompleted := false;
  maxProg := Min(fMaxProgress, gRes.Houses.Pearls[fPearlType].ProgressPerStage * fDelivered[fBuildStage].C);
  fProgress := EnsureRange(aProgress, 0, maxProg);
  If fProgress = fMaxProgress then
  begin
    SetStageProgress(0);
    SetStage(fBuildStage + 1);
  end;
end;

procedure TKMHousePearl.MapEdConfirm;
var I : Integer;
begin
  fConfirmed := true;

  If fBuildStage = gRes.Houses.Pearls[fPearlType].StageCount then
  begin
    fProgress:= 0;
    ActivatePearl;
    Exit;
  end else
  begin
    I := fBuildCost[fBuildStage].C - fDelivered[fBuildStage].C;
    gHands[Owner].Constructions.PearlList.AddHouse(self);
    If I > 0 then
    begin
      gHands[Owner].Deliveries.Queue.AddDemand(self, nil, fBuildCost[fBuildStage].W, I);
    end;

  end;
end;

function TKMHousePearl.BuildStage : Byte;
begin
  Result := fBuildStage;
end;

function TKMHousePearl.WaresDeliveredCount : Word;
begin
  If BuildStage = gRes.Houses.Pearls[PearlType].StageCount then
    Exit(0);

  Result := fDelivered[BuildStage].C;

end;

procedure TKMHousePearl.SetWaresCount(aCount : Integer);
var i : Integer;
begin
  If BuildStage = gRes.Houses.Pearls[PearlType].StageCount then
    Exit;
  for I := 0 to BuildStage - 1 do
    fDelivered[I].C := fBuildCost[I].C;
  fDelivered[BuildStage].C := aCount;
end;

end.
