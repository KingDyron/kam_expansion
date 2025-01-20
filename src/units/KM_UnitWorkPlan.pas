unit KM_UnitWorkPlan;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_CommonClasses, KM_Points, KM_TerrainTypes, KM_Units,
  KM_ResHouses,
  KM_ResTypes;


const
  MAX_WORKPLAN = 24;
type
  TKMWorkPlanAllowedEvent = function(aProduct: TKMWareType): Boolean of object;

  TKMUnitWorkPlan = class
  private
    fHome: TKMHouseType;
    fIssued: Boolean;
    function ChooseTree(const aLoc, aAvoid: TKMPoint; aRadius: Integer; aPlantAct: TKMPlantAct; aUnit: TKMUnit;
                        out Tree: TKMPointDir; out PlantAct: TKMPlantAct): Boolean;

    procedure Clear;
    procedure WalkStyle(const aLoc2: TKMPointDir; aTo, aWork: TKMUnitActionType; aCycles, aDelay: Byte; aFrom: TKMUnitActionType; aScript: TKMGatheringScript);
    procedure SubActAdd(aAct: TKMHouseActionType; aCycles: Single);
    procedure ResourcePlan(Res1: TKMWareType; Qty1: Byte; Res2: TKMWareType; Qty2: Byte; Prod1: TKMWareType; Prod2: TKMWareType = wtNone;Prod3: TKMWareType = wtNone);
    procedure DefaultPlan(aUnit: TKMUnit; aProduct : TKMWareType = wtNone);
    procedure ClearPlan;
    procedure ActSetByMultiplier(aUnit : TKMUnit; aMultiplier : Single = 0);
 public
    HasToWalk: Boolean;
    Loc: TKMPoint;
    ActionWalkTo: TKMUnitActionType;
    ActionWorkType: TKMUnitActionType;
    WorkCyc: Integer;
    WorkDir: TKMDirection;
    GatheringScript: TKMGatheringScript;
    AfterWorkDelay: Integer;
    ActionWalkFrom: TKMUnitActionType;
    WorkHouse : Pointer;
    Res, Prod: TKMWarePlan;
    GrainType : TKMGrainFarmSet;

    ActCount: Byte;
    HouseAct: array [0..MAX_WORKPLAN - 1] of record
      Act: TKMHouseActionType;
      TimeToWork: Word;
    end;

    AfterWorkIdle: Integer;
    ResourceDepleted: Boolean;
    TMPInt : Integer;
  public
    TotalWorkingTime : Integer;
    procedure FindPlan(aUnit: TKMUnit; aHome: TKMHouseType; aProduct: TKMWareType;
                       aLoc: TKMPoint; aPlantAct: TKMPlantAct);
    function ValidPlan : Boolean;
    function FindDifferentResource(aUnit: TKMUnit; aLoc: TKMPoint; const aAvoidLoc: TKMPoint): Boolean;
    function CanWork(aUnit : TKMUnit): Boolean;
    property IsIssued: Boolean read fIssued;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses
  SysUtils,
  KM_Game,
  KM_CommonHelpers,
  KM_Hand, KM_HandTypes, KM_Entity, KM_HandEntity,
  KM_ResUnits,
  KM_Houses, KM_HouseWoodcutters, KM_HouseSiegeWorkshop, KM_HouseWoodBurner, KM_HouseQueue,
  KM_HouseSwineStable,
  KM_MapEditor, KM_MapEdTypes,
  KM_Terrain, KM_ResWares, KM_Log, KM_ResMapElements, KM_ResTileset, KM_ResTilesetTypes,
  KM_Resource, KM_CommonUtils, KM_HandsCollection, Math;


{Houses are only a place on map, they should not issue or perform tasks (except Training)
Everything should be issued by units
Where to go, which walking style, what to do on location, for how long
How to go back in case success, incase bad luck
What to take from supply, how much, take2, much2
What to do, Work/Cycles, What resource to add to Output, how much
E.g. CoalMine: Miner arrives at home and Idles for 5sec, then takes a work task (depending on ResOut count)
Since Loc is 0,0 he immidietely skips to Phase X where he switches house to Work1 (and self busy for same framecount)
Then Work2 and Work3 same way. Then adds resource to out and everything to Idle for 5sec.
E.g. Farmer arrives at home and Idles for 5sec, then takes a work task (depending on ResOut count, HouseType and Need to sow corn)
...... then switches house to Work1 (and self busy for same framecount)
Then Work2 and Work3 same way. Then adds resource to out and everything to Idle for 5sec.}

procedure TKMUnitWorkPlan.Clear;
begin
  fIssued := False;
  HasToWalk := False;
  Loc := KMPOINT_ZERO;
  ActionWalkTo := uaWalk;
  ActionWorkType := uaWork;
  WorkCyc := 0;
  WorkDir := dirNA;
  GatheringScript := gsNone;
  AfterWorkDelay := 0;
  ActionWalkFrom := uaWalk;
  WorkHouse := nil;
  ActCount := 0;
  GrainType := GRAIN_FARM_SET_NONE;

  AfterWorkIdle := 0;
  ResourceDepleted := False;
  TMPInt := 0;

  Res.SetCount(WARES_IN_OUT_COUNT);
  Prod.SetCount(WARES_IN_OUT_COUNT);
end;


procedure TKMUnitWorkPlan.WalkStyle(const aLoc2: TKMPointDir; aTo, aWork: TKMUnitActionType; aCycles,aDelay: Byte; aFrom: TKMUnitActionType; aScript: TKMGatheringScript);
begin
  Loc := aLoc2.Loc;
  HasToWalk := True;
  ActionWalkTo := aTo;
  ActionWorkType := aWork;
  WorkCyc := aCycles;
  AfterWorkDelay := aDelay;
  GatheringScript := aScript;
  ActionWalkFrom := aFrom;
  WorkDir := aLoc2.Dir;
end;


procedure TKMUnitWorkPlan.SubActAdd(aAct: TKMHouseActionType; aCycles: Single);
begin
  If ActCount >= 24 then
    Exit;
  HouseAct[ActCount].Act := aAct;
  HouseAct[ActCount].TimeToWork := Round(gRes.Houses[fHome].Anim[aAct].Count * aCycles);
  Inc(ActCount);
end;


procedure TKMUnitWorkPlan.ActSetByMultiplier(aUnit : TKMUnit; aMultiplier : Single = 0);
var I : Integer;
begin
  TotalWorkingTime := 0;
  for I := low(HouseAct) to High(HouseAct) do
  begin
    HouseAct[I].TimeToWork := Round(HouseAct[I].TimeToWork * IfThen(aMultiplier = 0, aUnit.Home.WorkMultiplier, aMultiplier));
    Inc(TotalWorkingTime, HouseAct[I].TimeToWork);
  end;
  aUnit.Home.TotalWorkingTime := TotalWorkingTime;
end;

procedure TKMUnitWorkPlan.ResourcePlan(Res1: TKMWareType; Qty1: Byte; Res2: TKMWareType; Qty2: Byte; Prod1: TKMWareType; Prod2: TKMWareType = wtNone;Prod3: TKMWareType = wtNone);
begin
  ClearPlan;

  with Res[0] do
  begin
    W := Res1; C := Qty1;
  end;

  with Res[1] do
  begin
    W := Res2; C := Qty2;
  end;

  with Prod[0] do
  begin
    W := Prod1; C := gRes.Houses[fHome].GetWareProdCt(W);
  end;

  with Prod[1] do
  begin
    W := Prod2; C := gRes.Houses[fHome].GetWareProdCt(W);
  end;

  with Prod[2] do
  begin
    W := Prod3; C := gRes.Houses[fHome].GetWareProdCt(W);
  end;

end;

procedure TKMUnitWorkPlan.ClearPlan;
var
  I : Integer;
begin
  for I := 0 to WARES_IN_OUT_COUNT - 1 do
  begin
    Res[I].W := wtNone;
    Res[I].C := 0;

    Prod[I].W := wtNone;
    Prod[I].C := 0;
  end;

end;

procedure TKMUnitWorkPlan.DefaultPlan(aUnit: TKMUnit; aProduct : TKMWareType = wtNone);
var H : TKMHouse;
  I, K : Integer;
  NeededWare : array[1..WARES_IN_OUT_COUNT] of Byte;
  WArr : TKMWareTypeArray;
  allIsNone : Boolean;
begin
  ClearPlan;
  H := aUnit.Home;
  if aProduct <> wtNone then
    if not ((wtAll in gRes.Units[aUnit.UnitType].ProducesWares)
      or (aProduct in gRes.Units[aUnit.UnitType].ProducesWares)) then
      Exit;
  
  if aProduct <> wtNone then
    if gRes.Houses[fHome].DoesOrders then
    begin
      WArr := gRes.Wares[aProduct].OrderCost;

      allIsNone := false;
      for I := 0 to high(WArr) do
        if WArr[I] <> wtNone then
          allIsNone := false;

      if allIsNone then
        Exit;

      //clean array
      for K := 1 to 4 do
        NeededWare[K] := 0;

      //Count how many resources are needed
      for I := 0 to high(WArr) do
        if WArr[I] <> wtNone then
          for K := 1 to WARES_IN_OUT_COUNT do
            if H.WareInput[K] = WArr[I] then
              Inc(NeededWare[K]);


      for I := 0 to WARES_IN_OUT_COUNT - 1 do
        if NeededWare[I+1] > 0 then
        begin
          Res[I].W := H.WareInput[I+1];
          Res[I].C := NeededWare[I+1];
        end else
        begin
          Res[I].W := wtNone;
          Res[I].C := 0;
        end;

      Prod[0].W := aProduct;
      Prod[0].C := gRes.Houses[fHome].GetWareProdCt(aProduct);

      if length(gRes.Houses[H.HouseType].WorkAnim) > 0 then
      begin
        for I := 0 to High(gRes.Houses[H.HouseType].WorkAnim) do
          with gRes.Houses[H.HouseType].WorkAnim[I] do
            SubActAdd(Action,Cycles);

      end;

      Exit;
    end;
  if H is TKMHouseQueue then
  begin
    if TKMHouseQueue(H).Queue[0].W = wtNone then Exit;

    Prod[0].W := TKMHouseQueue(H).Queue[0].W;
    Prod[0].C := TKMHouseQueue(H).Queue[0].Qt;
    WArr := gRes.Wares[Prod[0].W].OrderCost;

    allIsNone := false;
    for I := 0 to high(WArr) do
      if WArr[I] <> wtNone then
        allIsNone := false;

    if allIsNone then
      Exit;

    //clean array
    for K := 1 to WARES_IN_OUT_COUNT do
      NeededWare[K] := 0;

    //Count how many resources are needed
    for I := 0 to high(WArr) do
      if WArr[I] <> wtNone then
        for K := 1 to 4 do
          if H.WareInput[K] = WArr[I] then
            Inc(NeededWare[K]);


    for I := 0 to WARES_IN_OUT_COUNT - 1 do
      if NeededWare[I+1] > 0 then
      begin
        Res[I].W := H.WareInput[I+1];
        Res[I].C := NeededWare[I+1] * Prod[0].C;
      end else
      begin
        Res[I].W := wtNone;
        Res[I].C := 0;
      end;
    Prod[0].C := TKMHouseQueue(H).Queue[0].Qt * gRes.Wares[Prod[0].W].GetProductionCount(htTailorsShop);


    if length(gRes.Houses[H.HouseType].WorkAnim) > 0 then
      for I := 0 to High(gRes.Houses[H.HouseType].WorkAnim) do
        with gRes.Houses[H.HouseType].WorkAnim[I] do
          SubActAdd(Action,(Cycles) * Prod[0].C);

    Exit;
  end;

  for I := 0 to WARES_IN_OUT_COUNT - 1 do
  begin
    Res[I].W := H.WareInput[I+1];

    if Res[I].W <> wtNone then
      Res[I].C := 1;

    if not ((wtAll in gRes.Units[aUnit.UnitType].ProducesWares)
      or (H.WareOutput[I+1] in gRes.Units[aUnit.UnitType].ProducesWares)) then
      Continue;


    Prod[I].W := H.WareOutput[I+1];

    if Prod[I].W <> wtNone then
      if Prod[I].W <> wtSawDust then
        Prod[I].C := gRes.Houses[fHome].GetWareProdCt(Prod[I].W);

  end;

  if length(gRes.Houses[H.HouseType].WorkAnim) > 0 then
    for I := 0 to High(gRes.Houses[H.HouseType].WorkAnim) do
      with gRes.Houses[H.HouseType].WorkAnim[I] do
        SubActAdd(Action,Cycles);

end;

//for not hard writen plan we need to check if its taking or producing anything
function TKMUnitWorkPlan.ValidPlan: Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to WARES_IN_OUT_COUNT - 1 do
  begin
    if Res[I].W <> wtNone then
      Exit(true);
    if Prod[I].W <> wtNone then
      Exit(true);
  end;
end;

function TKMUnitWorkPlan.FindDifferentResource(aUnit: TKMUnit; aLoc: TKMPoint; const aAvoidLoc: TKMPoint): Boolean;
var
  NewLoc: TKMPointDir;
  PlantAct: TKMPlantAct;
  Found: boolean;
  HW: TKMHouseWoodcutters;

  function GetFarmGrainType : TKMGrainFarmSet;
  begin
    Result[0] := gftNone;
    Result[1] := gftNone;
    Result[2] := gftNone;
    if aUnit.Home.HouseType = htFarm then
      Result := TKMHouseFarm(aUnit.Home).GrainTypes
    else
    if aUnit.Home.HouseType = htProductionThatch then
      Result := TKMHouseProdThatch(aUnit.Home).GrainTypes;
  end;

begin
  if (GatheringScript = gsWoodCutterCut) OR (GatheringScript = gsWoodCutterPlant) then
  begin
    HW := TKMHouseWoodcutters(aUnit.Home);
    HW.ValidateFlagPoint; //Validate Cutting point. It will be set to a valid one if needed.

    if HW.IsFlagPointSet then
      aLoc := HW.FlagPoint;
  end;
  GrainType := GetFarmGrainType;
  NewLoc.Dir := DirN;
  with gTerrain do
  case GatheringScript of
    gsClayMiner:       Found := FindClay(aLoc, aAvoidLoc, False, nil, NewLoc.Loc);
    gsStoneCutter:     Found := FindStone(aLoc, gRes.Units[aUnit.UnitType].MiningRange, aAvoidLoc, False, nil, NewLoc);
    gsFarmerSow:       Found := FindCornField(aLoc, gRes.Units[aUnit.UnitType].MiningRange, aAvoidLoc, taPlant, aUnit.Home, PlantAct, NewLoc, GrainType);
    gsFarmerCorn:      begin
                          Found := FindCornField(aLoc, gRes.Units[aUnit.UnitType].MiningRange, aAvoidLoc, taAny, aUnit.Home, PlantAct, NewLoc, GrainType);
                          if PlantAct = taPlant then
                          begin
                            GatheringScript := gsFarmerSow; //Switch to sowing corn rather than cutting
                            ActionWalkFrom  := uaWalkTool; //Carry our scythe back (without the corn) as the player saw us take it out
                            ActionWorkType  := uaWork1;
                            WorkCyc    := 10;
                            Prod[0].W  := wtNone; //Don't produce corn
                            Prod[0].C := 0;
                            Prod[1].W  := wtNone; //Don't produce seeds
                            Prod[1].C := 0;
                          end;
                        end;
    gsFarmerWine:      begin
                          Found := FindWineField(aLoc, gRes.Units[aUnit.UnitType].MiningRange, aAvoidLoc, nil, NewLoc);
                          NewLoc.Dir := dirN; //The animation for picking grapes is only defined for facing north
                        end;
    gsFisherCatch:     Found := FindFishWater(aLoc, gRes.Units[aUnit.UnitType].MiningRange, aAvoidLoc, False, NewLoc);
    gsWoodCutterCut:   Found := ChooseTree(aLoc, KMGetVertexTile(aAvoidLoc, WorkDir), gRes.Units[aUnit.UnitType].MiningRange, taCut, aUnit, NewLoc, PlantAct);
    gsWoodCutterPlant: Found := ChooseTree(aLoc, aAvoidLoc, gRes.Units[aUnit.UnitType].MiningRange, taPlant, aUnit, NewLoc, PlantAct);
    else                Found := False; //Can find a new resource for an unknown gathering script, so return with False
  end;

  if Found then
  begin
    Loc := NewLoc.Loc;
    WorkDir := NewLoc.Dir;
    Result := True;
  end
  else
    Result := False;
end;


function TKMUnitWorkPlan.ChooseTree(const aLoc, aAvoid: TKMPoint; aRadius: Integer; aPlantAct: TKMPlantAct; aUnit: TKMUnit; out Tree: TKMPointDir; out PlantAct: TKMPlantAct): Boolean;
var
  I: Integer;
  T: TKMPoint;
  treeList: TKMPointDirCenteredList;
  bestToPlant, secondBestToPlant: TKMPointCenteredList;
begin
  treeList := TKMPointDirCenteredList.Create(aLoc);
  bestToPlant := TKMPointCenteredList.Create(aLoc);
  secondBestToPlant := TKMPointCenteredList.Create(aLoc);

  gTerrain.FindTree(aLoc, aRadius, aAvoid, aPlantAct, treeList, bestToPlant, secondBestToPlant);

  //Convert taAny to either a Tree or a Spot
  if (aPlantAct in [taCut, taAny])
  and ((treeList.Count > 8) //Always chop the tree if there are many
       or (bestToPlant.Count + secondBestToPlant.Count = 0)
       or ((treeList.Count > 0) and (treeList.Count > ((bestToPlant.Count + secondBestToPlant.Count)/15)))
      ) then
  begin
    PlantAct := taCut;
    Result := treeList.GetWeightedRandom(Tree);
  end
  else
  begin
    PlantAct := taPlant;
    //First try stumps list
    for I := bestToPlant.Count - 1 downto 0 do
      if not TKMUnitCitizen(aUnit).CanWorkAt(bestToPlant[I], gsWoodCutterPlant) then
        bestToPlant.Delete(I);
    Result := bestToPlant.GetWeightedRandom(T);
    //Result := bestToPlant.GetClosest(aLoc, T);
    //Trees must always be planted facing north as that is the direction the animation uses
    if Result then
      Tree := KMPointDir(T, dirN)
    else
    begin
      //Try empty places list
      for I := secondBestToPlant.Count - 1 downto 0 do
        if not TKMUnitCitizen(aUnit).CanWorkAt(secondBestToPlant[I], gsWoodCutterPlant) then
          secondBestToPlant.Delete(I);
      //Result := secondBestToPlant.GetClosest(aLoc, T);
      Result := secondBestToPlant.GetWeightedRandom(T);
      //Trees must always be planted facing north as that is the direction the animation uses
      if Result then
        Tree := KMPointDir(T, dirN);
    end;
  end;

  treeList.Free;
  bestToPlant.Free;
  secondBestToPlant.Free;
end;

function TKMUnitWorkPlan.CanWork(aUnit : TKMUnit): Boolean;
var I : Integer;
  H : TKMHouse;
  hasRes, hasSpace : Boolean;
begin
  H := aUnit.Home;

  hasRes := true;
  //first check if it has enough resources
  if not H.DontNeedRes then
    for I := 0 to WARES_IN_OUT_COUNT - 1 do
      with Res[I] do
      begin
        if not ((W = wtNone) or (H.CheckWareIn(W) >= C)) then
          hasRes := false;
      end;

  hasSpace := true;
  //then check if it has enough space in output
  for I := 0 to WARES_IN_OUT_COUNT - 1 do
    with Prod[I] do
      if Prod[I].W <> wtNone then
        if (H.CheckWareOut(W) >= H.GetMaxOutWare) then
          hasSpace := false;

  Result := (hasSpace or H.ForceWorking) and (hasRes or H.DontNeedRes) and not ResourceDepleted and fIssued;
  Result := Result and ((H.GetHealth / H.MaxHealth) >= 0.5);
end;

procedure TKMUnitWorkPlan.FindPlan(aUnit: TKMUnit; aHome: TKMHouseType; aProduct: TKMWareType; aLoc: TKMPoint; aPlantAct: TKMPlantAct);
var
  I: Integer;
  tmp: TKMPointDir;
  tmp2: TKMPoint;
  plantAct: TKMPlantAct;
  HW: TKMHouseWoodcutters;
  srcHouse : TKMHouseSiegeWorkshop;
  isMiner, hardWritten : Boolean;
  tmpHouse : TKMHouse;
  treeID : Word;

  function GetFarmGrainType : TKMGrainFarmSet;
  begin
    Result[0] := gftNone;
    Result[1] := gftNone;
    Result[2] := gftNone;
    if aUnit.Home.HouseType = htFarm then
      Result := TKMHouseFarm(aUnit.Home).GrainTypes
    else
    if aUnit.Home.HouseType = htProductionThatch then
      Result := TKMHouseProdThatch(aUnit.Home).GrainTypes;
  end;
  function GetFarmCutGrainType : TKMGrainFarmSet;
  begin
    Result[0] := gftNone;
    Result[1] := gftNone;
    Result[2] := gftNone;
    if aUnit.Home is TKMHouseFarm then
      Result := TKMHouseFarm(aUnit.Home).CutGrainTypes
    else
    if aUnit.Home is TKMHouseProdThatch then
      Result := TKMHouseProdThatch(aUnit.Home).CutGrainTypes;
  end;
begin
  Clear;
  fIssued := false;
  fHome := aHome;
  AfterWorkIdle := gRes.Houses[aHome].WorkerRest * 10;

  //set gatrhering scripts for some houses

  GatheringScript := gRes.Houses[aHome].GatheringScript;
  //overwrite script
  if aUnit.UnitType = utCarpenter then
    if aProduct in [wtTimber, wtLog, wtWheel, wtWoodenShield, wtLance, wtAxe, wtBow, wtQuiver] then
      GatheringScript := gsCarpenter;
  tmpHouse := nil;
  if GatheringScript = gsNone then
    case aHome of
      htCoalMine : GatheringScript := gsCoalMiner;
      htGoldMine : GatheringScript := gsGoldMiner;
      htIronMine : GatheringScript := gsIronMiner;
      htBitinMine : GatheringScript := gsBitinMiner;
      htPottery : GatheringScript := gsClayMiner;
      htSwine : GatheringScript := gsSwineBreeder;
      htStables : GatheringScript := gsHorseBreeder;
      htQuarry : GatheringScript := gsStoneCutter;
      htFishermans : GatheringScript := gsFisherCatch;
      htHovel : GatheringScript := gsHovel;
      htMetallurgists : GatheringScript := gsMetallurgists;

      htFarm : begin
                GatheringScript := gsFarmerSow;
                tmpHouse := aUnit.Home;
              end;
      htVineyard : GatheringScript := gsFarmerWine;
      htIronSmithy : GatheringScript := gsIronSmithy;
      htWoodcutters : GatheringScript := gsWoodCutterPlant;
      htMerchant : GatheringScript := gsMerchant;
      htSiegeWorkshop : GatheringScript := gsSiegeCarpenter;
      htWoodBurner : GatheringScript := gsWoodBurner;
      htCollectors : begin
                      tmpHouse := aUnit.Home;
                      case TKMHouseCollectors(tmpHouse).Mode of
                        cmHunter : GatheringScript := gsHunter;
                        else GatheringScript := gsCollector;
                      end;
                     end;
      htAppleTree : GatheringScript := gsAppleTree;
      htShipyard :  GatheringScript := gsShipyard;
      htProductionThatch: begin
                            tmpHouse := aUnit.Home;
                            case aProduct of
                              wtStone: GatheringScript := gsStoneCutter;
                              wtTile: GatheringScript := gsClayMiner;
                              wtVegetables,
                              wtHay,
                              wtSeed,
                              wtCorn: GatheringScript := gsFarmerSow;
                              wtWine: GatheringScript := gsFarmerWine;
                            end;
                          end;
    end;

  tmp := KMPointDir(1, 1, dirNA);
  //check for resources to mine
  isMiner := true;
  tmp.Dir := dirN;
  case GatheringScript of
    gsCoalMiner : fIssued := gTerrain.FindOre(aLoc, wtCoal, tmp.Loc);
    gsGoldMiner : fIssued := gTerrain.FindOre(aLoc, wtGoldOre, tmp.Loc, aUnit.Home.IsMineShaft);
    gsIronMiner : fIssued := gTerrain.FindOre(aLoc, wtIronOre, tmp.Loc, aUnit.Home.IsMineShaft);
    gsBitinMiner : fIssued := gTerrain.FindOre(aLoc, wtBitinOre, tmp.Loc, aUnit.Home.IsMineShaft);
    gsClayMiner : fIssued := gTerrain.FindClay(aLoc, KMPOINT_ZERO, False, tmpHouse, tmp.Loc);
    //gsCollector : fIssued := gTerrain.FindJewerly(aLoc, KMPOINT_ZERO, False, tmp.Loc);
    gsStoneCutter : fIssued := gTerrain.FindStone(aLoc, gRes.Units[aUnit.UnitType].MiningRange, KMPOINT_ZERO, False, tmpHouse, tmp);
    gsFisherCatch : fIssued := gTerrain.FindFishWater(aLoc, gRes.Units[aUnit.UnitType].MiningRange, KMPOINT_ZERO, False, tmp);
    else isMiner := false;
  end;

  If GatheringScript = gsClayMiner then
    tmp.Dir := KamRandomDir('Clay miner random dir');

  //if Issued then mine something
  if isMiner then
    if fIssued then
    begin
      Loc := tmp.Loc;
      if aHome = htProductionThatch then
      begin
        DefaultPlan(aUnit, aProduct);
      end else
        DefaultPlan(aUnit);

      case GatheringScript of
        gsClayMiner : WalkStyle(tmp, uaWalk,uaWork,13,0,uaSpec,gsClayMiner);
        //gsCollector : WalkStyle(tmp, uaWalk,uaWork,8,0,uaWalkTool,gsCollector);
        gsStoneCutter : WalkStyle(tmp, uaWalk,uaWork,8,0,uaWalkTool,gsStoneCutter);
        gsFisherCatch : WalkStyle(tmp, uaWalk,uaWork2,8,0,uaWalkTool,gsFisherCatch);
      end;
      //if gTerrain.TileIsMineShaft(Loc) then
      //  ActSetByMultiplier(aUnit, 2.5);


    end else
    begin
      //try to find any ores
      ResourceDepleted := true;
      case GatheringScript of
        gsClayMiner : ResourceDepleted := not gTerrain.FindClay(aLoc, KMPOINT_ZERO, True, tmpHouse, tmp.loc);
        //gsCollector : ResourceDepleted := not gTerrain.FindJewerly(aLoc, KMPOINT_ZERO, True, tmp.loc);
        gsStoneCutter : ResourceDepleted := not gTerrain.FindStone(aLoc, gRes.Units[aUnit.UnitType].MiningRange, KMPOINT_ZERO, True, tmpHouse, tmp);
        gsFisherCatch : ResourceDepleted := not gTerrain.FindFishWater(aLoc, gRes.Units[aUnit.UnitType].MiningRange, KMPOINT_ZERO, True, tmp);
      end;


    end;

  hardWritten := false;

  case GatheringScript of
    //breeders doesn't need hay (hay is just optional)
    gsHorseBreeder,
    gsSwineBreeder:begin
                    hardWritten := true;
                    DefaultPlan(aUnit);
                    Res.Clear;
                    Res.AddWare(wtWater);
                    fIssued := true;
                    if aUnit.Home.CheckWareIn(wtCorn) > 0 then
                      Res.AddWare(wtCorn);
                    if aUnit.Home.CheckWareIn(wtHay) > 0 then
                      Res.AddWare(wtHay);
                    if aUnit.Home.CheckWareIn(wtVegetables) > 0 then
                      Res.AddWare(wtVegetables);

                    fIssued := Res.HasWares([wtWater]) and Res.HasAnyWares([wtCorn, wtHay, wtVegetables]);//corn or hay must be, vegetables are just additional

                   end;
    gsWoodCutterPlant,
    gsWoodCutterCut:    begin
                        hardWritten := true;
                        HW := TKMHouseWoodcutters(aUnit.Home);
                        HW.ValidateFlagPoint; //Validate Cutting point. It will be set to a valid one if needed.

                        if HW.IsFlagPointSet then
                          aLoc := HW.FlagPoint;

                        fIssued := ChooseTree(aLoc, KMPOINT_ZERO, gRes.Units[aUnit.UnitType].MiningRange, aPlantAct, aUnit, tmp, plantAct);
                        if fIssued then
                        begin
                          case plantAct of
                            taCut:    begin
                                        tmp2 := KMGetVertexTile(tmp.Loc, tmp.Dir);
                                        treeID := gTerrain.Land[tmp2.Y, tmp2.X].Obj;
                                        DefaultPlan(aUnit);
                                        WalkStyle(tmp, uaWalkBooty,uaWork,
                                                gMapElements[treeID].AxeHitTimes,
                                                20,uaWalkTool2,gsWoodCutterCut);
                                        //ClearPlan;
                                        Prod[0].C := gMapElements[treeID].TrunksCount;
                                      end;
                            taPlant:  WalkStyle(tmp, uaWalkTool,uaWork,12,0,uaWalk,gsWoodCutterPlant);
                            else      fIssued := False;
                          end;
                        end
                        else
                          case plantAct of
                            taCut:    if not gTerrain.CanFindTree(aLoc, gRes.Units[aUnit.UnitType].MiningRange) then
                                        ResourceDepleted := True; //No more trees to cut
                            taPlant:  if HW.WoodcutterMode = wmPlant then
                                        ResourceDepleted := True;   //No place for trees to plant
                          end;
                      end;
    gsCollector:      begin
                        hardWritten := true;

                        if TKMHouseCollectors(aUnit.Home).IsFlagPointSet then
                          aLoc := TKMHouseCollectors(aUnit.Home).FlagPoint;

                        Prod := gTerrain.FindCollectors(aLoc, KMPOINT_ZERO, False, tmp);
                        fIssued := not (Prod[0].W in [wtNone, wtAll]) and (aUnit.Home.CheckWareIn(Prod[0].W) < 5);
                        if fIssued then
                        begin
                          //ResourcePlan(wtNone, 0, wtNone, 0, Prod[0].W);
                          //Prod[0].C := 1;
                          Loc := tmp.Loc;
                          if gTerrain.GetObject(tmp.DirFaceLoc) <> OBJ_NONE then
                          begin
                            WalkStyle(tmp, uaWalk,uaWork,6,0,uaSpec,gsCollector);
                            SubActAdd(haWork3, 10);
                          end else
                            WalkStyle(tmp, uaWalk,uaWork,Prod[0].C,0,uaSpec,gsCollector);
                        end else
                          ResourceDepleted := gTerrain.FindCollectors(aLoc, tmp.Loc, True, tmp)[0].W = wtNone;

                      end;
    gsHunter:         begin
                        hardWritten := true;

                        if TKMHouseCollectors(aUnit.Home).IsFlagPointSet then
                          aLoc := TKMHouseCollectors(aUnit.Home).FlagPoint;

                        fIssued := gTerrain.FindHunter(aLoc, KMPOINT_ZERO, false, tmp);
                        if fIssued then
                        begin
                          Loc := tmp.Loc;
                          WalkStyle(tmp, uaWalk,uaWork,5,0,uaWalk,gsHunter);
                          if gTerrain.GetObject(tmp.Loc) = OBJ_NONE then
                          begin
                            if (aUnit.Home.CheckWareIn(wtLance) = 0) and not aUnit.Home.DontNeedRes then
                              fIssued := false
                            else
                              TMPInt := 1;
                          end;
                        end;{ else
                          ResourceDepleted := not gTerrain.FindHunter(aLoc, KMPOINT_ZERO, true, tmp);}

                      end;
    gsHovel:          begin
                        hardWritten := true;
                        DefaultPlan(aUnit);
                        Res.Clear;
                        Res.AddWare(wtWater);
                        if aUnit.Home.CheckWareIn(wtSeed) > 0 then
                          Res.AddWare(wtSeed);
                        if aUnit.Home.CheckWareIn(wtVegetables) > 0 then
                          Res.AddWare(wtVegetables);
                        if aUnit.Home.CheckWareIn(wtCorn) > 0 then
                          Res.AddWare(wtCorn);


                        {for I := 0 to High(Res) do
                        begin
                          if (Res[I].W in [wtSeed, wtVegetables, wtCorn]) and (aUnit.Home.CheckWareIn(Res[I].W) = 0) then
                            Res[I].W := wtNone;

                          if not TKMHouseHovel(aUnit.Home).CanFeedWithWare(Res[I].W) then
                            Res[I].W := wtNone;

                        end;}

                        fIssued := Res.HasWares([wtWater]) and Res.HasAnyWares([wtSeed, wtVegetables, wtCorn]);
                      end;
    gsAppleTree:      if aHome = htAppleTree then
                      begin
                        hardWritten := true;
                        TMPInt := TKMHouseAppleTree(aUnit.Home).CanWork(true, true);
                        fIssued := TMPInt > -1;
                        if fIssued then
                        begin
                          if TKMHouseAppleTree(aUnit.Home).NeedWater(TMPInt) then
                          begin
                            //tmpHouse := gHands[aUnit.Owner].GetClosestHouse(aUnit.Home.Position, [htWell], []);
                            tmpHouse := nil;
                            If (tmpHouse <> nil) and (KMLengthDiag(tmpHouse.Position, aUnit.Home.Position) <= 5) then //don't take water when well is nearby and has water in it
                            begin
                              //SubActAdd(haWork5, 5);
                              //tmpHouse.WareTakeFromOut(wtWater);//take water from nearest well
                              WorkHouse := tmpHouse;
                              tmp.Loc := tmpHouse.PointBelowEntrance;
                              tmp.Dir := DirN;
                              WalkStyle(tmp, uaWalk, uaWalk,0,1,uaWalk,gsAppleTree);
                              ResourcePlan(wtNone,0,wtNone, 0, wtNone)
                            end else
                            begin
                              //well not found
                              fIssued := false;
                              if aUnit.Home.CheckWareIn(wtWater) > 0 then //has water, use it
                              begin
                                fIssued := true;
                                ResourcePlan(wtWater,1,wtNone, 0, wtNone) ;
                                SubActAdd(haWork5, 6);//play clear animation
                              end else
                              begin
                                TMPInt := TKMHouseAppleTree(aUnit.Home).CanWork(true, false);//try to find any apple tree that doesn't need water
                                fIssued := TMPInt > -1;
                                if fIssued then
                                begin
                                  SubActAdd(haWork5, 6 * 20 div 10 + 1);//play clear animation
                                  ResourcePlan(wtNone,0,wtNone, 0, wtApple);
                                  Prod[0].C := 1;
                                end;
                              end;

                            end;

                          end else
                          begin
                            SubActAdd(haWork5, 6 * 20 div 10 + 1);//play clear animation
                            ResourcePlan(wtNone,1,wtNone, 0, wtApple);
                            Prod[0].C := 1;{gRes.Wares[wtApple].GetProductionCount(htAppleTree)}
                            fIssued := true;
                          end;

                          //fIssued := true;
                        end;
                      end;
    gsFarmerSow,
    gsFarmerCorn:
                     //if (aHome = htFarm) or (aHome = htProductionThatch) then
                      begin
                        hardWritten := true;
                        GrainType := GetFarmGrainType;
                        fIssued := gTerrain.FindCornField(aLoc, gRes.Units[aUnit.UnitType].MiningRange,
                                                          KMPOINT_ZERO, aPlantAct, tmpHouse, plantAct, tmp,
                                                          GetFarmCutGrainType);

                        

                        if fIssued then
                          case plantAct of
                            taCut:    begin
                                        ClearPlan;
                                        if gTerrain.GetGrainType(tmp.Loc) in GRAIN_GRAIN then
                                        begin
                                          if not ((aUnit.Home.IsValid(htFarm) and TFarm(aUnit.Home).HasGrain)
                                             or (aUnit.Home.IsValid(htProductionThatch) and TThatch(aUnit.Home).HasGrain)) then
                                            fIssued := false;
                                        end else
                                        if gTerrain.GetGrainType(tmp.Loc) in GRAIN_GRASS then
                                        begin
                                          if not ((aUnit.Home.IsValid(htFarm) and TFarm(aUnit.Home).HasGrass)
                                             or (aUnit.Home.IsValid(htProductionThatch) and TThatch(aUnit.Home).HasGrass)) then
                                            fIssued := false;
                                        end else
                                        if gTerrain.GetGrainType(tmp.Loc) in GRAIN_VEGE then
                                        begin
                                          if not ((aUnit.Home.IsValid(htFarm) and TFarm(aUnit.Home).HasVege)
                                             or (aUnit.Home.IsValid(htProductionThatch) and TThatch(aUnit.Home).HasVege)) then
                                            fIssued := false;
                                        end;

                                        if gTerrain.TileIsGrassField(tmp.Loc) and (gTerrain.Land^[tmp.Loc.Y, tmp.Loc.X].FieldAge >= CORN_AGE_MAX) then
                                          WalkStyle(KMPointDir(tmp.Loc, dirN), uaWalkTool2,uaWork2,6,0,uaWalkBooty2,gsFarmerCorn)
                                        else
                                        if gTerrain.TileIsVegeField(tmp.Loc) and (gTerrain.Land^[tmp.Loc.Y, tmp.Loc.X].FieldAge >= CORN_AGE_MAX) then
                                          WalkStyle(KMPointDir(tmp.Loc, dirN), uaWalkTool2,uaWork2,6,0,uaWalkBooty2,gsFarmerCorn)
                                        else
                                          WalkStyle(tmp, uaWalkTool,uaWork,10,0,uaWalkBooty,gsFarmerCorn);

                                        if aUnit.Home.HouseType = htProductionThatch then
                                        begin
                                          DefaultPlan(aUnit);
                                          ActSetByMultiplier(aUnit, 0.25);
                                        end;
                                      end;
                            taPlant:  WalkStyle(tmp, uaWalk,uaWork1,10,0,uaWalk,gsFarmerSow);
                            else      fIssued := False;
                          end;
                      end;

    gsFarmerWine:     begin
                        hardWritten := true;
                        fIssued := gTerrain.FindWineField(aLoc, gRes.Units[aUnit.UnitType].MiningRange, KMPOINT_ZERO, tmpHouse, tmp);
                        if aUnit.Home is TKMHouseVineyard then
                        begin
                          if TKMHouseVineyard(aUnit.Home).CanMakeWine(false) then
                          begin
                            //DefaultPlan(aUnit);
                            SubActAdd(haWork1,1);
                            SubActAdd(haWork2,7 + 5 * TKMHouseVineyard(aUnit.Home).WineToProduce);
                            SubActAdd(haWork5,1);
                            Prod[0].W := wtWine;
                            Prod[0].C := 1;
                            fIssued := true;
                          end else
                          if fIssued then
                          begin
                            //DefaultPlan(aUnit);
                            ClearPlan;

                            WalkStyle(KMPointDir(tmp.Loc,dirN), uaWalkTool2,uaWork2, 5,0,uaWalkBooty2,gsFarmerWine); //The animation for picking grapes is only defined for facing north
                          end else
                          if TKMHouseVineyard(aUnit.Home).CanMakeWine(true) then
                          begin
                            //DefaultPlan(aUnit);
                            SubActAdd(haWork1,1);
                            SubActAdd(haWork2,7 + 5 * TKMHouseVineyard(aUnit.Home).WineToProduce);
                            SubActAdd(haWork5,1);
                            Prod[0].W := wtWine;
                            Prod[0].C := 1;
                            fIssued := true;
                          end;
                        end else
                        begin
                          DefaultPlan(aUnit);
                          ActSetByMultiplier(aUnit, 0.3);
                          WalkStyle(KMPointDir(tmp.Loc,dirN), uaWalkTool2,uaWork2, 5,0,uaWalkBooty2,gsFarmerWine);
                        end;
                      end;

    gsSiegeCarpenter: begin
                        hardWritten := true;
                        srcHouse := TKMHouseSiegeWorkshop(aUnit.Home);

                        if srcHouse <> nil then
                        begin
                          if srcHouse.Queue[0] <> utNone then
                          begin
                            Res := srcHouse.GetNeededWares;
                            Res.SetCount(WARES_IN_OUT_COUNT);
                            SubActAdd(haWork1,1);
                            SubActAdd(haWork4,1);
                            SubActAdd(haWork2,SIEGE_CYCLES div 4);
                            SubActAdd(haWork4,1);
                            SubActAdd(haWork2,SIEGE_CYCLES div 4);
                            SubActAdd(haWork4,1);
                            SubActAdd(haWork2,SIEGE_CYCLES div 4);
                            SubActAdd(haWork4,1);
                            SubActAdd(haWork2,SIEGE_CYCLES div 4);
                            SubActAdd(haWork5,1);
                            fIssued := True;
                          end;
                        end;

                      end;

    gsMerchant:      begin
                      hardWritten := true;
                      fIssued := TKMHouseMerchant(aUnit.Home).CanWork;
                      if fIssued then
                      begin
                        tmp.loc := TKMHouseMerchant(aUnit.Home).AllyStore.PointBelowEntrance;

                        WalkStyle(tmp, uaWalk,uaWalk,0,50,uaWalk,gsMerchant);

                        for I := 1 to WARES_IN_OUT_COUNT do
                        begin
                          Res[I-1].W := aUnit.Home.WareInput[I];
                          Res[I-1].C := aUnit.Home.CheckWareIn(Res[I-1].W);
                        end;

                      end;

                     end;

    gsIronSmithy:     begin
                        hardWritten := true;

                        if (aUnit.Home.CheckWareIn(wtBitinOre) > 0) and (KamRandom(101, 'TKMUnitWorkPlan.FindPlan:TakeBitin') >= 50) then
                          ResourcePlan(wtBitinOre,1,wtCoal,1,wtBitin)
                        else
                          ResourcePlan(wtIronOre,1,wtCoal,1,wtIron);

                        for I := 0 to 3 do
                        begin
                          SubActAdd(haWork2,1);
                          SubActAdd(haWork3,1);
                        end;

                        SubActAdd(haWork2,1);
                        SubActAdd(haWork3,0.25);

                        fIssued := True;
                      end;

    gsWoodBurner:     begin
                        hardWritten := true;
                        fIssued := TKMHouseWoodBurner(aUnit.Home).GetFreeSlot <> -1;
                        if fIssued then
                        begin
                          GatheringScript := gsWoodBurner;
                          case TKMHouseWoodBurner(aUnit.Home).GetFreeSlot of
                            3 : If aUnit.Home.CheckWareIn(wtTrunk) >= 2 then  ResourcePlan(wtTrunk,2,wtNone,0,wtNone) else fIssued := false;
                            4 : If aUnit.Home.CheckWareIn(wtTrunk) >= 3 then  ResourcePlan(wtTrunk,3,wtNone,0,wtNone) else fIssued := false;
                            5 : If aUnit.Home.CheckWareIn(wtTrunk) >= 4 then ResourcePlan(wtTrunk,4,wtNone,0,wtNone) else fIssued := false;
                            else ResourcePlan(wtTrunk,1,wtNone,0,wtNone);
                          end;
                          if  fIssued then
                          begin
                            SubActAdd(haWork1,4);
                            SubActAdd(haWork2,2);
                            SubActAdd(haWork5,2);
                          end;
                        end;
                      end;
    gsMetallurgists:  begin
                          hardWritten := true;
                          if (gHands[aUnit.Owner].CanMakeJewerly
                          and (aUnit.Home.CheckWareIn(wtJewerly) < 5))
                          and (aUnit.Home.WareOrder[2] > 0)
                          and (aProduct = wtJewerly) then
                            ResourcePlan(wtCoal,1,wtNone,0,wtJewerly)
                          else
                            ResourcePlan(wtGoldOre,1,wtCoal,1,wtGold);

                          for I := 0 to 3 do
                          begin
                            SubActAdd(haWork2,1);
                            SubActAdd(haWork3,1);
                            SubActAdd(haWork4,1);
                          end;
                          SubActAdd(haWork2,1);

                          fIssued := True;
                      end;
    gsShipyard:       begin
                        hardWritten := true;
                        DefaultPlan(aUnit);
                        Res.Clear;
                        Res.CopyFrom(TShipYard(aUnit.Home).GetWarePlan);
                        Res.SetCount(WARES_IN_OUT_COUNT);
                        fIssued := TKMHouseShipYard(aUnit.Home).CanWork;
                      end;
  else
    hardWritten := false;
  end;


  if aUnit.Home is TKMHouseProdThatch then
    if GatheringScript in [gsStoneCutter, gsClayMiner, gsFarmerSow, gsFarmerCorn, gsFarmerWine] then
      TKMHouseProdThatch(aUnit.Home).TakePoint(Loc);

  if not hardWritten and not isMiner then
  begin
    if gRes.Houses[aHome].DoesOrders then
      DefaultPlan(aUnit, aProduct)
    else
      DefaultPlan(aUnit);

    fIssued := ValidPlan;
  end;
  WorkCyc := Max(Round(ifThen(aUnit.BootsAdded, WorkCyc * 0.75, WorkCyc)), 1);



  {if IsMiner and fIssued then
    if aHome is htProductionThatch then
        if GatheringScript in [gsStoneCutter, gsClayMiner, gsFarmerSow, gsFarmerCorn, gsFarmerWine] then
          TKMHouseProdThatch(aUnit.Home).TakePoint(Loc);}

  ActSetByMultiplier(aUnit);
  if gGame.Resource.SkipWater then
    for I := 0 to High(Res) do
      if Res[I].W = wtWater then
      begin
        Res[I].W := wtNone;
        Res[I].C := 0;
      end;
  If aUnit.BootsAdded then
    ActSetByMultiplier(aUnit, 0.75);

  if gHands[aUnit.Owner].IsAffectedbyMBD then
    if gGame.Params.MBD.IsEasy then
      ActSetByMultiplier(aUnit, 0.75)
    else
    if gGame.Params.MBD.IsHardOrRealism then
      ActSetByMultiplier(aUnit, 1.25);

  if aUnit.Home.HouseType = htProductionThatch then
  begin
    case aProduct of
      wtLog, wtWheel, wtSawDust : ActSetByMultiplier(aUnit, 0.65);
      wtTimber : ActSetByMultiplier(aUnit, 0.45);
      wtGold, wtIron, wtBitin, wtSteelE, wtBitinE : ActSetByMultiplier(aUnit, 0.80);
      wtBread, wtFlour : ActSetByMultiplier(aUnit, 0.70);
    end;

  end;
end;


procedure TKMUnitWorkPlan.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.CheckMarker('WorkPlan');
  LoadStream.Read(fHome, SizeOf(fHome));
  LoadStream.Read(fIssued);
//public
  LoadStream.Read(HasToWalk);
  LoadStream.Read(Loc);
  LoadStream.Read(ActionWalkTo, SizeOf(ActionWalkTo));
  LoadStream.Read(ActionWorkType, SizeOf(ActionWorkType));
  LoadStream.Read(WorkCyc);
  LoadStream.Read(WorkDir);
  LoadStream.Read(GatheringScript, SizeOf(GatheringScript));
  LoadStream.Read(AfterWorkDelay);
  LoadStream.Read(ActionWalkFrom, SizeOf(ActionWalkFrom));

  LoadStream.Read(ActCount);
  for I := 0 to ActCount - 1 do //Write only assigned
  begin
    LoadStream.Read(HouseAct[I].Act, SizeOf(HouseAct[I].Act));
    LoadStream.Read(HouseAct[I].TimeToWork);
  end;
  Res.SetCount(4);
  Prod.SetCount(4);
  for I := 0 to 3 do //Write only assigned
    begin
      LoadStream.Read(Res[I].W, SizeOf(Res[I].W));
      LoadStream.Read(Res[I].C);

      LoadStream.Read(Prod[I].W, SizeOf(Prod[I].W));
      LoadStream.Read(Prod[I].C);
    end;
  LoadStream.Read(AfterWorkIdle);
  LoadStream.Read(ResourceDepleted);
  LoadStream.Read(TMPInt);
  LoadStream.Read(WorkHouse, 4);
  LoadStream.Read(GrainType, SizeOf(GrainType));
end;


procedure TKMUnitWorkPlan.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('WorkPlan');
  SaveStream.Write(fHome, SizeOf(fHome));
  SaveStream.Write(fIssued);
//public
  SaveStream.Write(HasToWalk);
  SaveStream.Write(Loc);
  SaveStream.Write(ActionWalkTo, SizeOf(ActionWalkTo));
  SaveStream.Write(ActionWorkType, SizeOf(ActionWorkType));
  SaveStream.Write(WorkCyc);
  SaveStream.Write(WorkDir);
  SaveStream.Write(GatheringScript, SizeOf(GatheringScript));
  SaveStream.Write(AfterWorkDelay);
  SaveStream.Write(ActionWalkFrom, SizeOf(ActionWalkFrom));
  SaveStream.Write(ActCount);
  for I := 0 to ActCount - 1 do //Write only assigned
  begin
    SaveStream.Write(HouseAct[I].Act, SizeOf(HouseAct[I].Act));
    SaveStream.Write(HouseAct[I].TimeToWork);
  end;

  for I := 0 to 3 do //Write only assigned
  begin
    SaveStream.Write(Res[I].W, SizeOf(Res[I].W));
    SaveStream.Write(Res[I].C);
    SaveStream.Write(Prod[I].W, SizeOf(Prod[I].W));
    SaveStream.Write(Prod[I].C);
  end;
  SaveStream.Write(AfterWorkIdle);
  SaveStream.Write(ResourceDepleted);
  SaveStream.Write(TMPInt);
  SaveStream.Write(TKMHouse(WorkHouse).UID);
  SaveStream.Write(GrainType, SizeOf(GrainType));
end;

procedure TKMUnitWorkPlan.SyncLoad;
begin
  WorkHouse := gHands.GetHouseByUID(Integer(WorkHouse));
end;


end.
