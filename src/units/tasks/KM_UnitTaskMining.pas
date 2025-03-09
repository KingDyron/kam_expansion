unit KM_UnitTaskMining;
{$I KaM_Remake.inc}
interface
uses
  Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Units, KM_UnitWorkplan, KM_TerrainTypes,
  KM_ResTypes;


type
  // Resource mining task
  TKMTaskMining = class(TKMUnitTask)
  private
    fObjectType : Word;
    fBeastID: Byte;
    fWorkPlan: TKMUnitWorkPlan;
    fDistantResAcquired: Boolean; //Was distant resorce acquired ? (stone / fish / wood / corn / wine)
    fGrainType : TKMGrainType;
    function ResourceExists: Boolean;
    function ResourceTileIsLocked: Boolean;
    function ChooseToCutOrPlant: TKMPlantAct;
    procedure FindAnotherWorkPlan;
  public
    constructor Create(aUnit: TKMUnit; aWare: TKMWareType);
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    function GetActivityText: UnicodeString;
    property WorkPlan: TKMUnitWorkPlan read fWorkPlan;
    function Execute: TKMTaskResult; override;
    function CouldBeCancelled: Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;
  end;


implementation
uses
  KM_CommonUtils,
  KM_Game,
  KM_Houses, KM_HouseWoodcutters, KM_HouseSwineStable, KM_HouseSiegeWorkshop, KM_HouseWoodBurner, KM_HouseQueue,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_MapEditor, KM_MapEdTypes,
  KM_Resource, KM_ResMapElements, KM_ResTexts, KM_Log, KM_ResTileset, KM_ResTilesetTypes,
  KM_Hand, KM_ResUnits, KM_ScriptingEvents, KM_Terrain;


{ TTaskMining }
constructor TKMTaskMining.Create(aUnit: TKMUnit; aWare: TKMWareType);
var J : Byte;
begin
  inherited Create(aUnit);

  fType := uttMining;
  fWorkPlan := TKMUnitWorkPlan.Create;
  fDistantResAcquired := False;
  fBeastID  := 0;
  fObjectType := 0;
  fWorkPlan.FindPlan( fUnit,
                      fUnit.Home.HouseType,
                      aWare,
                      aUnit.Home.PointBelowEntrance,
                      ChooseToCutOrPlant
                      );
  fGrainType := gftNone;
  if fWorkPlan.IsIssued then
    if aUnit.Home.HSpec.DoesOrders then
      If fWorkPlan.ValidPlan then
        with aUnit.Home do
        begin
          J := GetWareOutIndex(aWare);
          if J > 0 then
            if WareOrder[J] <> MAX_WARES_ORDER then
              WareOrder[J] := WareOrder[J] - 1;
        end;

end;


destructor TKMTaskMining.Destroy;
begin
  // Make sure we don't abandon and leave our house with "working" animations
  if (fUnit <> nil)
    and not fUnit.Home.IsDestroyed
    and (fUnit.Home.GetState = hstWork) then
    fUnit.Home.SetState(hstIdle);

  FreeAndNil(fWorkPlan);

  inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TKMTaskMining.WalkShouldAbandon: Boolean;
begin
  Result := False;
  //Assert(fUnit is TKMUnitCitizen);
  if fPhase = 2 then //Unit is walking to mine-position
    Result := ResourceTileIsLocked or //If someone takes our place
              not ResourceExists or //Resource has gone
              not TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, WorkPlan.GatheringScript);
end;


//Chose if we don't care or prefer specific activity
//depending on orders or clogged output
function TKMTaskMining.ChooseToCutOrPlant: TKMPlantAct;
begin
  Result := taAny;

  case fUnit.Home.HouseType of
    htWoodcutters:  case TKMHouseWoodcutters(fUnit.Home).WoodcutterMode of
                      wmChop:         Result := taCut;
                      wmPlant:        Result := taPlant;
                      wmChopAndPlant: if (fUnit.Home.CheckWareOut(wtTrunk) >= fUnit.Home.GetMaxOutWare) and not fUnit.Home.ForceWorking then
                                        Result := taPlant
                                      else
                                        Result := taAny;
                    end;
    htFarm:         begin
                      case TKMHouseFarm(fUnit.Home).Mode of
                        wmChop:         Result := taCut;
                        wmPlant:        Result := taPlant;
                        wmChopAndPlant: Result := taAny;
                      end;
                      if Result = taAny then
                        if not fUnit.Home.ForceWorking then
                          if not (TKMHouseFarm(fUnit.Home).HasGrain
                          or TKMHouseFarm(fUnit.Home).HasGrass
                          or TKMHouseFarm(fUnit.Home).HasVege) then //if doesn't show these grain types than all we can do is plant
                            Result := taPlant;
                    end;

    htProductionThatch: if (not fUnit.Home.HasSpaceForWaresOut([wtCorn, wtSeed, wtVegetables, wtHay], true)) and not fUnit.Home.ForceWorking then
                          Result := taPlant
                        else
                          Result := taAny;
    else Result := taAny; //We don't care since other housetypes don't have concurent activities
  end;
end;


constructor TKMTaskMining.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskMining');
  fWorkPlan := TKMUnitWorkPlan.Create;
  fWorkPlan.Load(LoadStream);
  LoadStream.Read(fDistantResAcquired);
  LoadStream.Read(fBeastID);
  LoadStream.Read(fObjectType);
  LoadStream.Read(fGrainType, SizeOf(fGrainType));
end;


function TKMTaskMining.GetActivityText: UnicodeString;
begin
  case WorkPlan.GatheringScript of
    gsStoneCutter:     Result := gResTexts[TX_UNIT_TASK_STONE];
    gsFarmerSow:       Result := gResTexts[TX_UNIT_TASK_SOW_CORN];
    gsFarmerCorn:      Result := gResTexts[TX_UNIT_TASK_CUTTING_CORN];
    gsFarmerWine:      Result := gResTexts[TX_UNIT_TASK_GRAPES];
    gsFisherCatch:     Result := gResTexts[TX_UNIT_TASK_FISHING];
    gsWoodCutterCut:   Result := gResTexts[TX_UNIT_TASK_CUT_TREE];
    gsWoodCutterPlant: Result := gResTexts[TX_UNIT_TASK_PLANT_TREE];
    else                Result := 'Unknown';
  end;
end;


//Try to find alternative target for our WorkPlan
//Happens when we discover that resource is gone or is occupied by another busy unit
//Return False if new plan could not be found
procedure TKMTaskMining.FindAnotherWorkPlan;
var OldLoc: TKMPoint; OldDir: TKMDirection;
begin
  OldLoc := WorkPlan.Loc;
  OldDir := WorkPlan.WorkDir;

  //Tell the work plan to find a new resource of the same gathering script
  if WorkPlan.FindDifferentResource(fUnit, fUnit.Home.PointBelowEntrance, OldLoc) then
  begin
    //Must always give us a new location (or same location but different direction)
    Assert((OldDir <> WorkPlan.WorkDir) or not KMSamePoint(OldLoc, WorkPlan.Loc));
    fPhase := 0; //Set the walk again (Will become 1 after this loop)
    fUnit.SetActionLockedStay(0, WorkPlan.ActionWalkTo);
  end else
  begin
    fPhase := 99; //Abandon as there is no other work plan available (Exit the task on next update)
    fUnit.SetActionLockedStay(0, WorkPlan.ActionWalkTo);
  end;
end;


function TKMTaskMining.ResourceTileIsLocked: Boolean;
var
  P: TKMPoint;
begin
  if WorkPlan.GatheringScript = gsWoodCutterCut then
  begin
    P := KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir);
    // Check all tiles around the tree, same as we do in TKMTerrain.FindTree
    Result := not gTerrain.CanCutTreeAtVertex(fUnit.PositionNext, P);
  end
  else
  if WorkPlan.GatheringScript = gsMerchant then
  begin
    Result := not gTerrain.CheckPassability(WorkPlan.Loc.X, WorkPlan.Loc.Y, tpWalk);
  end
  else
  if WorkPlan.GatheringScript = gsHunter then
  begin
    Result := not gTerrain.CheckPassability(WorkPlan.Loc.X, WorkPlan.Loc.Y, tpWalk);
    Result := Result and not ArrayContains(gTerrain.GetObject(WorkPlan.Loc), [OBJ_NONE, 540, 541, 542, 543, 544, 545, 546, 547]);
    Result := Result and (gTerrain.GetUnit(WorkPlan.Loc).UnitType = utClayPicker);
  end
  else
    Result := gTerrain.TileIsLocked(WorkPlan.Loc) and (WorkPlan.WorkHouse = nil);
end;


function TKMTaskMining.ResourceExists: Boolean;
var P: TKMPoint;
  stage : Byte;
begin
  with gTerrain do
  case WorkPlan.GatheringScript of
    gsClayMiner:       Result := TileHasClay(WorkPlan.Loc.X, WorkPlan.Loc.Y); //Check stone deposit above Loc, which is walkable tile
    gsHunter :         Result := CanSetTrap(WorkPlan.Loc) or HasMeat(WorkPlan.Loc);
    gsStoneCutter:     Result := TileHasStone(WorkPlan.Loc.X, WorkPlan.Loc.Y-1); //Check stone deposit above Loc, which is walkable tile
    gsFarmerSow:       Result := (TileIsCornField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0))
                                 or (TileIsGrassField(WorkPlan.Loc) and InRange(Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge, 0, CORN_AGE_1 - 1))
                                 or (TileIsVegeField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0));
    gsFarmerCorn:      begin
                          stage := gFieldGrains[GetGrainType(WorkPlan.Loc)].GetStage(Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge);

                          Result := (stage <> 254) and ((stage >= CORN_AGE_MAX) or gFieldGrains[GetGrainType(WorkPlan.Loc)].Stage[stage].CanBeCut);
                          {Result := (TileIsCornField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = CORN_AGE_MAX))
                                    or (TileIsGrassField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge in [CORN_AGE_MAX, CORN_AGE_3]));}

                          if Result then exit; //Resource still exists so exit
                          //If corn has been cut we can possibly plant new corn here to save time
                          Result := (TileIsCornField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge = 0));
                          if Result then
                            with WorkPlan do
                            begin
                              GatheringScript := gsFarmerSow; //Switch to sowing corn rather than cutting
                              ActionWalkFrom  := uaWalkTool; //Carry our scythe back (without the corn) as the player saw us take it out
                              ActionWorkType  := uaWork1;
                              WorkCyc    := 10;
                              Prod[0].W   := wtNone; //Don't produce corn
                              Prod[0].C := 0;
                            end;
                        end;
    gsFarmerWine:      Result := TileIsWineField(WorkPlan.Loc) and (Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X].FieldAge >= CORN_AGE_MAX);
    gsFisherCatch:     Result := CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir),True);
    gsWoodCutterPlant: Result := TileGoodToPlantTree(WorkPlan.Loc.X, WorkPlan.Loc.Y);
    gsWoodCutterCut:   begin
                          P := KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir);
                          Result := ObjectIsChopableTree(P, caAgeFull);
                        end;
    else                Result := True;
  end;
end;


function TKMTaskMining.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
              in [1, 7]; //Allow cancel task only at walking phases
end;


{This is execution of Resource mining}
function TKMTaskMining.Execute: TKMTaskResult;
  procedure CalculateWorkingTime(aHouse : TKMHouse);
  var I : Integer;
  begin
    aHouse.WorkingTime := 0;
    for I := 0 to High(WorkPlan.HouseAct) do
      if I < fPhase2 then
        Inc(aHouse.WorkingTime, WorkPlan.HouseAct[I].TimeToWork);
  end;

const
  // Shortcuts to skip certain Phases
  SkipWalk = 9;
  SKIP_WORK = 11 + MAX_WORKPLAN; //Skip to certain Phases
var
  D: TKMDirection;
  TimeToWork, StillFrame: Integer;
  actWalkFrom: TKMUnitActionType;
  ResAcquired, hasRes: Boolean;
  addPercentage, I, K, L, tmp : integer;
begin
  Result := trTaskContinues;
  addPercentage := 0;
  //there's no point in doing a task if we can't return home
  if (fUnit.Home <> nil) and fUnit.Home.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  if fDistantResAcquired then
    actWalkFrom := WorkPlan.ActionWalkFrom
  else
    actWalkFrom := WorkPlan.ActionWalkTo;

  with fUnit do
  case fPhase of
    0:  if WorkPlan.HasToWalk then
        begin
          if WorkPlan.GatheringScript = gsCollector then
            case WorkPlan.WorkDir of
              dirN : fObjectType := gTerrain.Land^[WorkPlan.Loc.Y - 1, WorkPlan.Loc.X].Obj;
              dirS : fObjectType := gTerrain.Land^[WorkPlan.Loc.Y + 1, WorkPlan.Loc.X].Obj;
              dirE : fObjectType := gTerrain.Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X + 1].Obj;
              dirW : fObjectType := gTerrain.Land^[WorkPlan.Loc.Y, WorkPlan.Loc.X - 1].Obj;
            end;


          fDistantResAcquired := False; // we will set distant resource as acquired when we gather it
          Home.SetState(hstEmpty);
          SetActionGoIn(WorkPlan.ActionWalkTo, gdGoOutside, Home); //Walk outside the house

          //Woodcutter takes his axe with him when going to chop trees
          if (WorkPlan.GatheringScript = gsWoodCutterCut) then
            Home.CurrentAction.SubActionRem([haFlagpole])
          else
          if (WorkPlan.GatheringScript = gsMerchant)then
          begin

            for I := 0 to WorkPlan.Res.Count - 1 do
            begin
              if WorkPlan.Res[I].W <> wtNone then
              begin
                Home.WareTakeFromIn(WorkPlan.Res[I].W, WorkPlan.Res[I].C);
                Inc(addPercentage, WorkPlan.Res[I].C);
              end;

              gHands[fUnit.Owner].Stats.WareConsumed(WorkPlan.Res[I].W, WorkPlan.Res[I].C);
            end;
            fUnit.SetSpeed(-(addPercentage div 3), true);
          end;
          If (WorkPlan.GatheringScript in [gsCollector, gsHunter]) then
          begin
            fUnit.Condition := fUnit.Condition - 300;

            if fUnit.Condition < UNIT_MAX_CONDITION - 600 then
              if Home.CheckWareIn(wtVegetables) > 0 then
              begin
                Home.ProduceWare(wtVegetables, -1);
                fUnit.Condition := fUnit.Condition + 600;
              end;
          end;
          If (WorkPlan.GatheringScript in [gsHunter]) and (WorkPlan.TMPInt = 1) then
          begin
              if (Home.CheckWareIn(wtLance) > 0) and not Home.DontNeedRes then
                Home.ProduceWare(wtLance, -1)
              else
                Result := trTaskDone;
          end;

        end
        else
        begin
          fDistantResAcquired := True; // there is no distant resource, so its acquired by default
          fPhase := SkipWalk; //Skip walking part if there's no need in it, e.g. CoalMiner or Baker
          SetActionLockedStay(0, uaWalk);
          Exit;
        end;

    1:  //We cannot assume that the walk is still valid because the terrain could have changed while we were walking out of the house.
        SetActionWalkToSpot(WorkPlan.Loc, WorkPlan.ActionWalkTo);

    2: //Check if we are at the location. WalkTo could have failed or resource could have been exhausted
       if not KMSamePoint(PositionNext, WorkPlan.Loc) or not ResourceExists or
          not TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, WorkPlan.GatheringScript) then
         FindAnotherWorkPlan
       else
         SetActionLockedStay(0, WorkPlan.ActionWalkTo);

    3: //Before work tasks for specific mining jobs
       if WorkPlan.GatheringScript = gsAppleTree then
       begin
         SetActionGoIn(actWalkFrom, gdGoInside, TKMHouse(WorkPlan.WorkHouse));
         Inc(TKMHouseWell(WorkPlan.WorkHouse).WorkersInside);
       end else
       if WorkPlan.GatheringScript = gsFisherCatch then
       begin
         Direction := WorkPlan.WorkDir;
         SetActionLockedStay(13, uaWork1, False); //Throw the line out
       end else
         SetActionLockedStay(0, WorkPlan.ActionWalkTo);

    4: //Choose direction and time to work
       if WorkPlan.GatheringScript = gsAppleTree then
       begin
         if TKMHouse(WorkPlan.WorkHouse).CheckWareOut(wtWater) <= 0 then
          SetActionLockedStay(10 * 3 * (TKMHouseWell(WorkPlan.WorkHouse).WorkersInside + 1), uaWalk, False)
         else
          SetActionLockedStay(5, uaWalk, False);

         if TKMHouse(WorkPlan.WorkHouse).CheckWareOut(wtWater) > 0 then
            TKMHouse(WorkPlan.WorkHouse).WareTakeFromOut(wtWater, 1, true);
       end else
       begin
         if WorkPlan.WorkDir <> dirNA then
           Direction := WorkPlan.WorkDir;

         if gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count < 1 then
           for D := dirN to dirNW do
             if gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, D].Count > 1 then
             begin
               Direction := D;
               Break;
             end;

         TimeToWork := WorkPlan.WorkCyc * Math.max(gRes.Units[UnitType].UnitAnim[WorkPlan.ActionWorkType, Direction].Count, 1);


         if (workPlan.GatheringScript = gsCollector)
         and (WorkPlan.ActionWorkType = uaWork) then
         begin
          Inc(fPhase2);
          if fObjectType = OBJ_NONE then
            fPhase2 := 8;
           if not ArrayContains(fObjectType, [3, 4, 8, 9, 278, 279]) and (fPhase2 in [1]) then
            Inc(fPhase2);

           case fPhase2 of
            1, 3, 5, 6, 8 : SetActionLockedStay(TimeToWork, WorkPlan.ActionWorkType, False);
            2, 4, 7 : SetActionLockedStay(TimeToWork, uaWork1, False);

           end;
            fPhase := 3;

            if ((fPhase2 >= 8) and ArrayContains(fObjectType, [3, 4, 8, 9, 278, 279]))
            or ((fPhase2 >= 2) and not ArrayContains(fObjectType, [3, 4, 8, 9, 278, 279])) then
            begin
              fPhase2 := 0;
              fPhase := 4;
            end;

         end else
          SetActionLockedStay(TimeToWork, WorkPlan.ActionWorkType, False);


       end;
    5: //After work tasks for specific mining jobs
       if WorkPlan.GatheringScript = gsAppleTree then
       begin
         SetActionGoIn(WorkPlan.ActionWalkTo, gdGoOutside, TKMHouse(WorkPlan.WorkHouse));
         Dec(TKMHouseWell(WorkPlan.WorkHouse).WorkersInside);
       end else
       case WorkPlan.GatheringScript of
         gsWoodCutterCut:  SetActionLockedStay(10, WorkPlan.ActionWorkType, True, 5, 5); //Wait for the tree to start falling down
         gsFisherCatch:    SetActionLockedStay(15, uaWork, False); //Pull the line in
         else              SetActionLockedStay(0, WorkPlan.ActionWorkType);
       end;
    6: begin
         StillFrame := 0;
         case WorkPlan.GatheringScript of //Perform special tasks if required
           gsClayMiner:        fDistantResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wtTile);
           gsCollector:        begin
                                fDistantResAcquired := gTerrain.DecCollectorsOre(KMPointDir(WorkPlan.Loc, WorkPlan.WorkDir), WorkPlan.Prod[0].C);
                               end;
           gsHunter:           begin
                                fDistantResAcquired := false;
                                fObjectType := gTerrain.SetHunterTraps(WorkPlan.Loc);
                                if fObjectType <> OBJ_NONE then
                                begin
                                  fDistantResAcquired := true;
                                  WorkPlan.ActionWalkFrom := uaSpec;
                                end;

                               end;
           gsStoneCutter:      fDistantResAcquired := gTerrain.DecStoneDeposit(KMPoint(WorkPlan.Loc.X,WorkPlan.Loc.Y-1));
           gsFarmerSow:         gTerrain.SowCorn(WorkPlan.Loc, WorkPlan.GrainType, gHands[fUnit.Owner].VirtualWareTake('vtManure'));

           gsFarmerCorn:       begin
                                  fGrainType := gTerrain.GetGrainType(WorkPlan.Loc);
                                  fDistantResAcquired := gTerrain.CutCorn(WorkPlan.Loc);
                               end;
           gsFarmerWine:       begin
                                fDistantResAcquired := gTerrain.CutGrapes(WorkPlan.Loc);
                                fGrainType := gTerrain.GetGrainType(WorkPlan.Loc);
                               end;
           gsFisherCatch:      begin
                                  fDistantResAcquired := gTerrain.CatchFish(KMPointDir(WorkPlan.Loc,WorkPlan.WorkDir));
                                  if fDistantResAcquired then
                                    WorkPlan.ActionWorkType := uaWalkTool;
                                end;
           gsWoodCutterPlant:  //If the player placed a house plan here while we were digging don't place the
                                //tree so the house plan isn't canceled. This is actually the same as TSK/TPR IIRC
                                if TKMUnitCitizen(fUnit).CanWorkAt(WorkPlan.Loc, gsWoodCutterPlant) then
                                begin
                                  gTerrain.SetObject(WorkPlan.Loc, gTerrain.ChooseTreeToPlant(WorkPlan.Loc));
                                  fDistantResAcquired := True;
                                end;
           gsWoodCutterCut:    begin
                                  fDistantResAcquired := gTerrain.FallTree(KMGetVertexTile(WorkPlan.Loc, WorkPlan.WorkDir));
                                  StillFrame := 5;
                                end;
           gsAppleTree : fDistantResAcquired := true;

         end;

         if Home is TKMHouseProdThatch then
          TKMHouseProdThatch(Home).ClearPoints;
            {if WorkPlan.GatheringScript in [gsStoneCutter, gsClayMiner, gsFarmerSow, gsFarmerCorn, gsFarmerWine] then
              TKMHouseProdThatch(Home).RemovePoint(WorkPlan.Loc);}

         SetActionLockedStay(WorkPlan.AfterWorkDelay, WorkPlan.ActionWorkType, True, StillFrame, StillFrame);
       end;
    7: begin
          //give resources from mercahnt
          if WorkPlan.GatheringScript = gsMerchant then
            begin
              TKMHouseMerchant(Home).SendToAlly(WorkPlan.Res);

              fUnit.SetSpeed(24);
            end;
         //Removing the tree and putting a stump is handled in gTerrain.UpdateState from FallingTrees list
         SetActionWalkToSpot(Home.PointBelowEntrance, actWalkFrom); //Go home
         Thought := thHome;
       end;
    8: SetActionGoIn(actWalkFrom, gdGoInside, Home); //Go inside

    {Unit back at home and can process its booty now}
    9:    begin
            Thought := thNone;
            fPhase2 := 0;
            Home.SetState(hstWork);
            if not (WorkPlan.GatheringScript in [gsMerchant]) then
            begin
              hasRes := true;
              for I := 0 to high(WorkPlan.Res) do
                if WorkPlan.Res[I].W <> wtNone then
                  if Home.CheckWareIn(WorkPlan.Res[I].W) < WorkPlan.Res[I].C then
                    hasRes := false;

              if not hasRes then
                Result := trTaskDone
              else
              begin
                for I := 0 to high(WorkPlan.Res) do
                begin
                  if WorkPlan.Res[I].W <> wtNone then
                    Home.WareTakeFromIn(WorkPlan.Res[I].W, WorkPlan.Res[I].C);

                  gHands[fUnit.Owner].Stats.WareConsumed(WorkPlan.Res[I].W, WorkPlan.Res[I].C);
                end;
                CalculateWorkingTime(Home);
                if Home is TKMHouseProdThatch then
                begin
                  for I := 0 to high(WorkPlan.Prod)  do
                    TKMHouseProdThatch(Home).ProduceStarts(WorkPlan.Prod[I].W);
                end;

              end;
            end;

            //clay miner brought back clay
            if (WorkPlan.GatheringScript = gsClayMiner) and (WorkPlan.TMPInt = 2) then
              If Home.HouseType = htPottery then
                TKMHousePottery(Home).BringTile
              else
              If Home.HouseType = htProductionThatch then
                TKMHouseProdThatch(Home).BringTile;


            if WorkPlan.GatheringScript = gsShipyard then
              if TShipYard(fUnit.Home).CanWork then
                TShipYard(fUnit.Home).StartWorking
              else
                Exit(trTaskDone);

            if WorkPlan.GatheringScript = gsAppleTree then
              TKMHouseAppleTree(fUnit.Home).SetAnimation(WorkPlan.TMPInt);

            if Home is TKMHouseQueue then
              TKMHouseQueue(Home).InProgress := true;
            //Take required resources
            if Home.HouseType = htMetallurgists then
              if WorkPlan.Prod[0].W = wtJewerly then
                gHands[Owner].TakeJewerly;

            Home.CurrentAction.SubActionAdd([haSmoke]);
            if WorkPlan.GatheringScript = gsSwineBreeder then
            begin //Swines get feed and taken immediately
              fBeastID := TKMHouseSwineStable(Home).FeedBeasts(fWorkplan.Res);

              K := TKMHouseSwineStable(Home).GetPigsCount;//how many wtPig are produced

              for I := 0 to high(fWorkPlan.Prod) do
                If fWorkPlan.Prod[I].W = wtPig then
                  fWorkPlan.Prod[I].C := K;

              K := TKMHouseSwineStable(Home).TakeBeast;//how many pigs are taken
              for I := 0 to high(fWorkPlan.Prod) do
                If (fWorkPlan.Prod[I].W <> wtNone) and (fWorkPlan.Prod[I].W <> wtPig) then
                  fWorkPlan.Prod[I].C := fWorkPlan.Prod[I].C * K;

            end else
            if WorkPlan.GatheringScript = gsHovel then
              fBeastID := TKMHouseHovel(Home).FeedChicken(fWorkPlan.Res);

            if fDistantResAcquired and (fPhase2 < WorkPlan.ActCount) then
            begin
              Home.CurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
              //Keep unit idling till next Phase, Idle time is -1 to compensate TaskExecution Phase
              SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork - 1, uaWalk);
              Home.WorkingTime := 0;
            end
            else
            begin
              fPhase := SKIP_WORK; //Skip to work complete
              SetActionLockedStay(0, uaWalk);
              Exit;
            end;
          end;
    10..10 + MAX_WORKPLAN:
          begin
            Inc(fPhase2);

            //Feed a horse/pig
            if (WorkPlan.GatheringScript = gsHorseBreeder) and (fPhase2 = 1) then
            begin
              fBeastID := TKMHouseSwineStable(Home).FeedBeasts(fWorkPlan.Res);

            end;


            //Keep on working
            if fDistantResAcquired and (fPhase2 < WorkPlan.ActCount) then
            begin
              Home.CurrentAction.SubActionWork(WorkPlan.HouseAct[fPhase2].Act);
              if fPhase < WorkPlan.ActCount then
                SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork - 1, uaWalk) //-1 to compensate units UpdateState run
              else
                SetActionLockedStay(WorkPlan.HouseAct[fPhase2].TimeToWork - 2, uaWalk); //-2 to compensate 2 UpdateStates of a unit in last Act

              CalculateWorkingTime(Home);
            end
            else
            begin
              fPhase := SKIP_WORK; //Skip to step 31
              SetActionLockedStay(0, uaWalk);
              Exit;
            end;
          end;
    11 + MAX_WORKPLAN:
          begin
            if WorkPlan.GatheringScript = gsHorseBreeder then
            begin
              K := TKMHouseSwineStable(Home).TakeBeast; //Take the horse after feeding
              for I := 0 to high(fWorkPlan.Prod) do
                If fWorkPlan.Prod[I].W <> wtNone then
                  fWorkPlan.Prod[I].C := fWorkPlan.Prod[I].C * K;
            end;

            if Home.HouseType = htSiegeWorkshop then
              TKMHouseSiegeWorkshop(Home).UnitTrainingComplete;
            ResAcquired := false;
            case WorkPlan.GatheringScript of
              gsFarmerCorn : begin
                                ResAcquired := fDistantResAcquired;
                                if ResAcquired then
                                begin
                                  if Home is TKMHouseFarm then
                                    TKMHouseFarm(Home).GrainCut(fGrainType)
                                  else
                                  if Home is TKMHouseProdThatch then
                                    TKMHouseProdThatch(Home).GrainCut(fGrainType);
                                end;
                                ResAcquired := false;
                             end;

              gsFarmerWine : begin
                                ResAcquired := fDistantResAcquired;
                                if fGrainType = gftNone then
                                begin
                                  if Home is TKMHouseVineyard then
                                    TKMHouseVineyard(Home).ProduceWine;
                                end else
                                if ResAcquired then
                                begin
                                  if Home is TKMHouseVineyard then
                                    TKMHouseVineyard(Home).GrainCut(fGrainType)
                                  else
                                  if Home is TKMHouseProdThatch then
                                    TKMHouseProdThatch(Home).GrainCut(fGrainType);
                                end;
                                ResAcquired := false;
                             end;
              gsWoodCutterCut,
              gsStoneCutter,
              gsFisherCatch:  ResAcquired := fDistantResAcquired; // fResAcquired was set earlier when we tried to get resource
              gsAppleTree:    begin
                                ResAcquired := TKMHouseAppleTree(Home).IncGrowPhase(WorkPlan.TMPInt);
                                if ResAcquired then
                                  TKMHouseAppleTree(Home).MakeFruits(WorkPlan.TMPInt);
                                ResAcquired := false;
                              end;
              gsCoalMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wtCoal);
              gsGoldMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wtGoldOre);
              gsIronMiner:    ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wtIronOre);
              gsBitinMiner:   ResAcquired := gTerrain.DecOreDeposit(WorkPlan.Loc, wtBitinOre);
              gsClayMiner: begin

                              //ResAcquired := fDistantResAcquired;
                              ResAcquired := fWorkPlan.TMPInt = 0;//produces clay
                              if ResAcquired then
                                if fUnit.Home is TKMHousePottery then
                                  ResAcquired := TKMHousePottery(Home).TakeTile > 0
                                else
                                if fUnit.Home is TKMHouseProdThatch then
                                  ResAcquired := TKMHouseProdThatch(Home).TakeTile > 0;

                                //takes stored clay and puts it in the form
                              If fWorkPlan.TMPInt = 1 then
                                if fUnit.Home is TKMHousePottery then
                                  TKMHousePottery(Home).UseStoredClay
                                else
                                if fUnit.Home is TKMHouseProdThatch then
                                  TKMHouseProdThatch(Home).UseStoredClay

                           end;

              gsSwineBreeder: ResAcquired := fBeastID <> 0;
              gsHorseBreeder: ResAcquired := fBeastID <> 0;
              gsWoodBurner:   TKMHouseWoodBurner(Home).StartBurning;
              gsHovel:        begin
                                if fBeastID > 0 then
                                begin

                                  Home.ProduceWare(wtSausage, TKMHouseHovel(Home).TakeChicken(fBeastID));
                                end;
                                TKMHouseHovel(Home).MakeFeathers;
                                //Home.ProduceWare(wtFeathers, TKMHouseHovel(Home).ChickenCount);
                                ResAcquired := false;
                              end;
              gsCollector : begin
                              ResAcquired := true;
                              if (fObjectType <> 0) and (fObjectType <> 255) then
                                If ArrayContains(fObjectType, [540, 541, 542, 543, 544, 545, 546, 547, 548]) then
                                  TKMHouseCollectors(fUnit.Home).FillMeat(fObjectType)
                                else
                                if length(gMapElements[fObjectType].VWares) > 0 then
                                begin
                                  gHands[fUnit.Owner].AddJewerly(fObjectType);
                                  ResAcquired := false;
                                end;
                            end;
              gsHunter:     begin
                              ResAcquired := false;
                              TKMHouseCollectors(fUnit.Home).FillMeat(fObjectType);
                            end;
              gsShipyard:   TKMHouseShipyard(fUnit.Home).IncSketchPhase(fWorkPlan.Res);

              gsMerchant:     ResAcquired := false;
              else            ResAcquired := True;
            end;

            if ResAcquired then
            begin
              for I := 0 to high(WorkPlan.Prod) do
                if WorkPlan.Prod[I].W <> wtNone then
                begin
                  tmp := home.CheckWareIn(WorkPlan.Prod[I].W);
                  if (not Home.ForceWorking)
                    or (Home.CheckWareOut(WorkPlan.Prod[I].W) < Home.GetMaxOutWare)
                    or gRes.Houses[Home.HouseType].CanOverFill then
                  begin
                    Home.ProduceWare(WorkPlan.Prod[I].W, WorkPlan.Prod[I].C);
                    if WorkPlan.Prod[I].C > 0 then
                      Home.IncProductionCycle(WorkPlan.Prod[I].W);
                  end;

                if WorkPlan.Prod[I].C > 0 then
                  if WorkPlan.GatheringScript = gsCarpenter then
                    if Home.GetWareOutIndex(WorkPlan.Prod[I].W) > 0 then
                      if Home.ProductionCycle[Home.GetWareOutIndex(WorkPlan.Prod[I].W)] mod 8 = 7 then //12% for getting SawDust
                        Home.ProduceWare(wtSawDust)


                end;

            end;

              Home.SetState(hstIdle);
              SetActionLockedStay(WorkPlan.AfterWorkIdle-1, uaWalk);

              if Home is TKMHouseQueue then
              begin
                TKMHouseQueue(Home).RemWareFromQueue(0);
                TKMHouseQueue(Home).InProgress := false;
              end;

            end;

    else  Result := trTaskDone;
  end;

  Inc(fPhase);
end;


procedure TKMTaskMining.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskMining');
  fWorkPlan.Save(SaveStream);
  SaveStream.Write(fDistantResAcquired);
  SaveStream.Write(fBeastID);
  SaveStream.Write(fObjectType);
  SaveStream.Write(fGrainType, SizeOf(fGrainType));
end;

procedure TKMTaskMining.SyncLoad;
begin
  Inherited;
  fWorkPlan.SyncLoad;
end;

end.
