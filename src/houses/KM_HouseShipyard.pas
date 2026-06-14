unit KM_HouseShipyard;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_Defaults,
  KM_CommonClasses,
  KM_Points,
  KM_Houses,
  KM_ResTypes;

type
  TKMHouseShipyard = class(TKMHouse)
    private
      fWayOutID : Byte;
      fShipType, fNextShipType : TKMUnitType;
      fShipSketchPosition : TKMPointDir;
      fShipPhase : Byte;
      fShipBuiltOf : TKMWarePlan;
      fDoWork : Boolean;
      fWaresOut : TKMWarePlan;
      function GetPhasesCount : Byte;
      procedure SetNextShipType(aValue : TKMUnitType);overload;
    protected
       procedure Activate(aWasBuilt: Boolean); override;
    public
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;


      procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1); override;
      function CheckWareOut(aWare: TKMWareType): Word; override;
      procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
      function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;

      property ShipType : TKMUnitType read fShipType;
      property NextShipType : TKMUnitType read fNextShipType write SetNextShipType;
      property DoWork : Boolean read fDoWork write fDoWork;
      procedure SetNextShipType(aAmount : Integer);overload;
      property WaresOut : TKMWarePlan read fWaresOut;

      procedure IncSketchPhase(aWares : TKMWarePlan);
      procedure StartWorking;
      function CanWork : Boolean;

      function GetWarePlan : TKMWarePlan;


      procedure Paint; Override;
  end;

implementation
uses
  KM_CommonUtils,
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_Units, KM_UnitWarrior,
  KM_Resource, KM_ResUnits,
  KM_RenderPool,
  KM_ScriptingEvents,
  KM_Terrain;


Constructor TKMHouseShipyard.Load(LoadStream: TKMemoryStream);
begin
  Inherited;

  LoadStream.Read(fShipType, SizeOf(fShipType));
  LoadStream.Read(fShipSketchPosition);
  LoadStream.Read(fShipPhase);
  LoadStream.Read(fDoWork);
  LoadStream.Read(fNextShipType, SizeOf(fNextShipType));
  LoadStream.Read(fWayOutID);
  fWaresOut.Load(LoadStream);
  fShipBuiltOf.Load(LoadStream);
end;

function TKMHouseShipyard.GetPhasesCount : Byte;
begin
  Result := 0;
  case fShipType of
    utBoat: Result := gRes.Units.FishermansShipSketch[fShipSketchPosition.Dir].Count;
    utBattleShip: Result := gRes.Units.BattleShipSketch[fShipSketchPosition.Dir].Count;
    utShip : Result := gRes.Units.ShipSketch[fShipSketchPosition.Dir].Count;
  end;
end;

procedure TKMHouseShipyard.SetNextShipType(aValue: TKMUnitType);
begin
  if gHands[Owner].Locks.UnitUnlocked(aValue, htShipYard) then
    fNextShipType := aValue;

  if (fShipPhase = 0) and (GetState <> hstWork) then
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;
end;

procedure TKMHouseShipyard.Activate(aWasBuilt: Boolean);
  function CheckWater(aX, aY : Integer) : Boolean;
  begin
    Result := false;
    if gTerrain.TileInMapCoords(aX, aY, 1) then
      Result := gTerrain.CheckPassability(aX, aY, tpFish);
  end;
var I : integer;

begin
  Inherited;
  fShipType := SHIPYARD_ORDER[0];
  for I := 0 to High(SHIPYARD_ORDER) do
    if gHands[Owner].Locks.UnitUnlocked(SHIPYARD_ORDER[I], HouseType) then
    begin
      fShipType := SHIPYARD_ORDER[I];
      Break;
    end;


  fNextShipType := fShipType;
  fShipPhase := 0;
  fShipSketchPosition.Dir := dirN;

  if CheckWater(Entrance.X - 1, Entrance.Y + 4) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X - 1, Entrance.Y + 4);
    fShipSketchPosition.Dir := dirS;
    fWayOutID := 1
  end
  else
  if CheckWater(Entrance.X + 2, Entrance.Y + 4) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X + 2, Entrance.Y + 4);
    fShipSketchPosition.Dir := dirS;
    fWayOutID := 2
  end
  else
  if CheckWater(Entrance.X + 4, Entrance.Y + 2) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X + 4, Entrance.Y + 2);
    fShipSketchPosition.Dir := dirE;
    fWayOutID := 3
  end
  else
  if CheckWater(Entrance.X - 3, Entrance.Y + 2) then
  begin
    fShipSketchPosition.Loc := KMPoint(Entrance.X - 3, Entrance.Y + 2);
    fShipSketchPosition.Dir := dirW;
    fWayOutID := 4
  end else
    fWayOutID := 0;

  Assert(fWayOutID <> 0, 'TKMHouseShipyard did not found water')
end;

procedure TKMHouseShipyard.Save(SaveStream: TKMemoryStream);
begin
  Inherited;

  SaveStream.Write(fShipType, SizeOf(fShipType));
  SaveStream.Write(fShipSketchPosition);
  SaveStream.Write(fShipPhase);
  SaveStream.Write(fDoWork);
  SaveStream.Write(fNextShipType, SizeOf(fNextShipType));
  SaveStream.Write(fWayOutID);
  fWaresOut.Save(SaveStream);
  fShipBuiltOf.Save(SaveStream);
end;

procedure TKMHouseShipyard.IncSketchPhase(aWares : TKMWarePlan);
  procedure FinishShip;
    var U : TKMUnitWarrior;
  begin

    //gTerrain.UnlockTile(fShipSketchPosition.Loc);
    U := gHands[Owner].AddUnitGroup(fShipType, fShipSketchPosition.Loc, fShipSketchPosition.Dir, 1, 1).FlagBearer;


    if gHands[Owner].IsComputer then
    begin
      U.BoltCount := 100;
      U.InfinityAmmo := true;
      if fShipType = utBattleShip then
        WareTakeFromIn(wtBolt, CheckWareIn(wtBolt), true)
      else
      if fShipType = utBoat then
        WareTakeFromIn(wtAxe, CheckWareIn(wtAxe), true);
    end;

    if fShipBuiltOf.HasWare(wtBitinE) > 0 then
    begin
      U.Defence := U.Defence + (fShipBuiltOf.HasWare(wtBitinE) div 2);
      U.ProjectilesDefence := U.ProjectilesDefence + (fShipBuiltOf.HasWare(wtBitinE) / 2)
    end;
    if fShipBuiltOf.HasWare(wtSteelE) > 5 then
      U.SetSpeed(fShipBuiltOf.HasWare(wtSteelE) - 5, true);

    if fShipBuiltOf.HasWare(wtSteelE) > 0 then
      U.HitPointsMax := U.HitPointsMax + (fShipBuiltOf.HasWare(wtSteelE) div 2);

    U.HitPointsChangeFromScript(U.HitPointsMax);
    U.Condition := UNIT_MAX_CONDITION;
    //fShipType := utNone;
    fShipPhase := 0;
    //fShipSketchPosition.Loc := KMPOINT_INVALID_TILE;
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;
    fShipBuiltOf.Clear;
    gScriptEvents.ProcWarriorEquipped(U, U.Group);
  end;
var count : Integer;
begin
  Inc(fShipPhase);
  count := GetPhasesCount;

  if aWares.HasWare(wtBitinE) > 0 then
    fShipBuiltOf.AddWare(wtBitinE);
  if aWares.HasWare(wtSteelE) > 0 then
    fShipBuiltOf.AddWare(wtSteelE);

  if ((fShipType = utBoat) and (fShipPhase > 2))
    or ((fShipType = utShip) and (fShipPhase > 4))
    or ((fShipType = utBattleShip) and (fShipPhase > 5))
   then
  if aWares.HasWare(wtLeather) > 0 then
    fShipBuiltOf.AddWare(wtLeather);



  if fShipPhase >= count then
  begin
    FinishShip;
  end else
  if fShipPhase = 1 then
  begin
    //fShipSketchPosition.Dir := KaMRandomDir('TKMHouseShipyard.Activate');
    //fShipSketchPosition.Loc := gTerrain.FindPlaceForUnit(Position, tpFish, 5);
    //gTerrain.SetTileLock(fShipSketchPosition.Loc, tlRoadWork);
  end;


end;

procedure TKMHouseShipyard.StartWorking;
begin
    //gTerrain.SetTileLock(fShipSketchPosition.Loc, tlRoadWork);
end;

procedure TKMHouseShipyard.SetNextShipType(aAmount: Integer);
var I, Index: Integer;
  procedure SelectNext;
  begin
    IncLoop(Index, low(SHIPYARD_ORDER), high(SHIPYARD_ORDER), aAmount);
    fNextShipType := SHIPYARD_ORDER[Index];
  end;

begin
  Index := -1;
  for I := 0 to High(SHIPYARD_ORDER) do
    If SHIPYARD_ORDER[I] = fNextShipType then
    begin
      Index := I;
      Break;
    end;
  if Index = -1 then
    Exit;
  SelectNext;
  while not gHands[Owner].Locks.UnitUnlocked(fNextShipType, HouseType) do
    SelectNext;

  if (fShipPhase = 0) and (GetState <> hstWork) then
    if fNextShipType <> fShipType then
      fShipType := fNextShipType;


end;

function TKMHouseShipyard.CanWork: Boolean;
begin
  Result := gTerrain.CheckPassability(fShipSketchPosition.Loc, tpFish) or (fShipPhase > 0);
  Result := Result and (gTerrain.GetUnit(fShipSketchPosition.Loc) = nil);
  Result := Result and (fDoWork or (fShipPhase > 0));
end;

function TKMHouseShipyard.GetWarePlan: TKMWarePlan;
begin
  Result.Reset;
  case fShipType of
    utBoat :  begin
                Result.AddWare(wtLog);
                Result.AddWare(wtSkin);

                if CheckWareIn(wtSteelE) > 0 then
                  Result.AddWare(wtSteelE);
              end;
    utShip :  begin
                Result.AddWare(wtLog);
                Result.AddWare(wtSkin);

                if CheckWareIn(wtSteelE) > 0 then
                  Result.AddWare(wtSteelE);
                if CheckWareIn(wtBitinE) > 0 then
                  Result.AddWare(wtBitinE);
              end;
    utBattleShip :  begin
                      Result.AddWare(wtLog);
                      Result.AddWare(wtSkin);

                      if CheckWareIn(wtSteelE) > 0 then
                        Result.AddWare(wtSteelE);
                      if CheckWareIn(wtBitinE) > 0 then
                        Result.AddWare(wtBitinE);


                    end;
  end;

end;

procedure TKMHouseShipyard.WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1);
begin
  fWaresOut.AddWare(aWare, aCount);
  gHands[Owner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
end;

function TKMHouseShipyard.CheckWareOut(aWare: TKMWareType): Word;
begin
  Result := fWaresOut.HasWare(aWare);
end;

procedure TKMHouseShipyard.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var index : Integer;
begin
  index := fWaresOut.IndexOf(aWare, 1);
  Assert(index >= 0);
  {if index = -1 then
    Exit;}

  if aFromScript then
  begin
    aCount := Min(aCount, fWaresOut[index].C);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;

  Assert(aCount <= fWaresOut[index].C);

  fWaresOut[index].C := fWaresOut[index].C - aCount;

end;

function TKMHouseShipyard.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWaresOut.Count - 1 do
    if (fWaresOut[I].W = aWare) and (fWaresOut[I].C > 0) then
      Result := fWaresOut[I].C >= aCount;

end;

procedure TKMHouseShipyard.Paint;
  function SketchPointF : TKMPointF;
  begin
    Result.X := fShipSketchPosition.Loc.X + 0.5;
    Result.Y := fShipSketchPosition.Loc.Y + 0.5;
  end;
begin
  if not IsComplete then
    Inherited
  else
  begin

    //Incase we need to render house at desired step in debug mode
    if HOUSE_BUILDING_STEP = 0 then
    begin
      //Shipyard is Painted as seperated parts
      {if fIsOnSnow then
        gRenderPool.AddHouse(fType, fPosition, 1, 1, fSnowStep, -1, GetStonePic, GetSnowPic, false, false, 0)
      else
        gRenderPool.AddHouse(fType, fPosition, 1, 1, 0, -1, GetStonePic, -1, false, false, 0);}

      If fIsOnTerrain <> tptNone then
      begin
        If fSnowStep = 1 then
          gRenderPool.AddSpriteGSnow(fPosition, KMPOINT_ZERO, GetSnowPic + 1, 1, rxHouses, gHands[Owner].FlagColor)
        else
        begin
          gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2538, rxHouses, gHands[Owner].FlagColor);
          gRenderPool.AddSpriteGSnow(fPosition, KMPOINT_ZERO, GetSnowPic + 1, fSnowStep, rxHouses, gHands[Owner].FlagColor);
        end;
      end else
      gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2538, rxHouses, gHands[Owner].FlagColor);//house

      gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2534, rxHouses, gHands[Owner].FlagColor);//yard
      case fWayOutID of
        1: begin
            gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
            gRenderPool.AddSpriteG(fPosition, KMPoint(-102, 18), 2540, rxHouses, gHands[Owner].FlagColor);//bottom Right
           end;
        2: begin
            gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            gRenderPool.AddSpriteG(fPosition, KMPoint(0, 18), 2540, rxHouses, gHands[Owner].FlagColor);//bottom Right
           end;
        3:  begin
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2536, rxHouses, gHands[Owner].FlagColor);//right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            end;
        4:  begin
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2537, rxHouses, gHands[Owner].FlagColor);//left
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2539, rxHouses, gHands[Owner].FlagColor);//bottom Right
              gRenderPool.AddSpriteG(fPosition, KMPOINT_ZERO, 2535, rxHouses, gHands[Owner].FlagColor);//bottom Left
            end;
      end;


      if CurrentAction <> nil then
        if PaintHouseWork then
        gRenderPool.AddHouseWork(HouseType, fPosition, CurrentAction.SubAction, WorkAnimStep, WorkAnimStepPrev, GetFlagColor);

    end
    else
      gRenderPool.AddHouse(HouseType, fPosition,
        Min(HOUSE_BUILDING_STEP * 3, 1),
        EnsureRange(HOUSE_BUILDING_STEP * 3 - 1, 0, 1),
        Max(HOUSE_BUILDING_STEP * 3 - 2, 0)
        ,GetWoodPic, GetStonePic, GetSnowPic
        );
  end;

  if fShipPhase > 0 then
  begin
    case fShipType of
      utBoat : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.FishermansShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
      utBattleShip : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.BattleShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
      utShip : gRenderPool.AddAnimation( SketchPointF,
                                        gRes.Units.ShipSketch[fShipSketchPosition.Dir],
                                        fShipPhase - 1,
                                        0, rxUnits
                                       );
    end;

  end;

end;


end.
