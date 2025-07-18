unit KM_UnitTaskGoToWell;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses, KM_HousePearl;


{Throw a rock}
type
  TKMTaskGoToWell = class(TKMUnitTask)
  private
    fWell : TKMHouseWell;
    fHasWater : Boolean;
  public
    constructor Create(aUnit: TKMUnit; aWell : TKMHouse);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    property Well : TKMHouseWell read fWell;
    procedure Save(SaveStream: TKMemoryStream); override;

    function WalkShouldAbandon : Boolean; override;
    function CouldBeCancelled: Boolean; override;
  end;

  TKMTaskPearlRally = class(TKMUnitTask)
  private
    fPearl : TKMHousePearl;
  public
    constructor Create(aUnit: TKMUnit; aPearl : TKMHousePearl);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    property Pearl : TKMHousePearl read fPearl;
    procedure Save(SaveStream: TKMemoryStream); override;

    function WalkShouldAbandon : Boolean; override;
    function CouldBeCancelled: Boolean; override;
  end;


implementation
uses
  KM_Terrain,
  KM_CommonUtils,
  KM_HandsCollection, KM_Entity, KM_HandTypes, KM_HandEntity,
  KM_UnitWarrior,
  KM_Points,
  KM_ResTypes,
  KM_Projectiles, KM_CommonGameTypes;


{ TKMTaskCollectWares }
constructor TKMTaskGoToWell.Create(aUnit: TKMUnit; aWell : TKMHouse);
begin
  inherited Create(aUnit);
  fType := uttGoToWell;
  fWell := TKMHouseWell(aWell.GetPointer);
  fHasWater := false;
end;


destructor TKMTaskGoToWell.Destroy;
begin
  gHands.CleanUpHousePointer(TKMHouse(fWell));
  inherited;
end;


constructor TKMTaskGoToWell.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fWell, 4);
  LoadStream.Read(fHasWater);
end;


procedure TKMTaskGoToWell.SyncLoad;
begin
  inherited;

  fWell := TKMHouseWell(gHands.GetHouseByUID(Integer(fWell)))
end;

function TKMTaskGoToWell.CouldBeCancelled : Boolean;
begin
  Result := WalkShouldAbandon or (fPhase < 1);

end;
function TKMTaskGoToWell.WalkShouldAbandon: Boolean;
begin
  Result := not fWell.IsValid(htWell, false, true);
end;

function TKMTaskGoToWell.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Thought := thBucket;
            SetActionGoIn(uaWalk, gdGoOutside, Home);
            Home.SetState(hstEmpty);
          end;
      1:  SetActionWalkToSpot(fWell.Entrance, uaWalk, 1);
      //2:  SetActionGoIn(uaWalk, gdGoInside, fWell);//enter the well
      2:  if fWell.CheckWareOut(wtWater) > 0 then
          begin
            SetActionLockedStay(20, uaWalk); //take water
            fWell.DecProgress(150);
            fWell.WareTakeFromOut(wtWater, 1, true);
            fHasWater := true;
          end else
          begin
            SetActionLockedStay(150, uaWalk); // wait a little, maybe water will be produced soon
            {Inc(fPhase2);
            if fPhase2 < 5 then //wait only 5 times, then go back to home with nothing
              fPhase := 2;}
            fWell.DecProgress(150);
            fHasWater := true;
          end;
      //go back to house
      {3:  begin
            //SetActionGoIn(uaWalk, gdGoOutside, fWell);
            SetActionLockedStay(0, uaWalk); // wait a little, maybe water will be produced soon
            Thought := thNone;
          end;}
      3: SetActionWalkToSpot(Home.PointBelowEntrance);
      4: SetActionGoIn(uaWalk, gdGoInside, Home);
      //unit back at home
      5:  begin
            SetActionLockedStay(20, uaWalk);
            if fHasWater then
              Home.WareAddToIn(wtWater, 1, true);
            Home.SetState(hstIdle);
          end
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskGoToWell.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fWell.UID);
  SaveStream.Write(fHasWater);
end;



{ TKMTaskPearlRally }
constructor TKMTaskPearlRally.Create(aUnit: TKMUnit; aPearl : TKMHousePearl);
begin
  inherited Create(aUnit);
  fType := uttPearlRally;
  fPearl := TKMHousePearl(aPearl.GetPointer);
end;


destructor TKMTaskPearlRally.Destroy;
begin
  gHands.CleanUpHousePointer(TKMHouse(fPearl));
  inherited;
end;


constructor TKMTaskPearlRally.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fPearl, 4);
end;


procedure TKMTaskPearlRally.SyncLoad;
begin
  inherited;

  fPearl := TKMHousePearl(gHands.GetHouseByUID(Integer(fPearl)))
end;

function TKMTaskPearlRally.CouldBeCancelled : Boolean;
begin
  Result := WalkShouldAbandon or (fPhase < 1);

end;
function TKMTaskPearlRally.WalkShouldAbandon: Boolean;
begin
  Result := (fPearl = nil) or fPearl.IsDestroyed {fPearl.IsValid(htPearl, false, true)}{ or not fPearl.DoRally} or fUnit.IsHungry;
end;

function TKMTaskPearlRally.Execute: TKMTaskResult;
var target : TKMUnit;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            SetActionWalkToSpot(fPearl.GetClosestEntrance(Position).DirFaceLoc);
            SetSpeed(10, true);
            Thought := thImportant
          end;
      1:  SetActionGoIn(uaWalk, gdGoInside, fPearl);//enter the well
      2:  SetActionLockedStay(30, uaWalk); //prepare before attacking
      3:  begin
            If fPearl.DoRally then
            begin
              target := gTerrain.UnitsHitTestWithinRad(fPearl.Entrance,
                                            0,
                                            13,
                                            Owner, atEnemy, dirNA, not RANDOM_TARGETS, False);
              If (target = nil) or target.IsDeadOrDying then
              else
                gProjectiles.AimTarget(fPearl.RalenderGetClosestTower(target.PositionF), target, ptTowerBolt, fUnit, 15.99, 0);
              fPhase := 1;
              SetActionLockedStay(30, uaWalk);
            end else
              SetActionLockedStay(0, uaWalk);
          end;

      //go back to your duties
      4:begin
          SetActionGoIn(uaWalk, gdGoOutside, fPearl);
          SetSpeed(-10, true);
          Thought := thNone;
        end;
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskPearlRally.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(fPearl.UID);
end;

end.
