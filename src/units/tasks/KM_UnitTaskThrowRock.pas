unit KM_UnitTaskThrowRock;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Units;


{Throw a rock}
type
  TKMTaskThrowRock = class(TKMUnitTask)
  private
    fTarget: TKMUnit;
    fFlightTime: Word; //Thats how long it will take a stone to hit it's target
  public
    constructor Create(aUnit, aTarget: TKMUnit);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity, KM_Houses,

  KM_Projectiles, KM_Points, KM_CommonUtils,
  KM_CommonGameTypes, KM_ResTypes;


{ TTaskThrowRock }
constructor TKMTaskThrowRock.Create(aUnit, aTarget: TKMUnit);
begin
  inherited Create(aUnit);
  fType := uttThrowRock;
  fTarget := aTarget.GetPointer;
end;


destructor TKMTaskThrowRock.Destroy;
begin
  if (fUnit <> nil)
    and not fUnit.Home.IsDestroyed
    and (fUnit.Home.GetState = hstWork) then
      fUnit.Home.SetState(hstIdle, false); //Make sure we don't abandon and leave our tower with "working" animations

  gHands.CleanUpUnitPointer(fTarget);
  inherited;
end;


constructor TKMTaskThrowRock.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskThrowRock');
  LoadStream.Read(fTarget, 4);
  LoadStream.Read(fFlightTime);
end;


procedure TKMTaskThrowRock.SyncLoad;
begin
  inherited;
  fTarget := gHands.GetUnitByUID(Integer(fTarget));
end;


function TKMTaskThrowRock.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  //Target could have been killed by another Tower or in a fight
  if fUnit.Home.IsDestroyed or ((fTarget<>nil) and fTarget.IsDeadOrDying) then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            if fUnit.Home.HouseType = htWallTower then
            begin
              Home.CurrentAction.SubActionRem([haIdle, haWork1]);
              Home.SetState(hstWork, false);
            end else
            begin
              Home.SetState(hstWork); //Set house to Work state
              Home.CurrentAction.SubActionWork(haWork2); //show Recruits back
            end;
            SetActionStay(20, uaWalk);
          end;
      1:  begin
            if fUnit.Home.HouseType = htWatchTower then
            begin
              Home.WareTakeFromIn(Home.WareInput[1], 1);
              gHands[Owner].Stats.WareConsumed(Home.WareInput[1]);
            end else
            if fUnit.Home.HouseType = htWallTower then
              TKMHouseWallTower(fUnit.Home).RemoveBolt;

            if fUnit.Home.HouseType = htWallTower then
              fFlightTime := gProjectiles.AimTarget(KMPointF(PositionF.X, PositionF.Y - 2), fTarget, ptTowerBolt, fUnit, 15.99, 2)
            else
              fFlightTime := gProjectiles.AimTarget(KMPointF(PositionF.X, PositionF.Y - 1), fTarget, ptTowerRock, fUnit, RANGE_WATCHTOWER_MAX, RANGE_WATCHTOWER_MIN);

            gHands.CleanUpUnitPointer(fTarget); //We don't need it anymore
            SetActionLockedStay(1, uaWalk);
          end;
      2:  SetActionLockedStay(fFlightTime, uaWalk); //Pretend to look how it goes
      3:  begin
            //pretend to reload
            if fUnit.Home.HouseType = htWatchTower then
              SetActionLockedStay(TKMHouseTower(Home).ThrowingCycles + KamRandom(20, 'TKMTaskThrowRock.Execute1'), uaWalk)
            else
              SetActionLockedStay(2, uaWalk);

          end;
      4:  begin
            Home.SetState(hstIdle);
            if fUnit.Home.HouseType = htWallTower then
              SetActionStay(2 + KamRandom(2, 'TKMTaskThrowRock.Execute2'), uaWalk) //Idle before throwing another rock
            else
              SetActionStay(10 + KamRandom(10, 'TKMTaskThrowRock.Execute3'), uaWalk); //Idle before throwing another rock

          end;
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskThrowRock.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskThrowRock');
  if fTarget <> nil then
    SaveStream.Write(fTarget.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fFlightTime);
end;


end.
