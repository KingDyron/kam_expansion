﻿unit KM_UnitTaskCartographer;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Points,
  KM_CommonClasses, KM_Defaults, KM_Units,
  KM_HouseCartographers;


{Throw a rock}
type
  TKMTaskCartographer = class(TKMUnitTask)
  private
    fCenterPoint : TKMPoint;
    function GetRandomPos : TKMPoint;
    function Cartographers : TKMHouseCartographers;
  public
    constructor Create(aUnit: TKMUnit; aLoc : TKMPoint);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  Math,
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity, KM_Houses,
  KM_Projectiles, KM_CommonUtils,
  KM_CommonGameTypes, KM_ResTypes,
  KM_Terrain;

const
  MAX_PHASE2 = 5;
{ TTaskThrowRock }
constructor TKMTaskCartographer.Create(aUnit: TKMUnit; aLoc : TKMPoint);
begin
  inherited Create(aUnit);
  fType := uttCartographer;
  fCenterPoint := aLoc;
end;


destructor TKMTaskCartographer.Destroy;
begin
  if (fUnit <> nil)
    and not fUnit.Home.IsDestroyed
    and (fUnit.Home.GetState = hstWork) then
      fUnit.Home.SetState(hstIdle, false); //Make sure we don't abandon and leave our tower with "working" animations

  inherited;
end;


constructor TKMTaskCartographer.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskCartographer');
  LoadStream.Read(fCenterPoint);
end;

function TKMTaskCartographer.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  //Target could have been killed by another Tower or in a fight
  if fUnit.Home.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Home.SetState(hstEmpty);
            SetActionGoIn(uaWalk, gdGoOutside, Home);
          end;
      1:  begin
            SetActionWalkToSpot(GetRandomPos);
          end;
      2:  begin
            inc(fPhase2);
            SetActionLockedStay(50, uaWalk);
            If fPhase2 < MAX_PHASE2 then
              fPhase := 0;
          end;
      3:  SetActionWalkToSpot(Home.PointBelowEntrance);
      4:  begin
            SetActionGoIn(uaWalk, gdGoInside, Home);
          end;
      5:  begin
            Home.SetState(hstWork);
            Home.CurrentAction.SubActionAdd([haSmoke]);
            SetActionLockedStay(600, uaWalk);// 1 minute
            Home.TotalWorkingTime := 600;
            Home.WorkingTime := 0;
          end;
      6:  begin
            Home.SetState(hstIdle);
            SetActionLockedStay(Home.HSpec.WorkerRest * 10, uaWalk);
            Cartographers.CollectData(fCenterPoint);
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


procedure TKMTaskCartographer.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskCartographer');
  SaveStream.Write(fCenterPoint);
end;

function TKMTaskCartographer.GetRandomPos: TKMPoint;
const
  MAX_RANGE = 5;
var I : Integer;
  P : TKMPoint;
begin
  Result := fCenterPoint;
  I := 0;
  while I < 10 do
  begin
    P.X := Result.X + KaMRandom(MAX_RANGE * 2 + 1, 'TKMTaskCartographer.GetRandomPos') - MAX_RANGE;
    P.Y := Result.Y + KaMRandom(MAX_RANGE * 2 + 1, 'TKMTaskCartographer.GetRandomPos') - MAX_RANGE;
    If gTerrain.RouteCanBeMade(fCenterPoint, P, tpWalk) then
      Exit(P);
    Inc(I);
  end;
end;

function TKMTaskCartographer.Cartographers: TKMHouseCartographers;
begin
  Result := TKMHouseCartographers(fUnit.Home);
end;

end.
