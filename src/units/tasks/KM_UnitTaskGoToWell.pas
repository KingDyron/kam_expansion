unit KM_UnitTaskGoToWell;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses;


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


implementation
uses
  KM_Terrain,
  KM_CommonUtils,
  KM_HandsCollection, KM_Entity,
  KM_UnitWarrior,
  KM_Points,
  KM_ResTypes;


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
            Thought := thQuest;
            SetActionGoIn(uaWalk, gdGoOutside, Home);
            Home.SetState(hstEmpty);
          end;
      1:  SetActionWalkToSpot(fWell.PointBelowEntrance);
      2:  SetActionGoIn(uaWalk, gdGoInside, fWell);//enter the well
      3:  if fWell.CheckWareOut(wtWater) > 0 then
          begin
            SetActionLockedStay(5, uaWalk); //take water
            fWell.DecProgress(200);
            fWell.WareTakeFromOut(wtWater, 1, true);
            fHasWater := true;
          end else
          begin
            SetActionLockedStay(100, uaWalk); // wait a little, maybe water will be produced soon
            {Inc(fPhase2);
            if fPhase2 < 5 then //wait only 5 times, then go back to home with nothing
              fPhase := 2;}
            fWell.DecProgress(200);
            fHasWater := true;
          end;
      //go back to house
      4: SetActionGoIn(uaWalk, gdGoOutside, fWell);
      5: SetActionWalkToSpot(Home.PointBelowEntrance);
      6: SetActionGoIn(uaWalk, gdGoInside, Home);
      //unit back at home
      7:  begin
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

end.
