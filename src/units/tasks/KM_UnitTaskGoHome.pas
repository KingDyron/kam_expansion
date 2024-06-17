unit KM_UnitTaskGoHome;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units;


type
  TKMTaskGoHome = class(TKMUnitTask)
  public
    constructor Create(aUnit: TKMUnit);

    function Execute: TKMTaskResult; override;
    function CouldBeCancelled: Boolean; override;

  end;


implementation
Uses KM_HandsCollection, KM_ResTypes;

{ TTaskGoHome }
constructor TKMTaskGoHome.Create(aUnit: TKMUnit);
begin
  inherited;

  fType := uttGoHome;
  aUnit.Thought := thHome;
end;


function TKMTaskGoHome.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


function TKMTaskGoHome.Execute: TKMTaskResult;
//var U : TKMUnit;
begin
  Result := trTaskContinues;

  if fUnit.Home.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Thought := thHome;
            SetActionWalkToSpot(Home.PointBelowEntrance);
          end;
      1:  SetActionGoIn(uaWalk, gdGoInside, Home);
      2:  begin
            Thought := thNone; //Only stop thinking once we are right inside
            if (Home.HouseType <> htSiegeWorkshop) or (fUnit.UnitType = utCarpenter) then
              Home.SetState(hstIdle);
            SetActionStay(5, uaWalk);
            {U := gHands[1].TrainUnit(utSerf, Home);
            if U <> nil then
            begin
              U.Visible := false;
              U.Condition := UNIT_MAX_CONDITION;
              U.SetActionGoIn(uaWalk, gdGoOutside, Home);
              fUnit.CloseUnit;
            end; }


          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;

end.
