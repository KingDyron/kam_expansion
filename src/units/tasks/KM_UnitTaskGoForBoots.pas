unit KM_UnitTaskGoForBoots;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_CommonClasses,
  KM_Defaults, KM_Units, KM_Houses;


type
  TKMTaskGoGetBoots = class(TKMUnitTask)
  private
    fHouse : TKMHouse;
  public
    constructor Create(aUnit: TKMUnit; aHouse : TKMHouse);
    constructor Load(LoadStream: TKMemoryStream); override;

    function Execute: TKMTaskResult; override;
    function CouldBeCancelled: Boolean; override;
    function WalkShouldAbandon : Boolean; override;
    procedure SyncLoad; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
Uses  KM_HandsCollection,
      KM_Entity,
      KM_ResTypes, Math;

{ TTaskGoHome }
constructor TKMTaskGoGetBoots.Create(aUnit: TKMUnit; aHouse : TKMHouse);
begin
  inherited Create(aUnit);

  fType := uttGoGetBoots;
  aUnit.Thought := thHome;
  fHouse := aHouse;
  Inc(fHouse.BootsReserved);
end;


function TKMTaskGoGetBoots.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;

function TKMTaskGoGetBoots.WalkShouldAbandon: Boolean;
begin
  Result := (fHouse = nil) or (fHouse.IsDestroyed) or (fHouse.CheckWareOut(wtBoots) = 0);
  Result := Result and (fPhase <= 0);
end;

function TKMTaskGoGetBoots.Execute: TKMTaskResult;
//var U : TKMUnit;
begin
  Result := trTaskContinues;

  if (fHouse = nil) or fHouse.IsDestroyed or WalkShouldAbandon then
  begin
    Result := trTaskDone;
    fHouse.BootsReserved := Max(fHouse.BootsReserved - 1, 0);
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  begin
            Thought := thHome;
            SetActionWalkToSpot(fHouse.PointBelowEntrance);
          end;
      1:  SetActionGoIn(uaWalk, gdGoInside, fHouse);
      2:  begin
            Thought := thNone; //Only stop thinking once we are right inside
            if fHouse.CheckWareOut(wtBoots) > 0 then
            begin
              fHouse.WareTakeFromOut(wtBoots, 1, true);
              fHouse.BootsReserved := Max(fHouse.BootsReserved - 1, 0);
              fUnit.GiveBoots;
            end;
            
            SetActionStay(5, uaWalk);
          end;
      3:  SetActionGoIn(uaWalk, gdGoOutside, fHouse);
      4:  SetActionStay(5, uaWalk);
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;

constructor TKMTaskGoGetBoots.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fHouse, 4);
end;

procedure TKMTaskGoGetBoots.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fHouse.UID);
end;

procedure TKMTaskGoGetBoots.SyncLoad;
begin
  Inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;

end.
