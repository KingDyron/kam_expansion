unit KM_UnitTaskDie;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_Units, SysUtils;

type
  {Yep, this is a Task}
  TKMTaskDie = class(TKMUnitTask)
  private
    fShowAnimation: Boolean;
  public
    constructor Create(aUnit: TKMUnit; aShowAnimation: Boolean);
    constructor Load(LoadStream: TKMemoryStream); override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_ResSound, KM_Sound,
  KM_Points,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_Resource, KM_UnitWarrior, KM_ScriptingEvents,
  KM_ResUnits, KM_Terrain,
  KM_SpecialAnim;


{ TTaskDie }
constructor TKMTaskDie.Create(aUnit: TKMUnit; aShowAnimation: Boolean);
begin
  inherited Create(aUnit);
  fType := uttDie;
  fShowAnimation := aShowAnimation;
  //Shortcut to remove the pause before the dying animation which makes fights look odd
  if aUnit.Visible or (aUnit.InShip <> nil) then
  begin
    fPhase := 1; //Phase 0 can be skipped when the unit is visible
    //no need to execute,  when animation is seperate
    //Execute;
  end;
end;


constructor TKMTaskDie.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskDie');
  LoadStream.Read(fShowAnimation);
end;


procedure TKMTaskDie.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskDie');
  SaveStream.Write(fShowAnimation);
end;


function TKMTaskDie.Execute: TKMTaskResult;
var
  //SequenceLength: SmallInt;
  TempOwner: TKMHandID;
  TempUnitType: TKMUnitType;
  TempX, TempY: Word;
begin
  Result := trTaskContinues;
  with fUnit do
  case fPhase of
    0:    if Visible or (InShip <> nil) then
          begin
            SetActionLockedStay(0, uaWalk);
          end
          else
          begin
            if Home.IsValid then
            begin
              Home.SetState(hstIdle);
              Home.SetState(hstEmpty);
            end;
            if Home = nil then
            begin
              SetActionLockedStay(0, uaWalk);
            end else
              SetActionGoIn(uaWalk, gdGoOutside, gHands.HousesHitTest(fUnit.PositionNext.X, fUnit.PositionNext.Y));
          end;
    1:    begin
            if not fShowAnimation or (fUnit is TKMUnitAnimal) then //Animals don't have a dying sequence. Can be changed later.
              SetActionLockedStay(0, uaWalk, False)
            else
            begin
              //SequenceLength := gRes.Units[UnitType].UnitAnim[uaDie, Direction].Count;
              SetActionLockedStay({SequenceLength}0, uaDie, true);
              gSpecAnim.AddUnitDeath(Spec.UnitAnim[uaDie, Direction], PositionF + KMPointF(0.5, 0.5), PositionF + KMPointF(0.5, 0.5));
              //Do not play sounds if unit is invisible to gMySpectator
              //We should not use KaMRandom below this line because sound playback depends on FOW and is individual for each player
              if gMySpectator.FogOfWar.CheckTileRevelation(fUnit.Position.X, fUnit.Position.Y) >= 255 then
              begin
                if fUnit is TKMUnitWarrior then
                  gSoundPlayer.PlayWarrior(fUnit.UnitType, spDeath, fUnit.PositionF)
                else
                  gSoundPlayer.PlayCitizen(fUnit.UnitType, spDeath, fUnit.PositionF);
              end;
            end;
          end;
    else  begin
            //Store them before they get lost forever
            TempOwner := fUnit.Owner;
            TempUnitType := fUnit.UnitType;
            TempX := fUnit.Position.X;
            TempY := fUnit.Position.Y;
            if fUnit.IsAnimal then
              if TKMUnitAnimal(fUnit).SpawnerID <> -1 then
                gHands.PlayerAnimals.Spawners[TKMUnitAnimal(fUnit).SpawnerID].Animals.Remove(fUnit);

            fUnit.CloseUnit;          //This will FreeAndNil the Task and mark unit as "closed"

            //Can't run this in CloseUnit since it is used from other places like barracks equipping and we only want it for normal deaths
            //Notify the script that the unit is now gone from the game
            gScriptEvents.EventUnitAfterDied(TempUnitType, TempOwner, TempX, TempY);

            Result := trTaskContinues;  //Running UpdateState will exit without further changes
            Exit;                     //Next UpdateState won't happen cos unit is "closed"
          end;
  end;
  Inc(fPhase);
end;


end.
