unit KM_UnitTaskSelfTrain;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Houses, KM_HouseSchool, KM_Units;


//Train citizen in school
type
  TKMTaskSelfTrain = class(TKMUnitTask)
  private
    fTime : Word;
    fSchool: TKMHouseSchool;
  public
    constructor Create(aUnit: TKMUnit; aSchool: TKMHouseSchool);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity, KM_ResTypes, KM_Sound, KM_ResSound,
  KM_Game, KM_CommonHelpers, KM_MapTypes,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_Resource;


{ TTaskSelfTrain }
constructor TKMTaskSelfTrain.Create(aUnit: TKMUnit; aSchool: TKMHouseSchool);
begin
  inherited Create(aUnit);

  fType := uttSelfTrain;
  fSchool   := TKMHouseSchool(aSchool.GetPointer);
  fUnit.Visible := False;

  fTime := gRes.Units[fUnit.UnitType].SchoolTime;
  if gGame.Params.MBD.IsEasy then
    fTime := Round(fTime * 0.8)
  else
  if gGame.Params.MBD.IsHardOrRealism then
    fTime := Round(fTime * 1.25);

  if gHands[fUnit.Owner].HasPearl(ptValtaria) then
    fTime := Round(fTime * 0.8);
  if gGame.Params.MPMode = mmPacifist then
    fTime := Round(fTime * 0.75);

  fSchool.TotalWorkingTime := fTime * 5;
end;


constructor TKMTaskSelfTrain.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskSelfTrain');
  LoadStream.Read(fSchool, 4);
  LoadStream.Read(fTime);
end;


procedure TKMTaskSelfTrain.SyncLoad;
begin
  inherited;
  fSchool := TKMHouseSchool(gHands.GetHouseByUID(Integer(fSchool)));
end;


destructor TKMTaskSelfTrain.Destroy;
begin
  if (gGame = nil) or gGame.IsExiting then Exit; //fSchool will already be freed

  // If we abandon for some reason, clear the school animation
  if (fSchool <> nil) and (fPhase <= 5) and not fSchool.IsDestroyed then
    fSchool.SetState(hstIdle);

  gHands.CleanUpHousePointer(TKMHouse(fSchool));

  inherited;
end;


function TKMTaskSelfTrain.Execute:TKMTaskResult;
begin
  Result := trTaskContinues;

  //If the school has been destroyed then this task should not be running (school frees it on CloseHouse)
  //However, if we are past phase 6 (task ends on phase 7) then the school does not know about us (we have stepped outside)
  if fSchool.IsDestroyed and (fPhase <= 6) then
    //School will cancel the training on own destruction
    raise Exception.Create('Unexpected error. Destoyed school erases the task');

  with fUnit do
    case fPhase of
      0: begin
          fSchool.SetState(hstWork);
          //fSchool.CurrentAction.SubActionWork(haWork1);

          SetActionLockedStay(fTime, uaWalk);
        end;
      1: begin
          //fSchool.CurrentAction.SubActionWork(haWork2);
          SetActionLockedStay(fTime, uaWalk);
        end;
      2: begin
          //fSchool.CurrentAction.SubActionWork(haWork3);
          SetActionLockedStay(fTime, uaWalk);
        end;
      3: begin
          //fSchool.CurrentAction.SubActionWork(haWork4);
          SetActionLockedStay(fTime, uaWalk);
        end;
      4: begin
          //fSchool.CurrentAction.SubActionWork(haWork5);
          SetActionLockedStay(fTime, uaWalk);
        end;
      5: begin
          fSchool.SetState(hstIdle);
          SetActionLockedStay(5, uaWalk);
         end;
      6: begin
          gSoundPlayer.Play(sfxSchoolDing, fSchool.Entrance);
          // Put him in the school, so if it is destroyed while he is looking for place to exit he is placed somewhere
          fUnit.InHouse := fSchool;
          SetActionGoIn(uaWalk, gdGoOutside, fSchool);
          fSchool.UnitTrainingComplete(fUnit);


          if Assigned(fUnit.OnUnitTrained) then
            fUnit.OnUnitTrained(fUnit);

          if fSchool.CheckWareIn(wtBoots) > 0 then
            if fUnit.GiveBoots then
              fSchool.WareTakeFromIn(wtBoots);
         end;
      else Result := trTaskDone;
    end;
  Inc(fPhase);
end;


procedure TKMTaskSelfTrain.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskSelfTrain');
  SaveStream.Write(fSchool.UID);
  SaveStream.Write(fTime);
end;


end.

