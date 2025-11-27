unit KM_UnitTaskDismiss;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units, KM_Houses, KM_CommonClasses, KM_Points;


type
  TKMTaskDismiss = class(TKMUnitTask)
  private
    fSchool: TKMHouse;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function ShouldBeCancelled: Boolean;
    function CouldBeCancelled: Boolean; override;

    property School: TKMHouse read fSchool;
    function FindNewSchool: TKMHouse;

    function Execute: TKMTaskResult; override;
  end;

  TKMTaskDismissWarrior = class(TKMUnitTask)
  private
    fBarracks: TKMHouse;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function ShouldBeCancelled: Boolean;
    function CouldBeCancelled: Boolean; override;

    property Barracks: TKMHouse read fBarracks;
    function FindNewBarracks: TKMHouse;

    function Execute: TKMTaskResult; override;
  end;

  TKMTaskGoToStore = class(TKMUnitTask)
  private
    fLocTo: TKMPoint;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit; aLocTo : TKMPoint);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function CouldBeCancelled: Boolean; override;

    function Execute: TKMTaskResult; override;
  end;

  TKMTaskGoToPearl = class(TKMUnitTask)
  private
    fLocTo: TKMPoint;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit; aLocTo : TKMPoint);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function CouldBeCancelled: Boolean; override;

    function Execute: TKMTaskResult; override;
  end;

  TKMTaskGoToLoc = class(TKMUnitTask)
  private
    fLocTo: TKMPoint;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit; aLocTo : TKMPoint);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function CouldBeCancelled: Boolean; override;

    function Execute: TKMTaskResult; override;
  end;
implementation
uses
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_HouseBarracks,
  KM_UnitWarrior,
  KM_ResTypes, KM_Resource;


{ TTaskDismiss }
constructor TKMTaskDismiss.Create(aUnit: TKMUnit);
begin
  Assert(aUnit is TKMCivilUnit, 'Only civil units are allowed to be dismissed');
  inherited;

  gHands[fUnit.Owner].Stats.UnitDismissed(fUnit.UnitType);

  fType := uttDismiss;
  FindNewSchool;
end;


constructor TKMTaskDismiss.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskDismiss');
  LoadStream.Read(fSchool, 4);
end;


destructor TKMTaskDismiss.Destroy;
begin
  gHands.CleanUpHousePointer(fSchool);
  fUnit.DismissInProgress := False; //Reset dismissInProgress Flag to show proper UI
  gHands[fUnit.Owner].Stats.UnitDismissCanceled(fUnit.UnitType);

  inherited;
end;


function TKMTaskDismiss.ShouldBeCancelled: Boolean;
begin
  Result := (fSchool = nil) or fSchool.IsDestroyed;
end;


function TKMTaskDismiss.CouldBeCancelled: Boolean;
begin
  Result := fPhase <= 1; //Allow cancel dismiss only while walking to the school point below entrance
end;


procedure TKMTaskDismiss.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskDismiss');
  if fSchool <> nil then
    SaveStream.Write(fSchool.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
end;


procedure TKMTaskDismiss.SyncLoad;
begin
  inherited;
  fSchool := gHands[fUnit.Owner].Houses.GetHouseByUID(Integer(fSchool));
end;


function TKMTaskDismiss.FindNewSchool: TKMHouse;
var
  S: TKMHouse;
begin
  fSchool := nil;

  S := gHands[fUnit.Owner].FindHouse(htSchool, fUnit.Position);

  if (S <> nil) and fUnit.CanWalkTo(fUnit.Position, S.PointBelowEntrance, tpWalk, 0) then
    fSchool := S.GetPointer;

  Result := fSchool;
end;


procedure TKMTaskDismiss.InitDefaultAction;
begin
  //Do nothing here, as we have to continue old action, until it could be interrupted
end;


function TKMTaskDismiss.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if (fSchool = nil) or fSchool.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  SetActionWalkToSpot(fSchool.PointBelowEntrance, uaWalk, 0, fUnit.AnimStep); // Preserv current AnimStep
      1:  SetActionGoIn(uaWalk, gdGoInside, fSchool);
      2:  begin
            //Note: we do not set trTaskDone here, as we are going to destroy this task and Close (delete) unit
            //Setting to trTaskDone will force Unit.UpadateState to find new task/action for this unit
            if gMySpectator.Selected = fUnit then
              gMySpectator.Selected := nil; //Reset view, in case we were watching dismissed unit
            gHands[fUnit.Owner].Workless := gHands[fUnit.Owner].Workless + 1;
            gHands[fUnit.Owner].Stats.UnitLost(fUnit.UnitType);
            gHands[Owner].AddFestivalPoints(fptEconomy, -1);
            TKMCivilUnit(fUnit).KillInHouse; //Kill unit silently inside house
            Exit; //Exit immidiately, since we destroyed current task!
                  //Changing any task fields here (f.e. Phase) will try to change freed memory!
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;

{ TTaskDismiss }
constructor TKMTaskDismissWarrior.Create(aUnit: TKMUnit);
begin
  Assert(aUnit is TKMUnitWarrior, 'Only civil units are allowed to be dismissed');
  inherited;

  gHands[fUnit.Owner].Stats.UnitDismissed(fUnit.UnitType);

  fType := uttDismiss;
  FindNewBarracks;
end;


constructor TKMTaskDismissWarrior.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskDismiss');
  LoadStream.Read(fBarracks, 4);
end;


destructor TKMTaskDismissWarrior.Destroy;
begin
  gHands.CleanUpHousePointer(fBarracks);
  fUnit.DismissInProgress := False; //Reset dismissInProgress Flag to show proper UI
  gHands[fUnit.Owner].Stats.UnitDismissCanceled(fUnit.UnitType);

  inherited;
end;


function TKMTaskDismissWarrior.ShouldBeCancelled: Boolean;
begin
  Result := (fBarracks = nil) or fBarracks.IsDestroyed;
end;


function TKMTaskDismissWarrior.CouldBeCancelled: Boolean;
begin
  Result := fPhase <= 1; //Allow cancel dismiss only while walking to the school point below entrance
end;


procedure TKMTaskDismissWarrior.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskDismiss');
  if fBarracks <> nil then
    SaveStream.Write(fBarracks.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
end;


procedure TKMTaskDismissWarrior.SyncLoad;
begin
  inherited;
  fBarracks := gHands[fUnit.Owner].Houses.GetHouseByUID(Integer(fBarracks));
end;


function TKMTaskDismissWarrior.FindNewBarracks: TKMHouse;
var
  S: TKMHouse;
begin
  fBarracks := nil;

  S := gHands[fUnit.Owner].FindHouse(htBarracks, fUnit.Position);

  if (S <> nil) and fUnit.CanWalkTo(fUnit.Position, S.PointBelowEntrance, tpWalk, 0) then
    fBarracks := S.GetPointer;

  Result := fBarracks;
end;


procedure TKMTaskDismissWarrior.InitDefaultAction;
begin
  //Do nothing here, as we have to continue old action, until it could be interrupted
end;


function TKMTaskDismissWarrior.Execute: TKMTaskResult;
var I : Integer;
  U : TKMUnit;
begin
  Result := trTaskContinues;

  if (fBarracks = nil) or fBarracks.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  SetActionWalkToSpot(fBarracks.PointBelowEntrance, uaWalk, 0, fUnit.AnimStep); // Preserv current AnimStep
      1:  SetActionGoIn(uaWalk, gdGoInside, fBarracks);
      2:  begin
            //Note: we do not set trTaskDone here, as we are going to destroy this task and Close (delete) unit
            //Setting to trTaskDone will force Unit.UpadateState to find new task/action for this unit
            if gMySpectator.Selected = fUnit then
              gMySpectator.Selected := nil; //Reset view, in case we were watching dismissed unit
            gHands[fUnit.Owner].Workless := gHands[fUnit.Owner].Workless + 1;
            gHands[fUnit.Owner].Stats.UnitLost(fUnit.UnitType);

            for I := 0 to high(gRes.Units[UnitType].BarracksCost) do
              fBarracks.WareAddToIn(gRes.Units[UnitType].BarracksCost[I].W, gRes.Units[UnitType].BarracksCost[I].C);
            //give back that one recruit
            U := TKMUnit(TKMHouseBarracks(fBarracks).CreateRecruitInside(false));
            if U <> nil then
              U.Condition := fUnit.Condition;
            gHands[Owner].AddFestivalPoints(fptWarfare, -1);
            TKMUnitWarrior(fUnit).KillInHouse;
            Exit; //Exit immidiately, since we destroyed current task!
                  //Changing any task fields here (f.e. Phase) will try to change freed memory!
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


constructor TKMTaskGoToStore.Create(aUnit: TKMUnit; aLocTo: TKMPoint);
begin
  Inherited Create(aUnit);
  fLocTo := aLocTo;
  fType := uttGoToStore;
end;

constructor TKMTaskGoToStore.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLocTo);
end;

procedure TKMTaskGoToStore.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fLocTo);
end;

procedure TKMTaskGoToStore.InitDefaultAction;
begin
  ///do nothing here
end;

function TKMTaskGoToStore.CouldBeCancelled: Boolean;
begin
  Result := true;
end;

function TKMTaskGoToStore.Execute: TKMTaskResult;
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
            SetActionWalkToSpot(fLocTo, uaWalk, 3, fUnit.AnimStep); // Preserv current AnimStep
            fUnit.Thought := thImportant;
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


constructor TKMTaskGoToPearl.Create(aUnit: TKMUnit; aLocTo: TKMPoint);
begin
  Inherited Create(aUnit);
  fLocTo := aLocTo;
  fType := uttGoToPearl;
end;

constructor TKMTaskGoToPearl.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLocTo);
end;

procedure TKMTaskGoToPearl.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fLocTo);
end;

procedure TKMTaskGoToPearl.InitDefaultAction;
begin
  ///do nothing here
end;

function TKMTaskGoToPearl.CouldBeCancelled: Boolean;
begin
  Result := true;
end;

function TKMTaskGoToPearl.Execute: TKMTaskResult;
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
            SetSpeed(8, true);
            Defence := Defence + 8;
            SetActionWalkToSpot(fLocTo, uaWalk, 6, fUnit.AnimStep); // Preserv current AnimStep
            fUnit.Thought := thImportant;
          end;
      1:  begin
            Defence := Defence - 8;
            SetSpeed(-8, true);
            SetActionStay(0, uaWalk);
            fUnit.Thought := thNone;
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


constructor TKMTaskGoToLoc.Create(aUnit: TKMUnit; aLocTo: TKMPoint);
begin
  Inherited Create(aUnit);
  fLocTo := aLocTo;
  fType := uttGoToLoc;
end;

constructor TKMTaskGoToLoc.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fLocTo);
end;

procedure TKMTaskGoToLoc.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fLocTo);
end;

procedure TKMTaskGoToLoc.InitDefaultAction;
begin
  ///do nothing here
end;

function TKMTaskGoToLoc.CouldBeCancelled: Boolean;
begin
  Result := true;
end;

function TKMTaskGoToLoc.Execute: TKMTaskResult;
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
            SetActionWalkToSpot(fLocTo, uaWalk, 0, fUnit.AnimStep); // Preserv current AnimStep
            fUnit.Thought := thQuest;
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;

end.

