unit KM_UnitTaskDelivery;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Units, KM_Structure,
  KM_ResTypes;


type
  TKMDeliverKind = (dkToHouse, dkToConstruction, dkToUnit, dkToWall, dkToStructure, dkOther, dkNone);
  TKMDeliverStage = (
    dsUnknown,
    dsToFromHouse,     //Serf is walking to the offer house
    dsAtFromHouse,     //Serf is getting in / out from offer house
    dsToDestination,   //Serf is walking to destination (unit/house)
    dsAtDestination);  //Serf is operating with destination

  TKMTaskDeliver = class(TKMUnitTask)
  private
    fFrom: TKMHouse;
    fToHouse: TKMHouse;
    fToUnit: TKMUnit;
    fToLoc: TKMPoint;
    fToStruct: TKMStructure;
    fWareType: TKMWareType;
    fDeliverID: Integer;
    fDeliverKind: TKMDeliverKind;
    //Force delivery, even if fToHouse blocked ware from delivery.
    //Used in exceptional situation, when ware was carried by serf and delivery demand was destroyed and no one new was found
    fForceDelivery: Boolean;
    fTicker : Word;
    procedure CheckForBetterDestination;
    function FindBestDestination: Boolean;
    function GetDeliverStage: TKMDeliverStage;
    property FromHouse: TKMHouse read fFrom write fFrom;
    property ToHouse: TKMHouse read fToHouse write fToHouse;
    property ToStruct: TKMStructure read fToStruct write fToStruct;
    function CanAbandonWalk: Boolean;
  public
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aToHouse: TKMHouse; aWare: TKMWareType; aID: Integer); overload;
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aToUnit: TKMUnit; aWare: TKMWareType; aID: Integer); overload;
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aTo: TKMPoint; aWare: TKMWareType; aID: Integer); overload;
    constructor Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aTo: TKMStructure; aWare: TKMWareType; aID: Integer); overload;
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    property DeliverKind: TKMDeliverKind read fDeliverKind;
    property DeliverStage: TKMDeliverStage read GetDeliverStage;
    property ToUnit: TKMUnit read fToUnit write fToUnit;
    procedure DelegateToOtherSerf(aToSerf: TKMUnit);
    function Execute: TKMTaskResult; override;
    function CouldBeCancelled: Boolean; override;
    function CanRestartAction(aLastActionResult: TKMActionResult): Boolean; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function ObjToString(const aSeparator: String = ', '): String; override;

    procedure UpdateState; override;
    procedure Paint; override; //Used only for debug so far
  end;


  TKMTaskWHDeliver = class(TKMUnitTask)
  private
    fFrom: TKMHouse;
    fDeliverKind: TKMDeliverKind;
    fIndex : Integer;//index from list of WH deliveries
    //Force delivery, even if fToHouse blocked ware from delivery.
    //Used in exceptional situation, when ware was carried by serf and delivery demand was destroyed and no one new was found
    fForceDelivery: Boolean;
    procedure CheckForBetterDestination(aID : Byte);
    function FindBestDestination: Boolean;
    //function GetDeliverStage: TKMDeliverStage;
    property FromHouse: TKMHouse read fFrom write fFrom;
    function CanAbandonWalk: Boolean;
    procedure SetDeliveryKind;
    function SetValidIndex : Boolean;
  public
    constructor Create(aWHMan: TKMUnitWHMan);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    function CanRestartAction(aLastActionResult: TKMActionResult): Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function ObjToString(const aSeparator: String = ', '): String; override;
  end;

implementation
uses
  Math, TypInfo,
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_UnitWarrior, KM_HouseInn, KM_ResHouses,
  KM_UnitTaskBuild, KM_Log, KM_RenderAux,
  KM_Terrain,
  KM_HouseTownHall;


{ TTaskDeliver }
constructor TKMTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aToHouse: TKMHouse; aWare: TKMWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fType := uttDeliver;
  fTicker := 0;

  Assert((aFrom <> nil) and (aToHouse <> nil) and (aWare <> wtNone), 'Serf ' + IntToStr(fUnit.UID) + ': invalid delivery task');

  if gLog.CanLogDelivery then
    gLog.LogDelivery('Serf ' + IntToStr(fUnit.UID) + ' created delivery task ' + IntToStr(fDeliverID));

  FromHouse := aFrom.GetPointer; //Also will set fPointBelowFromHouse
  ToHouse := aToHouse.GetPointer; //Also will set fPointBelowToHouse
  //Check it once to begin with as the house could become complete before the task exits (in rare circumstances when the task
  // does not exit until long after the ware has been delivered due to walk interactions)

  if aToHouse.IsComplete then
  begin
    if aToHouse.IsUpgrading then
      fDeliverKind := dkToConstruction
    else
      fDeliverKind := dkToHouse
  end else
  if aToHouse.HouseType in WALL_HOUSES then
    fDeliverKind := dkToWall
  else
    fDeliverKind := dkToConstruction;

  fWareType   := aWare;
  fDeliverID  := aID;
end;


constructor TKMTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aToUnit: TKMUnit; aWare: TKMWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fType := uttDeliver;
  fTicker := 0;

  Assert((aFrom <> nil) and (aToUnit <> nil) and ((aToUnit is TKMUnitWarrior) or (aToUnit is TKMUnitWorker)) and (aWare <> wtNone), 'Serf '+inttostr(fUnit.UID)+': invalid delivery task');

  if gLog.CanLogDelivery then
    gLog.LogDelivery('Serf ' + IntToStr(fUnit.UID) + ' created delivery task ' + IntToStr(fDeliverID));

  FromHouse := aFrom.GetPointer;
  ToUnit    := aToUnit.GetPointer;
  fDeliverKind := dkToUnit;
  fWareType := aWare;
  fDeliverID := aID;
end;

constructor TKMTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aTo: TKMPoint; aWare: TKMWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fType := uttDeliver;
  fTicker := 0;

  Assert((aFrom <> nil) and (aWare <> wtNone), 'Serf '+inttostr(fUnit.UID)+': invalid delivery task');

  if gLog.CanLogDelivery then
    gLog.LogDelivery('Serf ' + IntToStr(fUnit.UID) + ' created delivery task ' + IntToStr(fDeliverID));

  FromHouse := aFrom.GetPointer;
  ToUnit    := nil;
  fToLoc := aTo;
  fDeliverKind := dkOther;
  fWareType := aWare;
  fDeliverID := aID;
end;

constructor TKMTaskDeliver.Create(aSerf: TKMUnitSerf; aFrom: TKMHouse; aTo: TKMStructure; aWare: TKMWareType; aID: Integer);
begin
  inherited Create(aSerf);
  fType := uttDeliver;
  fTicker := 0;

  Assert((aFrom <> nil) and (aWare <> wtNone), 'Serf '+inttostr(fUnit.UID)+': invalid delivery task');

  if gLog.CanLogDelivery then
    gLog.LogDelivery('Serf ' + IntToStr(fUnit.UID) + ' created delivery task ' + IntToStr(fDeliverID));

  FromHouse := aFrom.GetPointer;
  ToUnit    := nil;
  ToHouse := nil;
  ToStruct := aTo;
  fDeliverKind := dkToStructure;
  fWareType := aWare;
  fDeliverID := aID;
end;

constructor TKMTaskDeliver.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskDeliver');
  LoadStream.Read(fFrom, 4);
  LoadStream.Read(fToHouse, 4);
  LoadStream.Read(fToUnit, 4);
  LoadStream.Read(fToStruct, 4);
  LoadStream.Read(fToLoc); //Store ID, then substitute it with reference on SyncLoad
  LoadStream.Read(fForceDelivery);
  LoadStream.Read(fWareType, SizeOf(fWareType));
  LoadStream.Read(fDeliverID);
  LoadStream.Read(fDeliverKind, SizeOf(fDeliverKind));
  LoadStream.Read(fTicker);
end;


procedure TKMTaskDeliver.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskDeliver');
  SaveStream.Write(fFrom.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fToHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fToUnit.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fToStruct.UID);
  SaveStream.Write(fToLoc); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fForceDelivery);
  SaveStream.Write(fWareType, SizeOf(fWareType));
  SaveStream.Write(fDeliverID);
  SaveStream.Write(fDeliverKind, SizeOf(fDeliverKind));
  SaveStream.Write(fTicker);
end;


procedure TKMTaskDeliver.SyncLoad;
begin
  inherited;
  fFrom    := gHands.GetHouseByUID(Integer(fFrom));
  fToHouse := gHands.GetHouseByUID(Integer(fToHouse));
  fToUnit  := gHands.GetUnitByUID(Integer(fToUnit));
  fToStruct  := gHands.GetStructureByUID(Integer(fToStruct));
end;


destructor TKMTaskDeliver.Destroy;
begin
  if gLog.CanLogDelivery then
    gLog.LogDelivery('Serf ' + IntToStr(fUnit.UID) + ' abandoned delivery task ' + IntToStr(fDeliverID) + ' at phase ' + IntToStr(fPhase));

  if fUnit <> nil then
  begin
    if fDeliverID <> DELIVERY_NO_ID then
      gHands[fUnit.Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);

    if TKMUnitSerf(fUnit).Carry <> wtNone then
    begin
      gHands[fUnit.Owner].Stats.WareConsumed(TKMUnitSerf(fUnit).Carry);
      TKMUnitSerf(fUnit).CarryTake; //empty hands
    end;
  end;

  gHands.CleanUpHousePointer(fFrom);
  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpUnitPointer(fToUnit);
  inherited;
end;


//Note: Phase is -1 because it will have been increased at the end of last Execute
function TKMTaskDeliver.WalkShouldAbandon: Boolean;
begin
  Result := False;

  if not CanAbandonWalk then
    Exit;

  if not gHands[fUnit.Owner].Deliveries.Queue.IsDeliveryAlowed(fDeliverID) then
    Exit(True);

  //After step 2 we don't care if From is destroyed or doesn't have the ware
  if fPhase <= 2 then
    Result := Result
                or fFrom.IsDestroyed
                or fFrom.ShouldAbandonDeliveryFrom(fWareType)
                or fFrom.ShouldAbandonDeliveryFromTo(fToHouse, fWareType, fPhase = 2); //Make immidiate check only on Phase 2 (inside house)

  Result := Result or CanRestartAction(fLastActionResult);
  
  //do not abandon the delivery if target is destroyed/dead, we will find new target later
  case fDeliverKind of
    dkToHouse:        begin

                        Result := Result or fToHouse.IsDestroyed;
                        Result := Result or (not fForceDelivery and fToHouse.ShouldAbandonDeliveryTo(fWareType));

                        if Result then
                          if (fToHouse.HouseType = htTownhall) and not fToHouse.IsValid then
                            Result := Result and (fPhase < 5);
                      end;
    dkToStructure: Result := Result or fToStruct.IsComplete or fToStruct.IsDestroyed;
    dkToWall,
    dkToConstruction: if not fToHouse.IsUpgrading then
                        Result := Result or fToHouse.IsComplete;
    dkToUnit:         begin
                        Result := Result or (fToUnit = nil) or fToUnit.IsDeadOrDying;
                      end;
  end;
  //Result := Result or (fTicker > 100);//serf is stuck
end;


// We can restart some actions
function TKMTaskDeliver.CanRestartAction(aLastActionResult: TKMActionResult): Boolean;
begin
  Result :=     (aLastActionResult = arActCanNotStart)
            and (fDeliverKind = dkToHouse)
            and (fPhase - 1 = 6); //Serf tried to get inside destination house
end;


procedure TKMTaskDeliver.CheckForBetterDestination;
var
  NewToHouse: TKMHouse;
  NewToUnit: TKMUnit;
  newStruct : TKMStructure;
begin
  gHands[fUnit.Owner].Deliveries.Queue.CheckForBetterDemand(fDeliverID, NewToHouse, NewToUnit, newStruct, fUnit);

  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpUnitPointer(fToUnit);
  if NewToHouse <> nil then
  begin
    ToHouse := NewToHouse.GetPointer; //Use Setter here to set up fPointBelowToHouse
    if fToHouse.IsComplete then
    begin
      if fToHouse.IsUpgrading then
        fDeliverKind := dkToConstruction
      else
        fDeliverKind := dkToHouse
    end else
    if fToHouse.HouseType in WALL_HOUSES then
      fDeliverKind := dkToWall
    else
      fDeliverKind := dkToConstruction;
  end
  else
  if NewToUnit <> nil then    
  begin
    ToUnit := NewToUnit.GetPointer; //Use Setter here to clean up fPointBelowToHouse
    fDeliverKind := dkToUnit;
  end else
  if newStruct <> nil then
  begin
    ToStruct := newStruct; //Use Setter here to clean up fPointBelowToHouse
    fDeliverKind := dkToStructure;
  end;
end;


// Try to find best destination
function TKMTaskDeliver.FindBestDestination: Boolean;
var
  NewToHouse: TKMHouse;
  NewToUnit: TKMUnit;
  NewToStructure: TKMStructure;
begin
  if fPhase <= 2 then
  begin
    Result := False;
    Exit;
  end else
  if InRange(fPhase, 3, 3) then
  begin
    Result := True;
    Exit;
  end;

  fForceDelivery := False; //Reset ForceDelivery from previous runs
  gHands[fUnit.Owner].Deliveries.Queue.DeliveryFindBestDemand(TKMUnitSerf(fUnit), fDeliverID, fWareType, NewToHouse, NewToUnit, NewToStructure, fForceDelivery);

  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpUnitPointer(fToUnit);

  // New House
  if (NewToStructure = nil) and (NewToHouse <> nil) and (NewToUnit = nil) then
  begin
    ToHouse := NewToHouse.GetPointer; //Use Setter here to set up fPointBelowToHouse
    if fToHouse.IsComplete then
    begin
      if fToHouse.IsUpgrading then
        fDeliverKind := dkToConstruction
      else
        fDeliverKind := dkToHouse
    end else
    if fToHouse.HouseType in WALL_HOUSES then
      fDeliverKind := dkToWall
    else
      fDeliverKind := dkToConstruction;

    Result := True;
    if fPhase > 4 then
      fPhase := 4;
  end
  else
  // New Unit
  if (NewToStructure = nil) and (NewToHouse = nil) and (NewToUnit <> nil) then
  begin
    ToUnit := NewToUnit.GetPointer; //Use Setter here to clean up fPointBelowToHouse
    fDeliverKind := dkToUnit;
    Result := True;
    if fPhase > 4 then
      fPhase := 4;
  end
  else
  if (NewToStructure <> nil) and (NewToHouse = nil) and (NewToUnit = nil) then
  begin
    ToStruct := NewToStructure; //Use Setter here to clean up fPointBelowToHouse
    fDeliverKind := dkToStructure;
    Result := True;
    if fPhase > 4 then
      fPhase := 4;
  end else
  // No alternative
  if (NewToHouse = nil) and (NewToUnit = nil) then
    Result := False
  else
  // Error
    raise Exception.Create('Both destinations could not be');
end;


function TKMTaskDeliver.CouldBeCancelled: Boolean;
begin
  //Allow cancel task only at walking phases
  Result := ((fPhase - 1) //phase was increased at the end of execution
              <= 0)       //<= because fPhase is 0 when task is just created
            or ((fPhase - 1) = 5);
end;


//Get Delivery stage
function TKMTaskDeliver.GetDeliverStage: TKMDeliverStage;
var
  Phase: Integer;
begin
  Result := dsUnknown;
  Phase := fPhase - 1; //fPhase is increased at the phase end
  case Phase of
    -10..0,4: Result := dsToFromHouse;
    1..3:     Result := dsAtFromHouse;
    else
      case fDeliverKind of
        dkToHouse:         begin
                              case Phase of
                                5:    Result := dsToDestination;
                                else  Result := dsAtDestination;
                              end;
                            end;
        dkToStructure,
        dkToWall,
        dkToConstruction,
        dkToUnit:          begin
                              case Phase of
                                5,6:  Result := dsToDestination;
                                else  Result := dsAtDestination;
                              end;
                            end;
      end;
  end;
end;


function TKMTaskDeliver.CanAbandonWalk: Boolean;
begin
  case fDeliverKind of
    dkToHouse:         Result := fPhase <= 8;
    dkToWall,
    dkToConstruction:  Result := fPhase <= 7;
    dkToUnit:          Result := fPhase <= 6;
    dkToStructure,
    dkOther:          Result := fPhase <= 6;
  else
    raise Exception.Create('Unexpected type');
  end;
end;


//Delegate delivery task to other serf
procedure TKMTaskDeliver.DelegateToOtherSerf(aToSerf: TKMUnit);
begin
  //Allow to delegate task only while serf is walking to From House
  Assert(DeliverStage = dsToFromHouse, 'DeliverStage <> dsToFromHouse');

  gHands.CleanUpUnitPointer(fUnit);
  fUnit := aToSerf.GetPointer;

  InitDefaultAction; //InitDefaultAction, otherwise serf will not have any action
end;


function TKMTaskDeliver.Execute: TKMTaskResult;
var
  Worker: TKMUnit;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon and fUnit.Visible and not FindBestDestination then
    Exit(trTaskDone);

  {if fTicker > 1000 then //unit was stuck, so abondon this delivery
  begin
    Result := trTaskDone;
    fTicker := 0;
  end;}
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0:  begin
          SetActionWalkToSpot(fFrom.GetClosestEntrance(Position).DirFaceloc{fFrom.PointBelowEntrance});
        end;
    1:  begin
          SetActionGoIn(uaWalk, gdGoInside, fFrom);
        end;
    2:  begin
          //Serf is inside house now.
          //Barracks can consume the resource (by equipping) before we arrive
          //All houses can have resources taken away by script at any moment
          if fFrom.ShouldAbandonDeliveryFrom(fWareType)
             or fFrom.ShouldAbandonDeliveryFromTo(fToHouse, fWareType, True) then //For store evacuation
          begin
            SetActionLockedStay(5, uaWalk); //Wait a moment inside
            fPhase := 120; //Will get out of Barracks onthat Phase
            Exit;
          end;
          SetActionLockedStay(5,uaWalk); //Wait a moment inside
          CheckForBetterDestination; //Must run before TakenOffer and before taking ware so Offer is still valid
          fFrom.WareTakeFromOut(fWareType);
          CarryGive(fWareType);
          gHands[Owner].Deliveries.Queue.TakenOffer(fDeliverID);
        end;
    3:  begin
          if fFrom.IsDestroyed then //We have the resource, so we don't care if house is destroyed
            SetActionLockedStay(0, uaWalk)
          else
            SetActionGoIn(uaWalk, gdGoOutside, fFrom);
          Inc(fPhase); // jump to phase 5 immidiately
        end;
    4:  begin
          SetActionStay(5, uaWalk); //used only from FindBestDestination
          Thought := thQuest;
        end;
  end;

  if fPhase = 5 then
    TKMUnitSerf(fUnit).Thought := thNone; // Clear possible '?' thought after 4th phase

  //Get out barracks (special case after wait phase inside house, if resource is not available anymore)
  with TKMUnitSerf(fUnit) do
  if fPhase = 120 then
  begin
    SetActionGoIn(uaWalk, gdGoOutside, fFrom); //Step back out
    fPhase := 99; //Exit next run
    Exit;
  end;

  //Deliver into complete house
  if (fDeliverKind = dkToHouse) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
    5:  begin
          SetActionWalkToSpot(fToHouse.GetClosestEntrance(Position).DirFaceloc{fToHouse.PointBelowEntrance});
        end;
    6:  SetActionGoIn(uaWalk, gdGoInside, fToHouse);
    7:  SetActionLockedStay(5, uaWalk); //wait a bit inside
    8:  begin
          fToHouse.WareAddToIn(Carry);
          CarryTake;

          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while trying to GoOut

          //If serf bring smth into the Inn and he is hungry - let him eat immidiately
          if fUnit.IsHungry
            and (fToHouse.HouseType = htInn)
            and TKMHouseInn(fToHouse).HasFood
            and TKMHouseInn(fToHouse).HasSpace
            and TKMUnitSerf(fUnit).GoEat(TKMHouseInn(fToHouse), True) then
            Exit //Exit immidiately, since we created new task here and old task is destroyed!
                 //Changing any task fields here (f.e. Phase) could affect new task!
          else
          //Now look for another delivery from inside this house
          //But only if we are not hungry!
          //Otherwise there is a possiblity when he will go between houses until death
          if not fUnit.IsHungry
            and TKMUnitSerf(fUnit).TryDeliverFrom(fToHouse) then
            Exit //Exit immidiately, since we created new task here and old task is destroyed!
                 //Changing any task fields here (f.e. Phase) could affect new task!
          else
            //No delivery found then just step outside
            SetActionGoIn(uaWalk, gdGoOutside, fToHouse);
        end;
    else Result := trTaskDone;
  end;

  //Deliver into wip house
  if (fDeliverKind = dkToConstruction) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(fToHouse.PointBelowEntrance, uaWalk, 1.42);
    6:  begin
          // Then check if there is a worker hitting house just from the entrance
          Worker := gHands[fUnit.Owner].UnitsHitTest(fToHouse.PointBelowEntrance, utBuilder);
          if (Worker <> nil) and (Worker.Task <> nil)
            and ( (Worker.Task is TKMTaskBuildHouse) or (Worker.Task is TKMTaskBuildHouseUpgrade))
            and (Worker.Task.Phase >= 2) then
            // If so, then allow to bring resources diagonally
            SetActionWalkToSpot(fToHouse.Entrance, uaWalk, 1.42)
          else begin
            // else ask serf to bring resources from point below entrance (not diagonally)

            SetActionWalkToSpot(fToHouse.PointBelowEntrance);

          end;
        end;
    7:  begin
          Direction := KMGetDirection(PositionNext, fToHouse.Entrance);
          fToHouse.WareAddToBuild(Carry);
          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1, uaWalk);
        end;
    else Result := trTaskDone;
  end;
  //Deliver into wip house
  if (fDeliverKind = dkToWall) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(fToHouse.PointBelowEntrance, uaWalk, 1.42);
    6:  begin
          // Then check if there is a worker hitting house just from the entrance
          Worker := gHands[fUnit.Owner].UnitsHitTest(fToHouse.PointBelowEntrance, utBuilder);
          if (Worker <> nil) and (Worker.Task <> nil)
            and ( (Worker.Task is TKMTaskBuildHouse) or (Worker.Task is TKMTaskBuildHouseUpgrade))
            and (Worker.Task.Phase >= 2) then
            // If so, then allow to bring resources diagonally
            SetActionWalkToSpot(fToHouse.Entrance, uaWalk, 1.42)
          else begin
            // else ask serf to bring resources from point below entrance (not diagonally)

            SetActionWalkToSpot(fToHouse.PointBelowEntrance);

          end;
        end;
    7:  begin
          Direction := KMGetDirection(PositionNext, fToHouse.Entrance);
          fToHouse.WareAddToBuild(Carry);
          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1, uaWalk);
        end;
    else Result := trTaskDone;
  end;

  //Deliver to builder or soldier
  if fDeliverKind = dkToUnit then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
    5:  if fToUnit.UnitType in [utBoat, utShip, utBattleShip] then
          SetActionWalkToUnit(fToUnit, 2.99, uaWalk) //only for ships
        else
          SetActionWalkToUnit(fToUnit, 1.42, uaWalk); //When approaching from diagonal
    6:  begin
          //See if the unit has moved. If so we must try again
          if KMLengthDiag(fUnit.Position, fToUnit.Position) > 1.5 then
          begin
            SetActionWalkToUnit(fToUnit, 1.42, uaWalk); //Walk to unit again
            fPhase := 6;
            Exit;
          end;
          //Worker
          if (fToUnit.UnitType = utBuilder) and (fToUnit.Task <> nil) then
          begin
            //todo: Replace phase numbers with enums to avoid hardcoded magic numbers
            // Check if worker is still digging

            if ((fToUnit.Task is TKMTaskBuildWine) and (fToUnit.Task.Phase < 5))
              or ((fToUnit.Task is TKMTaskBuildRoad) and (fToUnit.Task.Phase < 4)) then
            begin
              SetActionLockedStay(5, uaWalk); //wait until worker finish digging process
              fPhase := 6;
              Exit;
            end;
            //build palisade
            if (fToUnit.Task is TKMTaskBuildPalisade) then
            begin

              if (fToUnit.Task.Phase < 4) then
              begin
                SetActionLockedStay(5, uaWalk); //wait until worker finish digging process
                fPhase := 6;
                Exit;
              end;
              //skip Phase to 11 so it costs only 1 timber
              fToUnit.Task.Phase := 11;//fToUnit.Task.Phase + 1;
            end else
            if fToUnit.Task is TKMTaskBuildRoad then
              TKMTaskBuildRoad(fToUnit.Task).AddSupply
            else
              fToUnit.Task.Phase := fToUnit.Task.Phase + 1;

            fToUnit.SetActionLockedStay(1, uaWork1); //Tell the worker to resume work by resetting his action (causes task to execute)
          end;
          //Warrior
          if (fToUnit is TKMUnitWarrior) then
          begin
            case Carry of
              wtBread : fToUnit.Feed(UNIT_MAX_CONDITION * 0.8); //Feed the warrior
              wtApple : fToUnit.Feed(UNIT_MAX_CONDITION * 0.75); //Feed the warrior
              wtSausage : fToUnit.Feed(UNIT_MAX_CONDITION * 0.9); //Feed the warrior
              wtFish : fToUnit.Feed(UNIT_MAX_CONDITION * 0.85); //Feed the warrior
              wtWine : fToUnit.Feed(UNIT_MAX_CONDITION * 0.75); //Feed the warrior

              wtBolt,
              wtStoneBolt,
              wtQuiver : TKMUnitWarrior(fToUnit).ReloadAmmo(Carry);
            end;
            case Carry of
              wtFood,
              wtBread,
              wtApple,
              wtSausage,
              wtFish,
              wtWine : begin
                        TKMUnitWarrior(fToUnit).RequestedFood := False;
                        //gHands[Owner].Deliveries.Queue.RemDemand(fToUnit, [wtBread, wtApple, wtSausage, wtFish, wtFood]);
                      end;

              wtBolt,
              wtStoneBolt,
              wtQuiver : begin
                          TKMUnitWarrior(fToUnit).RequestedAmmo := false;
                          //gHands[Owner].Deliveries.Queue.RemDemand(fToUnit, [wtBolt, wtStoneBolt, wtQuiver]);
                        end;
            end;

          end;

          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while staying
          SetActionLockedStay(5, uaWalk); //Pause breifly (like we are handing over the ware/food)
        end;
    7:  begin
          //After feeding troops, serf should walk away, but ToUnit could be dead by now
          if (fToUnit is TKMUnitWarrior) then
          begin
            if TKMUnitSerf(fUnit).TryDeliverFrom(nil) then
              Exit //Exit immidiately, since we created new task here and old task is destroyed!
                   //Changing any task fields (f.e. Phase) here could affect new task!
            else
              //No delivery found then just walk back to our From house
              //even if it's destroyed, its location is still valid
              //Don't walk to spot as it doesn't really matter
              SetActionWalkToHouse(fFrom, 5);
          end else
            SetActionStay(0, uaWalk); //If we're not feeding a warrior then ignore this step
        end;
    else Result := trTaskDone;
  end;

  if (fDeliverKind = dkToStructure) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(ToStruct.Position, uaWalk, 2);
    6:  SetActionStay(1, uaWalk);
    7:  begin
          Direction := KMGetDirection(PositionNext, fToLoc);
          gHands[Owner].Stats.WareConsumed(Carry);
          ToStruct.DeliverWare(Carry, 1);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1, uaWalk);
        end;
    else Result := trTaskDone;
  end;

  //Deliver into wip house
  if (fDeliverKind = dkOther) then
  with TKMUnitSerf(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(fToLoc, uaWalk, 2);
    6:  SetActionStay(1, uaWalk);
    7:  begin
          Direction := KMGetDirection(PositionNext, fToLoc);
          gHands[Owner].Stats.WareConsumed(Carry);
          CarryTake;
          gHands[Owner].Deliveries.Queue.GaveDemand(fDeliverID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(fDeliverID);
          fDeliverID := DELIVERY_NO_ID; //So that it can't be abandoned if unit dies while staying
          SetActionStay(1, uaWalk);
        end;
    else Result := trTaskDone;
  end;

  Inc(fPhase);
end;


function TKMTaskDeliver.ObjToString(const aSeparator: String = ', '): String;
var
  FromStr, ToUStr, ToHStr: String;
begin
  if Self = nil then Exit('nil');

  FromStr := 'nil';
  ToHStr := 'nil';
  ToUStr := 'nil';

  if fFrom <> nil then
    FromStr := fFrom.ObjToStringShort(',');

  if fToHouse <> nil then
    ToHStr := fToHouse.ObjToStringShort(',');

  if fToUnit <> nil then
    ToUStr := fToUnit.ObjToStringShort(',');

  Result := inherited +
            Format('%s|FromH = [%s]%s|ToH = [%s]%sFromU = [%s]%s|WareT = %s',
                   [aSeparator,
                    FromStr, aSeparator,
                    ToHStr, aSeparator,
                    ToUStr, aSeparator,
                    GetEnumName(TypeInfo(TKMWareType), Integer(fWareType)), aSeparator]);
end;

procedure TKMTaskDeliver.UpdateState;
begin
  if fUnit.Position = fUnit.PositionNext then
    Inc(fTicker)
  else
    fTicker := 0;
end;

procedure TKMTaskDeliver.Paint;
begin
  if SHOW_UNIT_ROUTES
    and (gMySpectator.LastSelected = fUnit) then
    begin
      if fFrom <> nil then
        gRenderAux.RenderWireTile(fFrom.PointBelowEntrance, icDarkBlue);

      if fToHouse <> nil then
        gRenderAux.RenderWireTile(fToHouse.PointBelowEntrance, icLightRed);
      if fToUnit <> nil then
        gRenderAux.RenderWireTile(fToUnit.Position, icRed);
    end;

end;



{TKMTaskWHDeliver}
constructor TKMTaskWHDeliver.Create(aWHMan: TKMUnitWHMan);
var WH : TKMUnitWHMan;
begin
  inherited Create(aWHMan);
  fType := uttWHDeliver;

  WH := TKMUnitWHMan(aWHMan);

  fFrom := WH.Delivery.FromHouse.GetPointer;
  fIndex := WH.GetValidDeliveryIndex;
  If fIndex <> DELIVERY_NO_ID then
    SetDeliveryKind;
end;

constructor TKMTaskWHDeliver.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
end;

procedure TKMTaskWHDeliver.SyncLoad;
begin
  Inherited;
end;

procedure TKMTaskWHDeliver.Save(SaveStream: TKMemoryStream);
begin
  inherited;
end;


destructor TKMTaskWHDeliver.Destroy;
var I : Integer;
begin
  gHands.CleanUpHousePointer(fFrom);
  TKMUnitWHMan(fUnit).HasDelivery := false;
  TKMUnitWHMan(fUnit).ResetFromHouse;
  if fUnit <> nil then
    with TKMUnitWHMan(fUnit) do
    begin
      for I := 0 to MAX_WH_WARES - 1 do
      begin
        if Delivery.List[I].ID <> DELIVERY_NO_ID then
        begin
          gHands[fUnit.Owner].Deliveries.Queue.AbandonDelivery(Delivery.List[I].ID);
        end;
        ResetDelivery(I);
      end;
    end;

  Inherited;
end;

procedure TKMTaskWHDeliver.SetDeliveryKind;
var WH : TKMUnitWHMan;
begin
  WH := TKMUnitWHMan(fUnit);
  fDeliverKind := dkNone;
  If WH.Delivery.List[fIndex].ToHouse <> nil then
  begin
    If WH.Delivery.List[fIndex].ToHouse.HouseType in WALL_HOUSES then
      fDeliverKind := dkToWall
    else
    If WH.Delivery.List[fIndex].ToHouse.IsComplete then
      fDeliverKind := dkToHouse
    else
      fDeliverKind := dkToConstruction;
  end else
  If WH.Delivery.List[fIndex].ToUnit <> nil then
    fDeliverKind := dkToUnit
  else
  If WH.Delivery.List[fIndex].ToStruct <> nil then
    fDeliverKind := dkToStructure
  else
  If WH.Delivery.List[fIndex].ToLoc <> KMPOINT_ZERO then
    fDeliverKind := dkOther;
  Assert(fDeliverKind <> dkNone, 'TKMTaskWHDeliver.SetDeliveryKind');
end;

procedure TKMTaskWHDeliver.CheckForBetterDestination(aID : Byte);
var
  NewToHouse: TKMHouse;
  NewToUnit: TKMUnit;
  newStruct : TKMStructure;
  delID : Integer;
  WH : TKMUnitWHMan;
  WT : TKMWareType;
begin
  WH := TKMUnitWHMan(fUnit);
  delID := WH.Delivery.List[aID].ID;
  gHands[fUnit.Owner].Deliveries.Queue.CheckForBetterDemand(delID, NewToHouse, NewToUnit, newStruct, fUnit);
  WT := gHands[fUnit.Owner].Deliveries.Queue.Delivery[delID].DemandWare;
  WH.SetNewDelivery(aID, NewToHouse, NewToUnit, newStruct, KMPOINT_ZERO,
                    WT, delID);
  SetDeliveryKind;
end;

function TKMTaskWHDeliver.FindBestDestination: Boolean;
var
  NewToHouse: TKMHouse;
  NewToUnit: TKMUnit;
  NewToStructure: TKMStructure;
  delID : Integer;
  WH : TKMUnitWHMan;
begin
  if fPhase <= 2 then
  begin
    Result := False;
    Exit;
  end else
  if InRange(fPhase, 3, 3) then
  begin
    Result := True;
    Exit;
  end;

  WH := TKMUnitWHMan(fUnit);
  delID := WH.Delivery.List[fIndex].ID;
  fForceDelivery := False; //Reset ForceDelivery from previous runs
  gHands[fUnit.Owner].Deliveries.Queue.DeliveryFindBestDemand(TKMUnitSerf(fUnit), delID, WH.Delivery.List[fIndex].Ware,
                                                              NewToHouse, NewToUnit, NewToStructure, fForceDelivery);

  Result := NewToHouse.IsValid or (NewToUnit <> nil) or (NewToStructure <> nil);
  WH.SetNewDelivery(fIndex, NewToHouse, NewToUnit, NewToStructure, KMPOINT_ZERO,
                    WH.Delivery.List[fIndex].Ware, delID);
  SetDeliveryKind;

  If fDeliverKind <> dkNone then
    if fPhase > 4 then
      fPhase := 4;
end;

function TKMTaskWHDeliver.SetValidIndex: Boolean;
begin
  fIndex := TKMUnitWHMan(fUnit).GetValidDeliveryIndex;
  Result := fIndex <> -1;
end;

function TKMTaskWHDeliver.CanAbandonWalk: Boolean;
begin
  case fDeliverKind of
    dkToHouse:         Result := fPhase <= 8;
    dkToWall,
    dkToConstruction:  Result := fPhase <= 7;
    dkToUnit:          Result := fPhase <= 6;
    dkToStructure,
    dkOther:          Result := fPhase <= 6;
  else
    raise Exception.Create('Unexpected type');
  end;
end;

function TKMTaskWHDeliver.WalkShouldAbandon: Boolean;
var
  delID : Integer;
  WH : TKMUnitWHMan;
  delivery : TKMWHDeliver;
begin
  Result := False;

  if not CanAbandonWalk then
    Exit;

  WH := TKMUnitWHMan(fUnit);
  delID := WH.Delivery.List[fIndex].ID;
  delivery := WH.Delivery.List[fIndex];

  if not gHands[fUnit.Owner].Deliveries.Queue.IsDeliveryAlowed(delID) then
    Exit(True);

  //After step 2 we don't care if From is destroyed or doesn't have the ware
  if fPhase <= 2 then
    Result := Result
                or fFrom.IsDestroyed
                or fFrom.ShouldAbandonDeliveryFrom(delivery.Ware)
                or fFrom.ShouldAbandonDeliveryFromTo(delivery.ToHouse, delivery.Ware, fPhase = 2); //Make immidiate check only on Phase 2 (inside house)

  Result := Result or CanRestartAction(fLastActionResult);

  //do not abandon the delivery if target is destroyed/dead, we will find new target later
  case fDeliverKind of
    dkToHouse:        begin

                        Result := Result or delivery.ToHouse.IsDestroyed;
                        Result := Result or (not fForceDelivery and delivery.ToHouse.ShouldAbandonDeliveryTo(delivery.Ware));

                        if Result then
                          if (delivery.ToHouse.HouseType = htTownhall) and not delivery.ToHouse.IsValid then
                            Result := Result and (fPhase < 5);
                      end;
    dkToStructure: Result := Result or delivery.ToStruct.IsComplete or delivery.ToStruct.IsDestroyed;
    dkToWall,
    dkToConstruction: if not delivery.ToHouse.IsUpgrading then
                        Result := Result or delivery.ToHouse.IsComplete;
    dkToUnit:         begin
                        Result := Result or (delivery.ToUnit = nil) or delivery.ToUnit.IsDeadOrDying;
                      end;
  end;
end;

function TKMTaskWHDeliver.CanRestartAction(aLastActionResult: TKMActionResult): Boolean;
begin
  Result := (aLastActionResult = arActCanNotStart)
            and (fDeliverKind = dkToHouse)
            and (fPhase - 1 = 6); //Serf tried to get inside destination house
end;

function TKMTaskWHDeliver.Execute: TKMTaskResult;
var I : Integer;
  inHouse : TKMHouse;
  toUnit : TKMUnit;
  toStruct : TKMStructure;
  del : TKMWHDelivery;
  carry : TKMWareType;
  Worker : TKMUnit;
begin
  Result := trTaskContinues;
  if WalkShouldAbandon and fUnit.Visible and not SetValidIndex {not FindBestDestination} then
    Exit(trTaskDone);

  del := TKMUnitWHMan(fUnit).Delivery;
  with TKMUnitWHMan(fUnit) do
  case fPhase of
    0:  begin
          SetActionWalkToSpot(fFrom.GetClosestEntrance(Position).DirFaceloc);
        end;
    1:  begin
          SetActionGoIn(uaWalk, gdGoInside, fFrom);
        end;
    2:  begin
          //Serf is inside house now.
          //Barracks can consume the resource (by equipping) before we arrive
          //All houses can have resources taken away by script at any moment
          if fFrom.ShouldAbandonDeliveryFrom(del.List[fIndex].Ware)
             or fFrom.ShouldAbandonDeliveryFromTo(del.List[fIndex].ToHouse, del.List[fIndex].Ware, True) then //For store evacuation
          begin
            SetActionLockedStay(5, uaWalk); //Wait a moment inside
            fPhase := 120; //Will get out of Barracks onthat Phase
            Exit;
          end;
          SetActionLockedStay(5,uaWalk); //Wait a moment inside
          fIndex := GetValidDeliveryIndex;
          for I := 0 to MAX_WH_WARES - 1 do
          begin
            If not del.List[I].Valid then
              Continue;
            CheckForBetterDestination(I); //Must run before TakenOffer and before taking ware so Offer is still valid
            fFrom.WareTakeFromOut(del.List[I].Ware);
            CarryGive(del.List[I].Ware);
            gHands[Owner].Deliveries.Queue.TakenOffer(del.List[I].ID);
          end;
        end;
    3:  begin
          if fFrom.IsDestroyed then //We have the resource, so we don't care if house is destroyed
            SetActionLockedStay(0, uaWalk)
          else
            SetActionGoIn(uaWalk, gdGoOutside, fFrom);
          Inc(fPhase); // jump to phase 5 immidiately
        end;
    4:  begin
          SetActionStay(5, uaWalk); //used only from FindBestDestination
          Thought := thQuest;
        end;
  end;

  //Deliver into complete house
  if (fDeliverKind = dkToHouse) then
  with TKMUnitWHMan(fUnit) do
  case fPhase of
    0..4:;
    5:  begin
          SetActionWalkToSpot(del.List[fIndex].ToHouse.GetClosestEntrance(Position).DirFaceloc);
        end;
    6:  SetActionGoIn(uaWalk, gdGoInside, del.List[fIndex].ToHouse);
    7:  SetActionLockedStay(5, uaWalk); //wait a bit inside
    8:  begin
          inHouse := del.List[fIndex].ToHouse;
          //give all wares to the same house
          for I := 0 to MAX_WH_WARES - 1 do
          begin
            If not del.List[I].Valid
            or not gHands[Owner].Deliveries.Queue.IsDeliveryAlowed(del.List[I].ID)
            and (inHouse = del.List[I].ToHouse) then
              Continue;

            inHouse.WareAddToIn(del.List[I].Ware);
            gHands[Owner].Deliveries.Queue.GaveDemand(del.List[I].ID);
            gHands[Owner].Deliveries.Queue.AbandonDelivery(del.List[I].ID);
            CarryTake(del.List[I].Ware);
            ResetDelivery(I);
          end;
          SetActionGoIn(uaWalk, gdGoOutside, inHouse);
        end;
      9:  begin
            fIndex := GetValidDeliveryIndex;
            If fIndex <> -1 then
            begin
              fPhase := 4;
              SetDeliveryKind;
            end;
            SetActionLockedStay(0, uaWalk);
          end
    else Result := trTaskDone;
  end;

  //Deliver into wip house
  if (fDeliverKind = dkToConstruction) then
  with TKMUnitWHMan(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(del.List[fIndex].ToHouse.PointBelowEntrance, uaWalk, 1.42);
    6:  begin
          // Then check if there is a worker hitting house just from the entrance
          Worker := gHands[fUnit.Owner].UnitsHitTest(del.List[fIndex].ToHouse.PointBelowEntrance, utBuilder);
          if (Worker <> nil) and (Worker.Task <> nil)
            and ( (Worker.Task is TKMTaskBuildHouse) or (Worker.Task is TKMTaskBuildHouseUpgrade))
            and (Worker.Task.Phase >= 2) then
            // If so, then allow to bring resources diagonally
            SetActionWalkToSpot(del.List[fIndex].ToHouse.Entrance, uaWalk, 1.42)
          else begin
            // else ask serf to bring resources from point below entrance (not diagonally)

            SetActionWalkToSpot(del.List[fIndex].ToHouse.PointBelowEntrance);

          end;
        end;
    7:  begin
          inHouse := del.List[fIndex].ToHouse;
          Direction := KMGetDirection(PositionNext, inHouse.Entrance);
          inHouse.WareAddToBuild(del.List[fIndex].Ware);
          gHands[Owner].Stats.WareConsumed(del.List[fIndex].Ware);

          //give all wares to the same house
          for I := 0 to MAX_WH_WARES - 1 do
          begin
            If not del.List[I].Valid
            or not gHands[Owner].Deliveries.Queue.IsDeliveryAlowed(del.List[I].ID) then
              Continue;
            inHouse.WareAddToBuild(del.List[I].Ware);
            gHands[Owner].Deliveries.Queue.GaveDemand(del.List[I].ID);
            gHands[Owner].Deliveries.Queue.AbandonDelivery(del.List[I].ID);
            CarryTake(del.List[I].Ware);
            ResetDelivery(I);
          end;
          SetActionStay(1, uaWalk);
        end;
      8:  begin
            fIndex := GetValidDeliveryIndex;
            If fIndex <> -1 then
            begin
              fPhase := 4;
              SetDeliveryKind;
            end;
            SetActionLockedStay(0, uaWalk);
          end
    else Result := trTaskDone;
  end;
  //Deliver to builder or soldier
  if fDeliverKind = dkToUnit then
  begin
    If fPhase < 7 then
    begin
      toUnit := del.List[fIndex].ToUnit;
      carry := del.List[fIndex].Ware;
    end;
    with TKMUnitWHMan(fUnit) do
    case fPhase of
      0..4:;
      5:  if toUnit.UnitType in [utBoat, utShip, utBattleShip] then
            SetActionWalkToUnit(toUnit, 2.99, uaWalk) //only for ships
          else
            SetActionWalkToUnit(toUnit, 1.42, uaWalk); //When approaching from diagonal
      6:  begin
            //See if the unit has moved. If so we must try again
            if KMLengthDiag(fUnit.Position, toUnit.Position) > 1.5 then
            begin
              SetActionWalkToUnit(toUnit, 1.42, uaWalk); //Walk to unit again
              fPhase := 6;
              Exit;
            end;
            //Worker
            if (toUnit.UnitType = utBuilder) and (toUnit.Task <> nil) then
            begin
              //todo: Replace phase numbers with enums to avoid hardcoded magic numbers
              // Check if worker is still digging

              if ((toUnit.Task is TKMTaskBuildWine) and (toUnit.Task.Phase < 5))
                or ((toUnit.Task is TKMTaskBuildRoad) and (toUnit.Task.Phase < 4)) then
              begin
                SetActionLockedStay(5, uaWalk); //wait until worker finish digging process
                fPhase := 6;
                Exit;
              end;
              //build palisade
              if (toUnit.Task is TKMTaskBuildPalisade) then
              begin

                if (toUnit.Task.Phase < 4) then
                begin
                  SetActionLockedStay(5, uaWalk); //wait until worker finish digging process
                  fPhase := 6;
                  Exit;
                end;
                //skip Phase to 11 so it costs only 1 timber
                toUnit.Task.Phase := 11;//fToUnit.Task.Phase + 1;
              end else
              if toUnit.Task is TKMTaskBuildRoad then
                TKMTaskBuildRoad(toUnit.Task).AddSupply
              else
                toUnit.Task.Phase := toUnit.Task.Phase + 1;

              toUnit.SetActionLockedStay(1, uaWork1); //Tell the worker to resume work by resetting his action (causes task to execute)
            end;
            //Warrior
            if (toUnit is TKMUnitWarrior) then
            begin
              case carry of
                wtBread : toUnit.Feed(UNIT_MAX_CONDITION * 0.8); //Feed the warrior
                wtSausage : toUnit.Feed(UNIT_MAX_CONDITION * 0.7); //Feed the warrior
                wtFish : toUnit.Feed(UNIT_MAX_CONDITION * 1); //Feed the warrior
                wtWine : toUnit.Feed(UNIT_MAX_CONDITION * 0.75); //Feed the warrior

                wtBolt,
                wtStoneBolt,
                wtQuiver : TKMUnitWarrior(toUnit).ReloadAmmo(carry);
              end;
              case carry of
                wtFood,
                wtBread,
                wtApple,
                wtSausage,
                wtFish,
                wtWine : begin
                          TKMUnitWarrior(toUnit).RequestedFood := False;
                          //gHands[Owner].Deliveries.Queue.RemDemand(fToUnit, [wtBread, wtApple, wtSausage, wtFish, wtFood]);
                        end;

                wtBolt,
                wtStoneBolt,
                wtQuiver : begin
                            TKMUnitWarrior(toUnit).RequestedAmmo := false;
                            //gHands[Owner].Deliveries.Queue.RemDemand(fToUnit, [wtBolt, wtStoneBolt, wtQuiver]);
                          end;
              end;

            end;

            gHands[Owner].Stats.WareConsumed(Carry);
            CarryTake(carry);
            gHands[Owner].Deliveries.Queue.GaveDemand(del.List[fIndex].ID);
            gHands[Owner].Deliveries.Queue.AbandonDelivery(del.List[fIndex].ID);
            ResetDelivery(fIndex);
            SetActionLockedStay(5, uaWalk); //Pause breifly (like we are handing over the ware/food)
          end;
      7:  begin
            fIndex := GetValidDeliveryIndex;
            If fIndex <> -1 then
            begin
              fPhase := 4;
              SetDeliveryKind;
              SetActionLockedStay(0, uaWalk);
            end else
              SetActionWalkToSpot(fFrom.GetClosestEntrance(Position).DirFaceloc, uaWalk, 5);
          end
      else Result := trTaskDone;
    end;

  end;

  if (fDeliverKind = dkToStructure) then
  begin
    If fPhase < 8 then
    toStruct := del.List[fIndex].ToStruct;
    with TKMUnitWHMan(fUnit) do
    case fPhase of
      0..4:;
          // First come close to point below house entrance
      5:  SetActionWalkToSpot(toStruct.Position, uaWalk, 2);
      6:  SetActionStay(1, uaWalk);
      7:  begin
            Direction := KMGetDirection(PositionNext, toStruct.Position);
            //give all wares to the same house
            for I := 0 to MAX_WH_WARES - 1 do
            begin
              If not del.List[I].Valid
              or not gHands[Owner].Deliveries.Queue.IsDeliveryAlowed(del.List[I].ID) then
                Continue;

              ToStruct.DeliverWare(Carry, 1);
              gHands[Owner].Deliveries.Queue.GaveDemand(del.List[I].ID);
              gHands[Owner].Deliveries.Queue.AbandonDelivery(del.List[I].ID);
              CarryTake(del.List[I].Ware);
              ResetDelivery(I);
            end;
            SetActionStay(1, uaWalk);
          end;
        8:  begin
              fIndex := GetValidDeliveryIndex;
              If fIndex <> -1 then
              begin
                fPhase := 4;
                SetDeliveryKind;
              end;
              SetActionLockedStay(0, uaWalk);
            end
      else Result := trTaskDone;
    end;
  end;

  //Deliver into wip house
  if (fDeliverKind = dkOther) then
  with TKMUnitWHMan(fUnit) do
  case fPhase of
    0..4:;
        // First come close to point below house entrance
    5:  SetActionWalkToSpot(del.List[fIndex].ToLoc, uaWalk, 2);
    6:  SetActionStay(1, uaWalk);
    7:  begin
          Direction := KMGetDirection(PositionNext, del.List[fIndex].ToLoc);
          gHands[Owner].Stats.WareConsumed(del.List[fIndex].Ware);
          CarryTake(del.List[fIndex].Ware);
          gHands[Owner].Deliveries.Queue.GaveDemand(del.List[fIndex].ID);
          gHands[Owner].Deliveries.Queue.AbandonDelivery(del.List[fIndex].ID);
          ResetDelivery(fIndex);
          SetActionStay(1, uaWalk);
        end;
    8:  begin
          fIndex := GetValidDeliveryIndex;
          If fIndex <> -1 then
          begin
            fPhase := 4;
            SetDeliveryKind;
          end;
          SetActionLockedStay(0, uaWalk);
        end
    else Result := trTaskDone;
  end;

  inc(fPhase);
end;

function TKMTaskWHDeliver.ObjToString(const aSeparator: string = ', '): string;
begin
  Result := Inherited;
end;

end.
