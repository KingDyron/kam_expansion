unit KM_UnitActionGoOutDock;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Houses, KM_Units;


type
  TKMBestExit = (beNone, beLeft, beCenter, beRight);

  {This is a [fairly :P] simple action making unit go inside/outside of house}
  TKMUnitActionGoOutDock = class(TKMUnitAction)
  private
    fStep: Single;
    fHouse: TKMHouse;
    fDock: TKMPointDir;
    fOutDockSpot: TKMPointDir;
    fInitiated: Boolean;
    fPushedUnit: TKMUnit;
    fWaitingForPush: Boolean;
    fIsTrained : Boolean;
    function FindBestExit(const aLoc: TKMPointDir): TKMBestExit;
    function TileHasIdleUnitToPush(X,Y: Word): TKMUnit;
    procedure WalkOut;
    function GetIsStarted: Boolean;
    function IsWalkingStraight : Boolean;
  public
    OnWalkedOut: TKMHouseTrainedEvent; //NOTE: Caller must sync these events after loading, used with caution
    constructor Create(aUnit: TKMUnit; aAction: TKMUnitActionType; aHouse: TKMHouse; aDock : TKMPointDir; aIsTrained : Boolean = false);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function ActName: TKMUnitActionName; override;
    function CanBeInterrupted(aForced: Boolean = True): Boolean; override;
    function GetExplanation: UnicodeString; override;
    property IsStarted: Boolean read GetIsStarted; // Is unit actually started exiting or going inside?
    function GetDoorwaySlide(aCheck: TKMCheckAxis): Single;
    function GetDoorwaySlides: TKMPointF;
    property IsTrained : Boolean read fIsTrained;
    function Execute: TKMActionResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_Resource, KM_Terrain, KM_UnitActionStay, KM_UnitActionWalkTo,
  KM_TerrainTypes,
  KM_HouseBarracks, KM_ResHouses, KM_ResUnits, KM_CommonUtils, KM_GameParams,
  KM_ResTypes;


{ TUnitActionGoInOut }
constructor TKMUnitActionGoOutDock.Create(aUnit: TKMUnit; aAction: TKMUnitActionType; aHouse: TKMHouse; aDock : TKMPointDir; aIsTrained : Boolean = false);
begin
  inherited Create(aUnit, aAction, True);

  //We might stuck trying to exit when house gets destroyed (1)
  //and we might be dying in destroyed house (2)
  fHouse          := aHouse.GetPointer;
  fInitiated      := False;
  fWaitingForPush := False;
  fDock := aDock;

  fStep := 0; //go Outside (one cell down)
  fIsTrained := aIsTrained;
end;


constructor TKMUnitActionGoOutDock.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('UnitActionGoInOut');
  LoadStream.Read(fStep);
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fPushedUnit, 4);
  LoadStream.Read(fDock);
  LoadStream.Read(fOutDockSpot);
  LoadStream.Read(fInitiated);
  LoadStream.Read(fWaitingForPush);
  LoadStream.Read(fIsTrained);
end;


procedure TKMUnitActionGoOutDock.SyncLoad;
begin
  inherited;

  fHouse := gHands.GetHouseByUID(Integer(fHouse));
  fPushedUnit := gHands.GetUnitByUID(Integer(fPushedUnit));
end;


destructor TKMUnitActionGoOutDock.Destroy;
begin

  gHands.CleanUpHousePointer(fHouse);
  gHands.CleanUpUnitPointer(fPushedUnit);


  if (fUnit <> nil)
    and fInitiated
    and (gTerrain.Land^[fUnit.PositionNext.Y, fUnit.PositionNext.X].IsUnit = fUnit) then
  begin
    if not fUnit.Visible then
    begin
      gTerrain.UnitRem(fUnit.PositionNext);
      if fDock = KMPOINT_ZERO then
        fUnit.PositionF := KMPointF(fDock); //Put us back inside the house
    end;
  end;

  if fUnit.Visible then
    fUnit.InHouse := nil; //We are not in any house now

  inherited;
end;


function TKMUnitActionGoOutDock.ActName: TKMUnitActionName;
begin
  Result := uanGoOutDock;
end;


function TKMUnitActionGoOutDock.GetExplanation: UnicodeString;
begin
  Result := 'Leaving the dock';
end;


// Is unit actually started exiting or going inside?
function TKMUnitActionGoOutDock.GetIsStarted: Boolean;
begin
  Result := fInitiated and not fWaitingForPush;
end;

function TKMUnitActionGoOutDock.IsWalkingStraight : Boolean;
begin
  Result := (fDock.Y - fOutDockSpot.Y = 0) or (fDock.X - fOutDockSpot.X = 0);
end;

//Attempt to find a tile below the door (on the street) we can walk to
//We can push idle units away. Check center first
function TKMUnitActionGoOutDock.FindBestExit(const aLoc: TKMPointDir): TKMBestExit;

  function ChooseBestExit(aL, aR: Boolean): TKMBestExit;
  begin
    //Choose randomly between left and right
    if (aL and aR) then
    begin
      if KaMRandom(2, 'TKMUnitActionGoInOut.FindBestExit.ChooseBestExit') = 0 then
        Result := beLeft
      else
        Result := beRight;
    end
    else
    if aL then
      Result := beLeft
    else
    if aR then
      Result := beRight
    else
      Result := beNone;
  end;

var
  U, UC, UL, UR: TKMUnit;
  L, R: Boolean;
begin
  if fUnit.CanStepTo(aLoc.X, aLoc.Y, tpFish) then
    Result := beCenter
  else
  begin
    case aLoc.Dir of
      dirS,
      dirN: begin
              L := fUnit.CanStepTo(aLoc.X-1, aLoc.Y, tpFish);
              R := fUnit.CanStepTo(aLoc.X+1, aLoc.Y, tpFish);
            end;
      dirW,
      dirE: begin
              L := fUnit.CanStepTo(aLoc.X, aLoc.Y-1, tpFish);
              R := fUnit.CanStepTo(aLoc.X, aLoc.Y+1, tpFish);
            end;
      else Raise Exception.Create('Wrong Direction, TKMUnitActionGoInOut.FindBestExit');
    end;
    //L := fUnit.CanStepTo(aLoc.X-1, aLoc.Y, tpWalk);
    //R := fUnit.CanStepTo(aLoc.X+1, aLoc.Y, tpWalk);

    Result := ChooseBestExit(L, R);

    if Result = beNone then
    begin
      U := nil;
      UL := nil;
      UR := nil;
      //U could be nil if tile is unwalkable for some reason
      UC := TileHasIdleUnitToPush(aLoc.X, aLoc.Y);
      if UC <> nil then
        Result := beCenter
      else
      begin
        UL := TileHasIdleUnitToPush(aLoc.X-1, aLoc.Y);
        L := UL <> nil;
        UR := TileHasIdleUnitToPush(aLoc.X+1, aLoc.Y);
        R := UR <> nil;
        Result := ChooseBestExit(L, R);
      end;

      case Result of
        beCenter: U := UC;
        beLeft:   U := UL;
        beRight:  U := UR;
      end;

      if U <> nil then
      begin
        fPushedUnit := U.GetPointer;
        fPushedUnit.SetActionWalkPushed(gTerrain.GetOutOfTheWay(U, KMPOINT_ZERO, tpFish));
      end;
    end;
  end;
end;


//Check that tile is walkable and there's no unit blocking it or that unit can be pushed away
function TKMUnitActionGoOutDock.TileHasIdleUnitToPush(X,Y: Word): TKMUnit;
var
  U: TKMUnit;
begin
  Result := nil;

  if gTerrain.TileInMapCoords(X,Y)
    and (gTerrain.CheckPassability(KMPoint(X,Y), fUnit.DesiredPassability))
    and (gTerrain.CanWalkDiagonally(fUnit.Position, X, Y))
    and (gTerrain.Land^[Y,X].IsUnit <> nil) then //If there's some unit we need to do a better check on him
  begin
    U := gTerrain.UnitsHitTest(X,Y); //Let's see who is standing there

    //Check that the unit is idling and not an enemy, so that we can push it away
    if (U <> nil)
    and not U.IsAnimal // Can't push animals
    and (U.Action is TKMUnitActionStay)
    and not TKMUnitActionStay(U.Action).Locked
    and (gHands.CheckAlliance(U.Owner, fUnit.Owner) = atAlly) then
      Result := U;
  end;
end;

//Start walking out of the house. unit is no longer in the house
procedure TKMUnitActionGoOutDock.WalkOut;
begin
  fUnit.Visible := false;
  fUnit.Direction := fDock.Dir;
  fUnit.PositionF := KMPointF(fDock);
  fUnit.PositionNext := fOutDockSpot.Loc;
  gTerrain.UnitAdd(fUnit.PositionNext, fUnit); //Unit was not occupying tile while inside
end;


function TKMUnitActionGoOutDock.GetDoorwaySlide(aCheck: TKMCheckAxis): Single;
begin
  Result := 0;
end;


function TKMUnitActionGoOutDock.GetDoorwaySlides: TKMPointF;
begin
  Result := KMPointF(0, 0);
end;


function TKMUnitActionGoOutDock.Execute: TKMActionResult;
var
  U: TKMUnit;
  distance: Single;
begin
  Result := arActContinues;

  if not fInitiated then
  begin
    //fStreet := KMPointDir(fUnit.Position.X, fUnit.Position.Y + 1 - Round(fStep), dirN);
    fOutDockSpot.Loc := fDock.DirFaceLoc;
    //inverted direction
    fOutDockSpot.Dir := fDock.Dir;

    case FindBestExit(fOutDockSpot) of
      beLeft:     case fDock.Dir of
                    dirS,
                    dirN: fOutDockSpot.X := fOutDockSpot.X-1;
                    dirW,
                    dirE: fOutDockSpot.Y := fOutDockSpot.Y-1;
                    else Raise Exception.Create('');
                  end;
      beCenter:  ;
      beRight:   case fDock.Dir of
                    dirS,
                    dirN: fOutDockSpot.X := fOutDockSpot.X+1;
                    dirW,
                    dirE: fOutDockSpot.Y := fOutDockSpot.Y+1;
                    else Raise Exception.Create('');
                  end;
      beNone:    Exit; //All street tiles are blocked by busy units. Do not exit the house, just wait
    end;

    //If we have pushed an idling unit, wait till it goes away
    //Wait until our push request is dealt with before we move out
    if (fPushedUnit <> nil) then
    begin
      fWaitingForPush := True;
      fInitiated := True;
      Exit;
    end
    else
      WalkOut; //Otherwise we can walk out now

    fInitiated := True;
  end;



  if fWaitingForPush then
  begin
    U := gTerrain.Land^[fOutDockSpot.Y,fOutDockSpot.X].IsUnit;
    if (U = nil) then //Unit has walked away
    begin
      fWaitingForPush := False;
      gHands.CleanUpUnitPointer(fPushedUnit);
      WalkOut;
    end
    else
    begin //There's still some unit - we can't go outside
      if (U <> fPushedUnit) //The unit has switched places with another one, so we must start again
        or not (U.Action is TKMUnitActionWalkTo) //Unit was interupted (no longer pushed), so start again
        or not TKMUnitActionWalkTo(U.Action).WasPushed then
      begin
        fInitiated := False;
        fWaitingForPush := False;
        gHands.CleanUpUnitPointer(fPushedUnit);
      end;
      Exit;
    end;
  end;

  Assert((fHouse <> nil) or (fOutDockSpot.X = fDock.X) or (fOutDockSpot.Y = fDock.Y)); //Must always go in/out the entrance of the house

  //Actual speed is slower if we are moving diagonally, due to the fact we are moving in X and Y
  distance := fUnit.GetEffectiveWalkSpeed(not IsWalkingStraight) / 2;
  fUnit.IsExchanging := fStep <= 1;
  fStep := fStep + distance;
  fUnit.PositionF := KMLerp(fDock, fOutDockSpot, fStep);
  fUnit.Visible := (fHouse = nil) or (fHouse.IsDestroyed) or (fStep > 0.1); //Make unit invisible when it's inside of House

  fUnit.Direction := KMGetDirection(fDock, fOutDockSpot);
  if (fStep >= 1) then
  begin
    Result := arActDone;
    fUnit.IsExchanging := False;

    fUnit.PositionF := KMPointF(fOutDockSpot);
    fUnit.InHouse := nil; //We are not in a house any longer
    if Assigned(OnWalkedOut) then
      OnWalkedOut(fHouse, fIsTrained);
    fUnit.IsExchanging := false;
  end
  else
    Inc(fUnit.AnimStep);
end;


procedure TKMUnitActionGoOutDock.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('UnitActionGoInOut');
  SaveStream.Write(fStep);
  SaveStream.Write(fHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fPushedUnit.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fDock);
  SaveStream.Write(fOutDockSpot);
  SaveStream.Write(fInitiated);
  SaveStream.Write(fWaitingForPush);
  SaveStream.Write(fIsTrained);
end;


function TKMUnitActionGoOutDock.CanBeInterrupted(aForced: Boolean = True): Boolean;
begin
  Result := not Locked; //Never interupt leaving barracks
end;


end.

