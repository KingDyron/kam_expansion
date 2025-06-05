unit KM_UnitActionSteer;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_Defaults, KM_CommonClasses, KM_Units, KM_Points;

{Steer in place for set time}
type
  TKMUnitActionSteer = class(TKMUnitAction)
  private
    fDesireToSteer, fStuckFor: Byte; //Likelihood of changing direction
    fVertexOccupied: TKMPoint; //The diagonal vertex we are currently occupying
    fNextPos: TKMPointDir; //The tile we are currently walking to
    procedure IncVertex(const aFrom, aTo: TKMPoint);
    procedure DecVertex;
    function ChooseNextStep(out Point: TKMPointDir): Boolean;
  public
    constructor Create(aUnit: TKMUnit; aActionType: TKMUnitActionType; aLocked: Boolean);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function ActName: TKMUnitActionName; override;
    function GetExplanation: UnicodeString; override;
    function Execute: TKMActionResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_CommonUtils, KM_Resource, KM_ResUnits;


{ TUnitActionSteer }
constructor TKMUnitActionSteer.Create(aUnit: TKMUnit; aActionType: TKMUnitActionType; aLocked: Boolean);
begin
  inherited Create(aUnit, aActionType, aLocked);

  Assert(aUnit is TKMUnitAnimal); //Only animals do steering
  fVertexOccupied := KMPOINT_ZERO;
  fNextPos.Loc        := KMPOINT_ZERO;
  fNextPos.Dir        := aUnit.Direction;
end;


destructor TKMUnitActionSteer.Destroy;
begin
  if not KMSamePoint(fVertexOccupied, KMPOINT_ZERO) then
    DecVertex;

  inherited;
end;


constructor TKMUnitActionSteer.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('UnitActionSteer');
  LoadStream.Read(fDesireToSteer);
  LoadStream.Read(fStuckFor);
  LoadStream.Read(fVertexOccupied);
  LoadStream.Read(fNextPos);
end;


function TKMUnitActionSteer.ActName: TKMUnitActionName;
begin
  Result := uanSteer;
end;


function TKMUnitActionSteer.GetExplanation: UnicodeString;
begin
  Result := 'Steering';
end;


procedure TKMUnitActionSteer.IncVertex(const aFrom, aTo: TKMPoint);
begin
  //Tell gTerrain that this vertex is being used so no other unit walks over the top of us
  Assert(KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'Steer vertex in use');

  fUnit.VertexAdd(aFrom,aTo);
  fVertexOccupied := KMGetDiagVertex(aFrom,aTo);
end;


procedure TKMUnitActionSteer.DecVertex;
begin
  //Tell gTerrain that this vertex is not being used anymore
  Assert(not KMSamePoint(fVertexOccupied, KMPOINT_ZERO), 'DecVertex 0:0 Steer');

  fUnit.VertexRem(fVertexOccupied);
  fVertexOccupied := KMPOINT_ZERO;
end;


function TKMUnitActionSteer.ChooseNextStep(out Point: TKMPointDir): Boolean;
var
  I,K,J: Integer;
  loc: TKMPoint;
  list: TKMPointList;
  goodSpot: Boolean;
begin
  Inc(fDesireToSteer);
  //Default is the next tile in the direction we're going
  loc := KMGetPointInDir(fUnit.Position, fUnit.Direction);
  //Decide whether we should change direction or not
  if (KaMRandom(10, 'TKMUnitActionSteer.ChooseNextStep') < fDesireToSteer)
  or not fUnit.CanStepTo(loc.X, loc.Y, fUnit.DesiredPassability) then
  begin
    fDesireToSteer := 0; //Reset it
    list := TKMPointList.Create;
    loc := fUnit.Position;
    for I:=-1 to 1 do
      for K:=-1 to 1 do
        if ((I<>0)or(K<>0)) and fUnit.CanStepTo(loc.X+I, loc.Y+K, fUnit.DesiredPassability) then
        begin
          //Directions next to our current one are preferable (looks nicer if animals don't make jarring direction changes often)
          goodSpot := KMGetDirection(I, K) in [KMNextDirection(fUnit.Direction), KMPrevDirection(fUnit.Direction)];
          for J:=0 to 5*Byte(goodSpot) do
            list.Add(KMPoint(loc.X+I, loc.Y+K));
        end;
    Result := list.GetRandom(Point.Loc);
    Point.Dir := KMGetDirection(fUnit.PositionPrev, Point.Loc);
    list.Free;
  end
  else
  begin
    Point.Loc := loc;
    Result := True;
    Point.Dir := KMGetDirection(fUnit.PositionPrev, Point.Loc);
  end;
end;


function TKMUnitActionSteer.Execute: TKMActionResult;
const
  STUCK_MAX_TIME = 200;
var
  dx, dy: Shortint;
  walkX, walkY, distance: Single;
  firstStep: Boolean;
  oldDir : TKMDirection;
begin
  if KMSamePoint(fNextPos.Loc, KMPOINT_ZERO) then
  begin
    fNextPos.Loc := fUnit.Position; //Set fNextPos to current pos so it initializes on the first run
    fNextPos.Dir := fUnit.Direction;
    firstStep := True;
  end
  else
    firstStep := False;

  // Use umtWalk move type here, since we just want to evaluate if we are close enough
  //distance := gRes.Units[fUnit.UnitType].GetEffectiveWalkSpeed(False);
  distance := fUnit.GetEffectiveWalkSpeed(False);
  //check if we need to change direction

   //Update unit direction so we are facing the way we are going
  //Save unit dir in case we will need to restore it
  oldDir := fUnit.Direction;
  //Update unit direction according to next Node
  If (oldDir <> fNextPos.Dir) and USE_UNIT_TURN_AROUND then
  begin
    //in original KaM units were turning, not sudenly changing their direction
    If GetDirDifference(oldDir, fNextPos.Dir) = 1 then
      fUnit.Direction := DIR_TO_PREV[oldDir]
    else
      fUnit.Direction := DIR_TO_NEXT[oldDir];

      {KMGetDirection(fNodeList[fNodePos], fNodeList[fNodePos+1]);}
    Exit(arActContinues);
  end else
    fUnit.Direction := fNextPos.Dir;

  if KMSamePointF(fUnit.PositionF, KMPointF(fNextPos), distance/2) then
  begin
    //Set precise position to avoid rounding errors
    fUnit.PositionF := KMPointF(fNextPos);

    //No longer using previous vertex
    if KMStepIsDiag(fUnit.PositionPrev, fUnit.PositionNext) and not firstStep and (fStuckFor = 0) then
      DecVertex;

    //Decide on next step
    if not ChooseNextStep(fNextPos) then
    begin
      Inc(fStuckFor);
      if fStuckFor > STUCK_MAX_TIME then
        Result := arActAborted //We have been stuck for a while so abort and TKMUnitAnimal.UpdateState will kill us
      else
        Result := arActContinues;
      Exit;
    end;
    fStuckFor := 0;

    //Do some house keeping because we have now stepped on a new tile
    fUnit.PositionNext := fNextPos.Loc;
    fUnit.Walk(fUnit.PositionPrev, fUnit.PositionNext); //Pre-occupy next tile
    if KMStepIsDiag(fUnit.PositionPrev,fUnit.PositionNext) then
      IncVertex(fUnit.PositionPrev,fUnit.PositionNext);



    fNextPos.Dir := KMGetDirection(fUnit.PositionPrev, fUnit.PositionNext);
    //If fNextPos.Dir <> fUnit.Direction then
    //  Exit(arActContinues);
    //Update unit direction according to next Node
    If (fNextPos.Dir <> fUnit.Direction) and USE_UNIT_TURN_AROUND then
    begin
      //in original KaM units were turning, not sudenly changing their direction
      If GetDirDifference(oldDir, fNextPos.Dir) = 1 then
        fUnit.Direction := DIR_TO_PREV[oldDir]
      else
        fUnit.Direction := DIR_TO_NEXT[oldDir];

        {KMGetDirection(fNodeList[fNodePos], fNodeList[fNodePos+1]);}
      Exit(arActContinues);
    end else
      fUnit.Direction := fNextPos.Dir;
  end;

  walkX := fNextPos.X - fUnit.PositionF.X;
  walkY := fNextPos.Y - fUnit.PositionF.Y;
  dx := sign(walkX); //-1,0,1
  dy := sign(walkY); //-1,0,1

  //distance := gRes.Units[fUnit.UnitType].GetEffectiveWalkSpeed((dx <> 0) and (dy <> 0));

  distance := fUnit.GetEffectiveWalkSpeed((dx <> 0) and (dy <> 0));

  fUnit.PositionF := KMPointF(fUnit.PositionF.X + dx*Math.min(distance,abs(walkX)),
                              fUnit.PositionF.Y + dy*Math.min(distance,abs(walkY)));

  Inc(fUnit.AnimStep);
  Result := arActContinues;
end;


procedure TKMUnitActionSteer.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('UnitActionSteer');
  SaveStream.Write(fDesireToSteer);
  SaveStream.Write(fStuckFor);
  SaveStream.Write(fVertexOccupied);
  SaveStream.Write(fNextPos);
end;


end.
