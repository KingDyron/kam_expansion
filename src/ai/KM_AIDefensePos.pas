unit KM_AIDefensePos;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_Entity, KM_UnitGroup,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_AITypes;


type
  TAIDefencePosition = class(TKMEntity)
  private
    fOnPatrolTime : Cardinal;
    fDontRestock : Boolean;
    fDefenceType: TKMAIDefencePosType; //Whether this is a front or back line defence position. See comments on TAIDefencePosType above
    fGroupType: TKMGroupType; //Type of group to defend this position (e.g. melee)
    fPosition, fPositionPatrol: TKMPointDir; //Position and direction the group defending will stand
    fRadius: Integer; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    fIsReturning : Boolean;
    fCurrentGroup: TKMUnitGroup; //Commander of group currently occupying position
    procedure SetCurrentGroup(aGroup: TKMUnitGroup);
    procedure SetGroupType(const Value: TKMGroupType);
    procedure SetDefenceType(const Value: TKMAIDefencePosType);
    procedure SetPosition(const Value: TKMPointDir);
    procedure SetPositionPatrol(const Value: TKMPointDir);
  public
    IsOnPatrol : Boolean;
    constructor Create(aUID: Integer; const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    property IsReturning : Boolean read fIsReturning;
    property DefenceType: TKMAIDefencePosType read fDefenceType write SetDefenceType;
    property GroupType: TKMGroupType read fGroupType write SetGroupType; //Type of group to defend this position (e.g. melee)
    property Position: TKMPointDir read fPosition write SetPosition; //Position and direction the group defending will stand
    property PositionPatrol: TKMPointDir read fPositionPatrol write SetPositionPatrol; //Position and direction the group defending will stand
    property Radius: Integer read fRadius write fRadius; //If fighting (or houses being attacked) occurs within this radius from this defence position, this group will get involved
    property DontRestock : Boolean read fDontRestock write fDontRestock;

    property CurrentGroup: TKMUnitGroup read fCurrentGroup write SetCurrentGroup;
    function CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad;
    procedure UpdateState;
  end;

  TAIDefencePositions = class
  private
    fOwner: TKMHandID;
    fPositions: TKMList;
    function GetPosition(aIndex: Integer): TAIDefencePosition; inline;
    function GetCount: Integer; inline;
    function CreateDefPosition(const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): TAIDefencePosition;
  public
    //Defines how defending troops will be formatted. 0 means leave unchanged.
    TroopFormations: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMFormation;

    constructor Create(aPlayer: TKMHandID);
    destructor Destroy; override;

    function Add(const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): Integer;
    function Insert(aIndex: Integer; const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    procedure Delete(aIndex: Integer);
    property Positions[aIndex: Integer]: TAIDefencePosition read GetPosition; default;
    function GetBacklineCount: Integer;
    function AverageUnitsPerGroup: Integer;

    function FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;

    function FindPositionAtLoc(aPos: TKMPoint ): TAIDefencePosition;

    procedure RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
    function FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;
    procedure GetArmyDemand(out aDemand : TAIArmyDemand);

    procedure MoveUp(aIndex: Integer);
    procedure MoveDown(aIndex: Integer);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
    procedure UpdateState;
    procedure Paint;
  end;

  TAIDefendPosition = class(TKMEntity)
    private
      fRadius : Word;
      fPosition : TKMPoint;
      procedure SetPosition(const Value: TKMPoint);
    public
      property Radius : Word read fRadius write fRadius;
      property Position : TKMPoint read fPosition write SetPosition;
      constructor Create(aUID : Integer; aPos : TKMPoint; aRadius : Word);
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TAIDefendPositions = class
    fList : array of TAIDefendPosition;
    fCount : Integer;
    fOwner : TKMHandID;
    function GetDefendPos(aIndex : Integer) : TAIDefendPosition;
  public
    property DefendPosition[aIndex : Integer] : TAIDefendPosition read GetDefendPos; default;
    property Count : Integer Read fCount;

    function FindDefendPosAtLoc(aLoc : TKMPoint) : TAIDefendPosition;
    function LocInDefendArea(aLoc : TKMPoint) : Boolean;

    procedure Add(aPos : TKMPoint; aRadius : Word);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    constructor Create(aOwner : TKMHandID);
    procedure Paint;
  end;

implementation
uses
  SysUtils, TypInfo,KM_Terrain,
  KM_Game, KM_GameUIDTracker, KM_GameParams, KM_HandsCollection, KM_RenderAux, KM_RenderPool, KM_Hand,
  KM_UnitGroupTypes, KM_InterfaceGame;


{ TAIDefencePosition }
constructor TAIDefencePosition.Create(aUID: Integer; const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType);
begin
  inherited Create(aUID);

  Assert(aGroupType in GROUP_TYPES_VALID, 'Invalid group type: ' + GetEnumName(TypeInfo(TKMGroupType), Integer(aGroupType)));
  fPosition := aPos;
  fGroupType := aGroupType;
  fRadius := aRadius;
  fDefenceType := aDefenceType;
  CurrentGroup := nil; //Unoccupied
  IsOnPatrol := false;
  fOnPatrolTime := 0;
  fPositionPatrol.Dir := dirN;
end;


destructor TAIDefencePosition.Destroy;
begin
  CurrentGroup := nil; //Ensure pointer is removed
  inherited;
end;


procedure TAIDefencePosition.SetCurrentGroup(aGroup: TKMUnitGroup);
begin
  //Release previous group
  gHands.CleanUpGroupPointer(fCurrentGroup);

  //Take new one
  if aGroup <> nil then
    fCurrentGroup := aGroup.GetPointer;
end;


procedure TAIDefencePosition.SetDefenceType(const Value: TKMAIDefencePosType);
begin
  Assert(gGameParams.IsMapEditor);
  fDefenceType := Value;
end;


procedure TAIDefencePosition.SetGroupType(const Value: TKMGroupType);
begin
  Assert(gGameParams.IsMapEditor);
  Assert(Value in GROUP_TYPES_VALID, 'Invalid group type: ' + GetEnumName(TypeInfo(TKMGroupType), Integer(Value)));
  fGroupType := Value;
end;


procedure TAIDefencePosition.SetPosition(const Value: TKMPointDir);
begin
  Assert(gGameParams.IsMapEditor);
  fPosition := Value;
end;

procedure TAIDefencePosition.SetPositionPatrol(const Value: TKMPointDir);
var I : Byte;
begin
  //Assert(gGameParams.IsMapEditor);
  if Value.Loc = fPositionPatrol.Loc then
  begin
    I := byte(fPositionPatrol.Dir) + 1;
    if I >= 9 then
      I := 1;
    fPositionPatrol.Dir := TKMDirection(I);
    Exit;
  end;

  fPositionPatrol := Value;
  if fPosition.Loc = fPositionPatrol.Loc then
    fPositionPatrol.Loc := KMPoint(0, 0);

end;


procedure TAIDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('ClsDefencePos');
  SaveStream.Write(fPosition);
  SaveStream.Write(fPositionPatrol);
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  SaveStream.Write(fRadius);
  SaveStream.Write(fDefenceType, SizeOf(fDefenceType));
  SaveStream.Write(fCurrentGroup.UID); //Store ID
  SaveStream.Write(IsOnPatrol); //Store ID
  SaveStream.Write(fOnPatrolTime); //Store ID
  SaveStream.Write(fIsReturning); //Store ID
  SaveStream.Write(fDontRestock); //Store ID
end;


constructor TAIDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('ClsDefencePos');
  LoadStream.Read(fPosition);
  LoadStream.Read(fPositionPatrol);
  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fRadius);
  LoadStream.Read(fDefenceType, SizeOf(fDefenceType));
  LoadStream.Read(fCurrentGroup, 4); //subst on syncload
  LoadStream.Read(IsOnPatrol); //subst on syncload
  LoadStream.Read(fOnPatrolTime); //subst on syncload
  LoadStream.Read(fIsReturning); //Store ID
  LoadStream.Read(fDontRestock); //Store ID
end;


procedure TAIDefencePosition.SyncLoad;
begin
  fCurrentGroup := gHands.GetGroupByUID(Integer(fCurrentGroup));
end;


function TAIDefencePosition.CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
begin
  Result := aGroup.GetMembersGroupType in [fGroupType, gtAny];

  if not Result then Exit;

  // Empty position accepts anything (e.g. happens at mission start)
  // As checked in KaM - it did not link big groups to filled Positions
  Result := (CurrentGroup = nil) or
            (aGroup.Count = 1);

  Result := Result and ((CurrentGroup = nil) or (CurrentGroup.Count < aMaxUnits));
end;


procedure TAIDefencePosition.UpdateState;
  function CurrentPos : TKMPointDir;
  begin
    if IsOnPatrol then
      Result := PositionPatrol
    else
      Result := Position;
    
  end;
var aToLoc : TKMPointDir;
begin
  //If the group is Dead or too far away we should disassociate
  //them from the defence position so new warriors can take up the defence if needs be
  if self.fDefenceType in [dtBackLine{, dtFrontLine}] then
  begin
    if (CurrentGroup = nil)
    or CurrentGroup.IsDead
    or ((CurrentGroup.InFight or (CurrentGroup.Order in [goAttackHouse, goAttackUnit]))
        and (KMLengthDiag(Position.Loc, CurrentGroup.Position) > Radius)) then
      CurrentGroup := nil;

  end else
  begin


    //If group is in front line and is too far away order to go back do defPos
    if CurrentGroup <> nil then
    begin
      //if not fIsReturning then
      If CurrentGroup.IsDead
        or ( (CurrentGroup.InFight or (CurrentGroup.Order in [goAttackHouse, goAttackUnit]))
          and (KMLengthDiag(CurrentPos.Loc, CurrentGroup.Position) > Radius + 10)
          {and not gHands[CurrentGroup.Owner].AI.General.DefendPositions.LocInDefendArea(CurrentGroup.Position)}) then
          CurrentGroup := nil;
      if CurrentGroup <> nil then
        if fIsReturning then
          if (KMLengthDiag(CurrentPos.Loc, CurrentGroup.Position) < 7) then
            fIsReturning := false;

      if CurrentGroup <> nil then
      //if not gHands[CurrentGroup.Owner].AI.General.DefendPositions.LocInDefendArea(CurrentGroup.Position) then
        if (KMLengthDiag(CurrentPos.Loc, CurrentGroup.Position) > Radius) then
        begin
            CurrentGroup.OrderWalk(CurrentPos.Loc, True, wtokAIGotoDefencePos, CurrentPos.Dir);
            fIsReturning := true;
        end;
    end;
  end;


  //Tell group to walk to its position
  //It's easier to repeat the order than check that all members are in place
  if (CurrentGroup <> nil)
    and CurrentGroup.IsIdleToAI([wtokFlagPoint, wtokHaltOrder]) then
  begin
    if (PositionPatrol.Loc.X <= 0) or (DefenceType = dtBackLine)then
    begin
      aToLoc := Position;// KMPointDir(PositionPatrol.Loc, Position.Dir);
      if IsOnPatrol and (DefenceType = dtBackLine) and (PositionPatrol.Loc.X > 0) then
        aToLoc := PositionPatrol;

      If CurrentGroup.CanWalkTo(aToLoc.Loc, 0) then
        CurrentGroup.OrderWalk(aToLoc.Loc, True, wtokAIGotoDefencePos, aToLoc.Dir);
    end else
    begin
      Inc(fOnPatrolTime);
      if fOnPatrolTime = 30 then
      begin
        fOnPatrolTime := 0;
        if IsOnPatrol then
        begin
          If CurrentGroup.CanWalkTo(Position.Loc, 0) then
            CurrentGroup.OrderWalk(Position.Loc, True, wtokAIGotoDefencePos, Position.Dir);
          IsOnPatrol := false
        end else
        begin
          IsOnPatrol := true;
          If CurrentGroup.CanWalkTo(PositionPatrol.Loc, 0) then
            CurrentGroup.OrderWalk(PositionPatrol.Loc, True, wtokAIGotoDefencePos, PositionPatrol.Dir);
        end;
      end else
      begin
        aToLoc := Position;
        if IsOnPatrol then
          aToLoc := PositionPatrol;

        If CurrentGroup.CanWalkTo(aToLoc.Loc, 0) then
          CurrentGroup.OrderWalk(aToLoc.Loc, True, wtokAIGotoDefencePos, aToLoc.Dir);

      end;

    end;




  end;
end;


{ TAIDefencePositions }
constructor TAIDefencePositions.Create(aPlayer: TKMHandID);
var
  GT: TKMGroupType;
begin
  inherited Create;

  fOwner := aPlayer;
  fPositions := TKMList.Create;

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    TroopFormations[GT].NumUnits := 9; //These are the defaults in KaM
    TroopFormations[GT].UnitsPerRow := 3;
  end;
end;


destructor TAIDefencePositions.Destroy;
begin
  fPositions.Free;

  inherited;
end;


function TAIDefencePositions.GetCount: Integer;
begin
  Result := fPositions.Count;
end;


//Find DefencePosition at location
function TAIDefencePositions.FindPositionAtLoc(aPos: TKMPoint): TAIDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if Positions[I].Position.Loc = aPos then
      Exit(Positions[I]);
end;


function TAIDefencePositions.GetPosition(aIndex: Integer): TAIDefencePosition;
begin
  if not InRange(aIndex, 0, Count - 1) then Exit(nil);

  Result := fPositions[aIndex];
end;


function TAIDefencePositions.CreateDefPosition(const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): TAIDefencePosition;
begin
  Assert(aGroupType in GROUP_TYPES_VALID, 'Invalid group type: ' + GetEnumName(TypeInfo(TKMGroupType), Integer(aGroupType)));

  Result := TAIDefencePosition.Create(gUIDTracker.GetNewUID, aPos, aGroupType, aRadius, aDefenceType);
end;


function TAIDefencePositions.Add(const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): Integer;
var
  defPos: TAIDefencePosition;
begin
  defPos := CreateDefPosition(aPos, aGroupType, aRadius, aDefenceType);
  fPositions.Add(defPos);
  Result := defPos.UID;
end;


function TAIDefencePositions.Insert(aIndex: Integer; const aPos: TKMPointDir; aGroupType: TKMGroupType; aRadius: Integer; aDefenceType: TKMAIDefencePosType): Integer;
var
  defPos: TAIDefencePosition;
begin
  Assert(InRange(aIndex, 0, Count), 'Can''t insert defence position at position ' + IntToStr(aIndex));
  defPos := CreateDefPosition(aPos, aGroupType, aRadius, aDefenceType);

  fPositions.Insert(aIndex, defPos);

  Result := defPos.UID;
end;


procedure TAIDefencePositions.Clear;
begin
  fPositions.Clear;
end;


procedure TAIDefencePositions.Delete(aIndex: Integer);
begin
  //Note: Order of positions is important (AI fills them with troops from 0 to N)
  fPositions.Delete(aIndex);
end;


function TAIDefencePositions.GetBacklineCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Positions[I].fDefenceType = dtBackLine then
      Inc(Result);
end;


function TAIDefencePositions.AverageUnitsPerGroup: Integer;
var
  GT: TKMGroupType;
  TypeCount: Integer;
begin
  Result := 0;
  TypeCount := 0;
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    Result := Result + TroopFormations[GT].NumUnits;
    Inc(TypeCount);
  end;
  Result := Result div TypeCount;
end;


function TAIDefencePositions.FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;
var
  I, Matched, tmp: Integer;
  Distance, Best: Single;
  GT, GT2 : TKMGroupType;
begin
  Result := False;
  Matched := -1;
  Best := MaxSingle;
  if aGroup.IsOnPatrolAttack then
    Exit;
  GT := aGroup.GetMembersGroupType;

  if GT = gtNone then
    Exit(false);


  //Try to link to existing group
  for I := 0 to Count - 1 do
  begin
    GT2 := Positions[I].GroupType;
    if GT = gtAny then
      tmp := TroopFormations[GT2].NumUnits
    else
      tmp := TroopFormations[GT].NumUnits;

    if Positions[I].CanAccept(aGroup, tmp) then
    begin
      //Take closest position that is empty or requries restocking
      Distance := KMLengthSqr(aGroup.Position, Positions[I].Position.Loc);
      if Distance < Best then
      begin
        Matched := I;
        Best := Distance;
        //KaM fills defence positions by their order of creation
        //Take first one we find - that's what KaM does
        if not aTakeClosest then Break;
      end;
    end;
  end;

  if Matched <> -1 then
  begin
    Result := True;
    if Positions[Matched].CurrentGroup = nil then
    begin
      //New position
      Positions[Matched].CurrentGroup := aGroup;
      GT2 := Positions[Matched].GroupType;
      if GT = gtAny then
        tmp := TroopFormations[GT2].UnitsPerRow
      else
        tmp := TroopFormations[GT].UnitsPerRow;

      if aGroup.UnitsPerRow <> tmp then
        aGroup.UnitsPerRow := tmp;
      aGroup.OrderWalk(Positions[Matched].Position.Loc, True, wtokAIGotoDefencePos);
    end
    else
      //Append to existing position
      RestockPositionWith(Positions[Matched].CurrentGroup, aGroup);
  end;
end;


procedure TAIDefencePositions.RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
var
  Needed, perRow: integer;
  DP : TAIDefencePosition;
begin
  DP := FindPositionOf(aDefenceGroup);

  if aDefenceGroup.GetMembersGroupType = gtAny then
    Needed := TroopFormations[DP.GroupType].NumUnits - aDefenceGroup.Count
  else
    Needed := TroopFormations[aDefenceGroup.GetMembersGroupType].NumUnits - aDefenceGroup.Count;


  if Needed <= 0 then exit;
  if aGroup.Count <= Needed then
    aGroup.OrderLinkTo(aDefenceGroup, True) //Link entire group
  else
    aGroup.OrderSplitLinkTo(aDefenceGroup, Needed, True); //Link only as many units as are needed



  if aDefenceGroup.GetMembersGroupType = gtAny then
    perRow := TroopFormations[DP.GroupType].UnitsPerRow
  else
    perRow := TroopFormations[aDefenceGroup.GetMembersGroupType].UnitsPerRow;


  if aDefenceGroup.UnitsPerRow <> perRow then
    aDefenceGroup.UnitsPerRow := perRow;
end;


//Find DefencePosition to which this Commander belongs
//(Result could be nil if CommanderCount > PositionsCount
function TAIDefencePositions.FindPositionOf(aGroup: TKMUnitGroup): TAIDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  if Positions[I].CurrentGroup = aGroup then
  begin
    Result := Positions[I];
    Break;
  end;
end;


procedure TAIDefencePositions.GetArmyDemand(out aDemand : TAIArmyDemand);
var I, Required: Integer;
  GT : TKMGroupType;
begin
  for GT := Low(TKMGroupType) to High(TKMGroupType) do
    aDemand[GT] := 0;

  for I := 0 to Count - 1 do
  begin
    Required := TroopFormations[Positions[I].GroupType].NumUnits;
    Inc(aDemand[Positions[I].GroupType], Required);
    {case Positions[I].GroupType of
      gtMelee:     Inc(aFootmen, Required);
      gtAntiHorse: Inc(aPikemen, Required);
      gtRanged:    Inc(aArchers, Required);
      gtMounted:   Inc(aHorsemen, Required);
    end;}
  end;
end;


procedure TAIDefencePositions.MoveUp(aIndex: Integer);
begin
  if aIndex < Count-1 then
    fPositions.Exchange(aIndex, aIndex+1);
end;


procedure TAIDefencePositions.MoveDown(aIndex: Integer);
begin
  if aIndex > 0 then
    fPositions.Exchange(aIndex, aIndex-1);
end;


procedure TAIDefencePositions.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.PlaceMarker('ClsDefencePositions');
  SaveStream.Write(fOwner);
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(Count);

  for I := 0 to Count - 1 do
    Positions[I].Save(SaveStream);
end;


procedure TAIDefencePositions.Load(LoadStream: TKMemoryStream);
var I, NewCount: Integer;
begin
  LoadStream.CheckMarker('ClsDefencePositions');
  LoadStream.Read(fOwner);
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(NewCount);

  for I := 0 to NewCount - 1 do
    fPositions.Add(TAIDefencePosition.Load(LoadStream));
end;


procedure TAIDefencePositions.SyncLoad;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Positions[I].SyncLoad;
end;


procedure TAIDefencePositions.UpdateState;
var I,K: Integer;
begin
  //Make sure no defence position Group is dead
  for I := 0 to Count - 1 do
    Positions[I].UpdateState;

  if gHands[fOwner].AI.General.DPDontRestock then
    Exit;
  //In KaM the order of defence positions is the priority: The first defined is higher priority
  for I := 0 to Count - 1 do
  if (Positions[I].CurrentGroup = nil) then
    for K := I + 1 to Count - 1 do
    if Positions[I].GroupType = Positions[K].GroupType then
      if not Positions[K].DontRestock then
      begin
        Positions[I].CurrentGroup := Positions[K].CurrentGroup; //Take new position
        Positions[K].CurrentGroup := nil; //Leave current position
        Break;
      end;
end;


procedure TAIDefencePositions.Paint;
var
  I: Integer;
  str: string;
begin
  for I := 0 to Count - 1 do
  begin
    gRenderAux.Quad(Positions[I].Position.Loc.X, Positions[I].fPosition.Loc.Y, DEFENCE_LINE_TYPE_COL[Positions[I].fDefenceType]);

    gRenderPool.RenderSpriteOnTile(Positions[I].Position.Loc, 510 + Byte(Positions[I].Position.Dir), gHands[fOwner].FlagColor);
    gRenderAux.CircleOnTerrain(Positions[I].Position.Loc.X-0.5, Positions[I].Position.Loc.Y-0.5, Positions[I].Radius,
                               gHands[fOwner].FlagColor AND $08FFFFFF,
                               gHands[fOwner].FlagColor);
    //group type and position number will be painted by InterfaceMapEditor

    if not gGameParams.IsMapEditor then
    begin
      str := 'nil';
      if Positions[I].CurrentGroup <> nil then
        str := IntToStr(Positions[I].CurrentGroup.UID);

      gRenderAux.Text(Positions[I].fPosition.Loc.X, Positions[I].fPosition.Loc.Y, str, icRed);
    end;
  end;
end;

constructor TAIDefendPosition.Create(aUID: Integer; aPos: TKMPoint; aRadius: Word);
begin
  Inherited Create(aUID);

  fRadius := aRadius;
  fPosition := aPos;
end;

procedure TAIDefendPosition.SetPosition(const Value: TKMPoint);
begin
  Assert(gGameParams.IsMapEditor);
  fPosition := Value;
end;

constructor TAIDefendPosition.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.Read(fRadius);
  LoadStream.Read(fPosition);
end;

procedure TAIDefendPosition.Save(SaveStream: TKMemoryStream);
begin
  Inherited;
  SaveStream.Write(fRadius);
  SaveStream.Write(fPosition);
end;

constructor TAIDefendPositions.Create(aOwner: TKMHandID);
begin
  Inherited Create;
  fOwner := aOwner;
  fCount := 0;
end;

function TAIDefendPositions.GetDefendPos(aIndex: Integer): TAIDefendPosition;
begin
  Assert(aIndex < fCount);
  Result := fList[aIndex];
end;

procedure TAIDefendPositions.Add(aPos: TKMPoint; aRadius: Word);
begin
  SetLength(fList, fCount + 1);

  fList[fCount] := TAIDefendPosition.Create(gUIDTracker.GetNewUID, aPos, aRadius);
  fCount := fCount + 1;
end;

function TAIDefendPositions.FindDefendPosAtLoc(aLoc: TKMPoint): TAIDefendPosition;
var I : Integer;
begin
  Result := nil;
  for I := 0 to fCount - 1 do
    if fList[I].Position = aLoc then
      Exit(fList[I]);

end;

function TAIDefendPositions.LocInDefendArea(aLoc : TKMPoint) : Boolean;
var I : Integer;
  distance : Single;
begin
  Result := false;
  for I := 0 to fCount - 1 do
  begin
    distance := KMLengthDiag(fList[I].Position, aLoc);
    if distance <= fList[I].Radius then
      Exit(true);
  end;

end;

procedure TAIDefendPositions.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  newCount := fCount;
  SaveStream.Write(fOwner);
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    fList[I].Save(SaveStream);

end;

procedure TAIDefendPositions.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;

begin
  LoadStream.read(fOwner);
  LoadStream.read(newCount);
  SetLength(fList, newCount);
  fCount := newCount;
  for I := 0 to newCount - 1 do
    fList[I] := TAIDefendPosition.Load(LoadStream);
end;

destructor TAIDefendPositions.Destroy;
var I : Integer;
begin
  for I := 0 to fCount - 1 do
    fList[I].Free;

  Inherited;

end;

procedure TAIDefendPositions.Paint;
var I : integer;
begin
  for I := 0 to Count - 1 do
  begin

    gRenderAux.Quad(fList[I].Position.X, fList[I].fPosition.Y, gHands[fOwner].FlagColor AND $28FFFFFF );

    gRenderPool.RenderSpriteOnTile(fList[I].Position, 519, gHands[fOwner].FlagColor);
    gRenderAux.CircleOnTerrain(fList[I].Position.X-0.5, fList[I].Position.Y-0.5, fList[I].Radius,
                               gHands[fOwner].FlagColor AND $28FFFFFF,
                               gHands[fOwner].FlagColor);
  end;
end;

end.