{
Artificial intelligence
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_ArmyAttack;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_UnitGroup,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Houses, KM_ResHouses, KM_ResWares, KM_NavMeshPathFinding,
  KM_Eye;

type
  TAISquad = class; // Combat group (orders to specific group: walk / attack)
  TAICompany = class; // Company (selection of targets, positions etc.)
  TKMArmyAttack = class; // Attack (time distribution, company initialization)

  TKMSquadList = array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMList;
  TKMSquadsArray = array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of record
    Count: Word;
    Squads: array of TAISquad;
    TargetUnit: TKMUnitArray;
    TargetHouse: TKMHouseArray;
  end;
  TKMTargetSelection = array of record
    Index: Word;
    CenterPoint: TKMPoint;
    CloseThreat, DistantThreat: Single;
  end;
  TKMCompanyMode = (cmAttack, cmDefence, cmDestruction);
  TKMCompanyState = (csAttack, csWalking, csIdle);

  TAISquad = class
  private
    fGroup: TKMUnitGroup;
    fOnPlace, fTargetChanged: Boolean;
    fFinalPosition: TKMPointDir;
    fTargetHouse: TKMHouse;
    fTargetUnit: TKMUnit;
    fWalkTimeLimit, fAttackTimeLimit: Cardinal;

    function SquadInFight(): Boolean; inline;
    function GetGroupPosition(): TKMPoint; inline;
    function PlanPath(aTick: Cardinal; var aActualPosition, aTargetPosition: TKMPoint; aOrderAttack: Boolean = False; aOrderDestroy: Boolean = False): Boolean;

    procedure SetTargetHouse(aHouse: TKMHouse);
    procedure SetTargetUnit(aUnit: TKMUnit);
    procedure SetTargetPosition(aLoc: TKMPointDir);
  public
    {$IFDEF DEBUG_NewAI}
    DEBUGPointPath: TKMPointArray;
    {$ENDIF}

    constructor Create(aGroup: TKMUnitGroup);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property Group: TKMUnitGroup read fGroup;
    property OnPlace: Boolean read fOnPlace;
    property InFight: Boolean read SquadInFight;
    property FinalPosition: TKMPointDir read fFinalPosition write SetTargetPosition;
    property Position: TKMPoint read GetGroupPosition;
    property WalkTimeLimit: Cardinal read fWalkTimeLimit write fWalkTimeLimit;
    property AttackTimeLimit: Cardinal read fAttackTimeLimit write fAttackTimeLimit;
    property TargetHouse: TKMHouse read fTargetHouse write SetTargetHouse;
    property TargetUnit: TKMUnit read fTargetUnit write SetTargetUnit;

    procedure UpdateState(aTick: Cardinal);
  end;

  TAICompany = class
  private
    fOwner: TKMHandID;
    fPathPosition: TKMPoint;
    fScanPosition: TKMPoint;
    fCompanyMode: TKMCompanyMode;
    fTargetOwner: TKMHandID;
    fTargetPoint: TKMPoint;
    fTargetHouse: TKMHouse;
    fTargetUnit: TKMUnit;
    fState: TKMCompanyState;
    fSquads: TKMSquadList;

    function GetPosition(): TKMPoint; overload;
    function GetPosition(var aSQRRadius: Single): TKMPoint; overload;
    function GetTargetPosition(): TKMPoint;
    function OrderToAttack(aActualPosition: TKMPoint; UA: TKMUnitArray; UGA: TKMUnitGroupArray; HA: TKMHouseArray): Boolean;
    function OrderMove(aTick: Cardinal; aActualPosition: TKMPoint): Boolean;
  public
    {$IFDEF DEBUG_NewAI}
    DEBUGScanRad: Single;
    DEBUGTargetU: TKMTargetSelection;
    DEBUGPointPath, DEBUG_UA_POINTS, DEBUG_UGA_POINTS: TKMPointArray;
    {$ENDIF}

    constructor Create(aOwner: TKMHandID; aCompanyMode: TKMCompanyMode);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property Owner: TKMHandID read fOwner write fOwner;
    property PathPosition: TKMPoint read fPathPosition;
    property ScanPosition: TKMPoint read fScanPosition;
    property CompanyMode: TKMCompanyMode read fCompanyMode write fCompanyMode;
    property TargetPoint: TKMPoint read fTargetPoint write fTargetPoint;
    property TargetPosition: TKMPoint read GetTargetPosition;
    property State: TKMCompanyState read fState;
    property Squads: TKMSquadList read fSquads;

    procedure InitCompany();
    procedure UpdateState(aTick: Cardinal);
    procedure AddSquad(aGroup: TKMUnitGroup);
    procedure DeleteSquad(aGT: TKMGroupType; aIdx: Integer);
    function SquadCnt(aTypes: TKMGroupTypeSet = GROUP_TYPES_VALID): Word;
    function WarriorCnt(aTypes: TKMGroupTypeSet = GROUP_TYPES_VALID): Word;
    function IsGroupInCompany(aGroup: TKMUnitGroup): Boolean;
    function SetTarget(aHouse: TKMHouse; aUnit: TKMUnit = nil): Boolean;
    function ActualizeTarget(aInitPosition: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; aAimCivilians: Boolean = True): Boolean;
  end;

  TKMArmyAttack = class
  private
    fOwner: TKMHandID;
    fCompanies: TKMList;

    function GetCount(): Integer;
    function GetCompany(aIdx: Integer): TAICompany;
  public
    constructor Create(aOwner: TKMHandID);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Count: Integer read GetCount;
    property Company[aIdx: Integer]: TAICompany read GetCompany; default;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandID);
    procedure CreateCompany(aTargetPoint: TKMPoint; aGroups: TKMUnitGroupArray; aCompanyMode: TKMCompanyMode = cmAttack);
    function IsGroupInAction(aGroup: TKMUnitGroup): Boolean;

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;

const
  // Houses in TARGET_HOUSES will be selected as a primary target (so the company will come to this point but it will not attack it because of this list)
  TARGET_HOUSES: TKMHouseTypeSet = [htBarracks, htStore, htSchool, htTownhall];
  // Houses in SCAN_HOUSES will be destroyed when they are in radius (it should also contain TARGET_HOUSES)
  SCAN_HOUSES: TKMHouseTypeSet = [htWatchTower, htBarracks, htStore, htSchool, htTownhall];
  // All houses for final stage of attack algorithm
  ALL_HOUSES: TKMHouseTypeSet = HOUSES_VALID;

implementation
uses
  Types,
  KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_NavMesh, KM_RenderAux,
  KM_UnitWarrior, KM_AIParameters;


{ TAISquad }
constructor TAISquad.Create(aGroup: TKMUnitGroup);
begin
  inherited Create;
  fGroup := aGroup.GetPointer();
  fOnPlace := True;
  fTargetChanged := True;
  fWalkTimeLimit := 0;
  fAttackTimeLimit := 0;
  fTargetHouse := nil;
  fTargetUnit := nil;
  //fFinalPosition := KMPointDir(KMPOINT_ZERO, dirNA);
end;


destructor TAISquad.Destroy();
begin
  gHands.CleanUpGroupPointer(fGroup);
  gHands.CleanUpUnitPointer(fTargetUnit);
  gHands.CleanUpHousePointer(fTargetHouse);
  inherited;
end;


constructor TAISquad.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.CheckMarker('Squad');
  LoadStream.Read(fOnPlace);
  LoadStream.Read(fTargetChanged);
  LoadStream.Read(fFinalPosition);
  LoadStream.Read(fWalkTimeLimit, SizeOf(fWalkTimeLimit));
  LoadStream.Read(fAttackTimeLimit, SizeOf(fAttackTimeLimit));
  //Subst on syncload
  LoadStream.Read(fGroup, 4);
  LoadStream.Read(fTargetUnit, 4);
  LoadStream.Read(fTargetHouse, 4);
end;


procedure TAISquad.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('Squad');
  SaveStream.Write(fOnPlace);
  SaveStream.Write(fTargetChanged);
  SaveStream.Write(fFinalPosition);
  SaveStream.Write(fWalkTimeLimit, SizeOf(fWalkTimeLimit));
  SaveStream.Write(fAttackTimeLimit, SizeOf(fAttackTimeLimit));
  if (fGroup <> nil) then
    SaveStream.Write(fGroup.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetUnit <> nil) then
    SaveStream.Write(fTargetUnit.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetHouse <> nil) then
    SaveStream.Write(fTargetHouse.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


procedure TAISquad.SyncLoad();
begin
  fGroup := gHands.GetGroupByUID(Integer(fGroup) );
  fTargetUnit := gHands.GetUnitByUID(Integer(fTargetUnit) );
  fTargetHouse := gHands.GetHouseByUID(Integer(fTargetHouse) );
end;


procedure TAISquad.SetTargetUnit(aUnit: TKMUnit);
begin
  if (fTargetUnit = aUnit) then
    Exit;
  // Archers will reaim (+ reset animation) only in case that new target is in specific distance from existing target
  if (aUnit <> nil) AND not aUnit.IsDeadOrDying then
  begin
    if (fGroup.GroupType = gtRanged)
      AND (fTargetUnit <> nil)
      //AND not fTargetUnit.IsDeadOrDying
      AND (
       (
         (fTargetUnit is TKMUnitWarrior)
         AND (aUnit is TKMUnitWarrior)
         AND (TKMUnitWarrior(fTargetUnit).Group = TKMUnitWarrior(aUnit).Group)
       ) OR (
         (KMDistanceSqr(fTargetUnit.CurrPosition, aUnit.CurrPosition) < sqr(AI_Par[ATTACK_SQUAD_ChangeTarget_DistTolerance]))
       )
      ) then
      Exit;
    fTargetChanged := True;
    gHands.CleanUpUnitPointer(fTargetUnit);
    fTargetUnit := aUnit.GetPointer;
  end
  else
    gHands.CleanUpUnitPointer(fTargetUnit); // aUnit = nil case
end;


procedure TAISquad.SetTargetPosition(aLoc: TKMPointDir);
begin
  fFinalPosition := aLoc;
end;


procedure TAISquad.SetTargetHouse(aHouse: TKMHouse);
begin
  if (aHouse <> nil) then
    SetTargetUnit(nil); // House have lower priority so if there is request for attacking house unit must be nil
  if (fTargetHouse = aHouse) then
    Exit;
  fTargetChanged := True;
  gHands.CleanUpHousePointer(fTargetHouse);
  if (aHouse <> nil) then
    fTargetHouse := aHouse.GetPointer;
end;


function TAISquad.SquadInFight(): Boolean;
begin
  Result := not fGroup.CanTakeOrders;
end;


function TAISquad.GetGroupPosition(): TKMPoint;
begin
  Result := fGroup.Position;
end;


// Update state of squad (group orders)
procedure TAISquad.UpdateState(aTick: Cardinal);
  function GetTargetPosition(): TKMPoint;
  var
    K: Integer;
    Dist, BestDist: Single;
    BestTgt: TKMUnit;
    G: TKMUnitGroup;
  begin
    Result := fTargetUnit.CurrPosition;
    // Get closest warrior in enemy group if the squad is ranged
    if (fGroup.GroupType = gtRanged) AND (fTargetUnit is TKMUnitWarrior) then
    begin
      BestTgt := nil;
      G := TKMUnitWarrior(fTargetUnit).Group;
      //Group will be nil if we target a warrior still walking out of barracks
      if G <> nil then
      begin
        BestDist := 1E10;
        for K := 0 to G.Count - 1 do
          if not G.Members[K].IsDeadOrDying then
          begin
            Dist := KMDistanceSqr(fGroup.Position,G.Members[K].CurrPosition);
            if (Dist < BestDist) then
            begin
              BestTgt := G.Members[K];
              BestDist := Dist;
            end;
          end;
        if (BestTgt <> nil) then
        begin
          gHands.CleanUpUnitPointer(fTargetUnit);
          fTargetUnit := BestTgt.GetPointer;
        end;
      end;
    end;
  end;
var
  ActPos, FinPos: TKMPoint;
begin
  fOnPlace := False;
  // Check group status and possibility to give order
  if (fGroup = nil) OR fGroup.IsDead OR InFight then
    Exit;

  // Check targets
  if (fTargetHouse <> nil) AND (fTargetHouse.IsDestroyed) then
    gHands.CleanUpHousePointer(fTargetHouse);
  if (fTargetUnit <> nil) AND (fTargetUnit.IsDead) then
    gHands.CleanUpUnitPointer(fTargetUnit);

  // Do order
  ActPos := fGroup.Position;
  if (fTargetUnit <> nil) then
  begin

    FinPos := GetTargetPosition();
    if PlanPath(aTick, ActPos, FinPos, True, False) then  // Check if squad is in the place
      Group.OrderWalk(FinPos, True, wtokAISquad, FinalPosition.Dir)
    else if
      fTargetChanged // Target has been changed
      OR (fGroup.GroupType <> gtRanged) // If not ranged then send a command to attack every update
      OR (KMDistanceSqr(FinPos,ActPos) > 12*12) // If ranged send attack command only until shoot distance to prevent reset of the shooting animation
      OR (fGroup.Order in [goNone, goWalkTo]) // If ranged send attack command also if target is already in range but previous command was not attack something
      OR (fAttackTimeLimit < aTick) // If ranged send attack command every X ticks to force at least first line of archers to shoot
       then
    begin
      fAttackTimeLimit := aTick + Round(AI_Par[ATTACK_SQUAD_ChangeTarget_Delay]);
      fTargetChanged := False;
      Group.OrderAttackUnit(fTargetUnit, True);
    end;
    fOnPlace := True;
  end
  else if (fTargetHouse <> nil) then
  begin
    FinPos := fTargetHouse.Position;
    if PlanPath(aTick, ActPos, FinPos, False, True) then
      Group.OrderWalk(FinPos, True, wtokAISquad, FinalPosition.Dir)
    else if (fGroup.GroupType <> gtRanged) OR fTargetChanged OR (fAttackTimeLimit < aTick) then
    begin
      fAttackTimeLimit := aTick + Round(AI_Par[ATTACK_SQUAD_ChangeTarget_Delay]);
      fTargetChanged := False;
      Group.OrderAttackHouse(fTargetHouse, True);
    end;
    fOnPlace := True;
  end
  else
  begin
    FinPos := FinalPosition.Loc;
    if PlanPath(aTick, ActPos, FinPos, False, False) then
      Group.OrderWalk(FinPos, True, wtokAISquad, FinalPosition.Dir)
    else if not KMSamePoint(Group.Position, FinalPosition.Loc) then // Dont repeat order and let archers fire
      Group.OrderWalk(FinalPosition.Loc, True, wtokAISquad, FinalPosition.Dir);
  end;
end;


function TAISquad.PlanPath(aTick: Cardinal; var aActualPosition, aTargetPosition: TKMPoint; aOrderAttack: Boolean = False; aOrderDestroy: Boolean = False): Boolean;
var
  InitPolygon, ClosestPolygon, Distance: Word;
  I: Integer;
  SQRDist: Single;
  PointPath: TKMPointArray;
begin
  Result := False;
  fOnPlace := False;
  SQRDist := KMDistanceSqr(aActualPosition, aTargetPosition);
  // Time limit (time limit MUST be always set by higher rank (platoon))
  if (not (aOrderAttack OR aOrderDestroy) AND (fWalkTimeLimit < aTick)) // Time limit is set to 0 in case that unit attack something
    // Target position is reached
    OR (SQRDist < sqr(AI_Par[ATTACK_SQUAD_TargetReached_Position]))
    // Target unit is close
    OR (aOrderAttack AND (SQRDist < sqr(AI_Par[ATTACK_SQUAD_TargetReached_Unit])))
    // Target house is close
    OR (aOrderDestroy AND (SQRDist < sqr(AI_Par[ATTACK_SQUAD_TargetReached_House])))
    // Archers should start fire as soon as possible
    OR ((aOrderAttack OR aOrderDestroy) AND (fGroup.GroupType = gtRanged) AND (SQRDist < sqr(AI_Par[ATTACK_SQUAD_TargetReached_RangedSquad]))) then
  begin
    fOnPlace := True;
    Exit;
  end;
  // Plan path with respect to enemy presence
  if gAIFields.NavMesh.Pathfinding.AvoidEnemyRoute(Group.Owner, Group.GroupType, aActualPosition, aTargetPosition, Distance, PointPath) then
  begin
    if (Distance < 5) then // Just to be sure (default value of pathfinding, if is group stuck it should not influence platoon)
    begin
      fOnPlace := True;
      Exit;
    end
    else
    begin
      InitPolygon := gAIFields.NavMesh.KMPoint2Polygon[aActualPosition];
      I := Length(PointPath)-2; // Skip next polygon -> fluent movement
      repeat
        aTargetPosition := PointPath[ Max(0, I) ];
        ClosestPolygon := gAIFields.NavMesh.KMPoint2Polygon[ aTargetPosition ];
        I := I - 1;
      until (I < 0) OR ( (InitPolygon <> ClosestPolygon)
                         AND (tpWalk in gTerrain.Land^[aTargetPosition.Y, aTargetPosition.X].Passability)
                         AND (aOrderAttack OR (KMDistanceSqr(aActualPosition, aTargetPosition) > sqr(AI_Par[ATTACK_SQUAD_MinWalkingDistance]))) );

      {$IFDEF DEBUG_NewAI}
      DEBUGPointPath := PointPath;
      {$ENDIF}
    end;
  end;
  Result := True;
end;


{ TAICompany }
constructor TAICompany.Create(aOwner: TKMHandID; aCompanyMode: TKMCompanyMode);
var
  GT: TKMGroupType;
begin
  inherited Create;
  fOwner := aOwner;

  fCompanyMode := aCompanyMode;
  fTargetPoint := KMPOINT_ZERO;
  fTargetOwner := -1;
  fTargetHouse := nil;
  fTargetUnit := nil;

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    fSquads[GT] := TKMList.Create();
end;


destructor TAICompany.Destroy();
var
  GT: TKMGroupType;
begin
  gHands.CleanUpUnitPointer(fTargetUnit);
  gHands.CleanUpHousePointer(fTargetHouse);
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    fSquads[GT].Free;
  inherited;
end;


constructor TAICompany.Load(LoadStream: TKMemoryStream);
var
  I,Cnt: Integer;
  GT: TKMGroupType;
begin
  inherited Create;
  LoadStream.CheckMarker('Company');
  LoadStream.Read(fOwner);
  LoadStream.Read(fPathPosition);
  LoadStream.Read(fScanPosition);
  LoadStream.Read(fCompanyMode, SizeOf(TKMCompanyMode));
  LoadStream.Read(fTargetPoint);
  LoadStream.Read(fTargetOwner);
  LoadStream.Read(fTargetUnit, 4);
  LoadStream.Read(fTargetHouse, 4);

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    fSquads[GT] := TKMList.Create();
    LoadStream.Read(Cnt);
    for I := 0 to Cnt - 1 do
      fSquads[GT].Add( TAISquad.Load(LoadStream) );
  end;
end;


procedure TAICompany.Save(SaveStream: TKMemoryStream);
var
  I,Cnt: Integer;
  GT: TKMGroupType;
begin
  SaveStream.PlaceMarker('Company');
  SaveStream.Write(fOwner);
  SaveStream.Write(fPathPosition);
  SaveStream.Write(fScanPosition);
  SaveStream.Write(fCompanyMode, SizeOf(TKMCompanyMode));
  SaveStream.Write(fTargetPoint);
  SaveStream.Write(fTargetOwner);
  if (fTargetUnit <> nil) then
    SaveStream.Write(fTargetUnit.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetHouse <> nil) then
    SaveStream.Write(fTargetHouse.UID) //Store ID
  else
    SaveStream.Write(Integer(0));

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    Cnt := fSquads[GT].Count;
    SaveStream.Write(Cnt);
    for I := 0 to Cnt - 1 do
      TAISquad( Squads[GT].Items[I] ).Save(SaveStream);
  end;
end;


procedure TAICompany.SyncLoad();
var
  I: Integer;
  GT: TKMGroupType;
begin
  fTargetUnit := gHands.GetUnitByUID(Integer(fTargetUnit) );
  fTargetHouse := gHands.GetHouseByUID(Integer(fTargetHouse) );
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    for I := 0 to fSquads[GT].Count - 1 do
      TAISquad( Squads[GT].Items[I] ).SyncLoad();
end;


function TAICompany.SetTarget(aHouse: TKMHouse; aUnit: TKMUnit = nil): Boolean;
begin
  Result := False;
  if (aHouse = nil) AND (aUnit <> nil) then
  begin
    gHands.CleanUpUnitPointer(fTargetUnit);
    fTargetUnit := aUnit.GetPointer;
    fTargetOwner := aUnit.Owner;
    Result := True;
  end
  else if (aHouse <> nil) AND (aUnit = nil) then
  begin
    gHands.CleanUpHousePointer(fTargetHouse);
    fTargetHouse := aHouse.GetPointer;
    fTargetOwner := aHouse.Owner;
    Result := True;
  end;
end;


function TAICompany.GetTargetPosition(): TKMPoint;
begin
  Result := KMPOINT_ZERO;
  if (fTargetHouse <> nil) AND not fTargetHouse.IsDestroyed then
    Result := fTargetHouse.Position
  else if (fTargetUnit <> nil) AND not fTargetUnit.IsDead then
    Result := fTargetUnit.CurrPosition;
end;


// Update state of company, detect targets, call functions for orders
procedure TAICompany.UpdateState(aTick: Cardinal);
  // Check if target still exist
  function CheckPrimaryTarget(): Boolean;
  begin
    Result := False;
    if (fTargetHouse <> nil) then
      if fTargetHouse.IsDestroyed then
        gHands.CleanUpHousePointer(fTargetHouse)
      else
        Result := True;
    if (fTargetUnit <> nil) then
      if fTargetUnit.IsDead then
        gHands.CleanUpUnitPointer(fTargetUnit)
      else
        Result := True;
  end;
  // Check if are all squads in position and we can continue
  function SquadsInPosition(): Boolean;
  var
    I: Integer;
    GT: TKMGroupType;
    Squad: TAISquad;
  begin
    Result := True;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      for I := fSquads[GT].Count - 1 downto 0 do
      begin
        Squad := fSquads[GT].Items[I];
        if (Squad = nil) OR (Squad.Group = nil) OR Squad.Group.IsDead then
          fSquads[GT].Delete(I)
        else
          Result := Result AND Squad.OnPlace;
      end;
  end;
  // Filter target houses according to house type and influence
  procedure FilterTargetHouses(var aHA: TKMHouseArray);
  var
    K, Cnt: Integer;
  begin
    if (fCompanyMode = cmDestruction) then
      Exit;
    Cnt := 0;
    for K := Low(aHA) to High(aHA) do
      if (aHA[K].HouseType in SCAN_HOUSES) OR
        (gAIFields.Influences.OwnPoint[fOwner, aHA[K].Position] > 100) OR
        (gAIFields.Influences.GetBestAllianceOwnership(fOwner, aHA[K].Position.X, aHA[K].Position.Y, atAlly) > 50) then
      begin
        aHA[Cnt] := aHA[K];
        Cnt := Cnt + 1;
      end;
    SetLength(aHA,Cnt);
  end;
  // Update state of Squads
  procedure UpdateSquadsState();
  var
    I: Integer;
    GT: TKMGroupType;
  begin
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      for I := fSquads[GT].Count - 1 downto 0 do
        TAISquad( fSquads[GT].Items[I] ).UpdateState(aTick);
  end;
var
  InPosition: Boolean;
  {$IFDEF DEBUG_NewAI}
  I: Integer;
  {$ENDIF}
  ScanRad, SQRRadius: Single;
  HA: TKMHouseArray;// Target houses in radius
  UA: TKMUnitArray; // Target closest soldiers in radius
  UGA: TKMUnitGroupArray; // Group of closest soldiers in radius (reference to UA)
  ClosestHouse: TKMHouse;
  ClosestUnit: TKMUnit;
begin
  if (SquadCnt = 0) then
    Exit;

  InPosition := SquadsInPosition();

  // Check target
  ClosestHouse := nil;
  ClosestUnit := nil;

  fScanPosition := GetPosition(SQRRadius);

  ScanRad := sqr(AI_Par[ATTACK_COMPANY_AttackRadius]) + Min(50*50, SQRRadius);
  HA := gHands.GetHousesInRadius(ScanPosition, sqr(AI_Par[ATTACK_COMPANY_AttackRadius]), fOwner, atEnemy, ALL_HOUSES, False);
  UA := gHands.GetGroupsMemberInRadius(ScanPosition, ScanRad, fOwner, atEnemy, UGA);
  FilterTargetHouses(HA);

  {$IFDEF DEBUG_NewAI}
  DEBUGScanRad := ScanRad;
  SetLength(DEBUG_UA_POINTS, Length(UA));
  SetLength(DEBUG_UGA_POINTS, Length(UA));
  for I := 0 to Length(UA) - 1 do
  begin
    DEBUG_UA_POINTS[I] := UA[I].CurrPosition;
    DEBUG_UGA_POINTS[I] := UGA[I].Position;
  end;
  {$ENDIF}

  // Actualize target
  if not CheckPrimaryTarget() then
  begin
    if (Length(UA) > 0) then
      ClosestUnit := UA[0];
    if (Length(HA) > 0) then
      ClosestHouse := HA[0];
    if not SetTarget(ClosestHouse, ClosestUnit) then
    begin
      if ActualizeTarget(fScanPosition, ClosestHouse, ClosestUnit, False) then
      begin
        if not gAIFields.Supervisor.FFA
          OR ((ClosestHouse <> nil) AND (ClosestHouse.Owner = fTargetOwner))
          OR ((ClosestUnit <> nil) AND (ClosestUnit.Owner = fTargetOwner)) then
          SetTarget(ClosestHouse, ClosestUnit)
        else
          fState := csIdle;
      end;
    end;
  end;

  // Give new orders
  if OrderToAttack(ScanPosition, UA, UGA, HA) then
    fState := csAttack
  else
  begin
    fState := csWalking;
    if InPosition AND not OrderMove(aTick, PathPosition) then
        fState := csIdle;
  end;

  UpdateSquadsState();
end;


function TAICompany.OrderToAttack(aActualPosition: TKMPoint; UA: TKMUnitArray; UGA: TKMUnitGroupArray; HA: TKMHouseArray): Boolean;
type
  TCompanyInfo = record
    GTCnt: array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of Word; // Count of soldiers sorted by GroupType
  end;
var
  AvailableSquads: TKMSquadsArray;
  CompanyInfo: TCompanyInfo;
  TargetU: TKMTargetSelection;

  // Find only available squads (we can give orders to them) and return if there are any
  function FindAvailableSquads(var aSquadsInCombat: Boolean): Boolean;
  var
    Output: Boolean;
    I: Integer;
    GT: TKMGroupType;
    Squad: TAISquad;
  begin
    Output := False;
    aSquadsInCombat := False;
    FillChar(CompanyInfo, SizeOf(CompanyInfo), #0);
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    begin
      AvailableSquads[GT].Count := 0;
      SetLength(AvailableSquads[GT].Squads, fSquads[GT].Count);
    end;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      for I := 0 to fSquads[GT].Count - 1 do
      begin
        Squad := fSquads[GT].Items[I];
        Inc(CompanyInfo.GTCnt[GT], Squad.Group.Count);
        if not Squad.InFight then
        begin
          Output := True;
          // Make sure that unit will not hunt target over the whole map and better stay inside company
          if (GT <> gtRanged) then // ranged units are fixed in Squad class(set / reset target cause that they dont shoot and just reset animation)
          begin
            Squad.TargetUnit := nil;
            //Squad.TargetHouse := nil; // This cause same problem like with archers
          end;
          AvailableSquads[GT].Squads[ AvailableSquads[GT].Count ] := Squad;
          Inc(AvailableSquads[GT].Count);
        end
        else
          aSquadsInCombat := True;
      end;
    Result := Output;
  end;

  // Calculate evaluation to enemy groups in specific radius
  procedure EvalEnemyGroupsInRadius();
  const
    INIT_DIST = 1000000;
    SQR_MAX_RANGE_INTEREST = 12*12;
    SQR_RANGE_OF_PROJECTILES = 11*11; // Sqr(  Max( Max(RANGE_BOWMAN_MAX,RANGE_ARBALETMAN_MAX),RANGE_SLINGSHOT_MAX )  );
  var
    Polygon: Word;
    I,K,L: Integer;
    SqrDist, SqrClosestDist, SqrClosestDistToRanged, CloseCombatProtection: Single;
    GT: TKMGroupType;
    Squad: TAISquad;
    GroupsInFightArr: TKMUnitGroupArray;
  begin
    SetLength(TargetU, Length(UA));
    for I := 0 to Length(TargetU) - 1 do
    begin
      // Get closest distance to Ranged groups and all groups
      SqrClosestDist := INIT_DIST;
      SqrClosestDistToRanged := INIT_DIST;
      for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
        for K := 0 to fSquads[GT].Count - 1 do
        begin
          Squad := fSquads[GT].Items[K];
          L := 0;
          while (L < Squad.Group.Count) do
          begin
            SqrDist := KMDistanceSqr(Squad.Group.Members[L].CurrPosition, UA[I].CurrPosition);
            if (SqrDist < SqrClosestDist) then
              SqrClosestDist := SqrDist;
            if (GT = gtRanged) AND (SqrDist < SqrClosestDistToRanged) then
              SqrClosestDistToRanged := SqrDist;
            L := L + 3;
          end;
        end;

      // Calculate threat level
      with TargetU[I] do
      begin
        Index := I;
        CenterPoint := UGA[I].Position;
        CloseThreat := 0;
        DistantThreat := 0;
        if (SqrClosestDist < SQR_MAX_RANGE_INTEREST) then
        begin
          CloseThreat := UGA[I].Count; // Threat level for close combat units
          DistantThreat := CloseThreat; // Threat level for ranged group type
          if (UGA[I].GroupType = gtRanged) then
          begin
            // Calculate distant threat level (determine whether archers are shooting at our troops)
            CloseThreat := CloseThreat * AI_Par[ATTACK_COMPANY_AttackRangedGain];
            DistantThreat := DistantThreat * Byte(SQR_RANGE_OF_PROJECTILES > SqrClosestDist) * AI_Par[ATTACK_COMPANY_AttackRangedGain];
            // Close threat level is computed with using influences
            CloseCombatProtection := 0;
            Polygon := gAIFields.NavMesh.KMPoint2Polygon[ UGA[I].Position ];
            for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
              if (GT <> gtRanged) then // Skip ranged units so AI have a tendency to walk through them if they are alone
                CloseCombatProtection := CloseCombatProtection + gAIFields.Influences.EnemyGroupPresence[fOwner, Polygon, GT]; // Tune parameters
            if (DistantThreat < CloseCombatProtection) then // Ranged units are well protected -> try shoot them
              CloseThreat := Byte(SqrClosestDistToRanged <= SQR_RANGE_OF_PROJECTILES) // Lowest priority (=1) but dont ignore them (if we have no archers call infantry)
            else // Ranged units are not protected -> attack with close combat units
              DistantThreat := Byte(SqrClosestDist <= SQR_RANGE_OF_PROJECTILES); // Lowest priority but dont ignore them (if they are in range of archers shoot at them)
          end
          else
          begin
            // Close combat threat level (in case that group already fight agaist more soldiers there is 0 threat)
            // In case that group kills ranged units threat must be increased
            if UGA[I].InFightAgaistGroups(GroupsInFightArr) then
            begin
              for K := 0 to Length(GroupsInFightArr) - 1 do
                if (GroupsInFightArr[K].GroupType = gtRanged) then
                  CloseThreat := CloseThreat + GroupsInFightArr[K].Count
                else
                  CloseThreat := CloseThreat - GroupsInFightArr[K].Count;
            end
            // If group does not fight check how far is from ranged units
            else
              CloseThreat := CloseThreat + Max(0, (sqr(AI_Par[ATTACK_COMPANY_ProtectRangedRadius]) - SqrClosestDistToRanged) * AI_Par[ATTACK_COMPANY_ProtectRangedGain] );
          end;
        end;
      end;
    end;
  end;

  // Distribute available groups agaist enemies
  function SelectTargetGroups(): Boolean;
  const
    INIT_THREAT = -10000000;
    INIT_DISTANCE = 10000000;
    BEST_TARGET: array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of array[0..3] of TKMGroupType = (
        (gtMelee, gtRanged, gtMounted, gtAntiHorse), // against gtMelee
        (gtMelee, gtRanged, gtAntiHorse, gtMounted), // against gtAntiHorse
        (gtMounted, gtRanged, gtMelee, gtAntiHorse), // against gtRanged
        (gtAntiHorse, gtRanged, gtMounted, gtMelee)  // against gtMounted
    );
  var
    Output: Boolean;
    I,K,L,BestPrioIdx, TargetIdx, ThreatIdx, Cnt: Integer;
    Threat, HighestThreat, Dist, BestDist, BestPrioCoeff: Single;
    GT, BestGT: TKMGroupType;
    Squad: TAISquad;
  begin
    Output := False;
    if (Length(TargetU) <= 0) then
      Exit;

    // Ranged groups view: our ranged unit -> select target => each unit should fire
    GT := gtRanged;
    ThreatIdx := 0; // Only for compiler
    for I := AvailableSquads[GT].Count - 1 downto 0 do
    begin
      Squad := AvailableSquads[GT].Squads[I];
      HighestThreat := INIT_THREAT;
      //BestDist := 0;
      Dist := 0;
      for K := 0 to Length(TargetU) - 1 do
        if (TargetU[K].DistantThreat > 0) then
        begin
          Dist := KMDistanceSqr(TAISquad(fSquads[GT].Items[I]).Position, UA[ TargetU[K].Index ].CurrPosition);
          Threat := TargetU[K].DistantThreat - Dist;
          if (Threat > HighestThreat) then
          begin
            //BestDist := Dist;
            HighestThreat := Threat;
            ThreatIdx := K;
          end;
        end;
      if (HighestThreat <> INIT_THREAT) then
      begin
        Output := True;
        Squad.TargetUnit := UA[ TargetU[ThreatIdx].Index ];
        if (Dist >= sqr(AI_Par[ATTACK_COMPANY_ProtectRangedAllInDist])) then // If is enemy too cloose aim him but dont decrease threat level so next part of code can call close combat support
          TargetU[ThreatIdx].DistantThreat := TargetU[ThreatIdx].DistantThreat - Squad.Group.Count;
        // Unit will be targeted by Ranged group -> if there was minimal priority for close combat then remove it
        if (TargetU[ThreatIdx].CloseThreat = 1) then
          TargetU[ThreatIdx].CloseThreat := 0;
        Dec(AvailableSquads[GT].Count);
        AvailableSquads[GT].Squads[I] := AvailableSquads[GT].Squads[ AvailableSquads[GT].Count ];
      end
      else
        Squad.TargetUnit := nil;
    end;

    // Close combat groups view: enemy units -> select oponent => keep something in reserve
    Cnt := Length(TargetU);
    BestGT := gtMelee;
    TargetIdx := 0; // For compiler
    BestPrioIdx := 0; // For compiler
    BestPrioCoeff := 1; // For compiler
    repeat
      // Find top threat
      HighestThreat := INIT_THREAT;
      for I := Cnt - 1 downto 0 do
      begin
        if (TargetU[I].CloseThreat <= 0) then
        begin
          Cnt := Cnt - 1;
          TargetU[I] := TargetU[Cnt];
        end
        else if (TargetU[I].CloseThreat > HighestThreat) then
        begin
          HighestThreat := TargetU[I].CloseThreat;
          ThreatIdx := I;
        end;
      end;
      if (Cnt <= 0) then
        break;
      // Find best close combat opponent
      BestDist := INIT_DISTANCE;
      for K := 0 to 3 do
      begin
        GT := BEST_TARGET[  UGA[ TargetU[ThreatIdx].Index ].GroupType, K  ];
        for L := 0 to AvailableSquads[GT].Count - 1 do
        begin
          Dist := KMDistanceSqr(AvailableSquads[GT].Squads[L].Position, UA[ TargetU[ThreatIdx].Index ].CurrPosition) + K * AI_Par[ATTACK_COMPANY_GroupTypePenalization];
          if (Dist < BestDist) then
          begin
            BestDist := Dist;
            TargetIdx := L;
            BestGT := GT;
            BestPrioIdx := K;
          end;
        end;
      end;
      // Set target
      if (BestDist < INIT_DISTANCE) then
      begin
        Output := True;
        Squad := AvailableSquads[BestGT].Squads[TargetIdx];
        Squad.TargetUnit := UA[ TargetU[ThreatIdx].Index ];
        // Update threat level and call another squad if needed
        case BestPrioIdx of
          0: BestPrioCoeff := AI_Par[ATTACK_COMPANY_DecreaseThreat_Prio1];
          1: BestPrioCoeff := AI_Par[ATTACK_COMPANY_DecreaseThreat_Prio2];
          2: BestPrioCoeff := AI_Par[ATTACK_COMPANY_DecreaseThreat_Prio3];
          3: BestPrioCoeff := AI_Par[ATTACK_COMPANY_DecreaseThreat_Prio4];
        end;
        TargetU[ThreatIdx].CloseThreat := TargetU[ThreatIdx].CloseThreat - Squad.Group.Count * BestPrioCoeff;
        Dec(AvailableSquads[BestGT].Count);
        AvailableSquads[BestGT].Squads[TargetIdx] := AvailableSquads[BestGT].Squads[ AvailableSquads[BestGT].Count ];
      end;
    until (Cnt <= 0) OR (BestDist >= INIT_DISTANCE);
    Result := Output;
  end;


  function OrderAttackHouse(): Boolean;
  const
    INIT_THREAT = 1000000;
    LIMIT_RANGED_DESTROY_ALL_HOUSES = 6; // Archers will start to shoot at other houses if there is not enough close combat units in company
    CLOSE_COMBAT_DISTANCE_LIMIT = 12; // Order to attack house starts at this distance (close combat groups; watchtowers have exception)
    MAX_SOLDIERS_VS_HOUSE = 12;
    MAX_ARCHERS_VS_TOWER = 9;
    ATTACK_WATCHTOWER_WITH_CLOSE_COMBAT_DIST = 5; // Attack watchtower with close combat units
    MAX_CLOSE_COMBAT_VS_UNIT_DIST = 3; // Kill citizens in this radius around house (workers tries to repair house)
  var
    Output: Boolean;
    TargetIdx: Word;
    I,K: Integer;
    Dist, BestDist: Single;
    GT: TKMGroupType;
    U: TKMUnit;
    GroupAttackCnt: TKMWordArray;
  begin
    Output := False;
    SetLength(GroupAttackCnt, Length(HA));
    FillChar(GroupAttackCnt[0], SizeOf(GroupAttackCnt[0]) * Length(GroupAttackCnt), #0);

    // Target watchtowers with archers
    GT := gtRanged;
    TargetIdx := 0; // Only for compiler
    for I := AvailableSquads[GT].Count - 1 downto 0 do
    begin
      BestDist := INIT_THREAT;
      for K := 0 to Length(HA) - 1 do
        if (HA[K].HouseType = htWatchTower)
          AND (GroupAttackCnt[K] < MAX_ARCHERS_VS_TOWER)
          AND (HA[K].CheckWareIn(wtStone) > 1)
          AND HA[K].HasOwner then // Ignore towers without stone and without recruit inside
        begin
          Dist := KMDistanceSqr(AvailableSquads[GT].Squads[I].Position, HA[K].Position);
          if (Dist < BestDist) then
          begin
            BestDist := Dist;
            TargetIdx := K;
          end;
        end;
      if (BestDist <> INIT_THREAT) then
      begin
        Output := True;
        Inc(GroupAttackCnt[TargetIdx], AvailableSquads[GT].Squads[I].Group.Count);
        // Find and kill workers who want to repair house
        U := gAIFields.Eye.GetClosestUnitAroundHouse(HA[TargetIdx].HouseType, HA[TargetIdx].Position, AvailableSquads[GT].Squads[I].Position);
        if (U <> nil) then
          AvailableSquads[GT].Squads[I].TargetUnit := U
        else
          AvailableSquads[GT].Squads[I].TargetHouse := HA[TargetIdx];
        Dec(AvailableSquads[GT].Count);
        AvailableSquads[GT].Squads[I] := AvailableSquads[GT].Squads[ AvailableSquads[GT].Count ];
      end;
    end;

    // Target everything else with close combat units
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    begin
      if GT = gtRanged then // Ignore ranged units if we have enough close combat support (avoid friendly fire)
        with CompanyInfo do
          if (GTCnt[gtMelee] + GTCnt[gtAntiHorse] + GTCnt[gtMounted] > LIMIT_RANGED_DESTROY_ALL_HOUSES) then
            continue;
      for I := AvailableSquads[GT].Count - 1 downto 0 do
      begin
        BestDist := INIT_THREAT;
        for K := 0 to Length(HA) - 1 do
          if (
               (HA[K].HouseType <> htWatchTower)
               OR ( KMDistanceSqr(AvailableSquads[GT].Squads[I].Position, HA[K].Position) < sqr(ATTACK_WATCHTOWER_WITH_CLOSE_COMBAT_DIST))
             )
            AND (GroupAttackCnt[K] < MAX_SOLDIERS_VS_HOUSE) then
          begin
            Dist := KMDistanceSqr(AvailableSquads[GT].Squads[I].Position, HA[K].Position);
            if (Dist < BestDist) AND (Dist <= sqr(CLOSE_COMBAT_DISTANCE_LIMIT)) then
            begin
              BestDist := Dist;
              TargetIdx := K;
            end;
          end;
        if (BestDist <> INIT_THREAT) then
        begin
          Output := True;
          GroupAttackCnt[TargetIdx] := GroupAttackCnt[TargetIdx] + AvailableSquads[GT].Squads[I].Group.Count;
          // Find and kill workers who want to repair house
          U := gAIFields.Eye.GetClosestUnitAroundHouse(HA[TargetIdx].HouseType, HA[TargetIdx].Position, AvailableSquads[GT].Squads[I].Position);
          if (U <> nil)
            AND ((GT = gtRanged) OR (KMDistanceSqr(U.CurrPosition, AvailableSquads[GT].Squads[I].Position) < sqr(MAX_CLOSE_COMBAT_VS_UNIT_DIST))) then
            AvailableSquads[GT].Squads[I].TargetUnit := U
          else
            AvailableSquads[GT].Squads[I].TargetHouse := HA[TargetIdx];
          Dec(AvailableSquads[GT].Count);
          AvailableSquads[GT].Squads[I] := AvailableSquads[GT].Squads[ AvailableSquads[GT].Count ];
        end;
      end;
    end;
    Result := Output;
  end;


  procedure Regroup();
  var
    Cnt: Word;
    I: Integer;
    GT: TKMGroupType;
    Squad: TAISquad;
    Positions: TKMPointArray;
    InitPolygons: TKMWordArray;
  begin
    Cnt := 0;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      Cnt := Cnt + AvailableSquads[GT].Count;

    if (Cnt = 0) then
      Exit;

    // Get positions around center of scan area
    SetLength(InitPolygons,1);
    InitPolygons[0] := gAIFields.NavMesh.KMPoint2Polygon[ fScanPosition ];
    gAIFields.NavMesh.Positioning.FindPositions(Cnt, Round(AI_Par[ATTACK_COMPANY_MinCombatSpacing]), InitPolygons, Positions);

    Cnt := 0;
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      for I := 0 to AvailableSquads[GT].Count - 1 do
      begin
        Squad := AvailableSquads[GT].Squads[I];
        Squad.FinalPosition := KMPointDir(  Positions[Cnt], KMGetDirection( Squad.Position, Positions[Cnt] )  );
        // Time limit does not make sence for unit in combat
        //Squad.TimeLimit := gGameParams.GameTick + KMDistanceAbs(Position, Squads[ClosestIdx].Position) * TIME_PER_A_TILE;
        Cnt := Cnt + 1;
        if (Cnt >= Length(Positions)) then
          Exit;
      end;
  end;


var
  FreeSquads, SquadsInCombat, AttackGroup, AttackHouse: Boolean;
begin
  // Find Available squads (groups who obey orders / are not in combat)
  FreeSquads := FindAvailableSquads(SquadsInCombat);
  AttackGroup := False;
  AttackHouse := False;

  // Attack groups in radius
  if (Length(UA) > 0) AND FreeSquads then
  begin
    // Calculate threat level of each enemy group in radius
    EvalEnemyGroupsInRadius();

    // Attack hostile units in radius
    AttackGroup := SelectTargetGroups();
  end;

  // Attack houses in radius
  if (Length(HA) > 0) AND FreeSquads then
    AttackHouse := OrderAttackHouse();

  // Move troops in reserve into new center point
  if AttackGroup OR AttackHouse then
    Regroup();

  Result := AttackGroup OR AttackHouse OR SquadsInCombat;
end;


function TAICompany.OrderMove(aTick: Cardinal; aActualPosition: TKMPoint): Boolean;

  function GetInitPolygons(aCnt: Integer; var aPointPath: TKMPointArray): TKMWordArray;
  var
    InitPolygon: Word;
    I, Idx: Integer;
    InitPolygons: TKMWordArray;
  begin
    // Get initial point on the path (it must be in specific distance from actual position to secure smooth moving of the company)
    I := Length(aPointPath)-1;
    while (I >= 0) AND (KMDistanceAbs(aActualPosition, aPointPath[I]) < AI_Par[ATTACK_COMPANY_MinimumMovement]) do
      I := I - 1;
    // Make sure that platoon will not start in actual polygon but position will be moved forward
    InitPolygon := gAIFields.NavMesh.KMPoint2Polygon[ aActualPosition ];
    repeat
      fPathPosition := aPointPath[ Max(0, I) ];
      I := I - 1;
    until (InitPolygon <> gAIFields.NavMesh.KMPoint2Polygon[ fPathPosition ]) OR (I < 0);

    I := Max(0,I + 1); // I = 0 we are in polygon of our target
    // Get several init polygons
    SetLength(InitPolygons, Min(Length(aPointPath) - I, Max(1, Round(AI_Par[ATTACK_COMPANY_Positioning_InitPolyCnt]))) );
    Idx := 0;
    while (I >= 0) AND (Idx < Length(InitPolygons)) do
    begin
      InitPolygons[Idx] := gAIFields.NavMesh.KMPoint2Polygon[ aPointPath[I] ];
      Idx := Idx + 1;
      I := I - 2;
    end;
    if (Idx <> Length(InitPolygons)) then
      SetLength(InitPolygons, Idx);
    Result := InitPolygons;
  end;

  procedure SetOrders(aCnt: Integer; aTargetPosition: TKMPoint; var aPositions: TKMPointArray);
  const
    INFLUENCE_DANGER = 20;
    INIT_DIST = 1000;
  var
    I, K, Dist, ClosestDist, ClosestIdx, SelectedTime: Integer;
    Dir: TKMDirection;
    Position: TKMPoint;
    GT: TKMGroupType;
    Squads: array of TAISquad;
    AvailableSquads: TBooleanArray;
    TagPositions: TKMPointTagList;
  begin
    // Get influence and set speed of move
    SelectedTime := Round(AI_Par[ATTACK_COMPANY_TimePerATile_Slow]);
    if (gAIFields.Influences.GetBestAllianceOwnership(fOwner, gAIFields.NavMesh.KMPoint2Polygon[aActualPosition], atEnemy) < INFLUENCE_DANGER) then
      SelectedTime := Round(AI_Par[ATTACK_COMPANY_TimePerATile_Fast]);
    // Get formations and set orders
    // Sort points according to aTargetPosition (it should work better than aActualPosition)
    TagPositions := TKMPointTagList.Create;
    try
      for I := 0 to Min(aCnt, High(aPositions)) do
        TagPositions.Add(aPositions[I], KMDistanceAbs(aTargetPosition, aPositions[I]));
      TagPositions.SortByTag();
      //fScanPosition := TagPositions.Items[TagPositions.Count - 1];
      //fPathPosition := TagPositions.Items[(TagPositions.Count - 1) div 2];
      Dir := KMGetDirection( aActualPosition, fPathPosition );

      SetLength(Squads, aCnt);
      SetLength(AvailableSquads, aCnt);
      K := 0;
      for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
        for I := 0 to fSquads[GT].Count - 1 do
        begin
          Squads[K] := fSquads[GT].Items[I];
          AvailableSquads[K] := True;
          K := K + 1;
        end;
      ClosestIdx := 0; // Only for compiler
      for I := TagPositions.Count - 1 downto 0 do
      begin
        Position := TagPositions.Items[I];
        ClosestDist := INIT_DIST;
        for K := 0 to Length(Squads) - 1 do
          if AvailableSquads[K] then
          begin
            Dist := KMDistanceAbs(Position, Squads[K].Position);
            if (Dist < ClosestDist) then
            begin
              ClosestDist := Dist;
              ClosestIdx := K;
            end;
          end;
        if (ClosestDist = INIT_DIST) then
          break;

        Squads[ClosestIdx].TargetUnit := nil;
        Squads[ClosestIdx].TargetHouse := nil;
        Squads[ClosestIdx].FinalPosition := KMPointDir(Position, Dir);
        Squads[ClosestIdx].WalkTimeLimit := aTick + KMDistanceAbs(Position, Squads[ClosestIdx].Position) * SelectedTime;
        AvailableSquads[ClosestIdx] := False;
      end;
    finally
      TagPositions.Free;
    end;
  end;

var
  Distance: Word;
  Cnt: Integer;
  TargetPoint: TKMPoint;
  PointPath, Positions: TKMPointArray;
  InitPolygons: TKMWordArray;
begin
  if (fCompanyMode = cmDefence) then
  begin
    TargetPoint := fTargetPoint;
    Result := not KMSamePoint(TargetPoint, aActualPosition);
  end
  else
  begin
    TargetPoint := GetTargetPosition();
    Result := not KMSamePoint(TargetPoint, KMPOINT_ZERO);
  end;

  if not Result then
    Exit;

  // Try find path to the target point
  //if gAIFields.NavMesh.Pathfinding.ShortestRoute(aActualPosition, TargetPoint, Distance, PointPath) then
  if gAIFields.NavMesh.Pathfinding.AvoidTrafficRoute(fOwner, aActualPosition, TargetPoint, Distance, PointPath) then
  begin
    Cnt := SquadCnt(); // Count of groups in company

    // Get init polygons -> polygons on road which are base for army positioning (groups will get order to walk there)
    InitPolygons := GetInitPolygons(Cnt, PointPath);

    // Get positions around init polygons
    gAIFields.NavMesh.Positioning.FindPositions(Cnt, Round(AI_Par[ATTACK_COMPANY_MinWalkSpacing]), InitPolygons, Positions);

    // Compute distance of new positions and select the closest group to walk there
    SetOrders(Cnt, TargetPoint, Positions);
  end;

  {$IFDEF DEBUG_NewAI}
  DEBUGPointPath := PointPath;
  {$ENDIF}
end;


function TAICompany.SquadCnt(aTypes: TKMGroupTypeSet = GROUP_TYPES_VALID): Word;
var
  GT: TKMGroupType;
begin
  Result := 0;
  for GT in aTypes do
    Result := Result + fSquads[GT].Count;
end;

function TAICompany.WarriorCnt(aTypes: TKMGroupTypeSet = GROUP_TYPES_VALID): Word;
var
  K: Integer;
  G: TKMUnitGroup;
  GT: TKMGroupType;
begin
  Result := 0;
  for GT in aTypes do
    for K := 0 to fSquads[GT].Count - 1 do
    begin
      G := TAISquad(fSquads[GT].Items[K]).Group;
      if (G <> nil) then
        Result := Result + G.Count;
    end;
end;


procedure TAICompany.AddSquad(aGroup: TKMUnitGroup);
begin
  fSquads[ aGroup.GroupType ].Add( TAISquad.Create(aGroup) );
end;


procedure TAICompany.DeleteSquad(aGT: TKMGroupType; aIdx: Integer);
var
  Squad: TAISquad;
begin
  Squad := fSquads[aGT].Items[aIdx];
  Squad.Free;
  fSquads[aGT].Delete(aIdx);
end;


procedure TAICompany.InitCompany();
begin
  fPathPosition := GetPosition();
  fScanPosition := fPathPosition;
end;


function TAICompany.GetPosition(): TKMPoint;
var
  I, Count: Integer;
  Output: TKMPoint;
  G: TKMGroupType;
begin
  Output := KMPOINT_ZERO;
  Count := 0;
  for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    for I := 0 to fSquads[G].Count-1 do
    begin
      Output := KMPointAdd(Output, TAISquad(fSquads[G].Items[I]).Group.Position);
      Count := Count + 1;
    end;

  if (Count > 0) then
  begin
    Output.X := Round( Output.X / Count );
    Output.Y := Round( Output.Y / Count );
  end;

  // If we cannot walk there choose first group instead
  I := gAIFields.NavMesh.KMPoint2Polygon[Output];
  if not (gTerrain.CheckPassability(Output, tpWalk)
          AND ((gAIFields.Influences.PresenceAllGroups[ fOwner, I ] > 0)
               //OR (gAIFields.Influences.ArmyTraffic[ fOwner, I ] > 0)
              )
         ) then
    for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      if (fSquads[G].Count > 0) then
      begin
        Output := TAISquad(fSquads[G].Items[0]).Group.Position;
        break;
      end;
  Result := Output;
end;


function TAICompany.GetPosition(var aSQRRadius: Single): TKMPoint;
var
  I: Integer;
  G: TKMGroupType;
begin
  Result := GetPosition();

  aSQRRadius := 0;
  for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    for I := 0 to fSquads[G].Count-1 do
      aSQRRadius := Max(aSQRRadius, KMDistanceSqr(Result, TAISquad(fSquads[G].Items[I]).Group.Position));
end;


function TAICompany.ActualizeTarget(aInitPosition: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; aAimCivilians: Boolean = True): Boolean;
var
  TargetGroup: TKMUnitGroup;
begin
  Result := False;
  aTargetUnit := nil;
  aTargetHouse := gHands.GetClosestHouse(aInitPosition, fOwner, atEnemy, TARGET_HOUSES, True);
  if (aTargetHouse = nil) then
  begin
    TargetGroup := gHands.GetClosestGroup(aInitPosition, fOwner, atEnemy);
    if (TargetGroup = nil) then
    begin
      if not aAimCivilians then
      begin
        aTargetHouse := gHands.GetClosestHouse(aInitPosition, fOwner, atEnemy, ALL_HOUSES, True);
        if (aTargetHouse <> nil) then
          fCompanyMode := cmDestruction;
      end
      else
      begin
        aTargetUnit := gHands.GetClosestUnit(aInitPosition, fOwner, atEnemy);
        if (aTargetUnit = nil) then
          Exit;
      end;
    end
    else
      aTargetUnit := TargetGroup.GetAliveMember;
  end;
  Result := True;
end;


function TAICompany.IsGroupInCompany(aGroup: TKMUnitGroup): Boolean;
var
  I: Integer;
  G: TKMGroupType;
begin
  Result := True;
  for G := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    for I := 0 to fSquads[G].Count-1 do
      if (TAISquad(fSquads[G].Items[I]).Group = aGroup) then
        Exit;
  Result := False;
end;


{ TKMArmyAttack }
constructor TKMArmyAttack.Create(aOwner: TKMHandID);
begin
  inherited Create;
  fCompanies := TKMList.Create();
  fOwner := aOwner;
end;


destructor TKMArmyAttack.Destroy;
begin
  fCompanies.Free;
  inherited;
end;


procedure TKMArmyAttack.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('ArmyAttack');
  SaveStream.Write(fOwner);
  SaveStream.Write( Integer(Count) );

  for I := 0 to Count - 1 do
    Company[I].Save(SaveStream);
end;


procedure TKMArmyAttack.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
begin
  LoadStream.CheckMarker('ArmyAttack');
  LoadStream.Read(fOwner);
  LoadStream.Read(NewCount);

  for I := 0 to NewCount - 1 do
    fCompanies.Add( TAICompany.Load(LoadStream) );
end;


procedure TKMArmyAttack.SyncLoad();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Company[I].SyncLoad();
end;


procedure TKMArmyAttack.AfterMissionInit();
begin

end;


procedure TKMArmyAttack.UpdateState(aTick: Cardinal);
  function DetectEnemyPresence(aPoint: TKMPoint): Boolean;
  var
    Idx: Integer;
    GT: TKMGroupType;
  begin
    Result := True;
    Idx := gAIFields.NavMesh.KMPoint2Polygon[aPoint];
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      if (gAIFields.Influences.EnemyGroupPresence[ fOwner, Idx, GT ] > 0) then
        Exit;
    Result := False;
  end;
var
  I: Integer;
  Company: TAICompany;
begin
  for I := Count - 1 downto 0 do
  begin
    Company := fCompanies.Items[I];
    Company.UpdateState(aTick);
    if (  (Company.SquadCnt = 0) OR (Company.State = csIdle)  ) then
      //AND (  (Company.CompanyMode in [cmAttack, cmDestruction]) OR not DetectEnemyPresence(Company.TargetPoint)  )  then
      fCompanies.Remove( Company );
  end;
end;


procedure TKMArmyAttack.OwnerUpdate(aPlayer: TKMHandID);
var
  I: Integer;
begin
  fOwner := aPlayer;
  for I := Count - 1 downto 0 do
    Company[I].Owner := aPlayer;
end;


function TKMArmyAttack.GetCount(): Integer;
begin
  Result := fCompanies.Count;
end;


function TKMArmyAttack.GetCompany(aIdx: Integer): TAICompany;
begin
  Result := fCompanies.Items[aIdx];
end;


function TKMArmyAttack.IsGroupInAction(aGroup: TKMUnitGroup): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if Company[I].IsGroupInCompany(aGroup) then
      Exit;
  Result := False;
end;


procedure TKMArmyAttack.CreateCompany(aTargetPoint: TKMPoint; aGroups: TKMUnitGroupArray; aCompanyMode: TKMCompanyMode = cmAttack);
  procedure PrepareCompany(var aCompany: TAICompany; aTargetHouse: TKMHouse; aTargetUnit: TKMUnit);
  var
    I: Integer;
  begin
    aCompany.SetTarget(aTargetHouse, aTargetUnit);
    for I := 0 to Length(aGroups) - 1 do
      aCompany.AddSquad( aGroups[I] );
    aCompany.InitCompany();
  end;
var
  Company: TAICompany;
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit := nil;
  Company := TAICompany.Create(fOwner, aCompanyMode);
  fCompanies.Add( Company );
  if (aCompanyMode = cmAttack) AND Company.ActualizeTarget(aTargetPoint, TargetHouse, TargetUnit) then
  begin
    PrepareCompany(Company, TargetHouse, TargetUnit);
  end
  else if (aCompanyMode = cmDefence) then
  begin
    Company.TargetPoint := aTargetPoint;
    PrepareCompany(Company, TargetHouse, TargetUnit);
  end
  else
    fCompanies.Remove( Company );
end;


procedure TKMArmyAttack.LogStatus(var aBalanceText: UnicodeString);
begin
  //aBalanceText := '';
end;


procedure TKMArmyAttack.Paint;
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $0000FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  CompanySelected: Boolean;
  K,L: Integer;
  {$IFDEF DEBUG_NewAI}
  M: Integer;
  {$ENDIF}
  Col, CompOpacity1, CompOpacity2, GroupOpacity1, GroupOpacity2: Cardinal;
  Position: TKMPoint;
  GT: TKMGroupType;
  Company: TAICompany;
  Squad: TAISquad;
begin
  //if (fOwner <> gMySpectator.HandID) then // Show just 1 player (it prevents notification to be mess)
  //  Exit;
  //if (fOwner <> 1) then // Show just 1 player (it prevents notification to be mess)
  //  Exit;
  CompanySelected := False;
  for K := 0 to Count - 1 do
  begin
    // Company status log
    Company := fCompanies.Items[K];
    CompOpacity1 := $09000000;
    CompOpacity2 := $33000000;
    // Check if company is selected
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      for L := Company.Squads[GT].Count - 1 downto 0 do
        if ((gMySpectator.Selected is TKMUnitGroup) AND (gMySpectator.Selected = TAISquad(Company.Squads[GT].Items[L]).Group)) then
        begin
          CompanySelected := True;
          CompOpacity1 :=  $20000000;
          CompOpacity2 :=  $FF000000;
        end;

    // Pathfinding (squads) + targets
    if CompanySelected then
      for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
        for L := Company.Squads[GT].Count - 1 downto 0 do
        begin
          Squad := Company.Squads[GT].Items[L];
          // Highlight selected group and squad
          GroupOpacity1 := $20000000;
          GroupOpacity2 := $30000000;
          if (gMySpectator.Selected is TKMUnitGroup) AND (gMySpectator.Selected = Squad.Group) then
          begin
            GroupOpacity1 := $99000000;
            GroupOpacity2 := $FF000000;
          end;
          // Order position of group
          Position := Squad.Group.OrderLoc.Loc;
          gRenderAux.CircleOnTerrain(Position.X, Position.Y, 0.25, $FF000000 OR COLOR_YELLOW, $FF000000 OR COLOR_YELLOW);
          // Position
          Position := Squad.Position;
          gRenderAux.CircleOnTerrain(Position.X, Position.Y, 0.75, GroupOpacity2 OR COLOR_GREEN, GroupOpacity2 OR COLOR_GREEN);

          // Target unit
          if (Squad.TargetUnit <> nil) then
          begin
            if not Squad.TargetUnit.IsDeadOrDying then
            begin
              gRenderAux.LineOnTerrain(Position, Squad.TargetUnit.CurrPosition, GroupOpacity2 OR COLOR_RED);
              {$IFDEF DEBUG_NewAI}
              if (Length(Squad.DEBUGPointPath) > 0) then
                for M := Length(Squad.DEBUGPointPath)-2 downto 0 do
                  gRenderAux.LineOnTerrain(Squad.DEBUGPointPath[M+1], Squad.DEBUGPointPath[M], GroupOpacity1 OR COLOR_BLUE);
              {$ENDIF}
            end;
          end
          // Target house
          else if (Squad.TargetHouse <> nil) then
            if not Squad.TargetHouse.IsDestroyed then
            begin
              gRenderAux.LineOnTerrain(Position, Squad.TargetHouse.Position, GroupOpacity2 OR COLOR_RED);
              {$IFDEF DEBUG_NewAI}
              if (Length(Squad.DEBUGPointPath) > 0) then
                for M := Length(Squad.DEBUGPointPath)-2 downto 0 do
                  gRenderAux.LineOnTerrain(Squad.DEBUGPointPath[M+1], Squad.DEBUGPointPath[M], GroupOpacity1 OR COLOR_BLUE);
              {$ENDIF}
            end;
          // Pathfinding
          if not KMSamePoint(Squad.FinalPosition.Loc,KMPOINT_ZERO) then
          begin
            //gRenderAux.LineOnTerrain(Position, Squad.FinalPosition.Loc, $AAFF5555);
            gRenderAux.CircleOnTerrain(Squad.FinalPosition.Loc.X, Squad.FinalPosition.Loc.Y, 1, 0, GroupOpacity1 OR COLOR_BLUE);
            {$IFDEF DEBUG_NewAI}
            if (Length(Squad.DEBUGPointPath) > 0) then
              for M := Length(Squad.DEBUGPointPath)-2 downto 0 do
                gRenderAux.LineOnTerrain(Squad.DEBUGPointPath[M+1], Squad.DEBUGPointPath[M], GroupOpacity1 OR COLOR_BLUE);
            {$ENDIF}
          end;
        end;

    // Change color according to state
    Col := 0; // For compiler
    case Company.State of
      csAttack:  Col := COLOR_RED;
      csWalking: Col := COLOR_BLUE;
      csIdle:    Col := COLOR_BLACK;
    end;

    // Mark company
    Position := Company.ScanPosition;
    {$IFDEF DEBUG_NewAI}
    //gRenderAux.CircleOnTerrain(Position.X, Position.Y, Sqrt(Company.DEBUGScanRad), CompOpacity1 OR Col, CompOpacity2 OR Col);
    {$ENDIF}
    gRenderAux.CircleOnTerrain(Position.X, Position.Y, 3, CompOpacity1 OR Col, CompOpacity2 OR Col);
    Position := Company.PathPosition;
    gRenderAux.CircleOnTerrain(Position.X, Position.Y, 2, CompOpacity1 OR COLOR_GREEN, CompOpacity2 OR COLOR_GREEN);

    // Target aim
    {$IFDEF DEBUG_NewAI}
    for L := 0 to Length(Company.DEBUGTargetU) - 1 do
    with Company.DEBUGTargetU[L] do
    begin
      Position := CenterPoint;
      // Close threath
      Col :=  (Min($FF, Max(0,Round(CloseThreat)) ) shl 24) OR COLOR_RED;
      gRenderAux.CircleOnTerrain(Position.X, Position.Y, 0.5, Col, $FF000000 OR COLOR_RED );
      // Distant threath
      Col :=  (Min($FF, Max(0,Round(DistantThreat)) ) shl 24) OR COLOR_BLACK;
      gRenderAux.CircleOnTerrain(Position.X, Position.Y+1, 0.5, Col, $FF000000 OR COLOR_BLACK );
    end;
    {$ENDIF}

    // Pathfinding (company)
    {$IFDEF DEBUG_NewAI}
    if (Length(Company.DEBUGPointPath) > 0) then
      for L := Length(Company.DEBUGPointPath)-2 downto 0 do
        gRenderAux.LineOnTerrain(Company.DEBUGPointPath[L+1], Company.DEBUGPointPath[L], CompOpacity2 OR COLOR_YELLOW);
    {$ENDIF}
  end;
  //gRenderAux.LineOnTerrain(P1, P2, $CCFF2222);
  //gRenderAux.CircleOnTerrain(CenterPlatoon.X, CenterPlatoon.Y, 5, $09FFFFFF, $99FFFFFF);
  //gRenderAux.Quad(Loc.X, Loc.Y, Color);
end;


// JUNK
{

function SelectClosestTarget(aInitPoint: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; var aTargetGroup: TKMUnitGroup): Boolean;
function TKMArmyAttack.SelectClosestTarget(aInitPoint: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; var aTargetGroup: TKMUnitGroup): Boolean;
begin
  Result := False;
  aTargetHouse := nil;
  aTargetUnit := nil;
  aTargetGroup := nil;
  // Compute common stuf first
  aTargetHouse := gHands.GetClosestHouse(aInitPoint, fOwner, atEnemy, TARGET_HOUSES, False);
  if (aTargetHouse = nil) then
  begin
    aTargetGroup := gHands.GetClosestGroup(aInitPoint, fOwner, atEnemy);
    if (aTargetGroup = nil) then
    begin
      aTargetUnit := gHands.GetClosestUnit(aInitPoint, fOwner, atEnemy);
      if (aTargetUnit = nil) then
         Exit;
    end;
  end;

  Result := True;
end;

// Try find new target house in ownership area
function SelectNewTargetHouse(aPosition: TKMPoint): TKMHouse;
var
  PLIdx: Word;
  Enemies: TKMHandIndexArray;
  HT: TKMHouseType;
  H: TKMHouse;
begin
  Result := nil;
  Enemies := gAIFields.Influences.GetAllAllianceOwnership(fOwner, aPosition.X, aPosition.Y, atEnemy);
  for PLIdx := 0 to Length(Enemies) - 1 do
    for HT in TARGET_HOUSES do
    begin
      H := gHands[ Enemies[PLIdx] ].Houses.FindHouse(HT, aPosition.X, aPosition.Y);
      if (H <> nil) then
      begin
        Result := H;
        Exit;
      end;
    end;
end;
//}


end.

