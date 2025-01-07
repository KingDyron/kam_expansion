{
Artificial intelligence
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_ArmyDefence;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_CommonTypes, KM_Defaults,
  KM_Points, KM_UnitGroup, KM_NavMeshDefences, KM_AIDefensePos;


type
  TKMFormation = record NumUnits, UnitsPerRow: Integer; end;

  TKMDefencePosition = class
  private
    fWeight, fLine: Word; // Higher number = higher enemy influence or closer to the first line
    fGroup: TKMUnitGroup; //Commander of group currently occupying position
    fPosition: TKMPointDir; //Position and direction the group defending will stand

    procedure SetGroup(aGroup: TKMUnitGroup);
    procedure SetPosition(const Value: TKMPointDir);
    function GetGroupType(): TKMGroupType;
  public
    constructor Create(aWeight, aLine: Word; aPos: TKMPointDir);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property Line: Word read fLine write fLine;
    property Weight: Word read fWeight write fWeight;
    property Position: TKMPointDir read fPosition write SetPosition; //Position and direction the group defending will stand
    property Group: TKMUnitGroup read fGroup write SetGroup;
    property GroupType: TKMGroupType read GetGroupType;

    function CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
    procedure UpdateState(aTick: Cardinal);
  end;


  TKMArmyDefence = class
  private
    fOwner: TKMHandID;
    fCityUnderAttack: Boolean;
    fFirstLineCnt: Word;
    fPositions: TKMList;

    function GetCount: Integer; inline;
    function GetPosition(aIndex: Integer): TKMDefencePosition; inline;
    function GetGroupsCount(): Word;
  public
    TroopFormations: array [GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMFormation; //Defines how defending troops will be formatted. 0 means leave unchanged.

    constructor Create(aOwner: TKMHandID);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Count: Integer read GetCount;
    property Positions[aIndex: Integer]: TKMDefencePosition read GetPosition; default;
    property FirstLineCnt: Word read fFirstLineCnt;
    property GroupsCount: Word read GetGroupsCount;
    property CityUnderAttack: Boolean read fCityUnderAttack;

    procedure OwnerUpdate(aOwner: TKMHandID);
    procedure UpdateDefences(aDefCnt: Word; aPNewDef: array of PDefencePosition);
    procedure UpdateFixedDefences();
    function DefenceStatus(): Single;
    function FindPositionOf(aGroup: TKMUnitGroup; aIgnoreFirstLine: Boolean = False): TKMDefencePosition;
    function FindPlaceForGroup(aGroup: TKMUnitGroup): Boolean;
    procedure ReleaseGroup(aGroup: TKMUnitGroup); overload;
    procedure ReleaseGroup(aDefPosIdx: Integer); overload;
    function ReleaseAllGroups(): TKMUnitGroupArray;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;

const
  MAX_SOLDIERS_IN_GROUP = 9; //These are the defaults in KaM
  FORMATION_OF_GROUP = 3;
  SQR_FIRST_LINE_RADIUS = 8*8;
  FIRST_LINE_MAX_IDX = 0;


implementation
uses
  Math,
  KM_Entity,
  KM_GameParams, KM_HandsCollection, KM_Hand, KM_RenderAux,
  KM_AIFields, KM_NavMesh,
  KM_UnitGroupTypes;


{ TKMDefencePosition }
constructor TKMDefencePosition.Create(aWeight, aLine: Word; aPos: TKMPointDir);
begin
  inherited Create;
  fWeight := aWeight;
  fLine := aLine;
  fPosition := aPos;
  Group := nil; //Unoccupied
end;


destructor TKMDefencePosition.Destroy;
begin
  Group := nil; //Ensure pointer is removed (property calls CleanUpGroupPointer)
  inherited;
end;


procedure TKMDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('AdvDefencePosition');
  SaveStream.Write(fWeight);
  SaveStream.Write(fLine);
  SaveStream.Write(fPosition);
  SaveStream.Write(fGroup.UID); //Store ID
end;


constructor TKMDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.CheckMarker('AdvDefencePosition');
  LoadStream.Read(fWeight);
  LoadStream.Read(fLine);
  LoadStream.Read(fPosition);
  LoadStream.Read(fGroup, 4); //Subst on syncload
end;


procedure TKMDefencePosition.SyncLoad();
begin
  fGroup := gHands.GetGroupByUID(Integer(fGroup));
end;


procedure TKMDefencePosition.SetGroup(aGroup: TKMUnitGroup);
begin
  gHands.CleanUpGroupPointer(fGroup);
  if (aGroup <> nil) then
    fGroup := aGroup.GetPointer;
end;


procedure TKMDefencePosition.SetPosition(const Value: TKMPointDir);
begin
  Assert(gGameParams.IsMapEditor);
  fPosition := Value;
end;


function TKMDefencePosition.GetGroupType(): TKMGroupType;
begin
  Result := gtMelee;
  if (Group <> nil) then
    Result := Group.GroupType;
end;


function TKMDefencePosition.CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
begin
  Result := (Group = nil)
            or ((GroupType = UNIT_TO_GROUP_TYPE[aGroup.UnitType]) and (Group.Count < aMaxUnits));
end;


procedure TKMDefencePosition.UpdateState(aTick: Cardinal);
begin
  // If the group is Dead or too far away we should disassociate
  // them from the defence position so new warriors can take up the defence if needs be
  if (Group = nil)
  or Group.IsDead
  or Group.InFight
  or (Group.Order in [goAttackHouse, goAttackUnit]) then
    gHands.CleanUpGroupPointer(fGroup);

  // Tell group to walk to its position
  // It's easier to repeat the order than check that all members are in place
  if (Group <> nil)
  and Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder])
  and Group.CanWalkTo(Position.Loc, 0) then
    Group.OrderWalk(Position.Loc, True, wtokAIGotoDefencePos, Position.Dir);
end;


{ TKMArmyDefence }
constructor TKMArmyDefence.Create(aOwner: TKMHandID);
var
  GT: TKMGroupType;
begin
  inherited Create;

  fOwner := aOwner;
  fCityUnderAttack := False;
  fPositions := TKMList.Create;

  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
  begin
    TroopFormations[GT].NumUnits := MAX_SOLDIERS_IN_GROUP;
    TroopFormations[GT].UnitsPerRow := FORMATION_OF_GROUP;
  end;
end;


destructor TKMArmyDefence.Destroy;
begin
  fPositions.Free;

  inherited;
end;


procedure TKMArmyDefence.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('AdvDefencePositions');
  SaveStream.Write(fOwner);
  SaveStream.Write(fCityUnderAttack);
  SaveStream.Write(fFirstLineCnt);
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));

  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
    Positions[I].Save(SaveStream);
end;


procedure TKMArmyDefence.Load(LoadStream: TKMemoryStream);
var
  I, newCount: Integer;
begin
  LoadStream.CheckMarker('AdvDefencePositions');
  LoadStream.Read(fOwner);
  LoadStream.Read(fCityUnderAttack);
  LoadStream.Read(fFirstLineCnt);
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));

  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
    fPositions.Add( TKMDefencePosition.Load(LoadStream) );
end;


procedure TKMArmyDefence.SyncLoad;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Positions[I].SyncLoad;
end;


procedure TKMArmyDefence.OwnerUpdate(aOwner: TKMHandID);
begin
  fOwner := aOwner;
end;


function TKMArmyDefence.GetCount: Integer;
begin
  Result := fPositions.Count;
end;


function TKMArmyDefence.GetPosition(aIndex: Integer): TKMDefencePosition;
begin
  Result := fPositions[aIndex];
end;


function TKMArmyDefence.GetGroupsCount(): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Ord(Positions[I].Group <> nil));
end;


function TKMArmyDefence.FindPlaceForGroup(aGroup: TKMUnitGroup): Boolean;
var
  K, BestIdx, Needed: Integer;
  BestWeight: Word;
  UG: TKMUnitGroup;
begin
  Result := False;

  //Try to link to existing group
  BestWeight := 0;
  BestIdx := -1;
  for K := 0 to Count - 1 do
    if Positions[K].CanAccept(aGroup, TroopFormations[aGroup.GroupType].NumUnits)
      AND (BestWeight < Positions[K].Weight) then
    begin
      BestIdx := K;
      BestWeight := Positions[K].Weight;
    end;

  if (BestIdx <> -1) then
  begin
    Result := True;
    //New position
    if (Positions[BestIdx].Group = nil) then
    begin
      Needed := TroopFormations[aGroup.GroupType].NumUnits;
      // Add group to new defense position
      Positions[BestIdx].Group := aGroup;
      // Defence position requires less soldiers -> distribute other soldiers into different defence position
      if (aGroup.Count > Needed) then
      begin
        UG := aGroup.OrderSplitUnit(aGroup.GetAliveMember, True);
        if (UG <> nil) then
        begin
          if (aGroup.Count - Needed > 0) then
            aGroup.OrderSplitLinkTo(UG, aGroup.Count - Needed, True); //Link only as many units as are needed
          //FindPlaceForGroup(UG); // The rest of group will be added later (not in this tick)
        end;
      end;
      UG := Positions[BestIdx].Group;
      UG.OrderWalk(Positions[BestIdx].Position.Loc, True, wtokAIGotoDefencePos, Positions[BestIdx].Position.Dir);
    end
    // Restock
    else
    begin
      //Append to existing position
      UG := Positions[BestIdx].Group;
      Needed := TroopFormations[ UG.GroupType ].NumUnits - UG.Count;
      if (aGroup.Count <= Needed) then
        aGroup.OrderLinkTo(UG, True) //Link entire group
      else
      begin
        aGroup.OrderSplitLinkTo(UG, Needed, True); //Link only as many units as are needed
        //FindPlaceForGroup(aGroup); // The rest of group will be added later (not in this tick)
      end;
    end;
    if (UG.UnitsPerRow <> TroopFormations[UG.GroupType].UnitsPerRow) then
      UG.UnitsPerRow := TroopFormations[UG.GroupType].UnitsPerRow;
  end;
end;


// Find DefencePosition of a group
function TKMArmyDefence.FindPositionOf(aGroup: TKMUnitGroup; aIgnoreFirstLine: Boolean = False): TKMDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (Positions[I].Group = aGroup) then
    begin
      if (aIgnoreFirstLine and (Positions[I].Line <= FIRST_LINE_MAX_IDX)) then
        Break;
      Result := Positions[I];
      Break;
    end;
end;


procedure TKMArmyDefence.ReleaseGroup(aGroup: TKMUnitGroup);
var
  DP: TKMDefencePosition;
begin
  DP := FindPositionOf(aGroup);
  if (DP <> nil) then
    DP.Group := nil;
end;


procedure TKMArmyDefence.ReleaseGroup(aDefPosIdx: Integer);
begin
  if (aDefPosIdx < Count) then
    Positions[aDefPosIdx].Group := nil;
end;


function TKMArmyDefence.ReleaseAllGroups(): TKMUnitGroupArray;
var
  K, Cnt: Integer;
begin
  Cnt := GroupsCount;
  SetLength(Result, Cnt);
  for K := 0 to Count - 1 do
    if (Positions[K].Group <> nil) then
    begin
      Dec(Cnt);
      Result[Cnt] := Positions[K].Group;
      ReleaseGroup(K);
    end;
end;


// Defence status = fulfill of first defence line
function TKMArmyDefence.DefenceStatus(): Single;
var
  I, Cnt: Integer;
begin
  Result := 0; // No defences
  if (Count > 0) then
  begin
    Cnt := 0;
    for I := 0 to Count - 1 do
      if (Positions[I].Group <> nil) then
        Inc(Cnt);
    Result := Cnt / Max(1,fFirstLineCnt);
  end;
end;


procedure TKMArmyDefence.UpdateDefences(aDefCnt: Word; aPNewDef: array of PDefencePosition);
var
  I, K: Integer;
  G: TKMUnitGroup;
  VisitedNewPos, VisitedExistPos: TBooleanArray;
begin
  //Get defence Polygons
  if (aDefCnt = 0) then
    Exit;

  SetLength(VisitedNewPos, aDefCnt);
  FillChar(VisitedNewPos[0], SizeOf(VisitedNewPos[0])*Length(VisitedNewPos), #0);
  SetLength(VisitedExistPos, fPositions.Count);
  if (Length(VisitedExistPos) > 0) then
    FillChar(VisitedExistPos[0], SizeOf(VisitedExistPos[0])*Length(VisitedExistPos), #0);

  // Compare new defences with old version and add new defences / remove old
  for I := 0 to aDefCnt - 1 do
    // Try find existing defence position
    for K := 0 to fPositions.Count - 1 do
      //if (Positions[K].Polygon = aPNewDef[I]^.Polygon) then // This cannot be used because 1 polygon can have 3 points
      if KMSamePoint(Positions[K].Position.Loc, aPNewDef[I]^.DirPoint.Loc) then
      begin
        VisitedNewPos[I] := True;
        VisitedExistPos[K] := True;
        Positions[K].Weight := aPNewDef[I]^.Weight;
        Positions[K].Line := aPNewDef[I]^.Line;
        break;
      end;

  // Remove old and unused defence positions
  for I := fPositions.Count - 1 downto 0 do
    if not VisitedExistPos[I] then
      fPositions.Delete(I);

  // Add new defence positions
  for I := 0 to aDefCnt - 1 do
    if not VisitedNewPos[I] then
      fPositions.Add(  TKMDefencePosition.Create( aPNewDef[I]^.Weight, aPNewDef[I]^.Line, aPNewDef[I]^.DirPoint )  );
  // Recalculate group positions in case that group is not complete and get count of first line
  fFirstLineCnt := 0;
  for I := fPositions.Count - 1 downto 0 do
  begin
    if (Positions[I].Line <= FIRST_LINE_MAX_IDX) then
      Inc(fFirstLineCnt);
    G := Positions[I].Group;
    if (G <> nil) AND (G.Count < TroopFormations[ G.GroupType ].NumUnits) then
    begin
      ReleaseGroup(I);
      if not G.IsDead then
        FindPlaceForGroup(G);
    end;
  end;
end;


procedure TKMArmyDefence.UpdateFixedDefences();
var
  K: Integer;
  DefPos: TAIDefencePosition;
  DP: TKMDefencePosArr;
  aPNewDef: array of PDefencePosition;
begin
  with gHands[fOwner].AI.General.DefencePositions do
  begin
    SetLength(DP,Count);
    SetLength(aPNewDef,Count);
    for K := 0 to Count - 1 do
    begin
      DefPos := Positions[K];
      DP[K].Line := 1;
      DP[K].Polygon := 0;
      DP[K].Weight := 1;
      DP[K].DirPoint := DefPos.Position;
      aPNewDef[K] := @DP[K];
    end;
    UpdateDefences(Count, aPNewDef);
  end;
end;


procedure TKMArmyDefence.AfterMissionInit();
begin

end;


procedure TKMArmyDefence.UpdateState(aTick: Cardinal);
const
  FAST_UPDATE = MAX_HANDS * 5;
  SLOW_UPDATE = MAX_HANDS * 20;
var
  I: Integer;
begin
  if (aTick mod FAST_UPDATE = fOwner) then
  begin
    for I := 0 to Count - 1 do
      Positions[I].UpdateState(aTick);
    if (aTick mod SLOW_UPDATE = fOwner) AND not gHands[fOwner].AI.Setup.AutoDefend then
      UpdateFixedDefences();
  end;
end;


procedure TKMArmyDefence.LogStatus(var aBalanceText: UnicodeString);
begin
  //aBalanceText := '';
end;


procedure TKMArmyDefence.Paint();
var
  I, K, Idx, Threat, AllianceIdx: Integer;
  Col: Cardinal;
  Loc, Pos: TKMPoint;
  GT: TKMGroupType;
  UGA: TKMUnitGroupArray;
begin
  if not OVERLAY_DEFENCES then
    Exit;

  if (fOwner <> gMySpectator.HandID) then // Show just 1 player (it prevents notification to be mess)
    Exit;

  // Draw defensive positions as a circles
  for I := 0 to Count - 1 do
  begin
    Loc := Positions[I].Position.Loc;
    Col := $22;
    if (Positions[I].Group = nil) then
      Col := 0;
    gRenderAux.CircleOnTerrain(Loc.X, Loc.Y, 1, (Col shl 24) OR tcGreen, $FFFFFFFF);
  end;

  // Draw defence position of selected unit
  if (gMySpectator.Selected is TKMUnitGroup) then
    for I := 0 to Count - 1 do
      if (gMySpectator.Selected = Positions[I].Group) then
      begin
        Loc := Positions[I].Position.Loc;
        gRenderAux.CircleOnTerrain(Loc.X, Loc.Y, 1, $AA000000 OR tcRed, $FFFFFFFF);
        break;
      end;

  if not gAIFields.Influences.GetAllianceIdx(fOwner,AllianceIdx) then
    Exit;
  // First line of defences
  for I := 0 to fPositions.Count - 1 do
    if (Positions[I].Line = 0) then
    begin
      Loc := Positions[I].Position.Loc;
      Idx := gAIFields.NavMesh.KMPoint2Polygon[Loc];
      Threat := 0;
      for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
        Threat := Threat + gAIFields.Influences.Presence[ AllianceIdx, Idx, GT ];

      // Draw defensive lines as a triangles
      Col := Max( $22, Min($FF, Threat) );
      gAIFields.NavMesh.DrawPolygon(Idx, Col, tcRed);

      // Draw hostile units around defensive lines
      if (Threat > 0) then
      begin
        UGA := gHands.GetGroupsInRadius(Loc, SQR_FIRST_LINE_RADIUS, fOwner, atEnemy);
        for K := 0 to Length(UGA) - 1 do
        begin
          Pos := UGA[K].Position;
          gRenderAux.CircleOnTerrain(Pos.X, Pos.Y, 1, $44000000 OR tcRed, $FF000000 OR tcRed);
          //gRenderAux.LineOnTerrain(Pos, Loc, $AA000000 OR clBlack);
        end;
      end;
    end;
end;


end.

