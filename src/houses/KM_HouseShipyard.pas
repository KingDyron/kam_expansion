unit KM_HouseShipyard;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_Defaults,
  KM_CommonClasses,
  KM_Points,
  KM_Houses,
  KM_ResTypes;

type
  TKMShipyardDock = record
    CurrentShip, NextShip : TKMUnitType;
    ShipPhase : Byte;
    Materials : TKMWarePlan;
    Started, DoRender : Boolean;
  end;

  TKMShipyardDockWithLoc = record
    CurrentShip, NextShip : TKMUnitType;
    ShipPhase : Byte;
    Materials : TKMWarePlan;
    Position : TKMPointDir;
  end;

  TKMShipyardDocks = array of TKMShipyardDock;

  TKMHouseShipyard = class(TKMHouse)
    private
      fWaresOut : TKMWarePlan;
      fOutCells: TKMPointDirList;
      fDocks: TKMShipyardDocks;
      function GetPhasesCount(aUnitType : TKMUnitType) : Byte;
      procedure ResetDocks;
    protected
       procedure SetHouseTerrain(aOwner : TKMHandID; aStage : TKMHouseStage; const aFlattenTerrain: Boolean = False); override;
    public
      constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
      destructor Destroy; override;
      constructor Load(LoadStream: TKMemoryStream); override;
      procedure Save(SaveStream: TKMemoryStream); override;
      procedure UpdatePosition(const aPos: TKMPoint); override;//Used only by map editor

      procedure WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1); override;
      function CheckWareOut(aWare: TKMWareType): Word; override;
      procedure WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False); override;
      function WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean; override;

      procedure SetNextShipType(aDock : Integer; aCount : Integer);
      property WaresOut : TKMWarePlan read fWaresOut;

      procedure IncSketchPhase(aDock : Integer);

      function CanBuildShipAt(aDock : Integer) : Boolean;
      function GetDockToWorkOn : Integer;
      function GetWarePlan(aDock : Integer) : TKMWarePlan;
      function GetShipStages(aDock : Integer) : Integer;

      function GetDock(aIndex : Integer) : TKMShipyardDockWithLoc;
      function DocksCount: Integer;
      procedure DigDock(aIndex : Integer);
      procedure StartBuildingShip(aIndex : Integer);
      procedure CancelBuildingShip(aIndex : Integer);


      function HitTest(X, Y: Integer): Boolean; override;

      procedure Paint; Override;
  end;

implementation
uses
  SysUtils,
  KM_CommonUtils,
  KM_Entity,
  KM_GameParams,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_Units, KM_UnitWarrior,
  KM_Resource, KM_ResUnits,
  KM_RenderPool,
  KM_ScriptingEvents,
  KM_Terrain;


constructor TKMHouseShipyard.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  fOutCells := TKMPointDirList.Create;
  gTerrain.GetShipyardDirCells(PosX + gRes.Houses[aHouseType].EntranceOffsetX, PosY + gRes.Houses[aHouseType].EntranceOffsetY, fOutCells);
  Inherited;
  ResetDocks;
end;

destructor TKMHouseShipyard.Destroy;
begin
  FreeAndNil(fOutCells);
  Inherited;
end;

Constructor TKMHouseShipyard.Load(LoadStream: TKMemoryStream);
var I : integer;
begin
  Inherited;

  fWaresOut.Load(LoadStream);
  fOutCells := TKMPointDirList.Create;
  fOutCells.LoadFromStream(LoadStream);
  ResetDocks;

  for I := 0 to fOutCells.Count - 1 do
    LoadStream.Read(fDocks[I], SizeOf(fDocks[I]) );

end;

function TKMHouseShipyard.GetPhasesCount(aUnitType : TKMUnitType) : Byte;
begin
  case aUnitType of
    utBoat        : Result := gRes.Units.FishermansShipSketch[dirN].Count;
    utShip        : Result := gRes.Units.ShipSketch[dirN].Count;
    utBattleShip  : Result := gRes.Units.BattleShipSketch[dirN].Count;
    else
      Result := 0;
  end;
end;

procedure TKMHouseShipyard.SetHouseTerrain(aOwner: TKMHandID; aStage: TKMHouseStage; const aFlattenTerrain: Boolean = False);
var I : Integer;
begin
  Inherited;
  for I := 0 to fOutCells.Count - 1 do
    gTerrain.SetHouseOnSinglePoint(self, fOutCells[I].Loc, HouseType, aStage, aOwner, aFlattenTerrain);
end;

procedure TKMHouseShipyard.UpdatePosition(const aPos: TKMPoint);
var
  wasOnSnow : TKMTerrPicType;
begin
  Assert(gGameParams.IsMapEditor);

  //We have to remove the house THEN check to see if we can place it again so we can put it on the old position
  SetHouseTerrain(HAND_NONE, hsNone);

  if gHands[Owner].CanAddHousePlan(aPos, HouseType) then
  begin
    gTerrain.RemRoad(Entrance);

    SetPosition(KMPoint(aPos.X - gRes.Houses[HouseType].EntranceOffsetX, aPos.Y - gRes.Houses[HouseType].EntranceOffsetY));
    gTerrain.SetRoad(Entrance, Owner, rtStone);
  end;
  fOutCells.Clear;
  gTerrain.GetShipyardDirCells(Position.X + gRes.Houses[HouseType].EntranceOffsetX, Position.Y + gRes.Houses[HouseType].EntranceOffsetY, fOutCells);
  ResetDocks;
  case fBuildState of
    hbsNoGlyph,
    hbsWood: SetHouseTerrain(Owner, hsFence); // Update terrain tiles for house;
    hbsStone,
    hbsDone: SetHouseTerrain(Owner, hsBuilt); // Update terrain tiles for house;
  end;



  //Do not remove all snow if house is moved from snow to snow
  wasOnSnow := fIsOnTerrain;
  CheckOnTerrain;
  if not (wasOnSnow <> tptNone) or not (fIsOnTerrain <> tptNone) then
    fSnowStep := 0;
end;

procedure TKMHouseShipyard.ResetDocks;
var I : integer;
begin
  SetLength(fDocks, fOutCells.Count);
  for I := 0 to fOutCells.Count - 1 do
  begin
    fDocks[I].CurrentShip := utNone;
    fDocks[I].NextShip := utNone;
    fDocks[I].ShipPhase := 0;
    fDocks[I].Materials.Reset;
    fDocks[I].DoRender := fBuildState = hbsDone;
  end;
end;

function TKMHouseShipyard.GetDock(aIndex : Integer) : TKMShipyardDockWithLoc;
begin
  Result.CurrentShip := fDocks[aIndex].CurrentShip;
  Result.NextShip := fDocks[aIndex].NextShip;
  Result.ShipPhase := fDocks[aIndex].ShipPhase;
  Result.Materials.CopyFrom(fDocks[aIndex].Materials);
  Result.Position := fOutCells[aIndex];
end;

function TKMHouseShipyard.DocksCount: Integer;
begin
  Result := fOutCells.Count;
end;

procedure TKMHouseShipyard.DigDock(aIndex : Integer);
begin
  fDocks[aIndex].DoRender := true;
end;

procedure TKMHouseShipyard.StartBuildingShip(aIndex : Integer);
begin
  fDocks[aIndex].Started := true;
end;

procedure TKMHouseShipyard.CancelBuildingShip(aIndex: Integer);
begin
  fDocks[aIndex].Started := false;
end;

function TKMHouseShipyard.HitTest(X: Integer; Y: Integer): Boolean;
  function InOutCells : Boolean;
  var I : Integer;
  begin
    Result := false;
    for I := 0 to fOutCells.Count - 1 do
      If KMSamePoint(fOutCells[I].Loc, KMPoint(X,Y) ) then
        Exit(true);
  end;

begin
  Result := Inherited or InOutCells;
end;

procedure TKMHouseShipyard.Save(SaveStream: TKMemoryStream);
var I : integer;
begin
  Inherited;

  fWaresOut.Save(SaveStream);
  fOutCells.SaveToStream(SaveStream);
  for I := 0 to fOutCells.Count - 1 do
    SaveStream.Write(fDocks[I], SizeOf(fDocks[I]) );
end;

procedure TKMHouseShipyard.IncSketchPhase(aDock : Integer);
var
  U: TKMUnit;
begin

  Inc(fDocks[aDock].ShipPhase);
  If fDocks[aDock].ShipPhase <= GetPhasesCount(fDocks[aDock].CurrentShip) then
    Exit;
  //Make new unit
  U := gHands[Owner].TrainUnit(fDocks[aDock].CurrentShip, Self);
  U.Visible := False; //Make him invisible as he is inside the barracks
  U.Condition := UNIT_MAX_CONDITION; //All soldiers start with 3/4, so groups get hungry at the same time
  U.PositionNext := fOutCells[aDock].Loc;
  U.SetActionGoOutDock(uaWalk, Self, fOutCells[aDock], true);

  fDocks[aDock].ShipPhase := 0;
  fDocks[aDock].CurrentShip := fDocks[aDock].NextShip;
  fDocks[aDock].Started := false;

  ProduceFestivalPoints(fptWarfare, 3);

  if Assigned(U.OnUnitTrained) then
    U.OnUnitTrained(U);
end;


procedure TKMHouseShipyard.SetNextShipType(aDock: Integer; aCount : Integer);
var I, Index: Integer;
  procedure SelectNext;
  begin
    IncLoop(Index, low(SHIPYARD_ORDER), high(SHIPYARD_ORDER), aCount);
    fDocks[aDock].NextShip := SHIPYARD_ORDER[Index];
  end;

begin
  Index := -1;
  for I := 0 to High(SHIPYARD_ORDER) do
    If SHIPYARD_ORDER[I] = fDocks[aDock].NextShip then
    begin
      Index := I;
      Break;
    end;
  if Index = -1 then
    Exit;
  SelectNext;
  while not gHands[Owner].Locks.UnitUnlocked(fDocks[aDock].NextShip, HouseType) do
    SelectNext;
  If (fDocks[aDock].CurrentShip <> fDocks[aDock].NextShip) and not fDocks[aDock].Started then
    fDocks[aDock].CurrentShip := fDocks[aDock].NextShip;
end;

function TKMHouseShipyard.CanBuildShipAt(aDock: Integer): Boolean;
begin
  Result := (fDocks[aDock].CurrentShip <> utNone) and not fDocks[aDock].Started and HasWaresIn( GetWarePlan(aDock) );
end;

function TKMHouseShipyard.GetDockToWorkOn: Integer;
var I, dock : Integer;
begin
  Result := -1;
  for I := 1 to fOutCells.Count do
  begin
    dock := ((LastOrderProduced + I) mod fOutCells.Count);
    If not CanBuildShipAt(dock) then
      Continue;
    LastOrderProduced := dock;
    Result := LastOrderProduced;
    Break;
  end;

end;

function TKMHouseShipyard.GetWarePlan(aDock : Integer) : TKMWarePlan;
begin
  Result.Reset;
  Assert(aDock >= 0, 'TKMHouseShipyard.GetWarePlan');
  If fDocks[aDock].Started then
    Exit;
  case fDocks[aDock].CurrentShip of
    utBoat :  begin
                Result.AddWare(wtLog, 3);
                Result.AddWare(wtSkin, 1);

                if CheckWareIn(wtSteelE) > 0 then
                  Result.AddWare(wtSteelE);
              end;
    utShip :  begin
                Result.AddWare(wtLog, 5);
                Result.AddWare(wtSkin, 2);

                if CheckWareIn(wtSteelE) >= 2 then
                  Result.AddWare(wtSteelE, 2);
                if CheckWareIn(wtBitinE) >= 2 then
                  Result.AddWare(wtBitinE, 2);
              end;
    utBattleShip :  begin
                      Result.AddWare(wtLog, 7);
                      Result.AddWare(wtSkin, 4);

                      if CheckWareIn(wtSteelE) >= 3 then
                        Result.AddWare(wtSteelE, 3);
                      if CheckWareIn(wtBitinE) >= 3 then
                        Result.AddWare(wtBitinE, 3);
                    end;
  end;

end;

function TKMHouseShipyard.GetShipStages(aDock: Integer): Integer;
begin
  Result := GetPhasesCount(fDocks[aDock].CurrentShip);
end;



procedure TKMHouseShipyard.WareAddToOut(aWare: TKMWareType; const aCount: Integer = 1);
begin
  fWaresOut.AddWare(aWare, aCount);
  gHands[Owner].Deliveries.Queue.AddOffer(Self,aWare,aCount);
end;

function TKMHouseShipyard.CheckWareOut(aWare: TKMWareType): Word;
begin
  Result := fWaresOut.HasWare(aWare);
end;

procedure TKMHouseShipyard.WareTakeFromOut(aWare: TKMWareType; aCount: Word = 1; aFromScript: Boolean = False);
var index : Integer;
begin
  index := fWaresOut.IndexOf(aWare, 1);
  Assert(index >= 0);
  {if index = -1 then
    Exit;}

  if aFromScript then
  begin
    aCount := Min(aCount, fWaresOut[index].C);
    if aCount > 0 then
    begin
      gHands[Owner].Stats.WareConsumed(aWare, aCount);
      gHands[Owner].Deliveries.Queue.RemOffer(Self, aWare, aCount);
    end;
  end;

  Assert(aCount <= fWaresOut[index].C);

  fWaresOut[index].C := fWaresOut[index].C - aCount;

end;

function TKMHouseShipyard.WareOutputAvailable(aWare: TKMWareType; const aCount: Word): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to fWaresOut.Count - 1 do
    if (fWaresOut[I].W = aWare) and (fWaresOut[I].C > 0) then
      Result := fWaresOut[I].C >= aCount;

end;

procedure TKMHouseShipyard.Paint;
  function GetRot(aIndex : Integer) : Byte;
  begin
    Result := (byte(fOutCells[aIndex].Dir) div 2 + 2) mod 4;
  end;

  function GetTex(aIndex : Integer) : Word;
  begin
    Result := byte(fOutCells[aIndex].Dir in [dirN, dirE]) + 969;
  end;

  function GetObjTex(aIndex : Integer) : Word;
  begin
    Result := byte(fOutCells[aIndex].Dir in [dirW, dirE]) + 653;
  end;

var I, id : integer;
begin
  Inherited;

  for I := 0 to fOutCells.Count - 1 do
    If fDocks[I].DoRender then
    begin
      gRenderPool.RenderTile(GetTex(I), fOutCells[I].Loc.X, fOutCells[I].Loc.Y, GetRot(I));
      case fOutCells[I].Dir of
        dirN : gRenderPool.RenderTile(971, fOutCells[I].Loc.X, fOutCells[I].Loc.Y + 1, 2);
        dirS : gRenderPool.RenderTile(971, fOutCells[I].Loc.X, fOutCells[I].Loc.Y - 1, 0);
        dirW : gRenderPool.RenderTile(971, fOutCells[I].Loc.X + 1, fOutCells[I].Loc.Y, 1);
        dirE : gRenderPool.RenderTile(971, fOutCells[I].Loc.X - 1, fOutCells[I].Loc.Y, 3);

      end;
      gRenderPool.RenderMapElement(GetObjTex(I), 0, fOutCells[I].Loc.X, fOutCells[I].Loc.Y);
      if fDocks[I].ShipPhase > 0 then
      begin
        case fDocks[I].CurrentShip of
          utShip : id := gRes.Units.ShipSketch[fOutCells[I].DIr].Step[fDocks[I].ShipPhase];
          utBattleShip : id := gRes.Units.BattleShipSketch[fOutCells[I].DIr].Step[fDocks[I].ShipPhase];
          utBoat : id := gRes.Units.FishermansShipSketch[fOutCells[I].DIr].Step[fDocks[I].ShipPhase];
          else
            id := 0;
        end;
        if id > 0 then
          gRenderPool.AddSpriteWH(fOutCells[I].Loc, KMPOINT_ZERO, id + 1, rxUnits, GetFlagColor);
      end;


      //try to render obj next to empty spot
      IF (fOutCells[I].Dir in [dirW, dirE])
        and (gTerrain.House(fOutCells[I].Loc.X, fOutCells[I].Loc.Y + 1) = nil) then
        gRenderPool.RenderMapElement(GetObjTex(I), 0, fOutCells[I].Loc.X, fOutCells[I].Loc.Y + 1)
      else
      IF (fOutCells[I].Dir in [dirN, dirS])
        and (gTerrain.House(fOutCells[I].Loc.X + 1, fOutCells[I].Loc.Y) = nil) then
        gRenderPool.RenderMapElement(GetObjTex(I), 0, fOutCells[I].Loc.X + 1, fOutCells[I].Loc.Y)
      else

    end;



end;


end.
