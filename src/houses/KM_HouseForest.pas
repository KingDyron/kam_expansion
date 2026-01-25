unit KM_HouseForest;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes;

type
  TKMForestTree = record
    Age : Word;
    ID : Byte;
    Obj : Word;
    Pos : TKMPointF;

  end;

  TKMHouseForest = class(TKMHouse)
  private
    fCount : Byte;
    fTrees : array of TKMForestTree;
    fCuttingAnimStep : Word;
    fCuttingAnimDir : TKMDirection;
    fCuttingPos : TKMPointF;

    fAITrees : TKMByteArray;

    function TreeWillCollide(aTreeID : Byte; aLoc : TKMPointF) : Boolean;
    function GetRandomPosForTree(aTreeID : Byte) : TKMPointF;
    procedure AddMapEdTreeSingle(aTreeID : Byte);
    procedure PlantAITree;
  protected
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    function AddTree(aTreeID : Byte; aScript : Boolean = false) : Boolean; overload;
    function AddTree(aTreeID : Byte; aCount : Byte) : Boolean; overload;

    procedure AddMapEdTree(aTreeID : Byte; aCount : integer = 1);
    procedure RemoveMapEdTree(aTreeID : Byte; aCount : integer = 1);

    procedure UpdateState(aTick: Cardinal); Override;
    procedure Paint; override;

    function HasTreeToCut : Integer;
    procedure StartCuttingTree(aID : Integer);
    procedure CutTree(aID : Integer);
    function TreeObjID(aID : Integer) : Integer;
    function TreesCount(aTreeID : Integer) : Integer;
    property TotalCount : Byte read fCount;


    Constructor Load(LoadStream : TKMemoryStream);Override;
    procedure Save(SaveStream : TKMemoryStream);Override;
  end;


implementation
uses
  Classes, SysUtils,
  KM_Game,
  KM_HandsCollection,
  KM_CommonUtils,
  KM_RenderPool,
  KM_ResMapElements,
  KM_Terrain,
  KM_Units;

constructor TKMHouseForest.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  fCount := 0;
  fAITrees := [];
end;

Constructor TKMHouseForest.Load(LoadStream : TKMemoryStream);
var I : Integer;
begin
  Inherited;
  LoadStream.Read(fCount);
  LoadStream.Read(fCuttingAnimStep);
  LoadStream.Read(fCuttingAnimDir);
  LoadStream.Read(fCuttingPos);

  SetLength(fTrees, fCount);
  for I := 0 to fCount - 1 do
    LoadStream.ReadData(fTrees[I]);

  LoadStream.Read(fAITrees);
end;

procedure TKMHouseForest.Save(SaveStream : TKMemoryStream);
var I : Integer;
begin
  Inherited;
  SaveStream.Write(fCount);
  SaveStream.Write(fCuttingAnimStep);
  SaveStream.Write(fCuttingAnimDir);
  SaveStream.Write(fCuttingPos);

  for I := 0 to fCount - 1 do
    SaveStream.WriteData(fTrees[I]);
  SaveStream.Write(fAITrees);
end;

function TKMHouseForest.TreeWillCollide(aTreeID : Byte; aLoc : TKMPointF) : Boolean;
var I : Integer;
  size1, size2 : Single;
begin
  Result := false;
  for I := 0 to fCount - 1 do
  begin
    size1 := gGrowingTrees[fTrees[I].ID].Size;
    size2 := gGrowingTrees[aTreeID].Size;
    if gHands[Owner].BuildDevUnlocked(24) then
    begin
      size1 := size1 * 0.90;
      size2 := size2 * 0.90;
    end;


    If KMLength(aLoc, fTrees[I].Pos) <=  size1 + size2  then
      Exit(true);
  end;
end;



function TKMHouseForest.GetRandomPosForTree(aTreeID : Byte): TKMPointF;
var I : Byte;
begin
  I := 0;
  repeat
    Result.X := KaMRandomS1(4.5, 'TKMHousePasture.GetPositionForAnimal 1');
    Result.Y := KaMRandomS1(3, 'TKMHousePasture.GetPositionForAnimal 2');
    Inc(I);
  until (I >= 10) or not TreeWillCollide(aTreeID, Result);//max 10 tries

  //no place for new tree
  If I >= 10 then
    Result := KMPOINTF_INVALID_TILE;
end;

function TKMHouseForest.AddTree(aTreeID : Byte; aScript : Boolean = false) : Boolean;
var newLoc : TKMPointF;
begin
  newLoc := GetRandomPosForTree(aTreeID);
  Result := true;
  If newLoc.X = -1 then//there is no place for new tree
    Exit(false);
  If (not aScript) and not gHands[Owner].VirtualWareTake('vtSapling') then
    Exit;

  inc(fCount);
  If length(fTrees) < fCount then
    SetLength(fTrees, fCount + 5);

  fTrees[fCount - 1].Pos := newLoc;
  fTrees[fCount - 1].Age := KaMRandom(4 * TERRAIN_PACE, 'TKMHouseForest.AddTree');
  fTrees[fCount - 1].Obj := 390{gGrowingTrees[aTreeID].ObjID};  //tree sapling
  fTrees[fCount - 1].ID := aTreeID;
end;

function TKMHouseForest.AddTree(aTreeID: Byte; aCount: Byte): Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to aCount - 1 do
    If AddTree(aTreeID) then
      Result := true;
end;


procedure TKMHouseForest.AddMapEdTreeSingle(aTreeID : Byte);
var I, R, M, obj, id: Integer;
begin
  If not AddTree(aTreeID, true) then
    Exit;
  id := fCount - 1;

  obj := gGrowingTrees[aTreeID].ObjID;
  M := 1;

  while gMapElements[obj].NextTreeAgeObj > 0 do
  begin
    obj := gMapElements[obj].NextTreeAgeObj;
    Inc(M);
  end;

  R := KaMRandom(M + 1, 'TKMHouseForest.AddMapEdTree');

  If R = 0 then
    //do nothing
    //function AddTree handles everything
  else
  begin
    obj := gGrowingTrees[aTreeID].ObjID;
    //R = 1 : use first stage, so it needs to start looking from second stage
    //find fitting age
    for I := 2 to R do
      obj := gMapElements[obj].NextTreeAgeObj;

    fTrees[id].Obj := obj;

    If R = 1 then
      fTrees[id].Age := 8 * TERRAIN_PACE
    else
    begin
      obj := gMapElements[obj].PrevTreeAgeObj;
      fTrees[id].Age := (8 * TERRAIN_PACE) + (gMapElements[Obj].TreeGrowAge * TERRAIN_PACE);
    end;
  end;
end;

procedure TKMHouseForest.AddMapEdTree(aTreeID : Byte; aCount : integer = 1);
var I : Integer;
begin
  for I := 1 to aCount do
    AddMapEdTreeSingle(aTreeID);
  If not ArrayContains(aTreeID, fAITrees) then
    fAITrees := fAITrees + [aTreeID];

end;

procedure TKMHouseForest.RemoveMapEdTree(aTreeID : Byte; aCount : integer = 1);
var I : integer;
begin
  for I := fCount - 1 downto 0 do
    If aTreeID = fTrees[I].ID then
    begin
      dec(aCount);
      fTrees[I] := fTrees[fCount - 1];
      dec(fCount);
      If aCount = 0 then
        Exit;
    end;
end;

procedure TKMHouseForest.PlantAITree;
var R : Byte;
begin
  If length(fAITrees) = 0 then
  begin
    R := KaMRandom(length(gGrowingTrees), 'TKMHouseForest.PlatAITree1');
    AddTree(R);
  end else
  begin
    R := KaMRandom(length(fAITrees), 'TKMHouseForest.PlatAITree2');
    AddTree(fAITrees[R]);
  end;
end;


function TKMHouseForest.HasTreeToCut: Integer;
var I : integer;
begin
  Result := -1;
  for I := 0 to fCount - 1 do
    If ObjectIsChoppableTree(fTrees[I].Obj, caAgeFull) then
      Exit(I);
end;

procedure TKMHouseForest.StartCuttingTree(aID: Integer);
var loc : TKMPointF;
begin
  fCuttingAnimStep := 1;
  case KaMRandom(4, 'TKMHouseForest.StartCuttingTree') of
    0 : fCuttingAnimDir := dirNW;
    1 : fCuttingAnimDir := dirNE;
    2 : fCuttingAnimDir := dirSW;
    3 : fCuttingAnimDir := dirSE
    else
      fCuttingAnimDir := dirNA;
  end;
  loc := fTrees[aID].Pos;
  case fCuttingAnimDir of
    dirNE: loc := loc + KMPointF(-0.5, 0.5);
    dirSE: loc := loc + KMPointF(-0.5, -0.5);
    dirSW: loc := loc + KMPointF(0.5, -0.5);
    dirNW: loc := loc + KMPointF(0.5, 0.5);
    else
      raise Exception.Create('TKMHouseForest.StartCuttingTree : wrong direction');
  end;
  fCuttingPos := KMPointF(loc.X + Entrance.X - 2.75, loc.Y + Entrance.Y - 4.75);
end;

procedure TKMHouseForest.CutTree(aID: Integer);
var obj: Integer;
begin
  fCuttingAnimStep := 0;
  fCuttingAnimDir := dirNA;

  gTerrain.AddFallingTree(KMPointF(fTrees[aID].Pos.X + Entrance.X - 1.75, fTrees[aID].Pos.Y + Entrance.Y - 3.75), fTrees[aID].Obj);
  obj := fTrees[aID].Obj;
  fTrees[aID] := fTrees[fCount - 1];
  dec(fCount);
  ProduceWare(wtTrunk, gMapElements[obj].TrunksCount);
  IncProductionCycle(1);

  if ProductionCycle[1] mod 5 = 0 then
    gHands[Owner].VirtualWareTake('vtSapling', -1);
end;

function TKMHouseForest.TreeObjID(aID: Integer): Integer;
begin
  Result := fTrees[aID].Obj;
end;

function TKMHouseForest.TreesCount(aTreeID : Integer): Integer;
var I : integer;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
    If fTrees[I].ID = aTreeID then
      Inc(Result);
end;


procedure TKMHouseForest.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  Inherited;
  If not IsComplete then
    Exit;
  If fCuttingAnimStep > 0 then
    Inc(fCuttingAnimStep);


  for I := 0 to fCount - 1 do
  with fTrees[I] do
  begin
    If Age = high(word) then
      Continue;
    Inc(Age);
    If Age = 8 * TERRAIN_PACE then
      Obj := gGrowingTrees[ID].ObjID
    else
    If Age = (gMapElements[Obj].TreeGrowAge * TERRAIN_PACE) + (8 * TERRAIN_PACE) then
    begin
      Obj := gMapElements[Obj].NextTreeAgeObj;
      If ObjectIsChoppableTree(Obj, caAgeFull) then
        Age := high(word);
    end;
  end;

  If gHands[Owner].IsComputer then
    If aTick mod TERRAIN_PACE = 0 then
      PlantAITree;
end;

procedure TKMHouseForest.Paint;
var I : Integer;
  entr : TKMPoint;
begin
  Inherited;
  If not IsComplete then
    Exit;
  entr := Entrance;


  I := ResOut[1];
  If I > 0 then
    gRenderPool.AddForestLogs(Position, I);
  for I := 0 to fCount - 1 do
  begin
    gRenderPool.RenderTree(fTrees[I].Obj, FlagAnimStep + I,
                                  fTrees[I].Pos.X + entr.X - 1.75, fTrees[I].Pos.Y + entr.Y - 3.75,
                                  entr);
  end;

  If fCuttingAnimStep > 0 then
    gRenderPool.AddUnit(utWoodCutter, TKMUnit(Worker).UID, uaWork, fCuttingAnimDir, fCuttingAnimStep - 1, 0, fCuttingPos.X, fCuttingPos.Y, GetFlagColor, True);
end;

end.
