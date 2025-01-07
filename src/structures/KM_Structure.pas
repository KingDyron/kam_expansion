unit KM_Structure;
{$I KaM_Remake.inc}
interface
uses KM_Defaults,
    KM_CommonTypes, KM_CommonClasses, KM_Points,

    KM_HandEntity, KM_HandTypes,
    KM_ResStructures, KM_ResTypes;

type
  TKMStructurePointProgress = record
    ID : Byte;
    Fraction : Single;
  end;

  TKMStructure = class(TKMHandEntity)
  private
    fRot : Byte;
    fIndex : Byte;
    fPosition : TKMPoint;
    fCost, fDelivered : TKMWarePlan;
    fCompleted, fIsDestroyed : Boolean;
    fBuildingProgress, fBuildReserve : Word;
    fLastBuildingPoint : TKMStructurePointProgress;//index of point in base order
    fPlaced : Boolean;
    function GetSpec : TKMStructureSpec;
    function GetBasic : TKMStructureBasic;
    function GetToDeliver : TKMWarePlan;
    function GetBridgeType : Byte;
    function GetMaxWorkers : Byte;
    function BuildingPoint : TKMPoint;
    function MaxFraction : Single;
    function GetProgress : Single;
  protected
    function GetPositionForDisplayF: TKMPointF; override;
    function GetIsSelectable: Boolean;  override;
  public
    LastCellID : Byte;
    constructor Create(aUID : Integer; aStructureType, aRotation : Integer; aLoc : TKMPoint; aOwner : TKMHandID);

    constructor Load(LoadStream : TKMemoryStream); override;
    procedure SyncLoad(LoadStream : TKMemoryStream);
    Destructor Destroy; override;
    procedure Save(SaveStream : TKMemoryStream); override;
    function HitTest(aX, aY : Integer) : Boolean;

    property Index : Byte read fIndex;
    property Rotation : Byte read fRot;
    property Position : TKMPoint read fPosition;
    property Cost : TKMWarePlan read fCost;
    property Delivered : TKMWarePlan read fDelivered;
    property ToDeliver : TKMWarePlan read GetToDeliver;
    property Spec : TKMStructureSpec read GetSpec;
    property Basic : TKMStructureBasic read GetBasic;
    property IsComplete : Boolean read fCompleted;
    property IsDestroyed : Boolean read fIsDestroyed;
    property MaxWorkers : Byte read GetMaxWorkers;

    function MaxBuildingProgress : Word;
    property BuildingProgress : Word read fBuildingProgress;
    property Progress : Single read GetProgress;

    procedure DeliverWare(aWare : TKMWareType; aCount : integer);
    function CanBuild : Boolean;
    function IsBridge : Boolean;
    property BridgeType : Byte read GetBridgeType;

    procedure GetCellsAround(aCells : TKMPointDirList);
    procedure IncBuildingProgress;
    procedure FinishStructure;
    procedure DestroyPlan;

    procedure Paint;
  end;

Implementation

uses
    Math,
    KM_RenderPool,
    KM_Resource,
    KM_TerrainTypes, KM_Terrain,
    KM_HandsCollection, KM_Hand,
    KM_CommonUtils;

constructor TKMStructure.Create(aUID: Integer; aStructureType, aRotation: Integer; aLoc: TKMPoint; aOwner: ShortInt);
var I : Integer;
begin
  Assert((aLoc.X <> 0) and (aLoc.Y <> 0));

  inherited Create(etStructure, aUID, aOwner);

  fPosition := aLoc;
  fIndex := aStructureType;
  fRot := aRotation;

  fCompleted := false;
  fIsDestroyed := false;

  fCost.CopyFrom(Spec.Cost);
  fDelivered.CopyFrom(Spec.Cost);

  for I := 0 to fDelivered.Count - 1 do
    fDelivered[I].C := 0;

  fBuildingProgress := 0;
  fBuildReserve := 0;
  for I := 0 to fCost.Count - 1 do
    gHands[Owner].Deliveries.Queue.AddDemand(self, fCost[I].W, fCost[I].C);
  gTerrain.SetStructurePlan(Position, Index, Rotation, hbsStone);
  fLastBuildingPoint.ID := 0;
  fLastBuildingPoint.Fraction := 0;
  fPlaced := false;
end;

destructor TKMStructure.Destroy;
begin
  Inherited;
end;

constructor TKMStructure.Load(LoadStream: TKMemoryStream);
begin
  Inherited;

  LoadStream.Read(fRot);
  LoadStream.Read(fIndex);
  LoadStream.Read(fPosition);
  LoadStream.Read(fCompleted);
  LoadStream.Read(fIsDestroyed);
  LoadStream.Read(fBuildingProgress);
  LoadStream.Read(fBuildReserve);
  LoadStream.Read(LastCellID);
  LoadStream.Read(fLastBuildingPoint, SizeOf(fLastBuildingPoint));
  LoadStream.Read(fPlaced);

  fCost.Load(LoadStream);
  fDelivered.Load(LoadStream);
end;

procedure TKMStructure.Save(SaveStream: TKMemoryStream);
begin
  Inherited;

  SaveStream.Write(fRot);
  SaveStream.Write(fIndex);
  SaveStream.Write(fPosition);
  SaveStream.Write(fCompleted);
  SaveStream.Write(fIsDestroyed);
  SaveStream.Write(fBuildingProgress);
  SaveStream.Write(fBuildReserve);
  SaveStream.Write(LastCellID);
  SaveStream.Write(fLastBuildingPoint, SizeOf(fLastBuildingPoint));
  SaveStream.Write(fPlaced);

  fCost.Save(SaveStream);
  fDelivered.Save(SaveStream);
end;

procedure TKMStructure.SyncLoad(LoadStream: TKMemoryStream);
begin

end;

function TKMStructure.HitTest(aX: Integer; aY: Integer): Boolean;
var X, Y : Integer;
begin
  Result := false;
  if fIsDestroyed then
    Exit;
  for X := 0 to Basic.Size.X - 1 do
  for Y := 0 to Basic.Size.Y - 1 do
    if Basic.PointXY[X, Y] > 0 then
    if ((X + Position.X + Basic.Offset.X) = aX) and ((Y + Position.Y + Basic.Offset.Y) = aY) then
      Exit(true);



end;

function TKMStructure.GetSpec: TKMStructureSpec;
begin
  Result := gRes.Structures[Index];
end;

function TKMStructure.GetBasic: TKMStructureBasic;
begin
  Result := Spec.Basic[fRot];
end;

function TKMStructure.GetToDeliver: TKMWarePlan;
var I : Integer;
begin
  Result.SetCount(0);

  if fCompleted then
    Exit;

  Result := fCost;
  for I := 0 to fCost.Count - 1 do
    Result[I].C := Result[I].C - fDelivered[I].C;
end;

function TKMStructure.GetBridgeType: Byte;
begin
  Result := Spec.BridgeType;
end;

function TKMStructure.GetMaxWorkers: Byte;
begin
  Result := Spec.MaxWorkers;
end;

function TKMStructure.IsBridge: Boolean;
begin
  Result := Spec.BridgeType > 0;
end;

function TKMStructure.CanBuild: Boolean;
var I : Integer;
begin
  Result := true;
  if fBuildReserve = 0 then   
    for I := 0 to fCost.Count - 1 do
      Result := Result and (fDelivered[I].C >= fCost[I].C);

  Result := Result and not self.IsComplete;
end;

procedure TKMStructure.GetCellsAround(aCells: TKMPointDirList);
  procedure Add(aLoc : TKMPointDir);
  begin
    if gTerrain.CheckPassability(aLoc.Loc, tpWalk) and not gTerrain.AvoidTile(aLoc.Loc) then
      aCells.Add(aLoc);
  end;
var X, Y : Integer;
  P : TKMPoint;
begin
  aCells.Clear;

  for X := 0 to Basic.Size.X - 1 do
    for Y := 0 to Basic.Size.Y - 1 do
      if Basic.PointXY[X, Y] > 0 then
      begin
        P.X := Position.X + X + Basic.Offset.X;
        P.Y := Position.Y + Y + Basic.Offset.Y;
        //above
        if (Basic.PointXY[X, Y - 1] = 0) then
          Add(KMPointDir(KMPointAbove(P), dirS));
        //below
        if (Basic.PointXY[X, Y + 1] = 0) then
          Add(KMPointDir(KMPointBelow(P), dirN));
        //left
        if (Basic.PointXY[X - 1, Y] = 0) then
          Add(KMPointDir(KMPointLeft(P), dirE));
        //right
        if (Basic.PointXY[X + 1, Y] = 0) then
          Add(KMPointDir(KMPointRight(P), dirW));
      end;


end;


procedure TKMStructure.DeliverWare(aWare: TKMWareType; aCount: Integer);
var I : Integer;
begin
  for I := 0 to fDelivered.Count - 1 do
    if fDelivered[I].W = aWare then
    begin
      Inc(fDelivered[I].C, aCount);
      Inc(fBuildReserve, Spec.BuildingStep * aCount);
    end;

end;

function TKMStructure.BuildingPoint: TKMPoint;
begin
  Result := KMPointAdd(Position, Basic.Offset, Basic.Order[Min(fLastBuildingPoint.ID, high(Basic.Order))]);
end;

function TKMStructure.MaxBuildingProgress: Word;
begin
  Result := Spec.MaxProgress;
end;

function TKMStructure.MaxFraction: Single;
begin
  Result := (Spec.MaxProgress / Spec.Count); //progress for each tile
end;

function TKMStructure.GetProgress: Single;
begin
  if fPlaced then
    Result := 1
  else
    Result := fLastBuildingPoint.ID / high(Basic.Order);
end;

function TKMStructure.GetPositionForDisplayF: TKMPointF;
begin
  Result := KMPointF(Position);
end;

function TKMStructure.GetIsSelectable: Boolean;
begin
  Result := not IsComplete and not IsDestroyed;
end;

procedure TKMStructure.IncBuildingProgress;
var count : integer;
begin
  fBuildingProgress := EnsureRange(fBuildingProgress + 1, 0, Spec.MaxProgress);
  fBuildReserve := EnsureRange(fBuildReserve - 1, 0, Spec.MaxProgress);

  count := length(Basic.Order);//total count of tiles

  if fPlaced then
  begin
    with fLastBuildingPoint do
      Fraction := EnsureRange(Fraction - MaxFraction / 3, 0, Fraction);

    if fLastBuildingPoint.Fraction <= 0 then //point has removed scaffolding
    begin
      if fLastBuildingPoint.ID = 0 then
      begin
        FinishStructure;
      end else
      begin
        dec(fLastBuildingPoint.ID);
        fLastBuildingPoint.Fraction := MaxFraction;
      end;
    end;

  end else
  begin
    inc(fLastBuildingPoint.Fraction, MaxFraction / 5);
    if fLastBuildingPoint.Fraction > MaxFraction then
    begin
      gTerrain.PlaceStructureTile(Position, Index, Rotation, Min(fLastBuildingPoint.ID, high(Basic.Order)));

      if fLastBuildingPoint.ID = count - 1 then
      begin
        fPlaced := true; //dont' reset fraction here
      end else
      begin
        Inc(fLastBuildingPoint.ID); //next point
        fLastBuildingPoint.Fraction := 0;
      end;
    end;
    gTerrain.FlattenTerrain(BuildingPoint, true, false, 0.25);
  end;



  {if fBuildingProgress >= Spec.MaxProgress then
    FinishStructure;}
end;

procedure TKMStructure.FinishStructure;
begin
  fCompleted := true;
  fBuildingProgress := Spec.MaxProgress;
  gTerrain.PlaceStructure(Position, Index, Rotation);
  gTerrain.SetStructurePlan(Position, Index, Rotation, hbsDone);
end;

procedure TKMStructure.DestroyPlan;
begin
  if IsComplete then  //do not remove completed plan
    Exit;
  if IsDestroyed then  //do not remove destroyed plan
    Exit;

  fIsDestroyed := true;
  //do not delete demand. for some reason it doesn't work
  //gHands[Owner].Deliveries.Queue.RemDemand(self);
  gTerrain.SetStructurePlan(Position, Index, Rotation, hbsWood);
end;

procedure TKMStructure.Paint;
var I, id : Integer;
  P : TKMPoint;
  fract : Integer;
begin
  if fIsDestroyed then
    Exit;
  if fCompleted then
    Exit;

  for I := 0 to Min(fLastBuildingPoint.ID, high(Basic.Order)) do
  begin
    if I < fLastBuildingPoint.ID then
      id := 816
    else
    begin
      fract := trunc(fLastBuildingPoint.Fraction / MaxFraction * MAX_POINT_STAGES); //% of tile progress
      case fract of
        0 : Continue;
        1 : id := 812;
        2 : id := 813;
        3 : id := 814;
        4 : id := 815;
        5 : id := 816;
        else id := 0;
      end;
    end;
    if id = 0 then
      Continue;
    P := KMPointAdd(Position, Basic.Offset, Basic.Order[I]);

    gRenderPool.RenderTerrain.RenderSpriteOnTile(id, P.X, P.Y, 0, rxGui);
  end;
end;


end.

                 