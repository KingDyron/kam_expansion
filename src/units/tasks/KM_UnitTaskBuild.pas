unit KM_UnitTaskBuild;
{$I KaM_Remake.inc}
interface
uses
  SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Houses, KM_Terrain, KM_Units, KM_ResHouses,
  KM_ResTypes;


//Do the building
type
  TKMTaskBuild = class(TKMUnitTask)
  public
    procedure CancelThePlan; virtual; abstract;
    function CouldBeCancelled: Boolean; override;
  end;

  TKMTaskBuildRoad = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fIsDigged: Boolean;
    fBuildID: Integer;
    fDemandSet: Boolean;
    fTileLockSet: Boolean;
    fRoadType: TKMRoadType;
    fSupplies : Byte;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer;  aRoadType : TKMRoadType);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    procedure AddSupply;
    function HasAllSupplies : Boolean;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildWine = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fIsDigged: Boolean;
    fBuildID: Integer;
    fDemandSet: Boolean;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildField = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fBuildID: Integer;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildGrassLand = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fBuildID: Integer;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildVegeField = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fBuildID: Integer;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;
  TKMTaskBuildRemove = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fBuildID: Integer;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildBridge = class(TKMTaskBuild)
  private
    fBridgeIndex : Word;
    fLoc: TKMPoint;
    fBuildID: Integer;
    fTileLockSet: Boolean;
    fLastPoint : Byte;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    procedure DeliverWare(aWare : TKMWareType);
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildHouseArea = class(TKMTaskBuild)
  private
    fHouse: TKMHouse;
    fHouseType: TKMHouseType;
    fHouseLoc: TKMPoint;
    fBuildID: Integer;
    fHouseNeedsWorker: Boolean;
    fHouseReadyToBuild: Boolean;
    fCellsToDig: array [0..15] of TKMPoint; //max house square is 4*4
    fLastToDig: ShortInt;
    function GetHouseEntranceLoc: TKMPoint;
  public
    constructor Create(aWorker: TKMUnitWorker; aHouseType: TKMHouseType; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    property DigState: ShortInt read fLastToDig;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property House: TKMHouse read fHouse;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Digging: Boolean;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildHouse = class(TKMUnitTask)
  private
    fHouse: TKMHouse;
    fBuildID: Integer;
    fBuildFrom: TKMPointDir; //Current WIP location
    fCells: TKMPointDirList; //List of surrounding cells and directions
  public
    constructor Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property House: TKMHouse read fHouse;
    function WalkShouldAbandon: Boolean; override;
    function CouldBeCancelled: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildHouseUpgrade = class(TKMUnitTask)
  private
    fHouse: TKMHouse;
    fBuildID: Integer;
    fBuildFrom: TKMPointDir; //Current WIP location
    fCells: TKMPointDirList; //List of surrounding cells and directions
  public
    constructor Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property House: TKMHouse read fHouse;
    function WalkShouldAbandon: Boolean; override;
    function CouldBeCancelled: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildHouseRepair = class(TKMUnitTask)
  private
    fHouse: TKMHouse;
    fRepairID: Integer; //Remember the house we repair to report if we died and let others take our place
    fBuildFrom: TKMPointDir; //Current WIP location
    fCells: TKMPointDirList; //List of surrounding cells and directions
  public
    constructor Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aRepairID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    property House: TKMHouse read fHouse;
    function WalkShouldAbandon: Boolean; override;
    function CouldBeCancelled: Boolean; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;

  TKMTaskBuildPalisade = class(TKMTaskBuild)
  private
    fLoc: TKMPoint;
    fBuildID: Integer;
    fDemandSet: Boolean;
    fTileLockSet: Boolean;
  public
    constructor Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    function WalkShouldAbandon: Boolean; override;
    procedure CancelThePlan; override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity,
  KM_HandLogistics, KM_HandsCollection, KM_Resource, KM_ResMapElements,
  KM_Game,
  KM_Hand, KM_HandTypes, KM_HandEntity, KM_TerrainTypes,
  Math,
  KM_ScriptingEvents;


{ TKMTaskBuild }
function TKMTaskBuild.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


{ TKMTaskBuildRoad }
constructor TKMTaskBuildRoad.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer; aRoadType : TKMRoadType);
begin
  inherited Create(aWorker);

  fType := uttBuildRoad;
  fLoc      := aLoc;
  fBuildID   := aID;
  fDemandSet := False;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
  fRoadType := aRoadType;
  fSupplies := 0;
end;


constructor TKMTaskBuildRoad.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildRoad');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fDemandSet);
  LoadStream.Read(fTileLockSet);
  LoadStream.Read(fRoadType, SizeOf(fRoadType));
  LoadStream.Read(fSupplies);
end;


destructor TKMTaskBuildRoad.Destroy;
begin
  if (fUnit <> nil) and fDemandSet then
    gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit);

  if fTileLockSet then
    gTerrain.UnlockTile(fLoc);

  //Yet unstarted
  if (fUnit <> nil) then
  begin
    if fBuildID <> -1 then
    begin
      if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftRoad) then
        //Allow other workers to take this task
        gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
      else
        //This plan is not valid anymore
        gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);
    end
    else
      //Autobuild AI should rebuild roads when worker dies (otherwise house is never built)
      if (gGame <> nil) and not gGame.IsExiting and gHands[fUnit.Owner].AI.Setup.AutoBuild and (fPhase < 9)
      and gHands[fUnit.Owner].CanAddFieldPlan(fLoc, ftRoad) then
      begin
        gHands[fUnit.Owner].Constructions.FieldworksList.AddField(fLoc, ftRoad,fRoadType);
      end;
  end;

  inherited;
end;


function TKMTaskBuildRoad.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftRoad);
end;


procedure TKMTaskBuildRoad.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;

function TKMTaskBuildRoad.HasAllSupplies: Boolean;
begin
  Result := false;
  //Exit(fSupplies >= 1);
  case fRoadType of
    rtNone,
    rtStone,
    rtWooden,
    rtClay: Result := fSupplies >= 1;
    rtExclusive: Result := fSupplies >= 3;
  end;
end;

procedure TKMTaskBuildRoad.AddSupply;
begin
  Inc(fSupplies);
  if HasAllSupplies then
    Inc(fPhase);
end;


function TKMTaskBuildRoad.Execute: TKMTaskResult;
  procedure TryToTakeWares(aWare : TKMWareType; aCount : Integer);
  var H : TKMHouse;
  begin
    H := TKMHouse(gTerrain.House(KMPointAbove(fLoc)));

    if H = nil then //house not found byt tile, try to find by hit test
      if gTerrain.Land[KMPointAbove(fLoc).Y, KMPointAbove(fLoc).X].TileLock in [tlHouse, tlDigged] then
        H := gHands[fUnit.Owner].HousesHitTest(KMPointAbove(fLoc).X, KMPointAbove(fLoc).Y);


    
    if H = nil then
    begin
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount, dtOnce, diHigh4);
      Exit;
    end else
    if H.CheckWareOut(aWare) <= 0 then
    begin
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount, dtOnce, diHigh4);
      Exit;
    end
    else
    if H.CheckWareOut(aWare) >= aCount then
    begin
      H.WareTakeFromOut(aWare, aCount, true);
      Inc(fSupplies, aCount);
    end else
    begin
      Inc(fSupplies, H.CheckWareOut(aWare));
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount - H.CheckWareOut(aWare), dtOnce, diHigh4);
      H.WareTakeFromOut(aWare, aCount, true);
    end;
    

  end;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
         Thought := thNone;
         gTerrain.SetTileLock(fLoc, tlRoadWork);
         fTileLockSet := True;

         CancelThePlan;
         {case fRoadType of
           rtNone,
           rtStone: gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtStone, 1, dtOnce, diHigh4);
           rtWooden: gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTimber, 1, dtOnce, diHigh4);
           rtClay: gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTile, 1, dtOnce, diHigh4);
           rtExclusive: begin
                          gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtStone, 1, dtOnce, diHigh4);
                          gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTile, 1, dtOnce, diHigh4);
                          gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTimber, 1, dtOnce, diHigh4);
                        end;
         end;}

         case fRoadType of
           rtNone,
           rtStone: TryToTakeWares(wtStone, 1);
           rtWooden: TryToTakeWares(wtTimber, 1);
           rtClay: TryToTakeWares(wtTile, 1);
           rtExclusive: begin
                          TryToTakeWares(wtStone, 1);
                          TryToTakeWares(wtTimber, 1);
                          TryToTakeWares(wtTile, 1);
                        end;
         end;
         //TryToTakeWares(wtStone, 1);
         fDemandSet := True;

         SetActionLockedStay(11,uaWork1,False);
       end;
    2: begin
         gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,uaWork1,False);
       end;
    3: begin
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,uaWork1,False);
       end;
    //Warning! This step value is harcoded in KM_UnitTaskDelivery
    4: begin //This step is repeated until Serf brings us some stone
          SetActionLockedStay(30,uaWork1);
          Thought := thStone;
          if not fIsDigged then
          begin
            gScriptEvents.ProcPlanRoadDigged(Owner, fLoc.X, fLoc.Y);
            fIsDigged := True;
          end;
       end;
    5: begin
         SetActionLockedStay(11,uaWork2,False);
         fDemandSet := False;
         Thought := thNone;
       end;
    6: begin
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(11,uaWork2,False);
       end;
    7: begin
         gTerrain.IncDigState(fLoc);
         gTerrain.FlattenTerrain(fLoc); //Flatten the terrain slightly on and around the road
         if gMapElements[gTerrain.Land^[fLoc.Y,fLoc.X].Obj].WineOrCorn then
           gTerrain.RemoveObject(fLoc); //Remove corn/wine/grass as they won't fit with road
         SetActionLockedStay(11,uaWork2,False);
       end;
    8: begin
         gTerrain.SetRoad(fLoc, Owner, fRoadType);
         gTerrain.RemoveObjectsKilledByRoad(fLoc);
         SetActionStay(5, uaWalk);
         gTerrain.UnlockTile(fLoc);
         fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if (fPhase<>4) or (HasAllSupplies) then inc(fPhase); //Phase=4 is when worker waits for rtStone
end;


procedure TKMTaskBuildRoad.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildRoad');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fDemandSet);
  SaveStream.Write(fTileLockSet);
  SaveStream.Write(fRoadType, SizeOf(fRoadType));
  SaveStream.Write(fSupplies);
end;


{ TTaskBuildWine }
constructor TKMTaskBuildWine.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);
  fType := uttBuildWine;
  fLoc      := aLoc;
  fBuildID   := aID;
  fDemandSet := False;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildWine.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.PlaceMarker('TaskBuildWine');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fDemandSet);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildWine.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftWine) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);

  if fDemandSet then
    gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit);

  if fTileLockSet then
    gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TKMTaskBuildWine.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftWine);
end;


procedure TKMTaskBuildWine.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildWine.Execute: TKMTaskResult;
  procedure TryToTakeWares(aWare : TKMWareType; aCount : Integer);
  var H : TKMHouse;
  begin
    H := TKMHouse(gTerrain.House(KMPointAbove(fLoc)));

    if H = nil then
    begin
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount, dtOnce, diHigh4);
      fDemandSet := true;
      Exit;
    end else
    if H.CheckWareOut(aWare) <= 0 then
    begin
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount, dtOnce, diHigh4);
      fDemandSet := true;
      Exit;
    end
    else
    if H.CheckWareOut(aWare) >= aCount then
    begin
      H.WareTakeFromOut(aWare, aCount, true);
      fDemandSet := false;
    end else
    begin
      fDemandSet := True;
      gHands[fUnit.Owner].Deliveries.Queue.AddDemand(nil, fUnit, aWare, aCount, dtOnce, diHigh4);
    end;


  end;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
   0: begin
        SetActionWalkToSpot(fLoc);
        Thought := thBuild;
      end;
   1: begin
        Thought := thNone;
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;

        CancelThePlan;

        gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        TryToTakeWares(wtTimber, 1);
        //gHands[Owner].Deliveries.Queue.AddDemand(nil,fUnit,wtTimber, 1, dtOnce, diHigh4);

        SetActionLockedStay(12*4,uaWork1,False);
      end;
   2: begin
        gTerrain.IncDigState(fLoc);
        SetActionLockedStay(24,uaWork1,False);
      end;
   3: begin
        gTerrain.IncDigState(fLoc);
        SetActionLockedStay(24,uaWork1,False);
      end;
   4: begin
        gTerrain.ResetDigState(fLoc);
        gTerrain.SetInitWine(fLoc, Owner); //Replace the terrain, but don't seed grapes yet
        SetActionLockedStay(30, uaWork1);
        Thought := thWood;
        if not fIsDigged then
        begin
          gScriptEvents.ProcPlanWinefieldDigged(Owner, fLoc.X, fLoc.Y);
          fIsDigged := True;
        end;
      end;
   //Warning! This step value is harcoded in KM_UnitTaskDelivery
   5: begin //This step is repeated until Serf brings us some wood
        SetActionLockedStay(30, uaWork1);
        Thought := thWood;
      end;
   6: begin
        fDemandSet := False;
        SetActionLockedStay(11*8, uaWork2, False);
        Thought := thNone;
      end;
   7: begin
        gTerrain.SetField(fLoc, Owner, ftWine);
        SetActionStay(5, uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
      end;
   else Result := trTaskDone;
  end;
  if (fPhase<>5) or (not fDemandSet) then inc(fPhase); //Phase=5 is when worker waits for rtWood
end;


procedure TKMTaskBuildWine.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildWine');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fDemandSet);
  SaveStream.Write(fTileLockSet);
end;


{ TTaskBuildField }
constructor TKMTaskBuildField.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildField;
  fLoc      := aLoc;
  fBuildID   := aID;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildField.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildField');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildField.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftCorn) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);

  if fTileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TKMTaskBuildField.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftCorn);
end;


procedure TKMTaskBuildField.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildField.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;
        CancelThePlan;
        SetActionLockedStay(0,uaWalk);
       end;
    2: begin
        SetActionLockedStay(11,uaWork1,False);
        inc(fPhase2);
        if fPhase2 = 2 then gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        if (fPhase2 = 4) and gMapElements[gTerrain.Land^[fLoc.Y,fLoc.X].Obj].WineOrCorn then
          gTerrain.RemoveObject(fLoc); //Remove grass/corn/wine as they take up most of the tile
        if fPhase2 in [6,8] then gTerrain.IncDigState(fLoc);
       end;
    3: begin
        Thought := thNone; //Keep thinking build until it's done
        gTerrain.SetField(fLoc, Owner, ftCorn);
        SetActionStay(5,uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if fPhase2 in [0,10] then inc(fPhase);
end;


procedure TKMTaskBuildField.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildField');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fTileLockSet);
end;

{ TTaskBuildGrassLand }
constructor TKMTaskBuildGrassLand.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildGrassLand;
  fLoc      := aLoc;
  fBuildID   := aID;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildGrassLand.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildField');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildGrassLand.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftGrassLand) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);

  if fTileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TKMTaskBuildGrassLand.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftGrassLand);
end;


procedure TKMTaskBuildGrassLand.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildGrassLand.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;
        CancelThePlan;
        SetActionLockedStay(0,uaWalk);
       end;
    2: begin
        SetActionLockedStay(11,uaWork1,False);
        inc(fPhase2);
        if fPhase2 = 2 then gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        if (fPhase2 = 4) and gMapElements[gTerrain.Land^[fLoc.Y,fLoc.X].Obj].WineOrCorn then
          gTerrain.RemoveObject(fLoc); //Remove grass/corn/wine as they take up most of the tile
        if fPhase2 in [6,8] then gTerrain.IncDigState(fLoc);
       end;
    3: begin
        Thought := thNone; //Keep thinking build until it's done
        gTerrain.SetField(fLoc, Owner, ftGrassLand, 0, gftGrass);
        SetActionStay(5,uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if fPhase2 in [0,10] then inc(fPhase);
end;


procedure TKMTaskBuildGrassLand.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildField');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fTileLockSet);
end;

 { TTaskBuildVegeField }
constructor TKMTaskBuildVegeField.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildGrassLand;
  fLoc      := aLoc;
  fBuildID   := aID;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildVegeField.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildField');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildVegeField.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftGrassLand) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);

  if fTileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TKMTaskBuildVegeField.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftGrassLand);
end;


procedure TKMTaskBuildVegeField.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildVegeField.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;
        CancelThePlan;
        SetActionLockedStay(0,uaWalk);
       end;
    2: begin
        SetActionLockedStay(11,uaWork1,False);
        inc(fPhase2);
        if fPhase2 = 2 then gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        if (fPhase2 = 4) and gMapElements[gTerrain.Land^[fLoc.Y,fLoc.X].Obj].WineOrCorn then
          gTerrain.RemoveObject(fLoc); //Remove grass/corn/wine as they take up most of the tile
        if fPhase2 in [6,8] then gTerrain.IncDigState(fLoc);
       end;
    3: begin
        Thought := thNone; //Keep thinking build until it's done
        gTerrain.SetField(fLoc, Owner, ftVegeField, 0, gftPumpkin);
        SetActionStay(5,uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if fPhase2 in [0,10] then inc(fPhase);
end;


procedure TKMTaskBuildVegeField.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildField');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fTileLockSet);
end;
{ TTaskBuildRemove }
constructor TKMTaskBuildRemove.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildRemove;
  fLoc      := aLoc;
  fBuildID   := aID;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildRemove.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildRemove');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildRemove.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftRemove) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
    else
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);

  if fTileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;


function TKMTaskBuildRemove.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftRemove, fUnit.Owner);
end;


procedure TKMTaskBuildRemove.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildRemove.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;
        CancelThePlan;
        SetActionLockedStay(0,uaWalk);
       end;
    2: begin
        SetActionLockedStay(22,uaWork1,False);
        inc(fPhase2);
        //if fPhase2 = 2 then gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house)
        if (fPhase2 = 6) and (gMapElements[gTerrain.Land^[fLoc.Y,fLoc.X].Obj].WineOrCorn or gTerrain.TileHasPalisade(fLoc.X, fLoc.Y)) then
          gTerrain.RemoveObject(fLoc); //Remove grass/corn/wine as they take up most of the tile

        if fPhase2 in [5,6] then gTerrain.IncDigState(fLoc);
        if fPhase2 = 7 then
        begin
          gTerrain.ResetDigState(fLoc);
          gTerrain.IncDigState(fLoc);
        end;

       end;
    3: begin
        Thought := thNone; //Keep thinking build until it's done

        if gTerrain.TileHasRoad(fLoc) then
          gTerrain.RemRoad(fLoc)
        else
        if gRes.Tileset[gTerrain.Land[fLoc.Y, fLoc.X].BaseLayer.Terrain].Wine
          or gRes.Tileset[gTerrain.Land[fLoc.Y, fLoc.X].BaseLayer.Terrain].Corn
          or gRes.Tileset[gTerrain.Land[fLoc.Y, fLoc.X].BaseLayer.Terrain].Grass then
        begin
          gTerrain.RemField(fLoc);
          gTerrain.SetDefTile(fLoc);
          gTerrain.UpdateFences(fLoc);
          gTerrain.IncDigState(fLoc);
        end
        else
        if gTerrain.TileHasPalisade(fLoc.X, fLoc.Y) then
          gTerrain.RemPalisade(fLoc);

        //gTerrain.IncDigState(fLoc);


        SetActionStay(5,uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if fPhase2 in [0,10] then inc(fPhase);
end;

procedure TKMTaskBuildRemove.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildRemove');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fTileLockSet);
end;

destructor TKMTaskBuildBridge.Destroy;
begin
  //Yet unstarted
  if fBuildID <> -1 then
    gHands[fUnit.Owner].Constructions.BridgePlanList.RemWorker(fBuildID);

  //if fTileLockSet then gTerrain.UnlockTile(fLoc);
  inherited;
end;

constructor TKMTaskBuildBridge.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildBridge;
  fLoc      := aLoc;
  fBuildID   := aID;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
  fBridgeIndex := gHands[fUnit.Owner].Constructions.BridgePlanList.Plans[fBuildID].Index;
  fLastPoint := 255;
end;

constructor TKMTaskBuildBridge.Load(LoadStream: TKMemoryStream);
begin
  Inherited;
  LoadStream.CheckMarker('TaskBuildBridge');
  LoadStream.Read(fBridgeIndex);
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fTileLockSet);
end;


procedure TKMTaskBuildBridge.DeliverWare(aWare: TKMWareType);
begin
  gHands[fUnit.Owner].Constructions.BridgePlanList.DeliverWares(fBuildID, aWare);
end;
function TKMTaskBuildBridge.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := gHands[fUnit.Owner].Constructions.BridgePlanList.ShouldAbondonJob(fBuildID);
end;

procedure TKMTaskBuildBridge.CancelThePlan;
begin
  fBuildID := -1;
end;

function TKMTaskBuildBridge.Execute: TKMTaskResult;
var phaseDone : Boolean;
  oldPointID : Byte;
begin

  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit, [wtAll]);
    Exit;
  end;
  phaseDone := true;
  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;

    1: begin
        gTerrain.SetTileLock(fLoc, tlFieldWork);
        fTileLockSet := True;
        //CancelThePlan;
        with gHands[fUnit.Owner].Constructions.BridgePlanList do
        begin
          if not HasAllBuildingMaterials(fBuildID) then //If all materials are there, don't make more demands
            AddCostDemands(fBuildID, TKMUnitWorker(fUnit));
        end;
        SetActionLockedStay(22,uaWalk, true);
       end;
    2: begin
        //worker must stay in this position until he get all materials

        with gHands[fUnit.Owner].Constructions.BridgePlanList do
          if HasAllBuildingMaterials(fBuildID) then
          begin
            SetActionLockedStay(10,uaWork2,false);
            IncBuildingProgress(fBuildID);
            phaseDone := Plans[fBuildID].BuildingProgress >= (gRes.Bridges[fBridgeIndex].MaxProgress * 2);

            if not Plans[fBuildID].BridgeBuilt then
              if Plans[fBuildID].BuildingProgress >= gRes.Bridges[fBridgeIndex].MaxProgress then
              begin
                with Plans[fBuildID] do
                begin
                  gTerrain.PlaceBridge(Loc, Index, Rotation);
                  gHands[fUnit.Owner].BuildBridge(Loc, Index, Rotation);
                end;
                Plans[fBuildID].BridgeBuilt := true;
              end else
              begin
                oldPointID := fLastPoint;
                with Plans[fBuildID] do
                begin
                  fLastPoint := Round(BuildingProgress / gRes.Bridges[fBridgeIndex].MaxProgress * (gRes.Bridges[fBridgeIndex].Count - 1));

                  if oldPointID <> fLastPoint then
                  begin
                    gTerrain.FlattenTerrain(KMPointAdd(Loc,  gRes.Bridges[fBridgeIndex].PointOrder[Rotation][fLastPoint]));
                  end;
                end;

              end;
            
          end else
          begin
            SetActionLockedStay(22,uaWalk,true);// wait until serfs deliver wares
            phaseDone := false;
          end;

       end;
    3: begin
        Thought := thNone;
        gHands[fUnit.Owner].Constructions.BridgePlanList.FinishBridge(fBuildID);

        SetActionStay(5,uaWalk);
        gTerrain.UnlockTile(fLoc);
        fTileLockSet := False;
       end;
    else Result := trTaskDone;
  end;
  if phaseDone then
    inc(fPhase);
end;


procedure TKMTaskBuildBridge.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildBridge');
  SaveStream.Write(fBridgeIndex);
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fTileLockSet);
end;

{ TTaskBuildHouseArea }
constructor TKMTaskBuildHouseArea.Create(aWorker: TKMUnitWorker; aHouseType: TKMHouseType; const aLoc: TKMPoint; aID: Integer);
var
  I,K: Integer;
  HA: TKMHouseArea;
begin
  inherited Create(aWorker);
  fType  := uttBuildHouseArea;
  fHouseType := aHouseType;
  fHouseLoc  := aLoc;
  fBuildID    := aID;
  fHouseNeedsWorker  := False; //House needs this worker to complete
  fHouseReadyToBuild := False; //House is ready to be built
  aWorker.Thought := thBuild;

  HA := gRes.Houses[fHouseType].BuildArea;

  //Fill Cells left->right, top->bottom. Worker will start flattening from the end (reversed)
  fLastToDig := -1;
  for I := 1 to 4 do for K := 1 to 4 do
  if HA[I,K] <> 0 then
  begin
    Inc(fLastToDig);
    fCellsToDig[fLastToDig] := KMPoint(fHouseLoc.X + K - 3, fHouseLoc.Y + I - 4);
  end;
end;


constructor TKMTaskBuildHouseArea.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildHouseArea');
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fHouseType, SizeOf(fHouseType));
  LoadStream.Read(fHouseLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fHouseNeedsWorker);
  LoadStream.Read(fHouseReadyToBuild);
  LoadStream.Read(fLastToDig);
  LoadStream.Read(fCellsToDig, SizeOf(fCellsToDig));
end;


procedure TKMTaskBuildHouseArea.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


{ We need to revert all changes made }
destructor TKMTaskBuildHouseArea.Destroy;
begin
  //Don't demolish the house when the game is exiting (causes wrong stats and errors in script)
  if (gGame = nil) or gGame.IsExiting then
	  Exit;

  //Yet unstarted
  if (fBuildID <> -1) then
    if gTerrain.CanPlaceHouse(GetHouseEntranceLoc,fHouseType) then
      //Allow other workers to take this task
      gHands[fUnit.Owner].Constructions.HousePlanList.ReOpenPlan(fBuildID)
    else
    begin
      //This plan is not valid anymore
      gHands[fUnit.Owner].Constructions.HousePlanList.ClosePlan(fBuildID);
      gHands[fUnit.Owner].Stats.HousePlanRemoved(fHouseType);
    end;

  //Destroy the house if worker was killed (e.g. by archer or hunger)
  //as we don't have mechanics to resume the building process yet
  if fHouseNeedsWorker and (fHouse <> nil) and not fHouse.IsDestroyed then
    //Use handID of unit killer, since he indirecty caused house demolishing
    fHouse.Demolish(fUnit.KilledBy);

  //Complete the task in the end (Worker could have died while trying to exit building area)
  if fHouseReadyToBuild and not fHouseNeedsWorker and (fHouse <> nil) and not fHouse.IsDestroyed then
  begin
    fHouse.BuildingState := hbsWood;
    gHands[fUnit.Owner].Constructions.HouseList.AddHouse(fHouse); //Add the house to JobList, so then all workers could take it
    gHands[fUnit.Owner].Deliveries.Queue.AddDemand(fHouse, nil, wtTimber, gRes.Houses[fHouse.HouseType].WoodCost, dtOnce, diHigh4);
    gHands[fUnit.Owner].Deliveries.Queue.AddDemand(fHouse, nil, wtStone, gRes.Houses[fHouse.HouseType].StoneCost, dtOnce, diHigh4);
    gHands[fUnit.Owner].Deliveries.Queue.AddDemand(fHouse, nil, wtTile, gRes.Houses[fHouse.HouseType].TileCost, dtOnce, diHigh4);

  end;

  gHands.CleanUpHousePointer(fHouse);
  inherited;
end;


function TKMTaskBuildHouseArea.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanPlaceHouse(GetHouseEntranceLoc, fHouseType);
end;


function TKMTaskBuildHouseArea.GetHouseEntranceLoc: TKMPoint;
begin
  Result.X := fHouseLoc.X + gRes.Houses[fHouseType].EntranceOffsetX;
  Result.Y := fHouseLoc.Y;
end;


//Tell if we are in Digging phase where we can walk on tlDigged tiles
//(incl. phase when we walk out)
function TKMTaskBuildHouseArea.Digging: Boolean;
begin
  Result := fPhase >= 2;
end;


procedure TKMTaskBuildHouseArea.CancelThePlan;
begin
  //House plan could be canceled during initial walk or while walking within the house area so
  //ignore it if it's already been canceled (occurs when trying to walk within range of an enemy tower during flattening)
  if fBuildID = -1 then Exit;
  gHands[fUnit.Owner].Constructions.HousePlanList.ClosePlan(fBuildID);
  gHands[fUnit.Owner].Stats.HousePlanRemoved(fHouseType);
  fBuildID := -1;
end;


//Prepare building site - flatten terrain
function TKMTaskBuildHouseArea.Execute: TKMTaskResult;
var OutOfWay: TKMPoint;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  if (fHouse <> nil) and fHouse.IsDestroyed then
  begin
    Result := trTaskDone;
    fUnit.Thought := thNone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          SetActionWalkToSpot(GetHouseEntranceLoc);
          Thought := thBuild;
        end;
    1:  begin
          CancelThePlan;
          Assert(fHouse = nil);

          fHouse := gHands[Owner].AddHouseWIP(fHouseType, fHouseLoc);
          Assert(fHouse <> nil, 'Failed to add wip house');
          fHouse := fHouse.GetPointer; //We need to register a pointer to the house

          fHouseNeedsWorker := True; //The house placed on the map, if something happens with Worker the house will be removed
          SetActionLockedStay(2, uaWalk);
          Thought := thNone;
        end;
    2:  //The house can become too steep after we flatten one part of it
        if CanWalkTo(fCellsToDig[fLastToDig], 0) then
          SetActionWalkToSpot(fCellsToDig[fLastToDig])
        else
        begin
          Result := trTaskDone;
          fUnit.Thought := thNone;
          Exit;
        end;
    3:  begin
          SetActionLockedStay(11,uaWork1,False); //Don't flatten terrain here as we haven't started digging yet
        end;
    4:  begin
          SetActionLockedStay(11,uaWork1,False);
          gTerrain.FlattenTerrain(fCellsToDig[fLastToDig]);
        end;
    5:  begin
          SetActionLockedStay(11,uaWork1,False);
          gTerrain.FlattenTerrain(fCellsToDig[fLastToDig]);
        end;
    6:  begin
          SetActionLockedStay(11,uaWork1,False);
          gTerrain.FlattenTerrain(fCellsToDig[fLastToDig]);
          gTerrain.FlattenTerrain(fCellsToDig[fLastToDig]); //Flatten the terrain twice now to ensure it really is flat

          if gTerrain.Land^[fCellsToDig[fLastToDig].Y, fCellsToDig[fLastToDig].X].TileLock = tlWallFence then
            gTerrain.SetTileLock(fCellsToDig[fLastToDig], tlWallFence) //Block passability on tile
          else
            gTerrain.SetTileLock(fCellsToDig[fLastToDig], tlDigged); //Block passability on tile

          if KMSamePoint(fHouse.Entrance, fCellsToDig[fLastToDig]) then
            if not (fHouse.HouseType in WALL_HOUSES) then
                gTerrain.SetRoad(fHouse.Entrance, Owner, gTerrain.GetRoadType(fHouse.Entrance.X, fHouse.Entrance.Y + 1));

          if not ((fHouse.HouseType = htAppleTree) and KMSamePoint(fHouse.Entrance, fCellsToDig[fLastToDig])) then
            gTerrain.RemoveObject(fCellsToDig[fLastToDig]); //All objects are removed
          Dec(fLastToDig);
        end;
    7:  begin
          //Walk away from building site, before we get trapped when house becomes stoned
          OutOfWay := gTerrain.GetOutOfTheWay(fUnit, KMPOINT_ZERO, tpWalk);
          //GetOutOfTheWay can return the input position (GetPosition in this case) if no others are possible
          if KMSamePoint(OutOfWay, KMPOINT_ZERO) or KMSamePoint(OutOfWay, Position) then
            OutOfWay := fHouse.PointBelowEntrance; //Don't get stuck in corners
          SetActionWalkToSpot(OutOfWay);
          fHouseNeedsWorker := False; //House construction no longer needs the worker to continue
          fHouseReadyToBuild := True; //If worker gets killed while walking house will be finished without him
          gScriptEvents.ProcHousePlanDigged(fHouse);
        end;
    else
        Result := trTaskDone;
  end;

  Inc(fPhase);

  if (fPhase = 7) and (fLastToDig >= 0) then
    fPhase := 2; //Repeat with next cell
end;


procedure TKMTaskBuildHouseArea.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildHouseArea');
  SaveStream.Write(fHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fHouseType, SizeOf(fHouseType));
  SaveStream.Write(fHouseLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fHouseNeedsWorker);
  SaveStream.Write(fHouseReadyToBuild);
  SaveStream.Write(fLastToDig);
  SaveStream.Write(fCellsToDig, SizeOf(fCellsToDig));
end;


{ TTaskBuildHouse }
constructor TKMTaskBuildHouse.Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aID: Integer);
begin
  inherited Create(aWorker);
  fType := uttBuildHouse;
  fHouse    := aHouse.GetPointer;
  fBuildID   := aID;
  aWorker.Thought := thBuild;

  fCells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(fCells, aWorker.DesiredPassability);
end;


constructor TKMTaskBuildHouse.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskBuildHouse');
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fBuildFrom);
  fCells := TKMPointDirList.Create;
  fCells.LoadFromStream(LoadStream);
end;


procedure TKMTaskBuildHouse.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


destructor TKMTaskBuildHouse.Destroy;
begin
  //We are no longer connected to the House (it's either done or we died)
  gHands[fUnit.Owner].Constructions.HouseList.RemWorker(fBuildID);
  gHands.CleanUpHousePointer(fHouse);
  FreeAndNil(fCells);
  inherited;
end;


// If we are walking to the house but the house is destroyed/canceled we should abandon immediately
// If house has not enough resource to be built, consider building task is done and look for a new
// task that has enough resouces. Once this house has building resources delivered it will be
// available from build queue again
// If house is already built by other workers
function TKMTaskBuildHouse.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed or not fHouse.CheckResToBuild or fHouse.IsComplete;
end;


function TKMTaskBuildHouse.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


{Build the house}
function TKMTaskBuildHouse.Execute: TKMTaskResult;
  function LocOccupied(aLoc : TKMPoint) : Boolean;
  var W : TKMUnitWorker;
  begin
    if not fUnit.CanWalkTo(aLoc, 0) then
      Exit(true);
    
    if (gTerrain.GetUnit(aLoc) = nil) or not (gTerrain.GetUnit(aLoc) is TKMUnitWorker) then //unit must be worker
      Exit(false);
    W := TKMUnitWorker(gTerrain.GetUnit(aLoc));
    Result := not W.IsDeadOrDying and (W <> fUnit); //ignore dead workers
  end;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    fUnit.Thought := thNone;
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWorker(fUnit) do
  case fPhase of
    0:  if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);
        end
        else
          Result := trTaskDone;
    1:  if not LocOccupied(fBuildFrom.Loc) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc);//walk directly to this place
        end else
        if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);//try to use different location
          fPhase := 0;
        end
        else
          Result := trTaskDone;


    //WARNING!!! THIS PHASE VALUE IS USED IN TKMTaskDelivery to construction !!!
    2:  begin
          //Face the building
          Direction := fBuildFrom.Dir;
          SetActionLockedStay(0, uaWalk);
        end;
    3:  begin
          //Start animation
          SetActionLockedStay(5, uaWork, False);
          Direction := fBuildFrom.Dir;
          //Remove house plan when we start the stone phase (it is still required for wood)
          //But don't do it every time we hit if it's already done!
          if ((fHouse.BuildingProgress > fHouse.MaxHealth - 100) or fHouse.IsStone)
             and (gTerrain.Land^[fHouse.Position.Y, fHouse.Position.X].TileLock <> tlHouse) then
            gTerrain.SetHouse(fHouse, Owner, hsBuilt);
            //gTerrain.SetHouse(fHouse.Position, fHouse.HouseType, hsBuilt, Owner);
        end;
    4:  begin
          //Update house on hummer hit
          fHouse.IncBuildingProgress;
          SetActionLockedStay(6, uaWork, False, 0, 5); //Do building and end animation
          if not (fHouse.HouseType in WALL_HOUSES) then
              gTerrain.SetRoad(fHouse.Entrance, Owner, gTerrain.GetRoadType(fHouse.Entrance.X, fHouse.Entrance.Y + 1));
          Inc(fPhase2);
        end;
    5:  begin
          SetActionStay(1, uaWalk);
          Thought := thNone;
        end;
    else Result := trTaskDone;
  end;
  Inc(fPhase);

  {Worker does 8 hits from any spot around the house and then goes to new spot,
   but if the house is done worker should stop activity immediately}
  if (fPhase = 5) and (not fHouse.IsComplete) then //If animation cycle is done
    if fPhase2 mod 8 = 0 then //if worker did [8] hits from same spot
      fPhase := 0 //Then goto new spot
    else
      fPhase := 3; //else do more hits
end;


procedure TKMTaskBuildHouse.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildHouse');
  SaveStream.Write(fHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fBuildID);
  SaveStream.Write(fBuildFrom);
  fCells.SaveToStream(SaveStream);
end;

{TKMTaskBuildHouseUpgrade}
constructor TKMTaskBuildHouseUpgrade.Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aID: Integer);
begin
  inherited Create(aWorker);
  fType := uttBuildHouseUpgrade;
  fHouse    := aHouse.GetPointer;
  fBuildID   := aID;
  aWorker.Thought := thBuild;

  fCells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(fCells, aWorker.DesiredPassability);
end;


constructor TKMTaskBuildHouseUpgrade.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskBuildHouse');
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fBuildFrom);
  fCells := TKMPointDirList.Create;
  fCells.LoadFromStream(LoadStream);
end;


procedure TKMTaskBuildHouseUpgrade.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


destructor TKMTaskBuildHouseUpgrade.Destroy;
begin
  //We are no longer connected to the House (it's either done or we died)
  gHands[fUnit.Owner].Constructions.HouseUpgradeList.RemWorker(fBuildID);
  gHands.CleanUpHousePointer(fHouse);
  FreeAndNil(fCells);
  inherited;
end;


// If we are walking to the house but the house is destroyed/canceled we should abandon immediately
// If house has not enough resource to be built, consider building task is done and look for a new
// task that has enough resouces. Once this house has building resources delivered it will be
// available from build queue again
// If house is already built by other workers
function TKMTaskBuildHouseUpgrade.WalkShouldAbandon: Boolean;
begin
  if fHouse = nil then
  Exit(true);
  Result := fHouse.IsDestroyed or not fHouse.CheckResToBuild or not fHouse.IsUpgrading;
end;


function TKMTaskBuildHouseUpgrade.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


{Build the house}
function TKMTaskBuildHouseUpgrade.Execute: TKMTaskResult;
  function LocOccupied(aLoc : TKMPoint) : Boolean;
  var W : TKMUnitWorker;
  begin
    if (gTerrain.GetUnit(aLoc) = nil) or not (gTerrain.GetUnit(aLoc) is TKMUnitWorker) then //unit must be worker
      Exit(false);
    W := TKMUnitWorker(gTerrain.GetUnit(aLoc));
    Result := not W.IsDeadOrDying and (W <> fUnit); //ignore dead workers
  end;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    fUnit.Thought := thNone;
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWorker(fUnit) do
  case fPhase of
    0:  if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);
        end
        else
          Result := trTaskDone;
    1:  if not LocOccupied(fBuildFrom.Loc) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc);//walk directly to this place
        end else
        if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);//try to use different location
          fPhase := 0;
        end
        else
          Result := trTaskDone;
    //WARNING!!! THIS PHASE VALUE IS USED IN TKMTaskDelivery to construction !!!
    2:  begin
          //Face the building
          Direction := fBuildFrom.Dir;
          SetActionLockedStay(0, uaWalk);
        end;
    3:  begin
          //Start animation
          SetActionLockedStay(5, uaWork, False);
          Direction := fBuildFrom.Dir;
          //Remove house plan when we start the stone phase (it is still required for wood)
          //But don't do it every time we hit if it's already done!
          if ((fHouse.BuildingProgress > fHouse.MaxHealth - 100) or fHouse.IsStone)
             and (gTerrain.Land^[fHouse.Position.Y, fHouse.Position.X].TileLock <> tlHouse) then
            gTerrain.SetHouse(fHouse, Owner, hsBuilt);
            //gTerrain.SetHouse(fHouse.Position, fHouse.HouseType, hsBuilt, Owner);
        end;
    4:  begin
          //Update house on hummer hit
          fHouse.IncBuildingUpgradeProgress;
          SetActionLockedStay(6, uaWork, False, 0, 5); //Do building and end animation
          Inc(fPhase2);
        end;
    5:  begin
          SetActionStay(1, uaWalk);
          Thought := thNone;
        end;
    else Result := trTaskDone;
  end;
  Inc(fPhase);

  {Worker does 8 hits from any spot around the house and then goes to new spot,
   but if the house is done worker should stop activity immediately}
  if (fPhase = 5) and fHouse.IsUpgrading then //If animation cycle is done
    if fPhase2 mod 8 = 0 then //if worker did [8] hits from same spot
      fPhase := 0 //Then goto new spot
    else
      fPhase := 3; //else do more hits
end;


procedure TKMTaskBuildHouseUpgrade.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildHouse');
  SaveStream.Write(fHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fBuildID);
  SaveStream.Write(fBuildFrom);
  fCells.SaveToStream(SaveStream);
end;



{ TTaskBuildHouseRepair }
constructor TKMTaskBuildHouseRepair.Create(aWorker: TKMUnitWorker; aHouse: TKMHouse; aRepairID: Integer);
begin
  inherited Create(aWorker);
  fType := uttBuildHouseRepair;
  fHouse    := aHouse.GetPointer;
  fRepairID := aRepairID;
  aWorker.Thought := thBuild;

  fCells := TKMPointDirList.Create;
  fHouse.GetListOfCellsAround(fCells, aWorker.DesiredPassability);
end;


constructor TKMTaskBuildHouseRepair.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskBuildHouseRepair');
  LoadStream.Read(fHouse, 4);
  LoadStream.Read(fRepairID);
  LoadStream.Read(fBuildFrom);
  fCells := TKMPointDirList.Create;
  fCells.LoadFromStream(LoadStream);
end;


procedure TKMTaskBuildHouseRepair.SyncLoad;
begin
  inherited;
  fHouse := gHands.GetHouseByUID(Integer(fHouse));
end;


destructor TKMTaskBuildHouseRepair.Destroy;
begin
  gHands[fUnit.Owner].Constructions.RepairList.RemWorker(fRepairID);
  gHands.CleanUpHousePointer(fHouse);
  FreeAndNil(fCells);
  inherited;
end;


function TKMTaskBuildHouseRepair.WalkShouldAbandon: Boolean;
begin
  Result := fHouse.IsDestroyed
            or not fHouse.IsDamaged
            or not fHouse.BuildingRepair;
end;


function TKMTaskBuildHouseRepair.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


{Repair the house}
function TKMTaskBuildHouseRepair.Execute: TKMTaskResult;
  function LocOccupied(aLoc : TKMPoint) : Boolean;
  var W : TKMUnitWorker;
  begin
    if (gTerrain.GetUnit(aLoc) = nil) or not (gTerrain.GetUnit(aLoc) is TKMUnitWorker) then //unit must be worker
      Exit(false);
    W := TKMUnitWorker(gTerrain.GetUnit(aLoc));
    Result := not W.IsDeadOrDying and (W <> fUnit); //ignore dead workers
  end;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with TKMUnitWorker(fUnit) do
    case fPhase of
    0:  if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);
        end
        else
          Result := trTaskDone;
    1:  if not LocOccupied(fBuildFrom.Loc) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc);//walk directly to this place
        end else
        if PickNextSpot(fCells, fBuildFrom) then
        begin
          Thought := thBuild;
          SetActionWalkToSpot(fBuildFrom.Loc, uaWalk, 1.42);//try to use different location
          fPhase := 0;
        end
        else
          Result := trTaskDone;
      2:  begin
            Direction := fBuildFrom.Dir;
            SetActionLockedStay(0, uaWalk);
          end;
      3:  begin
            SetActionLockedStay(5, uaWork, False, 0, 0); //Start animation
            Direction := fBuildFrom.Dir;
          end;
      4:  begin
            fHouse.AddRepair;
            SetActionLockedStay(6, uaWork,False, 0, 5); //Do building and end animation
            inc(fPhase2);
          end;
      5:  begin
            Thought := thNone;
            SetActionStay(1, uaWalk);
          end;
      else
          Result := trTaskDone;
    end;
  inc(fPhase);

  if fPhase = 5 then //If animation cycle is done
    if fPhase2 mod 5 = 0 then //if worker did [5] hits from same spot
      fPhase := 0 //Then goto new spot
    else
      fPhase := 3; //else do more hits
end;


procedure TKMTaskBuildHouseRepair.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildHouseRepair');
  SaveStream.Write(fHouse.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fRepairID);
  SaveStream.Write(fBuildFrom);
  fCells.SaveToStream(SaveStream);
end;


{ TKMTaskBuildRoad }
constructor TKMTaskBuildPalisade.Create(aWorker: TKMUnitWorker; const aLoc: TKMPoint; aID: Integer);
begin
  inherited Create(aWorker);

  fType := uttBuildPalisade;
  fLoc      := aLoc;
  fBuildID   := aID;
  fDemandSet := False;
  fTileLockSet := False;
  aWorker.Thought := thBuild;
end;


constructor TKMTaskBuildPalisade.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskBuildPalisade');
  LoadStream.Read(fLoc);
  LoadStream.Read(fBuildID);
  LoadStream.Read(fDemandSet);
  LoadStream.Read(fTileLockSet);
end;


destructor TKMTaskBuildPalisade.Destroy;
begin
  if (fUnit <> nil) and fDemandSet then
    gHands[fUnit.Owner].Deliveries.Queue.RemDemand(fUnit);

  if fTileLockSet then
    gTerrain.UnlockTile(fLoc);

  //Yet unstarted
  if (fUnit <> nil) then
  begin
    if fBuildID <> -1 then
    begin
      if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftPalisade) then
        //Allow other workers to take this task
        gHands[fUnit.Owner].Constructions.FieldworksList.ReOpenField(fBuildID)
      else
        //This plan is not valid anymore
        gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID);
    end
    else
      //Autobuild AI should rebuild roads when worker dies (otherwise house is never built)
      if (gGame <> nil) and not gGame.IsExiting and gHands[fUnit.Owner].AI.Setup.AutoBuild and (fPhase < 9)
      and gHands[fUnit.Owner].CanAddFieldPlan(fLoc, ftPalisade) then
        gHands[fUnit.Owner].Constructions.FieldworksList.AddField(fLoc, ftPalisade);
  end;

  inherited;
end;


function TKMTaskBuildPalisade.WalkShouldAbandon: Boolean;
begin
  //Walk should abandon if other player has built something there before we arrived
  Result := (fBuildID <> -1) and not gTerrain.CanAddField(fLoc.X, fLoc.Y, ftPalisade);
end;


procedure TKMTaskBuildPalisade.CancelThePlan;
begin
  gHands[fUnit.Owner].Constructions.FieldworksList.CloseField(fBuildID); //Close the job now because it can no longer be cancelled
  fBuildID := -1;
end;


function TKMTaskBuildPalisade.Execute: TKMTaskResult;
//var OutOfWay : TKMPoint;
begin
  Result := trTaskContinues;

  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;


  with fUnit do
  case fPhase of
    0: begin
         SetActionWalkToSpot(fLoc);
         Thought := thBuild;
       end;
    1: begin
         Thought := thNone;
         gTerrain.SetTileLock(fLoc, tlRoadWork);
         fTileLockSet := True;

         CancelThePlan;

         gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTimber, 1, dtOnce, diHigh4);
         fDemandSet := True;

         SetActionLockedStay(44,uaWork1,False);
       end;
    2: begin
         gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(44,uaWork1,False);
       end;
    3: begin
         gTerrain.IncDigState(fLoc);
         SetActionLockedStay(44,uaWork1,False);
       end;
    //Warning! This step value is harcoded in KM_UnitTaskDelivery
    4: begin //This step is repeated until Serf brings us some stone
         SetActionLockedStay(30,uaWalk);
         Thought := thWood;
       end;
    5: begin
         gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
         SetActionLockedStay(44,uaWork2,False);
         gTerrain.IncDigState(fLoc);
         gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTimber, 1, dtOnce, diHigh4);
       end;
    6: begin
         SetActionLockedStay(44,uaWork2,False);
       end;
    //Warning! This step value is harcoded in KM_UnitTaskDelivery
    7: begin //This step is repeated until Serf brings us some stone
         SetActionLockedStay(30,uaWalk);
         Thought := thWood;
       end;
    8: begin
         gTerrain.ResetDigState(fLoc); //Remove any dig over that might have been there (e.g. destroyed house) after first dig
         SetActionLockedStay(44,uaWork2,False);
         gTerrain.IncDigState(fLoc);
         gHands[Owner].Deliveries.Queue.AddDemand(nil, fUnit, wtTimber, 1, dtOnce, diHigh4);
       end;
    9: begin
         SetActionLockedStay(44,uaWork2,False);
       end;
    //Warning! This step value is harcoded in KM_UnitTaskDelivery
    10: begin //This step is repeated until Serf brings us some stone
         SetActionLockedStay(30,uaWalk);
         Thought := thWood;
       end;
    11: begin
         SetActionLockedStay(44,uaWork2,False);
         fDemandSet := False;
         Thought := thNone;
       end;
    12: begin
         SetActionLockedStay(44,uaWork2,False);
       end;
    13: begin
         gTerrain.FlattenTerrain(fLoc); //Flatten the terrain slightly on and around the road

         SetActionLockedStay(44,uaWork2,False);
       end;
    14: begin
         gTerrain.ResetDigState(fLoc);

         if gTerrain.CanAddField(fLoc.X, fLoc.Y, ftPalisade) then
          gHands[Owner].AddPalisade(fLoc);

         SetActionStay(5, uaWalk);
         fTileLockSet := False;
         gTerrain.UnlockTile(fLoc);
       end;
    else Result := trTaskDone;
  end;

  if fPhase<>4 then
  if fPhase<>7 then
  if fPhase<>10 then
    inc(fPhase); //Phase=4 is when worker waits for rtStone
end;


procedure TKMTaskBuildPalisade.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskBuildPalisade');
  SaveStream.Write(fLoc);
  SaveStream.Write(fBuildID);
  SaveStream.Write(fDemandSet);
  SaveStream.Write(fTileLockSet);
end;

end.
