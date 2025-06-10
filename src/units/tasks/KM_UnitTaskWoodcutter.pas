unit KM_UnitTaskWoodCutter;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Points,
  KM_CommonClasses, KM_Defaults, KM_Units,
  KM_Houses, KM_HouseForest;


{Throw a rock}
type
  TKMTaskForestCutting = class(TKMUnitTask)
  private
    fForest : TKMHouseForest;
    fTreeID : Integer;
  public
    constructor Create(aUnit: TKMUnit; aForest : TKMHouse; aTree : Integer);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    property Forest : TKMHouseForest read fForest;
  end;


implementation
uses
  Math,
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_CommonUtils,
  KM_CommonGameTypes,
  KM_Resource, KM_ResTypes, KM_ResMapElements;

const
  MAX_PHASE2 = 5;
{ TTaskThrowRock }
constructor TKMTaskForestCutting.Create(aUnit: TKMUnit; aForest : TKMHouse; aTree : Integer);
begin
  inherited Create(aUnit);
  fType := uttForestCutter;
  fForest := TKMHouseForest(aForest.GetPointer);
  fTreeID := aTree;
end;

destructor TKMTaskForestCutting.Destroy;
begin
  gHands.CleanUpHousePointer(TKMHouse(fForest));
  inherited;
end;

constructor TKMTaskForestCutting.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskForestCutting');
  LoadStream.Read(fForest, 4);
  LoadStream.Read(fTreeID);
end;

function TKMTaskForestCutting.Execute: TKMTaskResult;
var timeToWork : Integer;
begin
  Result := trTaskContinues;
  //Target could have been killed by another Tower or in a fight
  if fUnit.Home.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;
  with fUnit do
    case fPhase of
      0:  begin
            fForest.StartCuttingTree(fTreeID);
            SetActionStay(0, uaWork, true);
            fPhase2 := 0;
          end;
      1:  begin
            Direction := dirNE;
            SetActionStay(gRes.Units[UnitType].UnitAnim[uaWork, Direction].Count, uaWork, false);

            If fPhase2 < gMapElements[fForest.TreeObjID(fTreeID)].AxeHitTimes then
            begin
              fPhase := 0;
              Inc(fPhase2);
            end;
          end;
      2:  begin
            fForest.CutTree(fTreeID);
            SetActionStay(5, uaWalk);
          end;
      3:  begin
            SetActionStay(50, uaWalk);
          end;
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


procedure TKMTaskForestCutting.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskForestCutting');
  SaveStream.Write(fForest.UID);
  SaveStream.Write(fTreeID);
end;

procedure TKMTaskForestCutting.SyncLoad;
begin
  Inherited;
  fForest := TKMHouseForest(gHands.GetHouseByUID(integer(fForest)));
end;

end.
