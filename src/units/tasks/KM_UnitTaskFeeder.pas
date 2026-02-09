unit KM_UnitTaskFeeder;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_Points,
  KM_CommonClasses, KM_Defaults, KM_Units,
  KM_Houses, KM_UnitGroup;


{Throw a rock}
type
  TKMTaskFeeder = class(TKMUnitTask)
  private
    fToHouse : TKMHouse;
    fGroup : TKMUnitGroup;
    procedure FeedGroup;
  public
    constructor Create(aUnit: TKMUnit; aToHouse : TKMHouse; aGroup : TKMUnitGroup);
    destructor Destroy; override;
    constructor Load(LoadStream: TKMemoryStream); override;
    function Execute: TKMTaskResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure SyncLoad; override;

    function WalkShouldAbandon : Boolean; override;
  end;


implementation
uses
  Math,
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_CommonUtils,
  KM_CommonGameTypes,
  KM_Resource, KM_ResTypes, KM_ResMapElements;

{ TTaskThrowRock }
constructor TKMTaskFeeder.Create(aUnit: TKMUnit; aToHouse : TKMHouse; aGroup : TKMUnitGroup);
begin
  inherited Create(aUnit);
  fType := uttFeedGroup;
  fToHouse := aToHouse.GetPointer;
  fGroup := aGroup.GetPointer;
end;

destructor TKMTaskFeeder.Destroy;
begin
  If fGroup <> nil then
    fGroup.FoodRequested := false;
  gHands.CleanUpHousePointer(fToHouse);
  gHands.CleanUpGroupPointer(fGroup);
  inherited;
end;

constructor TKMTaskFeeder.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.CheckMarker('TaskFeeder');
  LoadStream.Read(fToHouse, 4);
  LoadStream.Read(fGroup, 4);
end;

procedure TKMTaskFeeder.FeedGroup;
var I : Integer;
  U : TKMUnit;
  unitsFed : Word;
begin
  unitsFed := 0;
  for I := 0 to fGroup.Count - 1 do
  begin
    U := fGroup.Members[I];
    If U.IsDeadOrDying or (U.Condition > UNIT_MAX_CONDITION * 0.65) then
      Continue;
    U.Condition := UNIT_MAX_CONDITION;
    If U.ConditionPace <= 10 then
      U.ConditionPace := 11;
    Inc(unitsFed);
  end;
  TKMUnitFeeder(fUnit).TakeFood(unitsFed);
end;

function TKMTaskFeeder.WalkShouldAbandon: Boolean;
begin
  Result := not fToHouse.IsValid
            or (fGroup = nil)
            or fGroup.IsDead;
end;

function TKMTaskFeeder.Execute: TKMTaskResult;
var WT : TKMWareType;
  neededFood, wareConsumed, houseFood: Word;
begin
  Result := trTaskContinues;
  //Target could have been killed by another Tower or in a fight
  if WalkShouldAbandon then
  begin
    Result := trTaskDone;
    Exit;
  end;
  with fUnit do
    case fPhase of
      //get inside the house
      0:  begin

            neededFood := Max(fGroup.Count - TKMUnitFeeder(fUnit).FoodCollected, 0);
            //no need to get to the storehouse, we already have food
            //jump to phase 4
            If neededFood = 0 then
            begin
              fPhase := 3;
              SetActionLockedStay(1, uaWalk);
            end else
              SetActionWalkToSpot(fToHouse.GetClosestEntrance(Position).DirFaceLoc);
          end;
      1: SetActionGoIn(uaWalk, gdGoInside, fToHouse);
      //take needed food
      2:  begin
            neededFood := fGroup.Count;
            SetActionLockedStay(neededFood, uaWalk);
            If neededFood = 0 then
              Result := trTaskDone
            else
            If neededFood > 0 then
            begin
              neededFood := Max(neededFood - TKMUnitFeeder(fUnit).FoodCollected, 0);
              If neededFood > 0 then
                for WT in WARES_HOUSE_FOOD do
                begin
                  houseFood := fToHouse.CheckWareIn(WT);
                  If houseFood > 0 then
                  begin
                    wareConsumed := Min(houseFood, neededFood);
                    fToHouse.WareTakeFromOut(WT, wareConsumed, true);
                    neededFood := neededFood - wareConsumed;
                    gHands[Owner].Stats.WareConsumed(WT, wareConsumed);
                    TKMUnitFeeder(fUnit).GiveFood(wareConsumed);
                  end;
                  If neededFood = 0 then
                    Break;
                end;

            end;

          end;
      3:  SetActionGoIn(uaWalk, gdGoOutside, fToHouse);
      //go to group
      4:  begin
            SetActionWalkToGroup(fGroup, 1.42, uaWalkTool);
          end;

      //now feed it
      5:  begin
            SetActionLockedStay(5, uaWalk);
            FeedGroup;
          end;

      //go back to the store
      6:  begin
            SetActionWalkToSpot(fToHouse.GetClosestEntrance(Position).DirFaceLoc, uaWalk, 5);
          end;


      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


procedure TKMTaskFeeder.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskFeeder');
  SaveStream.Write(fToHouse.UID);
  SaveStream.Write(fGroup.UID);
end;

procedure TKMTaskFeeder.SyncLoad;
begin
  Inherited;
  fToHouse := gHands.GetHouseByUID(integer(fToHouse));
  fGroup := gHands.GetGroupByUID(integer(fGroup));
end;

end.
