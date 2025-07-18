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
  public
    constructor Create(aUnit: TKMUnit; aToHouse : TKMHouse);
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
constructor TKMTaskFeeder.Create(aUnit: TKMUnit; aToHouse : TKMHouse);
begin
  inherited Create(aUnit);
  fType := uttFeedGroup;
  fToHouse := aToHouse.GetPointer;
  fGroup := nil;
end;

destructor TKMTaskFeeder.Destroy;
begin
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

function TKMTaskFeeder.WalkShouldAbandon: Boolean;
begin
  Result := (not fToHouse.IsValid) and (fPhase < 3);
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
            //no need to go to house if he has enough food alredy
            IF TKMUnitFeeder(fUnit).NeededFood = 0 then
            begin
              fPhase := 3;
              SetActionLockedStay(0, uaWalk);
            end else
              SetActionWalkToSpot(fToHouse.GetClosestEntrance(Position).DirFaceLoc);
          end;
      1: SetActionGoIn(uaWalk, gdGoInside, fToHouse);
      //take needed food
      2:  begin
            neededFood := TKMUnitFeeder(fUnit).NeededFood;
            SetActionLockedStay(neededFood, uaWalk);
            If neededFood = 0 then
              Result := trTaskDone
            else
            If neededFood > 0 then
            begin
              for WT in WARES_HOUSE_FOOD do
              begin
                houseFood := fToHouse.CheckWareIn(WT);
                If houseFood > 0 then
                begin
                  wareConsumed := Min(houseFood, neededFood);
                  fToHouse.WareTakeFromOut(WT, wareConsumed, true);
                  neededFood := neededFood - wareConsumed;
                   TKMUnitFeeder(fUnit).CollectFood(wareConsumed);
                  gHands[Owner].Stats.WareConsumed(WT, wareConsumed);
                end;
                If neededFood = 0 then
                  Break;
              end;

            end;

          end;
      3:  SetActionGoIn(uaWalk, gdGoOutside, fToHouse);
      //go to group
      4:  begin
            fGroup := TKMUnitGroup(TKMUnitFeeder(fUnit).FirstGroup);
            If fGroup = nil then
            begin
              Result := trTaskDone;
              Exit;
            end;
            SetActionWalkToUnit(fGroup.FlagBearer, 1.42, uaWalkTool);
          end;

      //now feed it
      5:  begin
            SetActionLockedStay(5, uaWalk);
            TKMUnitFeeder(fUnit).FeedGroup(fGroup);

            //try to feed another one
            fGroup := TKMUnitGroup(TKMUnitFeeder(fUnit).FirstGroup);
            If fGroup <> nil then
              fPhase := 3;
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
