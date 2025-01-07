unit KM_UnitVisual;
{$I KaM_Remake.inc}
interface
uses
  KM_ResTypes, KM_Points, KM_Defaults;

type
  TKMUnitVisualState = record
  public
    PositionF: TKMPointF; // Precise unit position
    Dir: TKMDirection;
    SlideX, SlideY: Single;
    Action: TKMUnitActionType;
    IsActGoInOutStarted: Boolean;
    InHouseType: TKMHouseType;
    AnimStep: Integer;
    AnimFraction: Single;

    procedure SetFromUnit(aUnit: TObject);
  end;

  // Purely visual thing. Split from TKMUnit to aviod mixup of game-logic and render Positions
  TKMUnitVisual = class
  private
    fUnit: TObject;
    fCurr: TKMUnitVisualState;
    fPrev: TKMUnitVisualState;
    fPrevPrev: TKMUnitVisualState;
  public
    constructor Create(aUnit: TObject);

    function GetLerp(aLag: Single): TKMUnitVisualState;
    procedure UpdateState;
  end;


implementation
uses
  Math, SysUtils,
  KromUtils,
  KM_Units, KM_UnitActionGoInOut,
  KM_Resource, KM_ResUnits, KM_ResHouses;


{ TKMUnitVisualState }
procedure TKMUnitVisualState.SetFromUnit(aUnit: TObject);
var
  U: TKMUnit;
  slide: TKMPointF;
begin
  U := TKMUnit(aUnit);
  PositionF := U.PositionF;
  Dir := U.Direction;
  slide := U.GetSlides;
  SlideX := slide.X;
  SlideY := slide.Y;
  AnimStep := U.AnimStep;
  AnimFraction := 0.0;

  IsActGoInOutStarted := False;

  if U.Action <> nil then
  begin
    Action := U.Action.ActionType;
    if (U.Action is TKMUnitActionGoInOut)
      and TKMUnitActionGoInOut(U.Action).IsStarted then
      IsActGoInOutStarted := True;
  end
  else
    Action := uaUnknown;

  if U.InHouse <> nil then
    InHouseType := U.InHouse.HouseType
  else
    InHouseType := htNone;
end;


{ TKMUnitVisual }
constructor TKMUnitVisual.Create(aUnit: TObject);
begin
  inherited Create;

  fUnit := TKMUnit(aUnit);
  fPrev.SetFromUnit(fUnit);
  fCurr.SetFromUnit(fUnit);
end;


function TKMUnitVisual.GetLerp(aLag: Single): TKMUnitVisualState;
var
  animCount: Integer;
  prevSlideX, prevSlideY: Single;
begin
  prevSlideX := fPrev.SlideX;
  prevSlideY := fPrev.SlideY;

  // Special case for a unit who just started exiting the house
  // In that case fPrev slide was not calculated with door slide consideration
  // and thus fPrev.Slide would be 0 in the most cases (or in all cases)
  // That will make unit 'jump' from fPrev.PositionF to fCurr.PositionF + fCurr.Slide, which looks very bad
  if not fPrev.IsActGoInOutStarted
    and fCurr.IsActGoInOutStarted
    and (fPrev.InHouseType <> htNone) then
  begin
    // Just add doorway offset to the fPrev.Slide then
    prevSlideX := prevSlideX + gRes.Houses[fPrev.InHouseType].GetDoorwayOffset(axX);
    prevSlideY := prevSlideY + gRes.Houses[fPrev.InHouseType].GetDoorwayOffset(axY);
  end;
  if TKMUnit(fUnit).InShip <> nil then
  begin
    Result.PositionF := KMLerp(fCurr.PositionF, fPrev.PositionF, 0);
    Result.SlideX := KromUtils.Lerp(fCurr.SlideX, prevSlideX, 0);
    Result.SlideY := KromUtils.Lerp(fCurr.SlideY, prevSlideY, 0);
  end else
  begin
    Result.PositionF := KMLerp(fCurr.PositionF, fPrev.PositionF, aLag);
    Result.SlideX := KromUtils.Lerp(fCurr.SlideX, prevSlideX, aLag);
    Result.SlideY := KromUtils.Lerp(fCurr.SlideY, prevSlideY, aLag);
  end;
  //If there's no lag, use the current state
  if aLag = 0.0 then
  begin
    Result.Dir := fCurr.Dir;
    Result.Action := fCurr.Action;
    Result.AnimStep := fCurr.AnimStep;
    Result.AnimFraction := 0.0;
  end
  else
  begin
    //Are we moving?
    if fCurr.PositionF <> fPrev.PositionF then
    begin
      //Always interpolate the animation if the unit is moving
      Result.AnimFraction := 1.0 - aLag;

      //If we were still and just started moving
      if fPrevPrev.PositionF = fPrev.PositionF then
      begin
        //Since the unit starts moving without warning we need to backwards interpolate
        Result.Dir := fCurr.Dir;
        Result.Action := fCurr.Action;

        //Find the previous anim step
        if fCurr.AnimStep = 0 then
        begin
          animCount := gRes.Units[TKMUnit(fUnit).UnitType].UnitAnim[Result.Action, Result.Dir].Count;
          Result.AnimStep := animCount - 1;
        end
        else
          Result.AnimStep := fCurr.AnimStep - 1;
      end
      else
      begin
        //Don't start a new action or change direction until the last one is 100% finished
        Result.Dir := fPrev.Dir;
        Result.Action := fPrev.Action;
        Result.AnimStep := fPrev.AnimStep;
      end;
    end
    else
    begin
      //Not moving. Don't start a new action or change direction until the last one is 100% finished
      Result.Dir := fPrev.Dir;
      Result.Action := fPrev.Action;
      Result.AnimStep := fPrev.AnimStep;
      //If action/dir/step is consistent we can interpolate the animation
      if (fCurr.Action = fPrev.Action) and (fCurr.Dir = fPrev.Dir) and (fCurr.AnimStep - fPrev.AnimStep = 1) then
        Result.AnimFraction := 1.0 - aLag
      else
        Result.AnimFraction := 0.0;
    end;
  end;
end;


procedure TKMUnitVisual.UpdateState;
begin
  fPrevPrev := fPrev;
  fPrev := fCurr;
  fCurr.SetFromUnit(fUnit);
end;

end.
