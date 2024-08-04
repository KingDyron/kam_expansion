unit KM_UnitTaskGoEat;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses, KM_HouseInn, SysUtils, KM_ResTypes;


type
  //Go to eat
  TKMTaskGoEat = class(TKMUnitTask)
  private
    fFeedCnt: Byte;
    fInn: TKMHouseInn; //Inn in which we are going to eat
    fPlace: ShortInt; //Units place in Inn
    fFoodEaten : TKMWareTypeSet;
  public
    constructor Create(aInn: TKMHouseInn; aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function Eating: Boolean;
    function Execute: TKMTaskResult; override;

    function CouldBeCancelled: Boolean; override;
    function WalkShouldAbandon: Boolean; override;

    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_Entity,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_CommonUtils;


{ TTaskGoEat }
constructor TKMTaskGoEat.Create(aInn: TKMHouseInn; aUnit: TKMUnit);
begin
  inherited Create(aUnit);

  fType := uttGoEat;
  fInn      := TKMHouseInn(aInn.GetPointer);
  fPlace    := -1;
  fFeedCnt  := 0;
  fFoodEaten := [];
end;


constructor TKMTaskGoEat.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('TaskGoEat');
  LoadStream.Read(fInn, 4);
  LoadStream.Read(fPlace);
  LoadStream.Read(fFeedCnt);
  LoadStream.Read(fFoodEaten, SizeOf(fFoodEaten));
end;


procedure TKMTaskGoEat.SyncLoad;
begin
  inherited;

  fInn := TKMHouseInn(gHands.GetHouseByUID(Integer(fInn)));
end;


function TKMTaskGoEat.WalkShouldAbandon: Boolean;
begin
  Result := fInn.IsDestroyed;
end;


destructor TKMTaskGoEat.Destroy;
begin
  //May happen when we die while desperatley trying to get some food
  if (fInn <> nil) and Eating then
    fInn.EatersGoesOut(fPlace);

  gHands.CleanUpHousePointer(TKMHouse(fInn));

  inherited;
end;


function TKMTaskGoEat.Eating: Boolean;
begin
  Result := fPlace <> -1;
end;


function TKMTaskGoEat.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                    = 1; //Allow cancel task only at walking phases
end;


function TKMTaskGoEat.Execute: TKMTaskResult;
  function RandomWare : TKMWareType;
  var WT : TKMWareType;
    I : Integer;//number of tries
  begin
    Result := wtNone;
    I := 1;
    Repeat
      case KamRandom(6, 'TKMTaskGoEat.Execute') of
        0 : WT := wtBread;
        1 : WT := wtWine;
        2 : WT := wtSausage;
        3 : WT := wtFish;
        4 : WT := wtApple;
        5 : WT := wtVegetables;
        else WT := wtNone;
      end;
      if not (WT in fFoodEaten) then
      begin
        Result := WT;
        fFoodEaten := fFoodEaten + [WT];
      end;
      Inc(I);
    Until (Result <> wtNone) or (I > 20);
  end;

  function FeedUnit(aWare : TKMWareType) : Boolean;
  var restoreCond : Single;
  begin
    Result := false;
    if aWare = wtNone then
      Exit;
    if (fFeedCnt >= 2) then  Exit; //Limit max number of times to eat in the Inn
    if (fUnit.Condition >= UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL) then Exit;

    case aWare of
      wtWine: restoreCond := UNIT_MAX_CONDITION * WINE_RESTORE;
      wtBread: restoreCond := UNIT_MAX_CONDITION * BREAD_RESTORE;
      wtSausage: restoreCond := UNIT_MAX_CONDITION * SAUSAGE_RESTORE;
      wtFish: restoreCond := UNIT_MAX_CONDITION * FISH_RESTORE;
      wtApple: restoreCond := UNIT_MAX_CONDITION * APPLE_RESTORE;
      wtVegetables: restoreCond := UNIT_MAX_CONDITION * VEGE_RESTORE;
      else restoreCond := 0;
    end;
    if fInn.CheckWareIn(aWare) = 0 then Exit;

    fInn.WareTakeFromIn(aWare);
    gHands[fUnit.Owner].Stats.WareConsumed(aWare);
    fUnit.SetActionLockedStay(29*4, uaEat);
    fInn.UpdateEater(fPlace, aWare);

    fUnit.Feed(restoreCond);
    if (fUnit.Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL) then
      if gHands[fUnit.Owner].VirtualWareTake('vtDishes', 1) then
        fUnit.Feed(UNIT_MAX_CONDITION div 6);
    if (fUnit.Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL) then
      if gHands[fUnit.Owner].VirtualWareTake('vtAppleJuice', 1) then
        fUnit.Feed(UNIT_MAX_CONDITION div 5);
    Inc(fFeedCnt);
  end;
begin
  Result := trTaskContinues;

  if fInn.IsDestroyed then
    Exit(trTaskDone);

  with fUnit do
  case fPhase of
   0: begin
        Thought := thEat;
        if (Home <> nil) and not Home.IsDestroyed then Home.SetState(hstEmpty);
        if not Visible and (InHouse <> nil) and not InHouse.IsDestroyed then
          SetActionGoIn(uaWalk, gdGoOutside, InHouse) //Walk outside the house
        else
          SetActionLockedStay(0, uaWalk); //Skip this step
      end;
   1: SetActionWalkToSpot(fInn.PointBelowEntrance);
   2: SetActionGoIn(uaWalk, gdGoInside, fInn); //Enter Inn
   3: begin
        SetActionLockedStay(0, uaWalk);
        fPlace := fInn.EaterGetsRandomEmptySlot(UnitType);
        //If there's no free place in the Inn skip to the step where we go out hungry
        if fPlace = -1 then
        begin
          fPhase := 7;
          Exit;
        end;
      end;
   4..9: if not FeedUnit(RandomWare) then
        SetActionLockedStay(29*4, uaWalk)
      else
        SetActionLockedStay(1, uaWalk);

   10: begin
        //Stop showing hungry if we no longer are,
        //but if we are then walk out of the inn thinking hungry
        //so that the player will know that we haven't been fed
        if Condition < UNIT_MIN_CONDITION then
          Thought := thEat
        else
          Thought := thNone;
        SetActionGoIn(uaWalk, gdGoOutside, fInn); //Exit Inn
        fInn.EatersGoesOut(fPlace);
        fPlace := -1;
      end;
  else
    Result := trTaskDone;
  end;

  Inc(fPhase);
end;


procedure TKMTaskGoEat.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.PlaceMarker('TaskGoEat');
  SaveStream.Write(fInn.UID); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fPlace);
  SaveStream.Write(fFeedCnt);
  SaveStream.Write(fFoodEaten, SizeOf(fFoodEaten));
end;


end.
