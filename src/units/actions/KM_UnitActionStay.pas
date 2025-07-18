unit KM_UnitActionStay;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KromUtils, KM_CommonClasses, KM_Units, SysUtils, Math, KM_Points;

type
  {Stay in place for set time}
  TKMUnitActionStay = class(TKMUnitAction)
  private
    StayStill:boolean;
    TimeToStay:integer;
    StillFrame:byte;
    procedure MakeSound(Cycle, Step: Byte);
  public
    constructor Create(aUnit: TKMUnit; aTimeToStay: Integer; aActionType: TKMUnitActionType; aStayStill: Boolean;
                       aStillFrame: Byte; aLocked: Boolean);
    constructor Load(LoadStream: TKMemoryStream); override;
    function ActName: TKMUnitActionName; override;
    function CanBeInterrupted(aForced: Boolean = True): Boolean; override;
    function HasNoAnim: Boolean; override;
    function GetExplanation: UnicodeString; override;
    function Execute: TKMActionResult; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    function ObjToStringShort(const aSeparator: String = ' '): String; override;
  end;


implementation
uses
  KM_HandsCollection, KM_Sound, KM_ResSound, KM_Resource, KM_ResUnits, KM_UnitWarrior;


{ TUnitActionStay }
constructor TKMUnitActionStay.Create(aUnit: TKMUnit; aTimeToStay: Integer; aActionType: TKMUnitActionType; aStayStill: Boolean;
                                     aStillFrame: Byte; aLocked: Boolean);
begin
  if aUnit is TKMUnitWarrior then
    if aActionType = uaWalk then
      if aStayStill and gRes.Units[aUnit.UnitType].SupportsAction(uaStay) and (gRes.Units[aUnit.UnitType].UnitAnim[uaStay, dirN].Count > 0) then
      begin
        aActionType := uaStay;
        aStayStill := false;
        aTimeToStay := gRes.Units[aUnit.UnitType].UnitAnim[uaStay, dirN].Count;
        aUnit.AnimStep := 0;
      end;

  inherited Create(aUnit, aActionType, aLocked);

  StayStill   := aStayStill;
  TimeToStay  := aTimeToStay;
  StillFrame  := aStillFrame;
end;


constructor TKMUnitActionStay.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('UnitActionStay');
  LoadStream.Read(StayStill);
  LoadStream.Read(TimeToStay);
  LoadStream.Read(StillFrame);
end;


function TKMUnitActionStay.ActName: TKMUnitActionName;
begin
  Result := uanStay;
end;


function TKMUnitActionStay.GetExplanation: UnicodeString;
begin
  Result := 'Staying';
end;


procedure TKMUnitActionStay.MakeSound(Cycle, Step: Byte);
begin
  if SKIP_SOUND then Exit;

  //Do not play sounds if unit is invisible to gMySpectator
  if gMySpectator.FogOfWar.CheckTileRevelation(fUnit.Position.X, fUnit.Position.Y) < 255 then exit;

  //Various UnitTypes and ActionTypes produce all the sounds
  case fUnit.UnitType of
    utBuilder:      case ActionType of
                      uaWork:  if Step = 3 then gSoundPlayer.Play(sfxHousebuild,fUnit.PositionF);
                      uaWork1: if Step = 0 then gSoundPlayer.Play(sfxDig,fUnit.PositionF);
                      uaWork2: if Step = 8 then gSoundPlayer.Play(sfxPave,fUnit.PositionF);
                    end;
    utHouseBuilder: case ActionType of
                      uaWork:  if Step = 7 then gSoundPlayer.Play(sfxHousebuild,fUnit.PositionF);
                    end;
    utFarmer:      case ActionType of
                      uaWork:  if Step = 8 then gSoundPlayer.Play(sfxCornCut,fUnit.PositionF);
                      uaWork1: if Step = 0 then gSoundPlayer.Play(sfxCornSow,fUnit.PositionF,True,0.6);
                    end;
    utClaypicker:   case ActionType of
                      uaWork : if Step = 4 then gSoundPlayer.Play(sfxDig,fUnit.PositionF,True,1.4);
                      uaWork1 : if Step in [3, 7] then gSoundPlayer.Play(sfxMineStone,fUnit.PositionF,True,1.4);
                    end;
    utStonemason: case  ActionType of
                      uaWork : if Step = 3 then gSoundPlayer.Play(sfxMinestone,fUnit.PositionF,True,1.4);
                  end;
    utWoodCutter:  case ActionType of
                      uaWork: if (fUnit.AnimStep mod Cycle = 3) and (fUnit.Direction <> dirN) then gSoundPlayer.Play(sfxChopTree, fUnit.PositionF,True)
                      else     if (fUnit.AnimStep mod Cycle = 0) and (fUnit.Direction =  dirN) then gSoundPlayer.Play(sfxWoodcutterDig, fUnit.PositionF,True);
                    end;
  end;
end;


function TKMUnitActionStay.Execute: TKMActionResult;
var
  cycle, step: Byte;
begin
  if not StayStill then
  begin

    cycle := gRes.Units[fUnit.UnitType].UnitAnim[ActionType, fUnit.Direction].Count;
    if Cycle = 0 then
    begin
      StepDone := true;
      TimeToStay := 0;
    end
    else
    begin

      step  := fUnit.AnimStep mod cycle;

      StepDone := fUnit.AnimStep mod cycle = 0;

      if TimeToStay >= 1 then MakeSound(cycle, step);

      Inc(fUnit.AnimStep);
    end;
  end
  else
  begin
    fUnit.AnimStep := StillFrame;
    StepDone := True;
  end;

  Dec(TimeToStay);
  if TimeToStay <= 0 then
    Result := arActDone
  else
    Result := arActContinues;
end;


procedure TKMUnitActionStay.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('UnitActionStay');
  SaveStream.Write(StayStill);
  SaveStream.Write(TimeToStay);
  SaveStream.Write(StillFrame);
end;


function TKMUnitActionStay.CanBeInterrupted(aForced: Boolean = True): Boolean;
begin
  Result := not Locked; //Initial pause before leaving barracks is locked
end;

function TKMUnitActionStay.HasNoAnim: Boolean;
begin
  Result := TimeToStay = 0;
end;


function TKMUnitActionStay.ObjToStringShort(const aSeparator: String): String;
begin
  Result := inherited + Format('%s[StayStill = %s%sTimeToStay = %d%sStillFrame = %d]', [
                               aSeparator,
                               BoolToStr(StayStill, True), aSeparator,
                               TimeToStay, aSeparator,
                               StillFrame]);
end;


end.
