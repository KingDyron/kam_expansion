unit KM_AIGoals;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_Defaults, KM_Points, KM_MapTypes;


type
  TKMGoal = packed record
    GoalType: TKMGoalType; //Victory, survive, neither
    GoalCondition: TKMGoalCondition; //Buildings, troops, time passing
    GoalStatus: TKMGoalStatus; //Must this condition be True or False (same as alive or dead) for victory/surival to occur?
    GoalTime: Cardinal; //Only used with gaTime. Amount of time (in game ticks) that must pass before this goal is complete
    MessageToShow: Integer; //Message to be shown when the goal is completed
    MessageHasShown: Boolean; //Whether we have shown this message yet
    HandIndex: TKMHandID; //Player whose buildings or troops must be destroyed
    Disabled: Boolean;
    BuldingsType : Byte;
    Position : TKMPoint;
    Radius : Byte;
    class operator Equal(A, B : TKMGoal) : Boolean;
  end;
  TKMGoalArray = array of TKMGoal;
  //Because the goal system is hard to understand, here are some examples:
  {Destroy troops of player 2 in order to win
  Script command: !ADD_GOAL 4 1 0 2
  GoalType=gltVictory
  GoalCondition=gcTroops
  GoalStatus=gsFalse         //Troops must be dead, non-existant. i.e. the condition that player 2 has troops must be FALSE.
  Player=play_2
  }

  {Save (protect) troops of player 1 or else you will lose the game
  Script command: !ADD_LOST_GOAL 4 0 0 1
  GoalType=gltSurvive
  GoalCondition=gcTroops
  GoalStatus=gsTrue         //Troops must be alive. i.e. the condition that player 1 has troops must be TRUE otherwise you lose.
  Player=play_1
  }

  {Display message 500 after 10 minutes (no goal, just message)
  Script command: !ADD_GOAL 2 0 500 600
  GoalType=gltNone
  GoalCondition=gcTime
  GoalStatus=gsTrue      //Time must have passed
  GoalTime=600
  MessageToShow=500
  }

  {Display message 510 upon buildings of player 4 being destroyed
  Script command: !ADD_GOAL 3 1 510 4 (in this case the script command would also require the buildings to be destroyed for a victory condition, as mentioned bellow)
  GoalType=gltNone            //If this was set to victory or survive not only would the message be displayed, but it would also be a condition for winning/losing
  GoalCondition=gcBuildings
  GoalStatus=gsFalse         //Buildings must be destroyed
  MessageToShow=500
  }


type
  TKMGoals = class
  private
    fCount: Integer;
    fGoals: TKMGoalArray;
    TimeVGoalIndex, TimeSGoalIndex : Word;
    function GetGoal(aIndex: Integer): TKMGoal;
    procedure SetGoal(aIndex: Integer; const Value: TKMGoal);
    function GetArray : TKMGoalArray;
  public
    procedure Clear;
    property Count: Integer read fCount;
    property Item[aIndex: Integer]: TKMGoal read GetGoal write SetGoal; default;
    property GoalsArray : TKMGoalArray read GetArray;
    procedure AddGoal(aType: TKMGoalType; aCondition: TKMGoalCondition; aStatus: TKMGoalStatus; aTime: Cardinal;
                      aMessageToShow: Integer; aHandIndex: TKMHandID; aBuildingType : Byte; ShowMessage : Boolean); overload; // Deprecated
    procedure AddGoal(aType: TKMGoalType; aCondition: TKMGoalCondition; aHandIndex: TKMHandID; ShowMessage : Boolean); overload;
    procedure AddGoal(const aGoal: TKMGoal); overload;
    procedure AddGoals(const aGoals: TKMGoalArray);

    procedure Delete(aIndex: Integer);
    procedure RemoveReference(aHandIndex: TKMHandID);
    procedure UpdateGoalsForHand(aHandIndex: TKMHandID; aEnable: Boolean);
    procedure SetMessageHasShown(aIndex: Integer);
    procedure AddDefaultGoals(aBuildings: Boolean; aOurPlayerIndex: TKMHandID; const aEnemyIndexes: array of TKMHandID);
    procedure SetLastGoalPosition(X, Y, aRadius : Integer);
    procedure DisableGoal(aIndex : Integer; ShowMessage : Boolean = true);
    procedure MessageShown(aIndex : Integer; ShowMessage : Boolean = true);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure ExportMessages(const aPath: UnicodeString);
  end;

implementation
uses
  Classes, SysUtils, Math;

{ TKMGoals }
function TKMGoals.GetGoal(aIndex: Integer): TKMGoal;
begin
  Result := fGoals[aIndex];
end;


procedure TKMGoals.SetGoal(aIndex: Integer; const Value: TKMGoal);
begin
  fGoals[aIndex] := Value;
end;

function TKMGoals.GetArray: TKMGoalArray;
begin
  Result := fGoals;
  SetLength(Result, fCount);
end;


procedure TKMGoals.AddGoal(aType: TKMGoalType; aCondition: TKMGoalCondition; aHandIndex: TKMHandID; ShowMessage : Boolean);
var
  status: TKMGoalStatus;
begin
  if aType = gltVictory then
    status := gsFalse
  else
    status := gsTrue;

  AddGoal(aType, aCondition, status, 0, 0, aHandIndex, 0, ShowMessage);
end;


procedure TKMGoals.AddGoal(aType: TKMGoalType; aCondition: TKMGoalCondition; aStatus: TKMGoalStatus; aTime: Cardinal;
                           aMessageToShow: Integer; aHandIndex: TKMHandID; aBuildingType : Byte; ShowMessage : Boolean);
begin
  if (aCondition = gcTime) then
    if (TimeSGoalIndex > 0) or (TimeVGoalIndex > 0) then
      Exit;

  SetLength(fGoals, fCount + 1);

  fGoals[fCount].GoalType := aType;
  fGoals[fCount].GoalCondition := aCondition;
  fGoals[fCount].GoalStatus := aStatus;
  fGoals[fCount].GoalTime := aTime;
  fGoals[fCount].MessageToShow := aMessageToShow;
  fGoals[fCount].HandIndex := aHandIndex;
  fGoals[fCount].Disabled := False;
  fGoals[fCount].BuldingsType := aBuildingType;
  fGoals[fCount].MessageHasShown := not ShowMessage;

  if (aCondition = gcTime) and (aType = gltSurvive) then
    TimeSGoalIndex := fCount + 1
  else
  if (aCondition = gcTime) and (aType in [gltNone, gltVictory]) then
    TimeVGoalIndex :=  fCount + 1;

  Inc(fCount);
end;


procedure TKMGoals.AddGoal(const aGoal: TKMGoal);
begin
  if fCount >= Length(fGoals) then
    SetLength(fGoals, fCount + 16);

  fGoals[fCount] := aGoal;
  Inc(fCount);
end;

procedure TKMGoals.AddGoals(const aGoals: TKMGoalArray);
var I, K : integer;
  doSkip : Boolean;
begin
  for I := 0 to High(aGoals) do
  begin
    doSkip := false;
    //add only unique goals
    for K := 0 to Count - 1 do
    begin
      if aGoals[I] = fGoals[K] then
      begin
        doSkip := true;
        break;
      end;
    end;
    if not doSkip then
      AddGoal(aGoals[I]);
  end;
end;


procedure TKMGoals.Clear;
begin
  if Self = nil then Exit;
             
  fCount := 0;
  SetLength(fGoals, 0);
end;


procedure TKMGoals.Delete(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, Count - 1));

  if (aIndex <> Count - 1) then
    Move(fGoals[aIndex + 1], fGoals[aIndex], (fCount - 1 - aIndex) * SizeOf(TKMGoal));
  Dec(fCount);
  //SetLength(fGoals, fCount);
end;


procedure TKMGoals.UpdateGoalsForHand(aHandIndex: TKMHandID; aEnable: Boolean);
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    if fGoals[I].HandIndex = aHandIndex then
      fGoals[I].Disabled := not aEnable;
end;


// We don't want anyones goal to use deleted player
// Used when we delete certain player from MapEd
procedure TKMGoals.RemoveReference(aHandIndex: TKMHandID);
var
  I: Integer;
begin
  for I := fCount - 1 downto 0 do
    if fGoals[I].HandIndex > aHandIndex then
      fGoals[I].HandIndex := pred(fGoals[I].HandIndex)
    else if fGoals[I].HandIndex = aHandIndex then
      Delete(I);
end;


procedure TKMGoals.SetMessageHasShown(aIndex: Integer);
begin
  fGoals[aIndex].MessageHasShown := True;
end;


procedure TKMGoals.AddDefaultGoals(aBuildings: Boolean; aOurPlayerIndex: TKMHandID; const aEnemyIndexes: array of TKMHandID);
var
  I: Integer;
  gc: TKMGoalCondition;
begin
  if aBuildings then
    gc := gcBuildings
  else
    gc := gcTroops;

  // Default Defeat condition is to lose army/town
  AddGoal(gltSurvive, gc, aOurPlayerIndex, true);

  // Default Victory conditions is to kill armies / destroy towns of all other players
  for I := 0 to Length(aEnemyIndexes) - 1 do
    AddGoal(gltVictory, gc, aEnemyIndexes[I], true);
end;

procedure TKMGoals.SetLastGoalPosition(X: Integer; Y: Integer; aRadius: Integer);
begin
  with fGoals[high(fGoals)] do
  begin
    Position := KMPoint(X, Y);
    Radius := aRadius;
  end;
end;

procedure TKMGoals.DisableGoal(aIndex : Integer; ShowMessage : Boolean = true);
begin
  Assert(InRange(aIndex, 0, Count - 1));
  fGoals[aIndex].Disabled := true;
end;

procedure TKMGoals.MessageShown(aIndex: Integer; ShowMessage: Boolean = True);
begin
  Assert(InRange(aIndex, 0, Count - 1));
  fGoals[aIndex].MessageHasShown := true;
end;

procedure TKMGoals.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('Goals');
  SaveStream.Write(TimeVGoalIndex);
  SaveStream.Write(TimeSGoalIndex);
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    SaveStream.Write(fGoals[I], SizeOf(fGoals[I]));
end;


procedure TKMGoals.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.CheckMarker('Goals');
  LoadStream.Read(TimeVGoalIndex);
  LoadStream.Read(TimeSGoalIndex);
  LoadStream.Read(fCount);
  SetLength(fGoals, fCount);
  for I := 0 to fCount - 1 do
    LoadStream.Read(fGoals[I], SizeOf(fGoals[I]));

end;


//In-house method to convert KaM 'show_message' goals into EVT scripts
procedure TKMGoals.ExportMessages(const aPath: UnicodeString);
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;

  for I := 0 to fCount - 1 do
  if Item[I].MessageToShow > 0 then
    SL.Add('TIME -1 ' +
           IntToStr(Item[I].GoalTime) +
           ' SHOW_MESSAGE 0 ' +
           IntToStr(Item[I].MessageToShow)); //-529 for TSK, -549 for TPR

  if SL.Count > 0 then
    SL.SaveToFile(aPath);
  SL.Free;
end;

class operator TKMGoal.Equal(A: TKMGoal; B: TKMGoal): Boolean;
begin
  Result :=     (A.GoalType         = B.GoalType)
            and (A.GoalCondition    = B.GoalCondition)
            and (A.GoalStatus       = B.GoalStatus)
            and (A.GoalTime         = B.GoalTime)
            and (A.MessageToShow    = B.MessageToShow)
            and (A.MessageHasShown  = B.MessageHasShown)
            and (A.HandIndex        = B.HandIndex)
            and (A.BuldingsType     = B.BuldingsType)
            and (A.Position         = B.Position)
            and (A.Radius           = B.Radius);

end;

end.
