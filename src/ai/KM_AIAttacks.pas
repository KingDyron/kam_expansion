unit KM_AIAttacks;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_CommonClasses, KM_Points, KM_AITypes;



const
  //KaM uses 0 for repeating attack in TSK (disused and replaced with later by Remake), 1 for once and 2 for repeating in TPR
  RemakeAttackType: array [0..2] of TKMAIAttackType = (aatRepeating, aatOnce, aatRepeating);
  KaMAttackType: array [TKMAIAttackType] of Byte = (1, 0);

type
  //Records must be packed so they are stored identically in MP saves (? padding bytes are unknown values)
  TKMAIAttack = packed record
    UID: Integer; //Attack UID, used to remove attack from script
    AttackType: TKMAIAttackType; //Once or repeating
    HasOccured: Boolean; //Has this attack happened already?
    Delay: Cardinal; //The attack will not occur before this time has passed
    TotalMen: Integer; //Number of idle (i.e. back line) warriors required in the AI army before the attack will launch
    GroupAmounts: TKMGroupTypeArray; //How many squads of each group type will be taken
    RandomGroups: Boolean; //Used instead of GroupAmounts, chooses groups randomly taking at most TotalMen warriors
    Target: TKMAIAttackTarget;
    Range: Integer; //Will only occur when target is within this tile range (not properly tested yet)
    CustomPosition: TKMPoint; //Used when Target = attCustomPosition
  end;


  TKMAIAttacks = class
  private
    fCount: Integer;
    fAttacks: array of TKMAIAttack;
    function GetAttack(aIndex: Integer): TKMAIAttack;
    procedure SetAttack(aIndex: Integer; const aValue: TKMAIAttack);
  public
    property Count: Integer read fCount;
    property Items[aIndex: Integer]: TKMAIAttack read GetAttack write SetAttack; default;

    function AddAttack(aAttack: TKMAIAttack): Integer; overload;
    function AddAttack(aAttackType: TKMAIAttackType; aDelay: Cardinal; aTotalMen: Integer; const aGroupAmounts: TKMGroupTypeArray;
                       aRandomGroups: Boolean; aTarget: TKMAIAttackTarget; aRange: Integer; aCustomPosition: TKMPoint): Integer; overload;
    function AddAttack(aAttackType: TKMAIAttackType; aDelay: Cardinal; aTotalMen: Integer;
                       aMeleeGroupCount, aAntiHorseGroupCount, aRangedGroupCount, aMountedGroupCount: Word;
                       aRandomGroups: Boolean; aTarget: TKMAIAttackTarget; aRange: Integer; aCustomPosition: TKMPoint): Integer; overload;
    function Remove(aAttackUID: Integer): Boolean;
    procedure Delete(aIndex: Integer);
    function CanOccur(aIndex: Integer; const aMenAvailable: TKMGroupTypeArray; const aGroupsAvailable: TKMGroupTypeArray; aTick: Cardinal): Boolean;
    procedure HasOccured(aIndex: Integer);
    procedure Clear;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  Math, KM_Game, KM_GameUIDTracker;


{ TAIAttacks }
function TKMAIAttacks.CanOccur(aIndex: Integer; const aMenAvailable: TKMGroupTypeArray; const aGroupsAvailable: TKMGroupTypeArray; aTick: Cardinal): Boolean;
var
  GT: TKMGroupType;
  totalMenAvailable: Word;
begin
  totalMenAvailable := aMenAvailable[gtAny];
  //Must have enough men available out of the types of groups that will attack
  for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
    if fAttacks[aIndex].RandomGroups or (fAttacks[aIndex].GroupAmounts[GT] > 0) then
      Inc(totalMenAvailable, aMenAvailable[GT]);

  Result := ((fAttacks[aIndex].AttackType = aatRepeating) or not fAttacks[aIndex].HasOccured)
            and (aTick >= fAttacks[aIndex].Delay)
            and (totalMenAvailable >= fAttacks[aIndex].TotalMen)
            and (totalMenAvailable > 0);

  //Must have enough groups of each type
  if not fAttacks[aIndex].RandomGroups then
    for GT := GROUP_TYPE_MIN to GROUP_TYPE_MAX do
      Result := Result and (aGroupsAvailable[GT] >= fAttacks[aIndex].GroupAmounts[GT]);

  //todo: Add support for the AI attack feature Range
end;


procedure TKMAIAttacks.HasOccured(aIndex: Integer);
begin
  fAttacks[aIndex].HasOccured := True;
end;


procedure TKMAIAttacks.Clear;
begin
  SetLength(fAttacks, 0);
  fCount := 0;
end;


function TKMAIAttacks.AddAttack(aAttack: TKMAIAttack): Integer;
begin
  if fCount >= Length(fAttacks) then
    SetLength(fAttacks, fCount + 16);

  aAttack.UID := gUIDTracker.GetNewUID;
  fAttacks[fCount] := aAttack;
  Inc(fCount);
  Result := aAttack.UID;
end;


function TKMAIAttacks.AddAttack(aAttackType: TKMAIAttackType; aDelay: Cardinal; aTotalMen: Integer; const aGroupAmounts: TKMGroupTypeArray;
                                aRandomGroups: Boolean; aTarget: TKMAIAttackTarget; aRange: Integer; aCustomPosition: TKMPoint): Integer;
var
  attack: TKMAIAttack;
begin
  attack.AttackType     := aAttackType;
  attack.HasOccured     := False;
  attack.Delay          := aDelay;
  attack.TotalMen       := aTotalMen;
  attack.GroupAmounts   := aGroupAmounts;
  attack.RandomGroups   := aRandomGroups;
  attack.Target         := aTarget;
  attack.Range          := aRange;
  attack.CustomPosition := aCustomPosition;
  Result := AddAttack(attack);
end;


function TKMAIAttacks.AddAttack(aAttackType: TKMAIAttackType; aDelay: Cardinal; aTotalMen: Integer;
                                aMeleeGroupCount, aAntiHorseGroupCount, aRangedGroupCount, aMountedGroupCount: Word;
                                aRandomGroups: Boolean; aTarget: TKMAIAttackTarget; aRange: Integer; aCustomPosition: TKMPoint): Integer;
var
  groupAmounts: TKMGroupTypeArray;
begin
  groupAmounts[gtMelee]     := aMeleeGroupCount;
  groupAmounts[gtAntiHorse] := aAntiHorseGroupCount;
  groupAmounts[gtRanged]    := aRangedGroupCount;
  groupAmounts[gtMounted]   := aMountedGroupCount;
  Result := AddAttack(aAttackType, aDelay, aTotalMen, groupAmounts, aRandomGroups, aTarget, aRange, aCustomPosition);
end;


//Remove AIAttack by its ID
//Result - True, if delete was succesfull, False, if no AIAttack was found
function TKMAIAttacks.Remove(aAttackUID: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to fCount - 1 do
    if fAttacks[I].UID = aAttackUID then
    begin
      Delete(I);
      Result := True;
      Exit;
    end;
end;


procedure TKMAIAttacks.Delete(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, Count - 1));

  if (aIndex <> Count - 1) then
    Move(fAttacks[aIndex + 1], fAttacks[aIndex], (Count - 1 - aIndex) * SizeOf(fAttacks[0]));

  Dec(fCount);
end;


function TKMAIAttacks.GetAttack(aIndex: Integer): TKMAIAttack;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fAttacks[aIndex];
end;


procedure TKMAIAttacks.SetAttack(aIndex: Integer; const aValue: TKMAIAttack);
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  fAttacks[aIndex] := aValue;
end;


procedure TKMAIAttacks.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('AIAttacks');
  SaveStream.Write(fCount);
  for I := 0 to fCount - 1 do
    SaveStream.Write(fAttacks[I], SizeOf(fAttacks[I]));
end;


procedure TKMAIAttacks.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.CheckMarker('AIAttacks');
  LoadStream.Read(fCount);
  SetLength(fAttacks, fCount);
  for I := 0 to fCount - 1 do
    LoadStream.Read(fAttacks[I], SizeOf(fAttacks[I]));
end;


end.

