unit KM_MapUtils;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_MapTypes;

  function GuessMPPathRel(const aName, aExt: string; aCRC: Cardinal): string;
  function GuessMissionPathRel(const aMissionFileRelSP, aMissionName: string; aMapFullCRC: Cardinal; aIsMultiplayer: Boolean): string;

  function DetermineMapKind(const aFolderName: UnicodeString; out aMapKind: TKMMapKind): Boolean;
  function GetMapKind(aIsMultiplayer: Boolean): TKMMapKind;

  function GetGoalDescription(aPlayer1, aPlayer2: TKMHandID; aGoalType: TKMGoalType; aGoalCondition: TKMGoalCondition;
                              aColPlayer1, aColPlayer2, aColTxt, aColBld: Cardinal; aBuildingType : Integer = -1): string;

implementation
uses
  SysUtils,
  KM_ResTexts, KM_Resource, KM_ResTypes, KM_ResHouses, KM_CommonUtils;


function GuessMPPathRel(const aName, aExt: string; aCRC: Cardinal): string;
var
  S: UnicodeString;
begin
  S := aName + '_' + IntToHex(aCRC, 8);
  Result := MAP_FOLDER_NAME[mkDL] + PathDelim + S + PathDelim + S + aExt;
  if not FileExists(ExeDir + Result) then
    Result := MAP_FOLDER_NAME[mkMP] + PathDelim + aName + PathDelim + aName + aExt;
end;


function GuessMissionPathRel(const aMissionFileRelSP, aMissionName: string; aMapFullCRC: Cardinal; aIsMultiplayer: Boolean): string;
begin
  if aIsMultiplayer then
    //In MP we can't store it since it will be MapsMP or MapsDL on different clients
    Result := GuessMPPathRel(aMissionName, '.dat', aMapFullCRC)
  else
    Result := aMissionFileRelSP; //In SP we store it
end;


//Try to determine TMapFolder for specified aFolderName
//Returns True when succeeded
function DetermineMapKind(const aFolderName: UnicodeString; out aMapKind: TKMMapKind): Boolean;
var
  MK: TKMMapKind;
begin
  Result := False;
  // no need to test mkUnknown
  for MK := Succ(Low(TKMMapKind)) to High(TKMMapKind) do
    if aFolderName = MAP_FOLDER_NAME[MK] then
    begin
      aMapKind := MK;
      Exit(True);
    end;
end;


function GetMapKind(aIsMultiplayer: Boolean): TKMMapKind;
begin
  if aIsMultiplayer then
    Result := mkMP
  else
    Result := mkSP;
end;


// Format mission goal description
function GetGoalDescription(aPlayer1, aPlayer2: TKMHandID; aGoalType: TKMGoalType; aGoalCondition: TKMGoalCondition;
                              aColPlayer1, aColPlayer2, aColTxt, aColBld: Cardinal; aBuildingType : Integer = -1): string;
type
  TKMGoalTypeDescWordKind = (dwkFirst, dwkSecond);
const
  GOAL_TYPE_DESC_TX: array[TKMGoalType, TKMGoalTypeDescWordKind] of Integer = (
    (TX_MAPED_GOALS_TYPE_NONE,            TX_MAPED_GOALS_TYPE_NONE),
    (TX_GOAL_CONDITION_DESC_WIN_WORD,     TX_GOAL_CONDITION_DESC_DESTROYED_WORD),
    (TX_GOAL_CONDITION_DESC_SURVIVE_WORD, TX_GOAL_CONDITION_DESC_SAVED_WORD)
  );

var
  I: Integer;
  goalCondStr, housesStr: string;
begin
  case aGoalCondition of
    gcBuildings:
      begin
        housesStr := '';
        for I := Low(GOAL_BUILDINGS_HOUSES) to High(GOAL_BUILDINGS_HOUSES) do
        begin
          if housesStr <> '' then
            housesStr := housesStr + ', ';

          housesStr := housesStr + gRes.Houses[GOAL_BUILDINGS_HOUSES[I]].HouseName;
        end;
        goalCondStr := Format(gResTexts[TX_GOAL_CONDITION_COND_DESC_BUILDINGS], [WrapWrappedColor(housesStr, aColBld)]);
      end;
    gcBuildingsType : If aBuildingType <> -1 then goalCondStr := gResTexts[HOUSE_VICTORY_ORDER[aBuildingType].TextID];
    else
      goalCondStr := gResTexts[GOAL_CONDITION_LIBX[aGoalCondition, aGoalType]];
  end;

  // Format text out of string like:
  // 'For player %0:s to %1:s player's %2:s %3:s must be %4:s'
  // We want to get smth like (depends of the locale, ofc)
  // 'For player 1 to win player's 2 troops must be destroyed'
  Result := Format(gResTexts[TX_GOAL_CONDITION_DESC_PATTERN], [WrapColor(aPlayer1 + 1, aColPlayer1),
                                                               WrapColor(gResTexts[GOAL_TYPE_DESC_TX[aGoalType, dwkFirst]], aColTxt),
                                                               WrapColor(aPlayer2 + 1, aColPlayer2),
                                                               WrapColor(goalCondStr, aColTxt),
                                                               WrapColor(gResTexts[GOAL_TYPE_DESC_TX[aGoalType, dwkSecond]], aColTxt)]);
end;



end.

