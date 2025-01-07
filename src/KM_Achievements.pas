unit KM_Achievements;
{$I KaM_Remake.inc}
interface
uses KM_CommonClasses,
    KM_Defaults,
    KM_ResTypes;

type
  TKMAchievement = class
  private
    fID : String; //indentifier to find achievement after loading progress
    fTextID : Word;
    function GetIsCompleted : Boolean;
    procedure SetIsCompleted(aValue : Boolean);
    function GetAbsToShow : Boolean;
    function GetLevelsCount : Byte;
    function GetCurrentLevel : Byte;
  public
    FullText : String;
    ToShow,
    CopyFromPrevious : Boolean;
    Progress, MaxProgress : Cardinal;
    Parent,
    Next : TKMAchievement;//after completing achievement you unlock next level

    procedure IncProgress(aCount : Integer = 1);
    function CurrentProgress : Single;
    property Completed : Boolean read GetIsCompleted write SetIsCompleted;
    property AbsToShow : Boolean read GetAbsToShow;
    property LevelsCount : Byte read GetLevelsCount;
    property CurrentLevel : Byte read GetCurrentLevel;
    constructor Create(aID : String; aText: Word; aMaxProgress : Cardinal; aCopy: Boolean = true);
  end;

  TKMAchievements = class
    private
      fCount : Integer;
      fList : array of TKMAchievement;
      fSortedList : array of TKMAchievement;
      fSortedCount : Integer;
      fLast : TKMAchievement;

      fTexts : array of String;

      fMakeSave : Boolean;

      procedure AddText(aText : String);
      function GetText(aIndex : Integer) : String;
      function GetAchievement(aIndex : Integer) : TKMAchievement;
      function GetSortedAchievement(aIndex : Integer) : TKMAchievement;
      function AddToList(aValue : TKMAchievement) : TKMAchievement;Overload;
      function AddToList(aID : String; aText : Word; aMaxProgress : Cardinal; CopyFromPrevious : Boolean = true) : TKMAchievement;Overload;
      procedure AddToLast(aValue : TKMAchievement);
      procedure AddNew(aValue : TKMAchievement);

      function Find(aID : String) : TKMAchievement;
      procedure GoldProduced(aCount : Integer);
      procedure FoodProduced(aCount : Integer);
      procedure UnitTrained(aUnitType: TKMUnitType; aCount : Integer);
    public
      constructor Create;
      destructor Destroy; override;
      Property Achievement[aIndex : Integer] : TKMAchievement read GetAchievement; default;
      property SortedCount : Integer read fSortedCount;
      property Sorted[aIndex : Integer] : TKMAchievement read GetSortedAchievement;

      procedure WareProduced(aWareType : TKMWareType; aCount : Integer);
      procedure GameWon;

      procedure SaveProgress;
      procedure LoadProgress;
      procedure ReloadTexts;
      procedure Visit;
      procedure MakeSave;

      function AnythingToShow : Boolean;

      procedure UpdateState;

  end;

var gAchievements : TKMAchievements;
implementation
uses SysUtils, Math,
    KM_CommonHelpers,
    KM_Game, KM_GameParams, KM_HandsCollection, KM_Hand,
    KM_ResTexts;

constructor TKMAchievement.Create(aID : String; aText: Word; aMaxProgress : Cardinal; aCopy: Boolean = true);
begin
  fID := aID;

  MaxProgress := aMaxProgress;
  fTextID := aText;
  //FullText := aText;

  Parent := nil;
  Next := nil;
  Progress := 0;
  ToShow := false;
  Completed := false;
  CopyFromPrevious := aCopy;
end;

function TKMAchievement.CurrentProgress: Single;
begin
  Result := Progress / MaxProgress;
end;

procedure TKMAchievement.IncProgress(aCount: Integer = 1);
var rest : Integer;
  oldProgress : Cardinal;
begin
  if self = nil then
    Exit;

  if Completed and (next <> nil) then
  begin
    Next.IncProgress(aCount);
    Exit;
  end;

  rest := (Progress + aCount) - MaxProgress;

  oldProgress := Progress;
  
  Progress := EnsureRange(Progress + aCount, 0, MaxProgress);

  if Progress >= MaxProgress then
  begin
    Completed := true;
    ToShow := oldProgress < Progress;

    if Next <> nil then
      if Next.CopyFromPrevious then
      begin
        Next.Progress := Progress;
        if rest > 0 then
          Next.IncProgress(rest);
      end;
  end;

end;

function TKMAchievement.GetIsCompleted : Boolean;
begin
  Result := Progress = MaxProgress;
end;

procedure TKMAchievement.SetIsCompleted(aValue : Boolean);
begin
  if aValue then
    Progress := MaxProgress
  else
    Progress := 0;
end;

function TKMAchievement.GetAbsToShow: Boolean;
begin
  Result := ToShow;
  if Parent <> nil then
    Result := Result or Parent.AbsToShow;
end;

function TKMAchievement.GetLevelsCount: Byte;
var A : TKMAchievement;
begin
  //find very first parent
  A := self;
  while A.Parent <> nil do
    A := A.Parent;

  Result := 1;
  while A.Next <> nil do
  begin
    Result := Result + 1;
    A := A.Next;
  end;
end;

function TKMAchievement.GetCurrentLevel: Byte;
var A : TKMAchievement;
begin
  Result := 1;
  A := Parent;
  while A <> nil do
  begin
    Inc(Result);
    A := A.Parent;
  end;
end;


constructor TKMAchievements.Create;
var textID : Integer;
  function NextText : Integer;
  begin
    Result := textID;
    Inc(TextID);
  end;
begin
  Inherited;
  textID := 0;
  fCount := 0;
  fSortedCount := 0;
  fMakeSave := false;

  AddNew( AddToList('Gold', NextText, 100) );
    AddToLast( AddToList('', NextText, 1000) );
    AddToLast( AddToList('', NextText, 10000) );
    AddToLast( AddToList('', textID, 100000) );

  AddNew( AddToList('Food', NextText, 1000) );
    AddToLast( AddToList('', NextText, 10000) );
    AddToLast( AddToList('', NextText, 100000) );

  AddNew( AddToList('TrainedUnits', NextText, 500) );
    AddToLast( AddToList('', NextText, 1000) );
    AddToLast( AddToList('', NextText, 5000) );

  AddNew( AddToList('GamesWon', NextText, 1) );
    AddToLast( AddToList('', NextText, 10) );
    AddToLast( AddToList('', NextText, 100) );
    AddToLast( AddToList('', NextText, 1000) );

  AddNew( AddToList('Palace', NextText, 1, false) );
    AddToLast( AddToList('', NextText, 1, false) );
    AddToLast( AddToList('', NextText, 1, false) );

  AddNew( AddToList('Realism', NextText, 1) );
    AddToLast( AddToList('', NextText, 15) );
    AddToLast( AddToList('', NextText, 34) );
    AddToLast( AddToList('', NextText, 666) );

  ReloadTexts;
  LoadProgress;
end;

destructor TKMAchievements.Destroy;
var I : Integer;
begin
  for I := 0 to fCount - 1 do
    fList[I].Free;

  Inherited;
end;

procedure TKMAchievements.AddText(aText: string);
begin
  SetLength(fTexts, length(fTexts) + 1);
  fTexts[high(fTexts)] := aText;
end;

function TKMAchievements.GetText(aIndex : Integer): string;
begin
  if InRange(aIndex, 0, high(fTexts)) then
    Result := fTexts[aIndex]
  else
    Result := '<<<Text| with ID = ' + IntToStr(aIndex) + ' not found >>>';
end;

function TKMAchievements.GetAchievement(aIndex: Integer): TKMAchievement;
begin
  Result := fList[aIndex];
end;

function TKMAchievements.GetSortedAchievement(aIndex: Integer): TKMAchievement;
begin
  Result := fSortedList[aIndex];

  while (Result.Completed) and (Result.Next <> nil) do
    Result := Result.Next;
end;

procedure TKMAchievements.AddToLast( aValue: TKMAchievement);
var S, S2 : String;
  I, K, count : Integer;
begin
  if aValue.fID = '' then
  begin
    S2 := '';
    S := fLast.fID;
    count := 0;
    for I := 1 to length(S) do
      if InRange(Ord(S[I]), 48, 57) then
      begin
        S2 := S2 + S[I];
        if count = 0 then
          count := I;
      end;

    if TryStrToInt(S2, K) then
    begin
      aValue.fID := Copy(S, 1, count - 1) + IntToStr(K + 1);
    end else
      aValue.fID := S + '0';
  end;

  aValue.Parent := fLast;
  fLast.Next := aValue;
  fLast := aValue;
end;

procedure TKMAchievements.AddNew( aValue: TKMAchievement);
begin
  Inc(fSortedCount);
  SetLength(fSortedList, fSortedCount);
  fSortedList[fSortedCount - 1] := aValue;
  fLast := aValue;
end;

function TKMAchievements.AddToList(aValue: TKMAchievement): TKMAchievement;
begin
  Inc(fCount);
  SetLength(fList, fCount);
  fList[fCount - 1] := aValue;
  Result := fList[fCount - 1];
end;

function TKMAchievements.AddToList(aID: string; aText: Word; aMaxProgress: Cardinal; CopyFromPrevious: Boolean = True): TKMAchievement;
begin
  Result := AddToList(TKMAchievement.Create(aID, aText, aMaxProgress, CopyFromPrevious) );
end;

function TKMAchievements.Find(aID: string): TKMAchievement;
  procedure Scan(A : TKMAchievement);
  begin
    if A.fID = aID then
    begin
      Result := A;
      Exit;
    end;
    if A.Next <> nil then
      Scan(A.Next);
  end;
var I : Integer;
begin
  Result := nil;
  for I := 0 to fSortedCount - 1 do
    Scan(fSortedList[I]);
end;

procedure TKMAchievements.ReloadTexts;
var S : String;
  I, K, count : Integer;
  skip : Boolean;
begin
  //make Texts
  SetLength(fTexts, 0);
  S := gResTexts[2109];
  count := 0;
  K := 0;
  skip := false;
  for I := 1 to length(S) - 1 do
  begin
    if skip then
      skip := false
    else
    begin
      if (S[I] = '|') and (S[I + 1] = '|') then
      begin
        AddText(Copy(S, K, count));
        count := 0;
        K := I + 2;
        skip := true;//we must skip one char
      end else
        Inc(count);
    end;
  end;
  if count > 0 then
    AddText(Copy(S, K, count + 1));

  //copy those text to Achievements
  for I := 0 to fCount - 1 do
    fList[I].FullText := GetText(fList[I].fTextID);
end;


procedure TKMAchievements.SaveProgress;
var S : TKMemoryStream;
  I : Integer;

begin
  S := TKMemoryStreamBinary.Create;

  try
    S.Write(fCount);
    for I := 0 to fCount - 1 do
    begin
      S.WriteANSI(fList[I].fID);
      S.Write(fList[I].Progress);
      S.Write(fList[I].ToShow);
    end;

    S.SaveToFile(ExeDir + SAVES_FOLDER_NAME + PathDelim + 'Achievements.dat');

  finally
    FreeAndNil(S);
  end;


end;

procedure TKMAchievements.LoadProgress;
var S : TKMemoryStream;
  I, nCount : Integer;
  id : String;
  tmpToShow : Boolean;
  tmpProgress : Cardinal;
  A : TKMAchievement;
begin
  if not FileExists(ExeDir + SAVES_FOLDER_NAME + PathDelim + 'Achievements.dat') then
    Exit;

  S := TKMemoryStreamBinary.Create;

  S.LoadFromFile(ExeDir + SAVES_FOLDER_NAME + PathDelim + 'Achievements.dat');

  S.Read(nCount);
  try
    for I := 0 to nCount - 1 do
    begin
      S.ReadANSI(id);
      S.Read(tmpProgress);
      S.Read(tmpToShow);

      A := Find(id);
      if A <> nil then
      begin
        A.Progress := tmpProgress;
        A.ToShow := tmpToShow;
      end;

    end;

  finally
    FreeAndNil(S);
  end;


end;

procedure TKMAchievements.Visit;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].ToShow := false;
  MakeSave;
end;

function TKMAchievements.AnythingToShow: Boolean;
var I : Integer;
begin
  Result := false;
  for I := 0 to High(fList) do
    Result := Result or fList[I].ToShow;
end;


procedure TKMAchievements.MakeSave;
begin
  fMakeSave := true;
end;

procedure TKMAchievements.UpdateState;
begin
  if fMakeSave then
  begin
    SaveProgress;
    fMakeSave := false;
  end;
end;



procedure TKMAchievements.GoldProduced(aCount: Integer);
begin
  Find('Gold').IncProgress(aCount);
  MakeSave;
end;

procedure TKMAchievements.FoodProduced(aCount: Integer);
begin
  Find('Food').IncProgress(aCount);
  MakeSave;
end;

procedure TKMAchievements.WareProduced(aWareType : TKMWareType; aCount : Integer);
begin
  if aWareType in WARES_HOUSE_FOOD then
    FoodProduced(aCount)
  else
  if aWareType = wtGold then
    GoldProduced(aCount);
end;

procedure TKMAchievements.UnitTrained(aUnitType: TKMUnitType; aCount: Integer);
begin
  Find('TrainedUnits').IncProgress(aCount);

  if aUnitType = utTrainedWolf then
    Find('Palace').IncProgress(aCount);
  if aUnitType = utPaladin then
    Find('Palace0').IncProgress(aCount);
  if aUnitType = utSpy then
    Find('Palace1').IncProgress(aCount);
end;

procedure TKMAchievements.GameWon;
var WT : TKMWareType;
  UT : TKMUnitType;
  H : TKMHand;
begin
  if self = nil then
    Exit;

  if gGameParams.IsNormalGame then
  begin
    Find('GamesWon').IncProgress;
    if gGameParams.MissionBuiltInDifficulty.IsRealism then
      Find('Realism').IncProgress;
  end;


  H := gMySpectator.Hand;
  for WT := WARE_MIN to WARE_MAX do
    WareProduced(WT, H.Stats.GetWaresProduced(WT));

  for UT := HUMANS_MIN to HUMANS_MAX do
    UnitTrained(UT, H.Stats.GetUnitTrainedQty(UT));


end;

end.
