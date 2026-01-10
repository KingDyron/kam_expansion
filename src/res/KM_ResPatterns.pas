unit KM_ResPatterns;
{$I KaM_Remake.inc}
interface
uses
  Classes;

type
  TKMObjectPattern = packed record
  private
    fObj : array[1..30, 1..30] of Word;
    fNames : array of AnsiString;
    fFileName : String;//it's only set after loading files
  public
    procedure Reset;
    function GetObject(X, Y : Integer) : Word;
    function GetName : AnsiString;
    procedure LoadFromFile(aPath : String);
    procedure SaveToFile(aPath : String);
    procedure SetName(aName : AnsiString);
  end;
  PKMObjectPattern = ^TKMObjectPattern;

  TKMPatternsCollection = class
    private
      fList : array of TKMObjectPattern;
      fDirName : String;
      function GetPattern(aIndex : Integer) : TKMObjectPattern;
    public
      constructor Create(aPath: String);
      property Pattern[aIndex: Integer] : TKMObjectPattern read GetPattern; default;
      function Count : Integer;
      procedure SetNamesToID;
      procedure Add(aPattern : TKMObjectPattern);
      procedure Delete(aIndex : Integer);
      function GetName : String;

      procedure Save(aClearDir : Boolean = true);
  end;

  TKMResPatterns = class
    private
      fList :array of TKMPatternsCollection;
      fLocal : TKMPatternsCollection;
      function GetItem(aIndex : Integer) : TKMPatternsCollection;
      function GetPattern(aCollection, aIndex : Integer) : TKMObjectPattern;
    public
      constructor Create;
      destructor Destroy;override;
      property Collection[aIndex : Integer] : TKMPatternsCollection read GetItem; default;
      property Pattern[aCollection, aIndex : Integer] : TKMObjectPattern read GetPattern;
      property Local : TKMPatternsCollection read fLocal;
      function Count : Integer;
      procedure AddNewPattern(startX, startY : Integer);
      function DeletePattern(aIndex : Integer) : Boolean;

      function PatternMoveUp(aIndex : Integer) : Integer;
      function PatternMoveDown(aIndex : Integer) : Integer;

      procedure Save(aLocalOnly : Boolean = true);
  end;

implementation
uses
    SysUtils,
    IOUtils,
    KM_Defaults,
    JsonDataObjects,
    KM_JsonHelpers,
    KM_ResLocales,
    KM_Terrain;


procedure TKMObjectPattern.Reset;
var I: Integer;
begin
  SetLength(fNames, gResLocales.Count);

  for I := 0 to High(fNames) do
    fNames[I] := '';
  FillChar(fObj, SizeOf(fObj), #0);
  fFileName := '';
end;

procedure TKMObjectPattern.LoadFromFile(aPath: string);
var I, K : Integer;
  nFile : TKMJson;
  nArr, nArr2 : TKMJsonArray;
begin
  Reset;

  If not FileExists(aPath) then
    Exit;
  fFileName := TPath.GetFileName(aPath);
  nFile := TKMJson.Create(aPath);

  for I := 0 to High(fNames) do
    If nFile.Contains(String(gResLocales[I].Code)) then
      fNames[I] := AnsiString(nFile.S[String(gResLocales[I].Code)]);

  nArr := nFile.A['Objects'];
  Assert(nArr.Count = 30, 'TKMObjectPattern.LoadFromFile, this pattern has not enough data :' + aPath);

  for I := 0 to 29 do
  begin
    nArr2 := nArr.A[I];
    Assert(nArr2.Count = 30, 'TKMObjectPattern.LoadFromFile, this pattern has not enough data :' + aPath);
    for K := 0 to 29 do
      fObj[I + 1, K + 1] := nArr2.I[K];

  end;

end;

procedure TKMObjectPattern.SaveToFile(aPath: string);
var I, K : Integer;
  nFile : TStringList;
  S : String;
begin
  nFile := TStringList.Create;

  try
    nFile.Add('{');//start of file

    for I := 0 to High(fNames) do
      If fNames[I] <> '' then
      begin
        S := #9 + Format('"%s": "%s",', [String(gResLocales[I].Code), fNames[I]]);
        nFile.Add(S);
      end;


    S := '';
    nFile.Add(#9 + '"Objects": [');//start of objects array
    for I := 1 to 30 do
    begin
      S := #9#9 + '['; //start of line of obj IDs

      for K := 1 to 30 do
      begin
        S := S + IntToStr(fObj[I, K]);
        If K < 30 then //add comma after every obj ID
          S := S + ', ';
      end;
      S := S + ']';//end of line of obj IDs
      If I < 30 then //add comma after every obj array
        S := S + ',';


      nFile.Add(S);//add every line
    end;
    nFile.Add(#9 + ']');//end of objects array

    nFile.Add('}');//end of file
    nFile.SaveToFile(aPath, TEncoding.UTF8);
  finally
    FreeAndNil(nFile);
  end;
end;


function TKMObjectPattern.GetObject(X: Integer; Y: Integer): Word;
begin
  Result := fObj[Y mod 30 + 1, X mod 30 + 1];
end;

function TKMObjectPattern.GetName: AnsiString;
var I : Integer;
begin
  I := gResLocales.UserLocaleIndex;
  Result := fNames[I];
  If Result = '' then
  begin
    I := gResLocales.IndexByCode(gResLocales.DefaultLocale);
    Result := fNames[I];
  end;
  If Result = '' then
    Result := AnsiString(TPath.GetFileNameWithoutExtension(fFilename));
  If Result = '' then
    Result := 'Custom pattern';
end;

procedure TKMObjectPattern.SetName(aName: AnsiString);
var I : integer;
begin
  I := gReslocales.UserLocaleIndex;

  fNames[I] := aName;
end;

destructor TKMResPatterns.Destroy;
var I : integer;
begin
  for I := 0 to High(fList) do
    FreeAndNil(fList[I]);

  Inherited;
end;

constructor TKMPatternsCollection.Create(aPath: string);
var path : String;
  I : Integer;
begin
  Inherited Create;
  If not DirectoryExists(aPath) then
    Exit;
  fDirName := TPath.GetFileName(aPath);
  Setlength(fList, 0);

  for path in TDirectory.GetFiles(aPath) do
    If TPath.GetExtension(path) = '.json' then
    begin
      I := length(fList);
      SetLength(fList, I + 1);
      fList[I].LoadFromFile(path);
    end;
end;

procedure TKMPatternsCollection.SetNamesToID;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].fFileName := Format('%.3d.json', [I]);
end;

procedure TKMPatternsCollection.Save(aClearDir : Boolean = true);
var I : Integer;
  path : String;
begin
  path := ExeDir + 'data' + PathDelim + 'Patterns' + PathDelim + fDirName;
  If aClearDir then
    If DirectoryExists(path) then
      TDirectory.Delete(path, true);
  SysUtils.ForceDirectories(path);
  for I := 0 to High(fList) do
    If fList[I].fFileName <> '' then
      fList[I].SaveToFile(path + PathDelim + fList[I].fFileName)
    else
      fList[I].SaveToFile(path + PathDelim + IntToStr(I) + '.json');
end;

function TKMPatternsCollection.GetPattern(aIndex: Integer): TKMObjectPattern;
begin
  Result := fList[aIndex];
end;

function TKMPatternsCollection.Count: Integer;
begin
  Result := length(fList);
end;

procedure TKMPatternsCollection.Add(aPattern: TKMObjectPattern);
begin
  SetLength(fList, Count + 1);
  fList[Count - 1] := aPattern;
end;

procedure TKMPatternsCollection.Delete(aIndex: Integer);
var I : integer;
begin
  for I := aIndex to Count - 2 do
    fList[I] := fList[I + 1];
  SetLength(fList, high(fList));
end;

function TKMPatternsCollection.GetName: string;
begin
  Result := fDirName;
end;





constructor TKMResPatterns.Create;
var path : String;
  I : Integer;
  PC : TKMPatternsCollection;
begin
  Inherited;
  If not DirectoryExists(ExeDir + 'data' + PathDelim + 'Patterns') then
    Exit;
  for path in TDirectory.GetDirectories(ExeDir + 'data' + PathDelim + 'Patterns' + PathDelim) do
    If not (TPath.GetFileName(path) = 'Local') then
    begin
      I := length(fList);
      SetLength(fList, I + 1);
      PC := TKMPatternsCollection.Create(path);
      fList[I] := PC;
    end;

  fLocal := TKMPatternsCollection.Create(ExeDir + 'data' + PathDelim + 'Patterns' + PathDelim + 'Local');
  fLocal.fDirName := 'Local';
end;

procedure TKMResPatterns.Save(aLocalOnly : Boolean = true);
var I : integer;
  path : String;
begin
  path := ExeDir + 'data' + PathDelim + 'Patterns' + PathDelim;
  SysUtils.ForceDirectories(path);
  If not aLocalOnly then
    for I := 0 to High(fList) do
      fList[I].Save;
  fLocal.fDirName := 'Local';
  fLocal.SetNamesToID;
  fLocal.Save;
end;

function TKMResPatterns.GetItem(aIndex: Integer): TKMPatternsCollection;
begin
  Result := fList[aIndex];
end;

function TKMResPatterns.GetPattern(aCollection: Integer; aIndex: Integer): TKMObjectPattern;
begin
  Result := fList[aCollection].Pattern[aIndex];
end;

function TKMResPatterns.Count: Integer;
begin
  Result := length(fList);
end;

procedure TKMResPatterns.AddNewPattern(startX, startY : Integer);
  function GetPattern : TKMObjectPattern;
  var I, K : Integer;
  begin
    Result.Reset;
    for I := 0 to 29 do
      for K := 0 to 29 do
      begin
        Result.fObj[I + 1, K + 1] := gTerrain.Land^[startY + I, startX + K].Obj;
      end;
  end;

begin
  fLocal.Add(GetPattern);
end;

function TKMResPatterns.DeletePattern(aIndex : Integer) : Boolean;
begin
  If aIndex = -1 then
    Exit(false);
  Result := true;
  fLocal.Delete(aIndex);
end;


function TKMResPatterns.PatternMoveUp(aIndex : Integer) : Integer;
var tmp : TKMObjectPattern;
begin
  If aIndex = 0 then
    Exit(0);
  tmp := fLocal.fList[aIndex - 1];
  fLocal.fList[aIndex - 1] := fLocal.fList[aIndex];
  fLocal.fList[aIndex] := tmp;
  Result := aIndex - 1; //return new index of moved pattern
end;

function TKMResPatterns.PatternMoveDown(aIndex : Integer) : Integer;
var tmp : TKMObjectPattern;
begin
  If aIndex = fLocal.Count - 1 then
    Exit(aIndex);

  tmp := fLocal.fList[aIndex + 1];
  fLocal.fList[aIndex + 1] := fLocal.fList[aIndex];
  fLocal.fList[aIndex] := tmp;
  Result := aIndex + 1; //return new index of moved pattern
end;


end.
