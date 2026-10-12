unit KM_ResPatterns;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Points,
  KM_Defaults,
  KM_JsonHelpers;

type
  TKMPatternType = (potNone, potObjects, potTileOverlay, potHeights);

  TKMPattern = packed record
  private
    fType : TKMPatternType;
    fData : array of array of Word;
    fSize : TKMPoint;
    fNames : array of AnsiString;
    fFileName : String;//it's only set after loading files
  public
    procedure Reset;
    procedure SetSize(X, Y : Integer);
    function GetObject(X, Y : Integer) : Word;
    function GetName : AnsiString;
    procedure LoadFromJson(JSON : TKMJsonObject);
    procedure LoadFromFile(aPath : String);
    procedure SaveToJson(JSON : TKMJsonObject);
    procedure SetName(aName : AnsiString);
    property PatternType : TKMPatternType read fType;
  end;
  PKMPattern = ^TKMPattern;

  TKMPatternsCollection = class
    private
      fList : array of TKMPattern;
      fDirName : String;
      fCount : Integer;
      function GetPattern(aIndex : Integer) : PKMPattern;
    public
      constructor Create(aPath: String);
      property Pattern[aIndex: Integer] : PKMPattern read GetPattern; default;
      function Count : Integer;
      procedure SetNamesToID;
      procedure Add(aPattern : TKMPattern);
      procedure Delete(aIndex : Integer);
      function GetName : String;

      procedure Save(aClearDir : Boolean = true);
  end;

  TKMResPatterns = class
    private
      fList :array of TKMPatternsCollection;
      fLocal : TKMPatternsCollection;
      function GetItem(aIndex : Integer) : TKMPatternsCollection;
      function GetPattern(aCollection, aIndex : Integer) : PKMPattern;
    public
      constructor Create;
      destructor Destroy;override;
      property Collection[aIndex : Integer] : TKMPatternsCollection read GetItem; default;
      property Pattern[aCollection, aIndex : Integer] : PKMPattern read GetPattern;
      property Local : TKMPatternsCollection read fLocal;
      function Count : Integer;
      procedure AddNewPattern(startX, startY, SizeX, SizeY : Integer; aType : TKMPatternType);
      function DeletePattern(aIndex : Integer) : Boolean;

      function PatternMoveUp(aIndex : Integer) : Integer;
      function PatternMoveDown(aIndex : Integer) : Integer;

      procedure Save(aLocalOnly : Boolean = true);
  end;

const
  PATTERN_TYPE_COLOR : array[TKMPatternType] of Cardinal = (icWhite, icLightGreen, icYellow, icOrange);
implementation
uses
    SysUtils,
    IOUtils,
    JsonDataObjects,
    KM_ResLocales,
    KM_Terrain;


procedure TKMPattern.Reset;
var I: Integer;
begin
  SetLength(fNames, gResLocales.Count);

  for I := 0 to High(fNames) do
    fNames[I] := '';
  FillChar(fData, SizeOf(fData), #0);
  fFileName := '';

  fType := potNone;
  SetLength(fData, 0);
  fSize := KMPOINT_ZERO;
end;

procedure TKMPattern.SetSize(X: Integer; Y: Integer);
begin
  fSize := KMPoint(X, Y);
  SetLength(fData, Y, X);
end;

procedure TKMPattern.LoadFromFile(aPath: string);
var I, K : Integer;
  nFile : TKMJsonObject;
  nArr, nArr2 : TKMJsonArrayNew;
  rev : Integer;
begin
  Reset;

  If not FileExists(aPath) then
    Exit;
  fFileName := TPath.GetFileName(aPath);
  nFile := TKMJsonObject.Create(aPath);
  rev := nFile.I['KaM Expansion version'];
  If rev = 0 then
  begin
    for I := 0 to High(fNames) do
      fNames[I] := AnsiString(nFile.GetS(String(gResLocales[I].Code), '') );

    nArr := nFile.A['Objects'];
    fSize := KMPoint(30, 30);
    SetLength(fData, 30, 30);
    fType := potObjects;
    Assert(nArr.Count = 30, 'TKMObjectPattern.LoadFromFile, this pattern has not enough data :' + aPath);

    for I := 0 to 29 do
    begin
      nArr2 := nArr.A[I];
      Assert(nArr2.Count = 30, 'TKMObjectPattern.LoadFromFile, this pattern has not enough data :' + aPath);
      for K := 0 to 29 do
        fData[I, K] := nArr2.I[K];
    end;
  end;

end;

procedure TKMPattern.LoadFromJson(JSON: TKMJsonObject);
var arr1, arr2 : TKMJsonArrayNew;
  I, K : Integer;
begin
  fType := TKMPatternType(JSON.I['Type']);
  fSize := KMPoint(JSON.I['SizeX'], JSON.I['SizeY']);
  SetLength(fData, fSize.Y, fSize.X);
  for I := 0 to High(fNames) do
    fNames[I] := AnsiString(JSON.GetS(gResLocales[I].Code));

  arr1 := JSON.A['Objects'];

  for I := 0 to arr1.Count - 1 do
  begin
    arr2 := arr1.A[I];
    Assert((fSize.Y = arr1.Count) and (fSize.X = arr2.Count), 'TKMObjectPattern.LoadFromJson, this pattern has not enough data');
    for K := 0 to arr2.Count - 1 do
      fData[I, K] := arr2.I[K];
  end;

end;

procedure TKMPattern.SaveToJson(JSON: TKMJsonObject);
var I, K : Integer;
  arr : TKMJsonArrayNew;
begin
  JSON.Add('Type', byte(fType));
  JSON.Add('SizeX', fSize.X);
  JSON.Add('SizeY', fSize.Y);
  for I := 0 to High(fNames) do
    JSON.Add(gResLocales[I].Code, fNames[I], '');

  arr := JSON.AddArray('Objects');
  for I := 0 to fSize.Y - 1 do
  begin
    arr.AddArray(true);
    for K := 0 to fSize.X - 1 do
      arr.A[I].Add(fData[I, K]);
  end;
end;


function TKMPattern.GetObject(X: Integer; Y: Integer): Word;
begin
  Result := fData[Y mod fSize.Y, X mod fSize.X];
end;

function TKMPattern.GetName: AnsiString;
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

procedure TKMPattern.SetName(aName: AnsiString);
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
  K : Integer;
  json : TKMJsonObject;
  jArr : TKMJsonArrayNew;
begin
  Inherited Create;
  If not DirectoryExists(aPath) then
    Exit;
  fDirName := TPath.GetFileName(aPath);
  json := TKMJsonObject.Create;
  Setlength(fList, 16);
  fCount := 0;
  for path in TDirectory.GetFiles(aPath) do
    If TPath.GetExtension(path) = '.json' then
    begin
      json.Clear;
      json.LoadFromFile(path);
      If json.I['KaM Expansion version'] <> 0 then
      begin
        jArr := Json.A['Patterns'];
        for K := 0 to jArr.Count - 1 do
        begin
          If fCount >= length(fList) then
            SetLength(fList, fCount + 16);
          fList[fCount].Reset;
          fList[fCount].LoadFromJson(jArr.O[K]);
          fList[fCount].fFileName := 'Custom pattern ' + fCount.ToString;
          Inc(fCount);
        end;
      end else
      begin
        If fCount >= length(fList) then
          SetLength(fList, fCount + 16);
        fList[fCount].LoadFromFile(path);
        Inc(fCount);
      end;
    end;
end;

procedure TKMPatternsCollection.SetNamesToID;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].fFileName := Format('%.3d.json', [I]);
end;

procedure TKMPatternsCollection.Save(aClearDir : Boolean = true);
const PATT_TYPE_NAME : array[potObjects..High(TKMPatternType)] of String = ('ObjectsList', 'TileOverlaysList', 'HeightsList');
var I : Integer;
  path : String;

  tpt : TKMPatternType;
  jObj : array[potObjects..High(TKMPatternType)] of TKMJsonObject;
  jArr : array[potObjects..High(TKMPatternType)] of TKMJsonArrayNew;

  jPatt : TKMJsonObject;
begin
  path := ExeDir + 'data' + PathDelim + 'Patterns' + PathDelim + fDirName;
  If aClearDir then
    If DirectoryExists(path) then
      TDirectory.Delete(path, true);
  SysUtils.ForceDirectories(path);

  for tpt := potObjects to High(TKMPatternType) do
  begin
    jObj[tpt] := TKMJsonObject.Create;
    jObj[tpt].Add('KaM Expansion version', GAME_REVISION_NUM);
    jArr[tpt] := jObj[tpt].AddArray('Patterns');
  end;



  for I := 0 to fCount - 1 do
  begin
    If fList[I].fType = potNone then
      Continue;

    jPatt := jArr[fList[I].fType].AddObject;
    fList[I].SaveToJson(jPatt);
  end;

  for tpt := potObjects to High(TKMPatternType) do
    If jArr[tpt].Count > 0 then
      jObj[tpt].SaveToFile(path + PathDelim + PATT_TYPE_NAME[tpt] + '.json');

  for tpt := potObjects to High(TKMPatternType) do
    FreeAndNil(jObj[tpt]);
end;

function TKMPatternsCollection.GetPattern(aIndex: Integer): PKMPattern;
begin
  Result := @fList[aIndex];
end;

function TKMPatternsCollection.Count: Integer;
begin
  Result := fCount;
end;

procedure TKMPatternsCollection.Add(aPattern: TKMPattern);
begin
  Inc(fCount);
  If fCount >= Length(fList) then
    SetLength(fList, fCount + 16);
  fList[fCount - 1] := aPattern;
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

function TKMResPatterns.GetPattern(aCollection: Integer; aIndex: Integer): PKMPattern;
begin
  Result := fList[aCollection].Pattern[aIndex];
end;

function TKMResPatterns.Count: Integer;
begin
  Result := length(fList);
end;

procedure TKMResPatterns.AddNewPattern(startX, startY, SizeX, SizeY : Integer; aType : TKMPatternType);
  function GetPattern : TKMPattern;
  var I, K : Integer;
  begin
    Result.Reset;
    Result.SetSize(SizeX, SizeY);
    Result.fType := aType;
    for I := 0 to SizeY - 1 do
      for K := 0 to SizeX - 1 do
        case aType of
          potNone : ;
          potObjects      : Result.fData[I, K] := gTerrain.Land^[startY + I, startX + K].Obj;
          potTileOverlay  : Result.fData[I, K] := gTerrain.Land^[startY + I, startX + K].TileOverlay;
          potHeights       : Result.fData[I, K] := gTerrain.Land^[startY + I, startX + K].Height;
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
var tmp : TKMPattern;
begin
  If aIndex = 0 then
    Exit(0);
  tmp := fLocal.fList[aIndex - 1];
  fLocal.fList[aIndex - 1] := fLocal.fList[aIndex];
  fLocal.fList[aIndex] := tmp;
  Result := aIndex - 1; //return new index of moved pattern
end;

function TKMResPatterns.PatternMoveDown(aIndex : Integer) : Integer;
var tmp : TKMPattern;
begin
  If aIndex = fLocal.Count - 1 then
    Exit(aIndex);

  tmp := fLocal.fList[aIndex + 1];
  fLocal.fList[aIndex + 1] := fLocal.fList[aIndex];
  fLocal.fList[aIndex] := tmp;
  Result := aIndex + 1; //return new index of moved pattern
end;


end.
