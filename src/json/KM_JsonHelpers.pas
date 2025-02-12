unit KM_JsonHelpers;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KM_CommonTypes, KM_ResTypes,
  KM_Points,
  JsonDataObjects;


type
  TKMJson = TJsonObject;
  TKMJsonArray = TJsonArray;

  TKMJsonRec = record
    Name, Path : String;
    Json : TKMJson;
  end;

  TKMJsonHelper = class helper for TKMJson
    constructor Create(aPath : String);overload;
    function GetArray(aName : String; out aValue : TKMStringArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TKMWordArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TIntegerArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TKMByteArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TBooleanArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TSingleArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TKMUnitTypeArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TKMHouseTypeArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TKMWareTypeArray) : Boolean; Overload;
    function GetArray(aName : String; out aValue : TJsonArray) : Boolean; Overload;

    function GetSet(aName : String; out aValue : TKMUnitTypeSet) : Boolean; Overload;
    function GetSet(aName : String; out aValue : TKMHouseTypeSet) : Boolean; Overload;
    function GetSet(aName : String; out aValue : TKMWareTypeSet) : Boolean; Overload;

    function GetAnim(aName : String; out aValue : TKMAnimation) : Boolean; Overload;
    function GetAnim(aName : String; out aValue : TKMAnimLoop) : Boolean; Overload;
    function GetAnim(out aValue : TKMAnimLoop) : Boolean; Overload;
    function GetAnim(out aValue : TKMAnimation) : Boolean; Overload;

    function SetIfContains(aName : String; out aValue : TKMAnimation) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMAnimLoop) : Boolean; overload;

    function SetIfContains(aName : String; out aValue : TKMStringArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMWordArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TIntegerArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMByteArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TBooleanArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TSingleArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMUnitTypeArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMHouseTypeArray) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : TKMWareTypeArray) : Boolean; overload;

    function SetIfContains(aName : String; out aValue : Integer) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : Byte) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : Word) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : String) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : Single) : Boolean; overload;
    function SetIfContains(aName : String; out aValue : Boolean) : Boolean; overload;

    function GetInteger(aName : String) : Integer;
    function GetSingle(aName : String) : Single;
    function GetBoolean(aName : String) : Boolean;
    function GetString(aName : String) : String;
    function GetPoint(aName : String) : TKMPoint;
    function GetPointF(aName : String) : TKMPointF;

    function GetHouseArea(aName : String; out aValue : TKMHouseAreaNew) : Boolean;

    function CRC : Cardinal;

  end;

  TKMJsonSaver = class
    private
      fStringList : TStringList;
      fDistance : Word;
      fInOneLine, fOldInOneLine: Boolean;
      procedure AddToLast(aText: String);
      procedure AddLine(aLine : String; aNewLine : Boolean = true);
      function ConvertDistance : String;
      procedure AddObject(aName : String);
      procedure AddArray(aName : String; aNewLine : Boolean = true);
      procedure AddCommaToLast;
      procedure AddValue(aName, aValue : String; aNewLine : Boolean = true);
      procedure SetOldOneLine;
      procedure GetOldOneLine;

    public
      constructor Create;
      destructor Destroy; override;

      procedure BeginFile;
      procedure EndFile;
      procedure BeginObject(aNewLine : Boolean = true);
      procedure EndObject(aNewLine : Boolean = true);
      procedure BeginArray(aNewLine : Boolean = true);
      procedure EndArray(aNewLine : Boolean = true);


      procedure WriteEmptyObject(aIsFirst : Boolean = false);
      procedure WriteObject(aName : String; aIsFirst : Boolean = false);
      procedure WriteArray(aName : String; aIsFirst : Boolean = false);
      procedure WriteToArray(aValue : String; aIsFirst : Boolean = false);
      procedure WriteValue(aName, aValue : String; aIsFirst : Boolean = false);

      procedure WriteLineObject(aName : String; aIsFirst : Boolean = false);
      procedure EndLineObject;
      procedure WriteLineArray(aName : String; aIsFirst : Boolean = false);
      procedure EndLineArray;

      Procedure Write(aName : String; aValue : Boolean; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : Integer; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : Word; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : Byte; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : ShortInt; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : SmallInt; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : Cardinal; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : Single; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : String; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMPoint; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMPointF; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMUnitType; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMHouseType; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMWareType; aIsFirst : Boolean = false);overload;

      Procedure Write(aName : String; aValue : TIntegerArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TInteger2Array; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMWordArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMStringArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMWareTypeArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMUnitTypeArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMHouseTypeArray; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMHouseTypeArray2; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMHouseArea; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMHouseAreaNew; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMWareType4; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMWareType8; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : THouseSupply; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : THouseSupply8; aIsFirst : Boolean = false);overload;

      Procedure Write(aName : String; aValue : TKMAnimLoop; aIsFirst : Boolean = false);overload;
      Procedure Write(aName : String; aValue : TKMAnimation; aIsFirst : Boolean = false);overload;

      procedure AddToArray(aValue : Integer; aIsFirst : Boolean = false);overload;
      procedure AddToArray(aValue : Word; aIsFirst : Boolean = false);overload;
      procedure AddToArray(aValue : Byte; aIsFirst : Boolean = false);overload;
      procedure AddToArray(aValue : Single; aIsFirst : Boolean = false);overload;
      procedure AddToArray(aValue : String; aIsFirst : Boolean = false);overload;


      procedure SaveToFile(aPath : String);
  end;

implementation
uses KromUtils, IOUtils, KM_CommonClassesExt,
    SysUtils, Math;

constructor TKMJsonHelper.Create(aPath: string);
begin
  self := ParseFromFile(aPath) as TKMJson;
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TKMStringArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := aArr.S[I];
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TKMWordArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := EnsureRange(aArr.I[I], low(Word), high(Word));
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TIntegerArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := aArr.I[I];
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TKMByteArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := EnsureRange(aArr.I[I], low(byte), high(byte));
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TBooleanArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := aArr.B[I];
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TSingleArray) : Boolean;
var I: Integer;
  aArr : TJsonArray;
begin
  Result := false;
  If not GetArray(aName, aArr) then
    Exit;
  Result := true;
  SetLength(aValue, aArr.Count);
  for I := 0 to aArr.Count - 1 do
    aValue[I] := aArr.D[I];
end;

function TKMJsonHelper.GetArray(aName : String; out aValue : TKMUnitTypeArray) : Boolean;
var I: Integer;
  W : TKMUnitType;
  aArr : TKMStringArray;
begin
  Result := false;

  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];

  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMUnitType>(aArr[I],  W) then
    begin
      SetLength(aValue, length(aValue) + 1);
      aValue[high(aValue)] := W;
      Result := true;
    end;
end;
function TKMJsonHelper.GetArray(aName : String; out aValue : TKMHouseTypeArray) : Boolean;
var I: Integer;
  W : TKMHouseType;
  aArr : TKMStringArray;
begin
  Result := false;

  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];

  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMHouseType>(aArr[I],  W) then
    begin
      SetLength(aValue, length(aValue) + 1);
      aValue[high(aValue)] := W;
      Result := true;
    end;
end;
function TKMJsonHelper.GetArray(aName : String; out aValue : TKMWareTypeArray) : Boolean;
var I: Integer;
  W : TKMWareType;
  aArr : TKMStringArray;
begin
  Result := false;

  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];

  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMWareType>(aArr[I],  W) then
    begin
      SetLength(aValue, length(aValue) + 1);
      aValue[high(aValue)] := W;
      Result := true;
    end;
end;
function TKMJsonHelper.GetArray(aName : String; out aValue : TJsonArray) : Boolean;
begin
  aValue := A[aName];
  Result := aValue.Count > 0;
end;

function TKMJsonHelper.GetSet(aName : String; out aValue : TKMUnitTypeSet) : Boolean;
var I: Integer;
  W : TKMUnitType;
  aArr : TKMStringArray;
begin
  Result := false;


  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];


  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMUnitType>(aArr[I],  W) then
    begin
      aValue := aValue + [W];
      Result := true;
    end;

end;

function TKMJsonHelper.GetSet(aName : String; out aValue : TKMHouseTypeSet) : Boolean;
var I: Integer;
  W : TKMHouseType;
  aArr : TKMStringArray;
begin
  Result := false;

  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];

  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMHouseType>(aArr[I],  W) then
    begin
      aValue := aValue + [W];
      Result := true;
    end;

end;

function TKMJsonHelper.GetSet(aName : String; out aValue : TKMWareTypeSet) : Boolean;
var I: Integer;
  W : TKMWareType;
  aArr : TKMStringArray;
begin
  Result := false;

  If not GetArray(aName, aArr) then
    Exit;
  aValue := [];

  for I := 0 to High(aArr) do
    if TKMEnumUtils.TryGetAs<TKMWareType>(aArr[I],  W) then
    begin
      aValue := aValue + [W];
      Result := true;
    end;

end;

function TKMJsonHelper.GetAnim(aName : String; out aValue : TKMAnimation) : Boolean;
var arr : TKMWordArray;
  json : TJsonObject;
begin
  Result := false;
  json := O[aName];
  if json.Contains('StepStart') then
  begin
    Result := true;
    aValue.Create(json.I['X'],json.I['Y'],
                  json.I['StepStart'],json.I['Count'],
                  json.I['Offset'],json.B['BackWard'])
  end else
  begin
    arr := [];
    GetArray('Steps', arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aValue.Create(json.I['X'],json.I['Y'], arr ,json.I['Offset']);
  end;

  if json.Contains('Extend') then
    aValue.Extend(json.I['Extend']);

end;

function TKMJsonHelper.GetAnim(aName : String; out aValue : TKMAnimLoop) : Boolean;
var arr : TIntegerArray;
  json : TJsonObject;
begin
  Result := false;
  json := O[aName];
  if json.Contains('StepStart') then
  begin
    Result := true;
    aValue.Create(json.I['X'],json.I['Y'],
                  json.I['StepStart'],json.I['Count'],
                  json.I['Offset'],json.B['BackWard'])

  end else
  begin
    arr := [];
    GetArray('Steps', arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aValue.Create(json.I['X'],json.I['Y'], arr ,json.I['Offset']);
  end;


end;

function TKMJsonHelper.GetAnim(out aValue : TKMAnimation) : Boolean;
var arr : TKMWordArray;
begin
  Result := false;
  if Contains('StepStart') then
  begin
    Result := true;
    aValue.Create(I['X'],I['Y'],
                  I['StepStart'],I['Count'],
                  I['Offset'],B['BackWard'])
  end else
  begin
    arr := [];
    GetArray('Steps', arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aValue.Create(I['X'],I['Y'], arr ,I['Offset']);
  end;

  if Contains('Extend') then
    aValue.Extend(I['Extend']);

end;

function TKMJsonHelper.GetAnim(out aValue : TKMAnimLoop) : Boolean;
var arr : TIntegerArray;
begin
  Result := false;
  if Contains('StepStart') then
  begin
    Result := true;
    aValue.Create(I['X'],I['Y'],
                  I['StepStart'],I['Count'],
                  I['Offset'],B['BackWard'])

  end else
  begin
    arr := [];
    GetArray('Steps', arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aValue.Create(I['X'],I['Y'], arr ,I['Offset']);
  end;


end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMAnimation) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetAnim(aName, aValue);
end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMAnimLoop) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetAnim(aName, aValue);
end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMStringArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMWordArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TIntegerArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMByteArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TBooleanArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TSingleArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMUnitTypeArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMHouseTypeArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : TKMWareTypeArray) : Boolean;
begin
  Result := false;
  if Contains(aName) then
    Result := GetArray(aName, aValue);
end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : Integer) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := I[aName];
end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : Byte) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := I[aName];
end;

function TKMJsonHelper.SetIfContains(aName : String; out aValue : Word) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := I[aName];

end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : String) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := S[aName];

end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : Single) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := D[aName];

end;
function TKMJsonHelper.SetIfContains(aName : String; out aValue : Boolean) : Boolean;
begin
  Result := Contains(aName);
  if Result then
    aValue := B[aName];
end;

function TKMJsonHelper.GetInteger(aName : String) : Integer;
begin
  Result := I[aName];
end;
function TKMJsonHelper.GetSingle(aName : String) : Single;
begin
  Result := D[aName];
end;
function TKMJsonHelper.GetBoolean(aName : String) : Boolean;
begin
  Result := B[aName];
end;
function TKMJsonHelper.GetString(aName : String) : String;
begin
  Result := S[aName];
end;
function TKMJsonHelper.GetPoint(aName : String) : TKMPoint;
begin
  Result := KMPoint(O[aName].I['X'], O[aName].I['Y']);

end;
function TKMJsonHelper.GetPointF(aName : String) : TKMPointF;
begin
  Result := KMPointF(O[aName].D['X'], O[aName].D['Y']);
end;

function TKMJsonHelper.GetHouseArea(aName : String; out aValue : TKMHouseAreaNew) : Boolean;
var I, K : Integer;
  arr1, arr2 : TKMJsonArray;
begin
  arr1 := self.A[aName];
  If arr1.Count = 0 then
    Exit(false);
  Result := true;
  FillChar(aValue, Sizeof(aValue), #0);
  for I := 0 to Min(arr1.Count -1, MAX_HOUSE_SIZE - 1) do
  begin
    arr2 := arr1.A[I];
    for K := 0 to Min(arr2.Count -1, MAX_HOUSE_SIZE - 1) do
      aValue[I + 1, K + 1] := arr2.I[K];
  end;

end;

function TKMJsonHelper.CRC: Cardinal;
var S : TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S);
    Result := Adler32CRC(S);
  finally
    S.Free;

  end;
end;


constructor TKMJsonSaver.Create;
begin
  Inherited;
  fStringList := TStringList.Create;
  fDistance := 0;
  fInOneLine := false;
end;

destructor TKMJsonSaver.Destroy;
begin
  fStringList.Free;
  Inherited;
end;

function TKMJsonSaver.ConvertDistance: string;
var I : Integer;
begin
  Result := '';
  for I := 1 to fDistance do
    Result := Result + #9;
end;
procedure TKMJsonSaver.AddToLast(aText: string);
var S : String;
begin
  S := fStringList.Strings[fStringList.Count - 1] + aText;
  fStringList.Strings[fStringList.Count - 1] := S;
end;

procedure TKMJsonSaver.AddLine(aLine: string; aNewLine : Boolean = true);
begin
  If aNewLine and not fInOneLine then
    fStringList.Add(ConvertDistance + aLine)
  else
    AddToLast(aLine);
end;


procedure TKMJsonSaver.AddObject(aName : String);
begin
  If aName <> '' then
    AddLine(Format('"%s" : ', [aName]) );
  BeginObject(not fInOneLine);
end;

procedure TKMJsonSaver.AddArray(aName : String; aNewLine : Boolean = true);
begin
  If aName <> '' then
    AddLine(Format('"%s" : ', [aName]));
  BeginArray(aNewLine);
end;

procedure TKMJsonSaver.AddCommaToLast;
var S : String;
begin
  S := fStringList.Strings[fStringList.Count - 1] + ',';
  fStringList.Strings[fStringList.Count - 1] := S;
end;

procedure TKMJsonSaver.AddValue(aName, aValue : String; aNewLine : Boolean = true);
begin
  AddLine(Format('"%s" : %s', [aName, aValue]), aNewLine);
end;

procedure TKMJsonSaver.BeginFile;
begin
  fDistance := 0;
  AddLine('{');
  Inc(fDistance);
end;

procedure TKMJsonSaver.EndFile;
begin
  fDistance := 0;
  AddLine('}');
end;

procedure TKMJsonSaver.BeginObject(aNewLine : Boolean = true);
begin
  AddLine('{', aNewLine);
  Inc(fDistance);
end;

procedure TKMJsonSaver.EndObject(aNewLine : Boolean = true);
begin
  Dec(fDistance);
  AddLine('}', aNewLine);
end;

procedure TKMJsonSaver.BeginArray(aNewLine : Boolean = true);
begin
  AddLine('[', aNewLine);
  Inc(fDistance);
end;

procedure TKMJsonSaver.EndArray(aNewLine : Boolean = true);
begin
  Dec(fDistance);
  AddLine(']', aNewLine);
end;


procedure TKMJsonSaver.SetOldOneLine;
begin
  fOldInOneLine := fInOneLine;
end;

procedure TKMJsonSaver.GetOldOneLine;
begin
  fInOneLine := fOldInOneLine;
end;

procedure TKMJsonSaver.WriteEmptyObject(aIsFirst: Boolean = False);
begin
  If not aIsFirst then
    AddCommaToLast;
  BeginObject(not fInOneLine);
end;
procedure TKMJsonSaver.WriteObject(aName : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddObject(aName);
end;

procedure TKMJsonSaver.WriteArray(aName : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddArray(aName);
end;

procedure TKMJsonSaver.WriteToArray(aValue : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine(aValue);
end;

procedure TKMJsonSaver.WriteValue(aName, aValue : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddValue(aName, aValue);
end;

procedure TKMJsonSaver.WriteLineObject(aName : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;

  If aName <> '' then
    AddLine(Format('"%s" : ', [aName]) )
  else
    AddLine('');
  fInOneLine := true;
  BeginObject(false);
end;

procedure TKMJsonSaver.EndLineObject;
begin
  fInOneLine := true;
  EndObject;
  fInOneLine := false;
end;

procedure TKMJsonSaver.WriteLineArray(aName : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  If aName <> '' then
    AddLine(Format('"%s" : ', [aName]));
  fInOneLine := true;
  BeginArray(false);
end;

procedure TKMJsonSaver.EndLineArray;
begin
  fInOneLine := true;
  EndArray;
  fInOneLine := false;
end;

procedure TKMJsonSaver.SaveToFile(aPath : String);
begin
  fStringList.SaveToFile(apath, TEncoding.UTF8);
end;


Procedure TKMJsonSaver.Write(aName : String; aValue : Boolean; aIsFirst : Boolean = false);
begin
  WriteValue(aName, BoolToStr(aValue, true), aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : Integer; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : Word; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : Byte; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : ShortInt; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : SmallInt; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : Cardinal; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : Single; aIsFirst : Boolean = false);
begin
  WriteValue(aName, aValue.ToString, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : String; aIsFirst : Boolean = false);
begin
  WriteValue(aName, '"' + aValue + '"', aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName: string; aValue: TKMPoint; aIsFirst: Boolean = False);
begin
  WriteLineObject(aName, aIsFirst);
  Write('X', aValue.X, true);
  Write('Y', aValue.Y);
  EndLineObject;
end;

Procedure TKMJsonSaver.Write(aName: string; aValue: TKMPointF; aIsFirst: Boolean = False);
begin
  WriteLineObject(aName, aIsFirst);
  Write('X', aValue.X, true);
  Write('Y', aValue.Y);
  EndLineObject;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMUnitType; aIsFirst : Boolean = false);
var S : String;
begin
  If not TKMEnumUtils.GetName<TKMUnitType>(aValue, S) then
    raise Exception.Create('Wrong TKMUnitType in TKMJsonSaver');
  Write(aName, S, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMHouseType; aIsFirst : Boolean = false);
var S : String;
begin
  If not TKMEnumUtils.GetName<TKMHouseType>(aValue, S) then
    raise Exception.Create('Wrong TKMHouseType in TKMJsonSaver');
  Write(aName, S, aIsFirst);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMWareType; aIsFirst : Boolean = false);
var S : String;
begin
  If not TKMEnumUtils.GetName<TKMWareType>(aValue, S) then
    raise Exception.Create('Wrong TKMWareType in TKMJsonSaver');
  Write(aName, S, aIsFirst);
end;


Procedure TKMJsonSaver.Write(aName : String; aValue : TIntegerArray; aIsFirst : Boolean = false);
var I, J : integer;
  S : String;
begin
  If length(aValue) = 0 then
    Exit;

  If not aIsFirst then
    AddCommaToLast;

  AddArray(aName, false);

  S := '';
  J := High(aValue);
  for I := 0 to J do
  begin
    S := S + aValue[I].ToString;
    If I < J then
      S := S + ', ';
  end;
  AddLine(S, false);

  EndArray(false);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TInteger2Array; aIsFirst : Boolean = false);
var I, K, J, L : integer;
  S : String;
begin
  If length(aValue) = 0 then
    Exit;

  If not aIsFirst then
    AddCommaToLast;

  AddArray(aName);

  L := high(aValue);
  for K := 0 to L do
  begin
    S := '';
    If K > 0 then
      AddCommaToLast;
    J := High(aValue[K]);
    BeginArray;
    for I := 0 to J do
    begin
      S := S + aValue[K, I].ToString;
      If I < J then
        S := S + ', ';
    end;
    AddLine(S, false);
    EndArray(false);
  end;

  EndArray(true);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMWordArray; aIsFirst : Boolean = false);
var I, J : integer;
  S : String;
begin
  If length(aValue) = 0 then
    Exit;

  If not aIsFirst then
    AddCommaToLast;

  AddArray(aName, false);

  S := '';
  J := High(aValue);
  for I := 0 to J do
  begin
    S := S + aValue[I].ToString;
    If I < J then
      S := S + ', ';
  end;
  AddLine(S, false);

  EndArray(false);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMStringArray; aIsFirst : Boolean = false);
var I, J : integer;
  S : String;
begin
  If length(aValue) = 0 then
    Exit;

  If not aIsFirst then
    AddCommaToLast;

  AddArray(aName, false);

  S := '';
  J := High(aValue);
  for I := 0 to J do
  begin
    S := S + '"' + aValue[I] + '"';
    If I < J then
      S := S + ', ';
  end;
  AddLine(S, false);

  EndArray(false);
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMWareTypeArray; aIsFirst : Boolean = false);
var S : String;
  I : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteLineArray(aName, aIsFirst);
  for I := 0 to High(aValue) do
  begin
    If not TKMEnumUtils.GetName<TKMWareType>(aValue[I], S) then
      raise Exception.Create('Wrong TKMWareType in TKMJsonSaver');

    AddToArray(S, I = 0);
  end;

  EndLineArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMUnitTypeArray; aIsFirst : Boolean = false);
var S : String;
  I : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteLineArray(aName, aIsFirst);
  for I := 0 to High(aValue) do
  begin
    If not TKMEnumUtils.GetName<TKMUnitType>(aValue[I], S) then
      raise Exception.Create('Wrong TKMUnitType in TKMJsonSaver');

    AddToArray(S, I = 0);
  end;

  EndLineArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMHouseTypeArray; aIsFirst : Boolean = false);
var S : String;
  I : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteLineArray(aName, aIsFirst);
  for I := 0 to High(aValue) do
  begin
    If not TKMEnumUtils.GetName<TKMHouseType>(aValue[I], S) then
      raise Exception.Create('Wrong TKMHouseType in TKMJsonSaver');

    AddToArray(S, I = 0);
  end;

  EndLineArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMHouseTypeArray2; aIsFirst : Boolean = false);
var S : String;
  I, K : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteArray(aName, aIsFirst);

  for I := 0 to High(aValue) do
  begin
    AddToArray('htNone', I = 0);
    fInOneLine := true;
    for K := 0 to High(aValue[I]) do
    begin
      If not TKMEnumUtils.GetName<TKMHouseType>(aValue[I, K], S) then
        raise Exception.Create('Wrong TKMHouseType in TKMJsonSaver');
      AddToArray(S);
    end;

    fInOneLine := false;
  end;
  EndArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMHouseArea; aIsFirst : Boolean = false);
var I, K : integer;
begin
  If not aIsFirst then
    AddCommaToLast;

  SetOldOneLine;
  AddArray(aName);

  for K := 1 to 4 do
  begin
    If K > 1 then
      AddCommaToLast;
    BeginArray(true);
    fInOneLine := true;
    for I := 1 to 4 do
    begin
      AddToArray(aValue[K, I], I = 1);
    end;
    EndArray(false);
    fInOneLine := false;
  end;

  EndArray(true);
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMHouseAreaNew; aIsFirst : Boolean = false);
var I, K : integer;
begin
  If not aIsFirst then
    AddCommaToLast;

  SetOldOneLine;
  AddArray(aName);

  for K := 1 to MAX_HOUSE_SIZE do
  begin
    If K > 1 then
      AddCommaToLast;
    BeginArray(true);
    fInOneLine := true;
    for I := 1 to MAX_HOUSE_SIZE do
    begin
      AddToArray(aValue[K, I], I = 1);
    end;
    EndArray(false);
    fInOneLine := false;
  end;

  EndArray(true);
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMWareType4; aIsFirst : Boolean = false);
var S : String;
  I : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteArray(aName, aIsFirst);
  for I := low(aValue) to High(aValue) do
  begin
    If not TKMEnumUtils.GetName<TKMWareType>(aValue[I], S) then
      raise Exception.Create('Wrong TKMWareType in TKMJsonSaver');

    AddToArray(S, I = low(aValue));
  end;

  EndArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMWareType8; aIsFirst : Boolean = false);
var S : String;
  I : Integer;
begin
  If length(aValue) = 0 then
    Exit;
  SetOldOneLine;
  WriteLineArray(aName, aIsFirst);
  for I := low(aValue) to High(aValue) do
  begin
    If not TKMEnumUtils.GetName<TKMWareType>(aValue[I], S) then
      raise Exception.Create('Wrong TKMWareType in TKMJsonSaver');

    AddToArray(S, I = low(aValue));
  end;

  EndLineArray;
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : THouseSupply; aIsFirst : Boolean = false);
var I, K : integer;
begin
  If not aIsFirst then
    AddCommaToLast;
  SetOldOneLine;

  AddArray(aName);

  for K := low(aValue) to high(aValue) do
  begin
    If K > 1 then
      AddCommaToLast;
    BeginArray(true);
    fInOneLine := true;
    for I := low(aValue[K]) to high(aValue[K]) do
    begin
      AddToArray(aValue[K, I], I = 1);
    end;
    EndArray(false);
    fInOneLine := false;
  end;

  EndArray(true);
  GetOldOneLine;
end;

Procedure TKMJsonSaver.Write(aName : String; aValue : THouseSupply8; aIsFirst : Boolean = false);
var I, K : integer;
begin
  If not aIsFirst then
    AddCommaToLast;
  SetOldOneLine;

  AddArray(aName);

  for K := low(aValue) to high(aValue) do
  begin
    If K > 1 then
      AddCommaToLast;
    BeginArray(true);
    fInOneLine := true;
    for I := low(aValue[K]) to high(aValue[K]) do
    begin
      AddToArray(aValue[K, I], I = 1);
    end;
    EndArray(false);
    fInOneLine := false;
  end;

  EndArray(true);
  GetOldOneLine;
end;

procedure TKMJsonSaver.Write(aName: string; aValue: TKMAnimLoop; aIsFirst: Boolean = False);
var I : Integer;
begin
  If aValue.Count = 0 then
    Exit;

  SetOldOneLine;


  WriteLineObject(aName, aIsFirst);
    Write('X', aValue.MoveX, true);
    Write('Y', aValue.MoveY);

    WriteArray('Steps');
      for I := 1 to aValue.Count do
        AddToArray(aValue.Step[I], I = 1);
    EndArray;
  EndLineObject;

  GetOldOneLine;
end;

procedure TKMJsonSaver.Write(aName: string; aValue: TKMAnimation; aIsFirst: Boolean = False);
var I : Integer;
begin
  If aValue.Count = 0 then
    Exit;

  SetOldOneLine;


  WriteLineObject(aName, aIsFirst);
    Write('X', aValue.X, true);
    Write('Y', aValue.Y);

    WriteArray('Steps');
      for I := 0 to aValue.Count - 1 do
        AddToArray(aValue.Step[I], I = 1);
    EndArray;
  EndLineObject;

  GetOldOneLine;
end;


procedure TKMJsonSaver.AddToArray(aValue : Integer; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine(aValue.ToString);
end;

procedure TKMJsonSaver.AddToArray(aValue : Word; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine(aValue.ToString);
end;

procedure TKMJsonSaver.AddToArray(aValue : Byte; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine(aValue.ToString);
end;

procedure TKMJsonSaver.AddToArray(aValue : Single; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine(aValue.ToString);
end;

procedure TKMJsonSaver.AddToArray(aValue : String; aIsFirst : Boolean = false);
begin
  If not aIsFirst then
    AddCommaToLast;
  AddLine('"' + aValue + '"');
end;


end.
