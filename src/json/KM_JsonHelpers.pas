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

    function CRC : Cardinal;

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

end.
