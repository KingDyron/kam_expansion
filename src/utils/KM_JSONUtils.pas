unit KM_JSONUtils;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_CommonTypes,
  KM_CommonClasses,
  KM_Points,
  JsonDataObjects;

  function JSONArrToValidArr(aARR : TJsonArray; out aUnits : TKMUnitTypeArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aHouses : TKMHouseTypeArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aWares : TKMWareTypeArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aInt : TIntegerArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMCardinalArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMWordArray) : Boolean; overload;
  function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMByteArray) : Boolean; overload;

  function JSONArrToValidSet(aARR : TJsonArray; out aUnits : TKMUnitTypeSet) : Boolean; overload;
  function JSONArrToValidSet(aARR : TJsonArray; out aHouses : TKMHouseTypeSet) : Boolean; overload;
  function JSONArrToValidSet(aARR : TJsonArray; out aWares : TKMWareTypeSet) : Boolean; overload;

  function JSONToAnim(aJSON : TJsonObject; out aAnim : TKMAnimLoop) : Boolean;overload;
  function JSONToAnim(aJSON : TJsonObject; out aAnim : TKMAnimation) : Boolean; overload;
  function JSONToPoint(aJSON : TJsonObject; out aPoint : TKMPoint) : Boolean;
  function JSONIfContains(var A : Integer; aName : String; aJSON : TJsonObject) : Boolean; overload;
  function JSONIfContains(var A : Word; aName : String; aJSON : TJsonObject) : Boolean; overload;
  function JSONIfContains(var A : Byte; aName : String; aJSON : TJsonObject) : Boolean; overload;
  function JSONIfContains(var A : Boolean; aName : String; aJSON : TJsonObject) : Boolean; overload;

  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TIntegerArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWordArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMByteArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMCardinalArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMUnitTypeArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMHouseTypeArray); overload;
  procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWareTypeArray); overload;

  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TIntegerArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMWordArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMByteArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMCardinalArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMUnitTypeArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMHouseTypeArray); overload;
  procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMWareTypeArray); overload;

  procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMUnitTypeSet); overload;
  procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMHouseTypeSet); overload;
  procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMWareTypeSet); overload;

  procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMUnitTypeSet); overload;
  procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMHouseTypeSet); overload;
  procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWareTypeSet); overload;

  function GetJSONCRC(aJSON : TJsonObject) : Cardinal;


implementation
uses
  SysUtils, KM_CommonClassesExt, KromUtils;

function GetJSONCRC(aJSON : TJsonObject) : Cardinal;
var S : TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    aJSON.SaveToStream(S);
    Result := Adler32CRC(S);
  finally
    S.Free;
  end;
end;

function JSONArrToValidArr(aARR : TJsonArray; out aUnits : TKMUnitTypeArray) : Boolean;
var I, J : Integer;
  U : TKMUnitType;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aUnits, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMUnitType>(aARR.S[I],  U) then
    begin
      J := length(aUnits);
      SetLength(aUnits, J + 1);
      aUnits[J] := U;
      Result := true;
    end;

end;

function JSONArrToValidArr(aARR : TJsonArray; out aHouses : TKMHouseTypeArray) : Boolean;
var I, J : Integer;
  H : TKMHouseType;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aHouses, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMHouseType>(aARR.S[I],  H) then
    begin
      J := length(aHouses);
      SetLength(aHouses, J + 1);
      aHouses[J] := H;
      Result := true;
    end;

end;

function JSONArrToValidArr(aARR : TJsonArray; out aWares : TKMWareTypeArray) : Boolean;
var I, J : Integer;
  W : TKMWareType;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aWares, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMWareType>(aARR.S[I],  W) then
    begin
      J := length(aWares);
      SetLength(aWares, J + 1);
      aWares[J] := W;
      Result := true;
    end;
end;

function JSONArrToValidArr(aARR : TJsonArray; out aInt : TIntegerArray) : Boolean;
var I, J : Integer;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aInt, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
  begin
    J := length(aInt);
    SetLength(aInt, J + 1);
    aInt[J] := aArr[I];
    Result := true;
  end;
end;

function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMCardinalArray) : Boolean;
var I, J : Integer;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aInt, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
  begin
    J := length(aInt);
    SetLength(aInt, J + 1);
    aInt[J] := aArr[I];
    Result := true;
  end;
end;

function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMWordArray) : Boolean;
var I, J : Integer;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aInt, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
  begin
    J := length(aInt);
    SetLength(aInt, J + 1);
    aInt[J] := aArr[I];
    Result := true;
  end;
end;

function JSONArrToValidArr(aARR : TJsonArray; out aInt : TKMByteArray) : Boolean;
var I, J : Integer;
begin
  Result := false;
  if aARR.Count > 0 then
    SetLength(aInt, 0)
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
  begin
    J := length(aInt);
    SetLength(aInt, J + 1);
    aInt[J] := aArr[I];
    Result := true;
  end;
end;


function JSONArrToValidSet(aARR : TJsonArray; out aUnits : TKMUnitTypeSet) : Boolean;
var I: Integer;
  U : TKMUnitType;
begin
  Result := false;
  if aARR.Count > 0 then
    aUnits := []
  else
    Exit(false);//don't change the array if nothing was found
  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMUnitType>(aARR.S[I],  U) then
    begin
      aUnits := aUnits + [U];
      Result := true;
    end;

end;

function JSONArrToValidSet(aARR : TJsonArray; out aHouses : TKMHouseTypeSet) : Boolean;
var I : Integer;
  H : TKMHouseType;
begin
  Result := false;
  if aARR.Count > 0 then
    aHouses := []
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMHouseType>(aARR.S[I],  H) then
    begin
      aHouses := aHouses + [H];
      Result := true;
    end;

end;

function JSONArrToValidSet(aARR : TJsonArray; out aWares : TKMWareTypeSet) : Boolean;
var I : Integer;
  W : TKMWareType;
begin
  Result := false;
  if aARR.Count > 0 then
    aWares := []
  else
    Exit(false);//don't change the array if nothing was found

  for I := 0 to aARR.Count - 1 do
    if TKMEnumUtils.TryGetAs<TKMWareType>(aARR.S[I],  W) then
    begin
      aWares := aWares + [W];
      Result := true;
    end;
end;

function JSONToAnim(aJSON : TJsonObject; out aAnim : TKMAnimLoop) : Boolean;
var arr : TIntegerArray;
begin
  Result := false;
  if aJSON.Contains('StepStart') then
  begin
    Result := true;
    aAnim.Create(aJSON.I['MoveX'],aJSON.I['MoveY'],aJSON.I['StepStart'],aJSON.I['Count'],aJSON.I['Offset'],aJSON.B['BackWard'])
  end else
  begin
    arr := [];
    JSONArrToValidArr(aJson.A['Steps'], arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aAnim.Create(aJSON.I['MoveX'],aJSON.I['MoveY'], arr ,aJSON.I['Offset']);
  end;
end;


function JSONToAnim(aJSON : TJsonObject; out aAnim : TKMAnimation) : Boolean;
var arr : TKMWordArray;
begin
  Result := false;
  if aJSON.Contains('StepStart') then
  begin
    Result := true;
    aAnim.Create(aJSON.I['MoveX'],aJSON.I['MoveY'],aJSON.I['StepStart'],aJSON.I['Count'],aJSON.I['Offset'],aJSON.B['BackWard'])
  end else
  begin
    arr := [];
    JSONArrToValidArr(aJson.A['Steps'], arr);
    if length(arr) = 0 then
      Exit;
    Result := true;
    aAnim.Create(aJSON.I['MoveX'],aJSON.I['MoveY'], arr ,aJSON.I['Offset']);
  end;
end;

function JSONToPoint(aJSON : TJsonObject; out aPoint : TKMPoint) : Boolean;
begin
  aPoint := KMPOINT_INVALID_TILE;
  Result := false;
  JSONIfContains(aPoint.X, 'X', aJSON);
  JSONIfContains(aPoint.Y, 'Y', aJSON);
  if aPoint <> KMPOINT_INVALID_TILE then
    Result := true;
end;

function JSONIfContains(var A : Integer; aName : String; aJSON : TJsonObject) : Boolean;
begin
  Result := aJSON.Contains(aName);
  if Result then
    A := aJSON.I[aName];
end;

function JSONIfContains(var A : Word; aName : String; aJSON : TJsonObject) : Boolean;
begin
  Result := aJSON.Contains(aName);
  if Result then
    A := aJSON.I[aName];
end;

function JSONIfContains(var A : Byte; aName : String; aJSON : TJsonObject) : Boolean;
begin
  Result := aJSON.Contains(aName);
  if Result then
    A := aJSON.I[aName];
end;

function JSONIfContains(var A : Boolean; aName : String; aJSON : TJsonObject) : Boolean;
begin
  Result := aJSON.Contains(aName);
  if Result then
    A := aJSON.B[aName];
end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TIntegerArray);
var newCount, I : Integer;
  arr : TIntegerArray;
begin
  aLoadStream.Read(newCount);
  SetLength(arr, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(arr[I]);
  aInt := arr;

end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWordArray);
var newCount, I : Integer;
  arr : TKMWordArray;
begin
  aLoadStream.Read(newCount);
  SetLength(arr, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(arr[I]);
  aInt := arr;

end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMByteArray);
var newCount, I : Integer;
  arr : TKMByteArray;
begin
  aLoadStream.Read(newCount);
  SetLength(arr, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(arr[I]);
  aInt := arr;

end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMCardinalArray);
var newCount, I : Integer;
  arr : TKMCardinalArray;
begin
  aLoadStream.Read(newCount);
  SetLength(arr, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(arr[I]);
  aInt := arr;

end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMUnitTypeArray);
var newCount, I : Integer;
begin
  aLoadStream.Read(newCount);
  SetLength(aInt, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(aInt[I], Sizeof(aInt[I]));
end;


procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMHouseTypeArray);
var newCount, I : Integer;
begin
  aLoadStream.Read(newCount);
  SetLength(aInt, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(aInt[I], Sizeof(aInt[I]));
end;

procedure LoadArrFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWareTypeArray);
var newCount, I : Integer;
begin
  aLoadStream.Read(newCount);
  SetLength(aInt, newCount);
  for I := 0 to newCount - 1 do
    aLoadStream.Read(aInt[I], Sizeof(aInt[I]));
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TIntegerArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I]);
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMWordArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I]);
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMByteArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I]);
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMCardinalArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I]);
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMUnitTypeArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I], SizeOf(aInt[I]));
end;
procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMHouseTypeArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I], SizeOf(aInt[I]));
end;

procedure SaveArrToStream(aSaveStream : TKMEmoryStream; aInt : TKMWareTypeArray);
var newCount, I : Integer;
begin
  newCount := length(aInt);
  aSaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
    aSaveStream.Write(aInt[I], SizeOf(aInt[I]));
end;

procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMUnitTypeSet);
var newCount: Integer;
  UT : TKMUnitType;
begin
  newCount := 0;
  for UT in aInt do
    Inc(newCount);
  aSaveStream.Write(newCount);
  for UT in aInt do
    aSaveStream.Write(UT, SizeOf(UT));
end;

procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMHouseTypeSet);
var newCount: Integer;
  UT : TKMHouseType;
begin
  newCount := 0;
  for UT in aInt do
    Inc(newCount);
  aSaveStream.Write(newCount);
  for UT in aInt do
    aSaveStream.Write(UT, SizeOf(UT));
end;

procedure SaveSetToStream(aSaveStream : TKMEmoryStream; aInt : TKMWareTypeSet);
var newCount: Integer;
  UT : TKMWareType;
begin
  newCount := 0;
  for UT in aInt do
    Inc(newCount);
  aSaveStream.Write(newCount);
  for UT in aInt do
    aSaveStream.Write(UT, SizeOf(UT));
end;

procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMUnitTypeSet);
var newCount, I : Integer;
  UT : TKMUnitType;
begin
  aInt := [];
  aLoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    aLoadStream.Read(UT, Sizeof(UT));
    aInt := aInt + [UT];
  end;
end;

procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMHouseTypeSet);
var newCount, I : Integer;
  UT : TKMHouseType;
begin
  aInt := [];
  aLoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    aLoadStream.Read(UT, Sizeof(UT));
    aInt := aInt + [UT];
  end;
end;

procedure LoadSetFromStream(aLoadStream : TKMEmoryStream; out aInt : TKMWareTypeSet);
var newCount, I : Integer;
  UT : TKMWareType;
begin
  aInt := [];
  aLoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    aLoadStream.Read(UT, Sizeof(UT));
    aInt := aInt + [UT];
  end;
end;

end.

