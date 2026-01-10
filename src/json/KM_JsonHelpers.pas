unit KM_JsonHelpers;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KM_CommonTypes, KM_ResTypes,
  KM_Points, KromUtils,
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
    function GetAnimPack(aName : String; out aValue : TKMAnimationPack) : Boolean;

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
      Procedure Write(aName : String; aValue : TKMByteArray; aIsFirst : Boolean = false);overload;
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

  TKMJsonArrayNew  = class;
  TKMJsonObject = class;
  TKMJsonValueType = (jvtNone, jvtBoolean, jvtInteger, jvtString, jvtSingle, jvtObject, jvtArray);
  TKMJsonValue = record
  private
    procedure AddNewLine(var aText : String);
    procedure AddTabs(var aText : String; aCount : Integer);
  public
    Name,
    Value : Pointer;
    ValueType : TKMJsonValueType;
    procedure SaveToObject(var aText : String; aLeft : Integer; aInOneLine : Boolean);
    procedure SaveToArray(var aText : String; aLeft : Integer; aInOneLine : Boolean);
  end;

  PKMJsonValue = ^TKMJsonValue;

  TKMJsonValueArray = array of TKMJsonValue;

  TKMJsonObject = class
  private
    fCount : Integer;
    fList : TKMJsonValueArray;
    fIsOneLiner : Boolean;
    fName : String;
    procedure AddTabs(var aText : String; aCount : Integer);
    procedure AddToList(var aName, aValue : String; aType : TKMJsonValueType);
    procedure AddObjectToList(aName : String; aObject : TKMJsonObject);
    procedure AddArrayToList(aName : String; aArray : TKMJsonArrayNew);
    procedure AddValue(aName, aValue : String; aType : TKMJsonValueType);
    procedure SaveToText(var aText : String; aLeft : Integer; aInOneLine : Boolean);
    procedure LoadFromText(var aText : String; var aID, aLine : Cardinal);

    function IndexOf(aName : String) : Integer;
    function GetValue(aName : String) : PKMJsonValue;

    function GetI(aName : String) : Integer;
    function GetD(aName : String) : Single;
    function GetS(aName : String) : String;
    function GetB(aName : String) : Boolean;
    function GetO(aName : String) : TKMJsonObject;
    function GetA(aName : String) : TKMJsonArrayNew;

    procedure SetI(aName : String; aValue : Integer);
    procedure SetD(aName : String; aValue : Single);
    procedure SetS(aName : String; aValue : String);
    procedure SetB(aName : String; aValue : Boolean);
    function GetCount : Integer;
  public
    destructor Destroy; override;
    procedure LoadFromFile(aPath : String);
    procedure SaveToFile(aPath : String);
    procedure Add(aName, aValue : String); overload;
    procedure Add(aName : String; aValue : Byte); overload;
    procedure Add(aName : String; aValue : ShortInt); overload;
    procedure Add(aName : String; aValue : Word); overload;
    procedure Add(aName : String; aValue : Integer); overload;
    procedure Add(aName : String; aValue : Cardinal); overload;
    procedure Add(aName : String; aValue : Single; maxDigits : Byte = 10); overload;
    procedure Add(aName : String; aValue : Boolean); overload;
    function AddObject(aName : String; aOneLiner : Boolean = false): TKMJsonObject;
    function AddArray(aName : String; aOneLiner : Boolean = false): TKMJsonArrayNew;

    property I[aName : String] : Integer read GetI write SetI;
    property D[aName : String] : Single read GetD write SetD;
    property S[aName : String] : String read GetS write SetS;
    property B[aName : String] : Boolean read GetB write SetB;
    property O[aName : String] : TKMJsonObject read GetO;
    property A[aName : String] : TKMJsonArrayNew read GetA;

    function GetObject(aName : String; out aObject : TKMJsonObject) : Boolean;
    function GetArray(aName : String; out aArray : TKMJsonArrayNew) : Boolean;

    property Name : String read fName write fName;
    property Count : Integer read GetCount;

  end;

  TKMJsonArrayNew = class
  private
    fCount : Integer;
    fList : TKMJsonValueArray;
    fIsOneLiner : Boolean;
    fName : String;
    procedure AddTabs(var aText : String; aCount : Integer);
    procedure AddToList(aValue : String; aType : TKMJsonValueType);
    procedure AddObjectToList(aObject : TKMJsonObject);
    procedure AddArrayToList(aArray : TKMJsonArrayNew);
    procedure SaveToText(var aText : String; aLeft : Integer; aInOneLine : Boolean);
    procedure LoadFromText(var aText : String; var aID, aLine : Cardinal);

    function GetI(aIndex : Word) : Integer;
    function GetD(aIndex : Word) : Single;
    function GetS(aIndex : Word) : String;
    function GetB(aIndex : Word) : Boolean;
    function GetO(aIndex : Word) : TKMJsonObject;
    function GetA(aIndex : Word) : TKMJsonArrayNew;

    procedure SetI(aIndex : Word; aValue : Integer);
    procedure SetD(aIndex : Word; aValue : Single);
    procedure SetS(aIndex : Word; aValue : String);
    procedure SetB(aIndex : Word; aValue : Boolean);
    function GetCount : Integer;
  public
    destructor Destroy; override;
    procedure Add(aValue : String); overload;
    procedure Add(aValue : Byte); overload;
    procedure Add(aValue : ShortInt); overload;
    procedure Add(aValue : Word); overload;
    procedure Add(aValue : Integer); overload;
    procedure Add(aValue : Cardinal); overload;
    procedure Add(aValue : Single; maxDigits : Byte = 10); overload;
    procedure Add(aValue : Boolean); overload;
    function AddObject(aOneLiner : Boolean = false): TKMJsonObject;
    function AddArray(aOneLiner : Boolean = false): TKMJsonArrayNew;

    property I[aIndex : Word] : Integer read GetI write SetI;
    property D[aIndex : Word] : Single read GetD write SetD;
    property S[aIndex : Word] : String read GetS write SetS;
    property B[aIndex : Word] : Boolean read GetB write SetB;
    property O[aIndex : Word] : TKMJsonObject read GetO;
    property A[aIndex : Word] : TKMJsonArrayNew read GetA;

    property Name : String read fName write fName;
    property Count : Integer read GetCount;
  end;

implementation
uses IOUtils, KM_CommonClassesExt,
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
    json.GetArray('Steps', arr);
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
    json.GetArray('Steps', arr);
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

function TKMJsonHelper.GetAnimPack(aName : String; out aValue: TKMAnimationPack): Boolean;
var arr : TKMJsonArray;
  I : Integer;
  anim : TKMAnimation;
begin
  Result := false;
  arr := self.A[aName];
  If (arr = nil) or (arr.Count = 0) then
    Exit;
  aValue.Count := arr.Count;
  for I := 0 to arr.Count - 1 do
  begin
    arr.O[I].GetAnim(anim);
    aValue[I] := anim;
  end;

  Result := aValue.Count > 0;

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
    FreeAndNil(S);

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
  FreeAndNil(fStringList);
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
var S : String;
  I : Integer;
begin
  S := aValue.ToString(ffNumber, 1, 5);
  for I := 1 to Length(S) do
    If S[I] = ',' then
      S[I] := '.';
  WriteValue(aName, S, aIsFirst);
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

Procedure TKMJsonSaver.Write(aName : String; aValue : TKMByteArray; aIsFirst : Boolean = false);
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
        AddToArray(aValue.Step[I], I = 0);
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

procedure TKMJsonValue.AddNewLine(var aText : String);
begin
  aText := aText + #10;
end;

procedure TKMJsonValue.AddTabs(var aText : String; aCount : Integer);
var I : Integer;
begin
  for I := 1 to aCount do
    aText := aText + #9;
end;

procedure TKMJsonValue.SaveToObject(var aText : String; aLeft : Integer; aInOneLine : Boolean);
var
  s1, s2 : String;
begin

  If not (ValueType in [jvtObject, jvtArray]) then
  begin
    s1 := String(Name);
    s2 := String(Value);
    If not aInOneLine then
    begin
      AddNewLine(aText);
      AddTabs(aText, aLeft);
    end;
  end;

  case ValueType of
    jvtNone,
    jvtBoolean,
    jvtInteger,
    jvtSingle: aText := aText + Format('"%s" : %s', [string(Name), string(Value)]);
    jvtString: aText := aText + Format('"%s" : "%s"', [string(Name), string(Value)]);
    jvtObject: begin
                //AddNewLine(aText);
                //AddTabs(aText, aLeft);
                TKMJsonObject(Value).SaveToText(aText, aLeft, aInOneLine);
               end;
    jvtArray: begin
                //AddNewLine(aText);
                //AddTabs(aText, aLeft);
                TKMJsonArrayNew(Value).SaveToText(aText, aLeft, aInOneLine);
               end;
  end;
end;


procedure TKMJsonValue.SaveToArray(var aText : String; aLeft : Integer; aInOneLine : Boolean);
var
  s1, s2 : String;
begin

  If not (ValueType in [jvtObject, jvtArray]) then
  begin
    s1 := String(Name);
    s2 := String(Value);
    If not aInOneLine then
    begin
      AddNewLine(aText);
      AddTabs(aText, aLeft);
    end;
  end;

  case ValueType of
    jvtNone,
    jvtBoolean,
    jvtInteger,
    jvtSingle: aText := aText + Format('%s', [string(Value)]);
    jvtString: aText := aText + Format('"%s"', [string(Value)]);
    jvtObject: begin
                TKMJsonObject(Value).SaveToText(aText, aLeft, aInOneLine);
               end;
    jvtArray: begin
                TKMJsonArrayNew(Value).SaveToText(aText, aLeft, aInOneLine);
               end;
  end;
end;

//////////////////////////////////////            TKMJsonObject
procedure TKMJsonObject.LoadFromFile(aPath: string);
var S : String;
  StringList : TStringList;
  I, L : Cardinal;
begin
  If not FileExists(aPath) then
    Exit;
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(aPath);
    S := StringList.Text;

    I := 1;
    while SysUtils.CharInSet(S[I], [#9, #10, ' ']) do
    begin
      Inc(I);
    end;
    fName := '';
    L := 1;
    LoadFromText(S, I, L);
  finally

  end;
end;

procedure TKMJsonObject.LoadFromText(var aText: string; var aID, aLine: Cardinal);
var newObj : TKMJsonObject;
  newArray : TKMJsonArrayNew;
  vName : String;
  vValue : String;
  isValueSingle : Boolean;
  nextValueExpected : Boolean;
  procedure SkipComment;
  begin
    while (aText[aID] <> #10) and (aID < length(aText)) do
      Inc(aID);
  end;

  procedure GetName;
  var startID, aCount : Cardinal;
  begin
    Inc(aID);
    aCount := 0;
    startID := aID;
    while (aText[aID] <> '"') and (aID < length(aText)) do
    begin
      Assert(not SysUtils.CharInSet(aText[aID], [#10]), 'Invalid symbols in value name at line : ' + aLine.ToString);
      Inc(aCount);
      Inc(aID);
    end;
    vName := Copy(aText, startID, aCount);
    Assert(length(vName) <> 0, 'name of a parameter is invalid at line : ' + aLine.ToString);
    Inc(aID);
  end;

  procedure GetStringValue;
  var startID, aCount : Cardinal;
  begin
    Inc(aID);
    aCount := 0;
    startID := aID;
    while (aText[aID] <> '"') and (aID < length(aText)) do
    begin
      Inc(aCount);
      If aText[aID] = #10 then
        Inc(aLine);
      Inc(aID);
    end;
    vValue := Copy(aText, startID, aCount);
    Assert(length(vValue) <> 0, 'string value of a parameter is invalid at line : ' + aLine.ToString);
    Inc(aID);
    IF aText[aID] = ',' then
      nextValueExpected := true;
  end;

  procedure GetNumericValue;
  var startID, aCount : Cardinal;
  begin
    aCount := 0;
    startID := aID;
    while SysUtils.CharInSet(aText[aID], ['-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']) and (aID < length(aText)) do
    begin
      Inc(aCount);

      If aText[aID] = '.' then
      begin
        isValueSingle := true;
        aText[aID] := ','; //we have to precorrect it
      end;
      Inc(aID);
    end;
    vValue := Copy(aText, startID, aCount);
    Assert(length(vValue) <> 0, 'numeric value of a parameter is invalid at line : ' + aLine.ToString);
    IF aText[aID] = ',' then
      nextValueExpected := true;
    //Inc(aID);
  end;

  procedure GetBooleanValue;
  var checkString : String;
    I : Integer;
  begin

    if SysUtils.CharInSet(aText[aID], ['f', 'F']) then
      CheckString := 'false'
    else
    if SysUtils.CharInSet(aText[aID], ['t', 'T']) then
      CheckString := 'true';

    for I := 1 to length(CheckString) do
    begin
      Assert(LowerCase(aText[aID]) = CheckString[I], 'Boolean value is invalid at line : ' + aLine.ToString);
      Inc(aID);
    end;
    vValue := CheckString;

    IF aText[aID] = ',' then
      nextValueExpected := true;
  end;

  procedure SkipAfterName;
  var colonFound : Boolean;
  begin
    colonFound := false;

    while SysUtils.CharInSet(aText[aID], [#$D, #10, #9, ' ', ':']) and (aID < length(aText)) do
    begin
      //Assert(aText[aID] <> #10, ': expected but #10 found');
      If aText[aID] = ':' then
      begin
        Assert(not colonFound, 'double " : " found at line : ' + aLine.ToString);
        colonFound := true;
      end;
      If aText[aID] = #10 then
        Inc(aLine);
      Inc(aID);
    end;

  end;

  procedure AddNewValue(aType : TKMJsonValueType);
  begin
    AddToList(vName ,vValue, aType);
    //nextValueExpected := false;
  end;
  procedure AddNewObject;
  begin
    AddObjectToList(vName, newObj);
    //nextValueExpected := false;
  end;
  procedure AddNewArray;
  begin
    AddArrayToList(vName, newArray);
    //nextValueExpected := false;
  end;

  procedure CheckForOneLiner;
  var I, J : integer;
  begin
    I := aID;
    J := 0;
    while (aText[I] <> #10) and (J < 6) do
    begin
      If SysUtils.CharInSet(aText[I], ['"', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'f', 'F', 't', 'T', '[']) then
        Inc(J);
      Inc(I);
    end;
    fIsOneLiner := J > 2;
  end;

var objectFinished : Boolean;
  tmpText : String;
begin
  fCount := 0;
  objectFinished := false;
  nextValueExpected := false;
  Assert(aText[aID] = '{', 'Json doesn''t start with { at line : ' + aLine.ToString);
  CheckForOneLiner;
  tmpText := Copy(aText, aID, length(aText));
  Inc(aID);
  Repeat

    If (aText[aID] = '/') and (aText[aID + 1] = '/') then
      SkipComment
    else
    If (aText[aID] = '"')then
    begin
      If fCount > 0 then
        Assert(nextValueExpected, ', not found after the parameter: "' + vName + '" : ' + vValue + '   at line : ' + aLine.ToString);
      nextValueExpected := false;
      GetName;
      SkipAfterName;
      If aText[aID] = '"' then
      begin
        GetStringValue;
        AddNewValue(jvtString);
      end else
      If SysUtils.CharInSet(aText[aID], ['-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      begin
        isValueSingle := false;
        GetNumericValue;
        If isValueSingle then
          AddNewValue(jvtSingle)
        else
          AddNewValue(jvtInteger);
      end else
      If SysUtils.CharInSet(aText[aID], ['f', 'F', 't', 'T']) then
      begin
        isValueSingle := false;
        GetBooleanValue;
        AddNewValue(jvtBoolean);
      end else
      If aText[aID] = '{' then
      begin
        newObj := TKMJsonObject.Create;
        newObj.Name := vName;
        newObj.LoadFromText(aText, aID, aLine);
        AddNewObject;
        If aText[aID] = ',' then
          nextValueExpected := true;
        //Inc(aID);
      end else
      If aText[aID] = '[' then
      begin
        newArray := TKMJsonArrayNew.Create;
        newArray.Name := vName;
        newArray.LoadFromText(aText, aID, aLine);
        AddNewArray;
        If aText[aID] = ',' then
          nextValueExpected := true;
        //Inc(aID);
      end;
    end else
    If aText[aID] = ',' then
      nextValueExpected := true
    else
      If aText[aID] = #10 then
        Inc(aLine);

    If aText[aID] = '}' then
    begin
      Assert(not nextValueExpected, 'Next parameter expected but found : } at line : ' + aLine.ToString);
      objectFinished := true;
    end;
    Assert(aText[aID] <> ']', 'unexpected ] found at line : ' + aLine.ToString);
    Inc(aID);
  Until (aID > length(aText)) or objectFinished;
end;

procedure TKMJsonObject.SaveToFile(aPath: string);
var S : string;
  stringList : TStringList;
begin
  S := '';
  stringList := TStringList.Create;
  try
    SaveToText(S, 0, fIsOneLiner);
    stringList.Add(S);
    stringList.SaveToFile(aPath);
  finally
    FreeAndNil(stringList);
  end;
end;

destructor TKMJsonObject.Destroy;
var I : integer;
begin
  for I := 0 to High(fList) do
    If fList[I].ValueType in [jvtObject, jvtArray] then
    begin
      FreeAndNil(TObject(fList[I].Value));
    end;
  Inherited;
end;

procedure TKMJsonObject.AddTabs(var aText: string; aCount: Integer);
var I : Integer;
begin
  for I := 1 to aCount do
    aText := aText + #9;
end;

procedure TKMJsonObject.AddToList(var aName, aValue : String; aType : TKMJsonValueType);
begin
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := aName;
  String(fList[fCount - 1].Value) := aValue;
  fList[fCount - 1].ValueType := aType;
end;
procedure TKMJsonObject.AddObjectToList(aName : String; aObject : TKMJsonObject);
begin
  aObject.fName := aName;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := aName;
  fList[fCount - 1].Value := Pointer(aObject);
  fList[fCount - 1].ValueType := jvtObject;
end;
procedure TKMJsonObject.AddArrayToList(aName : String; aArray : TKMJsonArrayNew);
begin
  aArray.fName := aName;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := aName;
  fList[fCount - 1].Value := Pointer(aArray);
  fList[fCount - 1].ValueType := jvtArray;
end;

procedure TKMJsonObject.AddValue(aName: string; aValue: string; aType : TKMJsonValueType);
begin
  If aValue = '' then
    Exit;
  AddToList(
    aName,//fDataList[AddStringToData(aName)],
    aValue,//fDataList[AddStringToData(aValue)],
    aType
    );
end;

procedure TKMJsonObject.Add(aName: string; aValue: string);       begin AddValue(aName, aValue, jvtString); end;
procedure TKMJsonObject.Add(aName : String; aValue : Byte);       begin AddValue(aName, aValue.ToString, jvtInteger); end;
procedure TKMJsonObject.Add(aName : String; aValue : ShortInt);   begin AddValue(aName, aValue.ToString, jvtInteger); end;
procedure TKMJsonObject.Add(aName : String; aValue : Word);       begin AddValue(aName, aValue.ToString, jvtInteger); end;
procedure TKMJsonObject.Add(aName : String; aValue : Integer);    begin AddValue(aName, aValue.ToString, jvtInteger); end;
procedure TKMJsonObject.Add(aName : String; aValue : Cardinal);   begin AddValue(aName, aValue.ToString, jvtInteger); end;

procedure TKMJsonObject.Add(aName : String; aValue : Single; maxDigits : Byte = 10);
var S : String;
  I : Integer;
begin
  S := aValue.ToString(ffGeneral, 1, maxDigits);
  for I := 1 to length(S) do
    If S[I] = ',' then
      S[I] := '.';//we have to precorrect it

  AddValue(aName, S, jvtSingle);
end;

procedure TKMJsonObject.Add(aName : String; aValue : Boolean);    begin AddValue(aName, BoolToStr(aValue, true), jvtBoolean); end;

function TKMJsonObject.AddObject(aName : String; aOneLiner : Boolean = false): TKMJsonObject;
begin
  Result := TKMJsonObject.Create;
  Result.fName := aName;
  Result.fIsOneLiner := aOneLiner;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := aName;
  fList[fCount - 1].Value := Pointer(Result);
  fList[fCount - 1].ValueType := jvtObject;
end;

function TKMJsonObject.AddArray(aName: string; aOneLiner : Boolean = false): TKMJsonArrayNew;
begin
  Result := TKMJsonArrayNew.Create;
  Result.fName := aName;
  Result.fIsOneLiner := aOneLiner;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := aName;
  fList[fCount - 1].Value := Pointer(Result);
  fList[fCount - 1].ValueType := jvtArray;
end;

function TKMJsonObject.IndexOf(aName: string): Integer;
var I : Integer;
begin
  Result := -1;
  for I := 0 to fCount - 1 do
    if aName = String(fList[I].Name) then
      Exit(I);
end;

function TKMJsonObject.GetValue(aName: string): PKMJsonValue;
var I : Integer;
begin
  Result := nil;
  I := IndexOf(aName);
  If I = -1 then
    Exit;
  Result := @fList[I];
end;

function TKMJsonObject.GetI(aName : String) : Integer;
var V : PKMJsonValue;
begin
  Result := 0;
  V := GetValue(aName);
  If V <> nil then
    Assert(TryStrToInt(String(V.Value), Result), 'Value:' + String(V.Value) + '; is not integer');
end;

function TKMJsonObject.GetD(aName : String) : Single;
var V : PKMJsonValue;
begin
  Result := 0;
  V := GetValue(aName);
  If V <> nil then
    Assert(TryStrToFloat(String(V.Value), Result), 'Value:' + String(V.Value) + '; is not single');
end;

function TKMJsonObject.GetS(aName : String) : String;
var V : PKMJsonValue;
begin
  Result := '';
  V := GetValue(aName);
  If V <> nil then
    Result := String(V.Value);
end;
function TKMJsonObject.GetB(aName : String) : Boolean;
var V : PKMJsonValue;
begin
  Result := false;
  V := GetValue(aName);
  If V <> nil then
    Result := String(V.Value) = 'true';
end;

function TKMJsonObject.GetO(aName : String) : TKMJsonObject;
var V : PKMJsonValue;
begin
  Result := nil;
  V := GetValue(aName);
  If V <> nil then
    If TObject(V.Value) is TKMJsonObject then
    Result := TKMJsonObject(V.Value);
end;

function TKMJsonObject.GetA(aName : String) : TKMJsonArrayNew;
var V : PKMJsonValue;
begin
  Result := nil;
  V := GetValue(aName);
  If V <> nil then
    If TObject(V.Value) is TKMJsonArrayNew then
      Result := TKMJsonArrayNew(V.Value);
end;

function TKMJsonObject.GetObject(aName : String; out aObject : TKMJsonObject) : Boolean;
begin
  aObject := GetO(aName);
  Result := aObject <> nil;
end;

function TKMJsonObject.GetArray(aName : String; out aArray : TKMJsonArrayNew) : Boolean;
begin
  aArray := GetA(aName);
  Result := aArray <> nil;
end;

procedure TKMJsonObject.SetI(aName : String; aValue : Integer);
var V : PKMJsonValue;
begin
  V := GetValue(aName);

  If V <> nil then
  begin
    V.ValueType := jvtInteger;
    String(V.Value) := aValue.ToString
  end else
    Add(aName, aValue);
end;

procedure TKMJsonObject.SetD(aName : String; aValue : Single);
var V : PKMJsonValue;
begin
  V := GetValue(aName);
  If V <> nil then
  begin
    V.ValueType := jvtSingle;
    String(V.Value) := aValue.ToString
  end else
    Add(aName, aValue);
end;

procedure TKMJsonObject.SetS(aName : String; aValue : String);
var V : PKMJsonValue;
begin
  V := GetValue(aName);
  If V <> nil then
  begin
    V.ValueType := jvtString;
    String(V.Value) := aValue
  end else
    Add(aName, aValue);
end;

procedure TKMJsonObject.SetB(aName : String; aValue : Boolean);
var V : PKMJsonValue;
begin
  V := GetValue(aName);
  If V <> nil then
  begin
    V.ValueType := jvtBoolean;
    String(V.Value) := aValue.ToString(true)
  end else
    Add(aName, aValue);
end;


function TKMJsonObject.GetCount: Integer;
begin
  If self = nil then
    Exit(0);
  Result := fCount;
end;

procedure TKMJsonObject.SaveToText(var aText : String; aLeft : Integer; aInOneLine : Boolean);
var I : Integer;
begin


  If fName <> '' then
  begin
    If aLeft > 0 then
      If not aInOneLine then
        aText := aText + #10;
    If not aInOneLine then
      AddTabs(aText, aLeft);
    aText := aText + '"' + fName + '":';
  end else
  If aLeft > 0 then
    If not aInOneLine then
    begin
      aText := aText + #10;//make new line and spaces before {
      AddTabs(aText, aLeft);
    end;

  //make new line after name
  If aLeft > 0 then
    If not (aInOneLine or fIsOneLiner) then
    begin
      aText := aText + #10;
      AddTabs(aText, aLeft);
    end;

  aText := aText + '{';

  If fCount > 0 then
  begin
    //save first value
    fList[0].SaveToObject(aText, aLeft + 1, aInOneLine or fIsOneLiner);
    for I := 1 to fCount - 1 do
    begin
      aText := aText + ',';
      fList[I].SaveToObject(aText, aLeft + 1, aInOneLine or fIsOneLiner);
    end;
  end;
  If not (aInOneLine or fIsOneLiner) then
  begin
    aText := aText + #10;
    AddTabs(aText, aLeft);
  end;
  aText := aText + '}';

end;

//////////////////////////////////////            TKMJsonArray

procedure TKMJsonArrayNew.AddTabs(var aText: string; aCount: Integer);
var I : Integer;
begin
  for I := 1 to aCount do
    aText := aText + #9;
end;

procedure TKMJsonArrayNew.AddToList(aValue: string; aType: TKMJsonValueType);
begin
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := '';
  String(fList[fCount - 1].Value) := aValue;
  fList[fCount - 1].ValueType := aType;
end;

procedure TKMJsonArrayNew.AddObjectToList(aObject : TKMJsonObject);
begin
  aObject.fName := '';
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := '';
  fList[fCount - 1].Value := Pointer(aObject);
  fList[fCount - 1].ValueType := jvtObject;
end;
procedure TKMJsonArrayNew.AddArrayToList(aArray : TKMJsonArrayNew);
begin
  aArray.fName := '';
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := '';
  fList[fCount - 1].Value := Pointer(aArray);
  fList[fCount - 1].ValueType := jvtArray;
end;



function TKMJsonArrayNew.GetI(aIndex : Word) : Integer;
begin
  Result := 0;
  If aIndex < fCount then
    Assert(TryStrToInt(String(fList[aIndex].Value), Result), 'Value:' + String(fList[aIndex].Value) + '; is not integer');
end;

function TKMJsonArrayNew.GetD(aIndex : Word) : Single;
begin
  Result := 0;
  If aIndex < fCount then
    Assert(TryStrToFloat(String(fList[aIndex].Value), Result), 'Value:' + String(fList[aIndex].Value) + '; is not Single');
end;
function TKMJsonArrayNew.GetS(aIndex : Word) : String;
begin
  Result := '';
  If aIndex < fCount then
    Result := String(fList[aIndex].Value);
end;
function TKMJsonArrayNew.GetB(aIndex : Word) : Boolean;
begin
  Result := false;
  If aIndex < fCount then
    Result := String(fList[aIndex].Value) = 'true';
end;

function TKMJsonArrayNew.GetO(aIndex : Word) : TKMJsonObject;
begin
  Result := nil;
  If aIndex < fCount then
  begin
    If TObject(fList[aIndex].Value) is TKMJsonObject then
      Result := TKMJsonObject(fList[aIndex].Value);
  end;
end;

function TKMJsonArrayNew.GetA(aIndex : Word) : TKMJsonArrayNew;
begin
  Result := nil;
  If aIndex < fCount then
  begin
    If TObject(fList[aIndex].Value) is TKMJsonArrayNew then
      Result := TKMJsonArrayNew(fList[aIndex].Value);
  end;
end;

procedure TKMJsonArrayNew.SetI(aIndex : Word; aValue : Integer);
begin
  If aIndex < fCount then
  begin
    fList[aIndex].ValueType := jvtInteger;
    String(fList[aIndex].Value) := aValue.ToString;
  end else
    Add(aValue);
end;

procedure TKMJsonArrayNew.SetD(aIndex : Word; aValue : Single);
begin
  If aIndex < fCount then
  begin
    fList[aIndex].ValueType := jvtSingle;
    String(fList[aIndex].Value) := aValue.ToString;
  end else
    Add(aValue);
end;

procedure TKMJsonArrayNew.SetS(aIndex : Word; aValue : String);
begin
  If aIndex < fCount then
  begin
    fList[aIndex].ValueType := jvtString;
    String(fList[aIndex].Value) := aValue;
  end else
    Add(aValue);
end;
procedure TKMJsonArrayNew.SetB(aIndex : Word; aValue : Boolean);
begin
  If aIndex < fCount then
  begin
    fList[aIndex].ValueType := jvtBoolean;
    String(fList[aIndex].Value) := aValue.ToString(true);
  end else
    Add(aValue);
end;

function TKMJsonArrayNew.GetCount: Integer;
begin
  If self = nil then
    Exit(0);
  Result := fCount;
end;

destructor TKMJsonArrayNew.Destroy;
var I : integer;
begin
  for I := 0 to High(fList) do
    If fList[I].ValueType in [jvtObject, jvtArray] then
    begin
      FreeAndNil(TObject(fList[I].Value));
    end;
  Inherited;
end;
procedure TKMJsonArrayNew.Add(aValue : String);     begin AddToList(aValue, jvtString); end;
procedure TKMJsonArrayNew.Add(aValue : Byte);       begin AddToList(aValue.ToString, jvtInteger); end;
procedure TKMJsonArrayNew.Add(aValue : ShortInt);   begin AddToList(aValue.ToString, jvtInteger); end;
procedure TKMJsonArrayNew.Add(aValue : Word);       begin AddToList(aValue.ToString, jvtInteger); end;
procedure TKMJsonArrayNew.Add(aValue : Integer);    begin AddToList(aValue.ToString, jvtInteger); end;
procedure TKMJsonArrayNew.Add(aValue : Cardinal);   begin AddToList(aValue.ToString, jvtInteger); end;
procedure TKMJsonArrayNew.Add(aValue : Single; maxDigits : Byte = 10);
var S : String;
  I : Integer;
begin
  S := aValue.ToString(ffGeneral, 1, maxDigits);
  for I := 1 to length(S) do
    If S[I] = ',' then
      S[I] := '.';//we have to precorrect it

  AddToList(S, jvtSingle);
end;
procedure TKMJsonArrayNew.Add(aValue : Boolean);    begin AddToList(aValue.ToString, jvtBoolean); end;

function TKMJsonArrayNew.AddObject(aOneLiner : Boolean = false): TKMJsonObject;
begin
  Result := TKMJsonObject.Create;
  Result.Name := '';
  Result.fIsOneLiner := aOneLiner;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := '';
  fList[fCount - 1].Value := Pointer(Result);
  fList[fCount - 1].ValueType := jvtObject;
end;

function TKMJsonArrayNew.AddArray(aOneLiner : Boolean = false): TKMJsonArrayNew;
begin
  Result := TKMJsonArrayNew.Create;
  Result.Name := '';
  Result.fIsOneLiner := aOneLiner;
  Inc(fCount);
  If fCount > length(fList) then
    SetLength(fList, fCount + 10);
  String(fList[fCount - 1].Name) := '';
  fList[fCount - 1].Value := Pointer(Result);
  fList[fCount - 1].ValueType := jvtArray;
end;


procedure TKMJsonArrayNew.SaveToText(var aText: string; aLeft: Integer; aInOneLine: Boolean);
var I : Integer;
begin
  //make new line before name
  If aLeft > 0 then
    If not aInOneLine then
      aText := aText + #10;
  If not aInOneLine then
    AddTabs(aText, aLeft);

  If fName <> '' then
    aText := aText + '"' + fName + '":';
    //make new line after name
  If aLeft > 0 then
    If not (aInOneLine or fIsOneLiner) then
    begin
      aText := aText + #10;
      AddTabs(aText, aLeft);
    end;

  aText := aText + '[';

  If fCount > 0 then
  begin
    //save first value
    fList[0].SaveToArray(aText, aLeft + 1, aInOneLine or fIsOneLiner);
    for I := 1 to fCount - 1 do
    begin
      aText := aText + ',';
      fList[I].SaveToArray(aText, aLeft + 1, aInOneLine or fIsOneLiner);
    end;
  end;
  If not (aInOneLine or fIsOneLiner) then
  begin
    aText := aText + #10;
    AddTabs(aText, aLeft);
  end;
  aText := aText + ']';
end;

procedure TKMJsonArrayNew.LoadFromText(var aText: string; var aID, aLine: Cardinal);
var newObj : TKMJsonObject;
  newArray : TKMJsonArrayNew;
  vValue : String;
  isValueSingle : Boolean;
  nextValueExpected : Boolean;

  function CkeckChar(aChar : Char; aSet : TSetOfAnsiChar) : Boolean;
  begin
    Result := CharInSet(aChar, aSet);
  end;

  procedure SkipComment;
  begin
    while (aText[aID] <> #10) and (aID < length(aText)) do
      Inc(aID);
  end;

  procedure GetStringValue;
  var startID, aCount : Cardinal;
  begin
    Inc(aID);
    aCount := 0;
    startID := aID;
    while (aText[aID] <> '"') and (aID < length(aText)) do
    begin
      Inc(aCount);
      If aText[aID] = #10 then
        Inc(aLine);
      Inc(aID);
    end;
    vValue := Copy(aText, startID, aCount);
    Assert(length(vValue) <> 0, 'string value of a parameter is invalid at line : ' + aLine.ToString);
    Inc(aID);
    IF aText[aID] = ',' then
      nextValueExpected := true;
  end;

  procedure GetNumericValue;
  var startID, aCount : Cardinal;
  begin
    aCount := 0;
    startID := aID;
    while CkeckChar(aText[aID], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']) and (aID < length(aText)) do
    begin
      Inc(aCount);
      If aText[aID] = '.' then
        isValueSingle := true;
      Inc(aID);
    end;
    vValue := Copy(aText, startID, aCount);
    Assert(length(vValue) <> 0, 'numeric value of a parameter is invalid at line : ' + aLine.ToString);
    IF aText[aID] = ',' then
      nextValueExpected := true;
    //Inc(aID);
  end;

  procedure GetBooleanValue;
  var checkString : String;
    I : Integer;
  begin

    if SysUtils.CharInSet(aText[aID], ['f', 'F']) then
      CheckString := 'false'
    else
    if SysUtils.CharInSet(aText[aID], ['t', 'T']) then
      CheckString := 'true';

    for I := 1 to length(CheckString) do
    begin
      Assert(LowerCase(aText[aID]) = CheckString[I], 'Boolean value is invalid at line : ' + aLine.ToString);
      Inc(aID);
    end;
    vValue := CheckString;

    IF aText[aID] = ',' then
      nextValueExpected := true;
  end;

  procedure TrySkipEmpty;
  var colonFound : Boolean;
  begin
    colonFound := false;
    while SysUtils.CharInSet(aText[aID], [#$D, #10, #9, ' ']) and (aID < length(aText)) do
    begin
      //Assert(aText[aID] <> #10, ': expected but #10 found');
      If aText[aID] = ':' then
      begin
        Assert(not colonFound, 'double " : " found at line : ' + aLine.ToString);
        colonFound := true;
      end;
      If aText[aID] = #10 then
        Inc(aLine);
      Inc(aID);
    end;
  end;

  procedure AddNewValue(aType : TKMJsonValueType);
  begin
    AddToList(vValue, aType);
    //nextValueExpected := false;
  end;
  procedure AddNewObject;
  begin
    AddObjectToList(newObj);
    //nextValueExpected := false;
  end;
  procedure AddNewArray;
  begin
    AddArrayToList(newArray);
    //nextValueExpected := false;
  end;

  procedure CheckForOneLiner;
  var I, J : integer;
  begin
    I := aID;
    J := 0;
    while (aText[I] <> #10) and (J < 6) do
    begin
      If SysUtils.CharInSet(aText[I], ['"', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'f', 'F', 't', 'T', '{']) then
        Inc(J);
      Inc(I);
    end;
    fIsOneLiner := J > 2;
  end;


var arrayFinished : Boolean;
begin
  fCount := 0;
  arrayFinished := false;
  nextValueExpected := false;
  Assert(aText[aID] = '[', 'Json array doesn''t start with [ at line : ' + aLine.ToString);
  CheckForOneLiner;
  Inc(aID);
  Repeat

    If (aText[aID] = '/') and (aText[aID + 1] = '/') then
      SkipComment;

    TrySkipEmpty;
    If SysUtils.CharInSet(aText[aID], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'f', 'F', 't', 'T', '"', '{', '[']) then
    begin
      if fCount > 0 then
        Assert( nextValueExpected, 'next value expected in array: ' + fName +  ' at line : ' + aLine.ToString);
    end;
    nextValueExpected := false;
    If aText[aID] = '"' then
    begin
      GetStringValue;
      AddNewValue(jvtString);
    end else
    If SysUtils.CharInSet(aText[aID], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
    begin
      isValueSingle := false;
      GetNumericValue;
      If isValueSingle then
        AddNewValue(jvtSingle)
      else
        AddNewValue(jvtInteger);
    end else
    If SysUtils.CharInSet(aText[aID], ['f', 'F', 't', 'T']) then
    begin
      GetBooleanValue;
      AddNewValue(jvtBoolean);
    end else
    If aText[aID] = '{' then
    begin
      newObj := TKMJsonObject.Create;
      //newObj.Name := self.Name;
      newObj.LoadFromText(aText, aID, aLine);
      AddNewObject;
      If aText[aID] = ',' then
        nextValueExpected := true;
      //Inc(aText[aID]);
    end else
    If aText[aID] = '[' then
    begin
      newArray := TKMJsonArrayNew.Create;
      //newArray.Name := self.Name;
      newArray.LoadFromText(aText, aID, aLine);
      AddNewArray;
      If aText[aID] = ',' then
        nextValueExpected := true;
      //Inc(aText[aID]);
    end else
    If aText[aID] = #10 then
      Inc(aLine);

    If aText[aID] = ']' then
    begin
      Assert(not nextValueExpected, 'Next value expected in json array but found : } at line : ' + aLine.ToString);
      arrayFinished := true;
    end;
    Assert(aText[aID] <> '}', 'unexpected } found at line : ' + aLine.ToString);
    Inc(aID);
  Until (aID > length(aText)) or arrayFinished;
end;

end.
