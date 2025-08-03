unit KM_ResDevelopment;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonTypes,
  JsonDataObjects,
  KM_JsonHelpers;

type
  TKMDevelopmentTreeType = (dttNone, dttBuilder, dttEconomy, dttArmy, dttAll);

const
  DEVELOPMENT_MIN = dttBuilder;
  DEVELOPMENT_MAX = dttArmy;
  DEVELOPMENT_MAX_ALL = dttAll;
  DEVELOPMENT_VALID = [DEVELOPMENT_MIN..DEVELOPMENT_MAX];
  DEVELOPMENT_COUNT = byte(DEVELOPMENT_MAX) - byte(DEVELOPMENT_MIN) + 1;

type
  PKMDevelopment = ^TKMDevelopment;
  TKMDevelopment = record
    HintID : Word;
    X{, Y} : Byte;//grid position
    GuiIcon, ID : Word;
    Parent : PKMDevelopment;
    Cost : Byte;
    Next : array of TKMDevelopment;
    function IndexOf(aDev : PKMDevelopment) : Integer;
    function AddNext(aId : Word; aX : Byte): PKMDevelopment;
    function RemNext(aIndex : Integer): Boolean;overload;
    function RemNext(aDev : PKMDevelopment): Boolean;overload;
  end;
  TKMDevelopmentTree = class
    private
      fLastID : Word;
      fList : TKMDevelopment;
      function GetCount : Word;
    public
      function FirstItem : PKMDevelopment;
      function GetNewId : Word;
      function GetItem(aID : Integer) : PKMDevelopment;

      property Count : Word read GetCount;
      procedure LoadFromJson(JSON : TKMJson);
  end;

  TKMDevelopmentTreeCollection = class
    private
      fCRC : Cardinal;
      fTree : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMDevelopmentTree;
      fTextCount : Word;
      fTexts : array of String;
      procedure AddText(aText : String);

      function GetTree(aType : TKMDevelopmentTreeType) : TKMDevelopmentTree;
    public
      constructor Create;
      destructor Destroy; override;
      property Tree[aType : TKMDevelopmentTreeType] : TKMDevelopmentTree read GetTree; default;
      property CRC : Cardinal read fCRC;
      procedure LoadFromJson(aPath : String);
      procedure ReloadTexts;
      function GetText(aID : Integer): String;

      function GetAllTakenAtY(aType :TKMDevelopmentTreeType; aY: Byte): TKMByteArray;

      procedure SaveToJson;
  end;
const
  TREE_TYPE_ICON : array[TKMDevelopmentTreeType] of Word = (0, 1079, 1077, 1080, 1081);
  TREE_TYPE_STRING : array[TKMDevelopmentTreeType] of String = ('None', 'Builder', 'Economy', 'Army', 'ALL');
  TREE_TYPE_HINT : array[TKMDevelopmentTreeType] of Word = (0, 2297, 2298, 2299, 1060);

implementation
uses
    SysUtils, Math,
    IOUtils,
    KM_Defaults,
    KM_ResLocales, KM_ResTexts;

function TKMDevelopmentTree.GetCount: Word;
begin
  Result := fLastID + 1;
end;

function TKMDevelopmentTree.FirstItem: PKMDevelopment;
begin
  Result := @fList;
end;

function TKMDevelopmentTree.GetItem(aID: Integer): PKMDevelopment;
  procedure Find(aDev : PKMDevelopment);
  var I : integer;
  begin
    If Result <> nil then
      Exit;

    If aDev.ID = aID then
    begin
      Result := aDev;
      Exit;
    end;
    for I := 0 to High(aDev.Next) do
      Find(@aDev.Next[I]);

  end;

begin
  Result := nil;
  Find(FirstItem);
end;

function TKMDevelopmentTree.GetNewId: Word;
begin
  Inc(fLastId);
  Result := fLastId;
end;

procedure TKMDevelopmentTree.LoadFromJson(JSON: TKMJson);
//var aID : Word;
  procedure CheckForNext(var aTo : TKMDevelopment; aJson: TKMJson);
  var nArr : TKMJsonArray;
    I : Integer;
  begin
    with aTo do
    begin
      //ID := aID;
      //Inc(aID);
      ID := aJson.I['ID'];
      fLastID := Max(ID, fLastID);
      HintID := aJson.I['HintID'];
      Cost := aJson.I['Cost'];
      Cost := max(Cost, 1);

      X := aJson.I['X'];
      //Y := aJson.I['Y'];
      GuiIcon := aJson.I['GuiIcon'];
      Parent := nil;

      nArr := aJson.A['Next'];
      SetLength(Next, Min(nArr.Count, 5));
      for I := 0 to High(Next) do
      begin
        CheckForNext(Next[I], nArr.O[I]);
        Next[I].Parent := @aTo;
      end;

    end;
  end;

begin
  //aID := 0;
  {fList.TextID := JSON.I['TextID'];
  fList.X := JSON.I['X'];
  fList.Y := JSON.I['Y'];
  fList.GuiIcon := JSON.I['GuiIcon'];
  fList.Parent := nil;}
  CheckForNext(fList, JSON);
end;

constructor TKMDevelopmentTreeCollection.Create;
var dtt: TKMDevelopmentTreeType;
begin
  for dtt := Low(fTree) to High(fTree) do
    fTree[dtt] := TKMDevelopmentTree.Create;
  ReloadTexts;
end;

destructor TKMDevelopmentTreeCollection.Destroy;
var dtt: TKMDevelopmentTreeType;
begin
  for dtt := Low(fTree) to High(fTree) do
    fTree[dtt].Free;
  Inherited;
end;

function TKMDevelopmentTreeCollection.GetTree(aType: TKMDevelopmentTreeType): TKMDevelopmentTree;
begin
  Result := fTree[aType];
end;

procedure TKMDevelopmentTreeCollection.LoadFromJson(aPath : String);
var Root : TKMJson;
var dtt: TKMDevelopmentTreeType;
begin
  If not FileExists(aPath) then
    Exit;
  Root := TJsonObject.ParseFromFile(aPath) as TKMJson;
  try
    fCRC := Root.Crc;
    for dtt := Low(fTree) to High(fTree) do
      If Root.Contains(TREE_TYPE_STRING[dtt]) then
        fTree[dtt].LoadFromJson(root.O[TREE_TYPE_STRING[dtt]]);
  finally
    Root.Free;
  end;

end;

procedure TKMDevelopmentTreeCollection.AddText(aText: string);
begin
  SetLength(fTexts, length(fTexts) + 1);
  fTexts[high(fTexts)] := aText;
end;

procedure TKMDevelopmentTreeCollection.ReloadTexts;
var S : String;
  I, K, count : Integer;
  skip : Boolean;
begin
  //make Texts
  SetLength(fTexts, 0);
  S := gResTexts[2289];
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
  fTextCount := Length(fTexts);
end;

function TKMDevelopmentTreeCollection.GetText(aID : Integer): String;
begin
  If aId < fTextCount then
    Result := fTexts[aID]
  else
    Result := 'Unknown text : ' + aID.ToString;
end;

function TKMDevelopmentTreeCollection.GetAllTakenAtY(aType :TKMDevelopmentTreeType; aY: Byte): TKMByteArray;
  procedure CheckNext(aDev :PKMDevelopment; aTop : Byte);
  var I : Integer;
  begin
    If aTop = aY then
    begin
      SetLength(Result, length(Result) + 1);
      Result[high(Result)] := aDev.X;
    end;

    If aTop + 1 <= aY then
      for I := 0 to High(aDev.Next) do
        CheckNext(@aDev.Next[I], aTop + 1);
  end;

begin
  SetLength(Result, 0);
  CheckNext(Tree[aType].FirstItem, 0);
end;

procedure TKMDevelopmentTreeCollection.SaveTOJson;
var nRoot : TKMJsonSaver;
  procedure SaveDevelopment(aDev : PKMDevelopment);
  var I : Integer;
  begin

    nRoot.Write('ID', aDev.ID, true);
    nRoot.Write('HintID', aDev.HintID);
    nRoot.Write('GuiIcon', aDev.GuiIcon);
    nRoot.Write('X', aDev.X);
    nRoot.Write('Cost', aDev.Cost);
    If length(aDev.Next) > 0 then
    begin
      nRoot.WriteArray('Next');

      for I := 0 to High(aDev.Next) do
      begin
        nRoot.WriteObject('', I = 0);
          SaveDevelopment(@aDev.Next[I]);
        nRoot.EndObject;
      end;
      nRoot.EndArray;
    end;
  end;
var dtt : TKMDevelopmentTreeType;
begin
  nRoot := TKMJsonSaver.Create;
  try
    nRoot.BeginFile;

    for dtt := Low(fTree) to High(fTree) do
    begin
      nRoot.WriteObject(TREE_TYPE_STRING[dtt], dtt = Low(fTree));
      SaveDevelopment(fTree[dtt].FirstItem);
      nRoot.EndObject;
    end;

    nRoot.EndFile;

    nRoot.SaveToFile(ExeDir + 'Export' + PathDelim + 'DevelopmentTree.json')
  finally
    nRoot.Free;
  end;
end;

function TKMDevelopment.IndexOf(aDev: PKMDevelopment): Integer;
var I : Integer;
begin
  Result := -1;
  for I := 0 to High(Next) do
    If @Next[I] = aDev  then
      Exit(I);
end;

function TKMDevelopment.AddNext(aId : Word; aX : Byte): PKMDevelopment;
var I : Integer;
begin
  I := length(Next);
  SetLength(Next, I + 1);
  Next[I].Parent := @self;
  Result := @Next[I];
  //Result.Parent := @self;
  Result.X := aX;
  Result.ID := aID;
end;

function TKMDevelopment.RemNext(aIndex: Integer): Boolean;
begin
  Result := false;
  Next[aIndex] := Next[high(Next)];
  Setlength(Next, high(Next));
end;

function TKMDevelopment.RemNext(aDev: PKMDevelopment): Boolean;
var I : Integer;
begin
  Result := false;
  I := IndexOf(aDev);
  If I = -1 then
    Exit;
  Result := RemNext(I);
end;

end.
