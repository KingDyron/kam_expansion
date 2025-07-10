unit KM_ResDevelopment;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonTypes,
  JsonDataObjects,
  KM_JsonHelpers;

type
  TKMDevelopmentTreeType = (dttEconomy, dttArmy);

  PKMDevelopment = ^TKMDevelopment;
  TKMDevelopment = record
    HintID : Word;
    X{, Y} : Byte;//grid position
    GuiIcon, ID : Word;
    Parent : PKMDevelopment;
    Next : array of TKMDevelopment;
  end;
  TKMDevelopmentTree = class
    private
      fList : TKMDevelopment;
    public
      function FirstItem : PKMDevelopment;

      procedure LoadFromJson(JSON : TKMJson);
  end;

  TKMDevelopmentTreeCollection = class
    private
      fCRC : Cardinal;
      fTree : array[TKMDevelopmentTreeType] of TKMDevelopmentTree;
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

      procedure SaveTOJson;
  end;

implementation
uses
    SysUtils, Math,
    IOUtils,
    KM_Defaults,
    KM_ResLocales, KM_ResTexts;
const TREE_TYPE_STRING : array[TKMDevelopmentTreeType] of String = ('Economy', 'Army');

function TKMDevelopmentTree.FirstItem: PKMDevelopment;
begin
  Result := @fList;
end;

procedure TKMDevelopmentTree.LoadFromJson(JSON: TKMJson);
var aID : Word;
  procedure CheckForNext(var aTo : TKMDevelopment; aJson: TKMJson);
  var nArr : TKMJsonArray;
    I : Integer;
  begin
    with aTo do
    begin
      fList.ID := aID;
      Inc(aID);
      HintID := aJson.I['HintID'];

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
  aID := 0;
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
    for dtt := Low(TKMDevelopmentTreeType) to High(TKMDevelopmentTreeType) do
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

end;

function TKMDevelopmentTreeCollection.GetText(aID : Integer): String;
begin
  Result := fTexts[aID];
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

    nRoot.Write('HintID', aDev.HintID, true);
    nRoot.Write('GuiIcon', aDev.GuiIcon);
    nRoot.Write('X', aDev.X);
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
      nRoot.WriteObject(TREE_TYPE_STRING[dtt], dtt = dttEconomy);
      SaveDevelopment(fTree[dtt].FirstItem);
      nRoot.EndObject;
    end;

    nRoot.EndFile;

    nRoot.SaveToFile(ExeDir + 'Export' + PathDelim + 'DevelopmentTree.json')
  finally
    nRoot.Free;
  end;
end;

end.
