unit KM_JsonData;
{$I KaM_Remake.inc}
interface
uses
  Classes, SyncObjs, KM_GameInfo, KM_Defaults, SysUtils, KM_CommonTypes,
  KM_JsonHelpers;


type
  TKMJsonData = class
  private
    fList: array of TKMJsonRec;
    fBattleTutorials,
    fTutorials : array of String;//paths for all tutorial maps
    procedure CollectFoldersInPath(const aFolder: string; aFileList: TStringList);

    function GetDefaultPath(aType: TKMDirType) : string;
    function GetTutorialName(aIndex: Integer) : String;
    function GetBattleTutorialName(aIndex: Integer) : String;
    procedure LoadTutorials;
    procedure FindJsonFiles;

    function GetJsonObject(aName : String) : TKMJson;
  public
    constructor Create;
    destructor Destroy; Override;

    property Path[aType : TKMDirType] : String read GetDefaultPath; default;
    property Path[aName : String] : TKMJson read GetJsonObject; default;

    function Animation(aName : String) : TKMAnimation;

    property Tutorial[aIndex : Integer] : String read GetTutorialName;
    property BattleTutorial[aIndex : Integer] : String read GetBattleTutorialName;
    function GetTutorialPath(aIndex : Integer) : String;
    function GetBattleTutorialPath(aIndex : Integer) : String;
    Function TutorialCount : Byte;
    Function BattleTutorialCount : Byte;

    procedure RefreshTutorials;
    procedure Reload;
  end;

const
  //default paths for files
  DEFAULT_JSON_PATH = 'data' + PathDelim + 'defines' +PathDelim ;
  SAVED_PATHS_FILE = 'paths.txt';
  OLD_HOUSES_FILE = 'old_houses';
  NEW_HOUSES_FILE = 'new_houses';
  WARES_FILE = 'wares';
  UNITS_FILE = 'units';
  MODDING_GRAPHICS = 'Modding graphics';

const FILE_NAMES : array[TKMDirType] of String =
  (
    OLD_HOUSES_FILE + '.json',
    NEW_HOUSES_FILE + '.json',
    UNITS_FILE + '.json',
    WARES_FILE + '.json',
    MODDING_GRAPHICS
  );

implementation
uses
  Math,
  KM_Resource,
  KM_Log,
  IOUtils;


constructor TKMJsonData.Create;
begin
  inherited Create;
  LoadTutorials;
  FindJsonFiles;
end;

destructor TKMJsonData.Destroy;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].Json.Free;
  Inherited;

end;

function TKMJsonData.GetJsonObject(aName: string): TKMJson;
var I : integer;
begin
  Result := nil;
  for I := 0 to high(fList) do
    if fList[I].Name = aName then
      Exit(fList[I].Json);
end;

procedure TKMJsonData.CollectFoldersInPath(const aFolder: string; aFileList: TStringList);
var
  filePath: string;
begin
  if not DirectoryExists(aFolder) then
    Exit;

  for filePath in TDirectory.GetDirectories(aFolder) do
    aFileList.Add(ExtractRelativePath(aFolder, filePath));
end;

procedure TKMJsonData.FindJsonFiles;
var path : String;
  I : integer;
begin
  for I := 0 to High(fList) do
    fList[I].Json.Free;

  SetLength(fList, 0);
  for path in TDirectory.GetFiles(ExeDir + DEFAULT_JSON_PATH) do
    if TPath.GetExtension(path) = '.json' then
    begin
      SetLength(fList, length(fList) + 1);
      fList[high(fList)].Path := path;
      fList[high(fList)].Name := TPath.GetFileNameWithoutExtension(path);
      fList[high(fList)].Json := TKMJson.Create(path);
    end;


end;

function TKMJsonData.GetDefaultPath(aType: TKMDirType) : string;
begin
  if aType = dtModding then
    Result := ExeDir + FILE_NAMES[aType]
  else
    Result := ExeDir + DEFAULT_JSON_PATH + FILE_NAMES[aType];
end;

procedure TKMJsonData.LoadTutorials;
var fileList : TStringList;
  I : Integer;
  S : String;
begin
  fileList := TStringList.Create;

  CollectFoldersInPath(ExeDir + TUTORIALS_FOLDER_NAME, fileList);
  SetLength(fTutorials, 0);
  for I := 0 to fileList.Count - 1 do
  begin
    S :=  TPath.GetFileName(fileList[I]);
    S := fileList[I] + PathDelim + S + '.dat';
    if FileExists(S) then
    begin
      SetLength(fTutorials, length(fTutorials) + 1);
      fTutorials[high(fTutorials)] := S;
    end;
  end;
  fileList.Clear;
  CollectFoldersInPath(ExeDir + BATTLE_TUTORIALS_FOLDER_NAME, fileList);
  SetLength(fBattleTutorials, 0);
  for I := 0 to fileList.Count - 1 do
  begin
    S :=  TPath.GetFileName(fileList[I]);
    S := fileList[I] + PathDelim + S + '.dat';
    if FileExists(S) then
    begin
      SetLength(fBattleTutorials, length(fBattleTutorials) + 1);
      fBattleTutorials[high(fBattleTutorials)] := S;
    end;
  end;

  fileList.Free;
end;

function TKMJsonData.Animation(aName: string): TKMAnimation;
begin
  GetJsonObject('animations').GetAnim(aName, Result);
end;


function TKMJsonData.GetTutorialName(aIndex: Integer): string;
begin
  Assert(InRange(aIndex, 0, high(fTutorials)));
  Result := Copy(TPath.GetFileName(fTutorials[aIndex]), 1, Length(TPath.GetFileName(fTutorials[aIndex])) - 4);
end;

function TKMJsonData.GetTutorialPath(aIndex: Integer): string;
begin
  Assert(InRange(aIndex, 0, high(fTutorials)));
  Result := fTutorials[aIndex];
end;

function TKMJsonData.TutorialCount: Byte;
begin
  Result := length(fTutorials);
end;

procedure TKMJsonData.RefreshTutorials;
begin
  LoadTutorials;
end;

function TKMJsonData.GetBattleTutorialName(aIndex: Integer): string;
begin
  Assert(InRange(aIndex, 0, high(fBattleTutorials)));
  Result := Copy(TPath.GetFileName(fBattleTutorials[aIndex]), 1, Length(TPath.GetFileName(fBattleTutorials[aIndex])) - 4);
end;

function TKMJsonData.GetBattleTutorialPath(aIndex: Integer): string;
begin
  Assert(InRange(aIndex, 0, high(fBattleTutorials)));
  Result := fBattleTutorials[aIndex];
end;

function TKMJsonData.BattleTutorialCount: Byte;
begin
  Result := length(fBattleTutorials);
end;


procedure TKMJsonData.Reload;
begin
  FindJsonFiles;
end;


end.
