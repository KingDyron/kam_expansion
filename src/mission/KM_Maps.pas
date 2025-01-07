unit KM_Maps;
{$I KaM_Remake.inc}
interface
uses
  Classes, SyncObjs,
  KM_MapTypes,
  KM_CommonTypes, KM_CommonClasses, KM_Defaults, KM_Pics, KM_ResTexts, KM_Points,
  KM_CampaignTypes;


type
  TKMapsSortMethod = (
    smByFavouriteAsc, smByFavouriteDesc,
    smByNameAsc, smByNameDesc,
    smBySizeAsc, smBySizeDesc,
    smByPlayersAsc, smByPlayersDesc,
    smByHumanPlayersAsc, smByHumanPlayersDesc,
    smByHumanPlayersMPAsc, smByHumanPlayersMPDesc,
    smByMissionModeAsc, smByMissionModeDesc);

  TKMMapInfo = class;
  TKMapEvent = procedure (aMap: TKMMapInfo) of object;
  TKMMapInfoAmount = (iaBase, iaExtra);

  TKMMapGoalInfo = packed record
    Cond: TKMGoalCondition;
    Play: TKMHandID;
    Stat: TKMGoalStatus;
  end;

  // Additional map data from TXT file
  TKMMapTxtInfo = class
  private
    // Saved
    // Desc fixed string in the .txt file. Should be saved
    fSmallDesc: UnicodeString;
    fBigDesc: UnicodeString;
    // Desc LibxID. Should be saved
    fSmallDescLibx: Integer;
    fBigDescLibx: Integer;
    fNameLibx: Integer;

    // Not saved
    // Translated text, which we load by LibxID. Should not be saved
    fSmallDescTranslated: UnicodeString;
    fBigDescTranslated: UnicodeString;
    fNameTranslated: UnicodeString;

    fBlockColorSelection: Boolean;
    function IsEmpty: Boolean;
    function GetBlockColorSelection: Boolean;

    procedure NormalizeDesc;

    procedure SetSmallDesc(const aSmallDesc: UnicodeString);
    procedure SetBigDesc(const aBigDesc: UnicodeString);

    function GetSmallDescSanitized: UnicodeString;
    function GetBigDescToDisplay: UnicodeString;
    function GetNameToDisplay: UnicodeString;
  public
    Author: UnicodeString;
    Version: UnicodeString;
    IsCoop: Boolean; // Some multiplayer missions are defined as coop
    IsSpecial: Boolean; // Some missions are defined as special (e.g. tower defence, quest, etc.)
    IsRMG: Boolean; // Missions that were generated via Random Map Generator
    IsPlayableAsSP: Boolean; // Is MP map playable as SP map ?

    DifficultyLevels: TKMMissionDifficultySet;

    BlockTeamSelection: Boolean;
    BlockPeacetime: Boolean;
    BlockFullMapPreview: Boolean;
    Weather : TKMSettingsWeather;

    constructor Create;

    // SmallDesc can be set as text or Libx index by the mapmaker
    // Since the data is in the text, we can't forbid the mapmaker to use EOLs and other unsupported symbols
    // Hence, read and let edit what is there, but sanitize for the display in the SP table
    property SmallDesc: UnicodeString read fSmallDesc write SetSmallDesc;
    property SmallDescLibx: Integer read fSmallDescLibx;
    procedure SetSmallDescLibxAndTranslation(aSmallDescLibx: Integer; aTranslation: UnicodeString);
    property SmallDescSanitized: UnicodeString read GetSmallDescSanitized;

    property BigDesc: UnicodeString write SetBigDesc;
    property BigDescLibx: Integer read fBigDescLibx;
    property NameLibx: Integer read fNameLibx;
    procedure SetBigDescLibxAndTranslation(aBigDescLibx: Integer; aTranslation: UnicodeString);
    procedure SetNameLibxAndTranslation(aNameLibx: Integer; aTranslation: UnicodeString);
    property BigDescToDisplay: UnicodeString read GetBigDescToDisplay;
    property NameToDisplay: UnicodeString read GetNameToDisplay;

    procedure Load(LoadStream: TKMemoryStream);
    procedure Save(SaveStream: TKMemoryStream);

    function IsSmallDescLibxSet: Boolean;
    function IsBigDescLibxSet: Boolean;

    function CanAddDefaultGoals: Boolean;

    procedure ResetInfo;

    procedure SaveTXTInfo(const aFilePath: String);
    procedure LoadTXTInfo(const aFilePath: String);
    function HasDifficultyLevels: Boolean;

    property BlockColorSelection: Boolean read GetBlockColorSelection write fBlockColorSelection;
  end;


  TKMMapTxtInfoArray = array of TKMMapTxtInfo;


  TKMMapInfo = class
  private
    fDir: String;
    fName: UnicodeString; //without extension
    fCRC: Cardinal;
    fDatCRC: Cardinal; //Used to speed up scanning
    fMapAndDatCRC: Cardinal; //Used to determine map by its .map + .dat files, ignoring other map data (.txt and .script)
    fVersion: AnsiString; //Savegame version, yet unused in maps, they always have actual version
    fInfoAmount: TKMMapInfoAmount;
    fKind: TKMMapKind; // SP / MP / DL
    fTxtInfo: TKMMapTxtInfo;
    fSize: TKMMapSize;
    fSizeText: String;
    fCustomScriptParams: TKMCustomScriptParamDataArray;

    fCampaignID : TKMCampaignId;
    fCampaignMission : Byte;

    procedure ResetInfo;
    procedure LoadFromStreamObj(aStreamObj: TObject; const aPath: UnicodeString);
    procedure SaveToStreamObj(aStreamObj: TObject; const aPath: UnicodeString);
    procedure SaveToFile(const aPath: UnicodeString);
    function GetSize: TKMMapSize;
    function GetSizeText: String;
    function GetFavouriteMapPic: TKMPic;
    function GetCanBeHumanCount: Byte;
    function GetCanBeOnlyHumanCount: Byte;
    function GetCanBeAICount: Byte;
    function GetCanBeOnlyAICount: Byte;
    function GetCanBeHumanAndAICount: Byte;
    function GetBigDesc: UnicodeString;
    function GetMapName: UnicodeString;
    function GetTxtInfo: TKMMapTxtInfo;
    function GetDimentions: TKMPoint;

    constructor Create; overload;
    function GetAICanBeOnlyClassic(aIndex: Integer): Boolean;
    function GetAICanBeOnlyAdvanced(aIndex: Integer): Boolean;
    procedure LoadFromFile(const aPath: UnicodeString);
  public
    MapSizeX, MapSizeY: Integer;
    MissionMode: TKMissionMode;
    LocCount: Byte;
    CanBeHuman: array [0..MAX_HANDS-1] of Boolean;
    CanBeClassicAI: array [0..MAX_HANDS-1] of Boolean;
    CanBeAdvancedAI: array [0..MAX_HANDS-1] of Boolean;
    DefaultHuman: TKMHandID;
    GoalsVictoryCount, GoalsSurviveCount: array [0..MAX_HANDS-1] of Byte;
    GoalsVictory: array [0..MAX_HANDS-1] of array of TKMMapGoalInfo;
    GoalsSurvive: array [0..MAX_HANDS-1] of array of TKMMapGoalInfo;
    Alliances: array [0..MAX_HANDS-1, 0..MAX_HANDS-1] of TKMAllianceType;
    FlagColors: array [0..MAX_HANDS-1] of Cardinal;
    IsFavourite: Boolean;

    class function CreateDummy: TKMMapInfo;

    constructor Create(const aMapName: string; aStrictParsing: Boolean; aMapKind: TKMMapKind; aSilent: Boolean = False); overload;
    constructor Create(const aDir, aMapName: string; aStrictParsing: Boolean; aMapKind: TKMMapKind = mkUnknown; aSilent: Boolean = False); overload;
    destructor Destroy; override;

    procedure AddGoal(aGoalType: TKMGoalType; aPlayer: TKMHandID; aCondition: TKMGoalCondition; aStatus: TKMGoalStatus; aPlayerIndex: TKMHandID);
    procedure LoadExtra(aAddDefailtGoals: Boolean = False);

    property TxtInfo: TKMMapTxtInfo read GetTxtInfo;
    property BigDesc: UnicodeString read GetBigDesc;
    property MapName: UnicodeString read GetMapName;
    property InfoAmount: TKMMapInfoAmount read fInfoAmount;
    property Dir: string read fDir;
    property Kind: TKMMapKind read fKind;
    property Name: UnicodeString read fName;
    function FullPath(const aExt: string): string;
    function HumanUsableLocs: TKMHandIDArray;
    function AIUsableLocs: TKMHandIDArray;
    function AdvancedAIUsableLocs: TKMHandIDArray;
    function FixedLocsColors: TKMCardinalArray;
    function AIOnlyLocsColors: TKMCardinalArray;
    function IsOnlyAILoc(aLoc: Integer): Boolean;
    property CRC: Cardinal read fCRC;
    property MapAndDatCRC : Cardinal read fMapAndDatCRC;
    function LocationName(aIndex: TKMHandID): string;
    property Size: TKMMapSize read GetSize;
    property SizeText: string read GetSizeText;
    property Dimensions: TKMPoint read GetDimentions;
    function IsValid: Boolean;
    function HumanPlayerCount: Byte;
    function HumanPlayerCountMP: Byte;
    function AIOnlyLocCount: Byte;
    function FileNameWithoutHash: UnicodeString;
    function HasReadme: Boolean;
    function DetermineReadmeFilePath: String;
    function GetLobbyColor: Cardinal;
    function IsFilenameEndMatchHash: Boolean;
    function IsSinglePlayer: Boolean;
    function IsMultiPlayer: Boolean;
    function IsPlayableForSP: Boolean;
    function IsSinglePlayerKind: Boolean;
    function IsMultiPlayerKind: Boolean;
    function IsDownloadedKind: Boolean;
    function IsBuildingMission: Boolean;
    function IsFightingMission: Boolean;
    function IsCampaign : Boolean;
    property FavouriteMapPic: TKMPic read GetFavouriteMapPic;

    property AICanBeOnlyClassic[aIndex: Integer]: Boolean read GetAICanBeOnlyClassic;
    property AICanBeOnlyAdvanced[aIndex: Integer]: Boolean read GetAICanBeOnlyAdvanced;

    property CanBeHumanCount: Byte read GetCanBeHumanCount;
    property CanBeOnlyHumanCount: Byte read GetCanBeOnlyHumanCount;
    property CanBeAICount: Byte read GetCanBeAICount;
    property CanBeOnlyAICount: Byte read GetCanBeOnlyAICount;
    property CanBeHumanAndAICount: Byte read GetCanBeHumanAndAICount;
    function HasDifferentAITypes(aExceptLoc: TKMHandID = -1): Boolean;

    property CampaignID : TKMCampaignId read fCampaignID write fCampaignID;
    property CampaignMission : Byte read fCampaignMission write fCampaignMission;
  end;


  TTCustomMapsScanner = class(TThread)
  private
    fMapKinds: TKMMapKindSet;
    fOnComplete: TNotifyEvent;
    procedure ProcessMap(const aPath: UnicodeString; aKind: TKMMapKind); virtual; abstract;
  public
    constructor Create(aMapKinds: TKMMapKindSet; aOnComplete: TNotifyEvent = nil);
    procedure Execute; override;
  end;

  TTMapsScanner = class(TTCustomMapsScanner)
  private
    fOnMapAdd: TKMapEvent;
    fOnMapAddDone: TNotifyEvent;
    procedure ProcessMap(const aPath: UnicodeString; aKind: TKMMapKind); override;
  public
    constructor Create(aMapFolders: TKMMapKindSet; aOnMapAdd: TKMapEvent; aOnMapAddDone, aOnTerminate: TNotifyEvent; aOnComplete: TNotifyEvent = nil);
  end;

  TTMapsCacheUpdater = class(TTCustomMapsScanner)
  private
    fIsStopped: Boolean;
    procedure ProcessMap(const aPath: UnicodeString; aKind: TKMMapKind); override;
  public
    procedure Stop;
    constructor Create(aMapFolders: TKMMapKindSet);
  end;


  TKMapsCollection = class
  private
    fCount: Integer;
    fMaps: array of TKMMapInfo;
    fMapFolders: TKMMapKindSet;
    fSortMethod: TKMapsSortMethod;
    fDoSortWithFavourites: Boolean;
    fCriticalSection: TCriticalSection;
    fScanner: TTMapsScanner;
    fScanning: Boolean; //Flag if scan is in progress
    fUpdateNeeded: Boolean;
    fOnRefresh: TNotifyEvent;
    fOnTerminate: TNotifyEvent;
    fOnComplete: TNotifyEvent;
    procedure Clear;
    procedure MapAdd(aMap: TKMMapInfo);
    procedure MapAddDone(Sender: TObject);
    procedure ScanTerminate(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure DoSort;
    function GetMap(aIndex: Integer): TKMMapInfo;
  public
    constructor Create(aKindSet: TKMMapKindSet; aSortMethod: TKMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False); overload;
    constructor Create(aKind: TKMMapKind; aSortMethod: TKMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False); overload;
    destructor Destroy; override;

    property Count: Integer read fCount;
    property Maps[aIndex: Integer]: TKMMapInfo read GetMap; default;
    procedure Lock;
    procedure Unlock;

    class function FullPath(const aDirName, aFileName, aExt: string; aMapKind: TKMMapKind): string; overload;
    class function FullPath(const aName, aExt: string; aMultiplayer: Boolean): string; overload;
    class function FullPath(const aName, aExt: string; aMapKind: TKMMapKind): string; overload;
    class function FullPath(const aName, aExt: string; aMapKind: TKMMapKind; aCRC: Cardinal): string; overload;
//    class function GuessMPPath(const aName, aExt: string; aCRC: Cardinal): string;
    class procedure GetAllMapPaths(const aExeDir: string; aList: TStringList);
    class function GetMapCRC(const aMapPath: string): Cardinal;
    class procedure GetMapLayersFiles(const aName : String; var aList: TStringList);
    class Function GetMapPath(const aName: string): string;
    procedure Refresh(aOnRefresh: TNotifyEvent;  aOnTerminate: TNotifyEvent = nil;aOnComplete: TNotifyEvent = nil);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TKMapsSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TKMapsSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing


    function Contains(const aNewName: UnicodeString): Boolean;
    procedure RenameMap(aIndex: Integer; const aName: UnicodeString);
    procedure DeleteMap(aIndex: Integer);
    procedure MoveMap(aIndex: Integer; const aName: UnicodeString; aMapKind: TKMMapKind);

    procedure UpdateState;
  end;


implementation
uses
  SysUtils, StrUtils, TypInfo, Math,
  KromUtils,
  KM_GameSettings, KM_FileIO,
  KM_MissionScript_Info,
  KM_ScriptPreProcessor, KM_ScriptFilesCollection,
  KM_ResLocales, KM_ResTypes,
  KM_CommonUtils, KM_Log, KM_MapUtils, KM_Utils, KM_GameParams,
  IOUtils;

const
  MAP_TXT_INFO_MARKER = 'MapTxtInfo';


{ TKMMapInfo }
class function TKMMapInfo.CreateDummy: TKMMapInfo;
begin
  Result := Create;
end;


// Dummy instance, used to fill fields
constructor TKMMapInfo.Create;
begin
  inherited;
end;


constructor TKMMapInfo.Create(const aMapName: string; aStrictParsing: Boolean; aMapKind: TKMMapKind; aSilent: Boolean = False);
begin
  Assert(aMapKind <> mkUnknown); // Do not allow to create 'unknown' maps with this constructor
  Create(ExeDir + MAP_FOLDER_NAME[aMapKind] + PathDelim + aMapName + PathDelim, aMapName, aStrictParsing, aMapKind, aSilent);
end;


constructor TKMMapInfo.Create(const aDir, aMapName: string; aStrictParsing: Boolean; aMapKind: TKMMapKind = mkUnknown; aSilent: Boolean = False);

  function GetLIBXCRC(const aSearchFile: UnicodeString): Cardinal;
  var
    searchRec: TSearchRec;
  begin
    Result := 0;
    FindFirst(aSearchFile, faAnyFile - faDirectory, searchRec);
    try
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          Result := Result xor Adler32CRC(ExtractFilePath(aSearchFile) + searchRec.Name);
      until (FindNext(searchRec) <> 0);
    finally
      FindClose(searchRec);
    end;
  end;

var
  I: Integer;
  datFile, mapFile, scriptFile, txtFile, libxFiles: string;
  datCRC, mapCRC, othersCRC: Cardinal;
  missionParser: TKMMissionParserInfo;
  scriptPreProcessor: TKMScriptPreProcessor;
  scriptFiles: TKMScriptFilesCollection;
  CSP: TKMCustomScriptParam;
begin
  inherited Create;

  fDir := aDir;
  fName := aMapName;
  fKind := aMapKind;

  fTxtInfo := TKMMapTxtInfo.Create;

  for CSP := Low(TKMCustomScriptParam) to High(TKMCustomScriptParam) do
  begin
    fCustomScriptParams[CSP].Added := False;
    fCustomScriptParams[CSP].Data := '';
  end;


  datFile := fDir + fName + '.dat';
  mapFile := fDir + fName + '.map';
  scriptFile := fDir + fName + EXT_FILE_SCRIPT_DOT; //Needed for CRC
  txtFile := fDir + fName + '.txt'; //Needed for CRC
  libxFiles := fDir + fName + '.*.libx'; //Needed for CRC

  fSizeText := ''; //Lazy initialization

  if not FileExists(datFile) then Exit;

  //Try loading info from cache, since map scanning is rather slow
  LoadFromFile(fDir + fName + '.mi'); //Data will be empty if failed

  //We will scan map once again if anything has changed
  //In SP mode (non-strict) we check DAT CRC and version, that is enough
  //In MP mode (strict) we also need exact CRCs to match maps between players

  datCRC := Adler32CRC(datFile);
  //.map file CRC is the slowest, so only calculate it if necessary
  othersCRC := 0; //Supresses incorrect warning by Delphi
  mapCRC := 0;
  if aStrictParsing then
  begin
    mapCRC := Adler32CRC(mapFile);
    othersCRC := mapCRC xor Adler32CRC(txtFile) xor GetLIBXCRC(libxFiles);
    fMapAndDatCRC := datCRC xor mapCRC;

    //Add main script CRC and all included scripts CRC
    if FileExists(scriptFile) then
    begin
      othersCRC := othersCRC xor Adler32CRC(scriptFile);
      scriptPreProcessor := TKMScriptPreProcessor.Create(aSilent);
      try
        if scriptPreProcessor.PreProcessFile(scriptFile) then
        begin
          //Copy custom script params
          for CSP := Low(TKMCustomScriptParam) to High(TKMCustomScriptParam) do
            fCustomScriptParams[CSP] := scriptPreProcessor.CustomScriptParams[CSP];

          scriptFiles := scriptPreProcessor.ScriptFilesInfo;
          for I := 0 to scriptFiles.IncludedCount - 1 do
            othersCRC := othersCRC xor Adler32CRC(scriptFiles[I].FullFilePath);
        end;
      finally
        scriptPreProcessor.Free;
      end;
    end;
  end;

  //Does the map need to be fully rescanned? (.mi cache is outdated?)
  if (fVersion <> GAME_REVISION) or
     (fDatCRC <> datCRC) or //In non-strict mode only DAT CRC matters (SP)
     (aStrictParsing and (fCRC <> datCRC xor othersCRC)) //In strict mode we check all CRCs (MP)
  then
  begin
    //Calculate OthersCRC if it wasn't calculated before
    if not aStrictParsing then
    begin
      mapCRC := Adler32CRC(mapFile);
      othersCRC := mapCRC xor Adler32CRC(scriptFile) xor Adler32CRC(txtFile);
    end;

    fCRC := datCRC xor othersCRC;
    fDatCRC := datCRC;
    fMapAndDatCRC := datCRC xor mapCRC;
    fVersion := GAME_REVISION;

    //First reset everything because e.g. CanBeHuman is assumed False by default and set True when we encounter SET_USER_PLAYER
    ResetInfo;

    missionParser := TKMMissionParserInfo.Create;
    try
      //Fill Self properties with MissionParser
      missionParser.LoadMission(datFile, Self, pmBase);
    finally
      missionParser.Free;
    end;


    if gGameSettings = nil // In case we are closing app and settings object is already destroyed
      then Exit;

    IsFavourite := gGameSettings.FavouriteMaps.Contains(fMapAndDatCRC);

    SaveToFile(fDir + fName + '.mi'); //Save new cache file
  end;
    //Load additional text info
    fTxtInfo.LoadTXTInfo(fDir + fName + '.txt');

  fInfoAmount := iaBase;
end;


destructor TKMMapInfo.Destroy;
begin
  FreeAndNil(fTxtInfo);

  inherited;
end;


procedure TKMMapInfo.AddGoal(aGoalType: TKMGoalType; aPlayer: TKMHandID; aCondition: TKMGoalCondition; aStatus: TKMGoalStatus; aPlayerIndex: TKMHandID);
var
  G: TKMMapGoalInfo;
begin
  G.Cond := aCondition;
  G.Play := aPlayerIndex;
  G.Stat := aStatus;

  case aGoalType of
    gltVictory: begin
                  SetLength(GoalsVictory[aPlayer], GoalsVictoryCount[aPlayer] + 1);
                  GoalsVictory[aPlayer, GoalsVictoryCount[aPlayer]] := G;
                  Inc(GoalsVictoryCount[aPlayer]);
                end;
    gltSurvive: begin
                  SetLength(GoalsSurvive[aPlayer], GoalsSurviveCount[aPlayer] + 1);
                  GoalsSurvive[aPlayer, GoalsSurviveCount[aPlayer]] := G;
                  Inc(GoalsSurviveCount[aPlayer]);
                end;
  end;
end;


function TKMMapInfo.FullPath(const aExt: string): string;
begin
  Result := fDir + fName + aExt;
end;


function TKMMapInfo.HumanUsableLocs: TKMHandIDArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeHuman[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMMapInfo.AIUsableLocs: TKMHandIDArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeClassicAI[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMMapInfo.AdvancedAIUsableLocs: TKMHandIDArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to MAX_HANDS - 1 do
    if CanBeAdvancedAI[I] then
    begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := I;
    end;
end;


function TKMMapInfo.IsOnlyAILoc(aLoc: Integer): Boolean;
begin
  Assert(aLoc < MAX_HANDS);
  Result := not CanBeHuman[aLoc] and (CanBeClassicAI[aLoc] or CanBeAdvancedAI[aLoc]);
end;


// Color is fixed for loc if map has BlockColorSelection attribute
// or if its only AI loc, no available for player
function TKMMapInfo.FixedLocsColors: TKMCardinalArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if Self = nil then Exit;

  SetLength(Result, LocCount);
  for I := 0 to LocCount - 1 do
    if TxtInfo.BlockColorSelection or IsOnlyAILoc(I) then
      Result[I] := FlagColors[I]
    else
      Result[I] := 0;
end;


// Colors that are used by only AI locs
function TKMMapInfo.AIOnlyLocsColors: TKMCardinalArray;
var
  I, K: Integer;
begin
  SetLength(Result, 0);
  if Self = nil then Exit;

  SetLength(Result, LocCount);
  K := 0;
  for I := 0 to LocCount - 1 do
    if IsOnlyAILoc(I) then
    begin
      Result[K] := FlagColors[I];
      Inc(K);
    end;

  SetLength(Result, K);
end;


function TKMMapInfo.LocationName(aIndex: TKMHandID): string;
begin
  Result := Format(gResTexts[TX_LOBBY_LOCATION_X], [aIndex + 1]);
end;


function TKMMapInfo.GetSize: TKMMapSize;
begin
  if fSize = msNone then
    fSize := MapSizeIndex(MapSizeX, MapSizeY);
  Result := fSize;
end;


function TKMMapInfo.GetSizeText: string;
begin
  if fSizeText = '' then
    fSizeText := MapSizeText(MapSizeX, MapSizeY);
  Result := fSizeText;
end;


function TKMMapInfo.GetTxtInfo: TKMMapTxtInfo;
begin
  if Self = nil then Exit(nil);

  Result := fTxtInfo;
end;


//Load additional information for map that is not in main SP list
procedure TKMMapInfo.LoadExtra(aAddDefailtGoals: Boolean = False);
var
  I, K: Integer;
  datFile: string;
  missionParser: TKMMissionParserInfo;
  gc: TKMGoalCondition;
begin
  //Do not append Extra info twice
  if fInfoAmount = iaExtra then Exit;

  //First reset everything because e.g. CanBeHuman is assumed False by default and set True when we encounter SET_USER_PLAYER
  ResetInfo;

  datFile := fDir + fName + '.dat';

  missionParser := TKMMissionParserInfo.Create;
  try
    //Fill Self properties with MissionParser
    missionParser.LoadMission(datFile, Self, pmExtra);
  finally
    missionParser.Free;
  end;

  if IsFightingMission then
    fTxtInfo.BlockPeacetime := True;

  fTxtInfo.LoadTXTInfo(fDir + fName + '.txt');

  fInfoAmount := iaExtra;

  if not aAddDefailtGoals then Exit;
  
  // Add Default goals for a certain maps
  if fTxtInfo.IsPlayableAsSP and fTxtInfo.CanAddDefaultGoals then
  begin
    if IsBuildingMission then
      gc := gcBuildings
    else
      gc := gcTroops;

    for I := 0 to LocCount - 1 do
    begin
      AddGoal(gltSurvive, I, gc, gsTrue, I);
      for K := 0 to LocCount - 1 do
      if I <> K then
        AddGoal(gltVictory, I, gc, gsFalse, K);
    end;
  end;
end;


procedure TKMMapInfo.ResetInfo;
var
  I, K: Integer;
begin
  MissionMode := mmBuilding;
  DefaultHuman := 0;
  fTxtInfo.ResetInfo;
  for I:=0 to MAX_HANDS-1 do
  begin
    FlagColors[I] := DEFAULT_PLAYERS_COLORS[I];
    CanBeHuman[I] := False;
    CanBeClassicAI[I] := False;
    CanBeAdvancedAI[I] := False;
    GoalsVictoryCount[I] := 0;
    SetLength(GoalsVictory[I], 0);
    GoalsSurviveCount[I] := 0;
    SetLength(GoalsSurvive[I], 0);
    for K:=0 to MAX_HANDS-1 do
      if I = K then
        Alliances[I,K] := atAlly
      else
        Alliances[I,K] := atEnemy;
  end;

  fCampaignID[0] := 0;
  fCampaignID[1] := 0;
  fCampaignID[2] := 0;
  fCampaignMission := 255;
end;


procedure TKMMapInfo.LoadFromStreamObj(aStreamObj: TObject; const aPath: UnicodeString);
var
  S: TKMemoryStream;
begin
  Assert(aStreamObj is TKMemoryStreamBinary, 'Wrong stream object class');

  S := TKMemoryStreamBinary(aStreamObj);

  S.LoadFromFile(aPath);

  //Internal properties
  S.Read(fCRC);
  S.Read(fDatCRC);
  S.Read(fMapAndDatCRC);
  S.ReadA(fVersion);

  //Exposed properties
  S.Read(MapSizeX);
  S.Read(MapSizeY);
  S.Read(MissionMode, SizeOf(TKMissionMode));
  S.Read(LocCount);
  S.Read(CanBeHuman, SizeOf(CanBeHuman));

  fTxtInfo.Load(S);

  IsFavourite := gGameSettings.FavouriteMaps.Contains(fMapAndDatCRC);
end;


procedure TKMMapInfo.LoadFromFile(const aPath: UnicodeString);
var
  S: TKMemoryStream;
  errorStr: UnicodeString;
begin
  if not FileExists(aPath) then Exit;

  S := TKMemoryStreamBinary.Create;
  try
    //Try to load map cache up to 3 times (in case its updating by other thread
    //its much easier and working well, then synchronize threads
    if not TryExecuteMethod(TObject(S), aPath, 'LoadFromStreamObj', errorStr, LoadFromStreamObj) then
      gLog.AddTime(errorStr);
  finally
    //Other properties are not saved, they are fast to reload
    S.Free;
  end;
end;


procedure TKMMapInfo.SaveToStreamObj(aStreamObj: TObject; const aPath: UnicodeString);
var
  S: TKMemoryStream;
begin
  Assert(aStreamObj is TKMemoryStreamBinary, 'Wrong stream object class');

  S := TKMemoryStreamBinary(aStreamObj);

  S.SaveToFile(aPath);
end;


procedure TKMMapInfo.SaveToFile(const aPath: UnicodeString);
var
  S: TKMemoryStream;
  errorStr: UnicodeString;
begin
  S := TKMemoryStreamBinary.Create;
  try
    //Internal properties
    S.Write(fCRC);
    S.Write(fDatCRC);
    S.Write(fMapAndDatCRC);
    S.WriteA(fVersion);

    //Exposed properties
    S.Write(MapSizeX);
    S.Write(MapSizeY);
    S.Write(MissionMode, SizeOf(TKMissionMode));
    S.Write(LocCount);
    S.Write(CanBeHuman, SizeOf(CanBeHuman));

    fTxtInfo.Save(S);

    //Try to save map cache up to 3 times (in case its updating by other thread
    //its much easier and working well, then synchronize threads
    if not TryExecuteMethod(TObject(S), aPath, 'SaveToStreamObj', errorStr, SaveToStreamObj) then
      gLog.AddTime(errorStr);
  finally
    //Other properties from text file are not saved, they are fast to reload
    S.Free;
  end;
end;


function TKMMapInfo.IsValid: Boolean;
begin
  Result := (LocCount > 0) and
            FileExists(fDir + fName + '.dat') and
            FileExists(fDir + fName + '.map');
end;


function TKMMapInfo.HumanPlayerCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_HANDS - 1 do
    if CanBeHuman[I] then
      Inc(Result);
end;


function TKMMapInfo.HumanPlayerCountMP: Byte;
begin
  Result := HumanPlayerCount;
  //Enforce MP limit
  if Result > MAX_LOBBY_PLAYERS then
    Result := MAX_LOBBY_PLAYERS;
end;


function TKMMapInfo.AIOnlyLocCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_HANDS - 1 do
    if (CanBeClassicAI[I] or CanBeAdvancedAI[I]) and not CanBeHuman[I] then
      Inc(Result);
end;


//Returns True if map filename ends with this map actual CRC hash.
//Used to check if downloaded map was changed
function TKMMapInfo.IsFilenameEndMatchHash: Boolean;
begin
  Result := (Length(fName) > 9)
    and (fName[Length(Name)-8] = '_')
    and (IntToHex(fCRC, 8) = RightStr(fName, 8));
end;


function TKMMapInfo.IsSinglePlayer: Boolean;
begin
  Result := HumanPlayerCount = 1;
end;


function TKMMapInfo.IsMultiPlayer: Boolean;
begin
  Result := HumanPlayerCount > 1;
end;


function TKMMapInfo.IsPlayableForSP: Boolean;
begin
  Result := IsSinglePlayerKind or TxtInfo.IsPlayableAsSP;
end;


function TKMMapInfo.IsSinglePlayerKind: Boolean;
begin
  Result := fKind = mkSP;
end;


function TKMMapInfo.IsMultiPlayerKind: Boolean;
begin
  Result := fKind = mkMP;
end;


function TKMMapInfo.IsDownloadedKind: Boolean;
begin
  Result := fKind = mkDL;
end;


function TKMMapInfo.IsBuildingMission: Boolean;
begin
  Result := MissionMode = mmBuilding;
end;


function TKMMapInfo.IsFightingMission: Boolean;
begin
  Result := MissionMode = mmFighting;
end;

function TKMMapInfo.IsCampaign: Boolean;
begin
  Result := (fCampaignID[0] <> 0)
            and (fCampaignID[1] <> 0)
            and (fCampaignID[2] <> 0)
            and (fCampaignMission <> 255);
end;

function TKMMapInfo.FileNameWithoutHash: UnicodeString;
begin
  if (fKind = mkDL) and IsFilenameEndMatchHash then
    Result := LeftStr(Name, Length(Name)-9)
  else
    Result := Name;
end;


function TKMMapInfo.DetermineReadmeFilePath: String;
begin
  if Self = nil then Exit('');
  
  Assert(gGameSettings <> nil, 'gGameSettings = nil!');

  Result := GetLocalizedFilePath(fDir + fName, gResLocales.UserLocale, gResLocales.FallbackLocale, '.pdf');
end;


function TKMMapInfo.GetFavouriteMapPic: TKMPic;
begin
  Result := MakePic(rxGuiMain, IfThen(IsFavourite, 77, 85), True);
end;


function TKMMapInfo.GetCanBeHumanCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CanBeHuman) to High(CanBeHuman) do
    if CanBeHuman[I] then
      Inc(Result);
end;


function TKMMapInfo.GetCanBeOnlyHumanCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CanBeHuman) to High(CanBeHuman) do
    if CanBeHuman[I] and not CanBeClassicAI[I] and not CanBeAdvancedAI[I] then
      Inc(Result);
end;


function TKMMapInfo.GetDimentions: TKMPoint;
begin
  Result := KMPoint(MapSizeX, MapSizeY);
end;


function TKMMapInfo.GetCanBeAICount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CanBeClassicAI) to High(CanBeClassicAI) do
    if CanBeClassicAI[I] or CanBeAdvancedAI[I] then
      Inc(Result);
end;


function TKMMapInfo.GetCanBeOnlyAICount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CanBeHuman) to High(CanBeHuman) do
    if (CanBeClassicAI[I] or CanBeAdvancedAI[I]) and not CanBeHuman[I] then
      Inc(Result);
end;


function TKMMapInfo.GetAICanBeOnlyAdvanced(aIndex: Integer): Boolean;
begin
  Result := CanBeAdvancedAI[aIndex] and not CanBeClassicAI[aIndex];
end;


function TKMMapInfo.GetAICanBeOnlyClassic(aIndex: Integer): Boolean;
begin
  Result := CanBeClassicAI[aIndex] and not CanBeAdvancedAI[aIndex];
end;


function TKMMapInfo.GetCanBeHumanAndAICount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(CanBeHuman) to High(CanBeHuman) do
    if (CanBeClassicAI[I] or CanBeAdvancedAI[I]) and CanBeHuman[I] then
      Inc(Result);
end;


function TKMMapInfo.HasDifferentAITypes(aExceptLoc: TKMHandID = -1): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to MAX_HANDS - 1 do
  begin
    if I = aExceptLoc then
      Continue;

    if CanBeClassicAI[I] and CanBeAdvancedAI[I] then
      Result := True;
  end;
end;


function TKMMapInfo.GetBigDesc: UnicodeString;
var
  CSP: TKMCustomScriptParam;
begin
  Result := '';
  for CSP := Low(TKMCustomScriptParam) to High(TKMCustomScriptParam) do
    if fCustomScriptParams[CSP].Added then
      Result := Result + WrapColor(gResTexts[CUSTOM_MAP_PARAM_DESCR_TX[CSP]] + ':', icRed) + '|'
                       + WrapColor('[' + fCustomScriptParams[CSP].Data + ']', icOrange) + '||';

  Result := Result + TxtInfo.BigDescToDisplay;

  // Add 1 new line for author & version section
  if (TxtInfo.Author <> '') or (TxtInfo.Version <> '') and (Result <> '') then
    Result := Result + '|';

  if (TxtInfo.Author <> '') then
    Result := Result + Format('|[$00B0FF]%s:[] %s', [gResTexts[TX_MAPED_MISSION_AUTHOR], TxtInfo.Author]);

  if TxtInfo.Version <> '' then
    Result := Result + Format('|[$7070FF]%s:[] %s', [gResTexts[TX_MAPED_MISSION_VERSION], TxtInfo.Version]);
end;

function TKMMapInfo.GetMapName: UnicodeString;
begin
  Result := IfThen(TxtInfo.NameToDisplay = '', Name, TxtInfo.NameToDisplay);
end;


function TKMMapInfo.HasReadme: Boolean;
begin
  Result := DetermineReadmeFilePath <> '';
end;


function TKMMapInfo.GetLobbyColor: Cardinal;
begin
  if fKind = mkDL then
    Result := $FFC9BBBB
  else
    Result := $FF9CF6FF;
end;


{ TKMMapTxtInfo }
constructor TKMMapTxtInfo.Create;
begin
  inherited;

  // Only these two fields should be "non-zero" by default
  fSmallDescLibx := LIBX_NO_ID;
  fBigDescLibx := LIBX_NO_ID;
  fNameLibx := LIBX_NO_ID;
  Weather.SetDefault;
end;


procedure TKMMapTxtInfo.SaveTXTInfo(const aFilePath: String);
var
  St: String;
  ft: TextFile;
  MD: TKMMissionDifficulty;

  procedure WriteLine(const aLineHeader: String; const aLineValue: String = ''); overload;
  begin
    Writeln(ft, aLineHeader);
    if aLineValue <> '' then
      Writeln(ft, aLineValue);
    //Writeln(ft);
  end;

  procedure WriteLine(const aLineHeader: String; const aLineValue: Integer); overload;
  begin
    Writeln(ft, aLineHeader);
    Writeln(ft, IntToStr(aLineValue));
    //Writeln(ft);
  end;
  procedure WriteLine(const aLineValue: Integer); overload;
  begin
    Writeln(ft, IntToStr(aLineValue));
    //Writeln(ft);
  end;

begin
  if IsEmpty then
  begin
    if FileExists(aFilePath) then
      KMDeleteFile(aFilePath);
    Exit;
  end;

  ForceDirectories(ExtractFilePath(aFilePath));

  AssignFile(ft, aFilePath);
  Rewrite(ft);

  if Author <> '' then
    WriteLine('Author', Author);

  if Version <> '' then
    WriteLine('Version', Version);

  if fSmallDescLibx <> LIBX_NO_ID then
    WriteLine('SmallDescLIBX', IntToStr(fSmallDescLibx))
  else
  if fSmallDesc <> '' then
    WriteLine('SmallDesc', fSmallDesc);

  if fBigDescLibx <> LIBX_NO_ID then
    WriteLine('BigDescLIBX', IntToStr(fBigDescLibx))
  else
  if fBigDesc <> '' then
    WriteLine('BigDesc', fBigDesc);

  if fNameLibx <> LIBX_NO_ID then
    WriteLine('NameLIBX', IntToStr(fNameLibx));

  if IsCoop then
    WriteLine('SetCoop');

  if IsSpecial then
    WriteLine('SetSpecial');

  if IsRMG then
    WriteLine('RMG');

  if IsPlayableAsSP then
    WriteLine('PlayableAsSP');

  if BlockPeacetime then
    WriteLine('BlockPeacetime');

  if BlockTeamSelection then
    WriteLine('BlockTeamSelection');

  if BlockColorSelection then
    WriteLine('BlockColorSelection');

  if BlockFullMapPreview then
    WriteLine('BlockFullMapPreview');

  if Weather.Overwrite then
  begin
    WriteLine('WeatherOverwrite');
    WriteLine(BoolToStr(Weather.Enabled, true));//enabled or disabled
    WriteLine(Weather.MaxCount);
    WriteLine(Weather.MaxSpawnCount);
    WriteLine(Weather.MinInterval);
    WriteLine(Weather.MaxInterval);
    WriteLine(Weather.MaxLifeTime);
    WriteLine(FloatToStr(Weather.MaxCloudSpeed));
    WriteLine(Weather.DecParticles);
    WriteLine(Weather.NightTime);
    WriteLine(Weather.NightSpeed);
    WriteLine(BoolToStr(Weather.DynamicLight, true));//enabled or disabled
  end;

  if HasDifficultyLevels then
  begin
    St := '';
    for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
      if MD in DifficultyLevels then
      begin
        if St <> '' then
          St := St + ',';
        St := St + GetEnumName(TypeInfo(TKMMissionDifficulty), Integer(MD));
      end;
    WriteLine('DifficultyLevels', St);
  end;

  CloseFile(ft);
end;


procedure TKMMapTxtInfo.LoadTXTInfo(const aFilePath: String);

  function LoadDescriptionFromLIBX(aIndex: Integer): UnicodeString;
  var
    missionTexts: TKMTextLibrarySingle;
  begin
    Result := '';
    if aIndex = -1 then Exit;
    missionTexts := TKMTextLibrarySingle.Create;
    missionTexts.LoadLocale(ChangeFileExt(aFilePath, '.%s.libx'));
    Result := missionTexts.Texts[aIndex];
    missionTexts.Free;
  end;


var
  I, tmpInt: Integer;
  St, S: String;
  ft: TextFile;
  stList: TStringList;
  MD: TKMMissionDifficulty;

  function NextLineToInt(aDefaultValue : Integer = 0) : Integer;
  begin
    Result := aDefaultValue;
    ReadLn(ft, S);
    TryStrToInt(S, Result);
  end;

  function NextLineToFloat(aDefaultValue : Single = 0) : Single;
  begin
    Result := aDefaultValue;
    ReadLn(ft, S);
    TryStrToFloat(S, Result);
  end;

  function NextLineToBool : Boolean;
  begin
    Result := false;
    ReadLn(ft, S);
    TryStrToBool(S, Result);
  end;

begin

  //Load additional text info
  if FileExists(aFilePath) then
  begin
    AssignFile(ft, aFilePath);
    FileMode := fmOpenRead;
    Reset(ft);
    repeat
      ReadLn(ft, St);
      if SameText(St, 'Author') then
        Readln(ft, Author);

      if SameText(St, 'Version') then
        Readln(ft, Version);
      if SameText(St, 'WeatherOverwrite') then
      begin
        Weather.Overwrite := true;
        Weather.Enabled := NextLineToBool;
        Weather.MaxCount := NextLineToInt;
        Weather.MaxSpawnCount := NextLineToInt;
        Weather.MinInterval := NextLineToInt;
        Weather.MaxInterval := NextLineToInt;
        Weather.MaxLifeTime := NextLineToInt;
        Weather.MaxCloudSpeed := NextLineToFloat;
        Weather.DecParticles := NextLineToInt;
        Weather.NightTime := NextLineToInt;
        Weather.NightSpeed := NextLineToInt;
        Weather.DynamicLight := NextLineToBool;
      end;

      if SameText(St, 'WeatherOverwrite') then
      begin

      end;

      if SameText(St, 'BigDesc') then
      begin
        Readln(ft, S);
        BigDesc := S; // Will reset BigDescLIBX if needed
      end;

      if SameText(St, 'BigDescLIBX') then
      begin
        Readln(ft, S);
        tmpInt := StrToIntDef(S, LIBX_NO_ID);
        if tmpInt <> LIBX_NO_ID then
          SetBigDescLibxAndTranslation(tmpInt, LoadDescriptionFromLIBX(tmpInt));
      end;

      if SameText(St, 'NameLIBX') then
      begin
        Readln(ft, S);
        tmpInt := StrToIntDef(S, LIBX_NO_ID);
        if tmpInt <> LIBX_NO_ID then
          SetNameLibxAndTranslation(tmpInt, LoadDescriptionFromLIBX(tmpInt));
      end;

      if SameText(St, 'SmallDesc') then
      begin
        ReadLn(ft, S);
        SmallDesc := S; // Will reset SmallDescLIBX if needed
      end;

      if SameText(St, 'SmallDescLIBX') then
      begin
        Readln(ft, S);
        tmpInt := StrToIntDef(S, LIBX_NO_ID);
        if tmpInt <> LIBX_NO_ID then
          SetSmallDescLibxAndTranslation(tmpInt, LoadDescriptionFromLIBX(tmpInt));
      end;

      if SameText(St, 'SetCoop') then
      begin
        IsCoop := True;
        BlockTeamSelection := True;
        BlockPeacetime := True;
        BlockFullMapPreview := True;
      end;

      if SameText(St, 'SetSpecial') then
        IsSpecial := True;
      if SameText(St, 'RMG') then
        IsRMG := True;
      if SameText(St, 'PlayableAsSP') then
        IsPlayableAsSP := True;
      if SameText(St, 'BlockTeamSelection') then
        BlockTeamSelection := True;
      if SameText(St, 'BlockColorSelection') then
        BlockColorSelection := True;
      if SameText(St, 'BlockPeacetime') then
        BlockPeacetime := True;
      if SameText(St, 'BlockFullMapPreview') then
        BlockFullMapPreview := True;

      if SameText(St, 'DifficultyLevels') then
      begin
        Readln(ft, S);
        stList := TStringList.Create;
        StringSplit(S, ',', stList);
        for I := 0 to stList.Count - 1 do
          for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
            if SameText(stList[I], GetEnumName(TypeInfo(TKMMissionDifficulty), Integer(MD))) then
              Include(DifficultyLevels, MD);
        stList.Free;
      end;
    until(eof(ft));
    CloseFile(ft);

    // Normalize descriptions
    NormalizeDesc;
  end;
end;


function TKMMapTxtInfo.GetSmallDescSanitized: UnicodeString;
var
  I: Integer;
begin
  if fSmallDescLibx = LIBX_NO_ID then
    Result := fSmallDesc
  else
    Result := fSmallDescTranslated;

  // Trim to EOL (for display in a table)
  I := Pos('|', Result);
  if I <> 0 then
    Result := LeftStr(Result, I);
end;


function TKMMapTxtInfo.GetBigDescToDisplay: UnicodeString;
begin
  if fBigDescLibx = LIBX_NO_ID then
    Result := fBigDesc
  else
    Result := fBigDescTranslated;
end;

function TKMMapTxtInfo.GetNameToDisplay: UnicodeString;
begin
  Result := IfThen(fNameLibx = LIBX_NO_ID, '', fNameTranslated);
end;


procedure TKMMapTxtInfo.SetSmallDesc(const aSmallDesc: UnicodeString);
begin
  fSmallDesc := aSmallDesc;

  if fSmallDesc <> '' then
    SetSmallDescLibxAndTranslation(LIBX_NO_ID, '');
end;


procedure TKMMapTxtInfo.SetBigDesc(const aBigDesc: UnicodeString);
begin
  fBigDesc := aBigDesc;

  if fBigDesc <> '' then
    SetBigDescLibxAndTranslation(LIBX_NO_ID, '');
end;


// Sets LibxID and its translation from Libx
procedure TKMMapTxtInfo.SetSmallDescLibxAndTranslation(aSmallDescLibx: Integer; aTranslation: UnicodeString);
begin
  fSmallDescLibx := aSmallDescLibx;
  fSmallDescTranslated :=  aTranslation;
  if fSmallDescLibx <> LIBX_NO_ID then
    fSmallDesc := '';
end;


// Sets LibxID and its translation from Libx
procedure TKMMapTxtInfo.SetBigDescLibxAndTranslation(aBigDescLibx: Integer; aTranslation: UnicodeString);
begin
  fBigDescLibx := aBigDescLibx;
  fBigDescTranslated :=  aTranslation;
  if fBigDescLibx <> LIBX_NO_ID then
    fBigDesc := '';
end;

procedure TKMMapTxtInfo.SetNameLibxAndTranslation(aNameLibx: Integer; aTranslation: UnicodeString);
begin
  fNameLibx := aNameLibx;
  fNameTranslated :=  aTranslation;
end;

function TKMMapTxtInfo.GetBlockColorSelection: Boolean;
begin
  if Self = nil then Exit(False);

  Result := fBlockColorSelection;
end;


// Normalize descriptions, thus they should have either LibxID or text
procedure TKMMapTxtInfo.NormalizeDesc;
begin
  if fSmallDescLibx = LIBX_NO_ID then
    fSmallDescTranslated := ''
  else
    fSmallDesc := '';

  if fBigDescLibx = LIBX_NO_ID then
    fBigDescTranslated := ''
  else
    fBigDesc := '';
end;


function TKMMapTxtInfo.IsSmallDescLibxSet: Boolean;
begin
  Result := SmallDescLibx <> LIBX_NO_ID;
end;


function TKMMapTxtInfo.IsBigDescLibxSet: Boolean;
begin
  Result := BigDescLibx <> LIBX_NO_ID;
end;


function TKMMapTxtInfo.CanAddDefaultGoals: Boolean;
begin
  Result := not IsSpecial and not IsCoop;
end;


function TKMMapTxtInfo.IsEmpty: Boolean;
begin
  Result := not (IsCoop or IsSpecial or IsPlayableAsSP or IsRMG
            or BlockTeamSelection or BlockColorSelection or BlockPeacetime or BlockFullMapPreview
            or (Author <> '') or (Version <> '')
            or (fSmallDesc <> '') or IsSmallDescLibxSet
            or (fBigDesc <> '') or IsBigDescLibxSet
            or (fNameLibx <> 0)
            or HasDifficultyLevels
            or Weather.Overwrite);
end;


function TKMMapTxtInfo.HasDifficultyLevels: Boolean;
var
  MD: TKMMissionDifficulty;
begin
  if Self = nil then Exit(False);

  Result := (DifficultyLevels <> []);
  //We consider there is no difficulty levels, if only one is presented
  for MD := MISSION_DIFFICULTY_MIN to MISSION_DIFFICULTY_MAX do
    Result := Result and (DifficultyLevels <> [MD]);
end;


procedure TKMMapTxtInfo.ResetInfo;
begin
  IsCoop := False;
  IsSpecial := False;
  IsRMG := False;
  IsPlayableAsSP := False;
  BlockTeamSelection := False;
  BlockColorSelection := False;
  BlockPeacetime := False;
  BlockFullMapPreview := False;
  DifficultyLevels := [];
  Author := '';
  Version := '';
  fSmallDesc := '';
  fSmallDescLibx := LIBX_NO_ID;
  fSmallDescTranslated := '';
  fBigDesc := '';
  fBigDescLibx := LIBX_NO_ID;
  fBigDescTranslated := '';

  fNameLibx := LIBX_NO_ID;
  fNameTranslated := '';
end;


procedure TKMMapTxtInfo.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker(MAP_TXT_INFO_MARKER);

  LoadStream.ReadW(Author);
  LoadStream.ReadW(Version);
  LoadStream.Read(IsCoop);
  LoadStream.Read(IsSpecial);
  LoadStream.Read(IsRMG);
  LoadStream.Read(IsPlayableAsSP);

  LoadStream.Read(BlockTeamSelection);
  LoadStream.Read(fBlockColorSelection);
  LoadStream.Read(BlockPeacetime);
  LoadStream.Read(BlockFullMapPreview);

  LoadStream.ReadW(fSmallDesc);
  LoadStream.Read(fSmallDescLibx);

  LoadStream.ReadW(fBigDesc);
  LoadStream.Read(fBigDescLibx);
  LoadStream.Read(fNameLibx);
  LoadStream.Read(Weather, SizeOf(Weather));
end;


procedure TKMMapTxtInfo.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker(MAP_TXT_INFO_MARKER);

  SaveStream.WriteW(Author);
  SaveStream.WriteW(Version);
  SaveStream.Write(IsCoop);
  SaveStream.Write(IsSpecial);
  SaveStream.Write(IsRMG);
  SaveStream.Write(IsPlayableAsSP);

  SaveStream.Write(BlockTeamSelection);
  SaveStream.Write(fBlockColorSelection);
  SaveStream.Write(BlockPeacetime);
  SaveStream.Write(BlockFullMapPreview);

  SaveStream.WriteW(fSmallDesc);
  SaveStream.Write(fSmallDescLibx);

  SaveStream.WriteW(fBigDesc);
  SaveStream.Write(fBigDescLibx);
  SaveStream.Write(fNameLibx);
  SaveStream.Write(Weather, SizeOf(Weather));
end;


{ TKMapsCollection }
constructor TKMapsCollection.Create(aKindSet: TKMMapKindSet; aSortMethod: TKMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False);
begin
  inherited Create;

  fMapFolders := aKindSet;
  fSortMethod := aSortMethod;
  fDoSortWithFavourites := aDoSortWithFavourites;

  //CS is used to guard sections of code to allow only one thread at once to access them
  //We mostly don't need it, as UI should access Maps only when map events are signaled
  //it mostly acts as a safenet
  fCriticalSection := TCriticalSection.Create;
end;


function TKMapsCollection.Contains(const aNewName: UnicodeString): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to fCount - 1 do
    if LowerCase(fMaps[I].Name) = LowerCase(aNewName) then
    begin
      Result := True;
      Exit;
    end;
end;


constructor TKMapsCollection.Create(aKind: TKMMapKind; aSortMethod: TKMapsSortMethod = smByNameDesc; aDoSortWithFavourites: Boolean = False);
begin
  Create([aKind], aSortMethod, aDoSortWithFavourites);
end;


destructor TKMapsCollection.Destroy;
begin
  //Terminate and release the Scanner if we have one working or finished
  TerminateScan;

  //Release TKMMapInfo objects
  Clear;

  fCriticalSection.Free;
  inherited;
end;


function TKMapsCollection.GetMap(aIndex: Integer): TKMMapInfo;
begin
  //No point locking/unlocking here since we return a TObject that could be modified/freed
  //by another thread before the caller uses it.
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fMaps[aIndex];
end;



//class function TKMapsCollection.GuessMPPath(const aName, aExt: string; aCRC: Cardinal): string;
//var
//  S: UnicodeString;
//begin
//  S := aName + '_' + IntToHex(aCRC, 8);
//  Result := MAP_FOLDER[mkDL] + PathDelim + S + PathDelim + S + aExt;
//  if not FileExists(ExeDir + Result) then
//    Result := MAP_FOLDER[mkMP] + PathDelim + aName + PathDelim + aName + aExt;
//end;


procedure TKMapsCollection.Lock;
begin
  fCriticalSection.Enter;
end;


procedure TKMapsCollection.Unlock;
begin
  fCriticalSection.Leave;
end;


procedure TKMapsCollection.Clear;
var
  I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  for I := 0 to fCount - 1 do
    FreeAndNil(fMaps[I]);
  fCount := 0;
  SetLength(fMaps, 0); //We could use Low and High. Need to reset array to 0 length
end;


procedure TKMapsCollection.UpdateState;
begin
  if Self = nil then Exit;

  if not fUpdateNeeded then Exit;

  if Assigned(fOnRefresh) then
    fOnRefresh(Self);

  fUpdateNeeded := False;
end;


procedure TKMapsCollection.DeleteMap(aIndex: Integer);
var
  I: Integer;
begin
   Lock;
   try
     Assert(InRange(aIndex, 0, fCount - 1));
     KMDeleteFolderToBin(fMaps[aIndex].Dir);
     fMaps[aIndex].Free;
     for I  := aIndex to fCount - 2 do
       fMaps[I] := fMaps[I + 1];
     Dec(fCount);
     SetLength(fMaps, fCount);
   finally
     Unlock;
   end;
end;


procedure TKMapsCollection.RenameMap(aIndex: Integer; const aName: UnicodeString);
begin
  MoveMap(aIndex, aName, fMaps[aIndex].fKind);
end;


procedure TKMapsCollection.MoveMap(aIndex: Integer; const aName: UnicodeString; aMapKind: TKMMapKind);
var
  I: Integer;
  dest: UnicodeString;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  if Trim(aName) = '' then Exit;

  Lock;
  try
    dest := ExeDir + MAP_FOLDER_NAME[aMapKind] + PathDelim + aName + PathDelim;
    Assert(fMaps[aIndex].Dir <> dest);

    KMMoveFolder(fMaps[aIndex].Dir, dest);

    //Remove the map from our list
    fMaps[aIndex].Free;
    for I  := aIndex to fCount - 2 do
      fMaps[I] := fMaps[I + 1];
    Dec(fCount);
    SetLength(fMaps, fCount);
  finally
    Unlock;
  end;
end;


//For private access, where CS is managed by the caller
procedure TKMapsCollection.DoSort;
var
  tempMaps: array of TKMMapInfo;

  //Return True if items should be exchanged
  function Compare(A, B: TKMMapInfo): Boolean;
  begin
    Result := False; //By default everything remains in place
    case fSortMethod of
      smByFavouriteAsc:       Result := A.IsFavourite and not B.IsFavourite;
      smByFavouriteDesc:      Result := not A.IsFavourite and B.IsFavourite;
      smByNameAsc:            Result := CompareTextLogical(A.Name, B.Name) < 0;
      smByNameDesc:           Result := CompareTextLogical(A.Name, B.Name) > 0;
      smBySizeAsc:            // Compare by actual map area, size indexes will be sorted automatically
                              Result := A.MapSizeX*A.MapSizeY < B.MapSizeX*B.MapSizeY;
      smBySizeDesc:           // Compare by actual map area, size indexes will be sorted automatically
                              Result := A.MapSizeX*A.MapSizeY > B.MapSizeX*B.MapSizeY;
      smByPlayersAsc:         Result := A.LocCount < B.LocCount;
      smByPlayersDesc:        Result := A.LocCount > B.LocCount;
      smByHumanPlayersAsc:    Result := A.HumanPlayerCount < B.HumanPlayerCount;
      smByHumanPlayersDesc:   Result := A.HumanPlayerCount > B.HumanPlayerCount;
      smByHumanPlayersMPAsc:  Result := A.HumanPlayerCountMP < B.HumanPlayerCountMP;
      smByHumanPlayersMPDesc: Result := A.HumanPlayerCountMP > B.HumanPlayerCountMP;
      smByMissionModeAsc:     Result := A.MissionMode < B.MissionMode;
      smByMissionModeDesc:    Result := A.MissionMode > B.MissionMode;
    end;
    if fDoSortWithFavourites and not (fSortMethod in [smByFavouriteAsc, smByFavouriteDesc]) then
    begin
      if A.IsFavourite and not B.IsFavourite then
        Result := False
      else if not A.IsFavourite and B.IsFavourite then
        Result := True
    end;

  end;

  procedure MergeSort(aLeft, aRight: Integer);
  var
    middle, I, J, ind1, ind2: integer;
  begin
    if aRight <= aLeft then
      exit;

    middle := (aLeft+aRight) div 2;
    MergeSort(aLeft, middle);
    Inc(middle);
    MergeSort(middle, aRight);
    ind1 := aLeft;
    ind2 := middle;
    for I := aLeft to aRight do
    begin
      if (ind1 < middle) and ((ind2 > aRight) or not Compare(fMaps[ind1], fMaps[ind2])) then
      begin
        tempMaps[I] := fMaps[ind1];
        Inc(ind1);
      end
      else
      begin
        tempMaps[I] := fMaps[ind2];
        Inc(ind2);
      end;
    end;
    for J := aLeft to aRight do
      fMaps[J] := tempMaps[J];
  end;
begin
  SetLength(tempMaps, fCount);
  MergeSort(0, fCount - 1);
end;


//For public access
//Apply new Sort within Critical Section, as we could be in the Refresh phase
//note that we need to preserve fScanning flag
procedure TKMapsCollection.Sort(aSortMethod: TKMapsSortMethod; aOnSortComplete: TNotifyEvent);
begin
  Lock;
  try
    if fScanning then
    begin
      fScanning := False;
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
      fScanning := True;
    end
    else
    begin
      fSortMethod := aSortMethod;
      DoSort;
      if Assigned(aOnSortComplete) then
        aOnSortComplete(Self);
    end;
  finally
    Unlock;
  end;
end;


procedure TKMapsCollection.TerminateScan;
begin
  if (fScanner <> nil) then
  begin
    fScanner.Terminate;
    fScanner.WaitFor;
    fScanner.Free;
    fScanner := nil;
    fScanning := False;
  end;
  fUpdateNeeded := False; //If the scan was terminated we should not run fOnRefresh next UpdateState
end;


//Start the refresh of maplist
procedure TKMapsCollection.Refresh(aOnRefresh: TNotifyEvent; aOnTerminate: TNotifyEvent = nil; aOnComplete: TNotifyEvent = nil);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fOnRefresh := aOnRefresh;
  fOnComplete := aOnComplete;
  fOnTerminate := aOnTerminate;

  //Scan will launch upon create automatically
  fScanning := True;
  fScanner := TTMapsScanner.Create(fMapFolders, MapAdd, MapAddDone, ScanTerminate, ScanComplete);
end;


procedure TKMapsCollection.MapAdd(aMap: TKMMapInfo);
begin
  Lock;
  try
    SetLength(fMaps, fCount + 1);
    fMaps[fCount] := aMap;
    Inc(fCount);

    //Set the scanning to False so we could Sort
    fScanning := False;

    //Keep the maps sorted
    //We signal from Locked section, so everything caused by event can safely access our Maps
    DoSort;

    fScanning := True;
  finally
    Unlock;
  end;
end;


procedure TKMapsCollection.MapAddDone(Sender: TObject);
begin
  fUpdateNeeded := True; //Next time the GUI thread calls UpdateState we will run fOnRefresh
end;


//All maps have been scanned
//No need to resort since that was done in last MapAdd event
procedure TKMapsCollection.ScanComplete(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    if Assigned(fOnComplete) then
      fOnComplete(Self);
  finally
    Unlock;
  end;
end;


//Scan was terminated
//No need to resort since that was done in last MapAdd event
procedure TKMapsCollection.ScanTerminate(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    if Assigned(fOnTerminate) then
      fOnTerminate(Self);
  finally
    Unlock;
  end;
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMultiplayer: Boolean): string;
begin
  Result := FullPath(aName, aExt, GetMapKind(aMultiplayer));
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMapKind: TKMMapKind): string;
begin
  Result := ExeDir + MAP_FOLDER_NAME[aMapKind] + PathDelim + aName + PathDelim + aName + aExt;
end;


class function TKMapsCollection.FullPath(const aDirName, aFileName, aExt: string; aMapKind: TKMMapKind): string;
begin
  Result := ExeDir + MAP_FOLDER_NAME[aMapKind] + PathDelim + aDirName + PathDelim + aFileName + aExt;
end;


class function TKMapsCollection.FullPath(const aName, aExt: string; aMapKind: TKMMapKind; aCRC: Cardinal): string;
var
  S: UnicodeString;
begin
  S := aName;
  if aMapKind = mkDL then
    S := S + '_' + IntToHex(Integer(aCRC), 8);
  Result := FullPath(S, aExt, aMapKind);
end;


class function TKMapsCollection.GetMapCRC(const aMapPath: string): Cardinal;
begin
  Result := 0;
  if FileExists(aMapPath) then
    Result := Adler32CRC(aMapPath);
end;


class procedure TKMapsCollection.GetAllMapPaths(const aExeDir: string; aList: TStringList);
var
  I: Integer;
  searchRec: TSearchRec;
  pathToMaps: TStringList;
begin
  aList.Clear;

  pathToMaps := TStringList.Create;
  try
    pathToMaps.Add(aExeDir + MAPS_FOLDER_NAME + PathDelim);
    pathToMaps.Add(aExeDir + MAPS_MP_FOLDER_NAME + PathDelim);
    pathToMaps.Add(aExeDir + TUTORIALS_FOLDER_NAME + PathDelim);

    //Include all campaigns maps
    FindFirst(aExeDir + CAMPAIGNS_FOLDER_NAME + PathDelim + '*', faDirectory, searchRec);
    try
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          pathToMaps.Add(aExeDir + CAMPAIGNS_FOLDER_NAME + PathDelim + searchRec.Name + PathDelim);
      until (FindNext(searchRec) <> 0);
    finally
      FindClose(searchRec);
    end;

    //Include all campaigns maps
    FindFirst(aExeDir + CAMPAIGNSMP_FOLDER_NAME + PathDelim + '*', faDirectory, searchRec);
    try
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          pathToMaps.Add(aExeDir + CAMPAIGNS_FOLDER_NAME + PathDelim + searchRec.Name + PathDelim);
      until (FindNext(searchRec) <> 0);
    finally
      FindClose(searchRec);
    end;

    for I := 0 to pathToMaps.Count - 1 do
    if DirectoryExists(pathToMaps[I]) then
    begin
      FindFirst(pathToMaps[I] + '*', faDirectory, searchRec);
      try
        repeat
          if (searchRec.Name <> '.') and (searchRec.Name <> '..')
          and FileExists(pathToMaps[I] + searchRec.Name + PathDelim + searchRec.Name + '.dat')
          and FileExists(pathToMaps[I] + searchRec.Name + PathDelim + searchRec.Name + '.map') then
            aList.Add(pathToMaps[I] + searchRec.Name + PathDelim + searchRec.Name + '.dat');
        until (FindNext(searchRec) <> 0);
      finally
        FindClose(searchRec);
      end;
    end;
  finally
    pathToMaps.Free;
  end;
end;

class procedure TKMapsCollection.GetMapLayersFiles(const aName : String; var aList: TStringList);
var Paths, fPath : String;
  Lists : array[0..4] of TStringList;
  I, K : Integer;
begin
  for I := 0 to High(Lists) do
    Lists[I] := TStringList.Create;
  fPath := GetMapPath(aName) + 'Layers';
  
  aList.Clear;
  if not DirectoryExists(fPath) then Exit;

  for Paths in TDirectory.GetFiles(fPath + PathDelim)do
    if ExtractFileExt(Paths) = '.tiles' then
      Lists[0].Add(ExtractRelativePath(fPath + PathDelim, Paths))
    else
    if ExtractFileExt(Paths) = '.height' then
      Lists[1].Add(ExtractRelativePath(fPath + PathDelim, Paths))
    else
    if ExtractFileExt(Paths) = '.objects' then
      Lists[2].Add(ExtractRelativePath(fPath + PathDelim, Paths))
    else
    if ExtractFileExt(Paths) = '.overlays' then
      Lists[3].Add(ExtractRelativePath(fPath + PathDelim, Paths))
    else
    if ExtractFileExt(Paths) = '.terrain' then
      Lists[4].Add(ExtractRelativePath(fPath + PathDelim, Paths));

  for I := 0 to 4 do
      for K := 0 to Lists[I].Count - 1 do
        aList.Add(Lists[I].Strings[K]);

end;
class function TKMapsCollection.GetMapPath(const aName: string): string;
begin
  Result := ExtractFilePath(FullPath(Trim(aName), '.map', gGameParams.IsMultiplayer));
end;

{ TTCustomMapsScanner }
constructor TTCustomMapsScanner.Create(aMapKinds: TKMMapKindSet; aOnComplete: TNotifyEvent = nil);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  fMapKinds := aMapKinds;
  fOnComplete := aOnComplete;
  FreeOnTerminate := False;
end;


procedure TTCustomMapsScanner.Execute;
var
  searchRec: TSearchRec;
  pathToMaps: string;
  MK: TKMMapKind;
begin
  gLog.MultithreadLogging := True; // We could log smth while create map cache or scan maps
  try
    try
      for MK in fMapKinds do
      begin
        pathToMaps := ExeDir + MAP_FOLDER_NAME[MK] + PathDelim;

        if not DirectoryExists(pathToMaps) then Continue;

        FindFirst(pathToMaps + '*', faDirectory, searchRec);
        try
          repeat
            if (searchRec.Name <> '.') and (searchRec.Name <> '..')
              and FileExists(TKMapsCollection.FullPath(searchRec.Name, '.dat', MK))
              and FileExists(TKMapsCollection.FullPath(searchRec.Name, '.map', MK)) then
            begin
              try
                ProcessMap(searchRec.Name, MK);
              except
                on E: Exception do
                  gLog.AddTime('Error loading map ''' + searchRec.Name + ''''); //Just silently log an exception
              end;
            end;
          until (FindNext(searchRec) <> 0) or Terminated;
        finally
          FindClose(searchRec);
        end;
      end;
    finally
      if not Terminated and Assigned(fOnComplete) then
        fOnComplete(Self);
    end;
  finally
    gLog.MultithreadLogging := False;
  end;
end;


{ TTMapsScanner }
//aOnMapAdd - signal that there's new map that should be added
//aOnMapAddDone - signal that map has been added
//aOnTerminate - scan was terminated (but could be not complete yet)
//aOnComplete - scan is complete
constructor TTMapsScanner.Create(aMapFolders: TKMMapKindSet; aOnMapAdd: TKMapEvent; aOnMapAddDone, aOnTerminate: TNotifyEvent; aOnComplete: TNotifyEvent = nil);
begin
  inherited Create(aMapFolders, aOnComplete);

  Assert(Assigned(aOnMapAdd));

  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging('MapsScanner', ThreadID);
  {$ENDIF}

  fOnMapAdd := aOnMapAdd;
  fOnMapAddDone := aOnMapAddDone;
  OnTerminate := aOnTerminate;
  FreeOnTerminate := False;
end;


procedure TTMapsScanner.ProcessMap(const aPath: UnicodeString; aKind: TKMMapKind);
var
  map: TKMMapInfo;
begin
  map := TKMMapInfo.Create(aPath, False, aKind);

  if SLOW_MAP_SCAN then
    Sleep(50);

  fOnMapAdd(map);
  fOnMapAddDone(Self);
end;


{ TTMapsCacheUpdater }
constructor TTMapsCacheUpdater.Create(aMapFolders: TKMMapKindSet);
begin
  inherited Create(aMapFolders);

  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging('MapsCacheUpdater', ThreadID);
  {$ENDIF}

  FreeOnTerminate := True;
end;


procedure TTMapsCacheUpdater.ProcessMap(const aPath: UnicodeString; aKind: TKMMapKind);
var
  map: TKMMapInfo;
begin
  //Simply creating the TKMMapInfo updates the .mi cache file
  if not fIsStopped then
  begin
    map := TKMMapInfo.Create(aPath, False, aKind);
    map.Free;
  end;
end;


procedure TTMapsCacheUpdater.Stop;
begin
  if Self <> nil then
    fIsStopped := True;
end;


end.

