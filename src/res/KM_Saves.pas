unit KM_Saves;
{$I KaM_Remake.inc}
interface
uses
  Classes, SyncObjs,
  KM_GameOptions, KM_GameInfo, KM_Minimap;


type
  TKMSavesSortMethod = (
    smByFileNameAsc, smByFileNameDesc,
    smByMapNameAsc, smByMapNameDesc,
    smByGameVersionAsc, smByGameVersionDesc,
    smByDescriptionAsc, smByDescriptionDesc,
    smByTimeAsc, smByTimeDesc,
    smByDateAsc, smByDateDesc,
    smByPlayerCountAsc, smByPlayerCountDesc,
    smByModeAsc, smByModeDesc);

  TKMSaveInfo = class;
  TKMSaveEvent = procedure (aSave: TKMSaveInfo) of object;

  TKMSaveInfoErrorType = (sietNone, sietFileNotExist, sietUnsupportedMods, sietUnsupportedFormat, sietUnsupportedVersion);

  TKMSaveInfoError = record
    ErrorString: UnicodeString;
    ErrorType: TKMSaveInfoErrorType;
  end;

  //Savegame info, most of which is stored in TKMGameInfo structure
  TKMSaveInfo = class
  private
    fPath: string; //TKMGameInfo does not stores paths, because they mean different things for Maps and Saves
    fFileName: string; //without extension
    fCrcCalculated: Boolean;
    fCRC: Cardinal;
    fSaveError: TKMSaveInfoError;
    fGameInfo: TKMGameInfo;
    fGameOptions: TKMGameOptions;
    procedure ScanSave;
    function GetCRC: Cardinal;
    procedure ResetSaveError;
    function IsValid(aStrict: Boolean): Boolean; overload;
  public
    constructor Create(const aName: String; aIsMultiplayer: Boolean);
    destructor Destroy; override;

    property GameInfo: TKMGameInfo read fGameInfo;
    property GameOptions: TKMGameOptions read fGameOptions;
    property Path: string read fPath;
    property FileName: string read fFileName;
    property CRC: Cardinal read GetCRC;
    property SaveError: TKMSaveInfoError read fSaveError;

    function IsValid: Boolean; overload;
    function IsValidStrictly: Boolean;
    function IsMultiplayer: Boolean;
    function IsReplayValid: Boolean;
    function LoadMinimap(aMinimap: TKMMinimap): Boolean; overload;
    function LoadMinimap(aMinimap: TKMMinimap; aChoosenStartLoc: Integer): Boolean; overload;
  end;

  TKMSavesScanner = class(TThread)
  private
    fMultiplayerPath: Boolean;
    fOnSaveAdd: TKMSaveEvent;
    fOnSaveAddDone: TNotifyEvent;
    fOnComplete: TNotifyEvent;
  public
    constructor Create(aMultiplayerPath: Boolean; aOnSaveAdd: TKMSaveEvent; aOnSaveAddDone, aOnTerminate, aOnComplete: TNotifyEvent);
    procedure Execute; override;
  end;

  TKMSavesCollection = class
  private
    fCount: Word;
    fSaves: array of TKMSaveInfo;
    fSortMethod: TKMSavesSortMethod;
    fCriticalSection: TCriticalSection;
    fScanner: TKMSavesScanner;
    fScanning: Boolean;
    fScanFinished: Boolean;
    fUpdateNeeded: Boolean;
    fOnRefresh: TNotifyEvent;
    fOnComplete: TNotifyEvent;
    fOnTerminate: TNotifyEvent;
    procedure Clear;
    procedure SaveAdd(aSave: TKMSaveInfo);
    procedure SaveAddDone(Sender: TObject);
    procedure ScanComplete(Sender: TObject);
    procedure ScanTerminate(Sender: TObject);
    procedure DoSort;
    function GetSave(aIndex: Integer): TKMSaveInfo;
  public
    constructor Create(aSortMethod: TKMSavesSortMethod = smByFileNameDesc);
    destructor Destroy; override;

    property Count: Word read fCount;
    property SavegameInfo[aIndex: Integer]: TKMSaveInfo read GetSave; default;
    procedure Lock;
    procedure Unlock;

    class function Path(const aName: UnicodeString; aIsMultiplayer: Boolean): UnicodeString;
    class function FullPath(const aName, aExt: UnicodeString; aIsMultiplayer: Boolean): UnicodeString;
    class function GetSaveCRC(const aName: UnicodeString; aIsMultiplayer: Boolean): Cardinal;
    class function GetSaveFolder(aIsMultiplayer: Boolean): UnicodeString;

    procedure Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean; aOnTerminate: TNotifyEvent = nil; aOnComplete: TNotifyEvent = nil);
    procedure TerminateScan;
    procedure Sort(aSortMethod: TKMSavesSortMethod; aOnSortComplete: TNotifyEvent);
    property SortMethod: TKMSavesSortMethod read fSortMethod; //Read-only because we should not change it while Refreshing
    property ScanFinished: Boolean read fScanFinished;

    function Contains(const aNewName: UnicodeString): Boolean;
    procedure DeleteSave(aIndex: Integer);
    procedure MoveSave(aIndex: Integer; const aName: UnicodeString);
    procedure RenameSave(aIndex: Integer; const aName: UnicodeString);

    function SavesList: UnicodeString;
    procedure UpdateState;
  end;


implementation
uses
  SysUtils, Math, KromUtils,
  KM_GameClasses,
  KM_Resource, KM_ResTexts, KM_FileIO,
  KM_CommonClasses, KM_Defaults, KM_CommonUtils, KM_Log,
  KM_GameTypes, KM_Utils;


{ TKMSaveInfo }
constructor TKMSaveInfo.Create(const aName: String; aIsMultiplayer: Boolean);
begin
  inherited Create;
  fPath := TKMSavesCollection.Path(aName, aIsMultiplayer);
  fFileName := aName;
  fGameInfo := TKMGameInfo.Create;
  fGameOptions := TKMGameOptions.Create;

  ResetSaveError;

  //We could postpone this step till info is actually required
  //but we do need title and TickCount right away, so it's better just to scan it ASAP
  ScanSave;
end;


procedure TKMSaveInfo.ResetSaveError;
begin
  fSaveError.ErrorString := '';
  fSaveError.ErrorType := sietNone;
end;


destructor TKMSaveInfo.Destroy;
begin
  fGameInfo.Free;
  fGameOptions.Free;
  inherited;
end;


function TKMSaveInfo.GetCRC: Cardinal;
begin
  if not fCrcCalculated then
  begin
    fCRC := Adler32CRC(fPath + fFileName + EXT_SAVE_MAIN_DOT);
    fCrcCalculated := True;
  end;
  Result := fCRC;
end;


procedure TKMSaveInfo.ScanSave;
var
  loadStream, headerStream: TKMemoryStream;
  compressedSaveBody: Boolean;
begin
  if not FileExists(fPath + fFileName + EXT_SAVE_MAIN_DOT) then
  begin
    fSaveError.ErrorString := gResTexts[TX_SAVE_SAVE_FILE_NOT_EXISTS];
    fSaveError.ErrorType := sietFileNotExist;
    Exit;
  end;

  fCrcCalculated := False; //make lazy load for CRC

  loadStream := TKMemoryStreamBinary.Create; //Read data from file into stream
  headerStream := TKMemoryStreamBinary.Create;
  loadStream.LoadFromFile(fPath + fFileName + EXT_SAVE_MAIN_DOT);

  loadStream.Read(compressedSaveBody);
  loadStream.LoadToStream(headerStream, SAVE_HEADER_MARKER);

  fGameInfo.Load(headerStream);
  fGameOptions.Load(headerStream);

  fSaveError.ErrorString := fGameInfo.ParseError.ErrorString;
  case fGameInfo.ParseError.ErrorType of
    gipetNone: ;
    gipetUnsupportedFormat:   fSaveError.ErrorType := sietUnsupportedFormat;
    gipetUnsupportedVersion:  fSaveError.ErrorType := sietUnsupportedVersion;
  end;

  if (fSaveError.ErrorType = sietNone) and (fGameInfo.DATCRC <> gRes.GetDATCRC) then
  begin
    fSaveError.ErrorString := gResTexts[TX_SAVE_UNSUPPORTED_MODS];
    fSaveError.ErrorType := sietUnsupportedMods;
  end;

  if not ((fSaveError.ErrorType = sietNone)
      or (ALLOW_LOAD_UNSUP_VERSION_SAVE and (fSaveError.ErrorType = sietUnsupportedVersion))) then
    fGameInfo.Title := fSaveError.ErrorString;

  headerStream.Free;
  loadStream.Free;
end;


function TKMSaveInfo.LoadMinimap(aMinimap: TKMMinimap): Boolean;
begin
  Result := LoadMinimap(aMinimap, LOC_ANY);
end;


// Try to laod save minimap.
// MP game save minimap is stored in a separate file (.sloc)
// SP game save minimap in stored in a main save file (.sav)
// But players could place saves in wrong folders occasionaly or intentionaly
// in that case we still could try to load save
function TKMSaveInfo.LoadMinimap(aMinimap: TKMMinimap; aChoosenStartLoc: Integer): Boolean;

  function LoadMPMinimap(aFilePath: string): Boolean;
  var
    gameLocalData: TKMGameMPLocalData;
  begin
    Result := False;
    try
      // Lets try to load Minimap for MP save
      gameLocalData := TKMGameMPLocalData.Create(0, aChoosenStartLoc, aMinimap);
      try
        Result := GameLocalData.LoadFromFile(aFilePath);
      finally
        gameLocalData.Free;
      end;
    except
      // Ignore any errors, because MP minimap is optional
      on E: Exception do
        // Just log error to log, do not crash game in case of any error here
        gLog.AddTime('Load MP save minimap from file '
          + aFilePath + ' exception: ' + E.ClassName + ': ' + E.Message
          {$IFDEF WDC} + sLineBreak + E.StackTrace {$ENDIF}
          );
    end;
  end;

  function LoadSPMinimap: Boolean;
  var
    loadStream, headerS: TKMemoryStream;
    dummyGameInfo: TKMGameInfo;
    dummyGameOptions: TKMGameOptions;
    compressedSaveBody, isMultiplayerFlagInSave: Boolean;
  begin
    Result := False;
    dummyGameInfo := TKMGameInfo.Create;
    dummyGameOptions := TKMGameOptions.Create;
    loadStream := TKMemoryStreamBinary.Create; //Read data from file into stream
    headerS := TKMemoryStreamBinary.Create;
    try
      loadStream.LoadFromFile(fPath + fFileName + EXT_SAVE_MAIN_DOT);
      loadStream.Read(compressedSaveBody); // Should be always True

      loadStream.LoadToStream(headerS, SAVE_HEADER_MARKER);

      dummyGameInfo.Load(headerS);
      dummyGameOptions.Load(headerS);
      headerS.Read(isMultiplayerFlagInSave);

      // Skip minimap load if save is actually made in MP game.
      // We store MP minimap in a separate file .sloc
      if not isMultiplayerFlagInSave then
      begin
        aMinimap.LoadFromStream(headerS);
        Result := True;
      end;  
    finally
      headerS.Free;
      loadStream.Free;
      dummyGameOptions.Free;
      dummyGameInfo.Free;
    end;
  end;

var
  localDataFilePath: string;
begin
  Result := False;
  if not FileExists(fPath + fFileName + EXT_SAVE_MAIN_DOT) then Exit;

  localDataFilePath := fPath + fFileName + EXT_SAVE_MP_LOCAL_DOT;

  // Check if we have MP minimap file in the save
  if FileExists(localDataFilePath) then
    Result := LoadMPMinimap(localDataFilePath)
  else
    Result := LoadSPMinimap;
end;


function TKMSaveInfo.IsValid(aStrict: Boolean): Boolean;
begin
  if not ALLOW_LOAD_UNSUP_VERSION_SAVE then
    aStrict := True;
  Result := FileExists(fPath + fFileName + EXT_SAVE_MAIN_DOT)
            and ((fSaveError.ErrorType = sietNone)
                 or (not aStrict and (fSaveError.ErrorType = sietUnsupportedVersion)))
            and fGameInfo.IsValid(True);
end;


//Check if save is valid (could be valid even for unsupported version)
function TKMSaveInfo.IsValid: Boolean;
begin
  Result := IsValid(False);
end;


//Check if save is valid (could NOT be valid for unsupported version)
function TKMSaveInfo.IsValidStrictly: Boolean;
begin
  Result := IsValid(True);
end;


function TKMSaveInfo.IsMultiplayer: Boolean;
begin
  Result := GetFileDirName(Copy(fPath, 0, Length(fPath) - 1)) = SAVES_MP_FOLDER_NAME;
end;


//Check if replay files exist at location
function TKMSaveInfo.IsReplayValid: Boolean;
begin
  Result := FileExists(fPath + fFileName + EXT_SAVE_BASE_DOT) and
            FileExists(fPath + fFileName + EXT_SAVE_REPLAY_DOT);
end;


{ TKMSavesCollection }
constructor TKMSavesCollection.Create(aSortMethod: TKMSavesSortMethod = smByFileNameDesc);
begin
  inherited Create;

  fSortMethod := aSortMethod;
  fScanFinished := True;

  //CS is used to guard sections of code to allow only one thread at once to access them
  //We mostly don't need it, as UI should access Maps only when map events are signaled
  //it acts as a safenet mostly
  fCriticalSection := TCriticalSection.Create;
end;


destructor TKMSavesCollection.Destroy;
begin
  //Terminate and release the Scanner if we have one working or finished
  TerminateScan;

  //Release TKMMapInfo objects
  Clear;

  fCriticalSection.Free;
  inherited;
end;


procedure TKMSavesCollection.Lock;
begin
  fCriticalSection.Enter;
end;


procedure TKMSavesCollection.Unlock;
begin
  fCriticalSection.Leave;
end;


procedure TKMSavesCollection.Clear;
var
  I: Integer;
begin
  Assert(not fScanning, 'Guarding from access to inconsistent data');
  for I := 0 to fCount - 1 do
    fSaves[I].Free;
  fCount := 0;
  SetLength(fSaves, 0); //We could use Low and High. Need to reset array to 0 length
end;


function TKMSavesCollection.GetSave(aIndex: Integer): TKMSaveInfo;
begin
  //No point locking/unlocking here since we return a TObject that could be modified/freed
  //by another thread before the caller uses it.
  Assert(InRange(aIndex, 0, fCount-1));
  Result := fSaves[aIndex];
end;


class function TKMSavesCollection.GetSaveCRC(const aName: UnicodeString; aIsMultiplayer: Boolean): Cardinal;
var
  savePath: UnicodeString;
begin
  Result := 0;
  savePath := FullPath(aName, EXT_SAVE_MAIN, aIsMultiplayer);
  if FileExists(savePath) then
    Result := Adler32CRC(savePath);
end;


class function TKMSavesCollection.GetSaveFolder(aIsMultiplayer: Boolean): UnicodeString;
begin
  if aIsMultiplayer then
    Result := SAVES_MP_FOLDER_NAME
  else
    Result := SAVES_FOLDER_NAME;
end;


function TKMSavesCollection.Contains(const aNewName: UnicodeString): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to fCount - 1 do
    if LowerCase(fSaves[I].FileName) = LowerCase(aNewName) then
    begin
      Result := True;
      Exit;
    end;
end;


procedure TKMSavesCollection.DeleteSave(aIndex: Integer);
var
  I: Integer;
begin
  Lock;
  try
    Assert(InRange(aIndex, 0, fCount-1));
    KMDeleteFolderToBin(fSaves[aIndex].Path);
    fSaves[aIndex].Free;
    for I := aIndex to fCount - 2 do
      fSaves[I] := fSaves[I+1]; //Move them down
    Dec(fCount);
    SetLength(fSaves, fCount);
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.MoveSave(aIndex: Integer; const aName: UnicodeString);
var
  I: Integer;
  dest: UnicodeString;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  if Trim(aName) = '' then Exit;

  Lock;
  try
    dest := Path(aName, fSaves[aIndex].IsMultiplayer);
    Assert(fSaves[aIndex].Path <> dest);

    KMMoveFolder(fSaves[aIndex].Path, dest);

    //Remove the map from our list
    fSaves[aIndex].Free;
    for I  := aIndex to fCount - 2 do
      fSaves[I] := fSaves[I + 1];
    Dec(fCount);
    SetLength(fSaves, fCount);
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.RenameSave(aIndex: Integer; const aName: UnicodeString);
begin
  MoveSave(aIndex, aName);
end;


//For private acces, where CS is managed by the caller
procedure TKMSavesCollection.DoSort;
var
  tempSaves: array of TKMSaveInfo;

  //Return True if items should be exchanged
  function Compare(A, B: TKMSaveInfo): Boolean;
  begin
    Result := False; //By default everything remains in place
    case fSortMethod of
      smByFileNameAsc:     Result := CompareText(A.FileName, B.FileName) < 0;
      smByFileNameDesc:    Result := CompareText(A.FileName, B.FileName) > 0;
      smByMapNameAsc:      Result := CompareTextLogical(A.GameInfo.Title, B.GameInfo.Title) < 0;
      smByMapNameDesc:     Result := CompareTextLogical(A.GameInfo.Title, B.GameInfo.Title) > 0;
      smByGameVersionAsc:  Result := GetGameVersionNum(A.GameInfo.Version) < GetGameVersionNum(B.GameInfo.Version);
      smByGameVersionDesc: Result := GetGameVersionNum(A.GameInfo.Version) > GetGameVersionNum(B.GameInfo.Version);
      smByDescriptionAsc:  Result := CompareText(A.GameInfo.GetTitleWithTime, B.GameInfo.GetTitleWithTime) < 0;
      smByDescriptionDesc: Result := CompareText(A.GameInfo.GetTitleWithTime, B.GameInfo.GetTitleWithTime) > 0;
      smByTimeAsc:         Result := A.GameInfo.TickCount < B.GameInfo.TickCount;
      smByTimeDesc:        Result := A.GameInfo.TickCount > B.GameInfo.TickCount;
      smByDateAsc:         Result := A.GameInfo.SaveTimestamp > B.GameInfo.SaveTimestamp;
      smByDateDesc:        Result := A.GameInfo.SaveTimestamp < B.GameInfo.SaveTimestamp;
      smByPlayerCountAsc:  Result := A.GameInfo.PlayerCount < B.GameInfo.PlayerCount;
      smByPlayerCountDesc: Result := A.GameInfo.PlayerCount > B.GameInfo.PlayerCount;
      smByModeAsc:         Result := A.GameInfo.MissionMode < B.GameInfo.MissionMode;
      smByModeDesc:        Result := A.GameInfo.MissionMode > B.GameInfo.MissionMode;
    end;
  end;

  procedure MergeSort(left, right: integer);
  var
    I, J, middle, ind1, ind2: integer;
  begin
    if right <= left then
      exit;

    middle := (left+right) div 2;
    MergeSort(left, middle);
    Inc(middle);
    MergeSort(middle, right);
    ind1 := left;
    ind2 := middle;
    for I := left to right do
    begin
      if (ind1 < middle) and ((ind2 > right) or not Compare(fSaves[ind1], fSaves[ind2])) then
      begin
        tempSaves[I] := fSaves[ind1];
        Inc(ind1);
      end
      else
      begin
        tempSaves[I] := fSaves[ind2];
        Inc(ind2);
      end;
    end;
    for J := left to right do
      fSaves[J] := tempSaves[J];
  end;

  procedure SortByVersion;
  var I, J : Integer;
  begin
    J := 0;
    //supported version first
    for I := 0 to fCount-1 do
    if fSaves[I].IsValid then
    begin
      tempSaves[J] := fSaves[I];
      inc(J);
    end;

    //now unsupperted
    for I := 0 to fCount-1 do
    if not fSaves[I].IsValid then
    begin
      tempSaves[J] := fSaves[I];
      inc(J);
    end;

    for I := 0 to fCount-1 do
      fSaves[I] := tempSaves[I];


  end;

begin
  SetLength(tempSaves, fCount);
  MergeSort(0, fCount-1);

  SortByVersion;


end;


class function TKMSavesCollection.Path(const aName: UnicodeString; aIsMultiplayer: Boolean): UnicodeString;
begin
  Result := ExeDir + GetSaveFolder(aIsMultiplayer) + PathDelim + aName + PathDelim;
end;


class function TKMSavesCollection.FullPath(const aName, aExt: UnicodeString; aIsMultiplayer: Boolean): UnicodeString;
begin
  Result := Path(aName, aIsMultiplayer) + aName + '.' + aExt;
end;


function TKMSavesCollection.SavesList: UnicodeString;
var
  I: Integer;
begin
  Lock;
  try
    Result := '';
    for I := 0 to fCount - 1 do
      Result := Result + fSaves[I].FileName + EolW;
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.UpdateState;
begin
  if Self = nil then Exit;

  if not fUpdateNeeded then Exit;

  if Assigned(fOnRefresh) then
    fOnRefresh(Self);

  fUpdateNeeded := False;
end;


//For public access
//Apply new Sort within Critical Section, as we could be in the Refresh phase
//note that we need to preserve fScanning flag
procedure TKMSavesCollection.Sort(aSortMethod: TKMSavesSortMethod; aOnSortComplete: TNotifyEvent);
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


procedure TKMSavesCollection.TerminateScan;
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
procedure TKMSavesCollection.Refresh(aOnRefresh: TNotifyEvent; aMultiplayerPath: Boolean; aOnTerminate: TNotifyEvent = nil; aOnComplete: TNotifyEvent = nil);
begin
  //Terminate previous Scanner if two scans were launched consequentialy
  TerminateScan;
  Clear;

  fScanFinished := False;
  fOnRefresh := aOnRefresh;
  fOnComplete := aOnComplete;
  fOnTerminate := aOnTerminate;

  //Scan will launch upon create automatcally
  fScanning := True;
  fScanner := TKMSavesScanner.Create(aMultiplayerPath, SaveAdd, SaveAddDone, ScanTerminate, ScanComplete);
end;


procedure TKMSavesCollection.SaveAdd(aSave: TKMSaveInfo);
begin
  Lock;
  try
    SetLength(fSaves, fCount + 1);
    fSaves[fCount] := aSave;
    Inc(fCount);

    //Set the scanning to False so we could Sort
    fScanning := False;

    //Keep the saves sorted
    //We signal from Locked section, so everything caused by event can safely access our Saves
    DoSort;

    fScanning := True;
  finally
    Unlock;
  end;
end;


procedure TKMSavesCollection.SaveAddDone(Sender: TObject);
begin
  fUpdateNeeded := True; //Next time the GUI thread calls UpdateState we will run fOnRefresh
end;


procedure TKMSavesCollection.ScanComplete(Sender: TObject);
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


//All saves have been scanned
//No need to resort since that was done in last SaveAdd event
procedure TKMSavesCollection.ScanTerminate(Sender: TObject);
begin
  Lock;
  try
    fScanning := False;
    fScanFinished := True;
    if Assigned(fOnTerminate) then
      fOnTerminate(Self);
  finally
    Unlock;
  end;
end;


{ TTSavesScanner }
//aOnSaveAdd - signal that there's new save that should be added
//aOnSaveAddDone - signal that save has been added
//aOnTerminate - scan was terminated (but could be not complete yet)
//aOnComplete - scan is complete
constructor TKMSavesScanner.Create(aMultiplayerPath: Boolean; aOnSaveAdd: TKMSaveEvent; aOnSaveAddDone, aOnTerminate, aOnComplete: TNotifyEvent);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  Assert(Assigned(aOnSaveAdd));

  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging('SavesScanner', ThreadID);
  {$ENDIF}

  fMultiplayerPath := aMultiplayerPath;
  fOnSaveAdd := aOnSaveAdd;
  fOnSaveAddDone := aOnSaveAddDone;
  fOnComplete := aOnComplete;
  OnTerminate := aOnTerminate;
  FreeOnTerminate := False;
end;


procedure TKMSavesScanner.Execute;
var
  pathToSaves: string;
  searchRec: TSearchRec;
  save: TKMSaveInfo;
begin
  gLog.MultithreadLogging := True; // We could log smth while doing saves scan
  try
    try
      pathToSaves := ExeDir + TKMSavesCollection.GetSaveFolder(fMultiplayerPath) + PathDelim;

      if not DirectoryExists(pathToSaves) then Exit;

      FindFirst(pathToSaves + '*', faDirectory, searchRec);
      try
        repeat
          if (searchRec.Name <> '.') and (searchRec.Name <> '..')
            and FileExists(TKMSavesCollection.FullPath(searchRec.Name, EXT_SAVE_MAIN, fMultiplayerPath))
            and FileExists(TKMSavesCollection.FullPath(searchRec.Name, EXT_SAVE_REPLAY, fMultiplayerPath))
            and FileExists(TKMSavesCollection.FullPath(searchRec.Name, EXT_SAVE_BASE, fMultiplayerPath)) then
          begin
            try
              save := TKMSaveInfo.Create(searchRec.Name, fMultiplayerPath);
              if SLOW_SAVE_SCAN then
                Sleep(50);
              fOnSaveAdd(save);
              fOnSaveAddDone(Self);
            except
              on E: Exception do
                gLog.AddTime('Error loading save ''' + searchRec.Name + ''''); //Just silently log an exception
            end;
          end;
        until (FindNext(searchRec) <> 0) or Terminated;
      finally
        FindClose(searchRec);
      end;
    finally
      if not Terminated and Assigned(fOnComplete) then
        fOnComplete(Self);
    end;
  finally
    gLog.MultithreadLogging := False;
  end;
end;


end.
