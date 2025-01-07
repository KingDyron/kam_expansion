unit KM_Log;
{$I KaM_Remake.inc}
interface
uses
  SyncObjs, KM_CommonTypes, KM_CommonClasses
  {$IFDEF KMR_GAME} // Not needed for server and other tools
  , Generics.Collections
  {$ENDIF}
  ;


type
  // Log message type
  TKMLogMessageType = (
    lmtDefault,            // Default type
    lmtDelivery,           // Delivery messages
    lmtCommands,           // All GIC commands
    lmtRandomChecks,       // Random checks
    lmtNetConnection,      // Messages about net connection/disconnection/reconnection
    lmtNetPacketOther,     // Messages about net packets (all packets, except GIP commands/ping/fps)
    lmtNetPacketCommand,   // Messages about GIP commands net packets
    lmtNetPacketPingFps,   // Messages about ping/fps net packets
    lmtDebug               // Debug
  );

  TKMLogMessageTypeSet = set of TKMLogMessageType;

  // Logging system
  TKMLog = class
  private
    CS: TCriticalSection;
    fLogFile: TextFile;
    fLogPath: UnicodeString;
    fFirstTick: cardinal;
    fPreviousTick: cardinal;
    fPreviousDate: TDateTime;
    // Enable thread safe mode (resource protection) when logging from multiple threads
    // Numeric value is used, instead of Boolean (or better LongBool), because we can set MutithreadLoggind from different threads / tasks / methods
    // Integer is used to be able to use AtomicIncrement method, which required 32bit values
    fMultithreadLogCounter: Integer;

    {$IFDEF KMR_GAME}
    fOnLogMessageList: TList<TUnicodeStringEvent>;
    {$ENDIF}

    procedure Lock;
    procedure Unlock;

    procedure InitLog;

    procedure NotifyLogSubs(aText: UnicodeString);

    procedure AddLineTime(const aText: UnicodeString; aLogType: TKMLogMessageType; aDoCloseFile: Boolean = True); overload;
    procedure AddLineTime(const aText: UnicodeString; aFlushImmidiately: Boolean = True); overload;
    procedure AddLineNoTime(const aText: UnicodeString; aWithPrefix: Boolean = True; aDoCloseFile: Boolean = True); overload;
    procedure AddLineNoTime(const aText: UnicodeString; aLogType: TKMLogMessageType; aWithPrefix: Boolean = True; aDoCloseFile: Boolean = True); overload;

    function GetMultithreadLogging: Boolean;
    procedure SetMultithreadLogging(const aValue: Boolean);
  public

    MessageTypes: TKMLogMessageTypeSet;
    constructor Create(const aPath: UnicodeString);
    destructor Destroy; override;

    // AppendLog adds the line to Log along with time passed since previous line added
    procedure AddTime(const aText: UnicodeString); overload;
    procedure AddTimeNoFlush(const aText: UnicodeString); overload;
    procedure AddTime(const aText: UnicodeString; num: Integer); overload;
    procedure AddTime(const aText: UnicodeString; num: Single); overload;
    procedure AddTime(num: Integer; const aText: UnicodeString); overload;
    procedure AddTime(const aText: UnicodeString; Res: boolean); overload;
    procedure AddTime(a, b: integer); overload;
    function IsDegubLogEnabled: Boolean;
    procedure LogDebug(const aText: UnicodeString);
    procedure LogDelivery(const aText: UnicodeString);
    procedure LogCommands(const aText: UnicodeString);
    procedure LogRandomChecks(const aText: UnicodeString);
    procedure LogNetConnection(const aText: UnicodeString);
    procedure LogNetPacketOther(const aText: UnicodeString);
    procedure LogNetPacketCommand(const aText: UnicodeString);
    procedure LogNetPacketPingFps(const aText: UnicodeString);
    function CanLogDelivery: Boolean;
    function CanLogCommands: Boolean;
    function CanLogRandomChecks: Boolean;
    function CanLogNetConnection: Boolean;
    function CanLogNetPacketOther: Boolean;
    function CanLogNetPacketCommand: Boolean;
    function CanLogNetPacketPingFps: Boolean;

    procedure SetDefaultMessageTypes;

    property MultithreadLogging: Boolean read GetMultithreadLogging write SetMultithreadLogging;

    // Add line if TestValue=False
    procedure AddAssert(const aMessageText: UnicodeString);
    // AddToLog simply adds the text
    procedure AddNoTime(const aText: UnicodeString; aWithPrefix: Boolean = True);
    procedure AddNoTimeNoFlush(const aText: UnicodeString);
    procedure DeleteOldLogs;
    property LogPath: UnicodeString read fLogPath; //Used by dedicated server
//    property OnLogMessage: TUnicodeStringEvent read fOnLogMessage write fOnLogMessage;
    procedure AddOnLogEventSub(const aOnLogMessage: TUnicodeStringEvent);
    procedure RemoveOnLogEventSub(const aOnLogMessage: TUnicodeStringEvent);
  end;

var
  gLog: TKMLog;


implementation
uses
  Classes, SysUtils,
  KM_FileIO,
  KM_Defaults, KM_CommonUtils;

const
  DEFAULT_LOG_TYPES_TO_WRITE: TKMLogMessageTypeSet = [lmtDefault, lmtNetConnection];


type
  // New thread, in which old logs are deleted (used internally)
  TKMOldLogsDeleter = class(TThread)
  private
    fPathToLogs: UnicodeString;
  public
    constructor Create(const aPathToLogs: UnicodeString);
    procedure Execute; override;
  end;


{ TKMOldLogsDeleter }
constructor TKMOldLogsDeleter.Create(const aPathToLogs: UnicodeString);
begin
  //Thread isn't started until all constructors have run to completion
  //so Create(False) may be put in front as well
  inherited Create(False);

  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging('OldLogsDeleter', ThreadID);
  {$ENDIF}

  //Must set these values BEFORE starting the thread
  FreeOnTerminate := True; //object can be automatically removed after its termination
  fPathToLogs := aPathToLogs;
end;


procedure TKMOldLogsDeleter.Execute;
var
  SearchRec: TSearchRec;
  fileDateTime: TDateTime;
begin
  if not DirectoryExists(fPathToLogs) then Exit;
  try
    if FindFirst(fPathToLogs + 'KaM*.log', faAnyFile - faDirectory, SearchRec) = 0 then
    repeat
      Assert(FileAge(fPathToLogs + SearchRec.Name, fileDateTime), 'How is that it does not exists any more?');

      if (Abs(Now - fileDateTime) > DEL_LOGS_OLDER_THAN) then
        KMDeleteFile(fPathToLogs + SearchRec.Name);
    until (FindNext(SearchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
end;


{ TKMLog }
constructor TKMLog.Create(const aPath: UnicodeString);
begin
  inherited Create;
  fMultithreadLogCounter := 0;
  fLogPath := aPath;
  fFirstTick := TimeGet;
  fPreviousTick := TimeGet;
  SetDefaultMessageTypes;

  if DEBUG_LOGS then
    Include(MessageTypes, lmtDebug);

  CS := TCriticalSection.Create;
  {$IFDEF KMR_GAME}
  fOnLogMessageList := TList<TUnicodeStringEvent>.Create;
  {$ENDIF}

  InitLog;
end;


procedure TKMLog.SetDefaultMessageTypes;
begin
  MessageTypes := DEFAULT_LOG_TYPES_TO_WRITE;
end;


destructor TKMLog.Destroy;
begin
  CS.Free;
  {$IFDEF KMR_GAME}
  fOnLogMessageList.Free;
  {$ENDIF}

  inherited;
end;


function TKMLog.GetMultithreadLogging: Boolean;
begin
  if Self = nil then Exit(False);

  Result := fMultithreadLogCounter > 0;
end;


procedure TKMLog.SetMultithreadLogging(const aValue: Boolean);
begin
  if Self = nil then Exit;

  {$IFDEF WDC}
  // Doing it faster way in Delphi
  if aValue then
    AtomicIncrement(fMultithreadLogCounter)
  else
    AtomicDecrement(fMultithreadLogCounter);
  {$ELSE}
  Lock;
  try
    if aValue then
      Inc(fMultithreadLogCounter)
    else
      Dec(fMultithreadLogCounter);
  finally
    Unlock;
  end;
  {$ENDIF}
end;


procedure TKMLog.Lock;
begin
  CS.Enter;
end;


procedure TKMLog.Unlock;
begin
  CS.Leave;
end;


procedure TKMLog.InitLog;
begin
  if BLOCK_FILE_WRITE then Exit;

  try
    ForceDirectories(ExtractFilePath(fLogPath));

    AssignFile(fLogFile, fLogPath);
    Rewrite(fLogFile);
    //           hh:nn:ss.zzz 12345.678s 1234567ms     text-text-text
    WriteLn(fLogFile, '   Timestamp    Elapsed     Delta  Thread    Description');
    CloseFile(fLogFile);
  except
    on E: Exception do
    begin
      E.Message := E.Message + '. Tried to init Log on path ''' + fLogPath + '''';
      raise E;
    end;
  end;
  AddLineTime('Log is up and running. Game version: ' + UnicodeString(GAME_VERSION));
end;


//Run thread to delete old logs.
procedure TKMLog.DeleteOldLogs;
begin
  if Self = nil then Exit;
  if not DELETE_OLD_LOGS then Exit;
  
  //No need to remember the instance, it's set to FreeOnTerminate
  TKMOldLogsDeleter.Create(ExtractFilePath(fLogPath));
end;


procedure TKMLog.AddOnLogEventSub(const aOnLogMessage: TUnicodeStringEvent);
begin
  {$IFDEF KMR_GAME}
  fOnLogMessageList.Add(aOnLogMessage);
  {$ENDIF}
end;


procedure TKMLog.RemoveOnLogEventSub(const aOnLogMessage: TUnicodeStringEvent);
begin
  {$IFDEF KMR_GAME}
  fOnLogMessageList.Remove(aOnLogMessage);
  {$ENDIF}
end;


procedure TKMLog.NotifyLogSubs(aText: UnicodeString);
{$IFDEF KMR_GAME}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF KMR_GAME}
  for I := 0 to fOnLogMessageList.Count - 1  do
    if Assigned(fOnLogMessageList[I]) then
      fOnLogMessageList[I](aText);
  {$ENDIF}
end;


//Lines are timestamped, each line invokes file open/close for writing,
//meaning that no lines will be lost if Remake crashes
procedure TKMLog.AddLineTime(const aText: UnicodeString; aLogType: TKMLogMessageType; aDoCloseFile: Boolean = True);
var
  lockedHere: Boolean;
begin
  if Self = nil then Exit;

  if BLOCK_FILE_WRITE then Exit;
  
  if not (aLogType in MessageTypes) then // write into log only for allowed types
    Exit;

  lockedHere := False;
  // Lock/Unlock only when in multithread logging mode. Its quite rare, so we do not need all the time
  if MultithreadLogging then
  begin
    Lock;
    lockedHere := True;
  end;
  try
    if not FileExists(fLogPath) then
      InitLog;  // Recreate log file, if it was deleted

    Append(fLogFile);
    //Write a line when the day changed since last time (useful for dedicated server logs that could be over months)
    if Abs(Trunc(fPreviousDate) - Trunc(Now)) >= 1 then
    begin
      WriteLn(fLogFile, '========================');
      WriteLn(fLogFile, '    Date: ' + FormatDateTime('yyyy/mm/dd', Now));
      WriteLn(fLogFile, '========================');
    end;
    WriteLn(fLogFile, Format('%12s %9.3fs %7dms %6d    %s', [
                  FormatDateTime('hh:nn:ss.zzz', Now),
                  TimeSince(fFirstTick) / 1000,
                  TimeSince(fPreviousTick),
                  TThread.CurrentThread.ThreadID,
                  aText]));

    if aDoCloseFile then
      CloseFile(fLogFile);

    fPreviousTick := TimeGet;
    fPreviousDate := Now;
  finally
    // We could be locked by other thread, thus unlock here only if this thread made a lock
    if lockedHere and MultithreadLogging then
      UnLock;
  end;

  NotifyLogSubs(aText);
end;


//Add line with timestamp
procedure TKMLog.AddLineTime(const aText: UnicodeString; aFlushImmidiately: Boolean = True);
begin
  AddLineTime(aText, lmtDefault, aFlushImmidiately);
end;


//Add line but without timestamp
procedure TKMLog.AddLineNoTime(const aText: UnicodeString; aLogType: TKMLogMessageType; aWithPrefix: Boolean = True;
                               aDoCloseFile: Boolean = True);
var
  lockedHere: Boolean;
begin
  if Self = nil then Exit;

  if BLOCK_FILE_WRITE then Exit;

  if not (aLogType in MessageTypes) then // write into log only for allowed types
    Exit;

  lockedHere := False;
  // Lock/Unlock only when in multithread logging mode. Its quite rare, so we do not need all the time
  if MultithreadLogging then
  begin
    Lock;
    lockedHere := True;
  end;
  try
    if not FileExists(fLogPath) then
      InitLog;  // Recreate log file, if it was deleted

    Append(fLogFile);
    if aWithPrefix then
      WriteLn(fLogFile, '                                      ' + aText)
    else
      WriteLn(fLogFile, aText);

    if aDoCloseFile then
      CloseFile(fLogFile);
  finally
    // We could be locked by other thread, thus unlock here only if this thread made a lock
    if lockedHere and MultithreadLogging then
      UnLock;
  end;

  NotifyLogSubs(aText);
end;


//Add line without timestamp
procedure TKMLog.AddLineNoTime(const aText: UnicodeString; aWithPrefix: Boolean = True; aDoCloseFile: Boolean = True);
begin
  AddLineNoTime(aText, lmtDefault, aWithPrefix, aDoCloseFile);
end;


procedure TKMLog.AddTime(const aText: UnicodeString);
begin
  AddLineTime(aText);
end;


procedure TKMLog.AddTimeNoFlush(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, False);
end;


function TKMLog.IsDegubLogEnabled: Boolean;
begin
  Result := lmtDebug in MessageTypes;
end;


procedure TKMLog.LogDebug(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtDebug);
end;


procedure TKMLog.LogDelivery(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtDelivery);
end;


procedure TKMLog.LogCommands(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtCommands);
end;


procedure TKMLog.LogRandomChecks(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineNoTime(aText, lmtRandomChecks);
end;


procedure TKMLog.LogNetConnection(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtNetConnection);
end;


procedure TKMLog.LogNetPacketOther(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtNetPacketOther);
end;


procedure TKMLog.LogNetPacketCommand(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtNetPacketCommand);
end;


procedure TKMLog.LogNetPacketPingFps(const aText: UnicodeString);
begin
  if Self = nil then Exit;
  AddLineTime(aText, lmtNetPacketPingFps);
end;


function TKMLog.CanLogDelivery: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtDelivery in MessageTypes;
end;


function TKMLog.CanLogCommands: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtCommands in MessageTypes;
end;


function TKMLog.CanLogRandomChecks: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtRandomChecks in MessageTypes;
end;


function TKMLog.CanLogNetConnection: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtNetConnection in MessageTypes;
end;


function TKMLog.CanLogNetPacketOther: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtNetPacketOther in MessageTypes;
end;


function TKMLog.CanLogNetPacketCommand: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtNetPacketCommand in MessageTypes;
end;


function TKMLog.CanLogNetPacketPingFps: Boolean;
begin
  if Self = nil then Exit(False);
  Result := lmtNetPacketPingFps in MessageTypes;
end;


procedure TKMLog.AddTime(const aText: UnicodeString; num: integer);
begin
  if Self = nil then Exit;

  AddLineTime(aText + ' ' + inttostr(num));
end;


procedure TKMLog.AddTime(const aText: UnicodeString; num: single);
begin
  if Self = nil then Exit;

  AddLineTime(aText + ' ' + floattostr(num));
end;


procedure TKMLog.AddTime(num: integer; const aText: UnicodeString);
begin
  if Self = nil then Exit;

  AddLineTime(inttostr(num) + ' ' + aText);
end;


procedure TKMLog.AddTime(const aText: UnicodeString; Res: boolean);
var
  s: UnicodeString;
begin
  if Self = nil then Exit;

  if Res then
    s := 'done'
  else
    s := 'fail';
  AddLineTime(aText + ' ... ' + s);
end;


procedure TKMLog.AddTime(A, B: integer);
begin
  if Self = nil then Exit;

  AddLineTime(inttostr(A) + ' : ' + inttostr(B));
end;


procedure TKMLog.AddAssert(const aMessageText: UnicodeString);
begin
  if Self = nil then Exit;

  AddLineNoTime('ASSERTION FAILED! Msg: ' + aMessageText);
  raise Exception.Create('ASSERTION FAILED! Msg: ' + aMessageText);
end;


procedure TKMLog.AddNoTime(const aText: UnicodeString; aWithPrefix: Boolean = True);
begin
  if Self = nil then Exit;

  AddLineNoTime(aText, aWithPrefix);
end;


procedure TKMLog.AddNoTimeNoFlush(const aText: UnicodeString);
begin
  if Self = nil then Exit;

  AddLineNoTime(aText, True, False);
end;


end.
