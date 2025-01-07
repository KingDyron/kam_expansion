unit KM_ServerQuery;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_NetClient, KM_MasterServer, KM_NetUDP, KM_NetworkTypes, KM_NetworkClasses,
  KM_CommonClasses, KM_CommonTypes;


const
  MAX_QUERIES = 16; //The maximum number of individual server queries that can be happening at once
  QUERY_TIMEOUT = 5000; //The maximum amount of time for an individual query to take (will take at least 2*ping)
  MIN_UDP_SCAN_TIME = 1000; //Assume we won't get any more UDP responses after this time (remove 'Refreshing...' text if no servers found)

type
  TKMServerInfo = record
    Name: string;
    IP: string;
    Port: Word;
    ServerType: TKMServerType;
    Ping: Word;
  end;

  TKMRoomInfo = record
    ServerIndex: Integer; //Points to some TKMServerInfo in TKMServerList
    GameRevision: TKMGameRevision;
    RoomID: Integer;
    OnlyRoom: Boolean; //Is this the only room in the server?
    GameInfo: TKMPGameInfo;
  end;

  TServerDataEvent = procedure(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal) of object;

  TServerSortMethod = (
    ssmByTypeAsc, ssmByTypeDesc, //Client or dedicated
    ssmByPasswordAsc, ssmByPasswordDesc, //Passworded or not
    ssmByNameAsc, ssmByNameDesc, //Server names
    ssmByPingAsc, ssmByPingDesc, //Server pings
    ssmByStateAsc, ssmByStateDesc, //Room state
    ssmByPlayersAsc, ssmByPlayersDesc); //Player count in room

  TKMQuery = class
  private
    fNetClient: TKMNetClient;
    fQueryActive: Boolean;
    fQueryIsDone: Boolean;
    fPingStarted: Cardinal;
    fQueryStarted: Cardinal;
    fIndexOnServer: Integer;
    fServerID: Integer;
    fOnServerData: TServerDataEvent;
    fOnQueryDone: TNotifyEvent;
    procedure NetClientReceive(aNetClient: TKMNetClient; aSenderIndex: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
    procedure PacketSend(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PerformQuery(const aAddress: string; aPort: Word; aServerID: Integer);
    procedure Disconnect;
    property OnServerData: TServerDataEvent read fOnServerData write fOnServerData;
    property OnQueryDone: TNotifyEvent read fOnQueryDone write fOnQueryDone;
    procedure UpdateStateIdle;
  end;

  TKMRoomList = class
  private
    fCount: Integer;
    fRooms: array of TKMRoomInfo;
    procedure AddRoom(aServerIndex, aRoomID: Integer; aOnlyRoom: Boolean; aGameRev: TKMGameRevision; aGameInfoStream: TKMemoryStream);
    function GetRoom(aIndex: Integer): TKMRoomInfo;
    procedure SetRoom(aIndex: Integer; aValue: TKMRoomInfo);
    procedure Clear;
  public
    destructor Destroy; override;
    property Rooms[aIndex: Integer]: TKMRoomInfo read GetRoom write SetRoom; default;
    property Count: Integer read fCount;
    procedure LoadData(aServerID: Integer; aStream: TKMemoryStream);
    procedure SwapRooms(A,B: Integer);
  end;

  TKMServerList = class
  private
    fCount: Integer;
    fLastQueried: Integer;
    fServers: array of TKMServerInfo;
    procedure AddServer(const aIP, aName: String; aPort: Word; aType: TKMServerType; aPing: Word);
    function GetServer(aIndex: Integer): TKMServerInfo;
    procedure Clear;
    procedure AddFromText(const aText: UnicodeString);
  public
    property Servers[aIndex: Integer]: TKMServerInfo read GetServer; default;
    property Count: Integer read fCount;
    procedure TakeNewQuery(aQuery: TKMQuery);
    procedure SetPing(aServerID: Integer; aPing: Cardinal);
  end;

  //Handles the master-server querrying and carries ServerList
  TKMServerQuery = class
  private
    fMasterServer: TKMMasterServer;
    fServerList: TKMServerList; //List of servers fetch from master
    fRoomList: TKMRoomList; //Info about each room populated after query completed
    fQuery: array[0..MAX_QUERIES-1] of TKMQuery;
    fSortMethod: TServerSortMethod;
    fRefreshStartedTime: Cardinal;
    fReceivedMasterServerList: Boolean;

    fUDPScanner: TKMNetUDPScan;

    fOnListUpdated: TNotifyEvent;
    fOnAnnouncements: TUnicodeStringEvent;

    procedure DetectUDPServer(const aAddress: String; const aPort: Word; const aName: string);
    procedure ReceiveServerList(const S: String);
    procedure ReceiveAnnouncements(const S: String);

    procedure ServerDataReceive(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal);
    procedure QueryDone(Sender: TObject);

    procedure Sort;
    procedure SetSortMethod(aMethod: TServerSortMethod);

    function ActiveQueryCount: Integer;
  public
    constructor Create(const aMasterServerAddress: String; aServerUDPScanPort: Word);
    destructor Destroy; override;

    property OnListUpdated: TNotifyEvent read fOnListUpdated write fOnListUpdated;
    property OnAnnouncements: TUnicodeStringEvent read fOnAnnouncements write fOnAnnouncements;
    property Servers: TKMServerList read fServerList;
    property Rooms: TKMRoomList read fRoomList;

    property SortMethod: TServerSortMethod read fSortMethod write SetSortMethod;

    procedure RefreshList;
    procedure FetchAnnouncements(const aLocale: AnsiString);
    procedure SendMapInfo(const aMapName: UnicodeString; aCRC: Cardinal; aPlayerCount: Integer);
    procedure UpdateStateIdle;
  end;


implementation
uses
  SysUtils,
  KM_Defaults, KM_CommonUtils;


{ TKMRoomList }
destructor TKMRoomList.Destroy;
begin
  // Free GameInfo
  Clear; 
  inherited;
end;


procedure TKMRoomList.AddRoom(aServerIndex, aRoomID: Integer; aOnlyRoom: Boolean; aGameRev: TKMGameRevision;
                              aGameInfoStream: TKMemoryStream);
begin
  if Length(fRooms) <= fCount then SetLength(fRooms, fCount+16);
  fRooms[fCount].ServerIndex := aServerIndex;
  fRooms[fCount].GameRevision := aGameRev;
  fRooms[fCount].RoomID := aRoomID;
  fRooms[fCount].OnlyRoom := aOnlyRoom;
  fRooms[fCount].GameInfo := TKMPGameInfo.Create;
  fRooms[fCount].GameInfo.LoadFromStream(aGameInfoStream);
  Inc(fCount);
end;


function TKMRoomList.GetRoom(aIndex:integer): TKMRoomInfo;
begin
  Result := fRooms[aIndex];
end;


procedure TKMRoomList.SetRoom(aIndex: Integer; aValue: TKMRoomInfo);
begin
  fRooms[aIndex] := aValue;
end;


procedure TKMRoomList.Clear;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fRooms[I].GameInfo.Free;
  fCount := 0;
  SetLength(fRooms, 0);
end;


procedure TKMRoomList.LoadData(aServerID: Integer; aStream: TKMemoryStream);
var
  I, roomCount, roomID: Integer;
  gameRev: TKMGameRevision;
begin
  aStream.Read(roomCount);
  for I := 0 to roomCount - 1 do //We don't actually use i as the server sends us RoomID (rooms might not be in order)
  begin
    aStream.Read(roomID);
    aStream.Read(gameRev);
    AddRoom(aServerID, roomID, (roomCount = 1), gameRev, aStream);
  end;
end;


procedure TKMRoomList.SwapRooms(A,B: Integer);
var
  temp: TKMRoomInfo;
begin
  temp := fRooms[A];
  fRooms[A] := fRooms[B];
  fRooms[B] := temp;
end;


{ TKMServerList }
procedure TKMServerList.AddServer(const aIP, aName: string; aPort: Word; aType: TKMServerType; aPing: Word);
begin
  if Length(fServers) <= fCount then SetLength(fServers, fCount+16);
  fServers[fCount].Name := aName;
  fServers[fCount].IP := aIP;
  fServers[fCount].Port := aPort;
  fServers[fCount].ServerType := aType;
  fServers[fCount].Ping := aPing;
  Inc(fCount);
end;


function TKMServerList.GetServer(aIndex:integer):TKMServerInfo;
begin
  Result := fServers[aIndex];
end;


procedure TKMServerList.Clear;
begin
  fCount := 0;
  SetLength(fServers, 0);
  fLastQueried := -1; //First is 0
end;


procedure TKMServerList.AddFromText(const aText: UnicodeString);

  function GetServerType(const aDedicated, aOS: UnicodeString): TKMServerType;
  begin
    if aDedicated = '0' then
      Result := mstClient
    else
      Result := mstDedicated;
  end;

var
  I: Integer;
  srvList, srvInfo: TStringList;
begin
  srvList := TStringList.Create;
  srvInfo := TStringList.Create;
  srvList.Text := aText; //Parse according to EOLs
  for I := 0 to srvList.Count - 1 do
  begin
    ParseDelimited(srvList[I], ',', srvInfo); //Automatically clears srvInfo and loads each value
    if srvInfo.Count = 5 then //Must have 5 parameters
      AddServer(srvInfo[1], srvInfo[0], StrToInt(srvInfo[2]), GetServerType(srvInfo[3], srvInfo[4]), 0);
  end;

  srvInfo.Free;
  srvList.Free;
end;


procedure TKMServerList.TakeNewQuery(aQuery: TKMQuery);
begin
  if fLastQueried < fCount - 1 then
  begin
    Inc(fLastQueried);
    aQuery.PerformQuery(Servers[fLastQueried].IP, Servers[fLastQueried].Port, fLastQueried);
  end
  else
    aQuery.Disconnect;
end;


procedure TKMServerList.SetPing(aServerID:Integer; aPing: Cardinal);
begin
  fServers[aServerID].Ping := aPing;
end;


{ TKMQuery }
constructor TKMQuery.Create;
begin
  inherited;

  fNetClient := TKMNetClient.Create;
  fQueryIsDone := False;
  fQueryActive := False;
end;


destructor TKMQuery.Destroy;
begin
  fNetClient.Free;

  inherited;
end;


procedure TKMQuery.PerformQuery(const aAddress: string; aPort: Word; aServerID: Integer);
begin
  fQueryActive := True;
  fServerID := aServerID;
  fQueryStarted := TimeGet;
  fNetClient.Disconnect;
  fNetClient.OnRecieveData := NetClientReceive;
  fNetClient.ConnectTo(aAddress, aPort);
end;


procedure TKMQuery.Disconnect;
begin
  fNetClient.Disconnect;
  fNetClient.OnRecieveData := nil;
end;


procedure TKMQuery.UpdateStateIdle;
begin
  if not fQueryActive then Exit;

  fNetClient.UpdateStateIdle;
  if TimeSince(fQueryStarted) > QUERY_TIMEOUT then
    fQueryIsDone := True; //Give up
  if fQueryIsDone then
  begin
    fQueryIsDone := False;
    fQueryActive := False;
    fOnQueryDone(Self);
  end;
end;


procedure TKMQuery.NetClientReceive(aNetClient: TKMNetClient; aSenderIndex: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
var
  M: TKMemoryStream;
  kind: TKMessageKind;
//  tmpInteger: Integer;
  tmpHandleIndex: TKMNetHandleIndex;
  tmpString: AnsiString;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStreamBinary.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(kind, SizeOf(TKMessageKind)); //Depending on kind message contains either Text or a Number

  case kind of
    mkGameVersion:
      begin
        M.ReadA(tmpString);
        if tmpString <> NET_PROTOCOL_REVISON then
          fQueryIsDone := True;
      end;

    mkIndexOnServer:
      begin
        M.Read(tmpHandleIndex);
        fIndexOnServer := tmpHandleIndex;
        fPingStarted := TimeGet;
        PacketSend(NET_ADDRESS_SERVER, mkGetServerInfo);
      end;

    mkServerInfo:
      begin
        fOnServerData(fServerID, M, fPingStarted);
        fQueryIsDone := True; //We cannot call fOnQueryDone now because that would disconnect the socket halfway through the receive procedure (crashes)
      end;
  end;

  M.Free;
end;


procedure TKMQuery.PacketSend(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfNoData);

  M := TKMemoryStreamBinary.Create;
  M.Write(aKind, SizeOf(TKMessageKind));

  fNetClient.SendData(fIndexOnServer, aRecipient, M.Memory, M.Size);
  M.Free;
end;


{ TKMServerQuery }
constructor TKMServerQuery.Create(const aMasterServerAddress: String; aServerUDPScanPort: Word);
var
  I: Integer;
begin
  inherited Create;
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress, False);
  fMasterServer.OnServerList := ReceiveServerList;
  fMasterServer.OnAnnouncements := ReceiveAnnouncements;

  fUDPScanner := TKMNetUDPScan.Create(aServerUDPScanPort);
  fUDPScanner.OnServerDetected := DetectUDPServer;

  fSortMethod := ssmByStateAsc; //Default sorting is by state

  fServerList := TKMServerList.Create;
  fRoomList := TKMRoomList.Create;
  for I := 0 to MAX_QUERIES - 1 do
  begin
    fQuery[I] := TKMQuery.Create;
    fQuery[I].OnServerData := ServerDataReceive;
    fQuery[I].OnQueryDone := QueryDone;
  end;
end;


destructor TKMServerQuery.Destroy;
var
  I: Integer;
begin
  fMasterServer.Free;
  fUDPScanner.Free;
  fServerList.Free;
  fRoomList.Free;
  for I := 0 to MAX_QUERIES - 1 do
    fQuery[I].Free;

  inherited;
end;


procedure TKMServerQuery.RefreshList;
var
  I: Integer;
begin
  //Clean up first
  for I := 0 to MAX_QUERIES - 1 do
    fQuery[I].Disconnect;
  fRoomList.Clear;
  fServerList.Clear;
  fReceivedMasterServerList := False;
  fRefreshStartedTime := TimeGet;

  fUDPScanner.ScanForServers;
  if LOCAL_SERVER_LIST then
    ReceiveServerList('Localhost,127.0.0.1,56789,0,Windows') //For debugging
    //+#13+'Localhost,127.0.0.1,56788,1,Windows'+#13+'Localhost,127.0.0.1,56787,1,Unix'
  else
    fMasterServer.QueryServerList; //Start the query
end;


procedure TKMServerQuery.DetectUDPServer(const aAddress: string; const aPort: Word; const aName: string);
var
  I: Integer;
begin
  //Make sure this isn't a duplicate (UDP is connectionless so we could get a response from an old query)
  for I := 0 to fServerList.Count - 1 do
    if (fServerList[I].IP = aAddress) and (fServerList[I].Port = aPort) then
      Exit;

  fServerList.AddServer(aAddress, aName, aPort, mstLocal, 0);
  for I := 0 to MAX_QUERIES - 1 do
    if not fQuery[I].fQueryActive then
      fServerList.TakeNewQuery(fQuery[I]);
end;


procedure TKMServerQuery.ReceiveServerList(const S: string);
var
  I: Integer;
begin
  fReceivedMasterServerList := True;
  fServerList.AddFromText(S);
  for I := 0 to MAX_QUERIES - 1 do
    if not fQuery[I].fQueryActive then
      fServerList.TakeNewQuery(fQuery[I]);
end;


procedure TKMServerQuery.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then
    fOnAnnouncements(S);
end;


procedure TKMServerQuery.ServerDataReceive(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal);
begin
  fRoomList.LoadData(aServerID, aStream); //Tell RoomsList to load data about rooms
  fServerList.SetPing(aServerID, TimeSince(aPingStarted)); //Tell ServersList ping

  Sort; //Apply sorting
  if Assigned(fOnListUpdated) then
    fOnListUpdated(Self);
end;


procedure TKMServerQuery.QueryDone(Sender: TObject);
begin
  fServerList.TakeNewQuery(TKMQuery(Sender));
end;


//Get the server announcements in specified language
procedure TKMServerQuery.FetchAnnouncements(const aLocale: AnsiString);
begin
  fMasterServer.FetchAnnouncements(aLocale);
end;


procedure TKMServerQuery.SendMapInfo(const aMapName: UnicodeString; aCRC: Cardinal; aPlayerCount: Integer);
begin
  fMasterServer.SendMapInfo(aMapName, aCRC, aPlayerCount);
end;


procedure TKMServerQuery.UpdateStateIdle;
var
  I: Integer;
begin
  fMasterServer.UpdateStateIdle;
  fUDPScanner.UpdateStateIdle;
  for I := 0 to MAX_QUERIES - 1 do
    fQuery[I].UpdateStateIdle;

  //Once we've received the master list, finished all queries and waited long enough for UDP scan
  //we should update the list (this removes 'Refreshing...' when no servers respond)
  if fReceivedMasterServerList and (ActiveQueryCount = 0) and (TimeSince(fRefreshStartedTime) > MIN_UDP_SCAN_TIME) then
  begin
    fReceivedMasterServerList := False; //Don't do this again until the next refresh
    fUDPScanner.TerminateScan; //Closes UDP listen
    if Assigned(fOnListUpdated) then
      fOnListUpdated(Self);
  end;
end;


procedure TKMServerQuery.Sort;
var tempRooms: array of TKMRoomInfo;

  function Compare(const A, B: TKMRoomInfo): Boolean;
  const
    STATE_SORT_ORDER: array[TMPGameState] of Byte = (4,1,2,3);
  var
    aServerInfo, bServerInfo: TKMServerInfo;
  begin
    Result := False; //By default everything remains in place
    aServerInfo := Servers[A.ServerIndex];
    bServerInfo := Servers[B.ServerIndex];
    case fSortMethod of
      ssmByPasswordAsc: Result := A.GameInfo.PasswordLocked and not B.GameInfo.PasswordLocked;
      ssmByPasswordDesc:Result := not A.GameInfo.PasswordLocked and B.GameInfo.PasswordLocked;
      ssmByTypeAsc:     Result := aServerInfo.ServerType > bServerInfo.ServerType;
      ssmByTypeDesc:    Result := aServerInfo.ServerType < bServerInfo.ServerType;
      ssmByNameAsc:     Result := CompareText(aServerInfo.Name, bServerInfo.Name) > 0;
      ssmByNameDesc:    Result := CompareText(aServerInfo.Name, bServerInfo.Name) < 0;
      ssmByPingAsc:     Result := aServerInfo.Ping > bServerInfo.Ping;
      ssmByPingDesc:    Result := aServerInfo.Ping < bServerInfo.Ping;
      ssmByStateAsc:    Result := STATE_SORT_ORDER[A.GameInfo.GameState] > STATE_SORT_ORDER[B.GameInfo.GameState];
      ssmByStateDesc:   Result := STATE_SORT_ORDER[A.GameInfo.GameState] < STATE_SORT_ORDER[B.GameInfo.GameState];
      ssmByPlayersAsc:  Result := A.GameInfo.ConnectedPlayerCount > B.GameInfo.ConnectedPlayerCount;
      ssmByPlayersDesc: Result := A.GameInfo.ConnectedPlayerCount < B.GameInfo.ConnectedPlayerCount;
    end;
    //Always put local servers at the top
    if (aServerInfo.ServerType = mstLocal) and (bServerInfo.ServerType <> mstLocal) then
      Result := False
    else
      if (aServerInfo.ServerType <> mstLocal) and (bServerInfo.ServerType = mstLocal) then
        Result := True;
  end;

  procedure MergeSort(left, right: integer);
  var
    I, J, middle, ind1, ind2: integer;
  begin
    if right <= left then
      Exit;

    middle := (left+right) div 2;
    MergeSort(left, middle);
    Inc(middle);
    MergeSort(middle, right);
    ind1 := left;
    ind2 := middle;
    for I := left to right do
    begin
      if (ind1 < middle) and ((ind2 > right) or not Compare(fRoomList[ind1], fRoomList[ind2])) then
      begin
        tempRooms[I] := fRoomList[ind1];
        Inc(ind1);
      end
      else
      begin
        tempRooms[I] := fRoomList[ind2];
        Inc(ind2);
      end;
    end;
    for J := left to right do
      fRoomList[J] := tempRooms[J];
  end;

begin
  SetLength(tempRooms, fRoomList.Count);
  MergeSort(0, fRoomList.Count - 1);
end;


procedure TKMServerQuery.SetSortMethod(aMethod: TServerSortMethod);
begin
  fSortMethod := aMethod;
  Sort; //New sorting method has been set, we need to apply it
end;


function TKMServerQuery.ActiveQueryCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_QUERIES-1 do
    if fQuery[I].fQueryActive then
      Inc(Result);
end;


end.

