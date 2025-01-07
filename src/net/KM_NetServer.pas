unit KM_NetServer;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
   {$IFDEF WDC}KM_NetServerOverbyte, {$ENDIF}
   {$IFDEF FPC}KM_NetServerLNet, {$ENDIF}
  Classes, SysUtils, Math, VerySimpleXML,
  KM_CommonClasses, KM_NetworkClasses, KM_NetworkTypes,
  KM_Defaults, KM_CommonUtils, KM_CommonTypes,
  {$IFDEF WDC}
    {$IFDEF CONSOLE}
      KM_ConsoleTimer
    {$ELSE}
      ExtCtrls
    {$ENDIF}
  {$ELSE}
    FPTimer
    {$IFDEF UNIX}
      , cthreads
    {$ENDIF}
  {$ENDIF};


{ Contains basic items we need for smooth Net experience:

    - start the server
    - stop the server

    - optionaly report non-important status messages

    - generate replies/messages:
      1. player# has disconnected
      2. player# binding (ID)
      3. players ping
      4. players IPs
      5. ...

    - handle orders from Host
      0. declaration of host (associate Hoster rights with this player)
      1. kick player#
      2. request for players ping
      3. request for players IPs
      4. ...
}

type
  TKMServerClient = class
  private
    fHandle: TKMNetHandleIndex;
    fRoom: Integer;
    fPingStarted: Cardinal;
    fPing: Word;
    fFPS: Word;
    //Each client must have their own receive buffer, so partial messages don't get mixed
    fBufferSize: Cardinal;
    fBuffer: array of Byte;
    //DoSendData(aRecipient: Integer; aData: Pointer; aLength: Cardinal);
    fScheduledPacketsCnt: Byte;
    fScheduledPacketsSize: Cardinal;
    fScheduledPackets: array of Byte;
  public
    constructor Create(aHandle: TKMNetHandleIndex; aRoom: Integer);
    procedure AddScheduledPacket(aData: Pointer; aLength: Cardinal);
    procedure ClearScheduledPackets;
    property Handle: TKMNetHandleIndex read fHandle; //ReadOnly
    property Room: Integer read fRoom write fRoom;
    property Ping: Word read fPing write fPing;
    property FPS: Word read fFPS write fFPS;
  end;


  TKMClientsList = class
  private
    fCount: Integer;
    fItems: array of TKMServerClient;
    function GetItem(Index: TKMNetHandleIndex):TKMServerClient;
  public
    destructor Destroy; override;
    property Count: Integer read fCount;
    procedure AddPlayer(aHandle: TKMNetHandleIndex; aRoom: Integer);
    procedure RemPlayer(aHandle: TKMNetHandleIndex);
    procedure Clear;
    property Item[Index: TKMNetHandleIndex]: TKMServerClient read GetItem; default;
    function GetByHandle(aHandle: TKMNetHandleIndex): TKMServerClient;
  end;


  TKMNetServer = class
  private
    {$IFDEF WDC} fServer:TKMNetServerOverbyte; {$ENDIF}
    {$IFDEF FPC} fServer:TKMNetServerLNet;     {$ENDIF}

    {$IFDEF WDC}
      {$IFDEF CONSOLE}
      fTimer: TKMConsoleTimer; //Use our custom TKMConsoleTimer instead of ExtCtrls.TTimer, to be able to use it in console application (DedicatedServer)
      {$ELSE}
      fTimer: TTimer;
      {$ENDIF}
    {$ELSE}
      fTimer: TFPTimer;
    {$ENDIF}

    fClientList: TKMClientsList;
    fListening: Boolean;
    BytesTX: Int64; //May exceed 4GB allowed by cardinal
    BytesRX: Int64;

    fPacketsAccumulatingDelay: Integer;
    fMaxRooms: Word;
    fHTMLStatusFile: String;
    fWelcomeMessage: UnicodeString;
    fServerName: AnsiString;
    fKickTimeout: Word;
    fRoomCount: Integer;
    fEmptyGameInfo: TKMPGameInfo;
    fGameFilter: TKMPGameFilter;
    fRoomInfo: array of record
                         HostHandle: TKMNetHandleIndex;
                         GameRevision: TKMGameRevision;
                         Password: AnsiString;
                         BannedIPs: array of String;
                         GameInfo: TKMPGameInfo;
                       end;

    fOnStatusMessage: TGetStrProc;
    procedure Error(const S: string);
    procedure Status(const S: string);
    procedure ClientConnect(aHandle: TKMNetHandleIndex);
    procedure ClientDisconnect(aHandle: TKMNetHandleIndex);
    procedure SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind); overload;
    procedure SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aParam: Integer; aImmidiate: Boolean = False); overload;
    procedure SendMessageInd(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aIndexOnServer: TKMNetHandleIndex; aImmidiate: Boolean = False);
    procedure SendMessageA(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; const aText: AnsiString);
    procedure SendMessageW(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; const aText: UnicodeString);
    procedure SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aStream: TKMemoryStream); overload;
    procedure SendMessageToRoom(aKind: TKMessageKind; aRoom: Integer; aStream: TKMemoryStream); overload;
    procedure SendMessageAct(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aStream: TKMemoryStream; aImmidiate: Boolean = False);
    procedure ScheduleSendData(aRecipient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal; aFlushQueue: Boolean = False);
    procedure SendScheduledData(aServerClient: TKMServerClient);
    procedure DoSendData(aRecipient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
    procedure RecieveMessage(aSenderHandle: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
    procedure DataAvailable(aHandle: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
    procedure SaveToStream(aStream: TKMemoryStream);
    function IsValidHandle(aHandle: TKMNetHandleIndex): Boolean;
    function AddNewRoom: Boolean;
    function GetFirstAvailableRoom: Integer;
    function GetRoomClientsCount(aRoom: Integer): Integer;
    function GetFirstRoomClient(aRoom: Integer): Integer;
    procedure AddClientToRoom(aHandle: TKMNetHandleIndex; aRoom: Integer; aGameRevision: TKMGameRevision);
    procedure BanPlayerFromRoom(aHandle: TKMNetHandleIndex; aRoom: Integer);
    procedure SaveHTMLStatus;
    procedure SetPacketsAccumulatingDelay(aValue: Integer);
    procedure SetGameFilter(aGameFilter: TKMPGameFilter);
  public
    constructor Create(aMaxRooms, aKickTimeout: Word; const aHTMLStatusFile, aWelcomeMessage: UnicodeString;
                       aPacketsAccDelay: Integer);
    destructor Destroy; override;
    procedure StartListening(aPort: Word; const aServerName: AnsiString);
    procedure StopListening;
    procedure ClearClients;
    procedure MeasurePings;
    procedure UpdateStateIdle;
    procedure UpdateState(Sender: TObject);
    property OnStatusMessage: TGetStrProc write fOnStatusMessage;
    property Listening: boolean read fListening;
    function GetPlayerCount:integer;
    procedure UpdateSettings(aKickTimeout: Word; const aHTMLStatusFile: UnicodeString; const aWelcomeMessage: UnicodeString; const aServerName: AnsiString; const aPacketsAccDelay: Integer);
    procedure GetServerInfo(aList: TList);
    property PacketsAccumulatingDelay: Integer read fPacketsAccumulatingDelay write SetPacketsAccumulatingDelay;
    property GameFilter: TKMPGameFilter read fGameFilter write SetGameFilter;
  end;


implementation
//uses
  //TypInfo, KM_Log;

const
  //Server needs to use some text constants locally but can't know about gResTexts
  {$I KM_TextIDs.inc}
  PACKET_ACC_DELAY_MIN = 5;
  PACKET_ACC_DELAY_MAX = 200;


{ TKMServerClient }
constructor TKMServerClient.Create(aHandle: TKMNetHandleIndex; aRoom: Integer);
begin
  inherited Create;

  fHandle := aHandle;
  fRoom := aRoom;
  SetLength(fBuffer, 0);
  SetLength(fScheduledPackets, 0);
  fBufferSize := 0;
end;


procedure TKMServerClient.ClearScheduledPackets;
begin
  fScheduledPacketsCnt := 0;
  fScheduledPacketsSize := 0;
  SetLength(fScheduledPackets, 0);
end;


procedure TKMServerClient.AddScheduledPacket(aData: Pointer; aLength: Cardinal);
begin
  Inc(fScheduledPacketsCnt);
  SetLength(fScheduledPackets, fScheduledPacketsSize + aLength);

  //Append data packet to the end of cumulative packet
  Move(aData^, fScheduledPackets[fScheduledPacketsSize], aLength);
  Inc(fScheduledPacketsSize, aLength);
  //gLog.AddTime(Format('*** add scheduled packet: length = %d Cnt = %d totalSize = %d', [aLength, fScheduledPacketsCnt, fScheduledPacketsSize]));
end;


{ TKMClientsList }
destructor TKMClientsList.Destroy;
begin
  Clear; //Free all clients

  inherited;
end;


function TKMClientsList.GetItem(Index: TKMNetHandleIndex): TKMServerClient;
begin
  Assert(InRange(Index, 0, fCount - 1),'Tried to access invalid client index');
  Result := fItems[Index];
end;


procedure TKMClientsList.AddPlayer(aHandle: TKMNetHandleIndex; aRoom: Integer);
begin
  Inc(fCount);
  SetLength(fItems, fCount);
  fItems[fCount - 1] := TKMServerClient.Create(aHandle, aRoom);
end;


procedure TKMClientsList.RemPlayer(aHandle: TKMNetHandleIndex);
var
  I, ID: Integer;
begin
  ID := -1; //Convert Handle to Index
  for I := 0 to fCount - 1 do
    if fItems[I].Handle = aHandle then
      ID := I;

  Assert(ID <> -1, 'TKMClientsList. Can not remove player');

  fItems[ID].Free;
  for I := ID to fCount - 2 do
    fItems[I] := fItems[I+1]; //Shift only pointers

  dec(fCount);
  SetLength(fItems, fCount);
end;


procedure TKMClientsList.Clear;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    FreeAndNil(fItems[I]);
  fCount := 0;
end;


function TKMClientsList.GetByHandle(aHandle: TKMNetHandleIndex): TKMServerClient;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to fCount-1 do
    if fItems[I].Handle = aHandle then
    begin
      Result := fItems[I];
      Exit;
    end;
end;


{ TKMNetServer }
constructor TKMNetServer.Create(aMaxRooms, aKickTimeout: Word; const aHTMLStatusFile, aWelcomeMessage: UnicodeString;
                                aPacketsAccDelay: Integer);
begin
  inherited Create;

  fEmptyGameInfo := TKMPGameInfo.Create;
  fEmptyGameInfo.GameTime := -1;

  fGameFilter := TKMPGameFilter.Create;

  fMaxRooms := aMaxRooms;

  if aPacketsAccDelay = -1 then
    fPacketsAccumulatingDelay := DEFAULT_PACKET_ACC_DELAY
  else
    fPacketsAccumulatingDelay := aPacketsAccDelay;

  fKickTimeout := aKickTimeout;
  fHTMLStatusFile := aHTMLStatusFile;
  fWelcomeMessage := aWelcomeMessage;
  fClientList := TKMClientsList.Create;
  {$IFDEF WDC} fServer := TKMNetServerOverbyte.Create; {$ENDIF}
  {$IFDEF FPC} fServer := TKMNetServerLNet.Create;     {$ENDIF}
  fListening := False;
  fRoomCount := 0;

  {$IFDEF WDC}
    {$IFDEF CONSOLE}
      fTimer := TKMConsoleTimer.Create;
      fTimer.OnTimerEvent := UpdateState;
    {$ELSE}
      fTimer := TTimer.Create(nil);
      fTimer.OnTimer := UpdateState;
    {$ENDIF}
    fTimer.Interval := fPacketsAccumulatingDelay;
    fTimer.Enabled  := True;
  {$ELSE}
    fTimer := TFPTimer.Create(nil);
    fTimer.OnTimer  := UpdateState;
    fTimer.Interval := fPacketsAccumulatingDelay;
    fTimer.Enabled  := True;
    fTimer.StartTimer;
  {$ENDIF}
end;


destructor TKMNetServer.Destroy;
begin
  StopListening; //Frees room info
  fServer.Free;
  fClientList.Free;
  fEmptyGameInfo.Free;
  FreeAndNil(fTimer);

  if fGameFilter <> nil then
    FreeAndNil(fGameFilter);

  inherited;
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Error(const S: string);
begin
  Status(S);
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMNetServer.Status(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: ' + S);
end;


procedure TKMNetServer.StartListening(aPort: Word; const aServerName: AnsiString);
begin
  fRoomCount := 0;
  Assert(AddNewRoom); //Must succeed

  fServerName := aServerName;
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  Status('Listening on port ' + IntToStr(aPort));
  fListening := True;
  SaveHTMLStatus;
end;


procedure TKMNetServer.StopListening;
var
  I: Integer;
begin
  fOnStatusMessage := nil;
  fServer.StopListening;
  fListening := False;
  for I := 0 to fRoomCount - 1 do
  begin
    FreeAndNil(fRoomInfo[I].GameInfo);
    SetLength(fRoomInfo[I].BannedIPs, 0);
  end;
  SetLength(fRoomInfo,0);
  fRoomCount := 0;
end;


procedure TKMNetServer.ClearClients;
begin
  fClientList.Clear;
end;


procedure TKMNetServer.MeasurePings;
var
  I: Integer;
  M: TKMemoryStream;
  tickCount: DWord;
begin
  tickCount := TimeGet;
  //Sends current ping info to everyone
  M := TKMemoryStreamBinary.Create;
  M.Write(fClientList.Count);
  for I := 0 to fClientList.Count - 1 do
  begin
    M.Write(fClientList[I].Handle);
    M.Write(fClientList[I].Ping);
    M.Write(fClientList[I].FPS);
    //gLog.AddTime(Format('Client %d measured ping = %d FPS = %d', [fClientList[I].Handle, fClientList[I].Ping, fClientList[I].FPS]));
  end;
  SendMessage(NET_ADDRESS_ALL, mkPingInfo, M);
  M.Free;

  //Measure pings. Iterate backwards so the indexes are maintained after kicking clients
  for I:=fClientList.Count-1 downto 0 do
    if fClientList[I].fPingStarted = 0 then //We have recieved mkPong for our previous measurement, so start a new one
    begin
      fClientList[I].fPingStarted := tickCount;
      SendMessage(fClientList[I].fHandle, mkPing);
    end
    else
      //If they don't respond within a reasonable time, kick them
      if TimeSince(fClientList[I].fPingStarted) > fKickTimeout*1000 then
      begin
        Status('Client timed out ' + inttostr(fClientList[I].fHandle));
        SendMessage(fClientList[I].fHandle, mkKicked, TX_NET_KICK_TIMEOUT, True);
        fServer.Kick(fClientList[I].fHandle);
      end;

end;


procedure TKMNetServer.UpdateStateIdle;
begin
  {$IFDEF FPC} fServer.UpdateStateIdle; {$ENDIF}
end;


function TKMNetServer.GetPlayerCount:integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to fClientList.fCount - 1 do
    if fClientList.Item[I].fRoom <> -1 then
      inc(Result);
end;


procedure TKMNetServer.UpdateSettings(aKickTimeout: Word; const aHTMLStatusFile: UnicodeString; const aWelcomeMessage: UnicodeString;
                                      const aServerName: AnsiString; const aPacketsAccDelay: Integer);
begin
  fKickTimeout := aKickTimeout;
  fHTMLStatusFile := aHTMLStatusFile;
  fWelcomeMessage := aWelcomeMessage;
  if aPacketsAccDelay = -1 then
    PacketsAccumulatingDelay := DEFAULT_PACKET_ACC_DELAY
  else
    PacketsAccumulatingDelay := aPacketsAccDelay;
  if fServerName <> aServerName then
    SendMessageA(NET_ADDRESS_ALL, mkServerName, aServerName);
  fServerName := aServerName;
end;


procedure TKMNetServer.GetServerInfo(aList: TList);
var
  I: Integer;
begin
  Assert(aList <> nil);
  for I := 0 to fRoomCount - 1 do
    if GetRoomClientsCount(I) > 0 then
      aList.Add(fRoomInfo[I].GameInfo);
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMNetServer.ClientConnect(aHandle: TKMNetHandleIndex);
begin
  fClientList.AddPlayer(aHandle, -1); //Clients are not initially put into a room, they choose a room later
  SendMessageA(aHandle, mkGameVersion, NET_PROTOCOL_REVISON); //First make sure they are using the right version
  if fWelcomeMessage <> '' then SendMessageW(aHandle, mkWelcomeMessage, fWelcomeMessage); //Welcome them to the server
  SendMessageA(aHandle, mkServerName, fServerName);
  SendMessageInd(aHandle, mkIndexOnServer, aHandle); //This is the signal that the client may now start sending
end;


procedure TKMNetServer.AddClientToRoom(aHandle: TKMNetHandleIndex; aRoom: Integer; aGameRevision: TKMGameRevision);
var
  I: Integer;
  M: TKMemoryStream;
begin
  if fClientList.GetByHandle(aHandle).Room <> -1 then exit; //Changing rooms is not allowed yet

  if aRoom = fRoomCount then
  begin
    if not AddNewRoom then //Create a new room for this client
    begin
      SendMessage(aHandle, mkRefuseToJoin, TX_NET_INVALID_ROOM, True);
      fServer.Kick(aHandle);
      Exit;
    end;
  end
  else
    if aRoom = -1 then
    begin
      aRoom := GetFirstAvailableRoom; //Take the first one which has a space (or create a new one if none have spaces)
      if aRoom = -1 then //No rooms available
      begin
        SendMessage(aHandle, mkRefuseToJoin, TX_NET_INVALID_ROOM, True);
        fServer.Kick(aHandle);
        Exit;
      end;
    end
    else
      //If the room is outside the valid range
      if not InRange(aRoom, 0, fRoomCount - 1) then
      begin
        SendMessage(aHandle, mkRefuseToJoin, TX_NET_INVALID_ROOM, True);
        fServer.Kick(aHandle);
        Exit;
      end;

  //Make sure the client is not banned by host from this room
  for I := 0 to Length(fRoomInfo[aRoom].BannedIPs) - 1 do
    if fRoomInfo[aRoom].BannedIPs[I] = fServer.GetIP(aHandle) then
    begin
      SendMessage(aHandle, mkRefuseToJoin, TX_NET_BANNED_BY_HOST, True);
      fServer.Kick(aHandle);
      Exit;
    end;

  //Let the first client be a Host
  if fRoomInfo[aRoom].HostHandle = NET_ADDRESS_EMPTY then
  begin
    fRoomInfo[aRoom].HostHandle := aHandle;
    //Setup revision for room on first host connection
    //other players should not be able to override it due to exe-CRC check
    fRoomInfo[aRoom].GameRevision := aGameRevision;
    Status('Host rights assigned to ' + IntToStr(fRoomInfo[aRoom].HostHandle));
  end
  else
  if fRoomInfo[aRoom].GameRevision <> aGameRevision then //Usually should never happen
  begin
    SendMessage(aHandle, mkRefuseToJoin, TX_NET_HOST_GAME_VERSION_DONT_MATCH, True);
    fServer.Kick(aHandle);
    Exit;
  end;

  Status('Client ' + IntToStr(aHandle) + ' has connected to room ' + IntToStr(aRoom));
  fClientList.GetByHandle(aHandle).Room := aRoom;

  M := TKMemoryStreamBinary.Create;
  M.Write(fRoomInfo[aRoom].HostHandle);
  fGameFilter.Save(M);
  SendMessage(aHandle, mkConnectedToRoom, M);
  M.Free;

  MeasurePings;
  SaveHTMLStatus;
end;


procedure TKMNetServer.BanPlayerFromRoom(aHandle: TKMNetHandleIndex; aRoom:integer);
begin
  SetLength(fRoomInfo[aRoom].BannedIPs, Length(fRoomInfo[aRoom].BannedIPs) + 1);
  fRoomInfo[aRoom].BannedIPs[Length(fRoomInfo[aRoom].BannedIPs) - 1] := fServer.GetIP(aHandle);
end;


//Someone has disconnected from us.
procedure TKMNetServer.ClientDisconnect(aHandle: TKMNetHandleIndex);
var
  room: Integer;
  client: TKMServerClient;
  M: TKMemoryStream;
begin
  client := fClientList.GetByHandle(aHandle);
  if client = nil then
  begin
    Status('Warning: Client ' + inttostr(aHandle) + ' was already disconnected');
    exit;
  end;
  room := client.Room;
  if room <> -1 then
    Status('Client '+inttostr(aHandle)+' has disconnected'); //Only log messages for clients who entered a room
  fClientList.RemPlayer(aHandle);

  if room = -1 then Exit; //The client was not assigned a room yet

  //Send message to all remaining clients that client has disconnected
  SendMessageInd(NET_ADDRESS_ALL, mkClientLost, aHandle);

  //Assign a new host
  if fRoomInfo[room].HostHandle = aHandle then
  begin
    if GetRoomClientsCount(room) = 0 then
    begin
      fRoomInfo[room].HostHandle := NET_ADDRESS_EMPTY; //Room is now empty so we don't need a new host
      fRoomInfo[room].Password := '';
      fRoomInfo[room].GameInfo.Free;
      fRoomInfo[room].GameInfo := TKMPGameInfo.Create;
      SetLength(fRoomInfo[room].BannedIPs, 0);
    end
    else
    begin
      fRoomInfo[room].HostHandle := GetFirstRoomClient(room); //Assign hosting rights to the first client in the room

      //Tell everyone about the new host and password/description (so new host knows it)
      M := TKMemoryStreamBinary.Create;
      M.Write(fRoomInfo[room].HostHandle);
      M.WriteA(fRoomInfo[room].Password);
      M.WriteW(fRoomInfo[room].GameInfo.Description);
      SendMessageToRoom(mkReassignHost, room, M);
      M.Free;

      Status('Reassigned hosting rights for room ' + inttostr(room) + ' to ' + inttostr(fRoomInfo[room].HostHandle));
    end;
  end;
  SaveHTMLStatus;
end;


procedure TKMNetServer.SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind);
var
  M: TKMemoryStream;
begin
  M := TKMemoryStreamBinary.Create; //Send empty stream
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aParam: Integer; aImmidiate: Boolean = False);
var
  M: TKMemoryStream;
begin
  M := TKMemoryStreamBinary.Create;
  M.Write(aParam);
  SendMessageAct(aRecipient, aKind, M, aImmidiate);
  M.Free;
end;


procedure TKMNetServer.SendMessageInd(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aIndexOnServer: TKMNetHandleIndex; aImmidiate: Boolean = False);
var
  M: TKMemoryStream;
begin
  M := TKMemoryStreamBinary.Create;
  M.Write(aIndexOnServer);
  SendMessageAct(aRecipient, aKind, M, aImmidiate);
  M.Free;
end;


procedure TKMNetServer.SendMessageA(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; const aText: AnsiString);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfStringA);

  M := TKMemoryStreamBinary.Create;
  M.WriteA(aText);
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessageW(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; const aText: UnicodeString);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfStringW);

  M := TKMemoryStreamBinary.Create;
  M.WriteW(aText);
  SendMessageAct(aRecipient, aKind, M);
  M.Free;
end;


procedure TKMNetServer.SendMessage(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aStream: TKMemoryStream);
begin
  //Send stream without changes
  SendMessageAct(aRecipient, aKind, aStream);
end;


procedure TKMNetServer.SendMessageToRoom(aKind: TKMessageKind; aRoom: Integer; aStream: TKMemoryStream);
var
  I: Integer;
begin
  //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
  for I := fClientList.Count - 1 downto 0 do
    if fClientList[i].Room = aRoom then
      SendMessage(fClientList[i].Handle, aKind, aStream);
end;


//Assemble the packet as [Sender.Recepient.Length.Data]
procedure TKMNetServer.SendMessageAct(aRecipient: TKMNetHandleIndex; aKind: TKMessageKind; aStream: TKMemoryStream; aImmidiate: Boolean = False);
var
  I: Integer;
  M: TKMemoryStream;
begin
  M := TKMemoryStreamBinary.Create;

  //Header
  M.Write(TKMNetHandleIndex(NET_ADDRESS_SERVER)); //Make sure constant gets treated as 4byte integer
  M.Write(aRecipient);
  M.Write(Word(1 + aStream.Size)); //Message kind + data size

  //Contents
  M.Write(Byte(aKind));
  aStream.Position := 0;
  M.CopyFrom(aStream, aStream.Size);

  if M.Size > MAX_PACKET_SIZE then
  begin
    Status('Error: Packet over size limit');
    M.Free;
    Exit;
  end;

  if aRecipient = NET_ADDRESS_ALL then
    //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
    for I := fClientList.Count - 1 downto 0 do
      ScheduleSendData(fClientList[i].Handle, M.Memory, M.Size, aImmidiate)
  else
    ScheduleSendData(aRecipient, M.Memory, M.Size, aImmidiate);

  M.Free;
end;


procedure TKMNetServer.SendScheduledData(aServerClient: TKMServerClient);
var
  P: Pointer;
begin
  if aServerClient.fScheduledPacketsCnt > 0 then
  begin
    GetMem(P, aServerClient.fScheduledPacketsSize + 1); //+1 byte for packets number
    try
      //packets size into 1st byte
      PByte(P)^ := aServerClient.fScheduledPacketsCnt;
      //Copy collected packets data with 1 byte shift
      Move(aServerClient.fScheduledPackets[0], Pointer(NativeUInt(P) + 1)^, aServerClient.fScheduledPacketsSize);
      DoSendData(aServerClient.fHandle, P, aServerClient.fScheduledPacketsSize + 1); //+1 byte for packets number
      aServerClient.ClearScheduledPackets;
    finally
      FreeMem(P);
    end;
  end;
end;


procedure TKMNetServer.SetPacketsAccumulatingDelay(aValue: Integer);
begin
  fPacketsAccumulatingDelay := EnsureRange(aValue, PACKET_ACC_DELAY_MIN, PACKET_ACC_DELAY_MAX);
  fTimer.Interval := fPacketsAccumulatingDelay;
end;


procedure TKMNetServer.SetGameFilter(aGameFilter: TKMPGameFilter);
begin
  if fGameFIlter <> nil then
    FreeAndNil(fGameFilter);
  fGameFilter := aGameFilter;
end;


procedure TKMNetServer.UpdateState(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to fClientList.Count - 1 do
  begin
    //if (fGlobalTickCount mod SCHEDULE_PACKET_SEND_SPLIT) = (I mod SCHEDULE_PACKET_SEND_SPLIT) then
    SendScheduledData(fClientList[I]);
  end;
end;


//Wrapper around fServer.SendData so we can count TX bytes (don't use fServer.SendData anywhere else)
procedure TKMNetServer.DoSendData(aRecipient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
begin
  Inc(BytesTX, aLength);
//  Inc(PacketsSent);
  //gLog.AddTime('++++ send data to ' + GetNetAddressStr(aRecipient) + ' length = ' + IntToStr(aLength));
  fServer.SendData(aRecipient, aData, aLength);
end;


procedure TKMNetServer.ScheduleSendData(aRecipient: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal; aFlushQueue: Boolean = False);
var
  senderClient: TKMServerClient;
begin
  senderClient := fClientList.GetByHandle(aRecipient);

  if senderClient = nil then Exit;

  if (senderClient.fScheduledPacketsSize + aLength > MAX_CUMULATIVE_PACKET_SIZE)
    or (senderClient.fScheduledPacketsCnt = 255) then //Max number of packets = 255 (we use 1 byte for that)
  begin
    //gLog.AddTime(Format('@@@ FLUSH fScheduledPacketsSize + aLength = %d > %d', [SenderClient.fScheduledPacketsSize + aLength, MAX_CUMULATIVE_PACKET_SIZE]));
    SendScheduledData(senderClient);
  end;

  senderClient.AddScheduledPacket(aData, aLength);

  if aFlushQueue then
    SendScheduledData(senderClient);
end;


procedure TKMNetServer.RecieveMessage(aSenderHandle: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
var
  kind: TKMessageKind;
  M, M2: TKMemoryStream;
  tmpInt: Integer;
  gameRev: TKMGameRevision;
  tmpSmallInt: TKMNetHandleIndex;
  tmpStringA: AnsiString;
  client: TKMServerClient;
  senderIsHost: Boolean;
  senderRoom: Integer;
begin
  Assert(aLength >= 1, 'Unexpectedly short message');

  M := TKMemoryStreamBinary.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(kind, SizeOf(TKMessageKind));

  //Sometimes client disconnects then we recieve a late packet (e.g. mkPong), in which case ignore it
  if fClientList.GetByHandle(aSenderHandle) = nil then
  begin
    Status('Warning: Received data from an unassigned client');
    exit;
  end;

  senderRoom := fClientList.GetByHandle(aSenderHandle).Room;
  senderIsHost := (senderRoom <> -1) and
                  (fRoomInfo[senderRoom].HostHandle = aSenderHandle);

  case kind of
    mkJoinRoom:
            begin
              M.Read(tmpInt); //Room to join
              M.Read(gameRev);
              if InRange(tmpInt, 0, Length(fRoomInfo)-1)
                and (fRoomInfo[tmpInt].HostHandle <> NET_ADDRESS_EMPTY)
                //Once game has started don't ask for passwords so clients can reconnect
                and (fRoomInfo[tmpInt].GameInfo.GameState = mgsLobby)
                and (fRoomInfo[tmpInt].Password <> '') then
                SendMessage(aSenderHandle, mkReqPassword)
              else
                AddClientToRoom(aSenderHandle, tmpInt, gameRev);
            end;
    mkPassword:
            begin
              M.Read(tmpInt); //Room to join
              M.Read(gameRev);
              M.ReadA(tmpStringA); //Password
              if InRange(tmpInt, 0, Length(fRoomInfo)-1)
                and (fRoomInfo[tmpInt].HostHandle <> NET_ADDRESS_EMPTY)
                and (fRoomInfo[tmpInt].Password = tmpStringA) then
                AddClientToRoom(aSenderHandle, tmpInt, gameRev)
              else
                SendMessage(aSenderHandle, mkReqPassword);
            end;
    mkSetPassword:
            if senderIsHost then
            begin
              M.ReadA(tmpStringA); //Password
              fRoomInfo[senderRoom].Password := tmpStringA;
            end;
    mkSetGameInfo:
            if senderIsHost then
            begin
              fRoomInfo[senderRoom].GameInfo.LoadFromStream(M);
              SaveHTMLStatus;
            end;
    mkKickPlayer:
            if senderIsHost then
            begin
              M.Read(tmpSmallInt);
              if fClientList.GetByHandle(tmpSmallInt) <> nil then
              begin
                SendMessage(tmpSmallInt, mkKicked, TX_NET_KICK_BY_HOST, True);
                fServer.Kick(tmpSmallInt);
              end;
            end;
    mkBanPlayer:
            if senderIsHost then
            begin
              M.Read(tmpSmallInt);
              if fClientList.GetByHandle(tmpSmallInt) <> nil then
              begin
                BanPlayerFromRoom(tmpSmallInt, senderRoom);
                SendMessage(tmpSmallInt, mkKicked, TX_NET_BANNED_BY_HOST, True);
                fServer.Kick(tmpSmallInt);
              end;
            end;
    mkGiveHost:
            if senderIsHost then
            begin
              M.Read(tmpSmallInt);
              if fClientList.GetByHandle(tmpSmallInt) <> nil then
              begin
                fRoomInfo[senderRoom].HostHandle := tmpSmallInt;
                //Tell everyone about the new host and password/description (so new host knows it)
                M2 := TKMemoryStreamBinary.Create;
                M2.Write(fRoomInfo[senderRoom].HostHandle);
                M2.WriteA(fRoomInfo[senderRoom].Password);
                M2.WriteW(fRoomInfo[senderRoom].GameInfo.Description);
                SendMessageToRoom(mkReassignHost, senderRoom, M2);
                M2.Free;
              end;
            end;
    mkResetBans:
            if senderIsHost then
            begin
              SetLength(fRoomInfo[senderRoom].BannedIPs, 0);
            end;
    mkGetServerInfo:
            begin
              M2 := TKMemoryStreamBinary.Create;
              SaveToStream(M2);
              SendMessage(aSenderHandle, mkServerInfo, M2);
              M2.Free;
            end;
    mkFPS: begin
              client := fClientList.GetByHandle(aSenderHandle);
              M.Read(tmpInt);
              client.FPS := tmpInt;
            end;
    mkPong:
            begin
              client := fClientList.GetByHandle(aSenderHandle);
//              M.Read(tmpInteger);
//              Client.FPS := tmpInteger;
              if (client.fPingStarted <> 0) then
              begin
                client.Ping := Math.Min(TimeSince(client.fPingStarted), High(Word));
                client.fPingStarted := 0;
              end;
            end;
  end;

  M.Free;
end;


//Someone has send us something
//Send only complete messages to allow to add server messages inbetween
procedure TKMNetServer.DataAvailable(aHandle: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal);
//  function GetMessKind(aSenderHandle: TKMNetHandleIndex; aData: Pointer; aLength: Cardinal): TKMessageKind;
//  var
//    M: TKMemoryStream;
//  begin
//    M := TKMemoryStream.Create;
//    M.WriteBuffer(aData^, aLength);
//    M.Position := 0;
//    M.Read(Result, SizeOf(TKMessageKind));
//    M.Free;
//  end;

var
  I, senderRoom: Integer;
  packetSender, packetRecipient: TKMNetHandleIndex;
  packetLength: Word;
  senderClient: TKMServerClient;
//  Kind: TKMessageKind;
begin
  Inc(BytesRX, aLength);
  senderClient := fClientList.GetByHandle(aHandle);
  if senderClient = nil then
  begin
    Status('Warning: Data Available from an unassigned client');
//    gLog.AddTime('Warning: Data Available from an unassigned client');
    Exit;
  end;

  //Append new data to buffer
  SetLength(senderClient.fBuffer, senderClient.fBufferSize + aLength);
  Move(aData^, senderClient.fBuffer[senderClient.fBufferSize], aLength);
  senderClient.fBufferSize := senderClient.fBufferSize + aLength;

//  gLog.AddTime('----  Received data from ' + GetNetAddressStr(aHandle) + ': length = ' + IntToStr(aLength));

  //Try to read data packet from buffer
  while senderClient.fBufferSize >= 6 do
  begin
    packetSender := PKMNetHandleIndex(@senderClient.fBuffer[0])^;
    packetRecipient := PKMNetHandleIndex(@senderClient.fBuffer[2])^;
    packetLength := PWord(@senderClient.fBuffer[4])^;

    //Do some simple range checking to try to detect when there is a serious error or flaw in the code (i.e. Random data in the buffer)
    if not (IsValidHandle(packetRecipient) and IsValidHandle(packetSender) and (packetLength <= MAX_PACKET_SIZE)) then
    begin
      //When we receive corrupt data kick the client since we have no way to recover (if in-game client will auto reconnect)
      Status('Warning: Corrupt data received, kicking client ' + IntToStr(aHandle));
      senderClient.fBufferSize := 0;
      SetLength(senderClient.fBuffer, 0);
      fServer.Kick(aHandle);
      Exit;
    end;

    if packetLength > senderClient.fBufferSize - 6 then
      Exit; //This message was split, so we must wait for the remainder of the message to arrive

    senderRoom := fClientList.GetByHandle(aHandle).Room;

    //If sender from packet contents doesn't match the socket handle, don't process this packet (client trying to fake sender)
    if packetSender = aHandle then
    begin
//      Kind := GetMessKind(PacketSender, @SenderClient.fBuffer[6], PacketLength);
//      gLog.AddTime(Format('Got msg %s from %d to %d',
//                          [GetEnumName(TypeInfo(TKMessageKind), Integer(Kind)), PacketSender, PacketRecipient]));
      case packetRecipient of
        NET_ADDRESS_OTHERS: //Transmit to all except sender
                //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
                for I := fClientList.Count - 1 downto 0 do
                  if (aHandle <> fClientList[i].Handle) and (senderRoom = fClientList[i].Room) then
                    ScheduleSendData(fClientList[i].Handle, @senderClient.fBuffer[0], packetLength+6);
        NET_ADDRESS_ALL: //Transmit to all including sender (used mainly by TextMessages)
                //Iterate backwards because sometimes calling Send results in ClientDisconnect (LNet only?)
                for I := fClientList.Count - 1 downto 0 do
                  if senderRoom = fClientList[i].Room then
                    ScheduleSendData(fClientList[i].Handle, @senderClient.fBuffer[0], packetLength+6);
        NET_ADDRESS_HOST:
                if senderRoom <> -1 then
                  ScheduleSendData(fRoomInfo[senderRoom].HostHandle, @senderClient.fBuffer[0], packetLength+6);
        NET_ADDRESS_SERVER:
                RecieveMessage(packetSender, @senderClient.fBuffer[6], packetLength);
        else    ScheduleSendData(packetRecipient, @senderClient.fBuffer[0], packetLength+6);
      end;
    end;

    //Processing that packet may have caused this client to be kicked (joining room where banned)
    //and in that case SenderClient is invalid so we must exit immediately
    if fClientList.GetByHandle(aHandle) = nil then
      Exit;

    if senderClient.fBufferSize > 6 + packetLength then //Check range
      Move(senderClient.fBuffer[6 + packetLength], senderClient.fBuffer[0], senderClient.fBufferSize-packetLength-6);
    senderClient.fBufferSize := senderClient.fBufferSize - packetLength - 6;
  end;
end;


procedure TKMNetServer.SaveToStream(aStream: TKMemoryStream);
var
  I, roomsNeeded, emptyRoomID: Integer;
  needEmptyRoom: boolean;
begin
  roomsNeeded := 0;
  for I := 0 to fRoomCount - 1 do
    if GetRoomClientsCount(I) > 0 then
      Inc(roomsNeeded);

  if roomsNeeded < fMaxRooms then
  begin
    Inc(roomsNeeded); //Need 1 empty room at the end, if there is space
    needEmptyRoom := True;
  end
  else
    needEmptyRoom := False;

  aStream.Write(roomsNeeded);
  emptyRoomID := fRoomCount;
  for I := 0 to fRoomCount - 1 do
  begin
    if GetRoomClientsCount(I) = 0 then
    begin
      if emptyRoomID = fRoomCount then
        emptyRoomID := I;
    end
    else
    begin
      aStream.Write(I); //RoomID
      aStream.Write(fRoomInfo[I].GameRevision);
      fRoomInfo[I].GameInfo.SaveToStream(aStream);
    end;
  end;
  //Write out the empty room at the end
  if needEmptyRoom then
  begin
    aStream.Write(emptyRoomID); //RoomID
    aStream.Write(TKMGameRevision(EMPTY_ROOM_DEFAULT_GAME_REVISION)); //no game revision was set yet
    fEmptyGameInfo.SaveToStream(aStream);
  end;
end;


function TKMNetServer.IsValidHandle(aHandle: TKMNetHandleIndex): Boolean;
begin
  //Can not use "in [...]" with negative numbers
  Result := (aHandle = NET_ADDRESS_OTHERS) or (aHandle = NET_ADDRESS_ALL)
         or (aHandle = NET_ADDRESS_HOST) or (aHandle = NET_ADDRESS_SERVER)
         or InRange(aHandle, FIRST_TAG, fServer.GetMaxHandle);
end;


function TKMNetServer.AddNewRoom: Boolean;
begin
  if fRoomCount = fMaxRooms then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  Inc(fRoomCount);
  SetLength(fRoomInfo,fRoomCount);
  fRoomInfo[fRoomCount-1].HostHandle := NET_ADDRESS_EMPTY;
  fRoomInfo[fRoomCount-1].GameRevision := 0;
  fRoomInfo[fRoomCount-1].Password := '';
  fRoomInfo[fRoomCount-1].GameInfo := TKMPGameInfo.Create;
  SetLength(fRoomInfo[fRoomCount-1].BannedIPs, 0);
end;


function TKMNetServer.GetFirstAvailableRoom: Integer;
var
  I: Integer;
begin
  for I := 0 to fRoomCount-1 do
    if GetRoomClientsCount(I) = 0 then
    begin
      Result := I;
      exit;
    end;
  if AddNewRoom then //Otherwise we must create a room
    Result := fRoomCount-1
  else
    Result := -1;
end;


function TKMNetServer.GetRoomClientsCount(aRoom: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to fClientList.Count - 1 do
    if fClientList[I].Room = aRoom then
      inc(Result);
end;


function TKMNetServer.GetFirstRoomClient(aRoom: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to fClientList.Count - 1 do
    if fClientList[I].Room = aRoom then
    begin
      Result := fClientList[I].fHandle;
      exit;
    end;
  raise Exception.Create('');
end;


procedure TKMNetServer.SaveHTMLStatus;

  function AddThousandSeparator(const S: string; Chr: Char=','): string;
  var
    I: Integer;
  begin
    Result := S;
    I := Length(S) - 2;
    while I > 1 do
    begin
      Insert(Chr, Result, I);
      I := I - 3;
    end;
  end;

  function ColorToText(aCol: Cardinal): string;
  begin
    Result := '#' + IntToHex(aCol and $FF, 2) + IntToHex((aCol shr 8) and $FF, 2) + IntToHex((aCol shr 16) and $FF, 2);
  end;

const
  BOOL_TEXT: array[Boolean] of string = ('0', '1');
var
  I, K, playerCount, clientCount, roomCount: Integer;
  xml: TXmlVerySimple;
  html: string;
  roomCountNode, clientCountNode, playerCountNode, node: TXmlNode;
  myFile: TextFile;
begin
  if fHTMLStatusFile = '' then exit; //Means do not write status

  roomCount := 0;
  playerCount := 0;
  clientCount := 0;

  xml := TXmlVerySimple.Create;

  try
    //HTML header
    html := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+sLineBreak+
            '<HTML>'+sLineBreak+'<HEAD>'+sLineBreak+'  <TITLE>KaM Remake Server Status</TITLE>'+sLineBreak+
            '  <meta http-equiv="content-type" content="text/html; charset=utf-8">'+sLineBreak+'</HEAD>'+sLineBreak;
    html := html + '<BODY>'+sLineBreak;
    html := html + '<TABLE border="1">'+sLineBreak+'<TR><TD><b>Room ID</b></TD><TD><b>State</b><TD><b>Player Count</b></TD></TD><TD><b>Map</b></TD><TD><b>Game Time</b></TD><TD><b>Player Names</b></TD></TR>'+sLineBreak;

    //XML header
    xml.Root.NodeName := 'server';
    roomCountNode := xml.Root.AddChild('roomcount'); //Set it later
    playerCountNode := xml.Root.AddChild('playercount');
    clientCountNode := xml.Root.AddChild('clientcount');
    xml.Root.AddChild('bytessent').Text := IntToStr(BytesTX);
    xml.Root.AddChild('bytesreceived').Text := IntToStr(BytesRX);

    for I:=0 to fRoomCount-1 do
      if GetRoomClientsCount(I) > 0 then
      begin
        inc(roomCount);
        inc(playerCount, fRoomInfo[I].GameInfo.PlayerCount);
        inc(clientCount, fRoomInfo[I].GameInfo.ConnectedPlayerCount);
        //HTML room info
        html := html + '<TR><TD>'+IntToStr(I)+
                       '</TD><TD>r'+ IntToStr(fRoomInfo[I].GameRevision) +
                       '</TD><TD>'+XMLEscape(GameStateText[fRoomInfo[I].GameInfo.GameState])+
                       '</TD><TD>'+IntToStr(fRoomInfo[I].GameInfo.ConnectedPlayerCount)+
                       '</TD><TD>'+XMLEscape(fRoomInfo[I].GameInfo.Map)+
                       '&nbsp;</TD><TD>'+XMLEscape(fRoomInfo[I].GameInfo.GetFormattedTime)+
                       //HTMLPlayersList does escaping itself
                       '&nbsp;</TD><TD>'+fRoomInfo[I].GameInfo.HTMLPlayersList+'</TD></TR>'+sLineBreak;
        //XML room info
        node := xml.Root.AddChild('room');
        node.Attribute['id'] := IntToStr(I);
        node.AddChild('state').Text := GameStateText[fRoomInfo[I].GameInfo.GameState];
        node.AddChild('roomplayercount').Text := IntToStr(fRoomInfo[I].GameInfo.PlayerCount);
        node.AddChild('map').Text := fRoomInfo[I].GameInfo.Map;
        node.AddChild('gametime').Text := fRoomInfo[I].GameInfo.GetFormattedTime;
        with node.AddChild('players') do
        begin
          for K:=1 to fRoomInfo[I].GameInfo.PlayerCount do
            with AddChild('player') do
            begin
              Text := UnicodeString(fRoomInfo[I].GameInfo.Players[K].Name);
              SetAttribute('color', ColorToText(fRoomInfo[I].GameInfo.Players[K].Color));
              SetAttribute('connected', BOOL_TEXT[fRoomInfo[I].GameInfo.Players[K].Connected]);
              SetAttribute('type', NetPlayerTypeName[fRoomInfo[I].GameInfo.Players[K].PlayerType]);
              SetAttribute('langcode', UnicodeString(fRoomInfo[I].GameInfo.Players[K].LangCode));
              SetAttribute('team', IntToStr(fRoomInfo[I].GameInfo.Players[K].Team));
              SetAttribute('spectator', BOOL_TEXT[fRoomInfo[I].GameInfo.Players[K].IsSpectator]);
              SetAttribute('host', BOOL_TEXT[fRoomInfo[I].GameInfo.Players[K].IsHost]);
              SetAttribute('won_or_lost', WonOrLostText[fRoomInfo[I].GameInfo.Players[K].WonOrLost]);
            end;
        end;
      end;
    //Set counts in XML
    roomCountNode.Text := IntToStr(roomCount);
    playerCountNode.Text := IntToStr(playerCount);
    clientCountNode.Text := IntToStr(clientCount);

    //HTML footer
    html := html + '</TABLE>'+sLineBreak+
                   '<p>Total sent: '+AddThousandSeparator(IntToStr(BytesTX))+' bytes</p>'+sLineBreak+
                   '<p>Total received: '+AddThousandSeparator(IntToStr(BytesRX))+' bytes</p>'+sLineBreak+
                   '</BODY>'+sLineBreak+'</HTML>';

    //Write HTML
    AssignFile(myFile, fHTMLStatusFile);
    ReWrite(myFile);
    Write(myFile,html);
    CloseFile(myFile);
    //Write XML
    xml.SaveToFile(ChangeFileExt(fHTMLStatusFile,'.xml'));
  finally
    xml.Free;
  end;
end;


end.

