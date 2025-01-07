unit KM_NetClient;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_NetClientOverbyte;


{ Contains basic items we need for smooth Net experience:

    - connect to server
    - signal if we have successfully connected to server
    - signal if we could not connect to server

    - disconnect from server (always successfull)
    - signal if we were forcefully disconnected by server

    - send binary data to other server clients
    - recieve binary data from other server clients

    - optionaly report non-important status messages

}
type
  TKMNetClient = class
  private
    fClient:TKMNetClientOverbyte;
    fConnected:boolean;

    fBufferSize:cardinal;
    fBuffer:array of byte;

    fOnConnectSucceed:TNotifyEvent;
    fOnConnectFailed:TGetStrProc;
    fOnForcedDisconnect:TGetStrProc;
    fOnRecieveData:TNotifyDataEvent;
    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure ConnectSucceed(Sender: TObject);
    procedure ConnectFailed(const S: string);
    procedure ForcedDisconnect(Sender: TObject);
    procedure RecieveData(aData:pointer; aLength:cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    property Connected:boolean read fConnected;
    function MyIPString:string;

    procedure ConnectTo(const aAddress:string; const aPort:string); //Try to connect to server
    property OnConnectSucceed:TNotifyEvent write fOnConnectSucceed; //Signal success
    property OnConnectFailed:TGetStrProc write fOnConnectFailed; //Signal fail and text description

    procedure Disconnect; //Disconnect from server
    property OnForcedDisconnect:TGetStrProc write fOnForcedDisconnect; //Signal we were forcelly disconnected

    procedure SendText(const aData:string); //For now we use just plain text
    property OnRecieveData:TNotifyDataEvent write fOnRecieveData;
    procedure SendData(aData:pointer; aLength:cardinal);

    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
  end;


implementation


constructor TKMNetClient.Create;
begin
  Inherited;
  fClient := TKMNetClientOverbyte.Create;
  fConnected := false;
end;


destructor TKMNetClient.Destroy;
begin
  fClient.Free;
  Inherited;
end;


function TKMNetClient.MyIPString:string;
begin
  Result := fClient.MyIPString;
end;


procedure TKMNetClient.Error(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Error '+S);
end;


procedure TKMNetClient.ConnectTo(const aAddress:string; const aPort:string);
begin
  fClient.OnError := Error;
  fClient.OnConnectSucceed := ConnectSucceed;
  fClient.OnConnectFailed := ConnectFailed;
  fClient.OnSessionDisconnected := ForcedDisconnect;
  fClient.OnRecieveData := RecieveData;
  fClient.ConnectTo(aAddress, aPort);
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connecting..');
end;


procedure TKMNetClient.ConnectSucceed(Sender: TObject);
begin
  fConnected := true;
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connected');
  fOnConnectSucceed(Self);
end;


procedure TKMNetClient.ConnectFailed(const S: string);
begin
  fConnected := false;
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Connection failed. '+S);
  fOnConnectFailed(S);
end;


procedure TKMNetClient.Disconnect;
begin
  fConnected := false;
  fClient.Disconnect;
end;


//Happens in following cases:
//  - when we deliberately disconnect
//  - when connection failed
//  - when server disconnects us
procedure TKMNetClient.ForcedDisconnect(Sender: TObject);
begin
  if fConnected then
  begin
    if Assigned(fOnStatusMessage) then fOnStatusMessage('Client: Forced disconnect');
    fOnForcedDisconnect('9');
  end;
  fConnected := false;
end;


procedure TKMNetClient.SendText(const aData:string);
begin
  SendData(@aData[1], length(aData));
end;


//Assemble the packet as [Length.Data]
procedure TKMNetClient.SendData(aData:pointer; aLength:cardinal);
var P:Pointer;
begin
  GetMem(P, aLength+4);
  PInteger(P)^ := aLength;
  Move(aData^, Pointer(NativeUInt(P)+4)^, aLength);
  fClient.SendData(P, aLength+4);
  FreeMem(P);
end;


//Split recieved data into single packets
procedure TKMNetClient.RecieveData(aData:pointer; aLength:cardinal);
var PacketLength:Cardinal;
begin
  //Append new data to buffer
  SetLength(fBuffer, fBufferSize + aLength);
  Move(aData^, fBuffer[fBufferSize], aLength);
  fBufferSize := fBufferSize + aLength;

  //Try to read data packet from buffer
  while fBufferSize >= 4 do
  begin
    PacketLength := PCardinal(fBuffer)^;
    if PacketLength <= fBufferSize-4 then
    begin
      fOnRecieveData(@fBuffer[4], PacketLength); //Skip PacketLength
      if 4+PacketLength < fBufferSize then //Check range
        Move(fBuffer[4+PacketLength], fBuffer[0], fBufferSize-PacketLength-4);
      fBufferSize := fBufferSize - PacketLength - 4;
    end else
      Exit;
  end;
end;


end.
