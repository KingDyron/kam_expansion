﻿unit KM_NetworkClasses;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, StrUtils, KM_CommonTypes, KM_CommonClasses, KM_NetworkTypes, KM_Defaults, KM_GameOptions;

type
  TKMPlayerGameResult = (pgrNone, pgrWin, pgrDefeat);

  //Stores information about a multiplayer game to be sent: host -> server -> queriers
  TKMPGameInfo = class
  public
    GameState: TMPGameState;
    PasswordLocked: Boolean;
    PlayerCount: Byte;
    GameOptions: TKMGameOptions;
    Players: array[1..MAX_LOBBY_SLOTS] of record
                                            Name: AnsiString;
                                            Color: Cardinal;
                                            Connected: Boolean;
                                            LangCode: AnsiString;
                                            Team: Integer;
                                            IsSpectator: Boolean;
                                            IsHost: Boolean;
                                            PlayerType: TKMNetPlayerType;
                                            WonOrLost: TWonOrLost;
                                          end;
    Description: UnicodeString;
    Map: UnicodeString;
    GameTime: TDateTime;
    constructor Create;
    destructor Destroy; override;
    function GetFormattedTime: UnicodeString;
    procedure LoadFromStream(aStream: TKMemoryStream);
    procedure SaveToStream(aStream: TKMemoryStream);
    function HTMLPlayersList: string;
    function ConnectedPlayerCount: Byte;
  end;


implementation
uses
  VerySimpleXML, KM_CommonUtils;


{ TKMPGameInfo }
procedure TKMPGameInfo.LoadFromStream(aStream: TKMemoryStream);
var
  I: Integer;
begin
  aStream.Read(GameState, SizeOf(GameState));
  aStream.Read(PasswordLocked);
  aStream.Read(PlayerCount);
  if GameOptions = nil then
    GameOptions := TKMGameOptions.Create;
  GameOptions.Load(aStream);
  for I := 1 to PlayerCount do
  begin
    aStream.ReadA(Players[I].Name);
    aStream.Read(Players[I].Color);
    aStream.Read(Players[I].Connected);
    aStream.ReadA(Players[I].LangCode);
    aStream.Read(Players[I].Team);
    aStream.Read(Players[I].IsSpectator);
    aStream.Read(Players[I].IsHost);
    aStream.Read(Players[I].PlayerType, SizeOf(Players[I].PlayerType));
    aStream.Read(Players[I].WonOrLost, SizeOf(Players[I].WonOrLost));
  end;
  aStream.ReadW(Description);
  aStream.ReadW(Map);
  aStream.Read(GameTime, SizeOf(GameTime));
end;


//Return string representation of games length
constructor TKMPGameInfo.Create;
begin
  inherited;
  GameOptions := TKMGameOptions.Create;
end;


destructor TKMPGameInfo.Destroy;
begin
  if GameOptions <> nil then
    FreeAndNil(GameOptions);
  inherited;
end;


function TKMPGameInfo.GetFormattedTime: UnicodeString;
begin
  if GameTime >= 0 then
    Result := TimeToString(GameTime)
  else
    Result := '';
end;


procedure TKMPGameInfo.SaveToStream(aStream: TKMemoryStream);
var
  I: Integer;
begin
  aStream.Write(GameState, SizeOf(GameState));
  aStream.Write(PasswordLocked);
  aStream.Write(PlayerCount);
  GameOptions.Save(aStream);
  for I := 1 to PlayerCount do
  begin
    aStream.WriteA(Players[I].Name);
    aStream.Write(Players[I].Color);
    aStream.Write(Players[I].Connected);
    aStream.WriteA(Players[I].LangCode);
    aStream.Write(Players[I].Team);
    aStream.Write(Players[I].IsSpectator);
    aStream.Write(Players[I].IsHost);
    aStream.Write(Players[I].PlayerType, SizeOf(Players[I].PlayerType));
    aStream.Write(Players[I].WonOrLost, SizeOf(Players[I].WonOrLost));
  end;
  aStream.WriteW(Description);
  aStream.WriteW(Map);
  aStream.Write(GameTime, SizeOf(GameTime));
end;


//This function should do its own XML escaping
function TKMPGameInfo.HTMLPlayersList: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to PlayerCount do
    if Players[I].PlayerType = nptHuman then
    begin
      if Result <> '' then Result := Result + ', ';
      if not Players[I].Connected then Result := Result + '<strike>';
      Result := Result + XMLEscape(UnicodeString(Players[I].Name));
      if not Players[I].Connected then Result := Result + '</strike>';
    end;
end;


function TKMPGameInfo.ConnectedPlayerCount: Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to PlayerCount do
    if Players[I].Connected and (Players[I].PlayerType = nptHuman) then
      Inc(Result);
end;


end.

