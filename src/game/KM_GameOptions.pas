unit KM_GameOptions;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_MapTypes;


type
  //Game options set in MP lobby
  //(maybe later we could use some of these for SP games too)
  TKMGameOptions = class
  public
    Peacetime: Word; //Peacetime in minutes
    SpeedPT: Single; //Game speed during peacetime
    SpeedAfterPT: Single; //Game speed after peacetime (usually slower)
    RandomSeed: Integer;
    MissionDifficulty: TKMMissionDifficulty;
    //new variables
    MissionBuiltInDifficulty: TKMMissionBuiltInDifficulty;
    Weather : TKMSettingsWeather;
    Mode : TKMMissionMode;
    constructor Create;
    procedure Reset;
    procedure Save(SaveStream: TKMemoryStream; aWeather : Boolean = true);
    procedure Load(LoadStream: TKMemoryStream; aWeather : Boolean = true);
    function ToString: string; reintroduce;
  end;


implementation
uses
  SysUtils, TypInfo, KM_Defaults;


{ TKMGameOptions }
constructor TKMGameOptions.Create;
begin
  inherited;

  //Default values are not always 0
  Reset;
end;


//Resets values to defaults
procedure TKMGameOptions.Reset;
begin
  Peacetime := DEFAULT_PEACE_TIME;
  SpeedPT := 1;
  SpeedAfterPT := 1;
  RandomSeed := 0; //Must be init later on. 0 is an erroneous value for KaMSeed
  MissionDifficulty := mdNone;
  MissionBuiltInDifficulty := mdbNormal;
  Weather.SetDefault;
  Mode := mmClassic;
end;


procedure TKMGameOptions.Load(LoadStream: TKMemoryStream; aWeather : Boolean = true);
begin
  LoadStream.Read(Peacetime);
  LoadStream.Read(SpeedPT);
  LoadStream.Read(SpeedAfterPT);
  LoadStream.Read(RandomSeed);
  LoadStream.Read(MissionDifficulty, SizeOf(MissionDifficulty));
  //the variables above are the same for KaM Remake
  //if we want to have their server to work also in expansion
  //we need to read our settings later
  if not aWeather then
    Exit;
  LoadStream.Read(MissionBuiltInDifficulty, SizeOf(MissionBuiltInDifficulty));
  LoadStream.Read(Weather, SizeOf(Weather));
  LoadStream.Read(Mode, SizeOf(Mode));
end;


procedure TKMGameOptions.Save(SaveStream: TKMemoryStream; aWeather : Boolean = true);
begin
  SaveStream.Write(Peacetime);
  SaveStream.Write(SpeedPT);
  SaveStream.Write(SpeedAfterPT);
  SaveStream.Write(RandomSeed);
  SaveStream.Write(MissionDifficulty, SizeOf(MissionDifficulty));
  //the variables above are the same for KaM Remake
  //if we want to have their server to work also in expansion
  //we need to write our settings later
  if not aWeather then
    Exit;
  SaveStream.Write(MissionBuiltInDifficulty, SizeOf(MissionBuiltInDifficulty));
  SaveStream.Write(Weather, SizeOf(Weather));
  SaveStream.Write(Mode, SizeOf(Mode));
end;


function TKMGameOptions.ToString: string;
begin
  Result := Format('PT = %d; SpeedPT = %s; SpeedAfterPT = %s; Seed = %d; Difficulty = %s',
                   [Peacetime, FormatFloat('0.##', SpeedPT), FormatFloat('0.##', SpeedAfterPT),
                    RandomSeed, GetEnumName(TypeInfo(TKMMissionDifficulty), Integer(MissionDifficulty))]);
end;


end.
