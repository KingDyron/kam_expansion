unit KM_WeatherCollection;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes,
  KM_CommonClasses,
  KM_Points,
  KM_ResTypes,
  KM_WeatherTypes,
  KM_Weather,
  KM_GameSettings;

  type
    TKMWeatherCollection = class
      private
        fTicker : Cardinal;
        fList : array of TKMWeather;
        function GetCount : Word;
        procedure AddItem(aWeather : TKMWeather); overload;
        procedure SpawnNewWeather;
        procedure DeleteWeather(aIndex : Integer);
        function GetTerrainDiv : Integer;
      public
        Settings : TKMSettingsWeather;
        constructor Create;
        destructor Destroy; override;
        procedure AddItem(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType); overload;

        property Count : Word read GetCount;

        procedure StartMission;

        procedure Save(SaveStream : TKMemoryStream);
        procedure Load(LoadStream : TKMemoryStream);
        procedure UpdateState;
        procedure Paint(aLag : Single; aClipRect : TKMRect);
    end;

implementation
uses
  KM_CommonUtils,
  KM_Terrain, KM_TerrainTypes;
constructor TKMWeatherCollection.Create;
begin
  Inherited;
  SetLength(fList, 0);
  fTicker := 600;
end;

destructor TKMWeatherCollection.Destroy;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].Free;

  Inherited;
end;

function TKMWeatherCollection.GetCount: Word;
begin
  Result := length(fList);
end;

procedure TKMWeatherCollection.AddItem(aWeather: TKMWeather);
begin
  SetLength(fList, Count + 1);
  fList[Count - 1] := aWeather;
end;

procedure TKMWeatherCollection.AddItem(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  SetLength(fList, Count + 1);
  case aType of
    wtNone: ;
    wtCloudy1:      fList[Count - 1] := TKMWeatherCloudy.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtCloudy2:      fList[Count - 1] := TKMWeatherCloudy2.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtRain:         fList[Count - 1] := TKMWeatherRain.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtStorm:        fList[Count - 1] := TKMWeatherStorm.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtSnow:         fList[Count - 1] := TKMWeatherSnow.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtSnowyStorm:   fList[Count - 1] := TKMWeatherSnowStorm.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtSandStorm1:   fList[Count - 1] := TKMWeatherSandStorm1.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtSandStorm2:   fList[Count - 1] := TKMWeatherSandStorm2.Create(aType, aPos, aSpeed, aLifeTime, aRX);
    wtTornado:      fList[Count - 1] := TKMWeatherTornado.Create(aType, aPos, aSpeed, aLifeTime, aRX);
  end;
end;

procedure TKMWeatherCollection.StartMission;
begin
  fTicker := 1;
end;

procedure TKMWeatherCollection.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
  tmpType : TKMWeatherType;
begin
  SaveStream.PlaceMarker('WeatherCollection');
  SaveStream.Write(fTicker);
  SaveStream.Write(Settings, SizeOf(Settings));

  newCount := Count;
  SaveStream.Write(newCount);
  for I := 0 to Count - 1 do
  begin
    tmpType := fList[I].WeatherType;
    SaveStream.Write(tmpType, SizeOf(tmpType));
    fList[I].Save(SaveStream);
  end;
end;

procedure TKMWeatherCollection.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
  tmp : TKMWeather;
  tmpType : TKMWeatherType;
begin
  LoadStream.CheckMarker('WeatherCollection');
  LoadStream.Read(fTicker);
  LoadStream.Read(Settings, SizeOf(Settings));

  LoadStream.Read(newCount);
  for I := 0 to newCount - 1 do
  begin
    LoadStream.Read(tmpType, SizeOf(tmpType));
    tmp := nil;
    case tmpType of
      wtNone: ;
      wtCloudy1:      tmp := TKMWeatherCloudy.Load(tmpType, LoadStream);
      wtCloudy2:      tmp := TKMWeatherCloudy2.Load(tmpType, LoadStream);
      wtRain:         tmp := TKMWeatherRain.Load(tmpType, LoadStream);
      wtStorm:        tmp := TKMWeatherStorm.Load(tmpType, LoadStream);
      wtSnow:         tmp := TKMWeatherSnow.Load(tmpType, LoadStream);
      wtSnowyStorm:   tmp := TKMWeatherSnowStorm.Load(tmpType, LoadStream);
      wtSandStorm1:   tmp := TKMWeatherSandStorm1.Load(tmpType, LoadStream);
      wtSandStorm2:   tmp := TKMWeatherSandStorm2.Load(tmpType, LoadStream);
      wtTornado:      tmp := TKMWeatherTornado.Load(tmpType, LoadStream);
    end;
    if tmp <> nil then
      if not tmp.Deleted then
        AddItem(tmp);
  end;
end;

procedure TKMWeatherCollection.SpawnNewWeather;
var P : TKMPoint;
  speed : TKMPointF;
  lifeTime : Cardinal;
  isRain, isStorm : Boolean;
  wt : TKMWeatherType;
begin
  if Count >= Settings.MaxCount * (GetTerrainDiv + 1) then
    Exit;

  isRain := false;
  isStorm := false;
  //get random pos
  P.X := KaMRandom(gTerrain.MapX - 1, 'TKMWeatherCollection.SpawnNewWeather 1');
  P.Y := KaMRandom(gTerrain.MapX - 1, 'TKMWeatherCollection.SpawnNewWeather 2');

  speed.X := KaMRandomS2(Settings.MaxCloudSpeed, 'TKMWeatherCollection.SpawnNewWeather 3');
  speed.Y := KaMRandomS2(Settings.MaxCloudSpeed, 'TKMWeatherCollection.SpawnNewWeather 4');
  lifeTime := 100 + KaMRandom(100 + Settings.MaxLifeTime, 'TKMWeatherCollection.SpawnNewWeather 5'); //weather should not live longer than 1.5 minutes

  case KaMRandom(100, '') of
    0..50: ;
    51..85 : isRain := true;
    86..99 :  begin
                isStorm := true;
                isRain := true;
              end;
  end;


  if not isRain then
    wt := wtCloudy1
  else
  begin
    case gTerrain.FindBestClimatType(P) of
      tcWarm1     : wt := wtRain;
      tcWet1      : wt := wtStorm;
      tcWet2      : If KaMRandom(100, 'Tornado') < 100 then wt := wtTornado else wt := wtStorm;
      tcNeutral   : wt := wtRain;
      tcCold1     : wt := wtSnow;
      tcCold2     : wt := wtSnowyStorm;
      tcDry1      : wt := wtSandStorm1;
      tcDry2      : wt := wtSandStorm2;
      else wt := wtCloudy2;
    end;

    if isStorm then
      case wt of
        wtRain: wt := wtStorm;
        wtSnow: wt := wtSnowyStorm;
        wtSandStorm1: wt := wtSandStorm2;
      end;

  end;

  //wt := wtCloudy1;
  AddItem(wt, KMPointF(P), speed, lifeTime, rxTrees);

end;

procedure TKMWeatherCollection.DeleteWeather(aIndex: Integer);
var I : Integer;
begin
  fList[aIndex].Free;
  for I := aIndex to High(fList) - 1 do
    fList[I] := fList[I + 1];
  SetLength(fList, high(fList));
end;

function TKMWeatherCollection.GetTerrainDiv: Integer;
begin
  Result := (gTerrain.MapX * gTerrain.MapY) div (50 * 50);
end;

procedure TKMWeatherCollection.UpdateState;
var I, K, C : Integer;
begin
  //first update every weathers
  for I := 0 to Count - 1 do
    fList[I].UpdateState;

  //weather is spawning automaticaly
  //so when player don't want it, it won't spawn
  //but weather can be spawn by scripts, so we still need to update them
  if Settings.Enabled then
  begin
    //now add new one
    dec(fTicker);
    if fTicker = 0 then //check once every 2 minutes
    begin
      fTicker := Settings.MinInterval + KaMRandom(Settings.MaxInterval - Settings.MinInterval, 'TKMWeatherCollection.UpdateState');


      //if KaMRandom(100, 'TKMWeatherCollection.UpdateState') < 50 then //50% of adding new weather
      for K := 0 to GetTerrainDiv do
      begin
        C := KamRandom(Settings.MaxSpawnCount, 'Random Weather count');
        for I := 0 to C do
          SpawnNewWeather;
      end;
    end;
  end;

  //delete unused
  for I := Count - 1 downto 0 do
    if fList[I].Deleted then
      DeleteWeather(I);
    
end;

procedure TKMWeatherCollection.Paint(aLag : Single; aClipRect : TKMRect);
var I : Integer;
begin
  KMRectGrow(aClipRect, -2);
  for I := 0 to Count - 1 do
    fList[I].Paint(aLag, aClipRect);
end;

end.
