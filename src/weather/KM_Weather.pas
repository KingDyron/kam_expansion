unit KM_Weather;
{$I KaM_Remake.inc}
interface
uses
  KM_Points,
  KM_CommonTypes,
  KM_CommonClasses,
  KM_ResTypes,
  KM_TerrainTypes,
  KM_WeatherTypes;

type

  TKMWeather = class
    private
      fPos, fSpeed : TKMPointF;
      fLifeTime, fAge : Cardinal;
      fAnims :array[TKMWeatherState] of TKMAnimation;
      fRX : TRXType;
      fDeleted : Boolean;
      fType : TKMWeatherType;
      fState : TKMWeatherState;

      fStyle, fMaxStyles : Byte;// how many clouds are in the same entity
      fClouds : array of TKMPointF;//make more clouds at the same time to add variety

      fCurrentClimate : TKMTerrainClimate;

      function GetRandomPos(aPos : TKMPointF; aRadius : Single) : TKMPointF;
      function CanAnimBeStopped : Boolean;
      procedure SetStyle(aValue : Byte);
      procedure AddCloud(X, Y : Single);
      procedure ExtendAnimations(aCount : Integer);
      procedure SetClimate;
      procedure UpdateCloud(aPos : TKMPointF); virtual;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
      constructor Load(aType : TKMWeatherType; LoadStream : TKMemoryStream);

      property Deleted : Boolean read fDeleted;
      property WeatherType : TKMWeatherType read fType;
      property Style : Byte read fStyle write SetStyle;

      procedure Save(SaveStream : TKMemoryStream);
      procedure UpdateState; virtual;
      procedure Paint(aLag : Single; aClipRect : TKMRect);
      procedure PaintClouds;virtual;

  end;

  TKMWeatherCloudy = class(TKMWeather)
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherCloudy2 = class(TKMWeather)
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherRain = class(TKMWeather)
    private
      procedure UpdateCloud(aPos : TKMPointF); override;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherStorm = class(TKMWeather)
    private
      procedure UpdateCloud(aPos : TKMPointF); override;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherSnow = class(TKMWeather)
    private
      procedure UpdateCloud(aPos : TKMPointF); override;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherSnowStorm = class(TKMWeather)
    private
      procedure UpdateCloud(aPos : TKMPointF); override;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherSandStorm1 = class(TKMWeather)
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherSandStorm2 = class(TKMWeather)
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;

  TKMWeatherTornado = class(TKMWeather)
    private
      procedure UpdateCloud(aPos : TKMPointF); override;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
  end;


implementation
uses
      Math,
      KM_Game,
      KM_GameParams,
      KM_RenderPool,
      KM_Particles,
      KM_CommonUtils, KM_CommonHelpers,
      KM_Terrain;

constructor TKMWeather.Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType);
begin
  Inherited Create;

  fType := aType;
  fPos := aPos;
  fSpeed := aSpeed;
  fLifeTime := aLifeTime;
  fRX := aRX;
  fDeleted := false;
  fAge := 0;
  fState := wsStart;

  fStyle := 0;
  fMaxStyles := 0;

  fCurrentClimate := gTerrain.FindBestClimatType(KMPoint(aPos));
end;


constructor TKMWeather.Load(aType : TKMWeatherType; LoadStream: TKMemoryStream);
var st : TKMWeatherState;
var nCount, I : Integer;
begin
  Inherited Create;

  //LoadStream.Read(fType, SizeOf(fType)); //skip it here
  fType := aType;
  LoadStream.Read(fPos);
  LoadStream.Read(fSpeed);
  LoadStream.Read(fLifeTime);
  LoadStream.Read(fRX, SizeOf(fRX));
  LoadStream.Read(fState, SizeOf(fState));
  LoadStream.Read(fAge);
  LoadStream.Read(fDeleted);
  LoadStream.Read(fStyle);
  LoadStream.Read(fMaxStyles);
  LoadStream.Read(fCurrentClimate, SizeOf(fCurrentClimate));

  LoadStream.Read(nCount);
  Setlength(fClouds, nCount);
  for I := 0 to nCount - 1 do
    LoadStream.Read(fClouds[I]);

  for st := Low(TKMWeatherState) to High(TKMWeatherState) do
    fAnims[st].LoadFromStream(LoadStream);
end;

procedure TKMWeather.Save(SaveStream: TKMemoryStream);
var st : TKMWeatherState;
var nCount, I : Integer;
begin
  //SaveStream.Write(fType, SizeOf(fType)); //skip it here
  SaveStream.Write(fPos);
  SaveStream.Write(fSpeed);
  SaveStream.Write(fLifeTime);
  SaveStream.Write(fRX, SizeOf(fRX));
  SaveStream.Write(fState, SizeOf(fState));
  SaveStream.Write(fAge);
  SaveStream.Write(fDeleted);
  SaveStream.Write(fStyle);
  SaveStream.Write(fMaxStyles);
  SaveStream.Write(fCurrentClimate, SizeOf(fCurrentClimate));

  nCount := Length(fClouds);
  SaveStream.Write(nCount);
  for I := 0 to nCount - 1 do
    SaveStream.Write(fClouds[I]);


  for st := Low(TKMWeatherState) to High(TKMWeatherState) do
    fAnims[st].SaveToStream(SaveStream);
end;

function TKMWeather.GetRandomPos(aPos : TKMPointF; aRadius : Single) : TKMPointF;

  function RandomBothRange : Single;
  begin
    Result := (400 - Random(801)) / 400 * aRadius;
  end;
begin
  //use default random here. Particles are not saved so we don't need KaMRandom
  Result.X := aPos.X + RandomBothRange;
  Result.Y := aPos.Y + RandomBothRange;

  {
  Result.X := aPos.X + KaMRandomS2(aRadius, 'TKMWeather.GetRandomPos 1');
  Result.Y := aPos.Y + KaMRandomS2(aRadius, 'TKMWeather.GetRandomPos 2');
  }
end;

function TKMWeather.CanAnimBeStopped: Boolean;
begin
  Result := fAge mod fAnims[fState].Count = 0;
end;

procedure TKMWeather.SetStyle(aValue: Byte);
begin
  fStyle := EnsureRange(aValue, 0, fMaxStyles - 1);
end;

procedure TKMWeather.AddCloud(X: Single; Y: Single);
begin
  SetLength(fClouds, length(fClouds) + 1);
  fClouds[high(fClouds)] := KMPointF(X, Y);
end;

procedure TKMWeather.ExtendAnimations(aCount: Integer);
var st : TKMWeatherState;
begin
  for st := Low(TKMWeatherState) to High(TKMWeatherState) do
    fAnims[st].Extend(aCount);
end;

procedure TKMWeather.SetClimate;
var oldC : TKMTerrainClimate;
begin
  oldC := fCurrentClimate;

  fCurrentClimate := gTerrain.FindBestClimatType(KMPoint(fPos));
  //let it disappear earlier when it passes different climates
  if fCurrentClimate <> oldC then
    Inc(fAge, 100);
end;

procedure TKMWeather.UpdateCloud(aPos: TKMPointF);
begin
  //do nothing in default
end;

procedure TKMWeather.UpdateState;
var I : Integer;
begin
  if fDeleted then
    Exit;
  fPos := KMPointFAdd(fPos, fSpeed);
  Inc(fAge);
  case fState of
    wsStart: If CanAnimBeStopped then
             begin
                fState := wsLoop;
                fAge := 0;
             end;

    wsLoop:   If (fAge >= fLifeTime) and CanAnimBeStopped then
              begin
                fState := wsEnd;
                fAge := 0;
              end;

    wsEnd:  If CanAnimBeStopped then
            begin
              fState := wsEnd;
              fDeleted := true;
            end;
  end;
  if fState = wsLoop then
  begin
    UpdateCloud(fPos);
    for I := 0 to High(fClouds) do
      UpdateCloud(fPos + fClouds[I]);

    if (fAge + 1) mod 50 = 0 then //refresh climate every 5 secs
      SetClimate;
  end;
  
end;


procedure TKMWeather.Paint(aLag : Single; aClipRect : TKMRect);
begin
  if not fDeleted then
      PaintClouds;
end;

procedure TKMWeather.PaintClouds;
var I : Integer;
begin
  for I := 1 to length(fClouds) do
    if fState = wsLoop then
      gRenderPool.AddAnimation(fPos + fClouds[I - 1], fAnims[fState], fAge div (I + 1), 0, fRX, false, false, 0, true)
    else
      gRenderPool.AddAnimation(fPos + fClouds[I - 1], fAnims[fState], fAge, 0, fRX, false, false, 0, true);

  gRenderPool.AddAnimation(fPos, fAnims[fState], fAge, 0, fRX, false, false, 0, true);
end;

constructor TKMWeatherCloudy.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 916, 5);
  fAnims[wsLoop].Create(0, 0, 921, 13);
  fAnims[wsEnd].Create(0, 0, 916, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(2, 1);
          AddCloud(3, 2);
          AddCloud(1, -3);
        end;
    3: AddCloud(1, 3);
  end;
end;

constructor TKMWeatherCloudy2.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);
  {fMaxStyles := 3;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');}

  fAnims[wsStart].Create(0, 0, 934, 5);
  fAnims[wsLoop].Create(0, 0, 939, 14);
  fAnims[wsEnd].Create(0, 0, 934, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(2, 5);
          AddCloud(4, 2);
          AddCloud(3, -3);
        end;
    3: begin
          AddCloud(2, 1);
          AddCloud(3, 2);
          AddCloud(1, -3);
          AddCloud(4, 1);
        end;
  end;
end;

constructor TKMWeatherRain.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 953, 5);
  fAnims[wsLoop].Create(0, 0, 958, 13);
  fAnims[wsEnd].Create(0, 0, 953, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(2, 0);
          AddCloud(3, 3);
          AddCloud(2, -3);
        end;
    3: begin
          AddCloud(2, 1);
          AddCloud(3, 2);
          AddCloud(1, -3);
          AddCloud(4, 1);
        end;
  end;
end;

procedure TKMWeatherRain.UpdateCloud(aPos: TKMPointF);
var X, Y : Integer;
begin
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(-1, 0), 0.5), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(0, 0), 0.5), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(1.3, 0), 0.5), fCurrentClimate);


  if gGameParams.MBD.IsRealism then
    if fAge mod 200 = 0 then
      for X := Max(aPos.RX - 4, 1) to Min(aPos.RX + 4, gTerrain.MapX - 1) do
      for Y := Max(aPos.RY - 4, 1) to Min(aPos.RY + 4, gTerrain.MapY - 1) do
        gTerrain.IncFieldAge(KMPoint(X, Y));

end;

constructor TKMWeatherStorm.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 971, 5);
  fAnims[wsLoop].Create(0, 0, 976, 13);
  fAnims[wsEnd].Create(0, 0, 971, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(1, 3);
          AddCloud(2, 4);
          AddCloud(4, -3);
        end;
    3: begin
          AddCloud(2, 1);
          AddCloud(3, 2);
          AddCloud(1, -3);
          AddCloud(4, 1);
        end;
  end;
end;

procedure TKMWeatherStorm.UpdateCloud(aPos: TKMPointF);
var I : Integer;
var X, Y : Integer;
begin
  for I := 1 to 3 do
  begin
    gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(-1, -0.5), 0.5), fCurrentClimate);
    gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(0, 0), 0.5), fCurrentClimate);
    gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(1.3, 0), 0.5), fCurrentClimate);
  end;
  if Random(200) <= 5 then
    gParticles.AddWhiteLightning(GetRandomPos(aPos, 2));


  if gGameParams.MBD.IsRealism then
    if fAge mod 100 = 0 then
      for X := Max(aPos.RX - 4, 1) to Min(aPos.RX + 4, gTerrain.MapX - 1) do
      for Y := Max(aPos.RY - 4, 1) to Min(aPos.RY + 4, gTerrain.MapY - 1) do
        gTerrain.IncFieldAge(KMPoint(X, Y));
end;

constructor TKMWeatherSnow.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 989, 5);
  fAnims[wsLoop].Create(0, 0, 994, 13);
  fAnims[wsEnd].Create(0, 0, 989, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(2, 1);
          AddCloud(3, 2);
          AddCloud(1, -3);
        end;
    3: begin
          AddCloud(2, 1);
          AddCloud(-3, 1);
          AddCloud(1, -3);
          AddCloud(3, 4);
        end;
  end;
end;

procedure TKMWeatherSnow.UpdateCloud(aPos: TKMPointF);
begin
  if fAge mod 3 <> 0 then
    Exit;

  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(-1, -0.8), 1), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(0, 0), 1), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(1.3, 0.5), 1), fCurrentClimate);
end;

constructor TKMWeatherSnowStorm.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 1007, 5);
  fAnims[wsLoop].Create(0, 0, 1012, 13);
  fAnims[wsEnd].Create(0, 0, 1007, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(1, -2);
          AddCloud(3, -2);
          AddCloud(-1, -3);
        end;
    3: begin
          AddCloud(2, -1);
          AddCloud(-3, 2);
          AddCloud(1, 3);
          AddCloud(-4, 3);
        end;
  end;
end;

procedure TKMWeatherSnowStorm.UpdateCloud(aPos: TKMPointF);
begin
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(-1, -0.8), 1), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(0, 0), 1), fCurrentClimate);
  gParticles.AddWeatherParticle(GetRandomPos(aPos + KMPointF(1.3, 0.8), 1), fCurrentClimate);

  if Random(200) <= 5 then
    gParticles.AddGoldLightning(GetRandomPos(aPos, 2));
end;


constructor TKMWeatherSandStorm1.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 1038, 5);
  fAnims[wsLoop].Create(0, 0, 1043, 13);
  fAnims[wsEnd].Create(0, 0, 1038, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-3, -2);
    2:  begin
          AddCloud(2, 0);
          AddCloud(3, 3);
          AddCloud(0, -4);
        end;
    3: begin
          AddCloud(3, 1);
          AddCloud(3, 5);
          AddCloud(5, -3);
          AddCloud(6, 3);
        end;
  end;
end;

constructor TKMWeatherSandStorm2.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 1056, 5);
  fAnims[wsLoop].Create(0, 0, 1061, 13);
  fAnims[wsEnd].Create(0, 0, 1056, 5, 0, true);
  ExtendAnimations(1);

  fMaxStyles := 4;
  Style := KaMRandom(fMaxStyles, 'TKMWeatherCloudy.Create');
  //add new clouds
  case Style of
    0: ;
    1: AddCloud(-2, -3);
    2:  begin
          AddCloud(1, 2);
          AddCloud(-3, -2);
          AddCloud(1, -3);
        end;
    3: begin
          AddCloud(4, -1);
          AddCloud(3, 2);
          AddCloud(1, -3);
          AddCloud(4, 1);
        end;
  end;
end;

constructor TKMWeatherTornado.Create(aType: TKMWeatherType; aPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX: TRXType);
begin
  aLifeTime := aLifeTime * 2;
  aSpeed.X := aSpeed.X / 2;
  aSpeed.Y := aSpeed.Y / 2;
  Inherited Create(aType, aPos, aSpeed, aLifeTime, aRX);

  fAnims[wsStart].Create(0, 0, 1413, 5);
  fAnims[wsLoop].Create(0, 0, 1418, 72);
  fAnims[wsEnd].Create(0, 0, 1413, 5, 0, true);
  fAnims[wsStart].Extend(1);
  fAnims[wsEnd].Extend(1);
  fMaxStyles := 1;
end;

procedure TKMWeatherTornado.UpdateCloud(aPos : TKMPointF);
var speed : TKMPointF;
begin
  Inherited;

  If fAge mod 100 = 0 then
  begin
    speed.X := KaMRandomS2(gGame.Weather.Settings.MaxCloudSpeed / 2, 'TKMWeatherTornado.SpawnNewWeather 1');
    speed.Y := KaMRandomS2(gGame.Weather.Settings.MaxCloudSpeed / 2, 'TKMWeatherTornado.SpawnNewWeather 2');
    gGame.Weather.AddItem(wtStorm, aPos + KMPointF(0, -4), speed, 100 + KaMRandom(100 + gGame.Weather.Settings.MaxLifeTime, 'TKMWeatherTornado'), rxTrees);
  end;

end;

end.
