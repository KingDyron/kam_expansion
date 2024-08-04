unit KM_Weather;
{$I KaM_Remake.inc}
interface
uses
  KM_Points,
  KM_CommonTypes,
  KM_CommonClasses,
  KM_ResTypes,
  KM_WeatherTypes;

type
  TKMWeather = class
    private
      fPos, fSpeed : TKMPointF;
      fLifeTime, fAge : Cardinal;
      fAnim : TKMAnimation;
      fRX : TRXType;
      fDeleted : Boolean;
      fType : TKMWeatherType;
    function GetRandomPos(aPos : TKMPointF; aRadius : Single) : TKMPointF;
    public
      constructor Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType; aAnimation : TKMAnimation);
      constructor Load(LoadStream : TKMemoryStream);

      property Deleted : Boolean read fDeleted;

      procedure Save(SaveStream : TKMemoryStream);
      procedure UpdateState;
      procedure Paint(aLag : Single; aClipRect : TKMRect);
  end;
implementation
uses KM_RenderPool,
      KM_Particles,
      KM_CommonUtils;

constructor TKMWeather.Create(aType : TKMWeatherType; aPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX: TRXType; aAnimation : TKMAnimation);
begin
  Inherited Create;

  fType := aType;
  fPos := aPos;
  fSpeed := aSpeed;
  fLifeTime := aLifeTime;
  fAnim := aAnimation;
  fRX := aRX;

  fDeleted := false;
  fAge := 0;
end;


constructor TKMWeather.Load(LoadStream: TKMemoryStream);
begin
  Inherited Create;

  LoadStream.Read(fType, SizeOf(fType));
  LoadStream.Read(fPos);
  LoadStream.Read(fSpeed);
  LoadStream.Read(fLifeTime);
  LoadStream.Read(fRX, SizeOf(fRX));
  LoadStream.Read(fAge);
  LoadStream.Read(fDeleted);
  fAnim.LoadFromStream(LoadStream);
end;

procedure TKMWeather.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fType, SizeOf(fType));;
  SaveStream.Write(fPos);
  SaveStream.Write(fSpeed);
  SaveStream.Write(fLifeTime);
  SaveStream.Write(fRX, SizeOf(fRX));
  SaveStream.Write(fAge);
  SaveStream.Write(fDeleted);

  fAnim.SaveToStream(SaveStream);
end;

function TKMWeather.GetRandomPos(aPos : TKMPointF; aRadius : Single) : TKMPointF;
begin
  Result.X := aPos.X + KaMRandomS2(aRadius, 'TKMWeather.GetRandomPos 1');
  Result.Y := aPos.Y + KaMRandomS2(aRadius, 'TKMWeather.GetRandomPos 2');
end;

procedure TKMWeather.UpdateState;
var I : Integer;
begin
  if fDeleted then
    Exit;
  case fType of
    wtNone : ;
    wtRain :
              for I := 1 to 3 do                  
              begin

                gParticles.Add(GetRandomPos(KMPointFAdd(fPos, KMPointF(-1, 0)), 0.5), //startpos
                                KMPointF(-0.1, 0.4), //speed/vector
                                10, rxTrees, //lifeTime, rxType
                                Anim(0, 0, [818]),
                                true
                              );

                gParticles.Add(GetRandomPos(KMPointFAdd(fPos, KMPointF(0, 0)), 0.5), //startpos
                                KMPointF(-0.1, 0.4), //speed/vector
                                10, rxTrees, //lifeTime, rxType
                                Anim(0, 0, [818]),
                                true
                              );
                gParticles.Add(GetRandomPos(KMPointFAdd(fPos, KMPointF(1.3, 0)), 0.5), //startpos
                                KMPointF(-0.1, 0.4), //speed/vector
                                10, rxTrees, //lifeTime, rxType
                                Anim(0, 0, [818]),
                                true
                              );

              end;

  end;
  fPos := KMPointFAdd(fPos, fSpeed);
  Inc(fAge);
  if fAge >= fLifeTime then
    fDeleted := true;
end;

procedure TKMWeather.Paint(aLag : Single; aClipRect : TKMRect);
begin
  if not fDeleted then
    if KMInRect(fPos, aClipRect) then
    begin
      gRenderPool.AddAnimation(fPos, fAnim, fAge, 0, fRX, false, false, 0, true);//clouds are always in front
    end;
end;

end.
