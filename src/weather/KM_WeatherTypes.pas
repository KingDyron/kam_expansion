unit KM_WeatherTypes;
{$I KaM_Remake.inc}
interface
  uses
      Types,
      KM_Points,
      KM_ResTypes,
      KM_CommonTypes, KM_CommonClasses;
type
  TKMParticle = packed record
    Speed,
    Pos : TKMPointF;
    LifeTime, Age : Cardinal;
    Animation : TKMAnimation;
    Deleted, InFront : Boolean;
    RX : TRXType;
    procedure Save(SaveStream : TKMemoryStream);
    procedure Load(LoadStream : TKMemoryStream);
  end;

  PKMParticle = ^TKMParticle;

  TKMParticles = array of TKMParticle;

  TKMParticlesHelper = record helper for TKMParticles
    function Count : Integer;
    procedure Save(SaveStream : TKMemoryStream);
    procedure Load(LoadStream : TKMemoryStream);
  end;


  TKMWeatherType = (wtNone, wtCloudy1, wtCloudy2, wtRain, wtStorm, wtSnow, wtSnowyStorm, wtSandStorm1, wtSandStorm2);

  TKMWeatherState = (wsStart, wsLoop, wsEnd);

implementation

procedure TKMParticle.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(Pos);
  SaveStream.Write(Speed);
  SaveStream.Write(LifeTime);
  SaveStream.Write(Age);
  SaveStream.Write(Deleted);
  SaveStream.Write(InFront);
  SaveStream.Write(RX, SizeOf(RX));
  Animation.SaveToStream(SaveStream);
end;

procedure TKMParticle.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(Pos);
  LoadStream.Read(Speed);
  LoadStream.Read(LifeTime);
  LoadStream.Read(Age);
  LoadStream.Read(Deleted);
  LoadStream.Read(InFront);
  LoadStream.Read(RX, SizeOf(RX));
  Animation.LoadFromStream(LoadStream);
end;

function TKMParticlesHelper.Count: Integer;
begin
  Result := length(Self);
end;

procedure TKMParticlesHelper.Save(SaveStream: TKMemoryStream);
var I, newCount : Integer;
begin
  newCount := length(self);
  SaveStream.Write(newCount);

  for I := 0 to newCount - 1 do
    self[I].Save(SaveStream);
end;

procedure TKMParticlesHelper.Load(LoadStream: TKMemoryStream);
var I, newCount : Integer;
begin
  LoadStream.Read(newCount);
  SetLength(self, newCount);
  for I := 0 to newCount - 1 do
    self[I].Load(LoadStream);
end;


end.
