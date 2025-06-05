unit KM_Particles;
{$I KaM_Remake.inc}
interface
  uses
    KM_Defaults,
    KM_Points,
    KM_WeatherTypes,
    KM_CommonTypes, KM_CommonClasses,
    KM_TerrainTypes,
    KM_ResTypes;

type
  TKMParticlesCollection = class
    private
      fCount : Integer;
      fCounter : Cardinal;
      fParticles : TKMParticles;
    protected
    public
      constructor Create;
      procedure Add(aStartPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX : TRXType; aAnimation : TKMAnimation; aInFront : Boolean = false);
      procedure AddSnow(aStartPos : TKMPointF);
      procedure AddRain(aStartPos : TKMPointF);
      procedure AddWhiteLightning(aStartPos : TKMPointF);
      procedure AddGoldLightning(aStartPos : TKMPointF);
      procedure AddWeatherParticle(aStartPos : TKMPointF; aClimate : TKMTerrainClimate);

      procedure Delete(aIndex : Integer);

      property Count : Integer read fCount;
      procedure UpdateState;
      procedure Paint(aTickLag : Single; aClipRect : TKMRect);
      procedure Save(SaveStream : TKMemoryStream);
      procedure Load(LoadStream : TKMemoryStream);
  end;

var gParticles : TKMParticlesCollection;

implementation
uses
  Math,
  KM_Game,
  KM_RenderPool;
constructor TKMParticlesCollection.Create;
begin
  Inherited;
  fCount := 0;
end;

procedure TKMParticlesCollection.Add(aStartPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX : TRXType; aAnimation: TKMAnimation; aInFront: Boolean = False);
begin
  if not KMInRect(aStartPos, gGame.GamePlayInterface.Viewport.GetClip) then
    Exit;

  Inc(fCounter);
  if gGame.Weather.Settings.DecParticles > 0 then
    if fCounter mod (10 - gGame.Weather.Settings.DecParticles) = 0 then
      Exit;

  if fCount >= length(fParticles) then
    SetLength(fParticles, fCount + 16);

  with fParticles[fCount] do
  begin
    Pos  := aStartPos;
    Speed := aSpeed;
    LifeTime := aLifeTime;
    Age := 0;
    Animation := aAnimation;
    Deleted := false;
    InFront := aInFront;
    RX := aRX
  end;
  Inc(fCount);
end;

procedure TKMParticlesCollection.AddSnow(aStartPos: TKMPointF);
begin
  Add(aStartPos, //startpos
      KMPointF(-0.025, 0.05), //speed/vector
      20, rxTrees, //lifeTime, rxType
      Anim(0, 0, [1025]),
      true
    );
end;

procedure TKMParticlesCollection.AddRain(aStartPos: TKMPointF);
begin
  Add(aStartPos, //startpos
      KMPointF(-0.1, 0.2), //speed/vector
      10, rxTrees, //lifeTime, rxType
      Anim(0, 0, [818]),
      true
    );
end;

procedure TKMParticlesCollection.AddWhiteLightning(aStartPos: TKMPointF);
begin
  Add(aStartPos, //startpos
      KMPointF(0, 0), //speed/vector
      2, rxTrees, //lifeTime, rxType
      Anim(0, 0, [1026 + Random(6)]),
      true
    );
end;

procedure TKMParticlesCollection.AddGoldLightning(aStartPos: TKMPointF);
begin
  Add(aStartPos, //startpos
      KMPointF(0, 0), //speed/vector
      2, rxTrees, //lifeTime, rxType
      Anim(0, 0, [1032 + Random(6)]),
      true
    );
end;

procedure TKMParticlesCollection.AddWeatherParticle(aStartPos: TKMPointF; aClimate: TKMTerrainClimate);
begin
  case aClimate of
    tcNone,
    tcWarm1,
    tcWarm2,
    tcWet1,
    tcWet2,
    tcNeutral : AddRain(aStartPos);
    tcCold1,
    tcCold2 : AddSnow(aStartPos);
    tcDry1,
    tcDry2: ;

  end;
end;

procedure TKMParticlesCollection.Delete(aIndex: Integer);
var I : Integer;
begin
  if not InRange(aIndex, 0, fCount - 1) then
  Exit;

  for I := aIndex to fCount - 2 do
    fParticles[I] := fParticles[I + 1];
  Dec(fCount);
end;

procedure TKMParticlesCollection.UpdateState;
var I : Integer;
begin
  for I := fCount - 1 downto 0 do
    with fParticles[I] do
    begin
      Inc(Age);
      if Age >= LifeTime then
      begin
        Deleted := true;
        Delete(I);
        Continue;
      end;
      Pos := KMPointFAdd(Pos, Speed);
    end;
end;

procedure TKMParticlesCollection.Paint(aTickLag : Single; aClipRect : TKMRect);
var I : Integer;
begin
  KMRectGrow(aClipRect, -2);
  for I := 0 to fCount - 1 do
  with fParticles[I] do
    if not Deleted then
      if KMInRect(Pos, aClipRect) then
      begin
        gRenderPool.AddAnimation(Pos, Animation, Age, 0, RX, false, false, 0, InFront);
      end;

end;

procedure TKMParticlesCollection.Save(SaveStream: TKMemoryStream);
begin
  Exit; //do not save particles data, they are spawning too often and are deleted after short time
  SaveStream.PlaceMarker('Particles');
  SaveStream.Write(fCount);
  fParticles.Save(SaveStream);
end;

procedure TKMParticlesCollection.Load(LoadStream: TKMemoryStream);
begin
  Exit; //do not save particles data, they are spawning too often and are deleted after short time
  LoadStream.CheckMarker('Particles');
  LoadStream.Read(fCount);
  fParticles.Load(LoadStream);
end;


end.
