unit KM_Particles;
{$I KaM_Remake.inc}
interface
  uses
    KM_Defaults,
    KM_Points,
    KM_WeatherTypes,
    KM_CommonTypes, KM_CommonClasses,
    KM_ResTypes;

type
  TKMParticlesCollection = class
    private
      fCount : Integer;
      fParticles : TKMParticles;
    protected
    public
      constructor Create;
      procedure Add(aStartPos, aSpeed : TKMPointF; aLifeTime : Cardinal; aRX : TRXType; aAnimation : TKMAnimation; aInFront : Boolean = false);
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
  KM_RenderPool;
constructor TKMParticlesCollection.Create;
begin
  Inherited;
  fCount := 0;
end;

procedure TKMParticlesCollection.Add(aStartPos: TKMPointF; aSpeed: TKMPointF; aLifeTime: Cardinal; aRX : TRXType; aAnimation: TKMAnimation; aInFront: Boolean = False);
begin

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
  SaveStream.PlaceMarker('Particles');
  SaveStream.Write(fCount);
  fParticles.Save(SaveStream);
end;

procedure TKMParticlesCollection.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('Particles');
  LoadStream.Read(fCount);
  fParticles.Load(LoadStream);
end;


end.
