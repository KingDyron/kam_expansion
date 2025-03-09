unit KM_MinimapCartographer;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_CommonClasses, KM_CommonTypes,
  KM_Points;

type
  //Intermediary class between TTerrain/Players and UI
  TKMMinimapCartographer = class
  private
  protected
    //We need to store map properties locally since Minimaps come from various
    //sources which do not have Terrain in them (TMissionParserPreview, Stream)
    fMapY: Word;
    fMapX: Word;
    fBase: TKMCardinalArray;
  public
    constructor Create(aMapX, aMapY : Word);

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property Base: TKMCardinalArray read fBase;

    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);

    procedure RevealCircle(aLoc : TKMPoint; aRadius : Word);
  end;


implementation
uses
  SysUtils, Math,
  KM_Resource,
  KM_Terrain;



{ TKMMinimap }
constructor TKMMinimapCartographer.Create(aMapX, aMapY : Word);
begin
  inherited Create;
  fMapX := aMapX;
  fMapY := aMapY;
  SetLength(fBase, fMapX * fMapY);
end;

procedure TKMMinimapCartographer.RevealCircle(aLoc : TKMPoint; aRadius : Word);
var X, Y, tileID : integer;
  rec : TKMRect;
  R, G, B : Byte;
  col: TKMColor3b;
  light: Smallint;
begin
  rec := KMRectGrow(aLoc, aRadius);
  rec.FitInMap(fMapX, fMapY);
  for X := rec.Left - 1 to rec.Right - 1 do
    for Y := rec.Top - 1 to rec.Bottom - 1 do
    If Sqr(X - aLoc.X) + Sqr(Y - aLoc.Y) <= aRadius * aRadius then
    begin
      tileID := Y * fMapX + X;
      light := Round(gTerrain.LandExt[Y + 1,X + 1].RenderLight * 64);
      col :=  gRes.Tileset[gTerrain.Land^[Y+1, X+1].BaseLayer.Terrain].MainColor;
      col.R := EnsureRange(col.R + light, 0, 255);
      col.G := EnsureRange(col.G + light, 0, 255);
      col.B := EnsureRange(col.B + light, 0, 255);
      fBase[tileID] := col.ToCardinal or $FF000000;
    end;

end;


procedure TKMMinimapCartographer.SaveToStream(SaveStream: TKMemoryStream);
var
  I: Integer;
  L: Cardinal;
begin
  SaveStream.PlaceMarker('MinimapCartographer');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
end;


procedure TKMMinimapCartographer.LoadFromStream(LoadStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  LoadStream.CheckMarker('MinimapCartographer');

  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(L);
  SetLength(fBase, L);
  if L > 0 then
    LoadStream.Read(fBase[0], L * SizeOf(Cardinal));

  if fMapX * fMapY = 0 then Exit;
end;

end.
