unit KM_Minimap;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMinimap = class abstract
  private
    fSepia: Boolean; //Less saturated display for menu

    // Event handlers for MinimapView
    fOnUpdateTextureSubs: array of TEvent; // Use plain arrays, since Generics are not good on Lazarus
    fOnResizeSubs: array of TEvent;

    procedure ApplySepia;
    procedure UpdateTexture;
  protected
    //We need to store map properties locally since Minimaps come from various
    //sources which do not have Terrain in them (TMissionParserPreview, Stream)
    fMapY: Word;
    fMapX: Word;
    fBase: TKMCardinalArray; //Base terrain layer
    procedure Resize(aX, aY: Word);
    procedure DoUpdate(aRevealAll: Boolean); virtual; abstract;
  public
    HandColors: array [0..MAX_HANDS-1] of Cardinal;
    HandLocs: array [0..MAX_HANDS-1] of TKMPoint;
    HandShow: array [0..MAX_HANDS-1] of Boolean;
    HandTeam: array [0..MAX_HANDS-1] of ShortInt;

    constructor Create(aSepia: Boolean);

    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    property Base: TKMCardinalArray read fBase;

    procedure SubOnUpdateTexture(aOnUpdateTexture: TEvent);
    procedure SubOnResize(aOnResize: TEvent);

    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);

    procedure SaveAsScreenShot(aMapName : String);

    procedure Update(aRevealAll: Boolean = False);
  end;


implementation
uses
  SysUtils, Math, Graphics,
  KromUtils;


{ TKMMinimap }
constructor TKMMinimap.Create(aSepia: Boolean);
begin
  inherited Create;

  fSepia := aSepia;
end;


procedure TKMMinimap.SubOnUpdateTexture(aOnUpdateTexture: TEvent);
begin
  // Happens quite rare, so we don't care much
  SetLength(fOnUpdateTextureSubs, Length(fOnUpdateTextureSubs) + 1);
  fOnUpdateTextureSubs[Length(fOnUpdateTextureSubs) - 1] := aOnUpdateTexture;
end;


procedure TKMMinimap.SubOnResize(aOnResize: TEvent);
begin
  // Happens quite rare, so we don't care much
  SetLength(fOnResizeSubs, Length(fOnResizeSubs) + 1);
  fOnResizeSubs[Length(fOnResizeSubs) - 1] := aOnResize;
end;


procedure TKMMinimap.Resize(aX, aY: Word);
var
  I: Integer;
begin
  fMapX := aX;
  fMapY := aY;
  SetLength(fBase, fMapX * fMapY);

  for I := 0 to Length(fOnResizeSubs) - 1 do
    if Assigned(fOnResizeSubs[I]) then
      fOnResizeSubs[I];
end;


//Sepia method taken from:
//http://www.techrepublic.com/blog/howdoi/how-do-i-convert-images-to-grayscale-and-sepia-tone-using-c/120
procedure TKMMinimap.ApplySepia;
const
  SEPIA_VAL = 0.4;
var
  I: Integer;
  R, G, B, R2, G2, B2: Byte;
begin
  for I := 0 to fMapX * fMapY - 1 do
  begin
    //We split color to RGB values
    R := fBase[I] and $FF;
    G := fBase[I] shr 8 and $FF;
    B := fBase[I] shr 16 and $FF;

    //Apply sepia coefficients and merge back with SEPIA_VAL factor
    R2 := Min(Round(0.393 * R + 0.769 * G + 0.189 * B), 255);
    R2 := Mix(R2, R, SEPIA_VAL);

    G2 := Min(Round(0.349 * R + 0.686 * G + 0.168 * B), 255);
    G2 := Mix(G2, G, SEPIA_VAL);

    B2 := Min(Round(0.272 * R + 0.534 * G + 0.131 * B), 255);
    B2 := Mix(B2, B, SEPIA_VAL);

    fBase[I] := (R2 + G2 shl 8 + B2 shl 16) or $FF000000;
  end;
end;


procedure TKMMinimap.Update(aRevealAll: Boolean = False);
begin
  if SKIP_RENDER then Exit;

  // No need to update if we did not initialize map sizes yet
  // F.e. when Cinematic starts in OnMissionStart script procedure in the replay
  if fMapX*fMapY = 0 then Exit;

  DoUpdate(aRevealAll);

  if fSepia then ApplySepia;

  UpdateTexture;
end;


procedure TKMMinimap.UpdateTexture;
var
  I: Integer;
begin
  for I := 0 to Length(fOnUpdateTextureSubs) - 1 do
    if Assigned(fOnUpdateTextureSubs[I]) then
      fOnUpdateTextureSubs[I];
end;


procedure TKMMinimap.SaveToStream(SaveStream: TKMemoryStream);
var
  I: Integer;
  L: Cardinal;
begin
  SaveStream.PlaceMarker('Minimap');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
  for I := 0 to MAX_HANDS - 1 do
  begin
    SaveStream.Write(HandColors[I]);
    SaveStream.Write(HandLocs[I]);
    SaveStream.Write(HandShow[I]);
  end;
end;


procedure TKMMinimap.LoadFromStream(LoadStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  LoadStream.CheckMarker('Minimap');

  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(L);
  SetLength(fBase, L);
  if L > 0 then
    LoadStream.Read(fBase[0], L * SizeOf(Cardinal));
  for I := 0 to MAX_HANDS - 1 do
  begin
    LoadStream.Read(HandColors[I]);
    LoadStream.Read(HandLocs[I]);
    LoadStream.Read(HandShow[I]);
  end;

  //Resize will update UV bounds. Resizing fBase is ok since the size does not changes
  Resize(fMapX, fMapY);

  if fMapX * fMapY = 0 then Exit;

  if fSepia then ApplySepia;

  UpdateTexture;
end;

procedure TKMMinimap.SaveAsScreenShot(aMapName: string);
var I : Integer;
  bmp : TBitmap;
  fileName, dateStr : String;

begin
  bmp := TBitmap.Create;
  bmp.SetSize(fMapX, fMapY);
  for I := 0 to High(fBase) do
    bmp.Canvas.Pixels[I mod fMapX, I div fMapX] := fBase[I] and $FFFFFF;

  DateTimeToString(dateStr, 'yyyy-mm-dd hh-nn-ss', Now); //2007-12-23 15-24-33
  fileName := ExeDir + 'screenshots\' + aMapName + '  ' + dateStr + '.png';
  bmp.SaveToFile(fileName);
  bmp.Free;
end;

end.
