unit KM_ResPalettes;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_CommonClasses;


type
  TKMPal = (
    palmap,
    pal0, //pal1, pal2, pal3, pal4, pal5, unused since we change brightness with OpenGL overlay
    palset,
    palset2,
    palbw,
    pallin,
    pal2_mapgold,
    pal2_setup);

  // Individual palette info
  TKMPaletteSpec = class
  strict private
    fData: array [0..255, 1..3] of Byte;
  public
    procedure GenerateBW;
    procedure GenerateLinear;
    procedure LoadFromFile(const aFileName: UnicodeString);
    function Color32(aIdx: Byte): Cardinal;
  end;

  // All the palettes
  TKMResPalettes = class
  private
    fPalettes: array [TKMPal] of TKMPaletteSpec;
    function GetPalette(aIndex: TKMPal): TKMPaletteSpec;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadPalettes(const aPath: UnicodeString);
    procedure LoadDefaultPalette(const aPath: UnicodeString);
    property Palettes[aIndex: TKMPal]: TKMPaletteSpec read GetPalette; default;
    function DefaultPalette: TKMPaletteSpec; //Default palette for the game
  end;


implementation


const
  // Palette filenames, except pallin which is generated proceduraly
  PALETTE_FILENAME: array [TKMPal] of string = (
    'map.bbm',
    'pal0.bbm', //'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm', 'pal5.bbm', unused
    'setup.bbm',
    'setup2.bbm',
    '', //Black`n`white
    '', //Linear
    'mapgold.lbm',
    'setup.lbm');


{ TKMPaletteSpec }
function TKMPaletteSpec.Color32(aIdx: Byte): Cardinal;
begin
  //Index 0 means that pixel is transparent
  if aIdx = 0 then
    Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 { $00000000 }
  else
    Result := fData[aIdx,1] + fData[aIdx,2] shl 8 + fData[aIdx,3] shl 16 or $FF000000;
end;


//Black-and-white palette for fonts
procedure TKMPaletteSpec.GenerateBW;
begin
  FillChar(fData, SizeOf(fData), #255);
  fData[0, 1] := 0;
  fData[0, 2] := 0;
  fData[0, 3] := 0;
end;


//Gradient palette for missing files (used by pallin)
procedure TKMPaletteSpec.GenerateLinear;
var
  I: Byte;
begin
  for I := 0 to 255 do
  begin
    fData[I, 1] := I;
    fData[I, 2] := I;
    fData[I, 3] := I;
  end;
end;


procedure TKMPaletteSpec.LoadFromFile(const aFileName: UnicodeString);
var
  S: TKMemoryStream;
begin
  if FileExists(aFileName) then
  begin
    S := TKMemoryStreamBinary.Create;
    S.LoadFromFile(aFileName);
    S.Seek(48, soFromBeginning);
    S.Read(fData, SizeOf(fData)); // 768bytes
    S.Free;
  end else
    GenerateLinear;
end;


{ TKMResPalettes }
constructor TKMResPalettes.Create;
var
  I: TKMPal;
begin
  inherited;

  for I := Low(TKMPal) to High(TKMPal) do
    fPalettes[I] := TKMPaletteSpec.Create;
end;


destructor TKMResPalettes.Destroy;
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
    FreeAndNil(fPalettes[I]);

  inherited;
end;


function TKMResPalettes.DefaultPalette: TKMPaletteSpec;
begin
  // Default palette to use when generating full-color RGB textures
  Result := fPalettes[pal0];
end;


function TKMResPalettes.GetPalette(aIndex: TKMPal): TKMPaletteSpec;
begin
  Result := fPalettes[aIndex];
end;


procedure TKMResPalettes.LoadPalettes(const aPath: UnicodeString);
var
  I: TKMPal;
begin
  for I := Low(TKMPal) to High(TKMPal) do
  case I of
    palbw:   fPalettes[I].GenerateBW;
    pallin:  fPalettes[I].GenerateLinear;
  else
    fPalettes[I].LoadFromFile(aPath + PALETTE_FILENAME[I]);
  end;
end;


//Load only Default Palette
procedure TKMResPalettes.LoadDefaultPalette(const aPath: UnicodeString);
begin
  fPalettes[pal0].LoadFromFile(aPath + PALETTE_FILENAME[pal0]);
end;


end.
