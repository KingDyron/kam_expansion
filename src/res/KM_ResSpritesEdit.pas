unit KM_ResSpritesEdit;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Dialogs, Graphics, Math, Types, StrUtils, SysUtils,
  KM_Defaults, KM_ResTypes, KM_ResHouses, KM_ResPalettes, KM_ResSprites,
  KM_ResTileset, KM_CommonTypes
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};

type
  TInterpExportType = (ietNormal, ietBase, ietBaseAlpha, ietBaseBlack, ietBaseWhite, ietShadows, ietTeamMask);

  // Class with additional editing properties
  TKMSpritePackEdit = class(TKMSpritePack)
  private
    fPalettes: TKMResPalettes;
    function GetLoaded: Boolean;
    procedure WriteRXZHeader(aStream: TStream; aFormat: TKMRXXFormat);
  protected
    procedure Allocate(aCount: Integer); override; //Allocate space for data that is being loaded
    procedure Expand;
  public
    constructor Create(aRT: TRXType; aPalettes: TKMResPalettes);

    property IsLoaded: Boolean read GetLoaded;
    procedure AdjoinHouseMasks(aResHouses: TKMResHouses);
    procedure GrowHouseMasks(aResHouses: TKMResHouses);
    procedure SoftWater(aTileset: TKMResTileset);
    procedure Delete(aIndex: Integer; aCount : integer);
    procedure LoadFromRXFile(const aFileName: string);
    procedure SaveToRXXFile(const aFileName: string; aFormat: TKMRXXFormat);
    procedure SaveToRXAFile(const aFileName: string; aFormat: TKMRXXFormat);
    function TrimSprites: Cardinal; //For debug
    procedure ClearTemp; override;
    procedure GetImageToBitmap(aIndex: Integer; aBmp, aMask: TBitmap);

    function ExportImageForInterp(const aFile: string; aIndex, aIndexBase: Integer; aBaseMoveX, aBaseMoveY: Integer; aExportType: TInterpExportType; aCanvasSize: Integer; aSimpleShadows: Boolean; aBkgColour: Cardinal): Boolean;
    function ExportPixelsForInterp(var pngData: TKMCardinalArray; aIndex: Integer; aMoveX, aMoveY: Integer; aExportType: TInterpExportType; aCanvasSize: Integer; aSimpleShadows: Boolean; aBkgColour: Cardinal): Boolean;
  end;


implementation
uses
  KM_SoftShadows,
  KM_IoPNG,
  KM_RenderTypes,
  KM_CommonClasses,
  KM_Utils;


const
  RX_GUIMAIN_PAL: array [1 .. 40] of TKMPal = (
    pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
    pal2_setup,   palset2,     palset2,     palset2,     palmap,
    palmap,      palmap,      palmap,      palmap,      palmap,
    palmap,      pal2_setup,   pal2_setup,   pal2_setup,   pal2_mapgold,
    pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_mapgold, pal2_setup,
    palmap,      palmap,      palmap,      palmap,      palmap,
    pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,   pal2_setup,
    pal2_setup,   pal2_setup,   pallin,      pallin,      pallin
  );

  // I couldn't find matching palettes for the 17th and 18th entries
  RX6_PAL: array [1 .. 20] of TKMPal = (
    palset,  palset,  palset,  palset,  palset,
    palset,  palset2, palset2, palset2, palmap,
    palmap,  palmap,  palmap,  palmap,  palmap,
    palmap,  pallin,  pallin,  pallin,  pallin
  );


{ TKMSpritePackEdit }
// We need to have access to the palettes to properly Expand RX files
constructor TKMSpritePackEdit.Create(aRT: TRXType; aPalettes: TKMResPalettes);
begin
  inherited Create(aRT);

  fPalettes := aPalettes;
end;


procedure TKMSpritePackEdit.Delete(aIndex: Integer; aCount : integer);
var I : Integer;
begin
  Assert(InRange(aIndex, 1, fRXData.Count));
  Assert(InRange(aIndex + aCount - 1, 1, fRXData.Count));

  for I := 0 to aCount - 1 do
    fRXData.Flag[aIndex + I] := 0;
end;


procedure TKMSpritePackEdit.Allocate(aCount: Integer);
begin
  inherited;
  fRXData.Count := aCount;

  SetLength(fRXData.Data, aCount);
end;


// Convert paletted data into RGBA and select Team color layer from it
procedure TKMSpritePackEdit.Expand;
  function HouseWIP(aID: Integer): TKMPaletteSpec;
  const
    // These are sprites with house building steps
    HOUSES_BUILDING_STAGES: array [0..55] of Word = (
      3,4,25,43,44,116,118,119,120,121,123,126,127,136,137,140,141,144,145,148,149,213,214,237,238,241,242,243,246,
      247,252,253,257,258,275,276,336,338,360,361,365,366,370,371,380,381,399,400,665,666,670,671,1658,1660,1682,1684);
  var
    I: Byte;
  begin
    Result := fPalettes.DefaultPalette;

    for I := 0 to High(HOUSES_BUILDING_STAGES) do
    if aID = HOUSES_BUILDING_STAGES[I] then
      Exit(fPalettes[pallin]);
  end;
var
  H: Integer;
  K, I: Integer;
  Palette: TKMPaletteSpec;
  L: Byte;
  Pixel: Integer;
begin
  with fRXData do
  for H := 1 to Count do
  begin
    //Choose proper palette
    case fRT of
      rxHouses:   Palette := HouseWIP(H);
      rxGuiMain:  Palette := fPalettes[RX_GUIMAIN_PAL[H]];
    else
      Palette := fPalettes.DefaultPalette;
    end;

    if Flag[H] = 1 then
    begin
      SetLength(RGBA[H], Size[H].X * Size[H].Y);
      SetLength(Mask[H], Size[H].X * Size[H].Y);

      for I := 0 to Size[H].Y - 1 do
      for K := 0 to Size[H].X - 1 do
      begin
        Pixel := I * Size[H].X + K;
        L := Data[H, Pixel]; //0..255

        //Flagcolors in KaM are 24..30
        //We decode them according to visualization pipeline to greyscale
        //and make a color transparency mask
        if RX_INFO[fRT].TeamColors and (L in [24..30])
        and (Palette <> fPalettes[pallin])
        and ((fRT <> rxHouses) or (H > 400))  //Skip the Inn Weapon Smithy and the rest
        and ((fRT <> rxGui) or InRange(H, 141, 154) or InRange(H, 521, 550)) then //Unit icons and scrolls
        begin
          //RGBA[H, Pixel] := cardinal(((L - 27) * 42 + 128) * 65793) OR $FF000000;
          //Use black and white background for more saturated colors
          if L < 27 then
            RGBA[H, Pixel] := FLAG_COLOR_DARK
          else
            RGBA[H, Pixel] := FLAG_COLOR_LITE;

          case L of
            24, 30: Mask[H, Pixel] := $60;   //38%
            25, 29: Mask[H, Pixel] := $90;   //56%
            26, 28: Mask[H, Pixel] := $C0;   //75%
            27:     Mask[H, Pixel] := $FF;   //100%
          end;
          HasMask[H] := True;
        end else
          RGBA[H, Pixel] := Palette.Color32(L);
      end;
    end;
  end;
end;


function TKMSpritePackEdit.ExportImageForInterp(const aFile: string; aIndex, aIndexBase: Integer; aBaseMoveX, aBaseMoveY: Integer; aExportType: TInterpExportType; aCanvasSize: Integer; aSimpleShadows: Boolean; aBkgColour: Cardinal): Boolean;
var
  pngData, pngDataBackground, pngAlpha, pngAlphaBackground: TKMCardinalArray;
  I: Integer;
  AlphaForeground, AlphaBackground: Byte;
  ResultBackground: Boolean;
begin
  if aIndex >= 0 then
    Result := ExportPixelsForInterp(pngData, aIndex, 0, 0, aExportType, aCanvasSize, aSimpleShadows, aBkgColour)
  else
  begin
    Result := True;
    SetLength(pngData, aCanvasSize*aCanvasSize);
  end;

  if aIndexBase >= 0 then
  begin
    ResultBackground := ExportPixelsForInterp(pngDataBackground, aIndexBase, aBaseMoveX, aBaseMoveY, aExportType, aCanvasSize, aSimpleShadows, aBkgColour);
    Result := Result or ResultBackground;

    //Since not all export formats contain the alpha value, we need to export the alpha so we can blend properly
    if aIndex >= 0 then
      ExportPixelsForInterp(pngAlpha, aIndex, 0, 0, ietNormal, aCanvasSize, aSimpleShadows, aBkgColour)
    else
      SetLength(pngAlpha, aCanvasSize*aCanvasSize);

    ExportPixelsForInterp(pngAlphaBackground, aIndexBase, aBaseMoveX, aBaseMoveY, ietNormal, aCanvasSize, aSimpleShadows, aBkgColour);

    //Place background pixels where it has higher alpha
    for I := Low(pngData) to High(pngData) do
    begin
      AlphaForeground := pngAlpha[I] shr 24;
      AlphaBackground := pngAlphaBackground[I] shr 24;
      if AlphaBackground > AlphaForeground then
        pngData[I] := pngDataBackground[I];
    end;
  end;

  SaveToPng(aCanvasSize, aCanvasSize, pngData, aFile);
end;


function TKMSpritePackEdit.ExportPixelsForInterp(var pngData: TKMCardinalArray; aIndex: Integer; aMoveX, aMoveY: Integer; aExportType: TInterpExportType; aCanvasSize: Integer; aSimpleShadows: Boolean; aBkgColour: Cardinal): Boolean;
var
  I, K, X, Y, dstX, dstY, CentreX, CentreY: Integer;
  M, A: Byte;
  C, RGB, R, G, B: Cardinal;
  TreatMask, isShadow: Boolean;
  srcWidth, srcHeight: Word;
  dstWidth, dstHeight: Word;
const
  CANVAS_Y_OFFSET = 14;
begin
  Result := False;

  CentreX := aCanvasSize div 2;
  CentreY := aCanvasSize div 2 + CANVAS_Y_OFFSET;

  srcWidth := fRXData.Size[aIndex].X;
  srcHeight := fRXData.Size[aIndex].Y;

  dstWidth := aCanvasSize;
  dstHeight := aCanvasSize;

  SetLength(pngData, dstWidth * dstHeight);

  {if EXPORT_SPRITES_NO_ALPHA then
    for I := Low(pngData) to High(pngData) do
      pngData[I] := $FFAF6B6B;}

  //Shadow export uses a black background
  if aExportType in [ietShadows, ietTeamMask, ietBaseAlpha] then
    for I := Low(pngData) to High(pngData) do
      pngData[I] := $FF000000;

  if (srcWidth > aCanvasSize) or (srcHeight > aCanvasSize) then
    Exit;

  //Export RGB values
  for I := 0 to fRXData.Size[aIndex].Y - 1 do
  for K := 0 to fRXData.Size[aIndex].X - 1 do
  begin
    dstY := I;
    dstX := K;
    if  (abs(fRXData.Pivot[aIndex].X) < CentreX)
    and (abs(fRXData.Pivot[aIndex].Y) < CentreY) then
    begin
      dstY := I + CentreY + fRXData.Pivot[aIndex].Y + aMoveY;
      dstX := K + CentreX + fRXData.Pivot[aIndex].X + aMoveX;
    end;

    TreatMask := fRXData.HasMask[aIndex] and (fRXData.Mask[aIndex, I*srcWidth + K] > 0);
    if (fRT = rxHouses)
      and ((aIndex < 680)
        or (aIndex = 1657)
        or (aIndex = 1659)
        or (aIndex = 1681)
        or (aIndex = 1683)
        or (aIndex > 2050)) then
      TreatMask := False;

    C := fRXData.RGBA[aIndex, I*srcWidth + K];
    RGB := C and $FFFFFF;
    A := (C shr 24);

    isShadow := (A > 0) and (A < $FF) and (not aSimpleShadows or (RGB = $0));

    if aExportType = ietShadows then
    begin
      //Find shadow pixels and make a greyscale mask
      if isShadow then
      begin
        pngData[dstY*dstWidth + dstX] := A or (A shl 8) or (A shl 16) or $FF000000;
        Result := True;
      end;

      Continue;
    end;

    if aExportType = ietTeamMask then
    begin
      if fRXData.HasMask[aIndex] then
        M := fRXData.Mask[aIndex, I*srcWidth + K]
      else
        M := 0;

      if M > 0 then
        Result := True;

      pngData[dstY*dstWidth + dstX] := M or (M shl 8) or (M shl 16) or $FF000000;

      Continue;
    end;

    if aExportType = ietBaseAlpha then
    begin
      if not isShadow then
      begin
        pngData[dstY*dstWidth + dstX] := A or (A shl 8) or (A shl 16) or $FF000000;
        Result := True;
      end;

      Continue;
    end;

    Result := True;

    if TreatMask and (aExportType = ietNormal) then
    begin
      M := fRXData.Mask[aIndex, I*srcWidth + K];

      //Replace background with corresponding brightness of Red
      if fRXData.RGBA[aIndex, I*srcWidth + K] = FLAG_COLOR_DARK then
        //Brightness < 0.5, mix with black
        pngData[dstY*dstWidth + dstX] := M
      else
        //Brightness > 0.5, mix with white
        pngData[dstY*dstWidth + dstX] := $FF + (255 - M) * $010100;
    end
    else
      pngData[dstY*dstWidth + dstX] := RGB;

    //Apply alpha
    if (aExportType = ietBaseWhite) or (aExportType = ietBaseBlack) then
      pngData[dstY*dstWidth + dstX] := pngData[dstY*dstWidth + dstX] or $FF000000
    else if (aExportType = ietBase) or (aExportType = ietNormal) then
      pngData[dstY*dstWidth + dstX] := pngData[dstY*dstWidth + dstX] or (fRXData.RGBA[aIndex, I*srcWidth + K] and $FF000000);

    //Is this a background pixel?
    if isShadow or (A = 0) then
    begin
      if aExportType = ietBase then
        pngData[dstY*dstWidth + dstX] := aBkgColour and $00FFFFFF
      else if aExportType = ietBaseWhite then
        pngData[dstY*dstWidth + dstX] := $FFFFFFFF
      else if aExportType = ietBaseBlack then
        pngData[dstY*dstWidth + dstX] := $FF000000;
    end;
  end;

  //1px grow of RGB into transparent areas
  //This helps because the interp algo handles transparency badly: It blends RGB from fully transparent pixels
  //Without this step you get a black halo around interpolated stuff
  if aExportType = ietBase then
  begin
    for Y := 0 to aCanvasSize-1 do
    for X := 0 to aCanvasSize-1 do
    begin
      if pngData[Y*aCanvasSize + X] shr 24 = 0 then
      begin
        C := 0;
        R := 0;
        G := 0;
        B := 0;
        for I := -1 to 1 do
        for K := -1 to 1 do
        begin
          dstY := EnsureRange(Y+I, 0, aCanvasSize-1);
          dstX := EnsureRange(X+K, 0, aCanvasSize-1);
          if pngData[dstY*aCanvasSize + dstX] shr 24 > 0 then
          begin
            Inc(C);
            R := R +  pngData[dstY*aCanvasSize + dstX]         and $FF;
            G := G + (pngData[dstY*aCanvasSize + dstX] shr 8 ) and $FF;
            B := B + (pngData[dstY*aCanvasSize + dstX] shr 16) and $FF;
          end;
        end;
        if C > 0 then
        begin
          R := R div C;
          G := G div C;
          B := B div C;
          RGB := (R and $FF) or ((G shl 8) and $FF00) or ((B shl 16) and $FF0000);
          pngData[Y*aCanvasSize + X] := RGB and $FFFFFF;
        end;
      end;
    end;
  end;
end;


function TKMSpritePackEdit.GetLoaded: Boolean;
begin
  Result := fRXData.Count > 0;
end;


procedure TKMSpritePackEdit.AdjoinHouseMasks(aResHouses: TKMResHouses);
var
  HT: TKMHouseType;
  ID1, ID2: Integer; //RGB and A index
  I, K, Lay, StepCount: Integer;
  T1, T2, tx, ty: Integer;
  Alpha: Byte;
begin
  {
  for HT := HOUSE_MIN to HOUSE_MAX do
  for Lay := 1 to 2 do //House is rendered in two layers since Stone does not covers Wood parts in e.g. Sawmill
  begin
    if Lay = 1 then
    begin
      ID1 := aResHouses[HT].WoodPic + 1;
      ID2 := aResHouses[HT].WoodPal + 1;
      StepCount := aResHouses[HT].WoodPicSteps;
    end else
    begin
      ID1 := aResHouses[HT].StonePic + 1;
      ID2 := aResHouses[HT].StonePal + 1;
      StepCount := aResHouses[HT].StonePicSteps;
    end;

    //Fill in alpha RXData
    tx := fRXData.Pivot[ID2].x - fRXData.Pivot[ID1].x;
    ty := fRXData.Pivot[ID2].y - fRXData.Pivot[ID1].y;
    for I := 0 to fRXData.Size[ID2].Y - 1 do
    for K := 0 to fRXData.Size[ID2].X - 1 do
    begin
      T2 := I * fRXData.Size[ID2].X + K;
      T1 := (I + ty) * fRXData.Size[ID1].X + K + tx;

      Alpha := fRXData.RGBA[ID2, T2] and $FF;
      fRXData.Mask[ID1, T1] := 255 - Round(Alpha / StepCount * 255);
    end;

    //Now we can discard building steps sprite
    fRXData.HasMask[ID1] := True;
    fRXData.Flag[ID2] := 0;
  end;
  }
end;


// Grow house building masks to account for blurred shadows edges being visible
procedure TKMSpritePackEdit.GrowHouseMasks(aResHouses: TKMResHouses);
var
  HT: TKMHouseType;
  ID: Integer; //RGB and A index
  I, K, Lay: Integer;
  T: Integer;
  A, B, C, D: Byte;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
  for Lay := 1 to 2 do //House is rendered in two layers since Stone does not cover Wood parts in e.g. Sawmill
  begin
    ID := IfThen(Lay = 1, aResHouses[HT].WoodPic, aResHouses[HT].StonePic) + 1;

    //Grow the masks
    //Since shadows direction is X+ Y- we can do just one pass into that direction
    for I := fRXData.Size[ID].Y - 1 downto 0 do
    for K := 0 to fRXData.Size[ID].X - 1 do
    begin
      T := I * fRXData.Size[ID].X + K;
      //Check 4 neighbours for now
      if (fRXData.Mask[ID, T] = 255) then
      begin
        A := fRXData.Mask[ID, I * fRXData.Size[ID].X + Min(K + 1, fRXData.Size[ID].X - 1)];
        B := fRXData.Mask[ID, I * fRXData.Size[ID].X + Max(K - 1, 0)];
        C := fRXData.Mask[ID, Min(I + 1, fRXData.Size[ID].Y - 1) * fRXData.Size[ID].X + K];
        D := fRXData.Mask[ID, Max(I - 1, 0) * fRXData.Size[ID].X + K];

        fRXData.Mask[ID, T] := Min(Min(A, B), Min(C, D));
      end;
    end;
  end;
end;


procedure TKMSpritePackEdit.SoftWater(aTileset: TKMResTileset);
var
  I, J, K, T: Integer;
  AR, AG, AB: Cardinal;
  Samples: Cardinal;
  Tmp: array [0 .. 32 * 32 - 1] of Cardinal;

  procedure AddAccum(aColor: Cardinal);
  begin
    if aColor = 0 then Exit;
    AR := AR + aColor and $FF;
    AG := AG + aColor shr 8 and $FF;
    AB := AB + aColor shr 16 and $FF;
    Inc(Samples);
  end;
begin
  for I := 1 to 256 do
  if fRXData.Flag[I + 300] <> 0 then
  begin
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;

      if (fRXData.RGBA[I + 300, T] <> 0) then
      begin
        //We take advantage of the checkerboard pattern to fill missing pixels
        AR := 0;
        AG := 0;
        AB := 0;
        Samples := 0;
        if K > 0 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K - 1]);
        if K < fRXData.Size[I].X - 1 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K + 1]);
        if J > 0 then
          AddAccum(fRXData.RGBA[I, (J - 1) * fRXData.Size[I].X + K]);
        if J < fRXData.Size[I].Y - 1 then
          AddAccum(fRXData.RGBA[I, (J + 1) * fRXData.Size[I].X + K]);

        fRXData.RGBA[I, T] := (AR div Samples) or (AG div Samples) shl 8 or (AB div Samples) shr 16 or $FF000000;
      end;
    end;
  end;

  //Soften water
  for I := 301 to 4200 do
  if fRXData.Flag[I] <> 0 then
  begin
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;

      if (fRXData.RGBA[I, T] = 0) then
      begin
        //We take advantage of the checkerboard pattern to fill missing pixels
        AR := 0;
        AG := 0;
        AB := 0;
        Samples := 0;
        if K > 0 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K - 1]);
        if K < fRXData.Size[I].X - 1 then
          AddAccum(fRXData.RGBA[I, J * fRXData.Size[I].X + K + 1]);
        if J > 0 then
          AddAccum(fRXData.RGBA[I, (J - 1) * fRXData.Size[I].X + K]);
        if J < fRXData.Size[I].Y - 1 then
          AddAccum(fRXData.RGBA[I, (J + 1) * fRXData.Size[I].X + K]);

        if Samples > 0 then
          Tmp[T] := Round(AR / Samples) or (Round(AG / Samples) shl 8) or (Round(AB / Samples) shr 16) or (Samples * $30000000);
      end
      else
        Tmp[T] := (fRXData.RGBA[I, T] and $FFFFFF) or $B0000000;
    end;

    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    begin
      T := J * fRXData.Size[I].X + K;
      fRXData.RGBA[I, T] := Tmp[T];
      Tmp[T] := 0;
    end;
  end;
end;


//Release RAM that is no longer needed
procedure TKMSpritePackEdit.ClearTemp;
var
  I: Integer;
begin
  inherited;

  for I := 1 to fRXData.Count do
    SetLength(fRXData.Data[I], 0);
end;


// Cut off empty pixels on sides
function TKMSpritePackEdit.TrimSprites: Cardinal;
var
  I,J,K: Integer;
  newRight, newLeft, newBottom, newTop: Word;
  offsetX, offsetY, newWidth, newHeight: Word;
  FoundPixel: Boolean;
begin
  Result := 0;
  Exit;

  for I := 1 to fRXData.Count do
  if fRXData.Flag[I] <> 0 then
  if fRXData.Size[I].X * fRXData.Size[I].Y <> 0 then
  begin
    // Check bounds
    newRight  := 0;
    newBottom := 0;
    newLeft   := fRXData.Size[I].X - 1;
    newTop    := fRXData.Size[I].Y - 1;
    FoundPixel := False;
    for J := 0 to fRXData.Size[I].Y - 1 do
    for K := 0 to fRXData.Size[I].X - 1 do
    if fRXData.RGBA[I, J * fRXData.Size[I].X + K] and $FF000000 <> 0 then
    begin
      newRight  := Max(newRight,  K);
      newBottom := Max(newBottom, J);
      newLeft   := Min(newLeft,   K);
      newTop    := Min(newTop,    J);
      FoundPixel := True;
    end;

    if not FoundPixel then
    begin
      // Entire image is transparent
      fRXData.Size[I].X := 1;
      fRXData.Size[I].Y := 1;
      Continue;
    end;

    Inc(newRight);
    Inc(newBottom);
    Assert((newLeft <= newRight) and (newTop <= newBottom), 'Left > Right or Top > Bottom');
    offsetX := newLeft;
    offsetY := newTop;
    newWidth := newRight - newLeft;
    newHeight := newBottom - newTop;

    Inc(Result, (fRXData.Size[I].X * fRXData.Size[I].Y) - newWidth * newHeight);

    // Do the trimming
    for J := 0 to newHeight - 1 do
    begin
      Move(
        fRXData.RGBA[I, (J + offsetY) * fRXData.Size[I].X + offsetX],
        fRXData.RGBA[I, J * newWidth],
        newWidth * 4); //RGBA is 4 bytes per pixel
      Move(
        fRXData.Mask[I, (J + offsetY) * fRXData.Size[I].X + offsetX],
        fRXData.Mask[I, J * newWidth],
        newWidth * 1); //Mask is 1 byte per pixel
    end;

    fRXData.Size[I].X := newWidth;
    fRXData.Size[I].Y := newHeight;
    fRXData.Pivot[I].X := fRXData.Pivot[I].X + offsetX;
    fRXData.Pivot[I].Y := fRXData.Pivot[I].Y + offsetY;
  end;
end;


procedure TKMSpritePackEdit.LoadFromRXFile(const aFileName: string);
var
  I: Integer;
  S: TMemoryStream;
  pivot: record X,Y: Integer; end;
begin
  if not FileExists(aFileName) then
  begin
    ShowMessage('File not found: ' + aFileName);
    Exit;
  end;

  S := TMemoryStream.Create;

  S.LoadFromFile(aFileName);
  S.ReadBuffer(fRXData.Count, 4);

  Allocate(fRXData.Count);

  S.ReadBuffer(fRXData.Flag[1], fRXData.Count);

  for I := 1 to fRXData.Count do
    if fRXData.Flag[I] = 1 then
    begin
      S.ReadBuffer(fRXData.Size[I].X, 4);
      S.ReadBuffer(pivot, 8);
      fRXData.Pivot[I].X := pivot.X; // We use SmallInt for pivot, while it is Integer in RX
      fRXData.Pivot[I].Y := pivot.Y;
      //Data part of each sprite is 8BPP paletted in KaM RX
      SetLength(fRXData.Data[I], fRXData.Size[I].X * fRXData.Size[I].Y);
      S.ReadBuffer(fRXData.Data[I,0], fRXData.Size[I].X * fRXData.Size[I].Y);
    end;
  S.Free;

  Expand; //Only KaM's rx needs expanding
end;


procedure TKMSpritePackEdit.WriteRXZHeader(aStream: TStream; aFormat: TKMRXXFormat);
var
  metadata: AnsiString;
  metadataLen: Word;
begin
  case aFormat of
    rxxZero:  ;// No header, just straight ZLib data
    rxxOne:   begin
                aStream.Write(RXX_VERSION_1[1], Length(RXX_VERSION_1));

                metadata := GAME_REVISION + ' ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now)
                ;
                metadataLen := Length(metadata);

                aStream.Write(metadataLen, 2);
                aStream.Write(metadata[1], metadataLen);
              end;
  else
    raise Exception.Create('Saving in unsupported format');
  end;
end;


procedure TKMSpritePackEdit.SaveToRXAFile(const aFileName: string; aFormat: TKMRXXFormat);
const
  SNS_MAX_ABS_VAL = CELL_SIZE_PX*5; // Empirical value
var
  I: Integer;
  SAT: TKMSpriteAtlasType;
  InputStream: TCompressionStream;
  OutputStream: TFileStream;
  baseRAM, colorRAM, texCount: Cardinal;
  numAtlases, numSprites, numPixels: Integer;
begin
  if IsEmpty then Exit;

  MakeGFX_BinPacking(tfRGBA8, 1, baseRAM, colorRAM, texCount, False);

  ForceDirectories(ExtractFilePath(aFileName));

  OutputStream := TFileStream.Create(aFileName, fmCreate);
  WriteRXZHeader(OutputStream, aFormat);
  InputStream := TCompressionStream.Create(clMax, OutputStream);

  //Sprite info
  InputStream.Write(fRXData.Count, 4);
  InputStream.Write(fRXData.Flag[1], fRXData.Count);

  for I := 1 to fRXData.Count do
    if fRXData.Flag[I] = 1 then
    begin
      InputStream.Write(fRXData.Size[I].X, SizeOf(fRXData.Size[I]));
      InputStream.Write(fRXData.Pivot[I].X, SizeOf(fRXData.Pivot[I]));
      if fRT = rxUnits then
      begin
        // Protection from incorrect values
        fRXData.SizeNoShadow[I].Left    := EnsureRange(fRXData.SizeNoShadow[I].Left,   -SNS_MAX_ABS_VAL, SNS_MAX_ABS_VAL);
        fRXData.SizeNoShadow[I].Top     := EnsureRange(fRXData.SizeNoShadow[I].Top,    -SNS_MAX_ABS_VAL, SNS_MAX_ABS_VAL);
        fRXData.SizeNoShadow[I].Right   := EnsureRange(fRXData.SizeNoShadow[I].Right,  -SNS_MAX_ABS_VAL, SNS_MAX_ABS_VAL);
        fRXData.SizeNoShadow[I].Bottom  := EnsureRange(fRXData.SizeNoShadow[I].Bottom, -SNS_MAX_ABS_VAL, SNS_MAX_ABS_VAL);

        InputStream.Write(fRXData.SizeNoShadow[I].Left, SizeOf(fRXData.SizeNoShadow[I]));
      end;
      InputStream.Write(fRXData.HasMask[I], 1);
    end;

  // Atlases
  for SAT := Low(fAtlases) to High(fAtlases) do
  begin
    numAtlases := Length(fAtlases[SAT]);
    InputStream.Write(numAtlases, 4);
    for I := Low(fAtlases[SAT]) to High(fAtlases[SAT]) do
      with fAtlases[SAT, I] do
      begin
        InputStream.Write(Container.Width, 2);
        InputStream.Write(Container.Height, 2);
        numSprites := Length(Container.Sprites);
        InputStream.Write(numSprites, 4);
        InputStream.Write(Container.Sprites[0], numSprites * SizeOf(Container.Sprites[0]));
        InputStream.Write(TexType, SizeOf(TKMTexFormat));
        numPixels := Length(Data);
        InputStream.Write(numPixels, 4);
        InputStream.Write(Data[0], numPixels * SizeOf(Data[0]));
      end;
  end;

  InputStream.Free;
  OutputStream.Free;
end;


procedure TKMSpritePackEdit.SaveToRXXFile(const aFileName: string; aFormat: TKMRXXFormat);
var
  I: Integer;
  InputStream: TMemoryStream;
  OutputStream: TFileStream;
  CompressionStream: TCompressionStream;
begin
  // No image was loaded yet
  //@Rey: Perhaps we should erase the file in such case, otherwise mapmaker will have to go into folder to delete rxx himself if he decided to "clear" it
  if IsEmpty then Exit;

  ForceDirectories(ExtractFilePath(aFileName));

  InputStream := TMemoryStream.Create;

  InputStream.Write(fRXData.Count, 4);
  InputStream.Write(fRXData.Flag[1], fRXData.Count);

  for I := 1 to fRXData.Count do
    if fRXData.Flag[I] = 1 then
    begin
      InputStream.Write(fRXData.Size[I].X, SizeOf(fRXData.Size[I]));
      InputStream.Write(fRXData.Pivot[I].X, SizeOf(fRXData.Pivot[I]));

      if fRT = rxUnits then
        InputStream.Write(fRXData.SizeNoShadow[I].Left, SizeOf(fRXData.SizeNoShadow[I]));
      InputStream.Write(fRXData.RGBA[I, 0], 4 * fRXData.Size[I].X * fRXData.Size[I].Y);
      InputStream.Write(fRXData.HasMask[I], 1);
      if fRXData.HasMask[I] then
        InputStream.Write(fRXData.Mask[I, 0], fRXData.Size[I].X * fRXData.Size[I].Y);
    end;
  OutputStream := TFileStream.Create(aFileName, fmCreate);

  WriteRXZHeader(OutputStream, aFormat);

  CompressionStream := TCompressionStream.Create(clMax, OutputStream);
  InputStream.Position := 0;
  CompressionStream.CopyFrom(InputStream, InputStream.Size);
  CompressionStream.Free;
  OutputStream.Free;
  InputStream.Free;
end;


procedure TKMSpritePackEdit.GetImageToBitmap(aIndex: Integer; aBmp, aMask: TBitmap);
var
  I, K, W, H: Integer;
  T: Cardinal;
begin
  if fRXData.Flag[aIndex] = 0 then Exit;

  W := fRXData.Size[aIndex].X;
  H := fRXData.Size[aIndex].Y;

  aBmp.SetSize(W, H);
  if aMask <> nil then
    aMask.SetSize(W, H);

  for I := 0 to H - 1 do
  for K := 0 to W - 1 do
  begin
    T := fRXData.RGBA[aIndex, I * W + K];

    aBmp.Canvas.Pixels[K,I] := T and $FFFFFF;

    if (aMask <> nil) and fRXData.HasMask[aIndex] then
    begin
      T := fRXData.Mask[aIndex, I * W + K];

      aMask.Canvas.Pixels[K,I] := T * 65793;
    end;
  end;
end;


end.
