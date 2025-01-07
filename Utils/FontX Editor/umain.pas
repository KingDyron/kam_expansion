﻿unit umain;
{$I ..\..\KaM_Remake.inc}
interface
uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, Math, ComCtrls, Buttons, Spin, StrUtils, KromUtils,
  KM_Defaults, KM_ResFonts, KM_ResFontsEdit;


type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    lbFonts: TListBox;
    Shape1: TShape;
    StatusBar1: TStatusBar;
    imgPreviewSmall: TImage;
    imgPreviewBig: TImage;
    Edit1: TEdit;
    CheckCells: TCheckBox;
    btnSaveFontX: TBitBtn;
    btnImportPng: TBitBtn;
    btnExportPng: TBitBtn;
    ScrollBar1: TScrollBar;
    pbFont: TPaintBox;
    GroupBox1: TGroupBox;
    seLetterY: TSpinEdit;
    Label7: TLabel;
    GroupBox2: TGroupBox;
    seCharSpacing: TSpinEdit;
    seLineSpacing: TSpinEdit;
    seWordSpacing: TSpinEdit;
    seBaseHeight: TSpinEdit;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    seAllYOffset: TSpinEdit;
    Label8: TLabel;
    GroupBox3: TGroupBox;
    sePadRight: TSpinEdit;
    sePadLeft: TSpinEdit;
    sePadBottom: TSpinEdit;
    sePadTop: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    GroupBox4: TGroupBox;
    Label9: TLabel;
    sePadding: TSpinEdit;
    rgSizeX: TRadioGroup;
    rgSizeY: TRadioGroup;
    procedure btnSaveFontXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbFontsClick(Sender: TObject);
    procedure btnExportPngClick(Sender: TObject);
    procedure pbFontMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure pbFontMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure CheckCellsClick(Sender: TObject);
    procedure btnImportPngClick(Sender: TObject);
    procedure seFontPropsChange(Sender: TObject);
    procedure seLetterYChange(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbFontPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    fBmp: TBitmap;
    fFontSpec: TKMFontSpecEdit;
    fCellX: Byte;
    fCellY: Byte;
    fRows: Word;
    fCols: Word;
    fSelectedLetter: Integer;
    fUpdating: Boolean;
    fPreviewText: UnicodeString;
    fPreviewCells: Boolean;
    procedure ScanFonts(const aPath: string);
    procedure LoadFont(const aFilename: string);
    procedure RefreshTextPreview;
  public
    procedure ShowBigImage(aShowCells: Boolean);
  end;


const
  TEX_SIZE = 512;
  FONTS_BG_COLOR = $AF6B6B;
var
  ExeDir: string;
  DataDir: string;


implementation
uses
  KromShellUtils;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'KaM FontX Editor (' + GAME_REVISION + ')';

  ExeDir := ExtractFilePath(ParamStr(0));
  DataDir := ExeDir;
  if DirectoryExists(ExeDir + '..\..\' + TKMFontSpec.FONTS_FOLDER) then //Remake project location
    DataDir := ExeDir + '..\..\';
  if DirectoryExists(ExeDir + TKMFontSpec.FONTS_FOLDER) then //Default location
    DataDir := ExeDir;

  ScanFonts(DataDir);

  fSelectedLetter := 0;

  //Off-screen bitmap which we draw OnPaint event
  fBmp := TBitmap.Create;
  fBmp.PixelFormat := pf24bit;
  fBmp.Width := TEX_SIZE;
  fBmp.Height := TEX_SIZE;

  DoubleBuffered := True;
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  fBmp.Free;
end;


procedure TfrmMain.FormResize(Sender: TObject);
begin
  fBmp.Width := pbFont.Width;
  fBmp.Height := pbFont.Height;

  ShowBigImage(CheckCells.Checked);
  pbFont.Repaint;
end;


procedure TfrmMain.btnSaveFontXClick(Sender: TObject);
begin
  if fFontSpec = nil then
  begin
    MessageBox(Handle, 'Please select editing font first', 'Error', MB_OK);
    Exit;
  end;

  if not RunSaveDialog(
    SaveDialog1,
    lbFonts.Items[lbFonts.ItemIndex],
    DataDir + TKMFontSpec.FONTS_FOLDER,
    'KaM FontX|*.' + TKMFontSpec.DEFAULT_EXT,
    TKMFontSpec.DEFAULT_EXT) then
    Exit;

  fFontSpec.SaveToFontX(SaveDialog1.FileName);
end;


procedure TfrmMain.ScanFonts(const aPath: string);
var
  SearchRec: TSearchRec;
begin
  lbFonts.Clear;

  if not DirectoryExists(aPath + TKMFontSpec.FONTS_FOLDER) then Exit;

  FindFirst(aPath + TKMFontSpec.FONTS_FOLDER + '*.' + TKMFontSpec.DEFAULT_EXT, faAnyFile - faDirectory, SearchRec);
  repeat
    lbFonts.Items.Add(SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TfrmMain.lbFontsClick(Sender: TObject);
begin
  LoadFont(DataDir + TKMFontSpec.FONTS_FOLDER + lbFonts.Items[lbFonts.ItemIndex]);

  ShowBigImage(CheckCells.Checked);
  pbFont.Repaint;
  Edit1Change(nil);
  StatusBar1.Panels.Items[0].Text := 'Font: ' + lbFonts.Items[lbFonts.ItemIndex];
end;


procedure TfrmMain.LoadFont(const aFilename: string);
begin
  if not FileExists(aFilename) then Exit;

  FreeAndNil(fFontSpec);
  fFontSpec := TKMFontSpecEdit.Create(fntArial); //fntArial, why not, it looks like we dont care

  fFontSpec.LoadFontX(aFilename);

  fCellX := fFontSpec.MaxLetterWidth + 1;
  fCellY := fFontSpec.MaxLetterHeight + 1;

  fUpdating := True;
  seBaseHeight.Value := fFontSpec.BaseHeight;
  seWordSpacing.Value := fFontSpec.WordSpacing;
  seCharSpacing.Value := fFontSpec.CharSpacing;
  seLineSpacing.Value := fFontSpec.Unknown;
  seAllYOffset.Value := 0;
  fUpdating := False;
end;


procedure TfrmMain.ShowBigImage(aShowCells: Boolean);
var
  I,K,L,M: Integer;
  pX, pY: Word;
  T, R: Cardinal;
  Let: TKMLetter;
  offset: Word;
  letter: Integer;
begin
  fBmp.Canvas.Brush.Color := FONTS_BG_COLOR;
  fBmp.Canvas.FillRect(fBmp.Canvas.ClipRect);

  if fFontSpec = nil then Exit;

  fCols := fBmp.Width div fCellX;
  fRows := fBmp.Height div fCellY;

  offset := ScrollBar1.Position;

  for I := 0 to fRows - 1 do
  for K := 0 to fCols - 1 do
  begin
    letter := offset + I * fCols + K;

    //Dont render anything beyond 65535 if we have Scroll positioned past last char
    if letter > 65535 then Break;
    if fFontSpec.Used[letter] = 0 then Continue;

    Let := fFontSpec.Letters[letter];
    for L := 0 to Let.Height - 1 do
    for M := 0 to Let.Width - 1 do
    begin
      pX := Round(Let.u1 * fFontSpec.TexSizeX) + M;
      pY := Round(Let.v1 * fFontSpec.TexSizeY) + L;
      T := fFontSpec.TexData[Let.AtlasId][pY * fFontSpec.TexSizeX + pX];

      //Blend with background
      R := Round(Lerp(FONTS_BG_COLOR and $FF, T and $FF, T shr 24 / 255)) +
           Round(Lerp(FONTS_BG_COLOR shr 8 and $FF, T shr 8 and $FF, T shr 24 / 255)) shl 8 +
           Round(Lerp(FONTS_BG_COLOR shr 16 and $FF, T shr 16 and $FF, T shr 24 / 255)) shl 16;

      fBmp.Canvas.Pixels[K * fCellX + M + 1, I * fCellY + L + 1] := R;
    end;
  end;

  if aShowCells then
  begin
    fBmp.Canvas.Pen.Color := $AAAAAA;

    for I := 0 to fRows - 1 do
    begin
      fBmp.Canvas.MoveTo(0, I * fCellY);
      fBmp.Canvas.LineTo(fBmp.Width - 1, I * fCellY);
    end;

    for K := 0 to fCols - 1 do
    begin
      fBmp.Canvas.MoveTo(K * fCellX, 0);
      fBmp.Canvas.LineTo(K * fCellX, fBmp.Height - 1);
    end;
  end;

  pbFont.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.pbFontPaint(Sender: TObject);
begin
  pbFont.Canvas.Draw(0, 0, fBmp);
end;


procedure TfrmMain.btnExportPngClick(Sender: TObject);
begin
  if not RunSaveDialog(SaveDialog1, '', ExeDir, 'PNG images|*.png', 'png') then Exit;

  fFontSpec.ExportGridPng(SaveDialog1.FileName, Rect(sePadLeft.Value, sePadTop.Value, sePadRight.Value, sePadBottom.Value));
end;


procedure TfrmMain.btnImportPngClick(Sender: TObject);
begin
  Assert(fFontSpec <> nil, 'Import needs donor font selected');

  if not RunOpenDialog(OpenDialog1, '', ExeDir, 'PNG images|*.png') then Exit;

  fFontSpec.TexSizeX := StrToInt(rgSizeX.Items[rgSizeX.ItemIndex]);
  fFontSpec.TexSizeY := StrToInt(rgSizeY.Items[rgSizeY.ItemIndex]);
  fFontSpec.TexPadding := sePadding.Value;
  fFontSpec.ImportGridPng(OpenDialog1.FileName);

  fCellX := fFontSpec.MaxLetterWidth + 1;
  fCellY := fFontSpec.MaxLetterHeight + 1;

  ShowBigImage(CheckCells.Checked);
  pbFont.Repaint;
  RefreshTextPreview;
end;


procedure TfrmMain.pbFontMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
  id, offset: Word;
  Let: TKMLetter;
begin
  if fFontSpec = nil then Exit;

  //Information about character below
  offset := ScrollBar1.Position;
  id := offset + (Y div fCellY) * fCols + X div fCellX;
  Let := fFontSpec.Letters[id];
  StatusBar1.Panels.Items[1].Text := 'Character: ' + IntToStr(id) + ' (' + IntToHex(id, 2) + 'h)';
  StatusBar1.Panels.Items[2].Text := Format('Width %d, Height %d, %d . %d',
                                            [Let.Width, Let.Height, Let.AtlasId, Let.YOffset]);
end;


procedure TfrmMain.pbFontMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  offset: Word;
begin
  if fFontSpec = nil then Exit;

  offset := ScrollBar1.Position;

  fSelectedLetter := offset + (Y div fCellY) * fCols + X div fCellX;
  GroupBox1.Caption := ' Letter: ' + IntToStr(fSelectedLetter) + ' (' + IntToHex(fSelectedLetter, 2) + 'h) ';

  Shape1.Width := fFontSpec.Letters[fSelectedLetter].Width + 4;
  Shape1.Height := fFontSpec.Letters[fSelectedLetter].Height + 4;

  Shape1.Left := pbFont.Left + (fSelectedLetter - offset) mod fCols * fCellX - 1;
  Shape1.Top := pbFont.Top + (fSelectedLetter - offset) div fCols * fCellY - 1;

  fUpdating := True;
  seLetterY.Value := fFontSpec.Letters[fSelectedLetter].YOffset;
  fUpdating := False;
end;


procedure TfrmMain.Edit1Change(Sender: TObject);
begin
  {$IFDEF FPC} //FPC uses unicode strings in Edit1
    fPreviewText := Trim(UTF8ToAnsi(Edit1.Text));
  {$ENDIF}
  {$IFDEF WDC} //Delphi uses ansi strings
    fPreviewText := Trim(Edit1.Text);
  {$ENDIF}

  RefreshTextPreview;
end;


procedure TfrmMain.RefreshTextPreview;
const
  PAD = 2;
var
  Bmp: TBitmap;
  I, L, M: Integer;
  srcCol, dstCol: Integer;
  dx: Integer;
  MyRect: TRect;
  Let: TKMLetter;
  pX, pY: Word;
  alpha: Byte;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := 512;
  Bmp.Height := 22;

  dx := 10;

  //Fill area
  Bmp.Canvas.Brush.Color := FONTS_BG_COLOR;
  Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);

  Bmp.Canvas.Pen.Style := psSolid;
  Bmp.Canvas.Pen.Color := clLtGray;
  Bmp.Canvas.MoveTo(0, 12 + PAD);
  Bmp.Canvas.LineTo(512, 12 + PAD);

  for I := 1 to Length(fPreviewText) do
  if fPreviewText[I] <> ' ' then
  begin
    Let := fFontSpec.Letters[Ord(fPreviewText[I])];

    for L := 0 to Let.Height - 1 do
    for M := 0 to Let.Width - 1 do
    begin
      pX := Round(Let.u1 * fFontSpec.TexSizeX) + M;
      pY := Round(Let.v1 * fFontSpec.TexSizeY) + L;

      srcCol := fFontSpec.TexData[Let.AtlasId][pY * fFontSpec.TexSizeX + pX] and $FFFFFF;
      dstCol := Bmp.Canvas.Pixels[dx + M, Let.YOffset + L + PAD] and $FFFFFF;
      alpha := 255 - (fFontSpec.TexData[Let.AtlasId][pY * fFontSpec.TexSizeX + pX] shr 24) and $FF;

      if fPreviewCells then
      begin
        if alpha = 255 then
          srcCol := $808080;
        alpha := 0;
      end;

      //srcCol + (dstCol - srcCol) * alpha
      Bmp.Canvas.Pixels[dx + M, Let.YOffset + L + PAD] :=
        ((srcCol and $FF) + ((dstCol and $FF - srcCol and $FF) * alpha) div 256) +
        ((srcCol shr 8 and $FF) + ((dstCol shr 8 and $FF - srcCol shr 8 and $FF) * alpha) div 256) shl 8 +
        ((srcCol shr 16 and $FF) + ((dstCol shr 16 and $FF - srcCol shr 16 and $FF) * alpha) div 256) shl 16;
    end;

    Inc(dx, Let.Width + fFontSpec.CharSpacing);
  end else
    Inc(dx, fFontSpec.WordSpacing);

  //Match phrase bounds
  Bmp.Width := Max(dx - Min(fFontSpec.CharSpacing, 0), 0) + 20; //Revert last char overlap (if spacing is negative)
  Bmp.Height := 22;

  imgPreviewSmall.Canvas.Brush.Color := FONTS_BG_COLOR;
  imgPreviewSmall.Canvas.FillRect(imgPreviewSmall.Canvas.ClipRect);
  imgPreviewSmall.Canvas.Draw((imgPreviewSmall.Width - Bmp.Width) div 2, (imgPreviewSmall.Height - Bmp.Height) div 2 + 2, Bmp); //Draw MyBitmap into Image1

  MyRect.Left := (imgPreviewBig.Width  - Bmp.Width*4 ) div 2;
  MyRect.Top  := (imgPreviewBig.Height - Bmp.Height*4) div 2;
  MyRect.Right  := MyRect.Left + Bmp.Width*4;
  MyRect.Bottom := MyRect.Top + Bmp.Height*4;

  imgPreviewBig.Canvas.Brush.Color := FONTS_BG_COLOR;
  imgPreviewBig.Canvas.FillRect(imgPreviewBig.Canvas.ClipRect);
  imgPreviewBig.Canvas.StretchDraw(MyRect, Bmp); //Draw MyBitmap into Image1

  Bmp.Free;
end;


procedure TfrmMain.CheckBox1Click(Sender: TObject);
begin
  fPreviewCells := CheckBox1.Checked;
  RefreshTextPreview;
end;


procedure TfrmMain.CheckCellsClick(Sender: TObject);
begin
  ShowBigImage(CheckCells.Checked);
  pbFont.Repaint;
end;


procedure TfrmMain.seFontPropsChange(Sender: TObject);
var
  I: Integer;
begin
  if fUpdating then Exit;

  if (Sender is TSpinEdit) and (TSpinEdit(Sender).Text = '') then Exit;

  fFontSpec.BaseHeight  := seBaseHeight.Value;
  fFontSpec.WordSpacing := seWordSpacing.Value;
  fFontSpec.CharSpacing := seCharSpacing.Value;
  fFontSpec.Unknown     := seLineSpacing.Value;

  if Sender = seAllYOffset then
  for I := 0 to fFontSpec.CharCount - 1 do
  if fFontSpec.Letters[I].Width > 0 then
    fFontSpec.Letters[I].YOffset := seAllYOffset.Value;

  RefreshTextPreview;
end;


procedure TfrmMain.seLetterYChange(Sender: TObject);
begin
  if fUpdating then Exit;
  if fSelectedLetter = 0 then Exit;

  fFontSpec.Letters[fSelectedLetter].YOffset := seLetterY.Value;
  RefreshTextPreview;
end;


procedure TfrmMain.ScrollBar1Change(Sender: TObject);
var
  offset: Word;
begin
  ShowBigImage(CheckCells.Checked);
  pbFont.Repaint;

  offset := ScrollBar1.Position;

  Shape1.Left := pbFont.Left + (fSelectedLetter - offset) mod fCols * fCellX - 1;
  Shape1.Top := pbFont.Top + (fSelectedLetter - offset) div fCols * fCellY - 1;
end;


initialization
{$IFDEF FPC}
{$I umain.lrs}
{$ENDIF}


end.
