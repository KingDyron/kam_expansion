unit RXXEditorForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, FileCtrl, Forms, Graphics, Spin, StdCtrls, SysUtils,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_IoPNG, KM_ResPalettes, KM_ResSprites, KM_ResSpritesEdit;


type
  TfmRXXEditor = class(TForm)
    btnAdd: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TFileOpenDialog;
    btnSaveRXX: TButton;
    lbSpritesList: TListBox;
    btnLoadRXX: TButton;
    btnDelete: TButton;
    btnReplace: TButton;
    btnExport: TButton;
    Panel1: TPanel;
    imgMain: TImage;
    Label1: TLabel;
    Panel2: TPanel;
    imgMask: TImage;
    Label2: TLabel;
    Label3: TLabel;
    edtPivotX: TSpinEdit;
    edtPivotY: TSpinEdit;
    chkHasMask: TCheckBox;
    chbImageStretch: TCheckBox;
    Label4: TLabel;
    chbOverload: TCheckBox;
    chbOnlyFolder: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure SaveDialog1Show(Sender: TObject);
    procedure btnLoadRXXClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveRXXClick(Sender: TObject);
    procedure lbSpritesListClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PivotChange(Sender: TObject);
    procedure chbImageStretchClick(Sender: TObject);
  private
    fLoading : Boolean;
    fPalettes: TKMResPalettes;
    fSprites: TKMSpritePackEdit;
    procedure UpdateList;
    procedure ImageExport(aID: Integer; const aFileName: string);
  end;


implementation
uses
  Math,
  KM_ResTypes;

{$R *.dfm}

procedure TfmRXXEditor.FormCreate(Sender: TObject);
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'RXX Editor (' + GAME_REVISION + ')';

  gLog := TKMLog.Create(ExeDir + 'RXXEditor.log');

  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  chbImageStretchClick(nil);
end;


procedure TfmRXXEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSprites);
  FreeAndNil(fPalettes);
  FreeAndNil(gLog);
end;


procedure TfmRXXEditor.lbSpritesListClick(Sender: TObject);
  procedure ToggleImageButtons(aEnabled: Boolean);
  begin
    edtPivotX.Enabled := aEnabled;
    edtPivotY.Enabled := aEnabled;
    btnDelete.Enabled := aEnabled;
    btnReplace.Enabled := aEnabled;
    btnExport.Enabled := aEnabled;
  end;
var
  id: Integer;
  bmpBase, bmpMask: TBitmap;
begin
  fLoading := true;
  ToggleImageButtons(true);

  {imgMain.Picture.Bitmap.Canvas.Brush.Color := 0;
  imgMain.Picture.Bitmap.Canvas.FillRect(imgMain.Picture.Bitmap.Canvas.ClipRect);
  imgMask.Picture.Bitmap.Canvas.Brush.Color := 0;
  imgMask.Picture.Bitmap.Canvas.FillRect(imgMain.Picture.Bitmap.Canvas.ClipRect);}
  //imgMain.Bitmap
  chkHasMask.Checked := False;

  if lbSpritesList.SelCount <> 1 then
  begin
    // With multiple images selected there's nothing we can display or edit, we can only Export them
    btnExport.Enabled := True;
    Exit;
  end;

  id := lbSpritesList.ItemIndex + 1;
  if id = 0 then Exit;
  if fSprites.RXData.Flag[id] = 0 then Exit;

  bmpBase := TBitmap.Create;
  bmpMask := TBitmap.Create;
  try
    fSprites.GetImageToBitmap(id, bmpBase, bmpMask);
    imgMain.Picture.Assign(bmpBase);

    if bmpMask.Width * bmpMask.Height <> 0 then
      imgMask.Picture.Assign(bmpMask);
  finally
    bmpBase.Free;
    bmpMask.Free;
  end;
  edtPivotX.Value := fSprites.RXData.Pivot[id].x;
  edtPivotY.Value := fSprites.RXData.Pivot[id].y;
  chkHasMask.Checked := fSprites.RXData.HasMask[id];

  ToggleImageButtons(True);
  fLoading := false;
end;


procedure TfmRXXEditor.OpenDialog1Show(Sender: TObject);
begin
  //Win7 needs InitialDir to be set OnShow after Execute
  OpenDialog1.InitialDir := ExeDir;
end;


procedure TfmRXXEditor.SaveDialog1Show(Sender: TObject);
begin
  //Win7 needs InitialDir to be set OnShow after Execute
  SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
end;


procedure TfmRXXEditor.btnLoadRXXClick(Sender: TObject);
var
  RT: TRXType;
begin
  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.Filter := 'RX, RXX packages (*.rx;*.rxx)|*.rxx;*.rx;';
  if DirectoryExists(ExeDir + 'data\sprites\') then
    OpenDialog1.InitialDir := ExeDir + 'data\sprites\'
  else
    OpenDialog1.InitialDir := ExeDir + 'data\';

  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  //GuiMain needs to know it's type to use special palette mappings

  if SameText(ExtractFileName(OpenDialog1.FileName), 'guimain.rxx') then
    RT := rxGuiMain
  else
  if SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'units.rxx')
    or SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'units_a.rxx') then
    RT := rxUnits
  else
  if SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'gui.rxx')
    or SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'gui_a.rxx')
    or SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'gui_a.rxa') then
    RT := rxGui
  else
  if SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'houses.rxx')
    or SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'houses_a.rxx')
    or SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'houses.rxa') then
    RT := rxHouses
  else
  if SameText(LowerCase(ExtractFileName(OpenDialog1.FileName)), 'tileset.rxx') then
    RT := rxTiles
  else
    RT := rxTrees;

  FreeAndNil(fSprites);
  fSprites := TKMSpritePackEdit.Create(RT, fPalettes);

  Label1.Caption := ExtractFileName(OpenDialog1.FileName);

  if not chbOverload.Checked or (chbOverload.Checked and not chbOnlyFolder.Checked) then
  begin
    if SameText(ExtractFileExt(OpenDialog1.FileName), '.rx') then
      fSprites.LoadFromRXFile(OpenDialog1.FileName)
    else
    if SameText(ExtractFileExt(OpenDialog1.FileName), '.rxx') then
      fSprites.LoadFromRXXFile(OpenDialog1.FileName);
  end;

  if chbOverload.Checked then
  begin
    fSprites.OverloadGeneratedFromFolder(not (RT in [rxTiles, rxGuiMain]), ExeDir + 'Modding graphics' + PathDelim, false);
    fSprites.OverloadRXXFilesFromFolder(ExeDir + 'Modding graphics' + PathDelim);
  end;

  btnSaveRXX.Enabled := fSprites.IsLoaded;
  btnAdd.Enabled := fSprites.IsLoaded;

  UpdateList;
end;


procedure TfmRXXEditor.btnAddClick(Sender: TObject);
var
  I: Integer;
begin
  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.InitialDir := ExeDir;
  OpenDialog1.Filter := 'Supported images (*.bmp;*.png)|*.bmp;*.png';
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  for I := 0 to OpenDialog1.Files.Count - 1 do
    fSprites.AddImage(ExtractFilePath(OpenDialog1.Files[I]),
                      ExtractFileName(OpenDialog1.Files[I]), fSprites.RXData.Count+1);

  UpdateList;
end;


procedure TfmRXXEditor.btnReplaceClick(Sender: TObject);
var
  id: Integer;
begin
  id := lbSpritesList.ItemIndex + 1;
  if id = 0 then Exit;

  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.InitialDir := ExeDir;
  OpenDialog1.Filter := 'Supported images (*.png)|*.png';
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  fSprites.AddImage(ExtractFilePath(OpenDialog1.FileName),
                    ExtractFileName(OpenDialog1.FileName), id);

  UpdateList;
end;


procedure TfmRXXEditor.btnDeleteClick(Sender: TObject);
var
  id: Integer;
begin
  id := lbSpritesList.ItemIndex + 1;
  if id = 0 then Exit;
  fSprites.Delete(id, lbSpritesList.SelCount);

  UpdateList;
end;


procedure TfmRXXEditor.btnExportClick(Sender: TObject);
var
  I: Integer;
begin
  if lbSpritesList.SelCount = 1 then
  begin
    SaveDialog1.InitialDir := ExeDir;
    SaveDialog1.FileName := Format('%d_%.4d.png', [byte(fSprites.RT) + 1 , lbSpritesList.ItemIndex + 1]);
    SaveDialog1.Filter := 'PNG image (*.png)|*.png';
    SaveDialog1.Options := SaveDialog1.Options - [ofAllowMultiSelect];
    if not SaveDialog1.Execute then Exit;

    ImageExport(lbSpritesList.ItemIndex + 1, SaveDialog1.FileName);
  end else
  if lbSpritesList.SelCount > 1 then
  begin
    SelectDirectoryDialog1.DefaultFolder := ExeDir;
    if not SelectDirectoryDialog1.Execute then Exit;

    for I := 0 to lbSpritesList.Items.Count - 1 do
      if lbSpritesList.Selected[I] then
        ImageExport(I+1, SelectDirectoryDialog1.FileName + '\' + IntToStr(I+1) + '.png');
  end;
end;


procedure TfmRXXEditor.ImageExport(aID: Integer; const aFileName: string);
var
  maskFileName: string;
begin
  if fSprites.RXData.Flag[aID] = 0 then Exit;

  fSprites.ExportImage(aFileName, aID);

  maskFileName := ChangeFileExt(aFileName, 'a.png');
  fSprites.ExportMask(maskFileName, aID);
end;


procedure TfmRXXEditor.btnSaveRXXClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'RX, RXX packages (*.rx;*.rxx)|*.rxx;*.rx;';
  SaveDialog1.FileName := SysUtils.ExtractFileName(OpenDialog1.FileName);
  if not SaveDialog1.Execute then Exit;
  gLog.AddTime('Trimmed ' + IntToStr(fSprites.TrimSprites));

  // Always save as latest format for now. Later on we could allow format change in UI
  fSprites.SaveToRXXFile(SaveDialog1.FileName, rxxOne);
end;


procedure TfmRXXEditor.chbImageStretchClick(Sender: TObject);
begin
  imgMain.Stretch := chbImageStretch.Checked;
  imgMain.Center := not chbImageStretch.Checked;

  imgMask.Stretch := chbImageStretch.Checked;
  imgMask.Center := not chbImageStretch.Checked;
  chbOnlyFolder.Enabled := chbOverload.Checked;
end;


procedure TfmRXXEditor.PivotChange(Sender: TObject);
var
  ID: Integer;
begin
  if fLoading then
    Exit;
  //To avoid OnChange misfire when we change selected Item we disable controls
  if not TEdit(Sender).Enabled then Exit;
  if not TryStrToInt(TEdit(Sender).Text, ID) then Exit;

  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  fSprites.RXData.Pivot[ID].x := edtPivotX.Value;
  fSprites.RXData.Pivot[ID].y := edtPivotY.Value;
end;


procedure TfmRXXEditor.UpdateList;
var
  I: Integer;
  prevIndex: Integer;
begin
  prevIndex := lbSpritesList.ItemIndex;

  lbSpritesList.Items.BeginUpdate;
  try
    lbSpritesList.Items.Clear;

    for I := 1 to fSprites.RXData.Count do
    if fSprites.RXData.Flag[I] = 0 then
      lbSpritesList.Items.Add(IntToStr(I) + '.')
    else
      lbSpritesList.Items.Add(Format('%d. %dx%d', [I, fSprites.RXData.Size[I].X, fSprites.RXData.Size[I].Y]));
  finally
    lbSpritesList.Items.EndUpdate;
  end;

  lbSpritesList.ItemIndex := Min(prevIndex, lbSpritesList.Items.Count - 1);

  lbSpritesListClick(Self);
end;


{$IFDEF FPC}
initialization
  {$i RXXEditorForm.lrs}
{$ENDIF}


end.
