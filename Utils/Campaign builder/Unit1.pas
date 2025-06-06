unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, Classes, ComCtrls, Controls, Dialogs, ExtDlgs, ExtCtrls, Forms,
  Graphics, Mask, Math, Spin, StdCtrls, SysUtils,
  KM_Defaults, KM_Campaigns, KM_ResSpritesEdit, KromUtils, inifiles,
  KM_CampaignTypes, Vcl.ColorGrd, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    tvList: TTreeView;
    btnSaveCMP: TButton;
    btnLoadCMP: TButton;
    btnLoadPicture: TButton;
    seMapCount: TSpinEdit;
    Label1: TLabel;
    seNodeCount: TSpinEdit;
    Label2: TLabel;
    dlgOpenPicture: TOpenDialog;
    dlgOpenCampaign: TOpenDialog;
    dlgSaveCampaign: TSaveDialog;
    Bevel1: TBevel;
    Label6: TLabel;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    imgBlackFlag: TImage;
    imgRedFlag: TImage;
    imgNode: TImage;
    rgBriefingPos: TRadioGroup;
    edtShortName: TMaskEdit;
    shpBriefing: TShape;
    Bevel2: TBevel;
    cbShowNodeNumbers: TCheckBox;
    Bevel3: TBevel;
    GroupBox1: TGroupBox;
    imgNewFlag: TImage;
    imgNewNode: TImage;
    cbShowBriefingPosition: TCheckBox;
    edtName: TEdit;
    Label3: TLabel;
    btnUnloadCMP: TButton;
    Label4: TLabel;
    eUnlockAfter: TEdit;
    eFlagColor: TEdit;
    Label5: TLabel;
    imgRedFlag2: TImage;
    imgBlackFlag2: TImage;
    procedure btnLoadPictureClick(Sender: TObject);
    procedure btnLoadCMPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure seMapCountChange(Sender: TObject);
    procedure seNodeCountChange(Sender: TObject);
    procedure btnSaveCMPClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgBriefingPosClick(Sender: TObject);
    procedure edtShortNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure edtShortNameKeyPress(Sender: TObject; var Key: Char);
    procedure cbShowNodeNumbersClick(Sender: TObject);
    procedure NewObjectImgDblClick(Sender: TObject);
    procedure Image1DragOver(Sender, Source: TObject; X, Y: Integer;
                             State: TDragState; var Accept: Boolean);
    procedure Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure NewObjectImgMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure cbShowBriefingPositionClick(Sender: TObject);
    procedure btnUnloadCMPClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure eUnlockAfterChange(Sender: TObject);
    procedure eFlagColorChange(Sender: TObject);
  private
    fExePath: string;
    fCampaignsPath: string;
    fSprites: TKMSpritePackEdit;
    imgFlags: array of TImage;
    imgNodes: array of TImage;

    fUpdating: Boolean;
    fSelectedMap: Integer;
    fSelectedNode: Integer;
    PrevX, PrevY: Integer;

    procedure LoadCmp(aFileName : String);

    function DlgQuestionShow(aCaption, aMsg: string): boolean;

    function GetCharset(aLang: string): TFontCharset;
    procedure LoadCampaignName(aFileName, aLocale: string);
    procedure SaveCampaignName(aFileName: string);
    procedure CreateDefaultLocaleLibxTemplate(aFileName: string);
  public
    procedure FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FlagEnter(Sender: TObject);
    procedure NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeEnter(Sender: TObject);

    procedure FlagNodeLeave(Sender: TObject);

    procedure SelectMap;
    procedure RefreshBackground;
    procedure RefreshFlags;
    procedure RefreshNodes;
    procedure UpdateList;
    procedure UpdateFlagCount;
    procedure UpdateNodeCount;
    procedure DrawFlagNumber(aIndexMap: Integer);
    procedure DrawNodeNumber(aIndexNode: Integer);

  end;

var
  Form1: TForm1;
  C: TKMCampaign;
  Locale: String;
implementation
{$R *.dfm}
uses
  KM_ResTypes, KM_ResSprites;


procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True; //Makes images drag around smoothly
  ScrollBox1.DoubleBuffered := True;

  Caption := string('Campaign Builder (' + GAME_REVISION + ')');

  fExePath := ExtractFilePath(ParamStr(0));
  fCampaignsPath := ExpandFileName(fExePath + '..\..\Campaigns\');

  Locale := 'eng';

  C := TKMCampaign.Create;
  fSelectedMap := -1;

  imgNode.Canvas.Font.Name := 'Verdana';
  imgNode.Canvas.Font.Style := [fsBold];
  imgNode.Canvas.Font.Size := 5;
  imgNode.Canvas.Font.Color := clWhite;

  imgBlackFlag.Canvas.Font.Name := 'Verdana';
  imgBlackFlag.Canvas.Font.Style := [fsBold];
  imgBlackFlag.Canvas.Font.Size := 8;
  imgBlackFlag.Canvas.Font.Color := clWhite;

  imgRedFlag.Canvas.Font.Name := 'Verdana';
  imgRedFlag.Canvas.Font.Style := [fsBold];
  imgRedFlag.Canvas.Font.Size := 8;
  imgRedFlag.Canvas.Font.Color := clWhite;

  //This line corrects a bug in UpdateList namely C.CampName line that returns at the start of the program #0#0#0
  edtShortNameChange(nil);

  seMapCount.MaxValue := MAX_CAMP_MAPS;
  seNodeCount.MaxValue := MAX_CAMP_NODES;

  fSprites := TKMSpritePackEdit.Create(rxCustom, nil);

  seMapCountChange(nil); //Initialise it to 1 map

  cbShowBriefingPositionClick(nil);

  if FileExists(ParamStr(1)) then
    LoadCmp(ParamStr(1));
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fSprites.Free;
end;


procedure TForm1.LoadCmp(aFileName : String);
var
  I: Integer;
begin
  C.LoadFromFile(aFileName);

  fSprites.Free;
  fSprites := TKMSpritePackEdit.Create(rxCustom, nil);
  if FileExists(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx') then
    fSprites.LoadFromRXXFile(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx')
  else
    ShowMessage('Campaign background image (images.rxx) could not be found');

  fSelectedMap := -1;
  fSelectedNode := -1;

  edtShortName.Text := C.ShortName;
  seMapCount.Value := C.MapCount;

  UpdateList;
  UpdateFlagCount;
  RefreshBackground;
  RefreshFlags;

  //Hide nodes that might have been open from the last campaign
  for I := 0 to Length(imgNodes) - 1 do
    imgNodes[I].Visible := False;

end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Img: TImage;
begin
  Img := nil;

  if (fSelectedMap <> -1) then
    if (fSelectedNode <> -1) then
      Img := imgNodes[fSelectedNode]
    else
      Img := imgFlags[fSelectedMap];

  if Img = nil then Exit;

  case Key of
    Ord('D'): Img.Left := Img.Left + 1;
    Ord('A'): Img.Left := Img.Left - 1;
    Ord('W'): Img.Top  := Img.Top  - 1;
    Ord('S'): Img.Top  := Img.Top  + 1;
  end;
  Img.Left := EnsureRange(Img.Left, Image1.Left, Image1.Left + 1024-Img.Width);
  Img.Top  := EnsureRange(Img.Top, Image1.Top, Image1.Top + 768-Img.Height);
  if (fSelectedNode <> -1) then
  begin
    //Position node centers, so that if someone changes the nodes they still look correct
    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left - Image1.Left + Img.Width div 2;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  - Image1.Top + Img.Height div 2;
  end
  else
  begin
    C.Maps[fSelectedMap].Flag.X := Img.Left - Image1.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - Image1.Top;
  end;

  StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
end;


procedure TForm1.Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  curItem: Integer;
begin
  if fUpdating then Exit;

  if Source = imgNewFlag then
  begin
    curItem          := seMapCount.Value;
    seMapCount.Value := curItem + 1;
    C.MapCount       := EnsureRange(seMapCount.Value, 1, MAX_CAMP_MAPS);

    C.Maps[C.MapCount - 1].Flag.X := EnsureRange(X - Image1.Left - ScrollBox1.HorzScrollBar.ScrollPos, 0, 1024 - imgNewFlag.Width);
    C.Maps[C.MapCount - 1].Flag.Y := EnsureRange(Y - Image1.Top - ScrollBox1.VertScrollBar.ScrollPos, 0, 768 - imgNewFlag.Height);

    fSelectedMap := C.MapCount - 1; //Always select last, just added MapFlag
  end else if (fSelectedMap <> -1) and (Source = imgNewNode) then
  begin
    curItem                        := seNodeCount.Value;
    seNodeCount.Value              := curItem + 1;
    C.Maps[fSelectedMap].NodeCount := EnsureRange(seNodeCount.Value, 0, MAX_CAMP_NODES);

    C.Maps[fSelectedMap].Nodes[curItem].X := (X - Image1.Left - ScrollBox1.HorzScrollBar.ScrollPos);
    C.Maps[fSelectedMap].Nodes[curItem].Y := (Y - Image1.Top - ScrollBox1.VertScrollBar.ScrollPos);

    fSelectedNode := C.Maps[fSelectedMap].NodeCount - 1; //Always select last, just added Node
  end;

  UpdateList;
  UpdateFlagCount;
  UpdateNodeCount;
  RefreshFlags;
end;


procedure TForm1.Image1DragOver(Sender, Source: TObject; X, Y: Integer;
                                State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = imgNewFlag) or
            ((fSelectedMap <> -1) and (Source = imgNewNode));
end;


procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //Test function for catching bugs
  StatusBar1.Panels[3].Text := 'X:' + IntToStr(X) + ' x Y: ' + IntToStr(Y);
end;


procedure TForm1.NewObjectImgMouseDown(Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
begin
  TImage(Sender).BeginDrag(False, 5);
end;


procedure TForm1.NewObjectImgDblClick(Sender: TObject);
begin
  if fUpdating then Exit;

  if Sender = imgNewFlag then
  begin
    seMapCount.Value := seMapCount.Value + 1;
    C.MapCount       := EnsureRange(seMapCount.Value, 1, MAX_CAMP_MAPS);

    if fSelectedMap > C.MapCount - 1 then
      fSelectedMap := -1;
  end else if (fSelectedMap <> -1) and (Sender = imgNewNode) then
  begin
    seNodeCount.Value              := seNodeCount.Value + 1;
    C.Maps[fSelectedMap].NodeCount := EnsureRange(seNodeCount.Value, 0, MAX_CAMP_NODES);

    if fSelectedNode > C.Maps[fSelectedMap].NodeCount - 1 then
      fSelectedNode := -1;
  end;

  UpdateList;
  UpdateFlagCount;
  UpdateNodeCount;
  RefreshFlags;
end;


procedure TForm1.FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fSelectedMap := TImage(Sender).Tag;
    fSelectedNode := -1;
    SelectMap;

    PrevX := X;
    PrevY := Y;
  end;
end;


procedure TForm1.FlagEnter(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := 'Map #' + IntToStr(TImage(Sender).Tag + 1);
end;


procedure TForm1.FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Img: TImage;
begin
  if (ssLeft in Shift) and (TImage(Sender).Tag = fSelectedMap) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), Image1.Left, Image1.Left + 1024-Img.Width);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), Image1.Top, Image1.Top + 768-Img.Height);

    C.Maps[fSelectedMap].Flag.X := Img.Left - Image1.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - Image1.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
end;


procedure TForm1.NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fSelectedNode := TImage(Sender).Tag;
    SelectMap;

    PrevX := X;
    PrevY := Y;
  end;
end;


procedure TForm1.NodeEnter(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := 'Node #' + IntToStr(TImage(Sender).Tag + 1);
end;


procedure TForm1.FlagNodeLeave(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := '';
end;


procedure TForm1.NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Img: TImage;
begin
  if (ssLeft in Shift) and (fSelectedMap <> -1) and (fSelectedNode <> -1) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), Image1.Left, Image1.Left + 1024-Img.Width);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), Image1.Top, Image1.Top + 768-Img.Height);

    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left + Img.Width div 2  - Image1.Left;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  + Img.Height div 2 - Image1.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
end;


function TForm1.GetCharset(aLang: string): TFontCharset;
begin
  if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
    Result := RUSSIAN_CHARSET
  else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
    Result := EASTEUROPE_CHARSET
  else if Pos(aLang, 'tur') <> 0 then
    Result := TURKISH_CHARSET
  else if Pos(aLang, 'lit,lat') <> 0 then
    Result := BALTIC_CHARSET
  else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
    Result := ANSI_CHARSET
  else
    Result := DEFAULT_CHARSET;
end;


procedure TForm1.LoadCampaignName(aFileName, aLocale: string);
var
  LibxFile: TStringList;
  I : Integer;
  VarText : String;
  VarIndex: Integer;
begin
  if not FileExists(Format(aFileName, [aLocale])) then Exit;
  LibxFile := TStringList.Create;
  LibxFile.LoadFromFile(Format(aFileName, [aLocale]));
  edtName.Font.Charset := GetCharset(aLocale);

  for I := 0 to LibxFile.Count - 1 do
  begin
    VarText := LibxFile.Strings[I];
    VarIndex := Pos('0:', VarText);
    if VarIndex > 0 then
    begin
      edtName.Text := Copy(VarText, VarIndex + 2, Length(VarText));
      Break;
    end;
  end;
  LibxFile.Free;
end;


procedure TForm1.SaveCampaignName(aFileName: string);
var
  LibxFile: TStringList;
  I: Integer;
begin
  LibxFile := TStringList.Create;
  LibxFile.LoadFromFile(aFileName);
  for I := 0 to LibxFile.Count - 1 do
    if Pos('0:', LibxFile.Strings[I]) > 0 then
    begin
      LibxFile.Strings[I] := '0:' + edtName.Text;
      Break;
    end;
  LibxFile.SaveToFile(aFileName);
  LibxFile.Free;
end;

procedure TForm1.CreateDefaultLocaleLibxTemplate(aFileName: string);
var
  LibxFile: TextFile;
  I: Integer;
begin
  AssignFile(LibxFile, aFileName);
  try
    ReWrite(LibxFile);

    Writeln(LibxFile, '');
    Writeln(LibxFile, 'MaxID:' + IntToStr(C.MapCount + 9) + EolW);
    Writeln(LibxFile, '0:' + edtName.Text);
    Writeln(LibxFile, '1:Mission %d');
    Writeln(LibxFile, '2:Campaign description');
    for I := 0 to C.MapCount-1 do
      Writeln(LibxFile, IntToStr(10 + I) + ':Mission description ' + IntToStr(I + 1));
  finally
    CloseFile(LibxFile);
  end;
end;


procedure TForm1.DrawNodeNumber(aIndexNode: Integer);
var
  txtWidth, txtHeight, txtLeft, txtTop: Integer;
begin
  if not cbShowNodeNumbers.Checked then Exit;

  txtWidth := imgNodes[aIndexNode].Canvas.TextWidth(IntToStr(aIndexNode +1));
  txtHeight := imgNodes[aIndexNode].Canvas.TextHeight(IntToStr(aIndexNode +1));
  txtLeft := (imgNodes[aIndexNode].Width - txtWidth) div 2;
  txtTop := (imgNodes[aIndexNode].Height - txtHeight) div 2;

  SetBkMode(imgNodes[aIndexNode].Canvas.Handle, TRANSPARENT);
  imgNodes[aIndexNode].Canvas.TextOut(txtLeft, txtTop, IntToStr(aIndexNode +1));
end;


procedure TForm1.DrawFlagNumber(aIndexMap: Integer);
const
  OFF: array [Boolean] of TPoint = ((X:-3; Y:-2), (X:-1; Y:-2));
var
  txtWidth, txtHeight, txtLeft, txtTop: Integer;
  isRedFlag: Boolean;
begin
  if not cbShowNodeNumbers.Checked then Exit;

  isRedFlag := aIndexMap <= fSelectedMap;

  txtWidth := imgFlags[aIndexMap].Canvas.TextWidth(IntToStr(aIndexMap +1));
  txtHeight := imgFlags[aIndexMap].Canvas.TextHeight(IntToStr(aIndexMap +1));
  txtLeft := (imgFlags[aIndexMap].Width - txtWidth) div 2 + OFF[isRedFlag].X;
  txtTop := (imgFlags[aIndexMap].Height - txtHeight) div 2 + OFF[isRedFlag].Y;

  SetBkMode(imgFlags[aIndexMap].Canvas.Handle, TRANSPARENT);
  imgFlags[aIndexMap].Canvas.TextOut(txtLeft, txtTop, IntToStr(aIndexMap + 1));
end;


procedure TForm1.btnSaveCMPClick(Sender: TObject);
begin
  if C.MapCount < 2 then
  begin
    ShowMessage('Campaign must have at least 2 missions');
    Exit;
  end;

  if Length(Trim(C.ShortName)) <> 3 then
  begin
    ShowMessage('Campaign short title must be 3 characters');
    Exit;
  end;

  dlgSaveCampaign.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);

  dlgSaveCampaign.FileName := 'info';

  if not dlgSaveCampaign.Execute then Exit;

  C.SaveToFile(dlgSaveCampaign.FileName);

  // Campaign might have no images yet
  //@Rey: If campaign images ar cleared by mapmaker we should delete the rxx then
  if not fSprites.IsEmpty then
    fSprites.SaveToRXXFile(ExtractFilePath(dlgSaveCampaign.FileName) + 'images.rxx', TKMRXXFormat.rxxOne);

  if FileExists(ExtractFilePath(dlgSaveCampaign.FileName) +
  Format(TEMPLATE_LIBX_FILE_TEXT, [Locale])) then
    SaveCampaignName(ExtractFilePath(dlgSaveCampaign.FileName) +
      Format(TEMPLATE_LIBX_FILE_TEXT, [Locale]))
  else
    CreateDefaultLocaleLibxTemplate(ExtractFilePath(dlgSaveCampaign.FileName) +
      Format(TEMPLATE_LIBX_FILE_TEXT, [Locale]));
end;


procedure TForm1.cbShowBriefingPositionClick(Sender: TObject);
begin
  shpBriefing.Visible := cbShowBriefingPosition.Checked;
end;


procedure TForm1.cbShowNodeNumbersClick(Sender: TObject);
begin
  RefreshFlags;
end;


function  TForm1.DlgQuestionShow(aCaption, aMsg: string): boolean;
var
  VarBool: boolean;
begin
  {$IFDEF MSWindows}
  if MessageBox(Handle, PChar(aCaption), PChar(aMsg), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = ID_YES then
    VarBool := true
  else
    VarBool := false;
  {$ENDIF}
  {$IFDEF Unix}
  if MessageDlg(aCaption, aMsg, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes then
    VarBool := true
  else
    VarBool := false;
  {$ENDIF}
  Result := VarBool;
end;


procedure TForm1.btnUnloadCMPClick(Sender: TObject);
var I: Integer;
begin
  if DlgQuestionShow('Unsaved data will be lost. Are you sure?', Self.Caption) then
  begin
    C.Free;
    fSprites.Free;
    Image1.Picture := nil;

    C := TKMCampaign.Create;
    fSprites := TKMSpritePackEdit.Create(rxCustom, nil);

    fSelectedMap := -1;

    edtName.Clear;
    edtShortName.Clear;

    seMapCount.Value := 1;
    seNodeCount.Value := 0;

    edtShortNameChange(nil);
    seMapCountChange(nil);

    for I := 0 to Length(imgNodes) - 1 do
      imgNodes[I].Visible := False;
  end;
end;

procedure TForm1.btnLoadCMPClick(Sender: TObject);
var Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(fExePath + '..\..\KaM_Remake_Settings.ini');
  Locale := Ini.ReadString('Game', 'Locale', 'eng');
  Ini.Free;

  if DirectoryExists(fCampaignsPath) then
    dlgOpenCampaign.InitialDir := fCampaignsPath
  else
    dlgOpenCampaign.InitialDir := fExePath;

  if not dlgOpenCampaign.Execute then Exit;
  
  LoadCmp(dlgOpenCampaign.FileName);

  LoadCampaignName(ExtractFilePath(dlgOpenCampaign.FileName) + TEMPLATE_LIBX_FILE_TEXT, Locale);
  if Length(edtName.Text) = 0 then
    LoadCampaignName(ExtractFilePath(dlgOpenCampaign.FileName) + TEMPLATE_LIBX_FILE_TEXT , 'eng');
end;


procedure TForm1.btnLoadPictureClick(Sender: TObject);
begin
  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);

  if not dlgOpenPicture.Execute then Exit;
  try
    fSprites.AddImage(ExtractFilePath(dlgOpenPicture.FileName),
                      ExtractFileName(dlgOpenPicture.FileName), 1);
    RefreshBackground;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;


end;


procedure TForm1.edtShortNameChange(Sender: TObject);
var
  cmp: TKMCampaignId;
begin
  if Length(Trim(edtShortName.Text)) = 3 then
  begin
    if fUpdating then Exit;

    cmp[0] := Ord(edtShortName.Text[1]);
    cmp[1] := Ord(edtShortName.Text[2]);
    cmp[2] := Ord(edtShortName.Text[3]);
    C.CampaignId := cmp;

    //Shortname may be used as mapname in List
    UpdateList;
  end;
end;


// Allow only Eng characters
procedure TForm1.edtShortNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['A'..'Z', 'a'..'z', #8 {Backspace}]) then
  begin
    Beep;
    Key := #0;
  end;
end;


procedure TForm1.eFlagColorChange(Sender: TObject);
begin
  if fSelectedMap = -1 then  Exit;

  C.Maps[fSelectedMap].FlagColor := StrToUInt64('$FF' + Copy(eFlagColor.Text, 2, 10));

end;

procedure TForm1.eUnlockAfterChange(Sender: TObject);
var lastIndex, I, J, K : Integer;
  S, S2 : String;
  arr : array[0..4] of Integer;
  strParam : String;
begin
  if fSelectedMap = -1 then Exit;

  lastIndex := 1;
  J := 0;
  S := eUnlockAfter.Text;


  K := 1;
  for I := 0 to 4 do
    arr[I] := 0;
  for I := 0 to 4 do
  begin
    if K > length(S) then
      Break;

    strParam := '';
    while (K <= length(S)) and (S[K] <> ',') and (S[K] <> '#32') do//stop if there is space or ,
    begin
      strParam := strParam + S[K];

      inc(K);
    end;
    if strParam = '' then
      Continue;

    if TryStrToInt(strParam, J) then
      arr[I] := J;
    inc(K);
  end;
  strParam := '';
  for I := 0 to 4 do
    strParam := strParam + IntToStr(arr[I]) + ',';

  for I := 0 to 4 do
    C.Maps[fSelectedMap].UnlockAfter[I] := arr[I];

end;

procedure TForm1.seMapCountChange(Sender: TObject);
begin
  if fUpdating then Exit;

  C.MapCount := EnsureRange(seMapCount.Value, 1, MAX_CAMP_MAPS);

  if fSelectedMap > C.MapCount - 1 then
    fSelectedMap := -1;

  UpdateList;
  UpdateFlagCount;
  RefreshFlags;
end;


procedure TForm1.seNodeCountChange(Sender: TObject);
begin
  if fUpdating or (fSelectedMap = -1) then Exit;

  C.Maps[fSelectedMap].NodeCount := EnsureRange(seNodeCount.Value, 0, MAX_CAMP_NODES);

  fSelectedNode := Min(fSelectedNode, C.Maps[fSelectedMap].NodeCount - 1);

  UpdateList;
  UpdateNodeCount;
  RefreshFlags;
end;

procedure TForm1.rgBriefingPosClick(Sender: TObject);
begin
  if fUpdating or (fSelectedMap = -1) then Exit;

  C.Maps[fSelectedMap].TextPos := TKMBriefingCorner(rgBriefingPos.ItemIndex);

  RefreshFlags;
end;


procedure TForm1.RefreshBackground;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    fSprites.GetImageToBitmap(1, Bmp, nil);
    Image1.Picture.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;


procedure TForm1.RefreshFlags;
var
  I: Integer;
begin
  for I := 0 to C.MapCount - 1 do
  begin
    imgFlags[I].Left := C.Maps[I].Flag.X + Image1.Left;
    imgFlags[I].Top := C.Maps[I].Flag.Y + Image1.Top;
    if I > fSelectedMap then
    begin
      if (C.Maps[I].FlagStyle = 1)
       then
        imgFlags[I].Picture.Bitmap := imgBlackFlag2.Picture.Bitmap
      else
        imgFlags[I].Picture.Bitmap := imgBlackFlag.Picture.Bitmap;

      imgFlags[I].Canvas.Font := imgBlackFlag.Canvas.Font;
    end else
    begin
      if (C.Maps[I].FlagStyle = 1) then
        imgFlags[I].Picture.Bitmap := imgRedFlag2.Picture.Bitmap
      else
        imgFlags[I].Picture.Bitmap := imgRedFlag.Picture.Bitmap;

      imgFlags[I].Canvas.Font := imgRedFlag.Canvas.Font;
    end;
    DrawFlagNumber(I);
  end;

  //In some versions of Delphi there is a bug with the Component's position on ScrollBox
  //This is for the Fix this bug. Where this is not a bug, it will not hurt and there where there is isapravit
  //Personally, I have Top components ScrollBox = - ScrollBox1.VertScrollBar.Position
  //Left and also
  shpBriefing.Top := Image1.Height - shpBriefing.Height + Image1.Top;

  if fSelectedMap = -1 then
  begin
    rgBriefingPos.Enabled := False;
    Exit;
  end;

  RefreshNodes;

  shpBriefing.Left := IfThen(C.Maps[fSelectedMap].TextPos = bcBottomRight, Image1.Width - shpBriefing.Width, 0) + Image1.Left;
end;


procedure TForm1.RefreshNodes;
var
  I: Integer;
begin
  for I := 0 to C.Maps[fSelectedMap].NodeCount - 1 do
  begin
    // Refresh canvas in case we have spoiled it with node number
    imgNodes[I].Picture.Bitmap := imgNode.Picture.Bitmap;
    imgNodes[I].Canvas.Font := imgNode.Canvas.Font;

    // Position node centers, so that if someone changes the nodes they still look correct
    imgNodes[I].Left := Image1.Left + C.Maps[fSelectedMap].Nodes[I].X - imgNodes[I].Width div 2;
    imgNodes[I].Top := Image1.Top + C.Maps[fSelectedMap].Nodes[I].Y - imgNodes[I].Height div 2;
    imgNodes[I].Left := EnsureRange(imgNodes[I].Left, Image1.Left, Image1.Left + 1024-imgNodes[I].Width);
    imgNodes[I].Top  := EnsureRange(imgNodes[I].Top, Image1.Top, Image1.Top + 768-imgNodes[I].Height);
    DrawNodeNumber(I);
  end;
end;

procedure TForm1.UpdateList;
var
  I, K: Integer;
  N, SN: TTreeNode;
begin
  fUpdating := True;

  tvList.Items.Clear;

  for I := 0 to C.MapCount - 1 do
  begin
    N := tvList.Items.AddChild(nil, C.ShortName + ' mission ' + IntToStr(I + 1));
    if fSelectedMap = I then
      N.Selected := True;

    for K := 0 to C.Maps[I].NodeCount - 1 do
    begin
      SN := tvList.Items.AddChild(N, 'node ' + IntToStr(K + 1));
      if (fSelectedMap = I) and (fSelectedNode = K) then
        SN.Selected := True;
    end;
  end;

  fUpdating := False;
end;


procedure TForm1.tvListChange(Sender: TObject; Node: TTreeNode);
begin
  if fUpdating then Exit;

  if Node.Parent = nil then
  begin
    fSelectedMap := Node.Index;
    fSelectedNode := -1;
  end
  else
  begin
    fSelectedMap := Node.Parent.Index;
    fSelectedNode := Node.Index;
  end;

  SelectMap;
end;


procedure TForm1.UpdateFlagCount;
var
  I: Integer;
begin
  //Create more flags if needed
  if C.MapCount > Length(imgFlags) then
  begin
    SetLength(imgFlags, C.MapCount);

    for I := 0 to C.MapCount - 1 do
    if imgFlags[I] = nil then
    begin
      imgFlags[I] := TImage.Create(Image1);
      imgFlags[I].Parent := ScrollBox1;
      imgFlags[I].AutoSize := True;
      imgFlags[I].Transparent := True;
      imgFlags[I].Tag := I;
      imgFlags[I].OnMouseDown := FlagDown; //Start drag
      imgFlags[I].OnMouseMove := FlagMove; //Drag
      imgFlags[I].OnMouseEnter := FlagEnter; //Hint
      imgFlags[I].OnMouseLeave := FlagNodeLeave; //Clear Hint
    end;
  end;

  // Hide unused flags
  for I := 0 to Length(imgFlags) - 1 do
    imgFlags[I].Visible := (I <= C.MapCount - 1);
end;


procedure TForm1.UpdateNodeCount;
var
  I: Integer;
begin
  if fSelectedMap = -1 then Exit;

  //Create nodes
  if C.Maps[fSelectedMap].NodeCount > Length(imgNodes) then
  begin
    SetLength(imgNodes, C.Maps[fSelectedMap].NodeCount);

    for I := 0 to C.Maps[fSelectedMap].NodeCount - 1 do
    if imgNodes[I] = nil then
    begin
      imgNodes[I] := TImage.Create(Image1);
      imgNodes[I].Parent := ScrollBox1;
      imgNodes[I].AutoSize := True;
      imgNodes[I].Transparent := True;
      imgNodes[I].Picture.Bitmap := imgNode.Picture.Bitmap;
      imgNodes[I].Canvas.Font := imgNode.Canvas.Font;
      imgNodes[I].Tag := I;
      imgNodes[I].OnMouseDown := NodeDown; //Start drag
      imgNodes[I].OnMouseMove := NodeMove; //Drag
      imgNodes[I].OnMouseEnter := NodeEnter; //Hint
      imgNodes[I].OnMouseLeave := FlagNodeLeave; //Clear Hint
    end;
  end;

  // Hide unused nodes
  for I := 0 to Length(imgNodes) - 1 do
    imgNodes[I].Visible := (I <= C.Maps[fSelectedMap].NodeCount - 1);
end;


procedure TForm1.SelectMap;
var
  I, M, N: Integer;
  finS : string;
begin
  if fUpdating then Exit;
  //ColorDialog1.Execute;
  fUpdating := True;
  //Try to select map in TreeList
  M := -1;
  N := -1;
  for I := 0 to tvList.Items.Count - 1 do
  begin
    if tvList.Items[I].Parent = nil then
    begin
      Inc(M);
      N := -1; //Reset node count
    end
    else
      Inc(N);

    if (M = fSelectedMap) and (N = fSelectedNode) then
      tvList.Items[I].Selected := True;
  end;

  rgBriefingPos.Enabled := True;

  seNodeCount.Value := C.Maps[fSelectedMap].NodeCount;
  rgBriefingPos.ItemIndex := Byte(C.Maps[fSelectedMap].TextPos);
  //eUnlockAfter.Text := '';
  finS := '';
  for I := 0 to 4 do
  begin
    finS := finS + IntToStr(C.Maps[fSelectedMap].UnlockAfter[I]) + ',';
  end;

  eUnlockAfter.Text := finS;

  eFlagColor.Text := '$' + Copy(IntToHex(C.Maps[fSelectedMap].FlagColor), 3, 10);
  //Update map info
  StatusBar1.Panels[0].Text := 'Selected map: ' + IntToStr(fSelectedMap +1) + '/' + IntToStr(fSelectedNode +1);

  fUpdating := False;

  UpdateNodeCount;
  RefreshFlags;
end;

end.
