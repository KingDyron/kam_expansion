unit KM_ScriptFilesCollection;
{$I KaM_Remake.inc}
interface
uses
  KM_ScriptingTypes;

type
  // Scripts can be included one into another with PreProcessor directives {$I filename.script} or {$INCLUDE filename.script}
  // This structure collects included files info
  TKMScriptFilesCollection = class
  private
    fMainFileInfo: TKMScriptFileInfo;
    fMainFilePath: UnicodeString;
    fHasDefDirectives: Boolean;
    fIncludedCnt: Integer;
    fIncluded: array of TKMScriptFileInfo;

    function GetIncluded(aIndex: Integer): TKMScriptFileInfo;

  public
    constructor Create;

    property MainFilePath: UnicodeString read fMainFilePath write fMainFilePath;
    procedure SetMainFileInfo(const aFullFilePath: UnicodeString; const aFileText: AnsiString);
    property HasDefDirectives: Boolean read fHasDefDirectives write fHasDefDirectives;

    property Included[I: Integer]: TKMScriptFileInfo read GetIncluded; default;
    property IncludedCount: Integer read fIncludedCnt;
    procedure AddIncludeInfo(const aIncludeInfo: TKMScriptFileInfo);

    procedure StripIncludedCnt;
//    function FindCodeLine(const aLine: AnsiString; out aFileNamesArr: TKMStringArray; out aRowsArr: TIntegerArray): Integer;
  end;

implementation
uses
  SysUtils;


{ TKMScriptFilesCollection }
constructor TKMScriptFilesCollection.Create;
begin
  inherited;

  fIncludedCnt := 0;
  fHasDefDirectives := False;
  SetLength(fIncluded, 8);
end;


procedure TKMScriptFilesCollection.AddIncludeInfo(const aIncludeInfo: TKMScriptFileInfo);
begin
  if Length(fIncluded) >= fIncludedCnt then
    SetLength(fIncluded, fIncludedCnt + 8);

  fIncluded[fIncludedCnt] := aIncludeInfo;
  Inc(fIncludedCnt);
end;


function TKMScriptFilesCollection.GetIncluded(aIndex: Integer): TKMScriptFileInfo;
begin
  Result := fIncluded[aIndex];
end;


procedure TKMScriptFilesCollection.SetMainFileInfo(const aFullFilePath: UnicodeString; const aFileText: AnsiString);
begin
  fMainFileInfo.FullFilePath := aFullFilePath;
  fMainFileInfo.FileName := ExtractFileName(aFullFilePath);
  fMainFileInfo.FileText := aFileText;
end;


procedure TKMScriptFilesCollection.StripIncludedCnt;
begin
  SetLength(fIncluded, fIncludedCnt);
end;


//
////Try to find line of code in all script files
////Returns number of occurences
//function TKMScriptFilesCollection.FindCodeLine(const aLine: AnsiString; out aFileNamesArr: TKMStringArray;
//                                               out aRowsArr: TIntegerArray): Integer;
//
//  procedure AddFoundLineInfo(var aFoundCnt: Integer; const aFileNameFound: String; aRowFound: Integer);
//  begin
//    if (aFoundCnt >= Length(aFileNamesArr))
//      or (aFoundCnt >= Length(aRowsArr)) then
//    begin
//      SetLength(aFileNamesArr, aFoundCnt + 8);
//      SetLength(aRowsArr, aFoundCnt + 8);
//    end;
//
//    aFileNamesArr[aFoundCnt] := aFileNameFound;
//    aRowsArr[aFoundCnt] := aRowFound;
//
//    Inc(aFoundCnt);
//  end;
//
//  procedure FindLine(var aFoundCnt: Integer; const aScriptFileInfo: TKMScriptFileInfo; var aStrings: TStringList);
//  var
//    I: Integer;
//  begin
//    aStrings.Clear;
//    aStrings.Text := aScriptFileInfo.FileText;
//
//    //Find all occurences of aLine in FileText
//    for I := 0 to aStrings.Count - 1 do
//      if aStrings[I] = aLine then
//        AddFoundLineInfo(aFoundCnt, aScriptFileInfo.FileName, I + 1);
//  end;
//
//var
//  strings: TStringList;
//  I, aFoundCnt: Integer; // Same name as a parameter?
//begin
//  strings := TStringList.Create; // Create TStringList only once for all files
//
//  aFoundCnt := 0;
//  //Find in main script file first
//  FindLine(aFoundCnt, fMainFileInfo, strings);
//
//  for I := 0 to fIncludedCnt - 1 do
//    //then find in included script files
//    FindLine(aFoundCnt, fIncluded[I], strings);
//
//  Result := aFoundCnt;
//  strings.Free;
//end;


end.
