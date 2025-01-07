unit KM_MapUtilsExt;
{$I KaM_Remake.inc}
interface
uses
  KM_Maps;

  function TryOpenMapPDF(aMap: TKMMapInfo): Boolean;

implementation
uses
  KM_CommonShellUtils;


function TryOpenMapPDF(aMap: TKMMapInfo): Boolean;
var
  pdfPath: string;
begin
  Result := False;
  if aMap = nil then Exit;

  pdfPath := aMap.DetermineReadmeFilePath;
  if pdfPath <> '' then
    Result := ShellOpenFile(pdfPath);
end;

end.
