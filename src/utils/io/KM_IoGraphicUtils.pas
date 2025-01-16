unit KM_IoGraphicUtils;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes
  {$IFDEF MSWindows}, Windows {$ENDIF}
  {$IFDEF WDC}, Vcl.Graphics, Vcl.Imaging.JPEG, Vcl.Imaging.PNGImage {$ENDIF} //Lazarus doesn't have JPEG library yet -> FPReadJPEG?
  ;

  procedure SavePixelDataToFile(const aFilePath: string; aImageType: TKMImageType; aWidth, aHeight: Integer; var aPixelData: TKMCardinalArray);

implementation
uses
  SysUtils, KM_FileIO
{$IFDEF WDC}, Vcl.Dialogs, KM_Log{$ENDIF}
  ;


procedure SavePixelDataToFile(const aFilePath: string; aImageType: TKMImageType; aWidth, aHeight: Integer; var aPixelData: TKMCardinalArray);
{$IFDEF WDC}
var
  filePath: string;
  mkbmp: TBitmap;
  jpg: TJpegImage;
  png: TPngImage;
{$ENDIF}
begin
{$IFDEF WDC}
  ForceDirectories(ExtractFilePath(aFilePath));
  filePath := ChangeFileExt(aFilePath, IMAGE_TYPE_EXT[aImageType]);

  mkbmp := TBitmap.Create;
  try
    try
      mkbmp.Handle := CreateBitmap(aWidth, aHeight, 1, 32, @aPixelData[0]);

      case aImageType of
        itJpeg: begin
                  jpg := TJpegImage.Create;
                  try
                    jpg.Assign(mkbmp);
                    jpg.ProgressiveEncoding := True;
                    jpg.ProgressiveDisplay  := True;
                    jpg.Performance         := jpBestQuality;
                    jpg.CompressionQuality  := 90;
                    jpg.Compress;
                    jpg.SaveToFile(filePath);
                  finally
                    jpg.Free;
                  end;
                end;
        itPng:  begin
                  png := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, aWidth, aHeight);
                  try
                    png.Assign(mkbmp);
                    png.SaveToFile(filePath);
                  finally
                    png.Free;
                  end;
                end;
        itBmp:  mkbmp.SaveToFile(filePath);
      end;
    except
      on E: Exception do
      begin
        gLog.AddTime('Error while saving image: ' + E.ClassName + ': ' + E.Message);

        ShowMessage('Error saving image. Try to reduce max image size or choose other image format');

        if FileExists(filePath) then
          KMDeleteFile(filePath);
      end;
    end;
  finally
    mkbmp.Free;
  end;
{$ENDIF}
end;


end.
