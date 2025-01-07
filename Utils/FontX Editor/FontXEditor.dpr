program FontXEditor;
uses
  Forms,
  umain in 'umain.pas' {frmMain},
  KM_ResFonts in '..\..\src\res\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\src\res\KM_ResFontsEdit.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas';

var
    frmMain: TfrmMain;


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
