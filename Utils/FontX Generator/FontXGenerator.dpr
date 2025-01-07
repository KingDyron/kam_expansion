program FontXGenerator;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  Form_Generator in 'Form_Generator.pas' {Form1},
  KM_ResFonts in '..\..\src\res\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\src\res\KM_ResFontsEdit.pas',
  KM_FontXGenerator in 'KM_FontXGenerator.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas';

begin
  Application.Initialize;
  {$IFDEF FPC} Application.MainFormOnTaskbar := True; {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
