program TranslationManager;
{$I ..\..\KaM_Remake.inc}
uses
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Forms,

  Unit1 in 'Unit1.pas' {Form1},
  Unit_PathManager in 'Unit_PathManager.pas',
  Unit_Text in 'Unit_Text.pas',
  TranslationManagerUtils in 'TranslationManagerUtils.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

var
  Form1: TForm1;

begin
  Application.Initialize;

  if GetWorkDir(True) = '' then
    Exit;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
