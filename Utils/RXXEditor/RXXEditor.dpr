program RXXEditor;
{$I ..\..\KaM_Remake.inc}


uses
  Forms,
  RXXEditorForm in 'RXXEditorForm.pas' {fmRXXEditor},
  KM_ResSprites in '..\..\src\res\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\src\res\KM_ResSpritesEdit.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas';

{$IFDEF WDC}
{$R *.res}
{$ENDIF}

var
  fmRXXEditor: TfmRXXEditor;

begin
  Application.Initialize;
  Application.CreateForm(TfmRXXEditor, fmRXXEditor);
  Application.Run;
end.
