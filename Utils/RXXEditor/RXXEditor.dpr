program RXXEditor;
{$I ..\..\KaM_Remake.inc}


uses
  Forms,
  RXXEditorForm in 'RXXEditorForm.pas' {fmRXXEditor},
  KM_ResSprites in '..\..\src\res\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\src\res\KM_ResSpritesEdit.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas',
  KM_Structure in '..\..\src\structures\KM_Structure.pas',
  KM_StructuresCollection in '..\..\src\structures\KM_StructuresCollection.pas',
  KM_Particles in '..\..\src\weather\KM_Particles.pas',
  KM_Weather in '..\..\src\weather\KM_Weather.pas',
  KM_WeatherCollection in '..\..\src\weather\KM_WeatherCollection.pas',
  KM_WeatherTypes in '..\..\src\weather\KM_WeatherTypes.pas';

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
