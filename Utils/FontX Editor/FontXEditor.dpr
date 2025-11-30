program FontXEditor;
uses
  Forms,
  umain in 'umain.pas' {frmMain},
  KM_ResFonts in '..\..\src\res\KM_ResFonts.pas',
  KM_ResFontsEdit in '..\..\src\res\KM_ResFontsEdit.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas',
  KM_Structure in '..\..\src\structures\KM_Structure.pas',
  KM_StructuresCollection in '..\..\src\structures\KM_StructuresCollection.pas',
  KM_Particles in '..\..\src\weather\KM_Particles.pas',
  KM_Weather in '..\..\src\weather\KM_Weather.pas',
  KM_WeatherCollection in '..\..\src\weather\KM_WeatherCollection.pas',
  KM_WeatherTypes in '..\..\src\weather\KM_WeatherTypes.pas',
  KM_GUIGameHouseArena in '..\..\src\gui\pages_game\house\KM_GUIGameHouseArena.pas',
  KM_GUIGameHouseCartographer in '..\..\src\gui\pages_game\house\KM_GUIGameHouseCartographer.pas',
  KM_GUIGameHouseForest in '..\..\src\gui\pages_game\house\KM_GUIGameHouseForest.pas',
  KM_GUIGameHousePasture in '..\..\src\gui\pages_game\house\KM_GUIGameHousePasture.pas',
  KM_GUIGameHousePearl in '..\..\src\gui\pages_game\house\KM_GUIGameHousePearl.pas';

var
    frmMain: TfrmMain;


{$R *.res}


begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
