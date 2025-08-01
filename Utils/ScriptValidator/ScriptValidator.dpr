program ScriptValidator;
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Forms,
  KM_Defaults,
  Unit1 in 'Unit1.pas' {Form1},
  ValidatorTypes in 'ValidatorTypes.pas',
  ConsoleMain in 'ConsoleMain.pas',
  KM_JsonData in '..\..\src\json\KM_JsonData.pas',
  KM_JsonHelpers in '..\..\src\json\KM_JsonHelpers.pas',
  KM_JSONUtils in '..\..\src\json\KM_JSONUtils.pas',
  KM_Structure in '..\..\src\structures\KM_Structure.pas',
  KM_StructuresCollection in '..\..\src\structures\KM_StructuresCollection.pas',
  KM_HouseArena in '..\..\src\houses\KM_HouseArena.pas',
  KM_GUIGameHouse in '..\..\src\gui\pages_game\KM_GUIGameHouse.pas',
  KM_InterfaceGamePlay in '..\..\src\gui\KM_InterfaceGamePlay.pas',
  KM_Game in '..\..\src\game\KM_Game.pas',
  KM_GUIGameHouseArena in '..\..\src\gui\pages_game\house\KM_GUIGameHouseArena.pas',
  KM_GUIGameHouseCartographer in '..\..\src\gui\pages_game\house\KM_GUIGameHouseCartographer.pas',
  KM_GUIGameHouseForest in '..\..\src\gui\pages_game\house\KM_GUIGameHouseForest.pas',
  KM_GUIGameHousePasture in '..\..\src\gui\pages_game\house\KM_GUIGameHousePasture.pas',
  KM_GUIGameHousePearl in '..\..\src\gui\pages_game\house\KM_GUIGameHousePearl.pas',
  KM_Particles in '..\..\src\weather\KM_Particles.pas',
  KM_Weather in '..\..\src\weather\KM_Weather.pas',
  KM_WeatherCollection in '..\..\src\weather\KM_WeatherCollection.pas',
  KM_WeatherTypes in '..\..\src\weather\KM_WeatherTypes.pas';

{$R *.res}

var
  fConsoleMain: TConsoleMain;
  fParamRecord: TCLIParamRecord;
  fArgs:        string;


function IsConsoleMode: Boolean;
var
  SI: TStartupInfo;
begin
//  Result := True;
  ZeroMemory(@SI, SizeOf(SI));
  SI.cb := SizeOf(StartUpInfo);
  GetStartupInfo(SI);
  Result := (SI.dwFlags and STARTF_USESHOWWINDOW) = 0;
end;


procedure ProcessParams;
var
  I: Integer;
begin
  if ParamCount = 0 then
  begin
    fParamRecord.Help := True;
    Exit;
  end;

  for I := 1 to ParamCount do // Skip 0, as this is the EXE-path
  begin
    fArgs := fArgs + ' ' + paramstr(I) + sLineBreak;

    if (paramstr(I) = '-h') or (paramstr(I) = '-help') then
    begin
      fParamRecord.Help := True;
      continue;
    end;

    if (paramstr(I) = '-a') or (paramstr(I) = '-all') then
    begin
      fParamRecord.AllMaps := True;
      continue;
    end;

    if (paramstr(I) = '-c') or (paramstr(I) = '-campaign') then
    begin
      fParamRecord.Campaign := True;
      continue;
    end;

    if (paramstr(I) = '-g') or (paramstr(I) = '-graphic') then
    begin
      fParamRecord.GraphicMode := True;
      continue;
    end;

    if (paramstr(I) = '-x') or (paramstr(I) = '-xmlapi') then
    begin
      fParamRecord.XmlApi := True;
      continue;
    end;

    if (paramstr(I) = '-v') or (paramstr(I) = '-verbose') then
    begin
      fParamRecord.Verbose := True;
      continue;
    end;

    if (paramstr(I) = '-V') or (paramstr(I) = '-version') then
    begin
      fParamRecord.Version := True;
      continue;
    end;

    // Only allow one script file
    if fParamRecord.ScriptFile = '' then
      fParamRecord.ScriptFile := paramstr(I);
  end;
end;


begin
  if not IsConsoleMode then
  begin
    FreeConsole; // Used to hide the console
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TForm1, Form1);
  //we can send script file as parameter even in window mode (f.e. from Notepad++)
    ProcessParams;

    if fParamRecord.AllMaps then
      Form1.btnValidateAll.Click
    else
    begin
      Form1.Edit1.Text := fParamRecord.ScriptFile;
      Form1.btnValidate.Click;
    end;

    Application.Run;
  end else
  begin
    try
      ProcessParams;
      fConsoleMain := TConsoleMain.Create;

      if not fParamRecord.XmlApi then
        writeln(VALIDATOR_START_TEXT);

      if fParamRecord.Verbose and not fParamRecord.XmlApi then
        writeln('VERBOSE: Arguments:' + sLinebreak + fArgs);

      if fParamRecord.Version and not fParamRecord.XmlApi then
      begin
        writeln('Game version: ' + GAME_REVISION + sLineBreak +
                'Validator version: ' + VALIDATOR_VERSION + sLineBreak);
      end;

      // Always exit after showing help.
      if fParamRecord.Help then
      begin
        fConsoleMain.ShowHelp;
        Exit;
      end;

      if fParamRecord.GraphicMode then
      begin
        Application.Initialize;
        Application.MainFormOnTaskbar := True;
        Application.CreateForm(TForm1, Form1);
        Application.Run;
      end else
        fConsoleMain.Start(fParamRecord);
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;

    if fConsoleMain <> nil then
      FreeAndNil(fConsoleMain);
  end;

end.
