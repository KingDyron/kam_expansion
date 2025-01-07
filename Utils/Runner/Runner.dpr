program Runner;
{$I KaM_Remake.inc}
uses
  {$IFDEF USE_MAD_EXCEPT}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  //FastMM4,
  Forms,
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  {$IFDEF WDC}
    WinApi.Windows, // To allow to set {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE} for 3Gb or 4Gb RAM usage for Win32 Delphi app
  {$ENDIF}
  Unit1 in 'Unit1.pas' {Form2},
  Unit_Runner in 'Unit_Runner.pas',
  Runner_Game in 'Runner_Game.pas',
  ComInterface in 'ComInterface.pas',
  ParallelRun in 'ParallelRun.pas',
  GeneticAlgorithm in 'GeneticAlgorithm.pas',
  GeneticAlgorithmParameters in 'GeneticAlgorithmParameters.pas';

{$R *.res}

{$IFDEF WDC}
  // Enable usage of 3Gb or 4Gb of RAM for Win32 Delphi application
  // https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Increasing_the_Memory_Address_Space
  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

procedure DebugLogString();
var
  K: Integer;
  Params: String;
  debugFile: TextFile;
begin
  Params := '';
  AssignFile(debugFile, 'DEBUG_inputParameters.txt');
  try
    rewrite(debugFile);
    for K := 0 to ParamCount do
      writeln(debugFile, ParamStr(K));
    CloseFile(debugFile);
  except
    //on E: EInOutError do
    //  writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
end;

{$IFDEF PARALLEL_RUNNER}
var
  ParRun: TKMParallelRun;
{$ENDIF}
begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  {$IFDEF PARALLEL_RUNNER}
  if (ParamCount > 0) then
  begin
    //DebugLogString();
    ParRun := TKMParallelRun.Create(Form2);
    try
      PARALLEL_RUN := True;
      ParRun.InitSimulation();
      ParRun.RunSimulation();
      ParRun.LogResults();
    finally
      ParRun.Free();
    end;
    Application.Terminate;
  end;
  {$ENDIF}
  Application.Run;
end.
