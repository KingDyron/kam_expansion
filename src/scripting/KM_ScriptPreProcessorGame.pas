unit KM_ScriptPreProcessorGame;
{$I KaM_Remake.inc}
{$WARN IMPLICIT_STRING_CAST OFF}
interface
uses
  uPSPreProcessor,
  KM_ScriptPreProcessor;

type
  TKMScriptPreProcessorGame = class(TKMScriptPreProcessor)
  protected
    function AllowGameUpdate: Boolean; override;
    function TryLoadCustomEventDirectives(const aDirectiveName, aDirectiveParam: string; aParser: TPSPascalPreProcessorParser): Boolean; override;
    function TryLoadCustomConsoleCommands(const aDirectiveName, aDirectiveParam: string; aParser: TPSPascalPreProcessorParser): Boolean; override;
  end;

implementation
uses
  SysUtils, Classes, TypInfo,
  KM_GameParams,
  KM_Scripting, KM_ScriptingEvents, KM_ScriptingTypes,
  KM_CommonUtils;


{ TKMScriptPreProcessorGame }
function TKMScriptPreProcessorGame.AllowGameUpdate: Boolean;
begin
  Result := ((gGameParams <> nil) and not gGameParams.IsMapEditor)
         or ((gGameParams = nil) and TKMScriptingCreator.IsScriptingCreated);
end;


function TKMScriptPreProcessorGame.TryLoadCustomEventDirectives(const aDirectiveName, aDirectiveParam: string; aParser: TPSPascalPreProcessorParser): Boolean;
var
  errorStr: UnicodeString;
  eventType: Integer;
  directiveParamSL: TStringList;
begin
  Result := inherited;
  //Load custom event handlers
  if not Result then Exit;

  //Do not do anything for while in MapEd
  //But we have to allow to preprocess file, as preprocessed file used for CRC calc in MapEd aswell
  //gGame could be nil here, but that does not change final CRC, so we can Exit
  if not AllowGameUpdate then Exit;

  try
    directiveParamSL := TStringList.Create;
    try
      StringSplit(aDirectiveParam, ':', directiveParamSL);
      eventType := GetEnumValue(TypeInfo(TKMScriptEventType), Trim(directiveParamSL[0]));

      if eventType = -1 then
      begin
        ErrorHandler.AppendErrorStr(Format('Unknown directive ''%s'' at [%d:%d]' + sLineBreak,
                                            [Trim(directiveParamSL[0]), aParser.Row, aParser.Col]));
        if ValidationIssues <> nil then
          ValidationIssues.AddError(aParser.Row, aParser.Col, Trim(directiveParamSL[0]), 'Unknown directive');
      end
      else
        gScriptEvents.AddEventHandlerName(TKMScriptEventType(eventType), AnsiString(Trim(directiveParamSL[1])));
    finally
      directiveParamSL.Free;
    end;
  except
    on E: Exception do
      begin
        errorStr := Format('Error loading directive ''%s'' at [%d:%d]', [aParser.Token, aParser.Row, aParser.Col]);
        ErrorHandler.AppendErrorStr(errorStr, errorStr + ' Exception: ' + E.Message
          {$IFDEF WDC} + sLineBreak + E.StackTrace {$ENDIF});
        if ValidationIssues <> nil then
          ValidationIssues.AddError(aParser.Row, aParser.Col, aParser.Token, 'Error loading directive');
      end;
  end;
end;


function TKMScriptPreProcessorGame.TryLoadCustomConsoleCommands(const aDirectiveName, aDirectiveParam: string; aParser: TPSPascalPreProcessorParser): Boolean;
var
  cmdName, procName: AnsiString;
  errorStr: UnicodeString;
  sl: TStringList;
begin
  Result := inherited;
  // Load custom event handlers
  if not Result then Exit;

  // Do not do anything for while in MapEd
  // But we have to allow to preprocess file, as preprocessed file used for CRC calc in MapEd as well
  // gGame could be nil here, but that does not change final CRC, so we can Exit
  if not AllowGameUpdate then Exit;

  try
    sl := TStringList.Create;
    try
      StringSplit(aDirectiveParam, ':', sl);
      cmdName := AnsiString(Trim(sl[0]));
      procName := AnsiString(Trim(sl[1]));

      gScriptEvents.AddConsoleCommand(cmdName, procName);
    finally
      FreeAndNil(sl);
    end;
  except
    on E: Exception do
      begin
        errorStr := Format('Error loading command ''%s'' at [%d:%d]', [aParser.Token, aParser.Row, aParser.Col]);
        ErrorHandler.AppendErrorStr(errorStr, errorStr + ' Exception: ' + E.Message
          {$IFDEF WDC} + sLineBreak + E.StackTrace {$ENDIF});
        if ValidationIssues <> nil then
          ValidationIssues.AddError(aParser.Row, aParser.Col, aParser.Token, 'Error loading command');
      end;
  end;
end;


end.

