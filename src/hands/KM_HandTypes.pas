unit KM_HandTypes;
interface
uses
  KM_ResFonts;

type
  TKMHandType = (
    hndHuman,
    hndComputer
  );

  TKMHandEntityType = (etNone, etUnit, etGroup, etHouse);

  //* House lock state
  TKMHandHouseLock = (
    hlNone,
    hlDefault,
    hlBlocked, // Never allowed
    hlGranted, // Always allowed
    hlNotVisible// Don't show in gui
  );
  //* House lock state
  TKMHandUnitLock = (
    ulUnlocked,
    ulBlocked, // Not allowed
    ulNotVisible// Don't show in gui
  );

  TKMOverlayTextSettings = record
    WordWrap: Boolean;
    Font: TKMFont;
  end;

  TKMConsolCommandType = (
      cctNone,
      cctStartRecording,
      cctStopRecording ,
      cctResetRecordings
    );


const CONSOLE_COMMANDS_NAME : array[TKMConsolCommandType] of String = (
      '',
      'start_recording',
      'stop_recording',
      'reset_recordings'
      );

function GetAIConsolCommand(aName : String) : TKMConsolCommandType;

const
  HAND_NONE = -1; //No player
  HAND_ANIMAL = -2; //animals
  UNIT_LOCK_FROM_BOOL : array[Boolean] of TKMHandUnitLock = (ulUnlocked, ulBlocked);
const
  DELIVERY_NO_ID = -1;

implementation

function GetAIConsolCommand(aName : String) : TKMConsolCommandType;
var I : TKMConsolCommandType;
begin
  Result := cctNone;
  for I := Low(TKMConsolCommandType) to High(TKMConsolCommandType) do
    if aName = CONSOLE_COMMANDS_NAME[I] then
      Exit(I);
end;

end.
