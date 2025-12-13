unit KM_HandTypes;
interface
uses
  KM_ResFonts, KM_ResTypes;

type
  TKMHandType = (
    hndHuman,
    hndComputer
  );

  TKMHandEntityType = (etNone, etUnit, etGroup, etHouse, etStructure);

  //* House lock state
  TKMHandHouseLock = (
    hlNone,
    hlDefault,
    hlBlocked, // Never allowed
    hlGranted, // Always allowed
    hlNotVisible// Don't show in gui
  );
  //* Development lock state
  TKMHandDevLock = (
    dlNone,
    dlBlocked, // Never allowed
    dlUnlocked, // Always allowed, unlocks previous
    dlNotVisible,
    dlUnlockedSingle// allowed single development
  );
  //* House lock state
  TKMHandUnitLock = (
    ulUnlocked,
    ulBlocked, // Not allowed
    ulNotVisible// Don't show in gui
  );
  TKMUnitHouseBlock = (uhtSchool, uhtBarracks, uhtTownhall, uhtSiegeWorkshop, uhtPalace, uhtShipyard);

  TKMOverlayTextSettings = record
    WordWrap: Boolean;
    Font: TKMFont;
    MaxWidth : Word;
    FromBottom : Boolean;
    AddBevel : Boolean;
    AllignTextToCenter : Boolean;
  end;

  TKMConsolCommandType = (
      cctNone,
      cctStartRecording,
      cctStopRecording ,
      cctResetRecordings,
      cctSaveRecord
    );
  TKMPearlActivatedAnim = record
    X, Y : Integer;
    Tick : Cardinal;
  end;

const CONSOLE_COMMANDS_NAME : array[TKMConsolCommandType] of String = (
      '',
      'start_recording',
      'stop_recording',
      'reset_recordings',
      'save_recording'
      );
      PEARL_GLOW_ANIM_DURATION = 50;

function GetAIConsolCommand(aName : String) : TKMConsolCommandType;
function UHTtoHouseType(aType : TKMUnitHouseBlock) : TKMHouseType;

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

function UHTtoHouseType(aType : TKMUnitHouseBlock) : TKMHouseType;
begin
  Result := htAny;
  case aType of
  uhtSchool           : Result := htSchool;
  uhtBarracks         : Result := htBarracks;
  uhtTownhall         : Result := htTownhall;
  uhtSiegeWorkshop    : Result := htSiegeWorkshop;
  uhtPalace           : Result := htPalace;
  uhtShipyard         : Result := htShipYard;

  end;
end;

end.
