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

const
  HAND_NONE = -1; //No player
  HAND_ANIMAL = -2; //animals
  UNIT_LOCK_FROM_BOOL : array[Boolean] of TKMHandUnitLock = (ulUnlocked, ulBlocked);
const
  DELIVERY_NO_ID = -1;

implementation

end.
