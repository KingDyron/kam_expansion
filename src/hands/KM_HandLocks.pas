unit KM_HandLocks;
{$I KaM_Remake.inc}
interface
uses
  KM_ResHouses,
  KM_CommonClasses, KM_Defaults,
  KM_ResTypes, KM_HandTypes;


type
  TKMBoolHouseType = array [TKMHouseType] of Boolean;

  // Permissions to build, trade and train
  TKMHandLocks = class
  private
    fHouseUnlocked: array [TKMHouseType] of Boolean; //If building requirements performed
    fUnitBlocked: array [TKMUnitType] of TKMHandUnitLock;   //Allowance derived from mission script
    fFieldBlocked: array[TKMLockFieldType] of Boolean;
    fHandHouseLock: array [TKMHouseType] of TKMHandHouseLock;
    fHandHouseMaxLvl: array [TKMHouseType] of Byte;
    fStructureLock,
    fDecorationLock : array of TKMHandUnitLock;
    procedure UpdateReqDone(aType: TKMHouseType);

    function GetHouseBlocked(aHouseType: TKMHouseType): Boolean;
    function GetHouseGranted(aHouseType: TKMHouseType): Boolean;
    function GetHandHouseLock(aHouseType: TKMHouseType): TKMHandHouseLock;
    procedure SetHandHouseLock(aHouseType: TKMHouseType; const aValue: TKMHandHouseLock);

    function GetHandHouseMaxLvl(aHouseType: TKMHouseType): Byte;
    procedure SetHandHouseMaxLvl(aHouseType: TKMHouseType; aValue: Byte);

    function GetStructureLock(aIndex : Integer) : TKMHandUnitLock;
    function GetDecorationLock(aIndex : Integer) : TKMHandUnitLock;

    procedure SetStructureLock(aIndex : Integer; aValue : TKMHandUnitLock);
    procedure SetDecorationLock(aIndex : Integer; aValue : TKMHandUnitLock);
  public
    AllowToTrade: array [WARE_MIN..WARE_MAX] of Boolean; //Allowance derived from mission script
    constructor Create;

    property HouseBlocked[aHouseType: TKMHouseType]: Boolean read GetHouseBlocked;
    property HouseGranted[aHouseType: TKMHouseType]: Boolean read GetHouseGranted;
    property HouseLock[aHouseType: TKMHouseType]: TKMHandHouseLock read GetHandHouseLock write SetHandHouseLock;
    property HouseMaxLvl[aHouseType: TKMHouseType]: Byte read GetHandHouseMaxLvl write SetHandHouseMaxLvl;
    property Structures[aIndex: Integer]: TKMHandUnitLock read GetStructureLock write SetStructureLock;
    property Decoration[aIndex: Integer]: TKMHandUnitLock read GetDecorationLock write SetDecorationLock;

    procedure HouseCreated(aType: TKMHouseType);
    function HouseCanBuild(aType: TKMHouseType): Boolean;

    procedure SetUnitBlocked(blockType: TKMHandUnitLock; aUnitType: TKMUnitType; aInTownHall: Boolean = False);
    function GetUnitBlocked(aUnitType: TKMUnitType; aInTownHall: Boolean = False): TKMHandUnitLock;
    function UnitUnlocked(aUnitType : TKMUnitType) : Boolean;
    function UnitsUnlocked(aUnitType : array of TKMUnitType) : Boolean;

    function FieldLocked(aType : TKMLockFieldType) : Boolean;//if field is locked than it's not visible
    procedure SetFieldLocked(aType : TKMLockFieldType; aLocked : Boolean);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  Math,
  KM_Resource,
  KM_ResMapElements;


{ TKMHandLocks }
constructor TKMHandLocks.Create;
var
  W: TKMWareType;
  HT: TKMHouseType;
  I : Integer;
begin
  inherited;

  for W := WARE_MIN to high(TKMWareType) do
    if W in WARES_VALID then
      AllowToTrade[W] := True;

  //Release Store at the start of the game by default
  fHouseUnlocked[htStore] := True;

  for HT := Low(TKMHouseType) to High(TKMHouseType) do
    fHandHouseLock[HT] := hlDefault;
  Setlength(fStructureLock, gRes.Structures.Count);
  Setlength(fDecorationLock, length(gDecorations));

  for I := 0 to High(fStructureLock) do
    fStructureLock[I] := ulUnlocked;
  for I := 0 to High(fDecorationLock) do
    fDecorationLock[I] := ulUnlocked;

end;


procedure TKMHandLocks.UpdateReqDone(aType: TKMHouseType);
var
  H: TKMHouseType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    if aType in gRes.Houses[H].ReleasedBy then
      fHouseUnlocked[H] := True;
end;


// New house, either built by player or created by mission script
procedure TKMHandLocks.HouseCreated(aType: TKMHouseType);
begin
  UpdateReqDone(aType);
end;


// Get effective permission
function TKMHandLocks.HouseCanBuild(aType: TKMHouseType): Boolean;
begin
  Result := False;
  case fHandHouseLock[aType] of
    hlDefault: Result := fHouseUnlocked[aType];
    hlBlocked: Result := False;
    hlGranted: Result := True;
    hlNotVisible: Result := False;
  end;
end;


function TKMHandLocks.GetHouseBlocked(aHouseType: TKMHouseType): Boolean;
begin
  Result := fHandHouseLock[aHouseType] in [hlBlocked, hlNotVisible];
end;


function TKMHandLocks.GetHouseGranted(aHouseType: TKMHouseType): Boolean;
begin
  Result := fHandHouseLock[aHouseType] = hlGranted;
end;


function TKMHandLocks.GetHandHouseLock(aHouseType: TKMHouseType): TKMHandHouseLock;
begin
  Result := fHandHouseLock[aHouseType];
end;

function TKMHandLocks.GetHandHouseMaxLvl(aHouseType: TKMHouseType): Byte;
begin
  Result := fHandHouseMaxLvl[aHouseType];
end;

procedure TKMHandLocks.SetHandHouseMaxLvl(aHouseType: TKMHouseType; aValue : Byte);
begin
  fHandHouseMaxLvl[aHouseType] := aValue;
end;

function TKMHandLocks.GetStructureLock(aIndex : Integer) : TKMHandUnitLock;
begin
  if not InRange(aIndex, 0, high(fStructureLock)) then
    Exit(ulUnlocked);
  Result := fStructureLock[aIndex];
end;

function TKMHandLocks.GetDecorationLock(aIndex : Integer) : TKMHandUnitLock;
begin
  if not InRange(aIndex, 0, high(fDecorationLock)) then
    Exit(ulUnlocked);
  Result := fDecorationLock[aIndex];
end;

procedure TKMHandLocks.SetStructureLock(aIndex : Integer; aValue : TKMHandUnitLock);
begin
  if not InRange(aIndex, 0, high(fStructureLock)) then
    Exit;

  fStructureLock[aIndex] := aValue;
end;

procedure TKMHandLocks.SetDecorationLock(aIndex : Integer; aValue : TKMHandUnitLock);
begin
  if not InRange(aIndex, 0, high(fDecorationLock)) then
    Exit;
  fDecorationLock[aIndex] := aValue;
end;


procedure TKMHandLocks.SetHandHouseLock(aHouseType: TKMHouseType; const aValue: TKMHandHouseLock);
begin
  Assert(aValue <> hlNone, 'Can''t set hlNone Hand House Lock');
  fHandHouseLock[aHouseType] := aValue;
end;


//procedure TKMHandLocks.SetHouseBlocked(aHouseType: TKMHouseType; const aValue: Boolean);
//begin
//  if aValue then
//    fPlayerHouseLock[aHouseType] := phlBlocked
//  else
//    fPlayerHouseLock[aHouseType] := phlDefault;
//end;
//
//
//procedure TKMHandLocks.SetHouseGranted(aHouseType: TKMHouseType; const aValue: Boolean);
//begin
//  fPlayerHouseLock[aHouseType] := phlGranted;
//end;

function TKMHandLocks.GetUnitBlocked(aUnitType: TKMUnitType; aInTownHall: Boolean = False): TKMHandUnitLock;
begin
  Result := fUnitBlocked[aUnitType];
end;


procedure TKMHandLocks.SetUnitBlocked(blockType: TKMHandUnitLock; aUnitType: TKMUnitType; aInTownHall: Boolean = False);
begin
  fUnitBlocked[aUnitType] := blockType;
end;

function TKMHandLocks.UnitUnlocked(aUnitType : TKMUnitType) : Boolean;
begin
  Result := fUnitBlocked[aUnitType] = ulUnlocked;
end;

function TKMHandLocks.UnitsUnlocked(aUnitType : array of TKMUnitType) : Boolean;
var UT : TKMUnitType;
begin
  Result := false;

  for UT in aUnitType do
    if fUnitBlocked[UT] = ulUnlocked then
      Exit(true);
end;

function TKMHandLocks.FieldLocked(aType : TKMLockFieldType) : Boolean;
begin
  Result := fFieldBlocked[aType];
end;

procedure TKMHandLocks.SetFieldLocked(aType : TKMLockFieldType; aLocked : Boolean);
begin
  fFieldBlocked[aType] := aLocked;
end;

procedure TKMHandLocks.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('HandLocks');
  SaveStream.Write(fHandHouseLock, SizeOf(fHandHouseLock));
  SaveStream.Write(fUnitBlocked, SizeOf(fUnitBlocked));
  SaveStream.Write(AllowToTrade, SizeOf(AllowToTrade));
  SaveStream.Write(fHouseUnlocked, SizeOf(fHouseUnlocked));
  SaveStream.Write(fHandHouseMaxLvl, SizeOf(fHandHouseMaxLvl));
  SaveStream.Write(fFieldBlocked, SizeOf(fFieldBlocked));
end;


procedure TKMHandLocks.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('HandLocks');
  LoadStream.Read(fHandHouseLock, SizeOf(fHandHouseLock));
  LoadStream.Read(fUnitBlocked, SizeOf(fUnitBlocked));
  LoadStream.Read(AllowToTrade, SizeOf(AllowToTrade));
  LoadStream.Read(fHouseUnlocked, SizeOf(fHouseUnlocked));
  LoadStream.Read(fHandHouseMaxLvl, SizeOf(fHandHouseMaxLvl));
  LoadStream.Read(fFieldBlocked, SizeOf(fFieldBlocked));
end;


end.
