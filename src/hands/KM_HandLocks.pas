unit KM_HandLocks;
{$I KaM_Remake.inc}
interface
uses
  KM_ResHouses,
  KM_CommonClasses, KM_Defaults,
  KM_ResTypes, KM_HandTypes, KM_ResDevelopment;


type
  TKMBoolHouseType = array [TKMHouseType] of Boolean;
  TKMUnitBlockRec = record
    UT : TKMUnitType;
    Block : TKMHandUnitLock;
    constructor Create(aUnitType : TKMUnitType; aBlock :TKMHandUnitLock = ulUnlocked);
  end;


  // Permissions to build, trade and train
  TKMHandLocks = class
  private
    fHouseUnlocked: array [TKMHouseType] of Boolean; //If building requirements performed
    //fUnitBlocked: array [TKMUnitType] of TKMHandUnitLock;   //Allowance derived from mission script
    fFieldBlocked: array[TKMLockFieldType] of Boolean;
    fHandHouseLock: array [TKMHouseType] of TKMHandHouseLock;
    fHandHouseMaxLvl: array [TKMHouseType] of Byte;
    fStructureLock,
    fDecorationLock : array of TKMHandUnitLock;
    fUnitBlocked: array[TKMUnitHouseBlock] of TKMArray<TKMUnitBlockRec>;
    fDevLock: array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of array of TKMHandDevLock;
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
    function HouseToType(aHouseType : TKMHouseType) : TKMUnitHouseBlock;
    function GetUnitBlockArray(aType : TKMUnitHouseBlock) : TKMArray<TKMUnitBlockRec>;

    function GetDevLock(aType : TKMDevelopmentTreeType; aID : Integer) : TKMHandDevLock;
    procedure SetDevLock(aType : TKMDevelopmentTreeType; aID : Integer; aValue : TKMHandDevLock);
  public
    AllowToTrade: array [WARE_MIN..WARE_MAX] of Boolean; //Allowance derived from mission script
    constructor Create;

    property HouseBlocked[aHouseType: TKMHouseType]: Boolean read GetHouseBlocked;
    property HouseGranted[aHouseType: TKMHouseType]: Boolean read GetHouseGranted;
    property HouseLock[aHouseType: TKMHouseType]: TKMHandHouseLock read GetHandHouseLock write SetHandHouseLock;
    property HouseMaxLvl[aHouseType: TKMHouseType]: Byte read GetHandHouseMaxLvl write SetHandHouseMaxLvl;
    property Structures[aIndex: Integer]: TKMHandUnitLock read GetStructureLock write SetStructureLock;
    property Decoration[aIndex: Integer]: TKMHandUnitLock read GetDecorationLock write SetDecorationLock;
    property UnitBlocked[aType : TKMUnitHouseBlock] : TKMArray<TKMUnitBlockRec> read GetUnitBlockArray;

    procedure HouseCreated(aType: TKMHouseType);
    function HouseCanBuild(aType: TKMHouseType): Boolean;

    procedure SetUnitBlocked(aUnitType: TKMUnitType; aHouseType: TKMHouseType; blockType: TKMHandUnitLock);
    function GetUnitBlocked(aUnitType: TKMUnitType; aHouseType: TKMHouseType): TKMHandUnitLock;
    function UnitUnlocked(aUnitType : TKMUnitType; aHouseType: TKMHouseType = htAny) : Boolean;
    function UnitsUnlocked(aUnitType : array of TKMUnitType; aHouseType: TKMHouseType = htAny) : Boolean;

    property DevelopmentLock[aType : TKMDevelopmentTreeType; aID : Integer] : TKMHandDevLock read GetDevLock write SetDevlock;
    function DevelopmentCount(aType : TKMDevelopmentTreeType) : Word;

    procedure ForceUnlockDevMapEd(aType : TKMDevelopmentTreeType; aID : Integer);
    procedure CheckDevLocksMapEd;
    procedure CheckDevLocksGame;



    function FieldLocked(aType : TKMLockFieldType) : Boolean;//if field is locked than it's not visible
    procedure SetFieldLocked(aType : TKMLockFieldType; aLocked : Boolean);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
  end;


implementation
uses
  Math,
  KM_GameParams, KM_MapTypes,
  KM_Resource,
  KM_ResUnits,
  KM_ResMapElements;


{ TKMHandLocks }
constructor TKMHandLocks.Create;
var
  W: TKMWareType;
  HT: TKMHouseType;
  I : Integer;
  dtt : TKMDevelopmentTreeType;
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

  for I := 0 to High(SCHOOL_GAME_ORDER) do
    fUnitBlocked[uhtSchool].Add(TKMUnitBlockRec.Create(SCHOOL_GAME_ORDER[I]));
  for I := 0 to High(BARRACKS_GAME_ORDER) do
    fUnitBlocked[uhtBarracks].Add(TKMUnitBlockRec.Create(BARRACKS_GAME_ORDER[I]));
  for I := 0 to High(TH_GAME_ORDER) do
    fUnitBlocked[uhtTownhall].Add(TKMUnitBlockRec.Create(TH_GAME_ORDER[I]));
  for I := 0 to High(SIEGE_GAME_ORDER) do
    fUnitBlocked[uhtSiegeWorkshop].Add(TKMUnitBlockRec.Create(SIEGE_GAME_ORDER[I]));
  for I := 0 to High(PALACE_UNITS_ORDER) do
    fUnitBlocked[uhtPalace].Add(TKMUnitBlockRec.Create(PALACE_UNITS_ORDER[I]));
  for I := 0 to High(SHIPYARD_ORDER) do
    fUnitBlocked[uhtShipyard].Add(TKMUnitBlockRec.Create(SHIPYARD_ORDER[I]));


  for dtt := Low(fDevLock) to High(fDevLock) do
  begin
    SetLength(fDevLock[dtt], gRes.Development.Tree[dtt].Count);
    for I := 0 to High(fDevLock[dtt]) do
    begin
      fDevLock[dtt, I] := dlNone;
    end;
  end;


end;


procedure TKMHandLocks.UpdateReqDone(aType: TKMHouseType);
var
  H: TKMHouseType;
begin
  for H := HOUSE_MIN to HOUSE_MAX do
    if aType in gRes.Houses[H].ReleasedBy then
      fHouseUnlocked[H] := True;

  If gGameParams.MPMode = mmTraverse then
  begin
    case aType of
      htSawMill:  begin
                    fHouseUnlocked[htWatchTower] := true;
                    fHouseUnlocked[htWall] := true;
                    fHouseUnlocked[htWall2] := true;
                    fHouseUnlocked[htWall3] := true;
                    fHouseUnlocked[htWall4] := true;
                    fHouseUnlocked[htWall5] := true;
                  end;
      htStoneWorkshop: fHouseUnlocked[htWatchTower] := true;
    end;
  end;
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

function TKMHandLocks.GetUnitBlockArray(aType: TKMUnitHouseBlock): TKMArray<TKMUnitBlockRec>;
begin
  Result := fUnitBlocked[aType];
end;

function TKMHandLocks.GetDevLock(aType : TKMDevelopmentTreeType; aID : Integer) : TKMHandDevLock;
begin
  Result := fDevLock[aType, aID];
end;

procedure TKMHandLocks.SetDevLock(aType : TKMDevelopmentTreeType; aID : Integer; aValue : TKMHandDevLock);
begin
  fDevLock[aType, aID] := aValue;
end;

function TKMHandLocks.DevelopmentCount(aType : TKMDevelopmentTreeType) : Word;
begin
  Result := length(fDevLock[aType]);
end;



procedure TKMHandLocks.CheckDevLocksMapEd;
var dtt : TKMDevelopmentTreeType;

  //in map ed we want as few commends as possible
  //so to unlock previous, just ignore it and set it to dlNone
  procedure UnlockPrevious(aType : TKMDevelopmentTreeType; aDev : PKMDevelopment);
  begin
    fDevLock[aType, aDev.ID] := dlNone;
    If aDev.Parent <> nil then
      UnlockPrevious(aType, aDev.Parent);
  end;

  procedure CheckDev(aType : TKMDevelopmentTreeType; aDev : PKMDevelopment; aState : TKMHandDevLock; aForceState : Boolean = false);
  var nextState : TKMHandDevLock;
    I : integer;
  begin
    fDevLock[dtt, aDev.ID] := aState;

    If aState = dlUnlocked then
      If aDev.Parent <> nil then
        UnlockPrevious(aType, aDev.Parent);

    for I := 0 to High(aDev.Next) do
    begin
      If aForceState then
        nextState := dlNone
      else
      begin
        nextState := fDevLock[dtt, aDev.Next[I].ID];
        If aState in [dlBlocked, dlNotVisible] then
        begin
          nextState := dlNone;
          aForceState := true;
        end;

      end;

      CheckDev(aType, @aDev.Next[I], nextState, aForceState);
    end;
  end;

begin
  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
    CheckDev(dtt, gRes.Development[dtt].FirstItem, fDevLock[dtt, 0]);
end;

procedure TKMHandLocks.ForceUnlockDevMapEd(aType: TKMDevelopmentTreeType; aID: Integer);
  //in map ed we want as few commends as possible
  //so to unlock previous, just ignore it and set it to dlNone
  procedure UnlockPrevious(aType : TKMDevelopmentTreeType; aDev : PKMDevelopment);
  begin
    If aDev = nil then
      Exit;
    fDevLock[aType, aDev.ID] := dlNone;
    If aDev.Parent <> nil then
      UnlockPrevious(aType, aDev.Parent);
  end;

begin
  fDevLock[aType, aID] := dlUnlocked;
  UnlockPrevious(aType, gRes.Development[aType].GetItem(aID).Parent);
end;

procedure TKMHandLocks.CheckDevLocksGame;
var dtt : TKMDevelopmentTreeType;


  procedure UnlockPrevious(aType : TKMDevelopmentTreeType; aDev : PKMDevelopment);
  begin
    fDevLock[aType, aDev.ID] := dlUnlocked;
    If aDev.Parent <> nil then
      UnlockPrevious(aType, aDev.Parent);
  end;

  procedure CheckDev(aType : TKMDevelopmentTreeType; aDev : PKMDevelopment; aState : TKMHandDevLock);
  var nextState : TKMHandDevLock;
    I : integer;
  begin
    fDevLock[aType, aDev.ID] := aState;

    If aState = dlUnlocked then
      If aDev.Parent <> nil then
        UnlockPrevious(aType, aDev.Parent);

    for I := 0 to High(aDev.Next) do
    begin
      nextState := fDevLock[dtt, aDev.Next[I].ID];
      If aState in [dlBlocked, dlNotVisible] then
        nextState := aState;
      CheckDev(aType, @aDev.Next[I], nextState);
    end;
  end;

begin
  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
    CheckDev(dtt, gRes.Development[dtt].FirstItem, fDevLock[dtt, 0]);
end;


function TKMHandLocks.HouseToType(aHouseType: TKMHouseType): TKMUnitHouseBlock;
begin
  Result := uhtSchool;
  case aHouseType of
    htSchool :        Result := uhtSchool;
    htBarracks :      Result := uhtBarracks;
    htTownHall :      Result := uhtTownHall;
    htSiegeWorkshop : Result := uhtSiegeWorkshop;
    htPalace :        Result := uhtPalace;
    htShipYard :      Result := uhtShipYard;
    else
      Assert(false, 'TKMHandLocks.HouseToType');
  end;
end;


function TKMHandLocks.GetUnitBlocked(aUnitType: TKMUnitType; aHouseType: TKMHouseType): TKMHandUnitLock;
var I : Integer;
  UHT : TKMUnitHouseBlock;
begin
  Result := ulNotVisible;
  if aHouseType = htAny then
  begin
    for UHT := low(TKMUnitHouseBlock) to high(TKMUnitHouseBlock) do
      for I := 0 to fUnitBlocked[UHT].Count - 1 do
      if fUnitBlocked[UHT].Item[I].UT = aUnitType then
        Exit(fUnitBlocked[UHT].Item[I].Block);

  end else
  begin
    UHT := HouseToType(aHouseType);
    for I := 0 to fUnitBlocked[UHT].Count - 1 do
    if fUnitBlocked[UHT].Item[I].UT = aUnitType then
      Exit(fUnitBlocked[UHT].Item[I].Block);

  end;
  //Result := fUnitBlocked[aUnitType];
end;


procedure TKMHandLocks.SetUnitBlocked(aUnitType: TKMUnitType; aHouseType: TKMHouseType; blockType: TKMHandUnitLock);
var I : Integer;
  UHT : TKMUnitHouseBlock;
begin
  if aHouseType = htAny then
  begin
    for UHT := low(TKMUnitHouseBlock) to high(TKMUnitHouseBlock) do
      for I := 0 to fUnitBlocked[UHT].Count - 1 do
      if fUnitBlocked[UHT].Item[I].UT = aUnitType then
        fUnitBlocked[UHT].Item[I] := TKMUnitBlockRec.Create(aUnitType, blockType);
  end else
  begin
    UHT := HouseToType(aHouseType);
    for I := 0 to fUnitBlocked[UHT].Count - 1 do
    if fUnitBlocked[UHT].Item[I].UT = aUnitType then
      fUnitBlocked[UHT].Item[I] := TKMUnitBlockRec.Create(aUnitType, blockType);

  end;

  //fUnitBlocked[aUnitType] := blockType;
end;

function TKMHandLocks.UnitUnlocked(aUnitType : TKMUnitType; aHouseType: TKMHouseType) : Boolean;
begin
  Result := GetUnitBlocked(aUnitType, aHouseType) = ulUnlocked;
  //Result := fUnitBlocked[aUnitType] = ulUnlocked;
end;

function TKMHandLocks.UnitsUnlocked(aUnitType : array of TKMUnitType; aHouseType: TKMHouseType) : Boolean;
var UT : TKMUnitType;
begin
  Result := false;

  for UT in aUnitType do
    IF GetUnitBlocked(UT, aHouseType) = ulUnlocked then
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
var UHT : TKMUnitHouseBlock;
  dtt : TKMDevelopmentTreeType;
  I, K : integer;
begin
  SaveStream.PlaceMarker('HandLocks');
  SaveStream.Write(fHandHouseLock, SizeOf(fHandHouseLock));
  //SaveStream.Write(fUnitBlocked, SizeOf(fUnitBlocked)); //deprecated
  for UHT := Low(TKMUnitHouseBlock) to High(TKMUnitHouseBlock) do
    fUnitBlocked[UHT].SaveToStream(SaveStream);
  SaveStream.Write(AllowToTrade, SizeOf(AllowToTrade));
  SaveStream.Write(fHouseUnlocked, SizeOf(fHouseUnlocked));
  SaveStream.Write(fHandHouseMaxLvl, SizeOf(fHandHouseMaxLvl));
  SaveStream.Write(fFieldBlocked, SizeOf(fFieldBlocked));

  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
  begin
    K := length(fDevLock[dtt]);
    SaveStream.Write(K);
    for I := 0 to K - 1 do
      SaveStream.WriteData(fDevLock[dtt, I]);
  end;
end;


procedure TKMHandLocks.Load(LoadStream: TKMemoryStream);
var UHT : TKMUnitHouseBlock;
  dtt : TKMDevelopmentTreeType;
  I, K : integer;
begin
  LoadStream.CheckMarker('HandLocks');
  LoadStream.Read(fHandHouseLock, SizeOf(fHandHouseLock));
  //LoadStream.Read(fUnitBlocked, SizeOf(fUnitBlocked)); //deprecated
  for UHT := Low(TKMUnitHouseBlock) to High(TKMUnitHouseBlock) do
    fUnitBlocked[UHT].LoadFromStream(LoadStream);

  LoadStream.Read(AllowToTrade, SizeOf(AllowToTrade));
  LoadStream.Read(fHouseUnlocked, SizeOf(fHouseUnlocked));
  LoadStream.Read(fHandHouseMaxLvl, SizeOf(fHandHouseMaxLvl));
  LoadStream.Read(fFieldBlocked, SizeOf(fFieldBlocked));

  for dtt := DEVELOPMENT_MIN to DEVELOPMENT_MAX do
  begin
    LoadStream.Read(K);
    SetLength(fDevLock[dtt], K);
    for I := 0 to K - 1 do
      LoadStream.ReadData(fDevLock[dtt, I]);
  end;
end;


constructor TKMUnitBlockRec.Create(aUnitType: TKMUnitType; aBlock: TKMHandUnitLock = ulUnlocked);
begin
  UT := aUnitType;
  Block := aBlock;
end;

end.
