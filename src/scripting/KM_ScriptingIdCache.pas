unit KM_ScriptingIdCache;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses, KM_Units, KM_UnitGroup, KM_CommonClasses;


//For caching unit/house/group IDs. Shared between States and Actions.
//Because scripts runs the same on every computer (i.e. no access to gMySpectator)
//we can safely use pointers within the cache

// We cache nil objects too, cos our goal is to resolve UID asap, no matter its state.
// Hence UID can be valid and pointer = nil when the object is dying
const
  CACHE_SIZE = 16; //Too big means caching becomes slow

type
  TKMScriptingIdCache = class
  private
    //3 separate bins used because we need to access class-specific fields (IsDead)
    //We employ circular buffers and store only position in buffer
    fUnitLastAdded: Byte;
    fUnitCache: array [0..CACHE_SIZE-1] of record UID: Integer; U: TKMUnit; end;
    fHouseLastAdded: Byte;
    fHouseCache: array[0..CACHE_SIZE-1] of record UID: Integer; H: TKMHouse; end;
    fGroupLastAdded: Byte;
    fGroupCache: array[0..CACHE_SIZE-1] of record UID: Integer; G: TKMUnitGroup; end;
  public
    procedure CacheUnit(aUnit: TKMUnit; aUID: Integer);
    procedure CacheHouse(aHouse: TKMHouse; aUID: Integer);
    procedure CacheGroup(aGroup: TKMUnitGroup; aUID: Integer);
    function GetUnit(aUID: Integer): TKMUnit;
    function GetHouse(aUID: Integer): TKMHouse;
    function GetGroup(aUID: Integer): TKMUnitGroup;
    procedure UpdateState;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;
  end;


implementation
uses
  KM_Entity,
  KM_GameParams, KM_HandsCollection,
  KM_DevPerfLog, KM_DevPerfLogTypes,
  KM_Defaults;


{ TKMScriptingIdCache }
procedure TKMScriptingIdCache.CacheUnit(aUnit: TKMUnit; aUID: Integer);
var
  I: Integer;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
    //Update cache item if changed (also from / to nil)
    if fUnitCache[I].UID = aUID then
    begin
      if fUnitCache[I].U <> aUnit then
      begin
        gHands.CleanUpUnitPointer(fUnitCache[I].U);
        if aUnit <> nil then
          fUnitCache[I].U := aUnit.GetPointer;
      end;
      Exit;
    end;

  //We need to release pointer if we remove unit from cache
  if fUnitCache[fUnitLastAdded].U <> nil then
    gHands.CleanUpUnitPointer(fUnitCache[fUnitLastAdded].U);

  fUnitCache[fUnitLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Units to find out that this UID is removed)
  if aUnit <> nil then
    fUnitCache[fUnitLastAdded].U := aUnit.GetPointer
  else
    fUnitCache[fUnitLastAdded].U := nil;

  fUnitLastAdded := (fUnitLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheHouse(aHouse: TKMHouse; aUID: Integer);
var
  I: Integer;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
    //Update cache item if changed (also from / to nil)
    if fHouseCache[I].UID = aUID then
    begin
      if fHouseCache[I].H <> aHouse then
      begin
        gHands.CleanUpHousePointer(fHouseCache[I].H);
        if aHouse <> nil then
          fHouseCache[I].H := aHouse.GetPointer;
      end;
      Exit;
    end;

  //We need to release pointer if we remove house from cache
  if fHouseCache[fHouseLastAdded].H <> nil then
    gHands.CleanUpHousePointer(fHouseCache[fHouseLastAdded].H);

  fHouseCache[fHouseLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Houses to find out that this UID is removed)
  if aHouse <> nil then
    fHouseCache[fHouseLastAdded].H := aHouse.GetPointer
  else
    fHouseCache[fHouseLastAdded].H := nil;

  fHouseLastAdded := (fHouseLastAdded + 1) mod CACHE_SIZE;
end;


procedure TKMScriptingIdCache.CacheGroup(aGroup: TKMUnitGroup; aUID: Integer);
var
  I: Integer;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
    if fGroupCache[I].UID = aUID then
    begin
      //Update cache item if changed (also from / to nil)
      if fGroupCache[I].G <> aGroup then
      begin
        gHands.CleanUpGroupPointer(fGroupCache[I].G);
        if aGroup <> nil then
          fGroupCache[I].G := aGroup.GetPointer;
      end;
      Exit;
    end;

  //We need to release pointer if we remove group from cache
  if fGroupCache[fGroupLastAdded].G <> nil then
    gHands.CleanUpGroupPointer(fGroupCache[fGroupLastAdded].G);

  fGroupCache[fGroupLastAdded].UID := aUID;
  //We could be asked to cache that certain UID is nil (saves us time scanning Groups to find out that this UID is removed)
  if aGroup <> nil then
    fGroupCache[fGroupLastAdded].G := aGroup.GetPointer
  else
    fGroupCache[fGroupLastAdded].G := nil;

  fGroupLastAdded := (fGroupLastAdded + 1) mod CACHE_SIZE;
end;


function TKMScriptingIdCache.GetUnit(aUID: Integer): TKMUnit;
var
  I: Integer;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
    if fUnitCache[I].UID = aUID then
    begin
      Result := fUnitCache[I].U;
      if (Result <> nil) and Result.IsDeadOrDying then
        Result := nil;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetUnitByUID(aUID);
  if (Result <> nil) and Result.IsDeadOrDying then
    Result := nil;

  CacheUnit(Result, aUID);
end;


function TKMScriptingIdCache.GetHouse(aUID:Integer): TKMHouse;
var
  I: Integer;
begin
  for I := Low(fHouseCache) to High(fHouseCache) do
    if fHouseCache[I].UID = aUID then
    begin
      Result := fHouseCache[I].H;
      if (Result <> nil) and Result.IsDestroyed then
        Result := nil;
      Exit;
    end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetHouseByUID(aUID);
  if (Result <> nil) and Result.IsDestroyed then
    Result := nil;

  CacheHouse(Result, aUID);
end;


function TKMScriptingIdCache.GetGroup(aUID: Integer): TKMUnitGroup;
var
  I: Integer;
begin
  for I := Low(fGroupCache) to High(fGroupCache) do
  if fGroupCache[I].UID = aUID then
  begin
    Result := fGroupCache[I].G;
    if (Result <> nil) and Result.IsDead then
      Result := nil;
    Exit;
  end;

  //Not found so do lookup and add it to the cache
  Result := gHands.GetGroupByUID(aUID);
  if (Result <> nil) and Result.IsDead then
    Result := nil;

  CacheGroup(Result, aUID);
end;


procedure TKMScriptingIdCache.UpdateState;
var
  I: Integer;
begin
  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psScripting);
  {$ENDIF}
  try
    //Clear out dead IDs every now and again
    //Leave them in the cache as nils, because we still might need to lookup that UID
    if gGameParams.Tick mod 11 = 0 then
    begin
      for I := Low(fUnitCache) to High(fUnitCache) do
        if (fUnitCache[I].U <> nil) and fUnitCache[I].U.IsDeadOrDying then
          gHands.CleanUpUnitPointer(fUnitCache[I].U);

      for I := Low(fHouseCache) to High(fHouseCache) do
        if (fHouseCache[I].H <> nil) and fHouseCache[I].H.IsDestroyed then
          gHands.CleanUpHousePointer(fHouseCache[I].H);

      for I := Low(fGroupCache) to High(fGroupCache) do
        if (fGroupCache[I].G <> nil) and fGroupCache[I].G.IsDead then
          gHands.CleanUpGroupPointer(fGroupCache[I].G);
    end;
  finally
    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psScripting);
    {$ENDIF}
  end;
end;


procedure TKMScriptingIdCache.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.PlaceMarker('ScriptingIdCache_Units');
  SaveStream.Write(fUnitLastAdded);
  for I := Low(fUnitCache) to High(fUnitCache) do
  begin
    SaveStream.Write(fUnitCache[I].UID);
    SaveStream.Write(fUnitCache[I].U.UID); //Store ID, then substitute it with reference on SyncLoad
  end;

  SaveStream.PlaceMarker('ScriptingIdCache_Houses');
  SaveStream.Write(fHouseLastAdded);
  for I := Low(fHouseCache) to High(fHouseCache) do
  begin
    SaveStream.Write(fHouseCache[I].UID);
    SaveStream.Write(fHouseCache[I].H.UID); //Store ID, then substitute it with reference on SyncLoad
  end;

  SaveStream.PlaceMarker('ScriptingIdCache_Groups');
  SaveStream.Write(fGroupLastAdded);
  for I := Low(fGroupCache) to High(fGroupCache) do
  begin
    SaveStream.Write(fGroupCache[I].UID);
    SaveStream.Write(fGroupCache[I].G.UID); //Store ID, then substitute it with reference on SyncLoad
  end;
end;


procedure TKMScriptingIdCache.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  LoadStream.CheckMarker('ScriptingIdCache_Units');
  LoadStream.Read(fUnitLastAdded);
  for I := Low(fUnitCache) to High(fUnitCache) do
  begin
    LoadStream.Read(fUnitCache[I].UID); //Load UID's
    LoadStream.Read(fUnitCache[I].U, 4); //And cached unit, SyncLoad later, when all game assets are ready
  end;

  LoadStream.CheckMarker('ScriptingIdCache_Houses');
  LoadStream.Read(fHouseLastAdded);
  for I := Low(fHouseCache) to High(fHouseCache) do
  begin
    LoadStream.Read(fHouseCache[I].UID); //Load only UID's
    LoadStream.Read(fHouseCache[I].H, 4); //And cached house, SyncLoad later, when all game assets are ready
  end;

  LoadStream.CheckMarker('ScriptingIdCache_Groups');
  LoadStream.Read(fGroupLastAdded);
  for I := Low(fGroupCache) to High(fGroupCache) do
  begin
    LoadStream.Read(fGroupCache[I].UID); //Load only UID's
    LoadStream.Read(fGroupCache[I].G, 4); //And cached group, SyncLoad later, when all game assets are ready
  end;
end;


procedure TKMScriptingIdCache.SyncLoad;
var
  I: Integer;
begin
  for I := Low(fUnitCache) to High(fUnitCache) do
    fUnitCache[I].U := gHands.GetUnitByUID(Integer(fUnitCache[I].U));

  for I := Low(fHouseCache) to High(fHouseCache) do
    fHouseCache[I].H := gHands.GetHouseByUID(Integer(fHouseCache[I].H));

  for I := Low(fGroupCache) to High(fGroupCache) do
    fGroupCache[I].G := gHands.GetGroupByUID(Integer(fGroupCache[I].G));
end;


end.
