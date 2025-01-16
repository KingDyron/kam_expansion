unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  Vcl.Controls,
  KM_RenderPool, KM_TerrainPainter, KM_TerrainDeposits, KM_TerrainSelection,
  KM_CommonTypes, KM_CommonClasses, KM_Defaults, KM_Points, KM_MapEditorHistory,
  KM_MapEdClipBoard,
  KM_MapEdTypes, KM_ResTexts, KM_HandEntity, KM_AITypes, KM_Utils;


type
  //Collection of map editing classes and map editor specific data
  TKMMapEditor = class
  private
    fAddedHouse : Boolean;
    fLandMapEd: TKMMapEdLand;
    fMainLandMapEd: PKMMapEdLand;
    fIsNewMap: Boolean;
    fSavedMapIsPlayable: Boolean; // Saved map is playable if there is at elast 1 enabled human loc with assets
    fTerrainPainter: TKMTerrainPainter;
    fHistory: TKMMapEditorHistory;
    fDeposits: TKMDeposits;
    fSelection: TKMSelection;
    fClipBoard: TKMMapEdClipboard;
    fRevealers: array [0..MAX_HANDS-1] of TKMPointTagList;
    fVisibleLayers: TKMMapEdVisibleLayerSet;
    //When you load a map script/libx/wav/etc. files are "attached" then copied when
    //saving if the path is different
    fAttachedFiles: array of UnicodeString;

    fLastErasedObjectLoc: TKMPoint;
    fLastRemoveTxID: Integer;

    function GetRevealer(aIndex: Byte): TKMPointTagList;
    procedure ProceedRoadCursorMode;
    procedure ProceedPalisadeCursorMode;
    procedure ProceedRemUnit(const X, Y: Integer);
    procedure ProceedUnitsCursorMode;
    //procedure ProceedEraseCursorMode;   //old one
    procedure ApplyBigErase(Size : Integer = 0);
    procedure DoBigErase(const X, Y: Integer);

    procedure UpdateField;
    function EraseTerrainObject(var aRemoveTxID: Integer): Boolean;
    procedure EraseObject(aEraseAll: Boolean);
    function ChangeEntityOwner(aEntity: TKMHandEntity; aOwner: TKMHandID): Boolean;
    procedure ChangeOwner(aChangeOwnerForAll: Boolean);
    procedure PaintSpawners(aLayer: TKMPaintLayer);
    procedure PaintDefences(aLayer: TKMPaintLayer);
    procedure PaintRevealFOW(aLayer: TKMPaintLayer);
    procedure PaintCenterScreen(aLayer: TKMPaintLayer);
    procedure PaintAIStart(aLayer: TKMPaintLayer);
    procedure SetRandomRes;

    procedure UpdateMapEdCursorParams(aMode: TKMChangeDefenceTypeMode; aDirInc: Integer = 1);  //change units and def pos direction

    procedure AddDefenceMarker(const aLoc: TKMPoint);
    procedure AddDefendMarker(const aLoc: TKMPoint);

    function GetCheckpointObjectsStr(removeTxID: Integer = TX_WORD_OBJECT): string; overload;
    function GetCheckpointObjectsStr(aCell: TKMPoint; removeTxID: Integer = TX_WORD_OBJECT): string; overload;
    function GetHistory: TKMMapEditorHistory;

    procedure UpdateSavedInfo;
  public
    LandMapEd: PKMMapEdLand;

    MissionDefSavePath: UnicodeString;

    ActiveMarker: TKMMapEdMarker;

    ResizeMapRect: TKMRect;
    RevealAll: array [0..MAX_HANDS-1] of Boolean;
    DefaultHuman: TKMHandID;
    PlayerHuman: array [0..MAX_HANDS - 1] of Boolean;
    PlayerClassicAI: array [0..MAX_HANDS - 1] of Boolean;
    PlayerAdvancedAI: array [0..MAX_HANDS - 1] of Boolean;

    SavedPlayableLocs: array [0..MAX_HANDS - 1] of Boolean;

    OnEyedropper: TIntegerEvent;

    constructor Create(aNewMap: Boolean; aTerrainPainter: TKMTerrainPainter; aOnHistoryUndoRedo, aOnHistoryAddCheckpoint: TEvent);
    destructor Destroy; override;

    procedure AfterCreated;

    procedure SetMainLandMapEd;

    property MainLandMapEd: PKMMapEdLand read fMainLandMapEd; //readonly

    property Deposits: TKMDeposits read fDeposits;
    property VisibleLayers: TKMMapEdVisibleLayerSet read fVisibleLayers write fVisibleLayers;
    property Selection: TKMSelection read fSelection;
    property ClipBoard: TKMMapEdClipboard read fClipBoard;
    property Revealers[aIndex: Byte]: TKMPointTagList read GetRevealer;
    property History: TKMMapEditorHistory read GetHistory write fHistory;

    property IsNewMap: Boolean read fIsNewMap;
    property SavedMapIsPlayable: Boolean read fSavedMapIsPlayable;
    procedure ValidatePlayerTypes;

    function CanHaveClassicAI: Boolean;
    function CanHaveAdvancedAI: Boolean;
    function OnlyAdvancedAIHand(aHandId: TKMHandID): Boolean;

    procedure DetermineGroupFormationAndDir(const aLoc: TKMPoint; aGroupType: TKMGroupType;
                                            out aFormation: TKMFormation; out aDir: TKMDirection);

    procedure ApplyAIMultiplayerSetup(aHandID: TKMHandID);

    procedure DetectAttachedFiles(const aMissionFile: UnicodeString);
    procedure SaveAttachements(const aMissionFile: UnicodeString);
    function HitTest(X,Y: Integer): TKMMapEdMarker;
    function HumanCount: Integer;
    procedure AddWorkersToHouses;
    procedure MouseDown(Button: TMouseButton);
    procedure MouseMove;
    procedure MouseUp(Button: TMouseButton; aOverMap: Boolean);
    procedure MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer);
    procedure UpdateState;
    procedure UpdateStateIdle;
    procedure Paint(aLayer: TKMPaintLayer; const aClipRect: TKMRect);

    procedure Reset;

    procedure DeletePlayer(aIndex: TKMHandID);
  end;


implementation
uses
  SysUtils, StrUtils, Math,
  KM_TerrainTypes, KM_Terrain, KM_FileIO,
  KM_AIDefensePos,
  KM_Units, KM_UnitGroup, KM_Houses, KM_HouseCollection,
  KM_GameParams, KM_Cursor, KM_ResMapElements, KM_ResHouses, KM_Resource, KM_ResUnits,
  KM_RenderAux, KM_RenderGameAux,
  KM_Hand, KM_HandsCollection, KM_HandEntityHelper, KM_HandTypes,
  KM_CommonUtils, KM_RenderDebug,
  KM_UnitGroupTypes,
  KM_ResTypes;

//defines default defence position radius for static AI 
const
  DEFAULT_DEFENCE_POSITION_RADIUS = 20;


{ TKMMapEditor }
constructor TKMMapEditor.Create(aNewMap: Boolean; aTerrainPainter: TKMTerrainPainter; aOnHistoryUndoRedo, aOnHistoryAddCheckpoint: TEvent);
var
  I: Integer;
begin
  inherited Create;

  SetMainLandMapEd;
  fMainLandMapEd := @fLandMapEd;

  MissionDefSavePath := '';

  fVisibleLayers := [melDeposits];
  fIsNewMap := aNewMap;

  FillChar(LandMapEd^[1,1], SizeOf(LandMapEd^[1,1])*MAX_MAP_SIZE*MAX_MAP_SIZE, #0);

  for I := 0 to MAX_HANDS - 1 do
  begin
    PlayerHuman[I] := True;
    PlayerClassicAI[I] := True;
    PlayerAdvancedAI[I] := false;
  end;

  fDeposits := TKMDeposits.Create;

  fTerrainPainter := aTerrainPainter;
  fSelection := TKMSelection.Create(fTerrainPainter);
  fClipBoard := TKMMapEdClipboard.Create;


  fHistory := TKMMapEditorHistory.Create;
  fHistory.OnUndoRedo := aOnHistoryUndoRedo;
  fHistory.OnAddCheckpoint := aOnHistoryAddCheckpoint;

  ResizeMapRect := KMRECT_ZERO;

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I] := TKMPointTagList.Create;
end;


destructor TKMMapEditor.Destroy;
var
  I: Integer;
begin
  FreeAndNil(fHistory);
  FreeAndNil(fDeposits);
  FreeAndNil(fSelection);
  FreeAndNil(fClipBoard);

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I].Free;

  inherited;
end;


function TKMMapEditor.GetRevealer(aIndex: Byte): TKMPointTagList;
begin
  Result := fRevealers[aIndex];
end;


procedure TKMMapEditor.ApplyAIMultiplayerSetup(aHandID: TKMHandID);
begin
  PlayerClassicAI[aHandID] := True;
  PlayerAdvancedAI[aHandID] := True;
  gHands[aHandID].AI.General.DefencePositions.Clear;
  gHands[aHandID].AI.General.Attacks.Clear;
  //Setup Multiplayer setup, for ClassicAI. Anyway we will consider Old/New AI on the game start
  gHands[aHandID].AI.Setup.ApplyMultiplayerSetup(False);
end;


procedure TKMMapEditor.DetectAttachedFiles(const aMissionFile: UnicodeString);

  procedure AddAttachment(var aAttachCnt: Integer; const aFileName: UnicodeString);
  begin
    if aAttachCnt >= Length(fAttachedFiles) then
      SetLength(fAttachedFiles, aAttachCnt + 8);

    fAttachedFiles[aAttachCnt] := aFileName;
    Inc(aAttachCnt);
  end;

var
  searchRec: TSearchRec;
  missionScriptFileName, missionName, recExt: UnicodeString;
  hasScript: Boolean;
  attachCnt: Integer;
begin
  hasScript := False;
  attachCnt := 0;
  SetLength(fAttachedFiles, 8);
  MissionDefSavePath := aMissionFile;
  missionName := ChangeFileExt(ExtractFileName(aMissionFile), '');
  FindFirst(ChangeFileExt(aMissionFile, '.*'), faAnyFile - faDirectory, searchRec);
  try
    repeat
      if (searchRec.Name <> '') and (searchRec.Name <> '.') and (searchRec.Name <> '..') then
      begin
        //Can't use ExtractFileExt because we want .eng.libx not .libx
        recExt := RightStr(searchRec.Name, Length(searchRec.Name) - Length(missionName));
        if (LowerCase(recExt) = '.map')
          or (LowerCase(recExt) = '.dat')
          or (LowerCase(recExt) = '.mi' ) then
          Continue;

        if LowerCase(recExt) = EXT_FILE_SCRIPT_DOT then
          hasScript := True;

        AddAttachment(attachCnt, ExtractFilePath(aMissionFile) + searchRec.Name);
      end;
    until (FindNext(searchRec) <> 0);
  finally
    FindClose(searchRec);
  end;

  //Add all scripts if we find main script
  if hasScript then
  begin
    missionScriptFileName := missionName + EXT_FILE_SCRIPT_DOT;
    FindFirst(ExtractFilePath(aMissionFile) + '*' + EXT_FILE_SCRIPT_DOT, faAnyFile - faDirectory, searchRec);
    try
      repeat
        if (searchRec.Name <> '.') and (searchRec.Name <> '..')
          and (searchRec.Name <> missionScriptFileName) then
          AddAttachment(attachCnt, ExtractFilePath(aMissionFile) + searchRec.Name);
      until (FindNext(searchRec) <> 0);
    finally
      FindClose(searchRec);
    end;
  end;

  SetLength(fAttachedFiles, attachCnt);
end;


procedure TKMMapEditor.ValidatePlayerTypes;
var
  I: Integer;
  hasAssets, hasDefault, noAssetsAtAll: Boolean;
begin
  noAssetsAtAll := True;
  hasDefault := False;

  try
    for I := 0 to gHands.Count - 1 do
    begin
      hasAssets := gHands[I].HasAssets;
      noAssetsAtAll := noAssetsAtAll and not hasAssets;
      hasDefault := hasDefault or (hasAssets and (DefaultHuman = I));
    end;
    //No default human player chosen
    if not hasDefault then
    begin
      for I := 0 to gHands.Count - 1 do
      begin
        if gHands[I].HasAssets and PlayerHuman[I] then
        begin
          DefaultHuman := I;
          hasDefault := True;
          Exit;
        end;
      end;
      //Still no default is set (no humans)
      //Find first hand and set it as enabled for humans and as default
      if not hasDefault then
        for I := 0 to gHands.Count - 1 do
          if gHands[I].HasAssets then
          begin
            PlayerHuman[I] := True;
            DefaultHuman := I;
            hasDefault := True;
            Exit;
          end;
    end;
  finally
    Assert(noAssetsAtAll or hasDefault, 'Can not set default human');
  end;
end;


procedure TKMMapEditor.UpdateSavedInfo;
var
  I: Integer;
begin
  fSavedMapIsPlayable := False;

  for I := 0 to MAX_HANDS - 1 do
  begin
    SavedPlayableLocs[I] := PlayerHuman[I] and gHands[I].HasAssets;
    fSavedMapIsPlayable := fSavedMapIsPlayable or SavedPlayableLocs[I];
  end;

  ValidatePlayerTypes;
end;


procedure TKMMapEditor.AfterCreated;
begin
  UpdateSavedInfo;
end;


procedure TKMMapEditor.SetMainLandMapEd;
begin
  if Self = nil then Exit;

  LandMapEd := @fLandMapEd;
end;


procedure TKMMapEditor.SaveAttachements(const aMissionFile: UnicodeString);
var
  I: Integer;
  missionPath, missionNewName, missionOldName, destPath: UnicodeString;
begin
  if not fIsNewMap then
  begin
    missionPath := ExtractFilePath(aMissionFile);
    missionNewName := GetFileDirName(aMissionFile);
    missionOldName := '';

    //Copy all attachments files into new folder
    for I := 0 to High(fAttachedFiles) do
      if FileExists(fAttachedFiles[I]) then
      begin
        destPath := missionPath + ExtractFileName(fAttachedFiles[I]);

        //Get MissionOldName from first attachment file
        if missionOldName = '' then
          missionOldName := GetFileDirName(fAttachedFiles[I]);

        if gCursor.CampaignData.Path = '' then
          if not SameFileName(destPath, fAttachedFiles[I]) then
          begin
            if FileExists(destPath) then
              KMDeleteFile(destPath);
            KMCopyFile(fAttachedFiles[I], destPath);
          end;
      end;

    // Rename all files inside new saved map folder
    KMRenameFilesInFolder(missionPath, missionOldName, missionNewName);

    //Update attached files to be in the new path
    SetLength(fAttachedFiles, 0);
    DetectAttachedFiles(aMissionFile);
  end;

  fIsNewMap := False; //Map was saved, its not a new map anymore
  UpdateSavedInfo;
end;


function TKMMapEditor.CanHaveClassicAI: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to gHands.Count - 1 do
    Result := Result or (gHands[I].HasAssets and PlayerClassicAI[I]);
end;


function TKMMapEditor.CanHaveAdvancedAI: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to gHands.Count - 1 do
    Result := Result or (gHands[I].HasAssets and PlayerAdvancedAI[I]);
end;


function TKMMapEditor.OnlyAdvancedAIHand(aHandId: TKMHandID): Boolean;
begin
  Result := PlayerAdvancedAI[aHandId]
    and not PlayerClassicAI[aHandId]
    and not PlayerHuman[aHandId];
end;


function TKMMapEditor.HitTest(X, Y: Integer): TKMMapEdMarker;
var
  I, K: Integer;
begin
  if   (melDefences in fVisibleLayers)
    or (mlDefencesAll in gGameParams.VisibleLayers) then
  begin
    for I := 0 to gHands.Count - 1 do
      for K := 0 to gHands[I].AI.General.DefencePositions.Count - 1 do
        if (gHands[I].AI.General.DefencePositions[K].Position.Loc.X = X)
        and (gHands[I].AI.General.DefencePositions[K].Position.Loc.Y = Y) then
        begin
          Result.MarkerType := mmtDefence;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;

  end;

  for I := 0 to gHands.Count - 1 do
    for K := 0 to gHands[I].AI.General.DefendPositions.Count - 1 do
      if gHands[I].AI.General.DefendPositions[K].Position = KMPoint(X, Y) then
      begin
        Result.MarkerType := mmtDefendPos;
        Result.Owner := I;
        Result.Index := K;
        Exit;
      end;

  if melRevealFOW in fVisibleLayers then
  begin
    for I := 0 to gHands.Count - 1 do
      for K := 0 to fRevealers[I].Count - 1 do
        if (fRevealers[I][K].X = X) and (fRevealers[I][K].Y = Y) then
        begin
          Result.MarkerType := mmtRevealFOW;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;
  if melSpawners in fVisibleLayers then
  begin
    for I := 0 to gHands.PlayerAnimals.SpawnersCount - 1 do
      if gHands.PlayerAnimals.Spawners[I].Loc = KMPoint(X, Y) then
      begin
        Result.MarkerType := mmtSpawner;
        Result.Index := I;
        Result.Owner := gMySpectator.HandID;
        Exit;
      end;
      
  end;

  //Else nothing is found
  Result.MarkerType := mmtNone;
  Result.Owner := HAND_NONE;
  Result.Index := -1;
end;


//How many human players there are in the mission
function TKMMapEditor.HumanCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(PlayerHuman) do
  if PlayerHuman[I] then
    Inc(Result);
end;


//aStageIncrement - stage increment, can be negative
procedure TKMMapEditor.UpdateField;
var
  P: TKMPoint;
  makeCheckpoint: Boolean;
begin
  P := gCursor.Cell;
  case gCursor.Mode of
    cmField:  begin
                if not gTerrain.TileIsCornField(P) and not gMySpectator.Hand.CanAddFieldPlan(P, ftCorn) then Exit;

                makeCheckpoint := not gTerrain.TileIsCornField(P);
                gMySpectator.Hand.AddField(P, ftCorn, gCursor.MapEdFieldAge, gCursor.GrainType);
                if makeCheckpoint then
                  fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                            [gResTexts[TX_WORD_CORN_FIELD], P.ToString]));
              end;
    cmGrassLand:begin
                  if not gTerrain.TileIsGrassField(P) and not gMySpectator.Hand.CanAddFieldPlan(P, ftGrassland) then Exit;

                  makeCheckpoint := not gTerrain.TileIsGrassField(P);
                  gMySpectator.Hand.AddField(P, ftGrassland, gCursor.MapEdGrassFieldAge, gCursor.GrainType);
                  if makeCheckpoint then
                    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                              [gResTexts[TX_WORD_CORN_FIELD], P.ToString]));
                end;

    cmVegeField:begin
                  if not gTerrain.TileIsVegeField(P) and not gMySpectator.Hand.CanAddFieldPlan(P, ftVegeField) then Exit;

                  makeCheckpoint := not gTerrain.TileIsVegeField(P);
                  gMySpectator.Hand.AddField(P, ftVegeField, gCursor.MapEdVegeFieldAge, gCursor.GrainType);
                  if makeCheckpoint then
                    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                              [gResTexts[TX_WORD_CORN_FIELD], P.ToString]));
                end;
    cmWine:   begin
                if not gTerrain.TileIsWineField(P) and not gMySpectator.Hand.CanAddFieldPlan(P, ftWine) then Exit;

                makeCheckpoint := not gTerrain.TileIsWineField(P);
                gMySpectator.Hand.AddField(P, ftWine, gCursor.MapEdWineFieldAge, gftWineRed);
                if makeCheckpoint then
                  fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                            [gResTexts[TX_WORD_WINE_FIELD], P.ToString]));
              end;
  end;
end;


function TKMMapEditor.EraseTerrainObject(var aRemoveTxID: Integer): Boolean;
var
  P: TKMPoint;
  isCorn, isWine: Boolean;
begin
  Result := False;
  P := gCursor.Cell;
  isCorn := gTerrain.TileIsCornField(P);
  isWine := gTerrain.TileIsWineField(P);

  //Delete tile object (including corn/wine objects as well)
  if (gTerrain.Land^[P.Y,P.X].Obj <> OBJ_NONE) then
  begin
    if isCorn and (gTerrain.GetCornStage(P) in [4,5]) then
    begin
      gTerrain.SetField(P, gTerrain.Land^[P.Y,P.X].TileOwner, ftCorn, 3); // For corn, when delete corn object reduce field stage to 3
      aRemoveTxID := TX_WORD_CORN_FIELD;
    end
    else
    if gTerrain.TileIsGrassField(P) and (gTerrain.GetGrassStage(P) > 0) then
    begin
      gTerrain.SetField(P, gTerrain.Land^[P.Y,P.X].TileOwner, ftGrassLand, 0); // For corn, when delete corn object reduce field stage to 3
      aRemoveTxID := TX_WORD_CORN_FIELD;
    end
    else
    if gTerrain.TileIsVegeField(P) and (gTerrain.GetVegeStage(P) > 0) then
    begin
      gTerrain.SetField(P, gTerrain.Land^[P.Y,P.X].TileOwner, ftVegeField, 0); // For corn, when delete corn object reduce field stage to 3
      aRemoveTxID := TX_WORD_CORN_FIELD;
    end
    else
    if isWine then
    begin
      gTerrain.RemField(P);
      aRemoveTxID := TX_WORD_WINE_FIELD;
    end
    else
    begin
      gTerrain.SetObject(P, OBJ_NONE);
      aRemoveTxID := TX_WORD_OBJECT;
    end;
    Result := True; // We deleted smth here
  end;
end;


//aEraseAll - if True all objects under the cursor will be deleted
procedure TKMMapEditor.EraseObject(aEraseAll: Boolean);
var
  entity: TKMHandEntity;
  P: TKMPoint;
  fieldsChanged, isCorn, isWine: Boolean;
  removeTxID: Integer;
begin
  fieldsChanged := False;
  P := gCursor.Cell;
  entity := gMySpectator.HitTestCursor(True);
  removeTxID := -1;

  try
    case entity.EntityType of
      etUnit: begin
                // Delete unit by using precise HitTest result from gCursor (rather than Position)
                gHands.RemAnyUnit(TKMUnit(entity).Position);
                if not aEraseAll then Exit;
              end;
      etHouse:begin
                gHands.RemAnyHouse(P);
                if not aEraseAll then Exit;
              end;
    end;

    isCorn := gTerrain.TileIsCornField(P);
    isWine := gTerrain.TileIsWineField(P);

    if EraseTerrainObject(removeTxID) and not aEraseAll then
      Exit;

    // Delete tile overlay (road/corn/wine)
    if gTerrain.Land^[P.Y,P.X].TileOverlay = toRoad then
    begin
      if not fieldsChanged then
        removeTxID := TX_WORD_ROAD;

      gTerrain.RemRoad(P);
      fieldsChanged := True;
    end else
    if gTerrain.Land^[P.Y,P.X].TileOverlay <> toNone then
    begin
      if not fieldsChanged then
        removeTxID := TX_WORD_OVERLAY;

      gTerrain.SetOverlay(P, toNone, True);
      fieldsChanged := True;
    end;

    if isCorn or isWine then
    begin
      if not fieldsChanged then
        removeTxID := IfThen(isCorn, TX_WORD_CORN_FIELD, TX_WORD_WINE_FIELD);

      gTerrain.RemField(P);
      fieldsChanged := True;
    end;
  finally
    if fieldsChanged then
    begin
      Assert(removeTxID <> -1);
      fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH],
                                                [gResTexts[removeTxID], P.ToString]));
    end;
  end;
end;


procedure TKMMapEditor.DeletePlayer(aIndex: TKMHandID);
begin
  if gHands = nil then Exit;
  if gHands.Count = 0 then Exit;
  if not InRange(aIndex, 0, gHands.Count - 1) then Exit;

  Revealers[aIndex].Clear;

  gHands.RemovePlayerAssets(aIndex);
end;


procedure TKMMapEditor.ChangeOwner(aChangeOwnerForAll: Boolean);
var
  P: TKMPoint;
begin
  P := gCursor.Cell;
  //Fisrt try to change owner of object on tile
  if not ChangeEntityOwner(gMySpectator.HitTestCursorWGroup, gMySpectator.HandID) or aChangeOwnerForAll then
    //then try to change owner tile (road/field/wine)
    if ((gTerrain.Land^[P.Y, P.X].TileOverlay = toRoad) or (LandMapEd^[P.Y, P.X].CornOrWine <> 0))
      and (gTerrain.Land^[P.Y, P.X].TileOwner <> gMySpectator.HandID) then
    begin
      gTerrain.Land^[P.Y, P.X].TileOwner := gMySpectator.HandID;
      fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_CHOWNER_SMTH], [P.ToString, '']));
    end;
end;

procedure TKMMapEditor.SetRandomRes;
var
  P: TKMPoint;
  H : TKMHouse;
  I, C, lC, hC : Integer;
  WT : TKMWareType;
begin
  P := gCursor.Cell;
  H := gHands.HousesHitTest(P.X, P.Y);

  if not H.IsValid(htAny, false, true) then
    Exit;
  lC := gCursor.MapEd_WaresMinCount;
  hC := gCursor.MapEd_WaresMaxCount;

  for I := 1 to WARES_IN_OUT_COUNT do
  begin

    WT := H.WareInput[I];
    if not (WT in [wtNone ,wtAll, wtFood, wtWarfare]) then
    begin
      if gCursor.MapEd_WaresRandomCount and (lC < 6) then
        C := lC + KaMRandom( IfThen(hc = 6, H.GetMaxInWare, hc) - lC + 1, 'TKMMapEditor.SetRandomRes 1')
      else
        C := IfThen(lC = 6, H.GetMaxOutWare, lc);
      H.ResIn[I] := C;
    end;

    WT := H.WareOutput[I];
    if not (WT in [wtNone ,wtAll, wtFood, wtWarfare]) then
    begin
      if gCursor.MapEd_WaresRandomCount and (lC < 6) then
        C := lC + KaMRandom( IfThen(hc = 6, H.GetMaxOutWare, hc) - lC + 1, 'TKMMapEditor.SetRandomRes 1')
      else
        C := IfThen(lC = 6, H.GetMaxOutWare, lc);
      H.ResOut[I] := C;
    end;
    
  end;

end;

procedure TKMMapEditor.UpdateMapEdCursorParams(aMode: TKMChangeDefenceTypeMode; aDirInc: Integer = 1);
begin
  case aMode of
    //change direction forward
    cdmDir:         gCursor.MapEdDirection := KMAddDirection(gCursor.MapEdDirection, aDirInc);
    // change group type of defence position
    cdmGroupType:   if gCursor.MapEdDefPosGroupType = GROUP_TYPE_MAX then
                      gCursor.MapEdDefPosGroupType := GROUP_TYPE_MIN
                    else
                      gCursor.MapEdDefPosGroupType := TKMGroupType(Ord(gCursor.MapEdDefPosGroupType) + 1);
    //change defence type - defensive/attacking
    cdmDefPosType:  if gCursor.MapEdDefPosType = dtFrontLine then
                      gCursor.MapEdDefPosType := dtBackLine
                    else
                    if gCursor.MapEdDefPosType = dtBackLine then
                      gCursor.MapEdDefPosType := dtGuardLine
                    else
                      gCursor.MapEdDefPosType := dtFrontLine;

  end;
end;


//Change owner for specified object
//returns True if owner was changed successfully
function TKMMapEditor.ChangeEntityOwner(aEntity: TKMHandEntity; aOwner: TKMHandID): Boolean;
var
  house: TKMHouse;
begin
  Result := False;
  if (aEntity = nil) or (aEntity.Owner = aOwner) then Exit;

  case aEntity.EntityType of
    etNone:   ;
    etHouse:  begin
                house := aEntity.AsHouse;
                house.OwnerUpdate(aOwner, True);
                gTerrain.SetHouseAreaOwner(house.Position, house.HouseType, aOwner); // Update minimap colors
                Result := True;
                fHistory.MakeCheckpoint(caHouses, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_CHOWNER_SMTH],
                                                         [gRes.Houses[house.HouseType].HouseName, house.Entrance.ToString]));
              end;
    etUnit:   begin
                if aEntity.AsUnit.IsAnimal then Exit;

                aEntity.AsUnit.OwnerUpdate(aOwner, True);
                Result := True;
                fHistory.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_CHOWNER_SMTH],
                                                        [gRes.Units[aEntity.AsUnit.UnitType].GUIName,
                                                         aEntity.AsUnit.Position.ToString]));
              end;
    etGroup:  begin
                aEntity.AsGroup.OwnerUpdate(aOwner, True);
                Result := True;
                fHistory.MakeCheckpoint(caUnits, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_CHOWNER_SMTH],
                                                        [gRes.Units[aEntity.AsGroup.FlagBearer.UnitType].GUIName,
                                                         aEntity.AsGroup.FlagBearer.Position.ToString]));
              end;
  end;
end;


procedure TKMMapEditor.MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer);
begin
  // not used atm
end;


procedure TKMMapEditor.MouseDown(Button: TMouseButton);
begin
  fAddedHouse := false;
  if gCursor.Mode in [cmObjects, cmObjectsBrush, cmElevate, cmEqualize, cmConstHeight,cmBrush, cmTiles, cmTileSelection] then
    fTerrainPainter.SetTempTerKind;

  if (Button = mbLeft) then
    case gCursor.Mode of
      cmSelection:  fSelection.Start;
      cmVegeField,
      cmGrassLand,
      cmField,
      cmWine:       UpdateField;
      cmObjects:    begin
                      fLastErasedObjectLoc := KMPOINT_INVALID_TILE;
                      fLastRemoveTxID := -1;
                    end;
      cmChangeResCount:   SetRandomRes;
  end;
end;


procedure TKMMapEditor.ProceedRoadCursorMode;
var
  P: TKMPoint;
begin
  P := gCursor.Cell;
  if gMySpectator.Hand.CanAddFieldPlan(P, ftRoad) {or (gTerrain.TileHasRoad(P) and (gTerrain.GetRoadType(P) <> gCursor.RoadType))} then
  begin
    //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
    if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
      gTerrain.RemField(P);

    gMySpectator.Hand.AddRoad(P, gCursor.RoadType);
    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH], [gResTexts[TX_WORD_ROAD], P.ToString]));
  end;
end;


procedure TKMMapEditor.ProceedPalisadeCursorMode;
var
  P: TKMPoint;
begin
  P := gCursor.Cell;
  if gMySpectator.Hand.CanAddFieldPlan(P, ftPalisade) then
  begin
    gMySpectator.Hand.AddPalisade(P)
    //gTerrain.SetPalisade(P, gMySpectator.HandID)
    //fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH], [gResTexts[TX_WORD_ROAD], P.ToString]));
  end;
end;
{
procedure TKMMapEditor.ProceedEraseCursorMode;
var
  P: TKMPoint;
begin
  P := gCursor.Cell;
  gHands.RemAnyHouse(P);
  if gTerrain.Land^[P.Y,P.X].TileOverlay = toRoad then
  begin
    gTerrain.RemRoad(P);
    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH],
                                              [gResTexts[TX_WORD_ROAD], P.ToString]));
  end else
  if gTerrain.Land^[P.Y,P.X].TileOverlay <> toNone then
  begin
    gTerrain.SetOverlay(P, toNone, True);
    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH],
                                              [gResTexts[TX_WORD_OVERLAY], P.ToString]));
  end;

  if gTerrain.TileIsCornField(P) then
  begin
    gTerrain.RemField(P);
    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH],
                                              [gResTexts[TX_WORD_CORN_FIELD], P.ToString]));
  end
  else
  if gTerrain.TileIsWineField(P) then
  begin
    gTerrain.RemField(P);
    fHistory.MakeCheckpoint(caTerrain, Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH],
                                              [gResTexts[TX_WORD_WINE_FIELD], P.ToString]));
  end else
  if gTerrain.TileHasPalisade(P.X, P.Y) then
  begin
    gTerrain.SetObject(P, 255);
  end;
end;
     }


procedure TKMMapEditor.ApplyBigErase(Size : Integer = 0);
begin
  if Size = 0 then
    IterateOverArea(gCursor.Cell, gCursor.MapEdSize, false, DoBigErase)
  else
    IterateOverArea(gCursor.Cell, 1, false, DoBigErase)

end;

procedure TKMMapEditor.DoBigErase(const X: Integer; const Y: Integer);
var
  P: TKMPoint;
begin
  if not gTerrain.TileInMapCoords(X, Y) then
    Exit;

  P := KMPoint(X,Y);
  gHands.RemAnyHouse(P);
  if gTerrain.Land^[P.Y,P.X].TileOverlay = toRoad then
  begin
    gTerrain.RemRoad(P);
  end else
  if gTerrain.Land^[P.Y,P.X].TileOverlay <> toNone then
  begin
    gTerrain.SetOverlay(P, toNone, True);
  end;

  if gTerrain.TileIsCornField(P) then
  begin
    gTerrain.RemField(P);
  end
  else
  if gTerrain.TileIsGrassField(P) then
  begin
    gTerrain.RemField(P);
  end
  else
  if gTerrain.TileIsVegeField(P) then
  begin
    gTerrain.RemField(P);
  end
  else
  if gTerrain.TileIsWineField(P) then
  begin
    gTerrain.RemField(P);
  end else
  if gTerrain.TileHasPalisade(P.X, P.Y) then
  begin
    gTerrain.RemPalisade(P);
  end;

end;


procedure TKMMapEditor.MouseMove;
var
  P: TKMPoint;
  H : TKMHouse;
begin
  // Only allow placing of roads etc. with the left mouse button
  if not (ssLeft in gCursor.SState) then Exit;

  P := gCursor.Cell;
  case gCursor.Mode of
    cmPalisade:   ProceedPalisadeCursorMode;
    cmRoad:       ProceedRoadCursorMode;
    cmVegeField,
    cmGrassLand,
    cmField,
    cmWine:       UpdateField;
    cmUnits:      ProceedUnitsCursorMode;
    cmErase:      ApplyBigErase;
    cmSelection:  fSelection.Resize;
    cmObjects:    if gCursor.Tag1 = OBJ_NONE then
                  begin
                    if EraseTerrainObject(fLastRemoveTxID) then
                      fLastErasedObjectLoc := gCursor.Cell;
                  end;
    cmPaintBucket:      ChangeOwner(ssShift in gCursor.SState);
    cmUniversalEraser:  EraseObject(ssShift in gCursor.SState);
    cmChangeResCount:   SetRandomRes;
    cmHouses:    if (ssShift in gCursor.SState) then
                  if TKMHouseType(gCursor.Tag1) in WALL_HOUSES then
                    if gMySpectator.Hand.CanAddHousePlan(P, TKMHouseType(gCursor.Tag1)) then
                    begin
                      if gCursor.MapEd_HouseSite then
                      begin
                        H := gMySpectator.Hand.AddHouseWip(TKMHouseType(gCursor.Tag1), P);
                        H.UpdatePosition(P);
                        H.UpdatePosition(P);
                        gTerrain.SetRoad(H.Entrance, H.Owner, rtStone);
                        H.BuildingState := hbsWood;
                      end else
                        H := gMySpectator.Hand.AddHouse(TKMHouseType(gCursor.Tag1), P.X, P.Y, True);

                      fAddedHouse := true;
                      if H <> nil then
                      begin
                        if gCursor.MapEd_HouseStyle > 0 then
                          H.Style := gCursor.MapEd_HouseStyle;

                        if gCursor.MapEd_HouseLevel > 0 then
                          H.CurrentLevel := gCursor.MapEd_HouseLevel;
                      end;

                      {fHistory.MakeCheckpoint(caHouses,
                                              Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                     [gRes.Houses[TKMHouseType(gCursor.Tag1)].HouseName,
                                                      P.ToString]));}
                    end;
  end;
end;


procedure TKMMapEditor.DetermineGroupFormationAndDir(const aLoc: TKMPoint; aGroupType: TKMGroupType;
                                                     out aFormation: TKMFormation; out aDir: TKMDirection);
var
  DP: TAIDefencePosition;
  determined: Boolean;
begin
  determined := False;
  if (mlDefencesAll in gGameParams.VisibleLayers)
    or ((gCursor.Mode = cmMarkers) and (gCursor.Tag1 = MARKER_DEFENCE)) then
  begin
    DP := gMySpectator.Hand.AI.General.DefencePositions.FindPositionAtLoc(aLoc);
    if (DP <> nil) and (DP.GroupType = aGroupType) then
    begin
      aDir := DP.Position.Dir;
      aFormation.CopyFrom(gMySpectator.Hand.AI.General.DefencePositions.TroopFormations[aGroupType]);
      determined := True;
    end;
  end;

  if not determined then
  begin
    aDir := gCursor.MapEdDirection;
    if gCursor.MapEdGroupFormation.NumUnits > 0 then
      aFormation.CopyFrom(gCursor.MapEdGroupFormation)
    else
    begin
      if not (aGroupType in [GROUP_TYPE_MIN..GRoUP_TYPE_MAX]) then
        aGroupType := gtMelee;

      aFormation.CopyFrom(gMySpectator.Hand.AI.General.DefencePositions.TroopFormations[aGroupType]);
    end;
  end;
end;


procedure TKMMapEditor.ProceedRemUnit(const X, Y : Integer);
var entity : TKMHandEntity;
begin
  if not gTerrain.TileInMapCoords(X, Y) then Exit;

  entity := gHands.PlayerAnimals.UnitsHitTest(X, Y);
  if entity = nil then
    entity := gHands.UnitsHitTest(X, Y);

  if entity.IsUnit then
    gHands.RemAnyUnit(TKMUnit(entity).Position);
end;

procedure TKMMapEditor.ProceedUnitsCursorMode;
var
  P: TKMPoint;
  //entity: TKMHandEntity;
  formation: TKMFormation;
  GT: TKMGroupType;
  dir: TKMDirection;
  U: TKMUnit;
begin
  P := gCursor.Cell;
  if gCursor.Tag1 = UNIT_REMOVE_TAG then
  begin
    IterateOverArea(P, gCursor.MapEdSize, gCursor.MapEdShape = hsSquare, ProceedRemUnit);
    {
    entity := gMySpectator.HitTestCursor(True);
    // Delete unit by using precise HitTest result from gCursor (rather than Position)
    if entity.IsUnit then
      gHands.RemAnyUnit(TKMUnit(entity).Position);}
  end
  else
  if gTerrain.CanPlaceUnit(P, TKMUnitType(gCursor.Tag1)) then
  begin
    formation.NumUnits := 1;
    formation.UnitsPerRow := 1;

    //Check if we can really add a unit
    if TKMUnitType(gCursor.Tag1) in UNITS_CITIZEN then
    begin

      U := gMySpectator.Hand.AddUnit(TKMUnitType(gCursor.Tag1), P, False);

      if medUnitBoots in gCursor.MapEd_Modifications then
        U.BootsAdded := true;
    end
    else
    if TKMUnitType(gCursor.Tag1) in UNITS_WARRIORS then
    begin
      GT := UNIT_TO_GROUP_TYPE[TKMUnitType(gCursor.Tag1)];

      DetermineGroupFormationAndDir(P, GT, formation, dir);

      gMySpectator.Hand.AddUnitGroup(TKMUnitType(gCursor.Tag1), P, dir, formation.UnitsPerRow, formation.NumUnits)
    end
    else
    begin
      U := gHands.PlayerAnimals.AddUnit(TKMUnitType(gCursor.Tag1), P);
      if U is TKMUNitFish then
        TKMUNitFish(U).FishCount := gCursor.MapEdFishCount;
    end;
  end;
end;

procedure TKMMapEditor.AddWorkersToHouses;
var I, J, K, Count, unitsCounter : Integer;
  H : TKMHouse;
  UT : TKMUnitType;
  U : TKMUnit;
  P : TKMPoint;
  unitsAdded : array[TKMUnitType] of Byte;
  procedure AddSerfsToStore(aLoc : TKMPoint);
  var L : Integer;
  begin
    for L := 0 to 8 do
    begin
      U := gMySpectator.Hand.AddUnit(utSerf, P, true);

      if U <> nil then
        if gCursor.MapEd_UnitAddBoots then
          U.BootsAdded := true;
    end;
  end;
begin

  Count := gMySpectator.Hand.Houses.Count;
  for I := 0 to count - 1 do
  begin
    H := gMySpectator.Hand.Houses[I];
    if H = nil then
      Continue;
    if H.IsDestroyed or (H.GetHealth = 0) then

    if H.HouseType in [htBarracks, htTownhall, htSchool, htInn, htMarket] then
      Continue;
    if H.IsClosedForWorker then
      Continue;
    //if H.HSpec.WorkerTypes = [] then
    //  Continue;
    P := KMPointBelow(H.Entrance);
    //check first if units where added automatically
    U := gHands.UnitsHitTest(P.X, P.Y);
    if U <> nil then     
      if (U.Owner = H.Owner) and H.CanHasWorker(U.UnitType) then
        Continue;

    if H.HouseType = htStore then
    begin
      if U = nil then
        AddSerfsToStore(P);
      Continue;
    end;
    FillChar(unitsAdded, sizeOf(unitsAdded), #0);
    unitsCounter := H.HSpec.MaxWorkersCount;
    for J := 0 to unitsCounter do  
      if unitsCounter > 0 then
      begin
        if length(H.HSpec.Workers) = 0 then
        begin
          unitsCounter := 0;
          Continue;
        end;

        for K := 0 to High(H.HSpec.Workers) do
        begin
          UT := H.HSpec.Workers[K].UnitType;

          if unitsCounter <= 0 then
            Break;

          if not gRes.Units[UT].IsValid then
            Continue;
          if unitsAdded[UT] >= H.HSpec.Workers[K].MaxCount then
            Continue;

          //U := nil;
          //if gTerrain.CanPlaceUnit(P, UT) then
          Inc(unitsAdded[UT]);
          U := gMySpectator.Hand.AddUnit(UT, P, true);

          if U <> nil then
            if gCursor.MapEd_UnitAddBoots then
              U.BootsAdded := true;

          if U <> nil then
            dec(unitsCounter);
        end;
      end;

  end;

end;
procedure TKMMapEditor.Reset;
begin
  if Self = nil then Exit;
  
  ActiveMarker.MarkerType := mmtNone;
end;


function TKMMapEditor.GetCheckpointObjectsStr(removeTxID: Integer = TX_WORD_OBJECT): string;
begin
  Result := GetCheckpointObjectsStr(gCursor.Cell, removeTxID);
end;


function TKMMapEditor.GetCheckpointObjectsStr(aCell: TKMPoint; removeTxID: Integer = TX_WORD_OBJECT): string;
begin
  if gCursor.Tag1 = OBJ_NONE then
    Result := Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_REMOVE_SMTH], [gResTexts[removeTxID], aCell.ToString])
  else
    Result := Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH] + ' %s', [gResTexts[removeTxID], IntToStr(gCursor.Tag1), aCell.ToString])
end;


function TKMMapEditor.GetHistory: TKMMapEditorHistory;
begin
  if Self = nil then Exit(nil);
  
  Result := fHistory;
end;


procedure TKMMapEditor.AddDefenceMarker(const aLoc: TKMPoint);
const
  UNIT_TYPES_BY_GT_LVL: array[GROUP_TYPE_MIN..GROUP_TYPE_MAX, TKMGroupLevel] of TKMUnitType =
                          ((utMilitia,  utAxeFighter,   utSwordFighter),
                           (utRebel,    utLanceCarrier, utPikeman),
                           (utRogue,    utBowman,       utCrossbowman),
                           (utVagabond, utScout,        utKnight),
                           (utCatapult, utWoodenWall,     utBallista),
                           (utRam,      utRam,          utRam),
                           (utClubMan,  utMaceFighter,  utFlailFighter),
                           (utBattleShip,  utBattleShip,  utBattleShip)
                          );

var
  groupType: TKMGroupType;
  dir: TKMDirection;
  G: TKMUnitGroup;
  formation : TKMFormation;
begin
  dir := gCursor.MapEdDirection;
  groupType := gCursor.MapEdDefPosGroupType;

  G := gHands.GroupsHitTest(aLoc.X, aLoc.Y);
  if G <> nil then
  begin
    dir := G.Direction;
    groupType := G.GroupType;
  end;

  if gMySpectator.Hand.AI.General.DefencePositions.FindPositionAtLoc(aLoc) <> nil then
    Exit;

  if gCursor.MapEdDefPosSetGroup then
  begin
    formation := gMySpectator.Hand.AI.General.DefencePositions.TroopFormations[groupType];
    if G = nil then
      gMySpectator.Hand.AddUnitGroup(UNIT_TYPES_BY_GT_LVL[groupType, gCursor.MapEdDefPosGroupLevel],
                                     aLoc, dir, formation.UnitsPerRow, formation.NumUnits);
  end;

  gMySpectator.Hand.AI.General.DefencePositions.Add(KMPointDir(aLoc, dir),
                                                    groupType,
                                                    gCursor.MapEdSize,
                                                    gCursor.MapEdDefPosType);

end;

procedure TKMMapEditor.AddDefendMarker(const aLoc: TKMPoint);
begin
  if gMySpectator.Hand.AI.General.DefendPositions.FindDefendPosAtLoc(aLoc) <> nil then
    Exit;
  gMySpectator.Hand.AI.General.DefendPositions.Add(aLoc, gCursor.MapEdSize);
end;

procedure TKMMapEditor.MouseUp(Button: TMouseButton; aOverMap: Boolean);

  function IsObjectDeleting: Boolean;
  begin
    Result := gCursor.Tag1 = OBJ_NONE;
  end;

  procedure ManageObjects;
  begin
    if IsObjectDeleting then
    begin
      if (fLastErasedObjectLoc <> KMPOINT_INVALID_TILE) and (fLastRemoveTxID <> -1) then
        fHistory.MakeCheckpoint(caTerrain, GetCheckpointObjectsStr(fLastErasedObjectLoc, fLastRemoveTxID));
    end
    else
      fHistory.MakeCheckpoint(caTerrain, GetCheckpointObjectsStr);
  end;

var
  P: TKMPoint;
  removeTxID: Integer;
  H : TKMHouse;
  I : integer;
begin
  //If the mouse is released over controls, most actions don't happen
  if not aOverMap then
  begin
    //Still need to make a checkpoint since painting has now stopped
    case gCursor.Mode of
      cmElevate:  fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE]);
      cmEqualize: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE]);
      cmElevateAll:   fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_HEIGHTS_ELEVATE_ALL]);
      cmConstHeight:  fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_HEIGHTS_CONST]);
      cmBrush:    fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_BRUSH]);
      cmObjects:  ManageObjects;
      cmObjectsBrush: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_OBJECTS_BRUSH]);
      cmTiles:    fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HINTS_TILES]);
      cmOverlays: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_OVERLAYS]);
    end;
    Exit;
  end;

  P := gCursor.Cell; //Get cursor position tile-wise
  case Button of
    mbLeft:   case gCursor.Mode of
                cmPalisade:   ProceedPalisadeCursorMode;
                cmRoad:       ProceedRoadCursorMode;
                cmHouses:     begin
                                if gMySpectator.Hand.CanAddHousePlan(P, TKMHouseType(gCursor.Tag1)) then
                                begin
                                  if gCursor.MapEd_HouseSite then
                                  begin
                                    H := gMySpectator.Hand.AddHouseWip(TKMHouseType(gCursor.Tag1), P);
                                    H.UpdatePosition(P);
                                    gTerrain.SetRoad(H.Entrance, gMySpectator.HandID, rtStone);
                                    H.BuildingState := hbsWood;
                                  end else
                                  begin
                                    H := gMySpectator.Hand.AddHouse(TKMHouseType(gCursor.Tag1), P.X, P.Y, True);

                                    if H <> nil then
                                    begin
                                      if gCursor.MapEd_HouseStyle > 0 then
                                        H.Style := gCursor.MapEd_HouseStyle;

                                      if gCursor.MapEd_HouseLevel > 0 then
                                        H.CurrentLevel := gCursor.MapEd_HouseLevel;

                                      if medHouseNoResource in gCursor.MapEd_Modifications then
                                        H.DontNeedRes := true;

                                      if medHouseForceWorking in gCursor.MapEd_Modifications then
                                        H.ForceWorking := true;
                                      case gCursor.MapEd_HouseFill of
                                        0, 10 : ;//do nothing
                                        1, 2, 3, 4, 5 : if H.GetMaxInWare <= 5 then
                                                          for I := 1 to 4 do
                                                          begin
                                                            if H.WareInput[I] <> wtNone then
                                                              H.WareAddToIn(H.WareInput[I], gCursor.MapEd_HouseFill, true);
                                                            if H.WareOutput[I] <> wtNone then
                                                              H.WareAddToOut(H.WareOutput[I], gCursor.MapEd_HouseFill);
                                                          end;
                                        11, 12, 13, 14, 15 :  if H.GetMaxInWare <= 5 then
                                                                for I := 1 to 4 do
                                                                begin
                                                                  if H.WareInput[I] <> wtNone then
                                                                    H.WareAddToIn(H.WareInput[I], KamRandom(gCursor.MapEd_HouseFill - 10 + 1, 'MapEd:FillHouse Input'), true);
                                                                  if H.WareOutput[I] <> wtNone then
                                                                    H.WareAddToOut(H.WareOutput[I], KamRandom(gCursor.MapEd_HouseFill - 10 + 1, 'MapEd:FillHouse Output'));
                                                                end;

                                        6, 16 : if H.GetMaxInWare <= 5 then
                                                  for I := 1 to 4 do
                                                  begin
                                                    if H.WareInput[I] <> wtNone then
                                                      H.WareAddToIn(H.WareInput[I], 5, true);
                                                    if H.WareOutput[I] <> wtNone then
                                                      H.WareAddToOut(H.WareOutput[I], 5);
                                                  end;
                                      end;

                                    end;
                                  end;

                                  fAddedHouse := H <> nil;
                                end;
                                if fAddedHouse then
                                  fHistory.MakeCheckpoint(caHouses,
                                                          Format(gResTexts[TX_MAPED_HISTORY_CHPOINT_ADD_SMTH],
                                                                 [gRes.Houses[TKMHouseType(gCursor.Tag1)].HouseName,
                                                                  P.ToString]));

                                //Holding shift allows to place that house multiple times
                                if not (ssShift in gCursor.SState) then
                                begin
                                  gCursor.Tag1 := 0; //Reset tag
                                  gCursor.Mode := cmRoad;
                                end;


                              end;
                cmElevate:    fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE]);
                cmEqualize:   fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE]);
                cmElevateAll: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_HEIGHTS_ELEVATE_ALL]);
                cmConstHeight: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_HEIGHTS_CONST]);
                cmBrush:      fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_BRUSH]);
                cmObjects:    if IsObjectDeleting then
                              begin
                                if EraseTerrainObject(removeTxID) then
                                  fHistory.MakeCheckpoint(caTerrain, GetCheckpointObjectsStr(removeTxID))
                                else
                                  ManageObjects;
                              end
                              else
                                fHistory.MakeCheckpoint(caTerrain, GetCheckpointObjectsStr);
                cmObjectsBrush: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_OBJECTS_BRUSH]);
                cmTiles:      fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HINTS_TILES] + ' ' + P.ToString);
                cmOverlays:   fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_OVERLAYS] + ' ' + P.ToString);
                cmMagicWater: fTerrainPainter.MagicWater(P);
                cmEyedropper: begin
                                fTerrainPainter.Eyedropper(P);

                                if Assigned(OnEyedropper) then
                                  OnEyedropper(gCursor.Tag1);

                                if not (ssShift in gCursor.SState) then  //Holding shift allows to choose another tile
                                  gCursor.Mode := cmTiles;
                              end;
                cmRotateTile: fTerrainPainter.RotateTile(P);
                cmUnits:      ProceedUnitsCursorMode;
                cmMarkers:    case gCursor.Tag1 of
                                MARKER_REVEAL:        fRevealers[gMySpectator.HandID].Add(P, gCursor.MapEdSize);
                                MARKER_DEFENCE:       AddDefenceMarker(P);
                                MARKER_CENTERSCREEN:  begin
                                                        gMySpectator.Hand.CenterScreen := P;
                                                        //Updating XY display is done in InterfaceMapEd
                                                      end;
                                MARKER_AISTART:       gMySpectator.Hand.AI.Setup.StartPosition := P;
                                MARKER_RALLY_POINT:   if gMySpectator.Selected is TKMHouseWFlagPoint then
                                                        TKMHouseWFlagPoint(gMySpectator.Selected).FlagPoint := P;
                                MARKER_DEFEND:       AddDefendMarker(P);
                                MARKER_ANIMALS:      gHands.PlayerAnimals.AddSpawner(P, gCursor.MapEdSize,
                                                                                    gCursor.MapEd_AnimalsCount,
                                                                                    gCursor.MapEd_AnimalsPace,
                                                                                    gCursor.MapEd_Animals);
                              end;
                cmErase:      begin
                                ApplyBigErase;

                                if gGameParams.IsMapEditor then
                                  fHistory.MakeCheckpoint(caAll, 'Removed Road/House/Wine');

                              end;

                cmPaintBucket:      ChangeOwner(ssShift in gCursor.SState);
                cmUniversalEraser:  EraseObject(ssShift in gCursor.SState);
              end;
    mbRight:  case gCursor.Mode of
                              //Actual change was made in UpdateStateIdle, we just register it is done here
                cmElevate:    fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE]);
                cmEqualize:   fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE]);
                cmElevateAll: fHistory.MakeCheckpoint(caTerrain, gResTexts[TX_MAPED_HEIGHTS_ELEVATE_ALL]);
                cmObjects,
                cmEyedropper,
                cmRotateTile: gCursor.Mode := cmNone;

                cmMarkers:    case gCursor.Tag1 of
                                MARKER_DEFENCE:       begin
                                                        if ssCtrl in gCursor.SState then
                                                          UpdateMapEdCursorParams(cdmDir, -1)//change dir backward
                                                        else
                                                        if ssShift in gCursor.SState then
                                                          UpdateMapEdCursorParams(cdmGroupType)//change gtype
                                                        else
                                                        if ssAlt in gCursor.SState then
                                                          UpdateMapEdCursorParams(cdmDefPosType)//change att/def
                                                        else
                                                          UpdateMapEdCursorParams(cdmDir);//change dir
                                                      end;
                              end;

                cmUnits:      if ssCtrl in gCursor.SState then
                                UpdateMapEdCursorParams(cdmDir, -1)
                              else
                              if ssShift in gCursor.SState then
                                UpdateMapEdCursorParams(cdmDir, 4)
                              else
                                UpdateMapEdCursorParams(cdmDir);
              end;
  end;
end;


procedure TKMMapEditor.PaintDefences(aLayer: TKMPaintLayer);
var
  DP: TAIDefencePosition;
  Loc1, Loc2 : TKMPointF;
  DD : TAIDefendPosition;
begin
  if not (melDefences in fVisibleLayers) then Exit;

  case aLayer of

    plTerrain:  if ActiveMarker.MarkerType = mmtDefence then
                begin
                  //Render defence position tiles covered
                  if InRange(ActiveMarker.Index, 0, gHands[ActiveMarker.Owner].AI.General.DefencePositions.Count - 1) then
                  begin
                    DP := gHands[ActiveMarker.Owner].AI.General.DefencePositions[ActiveMarker.Index];
                    gRenderPool.RenderDebug.RenderTiledArea(DP.Position.Loc, 0, DP.Radius, KMLengthDiag,
                                                            gHands[ActiveMarker.Owner].FlagColor AND $60FFFFFF,
                                                            icCyan);

                    gRenderAux.CircleOnTerrain(DP.Position.X - 0.5, DP.Position.Y - 0.5, 1,
                                              gHands[ActiveMarker.Owner].FlagColor AND $88FFFFFF,
                                              gHands[ActiveMarker.Owner].FlagColor);
                    if DP.PositionPatrol.Loc.X <= 0 then
                      Exit;
                    Loc1 := KMPointF(DP.Position.Loc);
                    Loc2 := KMPointF(DP.PositionPatrol.Loc);
                    Loc1.X := Loc1.X - 0.5;
                    Loc1.Y := Loc1.Y - 0.5;

                    Loc2.X := Loc2.X - 0.5;
                    Loc2.Y := Loc2.Y - 0.5;
                    gRenderPool.RenderWireTile(DP.PositionPatrol.Loc, $FF0000FF);

                    gRenderAux.LineOnTerrain([Loc1, Loc2], $FFFFFFFF, 4);
                    gRenderPool.RenderSpriteOnTile(DP.PositionPatrol.Loc,  510 + Byte(DP.PositionPatrol.Dir), gHands[ActiveMarker.Owner].FlagColor);

                  end;
                end else
                if ActiveMarker.MarkerType = mmtDefendPos then
                begin
                  //Render defence position tiles covered
                  if InRange(ActiveMarker.Index, 0, gHands[ActiveMarker.Owner].AI.General.DefendPositions.Count - 1) then
                  begin
                    DD := gHands[ActiveMarker.Owner].AI.General.DefendPositions[ActiveMarker.Index];
                    gRenderAux.CircleOnTerrain(DD.Position.X, DD.Position.Y, 2,
                                              gHands[ActiveMarker.Owner].FlagColor AND $08FFFFFF,
                                              gHands[ActiveMarker.Owner].FlagColor);
                    gRenderPool.RenderDebug.RenderTiledArea(DD.Position, 0, DD.Radius, KMLengthDiag,
                                                            gHands[ActiveMarker.Owner].FlagColor AND $60FFFFFF,
                                                            icCyan);
                  end;
                end;
  end;
end;

procedure TKMMapEditor.PaintSpawners(aLayer: TKMPaintLayer);
var Loc : TKMpoint;
begin
  if not (melSpawners in fVisibleLayers) then Exit;
  if ActiveMarker.MarkerType <> mmtSpawner then
    Exit;
  case aLayer of
    plTerrain:  begin
                  if InRange(ActiveMarker.Index, 0, gHands.PlayerAnimals.SpawnersCount - 1) then
                  begin
                    loc := gHands.PlayerAnimals.Spawners[ActiveMarker.Index].Loc;
                    gRenderAux.CircleOnTerrain(loc.X-0.5, loc.Y-0.5, 1,
                                              $8800FF00, $FF00FF00
                                              );
                  end;
                end;
  end;
end;

procedure TKMMapEditor.PaintRevealFOW(aLayer: TKMPaintLayer);
var
  I, K: Integer;
  loc: TKMPoint;
begin
  if not (melRevealFOW in fVisibleLayers) then Exit;

  for I := 0 to gHands.Count - 1 do
    for K := 0 to fRevealers[I].Count - 1 do
    begin
      loc := fRevealers[I][K];
      case aLayer of
        plTerrain:  begin
                      gRenderAux.CircleOnTerrain(loc.X-0.5, loc.Y-0.5,
                                             fRevealers[I].Tag[K],
                                             gHands[I].FlagColor and $20FFFFFF,
                                             gHands[I].FlagColor);

                    end;
        plCursors:  gRenderPool.RenderSpriteOnTile(loc, 394, gHands[I].FlagColor);
      end;
    end;

  if ActiveMarker.MarkerType <> mmtRevealFOW then
    Exit;
  case aLayer of
    plTerrain:  begin
                  if InRange(ActiveMarker.Index, 0, fRevealers[ActiveMarker.Owner].Count - 1) then
                  begin
                    loc := fRevealers[ActiveMarker.Owner][ActiveMarker.Index];
                    gRenderAux.CircleOnTerrain(loc.X-0.5, loc.Y-0.5, 1,
                                              gHands[ActiveMarker.Owner].FlagColor AND $88FFFFFF,
                                              gHands[ActiveMarker.Owner].FlagColor);
                  end;
                end;
  end;
end;


procedure TKMMapEditor.PaintCenterScreen(aLayer: TKMPaintLayer);
var
  I: Integer;
  loc: TKMPoint;
begin
  if not (melCenterScreen in fVisibleLayers) then Exit;

  for I := 0 to gHands.Count - 1 do
    if gHands[I].HasAssets then
    begin
      loc := gHands[I].CenterScreen;
      case aLayer of
        plTerrain:  gRenderAux.SquareOnTerrain(loc.X - 3, loc.Y - 2.5,
                                               loc.X + 2, loc.Y + 1.5,
                                               gHands[I].FlagColor);
        plCursors:  gRenderPool.RenderSpriteOnTile(loc, 391, gHands[I].FlagColor);
      end;
    end;
end;


procedure TKMMapEditor.PaintAIStart(aLayer: TKMPaintLayer);
var
  I: Integer;
  loc: TKMPoint;
begin
  if not (melAIStart in fVisibleLayers) then Exit;

  for I := 0 to gHands.Count - 1 do
    if gHands[I].HasAssets then
    begin
      loc := gHands[I].AI.Setup.StartPosition;
      case aLayer of
        plTerrain:  gRenderAux.SquareOnTerrain(loc.X - 3, loc.Y - 2.5,
                                               loc.X + 2, loc.Y + 1.5,
                                               gHands[I].FlagColor);
        plCursors:  gRenderPool.RenderSpriteOnTile(loc, 390, gHands[I].FlagColor);
      end;
    end;
end;


procedure TKMMapEditor.Paint(aLayer: TKMPaintLayer; const aClipRect: TKMRect);
var
  I, K: Integer;
  P: TKMPoint;
  G: TKMUnitGroup;
begin
  P := gCursor.Cell;

  if aLayer = plCursors then
    //With Buildings tab see if we can remove Fields or Houses
    if gCursor.Mode = cmErase then
      if gTerrain.TileIsCornField(P)
        or gTerrain.TileIsWineField(P)
        or gTerrain.TileHasPalisade(P.X, P.Y)
        or gTerrain.TileIsGrassField(P.X, P.Y)
        or (gTerrain.Land^[P.Y,P.X].TileOverlay = toRoad)
        or (gHands.HousesHitTest(P.X, P.Y) <> nil) then
        gRenderPool.RenderWireTile(P, icCyan) //Cyan quad
      else
        gRenderPool.RenderSpriteOnTile(P, TC_BLOCK); //Red X

  PaintDefences(aLayer);
  PaintRevealFOW(aLayer);
  PaintCenterScreen(aLayer);
  PaintAIStart(aLayer);
  PaintSpawners(aLayer);

  if melSelection in fVisibleLayers then
    fSelection.Paint(aLayer, aClipRect);

  if (melMapResize in fVisibleLayers) and not KMSameRect(ResizeMapRect, KMRECT_ZERO) then
    gRenderGameAux.RenderResizeMap(ResizeMapRect);

  if melWaterFlow in fVisibleLayers then
  begin
    for I := aClipRect.Top to aClipRect.Bottom do
    for K := aClipRect.Left to aClipRect.Right do
    if gTerrain.TileIsWater(K,I) then
    begin
      //todo: Waterflow indication here
      //gRenderPool.RenderSpriteOnTile(KMPoint(K,I), )
    end;
  end;


  //Show selected group order target
  if gMySpectator.Selected is TKMUnitGroup then
  begin
    G := TKMUnitGroup(gMySpectator.Selected);
    if G.MapEdOrder.Order <> gioNoOrder then
    begin
      gRenderAux.Quad(G.MapEdOrder.Pos.Loc.X, G.MapEdOrder.Pos.Loc.Y, $40FF00FF);
      gRenderAux.LineOnTerrain(G.Position.X - 0.5, G.Position.Y - 0.5, G.MapEdOrder.Pos.Loc.X - 0.5, G.MapEdOrder.Pos.Loc.Y - 0.5, $FF0000FF );
    end;
  end;
end;


procedure TKMMapEditor.UpdateState;
begin
  if Self = nil then Exit;

  if melDeposits in fVisibleLayers then
    fDeposits.UpdateAreas([rdStone, rdCoal, rdIron, rdGold, rdFish, rdBitinIron, rdClay]);

  fTerrainPainter.UpdateState;

  //todo: if mlNavMesh in VisibleLayers then
    //gAIFields.NavMesh.Init;
end;


procedure TKMMapEditor.UpdateStateIdle;
begin
  fTerrainPainter.UpdateStateIdle;
end;


end.

