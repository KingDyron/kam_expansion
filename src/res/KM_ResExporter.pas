﻿unit KM_ResExporter;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes,
  Generics.Collections,
  KM_ResSprites, KM_ResTypes,
  KM_WorkerThread,
  KM_Defaults;


type
  TKMAtlasAddress = record
    AtlasID: Integer;
    SpriteNum: Integer; // In the atlas
    constructor New(aAtlasID, aSpriteNum: Integer);
  end;

  // Resource exporter class
  TKMResExporter = class
  private
    fExportWorkerHolder: TKMWorkerThreadHolder;

    fAtlasMap: array [TKMSpriteAtlasType] of TDictionary<Integer, TKMAtlasAddress>;

    procedure PrepareAtlasMap(aSpritePack: TKMSpritePack);

    function GetOrCreateExportWorker: TKMWorkerThread;

    procedure ExportImageFromAtlas(aSpritePack: TKMSpritePack; aSpriteID: Integer; const aFilePath: string; const aFileMaskPath: string = '');
    procedure ExportFullImageDataFromAtlas(aSpritePack: TKMSpritePack; aSpriteID: Integer; const aFolder: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExportTreeAnimHD(aOnDone: TProc<String>);
    procedure ExportTreeAnim(aOnDone: TProc<String>);
    procedure ExportHouseAnimHD(aOnDone: TProc<String>);
    procedure ExportHouseAnim(aOnDone: TProc<String>);
    procedure ExportHouseMainPics(aOnDone: TProc<String>);
    procedure ExportUnitAnimHD(aUnitFrom, aUnitTo: TKMUnitType; aExportThoughts, aExportUnused: Boolean; aOnDone: TProc<String>);
    procedure ExportUnitAnim(aUnitFrom, aUnitTo: TKMUnitType; aExportUnused: Boolean; aOnDone: TProc<String>);
    procedure ExportSpritesFromRXXToPNG(aRT: TRXType; aOnDone: TProc<String>);
    procedure ExportSpritesFromRXAToPNG(aRT: TRXType; aOnDone: TProc<String>);
  end;


implementation
uses
  TypInfo,
  KromUtils,
  KM_Resource, KM_ResUnits, KM_ResHouses, KM_ResMapElements, KM_ResTexts,
  KM_FileIO, KM_IoPNG,
  KM_Points, KM_CommonTypes, KM_Log;


{ TKMAtlasAddress }
constructor TKMAtlasAddress.New(aAtlasID, aSpriteNum: Integer);
begin
  AtlasID := aAtlasID;
  SpriteNum := aSpriteNum;
end;


{ TKMResExporter }
constructor TKMResExporter.Create;
begin
  inherited;

  fAtlasMap[saBase] := TDictionary<Integer, TKMAtlasAddress>.Create;
  fAtlasMap[saMask] := TDictionary<Integer, TKMAtlasAddress>.Create;
end;


destructor TKMResExporter.Destroy;
begin
  if fExportWorkerHolder <> nil then
    // This will ensure all queued work is completed before destruction
    FreeAndNil(fExportWorkerHolder);

  FreeAndNil(fAtlasMap[saBase]);
  FreeAndNil(fAtlasMap[saMask]);

  inherited;
end;


function TKMResExporter.GetOrCreateExportWorker: TKMWorkerThread;
begin
  if fExportWorkerHolder = nil then
    fExportWorkerHolder := TKMWorkerThreadHolder.Create('ExportWorker');

  Result := fExportWorkerHolder.Worker;
end;


procedure TKMResExporter.PrepareAtlasMap(aSpritePack: TKMSpritePack);
var
  I, K: Integer;
  SAT: TKMSpriteAtlasType;
begin
  fAtlasMap[saBase].Clear;
  fAtlasMap[saMask].Clear;

  // Map spriteID to loaded from RXA Atlases
  for SAT := Low(aSpritePack.Atlases) to High(aSpritePack.Atlases) do
    for I := Low(aSpritePack.Atlases[SAT]) to High(aSpritePack.Atlases[SAT]) do
      with aSpritePack.Atlases[SAT, I] do
        for K := 0 to High(Container.Sprites) do
          fAtlasMap[SAT].Add(Container.Sprites[K].SpriteID, TKMAtlasAddress.New(I, K));
end;


procedure TKMResExporter.ExportSpritesFromRXXToPNG(aRT: TRXType; aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  GetOrCreateExportWorker.QueueWork(procedure
    var
      sprites: TKMResSprites;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      try
        if sprites.LoadSprites(aRT, true) then
          sprites[aRT].ExportAllSpritesFromRXData(ExeDir + 'Export' + PathDelim + RX_INFO[aRT].FileName + '_a.rxx' + PathDelim);
      finally
        sprites.Free;
      end;
    end, aOnDone, 'Export from ' + RX_INFO[aRT].FileName + '.rxx');
end;


procedure TKMResExporter.ExportSpritesFromRXAToPNG(aRT: TRXType; aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  GetOrCreateExportWorker.QueueWork(procedure
    var
      I: Integer;
      folderPath: string;
      sprites: TKMResSprites;
      spritePack: TKMSpritePack;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      try
        if sprites.LoadRXASprites(aRT) then
        begin
          spritePack := sprites[aRT];

          PrepareAtlasMap(spritePack);

          folderPath := ExeDir + 'Export' + PathDelim + RX_INFO[aRT].FileName + '.rxa' + PathDelim;
          ForceDirectories(folderPath);
          for I := 1 to spritePack.RXData.Count do
            ExportFullImageDataFromAtlas(spritePack, I, folderPath);
        end;
      finally
        sprites.Free;
      end;
    end, aOnDone, 'Export from ' + RX_INFO[aRT].FileName + '.rxa');
end;


procedure TKMResExporter.ExportUnitAnimHD(aUnitFrom, aUnitTo: TKMUnitType; aExportThoughts, aExportUnused: Boolean; aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
    var
      fullFolderPath, folderPath: string;
      UT: TKMUnitType;
      ACT: TKMUnitActionType;
      animation : TKMAnimation;
      DIR: TKMDirection;
      WT: TKMWareType;
      STEP, origSpriteID, spriteID: Integer;
      used: array of Boolean;
      spritePack: TKMSpritePack;
      folderCreated: Boolean;
      sprites: TKMResSprites;
      units: TKMResUnits;
      resTexts: TKMTextLibraryMulti;

    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      sprites.LoadRXASprites(rxUnits);
      spritePack := sprites[rxUnits];

      PrepareAtlasMap(spritePack);

      units := TKMResUnits.Create;
      resTexts := TKMTextLibraryMulti.Create;
      resTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');
      resTexts.ForceDefaultLocale := True;

      folderPath := ExeDir + 'Export' + PathDelim + 'UnitAnimHD' + PathDelim;
      ForceDirectories(folderPath);

      try
        for UT := aUnitFrom to aUnitTo do
          for ACT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
          begin
            folderCreated := False;
            for DIR := dirN to dirNW do
              if units[UT].UnitAnim[ACT,DIR].Count > 0 then
                for STEP := 0 to units[UT].UnitAnim[ACT, DIR].Count - 1 do
                begin
                  origSpriteID := units[UT].UnitAnim[ACT,DIR].Step[STEP] + 1;
                  if origSpriteID = 0 then Continue;

                  if not folderCreated then
                  begin
                    //Use default locale for Unit GUIName, as translation could be not good for file system
                    fullFolderPath := folderPath + resTexts.DefaultTexts[units[UT].GUITextID] + PathDelim + UNIT_ACT_STR[ACT] + PathDelim;
                    ForceDirectories(fullFolderPath);
                    folderCreated := True;
                  end;
                end;
          end;

        SetLength(used, Length(spritePack.RXData.Size));

        //Exclude actions
        for UT := Low(TKMUnitType) to High(TKMUnitType) do
          for ACT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
            for DIR := dirN to dirNW do
              if units[UT].UnitAnim[ACT,DIR].Count > 0 then
                for STEP := 0 to units[UT].UnitAnim[ACT,DIR].Count - 1 do
                begin
                  origSpriteID := units[UT].UnitAnim[ACT,DIR].Step[STEP] + 1;
                  used[origSpriteID] := origSpriteID <> 0;
                end;

        if utSerf in [aUnitFrom..aUnitTo] then
          //serfs carrying stuff
          for WT := WARE_MIN to WARE_MAX do
          begin
            folderCreated := False;
            for DIR := dirN to dirNW do
            begin
              animation := units.SerfCarry[WT, DIR];
              for STEP := 0 to animation.Count - 1 do
              begin
                origSpriteID := animation.Step[STEP]+1;
                if origSpriteID = 0 then Continue;

                if utSerf in [aUnitFrom..aUnitTo] then
                begin
                  if not folderCreated then
                  begin
                    //Use default locale for Unit GUIName, as translation could be not good for file system
                    fullFolderPath := folderPath + resTexts.DefaultTexts[units[utSerf].GUITextID] + PathDelim + 'Delivery' + PathDelim
                                    + GetEnumName(TypeInfo(TKMWareType), Integer(WT)) + PathDelim;
                    ForceDirectories(fullFolderPath);
                    folderCreated := True;
                  end;
                end;
              end;
            end;
          end;

        {if aExportThoughts then
        begin
          for TH := thEat to High(TKMUnitThought) do
          begin
            fullFolderPath := folderPath + 'Thoughts' + PathDelim + GetEnumName(TypeInfo(TKMUnitThought), Integer(TH)) + PathDelim;
            ForceDirectories(fullFolderPath);

            for STEP := THOUGHT_BOUNDS[TH,1] to  THOUGHT_BOUNDS[TH,2] do
            begin
              origSpriteID := units[UT].UnitAnim[ACT,DIR].Step[STEP+1] + 1;
              spriteID := gRes.Interpolation.UnitThought(TH, STEP, LVL / INTERP_LEVEL);
              ExportFullImageDataFromAtlas(spritePack, spriteID, fullFolderPath);
              used[spriteID] := True;
            end;
          end;
        end;}

        if not aExportUnused then Exit;

        fullFolderPath := folderPath + '_Unused' + PathDelim;
        ForceDirectories(fullFolderPath);

        for spriteID := 1 to Length(used)-1 do
          if not used[spriteID] then
          begin
            ExportFullImageDataFromAtlas(spritePack, spriteID, fullFolderPath);
            // Stop export if async thread is terminated by application
            if TThread.CheckTerminated then Exit;
          end;
      finally
        sprites.Free;
        units.Free;
        resTexts.Free;
      end;
    end, aOnDone, 'Export HD units anim');
end;


//Export Units graphics categorized by Unit and Action
procedure TKMResExporter.ExportUnitAnim(aUnitFrom, aUnitTo: TKMUnitType; aExportUnused: Boolean; aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
    var
      fullFolderPath, folderPath: string;
      UT: TKMUnitType;
      ACT: TKMUnitActionType;
      anim: TKMAnimLoop;
      animation : TKMAnimation;
      DIR: TKMDirection;
      WT: TKMWareType;
      STEP, spriteID:integer;
      used: array of Boolean;
      rxData: TRXData;
      spritePack: TKMSpritePack;
      folderCreated: Boolean;
      sprites: TKMResSprites;
      units: TKMResUnits;
      resTexts: TKMTextLibraryMulti;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      sprites.LoadSprites(rxUnits, true); //BMP can't show alpha shadows anyways
      spritePack := sprites[rxUnits];
      rxData := spritePack.RXData;

      units := TKMResUnits.Create;
      {resTexts := TKMTextLibraryMulti.Create;
      resTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');
      resTexts.ForceDefaultLocale := True;}

      resTexts := gResTexts;

      folderPath := ExeDir + 'Export' + PathDelim + 'UnitAnim' + PathDelim;
      ForceDirectories(folderPath);

      try
        for UT := aUnitFrom to aUnitTo do
          for ACT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
          begin
            folderCreated := False;
            for DIR := dirN to dirNW do
              if units[UT].UnitAnim[ACT,DIR].Count > 0 then
                for STEP := 0 to units[UT].UnitAnim[ACT, DIR].Count - 1 do
                begin
                  spriteID := units[UT].UnitAnim[ACT,DIR].Step[STEP] + 1;
                  if spriteID = 0 then Continue;

                  if not folderCreated then
                  begin
                    //Use default locale for Unit GUIName, as translation could be not good for file system
                    fullFolderPath := folderPath + resTexts.DefaultTexts[units[UT].GUITextID] + PathDelim + UNIT_ACT_STR[ACT] + PathDelim;
                    ForceDirectories(fullFolderPath);
                    folderCreated := True;
                  end;

                  spritePack.ExportFullImageData(fullFolderPath, spriteID);
                  // Stop export if async thread is terminated by application
                  if TThread.CheckTerminated then Exit;
                end;
          end;

        SetLength(used, Length(rxData.Size));

        //Exclude actions
        for UT := Low(TKMUnitType) to High(TKMUnitType) do
          for ACT := Low(TKMUnitActionType) to High(TKMUnitActionType) do
            for DIR := dirN to dirNW do
              if units[UT].UnitAnim[ACT,DIR].Count > 0 then
                for STEP := 0 to units[UT].UnitAnim[ACT,DIR].Count - 1 do
                begin
                  spriteID := units[UT].UnitAnim[ACT,DIR].Step[STEP]+1;
                  used[spriteID] := spriteID <> 0;
                end;

        if utSerf in [aUnitFrom..aUnitTo] then
          //serfs carrying stuff
          for WT := WARE_MIN to WARE_MAX do
          begin
            folderCreated := False;
            for DIR := dirN to dirNW do
            begin
              animation := units.SerfCarry[WT, DIR];
              for STEP := 0 to anim.Count - 1 do
              begin
                spriteID := animation.Step[STEP]+1;
                if spriteID <> 0 then
                begin
                  used[spriteID] := True;
                  if utSerf in [aUnitFrom..aUnitTo] then
                  begin
                    if not folderCreated then
                    begin
                      //Use default locale for Unit GUIName, as translation could be not good for file system (like russian '����������/�������' with slash in it)
                      fullFolderPath := folderPath + resTexts.DefaultTexts[units[utSerf].GUITextID] + PathDelim + 'Delivery' + PathDelim
                                      + GetEnumName(TypeInfo(TKMWareType), Ord(WT)) + PathDelim;
                      ForceDirectories(fullFolderPath);
                      folderCreated := True;
                    end;
                    spritePack.ExportFullImageData(fullFolderPath, spriteID);
                    // Stop export if async thread is terminated by application
                    if TThread.CheckTerminated then Exit;
                  end;
                end;
              end;
            end;
          end;

        {fullFolderPath := folderPath + 'Thoughts' + PathDelim;
        ForceDirectories(fullFolderPath);
        for T := thEat to High(TKMUnitThought) do
          for STEP := THOUGHT_BOUNDS[T,1] to  THOUGHT_BOUNDS[T,2] do
          begin
            spritePack.ExportFullImageData(fullFolderPath, STEP+1);
            used[STEP+1] := True;
          end;}

        if not aExportUnused then Exit;

        fullFolderPath := folderPath + '_Unused' + PathDelim;
        ForceDirectories(fullFolderPath);

        for spriteID := 1 to Length(used)-1 do
          if not used[spriteID] then
          begin
            spritePack.ExportFullImageData(fullFolderPath, spriteID);
            // Stop export if async thread is terminated by application
            if TThread.CheckTerminated then Exit;
          end;
      finally
        //sprites.Free;
        //units.Free;
        //resTexts.Free;
      end;
    end, aOnDone, 'Export units anim');
end;


procedure TKMResExporter.ExportHouseAnimHD(aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
    var
      fullFolderPath, folderPath: string;
      I, STEP, Q: Integer;
      beast, origSpriteID: Integer;
      HT: TKMHouseType;
      ACT: TKMHouseActionType;
      sprites: TKMResSprites;
      spritePack: TKMSpritePack;
      houses: TKMResHouses;
      resTexts: TKMTextLibraryMulti;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      sprites.LoadRXASprites(rxHouses);
      spritePack := sprites[rxHouses];

      folderPath := ExeDir + 'Export' + PathDelim + 'HouseAnimHD' + PathDelim;
      ForceDirectories(folderPath);

      houses := TKMResHouses.Create;

      resTexts := TKMTextLibraryMulti.Create;
      resTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');
      resTexts.ForceDefaultLocale := True;

      PrepareAtlasMap(spritePack);

      try
        for HT := HOUSE_MIN to HOUSE_MAX do
          for ACT := haWork1 to haFlag3 do
          begin
            if houses[HT].Anim[ACT].Count = 0 then Continue;

            fullFolderPath := folderPath + resTexts.DefaultTexts[houses[HT].HouseNameTextID] + PathDelim +
                                HOUSE_ACTION_STR[ACT] + PathDelim;
            ForceDirectories(fullFolderPath);

            for STEP := 0 to houses[HT].Anim[ACT].Count - 1 do
            begin
              origSpriteID := houses[HT].Anim[ACT].Step[STEP+1] + 1;
              if origSpriteID = 0 then Continue;
              ExportFullImageDataFromAtlas(spritePack, origSpriteID, fullFolderPath);
            end;
          end;

        for Q := 1 to 2 do
        begin
          if Q = 1 then
            HT := htSwine
          else
            HT := htStables;

          for beast := 1 to 5 do
            for I := 1 to 3 do
            begin
              if houses.BeastAnim[HT,beast,I].Count = 0 then Continue;

              fullFolderPath := folderPath + resTexts.DefaultTexts[houses[HT].HouseNameTextID] + PathDelim + 'Beast' + PathDelim +
                                int2fix(beast,2) + PathDelim;
              ForceDirectories(fullFolderPath);

              for STEP := 0 to houses.BeastAnim[HT,beast,I].Count - 1 do
              begin
                origSpriteID := houses.BeastAnim[HT,beast,I].Step[STEP+1]+1;
                if origSpriteID = 0 then Continue;
                ExportFullImageDataFromAtlas(spritePack, origSpriteID, fullFolderPath);
              end;
            end;
        end;
      finally
        resTexts.Free;
        houses.Free;
        sprites.Free;
      end;
    end, aOnDone, 'Export HD House animation');
end;


//Export Houses graphics categorized by House and Action
procedure TKMResExporter.ExportHouseAnim(aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
  var
    fullFolderPath, folderPath: string;
    houses: TKMResHouses;
    HT: TKMHouseType;
    ACT: TKMHouseActionType;
    Q, beast, I, K, origSpriteID: Integer;
    spritePack: TKMSpritePack;
    sprites: TKMResSprites;
    resTexts: TKMTextLibraryMulti;
  begin
    sprites := TKMResSprites.Create(nil, nil, True);
    sprites.LoadSprites(rxHouses, true); //BMP can't show alpha shadows anyways
    spritePack := sprites[rxHouses];

    folderPath := ExeDir + 'Export' + PathDelim + 'HouseAnim' + PathDelim;
    ForceDirectories(folderPath);

    houses := TKMResHouses.Create;

    resTexts := TKMTextLibraryMulti.Create;
    resTexts.LoadLocale(ExeDir + 'data' + PathDelim + 'text' + PathDelim + 'text.%s.libx');
    resTexts.ForceDefaultLocale := True;
    try
      for HT := HOUSE_MIN to HOUSE_MAX do
        for ACT := haWork1 to haFlag3 do
        begin
          if houses[HT].Anim[ACT].Count = 0 then Continue;

          fullFolderPath := folderPath + resTexts.DefaultTexts[houses[HT].HouseNameTextID] + PathDelim + HOUSE_ACTION_STR[ACT] + PathDelim;
          ForceDirectories(fullFolderPath);
          for K := 0 to houses[HT].Anim[ACT].Count - 1 do
          begin
            origSpriteID := houses[HT].Anim[ACT].Step[K] + 1;
            if origSpriteID <> 0 then
              spritePack.ExportFullImageData(fullFolderPath, origSpriteID);
            // Stop export if async thread is terminated by application
            if TThread.CheckTerminated then Exit;
          end;
        end;

      for Q := 1 to 2 do
      begin
        if Q = 1 then
          HT := htSwine
        else
          HT := htStables;

        for beast := 1 to 5 do
          for I := 1 to 3 do
          begin
            if houses.BeastAnim[HT,beast,I].Count = 0 then Continue;

            fullFolderPath := folderPath + resTexts.DefaultTexts[houses[HT].HouseNameTextID] + PathDelim + 'Beast' + PathDelim + int2fix(beast,2) + PathDelim;
            ForceDirectories(fullFolderPath);

            for K := 1 to houses.BeastAnim[HT,beast,I].Count do
            begin
              origSpriteID := houses.BeastAnim[HT,beast,I].Step[K]+1;
              if origSpriteID <> 0 then
                spritePack.ExportFullImageData(fullFolderPath, origSpriteID);
              // Stop export if async thread is terminated by application
              if TThread.CheckTerminated then Exit;
            end;
          end;
      end;
    finally
      resTexts.Free;
      houses.Free;
      sprites.Free;
    end;
  end, aOnDone, 'Export house anim');
end;

//Export Houses graphics categorized by House and Action
procedure TKMResExporter.ExportHouseMainPics(aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
  var
    StonePath, WoodPath, SnowPath, folderPath: string;
    houses: TKMResHouses;
    HT: TKMHouseType;
    origSpriteID: Integer;
    spritePack: TKMSpritePack;
    sprites: TKMResSprites;
    tpt : TKMTerrPicType;
    I : Integer;
    pearl : TKMPearlType;
  begin
    sprites := TKMResSprites.Create(nil, nil, True);
    sprites.LoadSprites(rxHouses, true); //BMP can't show alpha shadows anyways
    spritePack := sprites[rxHouses];

    folderPath := ExeDir + 'Export' + PathDelim + 'HousePics' + PathDelim;
    ForceDirectories(folderPath);
    houses := gRes.Houses;
    try
      WoodPath := folderPath + 'WoodPics' + PathDelim;
      ForceDirectories(WoodPath);
      StonePath := folderPath + 'StonePics' + PathDelim;
      ForceDirectories(StonePath);
      SnowPath := folderPath + 'SnowPics' + PathDelim;
      ForceDirectories(SnowPath);
      for HT := HOUSE_MIN to HOUSE_MAX do
      begin

        //woodpic
        origSpriteID := houses[HT].WoodPic + 1;
        spritePack.ExportFullImageData(WoodPath, origSpriteID);
        //stonepic
        origSpriteID := houses[HT].StonePic + 1;
        spritePack.ExportFullImageData(StonePath, origSpriteID);
        //snowpics
        for tpt := tptSnow to High(TKMTerrPicType) do
        begin
          origSpriteID := houses[HT].TerrPic[tpt] + 1;
          if origSpriteID > 1000 then
            spritePack.ExportFullImageData(SnowPath, origSpriteID);
        end;

        for I := 0 to High(houses[HT].Levels) do
        begin
          origSpriteID := houses[HT].Levels[I].StonePic + 1;
          If origSpriteID > 1 then spritePack.ExportFullImageData(StonePath, origSpriteID);

          origSpriteID := houses[HT].Levels[I].SnowPic + 1;
          If origSpriteID > 1 then spritePack.ExportFullImageData(SnowPath, origSpriteID);
        end;

        for I := 0 to High(houses[HT].Styles) do
        begin
          origSpriteID := houses[HT].Styles[I].StonePic + 1;
          If origSpriteID > 1 then spritePack.ExportFullImageData(StonePath, origSpriteID);

          origSpriteID := houses[HT].Styles[I].SnowPic + 1;
          If origSpriteID > 1 then spritePack.ExportFullImageData(SnowPath, origSpriteID);
        end;
        // Stop export if async thread is terminated by application
        if TThread.CheckTerminated then Exit;
      end;

      for pearl := ptValtaria to High(TKMPearlType) do
      begin
        for I := 0 to High(houses.Pearls[pearl].StagePics) do
        begin
          origSpriteID := houses.Pearls[pearl].StagePics[I] + 1;
          If origSpriteID > 1 then spritePack.ExportFullImageData(WoodPath, origSpriteID);
        end;

        origSpriteID := houses.Pearls[pearl].SnowPic;
        If origSpriteID > 1 then spritePack.ExportFullImageData(SnowPath, origSpriteID);

      end;


    finally
      //houses.Free;
      sprites.Free;
    end;
  end, aOnDone, 'Export houses main pics');
end;



procedure TKMResExporter.ExportImageFromAtlas(aSpritePack: TKMSpritePack; aSpriteID: Integer; const aFilePath: string; const aFileMaskPath: string = '');
var
  I, K: Integer;
  px, py: Integer;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
  atlasAddress: TKMAtlasAddress;
begin
  pngWidth := aSpritePack.RXData.Size[aSpriteID].X;//fGFXData[aSpritePack.RT, aSpriteID].PxWidth;
  pngHeight := aSpritePack.RXData.Size[aSpriteID].Y;//fGFXData[aSpritePack.RT, aSpriteID].PxHeight;

  SetLength(pngData, pngWidth * pngHeight);

  // Export RGB values
  if fAtlasMap[saBase].TryGetValue(aSpriteID, atlasAddress) then
    with aSpritePack.Atlases[saBase, atlasAddress.AtlasID] do
    begin
      // Copy rect
      for I := 0 to pngHeight - 1 do
        for K := 0 to pngWidth - 1 do
        begin
          px := Container.Sprites[atlasAddress.SpriteNum].OriginX + K;
          py := Container.Sprites[atlasAddress.SpriteNum].OriginY + I;

          pngData[I * pngWidth + K] := Data[py * Container.Width + px];
        end;

      SaveToPng(pngWidth, pngHeight, pngData, aFilePath);
    end;

  // Masks
  if (aFileMaskPath <> '') and fAtlasMap[saMask].TryGetValue(aSpriteID, atlasAddress) then
    with aSpritePack.Atlases[saMask, atlasAddress.AtlasID] do
    begin
      // Copy rect
      for I := 0 to pngHeight - 1 do
        for K := 0 to pngWidth - 1 do
        begin
          px := Container.Sprites[atlasAddress.SpriteNum].OriginX + K;
          py := Container.Sprites[atlasAddress.SpriteNum].OriginY + I;

          pngData[I * pngWidth + K] := Data[py * Container.Width + px];
        end;

      SaveToPng(pngWidth, pngHeight, pngData, aFileMaskPath);
    end;
end;


procedure TKMResExporter.ExportFullImageDataFromAtlas(aSpritePack: TKMSpritePack; aSpriteID: Integer; const aFolder: string);
var
  s: string;
begin
  if aSpritePack.RXData.Flag[aSpriteID] <> 1 then Exit;

  ExportImageFromAtlas(aSpritePack, aSpriteID, aFolder + Format('%d_%.4d.png', [Byte(aSpritePack.RT)+1, aSpriteID]),
                                               aFolder + Format('%d_%.4da.png', [Byte(aSpritePack.RT)+1, aSpriteID]));

//    if aSpritePack.RXData.HasMask[aIndex] then
//      ExportMask(aFolder + Format('%d_%.4da.png', [Byte(fRT)+1, aIndex]), aIndex);

  // Pivot
  s := IntToStr(aSpritePack.RXData.Pivot[aSpriteID].x) + sLineBreak + IntToStr(aSpritePack.RXData.Pivot[aSpriteID].y) + sLineBreak;

  //SizeNoShadow is used only for Units
  if aSpritePack.RT = rxUnits then
    s := s + IntToStr(aSpritePack.RXData.SizeNoShadow[aSpriteID].Left) + sLineBreak +
      IntToStr(aSpritePack.RXData.SizeNoShadow[aSpriteID].Top) + sLineBreak +
      IntToStr(aSpritePack.RXData.SizeNoShadow[aSpriteID].Right) + sLineBreak +
      IntToStr(aSpritePack.RXData.SizeNoShadow[aSpriteID].Bottom) + sLineBreak;

  WriteTextUtf8(s, aFolder + Format('%d_%.4d.txt', [Ord(aSpritePack.RT)+1, aSpriteID]));
end;


procedure TKMResExporter.ExportTreeAnimHD(aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
    var
      fullFolderPath, folderPath: string;
      I, J, spriteID: Integer;
      sprites: TKMResSprites;
      spritePack: TKMSpritePack;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      try
        sprites.LoadRXASprites(rxTrees);
        spritePack := sprites[rxTrees];

        folderPath := ExeDir + 'Export' + PathDelim + 'TreeAnimHD' + PathDelim;
        ForceDirectories(folderPath);

        PrepareAtlasMap(spritePack);

        for I := 0 to gRes.MapElements.Count - 1 do
          if (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0) then
            for J := 0 to gMapElements[I].Anim.Count - 1 do
            begin
              spriteID := gMapElements[I].Anim.Step[J + 1] + 1;
              if spriteID <> 0 then
              begin
                if gMapElements[I].Anim.Count > 1 then
                begin
                  fullFolderPath := folderPath + IntToStr(I) + PathDelim + IntToStr(J) + PathDelim;
                  ForceDirectories(fullFolderPath);
                end else
                  fullFolderPath := folderPath;

                ExportFullImageDataFromAtlas(spritePack, spriteID, fullFolderPath);
              end;
            end;
      finally
        sprites.Free;
      end;
    end, aOnDone, 'Export HD Tree animation');
end;


//Export Trees graphics categorized by ID
procedure TKMResExporter.ExportTreeAnim(aOnDone: TProc<String>);
begin
  // Make sure we loaded all of the resources (to avoid collisions with async res loader
  gRes.LoadGameResources(True);

  // Asynchroniously export data
  GetOrCreateExportWorker.QueueWork(procedure
    var
      fullFolderPath, folderPath: string;
      I, K, spriteID: Integer;
      sprites: TKMResSprites;
      spritePack: TKMSpritePack;
    begin
      sprites := TKMResSprites.Create(nil, nil, True);
      sprites.LoadSprites(rxTrees, true);
      spritePack := sprites[rxTrees];

      folderPath := ExeDir + 'Export' + PathDelim + 'TreeAnim' + PathDelim;
      ForceDirectories(folderPath);

      try
        for I := 0 to gRes.MapElements.Count - 1 do
        if (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0) then
        begin
          for K := 1 to gMapElements[I].Anim.Count do
          begin
            spriteID := gMapElements[I].Anim.Step[K] + 1;
            if spriteID <> 0 then
            begin
              if gMapElements[I].Anim.Count > 1 then
              begin
                fullFolderPath := folderPath + IntToStr(I) + PathDelim;
                ForceDirectories(fullFolderPath);
              end else
                fullFolderPath := folderPath;

              spritePack.ExportFullImageData(fullFolderPath, spriteID);
              // Stop export if async thread is terminated by application
              if TThread.CheckTerminated then Exit;
            end;
          end;
        end;
      finally
        sprites.Free;
      end;
    end, aOnDone, 'Export tree anim');
end;


end.
