unit KM_AIRecordBuilding;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_ResTypes,
  KM_AITypes, KM_HandTypes;

type

  TKMAICommand = record
    CommandType : Word;// ordinal value of TKMGameInputCommandType
    Loc : TKMPoint;
    HasWaited, Deleted : Boolean;
    Time : Cardinal;
    Params : array[0..3] of Integer;
    function IsValid : Boolean;
  end;
  PKMAICommand = ^TKMAICommand;
  TKMAICommandArray = TKMArray<TKMAICommand>;


  TKMAIRecorder = class
    private
      fOwner : TKMHandID;
      fCommands,//commands recorded
      fCommandsStored: TKMAICommandArray; //tmp commands loaded from file
      fRecording : Boolean;
      procedure StopRecording;
      procedure SaveRecording;
      procedure RemoveObj(aLoc : TKMPoint);
      procedure RemoveObjForHouse(aLoc : TKMPoint; aHouse : TKMHouseType);
      function MakeCommand(aIndex: Integer): Boolean;
    protected

    public
      constructor Create(aPlayer : TKMHandID);

      procedure ProceedGIPCommand(aCommand: Pointer; aTime: Cardinal);
      function MakeConsolCommands(aCommand : TKMConsolCommandType) : Boolean;
      procedure ResetRecordings;

      property IsRecording : Boolean read fRecording;
      function HasAnythingToSave : Boolean;
      function HasRecording : Boolean;
      procedure UpdateState(aTick: Cardinal);
      function DebugStr : String;
      procedure StartRecording;

      procedure AfterMissionStart;

      procedure Load(LoadStream : TKMemoryStream);
      procedure Save(SaveStream : TKMemoryStream);

      procedure LoadFromFile;
      procedure SaveToFile;

  end;

implementation

uses Math, SysUtils,
  KM_GameParams, KM_GameInputProcess,
  KM_Hand, KM_HandsCollection, KM_HandEntity,
  KM_Houses, KM_HouseMarket, KM_HouseWoodcutters, KM_HouseStore, KM_HouseHelpers,
  KM_Structure,
  KM_Terrain,
  KM_Resource, KM_ResHouses;

function TKMAICommand.IsValid: Boolean;
begin
  Result := (Loc <> KMPOINT_ZERO)
            and (Time > 0)
            and (CommandType > 0);
end;

constructor TKMAIRecorder.Create(aPlayer: TKMHandID);
begin
  fOwner := aPlayer;
  fRecording := false;
end;

procedure TKMAIRecorder.ProceedGIPCommand(aCommand: Pointer; aTime: Cardinal);
var tmp : TKMAICommand;
  command : TKMGameInputCommand;
  procedure SetParams(aLocX, aLocY : integer; aParams : array of integer); overload;
  var I : Integer;
  begin
    tmp.Loc := KMPoint(aLocX, aLocY);
    for I := 0 to High(aParams) do
      tmp.Params[I] := aParams[I];

  end;

  procedure SetParams(aLoc: TKMpoint; aParams : array of integer); overload;
  var I : Integer;
  begin
    tmp.Loc := aLoc;
    for I := 0 to High(aParams) do
      tmp.Params[I] := aParams[I];

  end;

  function HouseToLoc(aHouseUID : Integer) : TKMPoint;
  var srcHouse : TKMHouse;
  begin
    srcHouse := gHands[fOwner].Houses.GetHouseByUID(aHouseUID);
    if srcHouse = nil then
      Exit(KMPOINT_ZERO);
    Result := srcHouse.Entrance;
  end;
  function StructToLoc(aStructUID : Integer) : TKMPoint;
  var srcStructure : TKMStructure;
  begin
    srcStructure := gHands[fOwner].Structures.GetStructureByUID(aStructUID);
    if srcStructure = nil then
      Exit(KMPOINT_ZERO);
    Result := srcStructure.Position;
  end;

begin
  if not fRecording then
    Exit;
  command := PKMGameInputCommand(aCommand)^;

  tmp.CommandType := Ord(command.CommandType);
  tmp.Time := aTime;
  With command do
    case CommandType of
      gicBuildToggleFieldPlan :   SetParams(IntParams[0], WordParams[0], [WordParams[1], WordParams[2]]);//fieldType, RoadType
      gicBuildRemoveFieldPlan :   SetParams(IntParams[0], IntParams[1], []);//NoParams
      gicBuildRemoveHouse:        SetParams(IntParams[0], IntParams[1], []);//NoParams
      gicBuildRemoveHousePlan:    SetParams(IntParams[0], IntParams[1], []);//NoParams
      gicBuildHousePlan:          SetParams(IntParams[1], IntParams[2], [IntParams[0]]);//HouseType
      gicPlaceStructurePlan:         SetParams(SmallIntParams[0], SmallIntParams[1], [IntParams[0], SmallIntParams[2]]);//bridge Index, Rotation
      gicStructureRemove:         SetParams(StructToLoc(IntParams[0]), []);
      gicPlaceDecoration:         SetParams(IntParams[0], IntParams[1], [IntParams[2]]);//decoration ID

      gicHouseDeliveryModeNext,
      gicHouseDeliveryModePrev:   SetParams(HouseToLoc(command.IntParams[0]), []);//house ID


      gicHouseClosedForWorkerTgl: SetParams(HouseToLoc(command.IntParams[0]), []);//house ID
      gicHouseOrderProduct:      SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1], IntParams[2]]);//house ID, WareIndex, WareOrderCnt

      gicHouseMarketFrom,
      gicHouseMarketTo:          SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID, WareType

      gicHouseWoodcutterMode:    SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID, WoodcutterMode
      gicHouseCollectorsRally,
      gicHouseWoodcuttersCutting:    SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1], IntParams[2]]);//house ID, WoodcutterX, woodcutter Y
      gicHouseStoreBell:             SetParams(HouseToLoc(command.IntParams[0]), []);//house ID
      gicHouseShipDoWork:            SetParams(HouseToLoc(command.IntParams[0]), []);//house ID
      gicHouseForceWork:             SetParams(HouseToLoc(command.IntParams[0]), []);//house ID
      gicHouseStallBuyCoin,
      gicHouseStallBuyItem:          SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1], IntParams[2]]);//house ID, itemID, count
      gicHouseMerchantSendTo:        SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID , PlayerID

      gicHouseFarmToggleGrain:       SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1], IntParams[2]]);//house ID, aMode, aDirection
      gicHouseFruitTreeToggleType:   SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID, aDirection
      gicHouseMerchantSetType:       SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID, slotID
      gicWareDistributionChange:     SetParams(KMPOINT_INVALID_TILE, [IntParams[0],IntParams[1], IntParams[2]]);//houseType, wareType, aCount
      gicHouseStoreNotAcceptFlag:    SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1]]);//house ID, wareType
      gicHouseDeliveryToggle:         SetParams(HouseToLoc(command.IntParams[0]), [IntParams[1], IntParams[2]]);//house ID, wareType, aCount
      else
        Exit;//don't add command if not used later
    end;
  if tmp.IsValid then
    fCommands.Add(tmp);
end;

procedure TKMAIRecorder.RemoveObj(aLoc : TKMPoint);
begin
  gTerrain.SetObject(aLoc, 255);
  gTerrain.SetWareOnGround(aLoc, wtNone, 0);
end;

procedure TKMAIRecorder.AfterMissionStart;
  procedure SelectAround(aLoc : TkMPoint);
  var I, K : Integer;
  begin
    for I := -1 to 1 do
    for K := -1 to 1 do
      gTerrain.ReserveForAI(KMPoint(aLoc.X + I,  aLoc.Y + K));
      //gTerrain.SelectTile(aLoc.X + I, aLoc.Y + K, true);

  end;

var I, J, K, L, M : Integer;
  HA : TKMHouseAreaNew;
  P1, P2 : TKMPoint;
  HT : TKMHouseType;
begin
  for I := 0 to fCommandsStored.Count-1 do
  begin
    P1 := fCommandsStored[I].Loc;
    case TKMGameInputCommandType(fCommandsStored[I].CommandType) of
      gicBuildHousePlan:  begin
                            HT := TKMHouseType(fCommandsStored[I].Params[0]);
                            HA := gRes.Houses[HT].BuildArea;
                            for J := 1 to MAX_HOUSE_SIZE do
                              for K := 1 to MAX_HOUSE_SIZE do
                              if HA[J, K] > 0 then
                              begin
                                P2.X := P1.X + K - 3 - gRes.Houses[HT].EntranceOffsetX;
                                P2.Y := P1.Y + J - 4 - gRes.Houses[HT].EntranceOffsetY;
                                for L := -1 to 1 do
                                for M := -1 to 1 do
                                  gTerrain.ReserveForAI(P2 + KMPoint(L, M))
                              end;

                          end;
      gicBuildToggleFieldPlan: gTerrain.ReserveForAI(P1);//gTerrain.SelectTile(P1.X, P1.Y, true);
    end;
  end;
end;

procedure TKMAIRecorder.RemoveObjForHouse(aLoc : TKMPoint; aHouse : TKMHouseType);
var I, K : Integer;
  HA : TKMHouseAreaNew;
  P : TKMPoint;
begin
  HA := gRes.Houses[aHouse].BuildArea;

  for I := 1 to MAX_HOUSE_SIZE do
    for K := 1 to MAX_HOUSE_SIZE do
    if HA[I, K] > 0 then
    begin
      P.X := aLoc.X + K - 3 - gRes.Houses[aHouse].EntranceOffsetX;
      P.Y := aLoc.Y + I - 4 - gRes.Houses[aHouse].EntranceOffsetY;

      if gTerrain.TileInMapCoords(P.X, P.Y) then
      begin
        RemoveObj(P);
        //if gTerrain.GetFieldType(P) <> ftRoad then
        gTerrain.RemField(P);

        //gTerrain.SelectTile(P.X, P.Y, true);
        gTerrain.FlattenTerrain(P);
      end;
    end;

end;

function TKMAIRecorder.MakeCommand(aIndex: Integer): Boolean;
var aCommand : TKMAICommand;
  procedure MakeCommandWait;
  begin
    Result := fCommandsStored[aIndex].HasWaited; //don't try to make command again
    //if Result then
    //  aCommand.Deleted := true;

    aCommand.HasWaited := true;
    aCommand.Time := gGameParams.Tick + 1200;//Try to place after 2 minutes
    fCommandsStored[aIndex] := aCommand;
    Result := false;
  end;

var srcHouse : TKMHouse;
    srcStruct : TKMStructure;
  function LocToHouse(aLoc : TKMPoint) : Boolean;
  begin
    srcHouse := gHands[fOwner].HousesHitTest(aLoc.X, aLoc.Y);
    Result := srcHouse <> nil;
  end;
  function LocToStruct(aLoc : TKMPoint) : Boolean;
  begin
    srcStruct := gHands[fOwner].StructuresHitTest(aLoc.X, aLoc.Y);
    Result := srcStruct <> nil;
  end;

begin
  srcHouse := nil;
  aCommand := fCommandsStored[aIndex];
  Result := true;

  if not aCommand.IsValid then //do not make commad if its invalid. Delete command
    Exit(False);

  if TKMGameInputCommandType(aCommand.CommandType) in
    [gicHouseDeliveryModeNext, gicHouseDeliveryModePrev, gicHouseClosedForWorkerTgl, gicHouseOrderProduct,
     gicHouseMarketFrom, gicHouseMarketTo, gicHouseWoodcutterMode, gicHouseCollectorsRally, gicHouseWoodcuttersCutting,
     gicHouseStoreBell, gicHouseShipDoWork, gicHouseForceWork, gicHouseStallBuyCoin, gicHouseStallBuyItem, gicHouseMerchantSendTo,
     gicHouseFarmToggleGrain, gicHouseFruitTreeToggleType, gicHouseMerchantSetType, gicHouseStoreNotAcceptFlag, gicHouseDeliveryToggle] then
    if not LocToHouse(aCommand.Loc) then
      Exit//house not found, delete command
    else
    begin
      if not srcHouse.IsValid(htAny, false, true) then
      begin
        MakeCommandWait;
        Exit(False);
      end;
    end;

  if TKMGameInputCommandType(aCommand.CommandType) in [gicStructureRemove] then
    if not LocToStruct(aCommand.Loc) then
      Exit
    else
    begin
      IF srcStruct.IsDestroyed or srcStruct.IsComplete then
        Exit;
    end;


  with aCommand do
    case TKMGameInputCommandType(CommandType) of
      gicBuildToggleFieldPlan :   begin
                                    RemoveObj(Loc);
                                    if gHands[fOwner].CanAddFieldPlan(Loc, TKMFieldType(Params[0])) then
                                      gHands[fOwner].ToggleFieldPlan(Loc, TKMFieldType(Params[0]), true, TKMRoadType(Params[1]))
                                    else
                                      MakeCommandWait;
                                  end;
      gicBuildRemoveFieldPlan :   gHands[fOwner].RemFieldPlan(Loc, true);

      gicBuildRemoveHouse:        gHands[fOwner].RemHouse(Loc, false);
      gicBuildRemoveHousePlan:   gHands[fOwner].RemHousePlan(Loc);
      gicBuildHousePlan:          begin
                                    RemoveObjForHouse(Loc, TKMHouseType(Params[0]) );
                                    If gHands[fOwner].CanAddHousePlan(Loc, TKMHouseType(Params[0])) then
                                      gHands[fOwner].AddHousePlan(TKMHouseType(Params[0]), Loc)
                                    else
                                      MakeCommandWait;
                                  end;
      gicPlaceStructurePlan:      if gHands[fOwner].CanAddStructurePlan(Loc, Params[0], Params[1] ) then
                                    gHands[fOwner].AddStructurePlan(Loc, Params[0], Params[1])
                                  else
                                    MakeCommandWait;
      gicStructureRemove:         srcStruct.DestroyPlan;

      gicPlaceDecoration:         if gHands[fOwner].CanPlaceDecoration(Loc, Params[0]) then
                                    gHands[fOwner].CanPlaceDecoration(Loc, Params[0])
                                  else
                                    MakeCommandWait;

      gicHouseDeliveryModeNext:   //Delivery mode has to be delayed, to avoid occasional delivery mode button clicks
                                  srcHouse.SetNextDeliveryMode;
      gicHouseDeliveryModePrev:   //Delivery mode has to be delayed, to avoid occasional delivery mode button clicks
                                  srcHouse.SetPrevDeliveryMode;
      gicHouseClosedForWorkerTgl: srcHouse.IsClosedForWorker := not srcHouse.IsClosedForWorker;
      gicHouseOrderProduct:      srcHouse.WareOrder[Params[0]] := srcHouse.WareOrder[Params[0]] + Params[1];
      gicHouseMarketFrom:        If srcHouse.Market.NotNil then srcHouse.Market.ResFrom := TKMWareType(Params[0]);
      gicHouseMarketTo:          If srcHouse.Market.NotNil then srcHouse.Market.ResTo := TKMWareType(Params[0]);

      gicHouseWoodcutterMode:        If srcHouse.Woodcutters.NotNil then srcHouse.Woodcutters.WoodcutterMode := TKMWoodcutterMode(Params[0]);
      gicHouseCollectorsRally:       If srcHouse.Collectors.NotNil then srcHouse.Collectors.FlagPoint := KMPoint(Params[0], Params[1]);
      gicHouseWoodcuttersCutting:    If srcHouse.Woodcutters.NotNil then srcHouse.Woodcutters.FlagPoint := KMPoint(Params[0], Params[1]);

      gicHouseStoreBell:             if srcHouse.HouseType in [htStore, htTownhall] then
                                       srcHouse.Hand.ProceedStoreBell(srcHouse.PointBelowEntrance);

      gicHouseShipDoWork:            If srcHouse.Shipyard.NotNil then srcHouse.Shipyard.DoWork := not srcHouse.Shipyard.DoWork;

      gicHouseForceWork:             srcHouse.ForceWorking := not srcHouse.ForceWorking;
      gicHouseStallBuyCoin:          If srcHouse.Stall.NotNil then srcHouse.Stall.BuyCoin(Params[0], Params[1]);
      gicHouseStallBuyItem:          If srcHouse.Stall.NotNil then srcHouse.Stall.BuyItem(Params[0], Params[1]);
      gicHouseMerchantSendTo:        If srcHouse.Merchant.NotNil then srcHouse.Merchant.ToggleSendToHand(Params[0]);

      gicHouseFarmToggleGrain:       If srcHouse.HouseType = htFarm then
                                       srcHouse.Farm.SetNextGrainType(Params[0], Params[1])
                                     else
                                     If srcHouse.HouseType = htProductionThatch then
                                       srcHouse.ProductionThatch.SetNextGrainType(Params[0], Params[1]);

      gicHouseFruitTreeToggleType:   If srcHouse.AppleTree.NotNil then srcHouse.AppleTree.SetNextFruitType(Params[0]);
      gicHouseMerchantSetType:       srcHouse.SetWareSlot(Params[0], true);
      gicWareDistributionChange:     begin
                                        gHands[fOwner].Stats.WareDistribution[TKMWareType(Params[0]), TKMHouseType(Params[1])] := Params[2];
                                        gHands[fOwner].Houses.UpdateDemands;
                                     end;
      gicHouseStoreNotAcceptFlag:    If srcHouse.Store.NotNil then srcHouse.Store.ToggleNotAcceptFlag(TKMWareType(Params[0]));
      gicHouseDeliveryToggle:        srcHouse.ToggleAcceptWaresIn(TKMWareType(Params[0]), Params[1]);
    end;
end;

function TKMAIRecorder.MakeConsolCommands(aCommand: TKMConsolCommandType): Boolean;
begin
  Result := false;
  if not gHands[fOwner].IsComputer then
    Exit;

  Result := true;
  case aCommand of
      cctNone : ;
      cctStartRecording : StartRecording;
      cctStopRecording : StopRecording;
      cctSaveRecord : SaveRecording;
      else
        Result := false;
  end;
end;

procedure TKMAIRecorder.ResetRecordings;
begin
  fCommands.Clear;
  fCommandsStored.Clear;
  fRecording := false;
end;

procedure TKMAIRecorder.StartRecording;
begin
  fRecording := true;
end;

procedure TKMAIRecorder.StopRecording;
begin
  fRecording := false;
end;

procedure TKMAIRecorder.SaveRecording;
begin
  //fTMPCommands := fCommands;//save commands to tmp so it can be saved into file later
  SaveToFile;
end;


procedure TKMAIRecorder.UpdateState(aTick: Cardinal);
var I : Integer;
  //tmp : TKMAICommand;
begin
  if fRecording then
    Exit;

  for I := fCommandsStored.Count - 1 downto 0 do
    if not fCommandsStored[I].Deleted then
      if aTick >= fCommandsStored[I].Time then
        If MakeCommand(I) then
        begin
          //delete command
          fCommandsStored.Remove(I);
          //tmp := fCommandsStored[I];
          //tmp.Deleted := true;
          //fCommandsStored[I] := tmp;
        end;

end;

function TKMAIRecorder.DebugStr: string;
begin
  if not gHands[fOwner].IsComputer then
    Exit;

  if fRecording then
    Result := Format('Commands recorded: %d|', [fCommands.Count])
  else
    Result := Format('Commands stored: %d|', [fCommandsStored.Count]);
end;

procedure TKMAIRecorder.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('AIRecorder');
  LoadStream.Read(fOwner);
  LoadStream.Read(fRecording);
  fCommands.LoadFromStream(LoadStream);
  fCommandsStored.LoadFromStream(LoadStream);
end;

procedure TKMAIRecorder.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('AIRecorder');
  SaveStream.Write(fOwner);
  SaveStream.Write(fRecording);
  fCommands.SaveToStream(SaveStream);
  fCommandsStored.SaveToStream(SaveStream);
end;


function TKMAIRecorder.HasAnythingToSave: Boolean;
begin
  Result := (fCommands.Count > 0){ or (fCommandsStored.Count > 0)};
end;

function TKMAIRecorder.HasRecording: Boolean;
begin
  Result := fCommandsStored.Count > 0;
end;

procedure TKMAIRecorder.LoadFromFile;
var S : TKMemoryStream;
  path : String;
begin
  path := ChangeFileExt(gGameParams.MissionFullFilePath, '.AISetup' + IntToStr(fOwner));
  S := TKMemoryStreamBinary.Create;
  if not FileExists(path) then
    Exit;

  S.LoadFromFile(path);
  fCommandsStored.LoadFromStream(S);
  S.Free;
end;

procedure TKMAIRecorder.SaveToFile;
var S : TKMemoryStream;
  path : String;
begin
  S := TKMemoryStreamBinary.Create;
  path := ChangeFileExt(gGameParams.MissionFullFilePath, '.AISetup' + IntToStr(fOwner));

  fCommands.SaveToStream(S);

  S.SaveToFile(path);
  S.Free;

end;

end.
