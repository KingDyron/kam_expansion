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
    HasWaited : Boolean;
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
      fCommandsStored : TKMAICommandArray; //command loaded from file
      fRecording : Boolean;
      procedure StartRecording;
      procedure StopRecording;
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

      procedure Load(LoadStream : TKMemoryStream);
      procedure Save(SaveStream : TKMemoryStream);

      procedure LoadFromFile(LoadStream: TKMemoryStream);
      procedure SaveToFile(SaveStream: TKMemoryStream);

  end;

implementation

uses Math, SysUtils,
  KM_GameParams, KM_GameInputProcess,
  KM_HandsCollection,
  KM_Houses, KM_HouseMarket, KM_HouseWoodcutters,
  KM_Terrain,
  KM_Resource, KM_ResHouses
  ;

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
      gicPlaceBridgePlan:         SetParams(SmallIntParams[0], SmallIntParams[2], [IntParams[0], SmallIntParams[2]]);//bridge Index, Rotation
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
      else
        Exit;//don't add command if not used later
    end;
  if tmp.IsValid then
    fCommands.Add(tmp);
end;

procedure TKMAIRecorder.RemoveObj(aLoc : TKMPoint);
begin
  gTerrain.SetObject(aLoc, 255);
end;

procedure TKMAIRecorder.RemoveObjForHouse(aLoc : TKMPoint; aHouse : TKMHouseType);
var I, K : Integer;
  HA : TKMHouseArea;
  P : TKMPoint;
begin
  HA := gRes.Houses[aHouse].BuildArea;

  for I := 1 to 4 do
    for K := 1 to 4 do
    begin
      P.X := aLoc.X + I - 3;
      P.Y := aLoc.X + I - 4;
      RemoveObj(P);
    end;

end;

function TKMAIRecorder.MakeCommand(aIndex: Integer): Boolean;
var aCommand : TKMAICommand;
  procedure MakeCommandWait;
  begin
    Result := fCommandsStored[aIndex].HasWaited; //don't try to make command again
    aCommand.HasWaited := true;
    aCommand.Time := aCommand.Time + 1200;//Try to place after 2 minutes
    fCommandsStored[aIndex] := aCommand;
  end;

var srcHouse : TKMHouse;
  function LocToHouse(aLoc : TKMPoint) : Boolean;
  begin
    srcHouse := gHands[fOwner].HousesHitTest(aLoc.X, aLoc.Y);
    Result := srcHouse <> nil;
  end;

begin
  srcHouse := nil;
  aCommand := fCommandsStored[aIndex];
  Result := true;

  if not aCommand.IsValid then //do not make commad if its invalid. Delete command
    Exit;

  if TKMGameInputCommandType(aCommand.CommandType) in
    [gicHouseDeliveryModeNext, gicHouseDeliveryModePrev, gicHouseClosedForWorkerTgl, gicHouseOrderProduct,
     gicHouseMarketFrom, gicHouseMarketTo, gicHouseWoodcutterMode, gicHouseCollectorsRally, gicHouseWoodcuttersCutting,
     gicHouseStoreBell, gicHouseShipDoWork, gicHouseForceWork, gicHouseStallBuyCoin, gicHouseStallBuyItem, gicHouseMerchantSendTo,
     gicHouseFarmToggleGrain, gicHouseFruitTreeToggleType, gicHouseMerchantSetType] then
    if not LocToHouse(aCommand.Loc) then
      Exit//house not found, delete command
    else
    begin
      if not srcHouse.IsValid(htAny, false, true) then
      begin
        MakeCommandWait;
        Exit;
      end;
    end;



  with aCommand do
    case TKMGameInputCommandType(CommandType) of
        gicBuildToggleFieldPlan :   gHands[fOwner].ToggleFieldPlan(Loc, TKMFieldType(Params[0]), true, TKMRoadType(Params[1])  );
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
        gicPlaceBridgePlan:       if gHands[fOwner].CanAddBridgePlan(Loc, Params[0], Params[1] ) then
                                    gHands[fOwner].AddBridgePlan(Loc, Params[0], Params[1])
                                    else
                                      MakeCommandWait;

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
      gicHouseMarketFrom:        TKMHouseMarket(srcHouse).ResFrom := TKMWareType(Params[0]);
      gicHouseMarketTo:          TKMHouseMarket(srcHouse).ResTo := TKMWareType(Params[0]);

      gicHouseWoodcutterMode:        TKMHouseWoodcutters(srcHouse).WoodcutterMode := TKMWoodcutterMode(Params[0]);
      gicHouseCollectorsRally:       TKMHouseCollectors(srcHouse).FlagPoint := KMPoint(Params[0], Params[1]);
      gicHouseWoodcuttersCutting:    TKMHouseWoodcutters(srcHouse).FlagPoint := KMPoint(Params[0], Params[1]);

      gicHouseStoreBell:             if srcHouse.HouseType in [htStore, htTownhall] then
                                        gHands[srcHouse.Owner].ProceedStoreBell(srcHouse.PointBelowEntrance);

      gicHouseShipDoWork:            TKMHouseShipYard(srcHouse).DoWork := not TKMHouseShipYard(srcHouse).DoWork;

      gicHouseForceWork:             srcHouse.ForceWorking := not srcHouse.ForceWorking;
      gicHouseStallBuyCoin:          TKMHouseStall(srcHouse).BuyCoin(Params[0], Params[1]);
      gicHouseStallBuyItem:          TKMHouseStall(srcHouse).BuyItem(Params[0], Params[1]);
      gicHouseMerchantSendTo:        TKMHouseMerchant(srcHouse).ToggleSendToHand(Params[0]);

      gicHouseFarmToggleGrain:       If srcHouse.HouseType = htFarm then
                                       TKMHouseFarm(srcHouse).SetNextGrainType(Params[0], Params[1])
                                     else
                                     If srcHouse.HouseType = htProductionThatch then
                                       TKMHouseProdThatch(srcHouse).SetNextGrainType(Params[0], Params[1]);

      gicHouseFruitTreeToggleType:   TKMHouseAppleTree(srcHouse).SetNextFruitType(Params[0]);
      gicHouseMerchantSetType:       srcHouse.SetWareSlot(Params[0]);
      gicWareDistributionChange:     begin
                                        gHands[fOwner].Stats.WareDistribution[TKMWareType(Params[0]), TKMHouseType(Params[1])] := Params[2];
                                        gHands[fOwner].Houses.UpdateDemands;
                                     end;
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


procedure TKMAIRecorder.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  if fRecording then
    Exit;

  for I := fCommandsStored.Count - 1 downto 0 do
    if aTick >= fCommandsStored[I].Time then
      If MakeCommand(I) then
        fCommandsStored.Remove(I);

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

procedure TKMAIRecorder.LoadFromFile(LoadStream: TKMemoryStream);
begin
  fCommandsStored.LoadFromStream(LoadStream);
end;

procedure TKMAIRecorder.SaveToFile(SaveStream: TKMemoryStream);
begin
  if (fCommands.Count > 0) or IsRecording then
    fCommands.SaveToStream(SaveStream)//save new recorded commands
  else
    fCommandsStored.SaveToStream(SaveStream);
end;

end.
