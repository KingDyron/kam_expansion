unit KM_Scripting;
{$I KaM_Remake.inc}
{$WARN IMPLICIT_STRING_CAST OFF}
interface
uses
  Classes, SysUtils,
  uPSCompiler, uPSRuntime, uPSUtils, uPSDisassembly, uPSDebugger,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_FileIO,
  KM_ScriptingActions, KM_ScriptingEvents, KM_ScriptingIdCache, KM_ScriptingStates, KM_ScriptingTypes, KM_ScriptingUtils,
  KM_ScriptFilesCollection, KM_ScriptErrorHandler, KM_ScriptPreProcessor,
  KM_ScriptValidatorResult;

  // Dynamic scripts allow mapmakers to control the mission flow

  // Three classes exposed to scripting States, Actions and Utils

  // All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)

  // How to add new a method exposed to the scripting? Three steps:
  // 1. Add method to published section here below
  // 2. Add method declaration to Compiler (TKMScripting.ScriptOnUses)
  // 3. Add method name to Runtime (TKMScripting.LinkRuntime)

type
  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fCampaignDataTypeScriptCode: AnsiString;
    fByteCode: AnsiString;
    fDebugByteCode: AnsiString;
    fExec: TPSExec;

    fValidationIssues: TKMScriptValidatorResult;
    fErrorHandler: TKMScriptErrorHandler;
    fPreProcessor: TKMScriptPreProcessor;

    fStates: TKMScriptStates;
    fActions: TKMScriptActions;
    fIDCache: TKMScriptingIdCache;
    fUtils: TKMScriptUtils;

    function MakePSTypeHash(aType: TPSTypeRec): Integer;
    function IsScriptCodeNeedToCompile: Boolean;
    procedure AddError(aMsg: TPSPascalCompilerMessage);
    procedure CompileScript;
    procedure LinkRuntime;

    procedure SaveVar(SaveStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
    procedure LoadVar(LoadStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);

    function GetScriptFilesInfo: TKMScriptFilesCollection;
//    function GetCodeLine(aRowNum: Cardinal): AnsiString;
//    function FindCodeLine(aRowNumber: Integer; out aFileNamesArr: TKMStringArray; out aRowsArr: TIntegerArray): Integer;
    procedure RecreateValidationIssues;
    constructor Create(aOnScriptError: TUnicodeStringEvent; aForGame: Boolean); // Scripting has to be created via special TKMScriptingCreator
  public
    destructor Destroy; override;

    property ErrorHandler: TKMScriptErrorHandler read fErrorHandler;

    function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
    procedure ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Longint; ProcNo, Position: Cardinal; const PropData: tbtString);
    function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
    procedure ScriptOnAddType(Sender: TPSPascalCompiler; var aType: TPSType);
    procedure ScriptOnAddGlobalVar(Sender: TPSPascalCompiler; var aGlobalVar: TPSVar);

    //property ScriptCode: AnsiString read fScriptCode;
    property ScriptFilesInfo: TKMScriptFilesCollection read GetScriptFilesInfo;
    property PreProcessor: TKMScriptPreProcessor read fPreProcessor;

    function GetErrorMessage(aErrorMsg: TPSPascalCompilerMessage): TKMScriptErrorMessage; overload;
    function GetErrorMessage(const aErrorType, aShortErrorDescription, aModule: String; aRow, aCol, aPos: Integer): TKMScriptErrorMessage; overload;

    property ValidationIssues: TKMScriptValidatorResult read fValidationIssues;
    procedure LoadFromFile(const aFileName, aCampaignDataFilePath: UnicodeString; aCampaignData: TKMemoryStream);
    procedure ExportDataToText;
    procedure ExportScriptCode;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad;

    procedure SaveCampaignData(SaveStream: TKMemoryStream);
    procedure LoadCampaignData(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


  //Scripting creator that will store local instance reference in gScripting
  TKMScriptingCreator = class
  public
    class function CreateScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
    class function CreateGameScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
    class function IsScriptingCreated: Boolean;
  end;


const
  CAMPAIGN_DATA_TYPE = 'TKMCampaignData'; // Type of the global variable
  CAMPAIGN_DATA_VAR = 'CampaignData'; // Name of the global variable
  VALID_GLOBAL_VAR_TYPES: set of TPSBaseType = [
    btU8,  //Byte, Boolean, Enums
    btS8,  //ShortInt
    btU16, //Word
    btS16, //SmallInt
    btU32, //Cardinal / LongInt
    btS32, //Integer
    btSingle, //Single
    btString, //Means AnsiString in PascalScript.
    btUnicodeString, //string and UnicodeString
    btStaticArray, btArray, //Static and Dynamic Arrays
    btRecord, btSet,
    btProcPtr // type TProc = procedure
  ];

implementation
uses
  TypInfo, Math,
  {$IFDEF FPC} Hash, {$ENDIF}
  {$IFDEF WDC} System.Hash, {$ENDIF}
  KromUtils, KM_GameParams, KM_Resource, KM_ResUnits, KM_Log, KM_CommonUtils,
  KM_ScriptingConsoleCommands, KM_ScriptPreProcessorGame,
  KM_ResTypes, KM_CampaignTypes;

var
  gScripting: TKMScripting;


// PS needs regular procedures and functions as handlers
// Hence we have to wrap TKMScripting methods like so
function ScriptOnUsesFunc(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
begin
  Result := False;
  if gScripting <> nil then
    Result := gScripting.ScriptOnUses(Sender, Name);
end;


procedure ScriptOnUseVariableProc(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Integer; ProcNo, Position: Cardinal; const PropData: tbtString);
begin
  if gScripting <> nil  then
    gScripting.ScriptOnUseVariable(Sender, VarType, VarNo, ProcNo, Position, PropData);
end;


function ScriptOnExportCheckFunc(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
begin
  Result := False;
  if gScripting <> nil then
    Result := gScripting.ScriptOnExportCheck(Sender, Proc, ProcDecl);
end;


procedure ScriptOnAddTypeProc(Sender: TPSPascalCompiler; var aType: TPSType);
begin
  if gScripting <> nil then
    gScripting.ScriptOnAddType(Sender, aType);
end;


procedure ScriptOnAddGlobalVarProc(Sender: TPSPascalCompiler; var aGlobalVar: TPSVar);
begin
  if gScripting <> nil then
    gScripting.ScriptOnAddGlobalVar(Sender, aGlobalVar);
end;


{ TKMScriptingCreator }
// We need to save pointer to scripting object (in gScripting), as it is used by ScriptOnUsesFunc/ScriptOnUseVariableProc/ScriptOnExportCheckFunc
// These functions are regular methods and need TKMScripting object in global scope
class function TKMScriptingCreator.CreateScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
begin
  // Should never happen in 1 application, as only 1 TKMScripting object is needed
  FreeAndNil(gScripting);

  gScripting := TKMScripting.Create(aOnScriptError, False);
  Result := gScripting;
end;


// We need to save pointer to scripting object (in gScripting), as it is used by ScriptOnUsesFunc/ScriptOnUseVariableProc/ScriptOnExportCheckFunc
// These functions are regular methods and need TKMScripting object in global scope
class function TKMScriptingCreator.CreateGameScripting(aOnScriptError: TUnicodeStringEvent): TKMScripting;
begin
  // Should never happen in 1 application, as only 1 TKMScripting object is needed
  FreeAndNil(gScripting);

  gScripting := TKMScripting.Create(aOnScriptError, True);
  Result := gScripting;
end;


class function TKMScriptingCreator.IsScriptingCreated: Boolean;
begin
  Result := gScripting <> nil;
end;


{ TKMScripting }
constructor TKMScripting.Create(aOnScriptError: TUnicodeStringEvent; aForGame: Boolean);
begin
  inherited Create;

  // Create an instance of the script executer
  if DEBUG_SCRIPTING_EXEC then
    fExec := TPSDebugExec.Create //Use slow debug executor (about 3 times slower! never use on release version)
  else
    fExec := TPSExec.Create;

  fIDCache := TKMScriptingIdCache.Create;

  // Global object to get events
  fErrorHandler := TKMScriptErrorHandler.Create(aOnScriptError);
  //Use same error handler for PreProcessor and Scripting
  if aForGame then
    fPreProcessor := TKMScriptPreProcessorGame.Create(aOnScriptError, fErrorHandler, False) // Game scripting
  else
    fPreProcessor := TKMScriptPreProcessor.Create(aOnScriptError, fErrorHandler, False); // validator scripting

  gScriptEvents := TKMScriptEvents.Create(fExec, fPreProcessor.PSPreProcessor, fIDCache);
  fStates := TKMScriptStates.Create(fIDCache);
  fActions := TKMScriptActions.Create(fIDCache);
  fUtils := TKMScriptUtils.Create(fIDCache);

  fActions.OnSetLogLinesMaxCnt := fErrorHandler.SetLogLinesCntMax;

  gScriptEvents.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fStates.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fActions.OnScriptError := fErrorHandler.HandleScriptErrorString;
  fUtils.OnScriptError := fErrorHandler.HandleScriptErrorString;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(gScriptEvents);
  FreeAndNil(fStates);
  FreeAndNil(fActions);
  FreeAndNil(fIDCache);
  FreeAndNil(fExec);
  FreeAndNil(fUtils);
  FreeAndNil(fErrorHandler);
  FreeAndNil(fValidationIssues);
  FreeAndNil(fPreProcessor);
  gScripting := nil;

  inherited;
end;


procedure TKMScripting.RecreateValidationIssues;
begin
  FreeAndNil(fValidationIssues);

  fValidationIssues := TKMScriptValidatorResult.Create;
  fPreProcessor.ValidationIssues := fValidationIssues;
end;


// Use separate function to check if script is worth to compile
// Compilation will add 6 global vars for States/Actions/Utils/S/A/U
// So we have to compile or not compile script in both LoadFromFile and Load procedures
// to have no problems with number of global variables declared
function TKMScripting.IsScriptCodeNeedToCompile: Boolean;
begin
  // No need to compile script if its empty
  Result := Trim(fScriptCode) <> '';
end;


procedure TKMScripting.LoadFromFile(const aFileName, aCampaignDataFilePath: UnicodeString; aCampaignData: TKMemoryStream);
begin
  RecreateValidationIssues;

  if not fPreProcessor.PreProcessFile(aFileName, fScriptCode) then
    Exit; // Continue only if PreProcess was successful;

  // Do not continue compilation, if not needed, same as we do in Load procedure
  if not IsScriptCodeNeedToCompile then
    Exit;

  //Parse console commands procedures
  if gScriptEvents.HasConsoleCommands then
    try
      gScriptEvents.ParseConsoleCommandsProcedures(fScriptCode);
    except
      on E: EConsoleCommandParseError do
      begin
        fErrorHandler.AppendErrorStr(E.Message);
        fValidationIssues.AddError(E.Row, E.Col, E.Token, E.Message);
      end;
    end;

  if (aCampaignDataFilePath <> '') and FileExists(aCampaignDataFilePath) then
    fCampaignDataTypeScriptCode := ReadTextA(aCampaignDataFilePath)
  else
    fCampaignDataTypeScriptCode := '';

  CompileScript;

  LoadCampaignData(aCampaignData);

  fErrorHandler.HandleErrors;
end;


//The OnUses callback function is called for each "uses" in the script.
//It's always called with the parameter 'SYSTEM' at the top of the script.
//For example: uses ii1, ii2;
//This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'
function TKMScripting.ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
  //Register classes and methods to the script engine.
  //After that they can be used from within the script.
  procedure RegisterMethodCheck(aClass: TPSCompileTimeClass; const aDecl: String);
  begin
    // We are fine with Assert, cos it will trigger for devs during development
    if not aClass.RegisterMethod(AnsiString(aDecl)) then
      Assert(False, Format('Error registering "%s"', [aDecl]));
  end;

  procedure AddCampaignData;
  var
    campaignDataType: TPSType;
  begin
    // Add TKMCampaignData type declaration from campaigndata.script and variable CampaignData: TKMCampaignData
    if fCampaignDataTypeScriptCode <> '' then
      try
        campaignDataType := Sender.AddTypeS(CAMPAIGN_DATA_TYPE, fCampaignDataTypeScriptCode);
        Sender.AddUsedVariable(CAMPAIGN_DATA_VAR, campaignDataType);
      except
        on E: Exception do
        begin
          fErrorHandler.AppendErrorStr('Error in declaration of global campaign data type|');
          fValidationIssues.AddError(0, 0, '', 'Error in declaration of global campaign data type');
        end;
      end;
  end;

var
  c: TPSCompileTimeClass;
begin
  if Name = 'SYSTEM' then
  begin
    //*Types-Reg*//
    Sender.AddTypeS('TAnsiStringArray', 'array of AnsiString');
    Sender.AddTypeS('TByteSet', 'set of Byte');
    Sender.AddTypeS('TIntegerArray', 'array of Integer');
    Sender.AddTypeS('TKMAIAttackTarget', '(attClosestUnit, attClosestBuildingFromArmy, attClosestBuildingFromStartPos, attCustomPosition)');
    Sender.AddTypeS('TKMAIAttackType', '(aatOnce, aatRepeating)');
    Sender.AddTypeS('TKMAIDefencePosType', '(dtFrontLine, dtBackLine)');
    Sender.AddTypeS('TKMAIRepairMode', '(rmNone, rmRepairNever, rmRepairAlways, rmRepairManual)');
    Sender.AddTypeS('TKMArmyType', '(atIronThenLeather, atLeather, atIron, atIronAndLeather)');
    Sender.AddTypeS('TKMAudioFormat', '(afWav, afOgg)');
    Sender.AddTypeS('TKMDeliveryMode', '(dmClosed, dmDelivery, dmTakeOut)');
    Sender.AddTypeS('TKMDirection', '(dirNA, dirN, dirNE, dirE, dirSE, ' +
      'dirS, dirSW, dirW, dirNW)');
    Sender.AddTypeS('TKMFieldType', '(ftNone, ftRoad, ftCorn, ftWine, ftInitWine)');
    Sender.AddTypeS('TKMFont', '(fntAntiqua, fntGame, fntGrey, fntMetal, fntMini, ' +
      'fntOutline, fntArial, fntMonospaced)');
    Sender.AddTypeS('TKMGroupOrder', '(goNone, goWalkTo, goAttackHouse, goAttackUnit, goStorm)');
    Sender.AddTypeS('TKMGroupType', '(gtNone, gtAny, gtMelee, gtAntiHorse, gtRanged, ' +
      'gtMounted, gtMachines, gtMachinesMelee, gtWreckers, gtShips)');
    Sender.AddTypeS('TKMHandHouseLock', '(hlNone, hlDefault, hlBlocked, hlGranted)');
    Sender.AddTypeS('TKMHouseType', '(htNone, htAny, htArmorSmithy, htArmorWorkshop, htBakery, ' +
      'htBarracks, htButchers, htCoalMine, htFarm, htFishermans, ' +
      'htGoldMine, htInn, htIronMine, htIronSmithy, htMarket, ' +
      'htMetallurgists, htMill, htQuarry, htSawmill, htSchool, ' +
      'htSiegeWorkshop, htStables, htStore, htSwine, htTannery, ' +
      'htTownHall, htWatchTower, htWeaponSmithy, htWeaponWorkshop, htVineyard, ' +
      'htWoodcutters, ' +
      'htWall,            htWall2,           htWall3,         htWall4,         htWall5,  ' +
      'htHovel,           htSign,            htBitinMine,     htWallTower,     htWell, ' +
      'htStoneWorkshop,   htIronFoundry,     htMerchant,      htPottery,       htWoodBurner, ' +
      'htAppleTree,       htSmallStore,      htCollectors,    htTailorsShop,   htCottage, ' +
      'htHouse,           htPalace,          htStall,         htProductionThatch, ' +
      'htShipYard,        htCartographers,   htPearl,         htPasture,       htForest,' +
      'htArena)'
      );
    Sender.AddTypeS('TKMMissionDifficulty', '(mdNone, mdEasy3, mdEasy2, mdEasy1, mdNormal, ' +
      'mdHard1, mdHard2, mdHard3)');
    Sender.AddTypeS('TKMPoint', 'record ' +
        'X,Y: Integer; ' +
      'end;');
    Sender.AddTypeS('TKMTerrainKind', '(tkCustom, tkGrass, tkMoss, tkPaleGrass, tkCoastSand, ' +
      'tkGrassSand1, tkGrassSand2, tkGrassSand3, tkSand, tkGrassDirt, ' +
      'tkDirt, tkCobbleStone, tkGrassyWater, tkSwamp, tkIce, ' +
      'tkSnowOnGrass, tkSnowOnDirt, tkSnow, tkDeepSnow, tkStone, ' +
      'tkGoldMount, tkIronMount, tkAbyss, tkGravel, tkCoal, ' +
      'tkGold, tkIron, tkWater, tkFastWater, tkLava)');
    Sender.AddTypeS('TKMTerrainPassability', '(tpNone, tpWalk, tpWalkRoad, tpBuildNoObj, tpBuild, ' +
      'tpMakeRoads, tpCutTree, tpFish, tpCrab, tpWolf, ' +
      'tpElevate, tpWorker, tpOwn, tpFactor)');
    Sender.AddTypeS('TKMTerrainTileBrief', 'record ' +
        'X,Y: Word; ' +
        'Terrain: Word; ' +
        'Rotation: Byte; ' +
        'Height: Byte; ' +
        'Obj: Word; ' +
        'UpdateTerrain, UpdateRotation, UpdateHeight, UpdateObject: Boolean; ' +
      'end;');
    Sender.AddTypeS('TKMTileMaskKind', '(mkNone, mkSoft1, mkSoft2, mkSoft3, mkStraight, ' +
      'mkGradient)');
    Sender.AddTypeS('TKMTileOverlay', '(toNone, toDig1, toDig2, toDig3, toDig4, ' +
      'toRoad)');

    Sender.AddTypeS('TKMUnitType', '(utNone, utAny,' +
      'utSerf,         utWoodcutter,   utMiner,         utAnimalBreeder,' +
      'utFarmer,       utCarpenter,    utBaker,         utButcher,' +
      'utFisher,       utBuilder,      utStonemason,    utSmith,' +
      'utMetallurgist, utRecruit,      utOperator,      utClayPicker,' +
      'utFeeder,       utHouseBuilder, utMountedSerf,' +
      'utMilitia,      utAxeFighter,   utSwordFighter,  utBowman,' +
      'utCrossbowman,  utLanceCarrier, utPikeman,       utScout,' +
      'utKnight,       utBarbarian,' +
      'utRebel,        utRogue,        utWarrior,       utVagabond,' +
      'utCatapult,     utBallista,     utRam,           utGolem, ' +
      'utGiant,        utPaladin,      utArcher,        utSpy, ' +
      'utTrainedWolf,  utAmmoCart,     utPikeMachine,   utShip,' +
      'utClubMan,      utMaceFighter,  utFlailFighter,  utShieldBearer,' +
      'utFighter,      utSpikedTrap,   utWoodenWall,    utTorchMan,' +
      'utMedic,        utBattleShip,   utBoat,          utPyro,' +
      'utLekter,       utMobileTower,' +
      'utWolf,         utFish,         utWatersnake,    utSeastar, ' +
      'utCrab,         utWaterflower,  utWaterleaf,     utDuck,' +
      'utDeerMale,     utDeerFemale,   utFox,           utBoar,' +
      'utBear,         utLandDuck,     utRabbit,        utWhiteBear,' +
      'utSandSnake,    utSpider)');

    Sender.AddTypeS('TKMWareType', '(wtNone, wtTrunk, wtStone, wtTimber, wtIronOre, ' +
      'wtGoldOre, wtCoal, wtIron, wtGold, wtWine, ' +
      'wtCorn, wtBread, wtFlour, wtLeather, wtSausage, ' +
      'wtPig, wtSkin, wtWoodenShield, wtIronShield, wtLeatherArmor, ' +
      'wtIronArmor, wtAxe, wtSword, wtLance, wtPike, ' +
      'wtBow, wtCrossbow, wtHorse, wtFish, ' +
      'wtBitin,        wtVegetables,' +
      'wtBitinOre, wtStoneBolt,     wtLog,        wtSteelE,       wtBitinE,' +
      'wtWheel,    wtBolt,          wtQuiver,     wtWater,        wtTile,' +
      'wtSeed,     wtSawDust,       wtApple,      wtJewerly,      wtBoots,' +
      'wtHay,      wtMace,          wtFlail,      wtFeathers,     wtPlateArmor,' +
      'wtBitinArmor, wtEgg,' +
      // Special ware types
      'wtAll, wtWarfare, wtFood, wtValuable)');

    Sender.AddTypeS('TKMWoodcutterMode', '(wmChopAndPlant, wmChop, wmPlant)');
    // Dependent types of level 1
    Sender.AddTypeS('TKMAIAttackInfo', 'record ' +
        'UID: Integer; ' +
        'AttackType: TKMAIAttackType; ' +
        'HasOccured: Boolean; ' +
        'Delay: Cardinal; ' +
        'TotalMen: Integer; ' +
        'MeleeGroupCount: Integer; ' +
        'AntiHorseGroupCount: Integer; ' +
        'RangedGroupCount: Integer; ' +
        'MountedGroupCount: Integer; ' +
        'RandomGroups: Boolean; ' +
        'Target: TKMAIAttackTarget; ' +
        'CustomPosition: TKMPoint; ' +
      'end;');
    Sender.AddTypeS('TKMDefencePositionInfo', 'record ' +
        'UID: Integer; ' +
        'X, Y: Integer; ' +
        'Radius: Integer; ' +
        'GroupID: Integer; ' +
        'Dir: TKMDirection; ' +
        'GroupType: TKMGroupType; ' +
        'PositionType: TKMAIDefencePosType; ' +
      'end;');
    Sender.AddTypeS('TKMGroupTypeSet', 'set of TKMGroupType');
    Sender.AddTypeS('TKMHouseTypeSet', 'set of TKMHouseType');
    Sender.AddTypeS('TKMMissionDifficultySet', 'set of TKMMissionDifficulty');
    Sender.AddTypeS('TKMUnitTypeSet', 'set of TKMUnitType');
    Sender.AddTypeS('TKMWareTypeSet', 'set of TKMWareType');
    Sender.AddTypeS('TKMControlType', '(ctButton, ctLabel, ctImage, ctBevel, ctButtonFlat)');

    Sender.AddTypeS('TKMControlInfo', 'record ' +
        'ID: Integer; ' +
        'Left: Integer; ' +
        'Top: Integer; ' +
        'Width: Integer; ' +
        'Height: Integer; ' +
        'Caption, Hint : AnsiString; ' +
        'TexID : Word; ' +
        'Tag : Word; ' +
        'Enabled : Boolean; ' +
        'Visible : Boolean; ' +
        'CType : TKMControlType; ' +
        'ImageAlphaStep : Single; ' +
      'end;');
    Sender.AddTypeS('TKMHouseArea', 'array [1..4] of array [1..4] of Byte');

    Sender.AddTypeS('TKMCursorRenderType', '(crtNone, crtWireTile, crtTile, crtObject, crtUnit, crtHouseSite, crtHouse, crtX, crtDelete)');
    Sender.AddTypeS('TKMLockFieldType', '(lftRoadStone, lftRoadWooden, lftRoadClay, lftRoadExclusive, lftPalisade,' +
                      'lftField, lftGrassField, lftVegetablesField, lftWineField, lftRemove)');

    Sender.AddTypeS('TKMUnitStats', 'record ' +
        'ID: Integer; ' +
        'GroupID: Integer; ' +
        'UnitType: TKMUnitType; ' +
        'X: Word; ' +
        'Y: Word; ' +
        'Owner: Word; ' +
        'Attack: Word; ' +
        'Defence: Word; ' +
        'AttackHorse: Word; ' +
        'HP: Word; ' +
        'MaxHP: Word; ' +
        'Speed: Word; ' +
        'Sight: Word; ' +
        'Condition: Word; ' +
        'DamageHouse: Word; ' +
        'DamageUnits: Word; ' +
        'Ammo: Word; ' +
      'end;');

    Sender.AddTypeS('TKMWarePlanSingle', 'record ' +
        'W : TKMWareType; ' +
        'C : Word; ' +
      'end;');
    Sender.AddTypeS('TKMWarePlan', 'array of TKMWarePlanSingle');

    Sender.AddTypeS('TKMHouseStats', 'record ' +
        'ID : Integer; ' +
        'HouseType : TKMHouseType; ' +
        'X: Word; ' +
        'Y: Word; ' +
        'Owner: Word; ' +
        'FlagX: Word; ' +
        'FlagY: Word; ' +
        'IsDestroyed: Boolean; ' +
        'IsComplete: Boolean; ' +
        'RepairOn: Boolean; ' +
        'Damage: Word; ' +
        'MaxHealth : Word; ' +
        'Level : Byte; ' +
        'WareSlot : Byte; ' +
        'Wares : TKMWarePlan; ' +
      'end;');
    Sender.AddTypeS('TKMUnitThought', '(thNone, thEat, thHome, thBuild, thStone, thWood, thDeath, thQuest, thDismiss, thArmor, thSpy, thTile, thExclusive, thImportant, thBucket, thBoots, thDiamond, thBow, thTalk)');

    Sender.AddTypeS('TKMWeatherType', '(wttNone, wtCloudy1, wtCloudy2, wtRain, wtStorm, wtSnow, wtSnowyStorm, wtSandStorm1, wtSandStorm2, wtTornado)');

    //*Types-Reg*//

    // Add CampaignData type and variable only after addition of all other custom types,
    // so those types could be used in the TKMCampaignData declaration
    AddCampaignData;

    // Register classes and methods to the script engine.
    // After that they can be used from within the script.
    c := Sender.AddClassN(nil, AnsiString(fStates.ClassName));
    //*States-Check*//
    RegisterMethodCheck(c, 'function  AAIAttackHouseTypesGet(aHand: Byte): TKMHouseTypeSet');
    RegisterMethodCheck(c, 'function  AIArmyType(aHand: Byte): TKMArmyType');
    RegisterMethodCheck(c, 'function  AIAutoAttack(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  AIAutoAttackRange(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  AIAutoBuild(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  AIAutoDefence(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  AIAutoRepair(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'procedure AIDefencePositionGet(aHand: Byte; aID: Byte; out aX, aY: Integer; out aGroupType: Byte; out aRadius: Integer; ' +
      'out aDefType: Byte)');
    RegisterMethodCheck(c, 'function  AIDefencePositionGetByIndex(aHand: Integer; aIndex: Integer): TKMDefencePositionInfo');
    RegisterMethodCheck(c, 'function  AIDefendAllies(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  AIEquipRate(aHand: Byte; aType: Byte): Integer');
    RegisterMethodCheck(c, 'procedure AIGroupsFormationGet(aHand: Byte; aType: Byte; out aCount: Integer; out aColumns: Integer)');
    RegisterMethodCheck(c, 'procedure AIGroupsFormationGetEx(aHand: Integer; aGroupType: TKMGroupType; out aCount: Integer; out aColumns: Integer)');
    RegisterMethodCheck(c, 'function  AIRecruitDelay(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  AIRecruitLimit(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  AIRepairMode(aHand: Integer): TKMAIRepairMode');
    RegisterMethodCheck(c, 'function  AISerfsPerHouse(aHand: Byte): Single');
    RegisterMethodCheck(c, 'function  AISoldiersLimit(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  AIStartPosition(aHand: Byte): TKMPoint');
    RegisterMethodCheck(c, 'function  AIUnlimitedEquip(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  AIWorkerLimit(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  CampaignMissionID: Integer');
    RegisterMethodCheck(c, 'function  CampaignMissionsCount: Integer');
    RegisterMethodCheck(c, 'function  ClosestGroup(aHand: Integer; X, Y: Integer; aGroupType: Integer): Integer');
    RegisterMethodCheck(c, 'function  ClosestGroupEx(aHand: Integer; X, Y: Integer; aGroupType: TKMGroupType): Integer');
    RegisterMethodCheck(c, 'function  ClosestGroupMultipleTypes(aHand: Integer; X, Y: Integer; aGroupTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  ClosestGroupMultipleTypesEx(aHand: Integer; X, Y: Integer; aGroupTypes: TKMGroupTypeSet): Integer');
    RegisterMethodCheck(c, 'function  ClosestHouse(aHand: Integer; X, Y: Integer; aHouseType: Integer): Integer');
    RegisterMethodCheck(c, 'function  ClosestHouseEx(aHand: Integer; X, Y: Integer; aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  ClosestHouseMultipleTypes(aHand: Integer; X, Y: Integer; aHouseTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  ClosestHouseMultipleTypesEx(aHand: Integer; X, Y: Integer; aHouseTypes: TKMHouseTypeSet): Integer');
    RegisterMethodCheck(c, 'function  ClosestUnit(aHand: Integer; X, Y: Integer; aUnitType: Integer): Integer');
    RegisterMethodCheck(c, 'function  ClosestUnitEx(aHand: Integer; X, Y: Integer; aUnitType: TKMUnitType): Integer');
    RegisterMethodCheck(c, 'function  ClosestUnitMultipleTypes(aHand: Integer; X, Y: Integer; aUnitTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  ClosestUnitMultipleTypesEx(aHand: Integer; X, Y: Integer; aUnitTypes: TKMUnitTypeSet): Integer');
    RegisterMethodCheck(c, 'function  ConnectedByRoad(X1: Integer; Y1: Integer; X2: Integer; Y2: Integer): Boolean');
    RegisterMethodCheck(c, 'function  ConnectedByWalking(X1: Integer; Y1: Integer; X2: Integer; Y2: Integer): Boolean');
    RegisterMethodCheck(c, 'function  CursorPos(aPlayer : Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  FogRevealed(aHand: Byte; aX, aY: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GameSpeed: Single');
    RegisterMethodCheck(c, 'function  GameSpeedChangeAllowed: Boolean');
    RegisterMethodCheck(c, 'function  GameTime: Cardinal');
    RegisterMethodCheck(c, 'function  GroupAllowAllyToSelect(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GroupAssignedToDefencePosition(aGroupID: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GroupAt(aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupColumnCount(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupDead(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GroupIdle(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GroupInFight(aGroupID: Integer; aCountCitizens: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  GroupManualFormation(aGroupID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GroupMember(aGroupID: Integer; aMemberIndex: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupMemberCount(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupOrder(aGroupID: Integer): TKMGroupOrder');
    RegisterMethodCheck(c, 'function  GroupOwner(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupType(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupTypeEx(aGroupID: Integer): TKMGroupType');

    RegisterMethodCheck(c, 'function  HouseAllowAllyToSelect(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseAt(aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseBarracksRallyPointX(aBarracks: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseBarracksRallyPointY(aBarracks: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseBarracksRecruitBlock(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseBarracksRecruitsCount(aBarracks: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseBuildingProgress(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseCanPlace(aHouseType: byte; aX, aY : Integer; aIgnoreObjects : Boolean): Boolean');
    RegisterMethodCheck(c, 'function  HouseCanReachResources(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseDamage(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseDeliveryBlocked(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseDeliveryMode(aHouseID: Integer): TKMDeliveryMode');
    RegisterMethodCheck(c, 'function  HouseDestroyed(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseFlagPoint(aHouseID: Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  HouseGetAllUnitsIn(aHouseID: Integer): TIntegerArray');
    RegisterMethodCheck(c, 'function  HouseHasOccupant(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseHasWorker(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseIsComplete(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseOwner(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HousePosition(aHouseID: Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  HousePositionX(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HousePositionY(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseRepair(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseResourceAmount(aHouseID: Integer; aResource: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseSchoolQueue(aHouseID: Integer; QueueIndex: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseSiteIsDigged(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseTownHallMaxGold(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseType(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseTypeEx(aHouseID: Integer): TKMHouseType');
    RegisterMethodCheck(c, 'function  HouseTypeMaxHealth(aHouseType: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseTypeMaxHealthEx(aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  HouseTypeName(aHouseType: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  HouseTypeNameEx(aHouseType: TKMHouseType): AnsiString');
    RegisterMethodCheck(c, 'function  HouseTypeToOccupantType(aHouseType: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseTypeToWorkerType(aHouseType: TKMHouseType): TKMUnitType');
    RegisterMethodCheck(c, 'function  HouseUnlocked(aHand: Integer; aHouseType: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseWareAmount(aHouseID: Integer; aWare: TKMWareType): Integer');
    RegisterMethodCheck(c, 'function  HouseWareBlocked(aHouseID: Integer; aWareType: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseWareBlockedEx(aHouseID: Integer; aWareType: TKMWareType): Boolean');
    RegisterMethodCheck(c, 'function  HouseWareBlockedTakeOut(aHouseID: Integer; aWareType: TKMWareType): Boolean');
    RegisterMethodCheck(c, 'function  HouseWeaponsOrdered(aHouseID: Integer; aWareType: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseWeaponsOrderedEx(aHouseID: Integer; aWareType: TKMWareType): Integer');
    RegisterMethodCheck(c, 'function  HouseWoodcutterChopOnly(aHouseID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  HouseWoodcutterMode(aHouseID: Integer): TKMWoodcutterMode');
    RegisterMethodCheck(c, 'function  HouseWorker(aHouseID: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseArea(aHouseType: Integer): TKMHouseArea');
    RegisterMethodCheck(c, 'function  HouseEntranceOffset(aHouseType: Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  HouseTypeToID(aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  HouseIDtoType(aHouseType: Integer): TKMHouseType');

    RegisterMethodCheck(c, 'function  IsFieldAt(aHand: ShortInt; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsFieldPlanAt(var aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsHousePlanAt(var aHand: Integer; var aHouseType: TKMHouseType; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsMissionBlockColorSelection: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionBlockFullMapPreview: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionBlockPeacetime: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionBlockTeamSelection: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionBuildType: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionCoopType: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionFightType: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionPlayableAsSP: Boolean');
    RegisterMethodCheck(c, 'function  IsMissionSpecialType: Boolean');
    RegisterMethodCheck(c, 'function  IsPlanAt(var aHand: Integer; var aFieldType: TKMFieldType; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsRoadAt(aHand: ShortInt; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsRoadPlanAt(var aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsWinefieldAt(aHand: ShortInt; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  IsWinefieldPlanAt(var aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  KaMRandom: Single');
    RegisterMethodCheck(c, 'function  KaMRandomI(aMax: Integer): Integer');
    RegisterMethodCheck(c, 'function  LocationCount: Integer');
    RegisterMethodCheck(c, 'function  MapHeight: Integer');
    RegisterMethodCheck(c, 'function  MapFieldType(X: Integer; Y: Integer): TKMLockFieldType');
    RegisterMethodCheck(c, 'function  MapTileHasOnlyTerrainKind(X, Y: Integer; TerKind: TKMTerrainKind): Boolean');
    RegisterMethodCheck(c, 'function  MapTileHasOnlyTerrainKinds(X, Y: Integer; TerKinds: array of TKMTerrainKind): Boolean');
    RegisterMethodCheck(c, 'function  MapTileHasTerrainKind(X, Y: Integer; TerKind: TKMTerrainKind): Boolean');
    RegisterMethodCheck(c, 'function  MapTileHeight(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileIsCoal(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileIsGold(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileIsIce(X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileIsInMapCoords(X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileIsIron(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileIsSand(X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileIsSnow(X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileIsSoil(X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileIsStone(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileIsWater(X, Y: Integer; FullTilesOnly: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  MapTileObject(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileOverlay(X, Y: Integer): TKMTileOverlay');
    RegisterMethodCheck(c, 'function  MapTileOwner(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTilePassability(X, Y: Integer; aPassability: Byte): Boolean');
    RegisterMethodCheck(c, 'function  MapTilePassabilityEx(X, Y: Integer; aPassability: TKMTerrainPassability): Boolean');
    RegisterMethodCheck(c, 'function  MapTileSelected(X: Integer; Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileRotation(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapTileType(X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  MapWidth: Integer');
    RegisterMethodCheck(c, 'function  MarketFromWare(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function  MarketFromWareEx(aMarketID: Integer): TKMWareType');
    RegisterMethodCheck(c, 'function  MarketLossFactor: Single');
    RegisterMethodCheck(c, 'function  MarketOrderAmount(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function  MarketToWare(aMarketID: Integer): Integer');
    RegisterMethodCheck(c, 'function  MarketToWareEx(aMarketID: Integer): TKMWareType');
    RegisterMethodCheck(c, 'function  MarketValue(aRes: Integer): Single');
    RegisterMethodCheck(c, 'function  MarketValueEx(aWareType: TKMWareType): Single');
    RegisterMethodCheck(c, 'function  MissionAuthor: UnicodeString');
    RegisterMethodCheck(c, 'function  MissionDifficulty: TKMMissionDifficulty');
    RegisterMethodCheck(c, 'function  MissionDifficultyLevels: TKMMissionDifficultySet');
    RegisterMethodCheck(c, 'function  MissionVersion: UnicodeString');

    RegisterMethodCheck(c, 'function  PeaceTime: Cardinal');
    RegisterMethodCheck(c, 'function  PlayerAllianceCheck(aHand1: Byte; aHand2: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerColorFlag(aHand: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  PlayerColorText(aHand: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  PlayerDefeated(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerEnabled(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerGetAllGroups(aHand: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function  PlayerGetAllHouses(aHand: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function  PlayerGetAllUnits(aHand: Byte): TIntegerArray');
    RegisterMethodCheck(c, 'function  PlayerHouseTypeCanBuild(aHand: Integer; aHouseType: TKMHouseType): Boolean');
    RegisterMethodCheck(c, 'function  PlayerHouseTypeLock(aHand: Integer; aHouseType: TKMHouseType): TKMHandHouseLock');
    RegisterMethodCheck(c, 'function  PlayerIsAdvancedAI(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerIsAI(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerIsClassicAI(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerName(aHand: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  PlayerUnitTypeCanTrain(aHand: Integer; aUnitType: TKMUnitType): Boolean');
    RegisterMethodCheck(c, 'function  PlayerVictorious(aHand: Byte): Boolean');
    RegisterMethodCheck(c, 'function  PlayerWareDistribution(aHand: Byte; aWareType: Byte; aHouseType: Byte): Byte');
    RegisterMethodCheck(c, 'function  PlayerWareDistributionEx(aHand: Integer; aWareType: TKMWareType; aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  StatAIDefencePositionsCount(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatArmyCount(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatArmyPower(aHand: Byte): Single');
    RegisterMethodCheck(c, 'function  StatCitizenCount(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatHouseCount(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatHouseMultipleTypesCount(aHand: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  StatHouseMultipleTypesCountEx(aHand: Integer; aTypes: TKMHouseTypeSet): Integer');
    RegisterMethodCheck(c, 'function  StatHouseTypeCount(aHand: Byte; aHouseType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatHouseTypeCountEx(aHand: Integer; aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  StatHouseTypePlansCount(aHand: Byte; aHouseType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatHouseTypePlansCountEx(aHand: Integer; aHouseType: TKMHouseType): Integer');
    RegisterMethodCheck(c, 'function  StatPlayerCount: Integer');
    RegisterMethodCheck(c, 'function  StatResourceProducedCount(aHand: Byte; aResType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatResourceProducedCountEx(aHand: Integer; aWareType: TKMWareType): Integer');
    RegisterMethodCheck(c, 'function  StatResourceProducedMultipleTypesCount(aHand: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  StatResourceProducedMultipleTypesCountEx(aHand: Integer; aTypes: TKMWareTypeSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitCount(aHand: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatUnitKilledCount(aHand: Byte; aUnitType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatUnitKilledCountEx(aHand: Integer; aUnitType: TKMUnitType): Integer');
    RegisterMethodCheck(c, 'function  StatUnitKilledMultipleTypesCount(aHand: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitKilledMultipleTypesCountEx(aHand: Integer; aTypes: TKMUnitTypeSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitLostCount(aHand: Byte; aUnitType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatUnitLostCountEx(aHand: Integer; aUnitType: TKMUnitType): Integer');
    RegisterMethodCheck(c, 'function  StatUnitLostMultipleTypesCount(aHand: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitLostMultipleTypesCountEx(aHand: Byte; aTypes: TKMUnitTypeSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitMultipleTypesCount(aHand: Byte; aTypes: TByteSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitMultipleTypesCountEx(aHand: Integer; aTypes: TKMUnitTypeSet): Integer');
    RegisterMethodCheck(c, 'function  StatUnitTypeCount(aHand: Byte; aUnitType: Byte): Integer');
    RegisterMethodCheck(c, 'function  StatUnitTypeCountEx(aHand: Integer; aUnitType: TKMUnitType): Integer');
    RegisterMethodCheck(c, 'function  StatResourceTotalCount(aHand: Integer; aWareType: TKMWareType): Integer');
    //tower defence
    RegisterMethodCheck(c, 'function  TDHouseCanPlace(aHouseType: byte; aX, aY : Integer): Boolean');

    RegisterMethodCheck(c, 'function  UnitAllowAllyToSelect(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitAt(aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitCarrying(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitCarryingEx(aUnitID: Integer): TKMWareType');
    RegisterMethodCheck(c, 'function  UnitDead(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitDirection(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitDirectionEx(aUnitID: Integer): TKMDirection');
    RegisterMethodCheck(c, 'function  UnitDismissable(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitHasBitin(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitHasBitin(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitHome(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitHPCurrent(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitHPInvulnerable(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitHPMax(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitHunger(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitIdle(aUnitID: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitInHouse(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitLowHunger: Integer');
    RegisterMethodCheck(c, 'function  UnitMaxHunger: Integer');
    RegisterMethodCheck(c, 'function  UnitOwner(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitPosition(aUnitID: Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  UnitPositionX(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitPositionY(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitsGroup(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitType(aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'function  UnitTypeEx(aUnitID: Integer): TKMUnitType');
    RegisterMethodCheck(c, 'function  UnitTypeName(aUnitType: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  UnitTypeNameEx(aUnitType: TKMUnitType): AnsiString');
    RegisterMethodCheck(c, 'function  UnitTypeToID(aUnitType: TKMUnitType): Integer');
    RegisterMethodCheck(c, 'function  UnitIDToType(aUnitType: Integer): TKMUnitType');
    RegisterMethodCheck(c, 'function  WareTypeName(aWareType: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  WareTypeNameEx(aWareType: TKMWareType): AnsiString');
    RegisterMethodCheck(c, 'function  WareTypeToID(aWareType: TKMWareType): Integer');
    RegisterMethodCheck(c, 'function  WareIdToType(aWareType: Integer): TKMWareType');
    RegisterMethodCheck(c, 'function  WarriorInFight(aUnitID: Integer; aCountCitizens: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  HouseStats(aHouseID: Integer; aWithWares : Boolean): TKMHouseStats');
    RegisterMethodCheck(c, 'function  UnitStats(aUnitID: Integer): TKMUnitStats');
    //*States-Check*//

    c := Sender.AddClassN(nil, AnsiString(fActions.ClassName));
    //*Actions-Check*//
    RegisterMethodCheck(c, 'procedure AAIAttackHouseTypesSet(aHand: Byte; aHouses: TKMHouseTypeSet)');
    RegisterMethodCheck(c, 'procedure AIArmyType(aHand: Byte; aType: TKMArmyType)');
    RegisterMethodCheck(c, 'function  AIAttackAdd(aHand: Integer; aRepeating: Boolean; aDelay: Cardinal; aTotalMen: Integer; ' +
      'aMeleeGroupCount: Integer; aAntiHorseGroupCount: Integer; aRangedGroupCount: Integer; ' +
      'aMountedGroupCount: Integer; aRandomGroups: Boolean; aTarget: TKMAIAttackTarget; ' +
      'aCustomPosition: TKMPoint): Integer');
    RegisterMethodCheck(c, 'function  AIAttackAddEx(aHand: Integer; var aAttackInfo: TKMAIAttackInfo): Integer');
    RegisterMethodCheck(c, 'function  AIAttackRemove(aHand: Integer; aAIAttackUID: Integer): Boolean');
    RegisterMethodCheck(c, 'procedure AIAttackRemoveAll(aHand: Integer)');
    RegisterMethodCheck(c, 'procedure AIAutoAttack(aHand: Byte; aAutoAttack: Boolean)');
    RegisterMethodCheck(c, 'procedure AIAutoAttackRange(aHand: Byte; aRange: Integer)');
    RegisterMethodCheck(c, 'procedure AIAutoBuild(aHand: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'procedure AIAutoDefence(aHand: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'procedure AIAutoRepair(aHand: Byte; aAuto: Boolean)');
    RegisterMethodCheck(c, 'function  AIDefencePositionAdd(aHand: Byte; X, Y: Integer; aDir: Byte; aGroupType: Byte; aRadius: Integer; ' +
      'aDefType: Byte): Integer');
    RegisterMethodCheck(c, 'function  AIDefencePositionAddEx(aHand: Integer; aOrder: Integer; var aDefencePosition: TKMDefencePositionInfo): Integer');
    RegisterMethodCheck(c, 'procedure AIDefencePositionRemove(aHand: Byte; X, Y: Integer)');
    RegisterMethodCheck(c, 'procedure AIDefencePositionRemoveAll(aHand: Byte)');
    RegisterMethodCheck(c, 'procedure AIDefencePositionRemoveByUID(aHand: Integer; aUID: Integer)');
    RegisterMethodCheck(c, 'procedure AIDefendAllies(aHand: Byte; aDefend: Boolean)');
    RegisterMethodCheck(c, 'procedure AIEquipRate(aHand: Byte; aType: Byte; aRate: Integer)');
    RegisterMethodCheck(c, 'procedure AIGroupsFormationSet(aHand: Byte; aType: Byte; aCount: Integer; aColumns: Integer)');
    RegisterMethodCheck(c, 'procedure AIGroupsFormationSetEx(aHand: Integer; aGroupType: TKMGroupType; aCount: Integer; aColumns: Integer)');
    RegisterMethodCheck(c, 'procedure AIRecruitDelay(aHand: Byte; aDelay: Cardinal)');
    RegisterMethodCheck(c, 'procedure AIRecruitLimit(aHand: Byte; aLimit: Byte)');
    RegisterMethodCheck(c, 'procedure AIRepairMode(aHand: Integer; aRepairMode: TKMAIRepairMode)');
    RegisterMethodCheck(c, 'procedure AISerfsPerHouse(aHand: Byte; aSerfs: Single)');
    RegisterMethodCheck(c, 'procedure AISoldiersLimit(aHand: Byte; aLimit: Integer)');
    RegisterMethodCheck(c, 'procedure AIStartPosition(aHand: Byte; X, Y: Integer)');
    RegisterMethodCheck(c, 'procedure AIUnlimitedEquip(aHand: Byte; aUnlimitedEquip: Boolean)');
    RegisterMethodCheck(c, 'procedure AIWorkerLimit(aHand: Byte; aLimit: Byte)');
    RegisterMethodCheck(c, 'procedure CinematicEnd(aHand: Byte)');
    RegisterMethodCheck(c, 'procedure CinematicPanTo(aHand: Byte; X, Y: Integer; Duration: Integer)');
    RegisterMethodCheck(c, 'procedure CinematicStart(aHand: Byte)');
    RegisterMethodCheck(c, 'procedure FogCoverAll(aHand: Byte)');
    RegisterMethodCheck(c, 'procedure FogCoverCircle(aHand: Integer; X, Y: Integer; aRadius: Integer)');
    RegisterMethodCheck(c, 'procedure FogCoverRect(aHand: Integer; X1: Integer; Y1: Integer; X2: Integer; Y2: Integer)');
    RegisterMethodCheck(c, 'procedure FogRevealAll(aHand: Byte)');
    RegisterMethodCheck(c, 'procedure FogRevealCircle(aHand: Integer; X, Y: Integer; aRadius: Integer)');
    RegisterMethodCheck(c, 'procedure FogRevealRect(aHand: Integer; X1: Integer; Y1: Integer; X2: Integer; Y2: Integer)');
    RegisterMethodCheck(c, 'procedure GameSpeed(aSpeed: Single)');
    RegisterMethodCheck(c, 'procedure GameSpeedChangeAllowed(aAllowed: Boolean)');
    RegisterMethodCheck(c, 'function  GiveAnimal(aType: Integer; X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveAnimalEx(aType: TKMUnitType; X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveField(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GiveFieldAged(aHand: Integer; X, Y: Integer; aStage: Byte; aRandomAge: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  GiveGroup(aHand: Integer; aType: Integer; X, Y: Integer; aDir: Integer; aCount: Integer; ' +
      'aColumns: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveGroupEx(aHand: Integer; aType: TKMUnitType; X, Y: Integer; aDir: TKMDirection; ' +
      'aCount: Integer; aColumns: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveHouse(aHand: Integer; aHouseType: Integer; X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveHouseEx(aHand: Integer; aHouseType: TKMHouseType; X, Y: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveHouseSite(aHand: Integer; aHouseType: Integer; X, Y: Integer; aAddMaterials: Boolean): Integer');
    RegisterMethodCheck(c, 'function  GiveHouseSiteEx(aHand: Integer; aHouseType: TKMHouseType; X, Y: Integer; aWoodAmount: Integer; ' +
      'aStoneAmount, aTileAmount: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveHouseFromStats(aHand: Integer; aStats: TKMHouseStats): Integer');
    RegisterMethodCheck(c, 'function  GiveRoad(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GiveUnit(aHand: Integer; aType: Integer; X, Y: Integer; aDir: Integer): Integer');
    RegisterMethodCheck(c, 'function  GiveUnitEx(aHand: Integer; aType: TKMUnitType; X, Y: Integer; aDir: TKMDirection): Integer');
    RegisterMethodCheck(c, 'procedure GiveWares(aHand: Integer; aType: Integer; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure GiveWaresEx(aHand: Integer; aType: TKMWareType; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure GiveWeapons(aHand: Integer; aType: Integer; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure GiveWeaponsEx(aHand: Integer; aType: TKMWareType; aCount: Integer)');
    RegisterMethodCheck(c, 'function  GiveWineField(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  GiveWineFieldAged(aHand: Integer; X, Y: Integer; aStage: Byte; aRandomAge: Boolean): Boolean');
    RegisterMethodCheck(c, 'procedure GroupAllowAllyToSelect(aGroupID: Integer; aAllow: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupBlockOrders(aGroupID: Integer; aBlock: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupDisableHungryMessage(aGroupID: Integer; aDisable: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupHungerPaceSet(aGroupID: Integer; aPace: Cardinal)');
    RegisterMethodCheck(c, 'procedure GroupHungerSet(aGroupID: Integer; aHungerLevel: Integer)');
    RegisterMethodCheck(c, 'procedure GroupInfiniteAmmoSet(aGroupID: Integer; aInfinity: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupKillAll(aGroupID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupMakeHero(aGroupID: Integer; makeHero: Boolean)');
    RegisterMethodCheck(c, 'procedure GroupOrderAttackHouse(aGroupID: Integer; aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderAttackUnit(aGroupID: Integer; aUnitID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderFood(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderHalt(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderLink(aGroupID: Integer; aDestGroupID: Integer)');
    RegisterMethodCheck(c, 'function  GroupOrderSplit(aGroupID: Integer): Integer');
    RegisterMethodCheck(c, 'function  GroupOrderSplitUnit(aGroupID: Integer; aUnitID: Integer): Integer');
    RegisterMethodCheck(c, 'procedure GroupOrderStorm(aGroupID: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderWalk(aGroupID: Integer; X, Y: Integer; aDirection: Integer)');
    RegisterMethodCheck(c, 'procedure GroupOrderWalkEx(aGroupID: Integer; X, Y: Integer; aDirection: TKMDirection)');
    RegisterMethodCheck(c, 'procedure GroupSetFormation(aGroupID: Integer; aNumColumns: Byte)');
    RegisterMethodCheck(c, 'procedure HouseAddBuildingMaterials(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddBuildingMaterialsEx(aHouseID: Integer; aWoodAmount: Integer; aStoneAmount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddBuildingProgress(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddBuildingProgressEx(aHouseID: Integer; aBuildSteps: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddDamage(aHouseID: Integer; aDamage: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddRepair(aHouseID: Integer; aRepair: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddWaresTo(aHouseID: Integer; aType: Integer; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAddWaresToEx(aHouseID: Integer; aType: TKMWareType; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseAllow(aHand: Integer; aHouseType: Integer; aAllowed: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseAllowAllyToSelect(aHouseID: Integer; aAllow: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseAllowAllyToSelectAll(aHand: ShortInt; aAllow: Boolean)');
    RegisterMethodCheck(c, 'function  HouseBarracksEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseBarracksEquipEx(aHouseID: Integer; aUnitType: TKMUnitType; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'procedure HouseBarracksGiveRecruit(aHouseID: Integer)');
    RegisterMethodCheck(c, 'procedure HouseBarracksGiveRecruits(aHouseID: Integer; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseBarracksRecruitBlock(aHouseID: Integer; aBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseChangeOwner(aHouseID: Integer; aToOwner: Integer)');
    RegisterMethodCheck(c, 'procedure HouseDeliveryBlock(aHouseID: Integer; aDeliveryBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseDeliveryMode(aHouseID: Integer; aDeliveryMode: TKMDeliveryMode)');
    RegisterMethodCheck(c, 'procedure HouseDestroy(aHouseID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseDisableUnoccupiedMessage(aHouseID: Integer; aDisabled: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseRepairEnable(aHouseID: Integer; aRepairEnabled: Boolean)');
    RegisterMethodCheck(c, 'function  HouseSchoolQueueAdd(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseSchoolQueueAddEx(aHouseID: Integer; aUnitType: TKMUnitType; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'procedure HouseSchoolQueueRemove(aHouseID: Integer; QueueIndex: Integer)');
    RegisterMethodCheck(c, 'procedure HouseTakeWaresFrom(aHouseID: Integer; aType: Integer; aCount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseTakeWaresFromEx(aHouseID: Integer; aType: TKMWareType; aCount: Integer)');
    RegisterMethodCheck(c, 'function  HouseTownHallEquip(aHouseID: Integer; aUnitType: Integer; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'function  HouseTownHallEquipEx(aHouseID: Integer; aUnitType: TKMUnitType; aCount: Integer): Integer');
    RegisterMethodCheck(c, 'procedure HouseTownHallMaxGold(aHouseID: Integer; aMaxGold: Integer)');
    RegisterMethodCheck(c, 'procedure HouseUnlock(aHand: Integer; aHouseType: Integer)');
    RegisterMethodCheck(c, 'procedure HouseWareBlock(aHouseID: Integer; aWareType: Integer; aBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWareBlockEx(aHouseID: Integer; aWareType: TKMWareType; aBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWareBlockTakeOut(aHouseID: Integer; aWareType: TKMWareType; aBlocked: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWeaponsOrderSet(aHouseID: Integer; aWareType: Integer; aAmount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseWeaponsOrderSetEx(aHouseID: Integer; aWareType: TKMWareType; aAmount: Integer)');
    RegisterMethodCheck(c, 'procedure HouseWoodcutterChopOnly(aHouseID: Integer; aChopOnly: Boolean)');
    RegisterMethodCheck(c, 'procedure HouseWoodcutterMode(aHouseID: Integer; aWoodcutterMode: TKMWoodcutterMode)');
    RegisterMethodCheck(c, 'procedure Log(aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure LogLinesMaxCnt(aMaxLogLinesCnt: Integer)');
    RegisterMethodCheck(c, 'procedure MapBrush(X: Integer; Y: Integer; aSquare: Boolean; aSize: Integer; aTerKind: TKMTerrainKind; ' +
      'aRandomTiles: Boolean; aOverrideCustomTiles: Boolean)');
    RegisterMethodCheck(c, 'procedure MapBrushElevation(X: Integer; Y: Integer; aSquare: Boolean; aRaise: Boolean; aSize: Integer; ' +
      'aSlope: Integer; aSpeed: Integer)');
    RegisterMethodCheck(c, 'procedure MapBrushEqualize(X: Integer; Y: Integer; aSquare: Boolean; aSize: Integer; aSlope: Integer; ' +
      'aSpeed: Integer)');
    RegisterMethodCheck(c, 'procedure MapBrushFlatten(X: Integer; Y: Integer; aSquare: Boolean; aSize: Integer; aSlope: Integer; ' +
      'aSpeed: Integer)');
    RegisterMethodCheck(c, 'procedure MapBrushMagicWater(X: Integer; Y: Integer)');
    RegisterMethodCheck(c, 'procedure MapBrushWithMask(X: Integer; Y: Integer; aSquare: Boolean; aSize: Integer; aTerKind: TKMTerrainKind; ' +
      'aRandomTiles: Boolean; aOverrideCustomTiles: Boolean; aBrushMask: TKMTileMaskKind; ' +
      'aBlendingLvl: Integer; aUseMagicBrush: Boolean)');
    RegisterMethodCheck(c, 'procedure MapLayerLoad(aLayerName: string)');
    RegisterMethodCheck(c, 'procedure  MapTileFieldSet(X: Integer; Y: Integer; aOwner: Integer; aType: TKMLockFieldType)');
    RegisterMethodCheck(c, 'function  MapTileHeightSet(X, Y: Integer; Height: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileObjectSet(X, Y: Integer; Obj: Integer): Boolean');
    RegisterMethodCheck(c, 'function  MapTileOverlaySet(X, Y: Integer; aOverlay: TKMTileOverlay; aOverwrite: Boolean): Boolean');
    RegisterMethodCheck(c, 'procedure MapSetNightTime(aValue : Single)');
    RegisterMethodCheck(c, 'function  MapTilesArraySet(aTiles: array of TKMTerrainTileBrief; aRevertOnFail: Boolean; aShowDetailedErrors: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  MapTilesArraySetS(aTilesS: TAnsiStringArray; aRevertOnFail: Boolean; aShowDetailedErrors: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  MapTileSet(X, Y: Integer; aType: Integer; aRotation: Integer): Boolean');
    RegisterMethodCheck(c, 'procedure MarketSetTrade(aMarketID: Integer; aFrom: Integer; aTo: Integer; aAmount: Integer)');
    RegisterMethodCheck(c, 'procedure MarketSetTradeEx(aMarketID: Integer; aFrom: TKMWareType; aTo: TKMWareType; aAmount: Integer)');
    RegisterMethodCheck(c, 'procedure OverlayTextAppend(aHand: ShortInt; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure OverlayTextAppendFormatted(aHand: ShortInt; aText: AnsiString; aParams: array of const)');
    RegisterMethodCheck(c, 'procedure OverlayTextSet(aHand: ShortInt; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetFont(aHand: ShortInt; aFont: TKMFont)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetFormatted(aHand: ShortInt; aText: AnsiString; aParams: array of const)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetWordWrap(aHand: ShortInt; aWordWrap: Boolean)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetAlignToCenter(aHand: ShortInt; aSet: Boolean)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetAddBevel(aHand: ShortInt; aSet: Boolean)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetFromBottom(aHand: ShortInt; aSet: Boolean)');
    RegisterMethodCheck(c, 'procedure OverlayTextSetMaxWidth(aHand: ShortInt; aSet: Word)');
    RegisterMethodCheck(c, 'function PanelControlAdd(aPlayer : Shortint; aInfo : TKMControlInfo) : Integer');
    RegisterMethodCheck(c, 'procedure PanelControlChange(aPlayer : Shortint; aButtonID : Integer; aInfo : TKMControlInfo)');
    RegisterMethodCheck(c, 'procedure PanelControlVisible(aPlayer : Shortint; aButtonID : Integer; aVisible : Boolean)');
    RegisterMethodCheck(c, 'procedure PanelControlTexID(aPlayer : Shortint; aButtonID : Integer; aValue : Integer)');
    RegisterMethodCheck(c, 'procedure PanelControlCaption(aPlayer : Shortint; aButtonID : Integer; aValue : String)');
    RegisterMethodCheck(c, 'procedure PanelControlCaptionFormatted(aPlayer : Shortint; aButtonID : Integer; aValue : String; aParams: array of const)');
    RegisterMethodCheck(c, 'procedure PanelControlHint(aPlayer : Shortint; aButtonID : Integer; aValue : String)');
    RegisterMethodCheck(c, 'procedure PanelControlHintFormatted(aPlayer : Shortint; aButtonID : Integer; aValue : String; aParams: array of const)');
    RegisterMethodCheck(c, 'procedure PanelControlRect(aPlayer : Shortint; aButtonID : Integer; X, Y, Widht, Height : Integer)');
    RegisterMethodCheck(c, 'procedure PanelControlEnabled(aPlayer : Shortint; aButtonID : Integer; aValue : Boolean)');
    RegisterMethodCheck(c, 'procedure PanelControlAlphaStep(aPlayer : Shortint; aButtonID : Integer; aValue : Single)');
    RegisterMethodCheck(c, 'procedure PanelResize(aPlayer : Shortint; Left,Top, Width, Height : Integer)');
    RegisterMethodCheck(c, 'procedure PanelExpand(aPlayer: ShortInt; aExpanded: Boolean)');

    RegisterMethodCheck(c, 'procedure CursorCustomSet(aPlayer : Shortint; aMode : TKMCursorRenderType; aTag1, aTag2 : Integer)');
    RegisterMethodCheck(c, 'procedure MapTileSelect(X: Integer; Y: Integer; aSelected: Boolean)');
    RegisterMethodCheck(c, 'procedure WatchTowerRangeSet(aWatchTower : Integer; aRangeMin, aRangeMax : Single)');
    RegisterMethodCheck(c, 'procedure WatchTowerCyclesSet(aWatchTower : Integer; aCycles : Byte)');

    RegisterMethodCheck(c, 'procedure Peacetime(aPeacetime: Cardinal)');
    RegisterMethodCheck(c, 'function  PlanAddField(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function PlanFieldAdd(aHand: Integer; X: Integer; Y: Integer; aFIeldType: TKMLockFieldType): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddHouse(aHand: Integer; aHouseType: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddHouseEx(aHand: Integer; aHouseType: TKMHouseType; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddRoad(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  PlanAddWinefield(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  PlanConnectRoad(aHand: Integer; X1: Integer; Y1: Integer; X2: Integer; Y2: Integer; aCompleted: Boolean): Boolean');
    RegisterMethodCheck(c, 'function  PlanRemove(aHand: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'procedure PlayerAddDefaultGoals(aHand: Byte; aBuildings: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAllowField(const aPlayer: Integer; aFieldType : Byte; aAllow : Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAllowFieldEx(const aPlayer: Integer; aFieldType : TKMLockFieldType; aAllow : Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAllianceChange(aHand1: Byte; aHand2: Byte; aCompliment: Boolean; aAllied: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAllianceNFogChange(aHand1: Byte; aHand2: Byte; aCompliment: Boolean; aAllied: Boolean; aSyncAllyFog: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerDefeat(aHand: Integer)');
    RegisterMethodCheck(c, 'procedure PlayerGoalsRemoveAll(aHand: Integer; aForAllPlayers: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerHouseTypeLock(aHand: Integer; aHouseType: TKMHouseType; aLock: TKMHandHouseLock)');
    RegisterMethodCheck(c, 'procedure PlayerShareBeacons(aHand1: Integer; aHand2: Integer; aBothWays: Boolean; aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerShareFog(aHand1: Integer; aHand2: Integer; aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerShareFogCompliment(aHand1: Integer; aHand2: Integer; aShare: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerTradeAllowed(aHand: Integer; aWareType: TKMWareType; aAllowed: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerUnitTypeCanTrain(aHand: Integer; aUnitType: TKMUnitType; aCanTrain: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerWareDistribution(aHand: Byte; aWareType: Byte; aHouseType: Byte; aAmount: Byte)');
    RegisterMethodCheck(c, 'procedure PlayerWareDistributionEx(aHand: Integer; aWareType: TKMWareType; aHouseType: TKMHouseType; aAmount: Integer)');
    RegisterMethodCheck(c, 'procedure PlayerWin(aVictors: array of Integer; aTeamVictory: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerUpdateEntitiesSet(const aPlayer: Integer; doUpdate: Boolean)');
    RegisterMethodCheck(c, 'procedure PlayerAddWorkers(const aPlayer: Integer; addBoots: Boolean)');

    RegisterMethodCheck(c, 'function  PlayOGG(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function  PlayOGGAtLocation(aHand: ShortInt; aFileName: AnsiString; aVolume: Single; aRadius: Single; ' +
      'aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  PlayOGGAtLocationLooped(aHand: ShortInt; aFileName: AnsiString; aVolume: Single; aRadius: Single; ' +
      'aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  PlayOGGFadeMusic(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function  PlayOGGLooped(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function  PlaySound(aHand: ShortInt; aFileName: AnsiString; aAudioFormat: TKMAudioFormat; ' +
      'aVolume: Single; aFadeMusic: Boolean; aLooped: Boolean): Integer');
    RegisterMethodCheck(c, 'function  PlaySoundAtLocation(aHand: ShortInt; aFileName: AnsiString; aAudioFormat: TKMAudioFormat; ' +
      'aVolume: Single; aFadeMusic: Boolean; aLooped: Boolean; aRadius: Single; ' +
      'aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  PlayWAV(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function  PlayWAVAtLocation(aHand: ShortInt; aFileName: AnsiString; aVolume: Single; aRadius: Single; ' +
      'aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  PlayWAVAtLocationLooped(aHand: ShortInt; aFileName: AnsiString; aVolume: Single; aRadius: Single; ' +
      'aX, aY: Integer): Integer');
    RegisterMethodCheck(c, 'function  PlayWAVFadeMusic(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'function  PlayWAVLooped(aHand: ShortInt; aFileName: AnsiString; aVolume: Single): Integer');
    RegisterMethodCheck(c, 'procedure RemoveRoad(X, Y: Integer)');
    RegisterMethodCheck(c, 'procedure SetTradeAllowed(aHand: Integer; aResType: Integer; aAllowed: Boolean)');
    RegisterMethodCheck(c, 'procedure ShowMsg(aHand: ShortInt; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure ShowMsgFormatted(aHand: ShortInt; aText: AnsiString; Params: array of const)');
    RegisterMethodCheck(c, 'procedure ShowMsgGoto(aHand: ShortInt; aX, aY: Integer; aText: AnsiString)');
    RegisterMethodCheck(c, 'procedure ShowMsgGotoFormatted(aHand: ShortInt; aX, aY: Integer; aText: AnsiString; Params: array of const)');
    RegisterMethodCheck(c, 'procedure StopLoopedOGG(aSoundIndex: Integer)');
    RegisterMethodCheck(c, 'procedure StopLoopedWAV(aSoundIndex: Integer)');
    RegisterMethodCheck(c, 'procedure StopSound(aSoundIndex: Integer)');
    RegisterMethodCheck(c, 'procedure UnitAllowAllyToSelect(aUnitID: Integer; aAllow: Boolean)');
    RegisterMethodCheck(c, 'procedure UnitBlock(aHand: Byte; aType: Integer; aBlock: Boolean)');
    RegisterMethodCheck(c, 'procedure UnitBootsSet(aUnitID: Integer; aBoots: Boolean)');
    RegisterMethodCheck(c, 'function  UnitDirectionSet(aUnitID: Integer; aDirection: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitDirectionSetEx(aUnitID: Integer; aDirection: TKMDirection): Boolean');
    RegisterMethodCheck(c, 'procedure UnitDismiss(aUnitID: Integer)');
    RegisterMethodCheck(c, 'procedure UnitDismissableSet(aUnitID: Integer; aDismissable: Boolean)');
    RegisterMethodCheck(c, 'procedure UnitDismissCancel(aUnitID: Integer)');
    RegisterMethodCheck(c, 'procedure UnitHPChange(aUnitID: Integer; aHP: Integer)');
    RegisterMethodCheck(c, 'procedure UnitChangeSpec(aUnitID, aHPMax, aAttack, aAttackHorse, aDefence, aSpeed, aSight : Integer)');
    RegisterMethodCheck(c, 'procedure UnitHPSetInvulnerable(aUnitID: Integer; aInvulnerable: Boolean)');
    RegisterMethodCheck(c, 'procedure UnitHungerPaceSet(aUnitID: Integer; aPace: Cardinal)');
    RegisterMethodCheck(c, 'procedure UnitHungerSet(aUnitID: Integer; aHungerLevel: Integer)');
    RegisterMethodCheck(c, 'procedure UnitKill(aUnitID: Integer; aSilent: Boolean)');
    RegisterMethodCheck(c, 'function  UnitOrderWalk(aUnitID: Integer; X, Y: Integer): Boolean');
    RegisterMethodCheck(c, 'function  UnitSetInstantKill(aUnitID: Integer; isInstant: Boolean): Boolean');
    RegisterMethodCheck(c, 'procedure  UnitBlockWalking(aUnitID: Integer; aBlock: Boolean)');

    RegisterMethodCheck(c, 'procedure  GroupSetFlagColor(aGroupID: Integer; aColor: Cardinal)');
    RegisterMethodCheck(c, 'procedure  HouseSetStats(aHouseID: Integer; aStats: TKMHouseStats)');
    RegisterMethodCheck(c, 'procedure  MoveCamera(aPlayer, aX, aY : Integer)');
    RegisterMethodCheck(c, 'procedure  ResetZoom(aPlayer: Integer)');
    RegisterMethodCheck(c, 'procedure  UnitSetFlagColor(aUnitID: Integer; aColor: Cardinal)');
    RegisterMethodCheck(c, 'procedure  SpecialAnimAdd(aX, aY : Single; aAnim : array of Integer; aLoopTimes : Byte)');
    RegisterMethodCheck(c, 'procedure  SpecialAnimAddFront(aX, aY : Single; aAnim : array of Integer; aLoopTimes : Byte)');
    RegisterMethodCheck(c, 'procedure  UnitSetRage(aUnitID: Integer; aDuration: Integer)');
    RegisterMethodCheck(c, 'procedure  UnitSetStats(aUnitID: Integer; aStats: TKMUnitStats)');
    RegisterMethodCheck(c, 'procedure  UnitSetThought(aUnitID : Integer; aThought : TKMUnitThought)');
    RegisterMethodCheck(c, 'procedure  WeatherSpawn(aType : TKMWeatherType; aLifeTime : Cardinal; aX, aY : Single; aSpeedX, aSpeedY : Single)');
    RegisterMethodCheck(c, 'procedure  MusicPlay(aName : String; aForceOn : Boolean)');
    RegisterMethodCheck(c, 'procedure  DebugShowGrid(aShow: Boolean)');
    //*Actions-Check*//

    c := Sender.AddClassN(nil, AnsiString(fUtils.ClassName));
    //*Utils-Check*//
    RegisterMethodCheck(c, 'function  AbsI(aValue: Integer): Integer');
    RegisterMethodCheck(c, 'function  AbsS(aValue: Single): Single');
    RegisterMethodCheck(c, 'function  ArrayElementCount(aElement: AnsiString; aArray: array of String): Integer');
    RegisterMethodCheck(c, 'function  ArrayElementCountB(aElement: Boolean; aArray: array of Boolean): Integer');
    RegisterMethodCheck(c, 'function  ArrayElementCountI(aElement: Integer; aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function  ArrayElementCountS(aElement: Single; aArray: array of Single): Integer');
    RegisterMethodCheck(c, 'function  ArrayHasElement(aElement: AnsiString; aArray: array of String): Boolean');
    RegisterMethodCheck(c, 'function  ArrayHasElementB(aElement: Boolean; aArray: array of Boolean): Boolean');
    RegisterMethodCheck(c, 'function  ArrayHasElementI(aElement: Integer; aArray: array of Integer): Boolean');
    RegisterMethodCheck(c, 'function  ArrayHasElementS(aElement: Single; aArray: array of Single): Boolean');
    RegisterMethodCheck(c, 'function  ArrayRemoveIndexI(aIndex: Integer; aArray: TIntegerArray): TIntegerArray');
    RegisterMethodCheck(c, 'function  ArrayRemoveIndexS(aIndex: Integer; aArray: TAnsiStringArray): TAnsiStringArray');
    RegisterMethodCheck(c, 'function  BoolToStr(aBool: Boolean): AnsiString');
    RegisterMethodCheck(c, 'function  CeilTo(aValue: Single; aBase: Integer): Integer');
    RegisterMethodCheck(c, 'function  ColorBrightness(aHexColor: string): Single');
    RegisterMethodCheck(c, 'function  CompareString(Str1: string; Str2: string): Integer');
    RegisterMethodCheck(c, 'function  CompareText(Str1: string; Str2: string): Integer');
    RegisterMethodCheck(c, 'function  CopyString(Str: string; Index: Integer; Count: Integer): String');
    RegisterMethodCheck(c, 'procedure DeleteString(var Str: string; Index: Integer; Count: Integer)');
    RegisterMethodCheck(c, 'function  EnsureRangeI(aValue: Integer; aMin, aMax: Integer): Integer');
    RegisterMethodCheck(c, 'function  EnsureRangeS(aValue: Single; aMin, aMax: Single): Single');
    RegisterMethodCheck(c, 'function  FloorTo(aValue: Single; aBase: Integer): Integer');
    RegisterMethodCheck(c, 'function  Format(aFormatting: string; aData: array of const): string');
    RegisterMethodCheck(c, 'function  FormatFloat(aFormat: string; aValue: Single): string');
    RegisterMethodCheck(c, 'function  IfThen(aBool: Boolean; aTrue: AnsiString; aFalse: AnsiString): AnsiString');
    RegisterMethodCheck(c, 'function  IfThenI(aBool: Boolean; aTrue: Integer; aFalse: Integer): Integer');
    RegisterMethodCheck(c, 'function  IfThenS(aBool: Boolean; aTrue: Single; aFalse: Single): Single');
    RegisterMethodCheck(c, 'function  InAreaI(aX, aY: Integer; aXMin: Integer; aYMin: Integer; aXMax: Integer; aYMax: Integer): Boolean');
    RegisterMethodCheck(c, 'function  InAreaS(aX, aY: Single; aXMin: Single; aYMin: Single; aXMax: Single; aYMax: Single): Boolean');
    RegisterMethodCheck(c, 'function  InRangeI(aValue: Integer; aMin, aMax: Integer): Boolean');
    RegisterMethodCheck(c, 'function  InRangeS(aValue: Single; aMin, aMax: Single): Boolean');
    RegisterMethodCheck(c, 'procedure InsertString(Source: string; var Target: string; Index: Integer)');
    RegisterMethodCheck(c, 'function  KMPoint(X, Y: Integer): TKMPoint');
    RegisterMethodCheck(c, 'function  LowerCase(Str: string): String');
    RegisterMethodCheck(c, 'function  MaxI(A, B: Integer): Integer');
    RegisterMethodCheck(c, 'function  MaxInArrayI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function  MaxInArrayS(aArray: array of Single): Single');
    RegisterMethodCheck(c, 'function  MaxS(A, B: Single): Single');
    RegisterMethodCheck(c, 'function  MinI(A, B: Integer): Integer');
    RegisterMethodCheck(c, 'function  MinInArrayI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function  MinInArrayS(aArray: array of Single): Single');
    RegisterMethodCheck(c, 'function  MinS(A, B: Single): Single');
    RegisterMethodCheck(c, 'procedure MoveString(Source: string; var Destination: string; Count: Integer)');
    RegisterMethodCheck(c, 'function  Pos(SubStr: string; Str: string): Integer');
    RegisterMethodCheck(c, 'function  Power(aBase: Extended; aExp: Extended): Extended');
    RegisterMethodCheck(c, 'function  RandomRangeI(aFrom: Integer; aTo: Integer): Integer');
    RegisterMethodCheck(c, 'function  RGBDecToBGRHex(aR: Byte; aG: Byte; aB: Byte): AnsiString');
    RegisterMethodCheck(c, 'function  RGBToBGRHex(aHexColor: string): AnsiString');
    RegisterMethodCheck(c, 'function  RoundTo(aValue: Single; aBase: Integer): Integer');
    RegisterMethodCheck(c, 'function  Sqr(A: Extended): Extended');
    RegisterMethodCheck(c, 'function  StringReplace(Str: string; OldPattern: string; NewPattern: string; aReplaceAll: Boolean; ' +
      'aIgnoreCase: Boolean): String');
    RegisterMethodCheck(c, 'function  SumI(aArray: array of Integer): Integer');
    RegisterMethodCheck(c, 'function  SumS(aArray: array of Single): Single');
    RegisterMethodCheck(c, 'function  TimeToString(aTicks: Integer): AnsiString');
    RegisterMethodCheck(c, 'function  TimeToTick(aHours: Integer; aMinutes: Integer; aSeconds: Integer): Cardinal');
    RegisterMethodCheck(c, 'function  Trim(Str: string): String');
    RegisterMethodCheck(c, 'function  TrimLeft(Str: string): String');
    RegisterMethodCheck(c, 'function  TrimRight(Str: string): String');
    RegisterMethodCheck(c, 'function  TruncTo(aValue: Single; aBase: Integer): Integer');
    RegisterMethodCheck(c, 'function  UpperCase(Str: string): String');
    //*Utils-Check*//

    // Register objects
    AddImportedClassVariable(Sender, 'States', AnsiString(fStates.ClassName));
    AddImportedClassVariable(Sender, 'Actions', AnsiString(fActions.ClassName));
    AddImportedClassVariable(Sender, 'Utils', AnsiString(fUtils.ClassName));
    AddImportedClassVariable(Sender, 'S', AnsiString(fStates.ClassName));
    AddImportedClassVariable(Sender, 'A', AnsiString(fActions.ClassName));
    AddImportedClassVariable(Sender, 'U', AnsiString(fUtils.ClassName));

    Result := True;
  end
  else
    Result := False;
end;


procedure TKMScripting.ScriptOnUseVariable(Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Integer; ProcNo, Position: Cardinal; const PropData: tbtString);
begin
  //There's no variable type info here
  //GetVarCount is not including this current variable yet either
end;


{ The OnExportCheck callback function is called for each function in the script
  (Also for the main proc, with '!MAIN' as a Proc^.Name). ProcDecl contains the
  result type and parameter types of a function using this format:
  ProcDecl: ResultType + ' ' + Parameter1 + ' ' + Parameter2 + ' '+Parameter3 + .....
  Parameter: ParameterType+TypeName
  ParameterType is @ for a normal parameter and ! for a var parameter.
  A result type of 0 means no result}
function TKMScripting.ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
const
  PROCS: array [TKMScriptEventType] of record
    ParamCount: Byte;
    Typ: array [0..4] of Byte;
    Dir: array [0..3] of TPSParameterMode;
  end = (
    //*Events-Check*//
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnBeacon
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnFieldBuilt
    (ParamCount: 1; Typ: (0, btSingle, 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGameSpeedChanged
    (ParamCount: 4; Typ: (0, btS32 , btEnum, btS32 , btEnum); Dir: (pmIn, pmInOut, pmInOut, pmInOut)), // OnGroupBeforeOrderSplit
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupHungry
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupOrderAttackHouse
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupOrderAttackUnit
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupOrderLink
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupOrderMove
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnGroupOrderSplit
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseAfterDestroyed
    (ParamCount: 4; Typ: (0, btEnum, btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseAfterDestroyedEx
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseBuilt
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseDamaged
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseDestroyed
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHousePlanDigged
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHousePlanPlaced
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHousePlanPlacedEx
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHousePlanRemoved
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHousePlanRemovedEx
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseRepaired
    (ParamCount: 4; Typ: (0, btS32 , btEnum, btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseWareCountChanged
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnMarketTrade
    (ParamCount: 3; Typ: (0, btS32 , btEnum, btEnum, 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnMarketTradeEx

    (ParamCount: 4; Typ: (0, btS32 , btS32, btEnum, btS32     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnMerchantTrade

    (ParamCount: 0; Typ: (0, 0     , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnMissionStart
    (ParamCount: 0; Typ: (0, 0     , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPeacetimeEnd

    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // evtFieldPlanPlaced
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // evtFieldPlanRemoved
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // evtFieldPlanDigged
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btEnum     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // evtFieldPlanBuilt

    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanFieldPlaced
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanFieldRemoved
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanRoadDigged
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanRoadPlaced
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanRoadRemoved
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanWinefieldDigged
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanWinefieldPlaced
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlanWinefieldRemoved
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlayerDefeated
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnPlayerVictory
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnRoadBuilt
    (ParamCount: 0; Typ: (0, 0     , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnTick
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitAfterDied
    (ParamCount: 4; Typ: (0, btEnum, btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitAfterDiedEx
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitAttacked
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitDied
    (ParamCount: 1; Typ: (0, btS32 , 0     , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitTrained
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitWounded
    (ParamCount: 3; Typ: (0, btS32 , btEnum, btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnWareProduced
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnWarriorEquipped
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnWarriorWalked
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnWinefieldBuilt
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32 , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnCustomPanelButtonPressed
    (ParamCount: 4; Typ: (0, btS32 , btS32 , btS32 , btS32 ); Dir: (pmIn, pmIn, pmIn, pmIn)),  // OnCustomCursorClick
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0     , 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitHit
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32, 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnUnitSelected
    (ParamCount: 3; Typ: (0, btS32 , btS32 , btS32, 0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseSelected
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0,     0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnHouseUpgraded
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0,     0     ); Dir: (pmIn, pmIn, pmIn, pmIn)), // OnShipLoaded
    (ParamCount: 2; Typ: (0, btS32 , btS32 , 0,     0     ); Dir: (pmIn, pmIn, pmIn, pmIn)) // OnShipUnload
    //*Events-Check*//
  );
var
  I: Integer;
  EVT: TKMScriptEventType;
  pcm: TPSPascalCompilerMessage;
  handlers: TKMCustomEventHandlerArray;
begin
  Result := True;
  for EVT := Low(TKMScriptEventType) to High(TKMScriptEventType) do
  begin
    handlers := gScriptEvents.EventHandlers[EVT];
    for I := Low(handlers) to High(handlers) do
      if FastUppercase(handlers[I].ProcName) = FastUppercase(Proc.Name) then
        if not ExportCheck(Sender, Proc, Slice(PROCS[EVT].Typ, PROCS[EVT].ParamCount+1), Slice(PROCS[EVT].Dir, PROCS[EVT].ParamCount)) then
        begin
          // Error when validating the Proc arguments (count, types and directions):
          // ExportCheck does the check, but does not tell us details. We could override it of course,
          // but overall this seems unnecessary, since all the errors are basically "declaration does not match".
          // Hence we can just emit the error appending proc name to it and re-setting the Row/Col
          // (since they point to the files)

          // Do not set Module name here (1st parameter),
          // it could be set further as an included script on PreProcessor.AdjustMessages
          pcm := Sender.MakeError('', ecCustomError , 'Error in declaration of ''' + handlers[I].ProcName + '''');
          pcm.Row := Proc.DeclareRow;
          pcm.Col := Proc.DeclareCol;
          pcm.Pos := Proc.DeclarePos;
          Exit(False);
        end;
  end;
end;


procedure TKMScripting.ScriptOnAddType(Sender: TPSPascalCompiler; var aType: TPSType);
begin
  if FastUppercase(aType.Name) = FastUppercase(CAMPAIGN_DATA_TYPE) then
    // Mark TKMCampaignData type to export its name, so its export name will be available in runtime
    aType.ExportName := True;
end;


procedure TKMScripting.ScriptOnAddGlobalVar(Sender: TPSPascalCompiler; var aGlobalVar: TPSVar);
begin
  // There is variable type info here
  // We do not want to get "Variable 'CAMPAIGNDATA' never used" compilation hint, in case its not used in some campaign missions
  if FastUppercase(aGlobalVar.aType.Name) = FastUppercase(CAMPAIGN_DATA_TYPE) then
    aGlobalVar.Use;
end;


procedure TKMScripting.AddError(aMsg: TPSPascalCompilerMessage);
begin
  fErrorHandler.AppendError(GetErrorMessage(aMsg));
  fValidationIssues.AddError(aMsg.Row, aMsg.Col, aMsg.Param, aMsg.ShortMessageToString);
end;


procedure TKMScripting.CompileScript;
var
  I: Integer;
  compiler: TPSPascalCompiler;
  compileSuccess: Boolean;
  msg: TPSPascalCompilerMessage;
begin
  compiler := TPSPascalCompiler.Create; // create an instance of the compiler
  try
    compiler.OnUses := ScriptOnUsesFunc; // assign the OnUses event
    compiler.OnUseVariable := ScriptOnUseVariableProc;
    compiler.OnExportCheck := ScriptOnExportCheckFunc; // Assign the onExportCheck event
    compiler.OnAddType := ScriptOnAddTypeProc;
    compiler.OnAddGlobalVar := ScriptOnAddGlobalVarProc;

    compiler.AllowNoEnd := True; //Scripts only use event handlers now, main section is unused
    compiler.BooleanShortCircuit := True; //Like unchecking "Complete booolean evaluation" in Delphi compiler options

    compileSuccess := compiler.Compile(fScriptCode); // Compile the Pascal script into bytecode

    fPreProcessor.PSPreProcessor.AdjustMessages(compiler);

    for I := 0 to compiler.MsgCount - 1 do
    begin
      msg := compiler.Msg[I];

      if msg.ErrorType = 'Hint' then
      begin
        fErrorHandler.AppendHint(GetErrorMessage(msg));
        fValidationIssues.AddHint(msg.Row, msg.Col, msg.Param, msg.ShortMessageToString);
      end
      else if msg.ErrorType = 'Warning' then
      begin
        fErrorHandler.AppendWarning(GetErrorMessage(msg));
        fValidationIssues.AddWarning(msg.Row, msg.Col, msg.Param, msg.ShortMessageToString);
      end else
        AddError(msg);
    end;

    if not compileSuccess then
      Exit;

    compiler.GetOutput(fByteCode);            // Save the output of the compiler in the string Data.
    compiler.GetDebugOutput(fDebugByteCode);  // Save the debug output of the compiler
  finally
    compiler.Free;
  end;

  LinkRuntime;
end;


//Link the ByteCode with used functions and load it into Executioner
procedure TKMScripting.LinkRuntime;

  function ValidateVarType(aType: TPSTypeRec): UnicodeString;
  var
    I: Integer;
  begin
    //Check against our set of allowed types
    if not (aType.BaseType in VALID_GLOBAL_VAR_TYPES) then
    begin
      Result := Format('Unsupported global variable type %d (%s)|', [aType.BaseType, UnicodeString(aType.ExportName)]);
      Exit;
    end;

    //Check elements of arrays/records are valid too
    case aType.BaseType of
      btArray,
      btStaticArray:
        Result := ValidateVarType(TPSTypeRec_Array(aType).ArrayType);
      btRecord:
        begin
          Result := '';
          for I := 0 to TPSTypeRec_Record(aType).FieldTypes.Count - 1 do
            Result := Result + ValidateVarType(TPSTypeRec_Record(aType).FieldTypes[I]);
        end;
    end;
  end;

var
  classImp: TPSRuntimeClassImporter;
  I: Integer;
  V: PIFVariant;
  errStr: string;
begin
  //Create an instance of the runtime class importer
  classImp := TPSRuntimeClassImporter.Create;
  try
    //Register classes and their exposed methods to Runtime
    //(uppercase is not needed, FastUpperCase does this well. See uPSRuntime.pas, line 11387)
    with classImp.Add(TKMScriptStates) do
    begin
      //*States-Reg*//
      RegisterMethod(@TKMScriptStates.AAIAttackHouseTypesGet, 'AAIAttackHouseTypesGet');
      RegisterMethod(@TKMScriptStates.AIArmyType, 'AIArmyType');
      RegisterMethod(@TKMScriptStates.AIAutoAttack, 'AIAutoAttack');
      RegisterMethod(@TKMScriptStates.AIAutoAttackRange, 'AIAutoAttackRange');
      RegisterMethod(@TKMScriptStates.AIAutoBuild, 'AIAutoBuild');
      RegisterMethod(@TKMScriptStates.AIAutoDefence, 'AIAutoDefence');
      RegisterMethod(@TKMScriptStates.AIAutoRepair, 'AIAutoRepair');
      RegisterMethod(@TKMScriptStates.AIDefencePositionGet, 'AIDefencePositionGet');
      RegisterMethod(@TKMScriptStates.AIDefencePositionGetByIndex, 'AIDefencePositionGetByIndex');
      RegisterMethod(@TKMScriptStates.AIDefendAllies, 'AIDefendAllies');
      RegisterMethod(@TKMScriptStates.AIEquipRate, 'AIEquipRate');
      RegisterMethod(@TKMScriptStates.AIGroupsFormationGet, 'AIGroupsFormationGet');
      RegisterMethod(@TKMScriptStates.AIGroupsFormationGetEx, 'AIGroupsFormationGetEx');
      RegisterMethod(@TKMScriptStates.AIRecruitDelay, 'AIRecruitDelay');
      RegisterMethod(@TKMScriptStates.AIRecruitLimit, 'AIRecruitLimit');
      RegisterMethod(@TKMScriptStates.AIRepairMode, 'AIRepairMode');
      RegisterMethod(@TKMScriptStates.AISerfsPerHouse, 'AISerfsPerHouse');
      RegisterMethod(@TKMScriptStates.AISoldiersLimit, 'AISoldiersLimit');
      RegisterMethod(@TKMScriptStates.AIStartPosition, 'AIStartPosition');
      RegisterMethod(@TKMScriptStates.AIUnlimitedEquip, 'AIUnlimitedEquip');
      RegisterMethod(@TKMScriptStates.AIWorkerLimit, 'AIWorkerLimit');
      RegisterMethod(@TKMScriptStates.CampaignMissionID, 'CampaignMissionID');
      RegisterMethod(@TKMScriptStates.CampaignMissionsCount, 'CampaignMissionsCount');
      RegisterMethod(@TKMScriptStates.ClosestGroup, 'ClosestGroup');
      RegisterMethod(@TKMScriptStates.ClosestGroupEx, 'ClosestGroupEx');
      RegisterMethod(@TKMScriptStates.ClosestGroupMultipleTypes, 'ClosestGroupMultipleTypes');
      RegisterMethod(@TKMScriptStates.ClosestGroupMultipleTypesEx, 'ClosestGroupMultipleTypesEx');
      RegisterMethod(@TKMScriptStates.ClosestHouse, 'ClosestHouse');
      RegisterMethod(@TKMScriptStates.ClosestHouseEx, 'ClosestHouseEx');
      RegisterMethod(@TKMScriptStates.ClosestHouseMultipleTypes, 'ClosestHouseMultipleTypes');
      RegisterMethod(@TKMScriptStates.ClosestHouseMultipleTypesEx, 'ClosestHouseMultipleTypesEx');
      RegisterMethod(@TKMScriptStates.ClosestUnit, 'ClosestUnit');
      RegisterMethod(@TKMScriptStates.ClosestUnitEx, 'ClosestUnitEx');
      RegisterMethod(@TKMScriptStates.ClosestUnitMultipleTypes, 'ClosestUnitMultipleTypes');
      RegisterMethod(@TKMScriptStates.ClosestUnitMultipleTypesEx, 'ClosestUnitMultipleTypesEx');
      RegisterMethod(@TKMScriptStates.ConnectedByRoad, 'ConnectedByRoad');
      RegisterMethod(@TKMScriptStates.ConnectedByWalking, 'ConnectedByWalking');
      RegisterMethod(@TKMScriptStates.CursorPos, 'CursorPos');
      RegisterMethod(@TKMScriptStates.FogRevealed, 'FogRevealed');
      RegisterMethod(@TKMScriptStates.GameSpeed, 'GameSpeed');
      RegisterMethod(@TKMScriptStates.GameSpeedChangeAllowed, 'GameSpeedChangeAllowed');
      RegisterMethod(@TKMScriptStates.GameTime, 'GameTime');
      RegisterMethod(@TKMScriptStates.GroupAllowAllyToSelect, 'GroupAllowAllyToSelect');
      RegisterMethod(@TKMScriptStates.GroupAssignedToDefencePosition, 'GroupAssignedToDefencePosition');
      RegisterMethod(@TKMScriptStates.GroupAt, 'GroupAt');
      RegisterMethod(@TKMScriptStates.GroupColumnCount, 'GroupColumnCount');
      RegisterMethod(@TKMScriptStates.GroupDead, 'GroupDead');
      RegisterMethod(@TKMScriptStates.GroupIdle, 'GroupIdle');
      RegisterMethod(@TKMScriptStates.GroupInFight, 'GroupInFight');
      RegisterMethod(@TKMScriptStates.GroupManualFormation, 'GroupManualFormation');
      RegisterMethod(@TKMScriptStates.GroupMember, 'GroupMember');
      RegisterMethod(@TKMScriptStates.GroupMemberCount, 'GroupMemberCount');
      RegisterMethod(@TKMScriptStates.GroupOrder, 'GroupOrder');
      RegisterMethod(@TKMScriptStates.GroupOwner, 'GroupOwner');
      RegisterMethod(@TKMScriptStates.GroupType, 'GroupType');
      RegisterMethod(@TKMScriptStates.GroupTypeEx, 'GroupTypeEx');
      RegisterMethod(@TKMScriptStates.HouseAllowAllyToSelect, 'HouseAllowAllyToSelect');
      RegisterMethod(@TKMScriptStates.HouseAt, 'HouseAt');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointX, 'HouseBarracksRallyPointX');
      RegisterMethod(@TKMScriptStates.HouseBarracksRallyPointY, 'HouseBarracksRallyPointY');
      RegisterMethod(@TKMScriptStates.HouseBarracksRecruitBlock, 'HouseBarracksRecruitBlock');
      RegisterMethod(@TKMScriptStates.HouseBarracksRecruitsCount, 'HouseBarracksRecruitsCount');
      RegisterMethod(@TKMScriptStates.HouseBuildingProgress, 'HouseBuildingProgress');
      RegisterMethod(@TKMScriptStates.HouseCanPlace, 'HouseCanPlace');
      RegisterMethod(@TKMScriptStates.HouseCanReachResources, 'HouseCanReachResources');
      RegisterMethod(@TKMScriptStates.HouseDamage, 'HouseDamage');
      RegisterMethod(@TKMScriptStates.HouseDeliveryBlocked, 'HouseDeliveryBlocked');
      RegisterMethod(@TKMScriptStates.HouseDeliveryMode, 'HouseDeliveryMode');
      RegisterMethod(@TKMScriptStates.HouseDestroyed, 'HouseDestroyed');
      RegisterMethod(@TKMScriptStates.HouseFlagPoint, 'HouseFlagPoint');
      RegisterMethod(@TKMScriptStates.HouseGetAllUnitsIn, 'HouseGetAllUnitsIn');
      RegisterMethod(@TKMScriptStates.HouseHasOccupant, 'HouseHasOccupant');
      RegisterMethod(@TKMScriptStates.HouseHasWorker, 'HouseHasWorker');
      RegisterMethod(@TKMScriptStates.HouseIsComplete, 'HouseIsComplete');
      RegisterMethod(@TKMScriptStates.HouseOwner, 'HouseOwner');
      RegisterMethod(@TKMScriptStates.HousePosition, 'HousePosition');
      RegisterMethod(@TKMScriptStates.HousePositionX, 'HousePositionX');
      RegisterMethod(@TKMScriptStates.HousePositionY, 'HousePositionY');
      RegisterMethod(@TKMScriptStates.HouseRepair, 'HouseRepair');
      RegisterMethod(@TKMScriptStates.HouseResourceAmount, 'HouseResourceAmount');
      RegisterMethod(@TKMScriptStates.HouseSchoolQueue, 'HouseSchoolQueue');
      RegisterMethod(@TKMScriptStates.HouseSiteIsDigged, 'HouseSiteIsDigged');
      RegisterMethod(@TKMScriptStates.HouseTownHallMaxGold, 'HouseTownHallMaxGold');
      RegisterMethod(@TKMScriptStates.HouseType, 'HouseType');
      RegisterMethod(@TKMScriptStates.HouseTypeEx, 'HouseTypeEx');
      RegisterMethod(@TKMScriptStates.HouseTypeMaxHealth, 'HouseTypeMaxHealth');
      RegisterMethod(@TKMScriptStates.HouseTypeMaxHealthEx, 'HouseTypeMaxHealthEx');
      RegisterMethod(@TKMScriptStates.HouseTypeName, 'HouseTypeName');
      RegisterMethod(@TKMScriptStates.HouseTypeNameEx, 'HouseTypeNameEx');
      RegisterMethod(@TKMScriptStates.HouseTypeToOccupantType, 'HouseTypeToOccupantType');
      RegisterMethod(@TKMScriptStates.HouseTypeToWorkerType, 'HouseTypeToWorkerType');
      RegisterMethod(@TKMScriptStates.HouseUnlocked, 'HouseUnlocked');
      RegisterMethod(@TKMScriptStates.HouseWareAmount, 'HouseWareAmount');
      RegisterMethod(@TKMScriptStates.HouseWareBlocked, 'HouseWareBlocked');
      RegisterMethod(@TKMScriptStates.HouseWareBlockedEx, 'HouseWareBlockedEx');
      RegisterMethod(@TKMScriptStates.HouseWareBlockedTakeOut, 'HouseWareBlockedTakeOut');
      RegisterMethod(@TKMScriptStates.HouseWeaponsOrdered, 'HouseWeaponsOrdered');
      RegisterMethod(@TKMScriptStates.HouseWeaponsOrderedEx, 'HouseWeaponsOrderedEx');
      RegisterMethod(@TKMScriptStates.HouseWoodcutterChopOnly, 'HouseWoodcutterChopOnly');
      RegisterMethod(@TKMScriptStates.HouseWoodcutterMode, 'HouseWoodcutterMode');
      RegisterMethod(@TKMScriptStates.HouseWorker, 'HouseWorker');
      RegisterMethod(@TKMScriptStates.HouseArea, 'HouseArea');
      RegisterMethod(@TKMScriptStates.HouseEntranceOffset, 'HouseEntranceOffset');
      RegisterMethod(@TKMScriptStates.HouseTypeToID, 'HouseTypeToID');
      RegisterMethod(@TKMScriptStates.HouseIDtoType, 'HouseIDtoType');
      RegisterMethod(@TKMScriptStates.IsFieldAt, 'IsFieldAt');
      RegisterMethod(@TKMScriptStates.IsFieldPlanAt, 'IsFieldPlanAt');
      RegisterMethod(@TKMScriptStates.IsHousePlanAt, 'IsHousePlanAt');
      RegisterMethod(@TKMScriptStates.IsMissionBlockColorSelection, 'IsMissionBlockColorSelection');
      RegisterMethod(@TKMScriptStates.IsMissionBlockFullMapPreview, 'IsMissionBlockFullMapPreview');
      RegisterMethod(@TKMScriptStates.IsMissionBlockPeacetime, 'IsMissionBlockPeacetime');
      RegisterMethod(@TKMScriptStates.IsMissionBlockTeamSelection, 'IsMissionBlockTeamSelection');
      RegisterMethod(@TKMScriptStates.IsMissionBuildType, 'IsMissionBuildType');
      RegisterMethod(@TKMScriptStates.IsMissionCoopType, 'IsMissionCoopType');
      RegisterMethod(@TKMScriptStates.IsMissionFightType, 'IsMissionFightType');
      RegisterMethod(@TKMScriptStates.IsMissionPlayableAsSP, 'IsMissionPlayableAsSP');
      RegisterMethod(@TKMScriptStates.IsMissionSpecialType, 'IsMissionSpecialType');
      RegisterMethod(@TKMScriptStates.IsPlanAt, 'IsPlanAt');
      RegisterMethod(@TKMScriptStates.IsRoadAt, 'IsRoadAt');
      RegisterMethod(@TKMScriptStates.IsRoadPlanAt, 'IsRoadPlanAt');
      RegisterMethod(@TKMScriptStates.IsWinefieldAt, 'IsWinefieldAt');
      RegisterMethod(@TKMScriptStates.IsWinefieldPlanAt, 'IsWinefieldPlanAt');
      RegisterMethod(@TKMScriptStates.KaMRandom, 'KaMRandom');
      RegisterMethod(@TKMScriptStates.KaMRandomI, 'KaMRandomI');
      RegisterMethod(@TKMScriptStates.LocationCount, 'LocationCount');
      RegisterMethod(@TKMScriptStates.MapHeight, 'MapHeight');
      RegisterMethod(@TKMScriptStates.MapFieldType, 'MapFieldType');
      RegisterMethod(@TKMScriptStates.MapTileHasOnlyTerrainKind, 'MapTileHasOnlyTerrainKind');
      RegisterMethod(@TKMScriptStates.MapTileHasOnlyTerrainKinds, 'MapTileHasOnlyTerrainKinds');
      RegisterMethod(@TKMScriptStates.MapTileHasTerrainKind, 'MapTileHasTerrainKind');
      RegisterMethod(@TKMScriptStates.MapTileHeight, 'MapTileHeight');
      RegisterMethod(@TKMScriptStates.MapTileIsCoal, 'MapTileIsCoal');
      RegisterMethod(@TKMScriptStates.MapTileIsGold, 'MapTileIsGold');
      RegisterMethod(@TKMScriptStates.MapTileIsIce, 'MapTileIsIce');
      RegisterMethod(@TKMScriptStates.MapTileIsInMapCoords, 'MapTileIsInMapCoords');
      RegisterMethod(@TKMScriptStates.MapTileIsIron, 'MapTileIsIron');
      RegisterMethod(@TKMScriptStates.MapTileIsSand, 'MapTileIsSand');
      RegisterMethod(@TKMScriptStates.MapTileIsSnow, 'MapTileIsSnow');
      RegisterMethod(@TKMScriptStates.MapTileIsSoil, 'MapTileIsSoil');
      RegisterMethod(@TKMScriptStates.MapTileIsStone, 'MapTileIsStone');
      RegisterMethod(@TKMScriptStates.MapTileIsWater, 'MapTileIsWater');
      RegisterMethod(@TKMScriptStates.MapTileObject, 'MapTileObject');
      RegisterMethod(@TKMScriptStates.MapTileOverlay, 'MapTileOverlay');
      RegisterMethod(@TKMScriptStates.MapTileOwner, 'MapTileOwner');
      RegisterMethod(@TKMScriptStates.MapTilePassability, 'MapTilePassability');
      RegisterMethod(@TKMScriptStates.MapTilePassabilityEx, 'MapTilePassabilityEx');
      RegisterMethod(@TKMScriptStates.MapTileSelected, 'MapTileSelected');
      RegisterMethod(@TKMScriptStates.MapTileRotation, 'MapTileRotation');
      RegisterMethod(@TKMScriptStates.MapTileType, 'MapTileType');
      RegisterMethod(@TKMScriptStates.MapWidth, 'MapWidth');
      RegisterMethod(@TKMScriptStates.MarketFromWare, 'MarketFromWare');
      RegisterMethod(@TKMScriptStates.MarketFromWareEx, 'MarketFromWareEx');
      RegisterMethod(@TKMScriptStates.MarketLossFactor, 'MarketLossFactor');
      RegisterMethod(@TKMScriptStates.MarketOrderAmount, 'MarketOrderAmount');
      RegisterMethod(@TKMScriptStates.MarketToWare, 'MarketToWare');
      RegisterMethod(@TKMScriptStates.MarketToWareEx, 'MarketToWareEx');
      RegisterMethod(@TKMScriptStates.MarketValue, 'MarketValue');
      RegisterMethod(@TKMScriptStates.MarketValueEx, 'MarketValueEx');
      RegisterMethod(@TKMScriptStates.MissionAuthor, 'MissionAuthor');
      RegisterMethod(@TKMScriptStates.MissionDifficulty, 'MissionDifficulty');
      RegisterMethod(@TKMScriptStates.MissionDifficultyLevels, 'MissionDifficultyLevels');
      RegisterMethod(@TKMScriptStates.MissionVersion, 'MissionVersion');
      RegisterMethod(@TKMScriptStates.PeaceTime, 'PeaceTime');
      RegisterMethod(@TKMScriptStates.PlayerAllianceCheck, 'PlayerAllianceCheck');
      RegisterMethod(@TKMScriptStates.PlayerColorFlag, 'PlayerColorFlag');
      RegisterMethod(@TKMScriptStates.PlayerColorText, 'PlayerColorText');
      RegisterMethod(@TKMScriptStates.PlayerDefeated, 'PlayerDefeated');
      RegisterMethod(@TKMScriptStates.PlayerEnabled, 'PlayerEnabled');
      RegisterMethod(@TKMScriptStates.PlayerGetAllGroups, 'PlayerGetAllGroups');
      RegisterMethod(@TKMScriptStates.PlayerGetAllHouses, 'PlayerGetAllHouses');
      RegisterMethod(@TKMScriptStates.PlayerGetAllUnits, 'PlayerGetAllUnits');
      RegisterMethod(@TKMScriptStates.PlayerHouseTypeCanBuild, 'PlayerHouseTypeCanBuild');
      RegisterMethod(@TKMScriptStates.PlayerHouseTypeLock, 'PlayerHouseTypeLock');
      RegisterMethod(@TKMScriptStates.PlayerIsAdvancedAI, 'PlayerIsAdvancedAI');
      RegisterMethod(@TKMScriptStates.PlayerIsAI, 'PlayerIsAI');
      RegisterMethod(@TKMScriptStates.PlayerIsClassicAI, 'PlayerIsClassicAI');
      RegisterMethod(@TKMScriptStates.PlayerName, 'PlayerName');
      RegisterMethod(@TKMScriptStates.PlayerUnitTypeCanTrain, 'PlayerUnitTypeCanTrain');
      RegisterMethod(@TKMScriptStates.PlayerVictorious, 'PlayerVictorious');
      RegisterMethod(@TKMScriptStates.PlayerWareDistribution, 'PlayerWareDistribution');
      RegisterMethod(@TKMScriptStates.PlayerWareDistributionEx, 'PlayerWareDistributionEx');
      RegisterMethod(@TKMScriptStates.StatAIDefencePositionsCount, 'StatAIDefencePositionsCount');
      RegisterMethod(@TKMScriptStates.StatArmyCount, 'StatArmyCount');
      RegisterMethod(@TKMScriptStates.StatArmyPower, 'StatArmyPower');
      RegisterMethod(@TKMScriptStates.StatCitizenCount, 'StatCitizenCount');
      RegisterMethod(@TKMScriptStates.StatHouseCount, 'StatHouseCount');
      RegisterMethod(@TKMScriptStates.StatHouseMultipleTypesCount, 'StatHouseMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatHouseMultipleTypesCountEx, 'StatHouseMultipleTypesCountEx');
      RegisterMethod(@TKMScriptStates.StatHouseTypeCount, 'StatHouseTypeCount');
      RegisterMethod(@TKMScriptStates.StatHouseTypeCountEx, 'StatHouseTypeCountEx');
      RegisterMethod(@TKMScriptStates.StatHouseTypePlansCount, 'StatHouseTypePlansCount');
      RegisterMethod(@TKMScriptStates.StatHouseTypePlansCountEx, 'StatHouseTypePlansCountEx');
      RegisterMethod(@TKMScriptStates.StatPlayerCount, 'StatPlayerCount');
      RegisterMethod(@TKMScriptStates.StatResourceProducedCount, 'StatResourceProducedCount');
      RegisterMethod(@TKMScriptStates.StatResourceProducedCountEx, 'StatResourceProducedCountEx');
      RegisterMethod(@TKMScriptStates.StatResourceProducedMultipleTypesCount, 'StatResourceProducedMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatResourceProducedMultipleTypesCountEx, 'StatResourceProducedMultipleTypesCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitCount, 'StatUnitCount');
      RegisterMethod(@TKMScriptStates.StatUnitKilledCount, 'StatUnitKilledCount');
      RegisterMethod(@TKMScriptStates.StatUnitKilledCountEx, 'StatUnitKilledCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitKilledMultipleTypesCount, 'StatUnitKilledMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitKilledMultipleTypesCountEx, 'StatUnitKilledMultipleTypesCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitLostCount, 'StatUnitLostCount');
      RegisterMethod(@TKMScriptStates.StatUnitLostCountEx, 'StatUnitLostCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitLostMultipleTypesCount, 'StatUnitLostMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitLostMultipleTypesCountEx, 'StatUnitLostMultipleTypesCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitMultipleTypesCount, 'StatUnitMultipleTypesCount');
      RegisterMethod(@TKMScriptStates.StatUnitMultipleTypesCountEx, 'StatUnitMultipleTypesCountEx');
      RegisterMethod(@TKMScriptStates.StatUnitTypeCount, 'StatUnitTypeCount');
      RegisterMethod(@TKMScriptStates.StatUnitTypeCountEx, 'StatUnitTypeCountEx');
      RegisterMethod(@TKMScriptStates.StatResourceTotalCount, 'StatResourceTotalCount');
      //tower defence
      RegisterMethod(@TKMScriptStates.TDHouseCanPlace, 'TDHouseCanPlace');

      RegisterMethod(@TKMScriptStates.UnitAllowAllyToSelect, 'UnitAllowAllyToSelect');
      RegisterMethod(@TKMScriptStates.UnitAt, 'UnitAt');
      RegisterMethod(@TKMScriptStates.UnitCarrying, 'UnitCarrying');
      RegisterMethod(@TKMScriptStates.UnitCarryingEx, 'UnitCarryingEx');
      RegisterMethod(@TKMScriptStates.UnitDead, 'UnitDead');
      RegisterMethod(@TKMScriptStates.UnitDirection, 'UnitDirection');
      RegisterMethod(@TKMScriptStates.UnitDirectionEx, 'UnitDirectionEx');
      RegisterMethod(@TKMScriptStates.UnitDismissable, 'UnitDismissable');
      RegisterMethod(@TKMScriptStates.UnitHasBitin, 'UnitHasBitin');
      RegisterMethod(@TKMScriptStates.UnitHome, 'UnitHome');
      RegisterMethod(@TKMScriptStates.UnitHPCurrent, 'UnitHPCurrent');
      RegisterMethod(@TKMScriptStates.UnitHPInvulnerable, 'UnitHPInvulnerable');
      RegisterMethod(@TKMScriptStates.UnitHPMax, 'UnitHPMax');
      RegisterMethod(@TKMScriptStates.UnitHunger, 'UnitHunger');
      RegisterMethod(@TKMScriptStates.UnitIdle, 'UnitIdle');
      RegisterMethod(@TKMScriptStates.UnitInHouse, 'UnitInHouse');
      RegisterMethod(@TKMScriptStates.UnitLowHunger, 'UnitLowHunger');
      RegisterMethod(@TKMScriptStates.UnitMaxHunger, 'UnitMaxHunger');
      RegisterMethod(@TKMScriptStates.UnitOwner, 'UnitOwner');
      RegisterMethod(@TKMScriptStates.UnitPosition, 'UnitPosition');
      RegisterMethod(@TKMScriptStates.UnitPositionX, 'UnitPositionX');
      RegisterMethod(@TKMScriptStates.UnitPositionY, 'UnitPositionY');
      RegisterMethod(@TKMScriptStates.UnitsGroup, 'UnitsGroup');
      RegisterMethod(@TKMScriptStates.UnitType, 'UnitType');
      RegisterMethod(@TKMScriptStates.UnitTypeEx, 'UnitTypeEx');
      RegisterMethod(@TKMScriptStates.UnitTypeName, 'UnitTypeName');
      RegisterMethod(@TKMScriptStates.UnitTypeNameEx, 'UnitTypeNameEx');
      RegisterMethod(@TKMScriptStates.UnitTypeToID, 'UnitTypeToID');
      RegisterMethod(@TKMScriptStates.UnitIDToType, 'UnitIDToType');
      RegisterMethod(@TKMScriptStates.WareTypeName, 'WareTypeName');
      RegisterMethod(@TKMScriptStates.WareTypeNameEx, 'WareTypeNameEx');
      RegisterMethod(@TKMScriptStates.WareTypeToID, 'WareTypeToID');
      RegisterMethod(@TKMScriptStates.WareIdToType, 'WareIdToType');
      RegisterMethod(@TKMScriptStates.WarriorInFight, 'WarriorInFight');

      RegisterMethod(@TKMScriptStates.HouseStats, 'HouseStats');
      RegisterMethod(@TKMScriptStates.UnitStats, 'UnitStats');
      //*States-Reg*//
    end;

    with classImp.Add(TKMScriptActions) do
    begin
      //*Actions-Reg*//
      RegisterMethod(@TKMScriptActions.AAIAttackHouseTypesSet, 'AAIAttackHouseTypesSet');
      RegisterMethod(@TKMScriptActions.AIArmyType, 'AIArmyType');
      RegisterMethod(@TKMScriptActions.AIAttackAdd, 'AIAttackAdd');
      RegisterMethod(@TKMScriptActions.AIAttackAddEx, 'AIAttackAddEx');
      RegisterMethod(@TKMScriptActions.AIAttackRemove, 'AIAttackRemove');
      RegisterMethod(@TKMScriptActions.AIAttackRemoveAll, 'AIAttackRemoveAll');
      RegisterMethod(@TKMScriptActions.AIAutoAttack, 'AIAutoAttack');
      RegisterMethod(@TKMScriptActions.AIAutoAttackRange, 'AIAutoAttackRange');
      RegisterMethod(@TKMScriptActions.AIAutoBuild, 'AIAutoBuild');
      RegisterMethod(@TKMScriptActions.AIAutoDefence, 'AIAutoDefence');
      RegisterMethod(@TKMScriptActions.AIAutoRepair, 'AIAutoRepair');
      RegisterMethod(@TKMScriptActions.AIDefencePositionAdd, 'AIDefencePositionAdd');
      RegisterMethod(@TKMScriptActions.AIDefencePositionAddEx, 'AIDefencePositionAddEx');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemove, 'AIDefencePositionRemove');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemoveAll, 'AIDefencePositionRemoveAll');
      RegisterMethod(@TKMScriptActions.AIDefencePositionRemoveByUID, 'AIDefencePositionRemoveByUID');
      RegisterMethod(@TKMScriptActions.AIDefendAllies, 'AIDefendAllies');
      RegisterMethod(@TKMScriptActions.AIEquipRate, 'AIEquipRate');
      RegisterMethod(@TKMScriptActions.AIGroupsFormationSet, 'AIGroupsFormationSet');
      RegisterMethod(@TKMScriptActions.AIGroupsFormationSetEx, 'AIGroupsFormationSetEx');
      RegisterMethod(@TKMScriptActions.AIRecruitDelay, 'AIRecruitDelay');
      RegisterMethod(@TKMScriptActions.AIRecruitLimit, 'AIRecruitLimit');
      RegisterMethod(@TKMScriptActions.AIRepairMode, 'AIRepairMode');
      RegisterMethod(@TKMScriptActions.AISerfsPerHouse, 'AISerfsPerHouse');
      RegisterMethod(@TKMScriptActions.AISoldiersLimit, 'AISoldiersLimit');
      RegisterMethod(@TKMScriptActions.AIStartPosition, 'AIStartPosition');
      RegisterMethod(@TKMScriptActions.AIUnlimitedEquip, 'AIUnlimitedEquip');
      RegisterMethod(@TKMScriptActions.AIWorkerLimit, 'AIWorkerLimit');
      RegisterMethod(@TKMScriptActions.CinematicEnd, 'CinematicEnd');
      RegisterMethod(@TKMScriptActions.CinematicPanTo, 'CinematicPanTo');
      RegisterMethod(@TKMScriptActions.CinematicStart, 'CinematicStart');
      RegisterMethod(@TKMScriptActions.FogCoverAll, 'FogCoverAll');
      RegisterMethod(@TKMScriptActions.FogCoverCircle, 'FogCoverCircle');
      RegisterMethod(@TKMScriptActions.FogCoverRect, 'FogCoverRect');
      RegisterMethod(@TKMScriptActions.FogRevealAll, 'FogRevealAll');
      RegisterMethod(@TKMScriptActions.FogRevealCircle, 'FogRevealCircle');
      RegisterMethod(@TKMScriptActions.FogRevealRect, 'FogRevealRect');
      RegisterMethod(@TKMScriptActions.GameSpeed, 'GameSpeed');
      RegisterMethod(@TKMScriptActions.GameSpeedChangeAllowed, 'GameSpeedChangeAllowed');
      RegisterMethod(@TKMScriptActions.GiveAnimal, 'GiveAnimal');
      RegisterMethod(@TKMScriptActions.GiveAnimalEx, 'GiveAnimalEx');
      RegisterMethod(@TKMScriptActions.GiveField, 'GiveField');
      RegisterMethod(@TKMScriptActions.GiveFieldAged, 'GiveFieldAged');
      RegisterMethod(@TKMScriptActions.GiveGroup, 'GiveGroup');
      RegisterMethod(@TKMScriptActions.GiveGroupEx, 'GiveGroupEx');
      RegisterMethod(@TKMScriptActions.GiveHouse, 'GiveHouse');
      RegisterMethod(@TKMScriptActions.GiveHouseEx, 'GiveHouseEx');
      RegisterMethod(@TKMScriptActions.GiveHouseSite, 'GiveHouseSite');
      RegisterMethod(@TKMScriptActions.GiveHouseSiteEx, 'GiveHouseSiteEx');
      RegisterMethod(@TKMScriptActions.GiveHouseFromStats, 'GiveHouseFromStats');
      RegisterMethod(@TKMScriptActions.GiveRoad, 'GiveRoad');
      RegisterMethod(@TKMScriptActions.GiveUnit, 'GiveUnit');
      RegisterMethod(@TKMScriptActions.GiveUnitEx, 'GiveUnitEx');
      RegisterMethod(@TKMScriptActions.GiveWares, 'GiveWares');
      RegisterMethod(@TKMScriptActions.GiveWaresEx, 'GiveWaresEx');
      RegisterMethod(@TKMScriptActions.GiveWeapons, 'GiveWeapons');
      RegisterMethod(@TKMScriptActions.GiveWeaponsEx, 'GiveWeaponsEx');
      RegisterMethod(@TKMScriptActions.GiveWineField, 'GiveWineField');
      RegisterMethod(@TKMScriptActions.GiveWineFieldAged, 'GiveWineFieldAged');
      RegisterMethod(@TKMScriptActions.GroupAllowAllyToSelect, 'GroupAllowAllyToSelect');
      RegisterMethod(@TKMScriptActions.GroupBlockOrders, 'GroupBlockOrders');
      RegisterMethod(@TKMScriptActions.GroupDisableHungryMessage, 'GroupDisableHungryMessage');
      RegisterMethod(@TKMScriptActions.GroupHungerPaceSet, 'GroupHungerPaceSet');
      RegisterMethod(@TKMScriptActions.GroupHungerSet, 'GroupHungerSet');
      RegisterMethod(@TKMScriptActions.GroupInfiniteAmmoSet, 'GroupInfiniteAmmoSet');
      RegisterMethod(@TKMScriptActions.GroupKillAll, 'GroupKillAll');
      RegisterMethod(@TKMScriptActions.GroupMakeHero, 'GroupMakeHero');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackHouse, 'GroupOrderAttackHouse');
      RegisterMethod(@TKMScriptActions.GroupOrderAttackUnit, 'GroupOrderAttackUnit');
      RegisterMethod(@TKMScriptActions.GroupOrderFood, 'GroupOrderFood');
      RegisterMethod(@TKMScriptActions.GroupOrderHalt, 'GroupOrderHalt');
      RegisterMethod(@TKMScriptActions.GroupOrderLink, 'GroupOrderLink');
      RegisterMethod(@TKMScriptActions.GroupOrderSplit, 'GroupOrderSplit');
      RegisterMethod(@TKMScriptActions.GroupOrderSplitUnit, 'GroupOrderSplitUnit');
      RegisterMethod(@TKMScriptActions.GroupOrderStorm, 'GroupOrderStorm');
      RegisterMethod(@TKMScriptActions.GroupOrderWalk, 'GroupOrderWalk');
      RegisterMethod(@TKMScriptActions.GroupOrderWalkEx, 'GroupOrderWalkEx');
      RegisterMethod(@TKMScriptActions.GroupSetFormation, 'GroupSetFormation');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingMaterials, 'HouseAddBuildingMaterials');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingMaterialsEx, 'HouseAddBuildingMaterialsEx');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingProgress, 'HouseAddBuildingProgress');
      RegisterMethod(@TKMScriptActions.HouseAddBuildingProgressEx, 'HouseAddBuildingProgressEx');
      RegisterMethod(@TKMScriptActions.HouseAddDamage, 'HouseAddDamage');
      RegisterMethod(@TKMScriptActions.HouseAddRepair, 'HouseAddRepair');
      RegisterMethod(@TKMScriptActions.HouseAddWaresTo, 'HouseAddWaresTo');
      RegisterMethod(@TKMScriptActions.HouseAddWaresToEx, 'HouseAddWaresToEx');
      RegisterMethod(@TKMScriptActions.HouseAllow, 'HouseAllow');
      RegisterMethod(@TKMScriptActions.HouseAllowAllyToSelect, 'HouseAllowAllyToSelect');
      RegisterMethod(@TKMScriptActions.HouseAllowAllyToSelectAll, 'HouseAllowAllyToSelectAll');
      RegisterMethod(@TKMScriptActions.HouseBarracksEquip, 'HouseBarracksEquip');
      RegisterMethod(@TKMScriptActions.HouseBarracksEquipEx, 'HouseBarracksEquipEx');
      RegisterMethod(@TKMScriptActions.HouseBarracksGiveRecruit, 'HouseBarracksGiveRecruit');
      RegisterMethod(@TKMScriptActions.HouseBarracksGiveRecruits, 'HouseBarracksGiveRecruits');
      RegisterMethod(@TKMScriptActions.HouseBarracksRecruitBlock, 'HouseBarracksRecruitBlock');
      RegisterMethod(@TKMScriptActions.HouseChangeOwner, 'HouseChangeOwner');
      RegisterMethod(@TKMScriptActions.HouseDeliveryBlock, 'HouseDeliveryBlock');
      RegisterMethod(@TKMScriptActions.HouseDeliveryMode, 'HouseDeliveryMode');
      RegisterMethod(@TKMScriptActions.HouseDestroy, 'HouseDestroy');
      RegisterMethod(@TKMScriptActions.HouseDisableUnoccupiedMessage, 'HouseDisableUnoccupiedMessage');
      RegisterMethod(@TKMScriptActions.HouseRepairEnable, 'HouseRepairEnable');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueAdd, 'HouseSchoolQueueAdd');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueAddEx, 'HouseSchoolQueueAddEx');
      RegisterMethod(@TKMScriptActions.HouseSchoolQueueRemove, 'HouseSchoolQueueRemove');
      RegisterMethod(@TKMScriptActions.HouseTakeWaresFrom, 'HouseTakeWaresFrom');
      RegisterMethod(@TKMScriptActions.HouseTakeWaresFromEx, 'HouseTakeWaresFromEx');
      RegisterMethod(@TKMScriptActions.HouseTownHallEquip, 'HouseTownHallEquip');
      RegisterMethod(@TKMScriptActions.HouseTownHallEquipEx, 'HouseTownHallEquipEx');
      RegisterMethod(@TKMScriptActions.HouseTownHallMaxGold, 'HouseTownHallMaxGold');
      RegisterMethod(@TKMScriptActions.HouseUnlock, 'HouseUnlock');
      RegisterMethod(@TKMScriptActions.HouseWareBlock, 'HouseWareBlock');
      RegisterMethod(@TKMScriptActions.HouseWareBlockEx, 'HouseWareBlockEx');
      RegisterMethod(@TKMScriptActions.HouseWareBlockTakeOut, 'HouseWareBlockTakeOut');
      RegisterMethod(@TKMScriptActions.HouseWeaponsOrderSet, 'HouseWeaponsOrderSet');
      RegisterMethod(@TKMScriptActions.HouseWeaponsOrderSetEx, 'HouseWeaponsOrderSetEx');
      RegisterMethod(@TKMScriptActions.HouseWoodcutterChopOnly, 'HouseWoodcutterChopOnly');
      RegisterMethod(@TKMScriptActions.HouseWoodcutterMode, 'HouseWoodcutterMode');
      RegisterMethod(@TKMScriptActions.Log, 'Log');
      RegisterMethod(@TKMScriptActions.LogLinesMaxCnt, 'LogLinesMaxCnt');
      RegisterMethod(@TKMScriptActions.MapBrush, 'MapBrush');
      RegisterMethod(@TKMScriptActions.MapBrushElevation, 'MapBrushElevation');
      RegisterMethod(@TKMScriptActions.MapBrushEqualize, 'MapBrushEqualize');
      RegisterMethod(@TKMScriptActions.MapBrushFlatten, 'MapBrushFlatten');
      RegisterMethod(@TKMScriptActions.MapBrushMagicWater, 'MapBrushMagicWater');
      RegisterMethod(@TKMScriptActions.MapBrushWithMask, 'MapBrushWithMask');
      RegisterMethod(@TKMScriptActions.MapLayerLoad, 'MapLayerLoad');
      RegisterMethod(@TKMScriptActions.MapTileFieldSet, 'MapTileFieldSet');
      RegisterMethod(@TKMScriptActions.MapTileHeightSet, 'MapTileHeightSet');
      RegisterMethod(@TKMScriptActions.MapTileObjectSet, 'MapTileObjectSet');
      RegisterMethod(@TKMScriptActions.MapTileOverlaySet, 'MapTileOverlaySet');
      RegisterMethod(@TKMScriptActions.MapSetNightTime, 'MapSetNightTime');
      RegisterMethod(@TKMScriptActions.MapTilesArraySet, 'MapTilesArraySet');
      RegisterMethod(@TKMScriptActions.MapTilesArraySetS, 'MapTilesArraySetS');
      RegisterMethod(@TKMScriptActions.MapTileSet, 'MapTileSet');
      RegisterMethod(@TKMScriptActions.MarketSetTrade, 'MarketSetTrade');
      RegisterMethod(@TKMScriptActions.MarketSetTradeEx, 'MarketSetTradeEx');
      RegisterMethod(@TKMScriptActions.OverlayTextAppend, 'OverlayTextAppend');
      RegisterMethod(@TKMScriptActions.OverlayTextAppendFormatted, 'OverlayTextAppendFormatted');
      RegisterMethod(@TKMScriptActions.OverlayTextSet, 'OverlayTextSet');
      RegisterMethod(@TKMScriptActions.OverlayTextSetFont, 'OverlayTextSetFont');
      RegisterMethod(@TKMScriptActions.OverlayTextSetFormatted, 'OverlayTextSetFormatted');
      RegisterMethod(@TKMScriptActions.OverlayTextSetWordWrap, 'OverlayTextSetWordWrap');
      RegisterMethod(@TKMScriptActions.OverlayTextSetAlignToCenter, 'OverlayTextSetAlignToCenter');
      RegisterMethod(@TKMScriptActions.OverlayTextSetAddBevel, 'OverlayTextSetAddBevel');
      RegisterMethod(@TKMScriptActions.OverlayTextSetFromBottom, 'OverlayTextSetFromBottom');
      RegisterMethod(@TKMScriptActions.OverlayTextSetMaxWidth, 'OverlayTextSetMaxWidth');

      RegisterMethod(@TKMScriptActions.PanelControlAdd, 'PanelControlAdd');
      RegisterMethod(@TKMScriptActions.PanelControlChange, 'PanelControlChange');
      RegisterMethod(@TKMScriptActions.PanelControlVisible, 'PanelControlVisible');
      RegisterMethod(@TKMScriptActions.PanelControlTexID, 'PanelControlTexID');
      RegisterMethod(@TKMScriptActions.PanelControlCaption, 'PanelControlCaption');
      RegisterMethod(@TKMScriptActions.PanelControlCaptionFormatted, 'PanelControlCaptionFormatted');
      RegisterMethod(@TKMScriptActions.PanelControlHint, 'PanelControlHint');
      RegisterMethod(@TKMScriptActions.PanelControlHintFormatted, 'PanelControlHintFormatted');
      RegisterMethod(@TKMScriptActions.PanelControlRect, 'PanelControlRect');
      RegisterMethod(@TKMScriptActions.PanelControlEnabled, 'PanelControlEnabled');
      RegisterMethod(@TKMScriptActions.PanelControlAlphaStep, 'PanelControlAlphaStep');
      RegisterMethod(@TKMScriptActions.PanelResize, 'PanelResize');
      RegisterMethod(@TKMScriptActions.PanelExpand, 'PanelExpand');
      RegisterMethod(@TKMScriptActions.CursorCustomSet, 'CursorCustomSet');
      RegisterMethod(@TKMScriptActions.MapTileSelect, 'MapTileSelect');
      RegisterMethod(@TKMScriptActions.WatchTowerRangeSet, 'WatchTowerRangeSet');
      RegisterMethod(@TKMScriptActions.WatchTowerCyclesSet, 'WatchTowerCyclesSet');

      RegisterMethod(@TKMScriptActions.Peacetime, 'Peacetime');
      RegisterMethod(@TKMScriptActions.PlanAddField, 'PlanAddField');
      RegisterMethod(@TKMScriptActions.PlanFieldAdd, 'PlanFieldAdd');
      RegisterMethod(@TKMScriptActions.PlanAddHouse, 'PlanAddHouse');
      RegisterMethod(@TKMScriptActions.PlanAddHouseEx, 'PlanAddHouseEx');
      RegisterMethod(@TKMScriptActions.PlanAddRoad, 'PlanAddRoad');
      RegisterMethod(@TKMScriptActions.PlanAddWinefield, 'PlanAddWinefield');
      RegisterMethod(@TKMScriptActions.PlanConnectRoad, 'PlanConnectRoad');
      RegisterMethod(@TKMScriptActions.PlanRemove, 'PlanRemove');
      RegisterMethod(@TKMScriptActions.PlayerAddDefaultGoals, 'PlayerAddDefaultGoals');
      RegisterMethod(@TKMScriptActions.PlayerAllowField, 'PlayerAllowField');
      RegisterMethod(@TKMScriptActions.PlayerAllowFieldEx, 'PlayerAllowFieldEx');
      RegisterMethod(@TKMScriptActions.PlayerAllianceChange, 'PlayerAllianceChange');
      RegisterMethod(@TKMScriptActions.PlayerAllianceNFogChange, 'PlayerAllianceNFogChange');
      RegisterMethod(@TKMScriptActions.PlayerDefeat, 'PlayerDefeat');
      RegisterMethod(@TKMScriptActions.PlayerGoalsRemoveAll, 'PlayerGoalsRemoveAll');
      RegisterMethod(@TKMScriptActions.PlayerHouseTypeLock, 'PlayerHouseTypeLock');
      RegisterMethod(@TKMScriptActions.PlayerShareBeacons, 'PlayerShareBeacons');
      RegisterMethod(@TKMScriptActions.PlayerShareFog, 'PlayerShareFog');
      RegisterMethod(@TKMScriptActions.PlayerShareFogCompliment, 'PlayerShareFogCompliment');
      RegisterMethod(@TKMScriptActions.PlayerTradeAllowed, 'PlayerTradeAllowed');
      RegisterMethod(@TKMScriptActions.PlayerUnitTypeCanTrain, 'PlayerUnitTypeCanTrain');
      RegisterMethod(@TKMScriptActions.PlayerWareDistribution, 'PlayerWareDistribution');
      RegisterMethod(@TKMScriptActions.PlayerWareDistributionEx, 'PlayerWareDistributionEx');
      RegisterMethod(@TKMScriptActions.PlayerWin, 'PlayerWin');
      RegisterMethod(@TKMScriptActions.PlayerUpdateEntitiesSet, 'PlayerUpdateEntitiesSet');
      RegisterMethod(@TKMScriptActions.PlayerAddWorkers, 'PlayerAddWorkers');
      RegisterMethod(@TKMScriptActions.PlayOGG, 'PlayOGG');
      RegisterMethod(@TKMScriptActions.PlayOGGAtLocation, 'PlayOGGAtLocation');
      RegisterMethod(@TKMScriptActions.PlayOGGAtLocationLooped, 'PlayOGGAtLocationLooped');
      RegisterMethod(@TKMScriptActions.PlayOGGFadeMusic, 'PlayOGGFadeMusic');
      RegisterMethod(@TKMScriptActions.PlayOGGLooped, 'PlayOGGLooped');
      RegisterMethod(@TKMScriptActions.PlaySound, 'PlaySound');
      RegisterMethod(@TKMScriptActions.PlaySoundAtLocation, 'PlaySoundAtLocation');
      RegisterMethod(@TKMScriptActions.PlayWAV, 'PlayWAV');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocation, 'PlayWAVAtLocation');
      RegisterMethod(@TKMScriptActions.PlayWAVAtLocationLooped, 'PlayWAVAtLocationLooped');
      RegisterMethod(@TKMScriptActions.PlayWAVFadeMusic, 'PlayWAVFadeMusic');
      RegisterMethod(@TKMScriptActions.PlayWAVLooped, 'PlayWAVLooped');
      RegisterMethod(@TKMScriptActions.RemoveRoad, 'RemoveRoad');
      RegisterMethod(@TKMScriptActions.SetTradeAllowed, 'SetTradeAllowed');
      RegisterMethod(@TKMScriptActions.ShowMsg, 'ShowMsg');
      RegisterMethod(@TKMScriptActions.ShowMsgFormatted, 'ShowMsgFormatted');
      RegisterMethod(@TKMScriptActions.ShowMsgGoto, 'ShowMsgGoto');
      RegisterMethod(@TKMScriptActions.ShowMsgGotoFormatted, 'ShowMsgGotoFormatted');
      RegisterMethod(@TKMScriptActions.StopLoopedOGG, 'StopLoopedOGG');
      RegisterMethod(@TKMScriptActions.StopLoopedWAV, 'StopLoopedWAV');
      RegisterMethod(@TKMScriptActions.StopSound, 'StopSound');
      RegisterMethod(@TKMScriptActions.UnitAllowAllyToSelect, 'UnitAllowAllyToSelect');
      RegisterMethod(@TKMScriptActions.UnitBlock, 'UnitBlock');
      RegisterMethod(@TKMScriptActions.UnitBootsSet, 'UnitBootsSet');
      RegisterMethod(@TKMScriptActions.UnitDirectionSet, 'UnitDirectionSet');
      RegisterMethod(@TKMScriptActions.UnitDirectionSetEx, 'UnitDirectionSetEx');
      RegisterMethod(@TKMScriptActions.UnitDismiss, 'UnitDismiss');
      RegisterMethod(@TKMScriptActions.UnitDismissableSet, 'UnitDismissableSet');
      RegisterMethod(@TKMScriptActions.UnitDismissCancel, 'UnitDismissCancel');
      RegisterMethod(@TKMScriptActions.UnitHPChange, 'UnitHPChange');
      RegisterMethod(@TKMScriptActions.UnitChangeSpec, 'UnitChangeSpec');
      RegisterMethod(@TKMScriptActions.UnitHPSetInvulnerable, 'UnitHPSetInvulnerable');
      RegisterMethod(@TKMScriptActions.UnitHungerPaceSet, 'UnitHungerPaceSet');
      RegisterMethod(@TKMScriptActions.UnitHungerSet, 'UnitHungerSet');
      RegisterMethod(@TKMScriptActions.UnitKill, 'UnitKill');
      RegisterMethod(@TKMScriptActions.UnitOrderWalk, 'UnitOrderWalk');
      RegisterMethod(@TKMScriptActions.UnitSetInstantKill, 'UnitSetInstantKill');
      RegisterMethod(@TKMScriptActions.UnitBlockWalking, 'UnitBlockWalking');

      RegisterMethod(@TKMScriptActions.GroupSetFlagColor, 'GroupSetFlagColor');
      RegisterMethod(@TKMScriptActions.HouseSetStats, 'HouseSetStats');
      RegisterMethod(@TKMScriptActions.MoveCamera, 'MoveCamera');
      RegisterMethod(@TKMScriptActions.ResetZoom, 'ResetZoom');
      RegisterMethod(@TKMScriptActions.UnitSetFlagColor, 'UnitSetFlagColor');
      RegisterMethod(@TKMScriptActions.SpecialAnimAdd, 'SpecialAnimAdd');
      RegisterMethod(@TKMScriptActions.SpecialAnimAddFront, 'SpecialAnimAddFront');
      RegisterMethod(@TKMScriptActions.UnitSetRage, 'UnitSetRage');
      RegisterMethod(@TKMScriptActions.UnitSetStats, 'UnitSetStats');
      RegisterMethod(@TKMScriptActions.UnitSetThought, 'UnitSetThought');
      RegisterMethod(@TKMScriptActions.WeatherSpawn, 'WeatherSpawn');
      RegisterMethod(@TKMScriptActions.MusicPlay, 'MusicPlay');
      RegisterMethod(@TKMScriptActions.DebugShowGrid, 'DebugShowGrid');
      //*Actions-Reg*//
    end;

    with classImp.Add(TKMScriptUtils) do
    begin
      //*Utils-Reg*//
      RegisterMethod(@TKMScriptUtils.AbsI, 'AbsI');
      RegisterMethod(@TKMScriptUtils.AbsS, 'AbsS');
      RegisterMethod(@TKMScriptUtils.ArrayElementCount, 'ArrayElementCount');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountB, 'ArrayElementCountB');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountI, 'ArrayElementCountI');
      RegisterMethod(@TKMScriptUtils.ArrayElementCountS, 'ArrayElementCountS');
      RegisterMethod(@TKMScriptUtils.ArrayHasElement, 'ArrayHasElement');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementB, 'ArrayHasElementB');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementI, 'ArrayHasElementI');
      RegisterMethod(@TKMScriptUtils.ArrayHasElementS, 'ArrayHasElementS');
      RegisterMethod(@TKMScriptUtils.ArrayRemoveIndexI, 'ArrayRemoveIndexI');
      RegisterMethod(@TKMScriptUtils.ArrayRemoveIndexS, 'ArrayRemoveIndexS');
      RegisterMethod(@TKMScriptUtils.BoolToStr, 'BoolToStr');
      RegisterMethod(@TKMScriptUtils.CeilTo, 'CeilTo');
      RegisterMethod(@TKMScriptUtils.ColorBrightness, 'ColorBrightness');
      RegisterMethod(@TKMScriptUtils.CompareString, 'CompareString');
      RegisterMethod(@TKMScriptUtils.CompareText, 'CompareText');
      RegisterMethod(@TKMScriptUtils.CopyString, 'CopyString');
      RegisterMethod(@TKMScriptUtils.DeleteString, 'DeleteString');
      RegisterMethod(@TKMScriptUtils.EnsureRangeI, 'EnsureRangeI');
      RegisterMethod(@TKMScriptUtils.EnsureRangeS, 'EnsureRangeS');
      RegisterMethod(@TKMScriptUtils.FloorTo, 'FloorTo');
      RegisterMethod(@TKMScriptUtils.Format, 'Format');
      RegisterMethod(@TKMScriptUtils.FormatFloat, 'FormatFloat');
      RegisterMethod(@TKMScriptUtils.IfThen, 'IfThen');
      RegisterMethod(@TKMScriptUtils.IfThenI, 'IfThenI');
      RegisterMethod(@TKMScriptUtils.IfThenS, 'IfThenS');
      RegisterMethod(@TKMScriptUtils.InAreaI, 'InAreaI');
      RegisterMethod(@TKMScriptUtils.InAreaS, 'InAreaS');
      RegisterMethod(@TKMScriptUtils.InRangeI, 'InRangeI');
      RegisterMethod(@TKMScriptUtils.InRangeS, 'InRangeS');
      RegisterMethod(@TKMScriptUtils.InsertString, 'InsertString');
      RegisterMethod(@TKMScriptUtils.KMPoint, 'KMPoint');
      RegisterMethod(@TKMScriptUtils.LowerCase, 'LowerCase');
      RegisterMethod(@TKMScriptUtils.MaxI, 'MaxI');
      RegisterMethod(@TKMScriptUtils.MaxInArrayI, 'MaxInArrayI');
      RegisterMethod(@TKMScriptUtils.MaxInArrayS, 'MaxInArrayS');
      RegisterMethod(@TKMScriptUtils.MaxS, 'MaxS');
      RegisterMethod(@TKMScriptUtils.MinI, 'MinI');
      RegisterMethod(@TKMScriptUtils.MinInArrayI, 'MinInArrayI');
      RegisterMethod(@TKMScriptUtils.MinInArrayS, 'MinInArrayS');
      RegisterMethod(@TKMScriptUtils.MinS, 'MinS');
      RegisterMethod(@TKMScriptUtils.MoveString, 'MoveString');
      RegisterMethod(@TKMScriptUtils.Pos, 'Pos');
      RegisterMethod(@TKMScriptUtils.Power, 'Power');
      RegisterMethod(@TKMScriptUtils.RandomRangeI, 'RandomRangeI');
      RegisterMethod(@TKMScriptUtils.RGBDecToBGRHex, 'RGBDecToBGRHex');
      RegisterMethod(@TKMScriptUtils.RGBToBGRHex, 'RGBToBGRHex');
      RegisterMethod(@TKMScriptUtils.RoundTo, 'RoundTo');
      RegisterMethod(@TKMScriptUtils.Sqr, 'Sqr');
      RegisterMethod(@TKMScriptUtils.StringReplace, 'StringReplace');
      RegisterMethod(@TKMScriptUtils.SumI, 'SumI');
      RegisterMethod(@TKMScriptUtils.SumS, 'SumS');
      RegisterMethod(@TKMScriptUtils.TimeToString, 'TimeToString');
      RegisterMethod(@TKMScriptUtils.TimeToTick, 'TimeToTick');
      RegisterMethod(@TKMScriptUtils.Trim, 'Trim');
      RegisterMethod(@TKMScriptUtils.TrimLeft, 'TrimLeft');
      RegisterMethod(@TKMScriptUtils.TrimRight, 'TrimRight');
      RegisterMethod(@TKMScriptUtils.TruncTo, 'TruncTo');
      RegisterMethod(@TKMScriptUtils.UpperCase, 'UpperCase');
      //*Utils-Reg*//
    end;

    //Append classes info to Exec
    RegisterClassLibraryRuntime(fExec, classImp);

    if not fExec.LoadData(fByteCode) then // Load the data from the Data string.
    begin
      { For some reason the script could not be loaded. This is usually the case when a
        library that has been used at compile time isn't registered at runtime. }
      fErrorHandler.AppendErrorStr('Unknown error in loading bytecode to Exec|');
      fValidationIssues.AddError(0, 0, '', 'Unknown error in loading bytecode to Exec');
      Exit;
    end;

    if (fExec is TPSDebugExec) and TPSDebugExec(fExec).DebugEnabled then
      TPSDebugExec(fExec).LoadDebugData(fDebugByteCode);

    //Check global variables in script to be only of supported type
    for I := 0 to fExec.GetVarCount - 1 do
    begin
      V := fExec.GetVarNo(I);
      //Promote to Unicode just to make compiler happy
      if SameText(UnicodeString(V.FType.ExportName), 'TKMScriptStates')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptActions')
      or SameText(UnicodeString(V.FType.ExportName), 'TKMScriptUtils') then
        Continue;

      errStr := ValidateVarType(V.FType);
      fErrorHandler.AppendErrorStr(errStr);
      if errStr <> '' then
        fValidationIssues.AddError(0, 0, '', errStr);
      if fErrorHandler.HasErrors then
      begin
        //Don't allow the script to run
        fExec.Clear;
        Exit;
      end;
    end;

    //Link script objects with objects
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('STATES')), fStates);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('ACTIONS')), fActions);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('UTILS')), fUtils);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('S')), fStates);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('A')), fActions);
    SetVariantToClass(fExec.GetVarNo(fExec.GetVar('U')), fUtils);
  finally
    classImp.Free;
  end;

  //Link events into the script
  gScriptEvents.LinkEventsAndCommands;
end;


procedure TKMScripting.ExportDataToText;
var
  s: string;
  SL: TStringList;
begin
  if Self = nil then Exit;

  SL := TStringList.Create;
  try
    IFPS3DataToText(fByteCode, s);
    SL.Text := s;
    ForceDirectories(ExeDir  + 'Export' + PathDelim);
    SL.SaveToFile(ExeDir + 'Export' + PathDelim + 'script_DataText.txt');
  finally
    SL.Free;
  end;
end;


procedure TKMScripting.ExportScriptCode;
var
  SL: TStringList;
begin
  if Self = nil then Exit;

  SL := TStringList.Create;
  try
    SL.Text := fScriptCode;
    ForceDirectories(ExeDir  + 'Export' + PathDelim);
    SL.SaveToFile(ExeDir + 'Export' + PathDelim + 'script_code.script');
  finally
    SL.Free;
  end;
end;


procedure TKMScripting.LoadVar(LoadStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
var
  I: Integer;
  elemCount: Integer;
  offset: Cardinal;
begin
  //See uPSRuntime line 1630 for algo idea
  case aType.BaseType of
    btU8:            LoadStream.Read(tbtu8(Src^)); //Byte, Boolean
    btS8:            LoadStream.Read(tbts8(Src^)); //ShortInt
    btU16:           LoadStream.Read(tbtu16(Src^)); //Word
    btS16:           LoadStream.Read(tbts16(Src^)); //SmallInt
    btU32:           LoadStream.Read(tbtu32(Src^)); //Cardinal / LongInt
    btS32:           LoadStream.Read(tbts32(Src^)); //Integer
    btSingle:        LoadStream.Read(tbtsingle(Src^));
    btString:        LoadStream.ReadA(tbtString(Src^));
    btUnicodeString: LoadStream.ReadW(tbtUnicodeString(Src^));
    btStaticArray:begin
                    LoadStream.Read(elemCount);
                    Assert(elemCount = TPSTypeRec_StaticArray(aType).Size, 'Script array element count mismatches saved count');
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      LoadVar(LoadStream, Pointer(IPointer(Src) + offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btArray:      begin
                    LoadStream.Read(elemCount);
                    PSDynArraySetLength(Pointer(Src^), aType, elemCount);
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      LoadVar(LoadStream, Pointer(IPointer(Src^) + offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btRecord:     begin
                    LoadStream.Read(elemCount);
                    Assert(elemCount = TPSTypeRec_Record(aType).FieldTypes.Count, 'Script record element count mismatches saved count');
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[I]);
                      LoadVar(LoadStream, Pointer(IPointer(Src) + offset), TPSTypeRec_Record(aType).FieldTypes[I]);
                    end;
                  end;
    btSet:        begin
                    LoadStream.Read(elemCount);
                    Assert(elemCount = TPSTypeRec_Set(aType).RealSize, 'Script set element count mismatches saved count');
                    LoadStream.Read(Src^, elemCount);
                  end;
    //Already checked and reported as an error in LinkRuntime, no need to crash it here
    //else Assert(False);
  end;
end;


procedure TKMScripting.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  RecreateValidationIssues;

  LoadStream.CheckMarker('Script');
  LoadStream.ReadHugeString(fScriptCode);
  LoadStream.ReadA(fCampaignDataTypeScriptCode);
  gScriptEvents.Load(LoadStream);
  fIDCache.Load(LoadStream);

  // Do not compile script code, if not needed, same as we do in LoadFromFile procedure
  if IsScriptCodeNeedToCompile then
    CompileScript;

  LoadStream.CheckMarker('ScriptVars');
  //Read script variables
  LoadStream.Read(I);
  Assert(I = fExec.GetVarCount, 'Script variable count mismatches saved variables count');
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    LoadVar(LoadStream, @PPSVariantData(V).Data, V.FType);
  end;

  //The log path can't be stored in the save since it might be in MapsMP or MapsDL on different clients
  fErrorHandler.ScriptLogFile := ExeDir + ChangeFileExt(gGameParams.MissionFileRel, SCRIPT_LOG_EXT);
end;


procedure TKMScripting.SyncLoad;
begin
  fIDCache.SyncLoad;
end;


function TKMScripting.GetScriptFilesInfo: TKMScriptFilesCollection;
begin
  Result := fPreProcessor.ScriptFilesInfo;
end;


procedure TKMScripting.LoadCampaignData(LoadStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
  S: AnsiString;
  size: Cardinal;
  hash: Integer;
  errStr: String;
begin
  if LoadStream = nil then Exit; // silently Exit in case this is not a campaign script
  if LoadStream.Position = LoadStream.Size then Exit; // LoadStream is empty

  LoadStream.ReadA(S);
  //Campaign data format might change. If so, do not load it
  if S <> fCampaignDataTypeScriptCode then
    Exit;

  if LoadStream.Position = LoadStream.Size then Exit; // LoadStream is empty

  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    if FastUppercase(V.FType.ExportName) = FastUppercase(CAMPAIGN_DATA_TYPE) then
    begin
      LoadStream.Read(hash);
      LoadStream.Read(size);
      if (hash <> MakePSTypeHash(V.FType)) or (size <> V.FType.RealSize) then
      begin
        errStr := Format('var %s: %s declaration differs from previously saved declaration. Update it and rerun the mission',
                         [CAMPAIGN_DATA_VAR, CAMPAIGN_DATA_TYPE]);
        fErrorHandler.AppendErrorStr(errStr);
        fValidationIssues.AddError(0, 0, '', errStr);
        Exit;
      end;

      LoadVar(LoadStream, @PPSVariantData(V).Data, V.FType);
      Exit;
    end;
  end;
end;


procedure TKMScripting.SaveVar(SaveStream: TKMemoryStream; Src: Pointer; aType: TPSTypeRec);
var
  I: Integer;
  elemCount: Integer;
  offset: Cardinal;
begin
  //See uPSRuntime line 1630 for algo idea
  case aType.BaseType of
    btU8:            SaveStream.Write(tbtu8(Src^)); //Byte, Boolean
    btS8:            SaveStream.Write(tbts8(Src^)); //ShortInt
    btU16:           SaveStream.Write(tbtu16(Src^)); //Word
    btS16:           SaveStream.Write(tbts16(Src^)); //SmallInt
    btU32:           SaveStream.Write(tbtu32(Src^)); //Cardinal / LongInt
    btS32:           SaveStream.Write(tbts32(Src^)); //Integer
    btSingle:        SaveStream.Write(tbtsingle(Src^));
    btString:        SaveStream.WriteA(tbtString(Src^));
    btUnicodeString: SaveStream.WriteW(tbtUnicodeString(Src^));
    btStaticArray:begin
                    elemCount := TPSTypeRec_StaticArray(aType).Size;
                    SaveStream.Write(elemCount);
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      SaveVar(SaveStream, Pointer(IPointer(Src) + offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btArray:      begin
                    elemCount := PSDynArrayGetLength(Pointer(Src^), aType);
                    SaveStream.Write(elemCount);
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := TPSTypeRec_Array(aType).ArrayType.RealSize * I;
                      SaveVar(SaveStream, Pointer(IPointer(Src^) + offset), TPSTypeRec_Array(aType).ArrayType);
                    end;
                  end;
    btRecord:     begin
                    elemCount := TPSTypeRec_Record(aType).FieldTypes.Count;
                    SaveStream.Write(elemCount);
                    for I := 0 to elemCount - 1 do
                    begin
                      offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[I]);
                      SaveVar(SaveStream, Pointer(IPointer(Src) + offset), TPSTypeRec_Record(aType).FieldTypes[I]);
                    end;
                  end;
    btSet:        begin
                    elemCount := TPSTypeRec_Set(aType).RealSize;
                    SaveStream.Write(elemCount);
                    SaveStream.Write(Src^, elemCount);
                  end;
    //Already checked and reported as an error in LinkRuntime, no need to crash it here
    //else Assert(False);
  end;
end;


procedure TKMScripting.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  SaveStream.PlaceMarker('Script');

  //Write script code
  SaveStream.WriteHugeString(fScriptCode);
  SaveStream.WriteA(fCampaignDataTypeScriptCode);
  gScriptEvents.Save(SaveStream);
  fIDCache.Save(SaveStream);

  SaveStream.PlaceMarker('ScriptVars');
  //Write script global variables
  SaveStream.Write(fExec.GetVarCount);
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    SaveVar(SaveStream, @PPSVariantData(V).Data, V.FType);
  end;
end;


function TKMScripting.MakePSTypeHash(aType: TPSTypeRec): Integer;

  function GetHashStr(aPSType: TPSTypeRec): string;
  const
    SEP = '|';
  var
    I: Integer;
  begin
    if aPSType = nil then Exit('nil');

    Result := IntToStr(aPSType.BaseType);

    //Check elements of arrays/records are valid too
    case aPSType.BaseType of
      btArray,
      btStaticArray:
        Result := Result + SEP + GetHashStr(TPSTypeRec_Array(aPSType).ArrayType);
      btRecord:
        begin
          for I := 0 to TPSTypeRec_Record(aPSType).FieldTypes.Count - 1 do
            Result := Result + SEP + GetHashStr(TPSTypeRec_Record(aPSType).FieldTypes[I]);
        end;
    end;
  end;

var
  s: string;
begin
  if aType = nil then Exit(0);

  s := GetHashStr(aType);

  Result := THashBobJenkins.GetHashValue(s);
end;


procedure TKMScripting.SaveCampaignData(SaveStream: TKMemoryStream);
var
  I: Integer;
  V: PIFVariant;
begin
  SaveStream.WriteA(fCampaignDataTypeScriptCode);
  for I := 0 to fExec.GetVarCount - 1 do
  begin
    V := fExec.GetVarNo(I);
    if FastUppercase(V.FType.ExportName) = FastUppercase(CAMPAIGN_DATA_TYPE) then
    begin
      SaveStream.Write(MakePSTypeHash(V.FType));
      SaveStream.Write(V.FType.RealSize);
      SaveVar(SaveStream, @PPSVariantData(V).Data, V.FType);
      Exit;
    end;
  end;
end;


procedure TKMScripting.UpdateState;
begin
  gScriptEvents.ProcTick;
  fIDCache.UpdateState;
end;


//function TKMScripting.GetCodeLine(aRowNum: Cardinal): AnsiString;
//var
//  strings: TStringList;
//begin
//  strings := TStringList.Create;
//  strings.Text := fScriptCode;
//  Result := AnsiString(strings[aRowNum - 1]);
//  strings.Free;
//end;


//function TKMScripting.FindCodeLine(aRowNumber: Integer; out aFileNamesArr: TKMStringArray; out aRowsArr: TIntegerArray): Integer;
//begin
//  Result := fPreProcessor.fScriptFilesInfo.FindCodeLine(GetCodeLine(aRowNumber), aFileNamesArr, aRowsArr);
//end;


function TKMScripting.GetErrorMessage(aErrorMsg: TPSPascalCompilerMessage): TKMScriptErrorMessage;
begin
  Result := GetErrorMessage(aErrorMsg.ErrorType, EolW + '[' + aErrorMsg.ErrorType + '] ' + aErrorMsg.ShortMessageToString,
                            aErrorMsg.ModuleName, aErrorMsg.Row, aErrorMsg.Col, aErrorMsg.Pos);
end;


function TKMScripting.GetErrorMessage(const aErrorType, aShortErrorDescription, aModule: String; aRow, aCol, aPos: Integer): TKMScriptErrorMessage;
var
  errorMsg: UnicodeString;
begin
  errorMsg := Format(aShortErrorDescription + ' in ''%s'' at [%d:%d]', [aModule, aRow, aCol]);

  // Show game message only for errors. Do not show it for hints or warnings.
  if aErrorType = 'Error' then
    Result.GameMessage := errorMsg
  else
    Result.GameMessage := '';

   Result.LogMessage := errorMsg;
end;


end.
