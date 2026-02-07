unit KM_ResMapElements;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KromUtils,
  KM_ResTypes,
  KM_CommonTypes, KM_Defaults,
  KM_TerrainTypes;


type
  TKMKillByRoad = (kbrNever, kbrNWCorner, kbrWest, kbrAlways);

  TKMMapElement = packed record
    Anim: TKMAnimLoop;          //Animation loop info
    CuttableTree: LongBool;     //This tree can be cut by a woodcutter
    DiagonalBlocked: LongBool;  //Can't walk diagonally accross this object (mainly trees)
    AllBlocked: LongBool;       //All passibility blocked. Can't walk, swim, etc. EXCEPT BUILD
    WineOrCorn: LongBool;       //Draw multiple (4 or 2) sprites per object (corn or grapes)
    CanGrow: LongBool;          //This object can grow (i.e. change to another object)
    DontPlantNear: LongBool;    //This object can't be planted within one tile of
    Stump: ShortInt;            //Tree stump ID
    CanBeRemoved: LongBool;     //Can be removed in favor of building house (actually means we can build over this object)
    KillByRoad: TKMKillByRoad;  //Object will be removed if these neighboring tiles are roads
    SnowPic : Word;
    RandomPos,
    RenderAsTileOverlay, RotateToTile : Boolean;
    //Clay, Iron, Gold, Bitin, Coal, Stone : Word;

    Ware : TKMWareType;
    WareCount : Byte;

    VWareChance : Byte;
    VWares : array of record
      W : String;//Name
      Cmin, Cmax : Byte; //ammount of wares
      Ch : Byte; //chanses to get any of it
    end;
    AxeHitTimes : Byte;
    TrunksCount: Byte;
    LandStump: Word;
    IsFruit,
    IsGrass,
    IsVege,
    IsCorn,
    IsWine : Byte;
    RenderAsTwo : Boolean;
    TreeGrowAge : Byte;
    PrevTreeAgeObj,
    NextTreeAgeObj,
    FallTreeAnimObj : Word;

    LightRadius, LightPower : Byte;
    ObjectPrice : Single;
  end;

  TKMResMapElements = class
  private
    fCount: Integer;
    fCRC: Cardinal;
    procedure DuplicateFromPrevObj(aAnimSteps :array of TKMWordArray);
  public
    HighestPrice : Single;
    property Count: Integer read fCount;
    property CRC: Cardinal read fCRC;

    procedure LoadFromFile;
    function LoadFromJSON(aPath : String) : Cardinal;
    procedure SaveToJson(aPath : String);
    procedure SaveToFile(const aFileName: string);
    procedure ExportToText(const aFileName: string);
    Procedure ReloadJSONData(UpdateCRC: Boolean);
    procedure AfterResourceLoad;
  end;

  TKMGrainDat = packed record
    GuiIcon : Word;
    TextID : Word;
    Stage : array of record
              Obj, Terr : Word;// object and Terrain for each stage
              NextStage,
              Age: Byte;//field age
              GivesWares,
              CanBeCut,
              CanBePlant : Boolean;
            end;
    Dead : record
          Obj, Terr : Word;// object and Terrain for each stage
          end;
    Wine,
    Straw,
    Seeds,
    Hay,
    Vege: Single;
    function Valid : Boolean;
    function StagesCount : Byte;
    function GetStage(aAge : Byte) : Byte;
  end;

  TKMFruitTree = record
    Fruits : Single;
    ProgressPerStage : Word;
    MatureTreeStage : Byte;
    Stage : array of Word;
    HintID,
    GuiIcon : Word;
    ClimateMulti: array[TKMTerrainClimate] of Single;
    function StagesCount : Byte;
    function GetStage(aObj : Word) : Byte;
  end;
  TKMDecorationType = (dtObject, dtTile, dtTileOverlay);

  TKMDecoration = record
    ID, GuiIcon, TextID : Word;
    Cost : TKMVWarePlanCommon;
    DType : TKMDecorationType;
  end;

  TKMDecorationArray = array of TKMDecoration;

  TKMDecorationArrayHelper = record helper for TKMDecorationArray
    procedure Add(aType : TKMDecorationType; aID, aGuiIcon, aTextID : Word; aCost : TKMVWarePlanCommon);
  end;

  TKMGrowingTree = record
    ObjID : Word;
    Size : Single;
    GuiIcon : Word;
  end;

  function ObjectIsChoppableTree(aObjId: Integer): Boolean; overload;
  function ObjectIsChoppableTree(aObjId: Integer; aStage: TKMChopableAge): Boolean; overload;
  function ObjectIsChoppableTree(aObjId: Integer; aStages: TKMChopableAgeSet): Boolean; overload;

  function ObjectIsCorn(aObjId: Integer): Boolean;
  function ObjectIsGrass(aObjId: Integer): Boolean;
  function ObjectIsVege(aObjId: Integer): Boolean;
  function ObjectIsWine(aObjId: Integer): Boolean;
  function ObjectIsWare(aObjId: Integer): Boolean;
  function ObjectGetWare(aObjId: Integer): TKMWareType;

var
  //MapElem is in global access because of the recursive FloodFill algorithm
  //when it uses TKMResMapElements.MapElem each call takes 8 times more memory
  //on the stack (View>Debug>CPU>Stack) for reasons unknown to me.
  //OBJECTS_CNT = 284;
  OBJECTS_CNT: Integer;
  gMapElements: array of TKMMapElement;
  gFieldGrains: array[TKMGrainType] of TKMGrainDat;
  gFruitTrees : array of TKMFruitTree;
  gTreeTypeID : array[TKMTerrainClimate] of TIntegerArray;
  gDecorations : TKMDecorationArray;
  gGrowingTrees : array of TKMGrowingTree;

const
  //Chopable tree, Chopdown animation,
  //Age1, Age2, Age3, Age4, Falling, Stump
  CHOPABLE_TREES: array [1..14, TKMChopableAge] of Word = (
  //For grass
  (  88,  89,  90,  90,  91,  37), //These two are very look alike
  (  97,  98,  99, 100, 101,  41), //yet different in small detail and fall direction
  ( 102, 103, 104, 105, 106,  45),
  ( 107, 108, 109, 110, 111,  41),
  ( 112, 113, 114, 114, 115,  25), //These two are very look alike
  ( 116, 117, 118, 119, 120,  25), //yet different in small detail and fall direction
  //For grass and yellow
  (  92,  93,  94,  95,  96,  49),
  //For yellow soil only
  ( 121, 122, 123, 124, 125,  64),
  //For dirt (pine trees)
  ( 149, 150, 151, 151, 152,  29),
  ( 153, 154, 155, 155, 156,  29),
  ( 157, 158, 159, 160, 161,  33),
  ( 162, 163, 164, 165, 166,  33),
  ( 167, 168, 169, 170, 171,  33),
  ( 201, 168, 169, 170, 171,  33)
  );

  STUMPS: array[0..32] of Word = (14, 15, 16, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                            36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                            50, 51, 64, 65, 66);
  //Ages at which trees/fields grow up/change sprite multiplied by TERRAIN_PACE
  TREE_AGE_1 = 2400 div TERRAIN_PACE;
  TREE_AGE_2 = 5000 div TERRAIN_PACE;
  TREE_AGE_FULL = 8000 div TERRAIN_PACE; //Tree is old enough to be chopped
  TREE_AGE_SAPLING = 800 div TERRAIN_PACE;

  CORN_STAGES_COUNT = 7; //0..6
  GRASS_STAGES_COUNT = 8; //0..7
  //0 = empty field, 1 = sown corn, 2 = young seedings, 3 = seedings,
  //4 = greenish corn , 5 = ready to be cut, 6 = corn has been cut

  CORN_AGE_1 = 1400 div TERRAIN_PACE;    //Measured from KaM ~150sec
  CORN_AGE_2 = 2200 div TERRAIN_PACE;   //Number measured from KaM ~195sec
  CORN_AGE_3 = 4400 div TERRAIN_PACE;
  CORN_AGE_FULL = 6400 div TERRAIN_PACE; //Corn ready to be cut
  CORN_AGE_MAX  = 128; //todo: Remove. We set it to this once it's fully grown
  CORN_AGE_DEAD = 148; //todo: Remove. We set it to this once it's fully grown

  //Wine values have been tweaked for balance. In KaM they matched corn.
  WINE_STAGES_COUNT = 4; //0..3
  //0 = new fruits, 1 = starts to grow, 2 = continues to grow, 3 = ready to be harvested

  WINE_AGE_1 = 1600 div TERRAIN_PACE;
  WINE_AGE_2 = 3400 div TERRAIN_PACE;
  WINE_AGE_FULL = 5000 div TERRAIN_PACE; //Wine ready to be harvested

implementation
uses JsonDataObjects, KM_CommonUtils, KM_CommonClassesExt, Math, KM_JSONUtils,
  TypInfo, KM_Resource,
  KM_JsonHelpers;
const
  // We use Byte instead of TKMKillByRoad to have a shorter table
  OBJ_KILL_BY_ROAD: array [Byte] of Byte {TKMKillByRoad} = (
    1, 2, 2, 2, 2, 2, 1, 1, 0, 0, 2, 2, 2, 1, 1, 1,
    1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 2, 2, 0, 0, 1, 0, 1, 1, 1, 1, 0, 2, 2, 2,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

{ TKMResMapElements }
procedure TKMResMapElements.DuplicateFromPrevObj(aAnimSteps: array of TKMWordArray);
var firstID : Integer;
  id, I : Integer;
begin
  firstID := high(gMapElements);

  for I := 0 to High(aAnimSteps) do
  begin
    id := length(gMapElements);
    OBJECTS_CNT := id;
    fCount := OBJECTS_CNT + 1; //add new trees
    SetLength(gMapElements, id + 1);
    gMapElements[id] := gMapElements[firstID];
    gMapElements[id].Anim.Create(0, 0, aAnimSteps[I]);
  end;


end;

// Reading map elements properties and animation data
procedure TKMResMapElements.LoadFromFile;
begin
  OBJECTS_CNT := 648;
  fCount := OBJECTS_CNT + 1;
  SetLength(gMapElements, OBJECTS_CNT + 1);
  SetLength(gFruitTrees, 0);
  SetLength(gDecorations, 0);
  fCRC := LoadFromJSON(ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'Objects.json');

end;



procedure TKMResMapElements.SaveToFile(const aFileName: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  S.Write(gMapElements[0], fCount * SizeOf(TKMMapElement));
  S.SaveToFile(aFileName);
  FreeAndNil(S);
end;

function TKMResMapElements.LoadFromJSON(aPath : String) : Cardinal;
var I, K, J, aID, MatureTreeAge : Integer;
  jsonPath: string;
  nObjects, nObject : TJSONObject;
  nArr, nArr2, nArr3 : TJSONArray;
  arr : TIntegerArray;
  tmpGrain : TKMGrainDat;
  tmpElement : TKMMapElement;
  GT : TKMGrainType;
  TT : TKMTerrainClimate;
  S : String;
  tmpCost : TKMVWarePlanCommon;
  DT : TKMDecorationType;
  animSteps :array of TKMWordArray;
  inEditor : Boolean;
begin
  jsonPath :=  aPath;

  if not FileExists(jsonPath) then
    Exit;
  nObjects := TJsonObject.ParseFromFile(jsonPath) as TJsonObject;
  Result := GetJSONCRC(nObjects);

  nArr := nObjects.A['Objects'];
  for I := 0 to nArr.Count - 1 do
  begin
    nObject := nArr.O[I];
    if nObject.Contains('DuplicateFromLast') then
    begin
      nArr2 := nObject.A['DuplicateFromLast'];
      SetLength(animSteps, nArr2.Count);
      for K := 0 to nArr2.Count - 1 do
      begin
        nArr3 := nArr2.A[K];
        JSONArrToValidArr(nArr3, animSteps[K]);
        for J := 0 to High(animSteps[K]) do
          animSteps[K][J] := animSteps[K][J] - 1;
      end;

      DuplicateFromPrevObj(animSteps);

    end else
    if nObject.B['AddGrowableTree'] then
    begin
      tmpElement.LandStump := nObject.I['Stump'];
      tmpElement.AxeHitTimes := nObject.I['AxeHitTimes'];
      tmpElement.TrunksCount := nObject.I['TrunksCount'];
      tmpElement.FallTreeAnimObj := nObject.I['FallTreeAnimObj'];
      tmpElement.LightRadius := nObject.I['LightRadius'];
      tmpElement.LightPower := nObject.I['LightPower'];
      tmpElement.IsFruit := nObject.I['IsFruit'];
      MatureTreeAge := nObject.I['MatureTree'];
      tmpElement.TreeGrowAge := nObject.I['TreeGrowAge'];
      nArr2 := nObject.A['Stages'];
      inEditor := not nobject.B['NotPlaceableInEditor'];
      for K := 0 to nArr2.Count - 1 do
      begin
        aID := length(gMapElements);
        OBJECTS_CNT := aID;
        fCount := OBJECTS_CNT + 1; //add new trees
        SetLength(gMapElements, aID + 1);


        SetLength(arr, 0);
        nArr3 := nArr2.O[K].A['AnimSteps'];
        SetLength(arr, nArr3.Count);
        for J := 0 to nArr3.Count - 1 do
          arr[J] := nArr3.I[J] - 1;
        tmpElement.SnowPic := nArr2.O[K].I['SnowPic'];

        with gMapElements[aID] do
        begin
          Anim.Create(arr, 2);
          AxeHitTimes := tmpElement.AxeHitTimes;
          TrunksCount := IfThen(K>=MatureTreeAge, tmpElement.TrunksCount, 0);
          FallTreeAnimObj := IfThen(K>=MatureTreeAge, tmpElement.FallTreeAnimObj, 0);;
          IsFruit := tmpElement.IsFruit;
          TreeGrowAge := tmpElement.TreeGrowAge * (K + 1);
          SnowPic := tmpElement.SnowPic;

          Stump := IfThen(inEditor, -1, 12);
          CuttableTree := K >= MatureTreeAge;
          DiagonalBlocked := true;
          DiagonalBlocked := true;
          AllBlocked := false;
          WineOrCorn := false;
          CanGrow := true;
          DontPlantNear := true;
          RotateToTile := false;
          RenderAsTileOverlay := false;
          RandomPos := false;
          CanBeRemoved := false;
          LandStump := IfThen(K>=MatureTreeAge, tmpElement.LandStump, 0);
          IsCorn := 0;
          IsWine := 0;
          IsGrass := 0;
          NextTreeAgeObj := IfThen(K <> nArr2.Count - 1, aID + 1, 0);
        end;
      end;


    end;

    if nObject.B['AddObject'] then
    begin
      aID := length(gMapElements);
      OBJECTS_CNT := aID;
      fCount := OBJECTS_CNT + 1;
      SetLength(gMapElements, aID + 1);

      with gMapElements[aID] do
      begin
        PrevTreeAgeObj := 0;
        nArr2 := nObject.A['AnimSteps'];
        SetLength(arr, nArr2.Count);
        for K := 0 to nArr2.Count - 1 do
          arr[K] := nArr2.I[K] - 1;

        Anim.Create(arr, nObject.I['SlowAnim']);
        CuttableTree := nObject.B['IsCuttableTree'];
        DiagonalBlocked := nObject.B['DiagonalBlocked'];
        AllBlocked := nObject.B['AllBlocked'];
        WineOrCorn := nObject.B['WineOrCorn'];
        CanGrow := nObject.B['CanGrow'];
        DontPlantNear := nObject.B['DontPlantNear'];
        RotateToTile := nObject.B['RotateToTile'];
        RenderAsTileOverlay := nObject.B['RenderAsTileOverlay'];
        RandomPos := nObject.B['RandomPos'];
        CanBeRemoved := nObject.B['CanBeRemoved'];
        AxeHitTimes := nObject.I['AxeHitTimes'];
        TrunksCount := nObject.I['TrunksCount'];
        LandStump := nObject.I['Stump'];
        IsCorn := nObject.I['IsCorn'];
        IsVege := nObject.I['IsVege'];
        IsWine := nObject.I['IsWine'];
        IsGrass := nObject.I['IsGrass'];
        IsFruit := nObject.I['IsFruit'];
        TreeGrowAge := nObject.I['TreeGrowAge'];
        FallTreeAnimObj := nObject.I['FallTreeAnimObj'];
        NextTreeAgeObj := nObject.I['NextTreeAgeObj'];
        LightRadius := nObject.I['LightRadius'];
        LightPower := nObject.I['LightPower'];

        RenderAsTwo := nObject.B['RenderAsTwo'];
        if nObject.B['PlaceableInEditor'] then
          Stump := -1
        else
          Stump := 12;

        SnowPic := 255;
        ChangeIfDifferent(SnowPic, nObject.I['SnowPic'], 0);

        RenderAsTileOverlay := nObject.B['RenderAsTileOverlay'];
        RotateToTile := nObject.B['RotateToTile'];
        Ware := wtNone;
        WareCount := 0;
        If nObject.I['Clay'] > 0 then begin Ware := wtTile; WareCount := nObject.I['Clay'] end else
        If nObject.I['Stone'] > 0 then begin Ware := wtStone; WareCount := nObject.I['Stone'] end else
        If nObject.I['Bitin'] > 0 then begin Ware := wtBitinOre; WareCount := nObject.I['Bitin'] end else
        If nObject.I['Iron'] > 0 then begin Ware := wtIronOre; WareCount := nObject.I['Iron'] end else
        If nObject.I['Gold'] > 0 then begin Ware := wtGoldOre; WareCount := nObject.I['Gold'] end else
        If nObject.I['Coal'] > 0 then begin Ware := wtCoal; WareCount := nObject.I['Coal'] end;
        If nObject.Contains('Ware') then
          If TKMEnumUtils.TryGetAs<TKMWareType>(nObject.S['Ware'], Ware) then
          begin
            WareCount := nObject.I['WareCount'];
          end;

        VWareChance := nObject.I['MainGettingChance'];
        nArr2 := nObject.A['VirtualWares'];
        if nArr2.Count > 0 then
        begin
          SetLength(VWares, nArr2.Count);
          for K := 0 to nArr2.Count - 1 do
          begin
            VWares[K].W := nArr2.O[K].S['Type'];
            VWares[K].Cmin := nArr2.O[K].I['Min'];
            VWares[K].Cmax := nArr2.O[K].I['Max'];
            VWares[K].Ch := nArr2.O[K].I['Chance'];
          end;
        end;
        if (IsCorn > 0) or (IsGrass > 0) or (IsWine > 0) or (IsVege > 0) then
          KillByRoad := kbrAlways
        else
        if (FallTreeAnimObj > 0)
        or (NextTreeAgeObj > 0)
        or (LandStump > 0)
        or CuttableTree
        or (TrunksCount > 0)
        or (TreeGrowAge > 0) then
          KillByRoad := kbrNever
        else
          KillByRoad := kbrWest;
      end;


    end else
    begin
      aID := -1;
      ChangeIfDifferent(aID, nObject.I['ObjectID'], -1);

      If aID >= OBJECTS_CNT then
      begin
        OBJECTS_CNT := aID;
        fCount := OBJECTS_CNT + 1;
        SetLength(gMapElements, fCount);
      end;

      if (aID = -1){ or (aID > OBJECTS_CNT) }then
        Continue;
      with gMapElements[aID] do
      begin
        //only override things if they exists
        If nObject.Contains('AnimSteps') then
        begin
          nArr2 := nObject.A['AnimSteps'];
          SetLength(arr, nArr2.Count);
          for K := 0 to nArr2.Count - 1 do
            arr[K] := nArr2.I[K];
          Anim.Create(arr, nObject.I['SlowAnim']);
        end;
        PrevTreeAgeObj := 0;

        Ware := wtNone;
        WareCount := 0;
        If nObject.I['Clay'] > 0 then begin Ware := wtTile; WareCount := nObject.I['Clay'] end else
        If nObject.I['Stone'] > 0 then begin Ware := wtStone; WareCount := nObject.I['Stone'] end else
        If nObject.I['Bitin'] > 0 then begin Ware := wtBitinOre; WareCount := nObject.I['Bitin'] end else
        If nObject.I['Iron'] > 0 then begin Ware := wtIronOre; WareCount := nObject.I['Iron'] end else
        If nObject.I['Gold'] > 0 then begin Ware := wtGoldOre; WareCount := nObject.I['Gold'] end else
        If nObject.I['Coal'] > 0 then begin Ware := wtCoal; WareCount := nObject.I['Coal'] end;
        If nObject.Contains('Ware') then
          If TKMEnumUtils.TryGetAs<TKMWareType>(nObject.S['Ware'], Ware) then
          begin
            WareCount := nObject.I['WareCount'];
          end;

        If nObject.Contains('CanBeRemoved') then CanBeRemoved := nObject.B['CanBeRemoved'];
        If nObject.Contains('IsCuttableTree') then CuttableTree := nObject.B['IsCuttableTree'];
        If nObject.Contains('DiagonalBlocked') then DiagonalBlocked := nObject.B['DiagonalBlocked'];
        If nObject.Contains('AllBlocked') then AllBlocked := nObject.B['AllBlocked'];
        If nObject.Contains('WineOrCorn') then WineOrCorn := nObject.B['WineOrCorn'];
        If nObject.Contains('CanGrow') then CanGrow := nObject.B['CanGrow'];
        If nObject.Contains('DontPlantNear') then DontPlantNear := nObject.B['DontPlantNear'];
        If nObject.Contains('RotateToTile') then RotateToTile := nObject.B['RotateToTile'];
        If nObject.Contains('RenderAsTileOverlay') then RenderAsTileOverlay := nObject.B['RenderAsTileOverlay'];
        if nObject.Contains('RandomPos') then   RandomPos := nObject.B['RandomPos'];
        if nObject.Contains('RenderAsTwo') then   RenderAsTwo := nObject.B['RenderAsTwo'];

        if nObject.Contains('IsVege') then   IsCorn := nObject.I['IsVege'];
        if nObject.Contains('IsCorn') then   IsCorn := nObject.I['IsCorn'];
        if nObject.Contains('IsWine') then   IsWine := nObject.I['IsWine'];
        if nObject.Contains('IsGrass') then   IsGrass := nObject.I['IsGrass'];
        if nObject.Contains('IsFruit') then   IsFruit := nObject.I['IsFruit'];
        if nObject.Contains('TreeGrowAge') then   TreeGrowAge := nObject.I['TreeGrowAge'];
        if nObject.Contains('FallTreeAnimObj') then   FallTreeAnimObj := nObject.I['FallTreeAnimObj'];
        if nObject.Contains('NextTreeAgeObj') then   NextTreeAgeObj := nObject.I['NextTreeAgeObj'];

        if nObject.Contains('LightRadius') then   LightRadius := nObject.I['LightRadius'];
        if nObject.Contains('LightPower') then   LightPower := nObject.I['LightPower'];

        if nObject.Contains('AxeHitTimes') then
          AxeHitTimes := nObject.I['AxeHitTimes'];

        if nObject.Contains('TrunksCount') then
          TrunksCount := nObject.I['TrunksCount'];

        If nObject.Contains('SnowPic') then SnowPic := nObject.I['SnowPic'];
        If nObject.Contains('Stump') then LandStump := nObject.I['Stump'];

        VWareChance := nObject.I['MainGettingChance'];
        nArr2 := nObject.A['VirtualWares'];

        if nArr2.Count > 0 then
        begin
          SetLength(VWares, nArr2.Count);
          for K := 0 to nArr2.Count - 1 do
          begin
            VWares[K].W := nArr2[K].S['Type'];
            VWares[K].Cmin := nArr2[K].I['Min'];
            VWares[K].Cmax := nArr2[K].I['Max'];
            VWares[K].Ch := nArr2[K].I['Chance'];
          end;
        end;


        if nObject.Contains('PlaceableInEditor') then
        begin
          if nObject.B['PlaceableInEditor'] then
            Stump := -1
          else
            Stump := 12;
        end;
        if (IsCorn > 0) or (IsGrass > 0) or (IsWine > 0) or (IsVege > 0) then
          KillByRoad := kbrAlways
        else
        if (FallTreeAnimObj > 0)
        or (NextTreeAgeObj > 0)
        or (LandStump > 0)
        or CuttableTree
        or (TrunksCount > 0)
        or (TreeGrowAge > 0) then
          KillByRoad := kbrNever
        else
          KillByRoad := kbrWest;
      end;

    end;
  end;


  nArr := nObjects.A['Grain Types'];
  for I := 0 to nArr.Count - 1 do
  begin
    nObject := nArr.O[I];

    if not TKMEnumUtils.TryGetAs<TKMGrainType>(nObject.S['GrainType'],  GT) then
      raise Exception.Create('Error loading ' + jsonPath + ': wrong GranType name: ' + nObject.S['GrainType']);

    tmpGrain.GuiIcon := nObject.I['GuiIcon'];
    tmpGrain.TextID := nObject.I['TextID'];
    tmpGrain.Wine := nObject.D['Wine'];
    tmpGrain.Straw := nObject.D['Straw'];
    tmpGrain.Seeds := nObject.D['Seeds'];
    tmpGrain.Hay := nObject.D['Hay'];
    tmpGrain.Vege := nObject.D['Vege'];
    tmpGrain.Dead.Obj := nObject.O['Dead'].I['Object'];

    if tmpGrain.Dead.Obj = 255 then
      tmpGrain.Dead.Obj := 0;
    tmpGrain.Dead.Terr := nObject.O['Dead'].I['Terrain'];

    nArr2 := nObject.A['Stages'];
    SetLength(tmpGrain.Stage, nArr2.Count);
    for K := 0 to nArr2.Count - 1 do
    begin
      tmpGrain.Stage[K].Obj := nArr2.O[K].I['Object'];
      If tmpGrain.Stage[K].Obj = 0 then
        tmpGrain.Stage[K].Obj := 255;

      tmpGrain.Stage[K].Terr := nArr2.O[K].I['Terrain'];
      tmpGrain.Stage[K].NextStage := nArr2.O[K].I['NextStage'];
      tmpGrain.Stage[K].Age := nArr2.O[K].I['Age'] div TERRAIN_PACE;

      tmpGrain.Stage[K].GivesWares := nArr2.O[K].B['GivesWares'];
      tmpGrain.Stage[K].CanBeCut := nArr2.O[K].B['CanBeCut'];
      tmpGrain.Stage[K].CanBePlant := nArr2.O[K].B['CanBePlant'];
    end;

    if tmpGrain.Valid then
    begin
      gFieldGrains[GT] := tmpGrain;
    end;

  end;

  for I := 0 to OBJECTS_CNT - 1 do
    if gMapElements[I].NextTreeAgeObj > 0 then
      gMapElements[gMapElements[I].NextTreeAgeObj].PrevTreeAgeObj := I;

  SetLength(gFruitTrees, 0);
  nArr := nObjects.A['Fruit Trees'];
  SetLength(gFruitTrees, nArr.Count);

  for I := 0 to nArr.Count - 1 do
  begin
    nObject := nArr.O[I];
    gFruitTrees[I].Fruits := nObject.D['Fruits'];
    gFruitTrees[I].ProgressPerStage := nObject.I['ProgressPerStage'];
    gFruitTrees[I].MatureTreeStage := nObject.I['MatureTreeStage'];
    gFruitTrees[I].GuiIcon := nObject.I['GuiIcon'];
    gFruitTrees[I].HintID := nObject.I['HintID'];


    nArr2 := nObject.A['Stages'];
    SetLength(gFruitTrees[I].Stage, nArr2.Count);

    for K := 0 to nArr2.Count - 1 do
      gFruitTrees[I].Stage[K] := nArr2.I[K];
    gFruitTrees[I].ClimateMulti[tcNone] := 1;
    for TT := Low(TKMTerrainClimate) to High(TKMTerrainClimate) do
      if TT <> tcNone then
        if TKMEnumUtils.GetName<TKMTerrainClimate>(TT, S) then
          gFruitTrees[I].ClimateMulti[TT] := nObject.D[S];

  end;

  for TT := Low(TKMTerrainClimate) to High(TKMTerrainClimate) do
  begin
    if TKMEnumUtils.GetName<TKMTerrainClimate>(TT, S) then
      JSONArrToValidArr(nObjects.A[S], gTreeTypeID[TT]);
  end;


  nArr := nObjects.A['Decorations'];

  for I := 0 to nArr.Count - 1 do
  begin
    nObject := nArr.O[I];
    SetLength(tmpCost, 0);
    nArr2 := nObject.A['Cost'];
    for K := 0 to nArr2.Count - 1 do
    begin
      SetLength(tmpCost, length(tmpCost) + 1);
      tmpCost[high(tmpCost)].W := nArr2.O[K].S['VWareName'];
      tmpCost[high(tmpCost)].C := nArr2.O[K].I['Count'];
    end;

    if nObject.Contains('Type') then
    begin
      if not TKMEnumUtils.TryGetAs<TKMDecorationType>(nObject.S['Type'], DT) then
        Continue;
    end else
      DT := dtObject;
    gDecorations.Add(DT, nObject.I['ID'], nObject.I['GuiIcon'], nObject.I['TextID'], tmpCost);
  end;


  nArr := nObjects.A['GrowingTrees'];

  SetLength(gGrowingTrees, nArr.Count);
  for I := 0 to nArr.Count - 1 do
  begin
    nObject := nArr.O[I];
    gGrowingTrees[I].ObjID := nObject.I['ObjectID'];
    gGrowingTrees[I].Size := nObject.D['Size'];
    gGrowingTrees[I].GuiIcon := nObject.I['GuiIcon'];
  end;
    



  //gFieldGrains
    {Anim: TKMAnimLoop;}
  //SaveToJson(ExeDir + 'Export\Objects.json');
end;


procedure TKMResMapElements.SaveToJson(aPath: string);
var root, obj, obj2 : TKMJsonObject;
  arr, arr2 : TKMJsonArrayNew;
  I, K : integer;
  GT : TKMGrainType;
  clim : TKMTerrainClimate;
  S : String;

begin
  root := TKMJsonObject.Create;
  try
    arr := root.AddArray('Objects');
    //save objects data
    for I := 0 to high(gMapElements) do
    begin
      obj := arr.AddObject(false);
      with gMapElements[I] do
      begin
        obj.Add('ObjectID', I);
        If Anim.Count > 0 then
        begin
          arr2 := Obj.AddArray('AnimSteps', true);
          for K := 1 to Anim.Count do
            arr2.Add(Anim.Step[K])
        end;

        obj.Add('IsCuttableTree', CuttableTree, false);
        obj.Add('DiagonalBlocked', DiagonalBlocked, false);
        obj.Add('AllBlocked', AllBlocked, false);
        obj.Add('WineOrCorn', WineOrCorn, false);
        obj.Add('CanGrow', CanGrow, false);
        obj.Add('DontPlantNear', DontPlantNear, false);
        If Stump = -1 then
          obj.Add('PlaceableInEditor', true);
        obj.Add('CanBeRemoved', CanBeRemoved, false);
        //obj.Add('KillByRoad', KillByRoad, false);//skip it
        obj.Add('SnowPic', SnowPic, I);
        obj.Add('RandomPos', RandomPos, false);
        obj.Add('RenderAsTileOverlay', RenderAsTileOverlay, false);
        obj.Add('RotateToTile', RotateToTile, false);
        If (Ware <> wtNone)
          and (WareCount > 0)
          and TKMEnumUtils.GetName<TKMWareType>(Ware, S) then
          begin
            Obj.Add('Ware', S);
            Obj.Add('WareCount', WareCount);
          end;

        obj.Add('VWareChance', VWareChance, 0);
        If length(VWares) > 0 then
        begin
          arr2 := Obj.AddArray('VirtualWares');
          for K := 0 to High(VWares) do
          begin
            obj2 := arr2.AddObject(true);
            obj2.Add('Type', VWares[K].W);
            obj2.Add('Min', VWares[K].Cmin);
            obj2.Add('Max', VWares[K].Cmax);
            obj2.Add('Chance', VWares[K].Ch);
          end;
        end;

        obj.Add('AxeHitTimes', AxeHitTimes, 0);
        obj.Add('TrunksCount', TrunksCount, 0);
        obj.Add('Stump', LandStump, 0);
        obj.Add('IsFruit', IsFruit, 0);
        obj.Add('IsVege', IsVege, 0);
        obj.Add('IsCorn', IsCorn, 0);
        obj.Add('IsWine', IsWine, 0);
        obj.Add('RenderAsTwo', RenderAsTwo, false);
        obj.Add('TreeGrowAge', TreeGrowAge, 0);
        //obj.Add('PrevTreeAgeObj', PrevTreeAgeObj, 0);//no need to save it
        obj.Add('NextTreeAgeObj', NextTreeAgeObj, 0);
        obj.Add('FallTreeAnimObj', FallTreeAnimObj, 0);
        obj.Add('LightRadius', LightRadius, 0);
        obj.Add('LightPower', LightPower, 0);
        //obj.Add('ObjectPrice', ObjectPrice, 0);
      end;
    end;
    //save grains data
    arr := root.AddArray('Grain Types');
    for GT := GRAIN_MIN to GRAIN_MAX do
    begin
      obj := arr.AddObject;
      if not TKMEnumUtils.GetName<TKMGrainType>(GT, S) then
        raise Exception.Create('Error saving grains data');

      obj.Add('GrainType', S);
      with gFieldGrains[GT] do
      begin
        obj.Add('GuiIcon', GuiIcon, 0);
        obj.Add('TextID', TextID, 0);
        obj.Add('Wine', Wine, 0, 2);
        obj.Add('Straw', Straw, 0, 2);
        obj.Add('Seeds', Seeds, 0, 2);
        obj.Add('Hay', Hay, 0, 2);
        obj.Add('Vege', Vege, 0, 2);

        obj2 := obj.AddObject('Dead', true);
        obj2.Add('Object', Dead.Obj, 0);
        obj2.Add('Terrain', Dead.Terr, 0);

        arr2 := obj.AddArray('Stages');
        for K := 0 to high(Stage) do
        begin
          obj2 := arr2.AddObject(true);
          obj2.Add('Object', Stage[K].Obj, 0);
          obj2.Add('Terrain', Stage[K].Terr, 0);
          obj2.Add('NextStage', Stage[K].NextStage, 0);
          obj2.Add('Age', Integer(Stage[K].Age * TERRAIN_PACE), 0);
          obj2.Add('GivesWares', Stage[K].GivesWares, false);
          obj2.Add('CanBeCut', Stage[K].CanBeCut, false);
          obj2.Add('CanBePlant', Stage[K].CanBePlant, false);
        end;
          
      end;
    end;

    //save fruit trees data
    arr := root.AddArray('Fruit Trees');
    for I := 0 to High(gFruitTrees) do
    begin
      obj := arr.AddObject;
      with gFruitTrees[I] do
      begin
        obj.Add('GuiIcon', GuiIcon, 0);
        obj.Add('HintID', HintID, 0);
        obj.Add('Fruits', Fruits, 0);
        obj.Add('ProgressPerStage', ProgressPerStage, 0);
        obj.Add('MatureTreeStage', MatureTreeStage, 0);

        arr2 := obj.AddArray('Stages', true);
        for K := 0 to High(Stage) do
          arr2.Add(Stage[K]);

        for clim := TKMTerrainClimate(1) to High(TKMTerrainClimate) do
        begin
          if not TKMEnumUtils.GetName<TKMTerrainClimate>(clim, S) then
            raise Exception.Create('Error saving fruits data');
          obj.Add(S, ClimateMulti[clim], 0, 2);
        end;
      end;
    end;

    //save trees on climate
    for clim := TKMTerrainClimate(1) to High(TKMTerrainClimate) do
    begin
      if not TKMEnumUtils.GetName<TKMTerrainClimate>(clim, S) then
        raise Exception.Create('Error saving fruits data');
      arr := Root.AddArray(S, true);
      for I := 0 to high(gTreeTypeID[clim]) do
        arr.Add(gTreeTypeID[clim, I]);
    end;

    //save decorations
    arr := root.AddArray('Decorations');
    for I := 0 to high(gDecorations) do
    with gDecorations[I] do
    begin
      obj := arr.AddObject;
      if not TKMEnumUtils.GetName<TKMDecorationType>(DType, S) then
        raise Exception.Create('Error saving decoration data');

      obj.Add('Type', S);
      obj.Add('ID', ID, 0);
      obj.Add('GuiIcon', GuiIcon, 0);
      obj.Add('TextID', TextID, 0);

      arr2 := obj.AddArray('Cost');

      for K := 0 to High(Cost) do
      begin
        obj2 := arr2.AddObject(true);
        obj2.Add('VWareName', Cost[K].W);
        obj2.Add('Count', Cost[K].C);
      end;

    end;

    //save growing trees
    arr := root.AddArray('GrowingTrees');
    for I := 0 to High(gGrowingTrees) do
    with gGrowingTrees[I] do
    begin
      obj := arr.AddObject(true);

      obj.Add('ObjectID', ObjID, 0);
      obj.Add('GuiIcon', GuiIcon, 0);
      obj.Add('Size', Size, 0, 2);
    end;
    root.SaveToFile(aPath);
  finally
    FreeAndNil(root);
  end;

end;

Procedure TKMResMapElements.ReloadJSONData(UpdateCRC: Boolean);
//var oldCRC : Cardinal;
begin
  OBJECTS_CNT := 0;
  SetLength(gMapElements, 0);
  LoadFromFile;
end;

procedure TKMResMapElements.AfterResourceLoad;
var I, K : Integer;
begin
  HighestPrice := 0;
  for I := 0 to High(gMapElements) do
    with gMapElements[I] do
    begin
      ObjectPrice := 0;
      for K := 0 to High(VWares) do
        ObjectPrice := ObjectPrice + (gRes.Wares.VirtualWares.WareS[VWares[K].W].CoinPrice * VWares[K].Cmax * (VWares[K].Ch / 100));
      HighestPrice := Max(HighestPrice, ObjectPrice);
    end;

end;


procedure TKMResMapElements.ExportToText(const aFileName: string);
var
  I: Integer;
  ft: TextFile;
  str1, str2, str3, str4, str5, str6, str7: string;
begin
  AssignFile(ft, ExeDir + aFileName);
  Rewrite(ft);
  str1 := 'not AllBlocked and Block Build: ';
  str2 := 'AllBlocked and Allow Build: ';
  str3 := 'DiagonalBlocked and AllBlocked: ';
  str4 := 'DiagonalBlocked and Can Build: ';
  str5 := 'DiagonalBlocked and Can not build: ';
  str6 := 'AllBlocked and and Block Build: ';
  str7 := 'Stump <> -1: ';
  for I := 1 to fCount do
  begin
    //Writeln(ft);
    Write(ft, inttostr(I) + ' Anim: ' + inttostr(gMapElements[I].Anim.Count));
//    for K := 1 to 30 do
//      if gMapElements[I].Anim.Step[K] > 0 then
//        Write(ft, gMapElements[I].Anim.Step[K], '.')
//      else
//        Write(ft, '_.');
    Write(ft, '; DiagonalBlocked = ', gMapElements[I].DiagonalBlocked);
    Write(ft, '; AllBlocked = ', gMapElements[I].AllBlocked);
    Write(ft, '; CanBeRemoved = ', gMapElements[I].CanBeRemoved);
    Write(ft, '; Stump = ', IntToStr(gMapElements[I].Stump));
    Writeln(ft);

    if (gMapElements[I].Anim.Count > 0) and (gMapElements[I].Anim.Step[1] > 0)
      and (gMapElements[I].Stump = -1) then
    begin
      if not gMapElements[I].AllBlocked and not gMapElements[I].CanBeRemoved then
        str1 := str1 + IntToStr(I) + ' ';

      if gMapElements[I].AllBlocked and gMapElements[I].CanBeRemoved then
        str2 := str2 + IntToStr(I) + ' ';

      if gMapElements[I].AllBlocked and not gMapElements[I].CanBeRemoved then
        str6 := str6 + IntToStr(I) + ' ';

      if gMapElements[I].DiagonalBlocked and gMapElements[I].AllBlocked then
        str3 := str3 + IntToStr(I) + ' ';

      if gMapElements[I].DiagonalBlocked and gMapElements[I].CanBeRemoved then
        str4 := str4 + IntToStr(I) + ' ';

      if gMapElements[I].DiagonalBlocked and not gMapElements[I].CanBeRemoved then
        str5 := str5 + IntToStr(I) + ' ';
    end;

    if gMapElements[I].Stump <> -1 then
      str7 := str7 + IntToStr(I) + ' ';
    // for K:=1 to 16 do
    // write(ft,MapElem[I].CuttableTree,''); //Those are 1/0 so we can ommit space between them


    //Writeln(ft);
  end;
  Writeln(ft);
  Writeln(ft, str1);
  Writeln(ft);
  Writeln(ft, str2);
  Writeln(ft);
  Writeln(ft, str6);
  Writeln(ft);
  Writeln(ft, str3);
  Writeln(ft);
  Writeln(ft, str4);
  Writeln(ft);
  Writeln(ft, str5);
  Writeln(ft);
  Writeln(ft, str7);
  CloseFile(ft);
end;

function TKMGrainDat.Valid: Boolean;
begin
  Result := (Length(Stage) > 0)
            and (GuiIcon > 0)
            and (TextID > 0)
            and ((Straw > 0) or (Seeds > 0) or (Hay > 0) or (Wine > 0) or (Vege > 0));
end;

function TKMGrainDat.StagesCount: Byte;
begin
  Result := Length(Stage);
end;

function TKMGrainDat.GetStage(aAge: Byte): Byte;
var I : Integer;
begin

  if (aAge = 255) or InRange(aAge, CORN_AGE_MAX, CORN_AGE_DEAD) then
    Exit(CORN_AGE_MAX)
  else
    Result := 254;

  for I := 0 to StagesCount - 1 do
    if aAge = Stage[I].Age then
      Exit(I);
end;

function TKMFruitTree.StagesCount: Byte;
begin
  Result := Length(Stage);
end;

function TKMFruitTree.GetStage(aObj: Word): Byte;
var I : Integer;
begin
  Result := 0;
  for I := 0 to High(Stage) do
    if Stage[I] = aObj then
      Result := I;

end;

procedure TKMDecorationArrayHelper.Add(aType : TKMDecorationType; aID, aGuiIcon, aTextID : Word; aCost : TKMVWarePlanCommon);
var J : Integer;
begin
  J := length(self);
  SetLength(self, J + 1);
  self[J].ID := aID;
  self[J].GuiIcon := aGuiIcon;
  self[J].TextID := aTextID;
  self[J].DType := aType;
  self[J].Cost := aCost;
end;

function ObjectIsChoppableTree(aObjId: Integer): Boolean;
//var
//  I: Integer;
//  K: TKMChopableAge;
begin
  {for I := 1 to Length(CHOPABLE_TREES) do
    for K := Low(TKMChopableAge) to High(TKMChopableAge) do
      if (aObjId = CHOPABLE_TREES[I,K]) then
        Result := True;}

  Result := gMapElements[aObjID].CuttableTree;
end;


function ObjectIsChoppableTree(aObjId: Integer; aStage: TKMChopableAge): Boolean;
var
  I: Integer;
begin
  Result := false;
  if (aObjID = 255) then
    Exit(false);
  {Result := false;

  for I := 1 to Length(CHOPABLE_TREES) do
    if (aObjId = CHOPABLE_TREES[I, aStage]) then
      Result := true;
  Result := Result or gMapElements[aObjID].CuttableTree;}
  case aStage of
    caAge1,
    caAge2,
    caAge3: Result := gMapElements[aObjID].NextTreeAgeObj > 0;
    caAgeFull: Result := (gMapElements[aObjID].CuttableTree);
    caAgeFall: Result := (gMapElements[aObjID].FallTreeAnimObj = 0)
                and (gMapElements[aObjID].CuttableTree = false)
                and (gMapElements[aObjID].LandStump = 0)
                and (gMapElements[aObjID].NextTreeAgeObj = 0);

    caAgeStump: for I := 0 to high(STUMPS) do
                  If STUMPS[I] = aObjId then
                    Exit(true);
  end;

end;


function ObjectIsChoppableTree(aObjId: Integer; aStages: TKMChopableAgeSet): Boolean;
var
  stage: TKMChopableAge;
begin
  Result := false;

  for stage in aStages do
    If ObjectIsChoppableTree(aObjId, stage) then
      Result := true;
  {Result := False;
  for I := 1 to Length(CHOPABLE_TREES) do
    for stage in aStages do
      if (aObjId = CHOPABLE_TREES[I, stage]) then
        Result := True;

  Result := Result or gMapElements[aObjID].CuttableTree;}


end;


function ObjectIsCorn(aObjId: Integer): Boolean;
begin
  Result := gMapElements[aObjId].IsCorn > 0;
end;

function ObjectIsGrass(aObjId: Integer): Boolean;
begin
  Result := gMapElements[aObjId].IsGrass > 0;
end;

function ObjectIsVege(aObjId: Integer): Boolean;
begin
  Result := gMapElements[aObjId].IsVege > 0;
end;


function ObjectIsWine(aObjId: Integer): Boolean;
begin
  Result := gMapElements[aObjId].IsWine > 0;
end;

function ObjectIsWare(aObjId: Integer): Boolean;
begin
  Result := (gMapElements[aObjID].Ware <> wtNone) and (gMapElements[aObjID].WareCount > 0);
end;

function ObjectGetWare(aObjId: Integer) : TKMWareType;
begin
  IF gMapElements[aObjID].WareCount > 0 then
    Result := gMapElements[aObjID].Ware
  else
    Result := wtNone;
end;

end.
