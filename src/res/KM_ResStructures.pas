unit KM_ResStructures;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_CommonClasses, KM_Points,
  KM_ResTypes,
  JsonDataObjects,
  KM_CommonTypes;

type

  TKMStructureBasic = record
  private
    fTiles : TKMBridgeTileRect;
    fSize,
    fOffset,
    fEntrance : TKMPoint;
    fOrder : TKMPointArray;
    function GetTile(aX, aY : Integer) : TKMBridgeTile;
    function GetTileX(aLoc : TKMPoint) : TKMBridgeTile;
    function GetPointXY(aX, aY : Integer) : Byte;
    function GetPoint(aLoc : TKMPoint) : Byte;
  public
    property TileXY[aX, aY : Integer] : TKMBridgeTile read GetTile;
    property Tile[aLoc : TKMPoint] : TKMBridgeTile read GetTileX;

    property PointXY[aX, aY : Integer] : Byte read GetPointXY;
    property Point[aLoc : TKMPoint] : Byte read GetPoint;

    property Offset : TKMPoint read fOffset;
    property Size : TKMPoint read fSize;
    property Entrance : TKMPoint read fEntrance;
    property Order : TKMPointArray read fOrder;
  end;

  TKMStructureBasicArray = array[0..3] of TKMStructureBasic;

  TKMStructureSpec = class
  private
    {fObjects,
    fTiles : TKMWordArray;
    fRotation,
    fPoints : TKMByteArray;

    fTiles: TKMBridgeTileRect;

    fOffset,
    fEntrance : TKMPoint;}

    fBase : TKMStructureBasicArray;
    fSize : TKMPoint;

    fTextID : Word;
    fDescriptionID : Word;

    fGuiIcon : Word;
    fCost : TKMWarePlan;
    fMaxProgress : Word;
    fBuildingStep : Word;
    fIsBridge,
    fIsConstruction : Boolean;

    fMaxWorkers,
    fType: Byte;
    constructor LoadFromJSON(aBridge : TJSONObject);
    procedure AddCost(aWare : TKMWareType; aCount : Byte = 1);
    //function GetRectPoint(aRot, aX, aY : Integer) : TKMBridgeTile;
    //function GetRectPointI(aRot, aID : Integer) : TKMBridgeTile;
    //function GetPoint(aRot, aID : Integer) : Byte;
    function GetCount : Word;
    function GetSize(aRot : Integer) : TKMPoint;
    //function GetEntrance(aRot : Integer) : TKMPoint;
    //function GetOffset(aRot : Integer) : TKMPoint;
    function GetOrder(aRot : Integer) : TKMPointArray;
    function GetBasic(aRot : Integer) : TKMStructureBasic;

    procedure DoRotations;
  public
    property Count : Word Read GetCount;
    {property Points[aRot, aID : Integer] : Byte read GetPoint;
    property Rect[aRot, aX, aY : Integer] : TKMBridgeTile read GetRectPoint;
    property RectI[aRot, aID : Integer] : TKMBridgeTile read GetRectPointI;
    property Offset[aRot : Integer] : TKMPoint read GetOffset;
    property Entrance[aRot : Integer] : TKMPoint read GetEntrance;}
    property Size[aRot : Integer] : TKMPoint read GetSize;

    property Basic[aRot : Integer] : TKMStructureBasic read GetBasic;
    property Base: TKMStructureBasicArray read fBase;

    property GuiIcon : Word read fGuiIcon;
    property TextID : Word read fTextID;
    property DescriptionID : Word read fDescriptionID;

    property Cost : TKMWarePlan read fCost;
    property BuildingStep : Word read fBuildingStep;
    property MaxProgress : Word read fMaxProgress;
    property IsConstruction : Boolean read fIsConstruction;
    property IsBridge : Boolean read fIsBridge;
    property BridgeType : Byte read fType;
    property MaxWorkers : Byte read fMaxWorkers;



    property PointOrder[aRot : Integer] : TKMPointArray read GetOrder;

  end;

  TKMResStructures = class
  private
    fCRC : Cardinal;
    fList: array of TKMStructureSpec;
    function GetBridge(aIndex : Integer) : TKMStructureSpec;
    function LoadFromJSON : Cardinal;
    function GetCount : Word;
  public
    property CRC: Cardinal read fCRC;
    property Bridge[aIndex : Integer] : TKMStructureSpec read GetBridge; default;
    property Count : Word read GetCount;
    Procedure ReloadJSONData(UpdateCRC: Boolean);
    constructor Create;
    destructor Destroy; override;
  end;

  {TKMStructureBasic = record
    Index : Word;
    Loc: TKMPoint;
    Rotation : Byte;
  end;
  TKMStructureBasicArray = TKMArray<TKMStructureBasic>;

  function KMStructureBasic(aLoc : TKMPoint; aIndex : Word; aRot : Byte) : TKMStructureBasic;}

const MAX_POINT_STAGES = 5;

implementation
uses
  Math, KM_ResTexts, KM_Resource, KM_Defaults, KromUtils,
  KM_CommonClassesExt, KM_CommonUtils, KM_JSONUtils;
const
  TEST_ROTATION = 0;

function TKMStructureBasic.GetTile(aX: Integer; aY: Integer): TKMBridgeTile;
begin
  Result := fTiles[aX, aY];
end;

function TKMStructureBasic.GetTileX(aLoc: TKMPoint): TKMBridgeTile;
begin
  Result := GetTile(aLoc.X, aLoc.Y);
end;

function TKMStructureBasic.GetPointXY(aX: Integer; aY: Integer): Byte;
begin
  Result := 0;
  if (InRange(aX, 0, fSize.X - 1) and InRange(aY, 0, fSize.Y - 1)) then
    Result := fTiles[aX, aY].Point;
end;

function TKMStructureBasic.GetPoint(aLoc : TKMPoint): Byte;
begin
  Result := GetPointXY(aLoc.X, aLoc.Y);
end;


procedure TKMStructureSpec.AddCost(aWare: TKMWareType; aCount: Byte = 1);
begin
  fCost.AddWare(aWare, aCount);
end;

function TKMStructureSpec.GetCount: Word;
begin
  Result := fSize.X * fSize.Y;
end;

function TKMStructureSpec.GetSize(aRot : integer): TKMPoint;
begin
  case aRot of
    0, 2:Result := fSize;
    1, 3:Result := KMPoint(fSize.Y, fSize.X);
  end;
end;
{
function TKMStructureSpec.GetEntrance(aRot : integer): TKMPoint;
begin
  Result := fBase[aRot].Entrance;
end;

function TKMStructureSpec.GetOffset(aRot : integer): TKMPoint;
begin
  Result := fBase[aRot].Offset;
end;}

function TKMStructureSpec.GetOrder(aRot : integer): TKMPointArray;
begin
  Result := fBase[aRot].Order;
end;

function TKMStructureSpec.GetBasic(aRot: Integer): TKMStructureBasic;
begin
  Result := fBase[aRot mod 4];
end;

{function TKMStructureSpec.GetPoint(aRot, aID: Integer): Byte;
begin
  Result := 0;
  if not InRange(aID, 0, Count - 1) then
    Exit;

  Result := fBase[aRot].fTiles[aID mod fSize.X, aID div fSize.X].Point;
end;

function TKMStructureSpec.GetRectPoint(aRot, aX: Integer; aY: Integer): TKMBridgeTile;
//var aID : Integer;
begin
  Result.Point := 0;
  Result.Tile := 0;
  Result.Rot := 0;
  Result.Obj := 255;

  if not InRange(aX, 0, Size[aRot].X - 1) or not InRange(aY, 0, Size[aRot].Y - 1) then Exit;
  if aX + Size[aRot].X * aY > Count then Exit;

  Result := Base[aRot].TileXY[aX, aY];
end;

function TKMStructureSpec.GetRectPointI(aRot, aID: Integer): TKMBridgeTile;
begin
  Result := GetRectPoint(aRot, aID mod Size[aRot].X, aID div Size[aRot].X);
end;}

procedure TKMStructureSpec.DoRotations;
var I, J, K : Integer;
begin
  for I := 1 to High(fBase) do
  begin
    fBase[I].fSize := Size[I];
    //rotate tile array
    SetLength(fBase[I].fTiles, Size[I].X, Size[I].Y);
    for J := 0 to Size[I].X - 1 do
      for K := 0 to Size[I].Y - 1 do
      begin
        case I of
          0 :;
          1 : fBase[I].fTiles[J, K] := fBase[0].fTiles[Size[I].Y - K - 1,  J];
          2 : fBase[I].fTiles[J, K] := fBase[0].fTiles[Size[I].X - 1 -J,Size[I].Y - 1 - K];
          3 : fBase[I].fTiles[J, K] := fBase[0].fTiles[K,Size[I].X - 1 - J];
        end;

        case I of
          0 : ;
          1 : fBase[I].fTiles[J, K].Rot := (fBase[I].fTiles[J, K].Rot + 3) mod 4;
          2 : fBase[I].fTiles[J, K].Rot := (fBase[I].fTiles[J, K].Rot + 2) mod 4;
          3 : fBase[I].fTiles[J, K].Rot := (fBase[I].fTiles[J, K].Rot + 1) mod 4;
        end;

      end;

    //rotate entrance
    case I of
      0: fBase[I].fEntrance := fBase[0].fEntrance;
      1: fBase[I].fEntrance := KMPoint(fBase[0].fEntrance.Y, fBase[0].Size.X - 1 - fBase[0].fEntrance.X);
      2: fBase[I].fEntrance := KMPoint((fBase[0].fSize.X - 1) - fBase[0].fEntrance.X, (fBase[0].fSize.Y - 1) - fBase[0].fEntrance.Y);
      3: fBase[I].fEntrance := KMPoint((fBase[0].fSize.Y - 1) - fBase[0].fEntrance.Y, fBase[0].fEntrance.X);
    end;

    //rotate Offset
    fBase[I].fOffset := KMPointInvert(fBase[I].fEntrance);


  end;
end;


constructor TKMStructureSpec.LoadFromJSON(aBridge : TJSONObject);
  procedure CalculateBuildingStep;
  var I : Integer;
    waresCount : Integer;
  begin
    waresCount := 0;
    for I := 0 to fCost.Count - 1 do
      Inc(waresCount, fCost[I].C);

    fBuildingStep := fMaxProgress div waresCount;
    fMaxProgress := fBuildingStep * waresCount;
  end;
  procedure FindEntrance;
  var I : Integer;
  begin
    with fBase[0] do
    begin
      for I := 0 to Count - 1 do
        if fTiles[I mod fSize.X, I div fSize.X].Point = 2 then
        begin
          fEntrance.X := I mod fSize.X;
          fEntrance.Y := I div fSize.X;

          fOffset.X := -fEntrance.X;
          fOffset.Y := -fEntrance.Y;
        end;
      Assert(fEntrance <> KMPOINT_INVALID_TILE, 'no entrance found for construction');
    end;
  end;

  procedure SetOrder;
  var aOrder : TKMPointArray;
    procedure AddPoint(P : TKMPoint);
    var I : Integer;
    begin
      for I := 0 to High(aOrder) do  //do not add the same point
        if aOrder[I] = P then
          Exit;
      SetLength(aOrder, length(aOrder) + 1);

      aOrder[high(aOrder)] := P;
    end;

  var I, J : Integer;
     P : TKMPoint;
  begin
    for J := 0 to High(fBase) do
      with fBase[J] do
      begin
        P := Entrance;
        SetLength(aOrder, 0);
        if P = KMPOINT_INVALID_TILE then
          Exit;
        AddPoint(P);
        I := 0;
        while I < length(aOrder) do
        begin
          P := aOrder[I];
          if PointXY[P.X + 1, P.Y] > 0 then
            AddPoint(KMPoint(P.X + 1, P.Y));
          if PointXY[P.X - 1, P.Y] > 0 then
            AddPoint(KMPoint(P.X - 1, P.Y));
          if PointXY[P.X, P.Y - 1] > 0 then
            AddPoint(KMPoint(P.X, P.Y - 1));
          if PointXY[P.X, P.Y + 1] > 0 then
            AddPoint(KMPoint(P.X, P.Y + 1));
          Inc(I);
        end;
        fOrder := aOrder;
      end;
      
  end;

var I, J : Integer;
  nArr, nArr2 : TJSONArray;
  W : TKMWareType;
begin
  Inherited Create;
  fBase[0].fEntrance := KMPOINT_INVALID_TILE;
  //JSONToPoint(aBridge.O['Offset'], fOffset);
  JSONToPoint(aBridge.O['Size'], fSize);

  //JSONToPoint(aBridge.O['Entrance'], fEntrance);
  fBase[0].fSize := fSize;
  SetLength(fBase[0].fTiles, fSize.X, fSize.Y);


  nArr := aBridge.A['Tiles'];
  with fBase[0] do
    for I := 0 to nArr.Count - 1 do
    begin
      fTiles[I mod fSize.X, I div fSize.X].Order := high(Word);

      if nArr.O[I].Contains('Tile') then
      begin
        nArr2 := nArr.O[I].A['Tile'];
        for J := 0 to nArr2.Count do
        begin
          with fTiles[I mod fSize.X, I div fSize.X] do
            case J of
              0: Point  := nArr2.I[J];
              1: Tile   := nArr2.I[J];
              2: Rot    := nArr2.I[J];
              3: Obj    := nArr2.I[J];
            end;
        end;

      end else
      if nArr.Types[I] = jdtObject then
        with fTiles[I mod fSize.X, I div fSize.X] do
        begin
          Point := nArr.O[I].I['Point'];
          Obj := nArr.O[I].I['Obj'];
          Tile := nArr.O[I].I['TileID'];
          Rot := nArr.O[I].I['Rot'];
        end;
    end;

    
  {JSONArrToValidArr(aBridge.A['Points'], fPoints);
  JSONArrToValidArr(aBridge.A['Tiles'], fTiles);
  JSONArrToValidArr(aBridge.A['Rotation'], fRotation);
  JSONArrToValidArr(aBridge.A['Objects'], fObjects);}
  JSONIfContains(fTextID, 'TextID', aBridge);
  JSONIfContains(fDescriptionID, 'DescriptionID', aBridge);
  fIsConstruction := aBridge.B['IsConstruction'];
  fIsBridge := aBridge.B['IsBridge'];
  fGuiIcon := aBridge.I['GUIIcon'];
  fMaxProgress := aBridge.I['MaxProgress'];
  fType := aBridge.I['BridgeType'];
  fMaxWorkers := aBridge.I['MaxWorkers'];

  nArr := aBridge.A['Cost'];

  for I := 0 to nArr.Count - 1 do
  begin

    if TKMEnumUtils.TryGetAs<TKMWareType>(nArr.O[I].S['WareType'],  W) then
      AddCost(W, Max(nArr.O[I].I['Count'], 1))
    else
      raise Exception.Create('Wrong TKMWareType:' + nArr.O[I].S['WareType']);


  end;

  CalculateBuildingStep;
  //Rotate(3);
  if fBase[0].Entrance = KMPOINT_INVALID_TILE then
    FindEntrance;

  DoRotations; //create other rotations
  SetOrder;
end;

constructor TKMResStructures.Create;
begin
  Inherited;

  fCRC := LoadFromJSON;
end;

destructor TKMResStructures.Destroy;
var I : Integer;
begin
  for I := 0 to High(fList) do
    FreeAndNil(fList[I]);

  Inherited;
end;

function TKMResStructures.LoadFromJSON : Cardinal;
var jsonPath : String;
  I : Integer;
  nRoot : TJSONObject;
  nBridges : TJSONArray;
begin

  jsonPath := ExeDir + 'data' + PathDelim + 'defines' + PathDelim + 'Bridges.json';
  nRoot := TJSONObject.ParseFromFile(jsonPath) as TJSONObject;

  nBridges := nRoot.A['Bridges'];
  SetLength(fList, nBridges.Count);

  for I := 0 to nBridges.Count - 1 do
    fList[I] := TKMStructureSpec.LoadFromJSON(nBridges.O[I]);

  Result := GetJSONCRC(nRoot);
end;


function TKMResStructures.GetBridge(aIndex: Integer): TKMStructureSpec;
begin
  if not InRange(aIndex, 0, high(fList)) then
    Result := nil
  else
    Result := fList[aIndex];
end;

function TKMResStructures.GetCount: Word;
begin
  Result := length(fList);
end;

Procedure TKMResStructures.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin
  oldCRC := fCRC;

  fCRC := LoadFromJSON;
  if not UpdateCRC then
    fCRC := oldCRC;
end;

{function KMStructureBasic(aLoc : TKMPoint; aIndex : Word; aRot : Byte) : TKMStructureBasic;
begin
  Result.Loc := aLoc;
  Result.Index := aIndex;
  Result.Rotation := aRot;
end;}

end.
