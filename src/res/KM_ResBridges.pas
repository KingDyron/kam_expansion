unit KM_ResBridges;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KM_CommonClasses, KM_Points,
  KM_ResTypes,
  JsonDataObjects,
  KM_CommonTypes;

type


  TKMBridgeSpec = class
  private
    {fObjects,
    fTiles : TKMWordArray;
    fRotation,
    fPoints : TKMByteArray;}

    fTiles: TKMBridgeTileRect;

    fOffset,
    fSize,
    fEntrance : TKMPoint;

    fTextID : Word;
    fDescriptionID : Word;

    fGuiIcon : Word;
    fCost : TKMBuildCostSet;
    fMaxProgress : Word;
    fBuildingStep : Word;
    fIsBridge,
    fIsConstruction : Boolean;
    fOrder : array[0..3] of TKMPointArray;

    fType,
    fRotation : Byte;
    constructor LoadFromJSON(aBridge : TJSONObject);
    procedure AddCost(aWare : TKMWareType; aCount : Byte = 1);
    function GetRectPoint(aRot, aX, aY : Integer) : TKMBridgeTile;
    function GetRectPointI(aRot, aID : Integer) : TKMBridgeTile;
    function GetPoint(aRot, aID : Integer) : Byte;
    function GetCount : Word;
    function GetSize(aRot : Integer) : TKMPoint;
    function GetEntrance(aRot : Integer) : TKMPoint;
    function GetOffset(aRot : Integer) : TKMPoint;
    function GetOrder(aRot : Integer) : TKMPointArray;
  public
    property Count : Word Read GetCount;
    property Points[aRot, aID : Integer] : Byte read GetPoint;
    property Rect[aRot, aX, aY : Integer] : TKMBridgeTile read GetRectPoint;
    property RectI[aRot, aID : Integer] : TKMBridgeTile read GetRectPointI;
    property Offset[aRot : Integer] : TKMPoint read GetOffset;
    property Size[aRot : Integer] : TKMPoint read GetSize;
    property Entrance[aRot : Integer] : TKMPoint read GetEntrance;

    property GuiIcon : Word read fGuiIcon;
    property TextID : Word read fTextID;
    property DescriptionID : Word read fDescriptionID;

    property BuildCost : TKMBuildCostSet read fCost;
    property BuldingStep : Word read fBuildingStep;
    property MaxProgress : Word read fMaxProgress;
    property IsConstruction : Boolean read fIsConstruction;
    property IsBridge : Boolean read fIsBridge;
    property BridgeType : Byte read fType;

    property PointOrder[aRot : Integer] : TKMPointArray read GetOrder;

    procedure SetRotation(aRot : Byte);

  end;

  TKMResBridges = class
  private
    fCRC : Cardinal;
    fList: array of TKMBridgeSpec;
    function GetBridge(aIndex : Integer) : TKMBridgeSpec;
    function LoadFromJSON : Cardinal;
    function GetCount : Word;
  public
    property CRC: Cardinal read fCRC;
    property Bridge[aIndex : Integer] : TKMBridgeSpec read GetBridge; default;
    property Count : Word read GetCount;
    Procedure ReloadJSONData(UpdateCRC: Boolean);
    constructor Create;
    destructor Destroy; override;
  end;

  TKMBridgeBasic = record
    Index : Word;
    Loc: TKMPoint;
    Rotation : Byte;
  end;
  TKMBridgeBasicArray = TKMArray<TKMBridgeBasic>;

  function KMBridgeBase(aLoc : TKMPoint; aIndex : Word; aRot : Byte) : TKMBridgeBasic;

implementation
uses
  Math, KM_ResTexts, KM_Resource, KM_Defaults, KromUtils,
  KM_CommonClassesExt, KM_CommonUtils, KM_JSONUtils;
const
  TEST_ROTATION = 0;

procedure TKMBridgeSpec.AddCost(aWare: TKMWareType; aCount: Byte = 1);
begin
  fCost.AddWare(aWare, aCount);
end;

function TKMBridgeSpec.GetCount: Word;
begin
  Result := fSize.X * fSize.Y;
end;

function TKMBridgeSpec.GetSize(aRot : integer): TKMPoint;
begin
  case aRot of
    0, 2:Result := fSize;
    1, 3:Result := KMPoint(fSize.Y, fSize.X);
  end;
end;

function TKMBridgeSpec.GetEntrance(aRot : integer): TKMPoint;
begin
  case aRot of
    0: Result := fEntrance;
    1: Result := KMPoint(fEntrance.Y, fEntrance.X);
    2: Result := KMPoint((fSize.X - 1) - fEntrance.X, (fSize.Y - 1) - fEntrance.Y);
    3: Result := KMPoint((fSize.Y - 1) - fEntrance.Y, (fSize.X - 1) - fEntrance.X);
  end;
end;

function TKMBridgeSpec.GetOffset(aRot : integer): TKMPoint;
begin
  Result := KMPointInvert(GetEntrance(aRot));
end;

function TKMBridgeSpec.GetOrder(aRot : integer): TKMPointArray;
//var I, aX, aY : Integer;
begin
  //SetLength(Result, length(fOrder));
  {case TEST_ROTATION of
    0: Result := fOrder;
    1: Result := fOrder;
    2: Result := fOrder;
    3: Result := fOrder;
  end;}
  Result := fOrder[aRot];
  {for I := 0 to Count - 1 do
  begin
    aX := I mod Size.X;
    aY := I div Size.Y;

    if Rect[aX, aY].Order <> high(word) then
      Result[Rect[aX, aY].Order] := KMPoint(aX, aY);
  end;}

  {for I := 0 to High(Result) do
  begin
    Result[I] := fOrder[]
  end;}
end;

function TKMBridgeSpec.GetPoint(aRot, aID: Integer): Byte;
begin
  Result := 0;
  if not InRange(aID, 0, Count - 1) then
    Exit;


  case aRot of
    0: Result := fTiles[aID mod fSize.X, aID div fSize.X].Point;
    1: Result := fTiles[aID div fSize.Y,aID mod fSize.Y].Point;
    2:  begin
          aID := Count - aID - 1;
          Result := fTiles[aID mod fSize.X, aID div fSize.X].Point;
        end;
    3: Result := fTiles[aID div fSize.Y, fSize.Y - 1 - aID mod fSize.Y].Point;
  end;

  {case TEST_ROTATION of
    0: Result := fPoints[aID];
    1: Result := fPoints[(aID div fSize.Y) + ((aID mod fSize.Y) * fSize.X)];
    2: Result := fPoints[Count - 1 - aID];
    3: Result := fPoints[(aID div fSize.Y) + ((fSize.Y - 1 - aID mod fSize.Y) * fSize.X)];
  end;}
end;

function TKMBridgeSpec.GetRectPoint(aRot, aX: Integer; aY: Integer): TKMBridgeTile;
//var aID : Integer;
begin
  Result.Point := 0;
  Result.Tile := 0;
  Result.Rot := 0;
  Result.Obj := 255;

  if not InRange(aX, 0, Size[aRot].X - 1) or not InRange(aY, 0, Size[aRot].Y - 1) then Exit;
  if aX + Size[aRot].X * aY > Count then Exit;

  //Result := fTiles[aX, aY]{Points[aX + Size.X * aY]};
  case aRot of
    0: Result := fTiles[aX, aY];
    1: Result := fTiles[aY, aX];
    2: Result := fTiles[Size[aRot].X - 1 - aX, Size[aRot].Y - 1 - aY];
    3: Result := fTiles[Size[aRot].Y - 1 - aY, Size[aRot].X - 1 - aX];
  end;

  case aRot of
    0: ;
    1: Result.Rot := (Result.Rot + 1) mod 4;
    2: Result.Rot := (Result.Rot + 2) mod 4;
    3: Result.Rot := (Result.Rot + 3) mod 4;
  end;
  
end;

function TKMBridgeSpec.GetRectPointI(aRot, aID: Integer): TKMBridgeTile;
begin
  Result := GetRectPoint(aRot, aID mod Size[aRot].X, aID div Size[aRot].X);
end;

procedure TKMBridgeSpec.SetRotation(aRot: Byte);
begin
  fRotation := aRot mod 4;
end;

constructor TKMBridgeSpec.LoadFromJSON(aBridge : TJSONObject);
  procedure CalculateBuildingStep;
  var I : Integer;
    waresCount : Integer;
  begin
    waresCount := 0;
    for I := 0 to fCost.Count - 1 do
      Inc(waresCount, fCost.Cost[I].C);

    fBuildingStep := (trunc(fMaxProgress / waresCount) div 5) * 5;
    fMaxProgress := fBuildingStep * waresCount;
  end;
  procedure FindEntrance;
  var I : Integer;
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

  var I : Integer;
     P : TKMPoint;
  begin
    P := Entrance[fRotation];
    SetLength(aOrder, 0);
    if P = KMPOINT_INVALID_TILE then
      Exit;
    AddPoint(P);
    I := 0;
    while I < length(aOrder) do
    begin
      P := aOrder[I];

      if Rect[fRotation, P.X + 1, P.Y].Point > 0 then
        AddPoint(KMPoint(P.X + 1, P.Y));
      if Rect[fRotation, P.X - 1, P.Y].Point > 0 then
        AddPoint(KMPoint(P.X - 1, P.Y));
      if Rect[fRotation, P.X, P.Y - 1].Point > 0 then
        AddPoint(KMPoint(P.X, P.Y - 1));
      if Rect[fRotation, P.X, P.Y + 1].Point > 0 then
        AddPoint(KMPoint(P.X, P.Y + 1));
      Inc(I);
    end;
    fOrder[fRotation] := aOrder;
      
  end;

var I, J : Integer;
  nArr, nArr2 : TJSONArray;
  W : TKMWareType;
begin
  Inherited Create;
  fRotation := 0;
  fEntrance := KMPOINT_INVALID_TILE;
  //JSONToPoint(aBridge.O['Offset'], fOffset);
  JSONToPoint(aBridge.O['Size'], fSize);
  //JSONToPoint(aBridge.O['Entrance'], fEntrance);
  SetLength(fTiles, fSize.X, fSize.Y);


  nArr := aBridge.A['Tiles'];
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
  if fEntrance = KMPOINT_INVALID_TILE then
    FindEntrance;
  for I := 0 to 3 do
  begin
    fRotation := I;
    SetOrder;
  end;
  fRotation := 0
end;

constructor TKMResBridges.Create;
begin
  Inherited;

  fCRC := LoadFromJSON;
end;

destructor TKMResBridges.Destroy;
var I : Integer;
begin
  for I := 0 to High(fList) do
    fList[I].Free;

  Inherited;
end;

function TKMResBridges.LoadFromJSON : Cardinal;
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
    fList[I] := TKMBridgeSpec.LoadFromJSON(nBridges.O[I]);

  Result := GetJSONCRC(nRoot);
end;


function TKMResBridges.GetBridge(aIndex: Integer): TKMBridgeSpec;
begin
  if not InRange(aIndex, 0, high(fList)) then
    Result := nil
  else
    Result := fList[aIndex];
end;

function TKMResBridges.GetCount: Word;
begin
  Result := length(fList);
end;

Procedure TKMResBridges.ReloadJSONData(UpdateCRC: Boolean);
var oldCRC : Cardinal;
begin
  oldCRC := fCRC;

  fCRC := LoadFromJSON;
  if not UpdateCRC then
    fCRC := oldCRC;
end;

function KMBridgeBase(aLoc : TKMPoint; aIndex : Word; aRot : Byte) : TKMBridgeBasic;
begin
  Result.Loc := aLoc;
  Result.Index := aIndex;
  Result.Rotation := aRot;
end;

end.
