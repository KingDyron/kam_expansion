unit KM_HousePasture;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes, KM_Points,
  KM_Houses,
  KM_ResTypes, KM_TerrainTypes;

const
  MAX_ANIMALS = 10;
const
  ANIMAL_ACTION_COUNT = byte(high(TKMPastureAnimalAction)) + 1;
type
  TKMPastureAnimal = record
    AnimalType : TKMPastureAnimalType;
    Age : Integer;
    Pos, WalkTo, Speed : TKMPointF;
    Dir, ToDir : TKMDirection;
    Action : TKMPastureAnimalAction;
    ActionCycles : Word;
    AnimStep : Word;
    Color : Byte;
  end;

  TKMHousePasture = class(TKMHouse)
  private
    fAnimals : array[0..MAX_ANIMALS - 1] of TKMPastureAnimal;
    fFill : array[1..WARES_IN_OUT_COUNT] of Single;
    fBestClimate : TKMTerrainClimate;
    fObjects : array[0..2] of Word;
    function IsAnimalAt(aLoc : TKMPointF) : Boolean;
    function GetPositionForAnimal : TKMPointF;
    procedure FillWare(aWareType : TKMWareType; aAmount : Single);
    procedure SetAnimalActionWalk(var aAnimal : TKMPastureAnimal);
    procedure SetNewAction(var aAnimal : TKMPastureAnimal);
    procedure UpdateAnimal(var aAnimal : TKMPastureAnimal);
    procedure CheckClimate;
  protected
  public
    {function HasMoreEntrances : Boolean; override;
    function GetClosestEntrance(aLoc: TKMPoint): TKMPointDir; override;
    function Entrances : TKMPointDirArray;  override;}
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);

    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;

    procedure BuyAnimal(aAnimal : TKMPastureAnimalType);

    function GetPastureTileType : Word;
  end;

implementation
uses
  Classes,
  KM_HandsCollection,
  KM_RenderPool, KM_RenderAux,
  KM_Resource, KM_ResUnits,
  KM_Terrain,
  KM_CommonUtils;
{
function TKMHousePasture.HasMoreEntrances: Boolean;
begin
  Result := true;
end;

function TKMHousePasture.Entrances: TKMPointDirArray;
begin
  Result := [
              KMPointDir(Entrance.X, Entrance.Y, dirS),
              KMPointDir(Entrance.X - 1, Entrance.Y - 3, dirN)
            ];
end;

function TKMHousePasture.GetClosestEntrance(aLoc: TKMPoint): TKMPointDir;
const  ENTRANCE_POS : array[1..2] of TKMPoint = ( (X : 0; Y : 0),
                                                (X : -1; Y : -3));
const  ENTRANCE_DIR : array[1..2] of TKMDirection = (dirS, dirN);

var I : Integer;
  lastDist, tmp : Single;
begin
  Result := Inherited;

  lastDist := 99999;
  for I := low(ENTRANCE_POS) to High(ENTRANCE_POS) do
  begin
    tmp := KMLength(aLoc, Entrance + ENTRANCE_POS[I]);
    If tmp < lastDist then
    begin
      lastDist := tmp;
      Result.Loc := Entrance + ENTRANCE_POS[I];
      Result.Dir := ENTRANCE_DIR[I];
    end;
  end;
end;
}

constructor TKMHousePasture.Create(aUID: Integer; aHouseType: TKMHouseType; PosX: Integer; PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
begin
  Inherited;
  CheckClimate;
end;

procedure TKMHousePasture.CheckClimate;
var climates : array[TKMTerrainClimate] of byte;
  I : TKMTerrainClimate;
  entr : TKMPoint;
  K, L : Integer;
  X, Y : Integer;
  HA : TKMHouseAreaNew;
begin
  FillChar(climates, SizeOf(climates), #0);
  fBestClimate := tcNone;

  entr := Position;
  HA := HSpec.BuildArea;
  for K := 1 to MAX_HOUSE_SIZE do
  for L := 1 to MAX_HOUSE_SIZE do
    if HA[K,L] <> 0 then
    begin
      X := entr.X + L - 3;
      Y := entr.Y + K - 4;
      Inc(climates[gTerrain.FindBestClimatType(KMPoint(X, Y)) ]);
    end;

  for I := Low(TKMTerrainClimate) to High(TKMTerrainClimate) do
    If climates[fBestClimate] < climates[I] then
      fBestClimate := I;

  FillChar(fObjects, Sizeof(fObjects), #0);
  //set tree types
  case fBestClimate of
    tcDry1,
    tcDry2 :begin
              fObjects[0] := 381;
              fObjects[1] := 191;
              fObjects[2] := 200;
            end;
    tcWarm1,
    tcWarm2 :begin
              fObjects[0] := 385;
              fObjects[1] := 94;
              fObjects[2] := 109;
            end;
    tcWet1,
    tcWet2 :begin
              fObjects[0] := 370;
              fObjects[1] := 94;
              fObjects[2] := 374;
            end;
    tcNeutral:begin
              fObjects[0] := 172;
              fObjects[1] := 160;
              fObjects[2] := 582;
            end;
    tcCold1:begin
              fObjects[0] := 170;
              fObjects[1] := 165;
              fObjects[2] := 163;
            end;
    tcCold2:begin
              fObjects[0] := 426;
              fObjects[1] := 394;
              fObjects[2] := 163;
            end;
  end;

end;


function TKMHousePasture.IsAnimalAt(aLoc: TKMPointF): Boolean;
var I : integer;
begin
  Result := false;
  for I := 0 to MAX_ANIMALS - 1  do
    If fAnimals[I].AnimalType <> patNone then
      If (KMLengthDiag(aLoc, fAnimals[I].Pos) <= fAnimals[I].AnimalType.Spec.Size)
        or (KMLengthDiag(aLoc, fAnimals[I].WalkTo) <= fAnimals[I].AnimalType.Spec.Size)
      then
        Exit(true);
end;

function TKMHousePasture.GetPositionForAnimal: TKMPointF;
var I : Byte;
begin
  I := 0;
  repeat
    Result.X := KaMRandomS1(4.5, 'TKMHousePasture.GetPositionForAnimal 1');
    Result.Y := KaMRandomS1(2.75, 'TKMHousePasture.GetPositionForAnimal 2');
    Inc(I);
  until (I >= 10) or not IsAnimalAt(Result);//max 10 tries
end;

procedure TKMHousePasture.FillWare(aWareType: TKMWareType; aAmount: Single);
var I, C : Integer;

begin
  I := GetWareOutIndex(aWareType);
  If I < 1 then
    Exit;
  Inc(fFill[I], aAmount);
  C := trunc(fFill[I]);
  If C > 0 then
  begin
    Dec(fFill[I], C);
    WareAddToOut(aWareType, C);
  end;
end;

procedure TKMHousePasture.SetAnimalActionWalk(var aAnimal : TKMPastureAnimal);
var len : Single;
begin
  with aAnimal do
  begin
    Action := paaWalk;
    WalkTo := GetPositionForAnimal;
    ToDir := KMGetDirection(Pos, WalkTo);
    len := KMLength(Pos, WalkTo) / AnimalType.Spec.Speed;
    Speed.X := (WalkTo.X - Pos.X) / len;
    Speed.Y := (WalkTo.Y - Pos.Y) / len;
  end;
end;

procedure TKMHousePasture.SetNewAction(var aAnimal: TKMPastureAnimal);

  function GetAnimCycles : Word;
  begin
    //Result := gRes.Units.PastureAnimals[aAnimal.AnimalType].Anim[aAnimal.Action, dirN].Count;
    Result := aAnimal.AnimalType.Spec.Anim[aAnimal.Action, dirN].Count;
  end;
var oldAct : TKMPastureAnimalAction;
begin
  with aAnimal do
  begin
    oldAct := Action;
    repeat
    case Action of
      paaLayDown: Action := paaLying;
      paaLying: Action := paaStandUp;
      else
      begin
        case KaMRandom(4, 'TKMHousePasture.UpdateAnimal 1') of
          0 : Action := paaWalk;
          1 : Action := paaEat;
          2 : Action := paaWatch;
          3 : Action := paaLayDown;
        end;
      end;
    end;
    until oldAct <> Action;
    //Action := TKMPastureAnimalAction(KaMRandom(ANIMAL_ACTION_COUNT, 'TKMHousePasture.UpdateAnimal 1'));
    case Action of
      paaWalk:  SetAnimalActionWalk(aAnimal);
      paaLying,
      paaWatch: ActionCycles := (1 + KaMRandom(3, 'TKMHousePasture.UpdateAnimal 2')) * GetAnimCycles;
      paaEat: ActionCycles := (5 + KaMRandom(5, 'TKMHousePasture.UpdateAnimal 2')) * GetAnimCycles;

      paaLayDown,
      paaStandUp : ActionCycles := GetAnimCycles - 1;
    end;
    AnimStep := 0;
  end;

end;

procedure TKMHousePasture.UpdateAnimal(var aAnimal : TKMPastureAnimal);
begin
  with aAnimal do
  begin
    If AnimalType = patNone then
      Exit;
    Inc(Age);
    If Age >= AnimalType.Spec.KillAge then
    begin
      FillWare(wtPig, AnimalType.Spec.Meat);
      FillWare(wtSkin, AnimalType.Spec.Skin);
      Age := 0;
      AnimalType := patNone;
      Exit;
    end else
    If (AnimalType.Spec.Age > 0) and (Age mod AnimalType.Spec.Age = 0) then
    begin
      FillWare(wtFeathers, AnimalType.Spec.Feathers);
      FillWare(wtEgg, AnimalType.Spec.Eggs);
    end;
    //inc action animation
    Inc(AnimStep);

    case Action of
      paaWalk : begin
                  If Dir <> ToDir then
                  begin
                    If GetDirDifference(Dir, ToDir) = 1 then
                      Dir := DIR_TO_PREV[Dir]
                    else
                      Dir := DIR_TO_NEXT[Dir];
                      AnimStep := 0;
                  end else
                  If KMSamePointF(Pos, WalkTo, AnimalType.Spec.Speed / 2) then
                  begin
                    Pos := WalkTo;
                    SetNewAction(aAnimal);
                  end else
                  begin
                    Inc(Pos.X, Speed.X);
                    Inc(Pos.Y, Speed.Y);
                  end;
                end;
      else        begin
                    If AnimStep >= ActionCycles then
                      SetNewAction(aAnimal);
                  end;
    end;

  end;
end;

procedure TKMHousePasture.BuyAnimal(aAnimal: TKMPastureAnimalType);
var I : Integer;
begin
  //check for free space
  for I := 0 to MAX_ANIMALS - 1 do
    If fAnimals[I].AnimalType = patNone then
    begin
      If not gHands[Owner].VirtualWareTake('vtCoin', aAnimal.Spec.Cost) then
        Exit;
      fAnimals[I].AnimalType := aAnimal;
      fAnimals[I].Pos := GetPositionForAnimal;
      fAnimals[I].Dir := DirN;
      fAnimals[I].Age := -KaMRandom(1000, ' TKMHousePasture.BuyAnimal : age');
      fAnimals[I].Color := KaMRandom(length(aAnimal.Spec.Colors), 'TKMHousePasture.BuyAnimal : color');
      SetNewAction(fAnimals[I]);
      Exit;
    end;


end;

function TKMHousePasture.GetPastureTileType: Word;
begin
  case fBestClimate of
    tcDry1 : Result := 27;
    tcDry2 : Result := 28;
    tcWarm1 : Result := 0;
    tcWarm2 : Result := 26;
    tcWet1 : Result := 16;
    tcWet2 : Result := 34;
    tcNeutral : Result := 34{37};
    tcCold1 : Result := 315;
    tcCold2 : Result := 47;
    else
      Result := 0;
  end;
end;

procedure TKMHousePasture.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  Inherited;
  If not IsComplete then
    Exit;
  for I := 0 to MAX_ANIMALS - 1 do
    UpdateAnimal(fAnimals[I]);

end;


procedure TKMHousePasture.Paint;
const OBJECTS_OFFSET_X : array[0..2] of Integer = (-1, +3, 0);
      OBJECTS_OFFSET_Y : array[0..2] of Integer = (-3, -1, -1);
var I, K : Integer;
  spec : TKMPasAnimalSpec;
  entr : TKMPoint;
begin
  Inherited;
  If IsComplete then
  begin
    //gRenderPool.AddHousePasture(Entrance, FlagColor);
    entr := Entrance;
    for I := 0 to MAX_ANIMALS - 1 do
      If fAnimals[I].AnimalType <> patNone then
      begin
        {gRenderPool.AddAnimationG(KMPointF(Entrance.X + fAnimals[I].Pos.X - 1.5, Entrance.Y + fAnimals[I].Pos.Y - 3.5),
                                  gRes.Units.PastureAnimals[fAnimals[I].AnimalType].Anim[fAnimals[I].Action, fAnimals[I].Dir],
                                  //gRes.Units[utLandDuck].UnitAnim[uaWalk,fAnimals[I].Dir],
                                  fAnimals[I].AnimStep,
                                  FlagColor, rxUnits);}
        spec := fAnimals[I].AnimalType.Spec;
        gRenderPool.AddHousePastureAnimal(KMPointF(entr.X + fAnimals[I].Pos.X - 2.75, entr.Y + fAnimals[I].Pos.Y - 4.75),
                                          fAnimals[I].AnimalType,
                                          fAnimals[I].Action, fAnimals[I].Dir,
                                          fAnimals[I].AnimStep,
                                          Spec.Colors[fAnimals[I].Color, 1], Spec.Colors[fAnimals[I].Color, 0]);

      end;
      for I := 0 to High(fObjects) do
        If fObjects[I] > 0 then
          gRenderPool.RenderMapElement(fObjects[I], FlagAnimStep,
                                        entr.X + OBJECTS_OFFSET_X[I],
                                        entr.Y + OBJECTS_OFFSET_Y[I]);
      for I := 0 to 4 do
      for K := 0 to 2 do
          gRenderPool.RenderMapElement(322, FlagAnimStep + I + K,
                                        entr.X + I - 2,
                                        entr.Y + K - 4);
      for I := 0 to 1 do
      for K := 0 to 0 do
          gRenderPool.RenderMapElement(322, FlagAnimStep + I + K,
                                        entr.X + I + 1,
                                        entr.Y + K - 1);



  end;
end;

end.
