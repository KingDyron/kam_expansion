unit KM_HouseCottage;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,  KM_CommonTypes,
  KM_Houses,
  KM_ResTypes;

type
  TKMWorklessAge = (waBaby, waTeenager, waAdult);

type
  TKMHouseCottage = class(TKMHouse)
  private
    fWorklessCount : Byte;
    fMaxCount : Byte;
    fWorklessTime : TKMCardinalArray;

    fFamiliesCount: Byte;
    fFamilyKidCount, fKidBornTime, fKidBornMaxTime: TKMCardinalArray;
    fKidTime, fKidTimeMax: TKMCardinalArray;
    fKidAge: array of TKMWorklessAge;

    fFurnitures: Byte;//amount of taken furnitures 1 pic = 40 furnitures

    function GetStageDuration(aNewKidsCount : Byte) : Integer;
    function AddKid(aFamily, aTick : Cardinal; aFromScript : Boolean = false) : byte;
    procedure GrowKid(aIndex : Integer; aTick : Cardinal; aFromScript : Boolean = false);
    function GetKidCount: Byte;
    function GetKidAge(aIndex : Integer) : TKMWorklessAge;
    function GetKidAgeCount(aAge : TKMWorklessAge) : Byte;
    procedure AddFamily;
    procedure TakeFamily(aIndex : Integer);
    function GetKidDuration(aAge : TKMWorklessAge) : Word;
    function GetMaxCount : Byte;
    property MaxCount : Byte read GetMaxCount;

  protected
    procedure Activate(aWasBuilt: Boolean); override;
  public
    procedure ProductionComplete(aTick : Cardinal); //This should shift queue filling rest with utNone
    function GetProgress(aIndex : Integer) : Single;
    function GetFamilyProgress(aIndex : Integer) : Single;
    function GetKidProgress(aAge : TKMWorklessAge; aIndex : Integer) : Single;

    procedure Save(SaveStream: TKMemoryStream); override;
    constructor Load(LoadStream: TKMemoryStream); override;
    property Workless : Byte Read fWorklessCount;
    property KidCount : Byte read GetKidCount;
    property KidAge[aIndex : Integer] : TKMWorklessAge read GetKidAge;
    property KidAgeCount[aAge : TKMWorklessAge] : Byte read GetKidAgeCount;
    property FamilyQty : Byte read fFamiliesCount;


    procedure TakeWorkless;
    procedure UpdateState(aTick: Cardinal); override;
  end;


implementation
uses
  KM_Game,
  KM_Entity,
  KM_Units,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity,
  KM_CommonUtils, KM_JSONUtils;
const
  KID_AGE_MULTIPLIER : array[TKMWorklessAge] of Single = (0.8, 1.2, 1);

procedure TKMHouseCottage.Activate(aWasBuilt: Boolean);
var I, C : Integer;
begin
  inherited;

  if HouseType = htCottage then
    fFamiliesCount := 1//fMaxCount := 5
  else
  if HouseType = htHouse then
    fFamiliesCount := 3;//fMaxCount := 20;

  if HouseType = htCottage then
    fMaxCount := 5
  else
  if HouseType = htHouse then
    fMaxCount := 20;

  SetLength(fKidBornTime, fFamiliesCount);
  SetLength(fKidBornMaxTime, fFamiliesCount);
  SetLength(fFamilyKidCount, fFamiliesCount);
  SetLength(fWorklessTime, MaxCount);
  if not gGame.Params.IsMapEditor then
    if not aWasBuilt then
    begin
      if KamRandom(100, 'TKMHouseCottage.Activate1') < 25 then
      begin
        for I := 1 to 5 do
          AddFamily;
      end;

      for I := fFamiliesCount - 1 downto 0 do
        if KamRandom(100, 'TKMHouseCottage.Activate2') < 75 then
        begin
          AddKid(I, 1, true);
          C := high(fKidAge);
          fKidAge[C] := TKMWorklessAge(KamRandom(3, 'Baby age'))

        end;
      C := KamRandom(MaxCount + 1, 'Baby age');
      for I := 1 to C do
        ProductionComplete(1);


    end;

end;

function TKMHouseCottage.GetMaxCount: Byte;
begin
  Result := fMaxCount;
  If (HouseType = htCottage) and gHands[Owner].BuildDevUnlocked(2) then
    Inc(Result, 3);
end;

procedure TKMHouseCottage.AddFamily;
begin

  if (HouseType = htCottage) and (fFamiliesCount >= 2) then Exit;
  if (HouseType = htHouse) and (fFamiliesCount >= 5) then Exit;

  Inc(fProductionCycles[4]);
  if fProductionCycles[4] mod 5 <> 0 then
    Exit;

  Inc(fFamiliesCount);
  SetLength(fKidBornTime, fFamiliesCount);
  SetLength(fKidBornMaxTime, fFamiliesCount);
  SetLength(fFamilyKidCount, fFamiliesCount);
end;

procedure TKMHouseCottage.TakeFamily(aIndex : Integer);
var I : Integer;
begin
  if fFamiliesCount = 1 then
    Exit;

  Dec(fFamiliesCount);
  for I := aIndex to High(fKidBornTime) - 1 do
  begin
    fKidBornTime[I] := fKidBornTime[I + 1];
    fKidBornMaxTime[I] := fKidBornMaxTime[I + 1];
    fFamilyKidCount[I] := fFamilyKidCount[I + 1];
  end;
  SetLength(fKidBornTime, fFamiliesCount);
  SetLength(fKidBornMaxTime, fFamiliesCount);
  SetLength(fFamilyKidCount, fFamiliesCount);
end;
//Workless is produced and is ready to train in school
procedure TKMHouseCottage.ProductionComplete(aTick : Cardinal);
var I : Integer;
begin
  Inc(fWorklessCount);
  If fFurnitures > 0 then
    fFurnitures := fFurnitures - 1;
  for I := 0 to high(fWorklessTime) do
    if fWorklessTime[I] = 99999999 then
    begin
      fWorklessTime[I] := aTick - 1;
      Break;
    end;
end;

function TKMHouseCottage.GetStageDuration(aNewKidsCount : Byte) : Integer;
var I : Integer;
  percante : Single;
begin
  if HouseType = htCottage then
    Result := 300 + (aNewKidsCount * 250)
  else
    Result := 600 + (aNewKidsCount * 250);

  percante := 1;

  for I := 1 to gHands[Owner].Stats.GetHouseQty(htCottage) do
  begin
    percante := percante - 0.1;
    if percante <= 0.7 then
      Break;
  end;

  for I := 1 to gHands[Owner].Stats.GetHouseQty(htHouse) do
  begin
    percante := percante - 0.2;
    if percante <= 0.5 then
      Break;
  end;

  If gHands[Owner].HasPearl(ptValtaria) then
    Dec(percante, 0.1);

  If gHands[Owner].BuildDevUnlocked(0) then
    Dec(percante, 0.1);


  Result := Result - 10 * fWorklessCount + KamRandom(50, 'TKMHouseCottage.GetStageDuration');
  Result := Round(Result * percante);
end;

function TKMHouseCottage.GetKidDuration(aAge: TKMWorklessAge): Word;
var I : Integer;
  percante : Single;
begin
  if HouseType = htCottage then
    Result := 700
  else
    Result := 900;

  percante := 1;

  for I := 1 to gHands[Owner].Stats.GetHouseQty(htCottage) do
  begin
    percante := percante - 0.075;
    if percante <= 0.5 then
      Break;
  end;

  for I := 1 to gHands[Owner].Stats.GetHouseQty(htHouse) do
  begin
    percante := percante - 0.15;
    if percante <= 0.4 then
      Break;
  end;

  Result := Result - 10 * fWorklessCount + KamRandom(50, 'TKMHouseCottage.GetKidDuration');
  Result := Round(Result * percante * KID_AGE_MULTIPLIER[aAge]);
  If fFurnitures > 0 then
    Result := Round(Result * 0.8);
end;


function TKMHouseCottage.GetProgress(aIndex : Integer) : Single;
begin
  Assert(aIndex < length(fKidTime));

  Result := fKidTime[aIndex] / fKidTimeMax[aIndex];

end;

function TKMHouseCottage.GetFamilyProgress(aIndex : Integer) : Single;
begin
  Assert(aIndex < length(fKidBornTime));
  if fKidBornMaxTime[aIndex] < 150 then
    Result := 0.01
  else
    Result := Max(fKidBornTime[aIndex]/ fKidBornMaxTime[aIndex], 0.01);

end;

function TKMHouseCottage.GetKidProgress(aAge : TKMWorklessAge; aIndex : Integer) : Single;
var I, id : Integer;
begin
  id := -1;
  Result := 0;
  for I := 0 to KidCount - 1 do
    if (fKidAge[I] = aAge) then
    begin
      inc(id);
      if id = aIndex then
        Exit(GetProgress(I));
    end;
    

end;

function TKMHouseCottage.GetKidCount: Byte;
begin
  Result := length(fKidTime);
end;

function TKMHouseCottage.GetKidAge(aIndex : Integer) : TKMWorklessAge;
begin
  Assert(aIndex < length(fKidTime));

  Result := fKidAge[aIndex];
end;

function TKMHouseCottage.GetKidAgeCount(aAge: TKMWorklessAge): Byte;
var I : Integer;
begin
  Result := 0;
  for I := 0 to High(fKidTime) do
    if aAge = fKidAge[I] then
      Inc(Result);
end;

procedure TKMHouseCottage.TakeWorkless;
var I : Integer;
begin
  fWorklessCount := EnsureRange(fWorklessCount - 1, 0, MaxCount);

  for I := high(fWorklessTime) downto 0 do
    if fWorklessTime[I] <> 99999999 then
    begin
      fWorklessTime[I] := 99999999;
      Break;
    end;
end;


constructor TKMHouseCottage.Load(LoadStream: TKMemoryStream);
var nCount, I : Integer;
begin
  inherited;

  LoadStream.CheckMarker('HouseCottage');

  LoadStream.Read(fWorklessCount);
  LoadStream.Read(fFamiliesCount);
  LoadArrFromStream(LoadStream, fWorklessTime);
  LoadArrFromStream(LoadStream, fKidBornTime);
  LoadArrFromStream(LoadStream, fFamilyKidCount);
  LoadArrFromStream(LoadStream, fKidBornMaxTime);
  LoadArrFromStream(LoadStream, fKidTime);
  LoadArrFromStream(LoadStream, fKidTimeMax);

  LoadStream.Read(nCount);
  SetLength(fKidAge, nCount);
  for I := 0 to nCount - 1 do
    LoadStream.Read(fKidAge[I], SizeOf(fKidAge[I]));

  LoadStream.Read(fMaxCount);

  LoadStream.Read(fFurnitures);
end;

procedure TKMHouseCottage.Save(SaveStream: TKMemoryStream);
var I : Integer;
  nCount : integer;
begin
  inherited;

  SaveStream.PlaceMarker('HouseCottage');

  SaveStream.Write(fWorklessCount);
  SaveStream.Write(fFamiliesCount);
  SaveArrToStream(SaveStream, fWorklessTime);
  SaveArrToStream(SaveStream, fKidBornTime);
  SaveArrToStream(SaveStream, fFamilyKidCount);
  SaveArrToStream(SaveStream, fKidBornMaxTime);
  SaveArrToStream(SaveStream, fKidTime);
  SaveArrToStream(SaveStream, fKidTimeMax);

  nCount := length(fKidAge);
  SaveStream.Write(nCount);
  for I := 0 to nCount - 1 do
  SaveStream.Write(fKidAge[I], SizeOf(fKidAge[I]));
  SaveStream.Write(fMaxCount);
  SaveStream.Write(fFurnitures);

end;

function TKMHouseCottage.AddKid(aFamily, aTick : Cardinal; aFromScript : Boolean = false) : Byte;
  procedure Add;
  var J : Integer;
  begin
    J := length(fKidTime);
    Inc(Result);
    SetLength(fKidTime, J + 1);
    SetLength(fKidTimeMax, J + 1);
    SetLength(fKidAge, J + 1);
    fKidTimeMax[J] := GetKidDuration(waBaby);
    fKidTime[J] := 0;
    fKidAge[J] := waBaby;
  end;
begin
  Result := 0;

  if not aFromScript then
    if CheckWareIn(wtApple) > 0 then
      WareTakeFromIn(wtApple)
    else
      Exit;

  If fFurnitures = 0 then
    If gHands[Owner].VirtualWareTake('vtFurniture') then
      fFurnitures := 40;

  Add;
  Inc(fProductionCycles[aFamily mod 3 + 1]);
  
  if fProductionCycles[aFamily mod 3 + 1] mod 3 = 0 then
    Add;
  if fProductionCycles[aFamily mod 3 + 1] mod 5 = 0 then
    Add;
  if fProductionCycles[aFamily mod 3 + 1] mod 7 = 0 then
    Add;

end;

procedure TKMHouseCottage.GrowKid(aIndex : Integer; aTick : Cardinal; aFromScript : Boolean = false);
  procedure TakeKid;
  var I : Integer;
  begin
    for I := aIndex to High(fKidAge) - 1 do
    begin
      fKidAge[I] := fKidAge[I + 1];
      fKidTime[I] := fKidTime[I + 1];
      fKidTimeMax[I] := fKidTimeMax[I + 1];
    end;
    SetLength(fKidAge, high(fKidAge));
    SetLength(fKidTime, high(fKidTime));
    SetLength(fKidTimeMax, high(fKidTimeMax));

    AddFamily;

    ProductionComplete(aTick);
  end;
begin
  Inc(fKidTime[aIndex]);
  if aFromScript then
    fKidTime[aIndex] := fKidTimeMax[aIndex];

  if fKidTime[aIndex] < fKidTimeMax[aIndex] then
    Exit;
  if fKidAge[aIndex] = waAdult then
  begin
    TakeKid;
    Exit;
  end;

  case fKidAge[aIndex] of
    waBaby: fKidAge[aIndex] := waTeenager;
    waTeenager: fKidAge[aIndex] := waAdult;
  end;

  fKidTime[aIndex] := 0;
  fKidTimeMax[aIndex] := GetKidDuration(fKidAge[aIndex]);

end;

procedure TKMHouseCottage.UpdateState(aTick: Cardinal);
var I, C : Integer;
begin
  Inherited;
  if not IsComplete then
    Exit;
  if MaxCount = 0 then Exit;


  for I := fFamiliesCount - 1 downto 0 do
  begin
    Inc(fKidBornTime[I]);

    if not (fKidBornTime[I] >= fKidBornMaxTime[I]) then
      Continue;
    fKidBornTime[I] := 0;
    if KidCount + fWorklessCount >= MaxCount then
      fKidBornMaxTime[I] := 100 + KaMRandom(50, 'TKMHouseCottage.UpdateState')
    else
    begin
      if fKidBornMaxTime[I] < 200 then
      begin
        fKidBornMaxTime[I] := GetStageDuration(1);
        Continue;
      end;
      C := AddKid(I, aTick);
      Inc(fFamilyKidCount[I], C);
      If C > 0 then
        fKidBornMaxTime[I] := GetStageDuration(C)
      else
        fKidBornMaxTime[I] := 100;

      if fFamilyKidCount[I] >= 10 then
        TakeFamily(I);

    end;
  end;

  for I := high(fKidTime) downto 0 do
    GrowKid(I, aTick);

  for I := 0 to High(fWorklessTime) do
    if fWorklessTime[I] <> 99999999 then
      if (aTick - fWorklessTime[I]) mod 22000 = 0 then
      begin
        if CheckWareIn(wtApple) = 0 then
          TakeWorkless
        else
          WareTakeFromIn(wtApple, 1);
      end;
end;


end.
