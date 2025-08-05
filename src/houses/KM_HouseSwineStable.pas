unit KM_HouseSwineStable;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses,
  KM_ResTypes,
  KM_CommonClasses,
  KM_CommonTypes;

type
  // SwineStable has unique property - it needs to accumulate some resource before production begins, also special animation
  TKMHouseSwineStable = class(TKMHouse)
  private
    BeastAge: array[1..5] of Single; //Each beasts "age". Once Best reaches age 3+1 it's ready
    HayBreeded : array[1..5] of Single;
    BeastLastFeadTime : array[1..5] of Cardinal;
    fPigsProgress : Single;
  protected
    procedure MakeSound; override;
    function GetBeastAge(aIndex : Integer) : Single;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    function FeedBeasts(aFood : TKMWarePlan): Byte;
    function TakeBeast : Byte;
    property Beast[aIndex : Integer] : Single read GetBeastAge;
    function GetBeastsProgresses : TSingleArray;
    function GetBeastsProgressColors : TKMCardinalArray;
    function GetPigsCount : Byte;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
  end;

  TKMHouseHovel = class(TKMHouse)
  private
    fLast : byte;
    fSausage : Boolean;
    fFillFeather, fFillEggs : Single;
    BeastAge: array[1..3]of Single; //Each beasts "age". Once Best reaches age 3+1 it's ready
    BeastLastFeadTime : array[1..3] of Cardinal;
    CornBreeded : array[1..3] of Byte;

    BeastAnim: array[1..3]of byte;
    function GetBeastAge(aIndex : Integer) : Single;
  protected
    //procedure MakeSound; override;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    function FeedChicken(aFood : TKMWarePlan) : Byte;
    function TakeChicken(aID : Integer) : Byte;
    function ChickenCount : Byte;
    property GetsSausage : Boolean read fSausage;
    property Beast[aIndex : Integer] : Single read GetBeastAge;
    property FeathersProgress : Single read fFillFeather;
    property EggsProgress : Single read fFillEggs;
    function GetBeastsProgresses : TSingleArray;
    function GetBeastsProgressColors : TKMCardinalArray;
    function CanFeedWithWare(aWare : TKMWareType) : Boolean;
    procedure MakeFeathers;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick: Cardinal); override;
    procedure Paint; override;
  end;
const
  HOVEL_MAX_AGE = 5;
  DEAD_BEAST_TIME = 6000; //10 minutes
implementation
uses
  Math,
  KM_Sound,
  KM_ResSound,
  KM_GameParams, KM_CommonHelpers,
  KM_Hand, KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_RenderPool,
  KM_Defaults, KM_CommonUtils;


{TKMHouseSwineStable}
constructor TKMHouseSwineStable.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseSwineStable');
  LoadStream.Read(BeastAge, SizeOf(BeastAge));
  LoadStream.Read(HayBreeded, SizeOf(HayBreeded));
  LoadStream.Read(BeastLastFeadTime, SizeOf(BeastLastFeadTime));
  LoadStream.Read(fPigsProgress);
end;


//Return ID of beast that has grown up
function TKMHouseSwineStable.FeedBeasts(aFood : TKMWarePlan): Byte;

  procedure FeedAnotherBeast(aFoodM : Single);
  var I : Integer;
  begin

    for I := Low(BeastAge) to High(BeastAge) do
    if (BeastAge[I] < 4) then
    begin
      if (BeastAge[I] + aFoodM > 4) then
      begin
        aFoodM := (BeastAge[I] + aFoodM) - 4;
        BeastAge[I] := 4;
        FeedAnotherBeast(aFoodM);
      end
      else
        Inc(BeastAge[I], aFoodM);
      Exit;
    end;

  end;
var foodMulti, hayMulti : Single;
begin
  Result := KaMRandom(5, 'TKMHouseSwineStable.FeedBeasts') + 1;

  hayMulti := 0;

  if HouseType = htSwine then
  begin
    foodMulti := 1;
    if aFood.HasWares([wtVegetables]) then
      inc(hayMulti, 0.45);
    if aFood.HasWares([wtCorn]) then
      inc(hayMulti, 0.30);
    if aFood.HasWares([wtHay]) then
      inc(hayMulti, 0.25);
  end else
  begin
    foodMulti := 0;
    if aFood.HasWares([wtVegetables]) then
      inc(hayMulti, 0.25);
    if aFood.HasWares([wtCorn]) then
      inc(hayMulti, 0.35);
    if aFood.HasWares([wtHay]) then
      inc(hayMulti, 0.45);
  end;



  {if (aFood.HasWare(wtCorn) > 0) and (aFood.HasWare(wtHay) = 0) then //has corn but not hay
    Inc(foodMulti, 1)
  else
  if (aFood.HasWare(wtCorn) = 0) and (aFood.HasWare(wtHay) > 0) then //has hay but not corn
    Inc(foodMulti, 1)
  else begin
    //has both hay and corn
    if aFood.HasWare(wtCorn ) > 0 then
      Inc(foodMulti, 1); //2

    if aFood.HasWare(wtHay) > 0 then
      Inc(hayMulti, 0.75); //2


  end;

  if aFood.HasWare(wtVegetables) > 0 then
    Inc(hayMulti, 0.5); //vegetables can increase only hay

  if aFood.HasWares([wtCorn, wtHay, wtVegetables]) then  //if they get all types of food, add extra
    Inc(hayMulti, 0.25);}

  if HouseType = htStables then
  begin
    IF gHands[Owner].EconomyDevUnlocked(11) then
      Inc(foodMulti, 0.5);

    if BeastAge[Result] + foodMulti + hayMulti > 4 then
    begin
      foodMulti := (BeastAge[Result] + foodMulti + hayMulti) - 4;
      BeastAge[Result] := 4;
      FeedAnotherBeast(foodMulti);
    end else
    begin
      Inc(BeastAge[Result], foodMulti);
      Inc(BeastAge[Result], hayMulti);//in stables horses are growing faster
    end;
  end else
  begin
    if BeastAge[Result] + foodMulti > 4 then
    begin
      foodMulti := (BeastAge[Result] + foodMulti) - 4;
      BeastAge[Result] := 4;
      Inc(HayBreeded[Result], hayMulti);//swines are getting fat
      FeedAnotherBeast(foodMulti);
    end else
    begin
      Inc(BeastAge[Result], foodMulti);
      Inc(HayBreeded[Result], hayMulti);//swines are getting fat
    end;
    //Inc(fPigsProgress, hayMulti);

  end;
  BeastLastFeadTime[Result] := Tick + DEAD_BEAST_TIME;
end;

function TKMHouseSwineStable.TakeBeast : Byte;
var I : Integer;
begin
  Result := 0;
  fPigsProgress := fPigsProgress - trunc(fPigsProgress);
  for I := Low(BeastAge) to High(BeastAge) do
    if (BeastAge[I]>3) then
    begin
      Inc(Result);
      BeastAge[I] := 0;
      Inc(fPigsProgress, (HayBreeded[I]) - trunc(HayBreeded[I]) );
      HayBreeded[I] := 0;
    end;
  //if (aID<>0) and (BeastAge[aID]>3) then
  //  BeastAge[aID] := 0;


end;

function TKMHouseSwineStable.GetPigsCount: Byte;
var I : Integer;
begin
  Result := 0;
  for I := Low(BeastAge) to High(BeastAge) do
    if (BeastAge[I]>3) then
    begin
      //fPigsProgress := fPigsProgress + 3;
      //Inc(Result, 1);
      Inc(Result, trunc(HayBreeded[I]));
    end;
  if Result > 0 then
    Result := Result + Trunc(fPigsProgress); //you get 3 meat from 1 swine
end;

function TKMHouseSwineStable.GetBeastsProgressColors: TKMCardinalArray;
var I : Integer;
begin
  SetLength(Result, 5);
  if HouseType = htStables then
  begin
    for I := 0 to High(Result) do
      Result[I] := icLightGreen;
  end
  else
    for I := 0 to High(Result) do
      Result[I] := MixColor(icLightGreen, icLightRed, HayBreeded[I + 1] / 3);
end;

function TKMHouseSwineStable.GetBeastsProgresses: TSingleArray;
var I : Integer;
begin
  SetLength(Result, 5);

    for I := 0 to High(Result) do
      Result[I] := BeastAge[I + 1] / 3;
end;


//Make beast noises - each beast makes a noise (if it exists) with two second pauses between each one
procedure TKMHouseSwineStable.MakeSound;
var
  I: Integer;
begin
  inherited;

  if gMySpectator.FogOfWar.CheckTileRevelation(fPosition.X, fPosition.Y) < 255 then Exit;

  for I := 0 to 4 do
  if BeastAge[I+1] > 0 then
  if (FlagAnimStep + 20*I) mod 100 = 0 then
  begin
    if HouseType = htStables then
      gSoundPlayer.Play(TSoundFX(byte(sfxHorse1) + Random(4)), fPosition); //sfxHorse1..sfxHorse4
    if HouseType = htSwine   then
      gSoundPlayer.Play(TSoundFX(byte(sfxPig1)   + Random(4)), fPosition); //sfxPig1..sfxPig4
  end;
end;

function TKMHouseSwineStable.GetBeastAge(aIndex: Integer): Single;
begin
  Assert(InRange(aIndex, 1, 5));

  Result := EnsureRange(BeastAge[aIndex] / 3, 0, 1);
end;

procedure TKMHouseSwineStable.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseSwineStable');
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
  SaveStream.Write(HayBreeded, SizeOf(HayBreeded));
  SaveStream.Write(BeastLastFeadTime, SizeOf(BeastLastFeadTime));
  SaveStream.Write(fPigsProgress);
end;


procedure TKMHouseSwineStable.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  Inherited;
  if not gGameParams.MBD.IsRealism then
    Exit;
  for I := low(BeastAge) to high(BeastAge) do
    if BeastAge[I] >= 0 then
      if aTick >= BeastLastFeadTime[I] then
      begin
        BeastAge[I] := 0;
        HayBreeded[I] := 0;
      end;
end;

procedure TKMHouseSwineStable.Paint;
var
  I: Integer;
begin
  inherited;

  //We render beasts on top of the HouseWork (which is mostly flames in this case), because otherwise
  //Swinefarm looks okay, but Stables are totaly wrong - flames are right on horses backs!
  if fBuildState = hbsDone then
    for I := 1 to 5 do
      if BeastAge[I] > 0 then
        gRenderPool.AddHouseStableBeasts(HouseType, fPosition, I, Round(EnsureRange(BeastAge[I],1, 3)), FlagAnimStep);
  if HouseType = htSwine then
    gRenderPool.AddHouseSupply(HouseType, fPosition, [0, 0, 0, CheckWareIn(wtWater)], [], [])
  else
    gRenderPool.AddHouseSupply(HouseType, fPosition, [0, CheckWareIn(wtWater), 0, 0], [], []);
  //But Animal Breeders should be on top of beasts
  if CurrentAction <> nil then
    gRenderPool.AddHouseWork(HouseType, fPosition,
                            CurrentAction.SubAction * [haWork1, haWork2, haWork3, haWork4, haWork5],
                            WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);
end;


{TKMHouseSwineStable}
constructor TKMHouseHovel.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.CheckMarker('HouseHovel');
  LoadStream.Read(BeastAge, SizeOf(BeastAge));
  LoadStream.Read(fLast);
  LoadStream.Read(fSausage);
  LoadStream.Read(fFillFeather);
  LoadStream.Read(fFillEggs);
  LoadStream.Read(CornBreeded, SizeOf(CornBreeded));
  LoadStream.Read(BeastLastFeadTime, SizeOf(BeastLastFeadTime));
  LoadStream.Read(BeastAnim, SizeOf(BeastAnim));
end;

function TKMHouseHovel.TakeChicken(aID: Integer) : Byte;
begin
  Assert(InRange(aID, 1, 3));
  Result := 1;
  BeastAge[aID] := 0;
  CornBreeded[aID] := 0;
  fSausage := false;
end;

function TKMHouseHovel.FeedChicken(aFood : TKMWarePlan) : Byte;
var I : Integer;
  cornAdded : Boolean;
begin
  Result := 0;

  //find empty space
  {J := 0;
  for I := 1 to 3 do
    if BeastAge[I] = 0 then
    begin
      J := I;
      Break;
    end;}


  //fLast := fLast mod 3 + 1;
  fLast :=  KaMRandom(3, 'TKMHouseHovel.FeedChicken:Hovel random anim1') + 1;//it's full so feed random chicken
  if BeastAge[fLast] = 0 then
    BeastAnim[fLast] := KaMRandom(3, 'TKMHouseHovel.FeedChicken:Hovel random anim2');

  BeastLastFeadTime[fLast] := Tick + DEAD_BEAST_TIME;

  if aFood.HasWares([wtSeed]) then
  begin
    Inc(BeastAge[fLast], 0.4);
    Inc(fFillEggs, 0.1);
  end;

  if aFood.HasWares([wtVegetables]) then
    Inc(BeastAge[fLast], 0.2);
  if aFood.HasWares([wtSeed, wtVegetables]) then
    Inc(BeastAge[fLast], 0.1);


  if aFood.HasWares([wtCorn]) then
  begin
    cornAdded := false;
    if CornBreeded[fLast] = 0 then
    begin
      Inc(CornBreeded[fLast]);
      cornAdded := true;
    end else
      for I := Low(BeastAge) to High(BeastAge) do
        if (BeastAge[I] > 0) and (CornBreeded[I] = 0) then
        begin
          Inc(CornBreeded[I]);
          cornAdded := true;
          Break;
        end;

    if not cornAdded then //no chicken found that has taken 0 corn
      Inc(CornBreeded[fLast]);
  end;


  fSausage := false;
  if BeastAge[fLast] >= HOVEL_MAX_AGE then
  begin
    //BeastAge[fLast] := 0;
    Result := fLast;
    fSausage := true;
  end;


end;


function TKMHouseHovel.ChickenCount: Byte;
var I : Integer;
begin
  Result := 0;
  for I := 1 to 3 do
    if BeastAge[I] > 0 then
      Inc(Result);
end;

function TKMHouseHovel.GetBeastAge(aIndex: Integer): Single;
begin
  Assert(InRange(aIndex, 1, 3));

  Result := EnsureRange(BeastAge[aIndex] / HOVEL_MAX_AGE, 0, 1);
end;

function TKMHouseHovel.GetBeastsProgresses: TSingleArray;
begin
  SetLength(Result, 3);
  Result[0] := BeastAge[1] / HOVEL_MAX_AGE;
  Result[1] := BeastAge[2] / HOVEL_MAX_AGE;
  Result[2] := BeastAge[3] / HOVEL_MAX_AGE;
end;

function TKMHouseHovel.GetBeastsProgressColors: TKMCardinalArray;
var I : Integer;
begin
  SetLength(Result, 3);

  for I := Low(CornBreeded) to High(CornBreeded) do
    if CornBreeded[I] > 0 then
      Result[I - 1] := icRed
    else
      Result[I - 1] := icLightGreen
end;

function TKMHouseHovel.CanFeedWithWare(aWare: TKMWareType): Boolean;
var I : Integer;
begin
  if aWare = wtCorn then
  begin
    Result := false;
    for I := Low(BeastAge) to High(BeastAge) do
      if (BeastAge[I] > 0) and (CornBreeded[I] = 0) then
        Exit(true);


  end else
    Result := true;

end;

procedure TKMHouseHovel.MakeFeathers;
var I : Integer;
begin
  for I := Low(BeastAge) to High(BeastAge) do
    if BeastAge[I] > 0 then
    begin
      Inc(fFillFeather, 0.1);

      Inc(fFillFeather, CornBreeded[I] * 0.05);
    end;

  fFillFeather := fFillFeather + (0.1) * ChickenCount;

  ProduceWareFromFill(wtFeathers, fFillFeather);
  ProduceWareFromFill(wtEgg, fFillEggs);

 { count := Trunc(fFillFeather);

  if count > 0 then
  begin
    ProduceWare(wtFeathers, count);
    IncProductionCycle(wtFeathers, count);
  end;

  fFillFeather := fFillFeather - count;}

end;

procedure TKMHouseHovel.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseHovel');
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
  SaveStream.Write(fLast);
  SaveStream.Write(fSausage);
  SaveStream.Write(fFillFeather);
  SaveStream.Write(fFillEggs);
  SaveStream.Write(CornBreeded, SizeOf(CornBreeded));
  SaveStream.Write(BeastLastFeadTime, SizeOf(BeastLastFeadTime));
  SaveStream.Write(BeastAnim, SizeOf(BeastAnim));

end;

procedure TKMHouseHovel.UpdateState(aTick: Cardinal);
var I : Integer;
begin
  Inherited;
  if not gGameParams.MBD.IsRealism then
    Exit;
  for I := low(BeastAge) to high(BeastAge) do
    if BeastAge[I] >= 0 then
      if aTick > BeastLastFeadTime[I] + DEAD_BEAST_TIME then
      begin
        BeastAge[I] := 0;
        CornBreeded[I] := 0;
      end;
end;


procedure TKMHouseHovel.Paint;
var
  I: Integer;
begin
  inherited;

  //We render beasts on top of the HouseWork (which is mostly flames in this case), because otherwise
  //Swinefarm looks okay, but Stables are totaly wrong - flames are right on horses backs!
  if fBuildState = hbsDone then
    for I := 1 to 3 do
      if BeastAge[I] > 0 then
        gRenderPool.AddHouseStableBeasts(HouseType, fPosition, I, BeastAnim[I] + 1, FlagAnimStep + I * 10);

  //But Animal Breeders should be on top of beasts
  if CurrentAction <> nil then
    gRenderPool.AddHouseWork(HouseType, fPosition,
                            CurrentAction.SubAction * [haWork1, haWork2, haWork3, haWork4, haWork5],
                            WorkAnimStep, WorkAnimStepPrev, gHands[Owner].GameFlagColor);
end;


end.
