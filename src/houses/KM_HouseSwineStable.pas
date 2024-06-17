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
    BeastAge: array[1..5] of byte; //Each beasts "age". Once Best reaches age 3+1 it's ready
    HayBreeded : array[1..5] of byte;
    fPigsProgress : Single;
  protected
    procedure MakeSound; override;
    function GetBeastAge(aIndex : Integer) : Single;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    function FeedBeasts: Byte;
    procedure FeedBeastsHay(aID : Byte);
    function TakeBeast : Byte;
    property Beast[aIndex : Integer] : Single read GetBeastAge;
    function GetBeastsProgresses : TSingleArray;
    function GetBeastsProgressColors : TKMCardinalArray;
    function GetPigsCount : Byte;

    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override;
  end;

  TKMHouseHovel = class(TKMHouse)
  private
    fLast : byte;
    fSausage : Boolean;
    BeastAge: array[1..3]of byte; //Each beasts "age". Once Best reaches age 3+1 it's ready
    BeastAnim: array[1..3]of byte;
    function GetBeastAge(aIndex : Integer) : Single;
  protected
    //procedure MakeSound; override;
  public
    constructor Load(LoadStream: TKMemoryStream); override;
    function FeedChicken : Byte;
    procedure TakeChicken(aID : Integer);
    function ChickenCount : Byte;
    property GetsSausage : Boolean read fSausage;
    property Beast[aIndex : Integer] : Single read GetBeastAge;
    function GetBeastsProgresses : TSingleArray;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override;
  end;
implementation
uses
  Math,
  KM_Sound,
  KM_ResSound,
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
end;


//Return ID of beast that has grown up
function TKMHouseSwineStable.FeedBeasts: Byte;
begin
  Result := KaMRandom(5, 'TKMHouseSwineStable.FeedBeasts') + 1;
  Inc(BeastAge[Result]); //Let's hope it never overflows MAX
  {for I := 1 to Length(BeastAge) do
    if BeastAge[I] > 3 then
      Result := I;}
end;

procedure TKMHouseSwineStable.FeedBeastsHay(aID : Byte);
var
  I, K: Integer;
begin
  if HouseType = htSwine then
  begin
    Inc(HayBreeded[aID]);
    fPigsProgress := fPigsProgress + 0.25;
  end else
  begin
    if BeastAge[aID] <= 3 then
      Inc(BeastAge[aID])
    else
    begin
      //feed new beasts if aID is already at age max
      I := KaMRandom(5, 'TKMHouseSwineStable.FeedBeasts') + 1;
      K := 0;
      while (BeastAge[I] > 3) and (K < 10) do //try max 10 times
      begin
        Inc(K);
        I := KaMRandom(5, 'TKMHouseSwineStable.FeedBeasts') + 1;
      end;
      if (BeastAge[I] > 3) then
        Exit;
      Inc(BeastAge[I]);
    end;
  end;
end;


function TKMHouseSwineStable.TakeBeast : Byte;
var I : Integer;
begin
  Result := 0;
  for I := Low(BeastAge) to High(BeastAge) do
    if (BeastAge[I]>3) then
    begin
      Inc(Result);
      BeastAge[I] := 0;
      HayBreeded[I] := 0;
    end;
  fPigsProgress := fPigsProgress - trunc(fPigsProgress);
  //if (aID<>0) and (BeastAge[aID]>3) then
  //  BeastAge[aID] := 0;


end;

function TKMHouseSwineStable.GetPigsCount: Byte;
var I : Integer;
begin
 // Result := 0;
  {for I := Low(BeastAge) to High(BeastAge) do
    if (BeastAge[I]>3) then
    begin
      //fPigsProgress := fPigsProgress + 1 + HayBreeded[I] / 3;
      //Inc(Result, HayBreeded[I] div 2);
    end;}
  Result := Trunc(fPigsProgress);
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

  Result := BeastAge[aIndex] / 3;
end;

procedure TKMHouseSwineStable.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseSwineStable');
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
  SaveStream.Write(HayBreeded, SizeOf(HayBreeded));
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
        gRenderPool.AddHouseStableBeasts(HouseType, fPosition, I, Min(BeastAge[I],3), FlagAnimStep);

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
end;

procedure TKMHouseHovel.TakeChicken(aID: Integer);
begin
  Assert(InRange(aID, 1, 3));
  BeastAge[aID] := 0;
  fSausage := false;
end;

function TKMHouseHovel.FeedChicken : Byte;
var I, J : Integer;
begin
  Result := 0;

  //find empty space
  J := 0;
  for I := 1 to 3 do
    if BeastAge[I] = 0 then
    begin
      J := I;
      Break;
    end;


  //fLast := fLast mod 3 + 1;
  if J = 0 then
    fLast :=  KaMRandom(3, 'TKMHouseHovel.FeedChicken:Hovel random anim1') + 1//it's full so feed random chicken
  else
    fLast := J;//empty space found so add new chicken
  if BeastAge[fLast] = 0 then
    BeastAnim[fLast] := KaMRandom(3, 'TKMHouseHovel.FeedChicken:Hovel random anim2');

  Inc(BeastAge[fLast]);
  fSausage := false;
  if BeastAge[fLast] >= 4 then
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

  Result := BeastAge[aIndex] / 4;
end;

function TKMHouseHovel.GetBeastsProgresses: TSingleArray;
begin
  SetLength(Result, 3);
  Result[0] := BeastAge[1] / 4;
  Result[1] := BeastAge[2] / 4;
  Result[2] := BeastAge[3] / 4;
end;
procedure TKMHouseHovel.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  SaveStream.PlaceMarker('HouseHovel');
  SaveStream.Write(BeastAge, SizeOf(BeastAge));
  SaveStream.Write(fLast);
  SaveStream.Write(fSausage);
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
