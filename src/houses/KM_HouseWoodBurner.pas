unit KM_HouseWoodBurner;
{$I KaM_Remake.inc}
interface
uses
  Math,
  KM_CommonClasses, KM_Defaults,
  KM_Houses,
  KM_ResTypes;


type
  //School has one unique property - queue of units to be trained, 1 wip + 5 in line
  TKMHouseWoodBurner = class(TKMHouse)
  private
    fFuelTime : Cardinal;
    fFuel : Single;
    fWoodBurned : array[1..5] of Single;
    function BurnsSomething : Boolean;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure UpdateState(aTick : Cardinal); override;
    function GetFreeSlot : Integer;
    Procedure StartBurning;
    Function FuelLevel : Single;
    function BurningProgress(aID : integer) : Single;

  end;
implementation
uses
  KM_Entity,
  KM_Units, KM_CommonUtils,
  KM_HandsCollection, KM_Hand, KM_HandTypes, KM_HandEntity, KM_GameInfo, KM_UnitWarrior, KM_ResTexts;

Const WOOD_BURN_STEP = 0.008;
{ TKMHouseSign }
constructor TKMHouseWoodBurner.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var I : Integer;
begin
  inherited;
  fFuel := 0;
  for I := low(fWoodBurned) to high(fWoodBurned) do
    fWoodBurned[I] := 0;


end;

constructor TKMHouseWoodBurner.Load(LoadStream: TKMemoryStream);
var I : Integer;
begin
  inherited;

  LoadStream.CheckMarker('HouseWoodBurner');
  for I := low(fWoodBurned) to high(fWoodBurned) do
    LoadStream.Read(fWoodBurned[I]);

  LoadStream.Read(fFuel);
  LoadStream.Read(fFuelTime);

end;

procedure TKMHouseWoodBurner.Save(SaveStream: TKMemoryStream);
var I : Integer;
begin
  inherited;
  SaveStream.PlaceMarker('HouseWoodBurner');
  for I := low(fWoodBurned) to high(fWoodBurned) do
    SaveStream.Write(fWoodBurned[I]); //Store ID, then substitute it with reference on SyncLoad
  SaveStream.Write(fFuel);
  SaveStream.Write(fFuelTime);
end;

procedure TKMHouseWoodBurner.UpdateState(aTick : Cardinal);
var I : Integer;
const
  BURNING_DIFFERENCE : array[1..5] of Integer = (100, 100, 85, 70, 45);
begin
  inherited;
  if not IsComplete then
    Exit;
  if not IsValid then
    Exit;
  If not (aTick mod 10 = 0) then
    Exit;

  if (fFuel > 0) or ((fFuelTime <> 99999999) and (fFuelTime > aTick)) then
    CurrentAction.SubActionAdd([haSmoke])
  else
    CurrentAction.SubActionRem([haSmoke]);

  if fFuel > 0 then
  begin
    fFuel := fFuel - 0.002;
    fFuelTime := aTick + 300;
  end;

  if fFuelTime < aTick then
  begin
    for I := low(fWoodBurned) to high(fWoodBurned) do
      fWoodBurned[I] := 0;
    fFuelTime := 99999999;
  end;


  for I := low(fWoodBurned) to high(fWoodBurned) do
    if fWoodBurned[I] > 0 then
      if (fFuel > 0) or ((fFuelTime <> 99999999) and (fFuelTime > aTick)) then
      begin
        if KamRandom(100, 'TKMHouseWoodBurner.UpdateState') < BURNING_DIFFERENCE[I]  then
          fWoodBurned[I] := fWoodBurned[I] + WOOD_BURN_STEP;

        if fWoodBurned[I] >= 1 then
        begin
          fWoodBurned[I] := 0;
          case I of
            3:  ProduceWare(wtCoal, 2);
            4:  ProduceWare(wtCoal, 3);
            5:    ProduceWare(wtCoal, 4);
            else ProduceWare(wtCoal);
          end;

        end;

      end;


  if not HasWorkerInside then Exit;

  if fFuel <= 0 then
    if BurnsSomething then
      if CheckWareOut(wtCoal) < 5 then
      begin
        if CheckWareIn(wtCorn) = 0 then
          Exit;

        ProduceWare(wtCorn, -1);
        fFuel := fFuel + 0.05;
        //First go for trunk
        if CheckWareIn(wtSawDust) > 0 then
        begin
          fFuel := fFuel + 0.35;
          ProduceWare(wtSawDust, -1);
        end else
        if CheckWareIn(wtCoal) > 0 then //then take coal if trunk not found
        begin
          fFuel := fFuel + 1;
          ProduceWare(wtCoal, -1);
        end;

      end;
end;

function TKMHouseWoodBurner.GetFreeSlot: Integer;
var I : Integer;
begin
  Result := -1;
  if CheckWareOut(wtCoal) >= 5 then Exit;
  
  for I := low(fWoodBurned) to high(fWoodBurned) do
    if fWoodBurned[I] < WOOD_BURN_STEP then
      Exit(I);

end;

procedure TKMHouseWoodBurner.StartBurning;
var I : Integer;
begin
  if CheckWareOut(wtCoal) >= 5 then Exit;

  for I := low(fWoodBurned) to high(fWoodBurned) do
    if fWoodBurned[I] = 0 then
    begin
      fWoodBurned[I] := WOOD_BURN_STEP;
      Exit;
    end;
end;

function TKMHouseWoodBurner.FuelLevel: Single;
begin
  Result := fFuel;
end;

function TKMHouseWoodBurner.BurningProgress(aID: Integer): Single;
begin
  Result := fWoodBurned[aID];
end;

function TKMHouseWoodBurner.BurnsSomething: Boolean;
var I : Integer;
begin
  Result := false;
  for I := low(fWoodBurned) to high(fWoodBurned) do
    if fWoodBurned[I] > 0 then
      Exit(true);

end;


end.
