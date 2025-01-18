unit KM_GameDefines;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults,
  KM_CommonClasses,
  KM_ResTypes;

type
  TKMGameResource = record
    SkipCivilians : Boolean;
    SkipWater : Boolean;
    IsTSK, IsTPR : Boolean;
    procedure SetDefault;
  end;

  TKMGameResources = class
  private
    fGameRes : TKMGameResource;
  public
    property IsTSK : Boolean read fGameRes.IsTSK;
    property IsTPR : Boolean read fGameRes.IsTPR;
    function IsTSKorTPR : Boolean;
    property SkipCivilians : Boolean read fGameRes.SkipCivilians;
    property SkipWater : Boolean read fGameRes.SkipWater;

    constructor Create;
    procedure SetDefault;
    procedure ApplyAfterStart;
    procedure LoadFromJson(aFolderPath : String);
    procedure Load(LoadStream : TKMemoryStream);
    procedure Save(SaveStream : TKMemoryStream);
  end;

implementation
uses
  SysUtils,
  KM_JSONUtils, KM_JsonHelpers,
  KM_Resource, KM_ResMapElements,
  KM_HandTypes,KM_Hand, KM_HandsCollection;

constructor TKMGameResources.Create;
begin
  Inherited;
  SetDefault;
end;

procedure TKMGameResources.SetDefault;
begin
  fGameRes.SetDefault;
end;

procedure TKMGameResources.LoadFromJson(aFolderPath: string);
var nRoot : TKMJson;
  nO : TKMJson;
begin
  aFolderPath := ExtractFilePath(aFolderPath) + 'defines' + PathDelim + 'GameResources.json';
  if not FileExists(aFolderPath) then
    Exit;
  nRoot := TKMJson.Create(aFolderPath);
  if nRoot.Contains('GameSettings') then
  begin
    nO := nRoot.O['GameSettings'];
    nO.SetIfContains('SkipCivilians', fGameRes.SkipCivilians);
    nO.SetIfContains('SkipWater', fGameRes.SkipWater);
    nO.SetIfContains('IsTSK', fGameRes.IsTSK);
    nO.SetIfContains('IsTPR', fGameRes.IsTPR);
  end;
  nRoot.Free;
end;

procedure TKMGameResources.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fGameRes, SizeOf(fGameRes));
end;

procedure TKMGameResources.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fGameRes, SizeOf(fGameRes));
end;


procedure TKMGameResources.ApplyAfterStart;
  procedure HideHouse(aPlayer : Integer; aHouse : TKMHouseType);overload;
  begin
    gHands[aPlayer].Locks.HouseLock[aHouse] := hlNotVisible;
  end;
  procedure HideHouse(aPlayer : Integer; aHouses : TKMHouseTypeSet); overload;
  var HT : TKMHouseType;
  begin
    for HT in aHouses do
      HideHouse(aPlayer, HT);
  end;

  procedure HideUnit(aPlayer : Integer; aUnit : TKMUnitType); overload;
  begin
    gHands[aPlayer].Locks.SetUnitBlocked(aUnit, htAny, ulNotVisible);
  end;
  procedure HideUnit(aPlayer : Integer; aUnits : TKMUnitTypeSet); overload;
  var UT : TKMUnitType;
  begin
    for UT in aUnits do
      HideUnit(aPlayer, UT);
  end;

var I, K : Integer;
begin
  if SkipWater then  
    for I := 0 to gHands.Count - 1 do
      if gHands[I].Enabled then
        HideHouse(I, htWell);

  if SkipCivilians then
    for I := 0 to gHands.Count - 1 do
      if gHands[I].Enabled then
      begin
        HideHouse(I, htCottage);
        HideHouse(I, htHouse);
        gHands[I].Workless := high(word) div 2;
      end;

  if IsTSK then
  begin
    for I := 0 to gHands.Count - 1 do
      if gHands[I].Enabled then
      begin
        //HideHouse(I, htPottery);
        HideHouse(I, [htWall,            htWall2,           htWall3,         htWall4,         htWall5,
                      htHovel,           htSign,            htBitinMine,     htWallTower,     htWell,
                      htStoneWorkshop,   htIronFoundry,     htMerchant,      htWoodBurner,
                      htAppleTree,       htSmallStore,      htCollectors,    htTailorsShop,   htCottage,
                      htHouse,           htPalace,          htStall,         htProductionThatch,
                      htShipYard,        htTownHall,        htSiegeWorkshop]);

        for K := 0 to gRes.Structures.Count - 1 do
          gHands[I].Locks.Structures[K] := ulNotVisible;
        for K := 0 to high(gDecorations) do
          gHands[I].Locks.Decoration[K] := ulNotVisible;

        HideUnit(I,   [utRebel,        utRogue,       utWarrior,       utVagabond,
                      utCatapult,     utBallista,     utRam,           utGolem,
                      utGiant,        utPaladin,      utArcher,        utSpy,
                      utTrainedWolf,  utAmmoCart,     utPikeMachine,   utShip,
                      utClubMan,      utMaceFighter,  utFlailFighter,  utShieldBearer,
                      utFighter,      utSpikedTrap,   utWoodenWall,    utTorchMan,
                      utMedic,        utBattleShip,   utBoat,          utPyro,
                      utLekter]);

        gHands[I].Locks.SetFieldLocked(lftPalisade, true);
        gHands[I].Locks.SetFieldLocked(lftGrassField, true);
        gHands[I].Locks.SetFieldLocked(lftVegetablesField, true);
      end;
  end;
end;



procedure TKMGameResource.SetDefault;
begin
  SkipCivilians := false;
  SkipWater := false;
  IsTSK := false;
  IsTPR := false;
end;

function TKMGameResources.IsTSKorTPR: Boolean;
begin
  Result := IsTSK or IsTPR;
end;

end.

