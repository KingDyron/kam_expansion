unit KM_WareDistribution;
{$I KaM_Remake.inc}
interface
uses
  KM_ResTypes,
  KM_CommonClasses;

type
  TKMWareDistribution = class
  private
    fWareDistribution: TKMWareDistributionType;
    procedure SetWareDistribution(aWare: TKMWareType; aHouse: TKMHouseType; aValue: Byte);
    function GetWareDistribution(aWare: TKMWareType; aHouse: TKMHouseType): Byte;
  public
    constructor Create;
    property WareDistribution[aWare: TKMWareType; aHouse: TKMHouseType]: Byte read GetWareDistribution write SetWareDistribution; default;
    procedure LoadFromStr(aString: String);
    function PackToStr: String;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure LoadDefault;
  end;


implementation
uses
  SysUtils, Math, KM_Resource;

{TKMWareDistribution}
constructor TKMWareDistribution.Create;
begin
  Inherited;
  //fWareDistribution := gRes.Wares.WareDistribution;
end;


procedure TKMWareDistribution.SetWareDistribution(aWare: TKMWareType; aHouse: TKMHouseType; aValue: Byte);
var I, K : Integer;
begin
  for I := 0 to High(fWareDistribution) do
    if fWareDistribution[I].WareType = aWare then
      for K := 0 to High(fWareDistribution[I].Houses) do
        if fWareDistribution[I].Houses[K].House = aHouse then
          fWareDistribution[I].Houses[K].Qty := aValue;



  {case aWare of
    wtIron:  if aHouse = htWeaponSmithy   then fWareDistribution[1,1] := aValue else
              if aHouse = htArmorSmithy    then fWareDistribution[1,2] := aValue else
              if aHouse = htIronFoundry    then fWareDistribution[1,3] := aValue;

    wtCoal:   if aHouse = htIronSmithy     then fWareDistribution[2,1] := aValue else
              if aHouse = htMetallurgists  then fWareDistribution[2,2] := aValue else
              if aHouse = htWeaponSmithy   then fWareDistribution[2,3] := aValue else
              if aHouse = htArmorSmithy    then fWareDistribution[2,4] := aValue else
              if aHouse = htIronFoundry    then fWareDistribution[2,5] := aValue;

    wtTimber:   if aHouse = htArmorWorkshop  then fWareDistribution[3,1] := aValue else
              if aHouse = htWeaponWorkshop then fWareDistribution[3,2] := aValue;
    wtCorn:   if aHouse = htMill           then fWareDistribution[4,1] := aValue else
              if aHouse = htSwine          then fWareDistribution[4,2] := aValue else
              if aHouse = htStables        then fWareDistribution[4,3] := aValue else
              if aHouse = htHovel          then fWareDistribution[4,4] := aValue;
    else      raise Exception.Create('Unexpected resource at SetWareDistribution');
  end;}
end;


function TKMWareDistribution.GetWareDistribution(aWare: TKMWareType; aHouse: TKMHouseType): Byte;
var I, K : Integer;
begin
  //Some houses has different count of wares, so use Max word instead of max 5 wares
  if aWare in [wtAll, wtWarfare, wtFood, wtValuable] then
    Exit(1);
  if aHouse in [htStore, htBarracks, htMarket] then
    Exit(1);
  
  Result := gRes.Houses[aHouse].MaxWareCount;
  //find distribution
  for I := 0 to High(fWareDistribution) do
    if fWareDistribution[I].WareType = aWare then
      for K := 0 to High(fWareDistribution[I].Houses) do
        if fWareDistribution[I].Houses[K].House = aHouse then
          Result := fWareDistribution[I].Houses[K].Qty;

  {case aWare of
    wtIron:  if aHouse = htWeaponSmithy   then Result := fWareDistribution[1,1] else
              if aHouse = htArmorSmithy    then Result := fWareDistribution[1,2]else
              if aHouse = htIronFoundry    then Result := fWareDistribution[1,3];

    wtCoal:   if aHouse = htIronSmithy     then Result := fWareDistribution[2,1] else
              if aHouse = htMetallurgists  then Result := fWareDistribution[2,2] else
              if aHouse = htWeaponSmithy   then Result := fWareDistribution[2,3] else
              if aHouse = htArmorSmithy    then Result := fWareDistribution[2,4] else
              if aHouse = htIronFoundry    then Result := fWareDistribution[2,5];

    wtTimber:   if aHouse = htArmorWorkshop  then Result := fWareDistribution[3,1] else
              if aHouse = htWeaponWorkshop then Result := fWareDistribution[3,2];
    wtCorn:   if aHouse = htMill           then Result := fWareDistribution[4,1] else
              if aHouse = htSwine          then Result := fWareDistribution[4,2] else
              if aHouse = htStables        then Result := fWareDistribution[4,3] else
              if aHouse = htHovel           then Result := fWareDistribution[4,4];
    else      //Handled in 1st row to avoid repeating in if .. else lines
  end;}

end;


procedure TKMWareDistribution.LoadFromStr(aString: String);
//var I : Integer;
begin
  //fWareDistribution := gRes.Wares.WareDistribution;
  Exit;

end;


function TKMWareDistribution.PackToStr: String;
var
  I, J: Integer;
begin
  Exit;
  Result := '';
  for I := 1 to 4 do
    for J := 1 to 5 do
      Result := Result + IntToStr(5);
end;

procedure TKMWareDistribution.Save(SaveStream: TKMemoryStream);
var I, K, newCount, newCount2: Integer;
begin
  SaveStream.PlaceMarker('WareDistribution');
  newCount := length(fWareDistribution);
  SaveStream.Write(newCount);
  for I := 0 to newCount - 1 do
  begin
    SaveStream.Write(fWareDistribution[I].WareType, SizeOf(fWareDistribution[I].WareType));
    newCount2 := length(fWareDistribution[I].Houses);
    SaveStream.Write(newCount2);
    for K := 0 to newCount2 - 1 do
    begin
      SaveStream.Write(fWareDistribution[I].Houses[K].House, SizeOf(fWareDistribution[I].Houses[K].House));
      SaveStream.Write(fWareDistribution[I].Houses[K].Qty);
    end;
  end;

end;


procedure TKMWareDistribution.Load(LoadStream: TKMemoryStream);
var I, K, J, L : Integer;
begin
  LoadStream.CheckMarker('WareDistribution');
  LoadStream.Read(J);
  SetLength(fWareDistribution, J);

  for I := 0 to J - 1 do
  begin
    LoadStream.Read(fWareDistribution[I].WareType, SizeOf(fWareDistribution[I].WareType));
    LoadStream.Read(L);
    SetLength(fWareDistribution[I].Houses, L);
    for K := 0 to L - 1 do
    begin
      LoadStream.Read(fWareDistribution[I].Houses[K].House, SizeOf(fWareDistribution[I].Houses[K].House));
      LoadStream.Read(fWareDistribution[I].Houses[K].Qty);
    end;


  end;
  //fWareDistribution := gRes.Wares.WareDistribution;
end;

procedure TKMWareDistribution.LoadDefault;
begin
  fWareDistribution := gRes.Wares.WareDistribution;
end;

end.
