unit KM_GUIMapEdPlayerCheck;
{$I KaM_Remake.inc}
interface
uses
   Classes, KM_CommonTypes,
   KM_Controls, KM_ControlsBase, KM_ControlsPopUp,
   KM_Defaults, KM_ResTypes,
   KM_Points;

type
  TKMHousesMissingViewer = class(TKMControl)
  private
    fHGreen,
    fHRed : TKMHouseTypeArray;
    fGap, fColumnsCount : Byte;
    procedure SortByFirst;
  public
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
    procedure Paint; override;
  end;

  TKMHousesWarfareViewer = class(TKMControl)
  private
    fH : TKMHouseTypeArray;
    fCount : TKMWordArray;
    fGap, fColumnsCount : Byte;
    procedure SortByFirst;
  public
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
    procedure Paint; override;
  end;

  TKMMapEdPlayerCheck = class(TKMPopUpPanel)
  private
    procedure CheckMissingHouses(sender : TObject);
    procedure CheckMissingWarfare(sender : TObject);
    procedure CheckMissingWells(sender : TObject);
    procedure NoWellHouseClick(sender : TObject);
    procedure UpdatePositions;

  protected
    Button_CheckMissingUnlock : TKMButton;
    Button_CheckMissingWarfare : TKMButton;
    Button_CheckMissingWells : TKMButton;
    MissingHouses: TKMHousesMissingViewer;
    WarfareHouses: TKMHousesWarfareViewer;

    Panel_NoWellHouses : TKMPanel;
    NoWellHouses : array of TKMButtonFlat;//show max 16 houses
  public
    constructor Create(aParent : TKMPanel);

  end;

implementation
uses
  Math, SysUtils,
  KM_CommonClasses,
  KM_HandsCollection,
  KM_Houses,
  KM_Game,
  KM_RenderUI, KM_ResFonts, KM_Resource, KM_ResTexts,
  KM_ResHouses,
  KromUtils;

constructor TKMMapEdPlayerCheck.Create(aParent : TKMPanel);
var I : integer;
begin
  Inherited Create(aParent.MasterPanel, 600, 500, gResTexts[2336], pbYellow, true);
  BevelShade.HideParentOnClick;
  BevelShade.Hide;
  DragEnabled := true;

  Button_CheckMissingUnlock := TKMButton.Create(ItemsPanel, 5, 5, ItemsPanel.Width - 10, 25, gResTexts[2337], bsMenu);
  Button_CheckMissingUnlock.OnClick := CheckMissingHouses;

  MissingHouses := TKMHousesMissingViewer.Create(ItemsPanel, 5, 35, ItemsPanel.Width - 10, 80);
  MissingHouses.fHGreen := [];
  MissingHouses.Height := MissingHouses.fGap * 2;

  Button_CheckMissingWarfare := TKMButton.Create(ItemsPanel, 5, 5, ItemsPanel.Width - 10, 25, gResTexts[2338], bsMenu);
  Button_CheckMissingWarfare.OnClick := CheckMissingWarfare;

  WarfareHouses := TKMHousesWarfareViewer.Create(ItemsPanel, 5, 35, ItemsPanel.Width - 10, 80);
  WarfareHouses.Height := MissingHouses.fGap;

  Button_CheckMissingWells := TKMButton.Create(ItemsPanel, 5, 5, ItemsPanel.Width - 10, 25, gResTexts[2339], bsMenu);
  Button_CheckMissingWells.OnClick := CheckMissingWells;

  Panel_NoWellHouses := TKMPanel.Create(ItemsPanel, 5, 35, ItemsPanel.Width - 10, 40);
  with TKMBevel.Create(Panel_NoWellHouses, 0, 0, Panel_NoWellHouses.Width, Panel_NoWellHouses.Height) do
    AnchorsStretch;
    SetLength(NoWellHouses, MissingHouses.fColumnsCount);
    for I := 0 to High(NoWellHouses) do
    begin
      NoWellHouses[I] := TKMButtonFlat.Create(Panel_NoWellHouses, I * MissingHouses.fGap, 3, 33, 33, 0);
      NoWellHouses[I].OnClick := NoWellHouseClick;
      NoWellHouses[I].Hide;
    end;

  UpdatePositions;
end;

procedure TKMMapEdPlayerCheck.CheckMissingHouses(Sender : TObject);
var I : Integer;
  H, HT : TKMHouseType;
  hasAny : Boolean;
begin
  //FillChar(isMissing, sizeOf(isMissing), #0);

  MissingHouses.fHGreen := [];
  MissingHouses.fHRed := [];

  for I := 0 to High(HOUSE_ID_TO_TYPE) do
  begin
    H := HOUSE_ID_TO_TYPE[I];
    //check if player has any house that unlocks this house
    If gMySpectator.Hand.Stats.GetHouseQty(H) > 0 then
    begin
      hasAny := false;

      for HT in gRes.Houses[H].ReleasedBy do
        If gMySpectator.Hand.Stats.GetHouseQty(HT) > 0 then
          hasAny := true;

      If hasAny then
        Continue;
      //doesn't have any so add to the list
      for HT in gRes.Houses[H].ReleasedBy do
        If not (HT in [htNone, htAny]) then
        begin
          MissingHouses.fHRed := MissingHouses.fHRed + [HT];
          MissingHouses.fHGreen := MissingHouses.fHGreen + [H];
        end;
    end;
  end;

  MissingHouses.SortByFirst;
  MissingHouses.Height := MissingHouses.fGap * 2 +
                          MissingHouses.fGap * 2 * (Max(high(MissingHouses.fHGreen), high(MissingHouses.fHRed)) div MissingHouses.fColumnsCount);
  UpdatePositions;
end;

procedure TKMMapEdPlayerCheck.CheckMissingWarfare(Sender : TObject);
const
  MAIN_HOUSES = [htBarracks, htTownhall];
  LEATHER_HOUSES = [htWeaponWorkshop, htTailorsShop, htTannery, htSwine, htFarm];
  IRON_HOUSES = [htWeaponSmithy, htArmorSmithy, htIronSmithy];
  HORSE_HOUSES = [htStables, htFarm];
  SHIP_HOUSES = [htShipYard, htStoneWorkshop, htSawmill, htSwine];

  PROD_THATCH_HOUSES = [htStoneWorkshop, htSawmill, htFarm, htQuarry, htPottery, htVineYard, htIronSmithy, htIronFoundry];

var HT : TKMHouseType;
  houses : TKMHouseTypeSet;
  demands : TAIArmyDemand;
begin
  //FillChar(isMissing, sizeOf(isMissing), #0);

  WarfareHouses.fH := [];
  WarfareHouses.fCount := [];

  houses := MAIN_HOUSES;
  gMySpectator.Hand.AI.General.DefencePositions.GetArmyDemand(demands);


  If gMySpectator.Hand.AI.Setup.ArmyType in [atIronThenLeather, atIron, atIronAndLeather] then
    houses := houses + IRON_HOUSES;

  If gMySpectator.Hand.AI.Setup.ArmyType in [atIronThenLeather, atLeather, atIronAndLeather] then
  begin
    houses := houses + LEATHER_HOUSES;

    If (demands[gtWreckers] > 0) or (demands[gtMelee] > 0) or (demands[gtMounted] > 0) then
      houses := houses + [htArmorWorkshop];
  end;

  If demands[gtMounted] > 0 then
    houses := houses + HORSE_HOUSES;

  If (demands[gtMachines] > 0) or (demands[gtMachinesMelee] > 0) then
    houses := houses + [htSiegeWorkshop, htIronFoundry, htStoneWorkshop];
  If (demands[gtRanged] > 0) then
    houses := houses + [htHovel];


  for HT in houses do
  begin
    WarfareHouses.fH := WarfareHouses.fH + [HT];
    SetLength(WarfareHouses.fCount, length(WarfareHouses.fCount) + 1);
    WarfareHouses.fCount[high(WarfareHouses.fCount)] := gMySpectator.Hand.Stats.GetHouseQty(HT);
  end;

  WarfareHouses.SortByFirst;
  WarfareHouses.Height := WarfareHouses.fGap +
                          WarfareHouses.fGap * high(WarfareHouses.fH) div WarfareHouses.fColumnsCount;
  UpdatePositions;
end;

procedure TKMMapEdPlayerCheck.CheckMissingWells(sender: TObject);
var I, K : Integer;
  H : TKMHouse;
begin
  for I := 0 to High(NoWellHouses) do
  begin
    NoWellHouses[I].Hide;
    NoWellHouses[I].Tag := 0;
  end;

  K := 0;
  for I := 0 to gMySpectator.Hand.Houses.Count - 1 do
  begin
    H := gMySpectator.Hand.Houses[I];
    If not H.IsValid(htAny, false, true) then
      Continue;
    If H.GetWareInIndex(wtWater) > 0 then
    begin
      If not gHands.GetClosestHouse(H.Entrance, [htWell], [], IfThen(gMySpectator.Hand.IsComputer, 12, 6)).IsValid(htAny, false, true) then
      begin
        NoWellHouses[K].Tag := Integer(H);
        NoWellHouses[K].Show;
        NoWellHouses[K].TexID := gRes.Houses[H.HouseType].GUIIcon;
        Inc(K);
        If K >= high(NoWellHouses) then
          Break;
      end;

    end;

  end;


end;

procedure TKMMapEdPlayerCheck.NoWellHouseClick(sender: TObject);
var H : TKMHouse;
begin
  H := TKMHouse(TKMControl(Sender).Tag);
  gMySpectator.Selected := H;
  gGame.ActiveInterface.ViewPort.Position := H.Entrance.ToFloat;
end;

procedure TKMMapEdPlayerCheck.UpdatePositions;
var I, lastTop : integer;
begin
  lastTop := 5;
  for I := 2 to ItemsPanel.ChildCount - 1 do
  begin
    ItemsPanel.Childs[I].Top := lastTop;
    Inc(lastTop, ItemsPanel.Childs[I].Height + 5);
  end;
end;


constructor TKMHousesMissingViewer.Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fColumnsCount := Width div 36;
  fGap := Width div fColumnsCount;
end;

procedure TKMHousesMissingViewer.SortByFirst;
  procedure Swap(var H1 : TKMHouseType; var H2 : TKMHouseType);
  var tmp : TKMHouseType;
  begin
    tmp := H1;
    H1 := H2;
    H2 := tmp;
  end;
var I, K : Integer;
begin
  for I := 0 to Min(High(fHGreen), High(fHRed)) do
    for K := I + 1 to Min(High(fHGreen), High(fHRed)) do
      If HOUSE_TYPE_TO_ID[fHRed[I]] > HOUSE_TYPE_TO_ID[fHRed[K]] then
      begin
        Swap(fHRed[I], fHRed[K]);
        Swap(fHGreen[I], fHGreen[K]);
      end;
end;

procedure TKMHousesMissingViewer.Paint;
var I : Integer;
  l, t : Integer;
begin
  Inherited;
  l := AbsLeft;
  t := AbsTop;
  TKMRenderUI.WriteBevel(l, t, Width, Height, 1, 0.3);
  for I := 0 to Min(High(fHGreen), High(fHRed)) do
  If not (fHGreen[I] in [htNone, htAny]) then
  begin
    //
    TKMRenderUI.WriteBevel(l + (I mod fColumnsCount) * fGap, t + fGap + (I div fColumnsCount) * fGap * 2, 33, 33, TKMColor3f.New(0, 1, 0));
    TKMRenderUI.WritePicture(l + (I mod fColumnsCount) * fGap, t + fGap + (I div fColumnsCount) * fGap * 2, 33, 33,
                             [], rxGui, gRes.Houses[fHGreen[I]].GuiIcon);

    TKMRenderUI.WriteBevel(l + (I mod fColumnsCount) * fGap, t + (I div fColumnsCount) * fGap * 2, 33, 33, TKMColor3f.New(1, 0, 0));
    TKMRenderUI.WritePicture(l + (I mod fColumnsCount) * fGap, t + (I div fColumnsCount) * fGap * 2, 33, 33,
                             [], rxGui, gRes.Houses[fHRed[I]].GuiIcon);

    TKMRenderUI.WritePicture(l + (I mod fColumnsCount) * fGap, t - 3 + fGap + (I div fColumnsCount) * fGap * 2, 33, 0, [], rxGui, 954)
  end;

end;



constructor TKMHousesWarfareViewer.Create(aParent : TKMPanel; aLeft, aTop, aWidth, aHeight : Integer);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fColumnsCount := Width div 36;
  fGap := Width div fColumnsCount;
end;

procedure TKMHousesWarfareViewer.SortByFirst;
  procedure Swap(var H1 : TKMHouseType; var H2 : TKMHouseType);
  var tmp : TKMHouseType;
  begin
    tmp := H1;
    H1 := H2;
    H2 := tmp;
  end;
var I, K : Integer;
begin
  for I := 0 to high(fH) do
    for K := I + 1 to high(fH) do
      If HOUSE_TYPE_TO_ID[fH[I]] > HOUSE_TYPE_TO_ID[fH[K]] then
      begin
        Swap(fH[I], fH[K]);
        KromUtils.SwapInt(fCount[I], fCount[K]);
      end;
end;

procedure TKMHousesWarfareViewer.Paint;
var I : Integer;
  l, t : Integer;
begin
  Inherited;
  l := AbsLeft;
  t := AbsTop;
  TKMRenderUI.WriteBevel(l, t, Width, Height, 1, 0.3);
  for I := 0 to High(fH) do
  begin
    If fCount[I] = 0 then
      TKMRenderUI.WriteBevel(l + (I mod fColumnsCount) * fGap, t + (I div fColumnsCount) * fGap, 33, 33, TKMColor3f.New(1, 0, 0))
    else
      TKMRenderUI.WriteBevel(l + (I mod fColumnsCount) * fGap, t + (I div fColumnsCount) * fGap, 33, 33, TKMColor3f.New(0, 1, 0));
    TKMRenderUI.WritePicture(l + (I mod fColumnsCount) * fGap, t + (I div fColumnsCount) * fGap, 33, 33,
                             [], rxGui, gRes.Houses[fH[I]].GuiIcon);
    TKMRenderUI.WriteText(l + (I mod fColumnsCount) * fGap, t + 15 + (I div fColumnsCount) * fGap, 33, IntToStr(fCount[I]),
                          fntGrey, taRight);
  end;
end;




end.
