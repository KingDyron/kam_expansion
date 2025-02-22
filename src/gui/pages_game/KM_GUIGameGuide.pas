unit KM_GUIGameGuide;
{$I KaM_Remake.inc}
interface
uses
  KM_Controls, KM_ControlsBase, KM_CommonClasses,
  KM_ControlsWaresRow,
  KM_Pics, KM_ResTypes,
  KM_InterfaceGame, KM_ScriptingTypes,
  System.Classes;


type
  TKMGUIGameGuide = class
  private
    fOnShow : TNotifyEvent;
    fType : (gtUnits, gtHouses);
    fSelected : Byte;
    procedure Guide_SelectType(Sender : TObject);
    procedure ClosePanel(Sender : TObject);
    procedure RefreshPanel;
    procedure Guide_SelectIcon(Sender : TObject);
    procedure ReleasedByClick(Sender : TOBject);
    procedure WaresRowClick(Sender : TOBject);
  protected
    Pin_Open : TKMButtonFlatPin;
    Panel_Guide : TKMPanel;
      Image_Background: TKMImage;
      Image_ClosePanel: TKMImage;

      Button_Units,
      Button_Houses: TKMButton;

      Button_Select: array[0..high(GUIHouseOrderFull) - 1] of TKMButtonFlat;

      Image_Selected: TKMButtonFlat;
      Panel_House : TKMPanel;
        Icons_Workers: TKMUnitsButtonsMulti;
        WaresOut,
        WaresIn : array[1..4] of TKMWaresRow;

        WareRow_WoodCost,
        WareRow_StoneCost,
        WareRow_TileCost : TKMWaresRow;
        Button_VWares,
        Button_ReleasedBy : array[0..5] of TKMButtonFlat;
        Label_Description : TKMLabel;

      Panel_Unit : TKMPanel;
        Label_Attack,
        Label_AttackHorse,
        Label_Speed,
        Label_Sight,
        Label_AttackHouse,
        Label_Defense,
        Label_HP,
        Label_UnitDescription : TKMLabel;
        //VWareCost: array[0..11] of TKMButtonFlat;
        VWareCost: TKMControlArray;//array[0..11] of TKMButtonFlat;
        WareCost : array[1..4] of TKMWaresRow;


  public
    constructor Create(aParent: TKMPanel; aOnShow : TNotifyEvent);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
  end;


implementation
uses  KM_RenderUI, KM_Resource, KM_HandsCollection,
      KM_Defaults,
      KM_UtilsExt,
      KM_HouseSiegeWorkshop,
      KM_ResUnits, KM_ResHouses, KM_ResTexts, KM_ResFonts,
      SysUtils;

constructor TKMGUIGameGuide.Create(aParent: TKMPanel; aOnShow : TNotifyEvent);
var I, top : Integer;
begin
  inherited Create;
  fOnShow := aOnShow;
  Pin_Open := TKMButtonFlatPin.Create(aParent, 198, 20, 25, 32, 314);
  Pin_Open.OnClick := ClosePanel;
  Pin_Open.Hint := gResTexts[1947];
  Panel_Guide := TKMPanel.Create(aParent, 240, 0, 500, 900);

  Image_Background := TKMImage.Create(Panel_Guide, 0, 0, Panel_Guide.Width, Panel_Guide.Height, 18, rxGuiMain);
  Image_Background.ImageStretch;
  TKMBevel.Create(Panel_Guide, 20, 40, Panel_Guide.Width - 40, 40);

  Image_ClosePanel := TKMImage.Create(Panel_Guide, Panel_Guide.Width - 60, 10, 30, 30, 52);

  Image_ClosePanel.OnClick := ClosePanel;


  Button_Units := TKMButton.Create(Panel_Guide, 25, 45, 25, 30, 665, rxGui, bsGame);
  Button_Houses := TKMButton.Create(Panel_Guide, 55, 45, 25, 30, 391, rxGui, bsGame);
  Button_Units.OnClick := Guide_SelectType;
  Button_Houses.OnClick := Guide_SelectType;

  for I := 0 to High(Button_Select) do
  begin
    Button_Select[I] := TKMButtonFlat.Create(Panel_Guide, 0, 0, 30, 30, 0);
    Button_Select[I].OnClick := Guide_SelectIcon;
    Button_Select[I].Tag := I;
  end;

  Image_Selected := TKMButtonFlat.Create(Panel_Guide, 250, 100, 200, 200, 0);
  Image_Selected.Hitable := false;

  Icons_Workers := TKMUnitsButtonsMulti.Create(Panel_Guide, 260, 120, 200, 200);

  Panel_House := TKMPanel.Create(Panel_Guide, 0, 450, 500, Panel_Guide.Height - 450 - 40);
  Panel_House.Hitable := false;

    TKMBevel.Create(Panel_House, 20, 0, 460, Panel_House.Height - 20);

    TKMLabel.Create(Panel_House, 20, 5, 70, 20, gResTexts[1907], fntMetal, taCenter);

    WareRow_WoodCost := TKMWaresRow.Create(Panel_House, 30, 25, 50);
    WareRow_StoneCost := TKMWaresRow.Create(Panel_House, 30, 50, 50);
    WareRow_TileCost := TKMWaresRow.Create(Panel_House, 30, 75, 50);
    WareRow_WoodCost.TexID := gRes.Wares[wtTimber].GUIIcon;
    WareRow_StoneCost.TexID := gRes.Wares[wtStone].GUIIcon;
    WareRow_TileCost.TexID := gRes.Wares[wtTile].GUIIcon;

    WareRow_WoodCost.TxtOffset := -25;
    WareRow_WoodCost.TextOffset := 15;

    WareRow_StoneCost.TxtOffset := -25;
    WareRow_StoneCost.TextOffset := 15;

    WareRow_TileCost.TxtOffset := -25;
    WareRow_TileCost.TextOffset := 15;

    WareRow_WoodCost.WareCntAsNumber := true;
    WareRow_StoneCost.WareCntAsNumber := true;
    WareRow_TileCost.WareCntAsNumber := true;
    WareRow_WoodCost.Clickable := true;
    WareRow_StoneCost.Clickable := true;
    WareRow_TileCost.Clickable := true;
    WareRow_WoodCost.Hitable := true;
    WareRow_StoneCost.Hitable := true;
    WareRow_TileCost.Hitable := true;
    WareRow_WoodCost.HideHighlight := false;
    WareRow_StoneCost.HideHighlight := false;
    WareRow_TileCost.HideHighlight := false;
    WareRow_WoodCost.Tag :=  ord(wtTimber);
    WareRow_StoneCost.Tag :=  ord(wtStone);
    WareRow_TileCost.Tag :=  ord(wtTile);
    WareRow_WoodCost.OnClick := WaresRowClick;
    WareRow_StoneCost.OnClick := WaresRowClick;
    WareRow_TileCost.OnClick := WaresRowClick;
    WareRow_WoodCost.Hint := gResTexts[1917];
    WareRow_StoneCost.Hint := gResTexts[1917];
    WareRow_TileCost.Hint := gResTexts[1917];


    TKMLabel.Create(Panel_House, Panel_House.Width - 370, 5, 160, 20, gResTexts[1908], fntMetal, taCenter);
    TKMLabel.Create(Panel_House, Panel_House.Width - 190, 5, 160, 20, gResTexts[1909], fntMetal, taCenter);
    for I := 1 to 4 do
    begin
      WaresIn[I] := TKMWaresRow.Create(Panel_House, Panel_House.Width - 370, 5 + I * 25, 160);
      WaresIn[I].Hitable := true;
      WaresIn[I].HideHighlight := false;
      WaresIn[I].OnClick := WaresRowClick;
      WaresIn[I].Clickable := true;
      WaresIn[I].Hint := gResTexts[1917];
      WaresIn[I].Tag2 := 0;

      WaresOut[I] := TKMWaresRow.Create(Panel_House, Panel_House.Width - 190, 5 + I * 25, 160);
      WaresOut[I].Hitable := true;
      WaresOut[I].HideHighlight := false;
      WaresOut[I].OnClick := WaresRowClick;
      WaresOut[I].Clickable := true;
      WaresOut[I].Hint := gResTexts[1917];
      WaresOut[I].Tag2 := 0;
    end;

    TKMLabel.Create(Panel_House, 30, 150, 300, 20, gResTexts[1910], fntMetal, taLeft);
    for I := 0 to High(Button_ReleasedBy) do
    begin
      Button_ReleasedBy[I] := TKMButtonFlat.Create(Panel_House, 40 + I * 34, 175, 30, 30, 0);
      Button_ReleasedBy[I].OnClick := ReleasedByClick;
    end;

    TKMLabel.Create(Panel_House, 30, 215, 300, 20, gResTexts[1911], fntMetal, taLeft);
    for I := 0 to High(Button_VWares) do
    begin
      Button_VWares[I] := TKMButtonFlat.Create(Panel_House, 40 + I * 34, 240, 30, 30, 0);
      Button_VWares[I].Tag2 := 1;
      Button_VWares[I].OnClick := WaresRowClick;
    end;

    Label_Description := TKMLabel.Create(Panel_House, 25, 280, Panel_House.Width - 50, Panel_House.Height - 405, '', fntMetal, taLeft);
    Label_Description.WordWrap := true;

  Panel_House.Hide;


  //Unit Panel
  Panel_Unit := TKMPanel.Create(Panel_Guide, 0, 375, 500, Panel_Guide.Height - 375 - 40);
  Panel_Unit.Hitable := false;

    TKMBevel.Create(Panel_Unit, 20, -5, 460,  Panel_Unit.Height - 20);

    TKMBevel.Create(Panel_Unit, 30, 0, 190,  185);

    Label_Attack := TKMLabel.Create(Panel_Unit, 40, 5, 200, 20, gResTexts[1600], fntMetal, taLeft);
    Label_Defense := TKMLabel.Create(Panel_Unit, 40, 30, 200, 20, gResTexts[1601], fntMetal, taLeft);
    Label_HP := TKMLabel.Create(Panel_Unit, 40, 55, 200, 20, gResTexts[1603], fntMetal, taLeft);
    Label_AttackHorse := TKMLabel.Create(Panel_Unit, 40, 80, 200, 20, gResTexts[1913], fntMetal, taLeft);
    Label_Speed := TKMLabel.Create(Panel_Unit, 40, 105, 200, 20, gResTexts[1914], fntMetal, taLeft);
    Label_Sight := TKMLabel.Create(Panel_Unit, 40, 130, 200, 20, gResTexts[1915], fntMetal, taLeft);
    Label_AttackHouse := TKMLabel.Create(Panel_Unit, 40, 155, 200, 20, gResTexts[1916], fntMetal, taLeft);

    TKMBevel.Create(Panel_Unit, 460 - 195, 0, 190,  185);

    TKMLabel.Create(Panel_Unit, 275, 5, 200, 20, gResTexts[1912], fntMetal, taLeft);

    for I := 1 to High(WareCost) do
    begin
      WareCost[I] := TKMWaresRow.Create(Panel_Unit, 275, I * 22, 170);
      WareCost[I].Hitable := true;
      WareCost[I].Clickable := True;
      WareCost[I].OnClick := WaresRowClick;
      WareCost[I].HideHighlight := false;
      WareCost[I].Hint := gResTexts[1917];
      WareCost[I].WareCntAsNumber := true;
      WareCost[I].TextOffset := 0;
      WareCost[I].Tag2 := 0;
    end;

    top := WareCost[high(WareCost)].Bottom + 5;
    SetLength(VWareCost, 12);
    for I := 0 to High(VWareCost) do
    begin
      VWareCost[I] := TKMButtonFlat.Create(Panel_Unit, 272 + (I mod 6) * 30, top + (I div 6) * 32, 27, 30, 0, rxGui);
      VWareCost[I].Tag2 := 1;
      VWareCost[I].OnClick := WaresRowClick;
    end;


    TKMBevel.Create(Panel_Unit, 30, 195, 440,  Panel_Unit.Height - 20 - 195 - 10);
    Label_UnitDescription := TKMLabel.Create(Panel_Unit, 40, 205, 420, Panel_Unit.Height - 20 - 205 - 10, '', fntMetal, taLeft);
    Label_UnitDescription.WordWrap := true;

  Panel_Unit.Hide;

  fSelected := 255;
  Hide;
end;


procedure TKMGUIGameGuide.Show;
begin
  Panel_Guide.Show;
  Button_Units.FlagColor := gMySpectator.Hand.FlagColor;
  if Assigned(fOnShow) then
    fOnShow(Self);

  RefreshPanel;
end;


procedure TKMGUIGameGuide.Hide;
begin
  Panel_Guide.Hide;
end;


function TKMGUIGameGuide.Visible: Boolean;
begin
  Result := Panel_Guide.Visible;
end;

procedure TKMGUIGameGuide.Guide_SelectType(Sender: TObject);
begin
  if sender = Button_Units then
  begin
    if fType = gtUnits then
      Exit;

    fType := gtUnits;

  end else
  if sender = Button_Houses then
  begin
    if fType = gtHouses then
      Exit;

    fType := gtHouses;
  end;

  fSelected := 255;
  RefreshPanel;

end;

procedure TKMGUIGameGuide.ClosePanel(Sender: TObject);
begin
  if Sender = Pin_Open then
  begin
    if Visible then
      Hide
    else
      Show;

  end else
    Hide;
end;

procedure TKMGUIGameGuide.RefreshPanel;
  procedure ShowWareCost(aIndex, aCount: Integer; aWareType : TKMWareType);
  begin
    WareCost[aIndex].Tag := ord(aWareType);
    WareCost[aIndex].TexID := gRes.Wares[aWareType].GUIIcon;
    WareCost[aIndex].WareCount := aCount;
    WareCost[aIndex].MaxWares := aCount;
    WareCost[aIndex].Caption := gRes.Wares[aWareType].Title;

    WareCost[aIndex].Show;
  end;

var I, J, fullCount : Integer;
  HT, HT2 : TKMHouseType;
  UT : TKMUnitType;
  WT : TKMWareType;

  warePlan : TKMWarePlan;

begin
  if fType = gtUnits then
  begin
    for I := 0 to High(Button_Select) do
    begin
      Button_Select[I].FlagColor := gMySpectator.Hand.FlagColor;
      if I > high(MapEd_Order) then
        Button_Select[I].Hide
      else
      begin
        UT := MapEd_Order[I];
        Button_Select[I].Top := 100 + (I div 6) * 40;
        Button_Select[I].Left := 30 + (I mod 6) * 34;
        Button_Select[I].Height := 38;

        Button_Select[I].TexID := gRes.Units[UT].GUIIcon;
        Button_Select[I].Hint := gRes.Units[UT].GUIName;

        Button_Select[I].Show;
        Button_Select[I].Down := fSelected = I;

        case gRes.Units[UT].GetTrainingHouse of
          htSchool : Button_Select[I].BackBevelColor := $2276403E;
          htBarracks : Button_Select[I].BackBevelColor := $22FF0000;
          htTownhall : Button_Select[I].BackBevelColor := $2200BDFF;
          htPalace : Button_Select[I].BackBevelColor := $220000FF;
          htSiegeWorkshop : Button_Select[I].BackBevelColor := $22FF00C6;
          htShipyard : Button_Select[I].BackBevelColor := $22FFB400;
          else
            Button_Select[I].BackBevelColor := $22FFFFFF;
        end;

      end;
      
    end;

  end else
  begin
    for I := 0 to High(Button_Select) do
    begin
      Button_Select[I].BackBevelColor := 0;
      Button_Select[I].FlagColor := high(Cardinal);
      Button_Select[I].Top := 100 + (I div 6) * 34;
      Button_Select[I].Left := 30 + (I mod 6) * 34;
      Button_Select[I].Height := 32;

      Button_Select[I].TexID := gRes.Houses[GUIHouseOrderFull[I + 1]].GUIIcon;
      Button_Select[I].Hint := gRes.Houses[GUIHouseOrderFull[I + 1]].HouseName;
      Button_Select[I].Show;
      Button_Select[I].Down := fSelected = I;
    end;

    //Button_Select[High(Button_Select)].Hide;
  end;
  if fSelected = 255 then
  begin
    Panel_House.Hide;
    Panel_Unit.Hide;
    Image_Selected.TexID := 0;
    Image_Selected.Caption := '';
    Exit;
  end;

  if fType = gtUnits then
  begin
    UT := Soldiers_Order[fSelected];
    Panel_House.Hide;
    Panel_Unit.Show;
    Image_Selected.RX := rxGui;
    Image_Selected.TexID := gRes.Units[UT].GUIScroll;
    Image_Selected.FlagColor := gMySpectator.Hand.FlagColor;
    Image_Selected.Height := 250;
    Image_Selected.CapOffsetY := 100;
    Image_Selected.Caption := gRes.Units[UT].GUIName;
    Icons_Workers.Hide;
    with gRes.Units[UT] do
    begin
      Label_Attack.Caption := gResTexts[1600] + IntToStr(Attack);
      Label_Defense.Caption := gResTexts[1601] + IntToStr(Defence);
      Label_HP.Caption := gResTexts[1603] + IntToStr(HitPoints);
      Label_AttackHorse.Caption := gResTexts[1913] + IntToStr(AttackHorse);
      Label_Speed.Caption := gResTexts[1914] + IntToStr(AbsSpeed);
      Label_Sight.Caption := gResTexts[1915] + IntToStr(Sight);
      Label_AttackHouse.Caption := gResTexts[1916] + IntToStr(HouseDamage);
      Label_UnitDescription.Caption := Description;
    end;
    for I := 1 to high(WareCost) do
      WareCost[I].Hide;

    for I := 0 to High(VWareCost) do
        VWareCost[I].Hide;

    case gRes.Units[UT].GetTrainingHouse of
      htPalace: begin
                  warePlan := gRes.Units[UT].PalaceCost.Plan;
                  for I := 1 to 4 do
                    if warePlan[I - 1].W <> wtNone then
                    begin
                      fullCount := warePlan[I - 1].C + (gRes.Units[UT].PalaceCost.PhaseCount - 1);

                      ShowWareCost(I, fullCount, warePlan[I - 1].W);
                    end;
                  for I := 0 to High(VWareCost) do
                    if I < length(gRes.Units[UT].PalaceCost.Wares) then
                      with gRes.Units[UT].PalaceCost do
                      begin
                        J := Wares[I].Index;
                        //VWareCost[I].Caption := IntToStr(Wares[I].C);
                        TKMButtonFlat(VWareCost[I]).Caption := IntToStr(Wares[I].C);
                        VWareCost[I].Show;
                        VWareCost[I].Hint := gResTexts[gRes.Wares.VirtualWares.WareS[Wares[I].W].TextID];
                        TKMButtonFlat(VWareCost[I]).TexID := gRes.Wares.VirtualWares.WareS[Wares[I].W].GUIIcon;
                        //VWareCost[I].TexID := gRes.Wares.VirtualWares.Ware[J].GUIIcon;
                        VWareCost[I].Tag := J;
                      end;
                  SortVisibleControls(265, WareCost[high(WareCost)].Bottom + 3, 190, 3, VWareCost, false, true);
                end;
      htBarracks: for I := 0 to high(gRes.Units[UT].BarracksCost) do
                    if gRes.Units[UT].BarracksCost[I].W <> wtNone then
                      ShowWareCost(I + 1,
                                  gRes.Units[UT].BarracksCost[I].C,
                                  gRes.Units[UT].BarracksCost[I].W);

      htTownhall: ShowWareCost(1, gRes.Units[UT].TownhallCost, wtGold);


      htSiegeWorkshop:begin
                        warePlan := TKMHouseSiegeWorkshop.GetTotalCost(UT);

                        for I := 1 to 4 do
                          if warePlan[I - 1].W <> wtNone then
                            ShowWareCost(I, warePlan[I - 1].C, warePlan[I - 1].W);

                      end;
    end;



  end else
  begin
    Panel_House.Show;
    Panel_Unit.Hide;
    HT := GUIHouseOrderFull[fSelected + 1];
    Image_Selected.RX := rxHouses;
    Image_Selected.TexID := gRes.Houses[HT].GetRandomStonePic + 1;
    Image_Selected.FlagColor := high(Cardinal);
    Image_Selected.Height := 325;
    Image_Selected.CapOffsetY := 138;
    Image_Selected.Caption := gRes.Houses[HT].HouseName;
    Icons_Workers.Show;

    Icons_Workers.UnitPlan := gRes.Houses[HT].Workers;
    If gRes.Houses[HT].WorkerType <> utNone then
      Icons_Workers.Caption := 'Max : ' + gRes.Houses[HT].MaxWorkersCount.ToString
    else
      Icons_Workers.Caption := '';
    WareRow_WoodCost.WareCount := gRes.Houses[HT].WoodCost;
    WareRow_StoneCost.WareCount := gRes.Houses[HT].StoneCost;
    WareRow_TileCost.WareCount := gRes.Houses[HT].TileCost;

    for I := 1 to 4 do
    begin
      WT := gRes.Houses[HT].WareInput[I];
        WaresIn[I].Visible := WT <> wtNone;
      if WaresIn[I].Visible then
      begin
        WaresIn[I].TexID := gRes.Wares[WT].GuiIcon;
        WaresIn[I].WareCount := 1;
        WaresIn[I].Caption := gRes.Wares[WT].Title;
        WaresIn[I].Tag := Ord(WT);

        if not gRes.Wares[WT].IsValid then
          WaresIn[I].Clickable := false
        else
          WaresIn[I].Clickable := true;
      end;

      WT := gRes.Houses[HT].WareOutput[I];
      WaresOut[I].Visible := WT <> wtNone;

      if WaresOut[I].Visible then
      begin
        WaresOut[I].TexID := gRes.Wares[WT].GuiIcon;
        WaresOut[I].WareCount := gRes.Houses[HT].GetWareProdCt(WT);
        WaresOut[I].Caption := gRes.Wares[WT].Title;
        WaresOut[I].Tag := Ord(WT);

        if not gRes.Wares[WT].IsValid then
          WaresOut[I].Clickable := false
        else
          WaresOut[I].Clickable := true;

      end;
    end;

    I := 0;

    for HT2 in gRes.Houses[HT].ReleasedBy do
    begin
      if HT2 = htNone then
        Continue;

      Button_ReleasedBy[I].TexID := gRes.Houses[HT2].GUIIcon;
      Button_ReleasedBy[I].Tag := ord(HT2);
      Button_ReleasedBy[I].Show;
      Button_ReleasedBy[I].Hint := gRes.Houses[HT2].HouseName;
      Inc(I);
    end;

    for J := I to High(Button_ReleasedBy) do
      Button_ReleasedBy[J].Hide;

    Label_Description.Caption := gRes.Houses[HT].HouseDescription;
    J := 0;
    for I := 0 to gRes.Wares.VirtualWares.Count - 1 do
      if HT in gRes.Wares.VirtualWares.Ware[I].ProduceInHouses then
        if J <= high(Button_VWares) then
        begin
          Button_VWares[J].TexID := gRes.Wares.VirtualWares.Ware[I].GUIIcon;
          Button_VWares[J].Tag := I;
          Button_VWares[J].Show;
          Button_VWares[J].Hint := gResTexts[gRes.Wares.VirtualWares.Ware[I].TextID];
          inc(J);
        end;
    for I := J to high(Button_VWares) do
      Button_VWares[I].Hide;

  end;
end;

procedure TKMGUIGameGuide.Guide_SelectIcon(Sender: TObject);
begin
  fSelected := TKMButton(Sender).Tag;
  RefreshPanel;
end;

procedure TKMGUIGameGuide.ReleasedByClick(Sender: TObject);
var I : integer;
  HT : TKMHouseType;
begin
  HT := TKMHouseType(TKMButtonFlat(Sender).Tag);
  for I := 0 to High(Button_Select) do
    if GUIHouseOrderFull[I + 1] = HT then
    begin
      fSelected := I;
      Break;
    end;
  RefreshPanel;
end;

procedure TKMGUIGameGuide.WaresRowClick(Sender: TObject);
  function FindHouseWhichProduceWare(aWare: TKMWareType; aAvoid : TKMHouseType) : TKMHouseType;
  var J : Integer;
    HT, HT2 : TKMHouseType;
  begin
    Result := htNone;

    HT2 := TKMHouseType(Byte(aAvoid) + 1);
    for HT := HOUSE_MIN to HOUSE_MAX do
    begin
      if HT2 > HOUSE_MAX then
        HT2 := HOUSE_MIN;
      for J := 1 to 4 do
        If gRes.Houses[HT2].WareOutput[J] = aWare then
          Exit(HT2);
      HT2 := TKMHouseType(Byte(HT2) + 1);
    end;

  end;

  function FindHouseWhichProduceVWare(aWareID: Integer; aAvoid : TKMHouseType) : TKMHouseType;
  var HT, HT2 : TKMHouseType;
  begin
    Result := htNone;

    HT2 := TKMHouseType(Byte(aAvoid) + 1);
    for HT := HOUSE_MIN to HOUSE_MAX do
    begin
      if HT2 > HOUSE_MAX then
        HT2 := HOUSE_MIN;

      if HT2 in gRes.Wares.VirtualWares.Ware[aWareID].ProduceInHouses then
          Exit(HT2);

      HT2 := TKMHouseType(Byte(HT2) + 1);
    end;

  end;

var I : Integer;
    H : TKMHouseType;
    W : TKMWareType;
begin
  if TKMControl(Sender).Tag2 = 1 then
  begin
    H := FindHouseWhichProduceVWare(TKMControl(Sender).Tag, GUIHouseOrderFull[fSelected + 1] );
  end
  else
  begin
    W := TKMWareType( TKMControl(Sender).Tag );

    if not gRes.Wares[W].IsValid then Exit;
    H := FindHouseWhichProduceWare(W, GUIHouseOrderFull[fSelected + 1] );

  end;


  if H = htNone then
    Exit;
  fType := gtHouses;

  for I := 0 to High(Button_Select) do
    if GUIHouseOrderFull[I + 1] = H then
    begin
      fSelected := I;
      Break;
    end;

  RefreshPanel;



end;

end.


