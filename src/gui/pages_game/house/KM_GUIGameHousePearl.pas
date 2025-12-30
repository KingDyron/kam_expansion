unit KM_GUIGameHousePearl;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HousePearl;

type
  TKMGuiGamePearl = class(TKMPanel)
    private
      fLastPearlType : TKMPearlType;
      procedure Button_Confirm(Sender : TObject);
      procedure SelectPearl(Sender : TObject);

      //valtaria
      procedure SelectWares(Sender : TObject);
      procedure ValtariaExchange(Sender : TObject; Shift : TShiftState);

      //arium
      procedure AriumExchange(Sender : TObject; Shift : TShiftState);
      procedure AriumSelectVWare(Sender : TObject);
      procedure ButtonRepair(Sender : TObject);
      //agros
      procedure AgrosExchange(Sender : TObject; Shift : TShiftState);
      //ralender
      procedure RalenderSelectWares(Sender : TObject);
      procedure RalenderExchange(Sender : TObject; Shift : TShiftState);

      procedure UseSpecial(Sender : TObject);

      procedure ToggleAcceptWares(Sender : TObject; Shift : TShiftState);
      procedure ShowBonuses(aPearlType : TKMPearlType; aTop : Integer);
    protected
      Button_PearlType : TKMButtonFlat;
      Label_PearlType : TKMLabel;

      Panel_Build : TKMPanel;
        Button_SelectType : array[ptValtaria..high(TKMPearlType)] of TKMButtonFlat;
        Button_ConfirmBuild : TKMButton;
        CostRow, DeliveredRow : TKMCostsRowMultiMax;
      Panel_Pearl : array[ptValtaria..high(TKMPearlType)] of TKMPanel;

      //valtaria
        Button_NotAcceptWares,
        Button_VWaresFrom, Button_VWaresTo :array[1..WARES_IN_OUT_COUNT] of TKMButtonFlat;//default wares
        Button_VExchange : TKMButton;
        Label_VRatioTo : TKMLabel;
      //arium
        Button_ACoin : TKMButtonFlat;
        Button_AWares : array[0..3] of TKMButtonFlat;
        Button_AExchange : TKMButton;
        Label_ARatioFrom, Label_ARatioTo : TKMLabel;
        Button_Repair : TKMButton;
      //agros
        Button_AgRecruit : TKMButtonFlat;
        Button_AgExchange : TKMButton;
        Button_AgElite : TKMButtonFlat;
      //ralender
        Button_RNotAcceptEggs,
        Button_RWaresFrom : TKMButtonFlat;
        Button_RWaresTo :array[1..WARES_IN_OUT_COUNT] of TKMButtonFlat;//default wares
        Button_RExchange : TKMButton;
        Label_RRatioTo : TKMLabel;

      //special ability
        Button_UseSpecial : TKMButtonFlat;
        Icon_Reload : TKMImage;
      //constant bonuses
        Button_Bonuses :array[0..4] of TKMButtonFlat;


    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_RenderUI,
  KM_AITypes,
  KM_Resource, KM_ResTexts, KM_ResFonts, KM_ResUnits, KM_ResHouses,
  KM_Cursor,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity, KM_Hand,
  KM_UtilsExt,

  KM_GUIGameHouse;


constructor TKMGuiGamePearl.Create(aParent: TKMPanel);
var PT : TKMPearlType;
  panel : TKMPanel;
  I : Integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

  Panel_Build := TKMPanel.Create(self, 0, 15, Width, Height);

    for PT := Low(Button_SelectType) to High(Button_SelectType) do
    begin
      Button_SelectType[PT] := TKMButtonFlat.Create(Panel_Build, 3 + 44 * (byte(PT) - 1), 7, 42, 35, PEARL_ICONS[PT], rxGui);
      Button_SelectType[PT].OnClick := SelectPearl;
      Button_SelectType[PT].Tag := byte(PT);
      Button_SelectType[PT].Hint := gResTexts[byte(PT) + 2208];
    end;

    CostRow := TKMCostsRowMultiMax.Create(Panel_Build, 0, 65, Width, 25);
    CostRow.Caption := gResTexts[2037];
    CostRow.MaxInRow := 3;
    DeliveredRow := TKMCostsRowMultiMax.Create(Panel_Build, 0, 100, Panel_Build.Width, 25);
    DeliveredRow.Caption := gResTexts[2038];
    DeliveredRow.MaxInRow := 3;

    Button_ConfirmBuild := TKMButton.Create(Panel_Build, 1, 150, 180, 25, gResTexts[2207], bsGame);
    Button_ConfirmBuild.OnClick := Button_Confirm;

  Label_PearlType := TKMLabel.Create(self, 0, 5, Width, 20, '', fntOutline, taCenter);
  Label_PearlType.Hitable := false;
  Button_PearlType := TKMButtonFlat.Create(self, 0, Label_PearlType.Bottom, Width, 35, 0);
  Button_PearlType.Hitable := false;

  for PT := Low(Panel_Pearl) to High(Panel_Pearl) do
    Panel_Pearl[PT] := TKMPanel.Create(self, 0, 60, Width, Height);

  //valtaria  -----------------
    panel := Panel_Pearl[ptValtaria];
    TKMBevel.Create(panel, 0, 0, panel.Width, 210);
    for I := low(Button_VWaresFrom) to High(Button_VWaresFrom) do
    begin
      Button_VWaresFrom[I] := TKMButtonFlat.Create(panel, 35 + (I - 1) mod 3 * 40, 5 + (I - 1) div 3 * 42, 34, 38, 0, rxGui);
      Button_VWaresFrom[I].DownColor := $FF00FF00;
      Button_VWaresFrom[I].LineWidth := 2;
      Button_VWaresFrom[I].OnClick := SelectWares;
      Button_NotAcceptWares[I] := TKMButtonFlat.Create(panel, Button_VWaresFrom[I].Right - 15, Button_VWaresFrom[I].Top, 15, 15, 0, rxGuiMain);
      Button_NotAcceptWares[I].OnClickShift := ToggleAcceptWares;

      Button_VWaresTo[I] := TKMButtonFlat.Create(panel, 35 + (I - 1) mod 3 * 40, 123 + (I - 1) div 3 * 42, 34, 38, 0, rxGui);
      Button_VWaresTo[I].DownColor := $FF0000FF;
      Button_VWaresTo[I].LineWidth := 2;
      Button_VWaresTo[I].OnClick := SelectWares;
    end;
    Button_VExchange := TKMButton.Create(panel, 67, 92, 50, 23, 5, rxGui, bsMenu);
    Button_VExchange.OnClickShift := ValtariaExchange;
    Label_VRatioTo := TKMLabel.Create(panel, Button_VExchange.Right + 5, Button_VExchange.Top + 3, 100, 20, '', fntMetal, taLeft);


  //arium  -----------------
    panel := Panel_Pearl[ptArium];
    TKMBevel.Create(panel, 0, 0, panel.Width, 160);
    Button_ACoin := TKMButtonFlat.Create(panel, 74, 9, 34, 38, gRes.Wares.VirtualWares.WareS['vtCoin'].GUIIcon);
    Button_ACoin.Hint := gRes.Wares.VirtualWares.WareS['vtCoin'].Title;

    Label_ARatioFrom := TKMLabel.Create(panel, Button_ACoin.Right + 5, Button_ACoin.Top + 3, 100, 20, '', fntMetal, taLeft);

    Button_AExchange := TKMButton.Create(panel, 67, Button_ACoin.Bottom + 5, 50, 23, 5, rxGui, bsMenu);
    Button_AExchange.OnClickShift := AriumExchange;
    Label_ARatioTo := TKMLabel.Create(panel, Button_AExchange.Right + 5, Button_AExchange.Top + 3, 100, 20, '', fntMetal, taLeft);

    for I := 0 to High(Button_AWares) do
    begin
      Button_AWares[I] := TKMButtonFlat.Create(panel, 11 + I * 40, Button_AExchange.Bottom + 5, 34, 38,
                                                gRes.Wares.VirtualWares[2 + I].GUIIcon);
      Button_AWares[I].Hint := gRes.Wares.VirtualWares[2 + I].Title;
      Button_AWares[I].Tag := I + 2;
      Button_AWares[I].DownColor := $FF0000FF;
      Button_AWares[I].LineWidth := 2;
      Button_AWares[I].OnClick := AriumSelectVWare;
    end;

    Button_Repair := TKMButton.Create(panel, 5, Button_AWares[0].Bottom + 5, 50, 30, 39, rxGui, bsGame);
    Button_Repair.OnClick := ButtonRepair;

  //Agros  -----------------
    panel := Panel_Pearl[ptAgros];
    TKMBevel.Create(panel, 0, 0, panel.Width, 130);

    Button_AgRecruit := TKMButtonFlat.Create(panel, 74, 9, 34, 38, gRes.units[utRecruit].GUIIcon);
    Button_AgRecruit.Hint := gRes.units[utRecruit].GUIName;

    Button_AgExchange := TKMButton.Create(panel, 67, Button_ACoin.Bottom + 5, 50, 23, 5, rxGui, bsMenu);
    Button_AgExchange.Hint := gRes.units[utRecruit].GUIName + ' x1 --> ' + gResTexts[2208] + ' x1';
    Button_AgExchange.OnClickShift := AgrosExchange;

    Button_AgElite := TKMButtonFlat.Create(panel, 74, Button_AgExchange.Bottom + 5, 34, 38, 665);
    Button_AgElite.Hint := gResTexts[2208];


  //ralender  -----------------
    panel := Panel_Pearl[ptRalender];
    TKMBevel.Create(panel, 0, 0, panel.Width, 180);

    Button_RWaresFrom := TKMButtonFlat.Create(panel, 74, 9, 34, 38, gRes.Wares[wtEgg].GUIIcon);
    Button_RWaresFrom.Hint := gRes.Wares[wtEgg].Title;

    Button_RNotAcceptEggs := TKMButtonFlat.Create(panel, Button_RWaresFrom.Right - 15, Button_RWaresFrom.Top, 15, 15, 0, rxGuiMain);
    Button_RNotAcceptEggs.OnClickShift := ToggleAcceptWares;
    for I := low(Button_VWaresFrom) to High(Button_VWaresFrom) do
    begin
      Button_RWaresTo[I] := TKMButtonFlat.Create(panel, 35 + (I - 1) mod 3 * 40, 78 + (I - 1) div 3 * 42, 34, 38, 0, rxGui);
      Button_RWaresTo[I].DownColor := $FF0000FF;
      Button_RWaresTo[I].LineWidth := 2;
      Button_RWaresTo[I].OnClick := RalenderSelectWares;
    end;
    Button_RExchange := TKMButton.Create(panel, 67, 52, 50, 23, 5, rxGui, bsMenu);
    Button_RExchange.OnClickShift := RalenderExchange;
    Label_RRatioTo := TKMLabel.Create(panel, Button_RExchange.Right + 5, Button_RExchange.Top + 3, 100, 20, '', fntMetal, taLeft);

  for PT := Low(Panel_Pearl) to High(Panel_Pearl) do
    Panel_Pearl[PT].SetHeightToChilds;

  Button_UseSpecial := TKMButtonFlat.Create(self, 0, 0, Width, 40, 0);
  Button_UseSpecial.DownColor := $C5FF0000;
  Button_UseSpecial.LineWidth := 3;
  Button_UseSpecial.BackBevelColor := $55A87332;
  Button_UseSpecial.OnClick := UseSpecial;
  Button_UseSpecial.Hint := gResTexts[2213];

  for I := 0 to High(Button_Bonuses) do
  begin
    Button_Bonuses[I] := TKMButtonFlat.Create(self, 3 + I * 36, 0, 33, 33, 0, rxGui);
    Button_Bonuses[I].Hide;
  end;

  Icon_Reload := TKMImage.Create(self, 0, 0, Width, 40, 0);
  Icon_Reload.ImageCenter;
  Icon_Reload.Hitable := false;
end;

procedure TKMGuiGamePearl.Show(aHouse : TKMHouse; aTop : Integer);
var H : TKMHousePearl;
    PT : TKMPearlType;
    I : Integer;
    W : TKMWareType;
begin
  Inherited Show;
  Top := aTop;
  H := TKMHousePearl(aHouse);
  Panel_Build.Hide;
  for PT := Low(Panel_Pearl) to High(Panel_Pearl) do
    Panel_Pearl[PT].Hide;
  Button_UseSpecial.Hide;
  Icon_Reload.Hide;
  Label_PearlType.Caption := H.PearlName;
  Button_PearlType.TexID := PEARL_ICONS[H.PearlType];
  Button_PearlType.Visible := H.Confirmed;

  case H.PearlType of
    ptValtaria : Label_PearlType.FontColor := $FF00CC30;
    ptArium : Label_PearlType.FontColor := $FF9999FF;
    ptAgros : Label_PearlType.FontColor := $FF99FFB1;
    ptRalender : Label_PearlType.FontColor:= $FFFF33F3;
    else Label_PearlType.FontColor := $FFFFFFFF;
  end;
  Button_PearlType.BackBevelColor := Label_PearlType.FontColor and $55FFFFFF;

  If not H.Completed then
  begin
    for I := 0 to High(Button_Bonuses) do
      Button_Bonuses[I].Hide;
    Panel_Build.Show;
    CostRow.WarePlan := H.BuildCost;
    DeliveredRow.Top := CostRow.Bottom + 20;
    DeliveredRow.WarePlan := H.Delivered;
    Button_ConfirmBuild.Top := CostRow.Bottom + 20;
    Button_ConfirmBuild.Visible := not H.Confirmed;
    Button_ConfirmBuild.Enabled := H.PearlType <> ptNone;
    DeliveredRow.Visible := H.Confirmed;

    DeliveredRow.GreenToIndex := H.BuildStage;
    DeliveredRow.RedIndex := H.BuildStage + 1;
    DeliveredRow.RedIndex := 0;
    DeliveredRow.YellowIndex := 0;
    if H.PearlType <> ptNone then
      If H.Delivered[H.BuildStage].C = H.BuildCost[H.BuildStage].C then
        DeliveredRow.YellowIndex := H.BuildStage + 1
      else
        DeliveredRow.RedIndex := H.BuildStage + 1;

    for PT := low(Button_SelectType) to High(Button_SelectType) do
    begin
      Button_SelectType[PT].Visible := not H.Confirmed;
      Button_SelectType[PT].Down := PT = H.PearlType;
    end;

    Exit;
  end;

  Panel_Pearl[H.PearlType].Show;
  case H.PearlType of
    ptValtaria :  begin
                    for I := 1 to WARES_IN_OUT_COUNT do
                    begin
                      W := H.WareInput[I];
                      Button_NotAcceptWares[I].Visible := W in WARES_VALID;
                      Button_VWaresFrom[I].Visible := W in WARES_VALID;
                      Button_VWaresFrom[I].TexID := gRes.Wares[W].GUIIcon;
                      Button_VWaresFrom[I].Hint := gRes.Wares[W].Title;
                      Button_VWaresFrom[I].Down := H.ResFrom = W;
                      Button_VWaresFrom[I].Tag := ord(W);
                      Button_VWaresFrom[I].Caption := H.ResIn[I].ToString;
                      Button_NotAcceptWares[I].TexID := IfThen(H.GetAcceptWareIn(W) > 0, 32, 33);

                      W := H.WareOutput[I];
                      Button_VWaresTo[I].Visible := W in WARES_VALID;
                      Button_VWaresTo[I].TexID := gRes.Wares[W].GUIIcon;
                      Button_VWaresTo[I].Hint := gRes.Wares[W].Title;
                      Button_VWaresTo[I].Tag := ord(W);
                      Button_VWaresTo[I].Down := H.ResTo = W;
                      Button_VWaresTo[I].Caption := H.ResOut[I].ToString;
                    end;
                    Button_VExchange.Hint := Format(gResTexts[1988], [gRes.Wares[H.ResFrom].Title, H.ValtariaRatioFrom,
                                                                      gRes.Wares[H.ResTo].Title, H.ValtariaRatioTo]);
                    Label_VRatioTo.Caption := 'x' + H.ValtariaRatioTo.ToString;
                  end;

    ptArium : begin
                for I := 0 to high(Button_AWares) do
                begin
                  Button_AWares[I].Caption := gHands[H.Owner].VirtualWare[Button_AWares[I].Tag].ToString;
                  Button_AWares[I].Down := H.VResTo = Button_AWares[I].Tag;
                end;

                Button_ACoin.Caption := gHands[H.Owner].VirtualWare['vtCoin'].ToString;

                Button_AExchange.Hint := Format(gResTexts[1988], [gRes.Wares.VirtualWares.WareS['vtCoin'].Title, H.AriumRatioFrom,
                                                                  gRes.Wares.VirtualWares[H.VResTo].Title, H.AriumRatioTo]);
                Label_ARatioFrom.Caption := 'x' + H.AriumRatioFrom.ToString;
                Label_ARatioTo.Caption := 'x' + H.AriumRatioTo.ToString;
              end;

    ptAgros : begin
                Button_AgRecruit.Down := true;
                Button_AgRecruit.DownColor := IfThen(H.HasWorker, $FF00FF00, $FF0000FF);
              end;
    ptRalender :  begin
                    for I := 1 to WARES_IN_OUT_COUNT do
                    begin
                      W := H.WareOutput[I];
                      Button_RWaresTo[I].Visible := W in WARES_VALID;
                      Button_RWaresTo[I].TexID := gRes.Wares[W].GUIIcon;
                      Button_RWaresTo[I].Hint := gRes.Wares[W].Title;
                      Button_RWaresTo[I].Tag := ord(W);
                      Button_RWaresTo[I].Down := H.RResTo = W;
                      Button_RWaresTo[I].Caption := H.ResOut[I].ToString;
                    end;
                    Button_RWaresFrom.Caption := H.CheckWareIn(wtEgg).ToString;
                    Button_RNotAcceptEggs.TexID := IfThen(H.GetAcceptWareIn(W) > 0, 32, 33);

                    Button_RExchange.Hint := Format(gResTexts[1988], [gRes.Wares[wtEgg].Title, H.RalenderRatioFrom,
                                                                      gRes.Wares[H.RResTo].Title, H.RalenderRatioTo]);
                    Label_RRatioTo.Caption := 'x' + H.RalenderRatioTo.ToString;
                  end;
  end;
  Button_UseSpecial.Show;
  Icon_Reload.Show;
  Button_UseSpecial.Top := Panel_Pearl[H.PearlType].Bottom;
  Icon_Reload.Top := Button_UseSpecial.Top;

  case H.PearlType of
    ptValtaria : Icon_Reload.TexID := 1002;
    ptArium : Icon_Reload.TexID := 1003;
    ptAgros : Icon_Reload.TexID := 1004;
    ptRalender : Icon_Reload.TexID := 1005;
  end;

  Button_UseSpecial.Hint := gResTexts[2213] + ': ' + gResTexts[byte(H.PearlType) + 2213];

  Button_UseSpecial.BackBevelColor := Label_PearlType.FontColor and $55FFFFFF;
  Button_UseSpecial.DownColor := Label_PearlType.FontColor and $C5FFFFFF;
  Button_UseSpecial.Enabled := H.ReloadProgress = 1;
  Button_UseSpecial.Down := H.ReloadProgress = 1;
  If not Button_UseSpecial.Enabled then
    Button_UseSpecial.BackBevelColor := Button_UseSpecial.BackBevelColor and $33AAAAAA;

  Icon_Reload.AlphaStep := H.ReloadProgress;

  If H.Completed then
    ShowBonuses(H.PearlType, Button_UseSpecial.Bottom + 10);
end;

procedure TKMGuiGamePearl.ShowBonuses(aPearlType: TKMPearlType; aTop: Integer);
const BONUS_ICON : array[TKMPearlType] of array[0..4] of Word = (
      (0, 0, 0, 0, 0),
      (1010, 1011, 1012, 1013, 0),
      (1014, 1015, 1016, 1017, 1018),
      (1019, 1020, 1021, 1022, 1023),
      (1024, 1025, 1026, 1027, 0)
      );
      BONUS_HINT : array[TKMPearlType] of array[0..4] of Word = (
      (0, 0, 0, 0, 0),
      (2218, 2219, 2220, 2221, 0),
      (2222, 2223, 2224, 2225, 2226),
      (2227, 2228, 2229, 2230, 2231),
      (2232, 2233, 2234, 2235, 0)
      );
var I : Integer;
begin
  for I := 0 to High(Button_Bonuses) do
    Button_Bonuses[I].Top := aTop;

  fLastPearlType := aPearlType;

  for I := 0 to High(Button_Bonuses) do
  begin
    Button_Bonuses[I].Hint := gResTexts[BONUS_HINT[fLastPearlType, I]];
    Button_Bonuses[I].TexID := BONUS_ICON[fLastPearlType, I];
    Button_Bonuses[I].Visible := Button_Bonuses[I].TexID > 0;
  end;
  SortControls(0, aTop + 5,  182, 4, [Button_Bonuses[0], Button_Bonuses[1], Button_Bonuses[2], Button_Bonuses[3], Button_Bonuses[4]], false, true);

end;

procedure TKMGuiGamePearl.SelectPearl(Sender: TObject);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlSelectType, H, TKMControl(Sender).Tag);
end;

procedure TKMGuiGamePearl.Button_Confirm(Sender: TObject);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlConfirm, H);
end;

procedure TKMGuiGamePearl.SelectWares(Sender: TObject);
var I : Integer;
    H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  for I := 1 to WARES_IN_OUT_COUNT do
    If Sender = Button_VWaresFrom[I] then
      gGame.GameInputProcess.CmdHouse(gicPearlSelectResFrom, H, Button_VWaresFrom[I].Tag)
    else
    If Sender = Button_VWaresTo[I] then
      gGame.GameInputProcess.CmdHouse(gicPearlSelectResTo, H, Button_VWaresTo[I].Tag);

end;

procedure TKMGuiGamePearl.ValtariaExchange(Sender: TObject; Shift: TShiftState);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlDoExchange, H, IfThen((ssRight in Shift) or (ssShift in Shift), 10, 1));
end;

procedure TKMGuiGamePearl.AriumSelectVWare(Sender: TObject);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  H.VResTo := TKMControl(Sender).Tag;
  gGame.GameInputProcess.CmdHouse(gicPearlSelectVResTo, H, TKMControl(Sender).Tag);
end;

procedure TKMGuiGamePearl.AriumExchange(Sender: TObject; Shift: TShiftState);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlDoExchange, H, IfThen((ssRight in Shift) or (ssShift in Shift), 10, 1));
end;

procedure TKMGuiGamePearl.AgrosExchange(Sender: TObject; Shift: TShiftState);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlDoExchange, H, IfThen((ssRight in Shift) or (ssShift in Shift), 10, 1));
end;

procedure TKMGuiGamePearl.RalenderSelectWares(Sender: TObject);
var I : Integer;
    H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  for I := 1 to WARES_IN_OUT_COUNT do
    If Sender = Button_RWaresTo[I] then
      gGame.GameInputProcess.CmdHouse(gicPearlSelectRResTo, H, Button_RWaresTo[I].Tag);
end;

procedure TKMGuiGamePearl.RalenderExchange(Sender: TObject; Shift: TShiftState);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlDoExchange, H, IfThen((ssRight in Shift) or (ssShift in Shift), 10, 1));
end;

procedure TKMGuiGamePearl.UseSpecial(Sender: TObject);
var H : TKMHousePearl;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicPearlUseSpecial, H);
end;

procedure TKMGuiGamePearl.ButtonRepair(Sender: TObject);
begin
  gCursor.Mode := cmPearlRepair;
end;

procedure TKMGuiGamePearl.ToggleAcceptWares(Sender : TObject; Shift : TShiftState);
var H : TKMHousePearl;
  I : integer;
begin
  H := TKMHousePearl(gMySpectator.Selected);
  If Sender = Button_RNotAcceptEggs then
    if H.GetAcceptWareIn(wtEgg) > 0 then
      gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, wtEgg, -1000)
      else
      gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, wtEgg, 1000);

  for I := 1 to High(Button_NotAcceptWares) do
    if Sender = Button_NotAcceptWares[I] then
      if H.GetAcceptWareIn(H.WareInput[I]) > 0 then
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, H.WareInput[I], -1000)
      else
        gGame.GameInputProcess.CmdHouse(gicHouseDeliveryToggle, H, H.WareInput[I], 1000);

end;

end.

