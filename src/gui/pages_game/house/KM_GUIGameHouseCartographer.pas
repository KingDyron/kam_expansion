unit KM_GUIGameHouseCartographer;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_Controls, KM_ControlsBase, KM_ControlsMinimapView, KM_ControlsSwitch,
  KM_MinimapCartographer,
  KM_Houses, KM_HouseCartographers;

type
  TKMGuiGameCartographer = class(TKMPanel)
    private
      fLastPlayer : Integer;
      fLastUpdateTick : Cardinal;
      procedure ChangeMode(Sender : TObject);
      procedure Button_ToggleMode(Sender : TObject);
      procedure SelectPlayer(Sender : TObject);

      function GetArmyCaption(aCount : Integer) : String;
    protected
      Button_Type: array[TKMCartographersMode] of TKMButton;
      Panel_Cartographer : TKMPanel;
        Minimap : TKMMinimapCartographerView;
        Button_Toggle : array[TKMCartographersPaintLayer] of TKMButtonFlat;
      Panel_Spy : TKMPanel;
        Button_SelectPlayer : array[0..MAX_HANDS - 1] of TKMControl;
        Panel_Stats : TKMPanel;
          Button_SpyPlayer,
          Button_ArmyCount,
          Button_BestUnit : TKMButtonFlat;
          Button_GroupsCount : array[GROUP_TYPE_MIN..GROUP_TYPE_MAX] of TKMButtonFlat;

          Button_Houses : array[0..STRATEGY_HOUSES - 1] of TKMControl;

          Label_Citizens,
          Label_Killed,
          Label_Weapons : TKMLabel;
          //AI
          CheckBox_UnlimitedEquip,
          CheckBox_AutoRepair : TKMCheckBox;
          Label_Stats: TKMLabel;

    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;

      procedure UpdateState(aGlobalTickCount : Cardinal); override;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_RenderUI,
  KM_AITypes,
  KM_Resource, KM_ResTypes, KM_ResTexts, KM_ResFonts,
  KM_HandsCollection, KM_HandTypes, KM_HandEntity,
  KM_UtilsExt,

  KM_GUIGameHouse;

constructor TKMGuiGameCartographer.Create(aParent: TKMPanel);
const CPL_HINT : array[TKMCartographersPaintLayer] of Word = (1, 2, 3, 4, 5);
      CPL_TEXID : array[TKMCartographersPaintLayer] of Word = (972, 994, 991, 992, 993);
      CM_HINT : array[TKMCartographersMode] of Word = (1, 2);
      CM_TEXID : array[TKMCartographersMode] of Word = (996, 995);
var CPL : TKMCartographersPaintLayer;
    CM : TKMCartographersMode;
    GT : TKMGroupType;
    I, J, top : Integer;

    function NextTop(aTop : Integer) : Integer;
    begin
      Result := top;
      inc(top, aTop);
    end;

begin
  Inherited Create(aParent, 0, 100, aParent.Width, 600);
  fLastPlayer := -1;
  for CM := Low(TKMCartographersMode) to High(TKMCartographersMode) do
  begin
    Button_Type[CM] := TKMButton.Create(self, 10 + ord(CM) * 37, 5, 36, 33, CM_TEXID[CM], rxGui, bsGame);
    Button_Type[CM].Hint := gResTexts[CM_HINT[CM]];
    Button_Type[CM].Tag := ord(CM);
    Button_Type[CM].OnClick := ChangeMode;
  end;
    SortVisibleControls(-3, 5, Width, 80, [Button_Type[cmChartman], Button_Type[cmSpy]], false, true);
  Panel_Cartographer := TKMPanel.Create(self, 0, 45, Width - 5, Height - 45);

    Minimap := TKMMinimapCartographerView.Create(Panel_Cartographer, 0, 0, Panel_Cartographer.Width, Panel_Cartographer.Width);

    for CPL := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
    begin
      Button_Toggle[CPL] := TKMButtonFlat.Create(Panel_Cartographer, ord(CPL) * 38, Minimap.Bottom + 5, 33, 33, CPL_TEXID[CPL]);
      Button_Toggle[CPL].OnClick := Button_ToggleMode;
      Button_Toggle[CPL].Tag := ord(CPL);
      Button_Toggle[CPL].Hint := gResTexts[CPL_HINT[CPL]];
      Button_Toggle[CPL].LineWidth := 2;
      Button_Toggle[CPL].DownColor := $FF00FF00;
    end;

  Panel_Spy := TKMPanel.Create(self, 0, 45, Width, Height - 45);
    TKMBevel.Create(Panel_Spy, 0, 0, 189, 63);
    for I := 0 to High(Button_SelectPlayer) do
    begin
      Button_SelectPlayer[I] := TKMFlatButtonShape.Create(Panel_Spy, (I mod 9) * 21, (I div 9) * 21, 21, 21, IntToStr(I + 1), fntGrey, 0);
      Button_SelectPlayer[I].OnClick := SelectPlayer;
      Button_SelectPlayer[I].Tag := I;
    end;
    Button_SpyPlayer := TKMButtonFlat.Create(Panel_Spy, 167, 2, 20, 20, 32, rxGuiMain);
    Button_SpyPlayer.OnClick := SelectPlayer;
    Button_SpyPlayer.Hint := 'Disable/Enable spying selected player';

    top := Button_SelectPlayer[high(Button_SelectPlayer)].Bottom + 7;
    Panel_Stats := TKMPanel.Create(Panel_Spy, 0, top, Width, Panel_Spy.Height - top);
      top := 0;
      TKMBevel.Create(Panel_Stats, 0, top, 189, 135);

      Button_ArmyCount := TKMButtonFlat.Create(Panel_Stats, 10, top + 3, 70, 35, 665);
      Button_BestUnit := TKMButtonFlat.Create(Panel_Stats, 142, top + 3, 35, 35, 0);
      top := Button_BestUnit.Bottom + 5;

      TKMLabel.Create(Panel_Stats, 5, NextTop(17), 150, 15, 'Groups count:', fntMetal, taLeft);
      for GT := Low(Button_GroupsCount) to High(Button_GroupsCount) do
      begin
        J := ord(GT) - 2;
        Button_GroupsCount[GT] := TKMButtonFlat.Create(Panel_Stats, 30 + (J mod 4) * 33, top + (J div 4) * 38, 30, 38, GROUP_TYPE_GUI_ICON[GT]);
        Button_GroupsCount[GT].Hint := gResTexts[GROUP_TYPE_GUI_TEXT[GT]];
        Button_GroupsCount[GT].Caption := '0';
      end;
      top := Button_GroupsCount[high(Button_GroupsCount)].Bottom + 7;

      TKMBevel.Create(Panel_Stats, 0, top, 189, 90);
      for I := 0 to High(Button_Houses) do
      begin
        Button_Houses[I] := TKMButtonFlat.Create(Panel_Stats, 0, top, 30, 35, gRes.Houses[TKMHouseCartographers.StrategyHouse(I)].GUIIcon);
        Button_Houses[I].Hint := gRes.Houses[TKMHouseCartographers.StrategyHouse(I)].HouseName;
        TKMButtonFlat(Button_Houses[I]).Caption := '0';
      end;

      SortControls(0, top + 5,  189, 10, Button_Houses, false, true);
      top := Button_Houses[high(Button_Houses)].Bottom + 7;

      TKMBevel.Create(Panel_Stats, 0, top, 189, 65);
      Inc(top, 3);
      Label_Citizens := TKMLabel.Create(Panel_Stats, 5, NextTop(20), 189, 20, 'Citizens: ', fntMetal, taLeft);
      Label_Killed := TKMLabel.Create(Panel_Stats, 5, NextTop(20), 189, 20, 'Warriors killed: ', fntMetal, taLeft);
      Label_Weapons := TKMLabel.Create(Panel_Stats, 5, NextTop(20), 189, 20, 'Weapons: ', fntMetal, taLeft);
      //AI
      Inc(top, 5);
      TKMBevel.Create(Panel_Stats, 0, top, 189, 250);
      Inc(top, 3);
      CheckBox_UnlimitedEquip := TKMCheckBox.Create(Panel_Stats, 5, NextTop(20), 189, 20, gResTexts[811], fntMetal);
      CheckBox_UnlimitedEquip.Hitable := false;
      CheckBox_AutoRepair := TKMCheckBox.Create(Panel_Stats, 5, NextTop(20), 189, 20, gResTexts[489], fntMetal);
      CheckBox_AutoRepair.Hitable := false;
      Label_Stats := TKMLabel.Create(Panel_Stats, 5, NextTop(120), 180, 120, 'Auto attack range: ', fntMetal, taLeft);
      Label_Stats.WordWrap := true;
  Panel_Spy.Hide;
end;

procedure TKMGuiGameCartographer.Show(aHouse : TKMHouse; aTop : Integer);
const
  ARMY_TYPE_TEXTID: array[TKMArmyType] of Word = (TX_MAPED_AI_ARMY_TYPE_IRON_THEN_LEATHER,
                                                TX_MAPED_AI_ARMY_TYPE_IRON,
                                                TX_MAPED_AI_ARMY_TYPE_LEATHER,
                                                TX_MAPED_AI_ARMY_TYPE_MIXED);
  TARGET_TEXTID: array[TKMAIAttackTarget] of word = (TX_MAPED_AI_TARGET_CLOSEST,
                                                     TX_MAPED_AI_TARGET_HOUSE_ARMY,
                                                     TX_MAPED_AI_TARGET_HOUSE_START,
                                                     TX_MAPED_AI_TARGET_CUSTOM);
var CPL : TKMCartographersPaintLayer;
    CM : TKMCartographersMode;
    H : TKMHouseCartographers;
    I : Integer;
    player : TKMSpyPlayerData;
    GT : TKMGroupType;
    S : String;
    AAT : TKMAIAttackTarget;
begin
  Inherited Show;
  Panel_Cartographer.Hide;
  Panel_Spy.Hide;
  Top := aTop;
  H := TKMHouseCartographers(aHouse);
  for CM := Low(TKMCartographersMode) to High(TKMCartographersMode) do
    Button_Type[CM].ShowImageEnabled := H.Mode = CM;

  If H.Mode = cmSpy then
  begin
    Panel_Spy.Show;
    Button_SpyPlayer.TexID := IfThen(H.DoSpying, 33, 32);
    If fLastPlayer <> H.Owner then
    begin
      fLastPlayer := H.Owner;
      for I := 0 to High(Button_SelectPlayer) do
      begin
        Button_SelectPlayer[I].Visible := (I < gHands.Count) and gHands[I].Enabled and (I <> H.Owner);
        If Button_SelectPlayer[I].Visible then
          TKMFlatButtonShape(Button_SelectPlayer[I]).ShapeColor := gHands[I].FlagColor;

      end;
      SortControls(0, 0, 189, 63, 10, Button_SelectPlayer);
    end;
    for I := 0 to High(Button_SelectPlayer) do
      TKMFlatButtonShape(Button_SelectPlayer[I]).Down := H.PlayerToSpy = I;
    player := H.SpiedPlayer;
    Panel_Stats.Hide;
    IF player.ID = -1 then
      Exit;
    Panel_Stats.Show;

    Button_ArmyCount.Caption := GetArmyCaption(player.Army);

    Button_BestUnit.TexID := gRes.Units[player.StrongestUnit].GUIIcon;
    for GT := Low(Button_GroupsCount) to High(Button_GroupsCount) do
      Button_GroupsCount[GT].Caption := player.Groups[GT].ToString;
    for I := 0 to High(Button_Houses) do
      TKMButtonFlat(Button_Houses[I]).Caption := player.WarHouses[I].ToString;

    Label_Citizens.Caption :=  gResTexts[267]+': ' + player.Citizens.ToString;
    Label_Killed.Caption := gResTexts[2193]+': ' + player.WarriorsKilled.ToString;
    Label_Weapons.Caption := gResTexts[2194]+': ' + player.Weapons.ToString;
    //AI
    CheckBox_UnlimitedEquip.Checked := player.UnlimitedEquip;
    CheckBox_AutoRepair.Checked := player.AutoRepair;

    S := gResTexts[2195] + ':|   ' + player.AutoAttackRange.ToString + '|'
         + gResTexts[817] + ':|  ' + gResTexts[ARMY_TYPE_TEXTID[player.ArmyType]] + '|';
    I := 0;
    for AAT := low(TKMAIAttackTarget) to high(TKMAIAttackTarget) do
    If player.AttackTarget[AAT] then
    begin
      If I = 0 then
        S := S + gResTexts[2192] + ':';
      S := S + '|  ' + gResTexts[TARGET_TEXTID[AAT] ];
      Inc(I);
    end;


    Label_Stats.Caption := S
    //Label_AutoAttackRange.Caption := 'Attack Range:' + player.AutoAttackRange.ToString;
    //Label_ArmyType.Caption := 'ArmyType:|   ' + gResTexts[ARMY_TYPE_TEXTID[player.ArmyType]];
    //Label_Targets.Caption := 'Targets: ' + player.AttackTarget;
  end else
  begin
    Panel_Cartographer.Show;
    Minimap.SetMinimap(H.Minimap);
    for CPL := Low(TKMCartographersPaintLayer) to High(TKMCartographersPaintLayer) do
      Button_Toggle[CPL].Down := H.Layer[CPL];
  end;


end;

procedure TKMGuiGameCartographer.UpdateState(aGlobalTickCount: Cardinal);
begin
  Inherited;

  If aGlobalTickCount > fLastUpdateTick then//update minimap every 5 seconds
  begin
    If Minimap.Visible then
      Minimap.UpdateMinimap;
    fLastUpdateTick := aGlobalTickCount + 50;
  end;
end;

procedure TKMGuiGameCartographer.Button_ToggleMode(Sender: TObject);
var H : TKMHouseCartographers;
begin
  H := TKMHouseCartographers(gMySpectator.Selected);
  //H.ToggleLayer(TKMButtonFlat(Sender).Tag)
  gGame.GameInputProcess.CmdHouse(gicCartographersToggleView, H, TKMControl(Sender).Tag);
end;

procedure TKMGuiGameCartographer.ChangeMode(Sender : TObject);
var H : TKMHouseCartographers;
begin
  H := TKMHouseCartographers(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicCartographersMode, H, TKMControl(Sender).Tag);
end;

procedure TKMGuiGameCartographer.SelectPlayer(Sender: TObject);
var H : TKMHouseCartographers;
begin
  H := TKMHouseCartographers(gMySpectator.Selected);
  If Sender = Button_SpyPlayer then
    gGame.GameInputProcess.CmdHouse(gicCartographersDoSpying, H)
    //H.DoSpying := not H.DoSpying
  else
    gGame.GameInputProcess.CmdHouse(gicCartographersSelectPlayer, H, TKMControl(Sender).Tag);
   //H.PlayerToSpy := TKMControl(Sender).Tag;
end;

function TKMGuiGameCartographer.GetArmyCaption(aCount : Integer) : String;
const QT_RANGE : array[0..9] of word = (1, 16, 32, 64, 128, 256, 512, 732, 999, 9999);
var I, id : Integer;
begin
  id := 1;
  for I := High(QT_RANGE) downto 1 do
    If aCount < QT_RANGE[I] then
      id := I;  //last largest number larger than aCount

  Result := '0-0';

  If id > 0 then
    Result := Format('%d-%d', [QT_RANGE[id - 1], QT_RANGE[id] ]);

end;

end.
