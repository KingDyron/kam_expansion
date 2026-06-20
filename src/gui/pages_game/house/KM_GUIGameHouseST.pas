unit KM_GUIGameHouseST;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch,
  KM_Houses;

type
  TKMGuiGameSiegeTower = class(TKMPanel)
    private
      procedure Refresh(Tower : TKMHouseSiegeTower);
      procedure UnitClicked(Sender : TObject);
      procedure ButtonClickShift(Sender : TObject; Shift : TShiftState);
      procedure SetMode(Sender : TObject);
    protected
      Button_Unit : array[0..TKMHouseSiegeTower.MAX_UNITS_INSIDE - 1] of TKMButtonFlat;
      //DropList_Mode : TKMRadioGroup;
      Switch_Mode : TKMSwitch;
      Button_BuyDinner: TKMButtonFlat;
      Label_Arrow : TKMLabel;
      Button_Coin, Button_Dinner : TKMButtonFlat;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
      procedure Paint; override;
  end;

implementation
uses
  KM_CommonTypes,
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI, KM_Cursor,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts,
  KM_UnitWarrior;


constructor TKMGuiGameSiegeTower.Create(aParent: TKMPanel);
const
  SIEGE_TOWER_MODE_TEXT: array[TKMSiegeTowerMode] of Word = (2368, 2369, 2370, 2371, 2372, 2373, 2374, 2375);
var I, top : integer;
  stm : TKMSiegeTowerMode;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 400);

  for I := 0 to High(Button_Unit) do
  begin
    Button_Unit[I] := TKMButtonFlat.Create(self, 3 + I mod 5 * 35, I div 5 * 47, 33, 44, 0, rxGui);
    Button_Unit[I].OnClick := UnitClicked;
    Button_Unit[I].Down := true;
    Button_Unit[I].LineWidth := 2;
    Button_Unit[I].DownColor := icWhite;
    Button_Unit[I].TexID := 0;
    Button_Unit[I].Caption := '';
    //Button_Unit[I].Hide;
    Button_Unit[I].Tag := I;
  end;
  top := 100;
  TKMLabel.Create(self, 2, top, Width, 20, gResTexts[2367], fntMetal, taCenter);
  top := 117;
  {
  DropList_Mode := TKMRadioGroup.Create(self, 0, top, Width, 125, fntGrey);
  DropList_Mode.Hint := gResTexts[2367];
  for stm := stmRandom to high(TKMSiegeTowerMode) do
    DropList_Mode.Add(gResTexts[SIEGE_TOWER_MODE_TEXT[stm] ]);
  DropList_Mode.OnChange := SetMode;}
  Switch_Mode := TKMSwitch.Create(self, 0, top, Width, 35);
  for stm := stmRandom to high(TKMSiegeTowerMode) do
    Switch_Mode.Add(byte(stm) + 1223, gResTexts[SIEGE_TOWER_MODE_TEXT[stm]]);
  Switch_Mode.OnChange := SetMode;
  Switch_Mode.Offset := Width div 2 - 20;
  Switch_Mode.MainColor := TKMColor3f.New(0, 1, 0);

  Inc(top, 40);
  TKMLabel.Create(self, 2, top, Width, 20, gResTexts[2365], fntMetal, taCenter);
  Button_BuyDinner:= TKMButtonFlat.Create(self, Width div 2 - 16, top + 20, 33, 44, gRes.Wares.VirtualWares.WareS['vtDinner'].GUIIcon, rxGui);
  Button_BuyDinner.Hint := gResTexts[2366];
  Button_BuyDinner.Caption := '0';
  Button_BuyDinner.OnClickShift := ButtonClickShift;

  Label_Arrow := TKMLabel.Create(self, 0, top + 75, Width, 20, '-->', fntMetal, taCenter);
  Button_Coin := TKMButtonFlat.Create(self, Width div 2 - 50 - 16, top + 60, 33, 44, gRes.Wares.VirtualWares.WareS['vtCoin'].GUIIcon, rxGui);
  Button_Coin.Caption := 'x' + Round(gRes.Wares.VirtualWares.WareS['vtDinner'].CoinPrice).ToString;
  Button_Dinner := TKMButtonFlat.Create(self, Width div 2 + 50 - 16, top + 60, 33, 44, gRes.Wares.VirtualWares.WareS['vtDinner'].GUIIcon, rxGui);
  Button_Dinner.Caption := 'x1';
end;

procedure TKMGuiGameSiegeTower.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseSiegeTower(aHouse));
end;

procedure TKMGuiGameSiegeTower.Refresh(Tower: TKMHouseSiegeTower);
var I, minC, curC : Integer;
  W : TKMUnitWarrior;
begin
  curC := Tower.GetTotalWeight;
  minC := Tower.WorkersCount;
  for I := 0 to High(Button_Unit) do
  begin
    //Button_Unit[I].Hide;
    Button_Unit[I].DownColor := icWhite;
    Button_Unit[I].BackBevelColor := $00000000;
    Button_Unit[I].TexID := 0;
    Button_Unit[I].Caption := '';
    Button_Unit[I].Enabled := false;
    If I >= minC + Tower.MAX_UNITS_INSIDE - curC then
    begin
      Button_Unit[I].DownColor := icRed;
      Button_Unit[I].BackBevelColor := icRed and $40FFFFFF;
    end;
  end;

  for I := 0 to Tower.WorkersCount - 1 do
  begin
    If not (TObject(Tower.GetWorker(I)) is TKMUnitWarrior) then
      Continue;
    W := TKMUnitWarrior(Tower.GetWorker(I));
    with Button_Unit[I] do
    begin
      TexID := gRes.Units[W.UnitType].GuiIcon;
      minC := W.MinAmmoCountToOrder;
      curC := W.BoltCount;

      If curC > minC then
        DownColor := icGreen
      else
      If curC > minC * 0.5 then
        DownColor := icYellow
      else
      If curC > 0 then
        DownColor := icOrange
      else
        DownColor := icRed;
      Caption := curC.ToString;
      Button_Unit[I].Enabled := true;
      If W.NextOrder = woLeaveSiegeTower then
      Button_Unit[I].BackBevelColor := $40FFB700;
      //Enabled := W.IsShootingFromTower;
      //Show;
    end;
  end;
  Button_BuyDinner.Caption := gMySpectator.Hand.SiegeTowerDinner.ToString;
  Switch_Mode.Selected := byte(Tower.Mode);
end;

procedure TKMGuiGameSiegeTower.UnitClicked(Sender: TObject);
var I : Integer;
  W : TKMUnitWarrior;
  ST : TKMHouseSiegeTower;
begin
  I := TKMControl(Sender).Tag;
  ST := TKMHouseSiegeTower(gMySpectator.Selected);
  W := TKMUnitWarrior(ST.GetWorker(I));
  gGame.GameInputProcess.CmdUnit(gicWarriorLeaveTower, W);
end;

procedure TKMGuiGameSiegeTower.ButtonClickShift(Sender: TObject; Shift: TShiftState);
var I : Integer;
  ST : TKMHouseSiegeTower;
begin
  I := 1;
  If ssShift in Shift then
    I := I * 5;
  If ssRight in Shift then
    I := I * 5;
  If ssCtrl in Shift then
    I := I * 5;
  If ssAlt in Shift then
    I := I * 5;
  ST := TKMHouseSiegeTower(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicSiegeTowerDinner, ST, I);
end;

procedure TKMGuiGameSiegeTower.SetMode(Sender: TObject);
var
  ST : TKMHouseSiegeTower;
begin
  ST := TKMHouseSiegeTower(gMySpectator.Selected);
  gGame.GameInputProcess.CmdHouse(gicSiegeTowerMode, ST, Switch_Mode.Selected);
end;


procedure TKMGuiGameSiegeTower.Paint;
begin
  Label_Arrow.Visible := MasterControl.CtrlOver = Button_BuyDinner;
  Button_Coin.Visible := Label_Arrow.Visible;
  Button_Dinner.Visible := Label_Arrow.Visible;

  Inherited;
end;

end.

