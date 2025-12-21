unit KM_GUIGameHouseST;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase,
  KM_Houses, KM_HouseArena;

type
  TKMGuiGameSiegeTower = class(TKMPanel)
    private
      procedure Refresh(Tower : TKMHouseSiegeTower);
      procedure UnitClicked(Sender : TObject);
    protected
      Button_Unit : array[0..TKMHouseSiegeTower.MAX_UNITS_INSIDE - 1] of TKMButtonFlat;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;

implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts,
  KM_UnitWarrior;


constructor TKMGuiGameSiegeTower.Create(aParent: TKMPanel);
var I : integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

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

end;

procedure TKMGuiGameSiegeTower.UnitClicked(Sender: TObject);
var I : Integer;
  W : TKMUnitWarrior;
  ST : TKMHouseSiegeTower;
begin
  I := TKMControl(Sender).Tag;
  ST := TKMHouseSiegeTower(gMySpectator.Selected);
  W := TKMUnitWarrior(ST.GetWorker(I));
  W.OrderLeaveSiegeTower;
  gGame.GameInputProcess.CmdUnit(gicWarriorLeaveTower, W);
end;

end.

