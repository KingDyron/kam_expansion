unit KM_GUIGameHouseForest;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HousePearl;

type
  TKMGuiGameForest = class(TKMPanel)
    private
    protected
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess;


constructor TKMGuiGameForest.Create(aParent: TKMPanel);
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);
end;

procedure TKMGuiGameForest.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
end;

end.

