unit KM_GUIGameHouseArena;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HouseArena;

type

  TKMGuiGameArena = class(TKMPanel)
    private
    protected
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResUnits, KM_ResFonts;


constructor TKMGuiGameArena.Create(aParent: TKMPanel);
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);
end;

procedure TKMGuiGameArena.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
end;

end.

