unit KM_GUIGameHouseArena;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes, KM_ResDevelopment,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HouseArena;

type

  TKMGuiGameArena = class(TKMPanel)
    private
      procedure Refresh(Arena : TKMHouseArena);
    protected
      Button_Points : array[DEVELOPMENT_MIN..DEVELOPMENT_MAX] of TKMButtonFlat;
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
var dtt: TKMDevelopmentTreeType;
  I : Integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

  for dtt := Low(Button_Points) to High(Button_Points) do
  begin
    I := byte(dtt);
    Button_Points[dtt] := TKMButtonFlat.Create(self, I * 35, 0, 32, 32, TREE_TYPE_ICON[dtt]);
    Button_Points[dtt].Caption := '';
  end;

end;

procedure TKMGuiGameArena.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseArena(aHouse));
end;

procedure TKMGuiGameArena.Refresh(Arena: TKMHouseArena);
var dtt: TKMDevelopmentTreeType;
begin

  for dtt := Low(Button_Points) to High(Button_Points) do
  begin
    Button_Points[dtt].Caption := gMySpectator.Hand.DevPoints(dtt).ToString;
  end;

end;



end.

