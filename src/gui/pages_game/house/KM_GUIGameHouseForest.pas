unit KM_GUIGameHouseForest;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HouseForest;

type
  TKMGuiGameForest = class(TKMPanel)
    private
      fForest : TKMHouseForest;
      procedure PlantTree_Clicked(Sender : TObject; Shift : TShiftState);
    protected
      Panel_Shop : TKMPanel;
        Button_PlantTree : array of TKMButtonFlat;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_ResMapElements;


constructor TKMGuiGameForest.Create(aParent: TKMPanel);
var I : Integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

  Panel_Shop := TKMPanel.Create(self, 0, 0, Width, Height);

  SetLength(Button_PlantTree, length(gGrowingTrees));
  for I := 0 to High(Button_PlantTree) do
  begin
    Button_PlantTree[I] := TKMButtonFlat.Create(Panel_Shop, I mod 5 * 35, I div 5 * 37, 33, 35, gGrowingTrees[I].GuiIcon);
    Button_PlantTree[I].OnClickShift := PlantTree_Clicked;
    Button_PlantTree[I].Tag := I;
  end;
end;

procedure TKMGuiGameForest.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  fForest := TKMHouseForest(aHouse);
end;


procedure TKMGuiGameForest.PlantTree_Clicked(Sender: TObject; Shift: TShiftState);
begin
  IF fForest = nil then
    Exit;

  fForest.AddTree(TKMControl(Sender).Tag, IfThen(ssRight in Shift, 5, 1));
end;

end.

