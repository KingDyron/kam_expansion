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
        WareRow_TreeCount : TKMWaresRow;
        Button_Spalings : TKMButtonFlat;
    public
      constructor Create(aParent: TKMPanel);
      procedure Show(aHouse : TKMHouse; aTop : Integer); Reintroduce;
  end;
implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_HandsCollection,
  KM_Resource, KM_ResTexts, KM_ResMapElements;


constructor TKMGuiGameForest.Create(aParent: TKMPanel);
var I : Integer;
  top : Integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);

  Panel_Shop := TKMPanel.Create(self, 0, 0, Width, Height);
  top := 0;
  SetLength(Button_PlantTree, length(gGrowingTrees));
  for I := 0 to High(Button_PlantTree) do
  begin
    Button_PlantTree[I] := TKMButtonFlat.Create(Panel_Shop, I mod 5 * 35, I div 5 * 37, 33, 35, gGrowingTrees[I].GuiIcon);
    Button_PlantTree[I].OnClickShift := PlantTree_Clicked;
    Button_PlantTree[I].Tag := I;
    top := Button_PlantTree[I].Bottom;
  end;

  WareRow_TreeCount := TKMWaresRow.Create(Panel_Shop, 0, top + 10, Panel_Shop.Width - 30);
  WareRow_TreeCount.TexID := 1047;
  WareRow_TreeCount.WareCntAsNumber := true;
  WareRow_TreeCount.MaxWares := 100;
  WareRow_TreeCount.Hint := gResTexts[2286];
  WareRow_TreeCount.Height := 30;
  Button_Spalings := TKMButtonFlat.Create(Panel_Shop, WareRow_TreeCount.Right, top + 10, 30, 30, 0);
  Button_Spalings.Hint := gResTexts[gRes.Wares.VirtualWares.WareS['vtSapling'].TextID];
  Button_Spalings.TexID := gRes.Wares.VirtualWares.WareS['vtSapling'].GUIIcon;
end;

procedure TKMGuiGameForest.Show(aHouse : TKMHouse; aTop : Integer);
var I : Integer;
  hasSapling : Boolean;
begin
  Inherited Show;
  Top := aTop;
  fForest := TKMHouseForest(aHouse);
  hasSapling := gHands[fForest.Owner].VirtualWare['vtSapling'] > 0;
  for I := 0 to High(Button_PlantTree) do
    Button_PlantTree[I].Enabled := hasSapling;
  WareRow_TreeCount.WareCount := fForest.TotalCount;
  Button_Spalings.Caption := gHands[fForest.Owner].VirtualWare['vtSapling'].ToString;
end;



procedure TKMGuiGameForest.PlantTree_Clicked(Sender: TObject; Shift: TShiftState);
begin
  IF fForest = nil then
    Exit;
  gGame.GameInputProcess.CmdHouse(gicHouseForestPlantTree, TKMHouse(fForest), TKMControl(Sender).Tag, IfThen(ssRight in Shift, 5, 1))
end;

end.

