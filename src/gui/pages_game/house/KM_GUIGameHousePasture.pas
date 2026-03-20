unit KM_GUIGameHousePasture;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_Houses, KM_HousePasture;

type

  TKMAnimalCostView = class(TKMControl)
  private
    fTexID : Word;
    fCost,
    fTitle,
    fFeathers, fEgg, fSkin, fPig : String;
  public
    CoinTexID : Word;
    constructor Create(aParent : TKMPanel; aLeft, aTop, aWidth: Integer; aAnimal : TKMPastureAnimalType);
    procedure Paint;override;
  end;

  TKMGuiGamePasture = class(TKMPanel)
    private
      fCoinIndex : Word;
      fPasture : TKMHousePasture;
      procedure Refresh;
      procedure RefreshAnimals;
      procedure RefreshShop;

      procedure BuyAnimal_Click(Sender : TObject; Shift : TShiftState);
      procedure SellAnimal_Click(Sender : TObject);
    protected

      Panel_Animals : TKMPanel;
        Button_Count : array [0..MAX_ANIMALS - 1] of TKMButtonFlat;

      Panel_Shop : TKMPanel;
        View_Animal : array [0..high(PASTURE_ANIMALS_ORDER)] of TKMAnimalCostView;
        Button_Buy : array [0..high(PASTURE_ANIMALS_ORDER)] of TKMButtonFlat;
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


constructor TKMGuiGamePasture.Create(aParent: TKMPanel);
var I : Integer;
  PAT : TKMPastureAnimalType;
  top : Integer;
  animal : TKMPasAnimalSpec;
  S : String;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);
  fCoinIndex := gRes.Wares.VirtualWares.WareS['vtCoin'].Index;

  Panel_Animals := TKMPanel.Create(self, 0, 35, Width, 37 * 2);
    for I := 0 to High(Button_Count) do
      begin
        Button_Count[I] := TKMButtonFlat.Create(Panel_Animals, I mod 5 * 38, I div 5 * 37, 30, 34, 0);
        Button_Count[I].Tag := I;
        Button_Count[I].OnClick := SellAnimal_Click;
      end;

  Panel_Shop := TKMPanel.Create(self, 0, Panel_Animals.Bottom + 2, Width, Height);

    for I := low(Button_Buy) to high(Button_Buy) do
    begin
      PAT := PASTURE_ANIMALS_ORDER[I];
      animal := Pat.Spec;
      top := I * 35;
      View_Animal[I] := TKMAnimalCostView.Create(Panel_Shop, 0, top, Width + 2, PAT);
      View_Animal[I].CoinTexID := gRes.Wares.VirtualWares[fCoinIndex].GUIIcon;
      Button_Buy[I] := TKMButtonFlat.Create(Panel_Shop, 0, top, 30, 30, animal.GuiIcon);
      Button_Buy[I].OnClickShift := BuyAnimal_Click;
      //what it produces:
      S := '|' + gResTexts[140] + ':|';
      If animal.Feathers > 0 then S := S + gRes.Wares[wtFeathers].Title + ' x'+ animal.Feathers.ToString(ffNumber, 1, 1) + '|';
      If animal.Eggs > 0 then S := S + gRes.Wares[wtEgg].Title + ' x'+ animal.Eggs.ToString(ffNumber, 1, 1) + '|';
      If animal.Meat > 0 then S := S + gRes.Wares[wtPig].Title + ' x'+ animal.Meat.ToString(ffNumber, 1, 1) + '|';
      If animal.Skin > 0 then S := S + gRes.Wares[wtSkin].Title + ' x'+ animal.Skin.ToString(ffNumber, 1, 1) + '|';


      Button_Buy[I].Hint := Format(gResTexts[2280] + S, [gResTexts[animal.Hint], animal.Cost]);
      Button_Buy[I].Tag := I;
    end;


end;

procedure TKMGuiGamePasture.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  fPasture := TKMHOusePasture(aHouse);
  Refresh;
end;

procedure TKMGuiGamePasture.Refresh;
begin
  Panel_Animals.Hide;
  Panel_Shop.Hide;
  RefreshAnimals;
  RefreshShop;
end;

procedure TKMGuiGamePasture.RefreshAnimals;
var I : Integer;
  animal : TKMPastureAnimal;
begin
  Panel_Animals.Show;
  for I := 0 to High(Button_Count) do
  begin
    animal := fPasture.GetAnimal(I);
    //Button_Count[I].Visible := animal.AnimalType <> patNone;
    Button_Count[I].TexID := animal.AnimalType.Spec.GuiIcon;
    Button_Count[I].Hint := gResTexts[animal.AnimalType.Spec.Hint];
  end;

end;

procedure TKMGuiGamePasture.RefreshShop;
var I : Integer;
begin
  Panel_Shop.Show;
  for I := low(Button_Buy) to high(Button_Buy) do
  begin
    Button_Buy[I].Enabled := gHands[fPasture.Owner].VirtualWare[fCoinIndex] > PASTURE_ANIMALS_ORDER[I].Spec.Cost;
    If fPasture.IsAnimalAddedToBuy( byte( PASTURE_ANIMALS_ORDER[ Button_Buy[I].Tag ]) ) then
      Button_Buy[I].BackBevelColor := $5500FF00
    else
      Button_Buy[I].BackBevelColor := 0;
  end;
end;

procedure TKMGuiGamePasture.BuyAnimal_Click(Sender : TObject; Shift : TShiftState);
begin
  //fPasture.BuyAnimal(PASTURE_ANIMALS_ORDER[TKMControl(Sender).Tag]);
  If ssRight in Shift then
    gGame.GameInputProcess.CmdHouse(gicHousePastureToggleAnimal, TKMHouse(fPasture), TKMControl(Sender).Tag)
  else
    gGame.GameInputProcess.CmdHouse(gicHousePastureBuyAnimal, TKMHouse(fPasture), TKMControl(Sender).Tag);
end;

procedure TKMGuiGamePasture.SellAnimal_Click(Sender : TObject);
begin
  gGame.GameInputProcess.CmdHouse(gicHousePastureSellAnimal, TKMHouse(fPasture), TKMControl(Sender).Tag);
end;


constructor TKMAnimalCostView.Create(aParent: TKMPanel; aLeft: Integer; aTop: Integer; aWidth: Integer; aAnimal: TKMPastureAnimalType);
var
  animal : TKMPasAnimalSpec;
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, 30);

  animal := aAnimal.Spec;
  fTexID := animal.GuiIcon;
  fTitle := gResTexts[animal.Hint];

  fFeathers := 'x' + animal.Feathers.ToString(ffnumber, 1, 1);
  fEgg := 'x' + animal.Eggs.ToString(ffnumber, 1, 1);
  fSkin := 'x' + animal.Skin.ToString(ffnumber, 1, 1);
  fPig := 'x' + animal.Meat.ToString(ffnumber, 1, 1);
  fCost := 'x' + animal.Cost.ToString;
end;

procedure TKMAnimalCostView.Paint;
var al, at, aR : Integer;
begin
  Inherited;
  al := ABSLeft;
  at := AbsTop;
  aR := al + Width;
  TKMRenderUI.WriteBevel(al, at, Width, Height);
  TKMRenderUI.WriteText(al + 33, at + 5, Width - 33, fTitle, fntOutline, taLeft);

  {
  //animal gui icon
  TKMRenderUI.WriteBevel(al + 3, at + 3, 30, 30);
  TKMRenderUI.WritePicture(al + 3, at + 3, 30, 30, [], rxGui, fTexID);
  }
  //cost
  TKMRenderUI.WritePicture(aR - 60, at + 5, 20, 20, [], rxGui, CoinTexID);
  TKMRenderUI.WriteText(aR - 40, at + 7, 100, fCost, fntGrey, taLeft);

  {//Wares
  TKMRenderUI.WritePicture(aR - 75, at, 30, 30, [], rxGui, gRes.Wares[wtFeathers].GUIIcon);
  TKMRenderUI.WriteText(aR - 65, at + 13, 30, fFeathers, fntGrey, taLeft);

  TKMRenderUI.WritePicture(aR - 35, at, 30, 30, [], rxGui, gRes.Wares[wtEgg].GUIIcon);
  TKMRenderUI.WriteText(aR -25, at + 13, 30, fEgg, fntGrey, taLeft);

  TKMRenderUI.WritePicture(aR - 75, at + 30, 30, 30, [], rxGui, gRes.Wares[wtSkin].GUIIcon);
  TKMRenderUI.WriteText(aR - 65, at + 43, 30, fSkin, fntGrey, taLeft);

  TKMRenderUI.WritePicture(aR - 35, at + 30, 30, 30, [], rxGui, gRes.Wares[wtPig].GUIIcon);
  TKMRenderUI.WriteText(aR - 25, at + 43, 30, fPig, fntGrey, taLeft);}
end;

end.

