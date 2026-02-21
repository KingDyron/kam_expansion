unit KM_GUIGameHouseArena;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, Math, Classes,
  KM_Defaults,
  KM_ResTypes, KM_ResDevelopment,
  KM_Controls, KM_ControlsBase, KM_ControlsSwitch, KM_ControlsWaresRow,
  KM_ControlsProgressBar,
  KM_GUIGameCards,
  KM_Houses, KM_HouseArena;

type

  TKMGuiGameArena = class(TKMPanel)
    private
      procedure Refresh(Arena : TKMHouseArena);
      procedure SelectType_Click(Sender : TObject);

    protected
        Button_ShowCardGame : TKMButton;
        LVL_Progress : TKMPercentBar;

        CardGame : TKMGuiGameCards;
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
  I, top : Integer;
  WT : TKMWareType;
  FPT : TKMFestivalPointType;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);
  top := 0;
  LVL_Progress := TKMPercentBar.Create(self, 0, top, Width, 25, fntGrey);
  LVL_Progress.TextYOffset := -3;
  {
  CardGame := TKMGuiGameCards.Create(self.MasterPanel);
  Button_ShowCardGame := TKMButton.Create(self, 0, top + 27, Width, 25, 'Play cards', bsGame);
  Button_ShowCardGame.OnClick := SelectType_Click;
  }
end;

procedure TKMGuiGameArena.Show(aHouse : TKMHouse; aTop : Integer);
begin
  Inherited Show;
  Top := aTop;
  Refresh(TKMHouseArena(aHouse));
end;

procedure TKMGuiGameArena.Refresh(Arena: TKMHouseArena);
var
  minProgress, maxProgress, currProgress : Cardinal;
begin
  minProgress := Arena.LVLExp;
  maxProgress := Arena.NextLVLExp - minProgress;
  currProgress := Arena.EXP - minProgress;

  LVL_Progress.Position := Arena.GetLVLProgress;
  LVL_Progress.Caption := 'Level: ' + Arena.LVL.ToString + ' EXP: ' + currProgress.ToString + ' / ' + maxProgress.ToString;
end;

procedure TKMGuiGameArena.SelectType_Click(Sender: TObject);
begin

  If Sender = Button_ShowCardGame then
  begin
    If CardGame.Visible then
      CardGame.Hide
    else
      CardGame.Show;
    Exit;
  end;
end;

end.

