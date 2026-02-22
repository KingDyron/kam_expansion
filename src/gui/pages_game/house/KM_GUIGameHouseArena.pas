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
  TKMHappinessBar = class(TKMPercentBar)
    protected
    public
      NewPosition : Single;

      procedure UpdateState(aTickCount: Cardinal); override;
      procedure Paint; override;
  end;

  TKMGuiGameArena = class(TKMPanel)
    private
      procedure Refresh(Arena : TKMHouseArena);
      procedure SelectType_Click(Sender : TObject);

    protected

        Button_ShowCardGame : TKMButton;
        Label_Level : TKMLabelShadow;
        LVL_Progress : TKMHappinessBar;

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
var top : Integer;
begin
  Inherited Create(aParent, 0, 100, aParent.Width - 8, 600);
  top := 0;
  Label_Level := TKMLabelShadow.Create(self, 0, top, Width, 20, gResTexts[2352], fntGrey, taLeft);
  Inc(top, 20);
  LVL_Progress := TKMHappinessBar.Create(self, 0, top, Width, 25, fntGrey);
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
  Label_Level.Caption := gResTexts[2352] + ' ' + Arena.LVL.ToString;
  minProgress := Arena.LVLExp;
  maxProgress := Arena.NextLVLExp - minProgress;
  currProgress := Arena.EXP - minProgress;

  LVL_Progress.NewPosition := Arena.GetLVLProgress;
  LVL_Progress.Caption := currProgress.ToString + ' / ' + maxProgress.ToString;
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


procedure TKMHappinessBar.UpdateState(aTickCount: Cardinal);
begin
  Inherited;
end;

procedure TKMHappinessBar.Paint;
var dif : Single;
begin
  Inherited;
  If Position <> NewPosition then
  begin
    If NewPosition < Position then
      Position := 0;

    dif := (NewPosition - Position) / 20;
    Position := Position + (dif);

    If (NewPosition - Position) < 0.005 then
      Position := NewPosition;
  end;

end;

end.

