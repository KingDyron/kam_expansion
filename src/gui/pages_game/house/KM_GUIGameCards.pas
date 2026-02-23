unit KM_GUIGameCards;
{$I KaM_Remake.inc}
interface
uses
  SysUtils,
  KM_Defaults,
  KM_Controls, KM_ControlsPopUp,
  KM_CardGame, KM_CardGameTypes;

type

  TKMCardViewer = class(TKMControl)
  private

  public
    Card : TKMCard;
    procedure Paint; Override;
  end;

  TKMGuiGameCards = class(TKMPopUpPanel)
    private
    protected
      Cards : array of TKMCardViewer;
    public
      constructor Create(aParent : TKMPanel);
      procedure Show; override;
  end;

implementation
uses
  KM_Game, KM_GameInputProcess,
  KM_RenderUI,
  KM_Resource, KM_ResTexts, KM_ResFonts;

constructor TKMGuiGameCards.Create(aParent : TKMPanel);
const
  CARD_PANEL_WIDTH = 800;
  CARD_PANEL_HEIGHT = 700;
var I : integer;
begin
  Inherited Create(aParent, CARD_PANEL_WIDTH, CARD_PANEL_HEIGHT);

  SetLength(Cards, 10);
  for I := 0 to High(Cards) do
  begin
    Cards[I] := TKMCardViewer.Create(self, 10 + I * 70, 50, 60, 100);
    Cards[I].Card.Reset;
    Cards[I].Hide;
  end;
  BevelShade.HideParentOnClick;
end;

procedure TKMGuiGameCards.Show;
var I, last : integer;
  P : PKMCardGamePlayer;
begin
  Inherited;
  gGame.CardGame.GiveStartingCards;
  P := gGame.CardGame.GetPlayer(-1);

  last := 0;
  for I := 0 to high(P.ArmyCard) do
  begin
    Cards[last].Card := P.ArmyCard[I];
    Cards[last].Visible := true;
    Inc(last);
  end;

  for I := 0 to high(P.CommanderCard) do
  begin
    Cards[last].Card := P.CommanderCard[I];
    Cards[last].Visible := true;
    Inc(last);
  end;
  Cards[last].Card := P.SupportCard;
    Cards[last].Visible := true;
  Inc(last);
  Cards[last].Card := P.PalaceCard;
    Cards[last].Visible := true;
end;


procedure TKMCardViewer.Paint;
begin

  Inherited;
  If Card.CardType = ctNone then
    Exit;

  TKMRenderUI.WriteShape(AbsLeft, AbsTop, Width, Height, icWhite);

  If Card.Value > 0 then
    TKMRenderUI.WriteText(AbsLeft, AbsTop, 30, Card.Value.ToString, fntGrey, taLeft);


end;


end.

