unit KM_CardGame;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults,
  KM_CardGameTypes;


type
  TKMCardGame = class
  private
    //money in the poll
    fMoney : Byte;
    fTablePlayer : TKMCardGamePlayer;
    fPlayersCount : Byte;
    fPlayers : TKMCardGamePlayers;

    fDecks : TKMCardDecks;

    procedure CreateDeck;

    procedure ShuffleDeck(aDeckType : TKMCardDeckType);
    procedure ShuffleDecks;

  public
    constructor Create;

    procedure Reset;
    procedure AddPlayer(aHandID : TKMHandID);
  end;


implementation
uses
  Math,
  KM_HandsCollection, KM_Hand, KM_HandSpectator,
  KM_CommonUtils;

constructor TKMCardGame.Create;
begin
  Inherited;
  Reset;
  CreateDeck;
  ShuffleDecks;
end;

procedure TKMCardGame.Reset;
var I : Integer;
begin
  fTablePlayer.Reset;

  for I := 0 to fPlayersCount - 1 do
    fPlayers[I].Reset;
end;

procedure TKMCardGame.AddPlayer(aHandID: TKMHandID);
var I : Integer;
begin
  I := fPlayersCount;
  Inc(fPlayersCount);
  If fPlayersCount > length(fPlayers) then
    SetLength(fPlayers, fPlayersCount + 3);

  fPlayers[I].Reset;
  fPlayers[I].HandID := aHandID;
  fPlayers[I].IsAI := gHands[aHandID].IsComputer;
end;

procedure TKMCardGame.CreateDeck;
const
  DUPLICATE_ARMY_CARDS = 5;
  DUPLICATE_COMMANDER_CARDS = 3;
  DUPLICATE_SUPPORT_CARDS = 2;
  DUPLICATE_PALACE_CARDS = 4;
var I, J, C, K : integer;
  CT : TKMCardType;
begin
  for I := 1 to DUPLICATE_ARMY_CARDS do
  begin

    for CT := ARMY_CARD_MIN to ARMY_CARD_MAX do
    begin
      J := byte(CT) - byte(ARMY_CARD_MIN) + 1;
      fDecks[cdtArmy].AddCard(KMGameCard(CT, J,     crtWeak));
      fDecks[cdtArmy].AddCard(KMGameCard(CT, J + 1, crtLeather));
      fDecks[cdtArmy].AddCard(KMGameCard(CT, J + 2, crtIron));
    end;

  end;

  for I := 1 to DUPLICATE_COMMANDER_CARDS do
  begin

    for CT := COMMANDER_CARD_MIN to COMMANDER_CARD_MAX do
    begin
      //value
      case CT of
        ctLanceCarrier: J := 2;
        ctWarrior: J := 4;
        ctRecruit: J := 6;
        ctKnight: J := 10;
        else J := 0;
      end;
      //amount of them
      case CT of
        ctLanceCarrier: C := 4;
        ctWarrior: C := 3;
        ctRecruit: C := 2;
        ctKnight: C := 1;
        else C := 0;
      end;

      If (J > 0) then
        for K := 1 to C do
          fDecks[cdtCommander].AddCard(KMGameCard(CT, J));
    end;

  end;

  for I := 1 to DUPLICATE_SUPPORT_CARDS do
    for CT := SUPPORT_CARD_MIN to SUPPORT_CARD_MAX do
      fDecks[cdtSupport].AddCard(KMGameCard(CT));

  for I := 1 to DUPLICATE_PALACE_CARDS do
    for CT := PALACE_CARD_MIN to PALACE_CARD_MAX do
      fDecks[cdtPalace].AddCard(KMGameCard(CT));

  fDecks[cdtPalace].AddCard(KMGameCard(ctGoddess));

end;

procedure TKMCardGame.ShuffleDeck(aDeckType : TKMCardDeckType);
var tmp : TKMCard;
  I, R : integer;
begin

  for I := fDecks[aDeckType].Count - 1 downto 0 do
  begin
    R := KaMRandom(I, 'TKMCardGame.ShuffleDeck');
    tmp := fDecks[aDeckType].Cards[R];
    fDecks[aDeckType].Cards[R] := fDecks[aDeckType].Cards[I];
    fDecks[aDeckType].Cards[I] := tmp;
  end;
end;

procedure TKMCardGame.ShuffleDecks;
var CDT : TKMCardDeckType;
begin
  for CDT := Low(TKMCardDeckType) to High(TKMCardDeckType) do
    ShuffleDeck(CDT);
end;

end.
