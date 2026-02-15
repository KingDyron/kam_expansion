unit KM_CardGameTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults;

type
  TKMCardDeckType = (cdtArmy, cdtCommander, cdtSupport, cdtPalace);

  TKMCardType = (ctNone,
                //army
                ctMilitia, ctAntihorse, ctMounted, ctRanged, ctWreckers,
                //commander
                ctLanceCarrier, ctWarrior, ctRecruit, ctKnight,
                //support
                ctTime, ctBitinArmor, ctFeeder, ctStables, ctMobileTower, ctPikeMachine,
                ctRing, ctMedic, ctShieldBearer, ctTrap, ctWolfs, ctAmmoCart,
                //palace
                ctPaladin, ctBallista, ctCatapult, ctOperator, ctSpy,
                ctMobileWall, ctRam, ctEngineer, ctPyro, ctGoddess
                );

  TKMCardTypeSet = set of TKMCardType;
  TKMCardTypeArray = array of TKMCardType;

  TKMCardRankType = (crtNone, crtWeak, crtLeather, crtIron); //used only by army cards

const
  ARMY_CARD_MIN = ctMilitia;
  ARMY_CARD_MAX = ctWreckers;
  COMMANDER_CARD_MIN = ctLanceCarrier;
  COMMANDER_CARD_MAX = ctKnight;
  SUPPORT_CARD_MIN = ctTime;
  SUPPORT_CARD_MAX = ctAmmoCart;
  PALACE_CARD_MIN = ctPaladin;
  PALACE_CARD_MAX = ctPyro;
  PALACE_CARD_MAX_GODDES = ctGoddess;


  MAX_PLAYER_ARMY_CARDS = 5;        //max army cards that player can hold
  MAX_PLAYER_COMMANDER_CARDS = 3;   //max commander cards that player can hold
  MAX_TABLE_ARMY_CARDS = 2;         //max amount of army cards on the table from a player

type

  TKMCard = record
    CardType : TKMCardType;
    Value : Byte;
    Rank : TKMCardRankType;
    procedure CopyFrom(aCard : TKMCard);
    procedure Reset;
  end;
  TKMCardArray = array of TKMCard;

  TKMCardDeck = record
    Count : Integer;
    Cards : TKMCardArray;
    procedure Reset;
    procedure AddCard(const aCard : TKMCard);
  end;
  TKMCardDecks = array[TKMCardDeckType] of TKMCardDeck;

  TKMTableCards = record
    ArmyCard: array[0..MAX_TABLE_ARMY_CARDS - 1] of TKMCard;
    Commander,
    Support,
    Palace: TKMCard;
    procedure Reset;
  end;


  TKMCardGamePlayer = record
    HandID : TKMHandID;
    IsAI : Boolean;
    ArmyCard : array[0..MAX_PLAYER_ARMY_CARDS - 1] of TKMCard;
    CommanderCard : array[0..MAX_PLAYER_COMMANDER_CARDS - 1] of TKMCard;
    SupportCard,
    PalaceCard : TKMCard;
    Money : Byte;

    TableCards : TKMTableCards;
    procedure Reset;
  end;
  TKMCardGamePlayers = array of TKMCardGamePlayer;

function KMGameCard(aType : TKMCardType; aValue : Byte = 0; aRank : TKMCardRankType = crtNone) : TKMCard;

implementation

procedure TKMCard.CopyFrom(aCard: TKMCard);
begin
  CardType := aCard.CardType;
  Value := aCard.Value;
  Rank := aCard.Rank;
end;

procedure TKMCard.Reset;
begin
  CardType := ctNone;
  Value := 0;
  Rank := crtNone;
end;


procedure TKMCardGamePlayer.Reset;
var I : Integer;
begin
  for I := 0 to MAX_PLAYER_ARMY_CARDS - 1 do
    ArmyCard[I].Reset;
  for I := 0 to MAX_PLAYER_COMMANDER_CARDS - 1 do
    CommanderCard[I].Reset;
  SupportCard.Reset;
  PalaceCard.Reset;
  Money := 0;
end;

procedure TKMTableCards.Reset;
var I : Integer;
begin
  for I := 0 to MAX_TABLE_ARMY_CARDS - 1 do
    ArmyCard[I].Reset;
  Commander.Reset;
  Support.Reset;
  Palace.Reset;
end;

procedure TKMCardDeck.Reset;
begin
  Count := 0;
end;

procedure TKMCardDeck.AddCard(const aCard : TKMCard);
var I : Integer;
begin
  I := Count;
  Inc(Count);
  If Count > length(Cards) then
    SetLength(Cards, Count + 32);
  Cards[I].CopyFrom(aCard);
end;


function KMGameCard(aType : TKMCardType; aValue : Byte = 0; aRank : TKMCardRankType = crtNone) : TKMCard;
begin
  Result.CardType := aType;
  Result.Value := aValue;
  Result.Rank := aRank;
end;



end.
