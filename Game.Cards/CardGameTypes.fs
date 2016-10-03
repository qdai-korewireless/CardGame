namespace Game.Cards

[<AutoOpen>]
module CardGameTypes = 

    type Suit = | Clubs| Diamonds| Hearts| Spades
    type Rank = 
        | Two   | Three | Four
        | Five  | Six   | Seven
        | Eight | Nine  | Ten
        | Jack  | Queen | King
        | Ace

    type Card = Card of Suit*Rank
    type Deck = Card list
    type ShuffledDeck = ShuffledDeck of Deck
    type Deal = Deal of ShuffledDeck*Card option
    type Hand = Card list

    type Player = {Name:string;mutable Hand:Hand;Position:int}

    type RankPlayersFn = Player list -> Player list
    type DealHandsFn = Player list -> int -> ShuffledDeck -> Player list
    type CardGame = {RankPlayers : RankPlayersFn; DealHands : DealHandsFn}

    type Games =
        |Poker

