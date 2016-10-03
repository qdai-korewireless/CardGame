namespace Game.Cards
open Card
open Player

module GameService =

    let getGame game: CardGame=
        match game with
        |Poker -> PlayPoker.create()