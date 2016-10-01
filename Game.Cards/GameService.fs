namespace Game.Cards
open Card
open Player
open Poker
open PlayPoker

module GameService =
    type RankPlayers = Player list -> Player list

    let getGame game : RankPlayers=
        match game with
        |Poker -> evaluatePokerPlayers