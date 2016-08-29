module Game.Player
open Game.Cards

type Player = {Name:string;mutable Hand:Hand;Position:int}

let sortCardsInHand hand =
    hand |> List.sortBy rankScore
     