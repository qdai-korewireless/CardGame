﻿module Game.Player
open Game.Cards

type Player = {Name:string;mutable Hand:Hand;Position:int}

let sortCardsInHand hand =
    hand |> List.sortBy rankScore 
    |> List.rev
    |>Seq.groupBy (fun(Card(_,c)) -> c) |> Seq.map (fun(i,g) -> (g |> Seq.length), g) |> Seq.sortBy (fun(n,_) -> -n)
    |>Seq.collect(fun(n,c) -> c)
    |>Seq.toList
     