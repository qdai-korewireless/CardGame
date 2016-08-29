﻿module Game.Cards

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

let rankScore card =
    match card with
    |Card(_,Two) -> 2
    |Card(_,Three) -> 3
    |Card(_,Four) -> 4
    |Card(_,Five) -> 5
    |Card(_,Six) -> 6
    |Card(_,Seven) -> 7
    |Card(_,Eight) -> 8
    |Card(_,Nine) -> 9
    |Card(_,Ten) -> 10
    |Card(_,Jack) -> 11
    |Card(_,Queen) -> 12
    |Card(_,King) -> 13
    |Card(_,Ace) -> 14

let getAllSuits() =
    [Clubs;Diamonds;Hearts;Spades]
let getAllRanks() = 
    [Two;Three;Four;Five;Six;Seven;Eight;Nine;Ten;Jack;Queen;King;Ace]

let getNewDeck() = 
    let suits = getAllSuits()
    let ranks = getAllRanks()
    [for s in suits do
        for r in ranks do
            yield Card(s,r)]

let shuffle deck = 
    let rd = new System.Random(10000)
    deck |> List.sortBy(fun _ -> rd.Next()) |> ShuffledDeck


let dealCard deck = 
    match deck with
    | ShuffledDeck(top::rest) -> ShuffledDeck(rest),Some top
    | ShuffledDeck [] -> ShuffledDeck([]),None

let getDeckOfShuffledDeck (ShuffledDeck deck) =
    deck
