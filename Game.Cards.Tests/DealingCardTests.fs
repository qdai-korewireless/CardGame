module Game.Cards.Tests.Dealing
open Game.Cards
open FsUnit
open NUnit.Framework

//Install-Package FsUnit -Version 1.4.1

[<Test>]
let ``should return 51 cards after dealing a card from a new deck`` () = 
    let expected = 51
    let actual = getNewDeck() |> shuffle |> dealCard |> fst |> getDeckOfShuffledDeck |> List.length
    actual |> should equal expected

[<Test>]
let ``should return 0 card when dealing a card from depleted deck``() = 
    let expected = 0
    let actual = [] |> shuffle |> dealCard |> fst|> getDeckOfShuffledDeck |> List.length
    actual |> should equal expected