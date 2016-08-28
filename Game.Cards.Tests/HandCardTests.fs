module Game.Cards.Tests.Handing

open FsUnit
open NUnit.Framework
open Game.Cards
open Game.Player
open Game.PlayPoker

let shuffledDeck =
  ShuffledDeck
    [ Card (Hearts,Seven); Card (Clubs,Ten); Card (Clubs,Two);
      Card (Spades,Six); Card (Clubs,Eight); Card (Diamonds,Nine);
      Card (Spades,Two); Card (Diamonds,King); Card (Clubs,Three);
      Card (Hearts,Three); Card (Hearts,Five); Card (Diamonds,Jack);
      Card (Clubs,Four); Card (Spades,King); Card (Hearts,Eight);
      Card (Diamonds,Seven); Card (Diamonds,Five); Card (Hearts,Ten);
      Card (Clubs,Seven); Card (Hearts,Six); Card (Hearts,King);
      Card (Spades,Five); Card (Hearts,Two); Card (Hearts,Four);
      Card (Diamonds,Two); Card (Diamonds,Three); Card (Clubs,Jack);
      Card (Clubs,Six); Card (Diamonds,Six); Card (Spades,Queen);
      Card (Spades,Jack); Card (Diamonds,Ace); Card (Hearts,Nine);
      Card (Hearts,Ace); Card (Diamonds,Four); Card (Spades,Four);
      Card (Spades,Three); Card (Clubs,Queen); Card (Hearts,Queen);
      Card (Diamonds,Queen); Card (Spades,Seven); Card (Spades,Eight);
      Card (Clubs,Ace); Card (Spades,Nine); Card (Hearts,Jack);
      Card (Spades,Ten); Card (Clubs,Five); Card (Diamonds,Ten);
      Card (Clubs,Nine); Card (Clubs,King); Card (Spades,Ace);
      Card (Diamonds,Eight) ]

[<Test>]
let ``should return 5 cards for each one of 4 players``() = 
    let testPlayers = [{Name="Player 1";Hand=[];Position=1};
                      {Name="Player 2";Hand=[];Position=2};
                      {Name="Player 3";Hand=[];Position=3};
                      {Name="Player 4";Hand=[];Position=4}]

    let expected = [5;5;5;5]

    let actual = shuffledDeck |> dealHand testPlayers 5 |> List.map (fun(p)->p.Hand |>List.length)

    actual |> should equal expected

[<Test>]
let ``player 3 should have certain cards``() = 
    let testPlayers = [{Name="Player 1";Hand=[];Position=1};
                      {Name="Player 2";Hand=[];Position=2};
                      {Name="Player 3";Hand=[];Position=3};
                      {Name="Player 4";Hand=[];Position=4}]

    let expected = [Card(Clubs,Two);Card(Spades,Two);Card(Hearts,Five);Card(Hearts,Eight);Card(Clubs,Seven)]
    
    let player3 = shuffledDeck |> dealHand testPlayers 5 |> List.filter (fun(p) -> p.Name = "Player 3") |> List.head
    let actual = player3.Hand

    actual |> should equal expected
