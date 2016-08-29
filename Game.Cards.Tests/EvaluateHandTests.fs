module Game.Cards.Tests.Evaluating

open FsUnit
open NUnit.Framework
open Game.Cards
open Game.Player
open Game.PlayPoker

[<Test>]
let ``should player 2 win due to high hand``() = 
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Ten);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven)];
                    Position = 1}
    let player2 = {Name="Player 1";
                    Hand=[Card(Diamonds, Queen);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Queen);Card(Diamonds, Nine);Card(Diamonds, Six);Card(Diamonds, Seven)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected