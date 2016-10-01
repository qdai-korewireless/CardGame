module Game.Cards.Tests.Evaluating

open FsUnit
open NUnit.Framework
open Game.Cards.CardGameTypes
open Game.Cards.Card
open Game.Cards.Player
open Game.Cards.GameService


[<Test>]
let ``should player 2 win due to high hand``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Ten);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven);Card(Spades, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Queen);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven);Card(Hearts, Ace)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Queen);Card(Diamonds, Nine);Card(Diamonds, Six);Card(Diamonds, Seven);Card(Hearts, Two)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to one pair``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Diamonds, Ace);Card(Spades, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Five);Card(Clubs, Five);Card(Spades, Ace);Card(Diamonds, Jack);Card(Hearts, Two)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Hearts, King);Card(Spades, Five);Card(Clubs, Three);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to two pairs``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Diamonds, Ace);Card(Spades, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Spades, Ace);Card(Diamonds, Ace);Card(Diamonds, Five);Card(Clubs, Five);Card(Hearts, Two)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Hearts, King);Card(Spades, King);Card(Clubs, Three);Card(Hearts, Three);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to 3 of kind``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Ten);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven);Card(Spades, Ten)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Queen);Card(Diamonds, Nine);Card(Diamonds, Six);Card(Diamonds, Seven);Card(Hearts, Two)];
                    Position = 3}
    let expected = [player2;player1;player3]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to higher 3 of kind``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Ten);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven);Card(Spades, Ten)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Six);Card(Spades, Six);Card(Clubs, Six);Card(Hearts, Seven);Card(Hearts, Ace)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Diamonds, Seven);Card(Hearts, Two)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to 4 of kind``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Ten);Card(Diamonds, Nine);Card(Diamonds, Eight);Card(Diamonds, Seven);Card(Spades, Ten)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Five);Card(Hearts, Ace)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Six);Card(Hearts, Six);Card(Clubs, Six);Card(Diamonds, Seven);Card(Hearts, Two)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to flush``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Clubs, Ten);Card(Diamonds, Nine);Card(Spades, Eight);Card(Diamonds, Queen);Card(Diamonds, Seven)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Five);Card(Diamonds, Ace);Card(Diamonds, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Six);Card(Hearts, Six);Card(Clubs, Six);Card(Clubs, Seven);Card(Clubs, Ace)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to full house``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Diamonds, Ace);Card(Diamonds, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Five);Card(Clubs, Five);Card(Spades, Five);Card(Diamonds, Two);Card(Diamonds, Two)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player1;player3]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to straight``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Hearts, Five);Card(Diamonds, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Ten);Card(Clubs, Jack);Card(Spades, Queen);Card(Diamonds, King);Card(Diamonds, Ace)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to straight with Ace to Five``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Hearts, Five);Card(Diamonds, Two);Card(Diamonds, King);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Diamonds, Ace);Card(Diamonds, Five);Card(Clubs, Four);Card(Spades, Three);Card(Diamonds, Two)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player3;player1]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected

[<Test>]
let ``should player 2 win due to straight flush``() = 
    let evaluate = getGame Poker
    let player1 = {Name="Player 1";
                    Hand=[Card(Diamonds, Five);Card(Hearts, Five);Card(Diamonds, Five);Card(Diamonds, Five);Card(Diamonds, Three)];
                    Position = 1}
    let player2 = {Name="Player 2";
                    Hand=[Card(Clubs, Ten);Card(Clubs, Nine);Card(Clubs, Eight);Card(Clubs, Seven);Card(Clubs, Six)];
                    Position = 2}
    let player3 = {Name="Player 3";
                    Hand=[Card(Diamonds, Five);Card(Spades, Five);Card(Clubs, Five);Card(Hearts, Six);Card(Hearts, Ace)];
                    Position = 3}
    let expected = [player2;player1;player3]

    let actual = evaluate [player1;player2;player3]

    actual |> should equal expected