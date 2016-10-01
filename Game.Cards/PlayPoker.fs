namespace Game.Cards
open Card
open Player
open Poker

module PlayPoker =

    let (=!!=) (hand1:Hand) (hand2:Hand) = 
        let v1 = hand1 |> Seq.map(rankScore) |> Seq.toArray
        let v2 = hand2 |> Seq.map(rankScore) |> Seq.toArray
        if v1 > v2 then -1 else if v1< v2 then 1 else 0

    let dealHand players numOfCardsEachPlayer deck: Player list = 
        let numOfPlayers = players |> List.length
        let iter = numOfPlayers * numOfCardsEachPlayer-1
        let mutable remDeck = deck
        for i in seq{0..iter} do
            let pos = i%numOfPlayers
            let hand = players.[pos].Hand
            let tempDeck, aCard = dealCard remDeck
            remDeck <- tempDeck
            if aCard.IsSome then
                players.[pos].Hand <- aCard.Value::hand

        //revert cards order for each player
        players |> List.iter (fun(p) ->p.Hand <- (p.Hand |> List.rev))
        players

    let sortCardsForPlayers players =
        players |> List.iter (fun(p) -> p.Hand <- sortCardsInHand p.Hand) 
        players

    let sortPlayerByHands player1 player2 = 
        let value1 = player1.Hand |> getRule |> getRuleValue
        let value2 = player2.Hand |> getRule |> getRuleValue
        let cr = value1.CompareTo(value2)
        if cr = 0 then
            player1.Hand =!!= player2.Hand 
        else 
            -cr

    let sortPlayers players =
        players |> List.sortWith sortPlayerByHands 
    let evaluatePokerPlayers players = 
        players |> sortCardsForPlayers |> sortPlayers 