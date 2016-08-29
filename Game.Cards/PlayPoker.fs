module Game.PlayPoker
open Game.Cards
open Game.Player

let optToCard opt = 
    match opt with
    | Some card -> card
    | None -> failwith "input was None"

let dealHand players numOfCardsEachPlayer deck: Player list = 
    let numOfPlayers = players |> List.length
    let turn = 0

    let rec dealCardToPlayers (players:Player list) deck turn = 
        if turn = numOfCardsEachPlayer*numOfPlayers then
            players
        else
            let remDeck,aCard = dealCard deck
            if aCard = None then
                players
            else
                let aPlayer =  players.[turn % numOfPlayers]
                aPlayer.Hand <- (optToCard aCard) :: aPlayer.Hand
                dealCardToPlayers players remDeck (turn+1)

    let handedPlayers = dealCardToPlayers players deck turn
    //revert cards order for each player
    handedPlayers |> List.iter (fun(p) ->p.Hand <- (p.Hand |> List.rev))
    handedPlayers

let sortCardsForPlayers players =
    players |> List.iter (fun(p) -> p.Hand <- sortCardsInHand p.Hand) 
    players


//Rule 1: High Hand Rule
let highHandRule player1 player2 = 

    let rec sortPlayers player1 player2 cardPos= 
        if cardPos = (player1.Hand |> List.length) then
            0
        else
            let card1 = player1.Hand.[cardPos]
            let card2 = player2.Hand.[cardPos]
            let card1Value = rankScore card1
            let card2Value = rankScore card2
            if card1Value > card2Value then
                -1
            else if (card1Value < card2Value) then
                1
            else
                sortPlayers player1 player2 (cardPos+1)

    sortPlayers player1 player2 0

let evaluate players =
    players |> List.sortWith highHandRule
