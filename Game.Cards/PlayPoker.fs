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
