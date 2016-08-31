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

let sortWithRule rule = 
    List.sortWith rule

let groupCardsByRank player = 
    player.Hand |> Seq.groupBy (fun(Card(_,c)) -> c) |> Seq.map (fun(i,g) -> (g |> Seq.length), (g|>Seq.head)) |> Seq.sortBy (fun(x,_) -> -x) 

let groupCardsBySuit player = 
    player.Hand |> Seq.groupBy (fun(Card(s,_)) -> s) |> Seq.map (fun(i,g) -> (g |> Seq.length), (g|>Seq.head)) |> Seq.sortBy (fun(x,_) -> -x) 

let cardsAreStraight (cards:Hand) = 
    let card1 = rankScore cards.[0]
    let card2 = rankScore cards.[1]
    let card3 = rankScore cards.[2]
    let card4 = rankScore cards.[3]
    let mutable card5 = rankScore cards.[4]
    if card5 = 14 then
        card5 <- 1
    (card1+card5) = (card2 + card4) && (card3 + card3) = (card1+card5)

//Rule: Straight
let straightRule player1 player2 =
    let p1ST = cardsAreStraight player1.Hand
    let p2ST = cardsAreStraight player2.Hand
    if p1ST && not p2ST then
        -1
    else if p2ST && not p1ST then
        1
    else if p1ST && p2ST then 
        let p1c1 = rankScore player1.Hand.[0]
        let p2c1 = rankScore player2.Hand.[0]
        if p1c1 > p2c1 then
            -1
        else if p1c1 < p2c1 then
            1
        else
            0
    else
        0
//Rule: High Hand Rule
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
//Rule: One pair
let onePairRule player1 player2 = 
    let g1 = groupCardsByRank player1 |> Seq.toList
    let g2 = groupCardsByRank player2 |> Seq.toList
    let p1IsOP = g1 |> Seq.length = 4 && (fst g1.[0]) = 2
    let p2IsOP = g2 |> Seq.length = 4 && (fst g2.[0]) = 2
    if p1IsOP && not p2IsOP then
        -1
    else if p2IsOP && not p1IsOP then
        1
    else if p2IsOP && p1IsOP then
        let p1c1 = rankScore (snd g1.[0])
        let p2c1 = rankScore (snd g2.[0])
        if p1c1 > p2c1 then
            -1
        else if p1c1 < p2c1 then
            1
        else
            0
    else
        0

//Rule: Two pair
let twoPairRule player1 player2 = 
    let g1 = groupCardsByRank player1 |> Seq.toList
    let g2 = groupCardsByRank player2 |> Seq.toList
    let p1IsTP = g1 |> Seq.length = 3 && (fst g1.[0]) = 2
    let p2IsTP = g2 |> Seq.length = 3 && (fst g2.[0]) = 2
    if p1IsTP && not p2IsTP then
        -1
    else if p2IsTP && not p1IsTP then
        1
    else if p1IsTP && p2IsTP then
        let p1c1 = rankScore (snd g1.[0])
        let p2c1 = rankScore (snd g2.[0])
        if p1c1 > p2c1 then
            -1
        else if p1c1 < p2c1 then
            1
        else
            let p1c2 = rankScore (snd g1.[1])
            let p2c2 = rankScore (snd g2.[1])
            if p1c2 > p2c2 then
                -1
            else if p1c2 < p2c2 then
                1
            else
                0
    else
        0

//Rule: Three/Four of Kind
let numberOfKindRule player1 player2 numOfKind = 
    let h1Count,h1Card = groupCardsByRank player1 |> Seq.head
    let h2Count,h2Card = groupCardsByRank player2 |> Seq.head

    if h1Count = numOfKind && h2Count < numOfKind then
        -1
    else if h2Count = numOfKind && h1Count < numOfKind then
        1
    else if h1Count = numOfKind && h2Count = numOfKind then
        if (rankScore h1Card) > (rankScore h2Card) then
            -1
        else
            1
    else
        0
let fourOfKindRule player1 player2 = 
    numberOfKindRule player1 player2 4

let threeOfKindRule player1 player2 = 
    numberOfKindRule player1 player2 3

//Rule: Flush
let flushRule player1 player2 = 
    let h1Count,h1Card = groupCardsBySuit player1 |> Seq.head
    let h2Count,h2Card = groupCardsBySuit player2 |> Seq.head
    if h1Count = 5 && h2Count < 5 then
        -1
    else if h2Count = 5 && h1Count < 5 then
        1
    else
        0

//Rule: Full House
let fullHouseRule player1 player2 = 
    let g1 = groupCardsByRank player1 |> Seq.toList
    let g2 = groupCardsByRank player2 |> Seq.toList
    let p1IsFH = g1 |> Seq.length = 2 && (fst g1.[0]) = 3
    let p2IsFH = g2 |> Seq.length = 2 && (fst g2.[0]) = 3

    if p1IsFH && not p2IsFH then
        -1
    else if p2IsFH && not p1IsFH then
        1
    else if p1IsFH && p2IsFH then
        let p1c1 = rankScore (snd g1.[0])
        let p2c1 = rankScore (snd g2.[0])
        if p1c1 > p2c1 then
            -1
        else if p1c1 < p2c1 then
            1
        else
            let p1c2 = rankScore (snd g1.[1])
            let p2c2 = rankScore (snd g2.[1])
            if p1c2 > p2c2 then
                -1
            else if p1c2 < p2c2 then
                1
            else
                0
    else 
        0


let evaluate players =
    players 
    |> sortWithRule highHandRule 
    |> sortWithRule onePairRule
    |> sortWithRule twoPairRule
    |> sortWithRule threeOfKindRule 
    |> sortWithRule straightRule
    |> sortWithRule flushRule 
    |> sortWithRule fullHouseRule 
    |> sortWithRule fourOfKindRule 
