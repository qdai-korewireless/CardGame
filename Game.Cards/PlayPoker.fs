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

let groupCardsByRank (hand:Hand) = 
    hand |> Seq.groupBy (fun(Card(_,c)) -> c) |> Seq.map (fun(i,g) -> (g |> Seq.length), (g|>Seq.head)) |> Seq.sortBy (fun(x,_) -> -x) 

let groupCardsBySuit hand = 
    hand |> Seq.groupBy (fun(Card(s,_)) -> s) |> Seq.map (fun(i,g) -> (g |> Seq.length), (g|>Seq.head)) |> Seq.sortBy (fun(x,_) -> -x) 

let cardsAreFlush hand = 
    let count = groupCardsBySuit hand |> Seq.head |> fst
    count = 5

let cardsAreStraight (cards:Hand) = 
    let card1 = rankScore cards.[0]
    let card2 = rankScore cards.[1]
    let card3 = rankScore cards.[2]
    let card4 = rankScore cards.[3]
    let card5 = rankScore cards.[4]
    let mutable posibleResult = false
    if card1 = 14 then
        posibleResult <- (1+card2) = (card3 + card5) && (card4 + card4) = (1+card2)
    (card1+card5) = (card2 + card4) && (card3 + card3) = (card1+card5) || posibleResult

let cardsAreStraightFlush hand = 
    cardsAreStraight hand && cardsAreFlush hand

let rankSameType (hand1:Hand) (hand2:Hand) = 
    let p1c1 = rankScore hand1.[0]
    let p2c1 = rankScore hand2.[0]
    if p1c1 > p2c1 then
        -1
    else if p1c1 < p2c1 then
        1
    else
        0
let compareHands fn (hand1:Hand) (hand2:Hand) = 
    let r1, r2 = (fn hand1), (fn hand2)
    match (r1,r2) with
    |(true,false) -> -1
    |(false,true) -> 1
    |(true,true) -> rankSameType hand1 hand2
    |(false,false) -> 0

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

//Rule: Straight
let straightRule player1 player2 =
    compareHands cardsAreStraight player1.Hand player2.Hand

//Rule: Straight Flush
let straightFlushRule player1 player2 =
    compareHands cardsAreStraightFlush player1.Hand player2.Hand

let rec rankScoreAndCompare (g1:(int*Card) list) (g2:(int*Card) list) index =
    if index = -1 then
        0
    else
        let card1 = snd g1.[index]
        let card2 = snd g2.[index]

        let p1c1 = rankScore card1
        let p2c1 = rankScore card2
        if p1c1 > p2c1 then
            -1
        else if p1c1 < p2c1 then
            1
        else
            rankScoreAndCompare g1 g2 (index-1)

let cardsArePairs numOfGroup fstPairCount compareToCardIndex (hand1:Hand) (hand2:Hand) = 
    let g1 = groupCardsByRank hand1 |> Seq.toList
    let g2 = groupCardsByRank hand2 |> Seq.toList
    let r1 = g1 |> Seq.length = numOfGroup && (fst g1.[0]) = fstPairCount
    let r2 = g2 |> Seq.length = numOfGroup && (fst g2.[0]) = fstPairCount
    match (r1,r2) with
    |(true,false) -> -1
    |(false,true) -> 1
    |(true,true) -> rankScoreAndCompare g1 g2 compareToCardIndex
    |(false,false) -> 0

//Rule: One pair
let onePairRule player1 player2 = 
    cardsArePairs 4 2 0 player1.Hand player2.Hand

//Rule: Two pair
let twoPairRule player1 player2 = 
    cardsArePairs 3 2 1 player1.Hand player2.Hand

//Rule: Three/Four of Kind
let numberOfKindRule player1 player2 numOfKind = 
    let h1Count,h1Card = groupCardsByRank player1.Hand |> Seq.head
    let h2Count,h2Card = groupCardsByRank player2.Hand |> Seq.head

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
    let p1IsFL = cardsAreFlush player1.Hand
    let p2IsFL = cardsAreFlush player2.Hand
    if p1IsFL && not p2IsFL then
        -1
    else if p2IsFL && not p1IsFL then
        1
    else
        0

//Rule: Full House
let fullHouseRule player1 player2 = 
    cardsArePairs 2 3 1 player1.Hand player2.Hand

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
    |> sortWithRule straightFlushRule
