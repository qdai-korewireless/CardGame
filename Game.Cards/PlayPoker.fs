module Game.PlayPoker
open Game.Cards
open Game.Player

let NumOfCardsPerPlayer = 5
let matchCardSpecialCase card = 
    match card with
    |Card(_, Ace) -> Some Ace
    |Card(_, Five) -> Some Five
    |_ -> None

let (===) a b = rankScore a = rankScore b 
let (!==) a b = rankScore a <> rankScore b 
let (>>>) a b = 
    let c1 = (rankScore a - rankScore b = 1) 
    let c2 = (matchCardSpecialCase a) = (Some Ace) && (matchCardSpecialCase b) = (Some Five)
    (c1 || c2)

let (<<<) a b = rankScore a - rankScore b = -1
let (=*=) a b = rankSuit a = rankSuit b

let optToCard opt = 
    match opt with
    | Some card -> card
    | None -> failwith "input was None"

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

let sortWithRule rule = 
    List.sortWith rule

type PokerRules = 
    |HIGH_HAND 
    |ONE_PAIR
    |TWO_PAIR
    |THREE_OF_KIND
    |STRAIGHT
    |FLUSH
    |FULL_HOUSE
    |FOUR_OF_KIND
    |STRAIGHT_FLUSH

let getRuleValue rule =
    match rule with
    |Some HIGH_HAND -> 1 
    |Some ONE_PAIR -> 2
    |Some TWO_PAIR -> 3
    |Some THREE_OF_KIND -> 4
    |Some STRAIGHT -> 5
    |Some FLUSH -> 6
    |Some FULL_HOUSE -> 7
    |Some FOUR_OF_KIND -> 8
    |Some STRAIGHT_FLUSH -> 9
    |None -> 0

let getRule (hand:Hand) = 
    let card1 = hand.[0]
    let card2 = hand.[1]
    let card3 = hand.[2]
    let card4 = hand.[3]
    let card5 = hand.[4]

    let ruleMap = (
        card1 === card2 && card2 === card3 && card3 === card4,
        card1 === card2 && card2 === card3 && card3 !== card4 && card4 === card5,
        card1 =*= card2 && card2  =*= card3 && card3 =*= card4 && card4 =*= card5,
        card1 >>> card2 && card2 >>> card3 && card3 >>> card4 && card4 >>> card5,
        card1 === card2 && card2 === card3,
        card1 === card2 && card2 !== card3 && card3 === card4 && card4 !== card5,
        card1 === card2 && card2 !== card3 && card3 !== card4 && card4 !== card5)

    match ruleMap with
    |(_,_,true,true,_,_,_) -> Some STRAIGHT_FLUSH
    |(true,_,_,_,_,_,_) -> Some FOUR_OF_KIND
    |(_,true,_,_,_,_,_) -> Some FULL_HOUSE
    |(_,_,true,_,_,_,_) -> Some FLUSH
    |(_,_,_,true,_,_,_) -> Some STRAIGHT
    |(_,_,_,_,true,_,_) -> Some THREE_OF_KIND
    |(_,_,_,_,_,true,_) -> Some TWO_PAIR
    |(_,_,_,_,_,_,true) -> Some ONE_PAIR
    |_ -> None
 
let rec compareHands (hand1:Hand) (hand2:Hand) cardPos = 
    if cardPos = (hand1|> List.length) then
        0
    else
        let card1 = hand1.[cardPos]
        let card2 = hand2.[cardPos]
        let card1Value = rankScore card1
        let card2Value = rankScore card2
        if hand1 > hand2 then
            -1
        else if (card1Value < card2Value) then
            1
        else
            compareHands hand1 hand2 (cardPos+1)

let sortPlayerByHands player1 player2 = 
    let hand1 = player1.Hand
    let hand2 = player2.Hand
    let rule1 = getRule hand1
    let rule2 = getRule hand2
    let value1 = getRuleValue rule1
    let value2 = getRuleValue rule2
    if value1 > value2 then
        -1
    else if value1 < value2 then 
        1
    else
        compareHands hand1 hand2 0 

let sortPlayers players =
    players |> List.sortWith sortPlayerByHands 
let evaluate players = 
    players |> sortCardsForPlayers |> sortPlayers 