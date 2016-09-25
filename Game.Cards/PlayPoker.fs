module Game.PlayPoker
open Game.Cards
open Game.Player

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
let (=*=) a b = rankSuit a = rankSuit b

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

type PokerRules = |HIGH_HAND|ONE_PAIR|TWO_PAIR|THREE_OF_KIND|STRAIGHT|FLUSH|FULL_HOUSE|FOUR_OF_KIND|STRAIGHT_FLUSH 
    
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
        card1 === card2 && card2 === card3 && card3 === card4 && card4 !== card5,
        card1 === card2 && card2 === card3 && card3 !== card4 && card4 === card5,
        card1 =*= card2 && card2 =*= card3 && card3 =*= card4 && card4 =*= card5,
        card1 >>> card2 && card2 >>> card3 && card3 >>> card4 && card4 >>> card5,
        card1 === card2 && card2 === card3 && card3 !== card4 && card4 !== card5,
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

let sortPlayerByHands player1 player2 = 
    let value1 = player1.Hand |> getRule |> getRuleValue
    let value2 = player2.Hand |> getRule |> getRuleValue
    let cr = value1.CompareTo(value2)
    if cr = 0 then
        player1.Hand =!!= player2.Hand 
    else 
        cr

let sortPlayers players =
    players |> List.sortWith sortPlayerByHands 
let evaluate players = 
    players |> sortCardsForPlayers |> sortPlayers 