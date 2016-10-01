namespace Game.Cards
open Card
open Player
module Poker = 

    type PokerRules = |HIGH_HAND|ONE_PAIR|TWO_PAIR|THREE_OF_KIND|STRAIGHT|FLUSH|FULL_HOUSE|FOUR_OF_KIND|STRAIGHT_FLUSH 

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