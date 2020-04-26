module Poker

type Suit =
    | D // Diamonds
    | C // Clubs
    | H // Hearts
    | S // Spades

type Rank = 
    | A // Ace 
    | K // King
    | Q // Queen
    | J // Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

let rankVal = function
    | Two -> 0
    | Three -> 1
    | Four -> 2
    | Five -> 3
    | Six -> 4
    | Seven -> 5
    | Eight -> 6
    | Nine -> 7
    | Ten -> 8
    | J -> 9
    | Q -> 10
    | K -> 11
    | A -> 12

let getRank = function
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "7" -> Seven
    | "8" -> Eight
    | "9" -> Nine
    | "10" -> Ten
    | "J" -> J
    | "Q" -> Q
    | "K" -> K
    | "A" -> A
    | _ -> failwith "Invalid Rank"

type HandInfo = {
    hand: string list;
    score: int list;
    ranks: int list
}

let calcScore (handInfo: HandInfo) =
    let newRanks = 
        match handInfo.ranks |> List.take 2 with
        | [12; 3] -> [3; 2; 1; 0; -1]
        | _ -> handInfo.ranks
    let straight = newRanks.[0] - newRanks.[4] = 4   
    let flush = handInfo.hand |> List.map Seq.last |> List.distinct |> List.length = 1
    match (straight, flush) with
    | (s, f) when not s && not f -> [1], newRanks
    | (s, f) when s && not f-> [3;1;1;1], newRanks
    | (s, f) when not s && f-> [3;1;1;2], newRanks
    | _ -> [5;], newRanks

let evalScore (handInfo: HandInfo) = 
    match handInfo.score.Length with
    | 5 -> calcScore (handInfo)
    | _ -> handInfo.score, handInfo.ranks

let getInfo hand = 
    let info = List.map (fun (x: string) -> x.Remove(x.Length - 1)) hand |> Seq.countBy id |> Seq.toList |> List.map (fun (x,y) -> (rankVal (getRank x),y)) |> Seq.sortBy (fun (x, y) -> -y, -x) |> List.ofSeq
    let ranks = List.map (fun (x, y) -> x) info 
    let score = List.map (fun (x, y) -> y) info
    {hand = hand; score = score; ranks = ranks}

let score (hand: string list) = 
    hand
    |> getInfo
    |> evalScore

let bestHands (hands: string list) =
    hands
    |> List.map ((fun x -> x.Split [| ' ' |] |> Array.toList) >> score) |> List.mapi (fun index item -> (index, item))
    |> List.sortBy snd
    |> List.rev
    |> fun z -> List.filter (fun (x,y) -> y = (snd z.[0])) z
    |> List.sortBy fst
    |> List.map (fun (x,y) -> hands.[x])