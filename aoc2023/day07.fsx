open System.IO

let raw = File.ReadAllLines "day07.txt"
let parse (s: string) = 
    let splitted = s.Split(" ")
    (splitted[0] |> Seq.toList, int(splitted[1]))
let duplicates s =
   s
   |> Seq.toList
   |> List.groupBy id
   |> List.choose ( function
          | _, x::_::_ -> Some x
          | _ -> None )
let count x = Seq.filter ((=) x) >> Seq.length
let isKind n s = 
    let dupes = duplicates s
    let result = List.filter (fun d -> count d s = n) dupes |> List.tryHead
    match result with
    | None -> false
    | _    -> true
let isFive s = List.distinct s |> List.length = 1
let isFour s = isKind 4 s
let isFull s = isKind 3 s && isKind 2 s
let isThree s = isKind 3 s
let isTwo s = isKind 2 s && (duplicates s).Length = 2
let isOne s = isKind 2 s && (duplicates s).Length = 1
let baseStrength hand = 
    match hand with
    | hnd when isFive hnd -> 7
    | hnd when isFour hnd -> 6
    | hnd when isFull hnd -> 5
    | hnd when isThree hnd -> 4
    | hnd when isTwo hnd -> 3
    | hnd when isOne hnd -> 2
    | _ -> 1
let rec groupHands fives fours fulls threes twos ones highs hands =
    match hands with
    | []      -> [fives;fours;fulls;threes;twos;ones;highs]
    | h :: hs -> 
        let strength = fst h |> baseStrength
        match strength with
        | 7 -> groupHands (h :: fives) fours fulls threes twos ones highs hs 
        | 6 -> groupHands fives (h :: fours) fulls threes twos ones highs hs
        | 5 -> groupHands fives fours (h :: fulls) threes twos ones highs hs
        | 4 -> groupHands fives fours fulls (h :: threes) twos ones highs hs
        | 3 -> groupHands fives fours fulls threes (h :: twos) ones highs hs
        | 2 -> groupHands fives fours fulls threes twos (h :: ones) highs hs
        | _ -> groupHands fives fours fulls threes twos ones (h :: highs) hs
let cardStrength (card: list<char>) =
    let c = card[0]
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | _   -> int(string(c))
let rec compareHands (n1: list<char>) (n2: list<char>) =
    if n1 = n2
        then 0
        else
            let n1str = cardStrength n1
            let n2str = cardStrength n2
            let equal = n1str = n2str
            match equal with
            | true -> compareHands (List.tail n1) (List.tail n2) 
            | _    -> compare n1str n2str
let compareEntries(n1: list<char>, s1: int) (n2: list<char>, s2: int) =
    compareHands n1 n2
let solvePt1 input =
    let hands = Array.map parse input |> Array.toList
    let grouped = groupHands [] [] [] [] [] [] [] hands
    let fives = List.sortWith compareEntries grouped[0]
    let fours = List.sortWith compareEntries grouped[1]
    let fulls = List.sortWith compareEntries grouped[2]
    let threes = List.sortWith compareEntries grouped[3]
    let twos = List.sortWith compareEntries grouped[4]
    let ones = List.sortWith compareEntries grouped[5]
    let highs = List.sortWith compareEntries grouped[6]
    let sorted = highs @ ones @ twos @ threes @ fulls @ fours @ fives
    let total = List.mapi (fun i (_, bid) -> (i + 1) * bid) sorted |> List.sum
    $"{total.ToString()}"
let answerPt1 = solvePt1 raw

let cardStrengthPt2 (card: list<char>) =
    let c = card[0]
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 999
    | 'T' -> 10
    | _   -> int(string(c))

// TODO: need tomake J's act as wildcards, and make them weak in tie comparisons

let solvePt2 input =
    let hands = Array.map parse input |> Array.toList
    let grouped = groupHands [] [] [] [] [] [] [] hands
    let fives = List.sortWith compareEntriesPt2 grouped[0]
    let fours = List.sortWith compareEntriesPt2 grouped[1]
    let fulls = List.sortWith compareEntriesPt2 grouped[2]
    let threes = List.sortWith compareEntriesPt2 grouped[3]
    let twos = List.sortWith compareEntriesPt2 grouped[4]
    let ones = List.sortWith compareEntriesPt2 grouped[5]
    let highs = List.sortWith compareEntriesPt2 grouped[6]
    let sorted = highs @ ones @ twos @ threes @ fulls @ fours @ fives
    let total = List.mapi (fun i (_, bid) -> (i + 1) * bid) sorted |> List.sum
    $"{total.ToString()}"
let answerPt2 = solvePt2 raw

printfn "%s" answerPt1
printfn "%s" answerPt2