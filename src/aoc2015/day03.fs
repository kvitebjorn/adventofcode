module Day3

open System.IO

let raw = File.ReadAllText "day03.txt"

let rec deliver input (houses : Set<(int*int)>) x y =
    match input with
    | '^'::rest -> deliver rest (houses.Add((x,y+1))) x (y+1)
    | 'v'::rest -> deliver rest (houses.Add((x,y-1))) x (y-1)
    | '>'::rest -> deliver rest (houses.Add((x+1,y))) (x+1) y
    | '<'::rest -> deliver rest (houses.Add((x-1,y))) (x-1) y
    | _ -> houses

let solvePt1 input =
    let giftedHouses = deliver input (Set.empty.Add((0,0))) 0 0
    $"{giftedHouses.Count}"

let answerPt1 = solvePt1 (raw |> Seq.toList)

let solvePt2 (input: list<char>) =
    let numDirections = input.Length - 1
    let evens = [for i in 0..numDirections do
                    if (i % 2 = 0) then
                        yield input[i]]
    let odds = [for i in 0..numDirections do
                    if not (i % 2 = 0) then
                        yield input[i]]
    let giftedHousesSanta = deliver odds (Set.empty.Add((0,0))) 0 0
    let giftedHousesRoboSanta = deliver evens (Set.empty.Add((0,0))) 0 0
    let giftedUnion = Set.union giftedHousesSanta giftedHousesRoboSanta
    $"{giftedUnion.Count}"

let answerPt2 = solvePt2 (raw |> Seq.toList)