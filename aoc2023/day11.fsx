open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "input.txt" |> Array.map Seq.toList |> Array.toList

let rec expandRows xs n acc =
    match xs with
    | [] -> acc
    | _ -> 
        let s = List.head xs
        let numDistinct = List.distinct s |> List.length
        match numDistinct with
        | 1 -> expandRows xs[1..] (n + 1) (n::acc)
        | _ -> expandRows xs[1..] (n + 1) acc
let rotate n list = 
    match n with
    | 90  -> List.transpose list |> List.map List.rev
    | -90 -> List.map List.rev list |> List.transpose
    | _   -> list
let expandedRows = expandRows input 0 [] |> Set
let expandedCols = expandRows (rotate 90 input) 0 [] |> Set
let galaxies = new Dictionary<int, (int * int)>()
let mutable counter = 0
for row in [0..((List.length input) - 1)] do
    let gs = 
        input[row]
        |> List.indexed
        |> List.filter (fun (_, x) -> x = '#')
        |> List.map fst
    for col in gs do
        counter <- counter + 1
        galaxies.Add(counter, (col, row))
let rec pairs n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (pairs (k-1) xs) @ pairs k xs
let pairsToTry = pairs 2 [1..counter]
let dist p0 p1 expansionFactor =
    let (aX, aY) = galaxies[p0]
    let (bX, bY) = galaxies[p1]
    let spanningAX = Set.intersect expandedCols (Set([0..aX]))
    let spanningAY = Set.intersect expandedRows (Set([0..aY]))
    let spanningBX = Set.intersect expandedCols (Set([0..bX]))
    let spanningBY = Set.intersect expandedRows (Set([0..bY]))
    let axX = aX - spanningAX.Count + spanningAX.Count * expansionFactor
    let ayX = aY - spanningAY.Count + spanningAY.Count * expansionFactor
    let bxX = bX - spanningBX.Count + spanningBX.Count * expansionFactor
    let byX = bY - spanningBY.Count + spanningBY.Count * expansionFactor
    let dx = abs(axX- bxX) |> int64
    let dy = abs(ayX- byX) |> int64
    dx + dy
let sum = List.map (fun (p: list<int>) -> dist p[0] p[1] 2) pairsToTry |> List.sum
printfn $"{sum}"

let sumPt2 = List.map (fun (p: list<int>) -> dist p[0] p[1] 1000000) pairsToTry |> List.sum
printfn $"{sumPt2}"