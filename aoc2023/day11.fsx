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
    // obviously could be more elegant with zips and pairs but it's 2am lol
    let galaxyA = galaxies[p0]
    let galaxyB = galaxies[p1]
    let aX = fst galaxyA
    let aY = snd galaxyA
    let bX = fst galaxyB
    let bY = snd galaxyB
    let spanningAX = Set.intersect expandedCols (Set([0..aX]))
    let spanningAY = Set.intersect expandedRows (Set([0..aY]))
    let spanningBX = Set.intersect expandedCols (Set([0..bX]))
    let spanningBY = Set.intersect expandedRows (Set([0..bY]))
    let axX = spanningAX.Count * expansionFactor
    let ayX = spanningAY.Count * expansionFactor
    let axZ = aX - spanningAX.Count
    let ayZ = aY - spanningAY.Count
    let bxX = spanningBX.Count * expansionFactor
    let byX = spanningBY.Count * expansionFactor
    let bxZ = bX - spanningBX.Count
    let byZ = bY - spanningBY.Count
    let newAX = axX + axZ
    let newAY = ayX + ayZ
    let newBX = bxX + bxZ
    let newBY = byX + byZ
    let dx = abs(newAX - newBX) |> int64
    let dy = abs(newAY - newBY) |> int64
    dx + dy
let sum = List.map (fun (p: list<int>) -> dist p[0] p[1] 2) pairsToTry |> List.sum
printfn $"{sum}"

let sumPt2 = List.map (fun (p: list<int>) -> dist p[0] p[1] 1000000) pairsToTry |> List.sum
printfn $"{sumPt2}"