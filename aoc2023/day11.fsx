open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "input.txt" |> Array.map Seq.toList |> Array.toList
let rec expandRows acc xs =
    match xs with
    | [] -> acc
    | _ -> 
        let s = List.head xs
        let numDistinct = List.distinct s |> List.length
        match numDistinct with
        | 1 -> expandRows (s::s::acc) xs[1..] 
        | _ -> expandRows (s::acc) xs[1..]
let rotate n list = 
    match n with
    | 90  -> List.transpose list |> List.map List.rev
    | -90 -> List.map List.rev list |> List.transpose
    | _   -> list
let expanded = 
    input |>
    expandRows [] |>
    rotate 90 |>
    expandRows [] |>
    rotate 90
let galaxies = new Dictionary<int, (int * int)>()
let mutable counter = 0
let mutable row = 0
for row in [0..((List.length expanded) - 1)] do
    let gs = 
        expanded[row]
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
let dist p0 p1 =
    let galaxyA = galaxies[p0]
    let galaxyB = galaxies[p1]
    let dx = abs(fst galaxyA - fst galaxyB)
    let dy = abs(snd galaxyA - snd galaxyB)
    dx + dy
let sum = List.map (fun (p: list<int>) -> dist p[0] p[1]) pairsToTry |> List.sum
printfn $"{sum}"