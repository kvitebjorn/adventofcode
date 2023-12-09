open System.IO

let input = File.ReadAllLines "input.txt" 
            |> Array.toList
            |> List.map (fun s -> s.ToString().Split(" ") 
                                |> Array.toList 
                                |> List.map int)
let allZero = List.forall (fun elem -> elem = 0)
let rec step nums =
    match nums with
    | []  -> []
    | [_] -> []
    | x::y::zs -> (y - x) :: step (y::zs)
let rec steps list acc =
    let isZeros = allZero list
    match isZeros with
    | true -> (list::acc)
    | _    -> steps (step list) (list::acc)
let rec extrapolate lists acc =
    match lists with
    | []  -> acc
    | [_] -> acc
    | x::y::zs -> extrapolate (y::zs) (acc + (List.last y))
let solvePt1 =
    let total = List.map (fun h -> extrapolate (steps h []) 0) input |> List.sum
    $"{total}"
let answerPt1 = solvePt1 
printfn "%s" answerPt1

let rec extrapolatePt2 lists acc =
    match lists with
    | []  -> acc
    | [_] -> acc
    | x::y::zs -> extrapolatePt2 (y::zs) ((List.head y) - acc)
let solvePt2 =
    let total = List.map (fun h -> extrapolatePt2 (steps h []) 0) input |> List.sum
    $"{total}"
let answerPt2 = solvePt2
printfn "%s" answerPt2
