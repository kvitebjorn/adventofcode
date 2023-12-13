open System.IO

let input = File.ReadAllLines "input.txt" |> 
            Array.toList |> 
            List.map Seq.toList
let rec parse lists acc = 
    match lists with
    | [] -> acc
    | _  -> 
        let thisMap = List.takeWhile (fun l -> l <> []) lists
        parse lists[(List.length thisMap) + 1..] (thisMap::acc)
let parsed = parse input []
let rotate n list = 
    match n with
    | 90  -> List.transpose list |> List.map List.rev
    | _   -> list
let rec tryFindSymmetry l col =
    let len = List.length l
    let isInBounds = col < len
    if isInBounds then
        let size = if col < len - col then col else len - col
        let left = l[(col - size)..col - 1]
        let right = l[col..(col + size) - 1]
        let isSymmetric = left = (List.rev right)
        match isSymmetric with
        | true -> col
        | _    -> tryFindSymmetry l (col + 1)
    else 0
let reflection l = 
    let h = tryFindSymmetry l 1 
    if h = 0 then tryFindSymmetry (rotate 90 l) 1 else h * 100
let total = List.map reflection parsed |> List.sum
printfn $"{total}"