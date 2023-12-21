open System.IO

let rec parse acc lists =
    match lists with
    | [] -> acc
    | _ ->
        let thisMap = List.takeWhile (fun l -> l <> []) lists
        parse (thisMap :: acc) lists[(List.length thisMap) + 1 ..]

let parsed =
    File.ReadAllLines "input.txt" |> Array.toList |> List.map Seq.toList |> parse []

let rotate n list =
    match n with
    | 90 -> List.transpose list |> List.map List.rev
    | _ -> list

let rec tryFindSymmetry l symmetryFn col =
    let len = List.length l
    let isInBounds = col < len

    if isInBounds then
        let size = if col < len - col then col else len - col
        let left = l[(col - size) .. col - 1]
        let right = l[col .. (col + size) - 1]
        let isSymmetric = symmetryFn left right

        match isSymmetric with
        | true -> col
        | _ -> tryFindSymmetry l symmetryFn (col + 1)
    else
        0

let reflection l symmetryFn =
    let h = tryFindSymmetry l symmetryFn 1

    if h = 0 then
        tryFindSymmetry (rotate 90 l) symmetryFn 1
    else
        h * 100

let total =
    List.map (fun x -> reflection x (fun l r -> l = (List.rev r))) parsed
    |> List.sum

printfn $"{total}"

let symmetryFnPt2 l r =
    (l, List.rev r)
    ||> List.zip
    |> List.map (fun (c1: char list, c2: char list) -> List.mapi (fun i x -> if c1[i] = x then 0 else 1) c2)
    |> List.map List.sum
    |> List.sum
    |> (fun m -> m = 1)

let totalPt2 = List.map (fun x -> reflection x symmetryFnPt2) parsed |> List.sum
printfn $"{totalPt2}"
