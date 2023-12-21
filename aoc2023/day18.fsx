open System.IO
open System

let input = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.Split(" "))

let shoelace vertices perimeter =
    let mutable area = 0

    for (r1, c1), (r2, c2) in Seq.pairwise (vertices) do
        area <- area + ((r2 + r1) * (c2 - c1))

    let area' = abs (area) / 2
    int (area' - perimeter / 2 + 1) + perimeter

let rec dig instructions prev acc perimeter =
    match instructions with
    | [||] -> shoelace acc perimeter
    | _ ->
        let instruction: string array = Array.head instructions
        let dir = instruction[0]
        let n = instruction[1] |> int
        let rest = Array.tail instructions
        let (row, col) = (fst prev, snd prev)

        match dir with
        | "U" ->
            let pos = (row - n, col)
            dig rest pos (pos :: acc) (perimeter + n)
        | "D" ->
            let pos = (row + n, col)
            dig rest pos (pos :: acc) (perimeter + n)
        | "L" ->
            let pos = (row, col - n)
            dig rest pos (pos :: acc) (perimeter + n)
        | "R" ->
            let pos = (row, col + n)
            dig rest pos (pos :: acc) (perimeter + n)
        | _ -> 0

let lava = dig input (0, 0) [] 0
printfn "%s" $"{lava}"

let shoelace' vertices perimeter =
    let mutable area = 0L

    for (r1, c1), (r2, c2) in Seq.pairwise (vertices) do
        area <- area + ((r2 + r1) * (c2 - c1))

    let area' = abs (area) / 2L
    int64 (area' - perimeter / 2L + 1L) + perimeter

let rec dig' instructions prev acc perimeter =
    match instructions with
    | [||] -> shoelace' acc perimeter
    | _ ->
        let instruction: string array = Array.head instructions
        let color = instruction[2]
        let dir = color[7]
        let n = Convert.ToInt64(color[2..6], 16)
        let rest = Array.tail instructions
        let (row, col) = (fst prev, snd prev)

        match dir with
        | '3' ->
            let pos = (row - n, col)
            dig' rest pos (pos :: acc) (perimeter + n)
        | '1' ->
            let pos = (row + n, col)
            dig' rest pos (pos :: acc) (perimeter + n)
        | '2' ->
            let pos = (row, col - n)
            dig' rest pos (pos :: acc) (perimeter + n)
        | '0' ->
            let pos = (row, col + n)
            dig' rest pos (pos :: acc) (perimeter + n)
        | _ -> 0

let lavaPt2 = dig' input (0L, 0L) [] 0L
printfn "%s" $"{lavaPt2}"
