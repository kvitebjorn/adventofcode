open System.IO

let input = File.ReadAllLines "input.txt"
            |> Array.map (fun s -> s.Split(" "))
let shoelace vertices perimeter =
    let mutable area = 0
    for (r1, c1), (r2, c2) in Seq.pairwise(vertices) do
        area <- area + ((r2 + r1) * (c2 - c1))
    let area' = abs(area) / 2
    int(area' - perimeter / 2 + 1) + perimeter
let rec dig instructions prev acc perimeter =
    match instructions with
    | [||] -> shoelace acc perimeter
    | _  -> 
        let instruction : string array = Array.head instructions
        let dir = instruction[0]
        let n   = instruction[1] |> int
        let color = instruction[2]
        let rest = Array.tail instructions
        let (row, col) = (fst prev, snd prev)
        match dir with
        | "U" -> 
            let pos = (row - n, col)
            dig rest pos (pos::acc) (perimeter + n)
        | "D" -> 
            let pos = (row + n, col)
            dig rest pos (pos::acc) (perimeter + n)
        | "L" -> 
            let pos = (row, col - n)
            dig rest pos (pos::acc) (perimeter + n)
        | "R" ->
            let pos = (row, col + n)
            dig rest pos (pos::acc) (perimeter + n)
        | _   -> 0
let lava = dig input (0, 0) [] 0 
printfn "%s" $"{lava}"