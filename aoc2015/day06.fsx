open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllLines "input.txt"

let sum2dArray (grid: int[,]) = 
    let mutable sum = 0
    for i in 0..999 do
        for j in 0..999 do
            sum <- sum + grid[i,j]
    sum

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let parseCoords instruction = 
    let matched = Regex.Match(instruction, @"(\d+),(\d+) through (\d+),(\d+)$")
    let leftCornerX = int matched.Groups[1].Value
    let leftCornerY = int matched.Groups[2].Value
    let rightCornerX = int matched.Groups[3].Value
    let rightCornerY = int matched.Groups[4].Value
    [leftCornerX; leftCornerY; rightCornerX; rightCornerY]

let doTheLights (grid: int[,]) (coords: list<int>) (f: int -> int) =
    let leftCoordX = coords[0]
    let leftCoordY = coords[1]
    let rightCoordX = coords[2]
    let rightCoordY = coords[3]
    for i in leftCoordX..rightCoordX do
        for j in leftCoordY..rightCoordY do
            grid[i,j] <- f(grid[i,j])
    grid

let doTheThing instruction (grid: int[,]) =
    let coords = parseCoords instruction
    match instruction with 
    | Prefix "turn on" _  -> doTheLights grid coords (fun _ -> 1)
    | Prefix "turn off" _ -> doTheLights grid coords (fun _ -> 0)
    | Prefix "toggle" _   -> doTheLights grid coords (fun x -> 
                                                        match x with 
                                                        | 0 -> 1 
                                                        | 1 -> 0
                                                        | _ -> 0 )
    | _ -> grid

let rec solvePt1 input grid =
    match input with
    | x::xs -> solvePt1 xs (doTheThing x grid)
    | _     -> sprintf "%i" (sum2dArray grid)

let answerPt1 = solvePt1 (raw |> Seq.toList) (Array2D.zeroCreate 1000 1000)

let doTheThingPt2 instruction (grid: int[,]) =
    let coords = parseCoords instruction
    match instruction with 
    | Prefix "turn on" _  -> doTheLights grid coords (fun x -> x + 1)
    | Prefix "turn off" _ -> doTheLights grid coords (fun x -> 
                                                        match x < 1 with 
                                                        | false -> x - 1
                                                        | _ -> 0)
    | Prefix "toggle" _   -> doTheLights grid coords (fun x -> x + 2)
    | _ -> grid

let rec solvePt2 input grid =
    match input with
    | x::xs -> solvePt2 xs  (doTheThingPt2 x grid)
    | _     -> sprintf "%i" (sum2dArray grid)

let answerPt2 = solvePt2 (raw |> Seq.toList) (Array2D.zeroCreate 1000 1000)

printfn "%s" answerPt1
printfn "%s" answerPt2