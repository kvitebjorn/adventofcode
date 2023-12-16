open System.IO;

let input = File.ReadAllLines "input.txt" |> array2D
let isUp (r1, c1) (r2, c2) = r2 = (r1 - 1) && c1 = c2
let isDown (r1, c1) (r2, c2) = r2 = (r1 + 1) && c1 = c2
let isRight (r1, c1) (r2, c2) = r2 = r1 && c2 = (c1 + 1)
let isLeft (r1, c1) (r2, c2) = r2 = r1 && c2 = (c1 - 1)
let energized = Array2D.zeroCreate (Array2D.length1 input) (Array2D.length2 input)
let mutable seen = Set.empty<(int * int) * (int * int)>
let rec trace grid pos prev =
    let (row, col) = pos
    let inBounds = row >= 0 && col >= 0 
                    && row < Array2D.length1 input 
                    && col < Array2D.length2 input
    match inBounds with
    | false -> ()
    | true  ->
        let symbol = input[row, col]
        let stop = seen.Contains (pos, prev)
        if stop then ()
        else
            seen <- seen.Add (pos, prev)
            energized[row, col] <- 1
            match symbol with
            | '.'  ->
                if isUp prev pos then
                    trace grid (row - 1, col) pos
                if isDown prev pos then
                    trace grid (row + 1, col) pos
                if isLeft prev pos then
                    trace grid (row, col - 1) pos
                if isRight prev pos then
                    trace grid (row, col + 1) pos
            | '/'  ->
                if isUp prev pos then
                    trace grid (row, col + 1) pos
                if isDown prev pos then
                    trace grid (row, col - 1) pos
                if isLeft prev pos then
                    trace grid (row + 1, col) pos
                if isRight prev pos then
                    trace grid (row - 1, col) pos
            | '\\' ->
                if isUp prev pos then
                    trace grid (row, col - 1) pos
                if isDown prev pos then
                    trace grid (row, col + 1) pos
                if isLeft prev pos then
                    trace grid (row - 1, col) pos
                if isRight prev pos then
                    trace grid (row + 1, col) pos
            | '|'  -> 
                if isLeft prev pos || isRight prev pos then
                    trace grid (row - 1, col) pos
                    trace grid (row + 1, col) pos
                if isUp prev pos then
                    trace grid (row - 1, col) pos
                if isDown prev pos then
                    trace grid (row + 1, col) pos
            | '-'  -> 
                if isUp prev pos || isDown prev pos then
                    trace grid (row, col - 1) pos
                    trace grid (row, col + 1) pos
                if isRight prev pos then
                    trace grid (row, col + 1) pos
                if isLeft prev pos then
                    trace grid (row, col - 1) pos
            | _    -> ()
trace input (0, 0) (0, -1) |> ignore
let mutable sum = 0
Array2D.iter (fun e -> if e = 1 then sum <- sum + 1) energized
printfn $"{sum}"