module Day1

let rec solvePt1 input pos =
    match input with 
    | '('::tail -> solvePt1 tail (pos + 1)
    | ')'::tail -> solvePt1 tail (pos - 1)
    | _         -> pos

let input = System.IO.File.ReadAllText "day01.txt"
let answerPt1 = solvePt1 (input |> Seq.toList) 0

let rec solvePt2 input pos idx =
    if pos = -1 then
        idx
    else
        match input with 
        | '('::tail -> solvePt2 tail (pos + 1) (idx + 1)
        | ')'::tail -> solvePt2 tail (pos - 1) (idx + 1)
        | _         -> pos
let answerPt2 = solvePt2 (input |> Seq.toList) 0 0