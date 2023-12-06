let raw = System.IO.File.ReadAllText "day01.txt"

let rec solvePt1 input pos =
    match input with 
    | '('::tail -> solvePt1 tail (pos + 1)
    | ')'::tail -> solvePt1 tail (pos - 1)
    | _         -> pos.ToString()

let answerPt1 = solvePt1 (raw |> Seq.toList) 0

let rec solvePt2 input pos idx =
    if pos = -1 then
        idx.ToString()
    else
        match input with 
        | '('::tail -> solvePt2 tail (pos + 1) (idx + 1)
        | ')'::tail -> solvePt2 tail (pos - 1) (idx + 1)
        | _         -> pos.ToString()
let answerPt2 = solvePt2 (raw |> Seq.toList) 0 0

printfn "%s" answerPt1
printfn "%s" answerPt2