module Day1

let rec solve input pos =
    match input with 
    | '('::tail -> solve tail (pos + 1)
    | ')'::tail -> solve tail (pos - 1)
    | _         -> pos

let input = "((("
let answer = solve (input |> Seq.toList) 0