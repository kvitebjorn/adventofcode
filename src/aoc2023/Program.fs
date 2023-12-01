module Program

// TODO: there's gotta be a way to do this better...
let solutions =
   Map.empty.
        Add("Day1.answerPt1", Day1.answerPt1).
        Add("Day1.answerPt2", Day1.answerPt2);;

[<EntryPoint>]
let main args =
    let mutable day = ""
    let mutable part = ""
    try
        day  <- args.[0]
        part <- args.[1]
    with
        | _ -> failwith "Invalid args, expected {day} {part}"

    let solutionToGet = $"Day{day}.answerPt{part}"

    try
        printfn $"{solutions[solutionToGet]}"
    with
        | _ -> printfn $"No solution for day {day} part {part}"
    0
