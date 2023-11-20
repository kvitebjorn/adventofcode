﻿module Program

// TODO: there's gotta be a way to do this better...
let solutions =
   Map.empty.
        Add("Day1.answerPt1", Day1.answerPt1).
        Add("Day1.answerPt2", Day1.answerPt2).
        Add("Day2.answerPt1", Day2.answerPt1).
        Add("Day2.answerPt2", Day2.answerPt2).
        Add("Day3.answerPt1", Day3.answerPt1).
        Add("Day3.answerPt2", Day3.answerPt2).
        Add("Day4.answerPt1", Day4.answerPt1).
        Add("Day4.answerPt2", Day4.answerPt2).
        Add("Day5.answerPt1", Day5.answerPt1).
        Add("Day5.answerPt2", Day5.answerPt2);;

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
