module Day4

open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllLines "day04.txt"

let score n = if n = 0 then 0.0 else 2 ** (float(n) - 1.0)

let getWinners card =
    let matched = Regex.Match(card, @".*: ([\d\s]*) \| ([\d\s]*)")
    let winning = Set(matched.Groups[1].Value.Split(" ") |> Array.filter (fun s -> s <> ""))
    let actual  = Set(matched.Groups[2].Value.Split(" ") |> Array.filter (fun s -> s <> ""))
    Set.intersect winning actual |> Set.count

let solvePt1 input =
    let total = Seq.map (fun c -> score(getWinners c)) input |> Seq.sum
    $"{total.ToString()}"

let answerPt1 = solvePt1 raw

let solvePt2 input =
    let winners = List.mapi (fun i c -> (i, getWinners c)) input
    let copies = Array.create input.Length 1
    for (i, numWon) in winners do
        for j = i + 1 to i + numWon do
            copies[j] <- copies[j] + 1 * copies[i]
    let total = Array.sum copies
    $"{total.ToString()}"

let answerPt2 = solvePt2 (raw |> Seq.toList)
