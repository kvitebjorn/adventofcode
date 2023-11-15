module Day2

open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllLines "day02.txt"

let measure present=
    let matched = Regex.Match(present, @"(\d+)x(\d+)x(\d+)")
    let l = int matched.Groups[1].Value
    let w = int matched.Groups[2].Value
    let h = int matched.Groups[3].Value
    let lw = l * w
    let wh = w * h
    let hl = h * l
    let area = 2*lw + 2*wh + 2*hl
    let extra = Array.min [|lw; wh; hl|]
    area + extra

let rec solvePt1 input =
    let measurements = Array.map (fun i -> measure i) input
    let total = Array.sum measurements
    $"{total.ToString()}"

let answerPt1 = solvePt1 raw