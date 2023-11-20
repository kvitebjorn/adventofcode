module Day4

open System.IO

let raw = File.ReadAllText "day04.txt"

let doMd5 input n =
    use md5 = System.Security.Cryptography.MD5.Create()
    input + n.ToString()
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map(fun c -> c.ToString("X2"))
    |> Seq.reduce (+)
    |> Seq.toList

let rec solvePt1 input n =
    let result = doMd5 input n
    match result with 
    | '0'::'0'::'0'::'0'::'0'::_ -> n.ToString()
    | _ -> solvePt1 input (n+1)

let answerPt1 = solvePt1 raw 0

let rec solvePt2 input n =
    let result = doMd5 input n
    match result with 
    | '0'::'0'::'0'::'0'::'0'::'0'::_ -> n.ToString()
    | _ -> solvePt2 input (n+1)

let answerPt2 = solvePt2 raw 0