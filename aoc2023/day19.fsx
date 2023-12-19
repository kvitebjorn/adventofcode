open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Part = { x: int; m: int; a: int; s: int; mutable wf: string; mutable result: Option<string> }
let input = File.ReadAllLines "input.txt"
let (rawWorkflows, rawParts) = 
    let idx = Array.findIndex (fun l -> l = "") input
    let (wfs, parts) = Array.splitAt idx input
    (wfs, parts[1..])
let ops = Dictionary<string, (int -> int -> bool)>()
ops.Add(">", (fun a b -> a > b))
ops.Add("<", (fun a b -> a < b))
let makeExpression (body: string) =
    let matched = Regex.Match(body, @"(\w)([>|<|])(\d+):(\w+)")
    if matched.Length > 0 then
        let var = matched.Groups[1].Value
        let op  = matched.Groups[2].Value
        let num = matched.Groups[3].Value |> int
        let goto  = matched.Groups[4].Value
        let f = (fun (p: Part) ->
                let value = match var with
                            | "x" -> p.x
                            | "m" -> p.m
                            | "a" -> p.a
                            | "s" -> p.s
                            | _ -> 0
                let comp = ops[op]
                if comp value num then goto else "")
        f
    else
        let goto = body
        let f = (fun _ -> goto)
        f
let makeFn (body: string) = 
    let expressions' = body.Split(",")
    let expressions = Array.map makeExpression expressions'
    let f = (fun (p: Part) -> 
        let result = Array.map (fun f -> f(p)) expressions 
                    |> Array.filter (fun r -> r <> "") 
                    |> Array.head
        result)
    f
let mutable workflows = Dictionary<string, (Part -> string)>()
for wf in rawWorkflows do
    let matched = Regex.Match(wf, @"(\w+)\{(.*)\}")
    let name = matched.Groups[1].Value
    let body = matched.Groups[2].Value
    workflows.Add(name, (makeFn body))
let mutable parts = Dictionary<int, Part>()
let mutable i = 0
for rawPart in rawParts do
    let matched = Regex.Match(rawPart, @"{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}")
    let x' = matched.Groups[1].Value |> int
    let m' = matched.Groups[2].Value |> int
    let a' = matched.Groups[3].Value |> int
    let s' = matched.Groups[4].Value |> int
    parts.Add(i, {x = x'; m = m'; a = a'; s = s'; wf = "in"; result = None})
    i <- i + 1
let doTheThing partNum =
    let mutable loop = true
    while loop do
        parts[partNum].wf <- workflows[parts[partNum].wf](parts[partNum])
        if parts[partNum].wf = "A" || parts[partNum].wf = "R" then
            parts[partNum].result <- Some(parts[partNum].wf)
        if parts[partNum].result <> None then
            loop <- false
for partNum in [0..i - 1] do
    doTheThing partNum
let accepted = Seq.filter (fun p -> p.result.IsSome && p.result.Value = "A") parts.Values
let sum = Seq.map (fun p -> p.x + p.m + p.a + p.s) accepted |> Seq.sum
printfn $"{sum}"