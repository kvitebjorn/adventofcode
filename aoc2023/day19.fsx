open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Part = 
    { 
        x: int; 
        m: int; 
        a: int; 
        s: int; 
        mutable wf: string; 
        mutable result: Option<string> 
    }
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

type Expression = 
    { 
        priority: int;
        var: Option<string>; 
        op: Option<string>; 
        num: Option<int>; 
        goto: string 
    }
let mutable wfNameToExpressions = Dictionary<string, list<Expression>>()
let parseExpression idx body =
    let matched = Regex.Match(body, @"(\w)([>|<|])(\d+):(\w+)")
    if matched.Length > 0 then
        let var' = matched.Groups[1].Value
        let op'  = matched.Groups[2].Value
        let num' = matched.Groups[3].Value |> int
        let goto'  = matched.Groups[4].Value
        {
            priority = idx + 1
            var = Some(var')
            op = Some(op')
            num = Some(num')
            goto = goto'
        }
    else
        let goto' = body
        {
            priority = idx + 1
            var = None
            op = None
            num = None
            goto = goto'
        }
for wf in rawWorkflows do
    let matched = Regex.Match(wf, @"(\w+)\{(.*)\}")
    let name = matched.Groups[1].Value
    let body = matched.Groups[2].Value
    let expressions = body.Split(",") |> Array.mapi parseExpression |> Array.toList
    wfNameToExpressions.Add(name, expressions)
type Node = 
    { 
        name: string; 
        priority: Option<int>; 
        left: Option<Node>; 
        right: Option<Node>;
        depth: int;
    }
type Interval = { A: int; B: int }
let intersect (interval1: Interval) (interval2: Interval) =
    { A = max interval1.A interval2.A; B = min interval1.B interval2.B }
type XmasMap = Map<string, Interval>
let mutable sumPt2 = 0L
let rec tree goto priority' depth' (xmasMap: XmasMap) =
    match goto with
    | "A" -> 
            sumPt2 <- sumPt2 + 
                        int64(xmasMap.["x"].B - xmasMap.["x"].A + 1) *
                        int64(xmasMap.["m"].B - xmasMap.["m"].A + 1) *
                        int64(xmasMap.["a"].B - xmasMap.["a"].A + 1) *
                        int64(xmasMap.["s"].B - xmasMap.["s"].A + 1)
            {  
                name = goto 
                priority = None
                left  = None 
                right = None 
                depth = depth'
            } |> Some
    | "R" -> 
            { 
                name = goto 
                priority = None
                left  = None 
                right = None 
                depth = depth'
            } |> Some
    | _ -> 
        if priority' = wfNameToExpressions[goto].Length then None
        else
            let expr   = wfNameToExpressions[goto][priority']
            let isLessOp = expr.op = Some("<")
            let isExpr = expr.var.IsSome
            
            let xmasMapLeft = 
                match isExpr with
                | true -> xmasMap |> Map.map (fun k v -> if k = expr.var.Value then intersect v (if isLessOp then { A = 1; B = expr.num.Value - 1 } else { A = expr.num.Value + 1; B = 4000 }) else v)
                | _ -> xmasMap
            let left'  = tree expr.goto 0 (depth' + 1) xmasMapLeft
            let xmasMapRight =
                match isExpr with
                | true -> 
                    xmasMap |> Map.map (fun k v -> if k = expr.var.Value then intersect v (if isLessOp then { A = expr.num.Value; B = 4000 } else { A = 1; B = expr.num.Value }) else v)
                | _ -> xmasMap
            let right' = tree goto (priority' + 1) (depth' + 1) xmasMapRight
            { 
                name = goto
                priority = priority' |> Some
                left  = left'
                right = right' 
                depth = depth'
            } |> Some
let xmasMap =
    Map.ofList [
        "x", { A = 1; B = 4000 };
        "m", { A = 1; B = 4000 };
        "a", { A = 1; B = 4000 };
        "s", { A = 1; B = 4000 }
    ]
let tree' = tree "in" 0 0 xmasMap
printfn $"{sumPt2}"