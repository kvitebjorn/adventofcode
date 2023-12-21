open System
open System.IO
open System.Collections.Generic
open System.Linq
open System.Collections.Concurrent

let raw = File.ReadAllLines "input.txt"
let directions = raw[0] |> Seq.map (fun c -> if c = 'L' then 0 else 1) |> Seq.toList
let nodes = new Dictionary<string, list<string>>()

let _ =
    [ for line in raw[2..] do
          let splitted = line.Split(" = ")
          let node = splitted[0]
          let lr = splitted[1].Split(", ")
          let l = lr[0][1..]
          let r = lr[1][..2]
          nodes.Add(node, [ l; r ]) ]

let rec solvePt1 node step acc =
    let lr = nodes[node]
    let nextNode = lr[directions[step]]
    let nextStep = if step >= directions.Length - 1 then 0 else step + 1

    match nextNode with
    | "ZZZ" -> string (acc)
    | _ -> solvePt1 nextNode nextStep (acc + 1)

let answerPt1 = solvePt1 "AAA" 0 1
printfn "%s" answerPt1

let rec gcd (x: int64) (y: int64) = if y = 0 then x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let rec goPt2 node step acc =
    let lr = nodes[node]
    let nextNode = lr[directions[step]]
    let nextStep = if step >= directions.Length - 1 then 0 else step + 1
    let isDone = nextNode |> Seq.last = 'Z'

    match isDone with
    | true -> string (acc)
    | _ -> goPt2 nextNode nextStep (acc + 1)

let solvePt2 =
    let mutable counts = new ConcurrentBag<int64>()

    let startingNodes =
        (List.filter (fun n -> n.ToString().EndsWith("A")) (Seq.toList nodes.Keys))

    startingNodes
        .AsParallel()
        .WithDegreeOfParallelism(int (ceil ((float (Environment.ProcessorCount) * 0.75) * 2.0)))
        .ForAll(
            (fun n ->
                let steps = goPt2 n 0 1
                counts.Add(int64 (steps)))
        )

    let total = List.reduce (fun acc c -> lcm acc c) (Seq.toList counts)
    $"{total}"

let answerPt2 = solvePt2
printfn "%s" answerPt2
