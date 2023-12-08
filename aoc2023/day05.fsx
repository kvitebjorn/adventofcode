open System
open System.IO
open System.Linq
open System.Collections.Concurrent

let raw = File.ReadAllLines "day05.txt"

let split s =
    let folder a (cur, acc) = 
        match a with
        | "" -> [], cur::acc
        | _  -> a::cur, acc
    let result = Array.foldBack folder s ([Array.last s], []) 
    (fst result)::(snd result)
let stringToInts (s: string) = Seq.map (fun x -> x |> int64) (s.Split(" ") |> Array.toSeq)
let parseMap (m: seq<string>) = Seq.map stringToInts (Seq.tail m)
let rawMaps  = split raw[2..]
let seeds = Seq.map (fun x -> x |> int64) (raw[0].Replace("seeds: ", "").Split(" ") |> Array.toSeq)
let maps = List.map (fun i -> parseMap rawMaps[i]) [0..6]
let sourceToDest (mapsToCheck: seq<seq<int64>>) (source: int64) =
    let isBetween (n: int64) (map: seq<int64>) = 
        let lower = Seq.item 1 map
        let upper = (Seq.item 1 map) + (Seq.item 2 map)
        lower <= n && n <= upper
    let map = Seq.filter (fun m -> isBetween source m) mapsToCheck |> Seq.tryHead
    match map with
    | None -> source
    | _    -> source - ((Seq.item 1 map.Value) - (Seq.head map.Value))
let rec seedToLocation stage seed =
    let dest = sourceToDest maps[stage] seed
    match stage with
    | 6 -> dest
    | _ -> seedToLocation (stage + 1) dest

let solvePt1 =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let lowest = Seq.map (fun s -> seedToLocation 0 s) seeds |> Seq.min
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    $"{lowest.ToString()}"
let answerPt1 = solvePt1
printfn "%s" answerPt1

(* TODO: get rid of seedRanges, seedsPt2, etc. create a new starting map instead, 
         and work on ranges instead of ints
*)
let seedRanges = 
    Seq.map (fun x -> x |> int64) (raw[0].Replace("seeds: ", "").Split(" ") |> Array.toSeq)
let seedsPt2 =
    seedRanges
    |> Seq.chunkBySize 2
    |> Seq.map (fun p -> seq { p[0]..p[0]+p[1] })
    |> Seq.fold Seq.append Seq.empty<int64>
let solvePt2 =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let mutable results = new ConcurrentBag<int64>()
    seedsPt2
        .AsParallel()
        .WithDegreeOfParallelism(int(ceil((float(Environment.ProcessorCount) * 0.75) * 2.0)))
        .ForAll((fun s -> 
                    let result = seedToLocation 0 s
                    results.Add(int64(result))))
    let lowest = Seq.toList results |> Seq.min
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    $"{lowest.ToString()}"
let answerPt2 = solvePt2
printfn "%s" answerPt2
