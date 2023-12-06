module Day5

open System.IO
open FSharp.Collections.ParallelSeq

let raw = File.ReadAllLines "day05.txt"

let split s =
    let folder a (cur, acc) = 
        match a with
        | "" -> [], cur::acc
        | _  -> a::cur, acc
    let result = Array.foldBack folder s ([Array.last s], []) 
    (fst result)::(snd result)
let seeds = Seq.map (fun x -> x |> int64) (raw[0].Replace("seeds: ", "").Split(" ") |> Array.toSeq)
let seedRanges = 
    Seq.map (fun x -> x |> int64) (raw[0].Replace("seeds: ", "").Split(" ") |> Array.toSeq)
let seedsPt2 =
    seedRanges
    |> Seq.chunkBySize 2
    |> PSeq.map (fun p -> seq { p[0]..p[0]+p[1] })
    |> PSeq.fold Seq.append Seq.empty<int64>
let stringToInts (s: string) = Seq.map (fun x -> x |> int64) (s.Split(" ") |> Array.toSeq)
let parseMap (m: seq<string>) = Seq.map stringToInts (Seq.tail m)
let maps  = split raw[2..]
let seedToSoilMaps = parseMap maps[0]
let soilToFertilizerMaps = parseMap maps[1]
let fertilizerToWaterMaps = parseMap maps[2]
let waterToLightMaps = parseMap maps[3]
let lightToTemperatureMaps = parseMap maps[4]
let temperatureToHumidityMaps = parseMap maps[5]
let humidityToLocationMaps = parseMap maps[6]
let sourceToDest (maps: seq<seq<int64>>) (source: int64) =
    let isBetween (n: int64) (map: seq<int64>) = 
        let lower = Seq.item 1 map
        let upper = (Seq.item 1 map) + (Seq.item 2 map)
        lower <= n && n <= upper
    let map = Seq.filter (fun m -> isBetween source m) maps |> Seq.tryHead
    match map with
    | None -> source
    | _    -> source - ((Seq.item 1 map.Value) - (Seq.head map.Value))
let seedToLocation seed =
    seed |>
    sourceToDest seedToSoilMaps |>
    sourceToDest soilToFertilizerMaps |>
    sourceToDest fertilizerToWaterMaps |>
    sourceToDest waterToLightMaps |>
    sourceToDest lightToTemperatureMaps |>
    sourceToDest temperatureToHumidityMaps |>
    sourceToDest humidityToLocationMaps

let solvePt1 =
    let lowest = Seq.map seedToLocation seeds |> Seq.min
    $"{lowest.ToString()}"

let answerPt1 = solvePt1

let solvePt2 =
    let lowest = Seq.map seedToLocation seedsPt2 |> Seq.min
    $"{lowest.ToString()}"

let answerPt2 = solvePt2
