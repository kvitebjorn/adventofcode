module Day5

open System.IO

let raw = File.ReadAllLines "day05.txt"

let split s =
    let folder a (cur, acc) = 
        match a with
        | "" -> [], cur::acc
        | _  -> a::cur, acc
    let result = Array.foldBack folder s ([Array.last s], []) 
    (fst result)::(snd result)
let stringToInts (s: string) = List.map (fun x -> x |> int64) (s.Split(" ") |> Array.toList)
let parseMap (m: list<string>) = List.map stringToInts m[1..]
let sourceToDest (maps: list<list<int64>>) (source: int64) =
    let isBetween (n: int64) (map: list<int64>) = 
        let lower = map[1]
        let upper = map[1] + map[2]
        lower <= n && n <= upper
    let map = List.filter (fun m -> isBetween source m) maps
    match map with
    | [] -> source
    | _  -> 
        let m = List.head map
        source - (m[1] - m[0])
let seedToLocation
    seedToSoilMaps
    soilToFertilizerMaps
    fertilizerToWaterMaps
    waterToLightMaps
    lightToTemperatureMaps
    temperatureToHumidityMaps
    humidityToLocationMaps
    seed =
    seed |>
    sourceToDest seedToSoilMaps |>
    sourceToDest soilToFertilizerMaps |>
    sourceToDest fertilizerToWaterMaps |>
    sourceToDest waterToLightMaps |>
    sourceToDest lightToTemperatureMaps |>
    sourceToDest temperatureToHumidityMaps |>
    sourceToDest humidityToLocationMaps
    
let solvePt1 (input: string array) =
    let seeds = List.map (fun x -> x |> int64) (input[0].Replace("seeds: ", "").Split(" ") |> Array.toList)
    let maps  = split input[2..]
    let seedToSoilMaps = parseMap maps[0]
    let soilToFertilizerMaps = parseMap maps[1]
    let fertilizerToWaterMaps = parseMap maps[2]
    let waterToLightMaps = parseMap maps[3]
    let lightToTemperatureMaps = parseMap maps[4]
    let temperatureToHumidityMaps = parseMap maps[5]
    let humidityToLocationMaps = parseMap maps[6]
    let seedToLocationPartial seed = 
        seedToLocation     
            seedToSoilMaps
            soilToFertilizerMaps
            fertilizerToWaterMaps
            waterToLightMaps
            lightToTemperatureMaps
            temperatureToHumidityMaps
            humidityToLocationMaps 
            seed
    let lowest = List.map seedToLocationPartial seeds |> List.min
    $"{lowest.ToString()}"

let answerPt1 = solvePt1 raw

let generateSeeds seedRanges =
    seedRanges
    |> List.chunkBySize 2
    |> List.map (fun p -> seq { p[0]..p[0]+p[1] } |> Seq.toList)
    |> List.reduce List.append

let solvePt2 (input: string array) =
    let seedRanges = 
        List.map (fun x -> x |> int64) (input[0].Replace("seeds: ", "").Split(" ") |> Array.toList)
    let seeds = generateSeeds seedRanges
    let maps  = split input[2..]
    let seedToSoilMaps = parseMap maps[0]
    let soilToFertilizerMaps = parseMap maps[1]
    let fertilizerToWaterMaps = parseMap maps[2]
    let waterToLightMaps = parseMap maps[3]
    let lightToTemperatureMaps = parseMap maps[4]
    let temperatureToHumidityMaps = parseMap maps[5]
    let humidityToLocationMaps = parseMap maps[6]
    let seedToLocationPartial seed = 
        seedToLocation     
            seedToSoilMaps
            soilToFertilizerMaps
            fertilizerToWaterMaps
            waterToLightMaps
            lightToTemperatureMaps
            temperatureToHumidityMaps
            humidityToLocationMaps 
            seed
    let lowest = List.map seedToLocationPartial seeds |> List.min
    $"{lowest.ToString()}"

let answerPt2 = solvePt2 raw
