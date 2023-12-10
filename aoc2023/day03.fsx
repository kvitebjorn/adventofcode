open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllText "input.txt"
let getIndices input regex =
    seq { for m in Regex.Matches(input, regex) do yield m.Value, m.Index }
let toCoord idx length =
    (idx % length, idx / length)
let isAdjacent (x1,y1) (x2,y2) = 
    abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1
let solvePt1 (rawInput: string) =
    let length  = rawInput.IndexOf('\n') + 1
    let input   = rawInput.Replace('\n', '.')
    let symbols = getIndices input @"[^\d\.\s]"
    let numbers = getIndices input @"\d+"
    let symbolCoords = Seq.map (fun x -> toCoord (snd x) length) symbols
    let isValidNum (num, idx) =
        let numIndices = seq { for x in idx .. (idx + String.length(num) - 1) do yield x }
        let numCoords  = Seq.map (fun x -> toCoord x length) numIndices
        let distances  = Seq.map (fun num -> Seq.map (fun x -> isAdjacent num x) symbolCoords) numCoords
        let validNums  = Seq.filter (fun d -> Seq.length (Seq.filter (fun x -> x = true) d) > 0) distances
        Seq.length validNums > 0
    let validNumbers = Seq.filter isValidNum numbers
    let total = Seq.sum (Seq.map (fun x -> int(fst x)) validNumbers)
    $"{total.ToString()}"
let answerPt1 = solvePt1 raw
printfn "%s" answerPt1

let solvePt2 (rawInput: string) =
    let length     = rawInput.IndexOf('\n') + 1
    let input      = rawInput.Replace('\n', '.')
    let numbers    = getIndices input @"\d+"
    let gears      = getIndices input @"[\*]"
    let gearCoords = Seq.map (fun x -> toCoord (snd x) length) gears
    let isValidNum (num, idx) gearCoord =
        let numIndices = seq { for x in idx .. (idx + String.length(num) - 1) do yield x }
        let numCoords  = Seq.map (fun x -> toCoord x length) numIndices
        let distances  = Seq.map (fun num -> isAdjacent num gearCoord) numCoords
        let validNums  = Seq.filter (fun x -> x = true) distances
        Seq.length validNums > 0
    let gearToNums = Seq.map (fun x -> Seq.filter (fun y -> isValidNum y x) numbers) gearCoords
    let validGears = Seq.filter (fun x -> Seq.length x = 2) gearToNums
    let gearRatios = Seq.map (fun nums -> Seq.fold (*) 1 (Seq.map (fun num -> int(fst num)) nums)) validGears
    let total = Seq.sum gearRatios
    $"{total.ToString()}"
let answerPt2 = solvePt2 raw
printfn "%s" answerPt2