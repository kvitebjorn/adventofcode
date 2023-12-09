open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllLines "input.txt"

let parseSet s = 
    let regexRed   = @"(\d+) red"
    let regexGreen = @"(\d+) green"
    let regexBlue  = @"(\d+) blue"
    let matchedRed    = Regex.Match(s, regexRed)
    let matchedGreen  = Regex.Match(s, regexGreen)
    let matchedBlue   = Regex.Match(s, regexBlue)
    let getMatchVal (m: Match) = 
        match m.ToString() with
        | "" -> 0
        | _  -> int(m.Groups[1].Value)
    let r = getMatchVal matchedRed
    let g = getMatchVal matchedGreen
    let b = getMatchVal matchedBlue
    (r,g,b)

let parseGame g = 
    let regexStr = @"Game (\d+): "
    let matched  = Regex.Match(g, regexStr)
    let gameId   = int(matched.Groups[1].Value)
    let sets     = g.Replace(matched.ToString(), "").Split(";") |> Seq.map parseSet
    (gameId, sets)

let isGoodGame g =
    let red   = 12
    let green = 13
    let blue  = 14
    let impossiblePredicate (r,g,b) = r > red || g > green || b > blue
    let impossibleSets = Seq.filter impossiblePredicate (snd g)
    Seq.length impossibleSets = 0

let solvePt1 input =     
    let games = Seq.map parseGame input
    let total = Seq.sum (Seq.map fst (Seq.filter (fun g -> isGoodGame g) games))
    $"{total.ToString()}"

let answerPt1 = solvePt1 raw
printfn "%s" answerPt1

let getRed   (r, _, _) = r
let getGreen (_, g, _) = g
let getBlue  (_, _, b) = b

let getCubePower s =
    let maxRed   = Seq.max (Seq.map getRed s)
    let maxGreen = Seq.max (Seq.map getGreen s)
    let maxBlue  = Seq.max (Seq.map getBlue s)
    maxRed * maxGreen * maxBlue

let solvePt2 input =     
    let games = Seq.map parseGame input
    let total = Seq.sum (Seq.map (fun g -> getCubePower (snd g)) games)
    $"{total.ToString()}"

let answerPt2 = solvePt2 raw

printfn "%s" answerPt1
printfn "%s" answerPt2