open System.IO
open System.Text.RegularExpressions

let raw = File.ReadAllLines "input.txt"
let vowelPredicate (s: string) = Regex.Matches(s, @"[aeiou]").Count >= 3
let repetitionPredicate s = Regex.Matches(s, @"(.)\1").Count >= 1

let forbiddenPredicate s =
    Regex.Matches(s, @"ab|cd|pq|xy").Count < 1

let isNice s =
    vowelPredicate s && repetitionPredicate s && forbiddenPredicate s

let solvePt1 input =
    input |> Seq.filter isNice |> Seq.length |> sprintf "%i"

let answerPt1 = solvePt1 raw
printfn "%s" answerPt1

let repetitionPredicatePt2 s =
    Regex.Matches(s, @"(..).*\1").Count >= 1

let middlePredicate s = Regex.Matches(s, @"(.).\1").Count >= 1

let isNicePt2 s =
    repetitionPredicatePt2 s && middlePredicate s

let solvePt2 input =
    input |> Seq.filter isNicePt2 |> Seq.length |> sprintf "%i"

let answerPt2 = solvePt2 raw
printfn "%s" answerPt2
