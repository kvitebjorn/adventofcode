open System.IO
open System
open System.Text.RegularExpressions

let raw = File.ReadAllLines "day01.txt"

let parse (s: string) = 
    let digits = List.filter Char.IsDigit (Seq.toList s)
    int($"{List.head digits}{List.last digits}")

let solvePt1 input =     
    let total = Array.sum(Array.map parse input)
    $"{total.ToString()}"

let answerPt1 = solvePt1 raw

let getDigits s =
    let regexStr = @"([1-9]|one|two|three|four|five|six|seven|eight|nine)"
    let matched  = Regex.Match(s, regexStr)
    let matched2 = Regex.Match(s, regexStr, RegexOptions.RightToLeft)
    let first = matched.Groups[1].Value
    let last  = matched2.Groups[1].Value
    (first,last)

let toNumericDigit s =
    match s with
    | "one"   -> "1"
    | "two"   -> "2"
    | "three" -> "3"
    | "four"  -> "4"
    | "five"  -> "5"
    | "six"   -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine"  -> "9"
    | _       -> s

let parsePt2 (s: string) = 
    let digits = getDigits s
    let firstDigit = toNumericDigit (fst digits)
    let lastDigit  = toNumericDigit (snd digits)
    int($"{firstDigit}{lastDigit}")

let solvePt2 input =     
    let total = Array.sum(Array.map parsePt2 input)
    $"{total.ToString()}"

let answerPt2 = solvePt2 raw

printfn "%s" answerPt1
printfn "%s" answerPt2