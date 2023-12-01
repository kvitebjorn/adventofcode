module Day1

open System.IO
open System
open System.Text.RegularExpressions

let raw = File.ReadAllLines "day01.txt"

let parse (s: string) = 
    let digits = List.filter Char.IsDigit (Seq.toList s)
    int($"{List.head digits}{List.last digits}")

let rec solvePt1 input =     
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

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let toNumericDigit s =
    match s with
    | Prefix "one"   _ -> int(1).ToString()
    | Prefix "two"   _ -> int(2).ToString()
    | Prefix "three" _ -> int(3).ToString()
    | Prefix "four"  _ -> int(4).ToString()
    | Prefix "five"  _ -> int(5).ToString()
    | Prefix "six"   _ -> int(6).ToString()
    | Prefix "seven" _ -> int(7).ToString()
    | Prefix "eight" _ -> int(8).ToString()
    | Prefix "nine"  _ -> int(9).ToString()
    | _ -> s

let parsePt2 (s: string) = 
    let digits = getDigits s
    let firstDigit = toNumericDigit (fst digits)
    let lastDigit  = toNumericDigit (snd digits)
    int($"{firstDigit}{lastDigit}")

let rec solvePt2 input =     
    let total = Array.sum(Array.map parsePt2 input)
    $"{total.ToString()}"

let answerPt2 = solvePt2 raw
