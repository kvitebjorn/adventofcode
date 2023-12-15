open System.IO;

let input = File.ReadAllText "input.txt" 
            |> (fun s -> s.Replace("\n", ""))
            |> (fun s -> s.Split(','))
let rec hash s acc =
    if Seq.length s > 0 then
        let curr = Seq.head s
        let ascii = int curr
        let step1 = acc + ascii 
        let step2 = step1 * 17
        let step3 = step2 % 256
        hash (Seq.tail s) step3
    else acc
let total = Array.map (fun s -> hash s 0) input |> Array.sum
printfn $"{total}"