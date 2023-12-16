open System.IO;
open System.Collections.Generic

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

let boxes = new Dictionary<int, list<(string * int)>>()
let rec fillBoxes (ops: string array) =
    match ops.Length with
    | 0 -> ()
    | _ -> 
        let op = Array.head ops
        let code = op |> Seq.toList |> List.filter (fun c -> c = '=' || c = '-') |> List.head
        let parts = op.Split(code)
        let label = parts[0]
        let focal = if code = '=' then parts[1] |> int else 0
        let hash'  = hash label 0
        let (hasLenses, lenses) = boxes.TryGetValue hash'
        let newLenses = if hasLenses then List.filter (fun l -> fst l <> label) lenses else []
        match code with
        | '=' ->  
                if hasLenses then 
                    if newLenses.Length = lenses.Length then 
                        boxes[hash'] <- newLenses @ [(label, focal)]
                    else
                        let idx = List.findIndex (fun l -> fst l = label) lenses
                        boxes[hash'] <- List.insertAt idx (label, focal) newLenses
                else
                    boxes[hash'] <- [(label, focal)]
                fillBoxes (Array.tail ops)
        | _    ->
            if hasLenses then 
                boxes[hash'] <- newLenses
            fillBoxes (Array.tail ops)
fillBoxes input |> ignore
let scoreBox (box: KeyValuePair<int, list<string * int>>) = 
    List.mapi (fun i l -> (box.Key + 1) * (i + 1) * (snd l)) box.Value |> List.sum
let mutable totalPt2 = 0
for kvp in boxes do
    totalPt2 <- totalPt2 + scoreBox kvp
printfn $"{totalPt2}"