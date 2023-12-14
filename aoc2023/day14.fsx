open System.IO;

let input = File.ReadAllLines "input.txt" |> array2D
let transpose m =
    [ for columnIndex in [0..Array2D.length2 m - 1] do 
        yield m[*,columnIndex] ] |> array2D
let reverse m = 
    [ for rowIndex in [0..Array2D.length1 m - 1] do
        yield m[rowIndex,*] |> Array.rev ] |> array2D
let rec rotate n arr = 
    match n with
    | 90  -> transpose arr |> reverse
    | -90 -> reverse arr   |> transpose
    | 180 -> rotate 90 arr |> rotate 90
    | _   -> arr
let rec slideRocks arr i acc =
    if i < Array.length arr then 
        let mutable slide = Array.takeWhile (fun e -> e <> '#') arr[i..] |> Array.sort |> Array.rev
        let add = Array.tryItem (i + slide.Length) arr
        match add with
        | None -> ()
        | _    -> slide <- Array.append slide [|'#'|]
        slideRocks arr (i + slide.Length) (Array.append acc slide) 
    else acc
let tilt m = 
    [ for columnIndex in [0..Array2D.length2 m - 1] do 
        yield slideRocks m[*,columnIndex] 0 Array.empty ] |> array2D |> transpose
let rec sokoban m = 
    let up    = tilt m
    let down  = rotate 180 m |> tilt |> rotate 180
    let left  = rotate 90  m |> tilt |> rotate -90
    let right = rotate -90 m |> tilt |> rotate 90
    // TODO: return on acceptance criteria, otherwise let it die off
    // WOW!!! they only have to roll north!!! LOL fml - i should read
    // i wanna keep these matrix functions so ima just leave this idea here...
    0
let score row c = Array.filter (fun x -> x = 'O') row |> Array.length |> (fun x -> x * c)
let total = 
    let solved = tilt input
    let scores = [ for rowIndex in [0..Array2D.length1 input - 1] do 
                    yield score solved[rowIndex, *] (Array2D.length1 input - rowIndex) ]
    List.sum scores
printfn $"{total}"