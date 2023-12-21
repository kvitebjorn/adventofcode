open System.IO

let input = File.ReadAllLines "input.txt" |> array2D

let transpose m =
    [ for columnIndex in [ 0 .. Array2D.length2 m - 1 ] do
          yield m[*, columnIndex] ]
    |> array2D

let reverse m =
    [ for rowIndex in [ 0 .. Array2D.length1 m - 1 ] do
          yield m[rowIndex, *] |> Array.rev ]
    |> array2D

let rec rotate n arr =
    match n with
    | 90 -> transpose arr |> reverse
    | -90 -> reverse arr |> transpose
    | 180 -> rotate 90 arr |> rotate 90
    | _ -> arr

let rec slideRocks arr i acc =
    if i < Array.length arr then
        let mutable slide =
            Array.takeWhile (fun e -> e <> '#') arr[i..] |> Array.sort |> Array.rev

        let add = Array.tryItem (i + slide.Length) arr

        match add with
        | None -> ()
        | _ -> slide <- Array.append slide [| '#' |]

        slideRocks arr (i + slide.Length) (Array.append acc slide)
    else
        acc

let tilt m =
    [ for columnIndex in [ 0 .. Array2D.length2 m - 1 ] do
          yield slideRocks m[*, columnIndex] 0 Array.empty ]
    |> array2D
    |> transpose

let score row c =
    Array.filter (fun x -> x = 'O') row |> Array.length |> (fun x -> x * c)

let total =
    let solved = tilt input

    let scores =
        [ for rowIndex in [ 0 .. Array2D.length1 input - 1 ] do
              yield score solved[rowIndex, *] (Array2D.length1 input - rowIndex) ]

    List.sum scores

printfn $"{total}"

let tortoiseAndHare list =
    let mutable returnVal = (false, -1, -1)

    if List.length list < 2 then
        returnVal
    else
        let mutable tortoise = 0
        let mutable hare = 1
        let mutable cont = true

        while (((hare + 1) < list.Length) && cont) do
            if list[tortoise] = list[hare] then
                returnVal <- (true, tortoise, hare)
                cont <- false
            else
                tortoise <- tortoise + 1
                hare <- hare + 2

        returnVal

let rec totalPt2 m count acc =
    let (hasCycle, i, j) = tortoiseAndHare acc

    if hasCycle then
        let delta = 1000000000 % (i - j)
        acc[i + delta + 1]
    else
        let up = tilt m
        let left = rotate 90 up |> tilt |> rotate -90
        let down = rotate 180 left |> tilt |> rotate 180
        let right = rotate -90 down |> tilt |> rotate 90

        let score =
            [ for rowIndex in [ 0 .. Array2D.length1 input - 1 ] do
                  yield score m[rowIndex, *] (Array2D.length1 input - rowIndex) ]
            |> List.sum

        totalPt2 right (count + 1) (acc @ [ score ])

printfn $"{totalPt2 input 0 []}"
