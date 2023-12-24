open System.IO
open System.Collections.Generic

type Coord = { Row: int; Col: int }

let input = File.ReadAllLines "input.txt" |> array2D

let find2D needle (arr: char[,]) =
    let rec go x y =
        if y >= arr.GetLength 1 then None
        elif x >= arr.GetLength 0 then go 0 (y + 1)
        elif arr[x, y] = needle then Some(x, y)
        else go (x + 1) y

    go 0 0

let start' = find2D 'S' input

let start =
    { Row = (fst start'.Value)
      Col = (snd start'.Value) }

let wrapped n =
    let n' = n % 131
    if n' >= 0 then n' else 131 + n'

let candidates (point: Coord) =
    let directions = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

    seq {
        for d in directions do
            let newRow' = point.Row + (fst d)
            let newCol' = point.Col + (snd d)
            let newPoint = { Row = newRow'; Col = newCol' }

            if input[newPoint.Row |> wrapped, newPoint.Col |> wrapped] <> '#' then // Make sure it wasn't a rock
                yield newPoint
    }

let bfs (point: Coord) maxDist =
    let tiles = new Dictionary<int, int>()
    let visited = new HashSet<Coord>()
    let queue = new Queue<Coord * int>()
    queue.Enqueue((point, 0))

    while queue.Count > 0 do
        let currPoint, dist = queue.Dequeue()

        if dist = (maxDist + 1) || visited.Contains(currPoint) then
            ()
        else
            if tiles.ContainsKey(dist) then
                tiles[dist] <- tiles[dist] + 1
            else
                tiles.Add(dist, 1)

            visited.Add(currPoint) |> ignore

            for nextPoint in candidates currPoint do
                queue.Enqueue((nextPoint, dist + 1))

    tiles

let steps (start: Coord) maxSteps =
    let tiles = bfs start maxSteps

    tiles
    |> Seq.sumBy (fun kvp -> if kvp.Key % 2 = maxSteps % 2 then kvp.Value else 0)

let result = steps start 64
printfn $"{result}"

let quad y0 y1 y2 n =
    let a = (y2 - (2L * y1) + y0) / 2L
    let b = y1 - y0 - a
    let c = y0
    (a * (n * n)) + (b * n) + c

let size = Array2D.length1 input |> int64
let edge = size / 2L
let goal = 26501365L
let target = (goal - edge) / size |> int64
let y0 = steps start 65
let y1 = steps start (65 + 131)
let y2 = steps start (65 + (131 * 2))
let resultPt2 = quad y0 y1 y2 target
printfn $"{resultPt2}"
