open System
open System.IO
open System.Collections.Generic

type Node = { visited: bool; distance: int }

let input =
    File.ReadAllLines "input.txt"
    |> array2D
    |> Array2D.map string
    |> Array2D.map int

let nRows, nCols = Array2D.length1 input, Array2D.length2 input

let getAdjacent minMoves (col, row, dir, moves) =
    let r, l, u, d = 0, 1, 2, 3

    match dir with
    | 0
    | 1 ->
        if moves >= minMoves then
            [ dir, moves + 1; u, 1; d, 1 ]
        else
            [ dir, moves + 1 ]
    | 2
    | 3 ->
        if moves >= minMoves then
            [ dir, moves + 1; l, 1; r, 1 ]
        else
            [ dir, moves + 1 ]
    | _ -> [ dir, moves + 1 ]
    |> List.map (fun (dir, moves) ->
        match dir with
        | 0 -> (col + 1, row, dir, moves)
        | 1 -> (col - 1, row, dir, moves)
        | 2 -> (col, row - 1, dir, moves)
        | 3 -> (col, row + 1, dir, moves)
        | _ -> (-1, -1, -1, -1))

let isInBounds maxMoves grid (col, row, _, moves) =
    moves <= maxMoves
    && col >= 0
    && row >= 0
    && col < Array4D.length1 grid
    && row < Array4D.length2 grid

let InitialNode =
    { visited = false
      distance = Int32.MaxValue }

let cost (col, row, _, _) = input[row, col]

let dijkstra minMoves maxMoves (sCol, sRow) target =
    let grid = Array4D.create nCols nRows 4 (maxMoves + 1) InitialNode
    let queue = PriorityQueue<int * int * int * int, int>()
    let dirs = seq { 0..3 }

    for dir in dirs do
        queue.Enqueue((sCol, sRow, dir, 0), 0)

        grid[sCol, sRow, dir, 0] <-
            { grid[sCol, sRow, dir, 0] with
                distance = 0 }

    let isVisited (col, row, dir, moves) = grid[col, row, dir, moves].visited
    let dist (col, row, dir, moves) = grid[col, row, dir, moves].distance

    let minDist (col, row) =
        [ for dir in dirs do
              for moves in 0..maxMoves do
                  dist (col, row, dir, moves) ]
        |> Seq.min

    let rec visit () =
        match queue.Count, queue.Dequeue() with
        | 0, _ -> minDist target
        | _, node when isVisited node -> visit ()
        | _, (col, row, _, _) when (col, row) = target -> minDist (col, row)
        | _, (col, row, dir, moves) ->
            grid[col, row, dir, moves] <-
                { grid[col, row, dir, moves] with
                    visited = true }

            getAdjacent minMoves (col, row, dir, moves)
            |> Seq.filter (fun n -> isInBounds maxMoves grid n && (not (isVisited n)))
            |> Seq.map (fun n -> (n, dist (col, row, dir, moves) + cost n))
            |> Seq.filter (fun (n, d) -> d < dist n)
            |> Seq.iter (fun ((col', row', dir', moves'), d) ->
                grid[col', row', dir', moves'] <-
                    { grid[col', row', dir', moves'] with
                        distance = d }

                queue.Enqueue((col', row', dir', moves'), d))

            visit ()

    visit ()

let start = (0, 0)
let shortest = dijkstra 0 3 start (nCols - 1, nRows - 1)
printfn $"{shortest}"

let shortestPt2 = dijkstra 4 10 start (nCols - 1, nRows - 1)
printfn $"{shortestPt2}"
