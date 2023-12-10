#r "nuget: YC.QuickGraph"
open System.IO
open QuickGraph
open QuickGraph.Algorithms.ShortestPath
open QuickGraph.Algorithms.Observers

type TVertex = (int * int * char)
type TEdge   = Edge<(int * int * char)>
let mutable sx = -1
let mutable sy = -1
let input = File.ReadAllLines "input.txt"
let mutable g = new AdjacencyGraph<TVertex, TEdge>(true)
for i in [0..(input.Length - 1)] do
    let line = input[i]
    for j in [0..(line.Length - 1)] do
        let c = line[j]
        match c with
        | 'S' -> 
                sx <- j
                sy <- i
        | _ -> ()
        match c with
        | '.' -> ()
        | _   -> g.AddVertex((j, i, c)) |> ignore
let up   = ['|';'7';'F';'S']
let down = ['|';'L';'J';'S']
let left = ['-';'L';'F';'S']
let right = ['-';'7';'J';'S']
let isAdjacent (x1,y1,c1) (x2,y2,c2) =
    let dx = abs(x1 - x2)
    let dy = abs(y1 - y2)
    let isNextTo = dx + dy = 1
    if isNextTo then
        let isUp    = y1 - 1 = y2 && List.contains c2 up && List.contains c1 down
        let isDown  = y1 + 1 = y2 && List.contains c2 down && List.contains c1 up
        let isLeft  = x1 - 1 = x2 && List.contains c2 left && List.contains c1 right
        let isRight = x1 + 1 = x2 && List.contains c2 right && List.contains c1 left
        isUp || isDown || isLeft || isRight
    else false
for vertex in g.Vertices do
    let vertices = Seq.filter (fun v -> v <> vertex) g.Vertices
    let adjacent = Seq.filter (fun v -> isAdjacent vertex v) vertices
    for adjacentVertex in adjacent do
        let edge = new TEdge(vertex, adjacentVertex)
        g.AddEdge(edge) |> ignore
let dijkstra = new DijkstraShortestPathAlgorithm<TVertex, TEdge>(g, (fun x -> 1))
let distObserver = new VertexDistanceRecorderObserver<TVertex, TEdge>((fun x -> 1))
distObserver.Attach(dijkstra);
let pathObserver = new VertexPredecessorRecorderObserver<TVertex, TEdge>()
pathObserver.Attach(dijkstra);
let solvePt1 =
    dijkstra.Compute(TVertex(sx, sy, 'S'))
    let farthest = Seq.last distObserver.Distances.Values
    $"{farthest}"
let answerPt1 = solvePt1 
printfn "%s" answerPt1

let solvePt2 =
    """
        TODO: 
            1. get the main loop for 'S'; 
            2. do the same algorithm i did in 2015 on HEERD
                to identify pixels that had to be shaded in a target area
    """
let answerPt2 = solvePt2 
printfn "%s" answerPt2