open System.IO

type Label = Label of char
type Coord = { Row: int; Col: int }

type Visited =
    | Yes
    | No

type Start =
    { Position: Coord
      Visited: Visited }

    member this.Label = Label 'S'

type GardenPlot =
    { Position: Coord
      Visited: Visited }

    member this.Label = Label '.'

type Rock =
    { Position: Coord
      Visited: Visited }

    member this.Label = Label '#'

type Tile =
    | Start of Start
    | GardenPlot of GardenPlot
    | Rock of Rock
    | Nothing

    member this.Label =
        match this with
        | Start s -> s.Label
        | GardenPlot g -> g.Label
        | Rock r -> r.Label
        | Nothing -> failwith "feil"

    member this.Position =
        match this with
        | Start s -> s.Position
        | GardenPlot g -> g.Position
        | Rock r -> r.Position
        | Nothing -> failwith "feil"

    member this.Visit =
        match this with
        | Start s -> Start { s with Visited = Yes }
        | GardenPlot g -> GardenPlot { g with Visited = Yes }
        | Rock r -> Rock { r with Visited = Yes }
        | Nothing -> failwith "feil"

type Field =
    { Tiles: Tile[,] }

    member this.Step =
        // TODO: step in adjacent dirs and mark as visited
        //       don't process neighbors that have already been visited
        ()

let input = File.ReadAllLines "input.txt" |> array2D

let tiles =
    let toTile element coord =
        match element with
        | 'S' -> Start { Position = coord; Visited = Yes }
        | '.' -> GardenPlot { Position = coord; Visited = No }
        | '#' -> Rock { Position = coord; Visited = No }
        | _ -> failwith "feil"

    Array2D.mapi (fun r c element -> toTile element { Row = r; Col = c }) input

let field = { Tiles = tiles }
