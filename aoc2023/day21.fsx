open System.IO

type Label = Label of char
type Coord = { Row: int; Col: int }

type Visiting =
    | Yes
    | No

type Start =
    { Position: Coord
      Visiting: Visiting }

    member this.Label = Label 'S'

type Plot =
    { Position: Coord
      Visiting: Visiting }

    member this.Label = Label '.'

type Rock =
    { Position: Coord
      Visiting: Visiting }

    member this.Label = Label '#'

type Tile =
    | Start of Start
    | Plot of Plot
    | Rock of Rock
    | Nothing

    member this.Label =
        match this with
        | Start s -> s.Label
        | Plot g -> g.Label
        | Rock r -> r.Label
        | Nothing -> failwith "feil"

    member this.Position =
        match this with
        | Start s -> s.Position
        | Plot g -> g.Position
        | Rock r -> r.Position
        | Nothing -> failwith "feil"

    member this.Visiting =
        match this with
        | Start s -> s.Visiting
        | Plot g -> g.Visiting
        | Rock r -> r.Visiting
        | Nothing -> failwith "feil"

    member this.Visit =
        match this with
        | Start s ->
            Start
                { s with
                    Visiting =
                        match this.Visiting with
                        | Yes -> No
                        | No -> Yes }
        | Plot g ->
            Plot
                { g with
                    Visiting =
                        match this.Visiting with
                        | Yes -> No
                        | No -> Yes }
        | Rock r ->
            Rock
                { r with
                    Visiting =
                        match this.Visiting with
                        | Yes -> No
                        | No -> Yes }
        | Nothing -> failwith "feil"

type Garden =
    { Tiles: Tile[,] }

    member this.Print =
        for r in [ 0 .. Array2D.length1 this.Tiles - 1 ] do
            for c in [ 0 .. Array2D.length2 this.Tiles - 1 ] do
                let t = this.Tiles[r, c]
                let (Label c') = this.Tiles[r, c].Label

                let labelToPrint =
                    match t.Visiting with
                    | No -> c'
                    | Yes -> 'O'

                printf "%c" labelToPrint

            printfn ""

    member this.Neighbors(tile: Tile) =
        let (row, col) = tile.Position.Row, tile.Position.Col

        let isInBounds pos =
            pos.Row >= 0
            && pos.Col >= 0
            && pos.Row < Array2D.length1 this.Tiles
            && pos.Col < Array2D.length2 this.Tiles

        let north = { Row = row - 1; Col = col }
        let south = { Row = row + 1; Col = col }
        let east = { Row = row; Col = col + 1 }
        let west = { Row = row; Col = col - 1 }
        let neighbors = [ north; south; east; west ]

        List.filter isInBounds neighbors
        |> List.map (fun p -> this.Tiles[p.Row, p.Col])
        |> List.filter (fun t -> t.Label <> Label '#' && t.Visiting = No)

    member this.Step =
        let visiting =
            this.Tiles |> Seq.cast<Tile> |> Seq.filter (fun t -> t.Visiting = Yes)

        let neighbors = Seq.map (fun t -> this.Neighbors t) visiting |> Seq.concat

        let updatedTiles =
            Array2D.map
                (fun t ->
                    if Seq.contains t neighbors then t.Visit
                    else if Seq.contains t visiting then t.Visit
                    else t)
                this.Tiles

        { Tiles = updatedTiles }

    member this.Count =
        this.Tiles
        |> Seq.cast<Tile>
        |> Seq.fold (fun acc t -> if t.Visiting = Yes then acc + 1 else acc) 0

let input = File.ReadAllLines "input.txt" |> array2D

let tiles =
    let toTile element coord =
        match element with
        | 'S' -> Start { Position = coord; Visiting = Yes }
        | '.' -> Plot { Position = coord; Visiting = No }
        | '#' -> Rock { Position = coord; Visiting = No }
        | _ -> failwith "feil"

    Array2D.mapi (fun r c element -> toTile element { Row = r; Col = c }) input

let initialGarden = { Tiles = tiles }

let rec simulateSteps stepLimit stepNumber (garden: Garden) =
    printfn $"step # {stepNumber}"
    let keepWalking = stepNumber < stepLimit

    match keepWalking with
    | false -> garden
    | true -> simulateSteps stepLimit (stepNumber + 1) garden.Step

let resultGarden = simulateSteps 64 0 initialGarden
printfn "%A" resultGarden.Print
printfn "%d" resultGarden.Count
