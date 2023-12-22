open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines "input.txt"
type Name = Name of string

type PulseType =
    | High
    | Low

type Pulse =
    { Type: PulseType
      Source: Name
      Destination: Name }

type FlipFlopState =
    | On
    | Off

type FlipFlop =
    { State: FlipFlopState
      Destinations: list<Name> }

    member this.Toggle =
        { this with
            State =
                match this.State with
                | On -> Off
                | Off -> On }

type Conjunction =
    { SourcePulses: Map<Name, PulseType>
      Destinations: list<Name> }

    member this.Update pulse =
        { this with
            SourcePulses = this.SourcePulses |> Map.add pulse.Source pulse.Type }

type Broadcaster = { Destinations: list<Name> }
type Button = { Destinations: list<Name> }

type Module =
    | FlipFlop of FlipFlop
    | Conjunction of Conjunction
    | Broadcaster of Broadcaster
    | Button of Button
    | Nothing

    member this.PulseType =
        match this with
        | FlipFlop f ->
            match f.State with
            | On -> High
            | Off -> Low
        | Conjunction c ->
            if c.SourcePulses |> Map.values |> Seq.forall (fun pulseType -> pulseType = High) then
                Low
            else
                High
        | Broadcaster _ -> Low
        | Button _ -> Low
        | Nothing -> failwith "feil"

    member this.Destinations =
        match this with
        | FlipFlop f -> f.Destinations
        | Conjunction c -> c.Destinations
        | Broadcaster b -> b.Destinations
        | Button b -> b.Destinations
        | Nothing -> []

    member this.OnReceive p =
        let send, updatedModule =
            match this with
            | FlipFlop f when p.Type = Low -> (true, FlipFlop f.Toggle)
            | FlipFlop f -> (false, FlipFlop f)
            | Conjunction c -> (true, c.Update p |> Conjunction)
            | Nothing -> (false, this)
            | _ -> (true, this)

        let pulses =
            if send then
                this.Destinations
                |> List.map (fun d ->
                    { Source = p.Destination
                      Destination = d
                      Type = updatedModule.PulseType })
            else
                []

        (updatedModule, pulses)

let apply2 f g x = (f x, g x)

let start limit machine =
    printfn "start %A" machine

    let initialButtonPulse =
        { Source = Name "button"
          Destination = Name "broadcaster"
          Type = Low }

    let pushButton modules =
        let propogatePulse modules p =
            let m = modules |> Map.tryFind p.Destination |> Option.defaultValue Nothing
            let (m', ps) = m.OnReceive p
            (ps, modules |> Map.add p.Destination m')

        let nextState (pulses, modules) =
            match pulses with
            | [] -> None
            | p :: ps ->
                let (p', m') = propogatePulse modules p
                Some((p, m'), (ps @ p', m'))

        Seq.unfold nextState ([ initialButtonPulse ], modules)
        |> apply2 (Seq.map fst) (Seq.last >> snd)

    let pushButtonAgain (pushCount, modules, _) =
        let stopPushing = pushCount > limit

        match stopPushing with
        | true -> None
        | false ->
            let pulses', modules' = pushButton modules
            Some((pulses', pushCount), (pushCount + 1, modules', pulses'))

    (1, machine, Seq.empty)
    |> Seq.unfold pushButtonAgain
    |> apply2 (Seq.collect fst) (Seq.map snd)

let parseDestinations line =
    let matched = Regex.Match(line, @"-> (.*)")
    let destinations = matched.Groups[1].Value.ToString().Split(", ") |> Array.toList
    List.map (fun d -> Name d) destinations

let parseFlipFlop line name =
    (Name name,
     FlipFlop
         { State = Off
           Destinations = parseDestinations line })

let parseConjunction line name =
    (Name name,
     Conjunction
         { SourcePulses = Map.empty
           Destinations = parseDestinations line })

let parseBroadcaster line name =
    (Name name, Broadcaster { Broadcaster.Destinations = parseDestinations line })

let rec modules data acc =
    match data with
    | [||] -> acc
    | _ ->
        let line = Array.head data
        let matched = Regex.Match(line, @"(broadcaster|%\w+|&\w+)")
        let moduleTypeAndName = matched.Groups[1].Value

        let m =
            match moduleTypeAndName with
            | _ when moduleTypeAndName[0] = '%' -> parseFlipFlop line moduleTypeAndName[1..]
            | _ when moduleTypeAndName[0] = '&' -> parseConjunction line moduleTypeAndName[1..]
            | "broadcaster" -> parseBroadcaster line moduleTypeAndName
            | _ -> failwith "feil"

        modules (Array.tail data) (m :: acc)

let rec connect idx (machine: Map<Name, Module>) (ms: list<(Name * Module)>) =
    let isInBounds = idx < ms.Length

    match isInBounds with
    | false -> machine
    | true ->
        let mutable machine' = machine
        let m = ms[idx]

        match (snd m) with
        | Conjunction _ ->
            let sources =
                ms
                |> Seq.filter (fun (_, x) -> List.contains (fst m) x.Destinations)
                |> Seq.map fst
                |> Seq.map (fun n -> (n, Low))
                |> Map

            let updatedConjunction =
                Conjunction
                    { SourcePulses = sources
                      Destinations = (snd m).Destinations }

            let connected = (fst m, updatedConjunction)
            machine' <- machine.Add(connected)
        | _ -> machine' <- machine.Add(m)

        connect (idx + 1) machine' ms

let total =
    modules input []
    |> connect 0 Map.empty
    |> start 1000
    |> fst
    |> Seq.groupBy (fun p -> p.Type)
    |> Seq.map (snd >> Seq.length)
    |> Seq.reduce (fun acc n -> acc * n)

printfn $"{total}"
