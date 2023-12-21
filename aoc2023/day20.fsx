open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines "input.txt"
type Name = Name of string
type PulseType = High | Low
type Pulse = 
    {
        Type: PulseType
        Source: Name; 
        Destination: Name; 
    }
type FlipFlopState = On | Off
type FlipFlop = 
    {
        State: FlipFlopState; 
        Destinations: list<Name>
    }
    member this.Toggle = 
        { 
            this with State = match this.State with 
                                | On -> Off 
                                | Off -> On 
        }
type Conjunction = 
    {
        SourcePulses : Map<Name, PulseType>; 
        Destinations : list<Name>
    }
    member this.Update pulse = 
        {
            this with SourcePulses = this.SourcePulses |> Map.add pulse.Source pulse.Type
        }

type Broadcaster = 
    {
        Destinations: list<Name>
    }
type Button = 
    {
        Destinations: list<Name>
    }
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
            if c.SourcePulses 
                |> Map.values 
                |> Seq.forall (fun pulseType -> pulseType = High) 
            then Low 
            else High
        | Broadcaster _ -> Low
        | Button _      -> Low
        | Nothing       -> failwith "feil"
    member this.Destinations =
        match this with 
        | FlipFlop f    -> f.Destinations
        | Conjunction c -> c.Destinations
        | Broadcaster b -> b.Destinations
        | Button b      -> b.Destinations
        | Nothing       -> []
    member this.OnReceive p = 
        let send, updatedModule =
            match this with
            | FlipFlop f when p.Type = Low -> (true,  FlipFlop f.Toggle)
            | FlipFlop f                   -> (false, FlipFlop f)
            | Conjunction c                -> (true,  c.Update p |> Conjunction)
            | Nothing                      -> (false, this)
            | _                            -> (true,  this)
        let pulses = 
            if send 
            then this.Destinations 
                 |> List.map (fun d -> 
                                { 
                                    Source = p.Destination; 
                                    Destination = d; 
                                    Type = updatedModule.PulseType 
                                }) 
            else []
        (updatedModule, pulses)
let apply2 f g x = (f x, g x)
let start limit modules =
    let initialButtonPulse = 
        {
            Source = Name "button"; 
            Destination = Name "broadcaster"; 
            Type = Low
        }
    let pushButton modules = 
        let propogatePulse modules p =
            let m = modules 
                    |> Map.tryFind p.Destination 
                    |> Option.defaultValue Nothing
            let (m', ps) = m.OnReceive p
            (ps, modules |> Map.add p.Destination m')
        let nextState (pulses, modules) =
            match pulses with 
            | []    -> None
            | p::ps ->
                let (p', m') = propogatePulse modules p
                Some ((p, m'), (ps@p', m'))
        Seq.unfold nextState ([initialButtonPulse], modules) 
            |> apply2 (Seq.map fst) (Seq.last >> snd)
    let pushButtonAgain (pushCount, modules, _) =
        let stopPushing = pushCount > limit
        match stopPushing with
        | true  -> None
        | false -> 
            let pulses', modules' = pushButton modules
            Some ((pulses', pushCount), (pushCount + 1, modules', pulses'))
    (1, modules, Seq.empty) 
        |> Seq.unfold pushButtonAgain 
        |> apply2 (Seq.collect fst) (Seq.map snd) 
let parseFlipFlop line = 
    (Name "TODO", FlipFlop { State=Off; Destinations=[] })
let parseConjunction line = 
    (Name "TODO", Conjunction { SourcePulses = Map.empty; Destinations = [] })
let parseBroadcaster line = 
    (Name "TODO", Broadcaster { Broadcaster.Destinations = [] })
let rec modules data acc =
    match data with
    | [||] -> acc
    | _    -> 
        let line = Array.head data
        let matched = Regex.Match(line, @"(broadcaster|%\w+|&\w+)")
        let moduleTypeAndName = matched.Groups[1].Value
        let m = 
            match moduleTypeAndName with
            | _ when moduleTypeAndName[0] = '%' -> parseFlipFlop line
            | _ when moduleTypeAndName[0] = '&' -> parseConjunction line
            | "broadcaster"                     -> parseBroadcaster line
            | _                                 -> failwith "feil"
        modules (Array.tail data) (m::acc)
let mutable machine = Map.empty
let rec connect (ms: list<(Name * Module)>) = 
    match ms with
    | [] -> machine
    | _  -> 
        machine <- machine.Add(Name "TODO", (Broadcaster { Broadcaster.Destinations = [] }))
        connect (List.tail ms)

// TODO: for quick testing purposes to avoid parsing loll *remove after parsing done*
//       in other words, these are the examples, hard-coded into my typesystem,
//       for quick iterative algorithm development in the REPL etc.
let broadcaster' = Broadcaster { Broadcaster.Destinations = [Name "a"; Name "b"; Name "c"] }
let a' = FlipFlop { State=Off; Destinations=[Name "b"] }
let b' = FlipFlop { State=Off; Destinations=[Name "c"] }
let c' = FlipFlop { State=Off; Destinations=[Name "inv"] }
let inv' = Conjunction { SourcePulses = Map.empty; Destinations = [Name "a"] }
let connect' ms = 
    let mutable machine' = Map.empty
    machine' <- machine'.Add(Name "broadcaster", broadcaster')
    machine' <- machine'.Add(Name "a", a')
    machine' <- machine'.Add(Name "b", b')
    machine' <- machine'.Add(Name "c", c')
    machine' <- machine'.Add(Name "inv", inv')
    machine'
let total = 
    modules input []
    |> connect' // TODO: change this to the normal variation after parsing is done!!!
    |> start 1000
    |> fst
    |> Seq.groupBy (fun p -> p.Type) 
    |> Seq.map     (snd >> Seq.length) 
    |> Seq.reduce  (fun acc n -> acc * n)
printfn $"{total}"