module Program

open Argu
open SimChooch
open SimChooch.Simulation

type CliArguments =
    | Working_Directory of path: string
    | Listener of host: string * port: int
    | Data of base64: byte[]
    | Port of tcp_port: int
    | Log_Level of level: int
    | Detach

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Listener _ -> "specify a listener (hostname : port)."
            | Data _ -> "binary data in base64 encoding."
            | Port _ -> "specify a primary port."
            | Log_Level _ -> "set the log level."
            | Detach -> "detach daemon from console."

[<EntryPoint>]
let main argv =
    let maxTime = 100000UL
    let numPeople = 40000
    let numStations = 24
    printfn " *** Stations ***"
    let stations = DataGeneration.Stations.generate(numStations) |> Seq.toArray
    stations
    |> Array.iter (
        fun s ->
            printfn $"%02i{s.Id} ) %s{s.Name}"
        )
    printfn " *** People ***"
    let peopleService = Services.People(numPeople)
    let people = DataGeneration.People.generate peopleService stations numPeople maxTime
    (*people
    |> Seq.iter (
        fun p ->
            printfn $"%03i{p.Id} - %s{p.Name}"
            printfn $"      (%s{Domain.secondsToTimeOfDay p.EnterTime}) %s{p.StartingPoint.Name} > %s{p.Destination.Name}"
        *)
    //let sim = Simulator(maxTime)
    //sim.Setup()
    //sim.Run()
    0