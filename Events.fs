module SimChooch.Events

open SimChooch.Common
open SimChooch.Domain

type PutPersonInStation =
    {
        Time: uint64
        Person: Person
        Station: Station
    } interface IHasTime with
        member this.Time = this.Time
        
type PutTrainInStation =
    {
        Time: uint64
        Train: Train
        Station: Station
    } interface IHasTime with
        member this.Time = this.Time
        
type PullTrainOutOfStation =
    {
        Time: uint64
        Train: Train
        Station: Station
    } interface IHasTime with
        member this.Time = this.Time
    
type Commands =
    | PutTrainInStation of PutTrainInStation
    | PullTrainOutOfStation of PullTrainOutOfStation
    | PutPersonInStation of PutPersonInStation
    interface IHasTime with
        member this.Time =
            match this with
            | PutTrainInStation c -> c.Time
            | PullTrainOutOfStation c -> c.Time
            | PutPersonInStation c -> c.Time
module Commands =
    type CreatePutTrainInStationCommand = uint64 -> Train -> Station -> Commands
    let putTrainInStation : CreatePutTrainInStationCommand =
        fun time train station ->
            PutTrainInStation {
                Time = time
                Train = train
                Station = station
            }
    
    type CreatePullTrainOutOfStationCommand = uint64 -> Train -> Station -> Commands
    let pullTrainOutOfStation : CreatePutTrainInStationCommand =
        fun time train station ->
            PullTrainOutOfStation {
                Time = time
                Train = train
                Station = station
            }
        
    type CreatePutPersonInSimulationCommand = Person -> Commands
    let putPersonInSimulation : CreatePutPersonInSimulationCommand =
        fun person ->
            PutPersonInStation {
                Time = person.EnterTime
                Person = person
                Station = person.StartingPoint
            }

type PersonArrivedAtDestination =
    {
        Person: Person
        Station: Station
        Time: uint64
    }
    override this.ToString() =
        let person = (this.Person :> System.IFormattable).ToString("P", null)
        let station = (this.Station :> System.IFormattable).ToString("S", null)
        $"%s{person} exited at %s{station}."
    
    interface IHasTime with
        member this.Time = this.Time
        
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "N" -> $"%s{secondsToTimeOfDay this.Time} > %s{this.ToString()}\n           Total time was: %s{formatTimeSpan ((this.Person.ExitTime - this.Person.EnterTime) |> int)}"
            | _   -> this.ToString()

type TrainArrivedAtStation =
    {
        Train: Train
        Station: Station
        Time: uint64
    }
    override this.ToString() =
        let station = (this.Station :> System.IFormattable).ToString("S", null)
        $"Train %i{this.Train.Id} arrived at %s{station}."
    
    interface IHasTime with
        member this.Time = this.Time
        
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "N" -> sprintf $"%s{secondsToTimeOfDay this.Time} - %s{this.ToString()}"
            | _   -> this.ToString()

type TrainDepartedStation =
    {
        Train: Train
        Station: Station
        Time: uint64
    }
    override this.ToString() =
        let station = (this.Station :> System.IFormattable).ToString("S", null)
        sprintf $"Train %i{this.Train.Id} departed %s{station}."
    
    interface IHasTime with
        member this.Time = this.Time
        
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "N" -> sprintf $"%s{secondsToTimeOfDay this.Time} - %s{this.ToString()}\n           %i{this.Train.PassengerCount}/%i{this.Train.Capacity} passengers"
            | _   -> this.ToString()
    
type PersonEnteredStation =
    {
        Person: Person
        Station: Station
        Time: uint64
    }
    override this.ToString() =
        let person = (this.Person :> System.IFormattable).ToString("P", null)
        let station = (this.Station :> System.IFormattable).ToString("S", null)
        sprintf $"%s{person} entered at %s{station}."
    
    interface IHasTime with
        member this.Time = this.Time
        
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "N" -> sprintf $"%s{secondsToTimeOfDay this.Time} < %s{this.ToString()}"
            | _   -> this.ToString()

type Notifications =
    | PersonArrivedAtDestination of PersonArrivedAtDestination
    | PersonEnteredStation of PersonEnteredStation
    | TrainArrivedAtStation of TrainArrivedAtStation
    | TrainDepartedStation of TrainDepartedStation
    interface System.IFormattable with
        member this.ToString(format, _) =
            let str = match this with
                        | PersonEnteredStation n ->
                            (n :> System.IFormattable).ToString(format, null)
                        | PersonArrivedAtDestination n ->
                            (n :> System.IFormattable).ToString(format, null)
                        | TrainArrivedAtStation n ->
                            (n :> System.IFormattable).ToString(format, null)
                        | TrainDepartedStation n ->
                            (n :> System.IFormattable).ToString(format, null)
            str
            
module Notifications =
    type CreatePersonArrivedAtDestination = uint64 -> Person -> Station -> Notifications
    let personArrivedAtDestination : CreatePersonArrivedAtDestination =
        fun time person station ->
            PersonArrivedAtDestination {
                Time = time
                Person = person
                Station = station
            }
            
    type CreatePersonEnteredStation = uint64 -> Person -> Station -> Notifications
    let personEnteredStation : CreatePersonEnteredStation =
        fun time person station ->
            PersonEnteredStation {
                Time = time
                Person = person
                Station = station
            }
    
    type CreateTrainArrivedAtStation = uint64 -> Train -> Station -> Notifications
    let trainArrivedAtStation : CreateTrainArrivedAtStation =
        fun time train station ->
            TrainArrivedAtStation {
                Time = time
                Train = train
                Station = station
            }
    
    type CreateTrainDepartedStation = uint64 -> Train -> Station -> Notifications
    let trainDepartedStation : CreateTrainDepartedStation =
        fun time train station ->
            TrainDepartedStation {
                Time = time
                Train = train
                Station = station
            }
            
    let fromCommand (command: Commands) =
        match command with
        | PutTrainInStation c -> trainArrivedAtStation c.Time c.Train c.Station
        | PullTrainOutOfStation c -> trainDepartedStation c.Time c.Train c.Station
        | PutPersonInStation c -> personEnteredStation c.Time c.Person c.Station