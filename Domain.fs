module SimChooch.Domain

open System.Collections.Generic
open SimChooch.Common

let formatTimeSpan (totalSeconds: int) =
    let hours = totalSeconds / 3600
    let minutes = (totalSeconds % 3600) / 60
    let seconds = totalSeconds % 60
    
    match hours, minutes, seconds with
    | h, 0, 0 when h > 0 -> sprintf "%d hour%s" h (if h = 1 then "" else "s")
    | h, m, 0 when h > 0 -> sprintf "%d hour%s and %d minute%s" h (if h = 1 then "" else "s") m (if m = 1 then "" else "s")
    | h, m, s when h > 0 -> sprintf "%d hour%s, %d minute%s, and %d second%s" h (if h = 1 then "" else "s") m (if m = 1 then "" else "s") s (if s = 1 then "" else "s")
    | 0, m, 0 when m > 0 -> sprintf "%d minute%s" m (if m = 1 then "" else "s")
    | 0, m, s when m > 0 -> sprintf "%d minute%s and %d second%s" m (if m = 1 then "" else "s") s (if s = 1 then "" else "s")
    | 0, 0, s -> sprintf "%d second%s" s (if s = 1 then "" else "s")
    
type SecondsToTimeOfDay = uint64 -> string
let secondsToTimeOfDay : SecondsToTimeOfDay =
    fun seconds ->
        let hours = seconds / 3600UL
        let minutes = (seconds % 3600UL) / 60UL
        let secs = seconds % 60UL
        $"%02d{hours}:%02d{minutes}:%02d{secs}"
        
type EventError =
    | TrainFull of uint

// /// SimTime * PersonId * StationId
// type PutPersonInStation = uint64 * uint * uint
// /// SimTime * TrainId * StationId
// type PutTrainInStation = uint64 * uint * uint
// /// SimTime * TrainId * StationId
// type PullTrainOutOfStation = uint64 * uint * uint
// type Command =
//     /// SimTime * TrainId * StationId
//     | PutTrainInStation of PutTrainInStation
//     /// SimTime * TrainId * StationId
//     | PullTrainOutOfStation of PullTrainOutOfStation
//     /// SimTime * PersonId * StationId
//     | PutPersonInStation of PutPersonInStation
// module Command =
//     let createPutTrainInStationCommand trainId stationId simTime =
//         PutTrainInStation(simTime, trainId, stationId)
//        
//     let createPullTrainOutOfStationCommand trainId stationId simTime =
//         PullTrainOutOfStation(simTime, trainId, stationId)
//         
//     let createPutPersonInStationCommand personId stationId simTime =
//         PutPersonInStation(simTime, personId, stationId)

type Person =
    {
        Id: uint
        Name: string
        StartingPoint: Station
        Destination: Station
        mutable EnterTime: uint64
        mutable ExitTime: uint64
    }
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "P" -> this.Name
            | _ -> this.ToString()
and Track = {
        Distance: float
        Destination: Station
    }
and Station =
    {
        Id: uint
        Name: string
        mutable Next: Track option
        mutable Previous : Track option
        mutable Passengers: Person list
    }
    interface IHasId with
          member this.Id = this.Id
          
    interface System.IFormattable with
        member this.ToString(format, _) =
            match format with
            | "S" -> $"%s{this.Name} station" 
            | _ -> this.ToString()

module Person =
    /// entryTime -> id -> name -> starting StationID -> ending StationId -> Person
    type create = uint64 -> uint -> string -> Station -> Station -> Person
    let create : create =
        fun entryTime id name startingStation endingStation ->
        {
            Id = id
            Name = name
            EnterTime = entryTime
            ExitTime = 0UL
            StartingPoint = startingStation
            Destination = endingStation 
        }
        
    type enter = Person -> uint64 -> unit
    let enter : enter =
        fun person simTime ->
            person.EnterTime <- simTime
            ()
            
    type exit = Person -> uint64 -> unit
    let exit : exit =
        fun person simTime ->
            person.ExitTime <- simTime
            ()
            
module Track =
    type create = Station -> float -> Track
    let create : create =
        fun destination distance ->
            {
                Distance = distance
                Destination = destination 
            }

    
module Station =
    /// simTime -> Name -> Next track -> Previous track -> Station
    type Create = uint -> string -> Station
    let create : Create =
        fun id name ->
            {
                Id = id
                Name = name
                Next = None
                Previous = None
                Passengers = List.empty
            }
            
    type passengerEntersStation = Person -> Station -> uint64 -> unit
    let passengerEntersStation : passengerEntersStation =
        fun person station simTime ->
            Person.enter person simTime
            station.Passengers <- person :: station.Passengers
            ()
            
    let connect startStation endStation distance =
        let previousTrack = Track.create startStation distance
        let nextTrack = Track.create endStation distance
        startStation.Next <- Some nextTrack
        endStation.Previous <- Some previousTrack
    
type Train =
    {
        Speed: float
        Id: uint
        Capacity: uint
        /// key: destination id, value: list of people IDs going to that destination
        mutable Passengers: Dictionary<uint, Person list>
        mutable PassengerCount: uint
        mutable IsReversed: bool
    } with
        member this.IsFull = this.PassengerCount = this.Capacity
        
        interface IHasId with
            member this.Id = this.Id
        
module Train =
    type Create = uint -> uint -> uint -> float -> Train
    let create : Create =
        fun id carCapacity (numberOfCars: uint) speed ->
            {
                Id = id
                Capacity = carCapacity * numberOfCars
                Passengers = Dictionary<uint, Person list>()
                PassengerCount = 0u
                IsReversed = false
                Speed = speed
            }
   
    type AddPassengerToTrain = Train -> Person -> unit
    let addPassengerToTrain : AddPassengerToTrain =
        fun train passenger ->
            if not train.IsFull
            then
                train.PassengerCount <- train.PassengerCount + 1u
                if not (train.Passengers.ContainsKey passenger.Destination.Id)
                then
                    train.Passengers.Add(passenger.Destination.Id, [passenger])
                else
                    train.Passengers[passenger.Destination.Id] <- passenger :: train.Passengers[passenger.Destination.Id]
            ()
    
    type DropPassengersOffAtStation = uint64 -> Train -> Station -> Person list
    let dropPassengersOffAtStation : DropPassengersOffAtStation =
        fun simTime train station ->
            let stationId = station.Id
            let mutable passengersLeavingCount = 0
            let mutable leavingPassengers = List.empty
            if train.Passengers.ContainsKey stationId
            then
                train.Passengers[stationId] |> List.iter (
                    fun person ->
                        passengersLeavingCount <- passengersLeavingCount + 1
                        Person.exit person simTime
                        leavingPassengers <- person :: leavingPassengers
                        ()
                )
                train.Passengers[stationId] <- List.Empty
                train.PassengerCount <- train.PassengerCount - (passengersLeavingCount |> uint)
            leavingPassengers
    
    type PutPassengersOnTrain = Station -> Train -> unit
    let putPassengersOnTrain : PutPassengersOnTrain =
        fun station train ->
            station.Passengers |> List.iter (
                fun person ->
                    if not train.IsFull
                    then
                        addPassengerToTrain train person
                )
            station.Passengers <- List.empty