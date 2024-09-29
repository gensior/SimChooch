module SimChooch.Services

open SimChooch.Data
open SimChooch.Domain

type People() =
    let data = InMemoryArray<Person>(16)
    let mutable peopleArrivedCounter = 0u
    
    member this.CreatePerson =
        fun name entryTime startingStation destination ->
            let id = data.Count |> uint
            let person = Person.create entryTime id name startingStation destination
            data.Insert(person) |> ignore
            peopleArrivedCounter <- peopleArrivedCounter + 1u
            person
    
    member this.GetPerson =
        fun i -> data[i |> int]
        
    member this.TotalPeople () = data.Count
    
    member this.AverageWaitTime () =
        data.AsArray()
        |> Array.take (this.TotalPeople() |> int)
        |> Array.averageBy (fun x  -> (x.ExitTime - x.EnterTime |> float))
        
    member this.PeopleArrivedCounter
        with get() = peopleArrivedCounter
        and set(value) = peopleArrivedCounter <- value
        
type Stations() =
    let data = InMemoryMap<Station>()
    
    // member this.CreateStation =
    //     fun stationId name prev next ->
    //         let prevTrack = match prev with
    //                         | Some (id, dist) -> Some (Track.create id dist)
    //                         | None -> None
    //         let nextTrack = match next with
    //                         | Some (id, dist) -> Some (Track.create id dist)
    //                         | None -> None
    //         let station = Station.create stationId name nextTrack prevTrack
    //         data.Add(station)
    //         station
    
    member this.PassengerEntersStation =
        fun time person station ->
            Station.passengerEntersStation person station time
            ()
            
    member this.Count() = data.Count()
    
    member this.GetStation =
        data.Get
            
type Trains() =
    let data = InMemoryMap<Train>()
    
    member this.CreateTrain =
        fun id capacity cars speed ->
            let train = Train.create id capacity cars speed
            data.Add(train)
            train
            
    member this.ArriveAtStation =
        Train.dropPassengersOffAtStation
            
    member this.GetTrain =
        data.Get