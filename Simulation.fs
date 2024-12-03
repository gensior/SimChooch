module SimChooch.Simulation

open SimChooch.Domain.Common
open SimChooch.Worldbuilding.Data
open SimChooch.Domain
open SimChooch.Domain.Events
open SimChooch.Worldbuilding.Services

type INotificationObserver =
    abstract member OnNotification: Notifications -> unit
    
let consoleObserver = { new INotificationObserver with
    member _.OnNotification(notification) =
        let n = (notification :> System.IFormattable).ToString("N", null)
        printfn $"%s{n}"
}

type Simulator(endTime : uint64) =
    let events = EventQueue<Commands>()
    let mutable simTime = 0UL
    let endTime = endTime
    /// miles per hour
    let trainSpeed = 30.
    
    let people = PeopleService(16)
    let mutable observers = [consoleObserver]
    
    member _.AddObserver(observer: INotificationObserver) =
        observers <- observer :: observers
   
    member _.RemoveObserver(observer: INotificationObserver) =
        observers <- observers |> List.filter ((<>) observer)
        
    member _.Notify(notification: Notifications) =
        observers |> List.iter _.OnNotification(notification)
        notification
        
    member this.PutPersonInStation (command: PutPersonInStation) =
        Station.passengerEntersStation command.Person command.Station command.Time
        simTime <- command.Time
        // create notification
        let notification = Notifications.personEnteredStation command.Time command.Person command.Station
        this.Notify(notification) |> ignore
        
    member this.PutTrainInStation (command: PutTrainInStation) =
        let eventTime = command.Time
        let waitTime = 30UL
        let trainLeavesStation = Commands.pullTrainOutOfStation (eventTime + waitTime) command.Train command.Station
        let peopleGettingOffTrain = Train.dropPassengersOffAtStation command.Time command.Train command.Station
        let mutable peopleArrivedCount = 0u
        let notification = Notifications.trainArrivedAtStation command.Time command.Train command.Station
        this.Notify(notification) |> ignore
        peopleGettingOffTrain |> List.iter (fun person ->
            peopleArrivedCount <- peopleArrivedCount + 1u
            this.Notify(Notifications.personArrivedAtDestination command.Time person command.Station) |> ignore
        )
        people.PeopleArrivedCounter <- people.PeopleArrivedCounter - peopleArrivedCount
        events.Enqueue(trainLeavesStation)
        simTime <- command.Time
    
    member this.PullTrainOutOfStation (command: PullTrainOutOfStation) =
        Train.putPassengersOnTrain command.Time command.Station command.Train
        let track = if command.Train.IsReversed then
                            match command.Station.Previous with
                                | Some next -> next
                                | None ->
                                    command.Train.IsReversed <- not command.Train.IsReversed
                                    command.Station.Next.Value
                        else
                            match command.Station.Next with
                                | Some next -> next
                                | None ->
                                    command.Train.IsReversed <- not command.Train.IsReversed
                                    command.Station.Previous.Value
        let arriveTime = command.Time + ((track.Distance / command.Train.Speed * 3600.) |> uint64)
        let trainArrivesAtStation = Commands.putTrainInStation arriveTime command.Train track.Destination
        events.Enqueue(trainArrivesAtStation)
        simTime <- command.Time
        let notification = Notifications.trainDepartedStation command.Time command.Train command.Station
        this.Notify(notification) |> ignore
    
    member private this.Act command =
        match command with
        | PutPersonInStation c ->
            this.PutPersonInStation c
        | PutTrainInStation c ->
            this.PutTrainInStation c
        | PullTrainOutOfStation c ->
            this.PullTrainOutOfStation c
            
    member this.Setup () =
        let line1 = Line.create 1u "First Line" Direction.Southbound
        let station1 = Station.create 1u "First Street"
        let station2 = Station.create 2u "Second Street"
        Station.connect (station1, station2) 1.0 line1
                         
        let station3 = Station.create 3u "Third Street"
        Station.connect (station2, station3) 0.4 line1
        
        let station4 = Station.create 4u "Fourth Street"
        Station.connect (station3, station4) 1.2 line1
        
        let station5 = Station.create 5u "Fifth Street"
        Station.connect (station4, station5) 0.8 line1
        
        let station6 = Station.create 6u "Sixth Street"
        Station.connect (station5, station6) 1.2 line1 
        
        let station7 = Station.create 7u "Seventh Street"
        Station.connect (station6, station7) 0.8 line1
        
        let station8 = Station.create 8u "Eighth Street"
        Station.connect (station7, station8) 0.6 line1 
        
        let train = Train.create 1u 200u 4u trainSpeed
        
        let command = Commands.putTrainInStation 0UL train station1
        events.Enqueue(command)
        
        let person1 = people.CreatePerson "Lisa Smonk" 0UL station1 station8
        events.Enqueue(Commands.putPersonInSimulation person1)
        
        let person2 = people.CreatePerson "Jesse Franceschini" 1UL station1 station7
        events.Enqueue(Commands.putPersonInSimulation person2)
        
        let person3 = people.CreatePerson "MaryKate Moran" 31UL station2 station6
        events.Enqueue(Commands.putPersonInSimulation person3)
        
        let person4 = people.CreatePerson "Phoebe" 2063UL station3 station7
        events.Enqueue(Commands.putPersonInSimulation person4)
        
        let person5 = people.CreatePerson "Douglas Hughes" 1034UL station4 station3
        events.Enqueue(Commands.putPersonInSimulation person5)
        
        // People.createPerson 4005UL 1u 8u |> ignore
        // events.Enqueue(Command.createPutPersonInStationCommand 5u 1u 4005UL, 4005UL)
        // ()
        
    member this.Run () =
        while events.Count > 0 && simTime < endTime && people.PeopleArrivedCounter > 0u do
            let command = events.Dequeue()
            this.Act command
        printfn $"Simulation ended with avg wait time of %s{formatTimeSpan (people.AverageWaitTime() |> int)}"