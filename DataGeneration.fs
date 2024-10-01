module SimChooch.DataGeneration

open Bogus
open SimChooch.Domain
open SimChooch.Services

module Stations =
    let mutable private currentId = 0u
    let private getNextId() =
        currentId <- currentId + 1u
        currentId
    
    // Lists for generating plausible station names
    let neighborhoodTypes = [|
        "Park"; "Square"; "Gardens"; "Village"; "Heights"; "Hill"; "Valley"; "Green"; 
        "Plaza"; "Quarter"; "District"; "Commons"; "Crossing"; "Center"
    |]

    let landmarks = [|
        "University"; "Hospital"; "Library"; "Museum"; "Market"; "Theater"; "Stadium";
        "Gallery"; "Court"; "Exchange"; "Academy"; "Observatory"; "Botanic"; "Arcade"
    |]

    let cardinalSuffixes = [|"North"; "South"; "East"; "West"; "Central";|]
    let cardinalPrefixes = [| "Upper"; "Lower"; "Central"; "Outer"; "Inner" |]
        
    let generate (count: int) =
        let faker = Faker("en_US")
        
        let stationNameGenerator () =
            let nameType = faker.Random.Int(0, 4)
            match nameType with
            | 0 -> $"%s{faker.PickRandom(neighborhoodTypes)} %s{faker.PickRandom(cardinalSuffixes)}"
            | 1 -> $"%s{faker.Name.LastName()} %s{faker.PickRandom(neighborhoodTypes)}"
            | 2 -> $"%s{faker.PickRandom(landmarks)} %s{faker.PickRandom(neighborhoodTypes)}"
            | 3 -> $"%s{faker.Name.LastName()} %s{faker.PickRandom(landmarks)}"
            | _ -> $"%s{faker.PickRandom(cardinalPrefixes)} %s{faker.Name.LastName()} %s{faker.PickRandom(neighborhoodTypes)}"
        
        let stationGenerator =
            Faker<Station>()
                .CustomInstantiator(fun f ->
                    Station.create (getNextId() |> uint32) (stationNameGenerator())
                )

        stationGenerator.Generate(count)
        
module People =
    let mutable private currentId = 0u
    let private getNextId() =
        currentId <- currentId + 1u
        currentId
    
    let generate (people:People) (stations: Station[]) (count: int) (maxTime: uint64) =   
        let generator =
            Faker<Person>("en_US")
                .CustomInstantiator(fun f ->
                    let startingPoint = f.PickRandom(stations)
                    let destination = f.PickRandom(stations |> Array.filter (fun s -> s <> startingPoint))
                    people.CreatePerson (f.Name.FullName()) (f.Random.ULong(0UL, maxTime)) startingPoint destination
                )
        generator.Generate(count)