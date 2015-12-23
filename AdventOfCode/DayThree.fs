namespace AdventOfCode

module DayThree = 

    open System
    open System.IO
    open System.Collections.Generic

    let private input = lazy (File.ReadAllText(Path.Combine(Common.rootDirectory, "Day03.txt")) |> List.ofSeq)

    let visitHouse coordinates (map :Dictionary<'a, int>)  = 
        match map.ContainsKey(coordinates) with
            | true -> map.[coordinates] <- map.[coordinates] + 1 
                      map
            | false -> map.Add(coordinates, 1) |> ignore
                       map

    let move c (x, y) = 
        match c with 
         | '>' -> (x+1, y)
         | '<' -> (x-1, y)
         | '^' -> (x, y+1)
         | 'v' -> (x, y-1)
         | _ -> (x,y)

    let rec getHouseVisits str currentVisits currentPosition = 
        match str with
            [] -> currentVisits
            | x :: xs -> let currentVisits = currentVisits |> visitHouse(currentPosition)
                         let currentPosition = currentPosition |> move(x) 
                         getHouseVisits xs currentVisits currentPosition
                         


    let solve() =   
                     let visits = getHouseVisits (input.Force()) (Dictionary<int*int,int>()) (0,0)
                     let numberWithAPresent = visits |> Seq.length
                     
                     let santasPath = input.Force() |> List.indexed |> List.filter(fun (i, _) -> i%2 = 0) |> List.map(snd)
                     let robotsPath = input.Force() |> List.indexed |> List.filter(fun (i, _) -> i%2 = 1) |> List.map(snd)

                     let santasVisits = getHouseVisits santasPath (Dictionary<int*int,int>()) (0,0)
                     let santasAndRobotsVisits = getHouseVisits robotsPath santasVisits (0,0)
                     let santaAndRobotsHousesWithPresent = santasAndRobotsVisits |> Seq.length

                     (numberWithAPresent, santaAndRobotsHousesWithPresent)
