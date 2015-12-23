namespace AdventOfCode

module DayFourteeen = 

    open System
    open System.IO
    open System.Diagnostics


    type reindeerStats = { Speed : int; FlyingTime : int; RestingTime: int}

    //Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    let parseLine (mapState : Map<string, reindeerStats>) (line: string) = 
        match line.Split(' ') with
            | [|name; _; _;speed; _; _; timeFlying; _; _; _; _; _; _; timeResting; _|]  -> mapState.Add(name,  { Speed = Int32.Parse(speed)
                                                                                                                 FlyingTime = Int32.Parse(timeFlying)
                                                                                                                 RestingTime = Int32.Parse(timeResting) })

    [<DebuggerDisplay("{CurrentDistance}")>]
    type state = { CurrentDistance : int; TotalTime : int; RestingTime: int option; FlyingTime: int option; Points : int}
                    static member ``default`` = { CurrentDistance = 0; TotalTime = 0; RestingTime = None; FlyingTime = Some 0; Points = 0}

    let tick (stats :reindeerStats) state  = 
        match (state.RestingTime, state.FlyingTime) with
                            | (None,Some t) when t < stats.FlyingTime -> { state with FlyingTime = (t + 1) |> Some
                                                                                      TotalTime = state.TotalTime + 1
                                                                                      CurrentDistance = state.CurrentDistance + stats.Speed }
                            | (None, Some t) when t = stats.FlyingTime -> { state with FlyingTime = None
                                                                                       RestingTime = Some 1
                                                                                       TotalTime = state.TotalTime + 1 }
                            | (Some t, None) when t < stats.RestingTime -> { state with RestingTime = (t + 1) |> Some 
                                                                                        TotalTime = state.TotalTime + 1 }
                            | (Some t, None) when t = stats.RestingTime -> { state with RestingTime = None
                                                                                        FlyingTime = Some 1
                                                                                        CurrentDistance = state.CurrentDistance + stats.Speed
                                                                                        TotalTime = state.TotalTime + 1 }

    let tickAndAssignPoints (statsMap : Map<string, reindeerStats>) (stateMap : Map<string, state>) = 
        let incrementedStates = stateMap |> Map.map(fun k v -> tick(statsMap.[k]) v)
        let highestDistance = incrementedStates |> Map.toSeq |> Seq.map(fun (rName, state) -> state.CurrentDistance) |> Seq.distinct |> Seq.sortByDescending(fun d -> d) |> Seq.head
        incrementedStates |> Map.map(fun k v -> match v.CurrentDistance = highestDistance with
                                        | true -> { v with Points = v.Points + 1 }
                                        | false -> v )


    let partOne() = let jsonStr = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day14.txt"))
                    let reindeerStats = jsonStr |> Seq.fold parseLine Map.empty
                    let folder = tickAndAssignPoints(reindeerStats)

                    



                    let initialState = reindeerStats |> Map.toSeq |> Seq.map(fun (rName, _) -> (rName, state.``default``)) |> Map.ofSeq

                    let z = [1 .. 2503] |> Seq.fold( fun state counter -> state |> folder) initialState
                                        |> Map.map(fun k v -> (v.CurrentDistance, v.Points))
                                        |> Map.toSeq
                                        |> Seq.sortByDescending(fun (_, (_, d)) -> d)
                                        |> Array.ofSeq

                    let y = reindeerStats       
                                                |> Map.toSeq
                                                |> Seq.map(fun (rName, stats) -> (rName, [1 .. 2503] |> Seq.fold (fun s counter -> s |> tick(stats)) state.``default``))
                                                |> Seq.sortByDescending(fun (rName, distance) -> distance)
                                                |> Seq.map(snd)
                                                |> Array.ofSeq

                    //let (reindeers, stats) = reindeerStats |> Map.toSeq |> Seq.head
                    //let folder = tick(stats)
                    //let x = [1 .. 1000] |> Seq.fold(fun s counter -> folder(s)) state.``default``

                    2

