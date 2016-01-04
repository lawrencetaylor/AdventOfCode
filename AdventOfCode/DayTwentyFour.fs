namespace AdventOfCode

module DayTwentyFour = 

    open System
    open System.IO

    let rec combinations x options = 
        match x, options with
        | 0, _ -> [[]]
        | _, [] -> []
        | y , (x::xs) ->  ((combinations (y-1) xs) |> List.map ((@) [x])) @ combinations y xs

    let getResult partitionSize (nums: int list)   =

        let targetSize = (nums |> List.reduce (+)) / partitionSize

        [2..(nums.Length/partitionSize - 1)]
            |> List.map (fun n -> combinations n nums) 
            |> List.collect(fun i -> i)
            |> List.filter (fun option -> option |> List.sum = targetSize)
            |> Seq.groupBy (fun o -> o.Length)
            |> Seq.minBy (fun (partitionLength,_) -> partitionLength) 
            |> snd
            |> Seq.map (fun r -> r |> List.map(Convert.ToInt64) |> List.reduce (*)) 
            |> Seq.min      

    let solve() = let numbers = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day24-Real.txt")) 
                                |> Seq.map(Int32.Parse)
                                |> Seq.toList

                  numbers |> getResult(3)  |> printfn "Part 1: %d"
                  numbers |> getResult(4) |> printfn "Part 2: %d"

                  2

