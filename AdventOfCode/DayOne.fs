namespace AdventOfCode

module DayOne = 

    open System
    open AdventOfCode
    open System.IO

    let private input = lazy File.ReadAllText(Path.Combine(Common.rootDirectory, "Day01.txt"))

    let folder (floor, runningCount, firstBasement) c = let newFloor = match c with
                                                                        | '(' -> ( floor + 1)
                                                                        | ')' -> (floor - 1)
                                                                        | _ -> ( floor)
                                                        match (newFloor, firstBasement) with
                                                            | (-1, None) -> (newFloor, runningCount + 1, Some runningCount)
                                                            | _ -> (newFloor, runningCount + 1, firstBasement)

    let solve() = let (floor, totalCount, firstBasement) = input.Force() |> Seq.fold folder (0, 1, None) 
                  (floor, firstBasement)







                        

