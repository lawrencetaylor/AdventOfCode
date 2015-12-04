namespace AdventOfCode

module DayThree = 

    open System.IO
    open System.Collections.Generic

    let map = new Dictionary<int*int, bool>()

    let logVisit (x, y) = match map.ContainsKey((x,y)) with
                            | true -> map.[(x,y)] <- true
                            | false -> map.Add((x,y), false)

    let solutionP input =  let folder (currentX, currentY) c =   
                                                                 let newPosition = match c with
                                                                                  | '>' -> (currentX + 1, currentY)
                                                                                  | '<' -> (currentX - 1, currentY)
                                                                                  | '^' -> (currentX, currentY + 1)
                                                                                  | 'v' -> (currentX, currentY - 1)
                                                                                  | _ -> (currentX, currentY)
                                                                 logVisit((currentX, currentY))
                                                                 newPosition
                           
                           logVisit((0, 0))
                           input |> Seq.fold folder (0, 0) |> ignore
                           



    let solution() = let str = File.ReadAllText(@"../../../AdventOfCode/Day03.txt")
                     str |> Seq.indexed |> Seq.filter(fun (index, ch) -> index % 2 = 0) |> Seq.map(snd) |> solutionP |> ignore
                     str |> Seq.indexed |> Seq.filter(fun (index, ch) -> index % 2 = 1) |> Seq.map(snd) |> solutionP |> ignore

                     let moreThanOneVisit = map |> Seq.length
                     moreThanOneVisit
