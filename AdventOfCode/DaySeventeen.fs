namespace AdventOfCode

module DaySeventeen = 

    open System
    open System.IO

    let add value list  = list |> List.append([value])

    let rec combinations combination options total = 
        seq { 
                  match (total, options) with
                    | (0, _) -> yield combination
                    | (t, []) -> ()
                    | (t, x::xs) when x <= t -> yield! combinations (combination |> add(x)) xs (total - x)
                                                yield! combinations combination xs total
                    | (t, x::xs) -> ()
                    
        }


    let solve() = let x = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day17.txt")) |> Seq.map(Int32.Parse)
                                                                                             |> List.ofSeq
                                                                                             |> List.sort


                  let c = combinations List.empty x 150 |> Seq.toArray 
                  let partOne = c |> Seq.length
                  let partTwo = c |> Seq.groupBy(fun c -> c.Length)
                               |> Seq.sortBy(fst)
                               |> Seq.head
                               |> snd
                               |> Seq.length
                  (partOne, partTwo)

