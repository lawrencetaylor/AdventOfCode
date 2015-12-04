namespace AdventOfCode

module DayOne = 
    
    open FSharp.Data
    open System.IO

    let downloadString url =  FSharp.Data.Http.AsyncRequestString(url)


    let solution() =  let str = File.ReadAllText(@"../../../AdventOfCode/Day01.txt")
                      let folder (floor, runningCount, firstBasement) c = let newFloor = match c with
                                                                                          | '(' -> ( floor + 1)
                                                                                          | ')' -> (floor - 1)
                                                                                          | _ -> ( floor)
                                                                          match (newFloor, firstBasement) with
                                                                              | (-1, None) -> (newFloor, runningCount + 1, Some runningCount)
                                                                              | _ -> (newFloor, runningCount + 1, firstBasement)
                                                                          
                      
                      let (floor, totalCount, firstBasement) = str |> Seq.fold folder (0, 1, None)  
                      floor


                        

