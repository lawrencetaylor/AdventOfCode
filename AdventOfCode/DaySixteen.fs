namespace AdventOfCode

module DaySixteen = 

    open System
    open System.IO

                      let firstSue = @"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"

    let split (char : char) (str : string) = str.Split([|char|], StringSplitOptions.RemoveEmptyEntries )
    let toInt str = str |> Int32.Parse

    //Sue 1: goldfish: 6, trees: 9, akitas: 0
    let parse (str: string) = let colonIndex =  str.IndexOf(':')
                              let sueNumber = str.Substring(0, colonIndex).Replace("Sue", "").Trim() |> toInt
                              str.Substring(colonIndex + 1) |> split(',')
                                |> Seq.map(fun kvString -> match kvString |> split(':') with
                                                            | [|key; value|] -> (sueNumber, (key.Trim(), value |> toInt))
                                           )
   


    let solve() = 
                  let sueMap = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day16.txt"))
                                |> Seq.map(parse)
                                |> Seq.collect(fun i -> i)
                                |> Seq.groupBy(fst)
                                |> Map.ofSeq
                                |> Map.map(fun k values -> values |> Seq.map(snd) |> Map.ofSeq)


                  let couldBeSue sueNumber (conditionProp, conditionValue) = 
                    let sueProperties = sueMap.[sueNumber]

                    match sueProperties.ContainsKey(conditionProp) with
                        | true -> sueProperties.[conditionProp] = conditionValue
                        | false -> true

                  let couldBeSuePartTwo sueNumber (conditionProp, conditionValue) = 
                    let sueProperties = sueMap.[sueNumber]
                    match sueProperties.ContainsKey(conditionProp) with
                        | true -> match conditionProp with
                                    | "cats" ->  sueProperties.[conditionProp] > conditionValue
                                    | "trees" ->  sueProperties.[conditionProp] > conditionValue
                                    | "pomeranians" ->  sueProperties.[conditionProp] < conditionValue
                                    | "goldfish" ->  sueProperties.[conditionProp] < conditionValue
                                    | _ -> sueProperties.[conditionProp] = conditionValue
                        | false -> true


                  let conditions = firstSue |> split('\n') |> Seq.map(fun s -> match s |> split(':') with
                                                                                | [|key; value|] -> (key, value |> toInt)
                                                                     )
                                                            |> List.ofSeq

                  let rec satisfiesConditions couldBeFn c sueNumber = 
                    match c with
                        | [] -> true
                        | x::xs -> match couldBeFn sueNumber x with
                                    | true -> satisfiesConditions couldBeFn xs sueNumber
                                    | false -> false

                  let x = 405 |> satisfiesConditions(couldBeSuePartTwo)(conditions)

                  let partOne = [1 .. sueMap |> Seq.length] |> Seq.filter(satisfiesConditions(couldBeSue)(conditions)) |> Array.ofSeq
                  let partTwo = [1 .. sueMap |> Seq.length] |> Seq.filter(satisfiesConditions(couldBeSuePartTwo)(conditions)) |> Array.ofSeq

                  2

