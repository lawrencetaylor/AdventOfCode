namespace AdventOfCode

module DayTen = 

    open System
    open System.IO

    let loadElements = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day10 - Elements.txt"))
                        |> Seq.map(fun s -> match s.Split(',') with 
                                             | [|_ ; element;  evolution; strings|] -> (element.Trim(),( evolution.Split(' ') |> Array.filter(fun s -> String.IsNullOrWhiteSpace(s) |> not) |> Array.map(fun s -> s.Trim()), strings.Trim()))
                                  )
                        |> Map.ofSeq

    let evolve (elements : seq<string*int>) = elements |> Seq.map(fun (e, count) -> loadElements.[e] |> fst |> Seq.map(fun el -> (el, count))) 
                                                       |> Seq.collect(fun i -> i)
                                                       |> Seq.groupBy(fun (el, _) -> el)
                                                       |> Seq.map(fun (el, elements) -> (el, elements |> Seq.map(snd) |> Seq.fold (+) 0))

    let times multiplier arg = arg * multiplier

    let getString (element : string)  = loadElements.[element] |> snd

    let getCount (elements : seq<string*int>)  = elements |> Seq.map(fun (e, count) -> loadElements.[e] |> snd |> Seq.length |> times(count)) |> Seq.fold (+) 0

    let iterate (func : 'a -> 'a) init count   = [1 .. count] |> Seq.fold (fun state counter -> state |> func) init

    let partOne() = let testIterator = iterate evolve ([("Yb", 1)] |> Seq.ofList)

                    let iterationCount = 50

                    let test = testIterator iterationCount |> getCount

                    2

