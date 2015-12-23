namespace AdventOfCode

module DayThirteen = 

    open System
    open System.IO

    let rec permutations list taken = 
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations list (Set.add l taken)  do
                  yield l::perm }

    let parseLine (mapState : Map<string*string, int>) (line: string) = 
        match line.Trim().TrimEnd('.').Split(' ') with
            | [|name; _; gainOrLose; value; _; _; _; _; _; _; otherPerson|]  -> mapState.Add((name.Trim(), otherPerson.Trim()), match gainOrLose with
                                                                                                                                    | "gain" -> Int32.Parse(value)
                                                                                                                                    | "lose" -> -Int32.Parse(value)
                                                                                             )

    let calculateChange (map : Map<string*string,int>) sum (p1, p2)  = sum + map.[(p1,p2)] + map.[(p2,p1)]

    let rec sequentialPairs list : seq<string*string> = 
        seq { match list  with
                | [] -> ()
                | [x] -> ()
                | [x;y] -> yield (x,y)
                | x::y::xs -> yield (x,y)
                              yield! sequentialPairs (y::xs)
                           }

    let cyclicPairs list : seq<string*string> = 
        seq { match list with
                    | [] -> ()
                    | [x] -> ()
                    | [x;y] -> yield (x,y)
                    | l -> yield (l |> Seq.head, l |> Seq.last)
                           yield! l |> sequentialPairs
        
        }

    let addMyself (map : Map<string*string,int>)  otherPerson = map.Add(("Lawrence", otherPerson), 0)
                                                                   .Add((otherPerson, "Lawrence"), 0)

    let partOne() = let jsonStr = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day13.txt"))
                    let state = jsonStr |> Seq.fold parseLine Map.empty
                    let inviduals = state |> Map.toSeq |> Seq.map(fun ((n, _), _) -> n) |> Seq.distinct |> List.ofSeq
                    let state = inviduals |> Seq.fold addMyself state
                    let inviduals = state |> Map.toSeq |> Seq.map(fun ((n, _), _) -> n) |> Seq.distinct |> List.ofSeq


                    let permutationsOfI = (permutations inviduals Set.empty) |> Array.ofSeq
                                                |> Seq.map(fun perm -> ( perm |> cyclicPairs |> Seq.fold (calculateChange(state)) 0, perm))
                                                |> Seq.sortByDescending(fun (p, c) -> p)
                                                |> Seq.head

                    //let p = ["a"; "b"; "c"] |> cyclicPairs |> Array.ofSeq

                    2
