namespace AdventOfCode

module Seq = 

    let join leftSequence rightSequence leftKeySelector rightKeySelector = 
        seq { for lSeqItem in leftSequence do
                for rSeqItem in rightSequence do
                    if(leftKeySelector(lSeqItem) = rightKeySelector(rSeqItem)) then 
                        yield (lSeqItem, rSeqItem)

        }

module DayNine = 

    open System

    let rec permutations list taken = 
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations list (Set.add l taken)  do
                  yield l::perm }

    let pairs (source: seq<_>) = let firstS = source |> Seq.indexed
                                 let secondS = source |> Seq.indexed
                                 let join = Seq.join firstS (secondS)(fun (i, _) -> i)(fun (i, _) -> i - 1 )
                                                |> Seq.map(fun ((i, a), (j, b)) -> (a,b))
                                 join



    let solution() =  let lines = System.IO.File.ReadAllLines(@"../../../AdventOfCode/Day09.txt")
                      let x = lines |> Seq.map(fun s -> match s.Split(' ') with
                                                 | [|source; "to"; target; "="; distance|] -> ((source, target), Int32.Parse(distance))
                                               )
                                    |> Map.ofSeq

                      let locations = x |> Map.toSeq |> Seq.map(fst) |> Seq.map(fun (s, t) -> [|s;t|]) |> Seq.collect(fun i -> i) |> Set.ofSeq |> List.ofSeq

                      let all = (permutations locations Set.empty) |> Array.ofSeq

                      


                      let calculateDistances locationOrder = locationOrder |> pairs
                                                                           |> Seq.map(fun (s,t) -> match x.ContainsKey((s,t)) with
                                                                                                    | true ->  x.[(s,t)]
                                                                                                    | false ->  x.[(t,s)]
                                                                                      
                                                                                      )
                                                                           |> Seq.fold (+) 0

                      let d = all |> Seq.head |> calculateDistances

                      let sorted = all |> Seq.map(calculateDistances) |> Seq.sort |> Seq.rev |> Array.ofSeq
                        

                      2


