namespace AdventOfCode

module DayTwenty = 

    open System

    let sqrtInt (n : Int64) = Math.Sqrt(n |> Convert.ToDouble) |> Convert.ToInt64
    let add n m = n + m

    let getDivisors (n : Int64) = 
        seq { for i in [1L .. (n |> sqrtInt)] do
                if i <> n && (n % i) = 0L then 
                    yield i
                    yield n / i
        }

    let getDivisors2 (n : Int64) = 
        seq { for i in [1L .. (n |> sqrtInt)] do
                if i <> n && (n % i) = 0L then 
                    let dual = n / i
                    if (dual <= 50L) then yield i
                    if (i <= 50L) then yield dual
        }

    let comutePresentsForHouseNumber i = let divisorSums = i |> getDivisors |> Set.ofSeq |> Set.add(i) |> Array.ofSeq                 
                                         (i, (divisorSums |> Seq.fold (+) 0L) * 10L)

    let comutePresentsForHouseNumberP2 i = let divisorSums = i |> getDivisors2 |> Set.ofSeq |> Set.add(i) 
                                                               |> Array.ofSeq                 
                                           (i, (divisorSums |> Seq.fold (+) 0L) * 11L)

    let solve() = let y = [1L .. 10L] |> Seq.map(fun i -> (i, i |> comutePresentsForHouseNumber)) |> Array.ofSeq

                  let d = 100L |> comutePresentsForHouseNumberP2
    
                  let v = comutePresentsForHouseNumber 6L
                  let limit = 36000000L

                  let mutable counter = 1L
                  let mutable sum = 0L
                  while(sum <= limit) do
                    sum <- comutePresentsForHouseNumberP2(counter) |> snd
                    counter <- counter + 1L

                  


                  //let firstHouseExceedingLimit = Seq.initInfinite(comutePresentsForHouseNumber) |> Seq.filter(fun i -> i |> snd >= limit) |> Seq.head
                  //let firstHouseExceedingLimit2 = Seq.initInfinite(comutePresentsForHouseNumberP2) |> Seq.filter(fun i -> i |> snd >= limit) |> Seq.head
                  2

