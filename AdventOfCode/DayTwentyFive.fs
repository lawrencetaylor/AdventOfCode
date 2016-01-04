namespace AdventOfCode

module DayTwentyFive = 

    //(row, column)
    let rec indexOf seed (x,y) = 
        match (x, y) with
            | (1L, 1L) -> seed + 1
            | (n, 1L) -> indexOf (seed+1) (1L, n - 1L)
            | (n, m) -> indexOf (seed+1) (n + 1L, m - 1L)

    let rec generateSequence seed = 
        seq { 
              yield seed
              yield! generateSequence ((seed * 252533L) % 33554393L)
        }


    let solve() = let position = (2947L, 3029L)
                  let i = indexOf 0 position
                  let at = 20151125L |> generateSequence 
                                     |> Seq.take(i) 
                                     |> Seq.rev
                                     |> Seq.head
                  2


