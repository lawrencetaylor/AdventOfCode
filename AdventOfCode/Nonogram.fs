namespace AdventOfCode

module Seq2 = 

    let public all predicate s  = (s |> Seq.exists(predicate >> not) |> not)

module Nonogram = 

    open AdventOfCode

    type Value  = int
    type Row = seq<Value>
    type Col = seq<Value>

    type Cell = Set<Value>

    type CellState = Blank | Filled | Indetermiate | Error

    let blank (v : Value) = v < 0
    let filled = blank >> not

    let cellState (cell : Cell) =
        match cell with
            | c when c |> Set.isEmpty -> Error
            | c when c |> Seq2.all(blank) -> Blank
            | c when c |> Seq2.all(filled) -> Filled
            | _ -> Indetermiate

    let cellString cell = match cell |> cellState with
                            | Error -> "E"
                            | Blank -> "."
                            | Filled -> "#"
                            | Indetermiate -> "?"

    let rec solveRow v (clue : int list) = 
        match (v, clue) with
            | (v, []) -> [-v ; -v]
            | (v, x :: xs) ->   let inter = [v+1 .. v+x] |> List.rev
                                [-v ; -v] |> List.append(inter) |> List.append(solveRow (v + x + 1) xs)




    let solve = let x = solveRow 1 [4;3]
                2

