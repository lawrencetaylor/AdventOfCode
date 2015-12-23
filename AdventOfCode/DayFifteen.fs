namespace AdventOfCode

module DayFifteen = 
    open System.IO
    open System

    type incredientStats = 
        {   Capacity : int
            Durability : int
            Flavour : int
            Texture : int
            Calories : int
        }

    let trimEnd c (str : string) = str.TrimEnd [|c|]

    //Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
    let parseLine (mapState : Map<string, incredientStats>) (line: string) = 
        match line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
            |> Seq.map(trimEnd(',') >> trimEnd('.') >> trimEnd(':')) |> Array.ofSeq with
            | [|name; _; capacity;_; durability; _; flavour; _; texture; _; calories|]  -> 
                mapState.Add(name,  { Capacity =   Int32.Parse(capacity)
                                      Durability = Int32.Parse(durability)
                                      Flavour =    Int32.Parse(flavour)
                                      Texture =    Int32.Parse(texture)
                                      Calories =   Int32.Parse(calories) })



    let rec getSums (total : int) (ingredients : string list) : seq<(string*int) list> = 
        seq { 
              match ingredients with
               | [x] -> yield [(x, total)]
               | x :: xs -> for i in [0 .. total] do
                             let y = (getSums (total - i) xs) |> Array.ofSeq
                             let listToAppend = [(x,i)]
                             let append list = list |> List.append(listToAppend)
                             let z = y |> Seq.map(append) |> Array.ofSeq
                             yield! z

        }

    let calculate (ingredients : (int*incredientStats) list) = 

        let negativeToZero n = Math.Max(0, n)
        
        let getSum (map : incredientStats -> int) = 
            ingredients |> Seq.map(fun (count, state) -> count*map(state)) |> Seq.fold (+) 0 |> negativeToZero

        let values = [    getSum (fun s -> s.Capacity)
                          getSum (fun s -> s.Durability)
                          getSum (fun s -> s.Flavour)
                          getSum (fun s -> s.Texture)
                     ]
        match values |> List.contains(0) with
            | true -> (0, getSum(fun s -> s.Calories))
            | false -> (values |> Seq.fold (*) 1, getSum(fun s -> s.Calories))


  


    let partOne() = let statsMap = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day15.txt"))
                                        |> Seq.fold parseLine  Map.empty

                    //let x = [(44, statsMap.["Butterscotch"]);(56, statsMap.["Cinnamon"])] |> calculate

                    let ingredients = statsMap |> Map.toSeq |> Seq.map(fst) |> Seq.distinct
                    let sums = ingredients |> List.ofSeq |> getSums(100) 
                                           |> Seq.map(fun l -> l |> List.map(fun  (name, count) -> (count, statsMap.[name])))
                                           |> Seq.map(calculate)
                                           |> Seq.filter(fun (score, calories) -> calories = 500)
                                           |> Seq.sortDescending
                                           |> Seq.head
                    2

