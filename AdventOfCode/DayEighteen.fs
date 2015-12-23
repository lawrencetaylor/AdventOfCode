namespace AdventOfCode

module DayEighteen = 
    open System.IO
    open System
    open System.Text

    let parseCharacter c = match c with | '#' -> true | '.' -> false 

    let getWidth map =  map |> Map.toSeq |> Seq.length |> Convert.ToDouble |> Math.Sqrt |> Convert.ToInt32

    let parseLine (index, str) =  str |> Seq.indexed |> Seq.map(fun (a, b) -> ((index + 1, a + 1), b |> parseCharacter))

    let directions = [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)]
    let addTuple (a, b) (c, d) = (a+c, b+d)
                                 
    let calculateLight (map : Map<int*int, bool>) thisLight = 
            
              let numberOfNeighbouringLightsOn = directions |> Seq.map(addTuple(thisLight))
                                                            |> Seq.map(fun t -> let onOffVal =  map.[t]
                                                                                onOffVal)
                                                            |> Seq.filter(fun b -> b)
                                                            |> Seq.length
              match (map.[thisLight], numberOfNeighbouringLightsOn) with
                | (true, l)  -> [2;3] |> List.contains(l)
                | (false, l) -> l = 3

                            
    let iterateMap (cornersOn: bool) map (width : int) = 

        let calcLight = calculateLight(map)
        map |> Map.map(fun (i,j) v -> match (cornersOn, (i,j)) with
                                        | (true, (1, 1)) -> true 
                                        | (true, (1, y)) when y = width - 2 -> true
                                        | (true, (x, 1)) when x = width - 2 -> true
                                        | (true, (x, y)) when x = width - 2 && y = width - 2 -> true
                                        | (_, (0, _ )) -> v
                                        | (_, (x, _ )) when x = (width - 1) -> v
                                        | (_, (_, 0 )) -> v
                                        | (_, (_, y )) when y = (width - 1) -> v
                                        | (_, light) -> calcLight(light)
                      )

    let print map = 
        let width  = (map |> getWidth) - 2
        let lines = [1 .. width] |> Seq.map(fun i -> (i, StringBuilder())) |> Map.ofSeq

        for i in [1 .. width] do
            for j in [1 .. width] do
                let stringValue = match map.[(i, j)] with | true -> "#" | false -> "."
                lines.[i].Append(stringValue) |> ignore

        String.Join("\n", lines |> Map.toSeq |> Seq.sortBy(fst) |> Seq.map(fun (_, sb) -> sb.ToString()))

    let turnOnCorners width map  = 
        map |> Map.map(fun (i,j) v -> match (i,j) with
                                        | (1, 1) -> true
                                        | (1, y) when y = width - 2 -> true
                                        | (x, 1) when x = width - 2 -> true
                                        | (x, y) when x = width - 2 && y = width - 2 -> true
                                        | _ -> v
                      )


    let solve() = let x = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day18.txt")) 
                            |> Seq.indexed 
                            |> Seq.map(parseLine)
                            |> Seq.collect(fun i -> i)
                            |> Map.ofSeq

                  let width = x |> getWidth
                  let added = [0 .. width + 1 ] |> Seq.map(fun i -> [(0, i); (i, 0); (width + 1, i); (i, width+1)]) 
                                    |> Seq.collect(fun i -> i)
                                    |> Set.ofSeq

                  let newMap = added |> Seq.fold (fun (map : Map<int*int, bool>) n -> map.Add(n, false)) x 
                  let s = newMap |> print

                  let newWidth = newMap |> getWidth

                  let partOneMap = [1 .. 100] |> Seq.fold(fun m i -> iterateMap(false) m newWidth) newMap
                  let partTwoMap = [1 .. 100] |> Seq.fold(fun m i -> iterateMap(true) m newWidth) (newMap |> turnOnCorners(newWidth))

                  let partOne = partOneMap |> Map.filter(fun k v -> v) |> Map.toSeq |> Seq.length 
                  let partTwo = partTwoMap |> Map.filter(fun k v -> v) |> Map.toSeq |> Seq.length 


                  2
                                                                             

