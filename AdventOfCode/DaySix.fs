namespace AdventOfCode

module DaySix = 

    open System.IO
    open System
    open System.Collections.Generic

    type Point = int*int
    type SwitchType = Toggle of Point*Point | On of Point*Point | Off of Point*Point

    let toInt = Int32.Parse
    

    let getStartEnd (keyWord : string) (str: string) = let startIndex = str.IndexOf(keyWord) + keyWord.Length
                                                       let x = str.Substring(startIndex + 1)
                                                       let splittedonComma = x.Split(',') 
                                                       let startX = Int32.Parse(splittedonComma.[0])
                                                       let endY = Int32.Parse(splittedonComma.[2])

                                                       let x = splittedonComma.[1].Replace("through", ",").Split(',') 
                                                       let startY = Int32.Parse(x.[0])
                                                       let endX = Int32.Parse(x.[1])
                                                       ((startX, startY), (endX, endY))
                   
    //turn on 0,0 through 999,999
    //toggle 0,0 through 999,0
    let parse (str: string) = 
        match str.Split([|' ';','|], StringSplitOptions.RemoveEmptyEntries) with
           | [|"turn"; "on"; startX; startY; "through"; endX; endY|] -> SwitchType.On((startX |> toInt, startY |> toInt), (endX |> toInt, endY |> toInt))
           | [|"turn"; "off"; startX; startY; "through"; endX; endY|] -> SwitchType.Off((startX |> toInt, startY |> toInt), (endX |> toInt, endY |> toInt))
           | [|"toggle"; startX; startY; "through"; endX; endY|] -> SwitchType.Toggle((startX |> toInt, startY |> toInt), (endX |> toInt, endY |> toInt))
                                    
    let private input = lazy (File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day06.txt")))

    let apply (state : Dictionary<int*int, bool>) (switchType :SwitchType) = 
        match switchType with
            | On ((x1,y1),(x2,y2)) -> for x in [x1 .. x2] do
                                        for y in [y1 .. y2] do
                                            state.[(x,y)] <- true
            | Off ((x1,y1), (x2, y2)) -> for x in [x1 .. x2] do
                                            for y in [y1 .. y2] do
                                                state.[(x,y)] <- false
            | Toggle ((x1,y1), (x2, y2)) -> for x in [x1 .. x2] do
                                                for y in [y1 .. y2] do
                                                    state.[(x,y)] <- state.[(x,y)] |> not
        state

    let apply2 (state : Dictionary<int*int, int>) (switchType :SwitchType) = 
        match switchType with
            | On ((x1,y1),(x2,y2)) -> for x in [x1 .. x2] do
                                        for y in [y1 .. y2] do
                                            state.[(x,y)] <- (state.[(x,y)] + 1)
            | Off ((x1,y1), (x2, y2)) -> for x in [x1 .. x2] do
                                            for y in [y1 .. y2] do
                                                state.[(x,y)] <- (Math.Max(0, state.[(x,y)] - 1))
            | Toggle ((x1,y1), (x2, y2)) -> for x in [x1 .. x2] do
                                                for y in [y1 .. y2] do
                                                    state.[(x,y)] <- (state.[(x,y)] + 2)
        state

    let solve() = let x = input.Force() |> Seq.map(parse) |> Array.ofSeq

                  let state = Dictionary<int*int, bool>()
                  let state2 = Dictionary<int*int, int>()
                  for x in [0 .. 999] do
                    for y in [0 .. 999] do
                        state.Add((x,y), false)
                        state2.Add((x,y), 0)

                  let lightsOn = x |> Seq.fold apply state |> Seq.filter(fun kvp -> kvp.Value) |> Seq.length
                  let totalBrightness = x |> Seq.fold apply2 state2 |> Seq.map(fun kvp -> kvp.Value) |> Seq.fold (+) 0
                  2

