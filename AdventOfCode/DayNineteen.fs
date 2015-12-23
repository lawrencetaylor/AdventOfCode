namespace AdventOfCode

module DayNineteen = 

    open System
    open System.IO

    let rec parseStr (map : Map<string,seq<string>>) (str : char list)  = 
        seq {
            match str with
                | [] -> ()
                | x::y::xs when map.ContainsKey(String([|x;y|])) -> yield String([|x;y|])
                                                                    yield! parseStr map xs
                | x::xs -> yield String([|x|])
                           yield! parseStr map xs
            }

    let replaceAtIndex index replacement listOfElements    = 
        let (before, after) = listOfElements |> List.splitAt(index)
        match after with
            | [] -> before |> List.append([replacement]) |> String.Concat
            | x::xs -> [before;[replacement];xs] |> List.concat  |> String.Concat

    let replaceAtIndexMany index replacements listOfElements = 
        seq { for replacement in replacements do
                yield replaceAtIndex index replacement listOfElements
        }

    let replaceString (initialString : string) index (oldValue: string) (replacement : string) = 
        let before = initialString.Substring(0, index)
        let after = initialString.Substring(index + oldValue.Length)
        [before; replacement; after] |> String.Concat

    let rec getIndicesOfMatch (str: string) (search : string) startingIndex = 
        seq{ 
              match str.IndexOf(search) with
                | -1 -> ()
                | index -> yield startingIndex + index
                           yield! getIndicesOfMatch (str.Substring(index + search.Length)) search (startingIndex + index + search.Length)
        }
        
    let rec replace (str: string) (mapping : Map<string,seq<string>>) (mapKeys: Set<string>) (counter : int) = 
          seq {
                match str with
                    | "e" -> yield counter
                    | _ -> let keysToTry = mapKeys |> Seq.map(fun k -> k, getIndicesOfMatch str k 0 |> Seq.map(fun i -> (k, i)))
                                                   |> Seq.map(snd)
                                                   |> Seq.collect(fun i -> i)
                                                   |> Seq.sortByDescending(fun (str, i) -> str.Length)
                                                   |> Array.ofSeq
                           match keysToTry |> Seq.isEmpty with
                            | true -> ()
                            | false ->  for (oldValue, index) in keysToTry do
                                            for newValue in mapping.[oldValue] do
                                                let newString = replaceString str index oldValue newValue
                                                yield! replace newString mapping mapKeys (counter + 1)
              }
                            
    let solve() = let x = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day19.txt")) 
                  let mappings = x |> Seq.takeWhile(fun s -> System.String.IsNullOrWhiteSpace(s) |> not)
                                   |> Seq.map(fun s -> match s.Split([|' '|]) with
                                                         | [|source; "=>"; target|] -> (source, target)
                                             )
                                   |> Seq.groupBy(fst)
                                   |> Map.ofSeq
                                   |> Map.map(fun k v -> v |> Seq.map(snd))

                  let reverseMappings = x |> Seq.takeWhile(fun s -> System.String.IsNullOrWhiteSpace(s) |> not)
                                           |> Seq.map(fun s -> match s.Split([|' '|]) with
                                                                 | [|source; "=>"; target|] -> (target, source)
                                                     )
                                           |> Seq.groupBy(fst)
                                           |> Map.ofSeq
                                           |> Map.map(fun k v -> v |> Seq.map(snd))

                  let testString = x |> Seq.skipWhile(fun s -> System.String.IsNullOrWhiteSpace(s) |> not)
                                     |> Seq.collect(fun i -> i)
                                     |> Seq.toList
                                     |> parseStr(mappings)
                                     |> Seq.toList

                  let initialSet = Set.empty
                  let x = testString |> Seq.indexed
                                     |> Seq.filter(fun (index, value) -> mappings.ContainsKey(value))
                                     |> Seq.map(fun (index, value) -> (index, mappings.[value]))
                                     |> Seq.fold(fun s (index,replacements) -> (replaceAtIndexMany index replacements testString) 
                                                                                                            |> Seq.fold(fun set str -> set |> Set.add(str)) s
                                                ) initialSet

                  let startingString = testString |> String.Concat

                  let keys = reverseMappings |> Map.toSeq |> Seq.map(fst) |> Set.ofSeq

                  let values = (replace startingString reverseMappings keys 0) |> Seq.head

                  //let som = permute ["H"] mappings |> Array.ofSeq 

                  let X = ["H"; "O"; "H"] |> replaceAtIndex(2)("A")
                  
                  2

