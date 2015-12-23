namespace AdventOfCode

module DayFive = 

    open System.IO

    let vowels = ['a'; 'e'; 'i'; 'o'; 'u']
    let forbiddenStrings = ["ab";"ab";"cd"; "pq";"xy"]

    let rec containsVowels count str = 
        match (count, str) with
            | (0, _) -> true
            | (n, []) -> false
            | (n, x::xs) -> match vowels |> List.contains(x) with
                             | true -> containsVowels (n-1) xs
                             | false -> containsVowels n xs

    let rec containsDuplicate str = 
        match str with
            | [] -> false
            | x::y::str when x = y -> true
            | x::xs -> containsDuplicate xs

    let hasForbiddenString (str : string) = forbiddenStrings |> Seq.exists(fun s -> str.Contains(s))

    let isGoodForPartOne (str : string) = 
        match str |> hasForbiddenString with
            | true -> false
            | false -> let hasEnoughVowels = lazy (str |> List.ofSeq |> containsVowels(3) )
                       let hasDuplicateCharacters = lazy (str |> List.ofSeq |> containsDuplicate)
                       hasEnoughVowels.Force() && hasDuplicateCharacters.Force()

    let rec hasLetterSandwich str =
        match str with
            | [] -> false
            | x::y::z::xs when x = z -> true
            | x::xs -> hasLetterSandwich xs

    let rec hasRepeatedPairAux str (map: Map<char*char,int>) =     
        match (str, str) with
            | ([x], _) -> false
            | (x::y::xs, _)  when map.ContainsKey((x,y)) -> true
            | (x::y::xs, z1::z2::z3::zs) when x = y && map.ContainsKey((z2,z3)) -> true
            | (x::y::xs, z1::z2::z3::zs) when x = y  -> hasRepeatedPairAux xs (map.Add((x,y), 0).Add((z2,z3), 0))
            | (x::y::xs, z::zs) -> hasRepeatedPairAux zs (map.Add((x,y), 0))

    let hasRepeatedPair str = hasRepeatedPairAux str Map.empty

    let isGoodForPartTwo (str: string) = 
        let hasLetterSandwhich = str |> List.ofSeq |> hasLetterSandwich
        let hasDuplicatePair = str |> List.ofSeq |> hasRepeatedPair
        hasLetterSandwhich && hasDuplicatePair

    let private input = lazy (File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day05.txt")))

    let solve() =  

                  let partOne = input.Force() |> Seq.filter(isGoodForPartOne) |> Set.ofSeq
                  let partTwo = input.Force() |> Seq.filter(isGoodForPartTwo) |> Set.ofSeq
                  (partOne, partTwo)


                     



