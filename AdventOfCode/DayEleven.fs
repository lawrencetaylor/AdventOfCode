namespace AdventOfCode

module DayEleven = 

    open System

    let a = 97 |> Convert.ToChar

    let letters = [97 .. 97 + 25] |> Seq.map(Convert.ToChar) |> Array.ofSeq
                                  |> Seq.indexed
                                  |> Seq.map(fun (i, c) -> (c, i))
                                  |> Map.ofSeq

    let isGoodLetter c = match c with
                            | 'i' -> false
                            | 'o' -> false
                            | 'l' -> false
                            | _ -> true

    let isSeq (a,b,c) = (letters.[a] + 1  = letters.[b]) && (letters.[b] + 1 = letters.[c])

    let inc c = match c with 
                  | 'z' -> failwith "Cannot increment z"
                  | c' -> Convert.ToChar(Convert.ToInt32(c')+1)

    let rec hasIncrementing l = match l with
                                 | [] -> false
                                 | [_] -> false
                                 | [_;_] -> false
                                 | [a; b; c] when (a,b,c) |> isSeq -> true
                                 | a::b::c:: xs when (a,b,c) |> isSeq -> true
                                 | x::xs -> hasIncrementing xs

    let rec hasPairs number l = match (number, l) with
                                 | (0, _) -> true
                                 | (n, []) when n > 0 -> false
                                 | (n,x::y::xs) when x = y -> hasPairs (n-1) xs
                                 | (n,x::xs) -> hasPairs n xs
                                 
    let isGoodPassword (str : string) = 
                             let cArray = str |> Seq.toList
                             let hasIncrementing = cArray |> hasIncrementing
                             let hasPairs = cArray |> hasPairs(2)
                             let hasAllGoodLetters = str |> Seq.exists(isGoodLetter >> not) |> not
                             let result = hasIncrementing && hasPairs && hasAllGoodLetters
                             result
                            
    

    let rec increment str = match str with
                                | [] -> ['a']  
                                | x::xs -> match x with
                                           | 'z' ->  'a' :: (increment xs)
                                           | _ -> let x' = x |> inc
                                                  match x' |> isGoodLetter with
                                                      | true -> x' :: xs
                                                      | false -> increment (x'::xs)

                                
    let rec recurSeq seed : seq<char list> = seq { let newValue = seed |> increment
                                                   yield newValue |> List.rev
                                                   yield! newValue |> recurSeq }
                                     

    let partOne() = let isGood = "ghijmmaa" |> isGoodPassword

                    let next = "hxbxxyzz" |> List.ofSeq
                                          |> List.rev |> recurSeq 
                                          |> Seq.filter(fun c -> c |> String.Concat |> isGoodPassword)
                                          |> Seq.head
                                          |> String.Concat
                    let x = 2
                    let validPasswordSeq seed = seq {
                                                 while(true) do
                                                  let nextPossible = seed |> List.rev |> increment |> List.rev
                                                  match nextPossible |> String.Concat |> isGoodPassword with
                                                    | true -> yield nextPossible
                                                    | false -> ()
                                                }

                    let next = "abcdefgh" |> Seq.toList |> validPasswordSeq |> Seq.head
                   
                    let x = 2
                    a

