namespace AdventOfCode

module DayFour = 

    open System
    open System.Security.Cryptography
    open System.Text

    let md5 (data : byte array) : string =
        use md5 = MD5.Create()
        (StringBuilder(), md5.ComputeHash(data))
        ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
        |> string

    let hash = md5 "hello world"B;

    let hasLeadingZeros (str : string) = str.Substring(0, 6) = "000000"
    let passes (str: string) = Console.WriteLine("Testing " + str) |> ignore
                               str |> System.Text.Encoding.ASCII.GetBytes |> md5 |> hasLeadingZeros

    //yzbqklnj9962624
    let solution() = let x = "yzbqklnj"
                     let y = "yzbqklnj9962624" |> passes
                     let seq = Seq.initInfinite(fun index -> x + index.ToString())
                                    |> Seq.filter(passes)
                                    |> Seq.head
                     2
                     
                     

