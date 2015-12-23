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

    let hasLeadingZeros (zeros: int) (str : string) = 
        let testString = String('0', zeros)
        str.Substring(0, zeros) = testString

    let passes (zeros: int) (str: string) = 
                str |> System.Text.Encoding.ASCII.GetBytes |> md5 |> hasLeadingZeros(zeros)

    let solve() = let x = "yzbqklnj"
                  let nextCoin = Seq.initInfinite(fun index -> x + index.ToString())
                                    |> Seq.filter(passes(5))
                                    |> Seq.head
                  let anotherCoin = Seq.initInfinite(fun index -> x + index.ToString())
                                    |> Seq.filter(passes(6))
                                    |> Seq.head
                  (nextCoin, anotherCoin)
                     
                     

