namespace AdventOfCode

module DayEight = 

    open System
    open CSharpLib
    open System.Text.RegularExpressions

    let quote = "\""
    let replaceSingleBackSlash replacement (str : string) = str.Replace("\\\\", replacement)
    let replaceQuote replacement (str: string) = str.Replace("\"", replacement)
    let replaceSlash replacement (str: string) = str.Replace("\\", replacement)
    let replaceSingleQuote replacement (str: string) = str.Replace("\\\"", replacement)
    let replaceBoundaryQuotes replacement (str: string) = replacement + str.Substring(1, str.Length - 2) + replacement
    let resolve (str: string) = str.Replace(@"~", "\"").Replace(@"#", "\"")

    let rec findHexes (str: string) = seq { match str.IndexOf(@"\x") with
                                                | -1 -> ()
                                                | index -> let hexStr = str.Substring(index, 4)
                                                           yield hexStr
                                                           let otherHexes = str.Substring(index + 4) |> findHexes
                                                           for oH in otherHexes do
                                                            yield oH

                                      }
 
    let replaceHex (str: string) (hex: string) = let hexPart = hex.Substring(2,2)
                                                 //let charPart = HexHelpers.fromHex(hexPart)
                                                 let replaced = str.Replace(hex, "@")
                                                 replaced

    let replaceHexes (str: string) = str |> findHexes |> Seq.fold replaceHex str
                                    
    let length (str : string) = str.Length
    let sum ints = ints |> Seq.fold (+) 0

    let format = replaceSingleBackSlash("a") >> replaceSingleQuote("a") >> replaceHexes >> replaceBoundaryQuotes("") >> resolve

    let addQuotes s = "\"" + s + "\""
    let expand = replaceSlash("aa") >> replaceQuote("bb") >> addQuotes

    let solution() = 
                     let lines = System.IO.File.ReadAllLines(@"../../../AdventOfCode/Day08.txt")
                     let rawLength = lines |> Seq.map(length) |> sum
                     let formattedLength = lines |> Seq.map(format) |> Array.ofSeq
                     //                            |> Seq.map(length)
                     //                            |> sum
                     let difference = rawLength - (formattedLength |> Seq.map(length) |> sum)

                     let expanded = lines |> Seq.map(expand) //|> Seq.map(fun l -> (l, l.Length)) |> Map.ofSeq

                     let diff21 = (expanded |> Seq.map(length) |> sum) - rawLength

                     2

                     