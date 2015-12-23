namespace AdventOfCode

module DayTwo = 

    open System
    open System.IO
    open System.Text.RegularExpressions

    let times int x= x * int 

    let private input = lazy File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day02.txt"))

    let private parseLine (str : string)  = 
         match str.Split([|'x'|]) |> Seq.map(Int32.Parse) |> List.ofSeq with
            | [length;width;height] -> (length, width, height)
            | _ -> failwith "Invalid input"

    let surfaceArea (length, width, height) = 2*length*width + 2*width*height + 2*height * length
    let smallestFaceArea (length, width, height) = [length; width; height] |> List.sort |> Seq.take(2) |> Seq.fold (*) 1
    let volume (length, width, height) = length * width * height
    let smallestPerimeter (length, width, height) = [length + width ; width + height; height + length] |> List.min |> times(2)

    let solve() = let dimensions = input.Force() |> Seq.map(parseLine)
                  let totalArea = dimensions |> Seq.map(fun dimensions -> surfaceArea(dimensions) + smallestFaceArea(dimensions)) 
                                             |> Seq.fold (+) 0
                  let totalRibbon = dimensions |> Seq.map(fun dimensions -> volume(dimensions) + smallestPerimeter(dimensions))
                                               |> Seq.fold (+) 0
                  (totalArea, totalRibbon)

