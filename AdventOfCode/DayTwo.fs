namespace AdventOfCode

module DayTwo = 

    open System
    open System.IO
    open System.Text.RegularExpressions

    let times int x= x * int 

    let solution() = let dimensions = File.ReadAllLines(@"../../../AdventOfCode/Day02.txt")
                                                |> Seq.map(fun s -> let [l;w;h] =  s.Split([|'x'|]) |> Seq.map(Int32.Parse) |> List.ofSeq
                                                                    (l, w, h)
                                                          )
    
                     let totalAreaOfPaper = dimensions |> Seq.map(fun (l, w, h) ->  let totalSurface =  2*l*w + 2*w*h + 2*h*l
                                                                                    let [sw ; sl] = [l;w;h] |> List.sort |> Seq.take(2) |> Seq.toList
                                                                                    totalSurface + (sw * sl)
                                                                  )

                     let totalRibbon = dimensions |> Seq.map(fun (l, w, h) ->  let possibleHalfPerimiter = [l+w; l+h; h+w]
                                                                               let minPerimiter = possibleHalfPerimiter |> List.min |> times(2)
                                                                               let volume = l*w*h
                                                                               volume + minPerimiter
                                                                  )
                     //                                     )

                     let totalArea = totalAreaOfPaper |> Seq.fold (+) 0
                     let totalRibbon = totalRibbon |> Seq.fold (+) 0
                     (totalArea, totalRibbon)

