
#load "Common.fs"
#load "DayOne.fs"
#load "DayTwo.fs"
#load "DayThree.fs"



let x = AdventOfCode.Common.rootDirectory <- "C:\Users\ltaylor\Documents\Visual Studio 2013\Projects\AdventOfCode\AdventOfCode"
let (d1_1, d1_2) = AdventOfCode.DayOne.solve()
let (d2_1, d2_2) = AdventOfCode.DayTwo.solve()
let d3 = AdventOfCode.DayThree.solve()

    

    