// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
#r "../packages/FSharp.Data.2.2.5/lib/net40/bin/FSharp.Data.dll"
open AdventOfCode
open FSharp.Data

// Define your library scripting code here

let x = async { let! html = FSharp.Data.Http.AsyncRequestString("http://tomasp.net")
                return html }
            |> Async.Start