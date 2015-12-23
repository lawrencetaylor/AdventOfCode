namespace AdventOfCode


module DayTwelve = 

    open System.IO
    open Newtonsoft.Json.Linq
    open System.Collections.Generic
    open System

    let rec sum (seed : int) (jObj : JToken)  = 
        match jObj.Type with
            | JTokenType.Object -> let childProperties = JObject.FromObject(jObj).Children() |> Seq.cast<JProperty>
                                   match childProperties |> Seq.exists(fun p -> p.Value.Type = JTokenType.String && p.Value.Value<string>() = "red") with
                                    | true -> seed
                                    | false -> childProperties |> Seq.fold sum seed
            | JTokenType.Property -> JProperty.FromObject(jObj).Children() |> Seq.fold sum seed
            | JTokenType.Array -> JArray.FromObject(jObj) |> Seq.fold sum seed
            | JTokenType.Integer -> let integerV = jObj.Value<int>()
                                    seed + integerV
            | _ -> seed
                                    

    let letters = [97 .. 97 + 25] |> Seq.map(Convert.ToChar) |> Seq.map(Convert.ToString) |> Array.ofSeq

    let colors = ["red"; "orange"; "blue"; "purple"; "green"; "violet"; "yellow"]

    let replace replStr (str : string)   = str.Replace(replStr, " ")

    let replaceFold str repl = replace repl str

    let toQuoted (c: string) = String.Format("\"{0}\"", c)

    let splitAndAdd (str : string) = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                                        |> Seq.ofArray
                                        |> Seq.map(Int32.Parse)
                                        |> Seq.fold (+) 0



    let partOne() = let jsonStr = File.ReadAllText(Path.Combine(Common.rootDirectory, "Day12.txt"))
                    //let jsonStr = @"[1,""red"",5]"
                    let cSee = (jsonStr |> JObject.Parse) :> JToken 
                    let y = sum 0 cSee
                    
                    
                    let x = 2

                    //let jObj = JObject.Parse(jsonStr).ToObject<Dictionary<string,obj>>()
                    //let y = x jObj 0

//
//                    let result = letters |> Seq.append(colors)
//                                         |> Seq.map(toQuoted) 
//                                         |> Seq.fold replaceFold jsonStr
//                                         |> replace(":")
//                                         |> replace("\n")
//                                         |> replace("\r")
//                                         |> replace("[")
//                                         |> replace("]")
//                                         |> replace("{")
//                                         |> replace("}")
//                                         |> replace(",")
//                                         |> splitAndAdd
//
//
//
                   
                    
                    2
    

