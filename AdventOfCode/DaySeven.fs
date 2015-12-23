namespace AdventOfCode

module DaySeven = 

    open System
    open System.Collections.Generic


    type Variable = string
    type Value = int
    type Arg = Variable of Variable | Value of Value
                member x.GetVariableName = match x with
                                             | Variable n -> n
                                             | _ -> failwith "Not a variable"
    type Operators = AND of Arg*Arg
                     | OR of Arg*Arg
                     | RSHIFT of Arg*Arg
                     | LSHIFT of Arg*Arg
                     | NOT of Arg

    type Node = { Operator: string option 
                  Dependencies: string Set
                  Assignment: string
                  Index : int
                  Source: string}

    let binaryOperators = ["RSHIFT"; "AND"; "OR"; "LSHIFT"]
    let unaryOperators = ["NOT"]

    let toArg s = let mutable value = 0
                  match Int32.TryParse(s, &value) with
                    | true -> Value(value)
                    | false ->  Variable(s)

    let createBinaryOperator(name , a1 : int, a2 : int) = 
            match name with
                | "AND" -> a1 &&& a2
                | "OR" -> a1 ||| a2
                | "RSHIFT" -> a1 >>> a2
                | "LSHIFT" -> a1 <<< a2

    let creatUnaryOperator(name, a1) =
            match name with
               | "NOT" -> match -a1 with
                            | negative when negative < 0 -> negative + 65535
                            | postive -> postive


    let isBinaryOperatorString (str: string) = match binaryOperators |> Seq.exists(fun o -> str.Contains(o)) with
                                                | true -> binaryOperators |> Seq.filter(fun o -> str.Contains(o)) |> Seq.exactlyOne |> Some
                                                | false -> None


    let isUnaryOperatorString (str: string) = match unaryOperators |> Seq.exists(fun o -> str.Contains(o)) with
                                                | true -> unaryOperators |> Seq.filter(fun o -> str.Contains(o)) |> Seq.exactlyOne |> Some
                                                | false -> None

    let trim (s: string) = s.Trim()


    let parseStr (map: Map<string,int>) (str: string) = 
                                 let parts = str.Replace("->", "#").Split('#') |> Seq.map(trim) |> Array.ofSeq
                                 let source = parts.[0]
                                 let target = parts.[1].Trim()

                                 let getValue arg = match arg with
                                                     | Value v -> v
                                                     | Variable s -> map.[s.Trim()]

                                 let valueToAssign =  match source.Trim().IndexOf(' ') with
                                                        | -1 -> source |> toArg |> getValue
                                                        | _ -> match source |> isBinaryOperatorString with
                                                                | Some oName -> let split = source.Replace(oName, ",").Split(',') |> Seq.map(fun s -> s.Trim()) |> Array.ofSeq
                                                                                let arg1 = split.[0] |> toArg |> getValue
                                                                                let arg2 = split.[1] |> toArg |> getValue
                                                                                createBinaryOperator(oName, arg1, arg2)
                                                                | None -> match source |> isUnaryOperatorString with
                                                                            | Some oName -> let str = source.Replace(oName, "") |> toArg |> getValue
                                                                                            creatUnaryOperator(oName, str)
                                                                            | None -> str |> toArg |> getValue

                                 match map.ContainsKey(target) with
                                    | true -> map.[target] = valueToAssign |> ignore
                                              map
                                    | false -> map.Add(target, valueToAssign)

    let decompose (index: int, str: string) =
                                 let parts = str.Replace("->", "#").Split('#')
                                 let source = parts.[0]
                                 let target = parts.[1].Trim()

                                 let isSymbol arg = match arg with
                                                     | Variable v -> true
                                                     | _ -> false

                                 match source.Trim().IndexOf(' ') with
                                                        | -1 -> { Node.Operator = None; Dependencies = [source |> trim |> toArg] |> Seq.filter(isSymbol)
                                                                                                                                       |> Seq.map(fun v -> v.GetVariableName) |> Set.ofSeq ; Index = index ; Assignment = target; Source = str }
                                                        | _ -> match source |> isBinaryOperatorString with
                                                                | Some oName -> let split = source.Replace(oName, ",").Split(',') |> Seq.map(trim) |> Array.ofSeq
                                                                                let arg1 = split.[0] |> toArg 
                                                                                let arg2 = split.[1] |> toArg 
                                                                                { Node.Operator = oName |> Some; Node.Dependencies = [arg1; arg2] |> Seq.filter(isSymbol)
                                                                                                                                        |> Seq.map(fun v -> v.GetVariableName) |> Set.ofSeq  ; Index = index ; Assignment = target ; Source = str }
                                                                | None -> match source |> isUnaryOperatorString with
                                                                            | Some oName -> let str1 = source.Replace(oName, "") |> trim |> toArg
                                                                                            { Node.Operator = oName |> Some; Node.Dependencies = [str1] |> Seq.filter(isSymbol)
                                                                                                                                       |> Seq.map(fun v -> v.GetVariableName) |> Set.ofSeq  ; Index = index ; Assignment = target ; Source = str }
                                                                            | None -> 
                                                                                      { Node.Operator = None; Dependencies = [str |> trim |> toArg] |> Seq.filter(isSymbol)
                                                                                                                                       |> Seq.map(fun v -> v.GetVariableName) |> Set.ofSeq ; Index = index ; Assignment = target ; Source = str }





    

    let solution() = let lines = System.IO.File.ReadAllLines(@"../../../AdventOfCode/Day07.txt")

                     //let lines = [  
                     //               "456 -> y            ";
                     //               "NOT y -> i          ";
                     //               "x AND y -> d        ";
                     //               "y RSHIFT 2 -> g     ";
                     //               "x OR y -> e         ";
                     //               "x LSHIFT 2 -> f     ";
                     //               "123 -> x            ";
                     //               "NOT x -> h          "]

                     let y = lines |> Seq.indexed |> Seq.map(decompose) |> Seq.toArray

                     let iterate variables  = let mappedVariabled = variables |> Map.toSeq |> Seq.map fst |> Set.ofSeq
                                              let y2 = y |> Seq.filter(fun n -> n.Dependencies |> Set.intersect(mappedVariabled) = n.Dependencies) |> Array.ofSeq
                                              let variables = y2 |> Seq.map(fun n -> y.[n.Index].Source) |> Seq.fold parseStr variables
                                              variables

                     

                     let  variables = Map.empty
                     let y0 = y |> Seq.filter(fun n -> n.Dependencies |> Seq.isEmpty) |> Array.ofSeq
                     let mutable variables = y0 |> Seq.map(fun n -> y.[n.Index].Source) |> Seq.fold parseStr variables
                     variables.["b"] = 16076 |> ignore

                     let list = [1 .. 1000]

                     for i in list do
                        variables <- iterate variables

                     let aValue = variables.["a"]
                     let mutable variables2 = y0 |> Seq.map(fun n -> y.[n.Index].Source) |> Seq.fold parseStr variables

                     variables2.Add("b", aValue)

                     for i in list do
                        variables2 <- iterate variables2
  

                     


                     //let mappedVariabled = variables |> Map.toSeq |> Seq.map fst |> Set.ofSeq
                     //let y1 = y |> Seq.filter(fun n -> n.Dependencies |> Set.intersect(mappedVariabled) = n.Dependencies) |> Array.ofSeq
                     //let variables = y1 |> Seq.map(fun n -> y.[n.Index].Source) |> Seq.fold parseStr variables

  






                     let stack = Stack<int>()

                     let getDepndency symbol = 
                        let rowThatAssignsThsiVariable = y |> Seq.filter(fun n -> n.Assignment = symbol) |> Seq.exactlyOne
                        rowThatAssignsThsiVariable.Dependencies

                     let setResolved = System.Collections.Generic.List<string>()
                     let mutable x = "a"
                     let mutable unresolvedDependencies = Queue<string>()
                     unresolvedDependencies.Enqueue(x)



                     let pendingUnResolved unresolved = ((unresolved |> Set.ofSeq) - (setResolved |> Set.ofSeq)) |> Seq.isEmpty |> not
                     
                     while(pendingUnResolved(unresolvedDependencies)) do
                        let dependencyToResolve = unresolvedDependencies.Dequeue()
                        let node = y |> Seq.filter(fun n -> n.Assignment = dependencyToResolve) |> Seq.exactlyOne
                        stack.Push(node.Index)
                        setResolved.Add(node.Assignment)
                        for d in node.Dependencies do
                            unresolvedDependencies.Enqueue(d)
                     
                     let y = lines |> Seq.indexed |> Map.ofSeq

                     let mutable variables = Map.empty

                     while(stack |> Seq.isEmpty |> not) do
                         let lineToExecute = y.[stack.Pop()]
                         variables <- parseStr(variables)(lineToExecute)

                     2
                     

