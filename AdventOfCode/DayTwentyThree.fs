namespace AdventOfCode

module DayTwentyThree = 

    open System.Collections.Generic
    open System.IO

    type Register = A | B
    type RegisterInstruction = Half of Register
                               | Triple of Register
                               | Increment of Register
                               | Jump of int
                               | JumpIfEven of Register*int
                               | JumpIfOne of Register*int

    let format (str : string) = str.Trim().TrimEnd(',')

    let parseline (str: string) = let splitted = str.Split([|' '; ','|], System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(format) |> Array.ofSeq
                                  match splitted with
                                    | [|"jmp"; offset|] ->  Jump (offset |> System.Int32.Parse)
                                    | [|instruction; register; offset|] ->let reg = match register with  | "a" -> A | "b" -> B
                                                                          let jump = offset |> System.Int32.Parse
                                                                          match instruction with
                                                                            | "jie" -> JumpIfEven (reg, jump)
                                                                            | "jio" -> JumpIfOne (reg, jump)
                                    | [|instruction; register|] -> let reg = match register with  | "a" -> A | "b" -> B
                                                                   match instruction with
                                                                            | "hlf" -> Half reg
                                                                            | "tpl" -> Triple reg
                                                                            | "inc" -> Increment reg

    let instructions = File.ReadAllLines(Path.Combine(Common.rootDirectory, "Day23.txt")) 
                                    |> Seq.map(parseline)
                                    |> Seq.toArray

    

    let registerValues = new Dictionary<Register, int>()
    registerValues.Add(A,1)
    registerValues.Add(B,0)


    let add x y = x + y
    let times x y = x * y 
    let divide x y = y / x

    let log(instruction)  = System.Console.WriteLine(sprintf "A = %i, B = %i : %s" registerValues.[A] registerValues.[B] instruction)

    let rec runProgram instructionPointer (registerValues  : Dictionary<Register, int> ) = 
        if instructionPointer >= instructions.Length then (registerValues.[A], registerValues.[B])
        else
        match instructions.[instructionPointer] with
            | Half r ->  registerValues.[r] <- registerValues.[r] |> divide(2)
                         log(sprintf "hlf %A [%i]" r instructionPointer)
                         runProgram (instructionPointer |> add(1)) registerValues
            | Triple r -> registerValues.[r] <- registerValues.[r] |> times(3)
                          log(sprintf "tpl %A [%i]" r instructionPointer)
                          runProgram (instructionPointer |> add(1)) registerValues
            | Increment r -> registerValues.[r] <- registerValues.[r] |> add(1)
                             log(sprintf "inc %A [%i]" r instructionPointer)
                             runProgram (instructionPointer |> add(1)) registerValues
            | Jump i -> log(sprintf "jmp %i [%i]" i instructionPointer)
                        runProgram (instructionPointer |> add(i)) registerValues
            | JumpIfEven (r,i) ->  let jumpValue = if registerValues.[r] % 2 = 0 then i else 1
                                   log(sprintf "jie %A %i [%i]" r i instructionPointer)
                                   runProgram (instructionPointer |> add(jumpValue)) registerValues
            | JumpIfOne (r,i) ->  let jumpValue = if registerValues.[r] = 1 then i else 1
                                  log(sprintf "jio %A %i [%i]" r i instructionPointer)
                                  runProgram (instructionPointer |> add(jumpValue)) registerValues



                                     

    let solve() = 


                  let instructionPointer = 0
                  let (a, b) = runProgram 0 registerValues
                  2

