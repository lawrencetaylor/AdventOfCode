namespace AdventOfCode

module DayTwentyTwo = 
    open FSharp.Control

    type Boss = { HitCount: int; Damage: int}
                        member x.ReduceHit(amount) = { x with HitCount = x.HitCount - amount }

    type SpellType = MagicMissile | Drain | Shield | Poison | Recharge

    type Wizard = { HitCount: int; Armour: int; Mana: int;  AmountSpent: int}
                        member x.Pay(amount) = {x with Mana = x.Mana - amount; AmountSpent = x.AmountSpent + amount}
                        member x.AddArmour(amount) = { x with Armour = x.Armour + amount}
                        member x.GetMana(amount) = {x with Mana = x.Mana + amount}
                        member x.AddHit(amount) = { x with HitCount = x.HitCount + amount}
                        //member x.AddSpellType(spellType) = { x with Spells = x.Spells |> List.append([spellType])}

    let applyEffects effects wizard boss =
            let decrementedEffects = effects |> Map.map(fun k v -> v - 1)
            let (wizard, boss) = decrementedEffects |> Map.filter(fun k v -> v >= 0)
                                        |> Map.toSeq
                                        |> Seq.fold (fun (w : Wizard, b : Boss) (spellType, expiry) -> 
                                                                    match (spellType, expiry) with
                                                                    | (Shield, 0) -> (w.AddArmour(-7),b)
                                                                    | (Shield, _) -> (w,b)
                                                                    | (Recharge, _) -> (w.GetMana(101),b)
                                                                    | (Poison, _) -> (w,b.ReduceHit(3))
                                                    ) (wizard, boss)
            let cleanedEffects = decrementedEffects |> Map.filter(fun k v -> v > 0)
            (wizard, boss, cleanedEffects)

    let wizardPlay (wizard: Wizard) (boss: Boss) (effects : Map<SpellType, int>) (spellType: SpellType) = 
        match wizard.HitCount <= 0 with 
            | true -> (wizard, boss, effects)
            | false -> let (wizard, boss ,effects ) = applyEffects effects wizard boss
                       match boss.HitCount <= 0 with
                        | true -> (wizard, boss, effects)
                        | false -> 
                                //let wizard = wizard.AddSpellType(spellType)
                                match spellType with
                                    | MagicMissile ->  (wizard.Pay(53), boss.ReduceHit(4), effects)
                                    | Drain -> (wizard.Pay(73).AddHit(2), boss.ReduceHit(2), effects)
                                    | Shield -> (wizard.Pay(113).AddArmour(7), boss, effects.Add(Shield, 6))
                                    | Poison -> (wizard.Pay(173), boss ,effects.Add(Poison, 6))
                                    | Recharge -> (wizard.Pay(229), boss ,effects.Add(Recharge, 5))

    let bossPlay (wizard: Wizard) (boss: Boss) (effects : Map<SpellType, int>) = 
        match boss.HitCount <= 0 with
            | true -> (wizard, boss, effects)
            | false ->  let (wizard, boss ,effects ) = applyEffects effects wizard boss
                        match boss.HitCount <= 0 with
                            | true -> (wizard, boss, effects)
                            | false -> let bossDeal = System.Math.Max( boss.Damage - wizard.Armour, 1)
                                       (wizard.AddHit(-bossDeal), boss, effects)

    let mutable lastMin = System.Int32.MaxValue
   
    let rec playGame (counter: int) (wizard: Wizard) (boss: Boss) (effects: Map<SpellType, int>) (nextStep: int*Wizard*Boss*Map<SpellType, int> -> seq<SpellType>) = 
        asyncSeq {
                match wizard.AmountSpent > lastMin with 
                | true -> yield None
                | false ->

                let wizard = { wizard with HitCount = wizard.HitCount - 1}
                match wizard.HitCount with
                    | d when d <=0 -> yield None
                    | _ -> 
                        match counter % 2 = 0 with
                            | true -> let nextSteps = nextStep(counter, wizard, boss, effects)
                                      match boss.HitCount <= 0 with
                                        | true ->   yield  match lastMin > wizard.AmountSpent with
                                                                | true -> lastMin <- wizard.AmountSpent
                                                                          wizard.AmountSpent |> Some
                                                                | false -> None
                                        | false ->
                                                for step in nextSteps do
                                                  let (wizard, boss, effects) = wizardPlay wizard boss effects step
                                                  match wizard.HitCount <= 0 with
                                                      | true -> yield None
                                                      | false -> yield! playGame (counter + 1) wizard boss effects nextStep
                            | false -> let (wizard, boss, effects) = bossPlay wizard boss effects
                                       match boss.HitCount <= 0 with
                                        | true -> yield  match lastMin > wizard.AmountSpent with
                                                                | true -> lastMin <- wizard.AmountSpent
                                                                          wizard.AmountSpent |> Some
                                                                | false -> None
                                        | false -> yield! playGame (counter + 1) wizard boss effects nextStep
            }


    

    let getNextSteps manaCount (effects : Map<SpellType, int>)  = 

        let canUse spellType = effects.ContainsKey(spellType) |> not || effects.[spellType] <= 1
        
        seq {
                if(manaCount < 53) then ()
                if(manaCount >= 53) then yield MagicMissile
                if(manaCount >= 73) then yield Drain
                if(manaCount >= 113 && canUse(Shield)) then yield Shield
                if(manaCount >= 173 && canUse(Poison)) then yield Poison
                if(manaCount >= 229 && canUse(Recharge)) then yield Recharge
           
        } 

    let solve() = 

                 // D22.solve() |> ignore

                  System.Console.Write("Day 22!") |> ignore
    
                  let turns = [Recharge; Shield; Drain; Poison; MagicMissile] |> Array.ofList
                  let turns = [MagicMissile;Shield;Recharge;Poison;Shield;Recharge;Poison;Shield;Recharge;Poison;Shield;MagicMissile;Poison;Drain] |> Array.ofList
                  let turns = [MagicMissile;Shield;Recharge;Poison;Shield;Recharge;Poison;Shield;Recharge;Poison;Shield;MagicMissile] |> Array.ofList

                  let player = { HitCount = 50; Armour = 0; Mana = 500; AmountSpent = 0}
                  let boss = { HitCount = 14; Damage = 8}

                  let getTurn (counter, _, _, _) = [turns.[counter/2]] |> Seq.ofList

                  let getTurnAdv (counter, wizard, boss, effects) = 
                        match counter with
                            | c when turns |> Seq.length > c/2 -> [turns.[c/2]] |> Seq.ofList
                            | _ ->  getNextSteps wizard.Mana effects

                  let getAdv (counter, wizard, boss, effects) = getNextSteps wizard.Mana effects


                  let actualBoss = {HitCount = 58; Damage = 9}
                  //let actualBoss = {HitCount = 71; Damage = 10}

                  let s = playGame 0 player actualBoss Map.empty getTurnAdv 

                  let winningGames = playGame 0 player actualBoss Map.empty getAdv |> AsyncSeq.filter(fun v -> v.IsSome)
                                                                                   |> AsyncSeq.map(fun v -> v.Value)
                                                                                   |> AsyncSeq.toArray
                                                                                   |> Array.sortBy(fun d -> d)
                                                                                   |> Seq.head

                  System.Console.Write(sprintf "Min Amt: %i" winningGames)

                  System.Console.ReadLine()
                                                                                   


                                                                      //|> AsyncSeq.sortBy(fun d -> d.Value.AmountSpent)
                                                                      //|> Seq.head

                                                                      
                  //let str = "[" + System.String.Join(";", y.Value.Spells |> Seq.map(fun s -> s.ToString())) + "]"
                  //                                                    //|> Seq.map(fun v -> v.Value.AmountSpent)
                  //                                                    //|> Seq.sortByDescending(fun d -> d)
                  //                                                    //|> Array.ofSeq
                  //
                  //let doIWin turns = play true player boss turns Map.empty
                  //
                  //let doI = doIWin turns
                  //
                  //possiblePlays 250

                  //let plays = (possiblePlays 250 List.empty) |> Seq.filter(fun f -> doIWin(f))
                  //                                           |> Array.ofSeq

                  2

   



