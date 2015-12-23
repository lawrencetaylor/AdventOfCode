﻿
namespace AdventOfCode

module D22 = 

    type Spell = 
        { Name : string
          Cost : int
          Damage : int
          Heal : int
          Armor : int
          Mana : int
          Duration : int }

    let allSpells = 
        [ { Name = "Magic Missile"; Cost = 53;  Damage = 4; Heal = 0; Armor = 0; Mana = 0;   Duration = 1}
          { Name = "Drain";         Cost = 73;  Damage = 2; Heal = 2; Armor = 0; Mana = 0;   Duration = 1}
          { Name = "Shield";        Cost = 113; Damage = 0; Heal = 0; Armor = 7; Mana = 0;   Duration = 6}
          { Name = "Poison";        Cost = 173; Damage = 3; Heal = 0; Armor = 0; Mana = 0;   Duration = 6}
          { Name = "Recharge";      Cost = 229; Damage = 0; Heal = 0; Armor = 0; Mana = 101; Duration = 5} ]

    let rec play part myTurn spent (hp, mana, spells) (bossHp, bossDamage) map : seq<int option> =

        // part 2, check if I die before any effects play out
        if part = 2 && myTurn && hp = 1 then upcast [None] else

        // apply effects
        let mana = (spells |> List.sumBy (fun s -> s.Mana)) + mana
        let damage = spells |> List.sumBy (fun s -> s.Damage)
        let armor = spells |> List.sumBy (fun s -> s.Armor)

        // does the boss die from effects?
        let bossHp = bossHp - damage
        if bossHp <= 0 then 
            let calcCost = map |> List.map(fst) |> List.sumBy(fun m -> m.Cost)
            let testS = System.String.Join(";", map |> List.map(fst) |> List.rev |> List.map(fun m -> m.Name))
            let checkList = System.String.Join("\n", map |>List.map(snd))
            upcast [Some(spent)] 
        else

        // decrement duration of all effects, and groom expired ones
        let spells =
            spells
            |> List.map (fun s -> {s with Duration = s.Duration - 1})
            |> List.filter (fun s -> s.Duration > 0)

        if myTurn then
            // part 2, I lose 1 HP on my turn
            let hp = if part = 2 then hp - 1 else hp

            // what spells can I afford and don't already have running?
            match allSpells |> List.filter (fun s -> (s.Cost <= mana) && not (spells |> List.exists (fun s' -> s.Name = s'.Name))) with
            | [] -> upcast [None]
            | buyableSpells ->
                // play out the rest of the game with each possible purchase
                seq { for s in buyableSpells do
                          let tuple = (s, sprintf "Mana %i; Hit: %i; Spent: %i" mana hp spent)
                          let map = match spent with 
                                                | 0 -> [tuple]
                                                | d -> map |> List.append([tuple])
                          let spent = spent + s.Cost
                          let mana = mana - s.Cost
                          let extraDamage, heal, spells = 
                              if s.Duration = 1 then s.Damage, s.Heal, spells
                              else 0, 0, s :: spells

                          let bossHp = bossHp - extraDamage
                          if bossHp <= 0 then
                              yield Some(spent)
                          else
                              yield! play part false spent (hp + heal, mana, spells) (bossHp, bossDamage) map }

        // boss's turn
        else
            let damage = max (bossDamage - armor) 1
            let hp = hp - damage
            if hp <= 0 then upcast [None] else
            play part true spent (hp, mana, spells) (bossHp, bossDamage) map

    let solve() = 
        play 1 true 0 (50, 500, []) (71, 10) List.empty |> Seq.choose id |> Seq.min |> printfn "Part 1 - min to win: %d"
        play 2 true 0 (50, 500, []) (71, 10) List.empty |> Seq.choose id |> Seq.min |> printfn "Part 1 - min to win: %d"
        2
