namespace AdventOfCode

module DayTwentyOne = 

    type Player = Player | Boss
    type ItemType = Weapon | Armour | Rings
    type player = { Type : Player; HitPoints: int; Damage: int; Armour : int}
    type item = { Name: string; Cost: int; Damage: int; Armour: int; Type : ItemType}

    let itemsToPlayer list = 
        let damage = list |> Seq.map(fun l -> l.Damage) |> Seq.fold (+) 0
        let armour = list |> Seq.map(fun l -> l.Armour) |> Seq.fold (+) 0
        { Type = Player; HitPoints = 100; Damage = damage; Armour = armour }

    let weapons = [{ Type=Weapon; Name = "Dagger"; Cost = 8; Damage = 4; Armour = 0}
                   { Type=Weapon; Name = "Shortsword"; Cost = 10; Damage = 5; Armour = 0}
                   { Type=Weapon; Name = "Warhammer"; Cost = 25; Damage = 6; Armour = 0}
                   { Type=Weapon; Name = "Longsword"; Cost = 40; Damage = 7; Armour = 0}
                   { Type=Weapon; Name = "Greataxe"; Cost = 74; Damage = 8; Armour = 0}]

    let armour =  [{ Type=Armour; Name = "Leather"; Cost = 13; Damage = 0; Armour = 1}
                   { Type=Armour; Name = "Chainmail"; Cost = 31; Damage = 0; Armour = 2}
                   { Type=Armour; Name = "Splintmail"; Cost = 53; Damage = 0; Armour = 3}
                   { Type=Armour; Name = "Bandedmail"; Cost = 75; Damage = 0; Armour = 4}
                   { Type=Armour; Name = "Platemail"; Cost = 102; Damage = 0; Armour = 5}
                   ]

    let rings  =  [{ Type=Rings; Name = "Damage +1"; Cost = 25; Damage = 1; Armour = 0}
                   { Type=Rings; Name = "Damage +2"; Cost = 50; Damage = 2; Armour = 0}
                   { Type=Rings; Name = "Damage +3"; Cost = 100; Damage = 3; Armour = 0}
                   { Type=Rings; Name = "Defense +1"; Cost = 20; Damage = 0; Armour = 1}
                   { Type=Rings; Name = "Defense +2"; Cost = 40; Damage = 0; Armour = 2}
                   { Type=Rings; Name = "Defense +3"; Cost = 80; Damage = 0; Armour = 3}]

    let nonWeapons = [armour;rings] |> List.concat

    
    let rec armourOptions (currentOptions : Set<item>) remainingOptions totalNumber  = 
        seq { 
            match totalNumber with
                | 0 -> yield []
                | d -> for r in remainingOptions do
                         let newOptions = currentOptions |> Set.add(r)
                         let newRemaining = remainingOptions |> Set.remove(r)
                         let newTotal = totalNumber - 1
                         let items = (armourOptions newOptions newRemaining newTotal )
                         for i in items do
                            yield r::i
            }

    let armourPossibilities = 
        seq { for i in [0 .. 1] do
                yield! armourOptions Set.empty (armour |> Set.ofList) i
        }

    let ringPossibilities = 
        seq { for i in [0 .. 2] do
                yield! armourOptions Set.empty (rings |> Set.ofList) i
        
        }



    let buyingOptions = 
     seq {  for w in weapons do
               for a in armourPossibilities do
                 for r in ringPossibilities do
                    yield [[w]; a; r] |> List.concat
     }

    let rec play (attacker : player) (defender : player) = 
            match attacker.HitPoints with
                | p when p <= 0 -> defender
                | _ -> let deal = System.Math.Max( attacker.Damage - defender.Armour, 1)
                       play {defender with HitPoints = defender.HitPoints - deal} attacker

        

    let solve() =  let playerInitial = { player.Type = Player.Player; HitPoints = 8; Damage = 5; Armour = 5 }
                   let bossInitial = { player.Type = Player.Boss; HitPoints = 12; Damage = 7; Armour = 2 }
                   let winner = play playerInitial bossInitial

                   let z = buyingOptions |> Array.ofSeq

                   let properBoss = { player.Type = Player.Boss; HitPoints = 103; Damage = 9; Armour = 2 }

                   let isWinner boss player  = (play player boss ).Type = Player

                   let testWinner = isWinner playerInitial bossInitial

                   let pricedWeapons = buyingOptions |> Seq.map(fun l -> (l |> Seq.map(fun i -> i.Cost) |> Seq.fold (+) 0, l))
                                         |> Seq.map(fun (price, list) -> (list |> itemsToPlayer, (price, list)))

                   let partOne = pricedWeapons |> Seq.filter(fst >> isWinner(properBoss))
                                               |> Seq.map(snd >> fst)
                                               |> Seq.sortBy(fun i -> i)
                                               |> Seq.head

                   let partTwo = pricedWeapons |> Seq.filter(fst >> isWinner(properBoss) >> not) 
                                               |> Seq.map(snd >> fst)
                                               |> Seq.sortByDescending(fun i -> i)
                                               |> Seq.head

                   

                   
                   2

                   

