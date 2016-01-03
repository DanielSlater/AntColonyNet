namespace AntColonyNet
open FSharp.Collections.ParallelSeq

module AntColony =
    let rand = new System.Random()

    let rec selectRandomItemFromListWithWeightedProbabilityWithRand (items : ('t * float) list) (value : float) : 't =
        match items with
            | (item, prob) :: xs -> if value <= prob then
                                        item
                                    else
                                        selectRandomItemFromListWithWeightedProbabilityWithRand xs (value-prob)
            | _ -> failwith "broken"

    let selectRandomItemFromListWithWeightedProbability (items : ('t * float) list) =
        let prob = rand.NextDouble()
        selectRandomItemFromListWithWeightedProbabilityWithRand items prob

    let chooseNextAction (actions : list<'Action>) (getActionProbabilityDensity: 'Action -> float) : 'Action =
        if actions.Length = 1 then
            // only 1 action lets save some time
            actions.[0]
        else
            let actionProbabilityDensities = actions |> List.map (fun x -> (x, (getActionProbabilityDensity x)))
            let sumOfProbabilities = actionProbabilityDensities |> List.sumBy (fun x -> snd x)
            let actionProbabilities : ('Action * float) list 
                                    = actionProbabilityDensities 
                                                |> Seq.map (fun (action, probDensity) -> 
                                                                let prob = probDensity / (sumOfProbabilities - probDensity)
                                                                (action, prob))
                                                |> Seq.toList

            selectRandomItemFromListWithWeightedProbability actionProbabilities

    let rec runAnt (ant : Ant<'Action>) (actionProbability : 'Action -> float ): unit =
        let actions = ant.GetNextActions |> Seq.toList
        if actions.Length > 0 then
            let choice = chooseNextAction actions actionProbability
            ant.ApplyAction choice
            runAnt ant actionProbability
    
    let OptimizeCSharp<'Action when 'Action :> Action and 'Action : equality> 
            (newAnt : System.Func<Ant<'Action>>) 
            (maxIterations : int) 
            (antsPerIteration : int)
            (trailSystem : TrailSystem<'Action>) : 'Action seq =

        let mutable bestAnt = newAnt.Invoke()
        runAnt bestAnt trailSystem.ActionProbabilityDensity
        let mutable iterations = 0
        
        while iterations < maxIterations do
            let ants = seq { for i in 0 .. antsPerIteration do yield i }
                        |> PSeq.map (fun x -> let ant = newAnt.Invoke()
                                              runAnt ant trailSystem.ActionProbabilityDensity
                                              ant)

            for ant in ants do
                if ant.Cost < bestAnt.Cost then
                    printfn "Got new best iterations %i cost %f" iterations bestAnt.Cost
                    bestAnt <- ant

            trailSystem.LayTrails ants

            iterations <- iterations + 1

        bestAnt.ActionsCompleted

    let Optimize<'Action when 'Action :> Action and 'Action : equality> 
            (newAnt : unit -> Ant<'Action>) 
            (maxIterations : int) 
            (antsPerIteration : int)
            (trailSystem : TrailSystem<'Action>) : 'Action seq =
        let func = System.Func<Ant<'Action>>(newAnt)
        OptimizeCSharp<'Action> func maxIterations antsPerIteration trailSystem