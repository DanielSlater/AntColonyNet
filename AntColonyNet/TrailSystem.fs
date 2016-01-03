namespace AntColonyNet

type TrailSystem<'Action when 'Action :> Action and 'Action : equality> =
    abstract member ActionProbabilityDensity : 'Action -> float
    abstract member LayTrails : Ant<'Action> seq -> unit

type AntColonyTrailSystem<'Action when 'Action :> Action and 'Action : equality> 
        (trailStrength : float, heuristicStrength : float, trailDecay : float, initailTrial : float) =
    let probabilityDensitiesDict = new System.Collections.Generic.Dictionary<'Action, float>()
    let TrailsDict = new System.Collections.Generic.Dictionary<'Action, float>()

    let trailScore action =
        let mutable value = initailTrial
        TrailsDict.TryGetValue(action, ref value) |> ignore
        value
        
    let addOrUpdateDict (dict : System.Collections.Generic.Dictionary<'K, 'V>) key (valueFunc : 'V -> 'V) defaultValue =
        if dict.ContainsKey(key) then
            let result = valueFunc dict.[key]
            dict.[key] <- result
            result
        else
            let result = valueFunc defaultValue
            dict.Add(key, result)
            result

    let calculateActionProbabilityDensity (trailScore : float) (actionCost : float) : float =
        let actionScore = 1.0/actionCost
        (trailScore ** trailStrength) * (actionScore ** heuristicStrength)

    abstract member UpdateTrail : float -> float -> float
    default this.UpdateTrail (newTrails : float) oldTrail =
        (1.0-trailDecay)*oldTrail+newTrails

    interface TrailSystem<'Action> with
        member this.ActionProbabilityDensity (action : 'Action) =
            if probabilityDensitiesDict.ContainsKey(action) = false then
                lock probabilityDensitiesDict (fun () ->
                    let pd = calculateActionProbabilityDensity (trailScore action) action.Cost
                    probabilityDensitiesDict.[action] <- pd)
                
            probabilityDensitiesDict.[action]

        member this.LayTrails (ants : Ant<'Action> seq) =
            let sumOfTrailScores = ants |> Seq.collect (fun a -> 
                                            let trailScore = 1.0/a.Cost
                                            a.ActionsCompleted |> Seq.map (fun x -> (x, trailScore)))
                                        |> Seq.groupBy fst
                                        |> Seq.map (fun (key, values) -> (key, values |> Seq.sumBy snd))
            for (action, value) in sumOfTrailScores do
                let updateTrail = this.UpdateTrail value
                let trailScore = addOrUpdateDict TrailsDict action updateTrail initailTrial
                addOrUpdateDict probabilityDensitiesDict action (fun x-> calculateActionProbabilityDensity trailScore action.Cost) 0.0 |> ignore

type MaxMinAntColonyTrailSystem<'Action when 'Action :> Action and 'Action : equality> 
        (trailStrength : float, heuristicStrength : float, trailDecay : float, min : float, max : float) =
        inherit AntColonyTrailSystem<'Action>(trailStrength, heuristicStrength, trailDecay, max)

            // constrain trail to be between min and max
            override this.UpdateTrail newTrail oldTrail =
                let x = (1.0-trailDecay)*oldTrail+newTrail
                if x <= min then
                    min
                elif x >= max then
                    max
                else
                    x

//TODO: Implement Ant Colony System and ANTS versions described here http://people.idsia.ch/~luca/aco2004.pdf