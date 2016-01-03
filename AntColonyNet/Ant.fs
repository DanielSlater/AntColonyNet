namespace AntColonyNet

[<AbstractClass>]
type Ant<'Action when 'Action :> Action and 'Action : equality>() = 
    let mutable cost = 0.0
    let actions = new ResizeArray<'Action>()
    
    abstract member GetNextActions: seq<'Action> with get
    abstract member OnAction : 'Action -> unit

    member this.Cost with get() = cost
    member this.ActionsCompleted with get() : seq<'Action> = seq {for a in actions do
                                                                        yield a}
    member this.ApplyAction (transition : 'Action) : unit =
        this.OnAction transition
        cost <- cost + transition.Cost
        actions.Add(transition)
