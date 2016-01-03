module TrailSystemTests

open NUnit.Framework
open FsUnit
open AntColonyNet

type TestAction(value, cost)=
    interface Action with
        member this.Cost: float = cost
    member this.Value = value

type TestAnt(action) as this=
    inherit Ant<TestAction>()
        do this.ApplyAction action
        override this.GetNextActions = if this.ActionsCompleted |> Seq.exists ((=) action) then
                                            Seq.empty
                                       else
                                            seq { yield action}
        override this.OnAction action = 
            ()

[<Test>]
let ``Good results increase probability of trail being selected`` () =
    let ts = AntColonyTrailSystem<TestAction>(1.0, 1.0, 1.0, 0.01) :> TrailSystem<TestAction>
    let actions = [TestAction('A', 10.0); TestAction('B', 10.0)]
    
    Assert.AreEqual(actions.Head, actions.Head)
    Assert.AreEqual((ts.ActionProbabilityDensity actions.[0]), (ts.ActionProbabilityDensity actions.[1]))

    let startingProbability = (ts.ActionProbabilityDensity actions.Head)

    ts.LayTrails [TestAnt(actions.Head)]

    let probabilityAfter1Journey = (ts.ActionProbabilityDensity actions.Head)
    
    Assert.Greater(probabilityAfter1Journey, startingProbability)

    ts.LayTrails [TestAnt(actions.[1]); TestAnt(actions.[1]); TestAnt(actions.[1])]

    let probabilityAfter3Journey = (ts.ActionProbabilityDensity actions.[1])

    Assert.Greater(probabilityAfter3Journey, probabilityAfter1Journey)