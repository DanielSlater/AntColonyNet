module TrailSystemTests

open NUnit.Framework
open FsUnit
open AntColonyNet

type TestAction(value, cost)=
    interface Action with
        member this.Cost: float = cost
    member this.Value = value

[<Test>]
let ``Good results increase probability of trail being selected`` () =
    let ts = AntSystemTrailSystem<TestAction>(1.0, 1.0, 1.0, 1.0) :> TrailSystem<TestAction>
    let actions = [TestAction('A', 1.0); TestAction('B', 1.0)]
    
    Assert.AreEqual(actions.Head, actions.Head)
    Assert.AreEqual((ts.ActionProbabilityDensity actions.[0]), (ts.ActionProbabilityDensity actions.[1]))

    let prob1 = (ts.ActionProbabilityDensity actions.Head)

    ts.LayTrail [actions.Head] 0.001

    let prob2 = (ts.ActionProbabilityDensity actions.Head)
    
    Assert.Greater(prob2, prob1)

    ts.LayTrail [actions.[1]] 100000.0

    let prob3 = (ts.ActionProbabilityDensity actions.[1])

    Assert.Less(prob3, prob1)