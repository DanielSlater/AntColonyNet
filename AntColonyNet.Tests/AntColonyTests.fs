module AntColonyNet.Tests

open NUnit.Framework
open FsUnit
open AntColony
open Salesman

[<Test>]
let ``cumulative probability`` () =
    let result = AntColony.selectRandomItemFromListWithWeightedProbabilityWithRand [('a',0.25); ('b', 0.25); ('c', 0.25); ('d', 0.25)]
    Assert.AreEqual('d', result 0.9)
    Assert.AreEqual('a', result 0.1)
        
[<Test>]
let ``straight line has clear best answer`` () =
    let cities = [("A", 0.0, 1.0);
                    ("B", 1.0, 0.0);
                    ("C", 2.0, 0.0);
                    ("D", 3.0, 0.0);
                    ("E", 4.0, 0.0);
                    ("F", 5.0, 0.0);
                    ("G", 6.0, 0.0);
                    ("H", 7.0, 0.0);
                    ("I", 8.0, 1.0);]

    let travelDict = locationsToTravelDict cities
    let startCity = cities |> Seq.head
    let antFunc (_ : unit) =
        SalesmanAnt(travelDict, startCity) :> Ant<TravelToCity>
    let trailSystem = AntColonyTrailSystem<TravelToCity>(1.5, 1.0, 0.5, 0.1)
    let result = Optimize antFunc 50 50 trailSystem |> Seq.toList
    printfn "%A" result
    for i in 0..(cities.Length-2) do
        Assert.AreEqual(result.[i].City, cities.[i+1])
    

[<Test>]
let ``travelling salesman`` () =
    let cities = [("London", 51.0, 0.0);
                    ("Paris", 48.0, 2.0);
                    ("T'bilisi", 41.0, 44.0);
                    ("Berlin", 52.0, 13.0);
                    ("Athens", 37.0, 23.0);
                    ("Budapest", 47.0, 19.0);
                    ("Rome", 41.0, 12.0);
                    ("Riga", 56.0, 24.0);
                    ("Luxembourg", 49.0, 6.0);
                    ("Oslo", 59.0, 10.0);]

    let travelDict = locationsToTravelDict cities
    let startCity = cities |> Seq.head
    let antFunc (_ : unit) =
        SalesmanAnt(travelDict, startCity) :> Ant<TravelToCity>
    let trailSystem = AntColonyTrailSystem<TravelToCity>(1.0, 1.0, 1.0, 1.0)
    let result = Optimize antFunc 50 50 trailSystem
    printfn "%A" result
    ()
    
    