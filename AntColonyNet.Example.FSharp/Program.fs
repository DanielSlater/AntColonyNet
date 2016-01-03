open AntColonyNet
open AntColonyNet.Salesman
        

type NullTrailSystem() =
    interface TrailSystem<TravelToCity> with
        member this.ActionProbabilityDensity (action : TravelToCity) =
            1.0

        member this.LayTrails ants =
            ()

type CostOnlyTrailSystem() =
    interface TrailSystem<TravelToCity> with
        member this.ActionProbabilityDensity (action : TravelToCity) =
            1.0/(action :> Action).Cost

        member this.LayTrails ants =
            ()

[<EntryPoint>]
let main argv = 
    let cities = [
                ("London", 51.0, 0.0);
                ("Paris", 48.0, 2.0);
                ("T'bilisi", 41.0, 44.0);
                ("Berlin", 52.0, 13.0);
                ("Athens", 37.0, 23.0);
                ("Budapest", 47.0, 19.0);
                ("Rome", 41.0, 12.0);
                ("Riga", 56.0, 24.0);
                ("Luxembourg", 49.0, 6.0);
                ("Oslo", 59.0, 10.0);
                ("Kabul", 34.0, 69.0);
                ("Belgrade", 44.0, 20.0);
                ("Tunis", 36.0, 10.0);
                ("Ankara", 38.0, 57.0);
                ("Bern", 46.0, 7.0);
                ("Madrid", 40.0, 3.0);
                ("Bratislava", 48.0, 17.0);
                ("Lisbon", 38.0, 9.0);
                ("Stockholm", 59.0, 18.0);
                ("Amsterdam", 52.0, 4.0);
                ("Vilnius", 54.0, 25.0);
                ("Dublin", 53.0, 6.0);
                ("Jerusalem", 31.0, 35.0);
                ("Reykjavik", 64.0, 21.0);
                ("Helsinki", 60.0, 25.0);
                ("Prague", 50.0, 14.0);
                ("Copenhagen", 55.0, 12.0);
                ("Zagreb", 45.0, 15.0);
                ("Sophie", 42.0, 23.0);
                ("Brussels", 50.0, 4.0)]


    let travelDict = locationsToTravelDict cities

    let createAnt _ = SalesmanAnt(travelDict, cities.[0]) :> Ant<TravelToCity>

    let runColony (trailSystem : TrailSystem<TravelToCity>) action =
        let result = AntColony.Optimize action 50 50 trailSystem
        for x in result do
            printfn "%A" x

    printfn "Running completely random ant paths"
    let x = NullTrailSystem()
    runColony x createAnt
    printfn "Running ant paths based only on local distance"
    let y = CostOnlyTrailSystem()
    runColony y createAnt
    printfn "Running ant paths with trails"
    let z = AntColonyTrailSystem<TravelToCity>(1.5, 1.0, 0.5, 0.01)
    runColony z createAnt
    printfn "Running ant paths with trails"
    let z = MaxMinAntColonyTrailSystem<TravelToCity>(1.5, 1.0, 0.5, 0.01, 0.5)
    runColony z createAnt
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
