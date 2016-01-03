namespace AntColonyNet

open AntColonyNet

module Salesman = 
    [<StructuredFormatDisplay("{AsString}")>]
    type TravelToCity(fromCity : (string * float * float), toCity : (string * float * float)) =
        let (fromName, fromX, fromY) = fromCity
        let (toName, toX, toY) = toCity
        let distance = ((fromX-toX) ** 2.0 + (fromY-toY) ** 2.0) ** 0.5

        interface Action with
            member this.Cost: float = distance
        member this.City = toCity

        override this.ToString() = toName + " " + distance.ToString()
        member this.AsString = this.ToString()

    type SalesmanAnt(journies : Map<string * float * float, Map<string * float * float, TravelToCity>>, startCity) =
        inherit Ant<TravelToCity>()
        let mutable currentCity = startCity

        member this.CitiesVisited = seq { yield startCity 
                                          for a in this.ActionsCompleted do
                                               yield a.City}

        override this.GetNextActions =
            let visitedCities = this.CitiesVisited |> Set.ofSeq
            let unvisitedCities = journies |> Map.filter (fun k v -> not (visitedCities.Contains k))

            if unvisitedCities.Count = 0 && not (currentCity = startCity) then
                // return home if we have visited everywhere
                seq {yield journies.[currentCity].[startCity]}
            else
                // return actions to visit each city we haven't yet been to
                seq {for a in unvisitedCities do
                         yield journies.[currentCity].[a.Key]}

        override this.OnAction action =
            // set the current city to be the city when went to after an action occurs
            currentCity <- action.City
            ()

    // change a list of locations to be nested dictionary of all routes between locations
    let locationsToTravelDict locations = locations 
                                            |> List.map (fun x -> 
                                                            (x, locations 
                                                                |> List.map (fun y -> (y, TravelToCity(x, y))) 
                                                                |> Map.ofList)) 
                                            |> Map.ofList

