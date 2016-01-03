using System;
using System.Collections.Generic;
using System.Linq;

namespace AntColonyNet.Example.CSharp
{
    class City
    {
        public City(string name, double x, double y)
        {
            Y = y;
            X = x;
            Name = name;
        }

        public string Name { get; private set; }
        public double X { get; private set; }
        public double Y { get; private set; }
    }

    class TravelToCity : Action
    {
        private readonly City to;
        private readonly City @from;

        public TravelToCity(City from, City to)
        {
            this.to = to;
            this.@from = @from;
            var diffX = (from.X - to.X);
            var diffY = (from.Y - to.Y);
            Cost = Math.Sqrt(diffX * diffX + diffY * diffY);
        }

        public City Destination { get { return to; } }

        public double Cost { get; private set; }

        public override bool Equals(object obj)
        {
            var other = obj as TravelToCity;
            if (other == null)
                return false;

            return other.to == to && other.@from == @from;
        }

        public override int GetHashCode()
        {
            unchecked 
            { 
                return to.GetHashCode() + 23 * @from.GetHashCode(); 
            }
        }
    }

    class SalesmanAnt : Ant<TravelToCity>
    {
        private City currentCity;
        private readonly City startCity;
        private readonly Dictionary<City, Dictionary<City, TravelToCity>> allJournies;

        public SalesmanAnt(City startCity, Dictionary<City, Dictionary<City, TravelToCity>> allJournies)
        {
            currentCity = startCity;
            this.startCity = startCity;
            this.allJournies = allJournies;
        }

        public override void OnAction(TravelToCity newCity)
        {
            currentCity = newCity.Destination;
        }

        public IEnumerable<City> CitiesVisited
        {
            get
            {
                yield return startCity;
                foreach (var travel in ActionsCompleted)
                {
                    yield return travel.Destination;
                }
            }
        }

        public override IEnumerable<TravelToCity> GetNextActions
        {
            get
            {
                var visited = new HashSet<City>(CitiesVisited);
                var unvisitedCities = allJournies.Keys.Where(x => !visited.Contains(x)).ToList();

                if (unvisitedCities.Count == 0 && currentCity != startCity)
                    yield return allJournies[currentCity][startCity];
                foreach (var unvisitedCity in unvisitedCities)
                    yield return allJournies[currentCity][unvisitedCity];
            }
        }
    }

    class NullTrailSystem : TrailSystem<TravelToCity>
    {
        public double ActionProbabilityDensity(TravelToCity journey)
        {
            return 1.0;
        }

        public void LayTrails(IEnumerable<Ant<TravelToCity>> obj0)
        {
            //Do nothing
        }
    }

    class CostOnlyTrailSystem : TrailSystem<TravelToCity>
    {
        public double ActionProbabilityDensity(TravelToCity journey)
        {
            return 1.0/journey.Cost;
        }

        public void LayTrails(IEnumerable<Ant<TravelToCity>> obj0)
        {
            //Do nothing
        }
    }

    class Program
    {
// ReSharper disable once UnusedParameter.Local
        static void Main(string[] args)
        {
            var cities = new[]
            {
                new City("London", 51.0, 0.0),
                new City("Paris", 48.0, 2.0),
                new City("T'bilisi", 41.0, 44.0),
                new City("Berlin", 52.0, 13.0),
                new City("Athens", 37.0, 23.0),
                new City("Budapest", 47.0, 19.0),
                new City("Rome", 41.0, 12.0),
                new City("Riga", 56.0, 24.0),
                new City("Luxembourg", 49.0, 6.0),
                new City("Oslo", 59.0, 10.0),
                new City("Kabul", 34.0, 69.0),
                new City("Belgrade", 44.0, 20.0),
                new City("Tunis", 36.0, 10.0),
                new City("Ankara", 38.0, 57.0),
                new City("Bern", 46.0, 7.0),
                new City("Madrid", 40.0, 3.0),
                new City("Bratislava", 48.0, 17.0),
                new City("Lisbon", 38.0, 9.0),
                new City("Stockholm", 59.0, 18.0),
                new City("Amsterdam", 52.0, 4.0),
                new City("Vilnius", 54.0, 25.0),
                new City("Dublin", 53.0, 6.0),
                new City("Jerusalem", 31.0, 35.0),
                new City("Reykjavik", 64.0, 21.0),
                new City("Helsinki", 60.0, 25.0),
                new City("Prague", 50.0, 14.0),
                new City("Copenhagen", 55.0, 12.0),
                new City("Zagreb", 45.0, 15.0),
                new City("Sophie", 42.0, 23.0),
                new City("Brussels", 50.0, 4.0)
            };

            var travelDict = cities.ToDictionary(x => x, 
                x => cities.ToDictionary(y => y, y => new TravelToCity(x, y)));

            Func<Ant<TravelToCity>> action = () => new SalesmanAnt(cities[0], travelDict);

            Console.WriteLine("Running completely random ant paths");
            RunColony(new NullTrailSystem(), action);
            Console.WriteLine("Running ant paths based only on local distance");
            RunColony(new CostOnlyTrailSystem(), action);
            Console.WriteLine("Running ant paths with trails");
            RunColony(new AntColonyTrailSystem<TravelToCity>(2.0, 1.0, 0.8, 0.2), action);
            Console.WriteLine("Running min max ant trails");
            RunColony(new MaxMinAntColonyTrailSystem<TravelToCity>(2.0, 1.0, 0.8, 0.01, 0.5), action);
            Console.ReadKey();
        }

        public static void RunColony(TrailSystem<TravelToCity> system, Func<Ant<TravelToCity>> createAnt)
        {
            var result = AntColony.OptimizeCSharp(createAnt,
                50, 50, system);

            foreach (var x in result)
            {
                Console.WriteLine(x.Destination.Name);
            }
        }
    }
}
