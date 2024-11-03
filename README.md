Function 8

The primary goal of this module is to provide an efficient way to find the shortest path between two cities in a given road map. The implementation utilizes Dijkstra's algorithm, which is well-suited for graphs with non-negative weights.

##Types

The following types are defined in the module:

City: A type alias for String, representing the name of a city. Path: A type alias for [City], representing a sequence of cities forming a path. Distance: A type alias for Int, representing the distance between cities. RoadMap: A type alias for a list of tuples [(City, City, Distance)], representing roads connecting cities with distances. AdjacencyMatrix: An array representation of the graph (not used in the current implementation). PathMatrix: An array representation of paths (not used in the current implementation).

Functionality

The shortestPath function finds the shortest path between two cities in a road map using Dijkstra's algorithm.

Parameters roadmap: A list of tuples representing the roads between cities and their distances. start: The starting city. end: The destination city.

##Returns A list of cities representing the shortest path from the starting city to the destination city. If no path exists, it returns an empty list.

##Implementation Details Extract Unique Cities: The function retrieves a list of all unique cities from the road map.

Initialize Distances and Paths: It initializes distances and paths for each city, setting the distance to the start city as 0 and all others as infinity. Dijkstra's Algorithm: -The algorithm iteratively finds the closest unvisited city and updates the distances and paths of its neighbors. -It continues until the destination city is reached or all reachable cities are visited.

Helper Functions: Several helper functions manage distance and path lookups, updates, and unique city extraction.
