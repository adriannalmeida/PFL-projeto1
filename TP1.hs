import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


--versão mega eficiente do chat gpt não entidi um caralho
{--cities :: RoadMap -> [City]
cities = foldl (\acc (c1, c2, _) -> addIfNotIn c1 (addIfNotIn c2 acc)) []
    where
    addIfNotIn city acc = if city `elem` acc then acc else acc ++ [city]
--}
{--sort:: Ord(a) => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
    where 
        insert y [] = [y]
        insert y (x:xs) = if y <= x
        then y : x : xs
        else x : insert y xs
--}

{--noRepetition :: City -> [City] -> [City]
noRepetition x [] = [x]
noRepetition x (y:ys) = if x == y then []
    else noRepetition x ys--}

cities :: RoadMap -> [City]
cities = Data.List.sort . foldl (\acc (c1, c2, _) -> acc ++ noRepetition c1 acc ++ noRepetition c2 acc)[]
    where noRepetition x acc = if x `elem` acc then [] else [x]
    

    
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

isIn :: (City,City,Distance) -> City -> City -> Bool
isIn (c1, c2, _) x y = if x == c1 && y == c2 then True 
else False

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) s e = if  isIn (c1,c2,d) s e then Just d
else distance xs s e 

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
