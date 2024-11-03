
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List 
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Array

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjacencyMatrix = Array(Int,Int) Distance
type PathMatrix =  Array(Int, Int) (Maybe Int)


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

--Function 1
cities :: RoadMap -> [City]
cities = Data.List.sort . foldl (\acc (c1, c2, _) -> acc ++ noRepetition c1 acc ++ noRepetition c2 acc)[]
    where noRepetition x acc = if x `elem` acc then [] else [x]
    
{-
nesta função temos um acumulador que vai de tuplo
em tuplo e usa de forma recursiva a função noRepetition,
ou seja concatena o acc, com o no Repetiotion de c1 e c2,
que só vê se o elemento x que é uma cidade já se encontra 
na lista acc , se sim retorna uma lista vazia se não retorna
uma lista só com a cidade em falta.
-}


--Function 2 

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((c1, c2, _):xs) k z = 
    if (c1 == k && c2 == z) || (c1 == z && c2 == k) then True
    else areAdjacent xs k z


{--isIn :: City -> City -> City -> City -> Bool
isIn c1 c2 x y = if x == c1 && y == c2 then True 
else False--}

--Function 3 
-- se tiver de cidade 1 a cidade 1 é suporto dar zero ??
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) s e = if  areAdjacent [(c1, c2, d)] s e then Just d
else distance xs s e 


--Function 4

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((c1, c2, d):xs) k 
    | k== c1 = [(c2, d)]++ adjacent xs k
    | k== c2 = [(c1, d)]++ adjacent xs k
    | otherwise = adjacent xs k
                             


{--Questão ou problema:
oneWayAdjacent e oneWayDistance são iguais só que não vem tanto para um lado
como pelo outro. É suposto a função pedida ver para os dois lados ??
e se sim então aqui preciso mesmo de ter outras funções ??

Literalmente a unica diferença é na oneWayAdjacent que não ver para os dois lados e a 
oneWayDistance vai chamar esse em vez da original areAdjacent)
--}

-- ok lol é undirected lágrima

{--oneWayAdjacent :: RoadMap -> City -> City -> Bool
oneWayAdjacent [] _ _ = False
oneWayAdjacent ((c1, c2, _):xs) k z = 
    if (c1 == k && c2 == z)  then True
    else oneWayAdjacent xs k z

oneWayDistance :: RoadMap -> City -> City -> Maybe Distance
oneWayDistance [] _ _ = Nothing
oneWayDistance ((c1, c2, d):xs) s e = if  oneWayAdjacent [(c1, c2, d)] s e then Just d
else oneWayDistance xs s e --}

--Function 5 

dist :: RoadMap -> Path -> Int -> Maybe Distance
dist [] _ _ = Nothing
dist rm [_] acc = Just acc
dist rm [c1, c2] acc = case distance rm c1 c2 of
    Nothing -> Nothing
    Just d  -> Just (acc + d) 
dist rm (c1:c2:xs) acc = case distance rm c1 c2 of
    Nothing -> Nothing
    Just d -> dist rm (c2:xs) (acc+d)

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rmap city = dist rmap city 0


--Function 6 
--  degree -this function return a list of tuples wher first is city and second the degree 
degree :: RoadMap ->[City] -> [(City, Int)] 
degree rm [] = []
degree rm (x:xs) =foldr (\ x -> (++) [(x, length (adjacent rm x))]) [] xs
    -- [(x, length(adjacent rm x ))] ++ degree rm xs --versão mais inteligente

--this functions order the list by their degree
qsortsecond :: (City, Int) -> [(City, Int)] -> [(City, Int)]
qsortsecond x [] = [x]
qsortsecond x (y:ys)=
    if (snd x <= snd y)then [x] ++ [y] ++ ys
    else [y] ++ qsortsecond x ys

qsortfirst ::[(City, Int)] -> [(City, Int)]
qsortfirst [] = []
qsortfirst (x:xs) = qsortsecond x (qsortfirst xs) 

-- final-this function just filter the first cities with the highest degree 
final :: [(City, Int)] -> [(City, Int)]
final []=[]
final (x1:x2:xs) = if(snd x1 == snd x2) then [x1] ++ final(x2:xs) else [x1]

-- rome 
rome :: RoadMap -> [City]
rome rm = reverse( map fst ( final (reverse (qsortfirst (degree rm c ) )) ))
        where c =cities rm 
            
      

-- DEPOIS SUBSTITUIR PELA FUNÇÃO 4
{-adjacent :: RoadMap -> City -> [City]
adjacent rm city = [c2 | (c1, c2, _) <- rm, c1 == city] ++ [c1 | (c1, c2, _) <- rm, c2 == city]
-}
{-
-- preciso melhorar not very efficient 
addToVisited :: [City] -> [City] -> [City]
addToVisited [] visited = visited 
addToVisited (x:xs) visited =  if x `elem` visited 
    then addToVisited xs visited
    else addToVisited xs (x:visited)


scc :: RoadMap -> [City] -> [City] -> Bool
scc rm [] visited = (length (cities rm)) == (length visited) --se o tamanho de rm for igual ao dos visited então é scc
scc rm (city:xs) visited = 
                            let 
                                adj = map fst (adjacent rm city)
                                newVisited = addToVisited adj visited 
                                newToExplore = filter (`notElem` visited) adj ++ xs
                            in 
                                scc rm newToExplore newVisited 

-- necessário??
first :: (City, City, Distance) -> City
first (c1, _, _) = c1  

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected ((c1,b,c):rm) = let city = first (head rm) in scc rm [city] [city]
-}

--Function 7 
--tentei por menos chatgpt

addvisited ::[City] -> [City] -> [City]
addvisited [] visited = visited
addvisited (a:adj) visited = 
    if(a `notElem` visited) then [a] ++ (addvisited adj visited)
    else addvisited adj visited
                             
sch :: RoadMap -> RoadMap -> [City] -> [City] -> Bool
sch rm [] citys visited = if(citys/= visited) then False else True
sch rm ((a,b,d):xs) citys visited = if ((length (cities rm ))==length visited) then True
                                    else sch rm xs citys (addvisited adj visited) 
                                    where adj= map fst ( adjacent rm a )
                                                                             
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected rm = let cits=cities rm  
                         in sch rm rm cits [] 

--Function 8 
{-
possible_paths :: RoadMap -> [City ]-> City-> [City]-> [Path]
possible_paths rm [] next visited= []
possible_paths rm (a:adj) next visited   |(a==next) = []
                                         |( a `elem` visited )= possible_paths  rm adj next visited
                                         |otherwise possible_paths rm (map fst(adjacent rm a)) next (visited++[a])
-}
-- Function to find adjacent cities and distances from a given city in the roadmap




infinity :: Distance
infinity = maxBound `shiftR` 1

adjacent_ :: RoadMap -> City -> [(City, Distance)]
adjacent_ [] _ = []
adjacent_ ((c1, c2, d):xs) k 
    | k == c1 = (c2, d) : adjacent_ xs k
    | k == c2 = (c1, d) : adjacent_ xs k
    | otherwise = adjacent_ xs k

findClosest :: [(City, Distance)] -> [City] -> Maybe (City, Distance)
findClosest distances visited =
    let unvisitedDistances = filter (\(city, _) -> city `notElem` visited) distances
    in if null unvisitedDistances then Nothing else Just $ minimumBy (comparing snd) unvisitedDistances

shortestPath :: RoadMap -> City -> City -> Path
shortestPath roadmap start end =
  let
    cities = uniqueCities roadmap
    distances = [(city, if city == start then 0 else infinity) | city <- cities]
    paths = [(city, if city == start then [start] else []) | city <- cities]
    
    dijkstra :: [City] -> [(City, Distance)] -> [(City, Path)] -> [(City, Path)]
    dijkstra visited distList pathList
      | any (\(c, _) -> c == end && c `elem` visited) distList = pathList
      | otherwise =
          case findClosest distList visited of
            Nothing -> pathList
            Just (currentCity, currentDist) ->
              let
                  newVisited = currentCity : visited
                  neighbors = adjacent_ roadmap currentCity
                  
                  (newDistList, newPathList) = foldl' (updateDistances currentCity currentDist) (distList, pathList) neighbors
              in dijkstra newVisited newDistList newPathList

    updateDistances :: City -> Distance -> ([(City, Distance)], [(City, Path)]) -> (City, Distance) -> ([(City, Distance)], [(City, Path)])
    updateDistances currentCity currentDist (distList, pathList) (neighbor, dist) =
      let newDist = currentDist + dist
          oldDist = lookupList neighbor distList
      in if newDist < oldDist
         then (updateList neighbor newDist distList, updatePath neighbor (lookupPath currentCity pathList ++ [neighbor]) pathList)
         else (distList, pathList)

    lookupList :: City -> [(City, Distance)] -> Distance
    lookupList city = fromMaybe infinity . lookup city

    lookupPath :: City -> [(City, Path)] -> Path
    lookupPath city = fromMaybe [] . lookup city

    updateList :: City -> Distance -> [(City, Distance)] -> [(City, Distance)]
    updateList city newDist = map (\(c, d) -> if c == city then (c, newDist) else (c, d))

    updatePath :: City -> Path -> [(City, Path)] -> [(City, Path)]
    updatePath city newPath = map (\(c, p) -> if c == city then (c, newPath) else (c, p))

    uniqueCities :: RoadMap -> [City]
    uniqueCities roads = foldl' (\acc (c1, c2, _) -> acc `union` [c1, c2]) [] roads
      where
        union xs ys = xs ++ [y | y <- ys, y `notElem` xs]
    finalPaths = dijkstra [] distances paths
  in lookupPath end finalPaths

travelSales :: RoadMap -> Path
travelSales = undefined




gTest1 :: RoadMap
gTest1 = [("7","6",1),
            ("8","2",2),
            ("6","5",2),
            ("0","1",4),
            ("2","5",4),
            ("8","6",6),
            ("2","3",7),
            ("7","8",7),
            ("0","7",8),
            ("1","2",8),
            ("3","4",9),
            ("5","4",10),
            ("1","7",11)
            ,("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]
gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
