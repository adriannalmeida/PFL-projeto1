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

dist :: RoadMap -> Path -> Int -> Maybe Distance
dist [] _ _ = Nothing
dist rm [_] acc = Just acc
dist rm [c1, c2] acc = case distance rm c1 c2 of
    Nothing -> Nothing
    Just d  -> Just (acc + d) 
dist rm (c1:c2:xs) acc = case distance rm c1 c2 of
    Nothing -> Nothing
    Just d -> dist rm (c2:xs) (acc+d)

--Function 5 

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm city = dist rm city 0

--Function 6 
rome :: RoadMap -> [City]
rome = undefined
--Não feita


-- DEPOIS SUBSTITUIR PELA FUNÇÃO 4
{-adjacent :: RoadMap -> City -> [City]
adjacent rm city = [c2 | (c1, c2, _) <- rm, c1 == city] ++ [c1 | (c1, c2, _) <- rm, c2 == city]
-}

-- preciso melhorar not very efficient 
addToVisited :: [City] -> [City] -> [City]
addToVisited [] visited = visited 
addToVisited (x:xs) visited =  if x `elem` visited 
    then addToVisited xs visited
    else addToVisited xs (x:visited)

{-
scc :: RoadMap -> [City] -> [City] -> Bool
scc rm [] visited = (length (cities rm)) == (length visited)
scc rm (city:xs) visited = 
    let adj = adjacent rm city
        newVisited = addToVisited adj visited 
        newToExplore = filter (`notElem` visited) adj ++ xs
    in scc rm newToExplore newVisited 

-- necessário??
first :: (City, City, Distance) -> City
first (c1, _, _) = c1  

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected rm = let city = first (head rm)
    in scc rm [city] [city]

-}




shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined




gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
