import Data.List
main::IO()
main = do 
    print $ isNeighbour 1 2 [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] == True
    print $ isNeighbour 2 1 [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] == False

    print $ allPoints [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] == [1,2,3,4]

    print $ addNeighbour [1] [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] == [[1,2],[1,3]]
    print $ addNeighbour [1,2] [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] == [[1,2,3],[1,2,4]]

    print $ addNeighbours [[1]] [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]
    print $ addNeighbours (addNeighbours [[1]] [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]) [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

    print $ addNeighbours [[1]] [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1,2],[1,3],[1,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 == [[2,3]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths graph len point = helper len [[point]]
 where
    helper :: Int -> [Path] -> [Path]
    helper 0 res = res
    helper len res = helper (len - 1) (addNeighbours res graph)

allPoints :: Graph -> [Node]
allPoints = map (fst)

isNeighbour :: Node -> Node -> Graph -> Bool
isNeighbour parent neighbour graph = elem neighbour $ snd $ head $ filter (\(idx,nei) -> idx == parent) graph

addNeighbours :: [Path] -> Graph -> [Path]
addNeighbours lst graph = concat $ map (\x -> addNeighbour x graph) $ lst

addNeighbour :: Path -> Graph -> [Path]
addNeighbour lst graph = helper lst $ allPoints graph
 where
    helper :: Path -> [Node] -> [Path]
    helper lst points = map (\x -> lst ++ [x]) $ filter (\x -> isNeighbour (last lst) x graph) points