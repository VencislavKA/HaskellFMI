import Data.List
main::IO()
main = do 
    print $ 123
    -- print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    -- print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    -- print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1,2],[1,3],[1,4]]
    -- print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    -- print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]
    -- print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 == [[2,3]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

-- format :: Graph -> [(Int,Int)]
-- format graph = 

getConnected :: Graph -> Int -> [Int]
getConnected graph parent = snd $ head $ filter (\(par,ch) -> par == parent) graph

-- simplePaths :: Graph -> Int -> Int -> [[Int]]
-- simplePaths graph k n = 