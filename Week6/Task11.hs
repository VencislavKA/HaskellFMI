main::IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]
    print $ onlyArithmetic [[1,3,5],[4,14,24,1]] == [[1,3,5]] -- my test

isArithmetic :: [Int] -> Bool
isArithmetic xs = all ((== (head $ map (\(x,y) -> y - x) format)) . (\(x,y) -> y - x)) format
 where
    format = zip xs (tail xs)

onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter isArithmetic