main::IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    print $ isArithmetic [2, 4, 6, 8] == True -- my test

format :: [Int] -> [(Int,Int)]
format xs = zip xs (tail xs)

isArithmetic :: [Int] -> Bool
isArithmetic xs = all (== (head $ map (\(x,y) -> y - x) $ format xs)) $ map (\(x,y) -> y - x) $ format xs