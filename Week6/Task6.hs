main::IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> div x 2)]) 2 == 1 -- my test

getOddCompositionValue :: [Int -> Int] -> (Int -> Int)
getOddCompositionValue = foldl1 (.) . reverse