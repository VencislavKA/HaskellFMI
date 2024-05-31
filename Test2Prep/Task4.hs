main :: IO()
main = do
    print $ getOddCompositionValue [(\x -> x+1),(\x -> x*2),(\x -> x-1),(\x -> x `div` 2)] 2 == 2

getOddCompositionValue :: [Int -> Int] -> (Int -> Int)
getOddCompositionValue lst = (\x -> (foldl1 (.) $ map snd $ filter (odd . fst) $ zip [1..] lst) x)
