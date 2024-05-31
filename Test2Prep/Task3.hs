main :: IO()
main = do
    print $ (boundUp (+2) 4) 1 == 4
    print $ (boundUp (+2) 4) 3 == 5

boundUp :: (Int -> Int) -> Int -> (Int -> Int)
boundUp f y = (\x -> helper x)
 where
    helper x
     | f x > y = f x
     | otherwise = y