main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 5) 2 == 45 -- my test

-- switchSum :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
-- switchSum f g n number = switchSumHelper 1 f
--  where
--     switchSumHelper count func
--      | count == (n + 1) = 0
--      | odd count = func number + switchSumHelper (count + 1) (g . func)
--      | otherwise = func number + switchSumHelper (count + 1) (f . func)

switchSum :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
switchSum _ _ 0 = (\x -> 0)
switchSum f g n = (\x -> f x + switchSum g f (n - 1) (f x))