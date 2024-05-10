import Data.List
main::IO()
main = do
    print $ dominates (+2) (+1) [1, 2, 3]
    print $ not $ dominates (+1) (+2) [1, 2, 3]

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool -- True -> First function dominates -- False -> Secound function dominates
dominates f x [] = True
dominates f x xs = f (head xs) >= x (head xs) && dominates f x (drop 1 xs)