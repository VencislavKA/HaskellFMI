import Data.List

main :: IO()
main = do
    print $ isAritmeticProgretion [1, 3, 5, 7, 9]  -- True
    print $ isAritmeticProgretion [1, 2, 3, 4, 5]  -- True
    print $ isAritmeticProgretion [1, 2, 4, 8, 16] -- False

isAritmeticProgretion :: [Int] -> Bool
isAritmeticProgretion [] = True
isAritmeticProgretion [_] = True
isAritmeticProgretion (x:y:xs) = (y - x) == (head xs - y) && isAritmeticProgretion (y:xs)