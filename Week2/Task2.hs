main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 0 == 0 -- my test

sumDigitsIter :: Int -> Int
sumDigitsIter x
 | x < 0 = error "Invalid input!"
 | otherwise =  sumDigitsIterHelper 0 x
 where
    sumDigitsIterHelper :: Int -> Int -> Int
    sumDigitsIterHelper sum 0 = sum
    sumDigitsIterHelper sum number = sumDigitsIterHelper (mod number 10 + sum) (div number 10)