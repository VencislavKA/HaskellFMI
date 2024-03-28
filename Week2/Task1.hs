main :: IO()
main = do
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsIter 0 == 1 -- my test

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsRec 0 == 1 -- my test 

countDigitsIter :: Integer -> Int
countDigitsIter 0 = 1
countDigitsIter number
 | number < 0 = error "Invalid input!"
 | otherwise = countDigitsIterHelper 0 number
 where
    countDigitsIterHelper counter 0 = counter
    countDigitsIterHelper counter number = countDigitsIterHelper (counter + 1) (div number 10)

countDigitsRec :: Integer -> Int
countDigitsRec x
 | x < 0 = error "Invalid input!"
 | x < 10 = 1
 | otherwise = countDigitsRec (div x 10) + 1
