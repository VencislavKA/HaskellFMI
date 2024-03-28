main :: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1
    print $ countOccurrences 1234 4 == 1 -- my test

countOccurrences :: Int -> Int -> Int
countOccurrences 0 0 = 1
countOccurrences 0 _ = 0
countOccurrences number occ = countOccurrencesHelper 0 number occ
 where
    countOccurrencesHelper :: Int -> Int -> Int -> Int
    countOccurrencesHelper counter 0 occ = counter
    countOccurrencesHelper counter number occ
     | mod number 10 /= occ = countOccurrencesHelper counter (div number 10) occ
     | otherwise = countOccurrencesHelper (counter + 1) (div number 10) occ