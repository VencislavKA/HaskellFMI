import Data.Char (digitToInt)
main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 1 1 == [] -- my test

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 1 1 == [] -- my test

isContaining :: Int -> Int -> Bool
isContaining 0 n = False
isContaining number n = mod number 10 == n || isContaining (div number 10) n

isPrime :: Int -> Bool
isPrime 2 = True
isPrime number = number > 1 && isPrimeHelper 2 (ceiling (sqrt $ fromIntegral number))
 where
  isPrimeHelper d sqrtN
   | d > sqrtN = True
   | mod number d == 0 = False
   | otherwise = isPrimeHelper (d + 1) sqrtN

getPrimesLC::Int -> Int -> [Int]
getPrimesLC start end = [x| x <- [min start end .. max start end],isContaining x 7,isPrime x]

getPrimesHOF:: Int -> Int -> [Int]
getPrimesHOF start end = filter (elem 7 . map digitToInt . show) $ filter isPrime [min start end .. max start end] -- elem 7 . map digitToInt . show