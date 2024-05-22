import Data.Char
import Data.List
main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 1 1 == [] -- my test

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 1 1 == [] -- my test

isContaining :: Int -> Int -> Bool
isContaining n = elem n . map digitToInt . show

isPrime :: Int -> Bool
isPrime number = [1,number] == filter (\d -> mod number d == 0) [1 .. number]

getPrimesLC::Int -> Int -> [Int]
getPrimesLC start end = [x| x <- [min start end .. max start end],isContaining 7 x,isPrime x]

getPrimesHOF:: Int -> Int -> [Int]
getPrimesHOF start end = filter (\x -> isContaining 7 x && isPrime x) [min start end .. max start end]