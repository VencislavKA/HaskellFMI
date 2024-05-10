import Data.List
import Data.Char
main::IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 2 2 == 25 -- my test


isPrime :: Int -> Bool
isPrime number = [1,number] == filter (\d -> mod number d == 0) [1 .. number]

sumSpecialPrimes:: Int -> Int -> Int
sumSpecialPrimes n d = sum . take n . filter isPrime $ filter (elem d . map digitToInt . show)  [1 .. ]