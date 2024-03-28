main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53
    print $ sumPrimeDivs 1 == 1 -- my test

isPrime :: Int -> Bool
isPrime 2 = True
isPrime number = number > 1 && isPrimeHelper 2 (ceiling (sqrt $ fromIntegral number))
 where
  isPrimeHelper d sqrtN
   | d > sqrtN = True
   | mod number d == 0 = False
   | otherwise = isPrimeHelper (d + 1) sqrtN

sumPrimeDivs :: Int -> Int
sumPrimeDivs 0 = 0
sumPrimeDivs 1 = 1
sumPrimeDivs number = sumPrimeDivsHelper number 2 0
 where
    sumPrimeDivsHelper :: Int -> Int -> Int -> Int
    sumPrimeDivsHelper number counter res
     | counter > number = res
     | isPrime counter && mod number counter == 0 = sumPrimeDivsHelper number (counter + 1) (res + counter)
     | otherwise = sumPrimeDivsHelper number (counter + 1) res