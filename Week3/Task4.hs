main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990
    print $ sumDivisibleNumbers 5 0 1 == 15 -- my test 
    
sumDigits :: Int -> Int
sumDigits number = sumDigitsHelper number 0
 where
    sumDigitsHelper 0 res = res
    sumDigitsHelper number res = sumDigitsHelper (div number 10) (res + mod number 10)

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = sumDivisibleNumbersHelper (min start finish) (max start finish) k 0
 where
    sumDivisibleNumbersHelper realStart realFinish k sum
     | realStart > realFinish = sum
     | mod (sumDigits realStart) k == 0 = sumDivisibleNumbersHelper (realStart + 1) realFinish k (sum + realStart)
     | otherwise = sumDivisibleNumbersHelper (realStart + 1) realFinish k sum
