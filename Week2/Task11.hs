main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not
    print $ truncatablePrime 0 == False
    print $ truncatablePrime 1 == False
    print $ truncatablePrime 2 == True
    print $ truncatablePrime 37397 == True
    print $ truncatablePrime 1399 == False -- 1 is not prime
    print $ truncatablePrime 1733 == False -- 1 is not prime
    print $ truncatablePrime 1913 == False -- 1 is not prime
    print $ truncatablePrime 1931 == False -- 1 is not prime
    print $ truncatablePrime 1933 == False -- 1 is not prime
    print $ truncatablePrime 1973 == False -- 1 is not prime
    print $ truncatablePrime 19333 == False -- 1 is not prime
    print $ truncatablePrime 19739 == False -- 1 is not prime
    print $ truncatablePrime 73 == True -- my test

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime number = isPrimeHelper number 2
 where
  isPrimeHelper 1 2 = False
  isPrimeHelper number div
   | div == number = True
   | mod number div == 0 = False
   | otherwise = isPrimeHelper number (div + 1)

truncatablePrime :: Int -> Bool
truncatablePrime number = number < 10 && isPrime number || isPrime number && truncatablePrime (div number 10)