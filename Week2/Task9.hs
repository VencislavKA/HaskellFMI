main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14
    print $ everyOther 42141 == 42 -- my test

everyOther :: Int -> Int
everyOther number = everyOtherHelper 0 0 number
 where
    everyOtherHelper res count 0 = res
    everyOtherHelper res count number
     | even count = everyOtherHelper res (count + 1) (div number 10)
     | otherwise = everyOtherHelper (res * 10 + mod number 10) (count + 1) (div number 10)