main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True
    print $ isInteresting 1 == True -- my test

isInteresting :: Int -> Bool
isInteresting number = mod number (sumAllDigits 0 number) == 0

sumAllDigits :: Int -> Int -> Int
sumAllDigits sum 0 = sum
sumAllDigits sum number = sumAllDigits (sum + mod number 10) (div number 10)