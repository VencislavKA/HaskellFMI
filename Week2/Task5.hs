main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable 5020 5564 == True -- my test
    

areAmicable :: Int -> Int -> Bool
areAmicable number1 number2 = sumAllDividers 0 1 number1 == number2 && number1 == sumAllDividers 0 1 number2

sumAllDividers :: Int -> Int -> Int -> Int
sumAllDividers sum div number
  | div == number = sum
  | mod number div == 0 = sumAllDividers (sum + div) (div + 1) number
  | otherwise = sumAllDividers sum (div + 1) number

