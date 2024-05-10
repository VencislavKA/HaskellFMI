main::IO()
main = do
    print $ isContaining 123 0 == False
    print $ isContaining 123 1 == True
    print $ isContaining 123 3 == True
    print $ sumAllNumbers 60 65 == 61
    print $ sumAllNumbers 60 66 == 126
    print $ sumAllNumbers 60 70 == 195

isContaining :: Int -> Int -> Bool
isContaining number digit
 | number == 0 = False
 | otherwise = mod number 10 == digit || isContaining (div number 10) digit

sumAllNumbers :: Int -> Int -> Int
sumAllNumbers start end = sumAllNumbersHelper start end 0
 where
    sumAllNumbersHelper :: Int -> Int -> Int -> Int
    sumAllNumbersHelper start end sum
     | start == end = sum
     | mod start 4 == 1 && isContaining start 6 = sumAllNumbersHelper (start + 1) end (sum + start)
     | otherwise = sumAllNumbersHelper (start + 1) end sum