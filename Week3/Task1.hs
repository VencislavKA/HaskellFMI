main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22
    print $ removeFirstOccurrence 34321 3 == 3421 -- my test

reverseNumber :: Int -> Int
reverseNumber number = reverseNumberHelper number 0
 where
    reverseNumberHelper 0 res = res
    reverseNumberHelper number res = reverseNumberHelper (div number 10) (res * 10 + mod number 10)

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence number digit = removeFirstOccurrenceHelper number digit 1 False
 where
    removeFirstOccurrenceHelper :: Int -> Int -> Int -> Bool -> Int
    removeFirstOccurrenceHelper 0 digit res isFound = div (reverseNumber res) 10 
    removeFirstOccurrenceHelper number digit res isFound
     | mod number 10 == digit && not isFound = removeFirstOccurrenceHelper (div number 10) digit res True
     | otherwise = removeFirstOccurrenceHelper (div number 10) digit (res * 10 + mod number 10) isFound