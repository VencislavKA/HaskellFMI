main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521  
    print $ sortN 1490 == 9410 -- my test
    
    print $ countDigit 1231 1 == 2 -- my test
    print $ addNDugits 123 2 2 == 12322 -- my test

countDigit :: Int -> Int -> Int
countDigit number digit = countDigitHelper number digit 0
 where
    countDigitHelper 0 digit count = count
    countDigitHelper number digit count
     | mod number 10 == digit = countDigitHelper (div number 10) digit (count + 1)
     | otherwise = countDigitHelper (div number 10) digit count

addNDugits :: Int -> Int -> Int -> Int
addNDugits number digit 0 = number
addNDugits number digit count
 | digit > 10 = error "Incorect input!"
 | otherwise = addNDugits (number * 10 + digit) digit (count - 1)

sortN :: Int -> Int
sortN number = sortNHelper number 9 0
 where
    sortNHelper number 0 res = addNDugits res 0 (countDigit number 0)
    sortNHelper number n res
     | countDigit number n == 0 = sortNHelper number (n - 1) res
     | otherwise = sortNHelper number (n - 1) (addNDugits res n (countDigit number n))