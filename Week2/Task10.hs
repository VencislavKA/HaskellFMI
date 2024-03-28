main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11 
    print $ countPalindromes 1 3 == 1 -- my test

countPalindromes::Int -> Int -> Int
countPalindromes start end = countPalindromesHelper 0 (min start end + 1) (max start end)
 where
    countPalindromesHelper count start end
     | start == end = count
     | start == reverseNumber start 0 = countPalindromesHelper (count + 1) (start + 1) end
     | otherwise = countPalindromesHelper count (start + 1) end
     where
        reverseNumber 0 res = res
        reverseNumber number res = reverseNumber (div number 10) (res * 10 + mod number 10)
