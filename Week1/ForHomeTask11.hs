main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 34 == 43 -- my test


rev :: Int -> Int
rev number
 | number < 0 = (-1)
 | otherwise = revWraper 0 number
 where
    revWraper number1 0 = number1
    revWraper number1 number2 = revWraper (number1 * 10 + mod number2 10) (div number2 10)

