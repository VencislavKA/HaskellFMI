main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134
    print $ removeD 3 12345 == 1245 -- my test

removeD :: Int -> Int -> Int
removeD d number = removeDHelper d number 0 1
 where
    removeDHelper d 0 res mul = res
    removeDHelper d number res mul
     | mod number 10 == d = removeDHelper d (div number 10) res mul
     | otherwise = removeDHelper d (div number 10) (mod number 10 * mul + res) (mul * 10)