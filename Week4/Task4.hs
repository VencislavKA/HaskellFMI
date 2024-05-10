main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565
    print $ sumUnevenLC 1 10 == 25 -- my test

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565
    print $ sumUnevenHOF 1 10 == 25 -- my test


sumUnevenLC :: Int -> Int -> Int
sumUnevenLC start end = sum [x | x <- [min start end .. max start end],odd x]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF start end = sum $ filter odd [min start end .. max start end]