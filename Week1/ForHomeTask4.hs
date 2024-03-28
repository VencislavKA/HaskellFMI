main :: IO()
main = do
    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000
    print $ sumCubesPow 2 3 == 35 -- my test


    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000
    print $ sumCubesPow 1 2 == 9 -- my test

sumCubesPow :: Int -> Int -> Int
sumCubesPow x y = x ^ 3 + y ^ 3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow x y = x * x * x + y * y * y