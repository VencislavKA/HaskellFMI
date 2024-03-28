main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdG 2 6 == 2 -- my test

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
    print $ myGcdPM 1 3 == 1 -- my test

myGcdG :: Int -> Int -> Int
myGcdG x y 
 | x == 0 = y
 | y == 0 = x
 | x >= y = myGcdG (mod x y) y
 | y > x = myGcdG x (mod y x)

myGcdPM :: Int -> Int -> Int
myGcdPM 0 y = y
myGcdPM x 0 = x
myGcdPM x y
 | x >= y = myGcdPM (mod x y) y
 | y > x = myGcdPM x (mod y x)
