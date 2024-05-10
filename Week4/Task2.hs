import Data.List
main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 3 [1, 2, 3] == True -- my test


    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 3 [1, 2, 3] == True -- my test

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 3 [1, 2, 3] == True -- my test

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM digit xs
 | null xs = False
 | otherwise = head xs == digit || isPresentRecNonPM digit (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM digit (x:xs) = x == digit || isPresentRecNonPM digit xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem