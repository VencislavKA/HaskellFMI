import Data.List
import Data.Char
main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ specialSum 100 200 == 495 -- my test

specialSum :: Int -> Int -> Int
specialSum start end = sum $ filter (\x -> mod x 4 == 1 && elem 6 (map digitToInt $ show x)) [start .. end]