import Data.List
import Data.Char
main :: IO()
main = do
    print $ map (\x -> (length x, head x)) $ group $ sort $ map toLower "Mississippim"
    print $ sumNumbers 123 == 6
    print $ sumNumbers 54 == 9
    print $ sumNumbers 540 == 9
    print $ isInterestingNumber 410

sumNumbers:: Int -> Int
sumNumbers 0 = 0
sumNumbers number = sumNumbers (div number 10) + mod number 10

isInterestingNumber :: Int -> Bool
isInterestingNumber number = mod number (sumNumbers number) == 0