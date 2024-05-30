import Data.Char
main::IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)
    print $ checkNumber 1 == (1,0) -- my test

checkNumber :: Int -> (Int,Int)
checkNumber = foldl (\(evenSum,oddSum) (i,x) -> if even i then (evenSum + x,oddSum) else (evenSum,oddSum + x)) (0,0) . zip [0..] . map digitToInt . show

