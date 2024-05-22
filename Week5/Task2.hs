import Data.List
import Data.Char
main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

rev :: Int -> Int
rev = read . reverse . show

isPalindrome x = x == rev x

getDivs :: Int -> [Int]
getDivs n = filter (\x -> mod n x == 0 && isPalindrome x) [2 .. n]

getPalindromes :: Int -> Int
getPalindromes n = maximum divs + minimum divs
 where
    divs = getDivs n