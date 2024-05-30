import Data.Char
import Data.List
main::IO()
main = do
    print $ reduceStr "aA" == ""
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    print $ reduceStr "HaskellLl" == "Haskell" -- my test
                                                            
reduceStr :: [Char] -> [Char]
reduceStr str
     | reducedString == str = str
     | otherwise = reduceStr reducedString
 where
    reducedString = concat (filter (\x -> length x == 1) (groupBy (\x y -> (isUpper x && isLower y || isLower x && isUpper y) && (toLower x == toLower y) ) str))