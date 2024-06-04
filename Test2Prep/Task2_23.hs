import Data.Char
main :: IO()
main = do 
    print $ zip [1..] ['a' .. 'z']
    print $ zip [1..] "abode"
    print $ sum $ zipWith (\(id1,let1) (id2,let2) -> if id1 == id2 && let1 == let2 then 1 else 0) (zip [1..] ['a' .. 'z']) $ zip [1..] "abode"

    print $ solve ["abode","ABc","xyzD"] == [4,3,1]
    print $ solve ["abide","ABc","xyz"] == [4,3,0]
    print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7]
    print $ solve ["encode","abc","xyzD","ABmD"] == [1,3,1,3]

solve :: [String] -> [Int]
solve lst = map helper lst
 where
    helper = sum . zipWith (\(id1,let1) (id2,let2) -> if id1 == id2 && toLower let1 == toLower let2 then 1 else 0) (zip [1..] ['a' .. 'z']) . zip [1..]