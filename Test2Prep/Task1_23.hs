main :: IO()
main = do 
    print $ prodOdds [1,2,3,4,5,6] == 48
    print $ prodOdds [7.66,7,7.99,7] == 49.0

prodOdds :: Num a => [a] -> a
prodOdds = foldr (\(id,x) sum -> if odd id then x * sum else sum * 1) 1 . zip [0..]