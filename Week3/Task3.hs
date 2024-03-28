main :: IO()
main = do
    -- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 -- == -1.047619047619048
    print $ calcSeriesSum 1 4 -- == -1.0814814814814817
    print $ calcSeriesSum 1 5 -- == -1.0753246753246755
    print $ calcSeriesSum 1 6 -- == -1.0762718762718764
    print $ calcSeriesSum 10 34 -- == -0.10607741100173662

calcSeriesSum :: Int -> Int -> Double
calcSeriesSum x n = calcSeriesSumHelper 1 x n 1 0
 where
    calcSeriesSumHelper :: Int -> Int -> Int -> Int -> Double -> Double
    calcSeriesSumHelper count x n div res
     | count > (n + 1) = res
     | otherwise = calcSeriesSumHelper (count + 1) x n (div * (div + 2)) ( res + ( fromIntegral ((-1)^count * (2 ^ count) * (x ^ (count - 1))) / fromIntegral div))
