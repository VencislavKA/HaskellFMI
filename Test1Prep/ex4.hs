main :: IO()
main = do
    print 123

mySin :: Int -> Double -> Double
mySin n x = mySinHelper n x 0
 where
    mySinHelper :: Int -> Double -> Double -> Double
    mySinHelper 2 x sum = sum + x
    mySinHelper n x sum = (-1)^n * x^(2*n+1) / fromIntegral ( 2*n+1 )

