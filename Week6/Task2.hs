main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83] -- [785.3981633974483,157.07963267948966,125.66370614359172,62.83185307179586]
    print $ getVolumes [(1, 2), (3, 4), (5, 6), (7, 8)] == [6.28,113.1,471.24,1231.50] -- my test

type Cylinder = (Double, Double)

getVolumes :: [Cylinder] -> [Double]
getVolumes = map (\x -> roundTwoDigButWithMagic $ pi * fst x * fst x * snd x )

roundTwoDigButWithMagic :: Double -> Double
roundTwoDigButWithMagic = (/ 100) . fromIntegral . round . (100 *)