main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83] -- [785.3981633974483,157.07963267948966,125.66370614359172,62.83185307179586]
    print $ getVolumes [(1, 2), (3, 4), (5, 6), (7, 8)] == [6.283185307179586,113.09733552923255,471.23889803846896,1231.5043202071988] -- my test

type Cylinder = (Double, Double)

getVolumes :: [Cylinder] -> [Double]
getVolumes = map (\x -> pi * fst x * fst x * snd x )
