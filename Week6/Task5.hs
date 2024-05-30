import Data.Char (isDigit)
main::IO()
main = do
    print $ liesOn (line (0,0) (1,1)) (5.5,5.5)
    print $ liesOn (line (0,0) (1,1)) (0.5,0) == False

type Point = (Double,Double)

line :: Point -> Point -> (Double -> Double)
line p1 p2 = ((x2 - x1)/).((y1+).((y2-y1)*).(x1-))
 where
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f (x,y) = f x == y