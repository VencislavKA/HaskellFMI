main :: IO()
main = do 
    print 123
    print $ perimeter (Circle 5) == 31.41592653589793
    -- print $ perimeter (Rectangle 2.5 4.5) == 14
    -- print $ perimeter (Rectangle 5.5 20.6) == 52.2
    -- print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
    -- print $ perimeter (Cylinder 2.5 10) == 30

    -- print $ area (Circle 5) == 78.53981633974483
    -- print $ area (Rectangle 2.5 4.5) == 11.25
    -- print $ area (Rectangle 5.5 20.6) == 113.30000000000001
    -- print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
    -- print $ area (Cylinder 20 30) == 6283.185307179587


data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
    deriving(Show,Eq,Read,Ord)

parameter :: (Num a) => Shape a -> Double
parameter (Circle a) = 2 * pi * a
