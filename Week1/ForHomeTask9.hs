main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910
    print $ growingPlant 2 1 3 == 2 -- my test 

growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = growingPlantWrap True 0 upSpeed downSpeed desiredHeight
    where 
        growingPlantWrap isDay count upSpeed downSpeed desiredHeight
            | desiredHeight <= 0 = count
            | isDay == True = growingPlantWrap False (count + 1) upSpeed downSpeed (desiredHeight - upSpeed) -- day
            | isDay == False = growingPlantWrap True count  upSpeed downSpeed (desiredHeight + downSpeed) -- night