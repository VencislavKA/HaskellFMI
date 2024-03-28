main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84
    print $ finalGrade 5 5 5 5 5 5 5 5 5                == 5.00 -- my test

finalGrade :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it
 | freeOfKz && freeOfKt == False = (fromIntegral $ truncate $ ((1.0 / 2.0 * tk) + (1.0 / 4.0 * it) + (1.0 / 4.0 * kz)) * 100) / 100.0
 | freeOfKz == False && freeOfKt = (fromIntegral $ truncate $ ((1.0 / 2.0 * tk) + (1.0 / 4.0 * kt) + (1.0 / 4.0 * iz)) * 100) / 100.0
 | freeOfKz && freeOfKt = (fromIntegral $ truncate $ ((1.0 / 2.0 * tk) + (1.0 / 4.0 * kt) + (1.0 / 4.0 * kz)) * 100) / 100.0
 | otherwise = (fromIntegral $ truncate $ ((1.0 / 2.0 * tk) + (1.0 / 4.0 * it) + (1.0 / 4.0 * iz)) * 100) / 100.0
 where
    freeOfKz = kz >= 4.50 && kz1 >= 4.00 && kz2 >= 4.00 && iz == 0
    freeOfKt = kt >= 4.50 && kt1 >= 4.00 && kt2 >= 4.00 && it == 0
    d = (d1 + d2 + d3) / 3.0
    kt = (kt1 + kt2) / 2.0
    kz = (kz1 + kz2) / 2.0
    tk = (1.0 / 4.0 * d + 3.0 / 8.0 * kt + 3.0 / 8.0 * kz)