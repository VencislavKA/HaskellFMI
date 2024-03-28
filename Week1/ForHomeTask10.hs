main :: IO()
main = do
    print $ snail 3 2 1 == 2 
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1
    print $ snail 10 9 8 == 2 -- my test

snail :: Int -> Int -> Int -> Int
snail distance day night = snailWrap True 0 distance day night
where
    snailWrap isDay count distance day night
    | distance <= 0 = count
    | isDay == True = snailWrap False (count + 1) (distance - day) day night -- day
    | isDay == False = snailWrap True count (distance + night) day night -- night

