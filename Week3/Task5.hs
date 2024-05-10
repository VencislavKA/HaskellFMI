main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51
    print $ p 7 == 70 -- my test

pNoRec :: Int -> Int
pNoRec number = div (number * ((3 * number) - 1)) 2

p :: Int -> Int
p number = pHelper number 0 0
 where
    pHelper :: Int -> Int -> Int -> Int
    pHelper number counter res
     | counter == number = res
     | otherwise = pHelper number (counter + 1) (res + 3 * (counter + 1) - 2)
