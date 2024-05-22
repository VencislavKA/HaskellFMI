main::IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8 -- (((1 + 3) + 2) + 1) + 1
    print $ (pairCompose [(+2), (+3)]) 1 == 7 -- my test

-- evenCompose:: [Int -> Int] -> Int -> Int
-- evenCompose [] number = number
-- evenCompose (x:xs) number = (x . head xs) number + evenCompose (drop 1 xs) number

-- oddCompose:: [Int -> Int] -> Int -> Int
-- oddCompose (x:xs) number
--  | length (x:xs) > 2 = (x . head xs) number + oddCompose (drop 1 xs) number
--  | otherwise = x number

-- pairCompose :: [Int -> Int] -> Int -> Int
-- pairCompose list number
--  | even (length list) = evenCompose list number
--  | otherwise = oddCompose list number

pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] = id
pairCompose [f] = f
pairCompose (f1:f2:fn) = (\x -> (f1 . f2) x + pairCompose fn x)