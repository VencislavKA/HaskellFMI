import Data.List
main::IO()
main = do
    -------Ex 1---------
    -- print $ numDrink 9 3 == 13 
    -- print $ numDrink 15 4 == 19 
    -- print $ numDrink 761 3 == 1141

    -------Ex 2---------
    -- print $ fib 7 == 21

    -- print $ (aroundFib 100) 25 == [('1',3)] 
    -- print $ (aroundFib 180) 25 == [('1',5),('7',3)] 
    -- print $ (aroundFib 1700) 25 == [('1',4),('2',5),('0',6),('4',5),('5',7),('2',4),('6',7),('3',5), ('0',4),('8',5),('4',5),('4',4),('7',7),('7',6),('2',2)] 
    -- print $ (aroundFib 500) 42 == [('0',6),('2',7),('2',6)] 
    -- print $ (aroundFib 6000) 242 == [('5',31),('8',33),('8',31),('7',35),('7',31),('4',7)] 
 
    -------Ex 3---------
    print $ numContentChildren [1, 2, 3] [1, 1] == 1 
    print $ numContentChildren [1, 2] [1, 2, 3] == 2


-------Ex 1---------

numDrink :: Int -> Int -> Int
numDrink numBottles numExchange
 | numBottles < numExchange = numBottles
 | otherwise = numExchange + numDrink (numBottles - numExchange + 1) numExchange

--------------------

-------Ex 2---------

fib:: Int -> Int
fib n = fibHelper n 0 1
 where
    fibHelper::Int -> Int -> Int -> Int
    fibHelper 0 first secound = secound
    fibHelper n first secound = fibHelper (n - 1) secound (first + secound)



--------------------

-------Ex 3---------

numContentChildren :: [Int] -> [Int] -> Int
numContentChildren [] _ = 0
numContentChildren _ [] = 0
numContentChildren gs ss
 | head ss >= head gs = 1 + numContentChildren (drop 1 gs) (drop 1 ss)
 | otherwise = 0 + numContentChildren (drop 1 gs) (drop 1 ss)

--------------------