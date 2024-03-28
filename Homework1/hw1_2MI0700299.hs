main :: IO()
main = do
    -- Тестове направени за тест на задача 1
    print $ numStepCombinations 2 == 2
    print $ numStepCombinations 3 == 3
    print $ numStepCombinations 100 == 573147844013817084101

    -- Тестове направени за тест на задача 2
    print $ maxPersistenceMinSum 273 392 == 355
    print $ maxPersistenceMinSum 1000 2000 == 1679
    print $ maxPersistenceMinSum 55 105 == 77
    print $ maxPersistenceMinSum 195 756 == 679
    print $ maxPersistenceMinSum 2 85 == 77


numStepCombinations :: Integer -> Integer
numStepCombinations 1 = 1
numStepCombinations 2 = 2
numStepCombinations steps
 | steps < 0 = error "Incorrect input!"
 | otherwise = numStepCombinationsHelper 2 1 (steps - 2)
  where
    numStepCombinationsHelper x y 0 = x
    numStepCombinationsHelper x y n = numStepCombinationsHelper (x + y) x (n - 1)

multiplyNumbers :: Int -> Int -- функция сумираща цифрите в едно число
multiplyNumbers 0 = 0
multiplyNumbers number = multiplyNumbersHelper number 1
 where
    multiplyNumbersHelper 0 res = res
    multiplyNumbersHelper number res = multiplyNumbersHelper (div number 10) (res * mod number 10)

howPersistent :: Int -> Int -- функция показваща колко действия са били нужни за да се стигне до цифра
howPersistent number = howPersistentHelper number 0
 where
    howPersistentHelper number count
     | number < 10 = count
     | otherwise = howPersistentHelper (multiplyNumbers number) (count + 1)

sumNumbers :: Int -> Int -- сумира вскички цифри в едно число
sumNumbers number = sumNumbersHelper number 0
 where
    sumNumbersHelper 0 res = res
    sumNumbersHelper number res = sumNumbersHelper (div number 10) (res + mod number 10)

leastSumNumbers :: Int -> Int -> Int -- проверява и връща числото с най-малка съма на цифрите
leastSumNumbers number1 number2
 | sumNumbers number1 > sumNumbers number2 = number2
 | sumNumbers number2 > sumNumbers number1 = number1
 | otherwise = min number1 number2

maxPersistenceMinSum :: Int -> Int -> Int
maxPersistenceMinSum start end = maxPersistenceMinSumHelper start end 0 0
 where
    maxPersistenceMinSumHelper start end maxCount maxNumber
     | start > end = maxNumber
     | howPersistent start == maxCount = maxPersistenceMinSumHelper (start + 1) end maxCount (leastSumNumbers start maxNumber)
     | howPersistent start > maxCount = maxPersistenceMinSumHelper (start + 1) end (howPersistent start) start
     | otherwise = maxPersistenceMinSumHelper (start + 1) end maxCount maxNumber