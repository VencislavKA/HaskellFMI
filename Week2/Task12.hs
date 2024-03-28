main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98
    print $ findSum 0 0 4 == 0 -- my test

findSum:: Int -> Int -> Int -> Int
findSum a b n = findSumHelper a b n 0 + findSumHelper a b (n - 1) 0 + findSumHelper a b (n - 2) 0
 where
    findSumHelper a b 0 sum = sum + a
    findSumHelper a b n sum = findSumHelper a b (n - 1) (2^(n - 1) * b + sum )