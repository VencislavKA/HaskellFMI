main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1
    print $ (applyN (\x -> x - 2) 1) 2 == 0 -- my test

-- applyN :: (Int -> Int) -> Int -> Int -> Int
-- applyN func count = applyNHelper func $ count - 1
--  where
--     applyNHelper f 0 = f
--     applyNHelper f count = applyNHelper (f . func) (count - 1)

applyN :: (Eq a,Num a) => (a -> a) -> a -> (a -> a)
applyN f 0 = id
applyN f n = applyN f (n - 1) . f