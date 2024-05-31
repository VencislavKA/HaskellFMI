main::IO()
main = do
    print $ (func (+1) [1,1]) 1 == 6
    print $ (func (*2) [4,5]) 1 == 26

func :: (Int -> Int) -> [Int] -> (Int -> Int)
func f lst = (\x -> helper x f lst 0)
 where
    helper _ _ [] sum = sum
    helper x f (l:lst) sum = helper x f lst $ sum + (length lst + 1) * f (l * x)

