main :: IO()
main = do
    print 123

data Color = Red | Green | Blue -- цвят
 deriving (Read, Show, Eq)
data Tree = Empty | Node Color Tree Tree