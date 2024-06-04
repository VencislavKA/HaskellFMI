main :: IO()
main = do
    print 123

data NTree a = Nil | Node a [NTree a] deriving (Eq, Show)