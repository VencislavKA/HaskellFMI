import Data.List
import Data.Char
main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "(o^") 2 "^o)" == "(o^^o)(o^" -- my test

-- repeater :: String -> Int -> String -> String
-- repeater str count glue = repeaterHelper "" count
--  where
--     repeaterHelper res 1 = res ++ str
--     repeaterHelper res count = repeaterHelper (res ++ str ++ glue) (count - 1)

repeater :: String -> (Int -> String -> String)
repeater str count glue = if count == 1 then str else str ++ glue ++ repeater str (count - 1) glue
