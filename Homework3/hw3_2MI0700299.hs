import Data.List
import Data.Char
main::IO()
main = do
    -- test from ex1
    print $ generateFileSystem commands == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]
    -- test from ex2
    print $ getParentSize (generateFileSystem commands) "i"     == 584
    print $ getParentSize (generateFileSystem commands) "g"     == 94853
    print $ getParentSize (generateFileSystem commands) "b.txt" == 48381165
    print $ getParentSize (generateFileSystem commands) "abc"   == -1

type Command = String
type Size = Int
type Name = String

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

data FileSystem = Directory Name [FileSystem] | File Name Size
 deriving (Eq, Show)

------------------ ex1 --------------------------

generateFileSystem :: [Command]  -> FileSystem
generateFileSystem cmds = (\(dirName,sub) -> Directory dirName (map (\x -> createData (splitByDirectory cmds) x) sub )) $ head $ splitByDirectory cmds

-- function that creates the system objects
createData :: [(String,[Command])] -> String -> FileSystem
createData base str
 | isPrefixOf "dir " str = (\(dirName,sub) -> Directory dirName $ map (\x -> createData (splitByDirectory commands) x) sub ) $ head $ filter (\(name,_) -> name == systemObjectName) base
 | otherwise = File systemObjectName (stringToInt $ head $ words str)
 where
    systemObjectName = last $ words str

-- function that converts string to int
stringToInt :: String -> Int
stringToInt = foldl (\res x -> res * 10 + x) 0 . map digitToInt

-- function that sorts the comands as needed in ex 1
sortCommands :: [Command] -> [Command]
sortCommands = sortOn (not . isPrefixOf "dir ") . sortOn (last . words . map toLower)

-- romoves the command "$ ls". It is useless for the creation
filterLs :: [Command] -> [Command]
filterLs = filter (\x -> x /= "$ ls")

-- function that splits all command on diferent "cd " command
splitByDirectory :: [Command] -> [(String,[Command])]
splitByDirectory [] = []
splitByDirectory commands = [( directoryName , children )] ++ splitByDirectory (dropWhile (isNotCdCommand) (tail commands))
 where
    isNotCdCommand = not . isPrefixOf "$ cd "
    directoryName = last $ words $ head commands
    children = sortCommands . filterLs $ takeWhile (isNotCdCommand) (tail commands)

------------------ ex2 --------------------------

isContaing :: Name -> FileSystem -> Bool
isContaing name (File fname _) = fname == name
isContaing _ _ = False

sizeOfDirectory :: FileSystem -> Size
sizeOfDirectory (File _ size) = size
sizeOfDirectory (Directory _ sub) = sum $ map sizeOfDirectory sub

getParentSize :: FileSystem -> Name -> Size
getParentSize system fn = if all (== 0) $ helper system fn then (-1) else sum $ helper system fn
 where
    helper :: FileSystem -> Name -> [Size]
    helper (File name size) fname
     | fname == name = [size]
     | otherwise = [0]
    helper (Directory _ sub) fname
     | any (isContaing fname) sub = [sizeOfDirectory $ Directory "/" sub]
     | otherwise = concatMap (\x -> helper x fname) sub
