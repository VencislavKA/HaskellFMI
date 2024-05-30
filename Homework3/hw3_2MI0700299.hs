import Data.List
import Data.Char
main::IO()
main = do
    print $ map digitToInt "8504156"
--     print $ takeWhile (not . isPrefixOf "$ cd ") $ tail commands
--     print $ splitByDirectory commands

-- splitByDirectory :: [Command] -> [[Command]]
-- splitByDirectory [] = []
-- splitByDirectory commands = takeWhile (not . isPrefixOf "$ cd ") (tail commands) : splitByDirectory (dropWhile (not . isPrefixOf "$ cd ") (tail commands))

    print $ isDir "dir a"
    print $ not (isDir "$ ls")

    --print $ generateFileSystem commands -- == Directory "/" [Directory "a" [Directory "e" [File "i" 584], File "f" 29116, File "g" 2557, File "h.lst" 62596], Directory "d" [File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174, File "k" 7214296], File "b.txt" 14848514, File "c.dat" 8504156]


generateFileSystem :: [Command] -> FileSystem
generateFileSystem (cmd:commands)
     | isCdCreate cmd = Directory (last $ words cmd) [generateFileSystem commands]
     | isDir cmd = Directory (last $ words cmd) [generateFileSystem commands]
     | otherwise = File (last $ words cmd) (read $ head $ words cmd)

isCdUp :: Command -> Bool
isCdUp command = head parts == "$" && parts !! 1 == "cd" && last parts == ".."
 where
    parts = words command

isCdCreate :: Command -> Bool
isCdCreate command = head parts == "$" && parts !! 1 == "cd" && last parts /= ".."
 where
    parts = words command

isDir :: Command -> Bool
isDir = isPrefixOf "dir "

type Command = String
type Size = Int
type Name = String

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

data FileSystem = Directory Name [FileSystem] | File Name Size
 deriving (Eq, Show)

