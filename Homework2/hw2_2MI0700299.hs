import Data.List
main :: IO()
main = do
    -- print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0] 
    -- print $ warmerAfter [0,10,20,30] == [1,1,1,0] 
    -- print $ warmerAfter [21,22,23] == [1,1,0] 
    -- print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

    -- print $ daysUntilWarmer 23 [24,25,21,19,23,26,23] == 1
    -- print $ daysUntilWarmer 25 [21,19,23,26,23] == 4
    -- print $ daysUntilWarmer 26 [23] == 0

    -- print $ (setupRobots [0, 1] "LR") 3 -- == [-3, 4]
    print $ (setupRobots [-2, 0, 2] "RLL") 2 -- == [-2, 0, 0]
    -- print $ (setupRobots [-2, 0, 2] "RLL") 5 -- == [-5, -3, 3]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 -- == [-1,-1,0,2,5,8,9,13,14]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 -- == [-3,-2,0,1,7,7,10,12,15]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 -- == [-5,-4,-2,3,5,9,10,12,17]

-- Ex1
warmerAfter :: [Double] -> [Int]
warmerAfter [] = []
warmerAfter (x:xs) = daysUntilWarmer x xs : warmerAfter xs

daysUntilWarmer :: Double -> [Double] -> Int
daysUntilWarmer today xs = daysUntilWarmerHelper xs 0
 where
    daysUntilWarmerHelper [] _ = 0
    daysUntilWarmerHelper (x:xs) count
     | today < x = count + 1
     | otherwise = daysUntilWarmerHelper xs (count + 1)


-- Ex2
changeRobotDir :: Int -> [(Int,Char)] -> [(Int,Char)]
changeRobotDir fpos = map (\(pos,dir) -> if pos == fpos then (pos,if dir == 'L' then 'R' else 'L') else (pos,dir))

isRobotOnPos :: Int -> [(Int,Char)] -> Bool
isRobotOnPos fpos = elem fpos . map fst

getRobotDirOnPos :: Int -> [(Int,Char)] -> Char
getRobotDirOnPos fpos = head . map snd . filter (\(pos,dir) -> pos == fpos)

moveRobotOnPos :: Int -> [(Int,Char)] -> [(Int,Char)]
moveRobotOnPos fpos = map (\(pos,dir) -> if pos == fpos then (if dir == 'L' then (pos - 1,dir) else (pos + 1,dir)) else (pos,dir) )

-- [(-2,'R'),(0,'L'),(2,'L')] "RLL"
-- [(-2,'R'),(0,'R'),(1,'L')]
-- [(-1,'R'),(0,'L'),(1,'R')]
-- []
moveRobots :: [(Int,Char)] -> Int -> [(Int,Char)]
moveRobots zipped sec = moveRobotsHelper zipped sec 0
 where
    moveRobotsHelper :: [(Int,Char)] -> Int -> Int -> [(Int,Char)]
    moveRobotsHelper zipped 0 _ = zipped -- map fst zipped
    moveRobotsHelper zipped sec curPos
     | curPos >= length zipped = moveRobotsHelper zipped (sec - 1) 0
     | map snd zipped !! curPos == 'L' = if isRobotOnPos ((map fst zipped !! curPos) - 2) zipped && getRobotDirOnPos ((map fst zipped !! curPos) - 2) zipped == 'R' 
            then moveRobotsHelper (changeRobotDir (map fst zipped !! curPos) (changeRobotDir ((map fst zipped !! curPos) - 2) zipped)) sec (curPos + 1) 
            else if isRobotOnPos ((map fst zipped !! curPos) - 1) zipped && getRobotDirOnPos ((map fst zipped !! curPos) - 1) zipped == 'R'
                    then moveRobotsHelper (changeRobotDir (map fst zipped !! curPos) (changeRobotDir ((map fst zipped !! curPos) - 1) zipped)) sec (curPos + 1) 
                    else moveRobotsHelper (moveRobotOnPos (map fst zipped !! curPos) zipped) sec (curPos + 1)
     | otherwise = if isRobotOnPos ((map fst zipped !! curPos) + 2) zipped && getRobotDirOnPos ((map fst zipped !! curPos) + 2) zipped == 'L' 
            then moveRobotsHelper (changeRobotDir (map fst zipped !! curPos) (changeRobotDir ((map fst zipped !! curPos) + 2) zipped)) sec (curPos + 1) 
            else if isRobotOnPos ((map fst zipped !! curPos) + 1) zipped && getRobotDirOnPos ((map fst zipped !! curPos) + 1) zipped == 'L' 
                    then moveRobotsHelper (changeRobotDir (map fst zipped !! curPos) (changeRobotDir ((map fst zipped !! curPos) + 1) zipped)) sec (curPos + 1) 
                    else moveRobotsHelper (moveRobotOnPos (map fst zipped !! curPos) zipped) sec (curPos + 1)

setupRobots :: [Int] -> String -> (Int -> [(Int,Char )])
setupRobots pos lr = moveRobots (zip pos lr)

 -- proveri ne samo dali ima robot no i dali gleda kum posoko na koqto 6te se sre6tnat