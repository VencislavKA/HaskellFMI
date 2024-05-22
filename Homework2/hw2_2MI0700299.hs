import Data.List
main :: IO()
main = do
    -- Ex 1 --
    print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    print $ warmerAfter [21,22,23] == [1,1,0]
    print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

    -- Ex 2 --

    print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
    print $ (setupRobots [-2, 0, 2] "RLL") 2 == [-2, 0, 0]
    print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3] 
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14] 
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15] 
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17]

    ----------- MY TESTS -----------
    print $ length (zip [-2, 0, 2] "RLL") == 3

    print $ isRobotAgainstAndOnePosAway (1,'R') (zip [-2, 0, 2] "RLL")

    print $ isRobotAgainstAndTwoPosAway (0,'R') (zip [-2, 2] "RL")

    print $ changeRobotDirOnPos 2 (zip [-2, 0, 2] "RLL") == [(-2,'R'),(0,'L'),(2,'R')]
    print $ changeRobotDirOnPos (-2) (zip [-2, 0, 2] "RLL") == [(-2,'L'),(0,'L'),(2,'L')]
    print $ changeRobotDirOnPos 2 (changeRobotDirOnPos (-2) (zip [-2, 0, 2] "RLL")) == [(-2,'L'),(0,'L'),(2,'R')]

    print $ moveRobot (2,'L') (zip [-2, 0, 2] "RLL") == [(-2,'R'),(0,'L'),(1,'L')]
    print $ moveRobot (-2,'R') (zip [-2, 0, 2] "RLL") == [(-1,'R'),(0,'L'),(2,'L')]

    ----------- MY TESTS -----------

-- Ex 1 --

-- A function that counts the days until a day with a higher temperature.
countDaysUntilWarmer :: Double -> [Double] -> Int
countDaysUntilWarmer curDay [] = 0
countDaysUntilWarmer curDay days
 | length days == length (takeWhile (<= curDay) days) = 0
 | otherwise = (1+) . length $ takeWhile (<= curDay) days


warmerAfter :: [Double] -> [Int]
warmerAfter [] = []
warmerAfter (x:xs) = countDaysUntilWarmer x xs : warmerAfter xs

-- Ex 2 --

-- A function that checks if there is a robot one position away from the current one and if it is against it
isRobotAgainstAndOnePosAway :: (Int,Char) -> [(Int,Char)] -> Bool
isRobotAgainstAndOnePosAway robot = any (\(pos,dir) -> dir == 'L' && snd robot == 'R' && pos == (fst robot + 1))

-- A function that checks if there is a robot two position away from the current one and if it is against it
isRobotAgainstAndTwoPosAway :: (Int,Char) -> [(Int,Char)] -> Bool
isRobotAgainstAndTwoPosAway robot = any (\(pos,dir) -> dir == 'L' && snd robot == 'R' && pos == (fst robot + 2))

-- A function that checks if there is a robot position
isTwoRobotsOnOnePos :: Int -> [(Int,Char)] -> Bool
isTwoRobotsOnOnePos fpos = (2 ==) . length . filter (\(pos,dir) -> pos == fpos)

-- Function changing the direction of the robot at a specific position
changeRobotDirOnPos :: Int -> [(Int,Char)] -> [(Int,Char)]
changeRobotDirOnPos fpos = map (\(pos,dir) -> if pos == fpos then (if dir == 'L' then (pos,'R') else(pos,'L')) else (pos,dir))

moveRobot :: (Int,Char) -> [(Int,Char)] -> [(Int,Char)]
moveRobot robot = map (\(pos,dir) -> (if pos == fst robot && dir == snd robot then (if dir == 'L' then pos - 1 else pos + 1) else pos,dir))

-- Function that moves the robots
moveRobots :: [(Int,Char)] -> Int -> [Int]
moveRobots zipped sec = moveRobotsHelper zipped sec 0
 where
    moveRobotsHelper :: [(Int,Char)] -> Int -> Int  -> [Int]
    moveRobotsHelper zipped 0 curPos = map fst zipped
    moveRobotsHelper zipped sec curPos
     | curPos >= length zipped = moveRobotsHelper zipped (sec - 1) 0
     | isRobotAgainstAndOnePosAway (zipped !! curPos) zipped = moveRobotsHelper (if snd (zipped !! curPos) == 'L'
            then changeRobotDirOnPos (fst (zipped !! curPos)) (changeRobotDirOnPos (fst (zipped !! curPos) - 1) zipped )
            else changeRobotDirOnPos (fst (zipped !! curPos)) (changeRobotDirOnPos (fst (zipped !! curPos) + 1) zipped )) sec (curPos + 2)
     | isRobotAgainstAndTwoPosAway (zipped !! curPos) zipped = moveRobotsHelper (if snd (zipped !! curPos) == 'L'
            then changeRobotDirOnPos (fst (zipped !! curPos) - 1) (moveRobot (zipped !! curPos) (moveRobot (fst (zipped !! curPos) - 2,'R') zipped))
            else changeRobotDirOnPos (fst (zipped !! curPos) + 1) (moveRobot (zipped !! curPos) (moveRobot (fst (zipped !! curPos) + 2,'L') zipped))) sec (curPos + 2)
     | otherwise = moveRobotsHelper (moveRobot (zipped !! curPos) zipped) sec (curPos + 1)

setupRobots :: [Int] -> String -> (Int -> [Int])
setupRobots pos lr = moveRobots (zip pos lr)