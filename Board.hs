module Board where

import DataStructures

-- wolf ascii representation
wolfChar = "w"

-- sheep ascii representation
sheepChar = "s"

-- black field
blackChar = "-"

-- printing board based on input date
printBoard :: State -> Int -> Int -> IO ()
printBoard state boardX boardY  | boardY < 8 = do 
                                    putStrLn ((printRow state boardX boardY) ++ "|")
                                    printBoard state 0 (boardY + 1)
                                | otherwise = putStrLn ""

-- printing one row of board
printRow :: State -> Int -> Int -> String
printRow state boardX boardY | boardX < 8 = "|" ++ (printField state boardX boardY) ++ (printRow state (boardX + 1) boardY)
                             | otherwise = ""

-- printing field of board
printField :: State -> Int -> Int -> String
printField state boardX boardY | (boardX `mod` 2 == 0) && (boardY `mod` 2 == 0) || (boardX `mod` 2 /= 0) && (boardY `mod` 2 /= 0) = "_"
                               | otherwise = drawAnimalIfThereIsOne state boardX boardY

-- draw animal on board, if there is one
drawAnimalIfThereIsOne :: State -> Int -> Int -> String
-- if current column and row has wolf, draw it
drawAnimalIfThereIsOne ((Point wolfX wolfY):sheeps) boardX boardY | (boardX == wolfX && boardY == wolfY) = wolfChar
                                                    | otherwise = drawSheep sheeps boardX boardY 0

-- draw sheeps
drawSheep :: [Point] -> Int -> Int -> Int -> String
drawSheep [] _ _ _ = blackChar
drawSheep ((Point sheepX sheepY):remainingSheeps) boardX boardY sheepIndex  | (sheepX == boardX && sheepY == boardY) = show sheepIndex
                                                                            | otherwise = drawSheep remainingSheeps boardX boardY (sheepIndex + 1)
