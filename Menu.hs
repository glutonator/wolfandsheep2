module Menu where

import System.IO
import System.Exit
import Data.Binary
import Data.List
import Data.Ord
import Control.Exception
import System.Directory

import Data.Char
import DataStructures
import Board
import Alg

-- process comands during the game
processComands :: State -> IO()
processComands state = do
                  putStrLn "To move use command m nr_sheep l/r ****example: m 2 l "
                  putStrLn "n - start new game"
                  putStrLn "s - save current game"
                  putStrLn "f - list of existing saves"
                  putStrLn "e - exit game"
                  putStrLn "------------------------------------------------------"
                  (operationID:commandSettings) <- fmap words getLine
                  case operationID of
                    'm':_ -> do
                                  processSheepSteering state commandSettings
                    's':_ -> do 
                          save state
                          printBoard state 0 0
                          processComands state
                    'f':_ -> do 
                          listOfFiles
                          processComands state
                    'n':_ -> do 
                          putStrLn "new game"
                          printBoard [(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)] 0 0
                          processComands [(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]    
                    'e':_ -> do 
                          putStrLn "game exited"
                          menuLoop [(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]
                    _   -> do
                          putStrLn "Wrong command"
                          processComands state

-- process sheep steering command
processSheepSteering :: State -> [String] -> IO()
processSheepSteering state ((sheepIndex:_):(directionOfMovement:_):_)  = do
                                                                          steerSheep state (getSheepIndexAsAInt sheepIndex) directionOfMovement

-- get sheep index as int
getSheepIndexAsAInt :: Char -> Int
getSheepIndexAsAInt x | isDigit x == True = digitToInt x
                      | otherwise = 0

moveSheep :: State -> Int -> Int -> IO()
moveSheep state sheepIndex directionOfMovement = do
                                                  if (elem p (possibleOneSheepMoves sheepIndex state))
                                                    then do
                                                          -- move ship and rerender board
                                                          let temp = movepPointToNPositionSafe (sheepIndex + 1) p state
                                                          printBoard (temp) 0 0
                                                          let finalStateObject = getBestMove temp
                                                          let stateAftreWolfMove = getState finalStateObject

                                                          --warunek wygrania owiec
                                                          if (sheepsWin temp)
                                                            then do
                                                                  putStrLn "Sheep win"
                                                                  putStrLn "New game: "
                                                                  printBoard ([(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]) 0 0
                                                                  processComands ([(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)])
                                                            else do
                                                              putStrLn "Sheep did not win"
                                                              printBoard (stateAftreWolfMove) 0 0
                                                              --warunek wygrania wilka
                                                              -- if (getWinStateWolf finalStateObject)
                                                              if wolfWins stateAftreWolfMove  
                                                                then do
                                                                  putStrLn "Wolf win"
                                                                  putStrLn "New game: "
                                                                  printBoard ([(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]) 0 0
                                                                  processComands ([(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)])
                                                                else processComands (movepPointToNPositionSafe (sheepIndex + 1) p stateAftreWolfMove)

                                                  else 
                                                      do
                                                        putStrLn "Move is not possible"
                                                        processComands state
                                                  where possibleMoves = possibleOneSheepMoves sheepIndex state
                                                        xCurrent = xPoint ((sheepsPos state) !! sheepIndex)
                                                        xNext = xCurrent + directionOfMovement
                                                        yCurrent = yPoint ((sheepsPos state) !! sheepIndex)
                                                        yNext = yCurrent + 1
                                                        p = Point xNext yNext

-- steer sheep
steerSheep :: State -> Int -> Char -> IO()
steerSheep state sheepIndex directionOfMovement = do
                                              if elem sheepIndex [0..3]
                                              then case [directionOfMovement] of
                                                'l':_ -> moveSheep state sheepIndex (-1)
                                                'r':_ -> moveSheep state sheepIndex 1
                                                _ -> do
                                                      putStrLn "Wrong command 1"
                                              else
                                                putStrLn "Wrong command 2"



menuLoop defaultState = do 
    --clear screen --commented for testing
    -- putStr "\ESC[2J"
    --
    putStrLn "n - start new game"
    putStrLn "l - load saved game"
    putStrLn "f - list of existing saves"
    putStrLn "b - create folder for saves"
    putStrLn "e - exit game"
    putStrLn "------------------------------------------------------"
    putStr "Your decision: "
    input <- getChar
    putStrLn "\n"
    case input of 
        'n' -> do 
          printBoard defaultState 0 0
          processComands defaultState
        'l' -> do 
          fileout <- loadFile
          printBoard fileout 0 0
          processComands fileout
        'f' -> do 
          listOfFiles
          menuLoop defaultState
        'b' -> do createSaveDir
        'e' -> do putStrLn "game exited"

        _ -> (menuLoop defaultState)



listOfFiles = do
    rrr <- listDirectory "./savefiles"
    putStrLn(show rrr)

createSaveDir = createDirectory "./savefiles"


--tutaj są dobre funckje
save points = do
    putStrLn "Write file name:"
    putStr "Write here: "
    hFlush stdout
    filename <- getLine
    writeFile ("savefiles/"++filename) (show points)
    putStrLn ("Saved")

loadFile = do
    putStrLn "Write file name:"
    putStr "Write here: "
    hFlush stdout
    filename <- getLine
    dataInFile <- readFile ("savefiles/"++filename)
    let points :: [Point]
        points = read dataInFile
    return points
