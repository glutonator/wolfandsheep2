module Game where

import System.IO
import Board
import DataStructures
import Menu

-- defaultState :: State
-- defaultState = [(Point 3 6), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]

-- defaultState = [(Point 0 3), (Point 2 1), (Point 2 3), (Point 1 4), (Point 7 0)]

-- defaultState = [(Point 1 0), (Point 0 1), (Point 2 1), (Point 1 4), (Point 7 0)]

-- defaultState = [(Point 0 3), (Point 2 1), (Point 2 3), (Point 1 4), (Point 7 0)]
defaultState :: State
defaultState = [(Point 0 7), (Point 1 0), (Point 3 0), (Point 5 0), (Point 7 0)]


-- new game
newGame :: IO()
newGame = do
            menuLoop defaultState

