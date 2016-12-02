-- definition of a Player and
-- the moves a Player can make

module Player where

-- to run: 
-- ghci
-- :load Player
import System.IO 
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random
import Game

-- name, win, lose 
type PlayerScore = (String, Int, Int)
setPlayerScore :: String -> Int -> Int -> PlayerScore
setPlayerScore name win lose = (name, win, lose)

getPlayerName :: PlayerScore -> String
getPlayerName (name, win, loss) = name 

getPlayerWin :: PlayerScore -> Int 
getPlayerWin (name, win, loss) = win 

getPlayerLoss :: PlayerScore -> Int
getPlayerLoss (name, win, loss) = loss 

type Computer = (Int, Int) -> Int
computer_easy :: Computer
computer_easy (lowerB, upperB) 
 | (lowerB + 5)<upperB = (lowerB + 5)
 | otherwise = upperB
computer_medium :: Computer
computer_medium (lowerB, upperB) = ((lowerB+upperB)*2) `div` 3
computer_hard :: Computer
computer_hard (lowerB, upperB) = (lowerB+upperB+1) `div` 2
