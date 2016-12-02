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
 | checkBounds (lowerB, upperB) (lowerB + 5) = (lowerB + 5)
 | checkBounds (lowerB, upperB) (lowerB + 4) = (lowerB + 4)
 | checkBounds (lowerB, upperB) (lowerB + 1) = (lowerB + 1)
 | otherwise = upperB

computer_medium :: Computer
computer_medium (lowerB, upperB)
  | checkBounds (lowerB, upperB) ((((lowerB+upperB)*2) `div` 3) + lowerB)    = (((lowerB+upperB)*2) `div` 3) + lowerB
  | checkBounds (lowerB, upperB) (lowerB + (upperB `div` (upperB - lowerB))) = (lowerB + (upperB `div` (upperB - lowerB)))
  | otherwise = lowerB

checkBounds :: (Int, Int) -> Int -> Bool
checkBounds (low, high) x = (low < x ) && (x < high) 

computer_hard :: Computer
computer_hard (lowerB, upperB)
  | checkBounds (lowerB, upperB) ((lowerB+upperB+1) `div` 2) = (lowerB+upperB+1) `div` 2
  | otherwise = upperB
