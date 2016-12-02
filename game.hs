-- score keeping
module Game where

import System.IO 
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random

-- GameState is ((LowerBound, UpperBound), bomb)
type GameState = ((Int, Int), Int)
setGameState :: (Int, Int) -> Int -> GameState
setGameState bound bomb = (bound, bomb)

