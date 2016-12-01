-- definition of a game

module Game where

import System.Random
import Control.Monad (replicateM)
import Data.Set (Set)
import qualified Data.Set as Set

-- to run: 
-- ghci
-- :load Game
 
type PlayerRecord = (Int, Int) -- win, loss
type PlayerScore = (Char, Int, Int)      -- name, win, loss
-- A Player takes in a GameState and a Result and produces a A_Move
type Player = GameState -> Result -> A_Move
-- Bomb is where the 'mine' is located 
{- Change
   type Bomb = IO Int to type Bomb = Int
   Thinking the notMyNumber.hs can pass in a random bomb.
-}
type Bomb = Int
-- A_Move is a choice on the board, represented by an Int
type A_Move = Int                       -- a move for a player to make
-- Bound is (lowerBound, upperBound). Using tuple for readability.
type Bound = (A_Move, A_Move)

--GameState is ((LowerBound, UpperBound), bomb)
--type GameState = (A_Move, A_Move)
type GameState = (Bound, Bomb)

data Decision = Win | Lose
            deriving (Eq, Show)

data Action = Move A_Move GameState       -- do A_Move in GameState
            | Start GameState                      -- returns Starting
-- Result 
{-
TODO something to deal with after a move is made
-}
-- EndOfGame takes decision, Player1 Score, Player2 Score
data Result = EndOfGame Decision              -- end of game 
            | ContinueGame GameState          -- continue game and new upper and lower bounds
            deriving (Eq, Show)

type Game = Action -> Result
----------------- Player ------------------
-- player1 :: Player

-- player2 :: Player
----------------- Not My Number Game ------------------
{- Comment out this temporarily to reorganize the design-}
--new_Bomb :: Bomb
--new_Bomb = randomRIO (0,100::Int)

notMyNumber :: Game 
{-- 
aMove is not new_Bomb
aMove is greater than lowerB and less than upperB

then 
	if aMove < new_Bomb, then aMove becomes new upperBound
	if aMove > new_Bomb, then aMove becomes new lowerBound
	add aMove to p1 list and switch p1 and p2 (next person's turn)
--}
-- Move A_Move GameState
notMyNumber (Move aMove ((lowerB, upperB), bomb))
 | aMove == bomb                         = EndOfGame Lose 
 | validMove aMove lowerB upperB         = adjustBounds aMove (lowerB, upperB) bomb

notMyNumber Start ((lowerB, upperB),bomb) = ContinueGame ((lowerB, upperB),bomb)

validMove aMove lowerB upperB = (aMove > lowerB) && (aMove < upperB)

adjustBounds aMove (lowerB, upperB) bomb
 | aMove < bomb  = ContinueGame ((aMove, lowerB), bomb)
 | aMove > bomb  = ContinueGame ((lowerB, aMove), bomb)


