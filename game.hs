-- definition of a game

module Game where

import System.Random
import Control.Monad (replicateM)
import Data.Set (Set)
import qualified Data.Set as Set

-- to run: 
-- ghci
-- :load Game

type PlayerScore = (Char, Int, Int)      -- name, win, loss
-- A Player takes in a GameState and a Result and produces a A_Move
type Player = GameState -> Result -> A_Move
-- Bomb is where the 'mine' is located 
type Bomb = IO Int
-- A_Move is a choice on the board, represented by an Int
type A_Move = Int                       -- a move for a player to make

{- GameState is 
	(LowerBound,	 UpperBound
-}
type GameState = (A_Move, A_Move)

data Decision = Win | Lose
			deriving (Eq, Show)

data Action = Move A_Move GameState       -- do A_Move in GameState
            | Start                       -- returns Starting
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

new_Bomb :: Bomb
new_Bomb = randomRIO (0,100::Int)

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
notMyNumber (Move aMove (lowerB, upperB))
	| aMove == new_Bomb 				    = EndOfGame Lose 
	| validMove aMove lowerB upperB		    = adjustBounds aMove lowerB upperB

validMove aMove lowerB upperB = (aMove > lowerB) && (aMove < upperB)

adjustBounds aMove lowerB upperB
	| aMove < new_Bomb  	= ContinueGame (lowerB, aMove) 
	| aMove > new_Bomb  	= ContinueGame (aMove, upperB)


