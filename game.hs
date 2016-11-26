-- definition of a game

module Game where

import System.Random
import Control.Monad 
-- (replicateM)

-- to run: 
-- ghci
-- :load Game

type PlayerScore = (Char, Int, Int, Int)      -- name, win, loss, tie

-- A Player takes in a GameState and a Result and produces a A_Move
type Player = GameState -> Result -> A_Move
-- Bomb is where the 'mine' is located 
type Bomb = IO Int
-- A_Move is a choice on the board, represented by an Int
type A_Move = Int                       -- a move for a player to make

{- GameState is 
	([Lower Bound Array], [Upper Bound Array], 
	 [player1 moves],     [player2 moves])
-}
type GameState = ([A_Move], [A_Move], [A_Move], [A_Move])

-- Decision is win, loss, tie
-- win 1
-- tie 0
-- lost -1
type Decision = Int
-- State is a tuple containing the lower and upper bounds of available moves

type State = (A_Move, A_Move)           -- (lower bound, upper bound)

data Action = Move A_Move GameState       -- do A_Move in State
            | Start                       -- returns Starting
-- Result 
{-
TODO something to deal with after a move is made
-}
-- EndOfGame takes the Decision, Player1 Score, Player2 Score
data Result = EndOfGame Int PlayerScore PlayerScore             -- end of game 
            | ContinueGame GameState [A_Move]      -- continue game and available spaces
            deriving (Eq, Show)

type Game = Action -> Result

----------------- Not My Number Game ------------------

new_Bomb :: Bomb
new_Bomb = randomRIO (0,100::Int)




