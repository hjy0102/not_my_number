-- definition of a game

module Game where

-- to run: 
-- ghci
-- :load Game

type PlayerScore = (Char, Int, Int, Int)      -- name, win, loss, tie

-- A Player takes in a GameState and a Result and produces a A_Move
type Player = GameState -> Result -> A_Move
-- Bomb is where the 'mine' is located 
type Bomb = Int
-- A_Move is a choice on the board, represented by an Int
type A_Move = Int                       -- a move for a player to make
-- Decision is win, loss, tie
-- win 1
-- tie 0
-- lost -1
type Decision = Int
-- State is a tuple containing the lower and upper bounds of available moves
type State = (A_Move, A_Move)           -- (lower bound, upper bound)

data Action = Move A_Move State PlayerScore PlayerScore       -- do A_Move in State
            | Start                     -- returns Starting
-- Result 
-- EndOfGame takes the Decision, Player1 Score, Player2 Score
data Result = EndOfGame Int PlayerScore PlayerScore             -- end of game 
            | ContinueGame State [A_Move]      -- continue game and available spaces
            deriving (Eq, Show)

type GameState = Action -> Result

----------------- Not My Number Game ------------------

