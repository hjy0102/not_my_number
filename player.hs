-- definition of a Player and
-- the moves a Player can make

module Player where

-- to run: 
-- ghci
-- :load Player
import Game

-- A Player takes in a GameState and a Result and produces a A_Move
type Player = GameState -> Result -> A_Move

