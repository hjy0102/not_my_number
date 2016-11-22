-- definition of a Player and
-- the moves a Player can make

module Player where

-- to run: 
-- ghci
-- :load Player

type Player = (Int, Int, Int)			-- win, loss, tie

type A_Move = Int						-- a move for a player to make

type State = ([A_Move], [A_Move])		-- ([Player1], [Player2])

data Action = Move A_Move State 		-- do A_Move in State
			| Start						-- returns Starting

data Result = EndOfGame Int				-- end of game 
			| ContinueGame State [A_Move] 	-- continue game and available spaces

type Game = Action -> Result

type Player = Game -> Result -> A_Move
