-- CPSC 312 - 2016
-- Programming Project 2

module NotMyNumber where

-- to run: 
-- ghci
-- :load NotMyNumber

import System.IO 
import Player
import Game

type Leaderboard = 	(PlayerScore, PlayerScore)		-- Player1 score, Player2 score

