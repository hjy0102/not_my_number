-- CPSC 312 - 2016
-- Programming Project 2

module NotMyNumber where

-- to run: 
-- ghci
-- :load NotMyNumber

import System.IO 
import Player
import Game
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random

-- type Leaderboard = 	(PlayerScore, PlayerScore)		-- Player1 score, Player2 score

notMyNumber :: IO ()
notMyNumber = do
	args <- getArgs
	checkArgs args
	seed <- getSeed args
	playGame $ getRandomGen seed
	putStrLn "Game Over"

-- create a random generator with the seed given from args seed 
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed 

-- if there's a seed, use that
-- otherwise, getRandomSeed
-- seeds are strings ["123493"] that look like that
getSeed :: [String] -> IO Int
getSeed [] = getRandomSeed