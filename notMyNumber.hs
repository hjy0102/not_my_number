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
	playNewGame $ getRandomGen seed
	putStrLn "Game Over"

-- create a random generator with the seed given from args seed 
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed 

-- if there's a seed, use that
-- otherwise, getRandomSeed
-- seeds are strings ["123493"] that look like that

{-- 
$ is infix "application". It's defined as

($) :: (a -> b) -> (a -> b)
f $ x = f x

-- or 
($) f x = f x
-- or
($) = id

From [stackoverflow]
--}
getSeed :: [String] -> IO Int
getSeed [] = getRandomSeed       -- helper function to generate a seed 
getSeed (s:_) = return $ read s

-- use the random generator from System.Random to get a random seed
-- this is the solution to the problem about IO Int and getting
-- an int from it to make a new bomb (WARNING: not the best solution)
getRandomSeed :: IO Int
getRandomSeed = do
	my_RandomS <- getStdGen      
	return $ fst $ System.Random.random $ my_RandomS

-- this is how to start the new Game 
playNewGame :: StdGen -> IO ()
playNewGame n = do
	putStrLn $ "\n Welcome to NotMyNumber!"
	putStrLn $ "\n The objective of the game is not to be the player to find the bomb"
	putStrLn $ "\n The bomb is hidden in the field. Guess a number between 0 and" ++ (show (maxNum - 1)) ++ " to begin"
	let (inTargetNumber, newGen) = next n 
	let target = mod inTargetNumber maxNum
	guessFor target 0
	showBomb target
	again <- playAgain
	if again 
		then playNewGame newGen
		else quitPlaying

-- guessFor handles all the guessing
guessFor :: Int -> Int -> IO ()
























