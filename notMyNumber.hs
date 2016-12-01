-- CPSC 312 - 2016
-- Programming Project 2

module NotMyNumber where

-- to run: 
-- ghci
-- :l NotMyNumber

import System.IO 
-- import Player
-- import Game
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random
---------------------------------------------------------------------------
-- name, win, lose 
type PlayerScore = (String, Int, Int)
setPlayerScore :: String -> Int -> Int -> PlayerScore
setPlayerScore name win lose = (name, win, lose)

getPlayerName :: PlayerScore -> String
getPlayerName (name, win, loss) = name 

getPlayerWin :: PlayerScore -> Int 
getPlayerWin (name, win, loss) = win 

getPlayerLoss :: PlayerScore -> Int
getPlayerLoss (name, win, loss) = loss 

-- GameState is ((LowerBound, UpperBound), bomb)
type GameState = ((Int, Int), Int)
setGameState :: (Int, Int) -> Int -> GameState
setGameState bound bomb = (bound, bomb)

---------------------------------------------------------------------------
minNum = 0
maxNum = 101

start :: IO ()
start = do
  let range = (minNum, maxNum)
  args <- getArgs
  checkArgs args range
  putStrLn "Player1's name ?  "
  name <- getLine
  let player1     = setPlayerScore name 0 0
  putStrLn "Player2's name ?  "
  name2 <- getLine
  let comp_player = setPlayerScore name2 0 0
  let tournament_state = (player1, comp_player)
  seed <- getSeed args
  playNewGame player1 comp_player range $ getRandomGen seed
  putStrLn "Game Over. "

-- create a random generator with the seed given from args seed 
getRandomGen :: Int -> StdGen
getRandomGen seed = mkStdGen seed 

-- if there's a seed, use that
-- otherwise, getRandomSeed
-- seeds are strings ["123493"] that look like that
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
playNewGame :: PlayerScore -> PlayerScore -> (Int, Int) -> StdGen -> IO ()
playNewGame p1 p2 range n = do
    putStrLn $ "\nWelcome to NotMyNumber!"
    putStrLn $ "\nThe objective of the game is not to be the player to find the bomb. "
    putStrLn $ "The bomb is hidden in the field. Guess a number between " ++ (show ((fst range) + 1)) ++ " and " ++ (show ((snd range) - 1 ))  ++ " to begin."
    let (inTargetNumber, newGen) = next n 
    let bomb = mod inTargetNumber ((snd range)+1)
    let gameState = setGameState range bomb
    
    putStrLn $ "Who starts? 0 = " ++ (show (getPlayerName p1)) ++ ", 1 = " ++ (show (getPlayerName p2)) ++ ", 2 = exit."
    starter <- getLine
    setFirstPlayer starter p1 p2 bomb range
    showBomb bomb 
    putStrLn $ "\nThe Score: " ++ show (getPlayerName p1) ++ " has " ++ show (getPlayerWin p1) ++ " wins and " ++ show (getPlayerLoss p1) ++ " losses."
    putStrLn $ "\nThe Score: " ++ show (getPlayerName p2) ++ " has " ++ show (getPlayerWin p2) ++ " wins and " ++ show (getPlayerLoss p2) ++ " losses."
    again <- playAgain
    if again 
        then playNewGame p1 p2 range newGen
        else quitPlaying

setFirstPlayer line p1 p2 bomb range
  | ((read line:: Int) == 0)   = guessFor p1 p2 bomb 0 0 range
  | ((read line:: Int) == 1)   = guessFor p2 p1 bomb 0 0 range
  | ((read line:: Int) == 2)   = quitPlaying


-- guessFor handles all the guessing
-- guessFor takes in: player1 player2 
-- bomb location
-- attempts of player1, attempts of player2
-- acceptable range

guessFor :: PlayerScore -> PlayerScore -> Int -> Int -> Int -> (Int, Int) -> IO ()
guessFor p1 p2 bomb count_p1 count_p2 range = do
  whoseTurn p1
  showRange range
  guess <- getNumber "That's not in the range! Guess again:  " range
  if bomb == guess
    then foundBomb p1 $ count_p1 + 1
    else missedBomb p1 p2 bomb count_p1 count_p2 range guess 

whoseTurn :: PlayerScore -> IO ()
whoseTurn p1 = putStrLn $ "Whose turn: " ++ show (getPlayerName p1)

-- keeps asking for a number 
getNumber :: String -> (Int, Int) -> IO Int 
getNumber promptAgain range = 
  getFromStdin promptAgain getLine (isNum range) read 

-- foundBomb is what happens when you find the bomb
-- not good :( 
foundBomb :: PlayerScore -> Int -> IO ()
foundBomb player count = do
  let name = getPlayerName player
  let win = getPlayerWin player 
  let loss  = getPlayerLoss player
  let new_Player = setPlayerScore name win (loss+1)
  putStrLn $ show new_Player
  putStrLn "    BOOM  *&@&!^#&@!*(#@!!    You found the bomb."
  putStrLn $ show name ++ " died in " ++ show count ++ " turns."

-- missedBomb lets the players keep guessing
{-- TODO: handle changing the array of possible moves
!!!
The too low or too high is temporary for testing only
missedBomb p1 p2 bomb count range guess 

-- guessFor takes in:
-- player1 player2 
-- bomb location
-- attempts of player1, attempts of player2
-- acceptable range
-- returns IO ()
--}
missedBomb p1 p2 bomb count_p1 count_p2 (lower, upper) guess = do
  if bomb > guess
      then do 
        putStrLn "too low"
        guessFor p2 p1 bomb count_p2 (count_p1 + 1) (guess, upper)
        else do 
          putStrLn "too high"
          guessFor p2 p1 bomb count_p2 (count_p1 + 1) (lower, guess)


---------------------------------------------------------------------------
----------------------- interaction with player ---------------------------
---------------------------------------------------------------------------

-- yes and no responses are not case sensitive!!!
-- keeps prompting until a valid Y or N is returned from player
getYesNo :: String -> IO Char
getYesNo promptAgain = 
  getFromStdin promptAgain getChar (`elem` "yYnN") toUpper

getFromStdin :: String -> (IO a) -> (a -> Bool) -> (a -> b) -> IO b
getFromStdin promptAgain inputFunction checkOK transform_OK = do
  input <- inputFunction
  if checkOK input 
    then return $ transform_OK input 
    else do 
      putStr promptAgain
      getFromStdin promptAgain inputFunction checkOK transform_OK

playAgain :: IO Bool
playAgain = do
  putStr "One more round? Let's play again...? "
  again <- getYesNo "\nPlay again?  Y or N:  "
  return $ again == 'Y'

quitPlaying :: IO ()
quitPlaying = do 
  putStrLn "\nNahhhhhhhhh... bye."
  exitWith ExitSuccess

-- Argument verification (FOR TESTING, really... )
checkArgs :: [String] -> (Int, Int) -> IO ()
checkArgs args range =
  if verifyArgs range args
     then putStrLn "Okay! Let's play!"
     else exitWithBadArgs 

exitWithBadArgs :: IO ()
exitWithBadArgs = do 
  putStrLn "ummm... sorry but there's something wrong here" -- just throw some errors
  progName <- getProgName
  putStrLn $ "Use: " ++  progName ++ " [optional random seed]"
  exitWith $ ExitFailure 1

-- Legitimate arguments are none, or a string representing
-- a random seed.  Nothing else is accepted.
verifyArgs :: (Int, Int) -> [String] -> Bool
verifyArgs _ [] = True
verifyArgs range (x:xs) = null xs && isNum range x 

-- Verify that input is a number.  This approach was chosen as read raises an
-- exception if it can't parse its input.  This approach has the benefit
-- of being short, yet sufficient to allow the use of read on anything verified
-- with it, without having to deal with exceptions.
-- also, isNum should check if it's within the lowerB and upperB
isNum :: (Int, Int) -> String -> Bool
isNum _ [] = False 
isNum range (x:xs) = all isDigit xs && (x == '-' || isDigit x) && isInBounds range (x:xs)

-- isInBounds checks that the input is in bounds of the min and max 
isInBounds :: (Int, Int) -> String -> Bool
isInBounds range s = ((read s :: Int) > (fst range)) && ((read s :: Int) < (snd range))


---------------------------------------------------------------------------
-- FOR TESTING ONLY
showSeed :: Int -> IO ()
showSeed seed = putStrLn $ "The random seed is " ++ show seed
-- FOR TESTING ONLY
showBomb :: Int -> IO ()
showBomb answer = putStrLn $ "The bomb was at " ++ show answer

showRange :: (Int, Int) -> IO ()
showRange range = putStrLn $ "Choose a value between " ++ (show ((fst range) + 1)) ++ " and " ++ (show ((snd range)-1))




















