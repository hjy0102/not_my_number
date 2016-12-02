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

-- type Leaderboard = 	(PlayerScore, PlayerScore)		-- Player1 score, Player2 score

minNum = 0
maxNum = 101

start :: IO ()
start = do
  let range = (minNum, maxNum)
  putStrLn "Which mode? 0=2-players, 1=easy, 2=medium, 3=hard."
  args <- getArgs
  checkArgs args range
  seed <- getSeed args
  playNewGame range $ getRandomGen seed
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
playNewGame :: (Int, Int) -> StdGen -> IO ()
playNewGame range n = do
    putStrLn $ "\nWelcome to NotMyNumber!"
    putStrLn $ "The objective of the game is not to be the player to find the bomb"
    putStrLn $ "The bomb is hidden in the field. Guess a number between " ++ (show (fst range)) ++ " and " ++ (show (snd range)) ++ " to begin"
    let (inTargetNumber, newGen) = next n 
    let bomb = mod inTargetNumber ((snd range)+1)
    guessFor bomb 0 range
    showBomb bomb
    again <- playAgain
    if again 
      -- this is wrong !!! just placing for the program to compile
        then playNewGame range newGen
        else quitPlaying

-- guessFor handles all the guessing
-- takes in two Int arguments: one for the 
-- guess and one as a counter for the attempts
guessFor :: Int -> Int -> (Int, Int) -> IO ()
guessFor bomb count range = do
  -- putStr "Choose a number? "
  showRange range
  guess <- getNumber "That's not in the range! Guess again:  " range
  if bomb == guess
    then foundBomb $ count + 1
    else missedBomb bomb count range guess 

-- keeps asking for a number 
getNumber :: String -> (Int, Int) -> IO Int 
getNumber promptAgain range = 
  getFromStdin promptAgain getLine (isNum range) read 

-- foundBomb is what happens when you find the bomb
-- not good :( 
foundBomb :: Int -> IO ()
foundBomb count = do
  putStrLn "BOOM*&@&!^#&@!*(#@!! You found the bomb."
  putStrLn $ "You died in " ++ show count ++ " turns."

-- missedBomb lets the players keep guessing
{-- TODO: handle changing the array of possible moves
!!!
The too low or too high is temporary for testing only
--}
missedBomb bomb attempts (lower, upper) guess = do
  if bomb > guess
      then do 
        putStrLn "too low"
        guessFor bomb (attempts + 1) (guess, upper)
        else do 
          putStrLn "too high"
          guessFor bomb (attempts + 1) (lower, guess)




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
---------------------------------------------------------------------------








---------------------------------------------------------------------------
-- Tests

{--

isInBounds "1"
>> True
isInBounds "100"
>> True
isInBounds "101"
>> False
isInBounds "-1"
>> False

isNum "-1"
>> True

-- should start a game
playNewGame (1,10) (getRandomGen 20)


--}

------------------ New Code for NotMyNumber --------------------------------





start1 :: IO ()
start1 = do
  let range = (minNum, maxNum)
  putStrLn $ "\nWelcome to NotMyNumber!"
  putStrLn $ "The objective of the game is not to be the player to find the bomb"
  putStrLn $ "The bomb is hidden in the field. Guess a number between " ++ (show (fst range)) ++ " and " ++ (show (snd range)) ++ " to begin"
  playNewGame1 notMyNumber (notMyNumber Start) (0,0)
  else exitWithBadArgs
  putStrLn "Game Over"

playNewGame1 :: Game -> PlayerRecord -> Bool -> IO PlayerRecord
playNewGame1 game record isQuit= 
  let (inTargetNumber, newGen) = next (getRandomGen seed)
      bomb = mod inTargetNumber (snd range)
  in
    do
      putStrLn "Which mode? 0=2-players, 1=easy, 2=medium, 3=hard."
      mode <- getLine
      putStrLn "Who starts? 0=you, 1=computer"
      order <- getLine
      putStrLn ("mode is"++show mode) -- Just for test
      putStrLn ("order is"++show order)  -- Just for test
      seed <- getSeed []
      {- TODO make more if else -}
      if((read mode :: Int)==0)
        then putStrLn "Have not implemented this"
      else if ((read mode :: Int)==1)
        then person_play game (notMyNumber Start ((range),bomb)) easy_player record
      else if ((read mode :: Int)==2)
        then playNewGame1 game (notMyNumber Start ((range),bomb)) medium_player record
      else if ((read mode :: Int)==3)
        then playNewGame1 game (notMyNumber Start ((range),bomb)) hard_player record
      else 

person_play :: Game -> Result -> Player -> PlayerRecord -> IO PlayerRecord
-- opponent has played, the person must now play
person_play game (EndOfGame Lose) opponent (wins,losses) =
   do
      putStrLn "Computer won!"
      again <- playAgain
      if again 
      -- this is wrong !!! just placing for the program to compile
        then playNewGame1 game (wins,losses+1)
        else quitPlaying
      
person_play game (ContinueGame (bound, bomb)) opponent record =
   do
      {- TODO get User input as AMove -}
      -- move <-
      computer_play game (game (Move move (bound, bomb)) opponent record


computer_play :: Game -> Result -> Player -> PlayerRecord -> IO PlayerRecord
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play
computer_play game (EndOfGame Lose) opponent (wins,losses) =
   do
      putStrLn "You won!"
      again <- playAgain
      if again 
      -- this is wrong !!! just placing for the program to compile
        then playNewGame1 game (wins,losses+1)
        else quitPlaying
      
computer_play game result opponent (wins,losses) =
      let ContinueGame state = result
          opponent_move = opponent game result
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game (Move opponent_move state) (bound, bomb)) opponent (wins,losses)


-- 




