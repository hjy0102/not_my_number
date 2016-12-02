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

type Computer = (Int, Int) -> Int
computer_easy :: Computer
computer_easy (lowerB, upperB) 
 | (lowerB + 5)<upperB = (lowerB + 5)
 | otherwise = upperB
computer_medium :: Computer
computer_medium (lowerB, upperB) = ((lowerB+upperB)*2) `div` 3
computer_hard :: Computer
computer_hard (lowerB, upperB) = (lowerB+upperB+1) `div` 2

---------------------------------------------------------------------------
minNum = 0
maxNum = 101

start :: IO ()
start = do
  let range = (minNum, maxNum)
  seed <- getSeed []
  putStrLn $ "\nWelcome to NotMyNumber!"
  putStrLn $ "\nThe objective of the game is not to be the player to find the bomb. "
  putStrLn $ "The bomb is hidden in the field. Guess a number between " ++ (show ((fst range) + 1)) ++ " and " ++ (show ((snd range) - 1 ))  ++ " to begin."
  putStrLn "\nWhich mode? 0=2-players, 1=easy, 2=medium, 3=hard."
  mode <- getLine
  if ((read mode:: Int) == 0)
    then  do
          --args <- getArgs
          --checkArgs args range
          putStrLn "Player1's name ?  "
          name <- getLine
          let player1     = setPlayerScore name 0 0
          putStrLn "Player2's name ?  "
          name2 <- getLine
          let player2 = setPlayerScore name2 0 0
          let tournament_state = (player1, player2)
          --seed <- getSeed args
          playNewGame player1 player2 range $ getRandomGen seed
          putStrLn "Game Over. "
  else do
    let modeName = checkMode (read mode:: Int)
    putStrLn "Player's name ?  "
    name <- getLine
    let player     = setPlayerScore name 0 0
    let computer_player = setPlayerScore ("computer_"++modeName) 0 0
    let tournament_state = (player, computer_player)
    playNewGame player computer_player range $ getRandomGen seed
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
    putStrLn "Okay! Let's play!"
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
  if (getPlayerName p1 == "computer_easy")
    then  do 
          let guess = computer_easy range
          putStrLn ((getPlayerName p1)++" chooses "++ show guess)
          if bomb == guess
            then foundBomb p1 $ count_p1 + 1
            else missedBomb p1 p2 bomb count_p1 count_p2 range guess 
  else if (getPlayerName p1 == "computer_medium")
    then  do
          let guess = computer_medium range
          putStrLn ((getPlayerName p1)++" chooses "++ show guess)
          if bomb == guess
            then foundBomb p1 $ count_p1 + 1
            else missedBomb p1 p2 bomb count_p1 count_p2 range guess
  else if (getPlayerName p1 == "computer_hard")
    then  do
          let guess = computer_hard range
          putStrLn ((getPlayerName p1)++" chooses "++ show guess)
          if bomb == guess
            then foundBomb p1 $ count_p1 + 1
            else missedBomb p1 p2 bomb count_p1 count_p2 range guess
  else do
    guess <- getNumber "That's not in the range! Guess again:  " range
    if bomb == guess
      then foundBomb p1 $ count_p1 + 1
      else missedBomb p1 p2 bomb count_p1 count_p2 range guess 

whoseTurn :: PlayerScore -> IO ()
whoseTurn p1 = putStrLn $ "\nWhose turn: " ++ show (getPlayerName p1)

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

-- check which mode does the user choose and return corresponding computer name (e.g., computer_easy)
checkMode :: Int -> String
checkMode mode
 | mode == 1 = "easy"
 | mode == 2 = "medium"
 | mode == 3 = "hard"
 | otherwise = "strange"
---------------------------------------------------------------------------
-- FOR TESTING ONLY
showSeed :: Int -> IO ()
showSeed seed = putStrLn $ "The random seed is " ++ show seed
-- FOR TESTING ONLY
showBomb :: Int -> IO ()
showBomb answer = putStrLn $ "The bomb was at " ++ show answer

showRange :: (Int, Int) -> IO ()
showRange range = putStrLn $ "Choose a value between " ++ (show ((fst range) + 1)) ++ " and " ++ (show ((snd range)-1))

------------------ New Code for NotMyNumber --------------------------------





--start1 :: IO ()
--start1 = do
--  let range = (minNum, maxNum)
--  putStrLn $ "\nWelcome to NotMyNumber!"
--  putStrLn $ "The objective of the game is not to be the player to find the bomb"
--  putStrLn $ "The bomb is hidden in the field. Guess a number between " ++ (show (fst range)) ++ " and " ++ (show (snd range)) ++ " to begin"
--  playNewGame1 notMyNumber (notMyNumber Start) (0,0)
--  else exitWithBadArgs
--  putStrLn "Game Over"

--playNewGame1 :: Game -> PlayerRecord -> Bool -> IO PlayerRecord
--playNewGame1 game record isQuit= 
--  let (inTargetNumber, newGen) = next (getRandomGen seed)
--      bomb = mod inTargetNumber (snd range)
--  in
--    do
--      putStrLn "Which mode? 0=2-players, 1=easy, 2=medium, 3=hard."
--      mode <- getLine
--      putStrLn "Who starts? 0=you, 1=computer"
--      order <- getLine
--      putStrLn ("mode is"++show mode) -- Just for test
--      putStrLn ("order is"++show order)  -- Just for test
--      seed <- getSeed []
--      {- TODO make more if else -}
--      if((read mode :: Int)==0)
--        then putStrLn "Have not implemented this"
--      else if ((read mode :: Int)==1)
--        then person_play game (notMyNumber Start ((range),bomb)) easy_player record
--      else if ((read mode :: Int)==2)
--        then playNewGame1 game (notMyNumber Start ((range),bomb)) medium_player record
--      else if ((read mode :: Int)==3)
--        then playNewGame1 game (notMyNumber Start ((range),bomb)) hard_player record
--      else 

--person_play :: Game -> Result -> Player -> PlayerRecord -> IO PlayerRecord
---- opponent has played, the person must now play
--person_play game (EndOfGame Lose) opponent (wins,losses) =
--   do
--      putStrLn "Computer won!"
--      again <- playAgain
--      if again 
--      -- this is wrong !!! just placing for the program to compile
--        then playNewGame1 game (wins,losses+1)
--        else quitPlaying
      
--person_play game (ContinueGame (bound, bomb)) opponent record =
--   do
--      {- TODO get User input as AMove -}
--      -- move <-
--      computer_play game (game (Move move (bound, bomb)) opponent record


--computer_play :: Game -> Result -> Player -> PlayerRecord -> IO PlayerRecord
---- computer_play game current_result opponent tournament_state
---- person has played, the computer must now play
--computer_play game (EndOfGame Lose) opponent (wins,losses) =
--   do
--      putStrLn "You won!"
--      again <- playAgain
--      if again 
--      -- this is wrong !!! just placing for the program to compile
--        then playNewGame1 game (wins,losses+1)
--        else quitPlaying
      
--computer_play game result opponent (wins,losses) =
--      let ContinueGame state = result
--          opponent_move = opponent game result
--        in
--          do
--            putStrLn ("The computer chose "++show opponent_move)
--            person_play game (game (Move opponent_move state) (bound, bomb)) opponent (wins,losses)





