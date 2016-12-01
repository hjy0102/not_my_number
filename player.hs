-- definition of a Player and
-- the moves a Player can make

module Player where

-- to run: 
-- ghci
-- :load Player
import Game

easy_player :: Player
-- This player is for easy mode
easy_player _ (ContinueGame ((lowerB, upperB),_)) = lowerB + 5

medium_player :: Player
-- This player is for medium mode
medium_player _ (ContinueGame ((lowerB, upperB),_)) = ((lowerB+upperB)*2) `div` 3

hard_player :: Player
-- This player is for hard mode
-- TODO: Can make it more intelligent since it knows where the bomb is now
hard_player _ (ContinueGame ((lowerB, upperB),_)) = (lowerB+upperB+1) `div` 2