# Not My Number

'Not My Number' (working title) is another min(e/d) game -- this is supposed to be punny. The objective? Leave your opponent with the 'mine'. 
The game begins with the computer planting a 'mine' and as each player takes turns making a move, the 'safe' area begins to shrink to increase the chances of finding the 'mine' before the next player's turn, if the player who is making the turn did not find the 'mine' him/herself.

## Logistics 

Members: Player1, Player2 (another person or computer)
GameState: starts with array of [1,100]
Computer plants a "bomb" in a game field represented by a matrix from 1 - 100 inclusive — bomb is at location b.
Player1 chooses a number, x from [1, 100]. If x < b, x becomes the new lower limit and Player2 must choose a number from [x+1, 100]. if x > b, x becomes the new upper limit and Player2 must choose a number between [1, x-1].
On the next turn Player2 chooses a number, y. The interval continues to shrink. Eventually the interval becomes [x, y-1] or [y+1, x].
The interval always contains the value b and the chances of Player1 or Player2 choosing b increases as the possible number of choices decrease.

## Things we hope to add

* GUI for the game -- makes the game easier to play
* rewards or point system for 'successful' choices
* tournament system: keeps track of how many wins and losses for each player
* different modes for different levels of difficulty

# To run

### To play the game: 
```
ghci
:l NotMyNumber
start
```

### Troubleshoot and Development
Make sure ghci is downloaded and also install random package by, 
In terminal:
```
cabal install random
```

Also, for developing GUI: Fudget
To download Fudgets [check here](http://www.altocumulus.org/Fudgets/dist.html)

Then in terminal:
```
tar xf fudgets-160113.tar.gz 
cd Fudgets/hsrc/hbc_library 
cabal install 
cd ../.. 
cabal install
```