# Minesweeper

## Running Project
Run the following command to run the program  
```shell
stack build && stack exec minesweeper
```

## Playing the game
I chose to have three different difficulty levels going off of the minesweeper game that google hosts, easy (10x10 10 mines), medium (18x18 40 mines) and hard (24x24 99 mines).  
The user can then click one of these buttons to play minesweeper on their chosen difficulty. From this screen the user has:
- A home button which they can use to go back to the main page  
- A reset button which they can use to restart the game with a new board on the current difficulty  
- A mode button which allows the user to switch from mining and flagging mode  
- An auto move button which allows the user to get the AI to play a move (the AI first tries to play an obvious move and if it fails it plays a random move)

The user can also just mine and flag squares themselves. The current mode is visible as a picture on the mode button.  
If they are on mining mode they can left click to mine and right click to flag, and if they are on flagging mode they can left click to flag and right click to mine. 