# Plan for developing the scrabble AI

## The goal

### Basic requirements for us to be satisfied:
- Identify a valid move with a piece on the board as the first letter.
- Changing all pieces in hand if no moves can be identified.
- Play the first move identified.

### May do but probably not:
- Identify a valid move with a piece on the board as the last letter (would need a gaddag).
- Identify a valid move with a piece on the board in the middle of the word.
- Play the highest scoring move found within the timeout limit.

### Will not do no mater what:
- Identify a valid move which uses multiple pieces on the board.
- Change pieces intelligently.

## Development plan
1. Function for finding a valid word with a given start letter using the letters in the hand for the rest of the word.
2. Detect if it is the first move in the game.
3. Place a word using only letters in the hand if it is the first move in the game.
4. Function to find places on the board where we can start a word.
5. Function for checking if a word can be placed on the board (only check if it intersects other pieces at first).
6. Place the first valid word.
7. Using the previously defined functions to find a valid place to start a word, then finding a word using that start letter which fits in that position.
8. Fix the function from step 5 to check all other words created as a consequence of the placement.
9. Support all client messages.
10. Change all pieces in hand if no moves were found.
11. Make sure it plays nicely with other bots `:)`
12. At this point we should be satisfying the project requirements.

## Algoritm approach

### Finding valid words with given start letter using the letters in the hand for the rest of the word


### Finding valid places on the board where we can start a word
It would be a good idea to track places where a word can be started horizontally and vertically seperately.

There are two approaches:
1. Every time we update the board, we also update some list of possible starting positions, and we simply check that list.
2. We go through every piece on the board and note if it is a possible starting position.

A possible horizontal starting position would be a piece with no piece on the immediate right of it. A possible vertical starting position would be a piece with no piece immediatly below it.

We would only need to do this once every time we make a move, so approack 2 may be the best option (the board is updated more often than we make moves).

### Checking if a word intersects other pieces
Simply check the map if a piece is placed in any of the coordinates which the word wants to be placed in.

### Checking adjacent words
Do the following for each piece we are going to place:
1. Find the furthest connected piece to the left of the piece we are placing.
2. Use the step function to search the dictionary for the letter on the piece.
3. Move to the piece to the right right of it and continue the search with the letter on this one.
4. If there is no continuation using the letter we are at, the placement is invalid.
5. Repeat steps 3-4 until there is no piece on the right or the placement is invalid
6. Check if we have reached a valid word in the dictionary. If we have not, the placement is invalid.
7. Do steps 1-6, but vertically (start by moving up, then check down).

For horizontal words, it is only nessesary to check horizontally once.
For vertical words, it is only nessesary to check vertically once.