# Chess-AI

A Haskell-based Chess Engine and AI opponent.

## Requirements

* GHC
* Cabal

## Usage

### Install Dependencies

```
cabal install pretty-tree --install-method=copy --lib
cabal install sort --install-method=copy --lib
```

### Run the Game

```
cabal run Chess-AI
```

### Playing against the AI Opponent

1. Enter your name when prompted:

    ```
    ===================
    Welcome to Chess-AI
    ===================


    What is your name?
    $ Player1
    ```

2. Hit enter to start the game:

    ```
    Hi Player1!


    Starting game..
    ```

3. You will see a chess board on the screen. The white pieces are denoted with uppercase letters and hyphens (eg. `-K-` for King and `-N-` for Knight) and the black pieces are denoted with lowercase letters.

4. Follow the instructions on the screen to play a move or see available moves:

    ```
    -------------------------------------------------------------------

    >> Player1's turn (White):


          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)

    (8)  [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]  (8)
    (7)  [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]  (7)
    (6)  [   ][   ][   ][   ][   ][   ][   ][   ]  (6)
    (5)  [   ][   ][   ][   ][   ][   ][   ][   ]  (5)
    (4)  [   ][   ][   ][   ][   ][   ][   ][   ]  (4)
    (3)  [   ][   ][   ][   ][   ][   ][   ][   ]  (3)
    (2)  [-P-][-P-][-P-][-P-][-P-][-P-][-P-][-P-]  (2)
    (1)  [-R-][-N-][-B-][-Q-][-K-][-B-][-N-][-R-]  (1)

          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)


    Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves:
    $
    ```

    Eg. Check available moves for position E2:

    ```
    Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves:
    $ e2

    >> You can play: [E2 to E3,E2 to E4]


          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H) 

    (8)  [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]  (8)
    (7)  [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]  (7)
    (6)  [   ][   ][   ][   ][   ][   ][   ][   ]  (6)
    (5)  [   ][   ][   ][   ][   ][   ][   ][   ]  (5)
    (4)  [   ][   ][   ][   ][   ][   ][   ][   ]  (4)
    (3)  [   ][   ][   ][   ][   ][   ][   ][   ]  (3)
    (2)  [-P-][-P-][-P-][-P-][-P-][-P-][-P-][-P-]  (2)
    (1)  [-R-][-N-][-B-][-Q-][-K-][-B-][-N-][-R-]  (1)

          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)


    Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves:
    $
    ```

    Eg. Move the white pawn at E2 to E4:

    ```
    Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves:
    $ e2 to e4
    >> Player1 played E2 to E4
    ```

5. The AI opponent will play next. If enabled, you will see the first 2 levels of the minmax tree that the AI player uses to make decisions:

    ```
    >> Player1 played E2 to E4

    -------------------------------------------------------------------
    -------------------------------------------------------------------

    >> AI Player's turn (Black):


          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)

    (8)  [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]  (8)
    (7)  [ p ][ p ][ p ][ p ][ p ][ p ][ p ][ p ]  (7)
    (6)  [   ][   ][   ][   ][   ][   ][   ][   ]  (6)
    (5)  [   ][   ][   ][   ][   ][   ][   ][   ]  (5)
    (4)  [   ][   ][   ][   ][-P-][   ][   ][   ]  (4)
    (3)  [   ][   ][   ][   ][   ][   ][   ][   ]  (3)
    (2)  [-P-][-P-][-P-][-P-][   ][-P-][-P-][-P-]  (2)
    (1)  [-R-][-N-][-B-][-Q-][-K-][-B-][-N-][-R-]  (1)

          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)


    AI opponent calculating moves. Please wait.

    AI Player's Game Tree:
                                                                                                                current board (-10)
                                                                                                                        |
                                -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            /                                                          |                                                           |                                                          |                               \
                        H7 to H6(-10)                                              H7 to H5(-10)                                               G7 to G6(-10)                                              G7 to G5(-10)                        ...
                            |                                                          |                                                           |                                                          |
        ------------------------------------------------          --------------------------------------------------          -------------------------------------------------          -------------------------------------------------
        /             |            |            |        \        /             |             |             |        \        /             |             |            |        \        /             |             |            |        \
    F1 to A6(30)  E4 to E5(0)  G1 to H3(0)  G1 to F3(0)  ...  D1 to G4(90)  D1 to H5(80)  F1 to A6(20)  E4 to E5(0)  ...  D1 to H5(90)  F1 to A6(30)  E4 to E5(0)  G1 to H3(0)  ...  F1 to A6(30)  F2 to F4(10)  E4 to E5(0)  G1 to F3(0)  ...
        |             |            |            |                 |             |             |             |                 |             |             |            |                 |             |             |            |
        ...           ...          ...          ...               ...           ...           ...           ...               ...           ...           ...          ...               ...           ...           ...          ...

    >> AI Player played H7 to H6

    -------------------------------------------------------------------
    -------------------------------------------------------------------

    >> Player1's turn (White):


          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)

    (8)  [ r ][ n ][ b ][ q ][ k ][ b ][ n ][ r ]  (8)
    (7)  [ p ][ p ][ p ][ p ][ p ][ p ][ p ][   ]  (7)
    (6)  [   ][   ][   ][   ][   ][   ][   ][ p ]  (6)
    (5)  [   ][   ][   ][   ][   ][   ][   ][   ]  (5)
    (4)  [   ][   ][   ][   ][-P-][   ][   ][   ]  (4)
    (3)  [   ][   ][   ][   ][   ][   ][   ][   ]  (3)
    (2)  [-P-][-P-][-P-][-P-][   ][-P-][-P-][-P-]  (2)
    (1)  [-R-][-N-][-B-][-Q-][-K-][-B-][-N-][-R-]  (1)

          (A)  (B)  (C)  (D)  (E)  (F)  (G)  (H)


    Enter a move (eg. c1 to d2) or enter a position (eg. h5) to see available moves:
    ```

## Development

### Install Test Dependencies

```
cabal install pretty-tree --install-method=copy --lib
cabal install sort --install-method=copy --lib
cabal install HTF --install-method=copy
```

### Run all tests

```
cabal test all
```