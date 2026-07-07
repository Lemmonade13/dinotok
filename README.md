## Hi as3235-rcl229-sar344!

This is the `main` branch - use this for collaborating on code and be sure to add all the code you developed for your final project here. We will download everything upon the deadline.

## Your page

The project web-pages will also be hosted on github in this repo in the `page` branch. You can edit it by switching branches and modifying the files, or by pushing to the branch. Here is a link to a minimal web-page that you can use as a starting point:[https://pages.github.coecis.cornell.edu/ece3140-spr2026/as3235-rcl229-sar344/](https://pages.github.coecis.cornell.edu/ece3140-spr2026/as3235-rcl229-sar344/).

## Setup:

If you already have OCaml downloaded, you are done. Otherwise, you need to go through the following steps to set it up:

https://cs3110.github.io/textbook/chapters/preface/install.html

and also run the following command to install these OCaml packages:
opam install lwt_ppx

To run the game with the board as the controller, first build the game_controls.c file and program the board. This can be done by opening the Board folder as a project in MCUXpressoIDE, and game_controls.c is found in the source folder.

Then, in your terminal, open the project's root directory (as3235-rcl229-sar344) and run:

cd dinotok  
dune exec dinotok  

You will be prompted to select a board for both players (you can also select keyboard controls and randomized obstacles, which won't use the boards). If you only have one board, just select it for player one and choose randomized obstacles. The game will then start.

With the cord attached on the bottom left of the board, the controls for player 1 are:

- Tilt forward: Jump
- Tilt back: Slide (or, if you're already sliding, switch to standing)
- Tilt right: Frontflip (same as jump but looks cooler)
- Tilt left: Backflip (same as jump but looks cooler)
- Top right button (SW1): Send F-16 (takes time to load; game will display when ready)
- Bottom right button (SW3): Quit

The game can also be played where a player places the obstacles. On the second board, another player uses the buttons to choose when obstacles are sent, and whether they are high (SW1) or low (SW3). Program this board with the file obstacle_controls.c. If you play with two boards, you may have to run it a second time to confirm that the correct board is chosen (if you program a board for player 1 and select it for player 2, it won't do anything). 

## Code 

The game itself is contained in an Ocaml file, dinotok.ml, and is run in the file main.ml. We updated the game code so it could receive input from the board. The game control programming is contained in game_controls.c, which sets up the accelerometer, buttons and I2C. Initialization functions are defined in init.h and implemented in init.c. The obstacle control programming (for the second board) is in obstacle_controls.c, which just uses the buttons.

## Warning

It is highly recommended that you have a Mac and took CS 3110. If you did not take 3110, you have to download OCaml which is a bit of a pain, but it is doable. If you don't have Mac, we cannot gurantee that the code will work, because two of us had Macs, and while one of us had Windows, her GitHub decided to stop working before we could test the finalized code on it (it was never working well to begin with so that's not a huge surprise).