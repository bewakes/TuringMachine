# TuringMachine
Simulation of Turing Machine in console. Based on its formal definition. Implemented in Haskell.  
If you don't know about Turing machine, read it [here](https://en.wikipedia.org/wiki/Turing_machine)
## short demo  
below the machine lies the defition of the machine
![Short demo](/out.gif)
## Overview
Okay, for now its very nascent and no much features. It just does what it needs to do.  
There is a file `machineDefinition.text`, which ouputs 1's complete of tape symbols(1 and 0), contents of which is shown below:  

    alphabets 1 0
    states q0 q1 h
    initial_state q0
    halt_states h
    tape 1 0 0 1 0 1 1 1 0
    function
    q0 E h E
    q0 0 q1 1
    q0 1 q1 0
    q0 > q0 RIGHT
    q1 E h E
    q1 1 q0 RIGHT
    q1 0 q0 RIGHT

Perhaps you get about alphabets, states, initial_state and halt states.  
`E` means empty string.  
`RIGHT` means move right  
`LEFT` means move left  
`>` means left head end  

Sorry that You can not change the symbols, and the lines should be in the same order except for the function definitions located below `function` line.  
####Now the functions.  
Each line has four parts, first two parts are: `current state` and `current read symbol`.  
Second two parts are: `Next state` and `next Symbol` to be replaced or `move direction`.  
  
So, the  line ` q1 1 q0 RIGHT` means, `when in state q1 and reading symbol 1, move to right being at state q0`.  

NOTE: please don't leave any extra spaces(at the end of line) and new lines after last line. This is yet to be fixed.

## Usage
If you have ghc installed (`apt-get install ghc`), compile and run by:  
`ghc Main.hs`  
`./Main <path to program, if not provided, defaults to machineDefinition.text>`  
If not ghc installed (recommended to install though) I have included the linux executable `Main`. ENJOY!!
