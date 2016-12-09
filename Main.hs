import Machine
import Tape
import Data.List
import Data.Map

machineIO :: Machine -> IO ()
machineIO machine = let
    newmachine  = runMachine machine
    currState   = currentState machine
    index       = elemIndex currState $ haltingStates machine
    in
        if index == Nothing  then  do
            --putStrLn $ show currState
            putStrLn "no halt"
            putStrLn $ show (tape newmachine)
            machineIO newmachine
        else do 
            --putStrLn $ show currState
            putStrLn "halt"
            putStrLn $ show (tape newmachine)

------------------------ 
-- DATA
------------------------

halt = State "h"
q0 = State "q0"
q1 = State "q1"

_BLANK = ""
_LEFT_END = ">"

alpha = ["0","1", _BLANK, _LEFT_END]

_states = [halt, q0, q1]

funclist = [
        ((q0, ""), (halt, "")),
        ((q0, "0"), (q1, "1")),
        ((q0, "1"), (q1, "0")),
        ((q0, ">"), (q0, "->")),
        ((q1, ""), (halt, "")),
        ((q1, "1"), (q0, "->")),
        ((q1, "0"), (q0, "->"))
    ]

step funclist state = Data.Map.lookup state hmap
    where hmap = fromList funclist

t = Tape ((Prelude.map (Cell . show) [1,0,1,1,0,1]) ++ [Cell ""])


m = Machine { 
    states = _states,
    initialState = q0,
    haltingStates = [halt],
    function = funclist,
    tape = t,
    headPosition = 0,
    alphabet = alpha,
    currentState = q0
}


main = do
    putStrLn $ show t
    machineIO $ runMachine m
