import Machine
import Tape
import Data.List
import Data.Map
import System.Process
import Control.Concurrent

displayAsCell :: Int -> [String] -> String
displayAsCell spaces l@(x:xs) =top ++ "\n" ++ middle ++ "\n" ++ bottom
    where middle = (replicate spaces ' ')++ "| "++ (Data.List.foldl (++) "" (intersperse " | " l)) ++ " |"
          top = replicate (length middle) '-'
          bottom = top

renderMachine :: Machine -> IO ()
renderMachine (Machine _ _ _ _ _ tp headPos currst) = let
    numspaces | headPos <=5 = 20 - headPos*4
              | otherwise = 0
    cellElems = Prelude.map cell_value $ tape_cells tp
    displayElems | headPos <=5 = take 15 cellElems
                 | otherwise = take 15 $ drop (headPos-5) cellElems
    in 
        do
            threadDelay 800000
            system "clear"
            putStrLn $ displayAsCell numspaces displayElems
            putStrLn $ (replicate 20 ' ')++ "  ^"
            putStrLn $ "\t\t\t " ++ ((\(State x) -> x) currst)

machineIO :: Machine -> IO ()
machineIO machine = let
    newmachine  = runMachine machine
    currState   = currentState newmachine
    headp       = headPosition newmachine
    index       = elemIndex currState $ haltingStates machine
    in
        if index == Nothing  then  do
            renderMachine newmachine
            --putStrLn $ (show headp) ++ (show currState)
            --putStrLn "no halt"
            --putStrLn $ show (tape newmachine)
            machineIO newmachine
        else do 
            --putStrLn $ (show headp) ++ (show currState)
            --putStrLn "halt"
            renderMachine newmachine
            --putStrLn $ show (tape newmachine)

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

t = Tape ([Cell ">"]++(Prelude.map (Cell . show) [1,0,1,1,0,1,1,0,1,0]) )


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
