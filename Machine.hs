module Machine(Machine(..), State(..),  runMachine) where

import Tape
import System.Process
import Control.Concurrent
import System.IO(IO)

data State = State String deriving(Show, Ord, Eq)

data Machine = Machine 
    {
        states          :: [State],
        initialState    :: State,
        haltingStates   :: [State],
        function        :: [((State, String), (State, String))],
        alphabet        :: [String], -- consists of blank and left End symbol(>)
        tape            :: Tape,
        headPosition    :: Int,
        currentState    :: State
    } deriving(Show)

runMachine :: Machine -> Machine
runMachine machine = let 
    functionMap     = Data.Map.fromList $ function machine
    machineTape     = tape machine
    cellValues      = Prelude.map cell_value $ tape_cells machineTape
    headpos         = headPosition machine
    currState       = currentState machine
    currStateAlpha  = (currState, cellValues !! headpos)
    lookupResult    = Data.Map.lookup currStateAlpha functionMap
    nextStateAlpha  | lookupResult /= Nothing = extract $ lookupResult
                    | otherwise = (State "h0", "")
    extract         = (\(Just x) -> x)
    nextState       = fst nextStateAlpha
    nextAlpha       = snd nextStateAlpha
    nextHeadpos     | nextAlpha == "<-" = headpos - 1
                    | nextAlpha == "->" = headpos + 1
                    | otherwise         = headpos

    newCellValues   | nextAlpha == "<-" = cellValues
                    | nextAlpha == "->" = cellValues
                    | otherwise = take headpos cellValues ++ [nextAlpha] ++ (drop (headpos+1) cellValues)
    newTape         = Tape $ Prelude.map Cell newCellValues 
    in Machine {
        states = states machine,
        initialState = initialState machine,
        haltingStates = haltingStates machine,
        function = function machine,
        alphabet = alphabet machine,
        tape = newTape,
        headPosition = nextHeadpos,
        currentState = nextState
    }


