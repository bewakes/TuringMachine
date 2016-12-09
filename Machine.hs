module Machine(Machine(..), State(..),  runMachine) where

import Tape
import Data.Map

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
runMachine (Machine sts is haltsts func a tp headpos currState) = let 
    functionMap     = Data.Map.fromList $ func
    cellValues      = Prelude.map cell_value $ tape_cells tp
    currStateAlpha  | headpos >= length (tape_cells tp) = (currState, "")
                    | otherwise = (currState, cellValues !! headpos)
    lookupResult    = Data.Map.lookup currStateAlpha functionMap
    (nextState, nextAlpha)= (\(Just x) -> x) $ lookupResult
    nextHeadpos     | nextAlpha == "<-" = headpos - 1
                    | nextAlpha == "->" = headpos + 1
                    | otherwise         = headpos

    newCellValues   | nextAlpha == "<-" = cellValues
                    | nextAlpha == "->" = cellValues
                    | otherwise = take headpos cellValues ++ [nextAlpha] ++ (drop (headpos+1) cellValues)
    newTape         = Tape $ Prelude.map Cell newCellValues 

    in Machine sts is haltsts func a newTape nextHeadpos nextState 


