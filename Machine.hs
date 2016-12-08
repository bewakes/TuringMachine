import Tape
import Data.Map
import System.Process
import Control.Concurrent

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

t = Tape (Prelude.map (Cell . show) [1,0,1,1,0,1])


m = Machine { 
    states = _states,
    initialState = q0,
    haltingStates = [halt],
    function = funclist,
    tape = t,
    headPosition = 4,
    alphabet = []
}


main = putStrLn $ show $ step funclist (q0, "0") 
