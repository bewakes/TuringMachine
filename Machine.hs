import Tape

data State = State String deriving(Show)



data Machine = Machine 
    {
        states          :: [State],
        initialState    :: State,
        haltingStates   :: [State],
        function        :: [((State, String), (State, String))],
        alphabet        :: [String], -- consists of blank and left End symbol(>)
        tape            :: Tape,
        headPosition    :: Int
    } deriving(Show)


c = getCell "b"
t = Tape []

state = State "halt"

m = Machine { 
    states = [state],
    initialState = state,
    haltingStates = [state],
    function = [],
    tape = t,
    headPosition = 4,
    alphabet = []
}


main = putStrLn $ show m
