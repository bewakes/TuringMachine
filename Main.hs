import Machine
import System.Environment
import Tape
import Data.List
import Data.Map
import Display
import System.IO
import System.Process
import Control.Monad

machineIO :: Machine -> IO ()
machineIO machine = let
    newmachine  = runMachine machine
    currState   = currentState newmachine
    headp       = headPosition newmachine
    index       = elemIndex currState $ haltingStates machine
    in
        if index == Nothing  then  do
            renderMachine fps newmachine 
            machineIO newmachine
        else do 
            renderMachine fps newmachine

parseInput :: String -> Machine
parseInput raw = Machine sts inst hsts fn a tp hp cs
    where readlines = lines raw
          a = drop 1 $ words $ readlines !! 0
          sts = Data.List.map State $ drop 1 $ words $ readlines !! 1
          inst = State $ (drop 1 $ words $ readlines !! 2) !! 0
          hsts = Data.List.map State $ drop 1 $ words $ readlines !! 3
          hp = 0
          cs = inst
          tapecellValues = Data.List.map replaceSymbols (drop 1 $ words $ readlines !! 4) ++ [""]
          tp = Tape (Cell ">" : (Data.List.map Cell $ tapecellValues))
          funcstmts = drop 6 readlines
          replaced = (Data.List.map words funcstmts)
          fn =  Data.List.map getfunc replaced
          replaceSymbols x
            | x=="LEFT" = "<-"
            | x=="RIGHT" = "->"
            | x=="E" = ""
            | otherwise = x
          getfunc = (\l@[i,r,f,n] -> let [ii,rr,ff,nn] = Data.List.map replaceSymbols l in ((State ii, rr), (State ff, nn)))

readFileContent :: String -> IO String
readFileContent fname = do
    handler <- openFile fname ReadMode 
    contents <- hGetContents handler
    return contents
    --return $ show (parseInput contents)

------------------------ 
-- DATA
------------------------
fps = 1

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

t = Tape ([Cell ">"]++(Prelude.map (Cell . show) [1,0]) )


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


main = let
    getFilename list = if length list ==0 then "programs/machineDefinition.text" else list !! 0
    in
    do
        x <- getArgs
        content <- readFileContent $ getFilename (x)--"programs/machineDefinition.text"--(x!!0)
        system "clear"
        putStrLn $ show $ parseInput content
        renderMachine fps $ parseInput content
        machineIO $ runMachine $ parseInput content
