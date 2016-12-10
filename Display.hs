module Display(displayAsCell, renderMachine) where
import Tape
import Machine
import Data.List
import Control.Concurrent
import System.Process

displayAsCell :: Int -> [String] -> String
displayAsCell spaces l@(x:xs) =top ++ "\n" ++(replicate spaces ' ') ++ middle ++ "\n" ++ bottom
    where middle = "| "++ (Data.List.foldl (++) "" (intersperse " | " l)) ++ " |" ++ " ..."
          top = (replicate spaces ' ') ++ replicate (length middle) '-'
          bottom = top

renderMachine :: Float -> Machine -> IO ()
renderMachine fps (Machine _ _ _ _ _ tp headPos currst) = let
    numspaces | headPos <=5 = 20 - headPos*4
              | otherwise = 0
    cellElems = Prelude.map cell_value $ tape_cells tp
    displayElems | headPos <=5 = take 15 cellElems
                 | otherwise = take 15 $ drop (headPos-5) cellElems
    delay = ceiling (1000000 / fps)
    in 
        do
            threadDelay delay
            system "clear"
            putStrLn $ displayAsCell numspaces displayElems
            putStrLn $ (replicate 20 ' ')++ "  ^"
            putStrLn $ displayAsCell 19 ["STATE: "++ (\(State x) -> x) currst]
