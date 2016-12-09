module Tape(Cell(..), Tape(..)) where
import Data.List

data Cell = Cell { cell_value :: String }
data Tape = Tape { tape_cells :: [Cell] }

instance Show (Cell) where
    show cell = top ++"\n"++ middle ++"\n"++ bottom

        where middle    = "| " ++ cell_value cell ++ " |"
              top       = replicate (length middle) '-'
              bottom    = top


instance Show (Tape) where
    show tape = top ++"\n"++ middle ++"\n"++ bottom
        where middle            = "| " ++ (joinValues tape) ++ " |"
              joinValues m      = foldl (++) "" $ barred_values $ tape_cells m
              barred_values d   = intersperse  " | " $ map cell_value d
              top               = replicate (length middle) '-'
              bottom            = top

