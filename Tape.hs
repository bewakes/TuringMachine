import System.Process
import Control.Concurrent

data Cell = Cell (String, String, String)
data Tape = Tape [Cell]

instance Show (Cell) where
    show (Cell (top, middle, bottom)) = top ++ "\n" ++ middle ++ "\n" ++ bottom

instance Show (Tape) where
    show (Tape []) = ""
    show (Tape (b:[])) = show b
    show (Tape (Cell (t, m, b): boxes)) = top ++ "\n" ++ middle ++ "\n" ++ bottom
        where middle = m ++ ( cat $ excludingFirstOfMiddles boxes)
              top = replicate (length middle) '-'
              bottom = top
              cat = foldl (++) ""
              excludingFirstOfMiddles lst = (map (\(Cell(t,m,b)) -> tail m) lst)


getCell :: String -> Cell
getCell value = Cell (t, m, b)
    where m = "| " ++ value ++ " |"
          t = replicate  (length m) '-'
          b = t

display = disp 10
    where disp n 
            | n == 0 = putStrLn ""
            | otherwise = do
                system "clear"
                putStrLn $ replicate 5 '\n'
                putStrLn $ str
                threadDelay 800000
                disp (n-1)
            where str = show $ Tape $ map getCell (map show [1..n])
main = do
    display
