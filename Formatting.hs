import System.Process

data Box = Box (String, String, String)
data Boxes = Boxes [Box]

instance Show (Box) where
    show (Box (top, middle, bottom)) = top ++ "\n" ++ middle ++ "\n" ++ bottom

instance Show (Boxes) where
    show (Boxes []) = ""
    show (Boxes (b:[])) = show b
    show (Boxes (Box (t, m, b): boxes)) = top ++ "\n" ++ middle ++ "\n" ++ bottom
        where middle = m ++ ( cat $ excludingFirstOfMiddles boxes)
              top = replicate (length middle) '-'
              bottom = top
              cat = foldl (++) ""
              excludingFirstOfMiddles lst = (map (\(Box(t,m,b)) -> tail m) lst)


getBox :: String -> Box
getBox value = Box (t, m, b)
    where m = "| " ++ value ++ " |"
          t = replicate  (length m) '-'
          b = t

main = putStrLn $ show $ Boxes $ map getBox ["b","i"]
