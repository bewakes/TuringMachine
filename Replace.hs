{-module Replace(replace, elementsAfter, replaceChar) where-}

-- gives elements after given integer index
elementsAfter :: Int -> [a] -> [a]
elementsAfter n [] = []
elementsAfter n lst
    | n<= 0 = lst
    | n >= length lst = []
    | otherwise = elementsAfter (n-1) $ tail lst

-- replace a string(list) by another string(list) in given string(list)
replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _  [] = [] 
replace a b string = repl a b string
    where repl a b string  
            | string == [] = string
            | (take len_a string)== a = b++ (repl a b (elementsAfter len_a string)) 
            | otherwise = [head string] ++ (repl a b (tail string))
          len_a = length a
                

-- replace character
replaceChar:: Char -> Char -> [Char] -> [Char]
replaceChar _ _ "" = ""
replaceChar a b (x:xs) = (repl a b x) : replaceChar a b xs
    where repl a b x
            |a==x = b
            |otherwise = x

main = print $ replace "java" "haskell" "java is awesome"
    {-print $ replaceChar 'm' 'b' "mammamia"-}
    
