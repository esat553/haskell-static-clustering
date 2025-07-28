{-
Definieren Sie eine Funktion rest :: String -> String, die bis auf das erste Element alle Elemente eines Strings ausgibt. Sie können davon ausgehen, dass die Strings nicht leer ist.
-}
{-
english translation:
Define a function rest :: String -> String that returns all elements of a string except for the first one. You may assume that the string is not empty.
-}
rest :: String -> String
rest xs = drop 1 xs
-- or
rest' :: String -> String
rest' (_:xs) = xs