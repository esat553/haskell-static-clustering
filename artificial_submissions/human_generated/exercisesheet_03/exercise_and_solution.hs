{-
Implementieren Sie eine rekursive, nicht endrekursive Funktion take' und eine endrekursive Funktion take'', die zwei Parameter erwartet, die Anzahl der zu entnehmenden Elemente einer Liste
und eine Liste. Bei den Argumenten 2 und [9, 12, 18] sollte die Funktion beispielsweise die
Liste [9, 12] zuruckgeben. F ¨ ur Randf ¨ ¨alle sollte sie sich genauso verhalten wie die entsprechende
Standardbibliothek.
-}
{-
english translation:
Implement a recursive, non-tail-recursive function take' and a tail-recursive function take'', each of which takes two parameters: the number of elements to take from a list, and the list itself.
For example, given the arguments 2 and [9, 12, 18], the function should return [9, 12].
For edge cases, the function should behave the same as the corresponding standard library function.
-}

-- recursive
take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) xs (acc ++ [x])