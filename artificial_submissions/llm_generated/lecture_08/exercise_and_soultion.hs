{-
Schreibe eine Funktion analyse :: [String] -> [(Char, Int)], die für jede Zeichenkette in der Liste den ersten Buchstaben extrahiert und zurückgibt, wie oft dieser in der gesamten Liste an erster Stelle vorkommt. Verwende dabei map, filter, foldr und anonyme Funktionen.
-}
{-
english translation:
Write a function analyse :: [String] -> [(Char, Int)] that, for each string in the list, extracts the first character and returns how often this character appears as the first character in the entire list.
Use map, filter, foldr, and anonymous functions.
-}

analyse :: [String] -> [(Char, Int)]
analyse xs = 
    let firstChars = map head xs
    in map (\s -> 
        let c = head s
            count = foldr (\x acc -> if x == c then acc + 1 else acc) 0 firstChars
        in (c, count)
    ) xs