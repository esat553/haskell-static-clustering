{-
Implementiere eine Funktion dropLastN :: Int -> [a] -> [a], die die letzten n Elemente einer Liste entfernt. Verwende ausschließlich rekursive Definitionen (keine Bibliotheksfunktionen wie length, take, drop). Achte darauf, dass die Funktion auch für n > length xs sinnvoll terminiert.
-}
{-
english translation:
Implement a function dropLastN :: Int -> [a] -> [a] that removes the last n elements from a list. Use only recursive definitions (do not use library functions such as length, take, or drop). Ensure that the function terminates sensibly even when n > length xs.
-}

dropLastN :: Int -> [a] -> [a]
dropLastN n xs
  | n <= 0    = xs
  | otherwise = go xs (dropHelper n xs)
  where
    dropHelper :: Int -> [b] -> [b]
    dropHelper 0 zs = zs
    dropHelper k (_:zs) = dropHelper (k-1) zs
    dropHelper _ [] = []

    go :: [a] -> [b] -> [a]
    go _ [] = []
    go (y:ys) (_:zs) = y : go ys zs