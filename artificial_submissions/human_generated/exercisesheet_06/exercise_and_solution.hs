{-
Definieren Sie die Funktion unzip' :: [(a, b)] -> ([a], [b]), die sich genauso verhält wie die vordefinierte Funktion unzip
-}
{-
english translation:
Define the function unzip' :: [(a, b)] -> ([a], [b]) that behaves exactly like the predefined function unzip.
-}
unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\(xs, ys) (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])
-- oder
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])