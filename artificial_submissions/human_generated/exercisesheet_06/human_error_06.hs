unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldll (\(xs, ys) (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Funktionsnamen / Spelling Error in Function Name
Short Description: The function name 'foldll' is misspelled; it should be 'foldl'.
Intended Root Cause: Student makes a typographical error when writing the function name, resulting in an undefined function.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich