unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (xs ++ [x] , ys ++ [y])) ([], [])unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\(xs, ys) (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])
-- oder
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

{-
Ground Truth Metadata:
Error Type: Doppelte Funktionsdefinition / Duplicate Function Definition
Short Description: The function 'unzip'' is defined more than once in the same scope.
Intended Root Cause: Student does not recognize that each function name must be unique within the same module or script and mistakenly redefines an existing function.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1-3, 5-6
-}
-- RegEx-Cluster: Mehrfache Deklarationen