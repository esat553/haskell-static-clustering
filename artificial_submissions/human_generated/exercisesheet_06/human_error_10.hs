unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\(xs, ys) (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])
-- oder
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (a:xs, b:ys)) ([], [])

{-
Ground Truth Metadata:
Error Type: Falsche Variablennamen / Wrong Variable Names
Short Description: The variables 'a' and 'b' are used in the function body instead of the correct pattern variables 'x' and 'y' as bound in the lambda expression.
Intended Root Cause: Student loses track of variable naming in pattern matching and uses names that are not in scope or do not match the lambda pattern.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 5
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich