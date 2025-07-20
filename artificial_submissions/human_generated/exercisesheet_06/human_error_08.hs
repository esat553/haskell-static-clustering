unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (xs ++ x, ys ++ y)) ([], [])

{-
Ground Truth Metadata:
Error Type: Falsche Operatorwahl / Incorrect Operator Choice
Short Description: The '++' operator is used to append an element to a list, but '++' is for concatenating lists. The cons operator ':' should be used to add a single element.
Intended Root Cause: Student confuses the use of list concatenation ('++') with list construction (':'), leading to incorrect list manipulation.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur