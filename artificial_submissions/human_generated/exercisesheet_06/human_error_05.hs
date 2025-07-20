unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\(xs, ys) (x, y) -> xs ++ [x], ys ++ [y]) ([], [])
-- oder
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

{-
Ground Truth Metadata:
Error Type: Fehlende Klammerung im Ausdruck / Missing Parentheses in Expression
Short Description: Parentheses are missing in the tuple construction within the lambda body, so '(xs ++ [x], ys ++ [y])' is written without parentheses.
Intended Root Cause: Student overlooks the need for parentheses when returning a tuple in a lambda expression, leading to an invalid or unintended result.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich