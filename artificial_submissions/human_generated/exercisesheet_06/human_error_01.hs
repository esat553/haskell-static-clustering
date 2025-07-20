unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\xs \ys (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])

{-
Ground Truth Metadata:
Error Type: Fehlerhafte Lambda-Syntax / Incorrect Lambda Syntax
Short Description: The lambda function is written with multiple backslashes (separate parameter sections) instead of a single parameter list.
Intended Root Cause: Student misunderstands Haskell's syntax for lambda expressions with multiple parameters and separates them with multiple backslashes instead of a tuple.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler