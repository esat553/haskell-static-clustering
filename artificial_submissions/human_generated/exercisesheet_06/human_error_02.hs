unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldl (\(xs, ys) (x, y) --> (xs ++ [x], ys ++ [y])) ([], [])

{-
Ground Truth Metadata:
Error Type: Fehlerhafte Lambda-Syntax / Incorrect Lambda Syntax
Short Description: The lambda function uses '-->' instead of '->' as the parameter-to-body separator.
Intended Root Cause: Student makes a typographical error in the lambda expression by confusing the correct arrow symbol.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler