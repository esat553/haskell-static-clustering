rest' :: String -> String
rest' (x:xs) = "" + xs

{-
Ground Truth Metadata:
Error Type: Typkonflikt durch falschen Operator / Type Mismatch due to Wrong Operator
Short Description: The '+' operator is used for string concatenation, but Haskell requires the '++' operator for concatenating strings.
Intended Root Cause: Student confuses string concatenation in Haskell with the syntax of other programming languages, using '+' instead of '++'.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Numerischer Typenkonflikt