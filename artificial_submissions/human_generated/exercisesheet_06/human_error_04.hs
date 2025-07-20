unzip'' :: (a, b) -> ([a], [b])
unzip'' = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], [])

{-
Ground Truth Metadata:
Error Type: Falscher Typkonstruktor in Typensignatur / Wrong Type Constructor in Type Signature
Short Description: The type signature uses a tuple '(a, b)' as the input instead of a list of tuples '[(a, b)]', which does not match the intended function.
Intended Root Cause: Student confuses the expected input type and forgets that the function should operate on a list of tuples, not a single tuple.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur