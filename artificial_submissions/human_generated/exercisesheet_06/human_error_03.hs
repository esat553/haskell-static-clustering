unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(xs, ys) (x, y) -> (xs ++ [x], ys ++ [y])) ([], [])


{-
Ground Truth Metadata:
Error Type: Ungeeignete Standardfunktion / Inappropriate Standard Function Use
Short Description: The function 'foldr' is used instead of 'foldl', which does not match the expected argument structure for this specific implementation.
Intended Root Cause: Student confuses the appropriate use of standard folding functions and selects the wrong variant for the intended accumulator and argument order.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur