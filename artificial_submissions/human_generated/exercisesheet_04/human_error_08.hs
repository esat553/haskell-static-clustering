sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x = 0]

{-
Ground Truth Metadata:
Error Type: Falsche Operator-Syntax / Wrong Operator Syntax
Short Description: The assignment operator '=' is incorrectly used instead of the equality operator '==' in the list comprehension condition.
Intended Root Cause: Student confuses assignment and equality operators, leading to incorrect syntax.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler