sieve' :: [Integer] -> [Int]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Inkonsistente Typsignatur / Inconsistent Type Signature
Short Description: The function is declared to return '[Int]' but processes '[Integer]', causing inconsistency between declared and actual types.
Intended Root Cause: Student confuses or inconsistently uses similar numeric types in the type signature and implementation.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Numerischer Typenkonflikt