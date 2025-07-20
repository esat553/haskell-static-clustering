sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Falsche Operator-Syntax / Wrong Operator Syntax
Short Description: The recursive Function calls the itself with a syntax mistake.
Intended Root Cause: Student makes simple mistake either because of a typo or he forgot the function namegut.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Funktion nicht definiert