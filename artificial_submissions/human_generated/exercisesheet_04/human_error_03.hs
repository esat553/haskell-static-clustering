sieve :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Inkonsistenter Funktionsname / Function Name Mismatch
Short Description: The function name in the declaration ('sieve') does not match the function name in the definitions ('sieve'').
Intended Root Cause: Student inconsistently uses different names for the same function, causing confusion and definition errors.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Fehlendes Binding