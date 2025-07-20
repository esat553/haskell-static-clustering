sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y = y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Fehlerhafte List-Comprehension-Syntax / Incorrect List Comprehension Syntax
Short Description: An equals sign '=' is used instead of a pipe '|' in the list comprehension.
Intended Root Cause: Student does not know or misremembers the correct symbol for list comprehensions in Haskell and uses '=' instead of '|'.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler