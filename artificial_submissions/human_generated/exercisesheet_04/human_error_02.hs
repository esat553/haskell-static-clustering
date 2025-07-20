sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [ y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Fehlerhafte List-Comprehension-Syntax / Incorrect List Comprehension Syntax
Short Description: The list comprehension is missing the pipe '|' symbol before the generator, causing incorrect syntax.
Intended Root Cause: Student omits the required '|' symbol in the list comprehension syntax.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler