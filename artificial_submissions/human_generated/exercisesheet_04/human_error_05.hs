sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, mod xs x /= 0]


{-
Ground Truth Metadata:
Error Type: Verwechslung der Variablen / Variable Mix-up
Short Description: The variable 'xs' is mistakenly used as the first argument to 'mod' instead of 'y', mixing up variable usage within the list comprehension.
Intended Root Cause: Student confuses variable names within the comprehension, leading to incorrect application of functions to unintended variables.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Typenkonflikt