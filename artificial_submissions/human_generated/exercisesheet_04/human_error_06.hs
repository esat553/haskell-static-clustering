sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x != 0]

{-
Ground Truth Metadata:
Error Type: Typkonflikt durch falschen Operator / Type Mismatch due to Wrong Operator
Short Description: The operator '!=' is used for inequality, but Haskell uses '/='; this leads to a type error or unrecognized operator.
Intended Root Cause: Student applies syntax from other programming languages (e.g., C, Java) and uses '!=' instead of Haskell's '/=' operator.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich