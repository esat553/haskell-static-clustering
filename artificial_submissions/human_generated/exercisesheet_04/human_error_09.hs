sieve' :: [Integer] -> [Integer]
sieve' [] = []
let sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x /= 0]

{-
Ground Truth Metadata:
Error Type: Fehlerhafte let-Verwendung / Incorrect let Usage
Short Description: The keyword 'let' is incorrectly used in a top-level function definition.
Intended Root Cause: Misunderstanding of Haskell's `let` syntax; student treats top-level function definition as a local declaration.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler