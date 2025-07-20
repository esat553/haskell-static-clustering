take' z [] = []
take' z (x:xs)  if z <= 0 = []
                else otherwise = x : take' z 1 xs

{-
Ground Truth Metadata:
Error Type: Fehlerhafte If-Then-Else-Syntax / Incorrect If-Then-Else Syntax
Short Description: The function definition uses 'if' and 'else' incorrectly in a guard position, and the syntax does not follow Haskell's rules for conditional expressions.
Intended Root Cause: Student misunderstands Haskell's syntax for guards versus if-then-else expressions, leading to incorrect usage.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler