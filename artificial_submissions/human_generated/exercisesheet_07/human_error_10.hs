foo n = sqrt . (5 *) . sum . flip take n [1..50]

{-
Ground Truth Metadata:
Error Type: Falsche Argumentreihenfolge / Wrong Argument Order
Short Description: The argument order is incorrect; 'n' is applied before 'take', which does not match the expected order for 'flip take'.
Intended Root Cause: Student misunderstands the argument order required for higher-order functions and applies the variable in the wrong place within the composition.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonflikt