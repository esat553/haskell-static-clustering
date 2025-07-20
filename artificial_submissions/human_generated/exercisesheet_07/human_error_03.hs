foo = sum . (5 *) . sqrt . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Fehlerhafte Funktionsverkettung / Incorrect Function Composition
Short Description: The order of function composition is incorrect; 'sqrt' is applied after 'sum', which is not valid for the expected input and output types.
Intended Root Cause: Student misunderstands the flow of data in function composition and arranges the functions in an order that causes a type mismatch.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Numerischer Typenkonflikt