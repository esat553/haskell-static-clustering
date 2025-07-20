foo = sqrt . (5 *) sum . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Fehlerhafte Funktionsverkettung / Incorrect Function Composition
Short Description: A point ('.') is missing between '(5 *)' and 'sum', so the function composition is incomplete, leading to a syntax or type error.
Intended Root Cause: Student forgets that each function in a pointfree composition must be connected with a composition operator, resulting in an invalid expression.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Numerischer Typenkonflikt