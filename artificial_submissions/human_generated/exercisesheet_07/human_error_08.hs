foo = sqrt . (5 *) . sum , flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Falsche Operator-Syntax / Wrong Operator Syntax
Short Description: A comma ',' is used instead of a dot '.' for function composition, resulting in an invalid expression.
Intended Root Cause: Student makes a careless typographical error (typo) and uses ',' instead of '.' despite otherwise correct usage of function composition.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler