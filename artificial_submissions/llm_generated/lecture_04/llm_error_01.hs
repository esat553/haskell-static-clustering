dropLastN = Int -> [a] -> [a]
dropLastN n xs = xs 
{-
Ground Truth Metadata:
Error Type: Falscher Operator in Typdeklaration / Incorrect Operator in Type Declaration
Short Description: The function `dropLastN` uses `=` instead of `::` in its type signature, which is invalid Haskell syntax.
Intended Root Cause: Student confuses the syntax for type annotations with value definitions and mistakenly uses `=` to assign a type.
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler