doubleList :: {Int} -> {Int}
doubleList list = map(*2) list

{-
Ground Truth Metadata:
Error Type: Falsche Syntax in Typsignatur / Wrong Syntax in Type Signature
Short Description: Curly braces '{ }' are used instead of square brackets '[ ]' to denote a list type in the type signature.
Intended Root Cause: Student confuses Haskell's list type notation, using curly braces commonly found in other languages instead of square brackets.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler in Funktionsdeklaration