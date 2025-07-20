doubleList :: [Int] --> [Int]
doubleList list = map(*2) list

{-
Ground Truth Metadata:
Error Type: Falsche Syntax in Typsignatur / Wrong Syntax in Type Signature
Short Description: The type signature uses '-->' instead of the correct '->' to denote the function type.
Intended Root Cause: Student confuses or mistypes the function arrow in the type signature, possibly influenced by other languages or a typo.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich