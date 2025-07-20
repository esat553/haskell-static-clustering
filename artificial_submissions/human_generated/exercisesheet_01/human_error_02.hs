doubleList :: [Int] -> [Int]
    doubleList list = map(*2) list

{-
Ground Truth Metadata:
Error Type: Einrückungsfehler / Indentation Error
Short Description: The function definition is indented, which is not allowed for top-level declarations in Haskell.
Intended Root Cause: Student is unaware of or overlooks the requirement that top-level function definitions must not be indented.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich