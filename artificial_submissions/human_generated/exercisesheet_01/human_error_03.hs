    doubleList :: [Int] -> [Int]
doubleList list = map(*2) list

{-
Ground Truth Metadata:
Error Type: Einrückungsfehler / Indentation Error
Short Description: The type signature is indented, which is not allowed for top-level declarations in Haskell.
Intended Root Cause: Student does not follow Haskell's rule that type signatures must begin at the start of the line.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler