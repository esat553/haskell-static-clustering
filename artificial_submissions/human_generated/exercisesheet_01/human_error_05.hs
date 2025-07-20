doubledList :: [Int] -> [Int]
doubleList list = map(*2) list

{-
Ground Truth Metadata:
Error Type: Inkonsistenter Funktionsname / Function Name Mismatch
Short Description: The type signature declares 'doubledList', but the function implementation is named 'doubleList', causing a mismatch between signature and definition.
Intended Root Cause: Student introduces inconsistency in naming, possibly due to a typo or copy-paste error.
Error Class: Mental Typo (Korkmaz et al. 2015, Level 0)
Affected Line(s): 2
-}
-- RegEx-Cluster: Fehlendes Binding