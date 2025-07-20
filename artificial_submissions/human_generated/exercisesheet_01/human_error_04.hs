doubleList :: [Int] -> [Int]
doubledList list = map(*2) list


{-
Ground Truth Metadata:
Error Type: Inkonsistenter Funktionsname / Function Name Mismatch
Short Description: The function implementation is named 'doubledList', but the type signature declares 'doubleList', leading to a mismatch.
Intended Root Cause: Student introduces a typo or inconsistency in function naming between the type signature and the implementation.
Error Class: Mental Typo (Korkmaz et al. 2015, Level 0)
Affected Line(s): 2
-}
-- RegEx-Cluster: Fehlendes Binding