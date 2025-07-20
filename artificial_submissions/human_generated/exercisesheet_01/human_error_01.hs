doubleList :: [Int] -> [Int]
doubleList list = map((*2), list)

{-
Ground Truth Metadata:
Error Type: Fehlerhafter Funktionsaufruf / Wrong Function Call Syntax
Short Description: The arguments to 'map' are enclosed in parentheses and separated by a comma, which is not the correct function application syntax in Haskell.
Intended Root Cause: Student confuses function application syntax in Haskell with that of other languages, using parentheses and a comma.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität