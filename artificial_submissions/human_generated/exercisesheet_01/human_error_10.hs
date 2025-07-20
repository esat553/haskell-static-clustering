doubleList :: [Int] -> [Int]
let doubleList = [] * 2

{-
Ground Truth Metadata:
Error Type: Fehlerhafte let-Verwendung / Incorrect let Usage
Short Description: The keyword 'let' is incorrectly used at the top level of a function definition, and the expression '[] * 2' is also invalid for list multiplication in Haskell.
Intended Root Cause: Student misunderstands the use of 'let' bindings and list operations in Haskell, attempting to use both incorrectly.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler