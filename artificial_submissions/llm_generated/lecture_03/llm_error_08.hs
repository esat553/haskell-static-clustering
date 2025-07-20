describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList [] = []
describeTripleList (x:xs) = describeTriple xs : describeTripleList xs

describeTriple :: (Int, Int, Int) -> String
describeTriple _ = "Ein String"
{-
Ground Truth Metadata:
Error Type: Falsche Funktionsanwendung / Incorrect Function Application in Recursion
Short Description: The recursive call applies `describeTriple` to the entire tail `xs` instead of the current element `x`, resulting in a type mismatch.
Intended Root Cause: Student confuses the recursive structure of list processing and mistakenly applies an element-wise function to the entire list.
Affected Line(s): 3
-}
-- RegEx-Cluster: Typenkonflikt