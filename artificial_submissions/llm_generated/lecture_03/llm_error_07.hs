describeTripleList :: [String] -> [(Int, Int, Int)]
describeTripleList tuples = map describeTriple tuples

describeTriple :: (Int, Int, Int) -> String
describeTriple (0, 0, 0) = "Nulltupel"
describeTriple _ = "Anderes"

{-
Ground Truth Metadata:
Error Type: Falscher Funktionstyp / Incorrect Function Type
Short Description: The type signature of `describeTripleList` declares a return type of `[(Int, Int, Int)]`, but the function returns a list of strings.
Intended Root Cause: Student confuses input and output types in the type signature and inverts them, leading to a mismatch between the declared and actual function behavior.
Affected Line(s): 1-2
-}
-- RegEx-Cluster: Typenkonflikt