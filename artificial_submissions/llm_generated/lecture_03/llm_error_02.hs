describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (0, 0, 0)     = "Nulltupel"
describeTriple (x, x, x)     = "Gleich"
describeTriple (1, 0, 0)     = "Einheitsvektor"
describeTriple (0, 1, 0)     = "Einheitsvektor"
describeTriple (0, 0, 1)     = "Einheitsvektor"
describeTriple (_, _, _)     = "Gemischt"

{-
Ground Truth Metadata:
Error Type: Mehrfachbindung einer Pattern-Variable / Rebinding of Pattern Variable
Short Description: The pattern '(x, x, x)' attempts to bind the same variable 'x' multiple times in the same pattern, which is invalid in Haskell and leads to a compilation error.
Intended Root Cause: Student assumes that using the same variable name multiple times in a pattern will test for equality, not realizing that this creates separate bindings.
Affected Line(s): 6
-}
-- RegEx-Cluster: Mehrfachdefinition in Funktionsgleichung