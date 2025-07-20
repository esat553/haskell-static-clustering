describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (0, 0, 0) = "Nulltupel"
describeTriple (a, b, c)
  | allEqual = "Gleich"
  | otherwise  = "Gemischt"
where
allEqual = a == b && b == c 
{-
Ground Truth Metadata:
Error Type: Einrückungsfehler / Intendation Error
Short Description: The `where` clause is not correctly indented relative to the preceding equation, violating Haskell's layout rules.
Intended Root Cause: Student does not correctly apply Haskell’s layout sensitivity and assumes that `where` blocks can be defined independently of indentation level.
Affected Line(s): 9–10
-}
-- RegEx-Cluster: Parse-Fehler