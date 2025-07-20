describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (a,b,c)
  | a == b && b == c = result
  | otherwise = "Gemischt"
  where
    result (0,0,0) = "Nulltupel"
    result (_,_,_) = "Gleich"

{-
Ground Truth Metadata:
Error Type: Ungültige Definition in 'where'-Block / Invalid Definition in 'where' Block
Short Description: The `where` clause attempts to define `result` using pattern matching on arguments, but `result` is not a function and no parameters are passed to it in the guard body.
Intended Root Cause: Student misunderstands how local definitions in `where` clauses work and incorrectly tries to pattern-match on values that are not passed explicitly.
Affected Line(s): 8–10
-}
-- RegEx-Cluster: Falsche Funktionsarität