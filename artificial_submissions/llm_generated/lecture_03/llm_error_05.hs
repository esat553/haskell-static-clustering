describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

isUnitVector :: Bool
isUnitVector = (a == 1 && b == 0 && c == 0) || (a == 0 && b == 1 && c == 0)

describeTriple :: (Int, Int, Int) -> String
describeTriple (a, b, c)
  | isUnitVector = "Einheitsvektor"
  | otherwise    = "Gemischt"

{-
Ground Truth Metadata:
Error Type: Ungültiger Gültigkeitsbereich / Invalid Scope Reference
Short Description: The function `isUnitVector` is defined outside the scope of `a`, `b`, and `c`, but references them as if they were in scope.
Intended Root Cause: Student misunderstands lexical scoping in Haskell and assumes that variables from a pattern match are visible in top-level helper functions.
Affected Line(s): 5
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich