describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (a, b, c)
  | a == 0 && b == 0 && c == 0 = "Nulltupel"
  | a == b && b == c && a /= 0 = 0
  | otherwise                  = "Gemischt"

{-
Ground Truth Metadata:
Error Type: Inkonsistenter Rückgabetyp / Inconsistent Return Type
Short Description: One guard in the function returns an `Int` instead of a `String`, which does not match the declared return type of the function.
Intended Root Cause: Student confuses the expected output type of the function and accidentally returns a numeric literal instead of a string value.
Affected Line(s): 7
-}
-- RegEx-Cluster: Numerischer Typenkonflikt