describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (a, b, c)
  | checkEquality = "Gleich"
  | otherwise     = "Gemischt"
  where
    checkEquality = if a == b && b == c then 1 else 0

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp in Hilfsfunktion / Incorrect Return Type in Helper Function
Short Description: The helper function `checkEquality` returns an `Int`, but the guard condition expects a `Bool`.
Intended Root Cause: Student confuses conditional expressions with Boolean logic and does not recognize that guards require a Boolean condition.
Affected Line(s): 9
-}
-- RegEx-Cluster: Numerischer Typenkonflikt