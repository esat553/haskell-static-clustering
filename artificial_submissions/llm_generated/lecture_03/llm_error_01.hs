describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple (a, b, c)
  | a == 0 && b == 0 && c == 0 -> "Nulltupel"
  | a == b && b == c             -> "Gleich"  
  | otherwise                    -> "Gemischt"

{-
Ground Truth Metadata:
Error Type: Falsche Guard-Syntax / Incorrect Guard Syntax
Short Description: The guards in the function 'describeTriple' use '->' instead of '=', which is invalid Haskell syntax and causes a parse error.
Error Location: In the function 'describeTriple'.
Intended Root Cause: Student misremembers the syntax of guard expressions and incorrectly uses the arrow symbol '->' instead of the equals sign '=' to define the result of a matched guard.
Affected Line(s): 6-8
-}
-- RegEx-Cluster: Parse-Fehler