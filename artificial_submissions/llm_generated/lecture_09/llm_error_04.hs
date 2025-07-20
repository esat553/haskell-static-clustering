cleanVotes :: [String] -> [String]
cleanVotes = filter liftA2 (&&) ((>= 4) . length) (/= "None")

{-
Ground Truth Metadata:
Error Type: Falsche Argumentanordnung / Incorrect Argument Placement
Short Description: The function `liftA2` is not grouped correctly as an argument to `filter`, leading to a misinterpretation of the intended function structure.
Intended Root Cause: Student attempts to use `liftA2` inside a `filter` expression without proper parentheses or composition, resulting in syntactically invalid placement of function arguments.
Affected Line(s): 2
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur