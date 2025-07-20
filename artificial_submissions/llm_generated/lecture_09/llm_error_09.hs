cleanVotes :: [String] -> [String]
cleanVotes = filter (liftA2 (&&) (length . (> 4)) (/= "None") )

{-
Ground Truth Metadata:
Error Type: Falsche Reihenfolge in Funktionskomposition / Incorrect Function Composition Order
Short Description: The expression `(length . (> 4))` attempts to apply a number to a function instead of composing functions correctly; the comparison and `length` are in the wrong order.
Intended Root Cause: Student misunderstands the direction of function composition and writes `length . (> 4)` instead of `(> 4) . length`.
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich