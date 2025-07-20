cleanVotes :: [String] -> [String]
cleanVotes = map (liftA2 (&&) ((>= 4) . length) (/= "None"))

{-
Ground Truth Metadata:
Error Type: Falsche Standardfunktion verwendet / Incorrect Standard Function Used
Short Description: The function uses `map` instead of `filter`, which transforms each element into a `Bool` rather than removing elements based on a condition.
Intended Root Cause: Student confuses the purpose of `map` and `filter` and mistakenly uses `map` when the task requires selecting elements that satisfy a predicate.
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich