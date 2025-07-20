cleanVotes :: [String] -> [String]
cleanVotes = filter . liftA2 (&&) ((>= 4) . length) (/= "None")

{-
Ground Truth Metadata:
Error Type: Unvollständige Funktionsanwendung / Incomplete Function Application
Short Description: The expression `filter . liftA2 (&&) ...` composes `filter` with a function that still expects an argument, resulting in a function of type `a -> [String] -> [String]` instead of `[String] -> [String]`.
Intended Root Cause: Student attempts pointfree composition but forgets that `filter` expects a predicate, not a function constructor that still requires input.
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich