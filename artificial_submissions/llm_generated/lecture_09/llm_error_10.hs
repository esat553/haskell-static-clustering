cleanVotes :: [String] -> [String]
cleanVotes = filter . ((>= 4) . length)

{-
Ground Truth Metadata:
Error Type: Unvollständige Funktionskomposition / Incomplete Function Composition
Short Description: The composition `filter . ((>= 4) . length)` results in a function that expects an additional argument, producing a higher-order function instead of `[String] -> [String]`.
Intended Root Cause: Student attempts a pointfree definition but forgets to apply the composed predicate to `filter`, leaving the function incomplete.
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität