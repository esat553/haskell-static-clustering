analyse :: [String] -> [(Char, Int)]
analyse xs = map (head . filter) xs

{-
Ground Truth Metadata:
Error Type: Unvollständige Funktionsanwendung / Incomplete Function Application
Short Description: The function `filter` is used without providing the required predicate argument, leading to an invalid function composition with `head`.
Intended Root Cause: Student misunderstands the arity of `filter` and assumes it can be partially applied or composed without explicitly supplying a predicate.
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität