cleanVotes :: [String] -> [String]
cleanVotes = filter ( ((>= 4) . length) && (/= "None") )

{-
Ground Truth Metadata:
Error Type: Falsche Kombination von Prädikaten / Invalid Predicate Combination
Short Description: The boolean operator `&&` is used to combine two functions directly, rather than combining their results after applying them to an argument.
Intended Root Cause: Student assumes that `&&` can be used to combine predicate functions without applying them, overlooking that functions must be combined using a higher-order combinator like `liftA2`.
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität