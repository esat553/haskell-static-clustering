cleanVotes :: [String] -> [String]
cleanVotes = filter ( ((>= 4) . length) . (&&) . (/= "None") )
{-
Ground Truth Metadata:
Error Type: Falsche Funktionskomposition / Incorrect Function Composition
Short Description: The expression attempts to compose boolean functions using `(.)` and `(&&)` in a way that does not typecheck, since `(&&)` is a binary operator and cannot be composed like a unary function.
Intended Root Cause: Student misunderstands how to combine multiple predicate functions and incorrectly uses function composition instead of a combinator like `liftA2`.
Affected Line(s): 2
-}
-- RegEx-Cluster: Fehlende Instanz