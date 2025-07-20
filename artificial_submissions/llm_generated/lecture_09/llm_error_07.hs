cleanVotes :: [String] -> [String]
cleanVotes = filter ( (&&) ((>= 4) . length) ((/= "None")) )

{-
Ground Truth Metadata:
Error Type: Falsche Anwendung von `(&&)` auf Funktionen / Incorrect Application of `(&&)` to Functions
Short Description: The boolean operator `(&&)` is applied directly to two functions instead of their results, which is not valid in functional composition.
Intended Root Cause: Student assumes that `(&&)` can combine functions like predicate combinators, overlooking the need for a higher-order function such as `liftA2` to combine their return values.
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität