cleanVotes :: [String] -> [String]
cleanVotes = filter where
  cond1 = ((>= 4) . length)
  cond2 = (/= "None")

{-
Ground Truth Metadata:
Error Type: Falsche Verwendung von `where` ohne Definition / Misuse of `where` Without a Definition
Short Description: The `where` clause is used without a preceding function body to attach it to, leaving `filter where` as an incomplete and invalid expression.
Intended Root Cause: Student incorrectly assumes that `where` can define variables independently of an enclosing expression, and forgets to provide the actual predicate to `filter`.
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität