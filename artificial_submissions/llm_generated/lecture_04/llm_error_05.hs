dropLastN :: Int -> [a] -> [a]
dropLastN n xs
  | n <= 0 xs
  | otherwise = []

{-
Ground Truth Metadata:
Error Type: Syntaxfehler in Guard-Bedingung / Syntax Error in Guard Condition
Short Description: The guard `n <= 0 xs` is syntactically invalid because it lacks an `=` and improperly combines a condition with an expression.
Intended Root Cause: Student misunderstands the syntax of guards and incorrectly appends the list `xs` directly to the condition without a result expression.
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler