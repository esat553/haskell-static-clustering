unzip'' :: [(a, b)] -> ([a], [b])
unzip'' = foldr (\x, y xs, ys -> (x:xs, y:ys)) ([], [])

{-
Ground Truth Metadata:
Error Type: Fehlerhafte Lambda-Syntax / Incorrect Lambda Syntax
Short Description: The lambda expression uses commas to separate parameters instead of a tuple pattern or correct parameter list syntax.
Intended Root Cause: Student misunderstands Haskell's syntax for lambda expressions and uses commas instead of the required tuple pattern for multiple parameters.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler