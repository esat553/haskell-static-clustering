analyse :: [String] -> [(Char, Int)]
analyse xs = map (\s, c -> (c, 0)) xs
  where
    firstChars = map head xs
    count c = length (filter (==c) firstChars)

{-
Ground Truth Metadata:
Error Type: Ungültige Lambda-Parameterstruktur / Invalid Lambda Parameter Structure
Short Description: The lambda expression uses a comma-separated pair of arguments `(\s, c -> ...)`, which is not valid in Haskell. Lambdas can only take a single parameter unless destructuring a tuple in parentheses.
Intended Root Cause: Student confuses tuple patterns with multiple parameter syntax and incorrectly tries to pass two arguments to a lambda without using a tuple pattern.
Affected Line(s): 2
-}
-- RegEx-Cluster: Parse-Fehler