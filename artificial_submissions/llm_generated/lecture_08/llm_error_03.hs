analyse :: [String] -> [(Char, Int)]
analyse xs = map (\s -> length (filter (== head s) (map head xs))) xs

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp der Lambda-Funktion / Incorrect Return Type of Lambda Function
Short Description: The lambda function returns only an `Int`, although the function is expected to return a tuple `(Char, Int)` for each string.
Intended Root Cause: Student focuses solely on computing the count and forgets to include the corresponding character in the result tuple.
Affected Line(s): 2
-}
-- RegEx-Cluster: Typenkonflikt