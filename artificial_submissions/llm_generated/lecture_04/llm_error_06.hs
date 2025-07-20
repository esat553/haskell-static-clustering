dropLastN :: Int -> [a] -> [a]
dropLastN n xs = go xs
  where
    go :: [a] -> [a]
    go [] = []
    go (y:ys)
      | n > 0 = go ys
      | otherwise = y : go ys

{-
Ground Truth Metadata:
Error Type: Variable nicht im Gültigkeitsbereich / Variable Not in Scope
Short Description: The helper function `go` uses the variable `n`, which is not in scope because it was not passed as a parameter.
Intended Root Cause: Student assumes that top-level parameters are implicitly available in nested helper functions without explicitly passing them.
Affected Line(s): 7
-}
-- RegEx-Cluster: 