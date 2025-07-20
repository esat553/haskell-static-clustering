dropLastN :: Int -> [a] -> [a]
dropLastN n xs = go xs (dropHelper n xs)
  where
    dropHelper k list = if k <= 0 then list else dropHelper (k-1) (tail list)

    go :: [a] -> [a] -> [a]
    go _ [] = []
    go (y:ys) (z:zs) = y : go ys

{-
Ground Truth Metadata:
Error Type: Falsche Argumentanzahl bei Funktionsaufruf / Incorrect Number of Arguments in Function Call
Short Description: The recursive call to `go` provides only one argument, although `go` is defined to take two arguments.
Intended Root Cause: Student forgets to pass the second argument in a recursive call and overlooks the function’s full parameter structure.
Affected Line(s): 8
-}
-- RegEx-Cluster: Falsche Funktionsarität