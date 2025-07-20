dropLastN :: Int -> [a] -> [a]
dropLastN n xs = go xs (dropHelper n xs)

    go :: [a] -> [a] -> [a]
    go _ [] = []
    go (y:ys) (z:zs) = y : go ys zs

{-
Ground Truth Metadata:
Error Type: Fehlende 'where'-Klausel / Missing 'where' Clause
Short Description: Local definitions are indented as if they belong to a `where` block, but no `where` keyword is present, making the syntax invalid.
Intended Root Cause: Student assumes that indentation alone is sufficient for local definitions and forgets that a `where` keyword is required to introduce them.
Affected Line(s): 3–6
-}
-- RegEx-Cluster: Parse-Fehler