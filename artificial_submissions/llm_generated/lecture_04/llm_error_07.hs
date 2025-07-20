dropLastN :: Int -> [a] -> [a]
dropLastN 0 xs = xs
dropLastN (n+1) (x:xs) = dropLastN n xs
dropLastN _ [] = []

{-
Ground Truth Metadata:
Error Type: Ungültiges Pattern Matching / Invalid Pattern Matching Expression
Short Description: The pattern `(n+1)` is not allowed in Haskell, as arithmetic expressions are not permitted in pattern matches.
Intended Root Cause: Student incorrectly assumes that Haskell supports arithmetic deconstruction in patterns, similar to languages with more flexible pattern syntax.
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler