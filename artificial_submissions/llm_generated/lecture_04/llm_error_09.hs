dropLastN :: Int -> [a] -> [a]
dropLastN n (x:xs) =
    if n > 0 then
        n = n - 1
        dropLastN n xs
    else
        x:xs

{-
Ground Truth Metadata:
Error Type: Ungültige Zuweisung / Invalid Assignment
Short Description: The expression `n = n - 1` is used as if it were a mutable assignment, which is not allowed in Haskell’s purely functional paradigm.
Intended Root Cause: Student applies imperative thinking and expects to mutate the value of `n` inside a conditional block.
Affected Line(s): 5
-}
-- RegEx-Cluster: Parse-Fehler