dropLastN :: Int -> [a] -> [a]
dropLastN n (x:xs)
    | n > 0     = [x] : dropLastN (n-1) xs
    | otherwise = x:xs

{-
Ground Truth Metadata:
Error Type: Falsche Listenkonstruktion / Incorrect List Construction
Short Description: The expression `[x] : ...` constructs a list of lists, although the function is expected to return a flat list `[a]`.
Intended Root Cause: Student confuses the list constructor `:` with list literals and mistakenly wraps an element in brackets, leading to a type mismatch.
Affected Line(s): 3
-}
-- RegEx-Cluster: Unendlicher Typ