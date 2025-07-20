data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = v
    | otherwise      = lookupTM searchKey left

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp / Incorrect Return Type
Short Description: The function returns `v` instead of `Just v`, which does not match the expected return type `Maybe v`.
Intended Root Cause: Student forgets to wrap the result in a `Just` constructor and returns a plain value where a `Maybe` is expected.
Affected Line(s): 6
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur