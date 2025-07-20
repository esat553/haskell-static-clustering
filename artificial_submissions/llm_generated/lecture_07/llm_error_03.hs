data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = v
    | otherwise      = lookupTM searchKey left

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp / Incorrect Return Type
Short Description: The function returns `v` instead of `Just v`, which does not conform to the declared return type `Maybe v`.
Intended Root Cause: Student forgets to wrap the result value in the `Just` constructor and treats the function as if it returns a raw value.
Affected Line(s): 5
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur