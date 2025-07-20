data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> v -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey val (Node k v left right)
    | searchKey == k = Just v
    | otherwise      = Nothing

{-
Ground Truth Metadata:
Error Type: Falsche Typdeklaration / Incorrect Type Declaration
Short Description: The type signature incorrectly declares `v` as an input parameter, although it should only appear in the result type. This leads to a mismatch with the function definition and patterns.
Intended Root Cause: Student misunderstands how to declare function types and mistakenly treats the result type `v` as an input parameter.
Affected Line(s): 3
-}
-- RegEx-Cluster: Abweichende Arity