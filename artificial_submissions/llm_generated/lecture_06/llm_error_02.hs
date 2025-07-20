data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v
    | searchKey < k  = lookupTM searchKey left
    | otherwise      = lookupTM searchKey right

{-
Ground Truth Metadata:
Error Type: Falsche Typklasse in Signatur / Incorrect Type Class in Signature
Short Description: The function uses the `<` operator, which requires an `Ord` constraint, but the type signature only specifies `Eq`.
Intended Root Cause: Student underestimates the type requirements for ordered comparisons and includes only `Eq` instead of the necessary `Ord` constraint in the type signature.
Affected Line(s): 3
-}
-- RegEx-Cluster: Constraint nicht erfüllbar