data treeMap k v = Empty | Node k v (treeMap k v) (treeMap k v)

lookupTM :: Eq k => k -> treeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v
    | otherwise = Nothing

{-
Ground Truth Metadata:
Error Type: Ungültiger Typname / Invalid Type Name Capitalization
Short Description: The type constructor `treeMap` is not capitalized, which violates Haskell’s syntax rules for type and data constructors.
Intended Root Cause: Student does not apply Haskell’s naming conventions and mistakenly uses a lowercase name for a data type.
Affected Line(s): 1, 3
-}
-- RegEx-Cluster: Fehlerhafter Typ-Header