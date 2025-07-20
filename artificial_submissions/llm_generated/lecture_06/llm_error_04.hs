data TreeMap k v = Empty | Node k v TreeMap TreeMap

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v
    | otherwise      = Nothing

{-
Ground Truth Metadata:
Error Type: Fehlende Parametrisierung des Datentyps / Missing Type Parameters in Recursive Reference
Short Description: The recursive references to `TreeMap` in the `Node` constructor omit the type parameters `k` and `v`, making the data type invalid.
Intended Root Cause: Student assumes that recursive occurrences of a type constructor do not require explicit parameterization and omits the type arguments.
Affected Line(s): 1
-}
-- RegEx-Cluster: Kind-Konflikt (Constraint vs. Typ)