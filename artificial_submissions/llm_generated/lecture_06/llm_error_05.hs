data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v)
    | searchKey == k = Just v
    | otherwise      = Nothing

{-
Ground Truth Metadata:
Error Type: Ungültiges Pattern im Konstruktor / Invalid Constructor Pattern
Short Description: The pattern `Node k v` does not match the full structure of the constructor, which requires four components: key, value, left subtree, and right subtree.
Intended Root Cause: Student misremembers the arity of the `Node` constructor and provides too few pattern variables, resulting in a mismatch.
Affected Line(s): 5
-}
-- RegEx-Cluster: Falsche Arität für Konstruktor