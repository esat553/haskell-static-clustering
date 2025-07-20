data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right) =
    if searchKey == k then Just v

{-
Ground Truth Metadata:
Error Type: Unvollständiger if-Ausdruck / Incomplete if-expression
Short Description: The `if` expression is missing a corresponding `else` branch, which is required in Haskell to form a complete conditional expression.
Intended Root Cause: Student forgets that Haskell requires both `then` and `else` branches and assumes that `if` can be used as a standalone conditional.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler