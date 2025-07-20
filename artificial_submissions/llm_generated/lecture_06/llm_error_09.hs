data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: k -> TreeMap k v -> Maybe v
lookupTM searchKey (Node key val left right)
    | k == key = Just val
    | otherwise = Nothing

{-
Ground Truth Metadata:
Error Type: Variable nicht im Gültigkeitsbereich / Variable Not in Scope
Short Description: The function attempts to use the variable `k` in the guard expression, but `k` is only a type variable in the signature and not a bound value in the function body. This leads to a scope error, as `k` does not exist as a value in this context.
Intended Root Cause: Student confuses type variables with value variables and mistakenly refers to a type variable in the function body.
Affected Line(s): 5
-}

-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich