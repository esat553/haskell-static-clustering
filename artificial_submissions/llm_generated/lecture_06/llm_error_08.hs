data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Ord k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v
    | searchKey < k  = lookupTM left
    | otherwise      = lookupTM searchKey right

{-
Ground Truth Metadata:
Error Type: Falsche Argumentanzahl beim Funktionsaufruf / Incorrect Number of Arguments in Function Call
Short Description: The recursive call `lookupTM left` provides only one argument, but the function requires two: a key and a tree.
Intended Root Cause: Student forgets to pass the search key again during recursion and treats the helper call as if only the tree argument were needed.
Affected Line(s): 6
-}
-- RegEx-Cluster: Falsche Funktionsarität