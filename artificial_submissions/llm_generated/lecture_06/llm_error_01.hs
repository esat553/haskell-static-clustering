data TreeMap k v = Empty Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Eq k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v

{-
Ground Truth Metadata:
Error Type: Fehlende Konstruktor-Trennung / Missing Constructor Separator
Short Description: In the data declaration, the `|` symbol separating the constructors `Empty` and `Node` is missing, making the entire declaration syntactically invalid.
Intended Root Cause: Student forgets that alternative constructors in an algebraic data type must be separated using the `|` symbol and writes them as a single invalid constructor.
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonstruktor oder Klasse nicht definiert