{-
Definiere einen polymorphen Datentyp TreeMap k v, der entweder leer ist oder einen Knoten mit Schlüssel, Wert und zwei Teilbäumen enthält. Implementiere anschließend eine Funktion lookupTM :: Eq k => k -> TreeMap k v -> Maybe v, die zu einem gegebenen Schlüssel den zugehörigen Wert zurückliefert.
-}
{-
english translation:
Define a polymorphic data type TreeMap k v that is either empty or contains a node with a key, a value, and two subtrees. Then, implement a function lookupTM :: Eq k => k -> TreeMap k v -> Maybe v that, for a given key, returns the corresponding value.
-}

data TreeMap k v = Empty | Node k v (TreeMap k v) (TreeMap k v)

lookupTM :: Ord k => k -> TreeMap k v -> Maybe v
lookupTM _ Empty = Nothing
lookupTM searchKey (Node k v left right)
    | searchKey == k = Just v
    | searchKey < k  = lookupTM searchKey left
    | otherwise      = lookupTM searchKey right