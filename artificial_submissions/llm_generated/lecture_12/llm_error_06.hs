-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) = (mirror right, mirror left)

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp / Incorrect Return Type
Short Description: The expression `(mirror right, mirror left)` returns a tuple of trees instead of a `Tree`, which does not match the expected return type of the function.
Intended Root Cause: Student misinterprets the structure of the `Tree` data type and returns a tuple instead of reconstructing the tree using the `Node` constructor.
Affected Line(s): 11
-}
-- RegEx-Cluster: Typenkonflikt