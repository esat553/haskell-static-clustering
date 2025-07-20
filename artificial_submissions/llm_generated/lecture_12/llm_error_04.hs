-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node x left right) = Node x (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Ungültiger Konstruktor im Pattern Matching / Invalid Constructor in Pattern Matching
Short Description: The pattern match `Empty` references a constructor that does not exist in the definition of `Tree`, which only has `Leaf` and `Node`.
Intended Root Cause: Student assumes that all recursive data types have an empty constructor like `Empty`, overlooking the specific constructors defined for `Tree`.
Affected Line(s): 10
-}
-- RegEx-Cluster: Nicht definierter Datenkonstruktor