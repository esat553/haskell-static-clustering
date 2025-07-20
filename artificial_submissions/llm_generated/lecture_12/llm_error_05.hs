-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) = Node (x, mirror right, mirror left)

{-
Ground Truth Metadata:
Error Type: Falsche Konstruktoranwendung / Incorrect Constructor Application
Short Description: The expression `Node (x, mirror right, mirror left)` treats `Node` as a function of one 3-tuple argument, but `Node` expects three separate arguments.
Intended Root Cause: Student confuses constructor argument syntax and mistakenly groups multiple arguments into a tuple instead of passing them separately.
Affected Line(s): 11
-}
-- RegEx-Cluster: Falsche Funktionsarität