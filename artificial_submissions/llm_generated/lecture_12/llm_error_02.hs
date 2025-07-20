-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) = Node (mirror x) (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Falsche Rekursion auf Wert statt Teilbaum / Incorrect Recursion on Value Instead of Subtree
Short Description: The expression `mirror x` incorrectly applies `mirror` to the node's value `x`, which is not a subtree but a value of type `a`.
Intended Root Cause: Student mistakenly treats the node value as a subtree and applies the mirror function to it, misunderstanding the structure of the `Node` constructor.
Affected Line(s): 11
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur