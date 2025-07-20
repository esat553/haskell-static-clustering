-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree b
mirror (Leaf x) = Leaf x
mirror (Node x left right) = Node x (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Typinkonsistenz zwischen Signatur und Implementierung / Type Inconsistency Between Signature and Implementation
Short Description: The type signature `Tree a -> Tree b` suggests that the function transforms the type of the values stored in the tree, but the implementation preserves the values and only changes the structure, which requires the return type to be `Tree a`.
Intended Root Cause: Student incorrectly generalizes the return type without applying any transformation to the data within the tree, resulting in a type mismatch between specification and behavior.
Affected Line(s): 9
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur