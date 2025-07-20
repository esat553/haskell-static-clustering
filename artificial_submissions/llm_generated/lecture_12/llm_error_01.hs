-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror Leaf x = Leaf x
mirror Node x left right = Node x (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Ungültiges Pattern Matching / Invalid Pattern Matching
Short Description: The pattern `Leaf x` is incorrectly written as `mirror Leaf x`, treating `Leaf` as a function rather than using pattern matching syntax. Similarly, `Node x left right` is used without parentheses, causing a parse error.
Intended Root Cause: Student confuses constructor application with pattern matching and omits necessary parentheses around multi-argument constructors in pattern position.
Affected Line(s): 10-11
-}
-- RegEx-Cluster: Abweichende Arity