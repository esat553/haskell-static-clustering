-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf (mirror x)
mirror (Node x left right) = Node x (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Falscher Rekursionsaufruf auf Wert / Incorrect Recursive Call on Value
Short Description: In the pattern `Leaf x`, the recursive call `mirror x` is applied to the value `x` instead of preserving it. Since `x` is of type `a`, not `Tree a`, this leads to a type mismatch.
Intended Root Cause: Student mistakenly assumes that recursion must be applied in every case, including leaf values, and applies the function to a non-recursive data element.
Affected Line(s): 10
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur