-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror t =
  case t of
    Leaf x -> Leaf x
    Node x left right
      let mirrored_left = mirror right
          mirrored_right = mirror left
      in Node x mirrored_left mirrored_right

{-
Ground Truth Metadata:
Error Type: Fehlender Pattern-Arrow in `case`-Ausdruck / Missing Pattern Arrow in `case` Expression
Short Description: The `Node x left right` pattern is not followed by a `->`, which is required to separate the pattern from the corresponding expression in a `case` block.
Intended Root Cause: Student omits the required syntax element `->` in a `case` expression and tries to write a `let` block directly after the pattern without properly starting the expression body.
Affected Line(s): 14 -16
-}
-- RegEx-Cluster: Ungültiges 'let in' in Pattern