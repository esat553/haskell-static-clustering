-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) = Node x mirrored_right mirrored_left

{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Variablen / Use of Undefined Variables
Short Description: The variables `mirrored_right` and `mirrored_left` are used without being previously defined or bound in the function body.
Intended Root Cause: Student intends to use intermediate results of recursive calls but forgets to define or bind these names before using them in the constructor application.
Affected Line(s): 11
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich