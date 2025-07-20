--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node val left right) = Node val (mirror left)

{-
Ground Truth Metadata:
Error Type: Arity-Fehler / Arity Mismatch
Short Description: The 'Node' constructor is called with only two arguments ('val' and one subtree) instead of the required three ('val', 'left', 'right'), omitting the right subtree.
Error Location: In the 'mirror' function, construction of 'Node' in the recursive case (line 8).
Intended Root Cause: Student forgets that the 'Node' constructor expects three arguments and omits one when constructing the value.
Affected Line(s): 8
-}
-- RegEx-Cluster: Falsche Funktionsarität