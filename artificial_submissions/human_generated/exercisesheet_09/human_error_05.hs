--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node val left right) = Node (mirror right) val (mirror left)

{-
Ground Truth Metadata:
Error Type: Falsche Argumentreihenfolge / Wrong Argument Order
Short Description: The arguments for the 'Node' constructor are given in the wrong order; 'val' should be the first argument, followed by the left and right subtrees.
Error Location: In the 'mirror' function, 'Node' construction in the recursive case (line 8).
Intended Root Cause: Student misunderstands or forgets the expected order of arguments for the custom data type and mixes up the placement of value and subtrees.
Affected Line(s): 8
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur