--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Typkonflikt / Type Mismatch
Short Description: The function 'mirror' returns 'x' (of type 'a') instead of 'Leaf x' (of type 'Tree a'), resulting in a type mismatch.
Error Location: In the 'mirror' function, first pattern match for 'Leaf' (line 6).
Intended Root Cause: Student confuses the type of the constructor's argument with the type of the constructor itself and forgets to wrap the result in 'Leaf'.
Affected Line(s): 6
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur