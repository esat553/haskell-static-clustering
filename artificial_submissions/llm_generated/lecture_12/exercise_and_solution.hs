{-
Implementiere eine Funktion mirror :: Tree a -> Tree a, die den gegebenen Binärbaum spiegelt, also linke und rechte Teilbäume rekursiv vertauscht.

Beispiel:
mirror (Node 1 (leaf 2) (leaf 3))  
⟶ Node 1 (leaf 3) (leaf 2)
-}
{-
english translation:
Implement a function mirror :: Tree a -> Tree a that mirrors the given binary tree, i.e., recursively swaps the left and right subtrees.

Example:
mirror (Node 1 (leaf 2) (leaf 3))
⟶ Node 1 (leaf 3) (leaf 2)
-}
-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x

mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) = Node x (mirror right) (mirror left)
