-- precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)

leaf :: a -> Tree a
leaf x = Leaf x


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node x left right) =
  let temp = mirror left
      left = mirror right
      right = temp
  in Node x left right

{-no compile error found -}