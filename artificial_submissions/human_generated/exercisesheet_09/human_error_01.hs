data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (leaf x) = Leaf x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Konstruktor / Spelling Error in Constructor Name
Short Description: The constructor 'Leaf' is incorrectly written in lowercase as 'leaf', which is not recognized as a constructor in Haskell.
Error Location: In the 'mirror' function pattern match (line 6).
Intended Root Cause: Student is not aware that constructors in Haskell must start with an uppercase letter and accidentally uses a lowercase name.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler