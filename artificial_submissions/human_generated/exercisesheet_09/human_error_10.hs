--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung in Typdefinition / Incorrect Capitalization in Type Declaration
Short Description: 'show' is written in lowercase in the 'deriving' clause; it must be capitalized as 'Show'.
Intended Root Cause: Student is unaware that type classes in Haskell must start with a capital letter and uses the wrong capitalization.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 3
-}
-- RegEx-Cluster: Ungültiges Deriving