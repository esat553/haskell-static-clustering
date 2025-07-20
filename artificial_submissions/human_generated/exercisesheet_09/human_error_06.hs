--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  derivering Show


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Schlüsselwort / Spelling Error in Keyword
Short Description: The keyword 'deriving' is misspelled as 'derivering', resulting in a syntax error.
Error Location: In the data type declaration (line 2).
Intended Root Cause: Student makes a typographical error in a reserved keyword, demonstrating lack of attention to detail or unfamiliarity with Haskell's syntax.
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich