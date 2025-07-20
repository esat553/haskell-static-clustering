--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree -> Tree
mirror (Leaf x) = Leaf x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Falscher Typkonstruktor in Typensignatur / Wrong Type Constructor in Type Signature
Short Description: The type signature of 'mirror' omits the type parameter 'a' and is written as 'Tree -> Tree' instead of 'Tree a -> Tree a'.
Error Location: In the function type signature for 'mirror' (line 6).
Intended Root Cause: Student does not recognize that the data type 'Tree' is parameterized and must always be used with its type argument.
Affected Line(s): 6
-}
-- RegEx-Cluster: Falsche Anzahl von Typ-Argumenten