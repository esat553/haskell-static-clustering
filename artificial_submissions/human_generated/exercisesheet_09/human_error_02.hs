import Data.Sequence (Seq(Empty))
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = Right x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Bezeichner / Use of Undefined Identifier
Short Description: The constructor 'Right' is used instead of the defined constructor 'Leaf', leading to an undefined identifier error.
Error Location: In the 'mirror' function, pattern match result (line 7).
Intended Root Cause: Student confuses data constructors from different types or forgets the correct name of the defined constructor.
Affected Line(s): 7
-}
-- RegEx-Cluster: Typenkonflikt