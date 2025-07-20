--precode
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node val left right = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Syntaxfehler / Syntax Error
Short Description: The closing parenthesis is missing in the pattern match for 'Node', leading to a malformed function definition.
Intended Root Cause: Student does not pay enough attention to syntactic detail and omits a required closing parenthesis.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Parse-Fehler