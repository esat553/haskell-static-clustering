data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show)


mirror :: Tree a -> Tree a
mirror Leaf x = Leaf x
mirror (Node val left right) = Node val (mirror right) (mirror left)

{-
Ground Truth Metadata:
Error Type: Typenkonflikt bei String-Verkettung / Type Mismatch in String Concatenation
Short Description: Der Ausdruck `n ++ ": "` verursacht einen Fehler, da `n` vom Typ `Int` ist, `++` aber zwei Werte vom Typ `[Char]` (String) erwartet.
Intended Root Cause: Student geht fälschlicherweise davon aus, dass man einen `Int` direkt mit `++` an einen String anhängen kann, ohne ihn vorher mit `show` zu konvertieren.
Affected Line(s): 11
-}
-- RegEx-Cluster: Abweichende Arity