foo :: [Num a] -> [Num a]
foo xs = xs

{-
error:
    • Expected a type, but ‘Num a’ has kind ‘Constraint’
    • In the type signature: foo :: [Num a] -> [Num a]
  |
1 | foo :: [Num a] -> [Num a]
  |         ^^^^^

error:
    • Expected a type, but ‘Num a’ has kind ‘Constraint’
    • In the type signature: foo :: [Num a] -> [Num a]
  |
1 | foo :: [Num a] -> [Num a]
  |                    ^^^^^
-}