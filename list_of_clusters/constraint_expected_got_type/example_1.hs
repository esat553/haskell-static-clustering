data Term a = C a

eval :: Integral a => Term a => a
eval (C x) = x

{-
error:
    • Expected a constraint, but ‘Term a’ has kind ‘*’
    • In the type signature: eval :: Integral a => Term a => a
  |
3 | eval :: Integral a => Term a => a
  |                       ^^^^^^
-}